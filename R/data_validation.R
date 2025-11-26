#' Validate a dataframe based on the supplied schema.
#'
#' This function validates the dataframe and prints a log indicating successes and 
#' failures. It first checks for schema columns that are not in the supplied dataframe. 
#' 
#' Next, it validates each slot based on the schema's rules.
#' It first checks to make sure that there 
#' is data in the dataframe of the supplied slot, and that the values are not all NA  #' (otherwise the function exits). 
#' It tests if any NAs are in recommended or required columns.
#' It checks for duplicates in slots marked as "identifiers". 
#' Finally, it determines the "type" of slot (e.g., date, or menu, string) and validates the data based on the type's respective rules.
#'
#' @param schema the data schema to use
#' @param data a dataframe of the data to test
#' @returns Nothing, but prints a log of validation passes and failures.
#' @keywords validation
#' @export
validate <- function(schema, data, loglevel = c("all", "warnings", "errors")){

  # Set loglevel option, I prefer this over passing the loglevel to all the 
  # functions.  
  loglevel <- match.arg(loglevel)
  options(DHRtools.loglevel = loglevel)
  
  id_col <- get_first_identification_col(schema)
  log_note("Using", id_col, "as identifier column")
  ids <- data[[id_col]]
 
  cat("Checking data column names against dataframe\n")
  cat("--------\n")
  missing <- get_missing_schema_cols(schema, data)
 
  slots <- slot_names(schema)
  slots_to_process <- slots[!slots %in% missing]
  
  cat("\n")
  cat("Validating columns\n")
  cat("-------\n")
  for (slot in slots_to_process){
    validate_slot_with_data(schema,
                            slot = slot,
                            data = data, 
                            ids = ids)
  }
}

#' Validate a column of data using slot parameters 
#' 
#' This is the main validation function. 
#'
#' @param schema the data schema 
#' @param slot The name of the slot to test
#' @param data A dataframe. The relevant column will be pulled based on the slot name. 
#'   The dataframe should have a column with the same name as the slot, but if it doesn't the 
#'   function will exit
#' @param ids vector of ids, used for downstream error printing
#' @returns Nothing, but prints a log of validation successes and failures.
#' @importFrom lubridate %within%
#' @keywords internal, validation
validate_slot_with_data <- function(schema, slot, data, ids){
  
  x <- data[[slot]]
  
  # If any NA in a required column, send a warning. 
  importance <- get_field_importance(schema, slot)
  if (anyNA(x)){
    if (importance == 'required')    log_error(slot, "NA values in required slot \n Rows: ", paste0(which(is.na(x)), collapse = ", "))
    if (importance == 'recommended') log_warning(slot, "NA values in recommended slot. Total empty values: ", sum(is.na(x)))
  }
  
  # Check is the slot is present in the data
  if (is.null(x)){
    log_warning(slot, "No data. Skipping")
    return(NULL)
  }
  # Check if the data is all empty!
  if (all(is.na(x))){
    log_warning(slot, "Data is all blank/empty! Skipping")
    return(NULL)
  }
  
  type     <- get_slot_type(schema, slot)
  # Check if this is an identifier column -> if so, check for duplicates:
  if (is_identifier(schema, slot)){
    if (any(duplicated(x))){
      log_error(slot, "Duplicate values in identifier column")
      cat("  Duplicated values:", paste0(x[duplicated(x)], collapse = ", "), "\n")
    } else {
      log_pass(slot, "No duplicates in identifier column")
    }
  }
 
  if      ( type == 'date'    ){  validate_date_slot(   schema, slot, data = x, ids = ids) }
  else if ( type == 'decimal' ){  validate_decimal_slot(schema, slot, data = x, ids = ids) }
  else if ( type == 'Menu'    ){  log_basic_validation(x = validate_values(schema, slot, x = x),
                                                    slot = slot, data = x,  
                                            pass_message = "All values allowed",  
                                            fail_message = "Values not found in picklist", 
                                                     ids = ids) }
  else if ( type == 'WhitespaceMinimizedString'){ 
    rgx <- get_pattern(schema, slot = slot)
    if (!is.na(rgx)){ 
      validate_rgx(rgx = rgx, slot = slot, data = x, ids = ids)
    } else {
      log_warning(slot, "Type 'WhiteSpaceMinimized' no pattern: no validation attempted")
    }
  } else if (type == 'Provenance') {
      log_warning(slot, "Type: Provenance, skipping")
  } else { stop("Unhandled type:", type)}
}

#' Validate a date-type slot
#' 
#' 
validate_date_slot <- function(schema, slot, data, ids){
  # Check for date parsing
  is_validated <- is_date(data) | validate_values(schema, slot, data)
  log_basic_validation(x = is_validated, slot = slot, data = data, 
                       pass_message = "All are either dates or null values", 
                       fail_message = "Datetime parse fail",
                       ids = ids)
  # Check for whitespace.
  is_white <- check_whitespace(data = data)
  log_basic_validation(x = is_white, slot = slot, data = data,  
                       pass_message = "No whitespace detected",  
                       fail_message = "Whitespace detected", 
                       ids = ids)
  # Check for date range
  time_range <- get_date_range_as_interval(schema, slot)
  dates <- suppressWarnings(lubridate::ymd(data))
  within_range <- dates %within% time_range
  log_basic_validation(x = within_range, slot = slot, data = data, 
                       pass_message = "All dates within bounds",
                       fail_message = "Dates out of bounds", 
                       ids = ids)
}

#' Validate decimal slot
validate_decimal_slot <- function(schema, slot, data, ids){
  
  # check for decimal parsing
  is_validated <- is_decimal(data) | validate_values(schema,slot,data)
  log_basic_validation(x = is_validated, slot = slot, data = data, 
                       pass_message = "All values either decimal or null value", 
                       fail_message = "Decimal values failed validation")
  # check for whitspace
  log_basic_validation(x = check_whitespace(data), slot = slot, data = data,
                       pass_message = "No whitespace detected", 
                       fail_message = "Whitespace detected", 
                       ids = ids)
  # check for range
  num_range <- min_max_value(schema, slot)
  if (!all(is.na(num_range))){
    nums <- suppressWarnings(as.numeric(data))
    in_range <- nums <= max(num_range) & nums >= min(num_range)
    log_basic_validation(x = in_range, slot = slot, data = data,
                         pass_message = "All values within range", 
                         fail_message = "Values out of range", 
                         ids = ids)
  }
}


#' Validate a string bassed on supplied regex.
#' 
#' This function takes a regular expression and tests the supplied data with 
#' it. Since NA's fail validation, remove them, but be explicit about it.
#' 
#' @param rgx Regular Expression to test on the data
#' @param data A vector of strings to test the expression on
#' @returns Nothing, but logs 
#' @keywords internal, validation
validate_rgx <- function(rgx, slot, data, ids){
  if (any(is.na(data))) log_warning(slot, "RGX validation: ", sum(is.na(data)), " Empty values, testing remainder")
  is_pattern <- grepl(pattern = rgx, data[!is.na(data)], perl = T)
  log_basic_validation(x = is_pattern, slot = slot, data = data, 
                       pass_message = "All values passed regex check", 
                       fail_message = paste0("Failure parsing regex ", rgx), 
                       ids = ids)
} 

#' Return TRUE if x matches val OR is NA!
#' 
#' Converts both input values and permissible values to lowercase 
#' 
#' @param x a vector
#' @param vals values to match against
#' @returns Logical vector
is_permissible_value <- function(x, vals){
  return(tolower(x) %in% tolower(vals) | is.na(x))
}

#' Validate data against permissible values
#' 
#' Validate the values against the list of permissible values of the slot. 
#' If the slot is a multivalued slot, split based on ';' delimiter, and return FALSE if ANY of the values do not match.
#' 
#' Note: This will return TRUE if the value is NA! We should catch this
#' seperately.
#' 
#' @return Logical vector TRUE if passed validation, FALSE if not
#' @keywords internal, validation
validate_values <- function(schema, slot, x){
    vals <- get_permissible_values(schema, slot)
    if (check_multivalues(schema, slot)){
      x_split <- strsplit(x, split = "; {0,1}")
      x_val   <- sapply(x_split, is_permissible_value, vals = vals)
      is_val  <- sapply(x_val, function(x) !any(!x))
    } else {
      is_val <- is_permissible_value(x, vals)
    }
    return(is_val)
}

#' Are the values expressed in Year-Month-Date format?
#' 
#' @param x A vector of strings to test. 
#' @returns Logical vector indicating if the string could be coerced to date format
#' @keywords internal, validation
is_date <- function(x){
  #!is.na(suppressWarnings(lubridate::ymd(x)))
  !is.na(suppressWarnings(as.Date(x, format = "%Y-%m-%d")))
}

check_whitespace <- function(data){
  !grepl(x=data, "(^[ ]{1,}|[ ]{1,}$)")
}

#' Are the values decimals?
#' 
#' @param x A vextor of strings to test.
#' @returns Logical vector indicating if the string could be coerced to decimal format
#' @keywords internal, validation
is_decimal <- function(x){
  !is.na(suppressWarnings(as.numeric(x)))
}


#' Rename Title columns to internal slot names
#' 
#' Schema slots have internal names (characters and underscores only) and "Titles", 
#' which are human readable names and can contain characters that play poorly with internal code.
#' This function renames title slots to computer-friendly names.
#' It will not rename slots that are not a part of the standard, and will return them.
#' 
#' @param schema The data schema
#' @param data Dataframe, with title slots renamed.
#' @keywords validation
#' @export 
rename_title_to_cols <- function(schema, data){
  n <- slot_names(schema)
  t <- slot_titles(schema, slot_names(schema))
  x <- match(colnames(data), t) 
  not_na <- which(!is.na(x))
  colnames(data)[not_na] <- n[x][not_na]
  return(data)
}

#' Select only those columns that are found in the schema 
#' 
#' From a dataframe, select only those columns that are found in the schema. 
#' Note: this function works with both "name" (underscores only) and "title" (human readable). 
#' It also rearranges the columns to match the order of the slots of the schema.
#' 
#' @param schema the schema
#' @param data dataframe to
#' @param log TRUE/FALSE, whether to log the columns removed
#' @returns A dataframe, with only the columns from the schema
#' @keywords validation
#' @export
select_cols_of_schema <- function(schema, data, log = FALSE){
  # Get slots and titles, and interlace them
  s <- slot_names(schema)  
  t <- slot_titles(schema, s)
  x <- as.vector(rbind(s,t))
  # Retrieve columns that are in the data
  in_schema <- colnames(data) %in% x
  if (log){
    cat("Removing columns:", paste0(colnames(data)[!in_schema], collapse = ", "), "\n")
  }
  sel <- data[,in_schema]  
  # Just reaarange according to slot order.
  df <- data[,colnames(sel)[order(match(colnames(sel), x))]]
  return(df)
}

#' Test for missing schema columns in the data
#' 
#' @param schema The schema
#' @param data Dataframe to test
#' @returns Nothing, but prints a log of columns that are missing in the dataframe
#' @keywords internal, validation
get_missing_schema_cols <- function(schema, data){
  slots <- slot_names(schema)
  in_df <- slots %in% names(data)
  missing <- slots[!in_df]
  x <- sapply(missing, get_field_importance, schema = schema)
  if (length(missing)>0){
    cat("These standard fields are MISSING in the data:\n")
    cat("REQUIRED:\n")
    cat(paste0(names(x[x=='required']), collapse = ", "))
    cat("\nRecommended\n")
    cat(paste0(names(x[x=='recommended']), collapse = ", "))
    cat("\nOther:\n")
    cat(paste0(names(x[x=='normal']), collapse = ", "))
    cat("\n")
  }
  return(missing)
}