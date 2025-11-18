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
validate <- function(schema, data, log = c("all", "errors")){
  
  log <- match.arg(log)
 
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
                            data = df, 
                            log = log)
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
#' @returns Nothing, but prints a log of validation successes and failures.
#' @importFrom lubridate %within%
#' @keywords internal, validation
validate_slot_with_data <- function(schema, slot, data, log){
  
  x <- data[[slot]]
  
  # If any NA in a required column, send a warning. 
  importance <- get_field_importance(schema, slot)
  if (anyNA(x)){
    if (importance == 'required') log_error(slot, "NA values in required slot \n Rows: ", paste0(which(is.na(x)), collapse = ", "))
    #if (importance == 'recommended') cat("\n WARNING: NA values in recommended slot \nRows: ", paste0(which(is.na(x)), collapse = ", "), "\n")
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
  
  # Check if this is an identifier column -> if so, check for duplicates:
  if (is_identifier(schema, slot)){
    if (any(duplicated(x))){
      log_error(slot, "Duplicate values in identifier column")
      cat("  Duplicated values:", paste0(x[duplicated(x)], collapse = ", "), "\n")
    } else {
      log_success(slot, data, type, log)
    }
  }

  type     <- get_slot_type(schema, slot)
  is_value <- validate_values(schema,slot,x)
 
  if (type == 'date'   ){ 
    is_validated <- is_date(x)    | is_value 
    log_basic_validation(x = is_validated, slot = slot, type = type, data = x, log = log)
    log_basic_validation(x = check_whitespace(x), slot = slot, type = "date: check trailing whitespace", data = x, log = log)
    # Check that the dates are right!
    time_range <- get_date_range_as_interval(schema, slot)
    dates <- suppressWarnings(lubridate::ymd(x))
    if (all(is.na(dates))){
      log_warning(slot, "No dates parsed (none provided?) -> skipping range validation")
    } else {
      within_range <- dates %within% time_range
      if (all(within_range, na.rm = T)){
        cat("PASS: all dates within bounds\n")
      } else {
        log_error(slot, "date out of bounds.")
        off <- dates[!within_range]
        off <- off[!is.na(off)]
        cat("Offending values:", paste0(off, collapse = ", "), "\n")
      }
    }
  } else if (type == 'decimal'){ 
    is_validated <- is_decimal(x) | is_value 
    log_basic_validation(x = is_validated, slot = slot, type = type, data = x, log = log)
    log_basic_validation(x = check_whitespace(x), slot = slot, type = "decimal, check trailing whitespace", data = x, log = log) 
  } else if (type == 'Menu'){ 
    is_validated <- is_value                 
    log_basic_validation(x = is_validated, slot = slot, type = type, data = x, log = log)
  } else if (type == 'WhitespaceMinimizedString'){ 
    rgx <- get_pattern(schema, slot = slot)
    if (!is.na(rgx)){ 
      validate_rgx(rgx = rgx, slot = slot, data = x, log = log)
    } else {
      log_warning(slot, "Type 'WhiteSpaceMinimized' no pattern: no validation attempted")
    }
  } else if (type == 'Provenance') {
      log_warning(slot, "Type: Provenance, skipping")
  } else { stop("Unhandled type:", type)}
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
validate_rgx <- function(rgx, slot, data, log){
  if (any(is.na(data))) log_warning(slot, "RGX validation: ", sum(is.na(data)), "Empty values, testing remainder")
  is_pattern <- grepl(pattern = rgx, data[!is.na(data)], perl = T)
  log_basic_validation(x = is_pattern, slot = slot, type = 'String with Regex', data = data, log = log)
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
  !is.na(suppressWarnings(lubridate::ymd(x)))
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