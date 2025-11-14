#' Holds a list of urls from which to download schema files
#' 
#' @keywords internal
schema_urls <- function(){
  urls <- list()
  urls$grdi <- "https://raw.githubusercontent.com/cidgoh/pathogen-genomics-package/main/templates/grdi/schema.yaml"
  return(urls)
}

#' Download a schema file from a url
#' 
#' @keywords internal
download_schema <- function(yaml_url){
  file <- tempfile()
  download.file(url = yaml_url, destfile = file, method = "wget",  extra = "-4")
  return(file)
}

#' Load a schema file
#' 
#' @param file The path to the yaml schema file
#' @returns A schema file -> really just a parsed YAML loaded into R.
#' @keywords internal
load_schema <- function(file){
  schema <- yaml::yaml.load_file(file)
  return(schema)
}

#' Get the slots of a schema
#' 
#' Get the slots (or columns) of a schema
#' 
#' @param schema A schema file returned from [load_schema]
slot_names <- function(schema){
  return(names(schema$slots))
}

#' Get the category of the slots
#' 
#' @inheritParams slot_names
#' @param slots A vector of slots
#' 
#' @return A named list of the slots with their category
get_category <- function(schema, slots){
  check_slots_in_schema(schema,slots)
  usage <- schema$classes[[2]]$slot_usage
  sapply(FUN=function(x) usage[[x]]$slot_group, X=slots, USE.NAMES = FALSE)
}

#' Get the categories that the schema uses to group its slots
#' 
#' @inheritParams slot_names
#' 
#' @returns a vector of the categories of the schema
categories <- function(schema){
  unique(get_category(schema, slots = slot_names(schema)))
}

#' Get all the slots that belong to the supplied category
#' 
#' @inheritParams slot_names
#' @param category A single category
#' 
#' @returns a vector of the slots that belong to the category
get_slots_per_cat <- function(schema, category){
  if ( !all(category %in% categories(schema)) ){
    stop("supplied category not found in schema")
  }
  all_slots <- slot_names(schema)
  cats      <- get_category(schema, slots = all_slots)
  x <- all_slots[cats %in% category]
  return(x)
}

#' Retreive the ranges of a slot
#' 
#' Returns the 'range' value of a slot.
#' 
#' @inheritParams slot_names
#' @param slot A name of a single slot
#' @returns A vector of the range names
#' @keywords internal
slot_ranges <- function(schema, slot){
  check_slots_in_schema(schema,slot)
  slots <- schema$slots
  any_ofs <- unlist(slots[[slot]]$any_of)
  ranges <-  unlist(slots[[slot]]$range)
  menus <- unname(c(any_ofs, ranges))
  return(menus)
}

#' Get the permissible values of a range, if they exist
#' 
#' All slots have ranges. Some of these ranges have values
#' Get the permissible values of each range,
#' 
#' @inheritParams slot_ranges
#' @param ranges A list of ranges, returned from [slot_ranges]
#' 
#' @returns A vector of permissible values for each range, NULL if none exist
#' @keywords internal
get_menu_values <- function(schema, ranges){
  x <- sapply(ranges, function(x) names(schema$enums[[x]]$permissible_values))
  return(x)
}

#' Get all the permissible values of a slot, if they exist.
#' 
#' This gets the permissible values of each range and appends them into 
#' a single vector. This includes the values of the NullValueMenu
#' 
#' @inheritParams slot_ranges
#' 
#' @returns A vector of permissible values for the slot
#' @keywords internal, parsing
get_permissible_values <- function(schema, slot){
  ranges <- slot_ranges(schema, slot)    
  vals <- get_menu_values(schema, ranges)
  return(unname(unlist(vals)))
}

#' Get the importance of a slot 
#' 
#' The standards often define the importance of a slot. 
#' Retrieve this for a single slot.
#' 
#' @inheritParams get_menu_values
#' @return The importance of the field
#' @keywords internal
get_field_importance <- function(schema, slot){
  check_slots_in_schema(schema,slot)
  slots <- schema$slots
  if (!is.null(slots[[slot]]$required)){
    return("required")
  } else if (!is.null(slots[[slot]]$recommended)){
    return("recommended")
  } else {
    return("normal")
  }
}

#' Get all the enums for each slot
#' 
#' Return a giant list of the slots and their corresponding enum entries
#' 
#' @inheritParams slot_names
#' @return A named list of the slots and their enums
#' @keywords internal
all_enums_per_col <- function(schema){
  cols <- names(schema$slots)
  # Remove AMR slots, but only if they are present.
  amr_index <- unlist(lapply(FUN = grep, X = amr_regexes(), x = cols))
  if (length(amr_index)>0) cols <- cols[-amr_index]
  vals <- sapply(FUN=get_permissible_values, X=cols, schema = schema)
  menus <- vals[lengths(vals)>0]
  return(menus)
}

#' Get the null value of the schema
#'
#' Returns a valid NULL value. If no query is supplied to search for a specific
#' ontology term, than a list of all the possible NULL values are supplied.
#'
#' @inheritParams slot_names
#' @param x Qery to search for a specific ontology term
#' @return A schema compliant null value
#' @keywords internal
#' @export
get_null_value <- function(schema, x=NULL){
  nulls <- names(schema$enums$`null value menu`$permissible_values)
  if (is.null(x)){
    return(nulls)
  } else {
    value <- grep(x = nulls, pattern = x, value = TRUE)
    if (length(value)==0){
      stop("No value found for query ", x, " in null value menu")
    } else {
      return(value)
    }
  }
}

#' Return the description and comments tags of a GRDI field
#'
#' @inheritParams get_menu_values
#'
#' @export
get_info <- function(schema, slot){
  check_slots_in_schema(schema,slot)
  f <- schema$slots[[slot]]
  cat("Description: ", str_wrap(f$description, width = 80), "\n")
  cat("Comments: ", str_wrap(f$comments, width = 80), "\n")
}

#' Get a list of all the ontology terms used in the standard.
#'
#' This function retrieves a list of all the enums (terms) used in a standard, 
#' returned as a named list. Organisational terms and nulls are removed from 
#' this list, for ease of searching through them
#'
#' @inheritParams slot_names
#' 
#' @return A named vector of all ontology terms from columns with menus, where
#'         the name of each value is the specification field it belongs to.
#'
#' @export
get_all_field_ontology_terms <- function(schema){
  x      <- all_enums_per_col(schema)
  values <- unlist(x, use.names = FALSE)
  nm     <- rep(names(x), times = lengths(x))
  names(values) <- nm
  org_terms <- grepl(x=values, "organizational term")
  nulls     <- values %in% get_null_value(schema)
  filtered  <- values[!(nulls | org_terms)]
  return(filtered)
}

#' Get the regex pattern of the slots
#' 
#' @inheritParams get_category
#' @returns a named vector of the regex that applies to the slot, or NA if none present
#' @keywords internal, parsing
get_pattern <- function(schema, slots){
    x <- sapply(slots, function(x) schema$slots[[x]]$pattern, simplify = T)
    x <- sapply(x    , function(x) if (is.null(x)) return(NA) else return(x))
    return(x)
}