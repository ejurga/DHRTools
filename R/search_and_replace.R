#' amr_regexes
#'
#' Rerturn a vector to filter out AMR columns
#'
amr_regexes <-function(){
    c("_resistance_phenotype$",
      "_measurement(_units|_sign){0,1}$",
      "_laboratory_typing_[a-z_]+$",
      "_vendor_name$",
      "_testing_standard[a-z_]{0,}$",
      "_breakpoint$")
}

#' Append a term term if there are values already: 
#' 
#' @export
append_term <- function(x, terms){
  x[ is.na(x)] <- terms
  x[!is.na(x)] <- paste(x[!is.na(x)], terms, sep = '; ')
  return(x)
}

#' Search for a valid ontology term from a GRDI-AMR2 field
#' 
#' @param field The GRDI field to be searched over
#' @param x     Search term to be passed onto agrep
#' @param ...   Other arguments passed onto [agrep()]
#'
#' @export
grep_field_val <- function(schema, field, x, ...) {
  values <- get_permissible_values(schema, field)
  result <- agrep(x = values, pattern = x, value = TRUE, ...)
  return(result)
}

#' Replace values in a column with GRDI ontologies
#'
#' Given a column in a dataframe, this function will find the supplied term
#' in the data and replace it with an appropriate ontology from the GRDI
#' standard. Supports fuzzy matching via [agrep()]
#'
#' @param df Dataframe
#' @param col_name column name of the dataframe to replace over
#' @param term_query For looking up the ontology term we want to replace with.
#' @param data_query The value in the data that we want replace
#' @param term_query_dist Distance passed onto [agrep()], for fuzzy matching
#'  the desired ontology term
#' @param data_query_data Distance passed onto [agrep()], for fuzzy matching
#'  the value in the data.
#' @param select_GRDI_term If multiple results for an ontology term are returned,
#'  you may select one manually.
#'
#' @export
replace_with_GRDI_term <- function(df, col_name, term_query, data_query = NULL,
                                   term_query_dist = 0, data_query_dist = 0,
                                   select_GRDI_term = NULL){

  if (is.null(data_query)){
    data_query <- term_query
  }

  grdi_val <- grep_field_val(field = col_name,
                             x = term_query,
                             max.distance = term_query_dist)

  if (length(grdi_val) == 0){
    message("No grdi term found for query ", term_query)
    message("exiting")

  } else if (length(grdi_val) > 1) {
    message("Found GRDI terms for query ", term_query, ": ", paste(grdi_val, collapse = ', '))
    if (is.null(select_GRDI_term)){
      message("exiting")
    } else {
      grdi_val <- grdi_val[select_GRDI_term]
      message("Selecting term: ", grdi_val)
    }
  }

  if (length(grdi_val) == 1){
    x <- agrep(x = df[[col_name]], data_query, max.distance = data_query_dist)
    if (length(x)==0){
      message("No matches in data for query ", data_query)
      message("exiting")
    } else {
      n <- length(df[[col_name]][x])
      message("Replacing ", n,
              " instances of data values ",
              paste(unique(df[[col_name]][x]), collapse = ', '),
              " --> ",
              grdi_val)
      df[[col_name]][x] <- grdi_val
      return(df)
    }
  }
}

#' Search for an ontology term across all the possible GRDI fields.
#'
#' In case you want to search for a specific ontology term across all the
#' GRDI fields. Will print out the matches and the field to which that
#' ontology term belongs.
#'
#' @param x query to search the ontology terms with.
#' @param ... passed onto [agrep()] for fuzzy matching.
#'
#' @export
agrep_across_all_field_terms <- function(schema, x, ...) {
  fields <- get_all_field_ontology_terms(schema)
  results <- agrep(pattern = x, x = fields, value = TRUE)
  return(results)
}

#' Attempts to replace all values in a vector with GRDI terms
#'
#' @param x vector of strings to replace over
#' @param grdi_col The name of the grdi column with the ontology terms
#' @param term_query_dist For fuzzy searching.
#' @param ignore.case ignore case, passed onto agrep
#' @param ... passed onto agrep
#'
#' @return A vector with replaced terms, when found
#'
#' @export
replace_all_with_grdi <- function(x, grdi_col, term_query_dist = 0,
                                  ignore.case = TRUE, setNA = NA, ...){

  n_total_replaced = 0
  result <- as.character(x)
  vals <- unique(x)
  vals <- vals[!is.na(vals)]
  for (val in vals){
    grdi_val <- grep_field_val(field = grdi_col,
                               x = val,
                               max.distance = term_query_dist,
                               ignore.case = ignore.case, ...)
    if (length(grdi_val)>1) {
      message("Too many grdi terms for value ", val, " : ",
              paste(grdi_val, collapse = ", "))
    } else if (length(grdi_val)==0) {
      message("No grdi term for value ", val)
    } else {
      n_replaced <- sum(x==val, na.rm = TRUE)
      message(paste("Setting", n_replaced, "instances of", val, "-->", grdi_val))
      result[x==val] <- grdi_val
      n_total_replaced <- n_total_replaced + n_replaced
    }
  }
  if (anyNA(x)){
    message("Setting ", sum(is.na(x)), " NA's --> ", setNA)
    x[is.na(x)] <- setNA
    n_total_replaced <- n_total_replaced + sum(is.na(x))
  }
  message("Total replaced: ", n_total_replaced, ", ", round(n_total_replaced/length(x)*100, digits = 1), "%")
  return(result)
}
