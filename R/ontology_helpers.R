
#' Extract Ontology ID from GRDI term
#'
#' @param x Vector of GRDI terms in the format "Term name \[ONTOLOGY:0000000\]"
#'
extract_ont_id <- function(x){
  sub(x = x, "^.+\\[([A-Za-z_]+)[:_]([A-Z0-9]+)\\]", "\\1:\\2")
}

#' Get ontology name from an ontology ID
#'
#' Given a vector of ontology IDs (In the form of ONTOLOGY:00000000),
#' returns the lowercase name of the ontology ("ontology")
#'
#' @param x vector of ontology IDs
#'
extract_ontology_name <- function(x){
  tolower(sub(x = x, "([A-Za-z_]+):[A-Z0-9]+", "\\1"))
}

#' Separate GRDI ontology terms into two columns with the Term and ID
#'
#' A tidyverse eval function. Uses [tidyr::separate_wider_regex] to
#' separate a column of form "Term Name \[ONTOLOGY:00000000\]" in the
#' Term name in one column and the ID into another
#'
#' @param data  a data frame
#' @param <tidy-select> Column to separate
#'
#' @export
separate_ontology_terms <- function(data, col){
  separate_wider_regex(data, {{ col }},
                       patterns = c(Term = "^.+",
                                    "\\[",
                                    Id = "[A-Za-z_]+[:_][A-Z0-9]+",
                                    "\\]$"), too_few = "align_start") %>%
  mutate(Term = trimws(Term),
         Id = sub(x = Id, "_", ":"))
}