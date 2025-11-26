
#' Simply log an error and call the slot name!
#'
#' @param slot the name of the slot!
#' @param ... Additional items to print via cat
log_error <- function(slot, ...){
  cat(crayon::red("FAILURE"), " on ", slot, ": ", ..., "\n", sep = "")
}

#' Log a warning
#'
#' @inheritParams log_error
log_warning <- function(slot, ...){
  if (getOption("DHRtools.loglevel") %in% c("all", "warnings")){
    cat(crayon::yellow("Warning"), " on ", slot, ": ", ..., "\n", sep = "")
  }
}

#' Log a pass
#'
log_pass <- function(slot, ...){
  if (getOption("DHRtools.loglevel")=="all"){
    cat(crayon::green("Pass"), " on ", slot, ": ", ..., "\n", sep = "")
  }
}

#' Log a note
#'
log_note <- function(...){
  cat(crayon::blue("Note:"), ..., sep = " ", "\n")
}

#' Print out values that failed validation
#'
#' @param x Logical vector indicating pass/fail of validation
#' @param data The values that were validated -> used for printing out offending values
#' @param ids Vector of the chosen ID column
#' @keywords internal, validation
#' @returns Nothing, but prints out the offending values that failed validation
log_failures <- function(x, data, ids = NULL){
  tab <- table(data[!x])
  offenders <- paste0(dQuote(names(tab), q = FALSE), ' (', unname(tab), ')')
  cat("    Offending values:", paste0(offenders, collapse = ', '), '\n')
  row_n <- which(!x)
  cat("    Rows:", row_n,'\n')
  if (!is.null(ids))  cat("    IDs:", ids[row_n],'\n')
}

#' Print out if validation passed or failed 
#'
#' @param x Logical vector indication pass/fail of validation
#' @param slot slot name
#' @param data the vector of data that was validated
#' @param pass_message Message to send to [log_pass]
#' @param fail_message Message to send to [log_error]
#' @param ids (optional) vector of the ids, for printing.
#' @keywords internal, validation
#' @returns Nothing, but prints out if validation succeeded or failed.
log_basic_validation <- function(x, slot, data, pass_message, fail_message, ids = NULL){
  if ( all(x, na.rm = TRUE) ){
    log_pass(slot, pass_message) 
  } else {
    log_error(slot, fail_message)
    log_failures(x, data = data, ids = ids)
  }
}

log_success <- function(slot, data, type){
  
  if (getOption("DHRtools.loglevel")=="all"){
    log_pass(slot)
    if (type == 'Menu'){
      df <- as.data.frame(table(data))
      colnames(df) <- c("Value", "N")
      df <- dplyr::arrange(df, dplyr::desc(N))
      print(knitr::kable(df))
    } else {
      cat("Must implement for", type)
    }
  }
}