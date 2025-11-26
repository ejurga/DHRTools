#' General logging function
#' 
#' The logging function, which uses cat and crayon, along with the option DHRtools.loglevel to 
#' print out errors, warnings, passes, and notes, depending on the desired log level.
#' 
#' @param type One of either error, warning, pass, or note.
#' @param slot the name of the slot, prepended to the log message if not null
#' @param ... the rest of the message to print
#' @returns Nothing, but will print the log using cat.
#' @keywords logging, internal
log_message <- function(type = c("error", "warning", "pass", "note"), slot = NULL, ...){
  type <- match.arg(type)
  print_text <- function(...) cat(log_type, log_slot, ": ", ..., "\n", sep = "")
 
  if (!is.null(slot))  log_slot <- paste(" on", slot) else log_slot <- NULL
  if ( type=="error" ) { 
    log_type <- crayon::red("FAILURE")    
    print_text(...)
  } else if ( type=="warning" & getOption("DHRtools.loglevel") %in% c("all", "warnings") ) { 
    log_type <- crayon::yellow("Warning")
    print_text(...)
  } else if (type=="pass" & getOption("DHRtools.loglevel")=="all" ) { 
    log_type <- crayon::green("Pass")
    print_text(...)
  } else if (type=="note") { 
    log_type <- crayon::blue("Note") 
    print_text(...)
  } else { # Otherwise do nothing (i.e., refuse to print if the option isn't specified)!
  }
}

#' Print out values that failed validation
#'
#' @param x Logical vector indicating pass/fail of validation
#' @param data The values that were validated -> used for printing out offending values
#' @param ids Vector of the chosen ID column
#' @keywords internal, logging 
#' @returns Nothing, but prints out the offending values that failed validation
log_failures <- function(x, data, ids = NULL){
  tab <- table(data[!x])
  offenders <- paste0(dQuote(names(tab), q = FALSE), ' (', unname(tab), ')')
  cat("    Offending values:", paste0(offenders, collapse = ', '), '\n')
  row_n <- which(!x)
  cat("    Rows:", paste0(row_n, collapse = ", "), '\n')
  if (!is.null(ids))  cat("    IDs:", paste0(ids[row_n], collapse = ", "), '\n')
}

#' Print out if validation passed or failed 
#'
#' @param x Logical vector indication pass/fail of validation
#' @param slot slot name
#' @param data the vector of data that was validated
#' @param pass_message Message to send to [log_pass]
#' @param fail_message Message to send to [log_error]
#' @param ids (optional) vector of the ids, for printing.
#' @keywords internal, logging, validation
#' @returns Nothing, but prints out if validation succeeded or failed.
log_basic_validation <- function(x, slot, data, pass_message, fail_message, ids = NULL){
  if ( all(x, na.rm = TRUE) ){
    log_message("pass", slot, pass_message) 
  } else {
    log_message("error", slot, fail_message)
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