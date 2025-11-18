#' Print out values that failed validation
#'
#' @param x Logical vector indicating pass/fail of validation
#' @param data The values that were validated -> used for printing out offending values
#' @keywords internal, validation
#' @returns Nothing, but prints out the offending values that failed validation
log_failures <- function(x, data){
  tab <- table(data[!x])
  offenders <- paste0(names(tab), ' (', unname(tab), ')')
  cat("Offending values:", paste0(offenders, collapse = ', '), '\n')
  cat("Rows:", which(!x),'\n')
}

log_successes <- function(data, type){

  cat(crayon::green("PASS"), " for type", type, "\n")
  if (type == 'Menu'){
    df <- as.data.frame(table(data))
    colnames(df) <- c("Value", "N")
    df <- dplyr::arrange(df, dplyr::desc(N))
    print(knitr::kable(df))
  } else {
    cat("Must implement for", type)
  }
}

#' Print out if validation passed or failed 
#' 
#' @param x Logical vector indication pass/fail of validation
#' @param type The 'type' of the slot, returned from [get_slot_type]
#' @param data the vector of data that was validated
#' @keywords internal, validation
#' @returns Nothing, but prints out if validation succeeded or failed.
log_basic_validation <- function(x, type, data){
  if (all(x)){
    log_successes(data, type)
  } else {
    cat(crayon::red("FAILED"), " validation for type", type, "\n")
    log_failures(x=x, data=data)
  } 
  cat("\n")
}