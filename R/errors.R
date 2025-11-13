#' Ensure that the slot is in the schema!
#' 
#' @keywords internal,errors
check_slots_in_schema <- function(schema, slots){
  in_schema <- slots %in% slot_names(schema)
  if (!all(in_schema)){
    stop(message = paste("Slot(s)", paste0(slots[!in_schema], collapse = ","), "not found in schema"))
  }
}