
make_menu_sheet <- function(schema, wb){
  df <- menus_as_df(schema)
  openxlsx::addWorksheet(wb, "Menus")
  openxlsx::writeData(wb, "Menus", startCol = 1, startRow = 1, x = df, na.string = '')
}

menus_as_df <- function(schema){
  menus <- all_menus_per_col(schema)
  n <- max(length(menus))
  menus_n <- lapply(menus, `length<-`, n)
  df <- as.data.frame(menus_n)
  return(df)
}

make_sheet_from_slots <- function(schema, wb, sheet_name, slots, headers = NULL){
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::activeSheet(wb) <- sheet_name
  if (is.null(headers)){
    cats <- get_category(schema, slots)
  } else {
    cats <- headers
  }
  m <- matrix(data=c(cats, slots), nrow=2, ncol = length(slots), byrow = T)
  openxlsx::writeData(wb = wb, sheet = openxlsx::activeSheet(wb), x = m, colNames = F)
  bold_style <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::addStyle(wb = wb, sheet = openxlsx::activeSheet(wb), rows = 1:nrow(m), cols = 1:ncol(m), style = bold_style, gridExpand = TRUE, stack = TRUE)
  merge_same_horizontal(wb, x = cats, rows = 1)
  cols_with_menus <- names(all_menus_per_col(schema))
  cols_to_val <- slots[slots %in% cols_with_menus]
  sapply(FUN=apply_dropdown_validation, X=cols_to_val, wb=wb)
  sapply(FUN=format_according_to_importance, X=slots, wb=wb, schema=schema)
  openxlsx::freezePane(wb = wb, sheet = openxlsx::activeSheet(wb), firstActiveRow = 3, firstActiveCol = 2)
  openxlsx::setColWidths(wb, sheet = openxlsx::activeSheet(wb), cols=1:ncol(m), widths = "auto")
}

merge_same_horizontal <- function(wb, x, rows=1){
  runs   <- rle(x)
  ends   <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  ranges <- mapply(FUN  = seq,
                   from = starts,
                   to   = ends,
               SIMPLIFY = F)
  mapply(FUN = openxlsx::mergeCells,
        cols = ranges,
    MoreArgs = list(wb = wb,
                 sheet = openxlsx::activeSheet(wb),
                  rows = rows))
}

apply_dropdown_validation <- function(wb, col, startRow = 2, max_x = 5000){
  df <- openxlsx::readWorkbook(wb, sheet = openxlsx::activeSheet(wb) , startRow = startRow)
  y <- which(colnames(df)==col)
  coords <- get_validation_coords(wb, col)
  openxlsx::dataValidation(wb, sheet = openxlsx::activeSheet(wb), cols = y, rows = startRow+1:max_x, value = coords, type = "list")
}

get_validation_coords <- function(wb, col){
  df <- openxlsx::readWorkbook(wb, sheet = "Menus")
  y <- which(colnames(df)==col)
  x <- max(which(!is.na(df[col])))+1
  x_abs <- paste0("$", c(2,x))
  refs <- paste0("$",openxlsx::int2col(y), x_abs)
  str <- paste(refs, collapse = ":")
  val_str <- paste0("'Menus'!", str)
  return(val_str)
}

format_according_to_importance <- function(wb, schema, col, startRow=2){
  requ <- openxlsx::createStyle(fgFill = "yellow")
  recc <- openxlsx::createStyle(fgFill = "purple")
  df <- openxlsx::readWorkbook(wb, sheet = openxlsx::activeSheet(wb) , startRow = startRow)
  y <- which(colnames(df)==col)
  imp <- get_field_importance(schema, col)
  if (imp=="required"){
    openxlsx::addStyle(wb, sheet = openxlsx::activeSheet(wb), rows = startRow, cols = y, style = requ, stack = TRUE)
  } else if (imp=="recommended"){
    openxlsx::addStyle(wb, sheet = openxlsx::activeSheet(wb), rows = startRow, cols = y, style = recc, stack = TRUE)
  } else {}
}
