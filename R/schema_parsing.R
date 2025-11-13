
schema_urls <- function(){
  urls <- list()
  urls$grdi <- "https://raw.githubusercontent.com/cidgoh/pathogen-genomics-package/main/templates/grdi/schema.yaml"
  return(urls)
}

download_schema <- function(yaml_url){
  file <- tempfile()
  download.file(url = yaml_url, destfile = file, method = "wget",  extra = "-4")
  return(file)
}

load_schema <- function(file){
  schema <- yaml::yaml.load_file(file)
  return(schema)
}

slot_names <- function(schema){
  return(names(schema$slots))
}

get_category <- function(schema, slots){
 usage <- schema$classes[[2]]$slot_usage
 sapply(FUN=function(x) usage[[x]]$slot_group, X=slots, USE.NAMES = FALSE)
}

categories <- function(schema){
  unique(get_category(schema, slots = slot_names(schema)))
}

get_slots_per_cat <- function(schema, category){
  if ( !all(category %in% categories(schema)) ){
    stop("supplied category not found in schema")
  }
  all_slots <- slot_names(schema)
  cats      <- get_category(schema, slots = all_slots)
  x <- all_slots[cats %in% category]
  return(x)
}

get_menu_values <- function(schema, column){
  menu_names <- col_ranges(schema, column)
  if (any(menu_names %in% c("WhitespaceMinimizedString", "date", "time"))){
    return(NULL)
  } else {
    all_vals <- c()
    for (menu in menu_names){
      vals <- names(schema$enums[[menu]]$permissible_values)
      all_vals <- append(all_vals, vals)
    }
    return(all_vals)
  }
}

col_ranges <- function(schema, column){
  slots <- schema$slots
  any_ofs <- unlist(slots[[column]]$any_of)
  ranges <-  unlist(slots[[column]]$range)
  menus <- unname(c(any_ofs, ranges))
  return(menus)
}

get_field_importance <- function(schema, col){
  if (!col %in% names(schema$slots)) stop("col not found in schema")
  slots <- schema$slots
  if (!is.null(slots[[col]]$required)){
    return("required")
  } else if (!is.null(slots[[col]]$recommended)){
    return("recommended")
  } else {
    return("normal")
  }
}

all_menus_per_col <- function(schema){
  cols <- names(schema$slots)
  # Remove AMR slots, but only if they are present.
  amr_index <- unlist(lapply(FUN = grep, X = amr_regexes(), x = cols))
  if (length(amr_index)>0) cols <- cols[-amr_index]
  vals <- sapply(FUN=get_menu_values, X=cols, schema = schema)
  menus <- vals[lengths(vals)>0]
  return(menus)
}
