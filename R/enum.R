
proj_enum_name <- function(x, name_lookup) {
  names <- name_lookup(x)
  names[names == ""] <- NA_character_
  names
}

proj_enum_code <- function(x, name_lookup) {
  lookup <- 0:100
  names(lookup) <- name_lookup(lookup)
  unname(lookup[toupper(x)])
}

proj_type_name_ <- function(x) .Call(proj_c_type_name, as.integer(x))
proj_type_name <- function(x) {
  proj_enum_name(x, proj_type_name_)
}
proj_type_code <- function(x) {
  proj_enum_code(x, proj_type_name_)
}

proj_comp_name_ <- function(x) .Call(proj_c_comp_name, as.integer(x))
proj_comp_name <- function(x) {
  proj_enum_name(x, proj_comp_name_)
}

proj_comp_code <- function(x) {
  proj_enum_code(x, proj_comp_name_)
}

proj_wkt_type_ <- function(x) .Call(proj_c_wkt_type, as.integer(x))
proj_wkt_type_name <- function(x) {
  proj_enum_name(x, proj_wkt_type_)
}

proj_wkt_type_code <- function(x) {
  proj_enum_code(x, proj_wkt_type_)
}

proj_proj_string_type_ <- function(x) .Call(proj_c_proj_string_type, as.integer(x))
proj_proj_string_type_name <- function(x) {
  proj_enum_name(x, proj_proj_string_type_)
}

proj_proj_string_type_code <- function(x) {
  proj_enum_code(x, proj_proj_string_type_)
}

