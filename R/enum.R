
proj_type_name <- function(x) {
  names <- .Call(proj_c_type_name, as.integer(x))
  names[names == ""] <- NA_character_
  names
}

proj_type_code <- function(x) {
  lookup <- 0:100
  names(lookup) <- proj_type_name(lookup)
  unname(lookup[toupper(x)])
}
