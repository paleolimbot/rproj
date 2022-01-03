
# dynamically exported
st_crs.rproj_proj <- function(x, ...) {
  sf::st_crs(proj_make_compact_definition(x))
}

#' @export
as_proj.crs <- function(x, ...) {
  if (isTRUE(is.na(x$epsg))) {
    proj_create(x$input)
  } else {
    paste0("EPSG:", x$epsg)
  }
}
