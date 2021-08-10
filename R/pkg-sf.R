
# dynamically exported
st_crs.rlibproj_proj <- function(x, ...) {
  sf::st_crs(proj_make_compact_definition(x))
}
