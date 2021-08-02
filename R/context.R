
#' PROJ Global Context
#'
#' @return An external pointer to a PROJ context
#' @export
#'
#' @examples
#' proj_context()
#'
proj_context <- function() {
  new_proj_context(.Call(proj_c_pj_default_ctx))
}

new_proj_context <- function(x) {
  structure(x, class = "rlibproj_context")
}

#' @export
format.rlibproj_context <- function(x, ...) {
  sprintf("<proj_context at %s>", "addr")
}

#' @export
print.rlibproj_context <- function(x, ...) {
  cat(sprintf("<proj_context at %s>\n", "addr"))
  invisible(x)
}
