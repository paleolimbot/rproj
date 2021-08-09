
#' Lists of projections
#'
#' This class is probably unnecessary for users of rlibproj
#' but is used internally so that lists of PROJ objects
#' can be put into data frames (e.g., for the return value
#' of [proj_identify()]).
#'
#' @param x A [list()] of [PROJ objects][proj_create].
#'
#' @return An object of class "rlibproj_proj_list".
#' @export
#'
#' @examples
#' new_proj_list(list(proj_create("OGC:CRS84")))
#'
new_proj_list <- function(x = list()) {
  structure(x, class = "rlibproj_proj_list")
}

#' @export
`[.rlibproj_proj_list` <- function(x, i) {
  new_proj_list(NextMethod())
}

#' @export
`[<-.rlibproj_proj_list` <- function(x, i, value) {
  x <- unclass(x)
  x[i] <- lapply(value, as_proj)
  new_proj_list(x)
}

#' @export
`[[<-.rlibproj_proj_list` <- function(x, i, value) {
  x <- unclass(x)
  x[[i]] <- as_proj(value)
  new_proj_list(x)
}

#' @export
format.rlibproj_proj_list <- function(x, ...) {
  vapply(unclass(x), format.rlibproj_proj, character(1), ...)
}

#' @export
print.rlibproj_proj_list <- function(x, ...) {
  cat(sprintf("<proj_list[%d]>\n", length(x)))
  print(unclass(x), ...)
  invisible(x)
}

# data.frame() will call as.data.frame() with optional = TRUE
#' @export
as.data.frame.rlibproj_proj_list <- function(x, ..., optional = FALSE) {
  if (!optional) {
    NextMethod()
  } else {
    new_data_frame(list(x))
  }
}
