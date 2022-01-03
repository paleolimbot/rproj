
#' Get wk package transforms
#'
#' @param x A [PROJ object][proj_create]
#' @param trans A transform created with [wk::as_wk_trans()]
#'   on a [PROJ object][proj_create].
#' @inheritParams proj_trans
#' @param ... Unused
#' @param use_z,use_m Use `TRUE` to force inclusion of this dimension
#'   in the result or `FALSE` to explicitly omit it. Use `NA`
#'   to leave the dimensions of the output the same as the input.
#'   Use a length two vector to specify the dimensionality
#'   of the inverse transform.
#'
#' @return A [wk::new_wk_trans()]
#' @importFrom wk as_wk_trans
#' @export
#'
as_wk_trans.rproj_proj <- function(x, ..., use_z = NA, use_m = NA, verbose = FALSE) {
  # uses NA as the reverse if unspecified which is probably
  # better than dropping it by default
  use_z <- as.logical(use_z)[1:2]
  use_m <- as.logical(use_m)[1:2]

  # we probably don't want errors to be logged
  # to get around the log_level of whatever context created
  # `x`, we can make a clone
  if (!isTRUE(verbose)) {
    x <- proj_clone(
      x,
      ctx = proj_context_create(log_level = 0, ctx = proj_get_context(x))
    )
  }

  wk::new_wk_trans(
    .Call(rproj_c_trans, x, use_z, use_m, proj_direction_code("FWD")),
    "rproj_trans_proj"
  )
}

#' @rdname as_wk_trans.rproj_proj
#' @importFrom wk wk_trans_inverse
#' @export
wk_trans_inverse.rproj_trans_proj <- function(trans) {
  wk::new_wk_trans(
    .Call(rproj_c_trans_inverse, trans),
    "rproj_trans_proj"
  )
}

#' @export
print.rproj_trans_proj <- function(x, ...) {
  pj <- .Call(rproj_c_trans_get_pj, x)
  direction <- .Call(rproj_c_trans_get_direction, x)

  cat(
    sprintf(
      "<wk::new_wk_trans() at %s, direction %s -> ",
      proj_xptr_addr(x),
      proj_direction_name(direction)
    )
  )

  print(pj)

  invisible(x)
}

#' @importFrom wk wk_crs_proj_definition
#' @export
wk_crs_proj_definition.rproj_proj <- function(crs, proj_version = NULL, verbose = FALSE) {
  if (verbose) {
    proj_as_wkt(crs)
  } else {
    proj_make_compact_definition(crs)
  }
}

#' @importFrom wk wk_crs_equal_generic
#' @export
wk_crs_equal_generic.rproj_proj <- function(x, y) {
  proj_is_equivalent_to(
    sanitize_proj_crs(x), sanitize_proj_crs(y),
    criterion = "equivalent"
  )
}
