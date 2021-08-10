
#' Get wk package transforms
#'
#' @param x A [PROJ object][proj_create]
#' @param trans A transform created with [wk::as_wk_trans()]
#'   on a [PROJ object][proj_create].
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
as_wk_trans.rlibproj_proj <- function(x, ..., use_z = NA, use_m = NA) {
  # uses NA as the reverse if unspecified which is probably
  # better than dropping it by default
  use_z <- as.logical(use_z)[1:2]
  use_m <- as.logical(use_m)[1:2]

  wk::new_wk_trans(
    .Call(proj_c_trans, x, use_z, use_m, proj_direction_code("FWD")),
    "rlibproj_trans_proj"
  )
}

#' @rdname as_wk_trans.rlibproj_proj
#' @importFrom wk wk_trans_inverse
#' @export
wk_trans_inverse.rlibproj_trans_proj <- function(trans) {
  wk::new_wk_trans(
    .Call(proj_c_trans_inverse, trans),
    "rlibproj_trans_proj"
  )
}
