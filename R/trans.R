
#' Transform coordinates
#'
#' @param x An R object
#' @inheritParams proj_info
#' @param direction One of "fwd", "ident", or "inv".
#'
#' @return A transformed value of `x`
#' @export
#'
#' @examples
#' p <- proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857")
#' x <- proj_coord(-64, 45)
#' proj_trans(x, p)
#'
proj_trans <- function(x, pj, direction = "fwd") {
  UseMethod("proj_trans")
}

#' @rdname proj_trans
#' @export
proj_trans.matrix <- function(x, pj, direction = "fwd") {
  stopifnot(ncol(x) >= 4)
  direction <- proj_direction_code(assert_chr1(direction, "direction"))
  if (identical(direction, NA_integer_)) {
    stop("Invalid value for `direction`", call. = FALSE)
  }

  mode(x) <- "numeric"
  result <- .Call(proj_c_trans_matrix, as_proj(pj), x, direction)
  colnames(result) <- c("x", "y", "z", "t", "errno")
  result
}


#' Create coordinate matrices
#'
#' @param x,y,z,t Coordinate values
#'
#' @return A matrix with four columns (x, y, z, t)
#' @export
#'
#' @examples
#' proj_coord(1, 2)
#'
proj_coord <- function(x = NA_real_, y = NA_real_, z = NA_real_, t = NA_real_) {
  result <- cbind(x, y, z, t)
  colnames(result) <- c("x", "y", "z", "t")
  result
}
