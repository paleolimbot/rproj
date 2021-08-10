
#' Transform coordinates
#'
#' @param coord A matrix with four columns (x, y, z, t)
#' @inheritParams proj_info
#' @param direction One of "fwd", "ident", or "inv".
#' @param verbose Use `FALSE` to keep log output during transforms.
#'    This is usually not what you want, since you might be transforming
#'    millions of coordinates (which may result in millions of log
#'    messages).
#'
#' @return A transformed value of `coord`
#' @export
#'
#' @examples
#' p <- proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857")
#' x <- proj_coord(-64, 45)
#' proj_trans(p, x)
#'
proj_trans <- function(pj, coord, direction = "fwd", verbose = FALSE) {
  stopifnot(ncol(coord) >= 4)
  direction <- proj_direction_code(assert_chr1(direction, "direction"))
  if (identical(direction, NA_integer_)) {
    stop("Invalid value for `direction`", call. = FALSE)
  }

  mode(coord) <- "numeric"
  result <- .Call(proj_c_trans_matrix, as_proj(pj), coord, direction, assert_lgl1(verbose))
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
