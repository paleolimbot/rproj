
#' Summarise a geographic area
#'
#' The concept of an "area" in PROJ means a geographic one
#' that can span the antimeridian. This function uses some
#' heuristics to find the minimum longitude span of the
#' input (provided that the longitude span is less than
#' 180 degrees). The [wk::wk_crs()] of the input is
#' considered and the appropriate transform to WGS84
#' is applied.
#'
#' @inheritParams wk::wk_handle
#'
#' @return `c(west, south, east, north)` in WGS84
#' @export
#'
#' @examples
#' as_proj_area(wk::rct(-1, -1, 1, 1))
#' as_proj_area(wk::rct(-179, 179, 1, 1))
#'
as_proj_area <- function(handleable) {
  if (is.double(handleable) && (length(handleable) == 4)) {
    return(handleable)
  }

  # transform to OGC:CRS84 (assume NULL as lon/lat)
  crs <- wk::wk_crs(handleable)
  if (is.null(crs) || inherits(crs, "wk_crs_inherit")) {
    to_wgs84 <- wk::wk_affine_identity()
  } else {
    pipe <- proj_create_crs_to_crs(crs, "OGC:CRS84")
    to_wgs84 <- wk::as_wk_trans(pipe)
  }

  # in the future we could probably use an affine
  # translate and the bbox handler to avoid resolving
  # every coordinate into memory
  xy <- wk::wk_handle(
    handleable,
    wk::wk_transform_filter(
      trans = to_wgs84,
      wk::wk_vertex_filter(
        wk::xy_writer()
      )
    )
  )

  xy <- unclass(xy)

  if (length(xy$x) == 0) {
    return(c(-180, -90, 180, 90))
  }

  meridian_options <- seq(-180, 180, length.out = 100)
  widths <- vapply(
    meridian_options,
    function(x) diff(range(as_proj_area_norm(xy$x - x), finite = TRUE)),
    double(1)
  )

  if (all(widths > 180)) {
    range_x <- c(-180, 180)
  } else {
    meridian <- meridian_options[which.min(widths)]
    range_x <- as_proj_area_norm(
      range(as_proj_area_norm(xy$x - meridian), finite = TRUE) + meridian
    )
  }

  range_y <- range(xy$y, finite = TRUE)
  c(range_x[1], range_y[1], range_x[2], range_y[2])
}

as_proj_area_norm <- function(x) {
  ((x + 180) %% 360) - 180
}
