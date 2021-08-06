
# The idea here is that the PJ_AREA can have west > east for
# an area that wraps the back of the earth. This uses a heuristic
# to get the narrowest possible bounding box.
as_proj_area <- function(handleable) {
  # missing: transform to OGC:CRS84
  xy <- unclass(wk::wk_handle(handleable, wk::wk_vertex_filter(wk::xy_writer())))

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
