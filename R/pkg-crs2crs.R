
#' crs2crs transform engine
#'
#' @inheritParams proj_context
#' @inheritParams crs2crs::crs_engine_null
#' @inheritParams crs2crs::crs_proj_definition
#' @param allow_ballpark Use `FALSE` to omit ballpark transformations
#' @param accuracy The minimum desired accuracy for the transformation
#' @param bbox The bounding box to use when searching for appropriate
#'   transformations.
#' @param spatial_test Use "none" to skip using extents when selecting
#'   the appropriate transform between crs objects.
#' @inheritParams proj_create_crs_to_crs
#' @param ... Unused
#'
#' @return An engine that can be used with [crs_set_engine()].
#' @export
#'
crs_engine_rproj <- function(ctx = proj_context(), spatial_test = "intersects") {
  structure(list(ctx = ctx), class = "rlibrpoj_crs2crs_engine")
}

#' @rdname crs_engine_rproj
#' @importFrom crs2crs crs_engine_proj_pipeline
#' @export
crs_engine_proj_pipeline.rlibrpoj_crs2crs_engine <- function(engine, handleable, crs_to,
                                                             crs_from = wk::wk_crs(handleable),
                                                             bbox = wk::wk_bbox(handleable),
                                                             accuracy = NA_real_,
                                                             auth_name = NA_character_,
                                                             allow_ballpark = NA,
                                                             ...) {
  options <- character()
  if (!identical(auth_name, NA_character_)) {
    options <- c(options, paste0("AUTHORITY=", auth_name))
  }

  if (identical(allow_ballpark, TRUE)) {
    options <- c(options, "ALLOW_BALLPARK=yes")
  } else if (identical(allow_ballpark, FALSE)) {
    options <- c(options, "ALLOW_BALLPARK=no")
  }

  if (!identical(accuracy, NA_real_)) {
    options <- c(options, paste0("ACCURACY=", assert_dbl1(accuracy)))
  }

  crs_from <- crs_sanitize_proj_crs(crs_from)
  crs_to <- crs_sanitize_proj_crs(crs_to)
  if (!identical(engine$spatial_test, "intersects")) {
    bbox <- NULL
  }

  proj_create_crs_to_crs(crs_from, crs_to, options = options)
}

#' @rdname crs_engine_rproj
#' @importFrom crs2crs crs_engine_proj_pipeline_apply
#' @export
crs_engine_proj_pipeline_apply.rlibrpoj_crs2crs_engine <- function(engine, handleable, pipeline, ...) {
  trans <- as_wk_trans(crs_sanitize_proj_crs(pipeline))
  wk::wk_transform(handleable, pipeline)
}

#' @rdname crs_engine_rproj
#' @importFrom crs2crs crs_engine_get_wk_trans
#' @export
crs_engine_get_wk_trans.rlibrpoj_crs2crs_engine <- function(engine, handleable, crs_to, crs_from, ...) {
  pipeline <- crs_engine_proj_pipeline(engine, handleable, crs_to, crs_from)
  as_wk_trans(pipeline)
}

crs_sanitize_proj_crs <- function(crs) {
  if (inherits(crs, "rproj_proj")) {
    crs
  } else {
    as_proj(wk::wk_crs_proj_definition(crs, verbose = TRUE))
  }
}
