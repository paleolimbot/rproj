
#' crs2crs transform engine
#'
#' @inheritParams proj_context
#' @inheritParams crs2crs::crs_engine_null
#' @inheritParams crs2crs::crs_proj_definition
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
crs_engine_rlibproj <- function(ctx = proj_context(), spatial_test = "intersects") {
  structure(list(ctx = ctx), class = "rlibrpoj_crs2crs_engine")
}

#' @rdname crs_engine_rlibproj
#' @importFrom crs2crs crs_engine_proj_pipeline
#' @export
crs_engine_proj_pipeline.rlibrpoj_crs2crs_engine <- function(engine, handleable, crs_to,
                                                             crs_from = wk::wk_crs(handleable),
                                                             bbox = wk::wk_bbox(handleable),
                                                             accuracy = NA_real_,
                                                             auth_name = NA_character_,
                                                             allow_ballpark = NA,
                                                             ...) {
  crs_from <- crs_sanitize_proj_crs(crs_from)
  crs_to <- crs_sanitize_proj_crs(crs_to)
  if (!identical(engine$spatial_test, "intersects")) {
    bbox <- NULL
  }

  proj_create_crs_to_crs(
    crs_from, crs_to,
    area = bbox,
    auth_name = auth_name,
    allow_ballpark = allow_ballpark
  )
}

#' @rdname crs_engine_rlibproj
#' @importFrom crs2crs crs_engine_proj_pipeline_apply
#' @export
crs_engine_proj_pipeline_apply.rlibrpoj_crs2crs_engine <- function(engine, handleable, pipeline, ...) {
  stop("Not implemented")
}

#' @rdname crs_engine_rlibproj
#' @importFrom crs2crs crs_engine_get_wk_trans
#' @export
crs_engine_get_wk_trans.rlibrpoj_crs2crs_engine <- function(engine, handleable, crs_to, crs_from, ...) {
  pipeline <- crs_engine_proj_pipeline(engine, handleable, crs_to, crs_from)
  as_wk_trans(pipeline)
}

#' @rdname crs_engine_rlibproj
#' @importFrom crs2crs crs_proj_definition
#' @export
crs_proj_definition.rlibproj_proj <- function(crs, proj_version = NULL) {
  stop("Not implemented")
}


crs_sanitize_proj_crs <- function(crs) {
  if (inherits(crs, "rlibproj_proj")) {
    crs
  } else {
    as_proj(crs2crs::crs_proj_definition(crs))
  }
}
