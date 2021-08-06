
#' Create PROJ objects
#'
#' In PROJ speak, a "proj" can be a transformation, conversion,
#' CRS, ellipsoid, datum, or one of a few other types. The
#' CRS and conversion types are the most common and the
#' most useful but the other types are also occasionally
#' returned.
#'
#' @param definition A character vector definition. This can be
#'   a PROJ string, WKT, Authority:Code, or any other format PROJ
#'   understands.
#' @param source_crs,target_crs Source and/or target CRS definitions,
#'   coerced using [as_proj()].
#' @param pj A PROJ object or definition coerced using [as_proj()].
#' @param wkt A well-known text definition of the PROJ object.
#' @param options Options for instantiating the object by name
#' @param searched_name A name query
#' @param approximate_match Use `TRUE` for near matches
#' @param limit_result_count A limit on the number of matches
#' @param types The types of objects that should be queried. Type
#'   values of "crs" and "ellipsoid" are probably the most useful.
#' @param auth_name An authority name (e.g., "EPSG" or "OGC")
#' @param area A [PROJ area][as_proj_area] to use when selecting the
#'   best available transformation.
#' @param allow_ballpark Use `FALSE` to omit ballpark transformations
#' @param accuracy The minimum desired accuracy for the transformation
#' @param x An object to convert to a PROJ pointer
#' @param ... Passed to S3 methods
#' @inheritParams proj_context
#'
#' @export
proj_create <- function(definition, ctx = proj_context()) {
  .Call(proj_c_create, ctx, assert_chr1(definition))
}

#' @rdname proj_create
#' @export
proj_create_crs_to_crs <- function(source_crs, target_crs, area = NULL,
                                   auth_name = NA_character_,
                                   allow_ballpark = NA,
                                   accuracy = NA_real_,
                                   ctx = proj_context()) {
  if (!is.null(area)) {
    area <- as_proj_area(area)
  }

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

  .Call(
    proj_c_create_crs_to_crs,
    ctx,
    as_proj(source_crs, ctx = ctx),
    as_proj(target_crs, ctx = ctx),
    area,
    options
  )
}

#' @rdname proj_create
#' @export
proj_create_from_wkt <- function(wkt, ctx = proj_context()) {

}

#' @rdname proj_create
#' @export
proj_create_from_name <- function(searched_name, auth_name, types, approximate_match,
                                  limit_result_count, options = NULL,
                                  ctx = proj_context()) {

}

#' @rdname proj_create
#' @export
proj_guess_wkt_dialect <- function(wkt, ctx = proj_context()) {

}

#' @rdname proj_create
#' @export
proj_get_source_crs <- function(pj) {

}

#' @rdname proj_create
#' @export
proj_get_target_crs <- function(pj) {

}

#' @rdname proj_create
#' @export
proj_identify <- function(pj, auth_name, options = NULL, ctx = proj_context()) {

}

#' @rdname proj_create
#' @export
proj_get_non_deprecated <- function(pj, ctx = proj_context()) {

}

#' @rdname proj_create
#' @export
proj_normalize_for_visualization <- function(pj, ctx = proj_context()) {

}

#' @rdname proj_create
#' @export
as_proj <- function(x, ..., ctx = proj_context()) {
  UseMethod("as_proj")
}

#' @rdname proj_create
#' @export
as_proj.rlibproj_proj <- function(x, ..., ctx = proj_context()) {
  x
}

#' @rdname proj_create
#' @export
as_proj.character <- function(x, ..., ctx = proj_context()) {
  proj_create(x, ctx = ctx)
}


#' PROJ object information
#'
#' @param obj A [PROJ object][proj_create]
#' @param other Another [PROJ object][proj_create] against which
#'   `obj` should be compared
#' @param criterion An equivalence criterion. One of "strict",
#'   "equivalent", or "equivalent_except_axis_order_geogcrs".
#' @param wkt_type One of "wkt2_2015", "wkt_2015_simplified",
#'   "wkt2_2019", "wkt2_2019_simplified", "wkt1_gdal", or
#'   "wkt1_esri". Use `NULL` for the recommended default
#'   (wkt2_2019).
#' @param proj_string_type One of "proj_4" or "proj_5"
#' @param options String of additional options for various methods.
#' @inheritParams proj_create
#'
#' @export
#'
proj_info <- function(obj) {
  new_data_frame(.Call(proj_c_proj_info, as_proj(obj)))
}

#' @rdname proj_info
#' @export
proj_get_type <- function(obj) {
  proj_type_name(.Call(proj_c_get_type, as_proj(obj)))
}

#' @rdname proj_info
#' @export
proj_is_deprecated <- function(obj) {
  .Call(proj_c_is_deprecated, as_proj(obj))
}

#' @rdname proj_info
#' @export
proj_is_equivalent_to <- function(obj, other, criterion) {

}

#' @rdname proj_info
#' @export
proj_is_crs <- function(obj) {
  .Call(proj_c_is_crs, as_proj(obj))
}

#' @rdname proj_info
#' @export
proj_get_remarks <- function(obj) {
  .Call(proj_c_get_remarks, as_proj(obj))
}

#' @rdname proj_info
#' @export
proj_get_scope <- function(obj) {
  .Call(proj_c_get_scope, as_proj(obj))
}

#' @rdname proj_info
#' @export
proj_get_area_of_use <- function(obj) {
  raw <- .Call(proj_c_get_area_of_use, as_proj(obj))
  new_data_frame(
    list(
      name = raw[[2]],
      area = wk::rct(
        raw[[1]][1], raw[[1]][2], raw[[1]][3], raw[[1]][4],
        crs = "OGC:CRS84"
      )
    )
  )
}

#' @rdname proj_info
#' @export
proj_as_wkt <- function(obj, wkt_type = NULL, options = NULL,
                        ctx = proj_context()) {

}

#' @rdname proj_info
#' @export
proj_as_proj_string <- function(obj, proj_string_type = NULL, options = NULL,
                                ctx = proj_context()) {

}

#' @rdname proj_info
#' @export
proj_as_projjson <- function(obj, options = NULL, ctx = proj_context()) {

}

#' @export
format.rlibproj_proj <- function(x, ...) {
  sprintf("<proj at %s [%s]>\n", proj_xptr_addr(x), proj_get_type(x))
}

#' @export
print.rlibproj_proj <- function(x, ...) {
  cat(sprintf("<proj at %s [%s]>\n", proj_xptr_addr(x), proj_get_type(x)))
  cat(sprintf("* Description: %s\n", proj_info(x)$description))
  invisible(x)
}
