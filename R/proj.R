
#' Create PROJ objects
#'
#' In PROJ speak, a "proj" can be a transformation, conversion,
#' CRS, ellipsoid, datum, or one of a few other types. The
#' CRS and conversion types are the most common and the
#' most useful but the other types are also occasionally
#' useful.
#'
#' @param definition A character vector definition. This can be
#'   a PROJ string, WKT, Authority:Code, or any other format PROJ
#'   understands.
#' @param wkt A well-known text definition of the PROJ object.
#' @param options Options for instantiating the object by name
#' @param searched_name A name query
#' @param approximate_match Use `TRUE` for near matches
#' @param limit_result_count A limit on the number of matches
#' @param types The types of objects that should be queried. Type
#'   values of "crs" and "ellipsoid" are probably the most useful.
#' @param auth_name An authority name (e.g., "EPSG" or "OGC")
#' @param x An object to convert to a PROJ pointer
#' @param ... Passed to S3 methods
#' @inheritParams proj_context
#'
#' @export
proj_create <- function(definition, ctx = proj_context()) {

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
as_proj_proj <- function(x, ..., ctx = proj_context()) {
  UseMethod("as_proj_proj")
}

#' @rdname proj_create
#' @export
as_proj_proj.rlibproj_proj <- function(x, ..., ctx = proj_context()) {
  x
}

#' @rdname proj_create
#' @export
as_proj_proj.character <- function(x, ..., ctx = proj_context()) {
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
proj_proj_info <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_get_type <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_is_deprecated <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_is_equivalent_to <- function(obj, other, criterion) {

}

#' @rdname proj_proj_info
#' @export
proj_is_crs <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_get_remarks <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_get_scope <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_get_area_of_use <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_as_wkt <- function(obj, wkt_type = NULL, options = NULL,
                        ctx = proj_context()) {

}

#' @rdname proj_proj_info
#' @export
proj_as_proj_string <- function(obj, proj_string_type = NULL, options = NULL,
                                ctx = proj_context()) {

}

#' @rdname proj_proj_info
#' @export
proj_as_projjson <- function(obj, options = NULL, ctx = proj_context()) {

}

#' @rdname proj_proj_info
#' @export
proj_get_source_crs <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_get_target_crs <- function(obj) {

}

#' @rdname proj_proj_info
#' @export
proj_identify <- function(obj, auth_name, options = NULL, ctx = proj_context()) {

}

#' @rdname proj_proj_info
#' @export
proj_get_non_deprecated <- function(obj, ctx = proj_context()) {

}
