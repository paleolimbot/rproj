
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
#' @param auth_name An authority name (e.g., "EPSG" or "OGC")
#' @param area A [PROJ area][as_proj_area] to use when selecting the
#'   best available transformation.
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
                                   options = character(), ctx = proj_context()) {
  if (!is.null(area)) {
    area <- as_proj_area(area)
  }

  options <- as.character(options[!is.na(options)])

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
proj_create_from_wkt <- function(wkt, options = character(), ctx = proj_context()) {
  options <- as.character(options[!is.na(options)])
  raw <- .Call(proj_c_create_from_wkt, ctx, assert_chr1(wkt, "wkt"), options)

  # a few options depending on whether or not there were any warnings, errors
  # or whether or not a PJ* was constructed
  all_error_text <- NULL
  if (length(raw[[2]]) > 0) {
    warnings <- paste0("- ", raw[[2]], collapse = "\n")
    warning_txt <- paste0("WKT parser reported warning(s):\n", warnings)
    all_error_text <- warning_txt
  }

  if (length(raw[[3]]) > 0) {
    errors <- paste0("- ", raw[[3]], collapse = "\n")
    error_txt <- paste0("WKT parser reported error(s):\n", errors)
    all_error_text <- c(all_error_text, error_txt)
  }

  if (!is.null(all_error_text) && inherits(raw[[1]], "rlibproj_proj")) {
    warning(paste0(all_error_text, collapse = "\n"), call. = FALSE)
  } else if (!is.null(all_error_text)) {
    stop(paste0(all_error_text, collapse = "\n"), call. = FALSE)
  }

  raw[[1]]
}

#' @rdname proj_create
#' @export
proj_get_source_crs <- function(pj, ctx = proj_context()) {
  .Call(proj_c_get_source_crs, as_proj(pj), ctx)
}

#' @rdname proj_create
#' @export
proj_get_target_crs <- function(pj, ctx = proj_context()) {
  .Call(proj_c_get_target_crs, as_proj(pj), ctx)
}

#' @rdname proj_create
#' @export
proj_identify <- function(pj, auth_name = NULL, ctx = proj_context()) {
  auth_name <- as.character(auth_name[!is.na(auth_name)])

  if (length(auth_name) == 0) {
    return(new_data_frame(list(pj = list(), confidence = integer())))
  }

  results <- lapply(
    auth_name,
    function(auth) .Call(
      proj_c_identify,
      as_proj(pj),
      auth,
      ctx
    )
  )

  new_data_frame(
    list(
      pj = new_proj_list(unlist(lapply(results, "[[", 1))),
      confidence = unlist(lapply(results, "[[", 2))
    )
  )
}

#' @rdname proj_create
#' @export
proj_get_non_deprecated <- function(pj, ctx = proj_context()) {
  .Call(proj_c_get_non_deprecated, as_proj(pj), ctx)
}

#' @rdname proj_create
#' @export
proj_normalize_for_visualization <- function(pj, ctx = proj_context()) {
  .Call(proj_c_normalize_for_visualization, as_proj(pj), ctx)
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
#' @param other Another [PROJ object][proj_create] against which
#'   `pj` should be compared
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
proj_info <- function(pj) {
  new_data_frame(.Call(proj_c_proj_info, as_proj(pj)))
}

#' @rdname proj_info
#' @export
proj_get_type <- function(pj) {
  proj_type_name(.Call(proj_c_get_type, as_proj(pj)))
}

#' @rdname proj_info
#' @export
proj_is_deprecated <- function(pj) {
  .Call(proj_c_is_deprecated, as_proj(pj))
}

#' @rdname proj_info
#' @export
proj_is_equivalent_to <- function(pj, other, criterion = NULL, ctx = proj_context()) {
  if (is.null(criterion)) {
    criterion <- "strict"
  }

  criterion <- proj_comp_code(assert_chr1(criterion, "criterion"))
  if (identical(criterion, NA_integer_)) {
    stop("Invalid value for `criterion`.", call. = FALSE)
  }

  .Call(
    proj_c_is_equivalent_to,
    as_proj(pj),
    as_proj(other),
    criterion,
    ctx
  )
}

#' @rdname proj_info
#' @export
proj_is_crs <- function(pj) {
  .Call(proj_c_is_crs, as_proj(pj))
}

#' @rdname proj_info
#' @export
proj_get_remarks <- function(pj) {
  .Call(proj_c_get_remarks, as_proj(pj))
}

#' @rdname proj_info
#' @export
proj_get_scope <- function(pj) {
  .Call(proj_c_get_scope, as_proj(pj))
}

#' @rdname proj_info
#' @export
proj_get_area_of_use <- function(pj) {
  raw <- .Call(proj_c_get_area_of_use, as_proj(pj))
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
proj_as_wkt <- function(pj, wkt_type = NULL, options = character(),
                        ctx = proj_context()) {
  options <- as.character(options[!is.na(options)])
  if (is.null(wkt_type)) {
    wkt_type <- "WKT2_2015"
  }

  wkt_type <- proj_wkt_type_code(assert_chr1(wkt_type, "wkt_type"))
  if (identical(wkt_type, NA_integer_)) {
    stop("Invalid value for `wkt_type`", call. = FALSE)
  }

  .Call(proj_c_as_wkt, as_proj(pj), wkt_type, options, ctx)
}

#' @rdname proj_info
#' @export
proj_as_proj_string <- function(pj, proj_string_type = NULL, options = NULL,
                                ctx = proj_context()) {
  options <- as.character(options[!is.na(options)])
  if (is.null(proj_string_type)) {
    proj_string_type <- "PROJ_5"
  }

  proj_string_type <- proj_proj_string_type_code(assert_chr1(proj_string_type, "proj_string_type"))
  if (identical(proj_string_type, NA_integer_)) {
    stop("Invalid value for `proj_string_type`", call. = FALSE)
  }

  .Call(proj_c_as_proj_string, as_proj(pj), proj_string_type, options, ctx)
}

#' @rdname proj_info
#' @export
proj_as_projjson <- function(pj, options = NULL, ctx = proj_context()) {
  options <- as.character(options[!is.na(options)])
  .Call(proj_c_as_projjson, as_proj(pj), options, ctx)
}

#' @rdname proj_info
#' @export
proj_as_projjson_parsed <- function(pj) {
  jsonlite::fromJSON(
    proj_as_projjson(pj, options = "MULTILINE=NO"),
    simplifyVector = TRUE
  )
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

#' @export
`[[.rlibproj_proj` <- function(x, i) {
  proj_as_projjson_parsed(x)[[i]]
}

#' @export
`$.rlibproj_proj` <- function(x, i) {
  l <- proj_as_projjson_parsed(x)
  do.call(`$`, c(list(l), i))
}

#' @export
names.rlibproj_proj <- function(x) {
  names(proj_as_projjson_parsed(x))
}
