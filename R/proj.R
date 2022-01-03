
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
  .Call(rproj_c_create, ctx, assert_chr1(definition))
}

#' @rdname proj_create
#' @export
proj_clone <- function(pj, ctx = proj_context()) {
  .Call(rproj_c_clone, as_proj(pj), ctx)
}

#' @rdname proj_create
#' @export
proj_get_context <- function(pj) {
  .Call(rproj_c_get_context, pj)
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
    rproj_c_create_crs_to_crs,
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
  raw <- .Call(rproj_c_create_from_wkt, ctx, assert_chr1(wkt, "wkt"), options)

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

  if (!is.null(all_error_text) && inherits(raw[[1]], "rproj_proj")) {
    warning(paste0(all_error_text, collapse = "\n"), call. = FALSE)
  } else if (!is.null(all_error_text)) {
    stop(paste0(all_error_text, collapse = "\n"), call. = FALSE)
  }

  raw[[1]]
}

#' @rdname proj_create
#' @export
proj_get_source_crs <- function(pj, ctx = proj_context()) {
  .Call(rproj_c_get_source_crs, as_proj(pj), ctx)
}

#' @rdname proj_create
#' @export
proj_get_target_crs <- function(pj, ctx = proj_context()) {
  .Call(rproj_c_get_target_crs, as_proj(pj), ctx)
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
      rproj_c_identify,
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
  .Call(rproj_c_get_non_deprecated, as_proj(pj), ctx)
}

#' @rdname proj_create
#' @export
proj_normalize_for_visualization <- function(pj, ctx = proj_context()) {
  .Call(rproj_c_normalize_for_visualization, as_proj(pj), ctx)
}

#' @rdname proj_create
#' @export
as_proj <- function(x, ..., ctx = proj_context()) {
  UseMethod("as_proj")
}

#' @rdname proj_create
#' @export
as_proj.rproj_proj <- function(x, ..., ctx = proj_context()) {
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
  new_data_frame(.Call(rproj_c_proj_info, as_proj(pj)))
}

#' @rdname proj_info
#' @export
proj_get_type <- function(pj) {
  proj_type_name(.Call(rproj_c_get_type, as_proj(pj)))
}

#' @rdname proj_info
#' @export
proj_is_deprecated <- function(pj) {
  .Call(rproj_c_is_deprecated, as_proj(pj))
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
    rproj_c_is_equivalent_to,
    as_proj(pj),
    as_proj(other),
    criterion,
    ctx
  )
}

#' @rdname proj_info
#' @export
proj_is_crs <- function(pj) {
  .Call(rproj_c_is_crs, as_proj(pj))
}

#' @rdname proj_info
#' @export
proj_get_remarks <- function(pj) {
  .Call(rproj_c_get_remarks, as_proj(pj))
}

#' @rdname proj_info
#' @export
proj_get_scope <- function(pj) {
  .Call(rproj_c_get_scope, as_proj(pj))
}

#' @rdname proj_info
#' @export
proj_get_area_of_use <- function(pj) {
  raw <- .Call(rproj_c_get_area_of_use, as_proj(pj))
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

  .Call(rproj_c_as_wkt, as_proj(pj), wkt_type, options, ctx)
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

  .Call(rproj_c_as_proj_string, as_proj(pj), proj_string_type, options, ctx)
}

#' @rdname proj_info
#' @export
proj_as_projjson <- function(pj, options = NULL, ctx = proj_context()) {
  options <- as.character(options[!is.na(options)])
  .Call(rproj_c_as_projjson, as_proj(pj), options, ctx)
}

#' @rdname proj_info
#' @export
proj_as_projjson_parsed <- function(pj) {
  jsonlite::fromJSON(
    proj_as_projjson(pj, options = "MULTILINE=NO"),
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE
  )
}

proj_make_compact_definition <- function(pj, multiline = FALSE) {
  # for CRS, try to get an authority:code definition
  # otherwise, export as a PROJ5 string
  multiline_option <- if (multiline) "MULTILINE=YES" else "MULTILINE=NO"

  if (proj_is_crs(pj)) {
    id <- pj$id
    if (!is.null(id)) {
      return(paste0(id$authority, ":", id$code))
    }
    # try to identify this CRS using EPSG, then OGC, then ESRI
    proj_id <- proj_identify(pj, auth_name = c("EPSG", "OGC", "ESRI"))

    # keep to definitive results
    proj_id <- proj_id[proj_id$confidence == 100, , drop = FALSE]

    if (nrow(proj_id) > 0) {
      id <- proj_id$pj[[1]]$id
      if (!is.null(id)) {
        return(paste0(id$authority, ":", id$code))
      }
    }

    return(proj_as_wkt(pj, options = multiline_option))
  }

  tryCatch(
    proj_as_proj_string(pj, "PROJ_5", options = multiline_option),
    error = function(e) {
      proj_as_wkt(pj, options = multiline_option) # nocov
    }
  )
}

#' @export
format.rproj_proj <- function(x, ...) {
  proj_make_compact_definition(x)
}

#' @export
#' @importFrom utils str
str.rproj_proj <- function(object, ...) {
  cat(sprintf("<rproj_proj at %s> %s\n", proj_xptr_addr(object), format(object)))
  invisible(object)
}

#' @export
print.rproj_proj <- function(x, ...) {
  cat(sprintf("<rproj_proj at %s>\n", proj_xptr_addr(x)))

  # there are some objects (e.g., fresh proj_create_crs_to_crs("NAD27", "NAD83"))
  # that can't get exported to JSON but do work with proj_info
  parsed <- try(proj_as_projjson_parsed(x), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    info <- proj_info(x)
    cat("* Object can't be exported to JSON, falling back to proj_info()\n")
    for (nm in names(info)) {
      cat(sprintf("* %s: %s\n", nm, info[[nm]]))
    }
    return(x)
  }

  print_parsed(x, parsed)

  # for a source_crs->target_crs pipeline, display the source and
  # target CRS
  if (!is.null(parsed$source_crs)) {
    cat("* proj_get_source_crs():\n")
    print_parsed(proj_get_source_crs(x), parsed$source_crs, "  ")
  }

  if (!is.null(parsed$target_crs)) {
    cat("* proj_get_target_crs():\n")
    print_parsed(proj_get_target_crs(x), parsed$target_crs, "  ")
  }

  invisible(x)
}

print_parsed <- function(x, parsed = proj_as_projjson_parsed(x), indent = "") {
  if (!is.null(x)) {
    def <- proj_make_compact_definition(x, multiline = TRUE)
    cat(
      sprintf(
        "%s* Compact definition:\n  %s%s\n",
        indent, indent,
        gsub("\n", paste0("\n  ", indent), def)
      )
    )
  } else if(!is.null(parsed$id)) {
    cat(
      sprintf(
        "%s *Compact definition:\n  %s%s:%s\n",
        indent, indent, parsed$id$authority, parsed$id$code
      )
    )
  }

  properties <- list(
    "type", "name", "scope", "area",
    c("coordinate_system", "subtype"),
    c("method", "name")
  )

  for (nm in properties) {
    v <- try(parsed[[nm]], silent = TRUE)
    if (!is.null(v) && !inherits(v, "try-error")) {
      cat(
        sprintf(
          "%s* $%s: %s\n",
          indent,
          paste(nm, collapse = "$"),
          paste0('"', v, '"', collapse = ", ")
        )
      )
    }
  }

  # for any coordinate system with axes, list axes
  if (!is.null(parsed$coordinate_system$axis)) {
    cat(sprintf("%s* $coordinate_system$axis:\n", indent))
    for (i in seq_along(parsed$coordinate_system$axis)) {
      ax <- parsed$coordinate_system$axis[[i]]
      cat(sprintf("%s  - [[%d]] %s [%s]\n", indent, i, ax$name, ax$unit))
    }
  }

  # for a compound crs, print all components
  if (!is.null(parsed$components)) {
    for (i in seq_along(parsed$components)) {
      cat(sprintf("%s* $components[[%d]]:\n", indent, i))
      print_parsed(NULL, parsed$components[[i]], paste0(indent, "  "))
    }
  }

  invisible(parsed)
}

#' @export
`[[.rproj_proj` <- function(x, i) {
  proj_as_projjson_parsed(x)[[i]]
}

#' @export
`$.rproj_proj` <- function(x, i) {
  l <- proj_as_projjson_parsed(x)
  do.call(`$`, c(list(l), i))
}

#' @export
names.rproj_proj <- function(x) {
  names(proj_as_projjson_parsed(x))
}

#' @export
length.rproj_proj <- function(x) {
  tryCatch(length(proj_as_projjson_parsed(x)), error = function(e) NULL)
}
