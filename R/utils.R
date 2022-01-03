
proj_xptr_addr <- function(x) {
  .Call(rproj_c_xptr_addr, x)
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

sanitize_proj_crs <- function(crs) {
  if (inherits(crs, "rproj_proj")) {
    crs
  } else {
    as_proj(wk::wk_crs_proj_definition(crs))
  }
}

assert_chr1 <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.character(x) || (length(x) != 1)) {
    stop(sprintf("`%s` must be character of length 1", arg_name), call. = FALSE)
  }

  x
}

assert_dbl1 <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x) || (length(x) != 1)) {
    stop(sprintf("`%s` must be numeric of length 1", arg_name), call. = FALSE)
  }

  as.numeric(x)
}

assert_int1 <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x) || (length(x) != 1)) {
    stop(sprintf("`%s` must be numeric of length 1", arg_name), call. = FALSE)
  }

  xint <- as.integer(x)
  if (!is.na(xint) && !isTRUE(xint == x)) {
    stop(sprintf("`%s` must be an integer of length 1", arg_name), call. = FALSE)
  }

  xint
}

assert_lgl1 <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.logical(x) || (length(x) != 1) || identical(x, NA)) {
    stop(sprintf("`%s` must be logical of length 1", arg_name), call. = FALSE)
  }

  x
}


new_data_frame <- function(x) {
  structure(x, row.names = c(NA, length(x[[1]])), class = "data.frame")
}
