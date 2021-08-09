
proj_xptr_addr <- function(x) {
  .Call(proj_c_xptr_addr, x)
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

assert_lgl1 <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.logical(x) || (length(x) != 1)) {
    stop(sprintf("`%s` must be logical of length 1", arg_name), call. = FALSE)
  }

  x
}


new_data_frame <- function(x) {
  structure(x, row.names = c(NA, length(x[[1]])), class = "data.frame")
}
