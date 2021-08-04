
proj_xptr_addr <- function(x) {
  .Call(proj_c_xptr_addr, x)
}

assert_chr1 <- function(x, arg_name = "x") {
  if (!is.character(x) || (length(x) != 1)) {
    stop(sprintf("`%s` must be character of length 1"))
  }

  x
}
