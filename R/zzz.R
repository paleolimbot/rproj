
.onLoad <- function(...) {
  requireNamespace("libproj", quietly = TRUE)
  .Call(proj_c_init)
}
