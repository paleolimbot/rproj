
.onLoad <- function(...) {
  # load the libproj function pointers here
  requireNamespace("libproj", quietly = TRUE)
  .Call(proj_c_init)

  # assign the default context
  proj_context_env$ctx <- proj_context_create()
}
