
.onLoad <- function(...) {
  # load the libproj function pointers here
  requireNamespace("libproj", quietly = TRUE)
  .Call(proj_c_init)

  # try to assign the default context
  proj_context_env$ctx <- try(proj_context_create())
}
