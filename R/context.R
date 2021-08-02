
#' PROJ Context
#'
#' The PROJ context object wraps global options. Currently only the default
#' context configured via [libproj::libproj_configure()] is supported.
#'
#' @param context An external pointer to a [proj_context()]
#'
#' @return An external pointer to a PROJ context.
#' @export
#'
#' @examples
#' proj_context()
#'
proj_context <- function() {
  new_proj_context(.Call(proj_c_pj_default_ctx))
}

#' @rdname proj_context
#' @export
proj_context_is_network_enabled <- function(context = proj_context()) {
  .Call(proj_c_context_is_network_enabled, context)
}

#' @rdname proj_context
#' @export
proj_context_get_url_endpoint <- function(context = proj_context()) {
  .Call(proj_c_context_get_url_endpoint, context)
}

#' @rdname proj_context
#' @export
proj_context_get_use_proj4_init_rules <- function(context = proj_context()) {
  .Call(proj_c_context_get_use_proj4_init_rules, context)
}

#' @rdname proj_context
#' @export
proj_context_get_user_writable_directory <- function(context = proj_context()) {
  .Call(proj_c_context_get_user_writable_directory, context)
}

new_proj_context <- function(x) {
  structure(x, class = "rlibproj_context")
}

#' @export
format.rlibproj_context <- function(x, ...) {
  sprintf("<proj_context at %s>", proj_xptr_addr(x))
}

#' @export
print.rlibproj_context <- function(x, ...) {
  cat(sprintf("<proj_context at %s>\n", proj_xptr_addr(x)))
  cat(sprintf("* User writable directory: <%s>\n", proj_context_get_user_writable_directory(x)))

  if (proj_context_is_network_enabled(x)) {
    cat(sprintf("* Network access enabled from <%s>\n", proj_context_get_url_endpoint(x)))
  }

  if (proj_context_get_use_proj4_init_rules(x)) {
    cat("* Using PROJ4 '+init' rules\n")
  }

  invisible(x)
}
