
#' PROJ Context
#'
#' The PROJ context object wraps global options. The default context is that
#' configured via [libproj::libproj_configure()] at package load and is not
#' affected by subsequent calls to [libproj::libproj_configure()].
#'
#' @param ctx A [proj_context()]
#' @inheritParams libproj::libproj_configuration
#'
#' @return An external pointer to a PROJ context.
#' @export
#'
#' @examples
#' proj_context()
#'
proj_context <- function() {
  new_ctx <- .Call(proj_c_pj_default_ctx)
  attr(new_ctx, "config") <- libproj::libproj_configuration()
  new_ctx
}

#' @rdname proj_context
#' @export
proj_context_create <- function(search_path = NULL, db_path = NULL, ca_bundle_path = NULL,
                                network_endpoint = NULL, network_enabled = NULL,
                                ctx = NULL) {
  if (is.null(ctx)) {
    ctx <- .Call(proj_c_pj_default_ctx)
  }

  config <- list(
    search_path = search_path,
    db_path = db_path,
    ca_bundle_path = ca_bundle_path,
    network_endpoint = network_endpoint,
    network_enabled = network_enabled
  )

  libproj_config <- libproj::libproj_configuration()
  config <- config[!vapply(config, is.null, logical(1))]
  config <- c(config, libproj_config)[names(libproj_config)]
  new_ctx <- libproj::with_libproj_configuration(config, proj_context_clone(ctx))

  # not all config values can be retrieved, so save them as an attribute
  attr(new_ctx, "config") <- config

  new_ctx
}

#' @rdname proj_context
#' @export
proj_context_clone <- function(ctx = proj_context()) {
  .Call(proj_c_context_clone, ctx)
}

#' @rdname proj_context
#' @export
proj_context_is_network_enabled <- function(ctx = proj_context()) {
  .Call(proj_c_context_is_network_enabled, ctx)
}

#' @rdname proj_context
#' @export
proj_context_get_url_endpoint <- function(ctx = proj_context()) {
  .Call(proj_c_context_get_url_endpoint, ctx)
}

#' @rdname proj_context
#' @export
proj_context_get_use_proj4_init_rules <- function(ctx = proj_context()) {
  .Call(proj_c_context_get_use_proj4_init_rules, ctx)
}

#' @rdname proj_context
#' @export
proj_context_get_user_writable_directory <- function(ctx = proj_context()) {
  .Call(proj_c_context_get_user_writable_directory, ctx)
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
