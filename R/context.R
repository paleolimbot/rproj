
# env to keep our own version of the default context
# (because our logger is better than the default stderr
# and because we manage options via multiple contexts not by
# temporarily configuring PJ_DEFAULT_CTX)
proj_context_env <- new.env(parent = emptyenv())
proj_context_env$ctx <- NULL

#' PROJ Context
#'
#' The PROJ context object wraps global options. The default context is that
#' configured via [libproj::libproj_configure()] at package load and is not
#' affected by subsequent calls to [libproj::libproj_configure()].
#'
#' @param ctx A [proj_context()]
#' @param expr An expression to evaluate in the given context
#' @inheritParams libproj::libproj_configuration
#'
#' @return An external pointer to a PROJ context.
#' @export
#'
#' @examples
#' proj_context()
#'
proj_context <- function() {
  proj_context_env$ctx
}

#' @rdname proj_context
#' @export
proj_set_context <- function(ctx) {
  # check context via one of the C accessors
  proj_context_is_network_enabled()

  previous <- proj_context()
  proj_context_env$ctx <- ctx
  invisible(previous)
}

#' @rdname proj_context
#' @export
with_proj_context <- function(ctx, expr) {
  previous <- proj_set_context(ctx)
  on.exit(proj_set_context(previous))
  force(expr)
}

#' @rdname proj_context
#' @export
proj_context_create <- function(search_path = NULL, db_path = NULL, ca_bundle_path = NULL,
                                network_endpoint = NULL, network_enabled = NULL,
                                log_level = NULL, user_writable_directory = NULL,
                                ctx = NULL) {
  new_ctx <- .Call(rproj_c_context_create)

  config <- list(
    search_path = search_path,
    db_path = db_path,
    ca_bundle_path = ca_bundle_path,
    network_endpoint = network_endpoint,
    network_enabled = network_enabled,
    log_level = log_level,
    user_writable_directory = user_writable_directory
  )

  if (is.null(ctx)) {
    parent_config <- libproj::libproj_configuration()
  } else {
    libproj_config_attr <- libproj::libproj_configuration()
    parent_config_attr <- attr(ctx, "config", exact = TRUE)

    parent_config <- list(
      search_path = proj_context_get_search_paths(ctx),
      db_path = proj_context_get_database_path(ctx),
      ca_bundle_path = parent_config_attr$ca_bundle_path %||% libproj_config_attr$ca_bundle_path,
      network_endpoint = parent_config_attr$network_endpoint %||% libproj_config_attr$network_endpoint,
      network_enabled = proj_context_is_network_enabled(ctx),
      log_level = parent_config_attr$log_level %||% libproj_config_attr$log_level,
      user_writable_directory = proj_context_get_user_writable_directory(ctx)
    )
  }

  config <- config[!vapply(config, is.null, logical(1))]
  config <- c(config, parent_config)[names(parent_config)]

  do.call(libproj::libproj_configure, c(config, list(context = new_ctx)))

  # not all config values can be retrieved, so save them as an attribute
  attr(new_ctx, "config") <- config

  new_ctx
}

#' @rdname proj_context
#' @export
proj_context_clone <- function(ctx = proj_context()) {
  new_ctx <- .Call(rproj_c_context_clone, ctx)

  # apparently the log level doesn't survive the clone operation
  if (!is.null(attr(ctx, "config"))) {
    proj_context_set_log_level(attr(ctx, "config")$log_level, new_ctx)
  } else {
    proj_context_set_log_level(1L, new_ctx)
  }

  new_ctx
}

#' @rdname proj_context
#' @export
proj_context_set_log_level <- function(log_level = 1L, ctx = proj_context()) {
  log_level <- assert_int1(log_level)
  stopifnot(log_level >= 0, log_level <= 4)
  result <- .Call(rproj_c_context_set_log_level, ctx, log_level)
  if (!is.null(attr(ctx, "config"))) {
    attr(ctx, "config")$log_level <- log_level
  }
  invisible(result)
}

#' @rdname proj_context
#' @export
proj_context_is_network_enabled <- function(ctx = proj_context()) {
  .Call(rproj_c_context_is_network_enabled, ctx)
}

#' @rdname proj_context
#' @export
proj_context_get_url_endpoint <- function(ctx = proj_context()) {
  .Call(rproj_c_context_get_url_endpoint, ctx)
}

#' @rdname proj_context
#' @export
proj_context_get_use_proj4_init_rules <- function(ctx = proj_context()) {
  .Call(rproj_c_context_get_use_proj4_init_rules, ctx)
}

#' @rdname proj_context
#' @export
proj_context_get_user_writable_directory <- function(ctx = proj_context()) {
  .Call(rproj_c_context_get_user_writable_directory, ctx)
}

#' @rdname proj_context
#' @export
proj_context_get_search_paths <- function(ctx = proj_context()) {
  attr(ctx, "config")$search_path
}

#' @rdname proj_context
#' @export
proj_context_get_database_path <- function(ctx = proj_context()) {
  .Call(rproj_c_context_get_database_path, ctx)
}

#' @export
format.rproj_context <- function(x, ...) {
  sprintf("<proj_context at %s>", proj_xptr_addr(x))
}

#' @export
print.rproj_context <- function(x, ...) {
  cat(sprintf("<proj_context at %s>\n", proj_xptr_addr(x)))
  cat(sprintf("* db: <%s>\n", proj_context_get_database_path(x)))

  search <- proj_context_get_search_paths(x)
  if (!is.null(search)) {
    cat("* Search paths:\n")
    for (s in search) {
      cat(sprintf("  - <%s>\n", s))
    }
  }

  cat(sprintf("* User writable directory: <%s>\n", proj_context_get_user_writable_directory(x)))

  if (proj_context_is_network_enabled(x)) {
    cat(sprintf("* Network access enabled from <%s>\n", proj_context_get_url_endpoint(x)))
  }

  if (proj_context_get_use_proj4_init_rules(x)) {
    cat("* Using PROJ4 '+init' rules\n")
  }

  cat(sprintf("* Log level: %d\n", attr(x, "config")$log_level))

  invisible(x)
}
