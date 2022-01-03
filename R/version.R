
#' PROJ version information
#'
#' @param runtime Use FALSE to return the build-time
#'   proj version, which may be different than the runtime
#'   version if a different version of the
#'   [libproj package][libproj::libproj_version] was used to build
#'   this package.
#' @export
#'
#' @examples
#' proj_version()
#' proj_version(runtime = FALSE)
#'
#' # check for a minimum version of proj
#' proj_version() >= "8.1.0"
#'
proj_version <- function(runtime = TRUE) {
  version <- if (runtime) libproj::libproj_version() else .Call(rproj_c_version_build)
  package_version(version)
}
