# ============================================================================
# decomposer: zzz.R
#
# Package-level documentation and .onLoad
# ============================================================================

#' @importFrom data.table data.table rbindlist setcolorder copy
NULL

.onLoad <- function(libname, pkgname) {
  # Ensure data.table is initialized in the package namespace
  invisible()
}
