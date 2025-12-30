# ======================================================================
# Cache helpers
# ======================================================================

#' @export
get_cache_root <- local({
  cache_dir <- NULL
  function(create = TRUE) {
    if (is.null(cache_dir)) {
      base <- tryCatch(
        tools::R_user_dir("MSLipidMapper", which = "cache"),
        error = function(e) file.path(tempdir(), "mslipidmapper-cache")
      )
      cache_dir <<- base
    }
    if (isTRUE(create)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  }
})

#' @export
set_cache_root <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, nchar(path) > 0)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  env <- environment(get_cache_root)
  env$cache_dir <- normalizePath(path, winslash = "/", mustWork = FALSE)
  invisible(env$cache_dir)
}

#' @export
get_cache_subdir <- function(name, create = TRUE) {
  d <- file.path(get_cache_root(create = create), name)
  if (isTRUE(create)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, winslash = "/", mustWork = FALSE)
}
