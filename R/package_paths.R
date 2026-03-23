# Package asset helpers ---------------------------------------------------

#' Get bundled example data path
#'
#' @return Absolute path to the bundled example directory.
#' @export
mslipidmapper_example_path <- function() {
  p <- system.file("extdata", "examples", package = "MSLipidMapper")
  if (!nzchar(p)) stop("Bundled example data not found.", call. = FALSE)
  p
}

#' Get bundled lipid rules YAML path
#'
#' @return Absolute path to bundled lipid rules YAML.
#' @export
mslipidmapper_rules_path <- function() {
  p <- system.file("extdata", "lipid_rules.yaml", package = "MSLipidMapper")
  if (!nzchar(p)) stop("Bundled lipid rules YAML not found.", call. = FALSE)
  p
}

.mslm_app_www_dir <- function() {
  p <- system.file("app", "www", package = "MSLipidMapper")
  if (nzchar(p)) return(p)

  fallback <- file.path(getwd(), "www")
  if (dir.exists(fallback)) {
    return(normalizePath(fallback, winslash = "/", mustWork = FALSE))
  }

  ""
}

.mslm_rules_yaml_path <- function(path = NULL) {
  if (!is.null(path) && nzchar(path) && file.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }

  pkg_path <- system.file("extdata", "lipid_rules.yaml", package = "MSLipidMapper")
  if (nzchar(pkg_path)) return(pkg_path)

  fallback <- file.path(getwd(), "R", "lipid_rules.yaml")
  if (file.exists(fallback)) {
    return(normalizePath(fallback, winslash = "/", mustWork = FALSE))
  }

  NULL
}
