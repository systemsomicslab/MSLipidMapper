#!/usr/bin/env Rscript

script_path_from_args <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_arg)) {
    return("")
  }
  sub("^--file=", "", file_arg[1])
}

resolve_app_path <- function() {
  candidates <- character(0)
  script_path <- script_path_from_args()
  if (nzchar(script_path)) {
    script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
    root_dir <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
    candidates <- c(candidates, file.path(root_dir, "R", "test_class_specificity_app.R"))
  }
  candidates <- c(candidates, file.path(getwd(), "R", "test_class_specificity_app.R"))
  candidates <- unique(candidates)
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("Could not locate R/test_class_specificity_app.R", call. = FALSE)
  }
  hit[1]
}

source(resolve_app_path(), chdir = TRUE)
run_test_class_specificity_app()
