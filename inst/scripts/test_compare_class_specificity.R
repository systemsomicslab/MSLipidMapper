#!/usr/bin/env Rscript

parse_args_simple <- function(args) {
  out <- list()
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (!startsWith(arg, "--")) {
      i <- i + 1L
      next
    }

    keyval <- substring(arg, 3L)
    if (grepl("=", keyval, fixed = TRUE)) {
      parts <- strsplit(keyval, "=", fixed = TRUE)[[1]]
      key <- parts[1]
      value <- paste(parts[-1], collapse = "=")
      out[[key]] <- value
      i <- i + 1L
      next
    }

    key <- keyval
    if ((i < length(args)) && !startsWith(args[[i + 1L]], "--")) {
      out[[key]] <- args[[i + 1L]]
      i <- i + 2L
    } else {
      out[[key]] <- "TRUE"
      i <- i + 1L
    }
  }
  out
}

script_path_from_args <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_arg)) {
    return("")
  }
  sub("^--file=", "", file_arg[1])
}

resolve_util_path <- function() {
  candidates <- character(0)
  script_path <- script_path_from_args()
  if (nzchar(script_path)) {
    script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
    root_dir <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
    candidates <- c(candidates, file.path(root_dir, "R", "test_class_specificity_metrics.R"))
  }
  candidates <- c(
    candidates,
    file.path(getwd(), "R", "test_class_specificity_metrics.R")
  )
  candidates <- unique(candidates)
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("Could not locate R/test_class_specificity_metrics.R", call. = FALSE)
  }
  hit[1]
}

args <- parse_args_simple(commandArgs(trailingOnly = TRUE))

if (is.null(args$input) || !nzchar(args$input)) {
  stop("Usage: Rscript inst/scripts/test_compare_class_specificity.R --input path/to/se.rds [--output_dir dir] [--assay_name abundance] [--class_col LipidClass] [--target_class PC] [--group_col Group] [--pseudocount 1] [--annotate_acyl TRUE] [--lipid_col Metabolite.name] [--rules_yaml path]", call. = FALSE)
}

if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
  stop("Package 'SummarizedExperiment' is required.", call. = FALSE)
}

source(resolve_util_path(), chdir = TRUE)

input_path <- normalizePath(args$input, winslash = "/", mustWork = TRUE)
output_dir <- if (!is.null(args$output_dir) && nzchar(args$output_dir)) {
  args$output_dir
} else {
  file.path(getwd(), "class_specificity_test_output")
}

se <- readRDS(input_path)
result <- compute_test_class_specificity_metrics(
  se = se,
  assay_name = if (!is.null(args$assay_name)) args$assay_name else NULL,
  class_col = if (!is.null(args$class_col)) args$class_col else "LipidClass",
  target_class = if (!is.null(args$target_class)) args$target_class else NULL,
  group_col = if (!is.null(args$group_col)) args$group_col else NULL,
  pseudocount = if (!is.null(args$pseudocount)) as.numeric(args$pseudocount) else 1,
  annotate_acyl = .tcs_as_flag(if (!is.null(args$annotate_acyl)) args$annotate_acyl else FALSE),
  lipid_col = if (!is.null(args$lipid_col)) args$lipid_col else "Metabolite.name",
  rules_yaml = if (!is.null(args$rules_yaml)) args$rules_yaml else NULL,
  verbose = TRUE
)

write_test_class_specificity_outputs(result, output_dir = output_dir)
message("Wrote outputs to: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
