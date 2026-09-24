# MSLipidMapper: mzTab-M pairwise metabolic-state pilot
#
# RStudio usage:
#   1. Open the MSLipidMapper RStudio project/repository.
#   2. Edit the settings below. Leave mztab_file as NULL to select a file.
#   3. Source this entire script.

# ---- Settings -----------------------------------------------------------
mztab_file <- NULL
output_directory <- file.path(getwd(), "metabolic_state_output")
sample_class_column <- "class"
lipid_subclass_column <- "subclass"
exclude_sample_classes <- c("Blank", "QC")
acyl_chains_to_test <- c("18:0", "20:4", "22:6", "24:0", "24:1")
subclass_aggregation <- "sum"       # "sum", "mean", or "median"
two_group_test <- "welch_t"         # "welch_t" or "wilcox"
p_adjust_method <- "BH"
log2fc_pseudocount <- 1

# ---- Load only the code needed by this pilot ----------------------------
# This intentionally does not require installing MSLipidMapper as a package.
required_packages <- c(
  "SummarizedExperiment", "S4Vectors", "stringr", "dplyr", "tidyr",
  "yaml", "rlang", "purrr"
)
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall Bioconductor packages with BiocManager and CRAN packages with install.packages().",
    call. = FALSE
  )
}

source(file.path(getwd(), "R", "package_paths.R"), local = FALSE)
source(file.path(getwd(), "R", "mztab_m_import.R"), local = FALSE)
source(file.path(getwd(), "R", "lipid_chain_parser_se.R"), local = FALSE)
source(file.path(getwd(), "R", "pathway_knowledge.R"), local = FALSE)
source(file.path(getwd(), "R", "mztab_metabolic_state.R"), local = FALSE)

# ---- Select input -------------------------------------------------------
if (is.null(mztab_file) || !nzchar(mztab_file)) {
  if (!interactive()) {
    stop("Set mztab_file when running non-interactively.", call. = FALSE)
  }
  mztab_file <- file.choose()
}
mztab_file <- normalizePath(mztab_file, winslash = "/", mustWork = TRUE)
if (!grepl("\\.(mztab|mztab-m|txt|tsv)$", mztab_file, ignore.case = TRUE)) {
  warning("The selected file does not have a usual mzTab-M extension.", call. = FALSE)
}

# ---- Run all pairwise comparisons --------------------------------------
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
dataset_id <- tools::file_path_sans_ext(basename(mztab_file))
rdf_file <- file.path(output_directory, paste0(dataset_id, "-metabolic-state.ttl"))

state <- mztab_pairwise_subclass_rdf(
  x = mztab_file,
  group_col = sample_class_column,
  subclass_col = lipid_subclass_column,
  exclude_sample_classes = exclude_sample_classes,
  acyl_chains = acyl_chains_to_test,
  aggregate = subclass_aggregation,
  test = two_group_test,
  pseudocount = log2fc_pseudocount,
  p_adjust_method = p_adjust_method,
  dataset_id = dataset_id,
  output_file = rdf_file
)

# ---- Save tabular views -------------------------------------------------
statistics_file <- file.path(output_directory, paste0(dataset_id, "-statistics.csv"))
coverage_file <- file.path(output_directory, paste0(dataset_id, "-acyl-coverage.csv"))
utils::write.csv(state$statistics, statistics_file, row.names = FALSE, na = "")
utils::write.csv(state$acyl_coverage, coverage_file, row.names = FALSE, na = "")

message("Completed mzTab-M metabolic-state mapping")
message("  Input:      ", mztab_file)
message("  Statistics: ", normalizePath(statistics_file, winslash = "/", mustWork = TRUE))
message("  Coverage:   ", normalizePath(coverage_file, winslash = "/", mustWork = TRUE))
message("  RDF:        ", normalizePath(rdf_file, winslash = "/", mustWork = TRUE))
message("  Comparisons: ", nrow(unique(state$statistics[c("group_a", "group_b")])))

# Show the result in RStudio's data viewer.
if (interactive()) View(state$statistics)
