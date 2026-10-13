# Run from the repository in RStudio after editing these settings.
# This creates RDF as JSON-LD containing the pathway and sample-level values.
mztab_file <- NULL                    # NULL opens file.choose() in RStudio
network_file <- file.path("inst", "extdata", "examples", "remodeling_lipidonly.cyjs")
output_directory <- file.path(getwd(), "pathway_measurement_output")
acyl_chains <- c("18:0", "20:4", "22:6", "24:0", "24:1")
exclude_sample_classes <- character() # Keep all samples unless explicitly excluded
normalization <- "none"              # Describe the actual input; no normalization is applied here
unit <- "unspecified"

source(file.path("R", "package_paths.R"))
source(file.path("R", "mztab_m_import.R"))
source(file.path("R", "lipid_chain_parser_se.R"))
source(file.path("R", "pathway_knowledge.R"))
source(file.path("R", "pathway_measurements.R"))
if (is.null(mztab_file)) {
  if (!interactive()) stop("Set mztab_file before non-interactive execution.")
  mztab_file <- file.choose()
}
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
dataset_id <- tools::file_path_sans_ext(basename(mztab_file))
output_file <- file.path(output_directory, paste0(dataset_id, "-pathway-measurements.json"))
result <- mztab_pathway_measurements(
  mztab_file, network_file, output_file = output_file,
  acyl_chains = acyl_chains, exclude_sample_classes = exclude_sample_classes,
  normalization = normalization, unit = unit, dataset_id = dataset_id
)
utils::write.csv(result$node_mapping, file.path(output_directory, paste0(dataset_id, "-node-mapping.csv")), row.names = FALSE, na = "")
utils::write.csv(result$acyl_coverage, file.path(output_directory, paste0(dataset_id, "-acyl-coverage.csv")), row.names = FALSE)
message("Saved pathway and sample measurements: ", normalizePath(output_file, winslash = "/"))
