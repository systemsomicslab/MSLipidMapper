# Synthetic example; no experimental or patient data.
source("R/package_paths.R")
source("R/pathway_knowledge.R")
source("R/pathway_measurements.R")
mat <- rbind(c(10, 12, 20, 22), c(5, NA, 0, 8), c(7, 9, 11, 13))
dimnames(mat) <- list(c("L001", "L002", "L003"), paste0("S", 1:4))
se <- SummarizedExperiment::SummarizedExperiment(
  assays = list(abundance = mat),
  rowData = S4Vectors::DataFrame(
    subclass = c("PC", "PC", "PE"),
    `Metabolite name` = c("PC 16:0_24:1", "PC 18:0_18:1", "PE 18:0_24:1"),
    acyl_chains = I(list(c("16:0", "24:1"), c("18:0", "18:1"), c("18:0", "24:1"))),
    check.names = FALSE),
  colData = S4Vectors::DataFrame(class = c("Control", "Control", "Treatment", "Treatment")))
out <- mztab_pathway_measurements(se,
  "inst/extdata/examples/remodeling_lipidonly.cyjs",
  "inst/extdata/examples/pathway-measurements-example.json",
  acyl_chains = c("18:0", "24:1"), dataset_id = "synthetic_example",
  normalization = "none", unit = "arbitrary")
cat("Synthetic example:", length(Filter(function(n) identical(n[["@type"]], "mslm:Sample"), out$document[["@graph"]])), "samples;",
    sum(out$node_mapping$status == "measured"), "measured nodes\n")
