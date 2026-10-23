# Synthetic example: source selected files so this also works before reinstalling.
options(MSLipidMapper.id_registry = file.path("inst", "extdata", "pathway_knowledge", "source", "example-id-registry.sqlite"))
for (file in c("resource_ids.R", "pathway_knowledge.R", "pathway_measurements.R", "organism_pathways.R")) {
  source(file.path("R", file))
}
se <- SummarizedExperiment::SummarizedExperiment(
  assays = list(abundance = matrix(c(10, 5, 3, NA, NA, 7), 3, 2,
    dimnames = list(c("PC-a", "PC-b", "TG-a"), c("S1", "S2")))),
  rowData = S4Vectors::DataFrame(subclass = c("PC", "PC", "TG"),
    acyl_chains = I(list(character(), character(), character()))),
  colData = S4Vectors::DataFrame(class = c("A", "B")))
output <- file.path("inst", "extdata", "examples", "organism-pathway-measurements-example.jsonld")
result <- organism_pathway_measurements(se, "10090", output,
  dataset_id = "synthetic-mouse-example", normalization = "none", unit = "arbitrary")
print(result$node_mapping)
cat("Synthetic example written to", output, "\n")
