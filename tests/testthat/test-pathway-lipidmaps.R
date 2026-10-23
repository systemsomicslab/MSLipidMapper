test_that("LIPID MAPS fills missing classes without replacing ontology", {
  rd <- data.frame(Ontology = c("PS", "", NA, "Unknown", "custom", ""),
    lipidmaps_id = c("LMGP01010001", "LMGP01010001", "GP0301",
      "https://www.lipidmaps.org/databases/lmsd/LMGP01010001", "GP0101", "invalid"))
  expect_equal(.mslm_pathway_feature_classes(rd), c("PS", "PC", "PS", "PC", "custom", "Unknown"))
  ambiguous <- data.frame(label = c("PE", "PMME"), lipidmaps_class_id = c("GP0201", "GP0201"))
  expect_equal(.mslm_pathway_feature_classes(data.frame(LM_ID = "LMGP02010001"), mapping = ambiguous), "Unknown")
})

test_that("ID-only measurements map and ontology wins for mixed inputs", {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(c(10, 20, 30), 3, 1,
      dimnames = list(c("a", "b", "c"), "sample"))),
    rowData = S4Vectors::DataFrame(Ontology = c("", "PS", NA),
      LM_ID = c("LMGP01010001", "LMGP01010001", "invalid")),
    colData = S4Vectors::DataFrame(class = "A"))
  net <- list(elements = list(nodes = list(list(data = list(id = "pc", label = "PC")),
    list(data = list(id = "ps", label = "PS")))), edges = list())
  out <- mztab_pathway_measurements(se, net)
  expect_equal(out$node_mapping$status, c("measured", "measured"))
  features <- Filter(function(n) identical(n[["@type"]], "mslm:MolecularFeature"), out$document[["@graph"]])
  expect_length(features, 3L)
  SummarizedExperiment::rowData(se)$Ontology <- NULL
  out <- mztab_pathway_measurements(se, net)
  expect_equal(out$node_mapping$status, c("measured", "not_measured"))
})
