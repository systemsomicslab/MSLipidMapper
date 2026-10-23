test_that("pairwise subclass and acyl-subset statistics map to RDF", {
  skip_if_not_installed("SummarizedExperiment")
  mat <- matrix(c(
    10, 12, 20, 22, 30, 32,
     5,  6, 10, 11, 15, 16,
     8,  9,  8,  9,  8,  9
  ), nrow = 3, byrow = TRUE,
  dimnames = list(c("PC 16:0_24:1", "PE 18:0_24:1", "PC 16:0_18:1"), paste0("S", 1:6)))
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = mat),
    rowData = S4Vectors::DataFrame(
      subclass = c("PC", "PE", "PC"),
      `Metabolite name` = rownames(mat), check.names = FALSE
    ),
    colData = S4Vectors::DataFrame(
      class = rep(c("A", "B", "C"), each = 2), row.names = colnames(mat)
    )
  )
  out <- mztab_pairwise_subclass_rdf(
    se, acyl_chains = "24:1", test = "welch_t", dataset_id = "unit_test",
    exclude_sample_classes = character()
  )
  expect_equal(nrow(unique(out$statistics[c("group_a", "group_b")])), 3L)
  expect_true(all(c("subclass", "acyl_subset") %in% out$statistics$scope))
  expect_equal(out$acyl_coverage$n_features, 2L)
  pc24 <- subset(out$statistics,
    group_a == "A" & group_b == "B" & subclass == "PC" & acyl_chain == "24:1")
  expect_equal(pc24$n_molecular_species, 1L)
  expect_equal(pc24$mean_a, 11)
  expect_equal(pc24$mean_b, 21)
  expect_identical(pc24$mapping_status, "mapped")
  expect_match(out$turtle, "mslm:acylChainSignature \"24:1\"", fixed = TRUE)
  expect_match(out$turtle, "mslm:LipidSubclassObservation", fixed = TRUE)
  expect_match(out$turtle, "mslm:classLabel \"PC\"", fixed = TRUE)
  expect_match(out$turtle, "mslm:lipidMapsSubClassCode \"GP0101\"", fixed = TRUE)
  ids <- regmatches(out$turtle, gregexpr("https://systemomicslab.github.io/MSLipidMapper/resource/[^>]+", out$turtle))[[1]]
  expect_true(all(grepl("/MSLM[A-Z][0-9]{6,}$", ids)))
})
