test_that("Generic upload accepts ID-only annotations and preserves feature alignment", {
  assay <- data.frame(sample_id = c("s1", "s2"), class = c("A", "B"),
    lipid_a = c(10, 20), lipid_b = c(30, 40), lipid_c = c(50, 60))
  features <- data.frame(name = c("lipid_b", "lipid_a", "lipid_c"),
    id = c("LMGP03010001", "LMGP01010001", NA))
  build <- function(...) .build_lipidomics_se_generic_cols(assay, features,
    "sample_id", "class", "name", ...)
  se <- build(lipidmaps_col = "id")
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  expect_equal(rd$LIPIDMAPS_ID, c("LMGP01010001", "LMGP03010001", NA))
  expect_equal(rd$subclass, c("PC", "PS", "Unknown"))
  expect_true(all(is.na(rd$Ontology)))
  expect_equal(unname(SummarizedExperiment::assay(se)), rbind(c(10, 20), c(30, 40), c(50, 60)))
  expect_error(build(), "LIPID MAPS ID column or an Ontology")
  expect_error(build(lipidmaps_col = "missing"), "Annotation column not found")
  expect_error(build(ontology_col = "id", lipidmaps_col = "id"), "different columns")
})

test_that("Generic upload retains ontology priority and legacy ontology-only input", {
  assay <- data.frame(sample_id = "s1", a = 10, b = 20)
  features <- data.frame(name = c("a", "b"), Ontology = c("PS", ""),
    id = c("LMGP01010001", "LMGP01010001"))
  se <- .build_lipidomics_se_generic_cols(assay, features, "sample_id",
    feature_name_col = "name", ontology_col = "Ontology", lipidmaps_col = "id")
  expect_equal(as.character(SummarizedExperiment::rowData(se)$subclass), c("PS", "PC"))
  legacy <- .build_lipidomics_se_generic_cols(assay, features, "sample_id",
    feature_name_col = "name", ontology_col = "Ontology")
  expect_equal(as.character(SummarizedExperiment::rowData(legacy)$subclass), c("PS", "Unknown"))
})

test_that("Generic submit accepts an empty Ontology selector", {
  assay_path <- tempfile(fileext = ".csv")
  feature_path <- tempfile(fileext = ".csv")
  on.exit(unlink(c(assay_path, feature_path)))
  utils::write.csv(data.frame(sample_id = "s1", lipid_a = 10), assay_path, row.names = FALSE)
  utils::write.csv(data.frame(feature_id = "lipid_a", label = "PC 16:0_18:1", LIPIDMAPS_ID = "LMGP01010001"), feature_path, row.names = FALSE)
  shiny::testServer(mod_upload_server, {
    session$setInputs(lipid_format = "generic",
      generic_assay_file = list(datapath = assay_path),
      generic_feature_file = list(datapath = feature_path),
      generic_sample_id_colname = "sample_id", generic_sample_class_colname = "",
      generic_feature_name_colname = "feature_id", generic_label_colname = "label", generic_ontology_colname = "",
      generic_lipidmaps_colname = "LIPIDMAPS_ID")
    html <- output$generic_feature_col_ui$html
    expect_match(html, 'value="LIPIDMAPS_ID" selected', fixed = TRUE)
    expect_match(html, 'value="label" selected', fixed = TRUE)
    session$setInputs(build_generic = 1)
    session$flushReact()
    se <- session$returned$se_all()
    expect_s4_class(se, "SummarizedExperiment")
    expect_equal(as.character(SummarizedExperiment::rowData(se)$LIPIDMAPS_ID), "LMGP01010001")
    expect_equal(as.character(SummarizedExperiment::rowData(se)[["Metabolite name"]]), "PC 16:0_18:1")
    expect_equal(rownames(se), "lipid_a")
  })
})

test_that("display labels preserve stable keys and must cover every assay feature", {
  assay <- data.frame(sample_id = "s1", f1 = 10, f2 = 20)
  features <- data.frame(feature_id = c("f2", "f1"),
    label = c("PC 16:0_18:1", "PC 16:0_18:1"), LM_ID = "LMGP01010001")
  build <- function(df = features, ...) .build_lipidomics_se_generic_cols(assay, df,
    "sample_id", feature_name_col = "feature_id", lipidmaps_col = "LM_ID", label_col = "label", ...)
  se <- build()
  expect_equal(rownames(se), c("f1", "f2"))
  expect_equal(as.character(SummarizedExperiment::rowData(se)$label), rep("PC 16:0_18:1", 2))
  expect_equal(as.numeric(SummarizedExperiment::assay(se)), c(10, 20))
  expect_error(build(features[1, ]), "non-empty display label")
  features$label[1] <- " "
  expect_error(build(features), "non-empty display label")
  features$feature_id <- c("f1", "f1")
  expect_error(build(features), "keys must be non-empty and unique")
})
