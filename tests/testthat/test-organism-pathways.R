test_that("prebuilt organism coverage is explicit and isolated", {
  catalog <- list_organism_pathway_datasets()
  expect_equal(catalog$n_reactions, c(6L, 6L, 6L, 4L, 1L))
  mouse <- read_organism_pathway("10090")
  expect_equal(nrow(mouse$classes), 5L)
  expect_true(all(grepl("^GP[0-9]{4}$", mouse$classes$lipidmaps_class_id)))
  expect_equal(nrow(search_organism_pathway("GP0201", "10090")$classes), 3L)
  expect_equal(length(unique(mouse$classes$uri[mouse$classes$lipidmaps_class_id == "GP0201"])), 3L)
  expect_false(any(grepl("reaction_pe_to_pc$", mouse$reactions$uri)))
  expect_equal(nrow(search_organism_pathway("PC", "10090")$reactions), 2L)
  expect_equal(nrow(search_organism_pathway("PC", "83333")$reactions), 0L)
  expect_error(read_organism_pathway("../../9606"), "Unknown")
  expect_error(list_organism_pathway_datasets(".."), "Invalid")
  expect_error(search_organism_pathway("", "9606"), "non-empty")
})

test_that("measurements map to organism RDF without cyjs and preserve missing values", {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(c(10, 5, 3, NA, NA, 7), 3, 2,
      dimnames = list(c("L1", "L2", "L3"), c("S1", "S2")))),
    rowData = S4Vectors::DataFrame(subclass = c("PC", "PC", "TG"),
      acyl_chains = I(list(character(), character(), character()))),
    colData = S4Vectors::DataFrame(class = c("A", "B")))
  path <- tempfile(fileext = ".jsonld")
  result <- organism_pathway_measurements(se, "10090", path, dataset_id = "test-mouse")
  expect_true(file.exists(path))
  expect_equal(result$knowledge_manifest$taxon_id, "10090")
  graph <- result$document[["@graph"]]
  types <- function(type) Filter(function(n) identical(n[["@type"]], paste0("mslm:", type)), graph)
  expect_length(types("PathwayConnection"), 0L)
  expect_length(types("MolecularFeature"), 3L)
  scopes <- types("LipidMeasurementScope")
  pc_entity <- Filter(function(n) identical(n$msdialOntologyLabel, "PC"), types("MSDialMappableLipidEntity"))[[1]]
  pc <- Filter(function(n) identical(n$observedLipidSubclass[["@id"]], pc_entity[["@id"]]), scopes)[[1]]
  values <- Filter(function(n) identical(n$measuredEntity[["@id"]], pc[["@id"]]), types("SampleMeasurement"))
  expect_equal(values[[1]]$value, 15)
  expect_true(is.na(values[[2]]$value))
  expect_length(Filter(function(n) "https://systemomicslab.github.io/MSLipidMapper/ontology/ClassReaction" %in% unlist(n[["@type"]]), graph), 6L)
  expect_false("TG" %in% result$node_mapping$subclass)
  subclass <- Filter(function(n) identical(n$msdialOntologyLabel, "PC"), types("MSDialMappableLipidEntity"))[[1]]
  expect_match(subclass$canonicalEntity[["@id"]], "/MSLMC[0-9]{6,}$")
  expect_equal(subclass$lipidMapsSubClassCode, "GP0101")
  expect_true(all(vapply(graph, function(n) {
    id <- n[["@id"]]
    !startsWith(id, "https://systemomicslab.github.io/MSLipidMapper/resource/") ||
      grepl("/(MSLM[A-Z][0-9]{6,}|lipidmaps-classification/GP[0-9]{4})$", id)
  }, logical(1))))
  expect_error(organism_pathway_measurements(se, "9606", network = list()), "fixed")
})
