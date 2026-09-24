test_that("pathway knowledge assets are bundled", {
  assets <- list_pathway_knowledge_assets()
  expect_true(all(c("ontology", "curated", "release", "mapping", "organism", "query", "example") %in% assets$role))
  expect_true(any(assets$path == "curated/ceramide-pilot.ttl"))
  expect_true(any(assets$path == "curated/phospholipid-pilot.ttl"))
})

test_that("MS-DIAL ontology labels map to LIPID MAPS classifications", {
  mapped <- map_msdial_ontology_to_pathway(c("PC", " PE ", "EtherPC"))
  expect_identical(mapped$matched, c(TRUE, TRUE, FALSE))
  expect_identical(mapped$lipidmaps_subclass_code[1:2], c("GP0101", "GP0201"))
  expect_identical(mapped$lipidmaps_id[1:2], c("", ""))
})

test_that("organism-specific Rhea-UniProt queries are rendered", {
  organisms <- list_pathway_organisms()
  expect_true(all(c("9606", "10090", "10116", "559292", "83333") %in% organisms$taxon_id))
  query <- render_rhea_uniprot_query("9606")
  expect_match(query, "taxon:9606", fixed = TRUE)
  expect_false(grepl("__TAXON_ID__", query, fixed = TRUE))
  expect_error(render_rhea_uniprot_query("not-configured"))
})

test_that("pathway knowledge manifest records primary sources", {
  manifest <- read_pathway_knowledge_manifest()
  ids <- vapply(manifest$sources, `[[`, character(1), "id")
  expect_true(all(c("reactome", "rhea", "msdial", "lipidmaps") %in% ids))
  expect_identical(manifest$knowledge_release, "0.1.0")
})

test_that("pathway knowledge passes lightweight integrity checks", {
  checks <- validate_pathway_knowledge(error = FALSE)
  expect_true(all(checks$ok), info = paste(checks$check[!checks$ok], collapse = ", "))
})
