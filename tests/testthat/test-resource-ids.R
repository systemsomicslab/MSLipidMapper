test_that("accessions persist in the ledger and framing avoids name collisions", {
  withr::local_options(MSLipidMapper.id_registry = tempfile(fileext = ".sqlite"))
  vectors <- jsonlite::read_json(mslipidmapper_pathway_knowledge_path("source", "identifier-test-vectors.json"))
  for (vector in vectors) {
    expect_identical(do.call(.mslm_resource_uri, vector$keys), vector$uri)
  }
  expect_false(identical(.mslm_resource_uri("test", "a/b"), .mslm_resource_uri("test", "a_b")))
  expect_false(identical(.mslm_resource_uri("test", "a", "bc"), .mslm_resource_uri("test", "ab", "c")))
  expect_match(.mslm_resource_uri("test", "name with spaces"), "/MSLMZ[0-9]{6,}$")
  expect_identical(.mslm_resource_uri("test", "PC"), vectors[[1]]$uri)
})
