test_that("junctions become reaction nodes and chained junctions merge", {
  net <- list(elements = list(nodes = list(
    list(data = list(id = "pc", label = "PC")), list(data = list(id = "ps", label = "PS")),
    list(data = list(id = "ser", label = "Serine")), list(data = list(id = "lpc", label = "LPC")),
    list(data = list(id = "j1", label = "Node1")), list(data = list(id = "j2", label = "")),
    list(data = list(id = "dead", label = "983"))),
    edges = list(
      list(data = list(source = "pc", target = "j1")), list(data = list(source = "ser", target = "j1")),
      list(data = list(source = "j1", target = "j2")), list(data = list(source = "j2", target = "ps")),
      list(data = list(source = "pc", target = "lpc")), list(data = list(source = "ps", target = "dead")))))
  parsed <- .mslm_network_reactions(net)
  expect_setequal(parsed$entities, c("PC", "PS", "Serine", "LPC"))
  keys <- vapply(parsed$reactions, function(r) paste(paste(sort(r$substrates), collapse = "+"),
                                                    paste(r$products, collapse = "+"), sep = ">"), character(1))
  # The dead-end group node has no product, so it yields no reaction.
  expect_setequal(keys, c("PC>LPC", "PC+Serine>PS"))
})

test_that("LLM summary is compact, aligned and annotates known reactions", {
  mat <- rbind(c(10, 20, NA, 4), c(5, 0, NA, 6), c(7, 8, 9, 10))
  dimnames(mat) <- list(c("L1", "L2", "L3"), c("S1", "S2", "S3", "S4"))
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = mat),
    rowData = S4Vectors::DataFrame(subclass = c("PC", "PC", "Cer_NS"),
      acyl_chains = I(list(c("16:0", "18:1"), c("18:0", "18:1"), "16:0"))),
    colData = S4Vectors::DataFrame(class = c("A", "A", "B", "B")))
  net <- list(elements = list(nodes = list(
    list(data = list(id = "pc", label = "PC")), list(data = list(id = "ps", label = "PS"))),
    edges = list(list(data = list(source = "pc", target = "ps")))))
  path <- tempfile(fileext = ".cyjs")
  jsonlite::write_json(net, path, auto_unbox = TRUE)
  out <- tempfile(fileext = ".json")
  s <- .mslm_llm_summary(list(se = se, groups = c("A", "A", "B", "B")), c(toy = path), out,
                         include_subsets = TRUE, acyl_chains = c("18:1"), include_features = TRUE)
  doc <- jsonlite::read_json(out)
  expect_identical(doc$format, "mslipidmapper-llm-summary/1")
  expect_equal(unlist(doc$samples$ids), c("S1", "S2", "S3", "S4"))
  pc <- doc$lipids$PC
  expect_equal(vapply(pc$values, function(v) if (is.null(v)) NA_real_ else v, numeric(1)), c(15, 20, NA, 10))
  expect_equal(pc$groups$A$mean, 17.5)
  expect_equal(pc$groups$B$n, 1L)
  expect_equal(pc$n_features, 2L)
  expect_equal(unlist(pc$pathways), "toy")
  expect_equal(pc$acyl_subsets[["18:1"]]$n_features, 2L)
  expect_identical(doc$lipids$PS$status, "not_measured")
  expect_equal(unlist(doc$unmapped_classes), "Cer_NS")
  expect_length(doc$reactions, 1L)
  r <- doc$reactions[[1]]
  expect_equal(unlist(r$substrates), "PC")
  expect_equal(unlist(r$products), "PS")
  # PC -> PS is in the bundled reaction annotation table.
  expect_equal(unlist(r$enzymes[[1]]$genes), "PTDSS1")
  expect_equal(unlist(r$cosubstrates), "L-serine")
  expect_length(doc$features, 3L)
  # No URIs: the summary is meant to be read directly.
  expect_false(grepl("https?://", paste(readLines(out, warn = FALSE), collapse = "")))
})

test_that("bundled pathways yield annotated reactions", {
  parsed <- lapply(.mslm_api_networks(), function(p) .mslm_network_reactions(jsonlite::fromJSON(p, simplifyVector = FALSE)))
  all <- unlist(lapply(parsed, `[[`, "reactions"), recursive = FALSE)
  expect_gt(length(all), 50L)
  ann <- .mslm_reaction_annotations()
  hit <- vapply(all, function(r) !is.null(.mslm_annotate_reaction(r, ann)$enzymes), logical(1))
  expect_true(any(hit))
})

test_that("API accepts output format lists including llm", {
  parse <- function(x) .mslm_api_config(charToRaw(x), "csv")
  expect_identical(.mslm_api_formats(parse("output:\n  format: [pdf, llm]")$output$format), c("pdf", "llm"))
  expect_identical(.mslm_api_formats("both"), c("pdf", "jsonld"))
  expect_error(parse("output:\n  format: [both, llm]"), "output.format")
  expect_error(parse("output:\n  llm_features: maybe"), "llm_features")
})
