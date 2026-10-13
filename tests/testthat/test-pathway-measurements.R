by_type <- function(out, type) Filter(function(n) identical(n[["@type"]], paste0("mslm:", type)), out$document[["@graph"]])

test_that("JSON-LD contains topology, samples and subset values without display data", {
  mat <- rbind(c(10, 20, NA), c(5, 0, NA), c(7, 8, 9))
  dimnames(mat) <- list(c("L1", "L2", "L3"), c("S1", "S2", "S3"))
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = mat),
    rowData = S4Vectors::DataFrame(subclass = c("PC", "PC", "PE"),
      acyl_chains = I(list(c("16:0", "24:1"), c("18:0", "24:1"), "18:0"))),
    colData = S4Vectors::DataFrame(class = c("A", "B", "B")))
  net <- list(elements = list(nodes = list(
    list(data = list(id = "pc", label = "PC", path = "http://localhost/plot.svg", SUID = 123), position = list(x = 1, y = 2)),
    list(data = list(id = "pe", label = "PE")),
    list(data = list(id = "ps", label = "PS")),
    list(data = list(id = "junction", label = "Node1"))),
    edges = list(list(data = list(id = "e1", source = "pe", target = "pc", interaction = "interacts with")))),
    style = list(list(selector = "node", style = list(shape = "ellipse"))))
  path <- tempfile(fileext = ".json")
  out <- mztab_pathway_measurements(se, net, path, acyl_chains = c("24:1", "18:0", "22:6"))
  expect_named(out$document, c("@context", "@graph"))
  expect_length(by_type(out, "Sample"), 3L)
  expect_length(by_type(out, "MolecularFeature"), 3L)
  expect_length(by_type(out, "PathwayJunction"), 1L)
  connections <- by_type(out, "PathwayConnection")
  expect_length(connections, 1L)
  expect_match(connections[[1]]$sourceEntity[["@id"]], "/pe$")
  expect_match(connections[[1]]$targetEntity[["@id"]], "/pc$")
  scopes <- by_type(out, "LipidMeasurementScope")
  pc <- Filter(function(n) grepl("/pc/scope/whole$", n[["@id"]]), scopes)[[1]]
  measurements <- by_type(out, "SampleMeasurement")
  vals <- Filter(function(n) identical(n$measuredEntity[["@id"]], pc[["@id"]]), measurements)
  expect_equal(vapply(vals, function(n) n$value, numeric(1)), c(15, 20, NA))
  expect_equal(vals[[3]]$validFeatureCount, 0L)
  expect_match(vals[[3]]$valueStatus[["@id"]], "MissingValue$")
  subset <- Filter(function(n) grepl("/pc/scope/acyl-18", n[["@id"]]), scopes)[[1]]
  expect_length(subset$memberFeature, 1L)
  subset_vals <- Filter(function(n) identical(n$measuredEntity[["@id"]], subset[["@id"]]), measurements)
  expect_equal(subset_vals[[2]]$value, 0)
  absent <- Filter(function(n) grepl("/pc/scope/acyl-22", n[["@id"]]), scopes)[[1]]
  expect_match(absent$detectionStatus[["@id"]], "NotMeasured$")
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_null(json$elements)
  text <- paste(readLines(path), collapse = "\n")
  expect_false(grepl('"(path|pathpng|position|style|SUID|zoom|selected|p_value|effect_size|log2fc)"', text))
  expect_true(grepl('"@id"', text, fixed = TRUE))
  single <- mztab_pathway_measurements(se[, 1], net, path, acyl_chains = "24:1")
  expect_length(by_type(single, "Sample"), 1L)
  expect_error(mztab_pathway_measurements(se, net, assay_name = "missing"), "assay_name")
  expect_error(mztab_pathway_measurements(se, net, node_subclasses = c(bad = "PC")), "node_subclasses")
  filtered <- mztab_pathway_measurements(se, net, aggregate = "mean", exclude_sample_classes = "A")
  expect_length(by_type(filtered, "Sample"), 2L)
  broken <- net
  broken$elements$edges[[1]]$data$target <- "missing"
  expect_error(mztab_pathway_measurements(se, broken), "endpoint")
})

test_that("sum composition is not an individual chain and bundled topology survives", {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(c(10, 20), 2, 1, dimnames = list(c("L1", "L2"), "S1"))),
    rowData = S4Vectors::DataFrame(subclass = c("PC", "PC"),
      `Metabolite name` = c("PC 34:1", "PC 16:0_18:1"), check.names = FALSE),
    colData = S4Vectors::DataFrame(class = "A"))
  root <- file.path(getwd(), "inst", "extdata", "examples")
  if (!dir.exists(root)) root <- file.path("..", "..", "inst", "extdata", "examples")
  for (name in c("remodeling_lipidonly.cyjs", "ceramidepathway_lipidonly.cyjs", "global.cyjs")) {
    net <- jsonlite::fromJSON(file.path(root, name), simplifyVector = FALSE)
    out <- mztab_pathway_measurements(se, net, acyl_chains = c("34:1", "18:1"))
    expect_length(by_type(out, "PathwayConnection"), length(net$elements$edges))
    expect_equal(length(by_type(out, "PathwayLipidNode")) + length(by_type(out, "PathwayEntity")) +
      length(by_type(out, "PathwayJunction")), length(net$elements$nodes))
    expect_equal(out$acyl_coverage$n_features, c(0L, 1L))
  }
})
