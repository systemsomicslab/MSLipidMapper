test_that("FC panels compute per-species B/A and select both directions", {
  mat <- rbind(up = c(1, 1, 4, 4), down = c(8, 8, 2, 2),
               flat = c(3, 3, 3, 3), invalid = c(-1, -1, 2, 2),
               missing = c(NA, 1, 2, 2), pe = c(1, 1, 32, 32))
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = mat),
    rowData = S4Vectors::DataFrame(subclass = c(rep("PC", 5), "PE")),
    colData = S4Vectors::DataFrame(class = c("A", "A", "B", "B")))
  out <- .mslm_fc_panel_data(se, "subclass", "class", "A", "B", top_n = 1)
  pc <- out$classes[[1]]
  expect_equal(pc$total, 5L)
  expect_equal(pc$valid, 3L)
  expect_equal(pc$excluded, 2L)
  expect_equal(vapply(pc$rows, `[[`, character(1), "name"), c("up", "down"))
  expect_equal(vapply(pc$rows, `[[`, numeric(1), "fc"), c(2, -2), tolerance = 1e-7)
  expect_equal(out$domain, 5)
  reverse <- .mslm_fc_panel_data(se, "subclass", "class", "B", "A", top_n = 1)
  expect_equal(reverse$classes[[1]]$rows[[1]]$name, "down")
  expect_equal(reverse$classes[[1]]$rows[[1]]$fc, 2, tolerance = 1e-7)
  expect_error(.mslm_fc_panel_data(se, "subclass", "class", "A", "A"), "different")
  expect_error(.mslm_fc_panel_data(se[, 1:3], "subclass", "class", "A", "B"), "two samples")
})

test_that("log2 input uses differences and allows negative log intensities", {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(log2 = rbind(L1 = c(-2, -2, 1, 1), L2 = c(4, 4, 1, 1))),
    rowData = S4Vectors::DataFrame(subclass = c("PC", "PC")),
    colData = S4Vectors::DataFrame(class = c("A", "A", "B", "B")))
  out <- .mslm_fc_panel_data(se, "subclass", "class", "A", "B", scale = "log2")
  expect_equal(vapply(out$classes[[1]]$rows, `[[`, numeric(1), "fc"), c(3, -3))
  expect_equal(out$classes[[1]]$excluded, 0L)
  # A class with only zero FCs is retained, with no misleading 'up/down' bars.
  SummarizedExperiment::assay(se)[] <- 0
  out <- .mslm_fc_panel_data(se, "subclass", "class", "A", "B")
  expect_length(out$classes[[1]]$rows, 0)
  expect_equal(out$domain, 1)
})
