project_fixture <- function() {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = matrix(c(1, 2, 4, 8), 1,
      dimnames = list("PC 16:0_18:1", c("s1", "s2", "s3", "s4")))),
    rowData = S4Vectors::DataFrame(subclass = "PC"),
    colData = S4Vectors::DataFrame(class = c("A", "A", "B", "B")))
  .mslm_project_create(
    list(full = se, selected = se, tx = NULL, tx_raw = NULL),
    list(se = se, ready = TRUE, settings = list(method = "none", offset = 1e-9)),
    list(dataset = "lipid", adv = .default_feature_adv()),
    list(map = list(), selected = NULL, settings = list()))
}

test_that("project files retain panel-specific styles and validate their types", {
  x <- project_fixture()
  x$state$feature$panel_styles <- list(
    cor = list(plot_font_size = 18),
    net = list(plot_font_size = 16, box_linewidth = 0.4))
  file <- tempfile(fileext = ".mslm.rds")
  on.exit(unlink(file))
  .mslm_project_write(x, file)
  expect_identical(.mslm_project_read(file)$state$feature$panel_styles,
                   x$state$feature$panel_styles)
  x$state$feature$panel_styles$cor$plot_font_size <- "large"
  expect_error(.mslm_project_validate(x), "plot_font_size")
})

test_that("project files round trip data and reject incompatible or corrupt files", {
  x <- project_fixture()
  file <- tempfile(fileext = ".mslm.rds")
  on.exit(unlink(file))
  .mslm_project_write(x, file)
  expect_identical(.mslm_project_read(file), x)
  bad <- x; bad$schema_version <- 2L
  expect_error(.mslm_project_validate(bad), "unsupported schema")
  bad <- x; bad$format <- "other"
  expect_error(.mslm_project_validate(bad), "unrecognized")
  bad <- x; bad$state$normalization$se <- x$state$upload$selected[, 1:2]
  expect_error(.mslm_project_validate(bad), "dimensions")
  bad <- x; bad$state$feature$adv$bad <- new.env()
  expect_error(.mslm_project_validate(bad), "only data")
  bad <- x; bad$state$upload$selected <- "not data"
  expect_error(.mslm_project_validate(bad), "SummarizedExperiment")
  bad <- x; bad$state$feature$adv$plot_font_size <- "large"
  expect_error(.mslm_project_validate(bad), "plot_font_size")
  bad <- x; bad$state$network$settings$diff_alpha <- list(0.05)
  expect_error(.mslm_project_validate(bad), "diff_alpha")
  writeLines("not an RDS file", file)
  expect_error(.mslm_project_read(file))
})

test_that("upload snapshots retain excluded samples and transcriptomics", {
  x <- project_fixture()
  state <- x$state$upload
  state$selected <- state$full[, 1:2]
  state$tx_raw <- state$full
  rownames(state$tx_raw) <- "GENE1"
  state$tx <- state$tx_raw[, 1:2]
  shiny::testServer(mod_upload_server, {
    session$flushReact()
    handle <- session$returned
    handle$project_restore(state)
    session$flushReact()
    snapshot <- handle$project_export()
    expect_equal(ncol(snapshot$full), 4L)
    expect_equal(ncol(snapshot$selected), 2L)
    expect_equal(ncol(snapshot$tx_raw), 4L)
    expect_equal(colnames(snapshot$tx), colnames(snapshot$selected))
    expect_equal(rownames(snapshot$tx), "GENE1")
  })
})

test_that("project restores source, normalization and settings without re-upload", {
  x <- project_fixture()
  x$state$normalization$se <- normalize_se(x$state$upload$selected, "log2")
  x$state$normalization$settings$method <- "log2"
  x$state$feature$adv$plot_font_size <- 18
  els <- list(nodes = list(list(data = list(id = "pc", label = "PC"),
                                position = list(x = 15, y = 25))), edges = list())
  shiny::testServer(function(input, output, session) {
    u <- mod_upload_server("u")
    n <- mod_normalize_server("n", u$se)
    f <- mod_feature_generic_server("feat", se_lipid = u$se)
    net <- mod_cyto_server("net", se_lipid_reactive = u$se, elements = els)
    .mslm_project_server(input, output, session, u, n, f, net)
  }, {
    session$flushReact()
    file <- tempfile(fileext = ".rds")
    on.exit(unlink(file))
    .mslm_project_write(x, file)
    session$setInputs(project_file = list(datapath = file))
    session$flushReact()
    expect_equal(u$se(), x$state$upload$selected)
    expect_true(n$ready())
    expect_equal(n$se(), x$state$normalization$se)
    expect_equal(f$adv_reactive()$plot_font_size, 18)
    expect_match(output$project_status, "Restored project")

    # Invalid imports must not replace the existing input.
    bad <- x; bad$schema_version <- 99L; saveRDS(bad, file)
    session$setInputs(project_file = list(datapath = file, name = "bad.rds"))
    expect_match(output$project_status, "Restore failed")
    expect_equal(u$se(), x$state$upload$selected)
    expect_true(n$ready())

    session$setInputs(project_prepare = 1L)
    session$setInputs(project_capture = list(request = 2L, graph = NULL))
    expect_match(output$project_status, "Prepared:")
    expect_match(as.character(output$project_download_ui$html), "Download prepared project")

    # Changing the source invalidates the restored normalized data.
    state <- x$state$upload
    state$selected <- state$selected[, 1:2]
    u$project_restore(state)
    session$flushReact()
    expect_false(n$ready())
    expect_null(n$se())
    session$setInputs(`n-method` = "none", `n-apply` = 1L)
    expect_true(n$ready())
    expect_equal(n$se(), state$selected)
  })
})

test_that("network snapshot uses the correct browser graph and removes old image URLs", {
  x <- project_fixture()
  els <- list(nodes = list(list(data = list(id = "pc", label = "PC", path = "/old.svg"),
                                position = list(x = 0, y = 0))), edges = list())
  shiny::testServer(mod_cyto_server,
    args = list(se_lipid_reactive = shiny::reactive(x$state$upload$selected), elements = els), {
    session$flushReact()
    handle <- session$returned
    first <- rv_selected()
    add_network("Second", els)
    second <- setdiff(names(rv_nets$map), first)[[1]]
    graph <- list(elements = els)
    graph$elements$nodes[[1]]$position <- list(x = 321, y = 456)
    snapshot <- handle$project_export(graph, second)
    expect_equal(snapshot$map[[second]]$elements_work$nodes[[1]]$position$x, 321)
    expect_equal(snapshot$map[[first]]$elements_work$nodes[[1]]$position$x, 0)
    expect_equal(snapshot$map[[second]]$elements_work$nodes[[1]]$data$path, "")
    snapshot$settings$include_multiomics <- TRUE
    snapshot$selected <- second
    handle$project_restore(snapshot)
    session$flushReact()
    expect_equal(rv_selected(), second)
    expect_equal(rv_nets$map[[second]]$elements_work$nodes[[1]]$position$x, 321)
    expect_true(applied_settings()$include_multiomics)
    expect_equal(names(rv_nets$map), names(snapshot$map))

    # Clearing all saved networks must clear the workspace, without default reloads.
    snapshot$map <- list()
    snapshot$selected <- NULL
    handle$project_restore(snapshot)
    session$flushReact()
    expect_length(rv_nets$map, 0)
    expect_null(rv_selected())
  })
})
