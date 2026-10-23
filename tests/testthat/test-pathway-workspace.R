test_that("pathway workspace has one settings entry point and unique input IDs", {
  skip_if_not_installed("xml2")
  doc <- xml2::read_html(as.character(mod_cyto_ui("path")))
  ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
  expect_false(anyDuplicated(ids) > 0L)
  expect_length(xml2::xml_find_all(doc, "//*[@id='path-net_select']"), 1)
  expect_length(xml2::xml_find_all(doc, "//*[@id='path-settings_apply']"), 1)
  expect_length(xml2::xml_find_all(doc, "//*[@data-tab]"), 4)
  expect_length(xml2::xml_find_all(doc, "//*[@id='path-settings_drawer' and @hidden]"), 1)
  expect_length(xml2::xml_find_all(doc, "//*[@id='path-fc_enabled']"), 1)
  expect_length(xml2::xml_find_all(doc, "//*[@id='path-map_edit_enabled']"), 0)
  expect_length(xml2::xml_find_all(doc, "//*[@id='path-map_local_svgs']/following-sibling::*[1][contains(@class,'mslm-pathway-save')]"), 1)
  expect_length(xml2::xml_find_all(doc, "//*[@id='path-settings_pane_display']//*[@id='path-net_select']"), 1)
  expect_length(xml2::xml_find_all(doc, "//*[contains(@class,'mslm-pathway-header')]//*[@id='path-net_select']"), 0)
})

test_that("local pathway scripts are versioned to prevent stale style caches", {
  skip_if_not_installed("xml2")
  doc <- xml2::read_html(as.character(cyto_js_deps("cache-test")))
  scripts <- xml2::xml_attr(xml2::xml_find_all(doc, "//script[starts-with(@src,'/vendor/')]"), "src")
  expect_gte(length(scripts), 4)
  expect_true(all(grepl("\\?v=[[:xdigit:]]{12}$", scripts)))
})

test_that("analytical settings stay applied until the explicit commit", {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = rbind(L1 = c(1, 1, 4, 4), L2 = c(2, 2, 1, 1))),
    rowData = S4Vectors::DataFrame(subclass = c("PC", "PC"),
      acyl_chains = I(list("16:0", "20:4"))),
    colData = S4Vectors::DataFrame(class = c("A", "A", "B", "B")))
  # Supply an explicit tiny graph to avoid relying on the default-network catalog.
  els <- list(nodes = list(list(data = list(id = "pc", label = "PC", shared_name = "PC"),
    position = list(x = 0, y = 0))), edges = list())
  shiny::testServer(mod_cyto_server, args = list(se_lipid_reactive = shiny::reactive(se), elements = els), {
    session$flushReact()
    expect_equal(.cfg("agg_fun"), "sum")
    expect_equal(.cfg("diff_group_a"), "A")
    session$setInputs(agg_fun = "mean", fc_top_n = 2L, diff_group_a = "B", diff_group_b = "A")
    expect_equal(.cfg("agg_fun"), "sum")
    expect_equal(.cfg("diff_group_a"), "A")
    net_id <- rv_selected()
    add_network("Other pathway", els)
    other_id <- setdiff(names(rv_nets$map), net_id)[[1]]
    rv_nets$map[[other_id]]$elements_work$nodes[[1]]$data$path <- "other-old-plot.svg"
    rv_nets$map[[net_id]]$elements_work$nodes[[1]]$data$path <- "old-plot.svg"
    rv_nets$map[[net_id]]$elements_work$nodes[[1]]$data$heatmap_path <- "old-hm.svg"
    projection_state$map[[net_id]] <- "mapped"
    selected_node(list(id = "pc"))
    session$setInputs(settings_apply = 1L)
    expect_equal(.cfg("agg_fun"), "mean")
    expect_equal(.cfg("fc_top_n"), 2L)
    expect_equal(.cfg("diff_group_a"), "B")
    expect_equal(rv_nets$map[[net_id]]$elements_work$nodes[[1]]$data$path, "")
    expect_equal(rv_nets$map[[net_id]]$elements_work$nodes[[1]]$data$heatmap_path, "")
    expect_equal(rv_nets$map[[other_id]]$elements_work$nodes[[1]]$data$path, "")
    expect_equal(projection_state$map[[net_id]], "needs_mapping")
    expect_null(selected_node())
    session$setInputs(diff_group_a = "A", diff_group_b = "A", settings_apply = 2L)
    expect_equal(.cfg("diff_group_a"), "B")
    session$setInputs(diff_group_a = "B", fc_top_n = 0L, settings_apply = 3L)
    expect_equal(.cfg("fc_top_n"), 2L)
    session$setInputs(fc_top_n = 2L, chain_codes = "20:4")
    expect_equal(nrow(se_lipid_effective()), 2L)
    session$setInputs(settings_apply = 4L)
    expect_equal(nrow(se_lipid_effective()), 1L)
    session$setInputs(chain_codes = NULL)
    expect_equal(nrow(se_lipid_effective()), 1L)
    session$setInputs(settings_apply = 5L)
    expect_equal(nrow(se_lipid_effective()), 2L)
  })
})

test_that("clearing projection preserves network structure and user attributes", {
  el <- list(nodes = list(list(data = list(id = "pc", label = "PC", Color = "orange",
    path = "old.svg", heatmap_path = "old-hm.svg", enrich_html = "old result"),
    position = list(x = 5, y = 9))), edges = list(list(data = list(source = "pc", target = "pc"))))
  out <- .mslm_clear_projection_elements(el)
  expect_equal(out$nodes[[1]]$data$path, "")
  expect_equal(out$nodes[[1]]$data$enrich_html, "")
  expect_equal(out$nodes[[1]]$data$Color, "orange")
  expect_equal(out$nodes[[1]]$position, el$nodes[[1]]$position)
  expect_identical(out$edges, el$edges)
})
