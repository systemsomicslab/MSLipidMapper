test_that("figure controls move without duplicating inputs or moving analysis thresholds", {
  skip_if_not_installed("xml2")
  registry <- .mslm_figure_registry()
  module_ids <- c(pca="pca", oplsda="opls", feat="feat", hm="hm", cor="cor",
                  volcano="volc", enrich="enrich", decoupled="dc", net="net")
  for (fun in names(registry)) {
    spec <- registry[[fun]]
    id <- module_ids[[spec$panel]]
    ui <- get(fun)(id)
    result <- .mslm_collect_figure_controls(ui, id, spec)
    doc <- xml2::read_html(htmltools::renderTags(result)$html)
    stash <- xml2::xml_find_first(doc, paste0("//*[@id='mslm-figure-stash-", spec$panel, "']"))
    for (field in spec$fields) {
      target <- paste0(id, "-", field)
      expect_length(xml2::xml_find_all(doc, paste0("//*[@id='", target, "']")), 1L)
      expect_length(xml2::xml_find_all(stash, paste0(".//*[@id='", target, "']")), 1L)
    }
    if (spec$panel == "volcano") {
      expect_length(xml2::xml_find_all(stash, ".//*[@id='volc-p_thresh']"), 0L)
      expect_length(xml2::xml_find_all(doc, "//*[@id='volc-p_thresh']"), 1L)
    }
    local_fields <- switch(spec$panel,
      pca = c("pc_x", "pc_y", "pc_loading", "top_n_each", "topn_heat", "z_lim"),
      oplsda = c("topn_vip", "topn_heat", "z_lim"),
      feat = c("lipid-n_top_mol", "lipid-n_top_chain"),
      hm = c("top_n", "samples_on_class", "cluster_samples_class", "cluster_features_mol"),
      cor = c("single_plot_type", "max_lipids_heat", "max_genes_heat"),
      volcano = "view", enrich = c("plot_type", "upset_terms_n"),
      net = c("hm_top_n", "popup_content"), character())
    for (field in local_fields) {
      target <- paste0(id, "-", field)
      expect_length(xml2::xml_find_all(doc, paste0("//*[@id='", target, "']")), 1L)
      expect_length(xml2::xml_find_all(stash, paste0(".//*[@id='", target, "']")), 0L)
    }
  }
})
