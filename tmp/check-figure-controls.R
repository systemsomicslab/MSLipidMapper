.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
for (file in c("package_paths", "cyto_js_deps", "mod_plot_lipid", "mod_plot_tx", "mod_feature_generic", "mod_pca_generic", "mod_oplsda_generic", "Heatmap", "corrlation_module", "mod_volcano", "Enrich_module", "mod_decoupled_chains", "Mod_cyto", "figure_settings")) source(paste0("R/", file, ".R"))
for (fun in names(.mslm_figure_registry())) {
  spec <- .mslm_figure_registry()[[fun]]
  id <- c(pca="pca",oplsda="opls",feat="feat",hm="hm",cor="cor",volcano="volc",enrich="enrich",decoupled="dc",net="net")[[spec$panel]]
  ui <- get(fun)(id)
  result <- .mslm_collect_figure_controls(ui, id, spec)
  doc <- xml2::read_html(htmltools::renderTags(result)$html)
  stash <- xml2::xml_find_first(doc, paste0("//*[@id='mslm-figure-stash-", spec$panel, "']"))
  stopifnot(!inherits(stash, "xml_missing"))
  for (field in spec$fields) {
    expected <- paste0(id,"-",field)
    nodes <- xml2::xml_find_all(doc, paste0("//*[@id='", expected, "']"))
    stopifnot(length(nodes)==1L,
              length(xml2::xml_find_all(stash, paste0(".//*[@id='", expected, "']")))==1L)
  }
  if (spec$panel=="volcano") stopifnot(length(xml2::xml_find_all(stash,".//*[@id='volc-p_thresh']"))==0L)
  if (spec$panel=="decoupled") stopifnot(length(xml2::xml_find_all(stash,".//*[@id='dc-corr_method']"))==0L)
  cat(spec$panel, ":", length(spec$fields), "figure controls moved, input IDs unique\n")
}
