.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
library(shiny)
source("R/mod_feature_generic.R")
se <- SummarizedExperiment::SummarizedExperiment(
  assays=list(abundance=matrix(1:6,2,3)),
  colData=S4Vectors::DataFrame(class=c("A","B","A"), sample_id=c("s1","s2","s3")))
testServer(mod_feature_generic_server, args=list(se_lipid=se), {
  session$flushReact()
  open_adv(scope="common")
  html <- htmltools::renderTags(feature_adv_modal())$html
  stopifnot(grepl("adv_color_pickers", html), !grepl("adv_plot_font_size", html), !grepl("adv_facet_var", html))
  original <- rv$adv
  session$setInputs(adv_apply=1)
  stopifnot(identical(rv$adv$plot_font_size, original$plot_font_size), identical(rv$adv$box_linewidth, original$box_linewidth))
  open_adv(scope="figure", panel="cor")
  html <- htmltools::renderTags(feature_adv_modal())$html
  stopifnot(grepl("adv_plot_font_size", html), !grepl("adv_box_width", html), !grepl("adv_color_pickers", html))
  session$setInputs(adv_plot_font_size=18, adv_apply=2)
  stopifnot(panel_settings("cor")$plot_font_size==18, rv$adv$plot_font_size==12, panel_settings("net")$plot_font_size==12)
  open_adv(scope="figure", panel="feat")
  html <- htmltools::renderTags(feature_adv_modal())$html
  stopifnot(grepl("adv_box_linewidth", html), grepl("adv_add_p", html), !grepl("adv_color_pickers", html))
  rv$adv <- modifyList(rv$adv, list(plot_font_size=20, palette_map=c(A="#ff0000", B="#0000ff")))
  stopifnot(panel_settings("cor")$plot_font_size==18, panel_settings("net")$plot_font_size==12,
            panel_settings("cor")$palette_map[["A"]]=="#ff0000")
  exported <- session$returned$project_export()
  session$returned$project_restore(exported)
  stopifnot(panel_settings("cor")$plot_font_size==18)
  open_adv(scope="figure", panel="net")
  values <- .default_feature_adv()
  values$box_linewidth <- 0.4
  values$plot_font_size <- 16
  values <- setNames(values, paste0("adv_", names(values)))
  values$adv_apply <- 3
  do.call(session$setInputs, values)
  stopifnot(panel_settings("net")$box_linewidth==0.4, panel_settings("net")$plot_font_size==16,
            rv$adv$box_linewidth==1, rv$adv$plot_font_size==20, panel_settings("cor")$plot_font_size==18)
  open_adv(scope="figure", panel="volcano")
  html <- htmltools::renderTags(feature_adv_modal())$html
  stopifnot(grepl("Volcano: Figure settings", html))
  session$setInputs(adv_plot_font_size=22, adv_apply=4)
  stopifnot(panel_settings("volcano")$plot_font_size==22, panel_settings("cor")$plot_font_size==18)
  open_adv(scope="figure", panel="decoupled")
  html <- htmltools::renderTags(feature_adv_modal())$html
  stopifnot(grepl("adv_chain_heatmap_font_size", html), grepl("adv_chain_heatmap_value_size", html))
  session$setInputs(adv_plot_font_size=14, adv_chain_heatmap_font_size=16, adv_chain_heatmap_value_size=4, adv_apply=5)
  stopifnot(panel_settings("decoupled")$plot_font_size==14,
            panel_settings("decoupled")$chain_heatmap_font_size==16,
            panel_settings("decoupled")$chain_heatmap_value_size==4,
            panel_settings("volcano")$plot_font_size==22)
  legacy <- session$returned$project_export()
  legacy$panel_styles <- NULL
  session$returned$project_restore(legacy)
  stopifnot(panel_settings("cor")$plot_font_size==20)
})
cat("Modal contents, shared-setting isolation, per-panel fonts and project restore verified\n")
