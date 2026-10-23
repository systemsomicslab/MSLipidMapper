.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
library(shiny)
source("R/mod_feature_generic.R")
source("R/mod_volcano.R")
source("R/figure_settings.R")
ui <- fluidPage(
  .mslm_figure_portal_ui(),
  actionButton("open_volcano", "Figure settings"),
  actionButton("open_shared", "Shared settings"),
  .mslm_collect_figure_controls(mod_volcano_ui("volc"), "volc", .mslm_figure_registry()$mod_volcano_ui),
  verbatimTextOutput("echo"))
server <- function(input, output, session) {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays=list(abundance=matrix(1:6,2,3)),
    colData=S4Vectors::DataFrame(class=c("A","A","B")))
  feat <- mod_feature_generic_server("feat", se_lipid=se)
  settings <- feat$panel_adv_reactive("volcano")
  observeEvent(input$open_volcano, feat$open_adv(scope="figure",panel="volcano"))
  observeEvent(input$open_shared, feat$open_adv(scope="common"))
  output$echo <- renderText(paste(input[["volc-point_size"]], input[["volc-view"]], settings()$plot_font_size))
}
runApp(shinyApp(ui,server), host="127.0.0.1", port=3877, launch.browser=FALSE)
