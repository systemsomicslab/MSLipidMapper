.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
library(shiny)
library(ggplot2)
`%||%` <- function(a,b) if (is.null(a)) b else a
find <- function(expr, name) {
  if (missing(expr) || (!is.call(expr) && !is.expression(expr))) return(NULL)
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && identical(expr[[2]], as.name(name))) return(expr)
  for (part in as.list(expr)) {
    found <- find(part, name)
    if (!is.null(found)) return(found)
  }
  NULL
}
volcano <- parse("R/mod_volcano.R")
chain <- parse("R/mod_decoupled_chains.R")
testServer(function(input, output, session) {
  rv <- reactiveValues(out=list(plot=ggplot(data.frame(x=1:3,y=1:3),aes(x,y))+geom_point()))
  adv_reactive <- reactive(list(plot_font_size=input$font_size %||% 12))
  .get_adv <- function() list(plot_font_size=input$scatter_font_size %||% 12)
  eval(find(volcano, "figure_font"))
  eval(find(volcano, "styled_plot"))
  eval(find(chain, ".apply_panel_fonts"))
}, {
  stopifnot(styled_plot()$theme$axis.text$size == 12)
  session$setInputs(font_size=18, scatter_font_size=20)
  stopifnot(styled_plot()$theme$axis.text$size == 18)
  p <- .apply_panel_fonts(ggplot()+annotate("text", x=0,y=0,label="r = 0.5"))
  stopifnot(p$theme$axis.text$size == 20, abs(p$layers[[1]]$aes_params$size-20/(72.27/25.4)) < 1e-8)
  png("tmp/chain-font-check.png")
  print(p)
  dev.off()
  session$setInputs(font_size=NA, scatter_font_size=NA)
  stopifnot(figure_font()==12, .apply_panel_fonts(ggplot())$theme$axis.text$size==12)
})
source("R/mod_volcano.R")
source("R/mod_decoupled_chains.R")
stopifnot(!grepl("v-font_size", htmltools::renderTags(mod_volcano_ui("v"))$html),
          !grepl("c-scatter_font_size", htmltools::renderTags(mod_decoupled_chains_ui("c"))$html))
cat("Local fonts update without analysis; defaults, annotations and UI verified\n")
