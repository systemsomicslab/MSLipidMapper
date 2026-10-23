.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
library(shiny)
`%||%` <- function(a,b) if (is.null(a)) b else a
lines <- readLines("R/corrlation_module.R", warn=FALSE)
logic <- parse(text=lines[seq(grep("    has_multiomics <-", lines), grep("    adv_get <-", lines)-1)])
testServer(function(input, output, session) {
  se_tx_reactive <- reactiveVal(NULL)
  messages <- new.env()
  session$sendInputMessage <- function(inputId, message) messages[[inputId]] <- message
  eval(logic)
}, {
  session$flushReact()
  stopifnot(!has_multiomics(), messages$single_mode$value == "lipid_lipid",
            !grepl("lipid_gene", messages$single_mode$options, fixed=TRUE))
  se_tx_reactive(SummarizedExperiment::SummarizedExperiment(assays=list(abundance=matrix(1:6,2,3))))
  session$flushReact()
  stopifnot(has_multiomics(), grepl("lipid_gene", messages$single_mode$options, fixed=TRUE),
            !grepl("ratio", messages$single_mode$options, fixed=TRUE))
  session$setInputs(single_mode="gene_gene")
  se_tx_reactive(NULL)
  session$flushReact()
  stopifnot(!has_multiomics(), messages$single_mode$value == "lipid_lipid")
})
source("R/corrlation_module.R")
html <- htmltools::renderTags(mod_plot_cor_ui("check"))$html
stopifnot(!grepl("lipid_ratio_gene", html, fixed=TRUE), grepl("check-focus_tabs", html, fixed=TRUE))
cat("No-data modes, loading/unloading multiomics, ratio removal and UI generation verified\n")
