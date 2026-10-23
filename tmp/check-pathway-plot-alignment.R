.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5",.libPaths()))
source("R/package_paths.R")
source("R/cyto_js_deps.R")
source("R/Mod_cyto.R")
html <- htmltools::renderTags(mod_cyto_ui("net"))$html
doc <- xml2::read_html(html)
cards <- xml2::xml_find_all(doc, "//*[contains(@class, 'mslm-selected-node-plots')]/*[contains(@class, 'plot-card')]")
stopifnot(length(cards)==2)
for (card in cards) {
  stopifnot(length(xml2::xml_find_all(card, "./*[contains(@class, 'mslm-node-plot-header')]"))==1,
            length(xml2::xml_find_all(card, "./*[contains(@class, 'cy-plot-frame')]"))==1)
}
stopifnot(length(xml2::xml_find_all(cards[[2]], ".//*[@id='net-molecule_ui']"))==1,
          grepl("grid-template-rows:subgrid",html,fixed=TRUE))
cat("Both cards share header/plot/download rows; molecule selector retained\n")
