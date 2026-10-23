.libPaths(c('C:/Users/takak/AppData/Local/R/win-library/4.5', .libPaths()))
if ('--installed' %in% commandArgs(TRUE)) {
  suppressPackageStartupMessages(suppressWarnings(library(MSLipidMapper)))
  ui_fun <- get('mod_cyto_ui', asNamespace('MSLipidMapper'))
} else {
  source('R/package_paths.R')
  source('R/cyto_js_deps.R')
  source('R/Mod_cyto.R')
  ui_fun <- mod_cyto_ui
}
doc <- xml2::read_html(as.character(ui_fun('path')))
stopifnot(xml2::xml_attr(xml2::xml_find_first(doc, '//*[@id="path-net_select"]'), 'size') == '3')
stopifnot(length(xml2::xml_find_all(doc, '//button[normalize-space(.)="Panel settings"]')) == 1)
stopifnot(length(xml2::xml_find_all(doc, '//button[normalize-space(.)="Settings"]')) == 0)
cat('Compact candidate list and single Panel settings button verified.\n')
