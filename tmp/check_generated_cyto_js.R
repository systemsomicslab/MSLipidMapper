.libPaths(c('C:/Users/takak/AppData/Local/R/win-library/4.5', .libPaths()))
source('R/package_paths.R')
source('R/cyto_js_deps.R')
tags <- cyto_js_deps('test-cy')
for (tag in tags) {
  if (inherits(tag, 'shiny.tag') && identical(tag$name, 'script') && is.null(tag$attribs$src)) {
    writeLines(as.character(tag$children[[1]]), 'tmp/generated-cyto-check.js', useBytes = TRUE)
  }
}
