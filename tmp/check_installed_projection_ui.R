.libPaths(c('C:/Users/takak/AppData/Local/R/win-library/4.5', .libPaths()))
suppressPackageStartupMessages(suppressWarnings(library(MSLipidMapper)))
ns <- asNamespace('MSLipidMapper')
stopifnot(is.function(get('mod_plot_gene_server', ns)))
doc <- xml2::read_html(as.character(get('mod_cyto_ui', ns)('path')))
stopifnot(length(xml2::xml_find_all(doc, '//*[@id="path-settings_pane_display"]//*[@id="path-net_select"]')) == 1)
stopifnot(length(xml2::xml_find_all(doc, '//*[contains(@class,"mslm-pathway-header")]//select')) == 0)
stopifnot(exists('.mslm_clear_projection_elements', ns, inherits = FALSE))
for (file in c('mslm-cytoscape-style.js', 'mslm-pathway-workspace.js')) {
  installed <- system.file('app','www','vendor',file,package='MSLipidMapper')
  stopifnot(nzchar(installed), identical(unname(tools::md5sum(installed)),
    unname(tools::md5sum(file.path('inst/app/www/vendor',file)))))
}
cat('Installed current-pathway UI, settings selection, reset helper and matching assets verified.\n')
