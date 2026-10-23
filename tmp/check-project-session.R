Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
.libPaths(c('C:/Users/takak/AppData/Local/R/win-library/4.5', .libPaths()))
for (f in c('normalize.R','project_session.R','upload_module.R','norm_module.R',
            'package_paths.R','cyto_js_deps.R','cyto_utils.R','mod_feature_generic.R','Mod_cyto.R')) source(file.path('R', f), encoding='UTF-8')
testthat::test_file('tests/testthat/test-project-session.R', reporter='summary', stop_on_failure=TRUE)
testthat::test_file('tests/testthat/test-pathway-workspace.R', reporter='summary', stop_on_failure=TRUE)
writeLines(as.character(.mslm_project_ui()), 'tmp/project-ui.html', useBytes=TRUE)
writeLines(as.character(cyto_js_deps('net-cy')), 'tmp/project-cyto-ui.html', useBytes=TRUE)
