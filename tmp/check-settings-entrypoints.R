.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
find_assignment <- function(expr, name) {
  if (missing(expr) || (!is.call(expr) && !is.expression(expr))) return(NULL)
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && identical(expr[[2]], as.name(name))) return(expr)
  for (part in as.list(expr)) {
    found <- find_assignment(part, name)
    if (!is.null(found)) return(found)
  }
  NULL
}
tree <- parse("R/run_mslipidmapper_app.R")
.ic <- shiny::icon
eval(find_assignment(tree, ".boxTitleWithAdv"))
eval(find_assignment(tree, ".settingsGuide"))
for (panel in c("pca", "oplsda", "feat", "hm", "cor", "volcano", "enrich", "decoupled", "net")) {
  html <- htmltools::renderTags(.boxTitleWithAdv(panel, paste0("open_adv_", panel)))$html
  stopifnot(!grepl("Advanced setting", html, fixed=TRUE))
  stopifnot(grepl("Figure settings", html, fixed=TRUE) == (panel %in% c("feat", "cor", "net")))
  stopifnot(grepl("Shared: colors / order", html, fixed=TRUE) == (!panel %in% c("volcano", "enrich")))
  stopifnot(nchar(htmltools::renderTags(.settingsGuide(panel))$html) > 0)
}
source("R/mod_feature_generic.R")
source("R/project_session.R")
se <- SummarizedExperiment::SummarizedExperiment(assays=list(abundance=matrix(1:6,2,3)),
  colData=S4Vectors::DataFrame(class=c("A","A","B")))
x <- .mslm_project_create(
  list(full=se, selected=se, tx=NULL, tx_raw=NULL),
  list(se=se, ready=TRUE, settings=list(method="none", offset=1e-9)),
  list(dataset="lipid", adv=.default_feature_adv(), panel_styles=list(cor=list(plot_font_size=18))),
  list(map=list(), selected=NULL, settings=list()))
path <- tempfile(fileext=".mslm.rds")
.mslm_project_write(x, path)
stopifnot(.mslm_project_read(path)$state$feature$panel_styles$cor$plot_font_size==18)
unlink(path)
x$state$feature$panel_styles$cor$plot_font_size <- "bad"
stopifnot(inherits(try(.mslm_project_validate(x), silent=TRUE), "try-error"))
cat("All nine entrypoints and project file validation verified\n")
