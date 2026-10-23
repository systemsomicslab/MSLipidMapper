suppressPackageStartupMessages(library(MSLipidMapper))
suppressPackageStartupMessages(library(ggplot2))

# Exercise compiled libraries and plotting extensions against the older,
# container-matched dependency set rather than only checking package presence.
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
stopifnot(DBI::dbGetQuery(con, "select 42 as value")$value == 42)
DBI::dbDisconnect(con)
stopifnot(nrow(readr::read_csv(I("name,value\nPC,1\nPE,2\n"), show_col_types = FALSE)) == 2L)
stopifnot(igraph::vcount(igraph::make_ring(4)) == 4L)
stopifnot(nrow(rgoslin::parseLipidNames(c("PC 16:0/18:1", "PE 18:0/20:4"))) == 2L)
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
  ggprism::theme_prism() + ggpmisc::stat_poly_line()
svg <- tempfile(fileext = ".svg")
ggplot2::ggsave(svg, patchwork::wrap_plots(p, p), device = svglite::svglite,
  width = 8, height = 4)
stopifnot(file.info(svg)$size > 1000)
cat("SMOKE: SQLite, CSV, igraph, rgoslin and combined SVG plotting passed\n")

if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat", repos = BiocManager::repositories()["BioCcontainers"], Ncpus = 4)
}
testthat::test_dir("/srv/app/tests/testthat", reporter = "summary",
  env = new.env(parent = asNamespace("MSLipidMapper")), stop_on_failure = TRUE)
