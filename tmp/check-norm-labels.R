.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
library(ggplot2)
`%||%` <- function(a, b) if (is.null(a)) b else a
find_builder <- function(expr) {
  if (!is.call(expr) && !is.expression(expr)) return(NULL)
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name(".make_qc_plot"))) return(expr)
  for (part in as.list(expr)) {
    found <- find_builder(part)
    if (!is.null(found)) return(found)
  }
  NULL
}
eval(find_builder(parse("R/norm_module.R")))
.get_norm_matrix <- function(se) se
set.seed(1)
mat <- matrix(rexp(200 * 50), 200, 50)
colnames(mat) <- paste0("190828_TT1_Hclass_Aging_", seq_len(50), "_24M_GF_F_Kidney_1_Pos_long_sample_name")
for (angle in c(0, 45, 90)) {
  p <- .make_qc_plot(mat, angle)
  stopifnot(max(nchar(unlist(strsplit(p$scales$scales[[1]]$labels(colnames(mat)), "\n")))) <= 36)
  ggsave(paste0("tmp/norm-labels-", angle, ".png"), p, width = 12, height = 6, dpi = 100)
}
cat("Rendered 50 long sample labels at 0, 45, 90 degrees\n")
