.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
source("R/plot_helpers_lipid.R")
invisible(parse("R/mod_plot_lipid.R"))
set.seed(1)
mat <- matrix(rexp(10 * 50), 10, 50, dimnames = list(paste("CAR", 10:19, sep=" "), paste0("long_sample_Kidney_", 1:50)))
se <- SummarizedExperiment::SummarizedExperiment(
  assays = list(abundance = mat),
  rowData = S4Vectors::DataFrame(subclass = rep("CAR", 10)),
  colData = S4Vectors::DataFrame(sample_id = colnames(mat), class = rep(c("Female", "Male"), each=25)))
ht <- make_class_heatmap_CH(se, "subclass", "CAR", topN = 10)
stopifnot(!ht@column_names_param$show, ncol(ht@matrix) == 50, nrow(ht@matrix) == 10)
png("tmp/feature-heatmap-layout.png", width=1500, height=470, res=100)
ComplexHeatmap::draw(ht, heatmap_legend_side="bottom", annotation_legend_side="bottom", merge_legends=TRUE)
dev.off()
pdf("tmp/feature-heatmap-layout.pdf", width=12, height=5)
ComplexHeatmap::draw(ht, heatmap_legend_side="bottom", annotation_legend_side="bottom", merge_legends=TRUE)
dev.off()
cat("Rendered 10 molecules x 50 samples, with sample labels hidden\n")
