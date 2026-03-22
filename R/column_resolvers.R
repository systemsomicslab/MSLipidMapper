# Column resolver helpers -------------------------------------------------

.pick_first_existing_col <- function(candidates, available) {
  hit <- candidates[candidates %in% available][1]
  if (length(hit) && !is.na(hit) && nzchar(hit)) hit else NULL
}

.resolve_lipid_name_col <- function(x, default = NULL) {
  available <- if (methods::is(x, "SummarizedExperiment")) {
    colnames(SummarizedExperiment::rowData(x))
  } else {
    colnames(x)
  }

  .pick_first_existing_col(
    c("Metabolite.name", "Metabolite name"),
    available
  ) %||% default
}

.resolve_feature_label_col <- function(x, default = NULL) {
  .resolve_lipid_name_col(x, default = default)
}
