#' Aggregate molecule-level SE to subclass-level SE via rowData$subclass
#' @param se_mol Molecule-level SE with assay "abundance".
#' @param fun One of "sum","mean","median","proportion_within_class".
#' @param na_rm Remove NAs in aggregation.
#' @return A SummarizedExperiment where rows are subclasses.
#' @export
aggregate_to_subclass_se <- function(se_mol,
                                     fun = c("sum","mean","median","proportion_within_class"),
                                     na_rm = TRUE) {
  stopifnot(methods::is(se_mol, "SummarizedExperiment"))
  fun <- match.arg(fun)
  
  X  <- SummarizedExperiment::assay(se_mol, "abundance")
  rd <- SummarizedExperiment::rowData(se_mol)
  cd <- SummarizedExperiment::colData(se_mol)
  
  if (!"subclass" %in% colnames(rd)) stop("rowData(se)$subclass not found (copy from 'ontology' first).")
  subclass <- as.character(rd$Ontology)
  subclass[is.na(subclass) | subclass == ""] <- "Unknown"
  
  idx <- split(seq_len(nrow(X)), subclass)
  
  agg_mat <- switch(fun,
                    sum    = sapply(idx, \(ii) colSums(X[ii, , drop = FALSE], na.rm = na_rm)),
                    mean   = sapply(idx, \(ii) colMeans(X[ii, , drop = FALSE], na.rm = na_rm)),
                    median = sapply(idx, \(ii) apply(X[ii, , drop = FALSE], 2, stats::median, na.rm = na_rm)),
                    proportion_within_class = {
                      sums  <- sapply(idx, \(ii) colSums(X[ii, , drop = FALSE], na.rm = na_rm))
                      denom <- matrix(colSums(sums, na.rm = TRUE), nrow = 1)
                      sweep(sums, 2, as.numeric(denom), FUN = function(n, d) ifelse(d > 0, n/d, NA_real_))
                    }
  )
  agg_mat <- t(agg_mat)
  rownames(agg_mat) <- names(idx)
  
  SummarizedExperiment::SummarizedExperiment(
    assays  = list(abundance = agg_mat),
    rowData = S4Vectors::DataFrame(
      subclass    = rownames(agg_mat),
      n_molecules = vapply(idx, length, integer(1)),
      row.names   = rownames(agg_mat),
      check.names = FALSE
    ),
    colData = cd
  )
}

#' Convert SE to tidy long table for plotting/statistics
#' @param se A SummarizedExperiment with assay "abundance".
#' @param level_label A label stored in output (e.g., "molecule" or "subclass").
#' @param include_rowdata RowData columns to keep.
#' @return A tibble in long format.
#' @export
as_long_table <- function(se,
                          level_label = "molecule",
                          include_rowdata = c("name","class","subclass","mz","rt_min")) {
  X  <- SummarizedExperiment::assay(se, "abundance")
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  cd <- as.data.frame(SummarizedExperiment::colData(se))
  
  tibble::as_tibble(X, rownames = "feature_id") |>
    tidyr::pivot_longer(-feature_id, names_to = "sample", values_to = "abundance") |>
    dplyr::left_join(
      rd |>
        tibble::rownames_to_column("feature_id") |>
        dplyr::select(dplyr::all_of(c("feature_id", intersect(include_rowdata, names(rd))))),
      by = "feature_id"
    ) |>
    dplyr::left_join(cd |> tibble::rownames_to_column("sample"), by = "sample") |>
    dplyr::mutate(level = level_label, .before = 1)
}
