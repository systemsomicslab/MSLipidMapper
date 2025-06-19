# ========================
# Module: load_se_module.R
# ========================

# ---- Required packages ----
library(SummarizedExperiment)
library(readr)
library(S4Vectors)
library(dplyr)
library(gprofiler2)

# ---- Function: Load Lipidomics CSV as SummarizedExperiment ----
load_lipidomics_se <- function(csv_path, annotation_cols = 1:35, header_rows = 5, data_start_row = 6) {
  raw_header <- read_csv(csv_path, col_names = FALSE, n_max = header_rows)
  true_colnames <- as.character(unlist(raw_header[5, ]))
  group_info <- as.character(unlist(raw_header[1, (max(annotation_cols) + 1):length(true_colnames)]))
  
  df <- read_csv(csv_path, skip = data_start_row - 1, col_names = FALSE)
  colnames(df) <- true_colnames
  
  row_data <- df[, annotation_cols]
  assay_data <- df[, -(annotation_cols)]
  
  assay_matrix <- as.matrix(data.frame(lapply(assay_data, as.numeric)))
  sample_names <- colnames(df)[(max(annotation_cols) + 1):ncol(df)]
  colnames(assay_matrix) <- sample_names
  
  col_data <- DataFrame(
    sample_id = sample_names,
    group = group_info
  )
  rownames(col_data) <- sample_names
  
  se <- SummarizedExperiment(
    assays = list(abundance = assay_matrix),
    rowData = row_data,
    colData = col_data
  )
  
  rownames(se) <- make.unique(as.character(rowData(se)[["Metabolite name"]]))
  return(se)
}

# ---- Function: Load Transcriptome CSV as SummarizedExperiment ----
load_transcriptome_se <- function(csv_path, organism = "mmusculus", target = "ENSG") {
  df <- read_csv(csv_path)
  gene_ids <- df$GeneID
  
  ensembl_map <- gconvert(query = gene_ids,
                          organism = organism,
                          target = target,
                          mthreshold = Inf,
                          filter_na = TRUE) %>%
    select(input, target)
  
  row_data <- df |> 
    select(GeneID) |> 
    left_join(ensembl_map, by = c("GeneID" = "input")) |>
    rename(EnsemblID = target)
  
  rownames(row_data) <- make.unique(row_data$GeneID)
  
  assay_data <- df |> select(-GeneID)
  assay_matrix <- as.matrix(assay_data)
  rownames(assay_matrix) <- rownames(row_data)
  
  col_data <- DataFrame(sample_id = colnames(assay_matrix))
  rownames(col_data) <- colnames(assay_matrix)
  
  se <- SummarizedExperiment(
    assays = list(abundance = assay_matrix),
    rowData = row_data,
    colData = col_data
  )
  return(se)
}
