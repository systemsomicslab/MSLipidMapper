#' MS-DIAL lipidomics CSV -> SummarizedExperiment (molecule level)
#'
#' Reads an MS-DIAL lipidomics alignment table (CSV), splits row annotations and
#' assay matrix, and produces a SummarizedExperiment. It assumes the first
#' `header_rows` rows contain metadata/header with true column names at
#' row `header_rows`. The first row on the sample side is treated as sample class.
#'
#' The rowData column "ontology" (if present) is copied to "subclass".
#' Assay name is "abundance".
#'
#' @param csv_path Path to MS-DIAL alignment CSV.
#' @param annotation_cols Integer vector for row-annotation columns (e.g., 1:35).
#' @param header_rows Number of header rows (true column names at this row).
#' @param data_start_row Data starts at this 1-based row.
#' @return A SummarizedExperiment object with assay "abundance".
#' @export
load_lipidomics_se <- function(csv_path,
                               annotation_cols = 1:35,
                               header_rows = 5,
                               data_start_row = 6) {
  stopifnot(file.exists(csv_path))
  
  # Step 1: read header to extract true column names and sample class labels
  raw_header <- readr::read_csv(csv_path, col_names = FALSE, n_max = header_rows, show_col_types = FALSE)
  true_colnames <- as.character(unlist(raw_header[header_rows, ]))
  sample_class_labels <- as.character(unlist(raw_header[1, (max(annotation_cols) + 1):length(true_colnames)]))
  
  # Step 2: read full body and set proper colnames
  df <- readr::read_csv(csv_path, skip = data_start_row - 1, col_names = FALSE, show_col_types = FALSE)
  colnames(df) <- true_colnames
  
  # Step 3: split annotation (row-level) and assay (sample-level)
  row_data <- df[, annotation_cols, drop = FALSE]
  assay_data <- df[, -(annotation_cols), drop = FALSE]
  
  # Step 4: coerce assay to numeric matrix
  assay_matrix <- as.matrix(data.frame(lapply(assay_data, function(x) suppressWarnings(as.numeric(x)))))
  sample_names <- colnames(df)[(max(annotation_cols) + 1):ncol(df)]
  colnames(assay_matrix) <- sample_names
  
  # Step 5: build colData with sample class labels
  has_class <- length(sample_class_labels) == length(sample_names)
  col_data <- S4Vectors::DataFrame(
    sample_id = sample_names,
    class     = if (has_class) sample_class_labels else rep(NA_character_, length(sample_names))
  )
  base::rownames(col_data) <- sample_names
  
  # Step 6: construct SE (keep all original row annotations)
  se <- SummarizedExperiment::SummarizedExperiment(
    assays  = list(abundance = assay_matrix),
    rowData = S4Vectors::DataFrame(row_data, check.names = FALSE),
    colData = col_data
  )
  
  # Step 7: set rownames from a best-effort "Metabolite name" (fallbacks included)
  rn_source <- "Metabolite name"
  if (!rn_source %in% colnames(SummarizedExperiment::rowData(se))) {
    alt <- c("Name", "Title", "Alignment ID")
    hit <- alt[alt %in% colnames(SummarizedExperiment::rowData(se))][1]
    rn_source <- if (length(hit)) hit else colnames(SummarizedExperiment::rowData(se))[1]
  }
  base::rownames(se) <- make.unique(as.character(SummarizedExperiment::rowData(se)[[rn_source]]))
  
  # Ensure subclass is available from ontology
  rd <- SummarizedExperiment::rowData(se)
  if ("Ontology" %in% colnames(rd)) {
    if (!"subclass" %in% colnames(rd)) rd$subclass <- as.character(rd$Ontology)
  } else {
    if (!"subclass" %in% colnames(rd)) rd$subclass <- "Unknown"
  }
  SummarizedExperiment::rowData(se) <- rd
  
  se
}
