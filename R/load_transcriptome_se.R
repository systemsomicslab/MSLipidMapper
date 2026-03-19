#' Load a transcriptome count table as a SummarizedExperiment
#'
#' This function reads a wide-format CSV file containing gene symbols,
#' Ensembl IDs, and sample-wise numeric abundance/count columns, then
#' constructs a SummarizedExperiment object.
#'
#' Expected input format:
#'   - one column for gene symbols
#'   - one column for Ensembl IDs
#'   - remaining columns are sample abundances / counts
#'
#' Column selection is done by column index, not by column name.
#'
#' @param csv_path Character. Path to the transcriptome CSV file.
#' @param symbol_col Integer. Column index for the gene symbol column.
#' @param ensembl_col Integer. Column index for the Ensembl ID column.
#' @param organism Character. One of c("mmusculus", "hsapiens").
#'   Used for Ensembl ID format validation.
#' @param none_tokens Character vector. Values treated as missing Ensembl IDs.
#' @param keep_unmapped Character. How to handle rows with missing Ensembl IDs.
#'   One of c("drop", "keep_na", "error").
#'
#' @return A SummarizedExperiment object with:
#'   - assay "abundance": numeric matrix (#genes x #samples)
#'   - rowData: GeneSymbol and EnsemblID
#'   - colData: sample_id
#'
#' @examples
#' \dontrun{
#' se <- load_transcriptome_se_from_symbol_ensembl(
#'   csv_path    = "counts.csv",
#'   symbol_col  = 1,
#'   ensembl_col = 2,
#'   organism    = "hsapiens"
#' )
#' }
#' @export
load_transcriptome_se_from_symbol_ensembl <- function(
    csv_path,
    symbol_col      = 1,
    ensembl_col     = 2,
    organism        = c("mmusculus", "hsapiens"),
    none_tokens     = c("none", "na", "n/a", "-", "."),
    keep_unmapped   = c("drop", "keep_na", "error")
) {
  organism <- match.arg(organism)
  keep_unmapped <- match.arg(keep_unmapped)

  # Read input CSV
  df <- readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)

  # Validate column index arguments
  if (!is.numeric(symbol_col) || length(symbol_col) != 1 || is.na(symbol_col)) {
    stop("symbol_col must be a single numeric column index.")
  }
  if (!is.numeric(ensembl_col) || length(ensembl_col) != 1 || is.na(ensembl_col)) {
    stop("ensembl_col must be a single numeric column index.")
  }

  symbol_col  <- as.integer(symbol_col)
  ensembl_col <- as.integer(ensembl_col)

  if (symbol_col < 1 || symbol_col > ncol(df)) {
    stop(sprintf("symbol_col=%d is out of range. ncol(df)=%d", symbol_col, ncol(df)))
  }
  if (ensembl_col < 1 || ensembl_col > ncol(df)) {
    stop(sprintf("ensembl_col=%d is out of range. ncol(df)=%d", ensembl_col, ncol(df)))
  }
  if (symbol_col == ensembl_col) {
    stop("symbol_col and ensembl_col must refer to different columns.")
  }

  # Extract symbol and Ensembl columns
  symbol <- trimws(as.character(df[[symbol_col]]))
  ens    <- trimws(as.character(df[[ensembl_col]]))

  # Convert predefined missing tokens to NA
  ens_lc <- tolower(ens)
  ens[ens_lc %in% tolower(none_tokens)] <- NA_character_

  # Convert empty strings to NA
  symbol[symbol == ""] <- NA_character_
  ens[ens == ""]       <- NA_character_

  stopifnot(length(symbol) == nrow(df), length(ens) == nrow(df))

  # Extract sample columns as all columns except symbol / Ensembl columns
  sample_idx <- setdiff(seq_len(ncol(df)), c(symbol_col, ensembl_col))
  assay_df <- df[, sample_idx, drop = FALSE]

  # Coerce non-numeric sample columns to numeric
  non_num <- vapply(assay_df, function(x) !is.numeric(x), logical(1))
  if (any(non_num)) {
    warning("Some sample columns were not numeric and were coerced with as.numeric().")
    assay_df[non_num] <- lapply(
      assay_df[non_num],
      function(x) suppressWarnings(as.numeric(x))
    )
  }

  # Handle missing Ensembl IDs
  unmapped <- is.na(ens)
  n_unmapped <- sum(unmapped, na.rm = TRUE)

  if (n_unmapped > 0) {
    if (keep_unmapped == "error") {
      stop(sprintf("There are %d rows with missing Ensembl IDs (NA / none-like values).", n_unmapped))
    } else if (keep_unmapped == "drop") {
      warning(sprintf("Dropped %d rows with missing Ensembl IDs.", n_unmapped))
      keep <- !unmapped
      assay_df <- assay_df[keep, , drop = FALSE]
      symbol   <- symbol[keep]
      ens      <- ens[keep]
    } else if (keep_unmapped == "keep_na") {
      warning(sprintf("There are %d rows with missing Ensembl IDs; they were retained.", n_unmapped))
    }
  }

  # Remove rows where all sample values are NA
  keep_data <- rowSums(!is.na(as.data.frame(assay_df))) > 0
  if (!all(keep_data)) {
    n_drop <- sum(!keep_data)
    warning(sprintf("Dropped %d rows because all sample values were NA.", n_drop))
    assay_df <- assay_df[keep_data, , drop = FALSE]
    symbol   <- symbol[keep_data]
    ens      <- ens[keep_data]
  }

  # Validate Ensembl ID format, skipping NA values
  ok_prefix <- if (organism == "hsapiens") "^ENSG\\d+" else "^ENSMUSG\\d+"
  bad <- which(!is.na(ens) & !grepl(ok_prefix, ens))

  if (length(bad)) {
    stop(sprintf(
      "Found %d Ensembl IDs inconsistent with organism '%s'. Example(s): %s",
      length(bad),
      organism,
      paste(unique(ens[bad][1:min(3, length(bad))]), collapse = ", ")
    ))
  }

  # Use gene symbols as row names and make them unique
  if (any(is.na(symbol))) {
    n_na_sym <- sum(is.na(symbol))
    warning(sprintf(
      "There are %d rows with missing gene symbols; they will be labeled as 'NA' and uniquified.",
      n_na_sym
    ))
  }

  rn <- make.unique(ifelse(is.na(symbol), "NA", symbol))

  # Convert assay data frame to numeric matrix
  assay_matrix <- as.matrix(as.data.frame(assay_df))
  storage.mode(assay_matrix) <- "double"

  stopifnot(length(rn) == nrow(assay_matrix))

  rownames(assay_matrix) <- rn
  sample_names <- colnames(assay_matrix)

  # Build rowData and colData
  row_data <- S4Vectors::DataFrame(
    GeneSymbol = symbol,
    EnsemblID  = ens,
    row.names  = rn
  )

  col_data <- S4Vectors::DataFrame(
    sample_id = sample_names,
    row.names = sample_names
  )

  # Return SummarizedExperiment
  SummarizedExperiment::SummarizedExperiment(
    assays  = list(abundance = assay_matrix),
    rowData = row_data,
    colData = col_data
  )
}