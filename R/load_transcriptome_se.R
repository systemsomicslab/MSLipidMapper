#' Load a transcriptome count table as SummarizedExperiment
#'
#' Reads a wide CSV with a \code{GeneID} column (gene identifier) and one column
#' per sample (raw counts / expression), converts gene IDs to Ensembl IDs via
#' \code{gprofiler2::gconvert()}, and returns a \code{SummarizedExperiment}.
#'
#' Expected format:
#' \itemize{
#'   \item \strong{GeneID}: character gene IDs (e.g., symbols or Ensembl IDs)
#'   \item \strong{Sample columns}: numeric counts/expressions per sample
#' }
#'
#' @param csv_path Path to the transcriptome CSV file.
#' @param organism Organism code for g:Profiler (default \code{"mmusculus"}).
#'   Examples: \code{"hsapiens"}, \code{"mmusculus"}.
#' @param target g:Profiler target ID space (default \code{"ENSG"} for Ensembl Gene IDs).
#' @return A \code{SummarizedExperiment} with:
#'   \itemize{
#'     \item assay \code{abundance}: numeric matrix (#genes x #samples)
#'     \item rowData: \code{GeneID} and \code{EnsemblID}
#'     \item colData: \code{sample_id}
#'   }
#' @examples
#' \dontrun{
#' se <- load_transcriptome_se("counts.csv", organism = "hsapiens", target = "ENSG")
#' }
#' @export
# 必須: GeneSymbol と EnsemblID を CSV に用意
# 例: GeneSymbol,EnsemblID,S1,S2,S3,...
#     A1cf,ENSMUSG00000012345,10.2, 9.8, 11.0

load_transcriptome_se_from_symbol_ensembl <- function(
    csv_path,
    symbol_col      = "Gene",
    ensembl_col     = "EnsemblID",
    organism        = c("mmusculus","hsapiens"),
    none_tokens     = c("none","na","n/a","-","."),
    keep_unmapped   = c("drop","keep_na","error")  # ← ここで挙動を選択
) {
  organism <- match.arg(organism)
  keep_unmapped <- match.arg(keep_unmapped)

  df <- readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)

  # --- 必須列 ---
  cn <- colnames(df)
  stopifnot(symbol_col %in% cn, ensembl_col %in% cn)

  # --- 取り出し & 前処理 ---
  symbol <- trimws(as.character(df[[symbol_col]]))
  ens    <- trimws(as.character(df[[ensembl_col]]))

  # “none”系トークン → NA（大文字小文字無視）
  ens_lc <- tolower(ens)
  ens[ens_lc %in% tolower(none_tokens)] <- NA_character_

  # 空文字も NA に
  symbol[symbol == ""] <- NA_character_
  ens[ens == ""]       <- NA_character_

  stopifnot(length(symbol) == nrow(df), length(ens) == nrow(df))

  # --- サンプル列抽出 ---
  sample_idx <- setdiff(seq_len(ncol(df)), match(c(symbol_col, ensembl_col), cn))
  assay_df <- df[, sample_idx, drop = FALSE]

  # 数値化
  non_num <- vapply(assay_df, function(x) !is.numeric(x), logical(1))
  if (any(non_num)) {
    warning("非数値サンプル列を as.numeric で変換（NA になる値があるかも）。")
    assay_df[non_num] <- lapply(assay_df[non_num], function(x) suppressWarnings(as.numeric(x)))
  }

  # --- アンマップ行の扱い ---
  unmapped <- is.na(ens)
  n_unmapped <- sum(unmapped, na.rm = TRUE)
  if (n_unmapped > 0) {
    if (keep_unmapped == "error") {
      stop(sprintf("EnsemblID が未マップ（NA/none等）な行が %d 件あります。", n_unmapped))
    } else if (keep_unmapped == "drop") {
      warning(sprintf("%d 行を未マップ（NA）として除外しました。", n_unmapped))
      keep <- !unmapped
      assay_df <- assay_df[keep, , drop = FALSE]
      symbol   <- symbol[keep]
      ens      <- ens[keep]
    } else {
      # keep_na：残す（rowData の EnsemblID は NA）
      warning(sprintf("未マップ（NA）の行が %d 件あります（残します）。", n_unmapped))
    }
  }

  # --- 全サンプル NA 行は除外 ---
  keep_data <- rowSums(!is.na(as.data.frame(assay_df))) > 0
  if (!all(keep_data)) {
    n_drop <- sum(!keep_data)
    warning(sprintf("%d 行を全サンプル NA のため除外しました。", n_drop))
    assay_df <- assay_df[keep_data, , drop = FALSE]
    symbol   <- symbol[keep_data]
    ens      <- ens[keep_data]
  }

  # --- Ensembl 形式チェック（NA はスキップ） ---
  ok_prefix <- if (organism == "hsapiens") "^ENSG\\d+" else "^ENSMUSG\\d+"
  bad <- which(!is.na(ens) & !grepl(ok_prefix, ens))
  if (length(bad)) {
    stop(sprintf("EnsemblID 形式が organism(%s) と不整合な行が %d 件（例: %s）",
                 organism, length(bad),
                 paste(unique(ens[bad][1:min(3, length(bad))]), collapse=", ")))
  }

  # --- 行名 = GeneSymbol（ユニーク化） ---
  if (any(is.na(symbol))) {
    n_na_sym <- sum(is.na(symbol))
    warning(sprintf("GeneSymbol が NA の行が %d 件あります。'NA' としてユニーク化します。", n_na_sym))
  }
  rn <- make.unique(ifelse(is.na(symbol), "NA", symbol))

  # --- 行列化 ---
  assay_matrix <- as.matrix(as.data.frame(assay_df))
  storage.mode(assay_matrix) <- "double"
  stopifnot(length(rn) == nrow(assay_matrix))
  rownames(assay_matrix) <- rn
  sample_names <- colnames(assay_matrix)

  # --- rowData / colData ---
  row_data <- S4Vectors::DataFrame(
    GeneSymbol = symbol,
    EnsemblID  = ens,
    row.names  = rn
  )
  col_data <- S4Vectors::DataFrame(
    sample_id = sample_names,
    row.names = sample_names
  )

  SummarizedExperiment::SummarizedExperiment(
    assays  = list(abundance = assay_matrix),
    rowData = row_data,
    colData = col_data
  )
}


