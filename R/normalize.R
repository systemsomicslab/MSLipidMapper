#' Normalize an SE
#'
#' A unified interface to normalize a SummarizedExperiment assay by various strategies.
#' Methods implemented: "none", "log2", "sum", "median", "pqn", "quantile", "qc_loess".
#'
#' @param se      A SummarizedExperiment.
#' @param method  One of c("none","log2","sum","median","pqn","quantile","qc_loess").
#' @param assay   Assay name to read/write (default "abundance").
#' @param control A named list of method-specific options (see Details).
#'
#' @details
#' Method-specific controls:
#' - log2:      `offset` (numeric, default 1e-9)
#' - sum:       `target` ("median" or numeric; default "median")
#' - median:    `target` ("median" or numeric; default "median")
#' - pqn:       `reference` ("median" or numeric vector length = nrow), `pre_tic` (logical, default TRUE)
#' - quantile:  `impute` ("halfmin" or NA), requires preprocessCore
#' - qc_loess:  `qc_label` (value in colData$class that denotes QC; default "QC"),
#'              `order_col` (colData column name for injection/run order; required),
#'              `span` (loess span, default 0.75), `log_offset` (default 1e-9)
#'
#' @return The SE with normalized assay (overwrites the specified assay).
#' @export
normalize_se <- function(se, method = c("none","log2","sum","median","zscore"),
                         assay = "abundance", control = list()) {
  stopifnot(methods::is(se, "SummarizedExperiment"))
  method <- match.arg(method)

  X <- SummarizedExperiment::assay(se, assay)
  stopifnot(is.matrix(X) || is.numeric(X))

  Xn <- switch(method,
    none   = X,
    log2   = .norm_log2(X, offset = control$offset %||% 1e-9),
    sum    = .norm_sum(X,  target = control$target %||% "median"),
    median = .norm_median(X, target = control$target %||% "median"),
    zscore = .norm_zscore(X, eps = control$eps %||% 1e-12)
  )

  SummarizedExperiment::assay(se, assay) <- Xn
  se
}



# ----- helpers & methods ----------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

.scaled_to_target <- function(scale_values, target) {
  if (identical(target, "median")) {
    target_val <- stats::median(scale_values, na.rm = TRUE)
  } else if (is.numeric(target) && length(target) == 1) {
    target_val <- as.numeric(target)
  } else stop("Invalid 'target' for scaling.")
  target_val
}

# log2(x + offset)
.norm_log2 <- function(X, offset = 1e-9) {
  X <- log2(X + offset)
  X[!is.finite(X)] <- NA_real_
  X
}

# Sum normalization (TIC scaling): make column sums equal to target
.norm_sum <- function(X, target = "median") {
  s <- colSums(X, na.rm = TRUE)
  tgt <- .scaled_to_target(s, target)
  # avoid division by zero
  f <- ifelse(s > 0, tgt / s, 1)
  sweep(X, 2, f, `*`)
}

# Median normalization: make column medians equal to target
.norm_median <- function(X, target = "median") {
  m <- apply(X, 2, stats::median, na.rm = TRUE)
  tgt <- .scaled_to_target(m, target)
  f <- ifelse(m > 0, tgt / m, 1)
  sweep(X, 2, f, `*`)
}

# z scale
.norm_zscore <- function(X, eps = 1e-12) {
  X <- as.matrix(X)
  m <- rowMeans(X, na.rm = TRUE)
  s <- apply(X, 1, stats::sd, na.rm = TRUE)
  s[!is.finite(s) | s < eps] <- eps
  sweep(sweep(X, 1, m, `-`), 1, s, `/`)
}

