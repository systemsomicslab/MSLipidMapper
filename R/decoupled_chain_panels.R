# decoupled_chain_panels.R
# ============================================================
# Utilities for "decoupled chain" scatter panels
#  - find_decoupled_chains(): build long data (C vs S) + (R vs S) + scores
#  - plot_chain_vs_total_panel(): single scatter panel (fills by sample class)
#  - export_subclass_chains_pdf(): multi-panel PDF for one subclass & many chains
#
# Assumptions:
#  - se is a SummarizedExperiment (or compatible) with:
#      * assay(se, assay_name): numeric matrix [features x samples]
#      * rowData(se)[[subclass_col]]: subclass label per feature
#      * rowData(se)[[acyl_col]]: list-column; each element is character vector of chains
#      * colData(se)$class or colData(se)$Class: sample class labels (optional)
# ============================================================

# ----------------------------
# internal helpers
# ----------------------------

.require_pkgs <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.as_chr1 <- function(x) {
  if (is.list(x)) return(vapply(x, function(z) as.character(z)[1], character(1)))
  as.character(x)
}

.trim_chr <- function(x) trimws(as.character(x))

.feature_has_chain <- function(acyl_list, chain) {
  # acyl_list: list-column; each element: character vector of chains
  chain <- trimws(as.character(chain))
  vapply(acyl_list, function(x) {
    if (is.null(x)) return(FALSE)
    xx <- trimws(as.character(x))
    any(xx == chain)
  }, logical(1))
}

.infer_class_col <- function(cd) {
  # prefer "class" then "Class"
  if ("class" %in% names(cd)) return("class")
  if ("Class" %in% names(cd)) return("Class")
  NULL
}

.safe_log_ratio <- function(S, R, pseudo = 1) {
  # log((S+p)/(R+p)) in natural log
  S <- as.numeric(S); R <- as.numeric(R)
  p <- as.numeric(pseudo)
  if (!is.finite(p) || p <= 0) p <- 1
  log((S + p) / (R + p))
}

# ============================================================
# 1) Build decoupled chain candidates from SE
# ============================================================
find_decoupled_chains <- function(se,
                                 chains = c("18:2","20:4","22:6"),
                                 assay_name = "abundance",
                                 subclass_col = "subclass",
                                 acyl_col = "acyl_chains",
                                 exclude_sample_classes = c("QC","Blank"),
                                 log1p = TRUE,
                                 pseudo = 1,
                                 min_species_total = 5,
                                 min_species_chain = 3,
                                 min_chain_fraction_median = 0.01,
                                 corr_method = c("pearson","spearman"),
                                 use_lm_r2 = TRUE,
                                 score_against = c("total","rest")) {

  .require_pkgs(c("SummarizedExperiment", "dplyr", "tibble"))
  corr_method  <- match.arg(corr_method)
  score_against <- match.arg(score_against)

  stopifnot(assay_name %in% SummarizedExperiment::assayNames(se))
  A <- SummarizedExperiment::assay(se, assay_name)
  if (!is.matrix(A)) A <- as.matrix(A)

  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!subclass_col %in% names(rd)) stop("rowData missing: ", subclass_col, call. = FALSE)
  if (!acyl_col %in% names(rd))     stop("rowData missing: ", acyl_col, call. = FALSE)

  subclass  <- as.character(rd[[subclass_col]])
  acyl_list <- rd[[acyl_col]]
  if (!is.list(acyl_list)) stop("rowData$", acyl_col, " must be a list-column.", call. = FALSE)

  # ---- sample filter using colData (class / Class)
  cd <- as.data.frame(SummarizedExperiment::colData(se))
  class_col <- .infer_class_col(cd)

  keep_samp <- rep(TRUE, ncol(A))
  if (!is.null(class_col) && length(exclude_sample_classes)) {
    keep_samp <- !(as.character(cd[[class_col]]) %in% exclude_sample_classes)
  }

  A <- A[, keep_samp, drop = FALSE]
  samp_names <- colnames(A)

  # sample metadata map AFTER filtering
  cd2 <- cd[keep_samp, , drop = FALSE]

  # If rownames(cd2) are sample names, reorder to match colnames(A)
  if (!is.null(rownames(cd2)) &&
      length(rownames(cd2)) == nrow(cd2) &&
      all(samp_names %in% rownames(cd2))) {
    cd2 <- cd2[samp_names, , drop = FALSE]
  }

  sample_map <- tibble::tibble(sample = samp_names)
  if (!is.null(class_col)) {
    sample_map <- dplyr::mutate(sample_map, class = as.character(cd2[[class_col]]))
  } else {
    sample_map <- dplyr::mutate(sample_map, class = NA_character_)
  }

  # ---- work scale (per-feature)
  A_work <- if (isTRUE(log1p)) log10(pmax(A, 0) + pseudo) else A

  idx_by_sc <- split(seq_len(nrow(A_work)), subclass)

  # ---- subset S and rest R per chain (and C total)
  out_long <- list()
  chains <- unique(.trim_chr(chains))
  chains <- chains[nzchar(chains)]
  if (!length(chains)) stop("chains is empty.", call. = FALSE)

  for (ch in chains) {
    has_ch <- .feature_has_chain(acyl_list, ch)

    SR_df <- dplyr::bind_rows(lapply(names(idx_by_sc), function(sc) {
      idx_all <- idx_by_sc[[sc]]
      if (length(idx_all) < min_species_total) return(NULL)

      idx_ch   <- idx_all[has_ch[idx_all]]
      if (length(idx_ch) < min_species_chain) return(NULL)

      idx_rest <- idx_all[!has_ch[idx_all]]
      # NOTE: idx_rest can be length 0 (rare) -> R becomes 0 (or 0-length sum)
      C <- as.numeric(colSums(A_work[idx_all,  , drop = FALSE], na.rm = TRUE))
      S <- as.numeric(colSums(A_work[idx_ch,   , drop = FALSE], na.rm = TRUE))
      R <- if (length(idx_rest) > 0) {
        as.numeric(colSums(A_work[idx_rest, , drop = FALSE], na.rm = TRUE))
      } else {
        rep(0, length(samp_names))
      }

      tibble::tibble(
        subclass = sc,
        sample   = samp_names,
        chain    = ch,
        C        = C,
        S        = S,
        R        = R,
        n_total  = length(idx_all),
        n_chain  = length(idx_ch),
        n_rest   = length(idx_rest)
      )
    }))

    if (nrow(SR_df) == 0) next

    tmp <- SR_df |>
      dplyr::mutate(
        frac_total = ifelse(is.finite(C) & C != 0, S / C, NA_real_),    # old-style fraction vs total
        frac_parts = ifelse(is.finite(S + R) & (S + R) != 0, S / (S + R), NA_real_), # composition fraction using parts
        log_ratio  = .safe_log_ratio(S, R, pseudo = pseudo)            # log((S+p)/(R+p))
      ) |>
      dplyr::group_by(subclass, chain, n_total, n_chain, n_rest) |>
      dplyr::filter(stats::median(frac_total, na.rm = TRUE) >= min_chain_fraction_median) |>
      dplyr::ungroup()

    out_long[[ch]] <- tmp
  }

  dat <- dplyr::bind_rows(out_long)
  if (nrow(dat) == 0) {
    stop("No pairs passed filters. Lower min_species_* or fraction filter.", call. = FALSE)
  }

  # Attach sample class
  dat <- dplyr::left_join(dat, sample_map, by = "sample")

  # ---- score decoupling (both total and rest)
  res <- dat |>
    dplyr::group_by(subclass, chain) |>
    dplyr::summarise(
      n_samples = sum(is.finite(C) & is.finite(S) & is.finite(R)),
      n_total   = max(n_total, na.rm = TRUE),
      n_chain   = max(n_chain, na.rm = TRUE),
      n_rest    = max(n_rest, na.rm = TRUE),

      # total-based: S ~ C
      cor_total = {
        x <- C; y <- S
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) < 3) NA_real_ else suppressWarnings(stats::cor(x[ok], y[ok], method = corr_method))
      },
      r2_total = {
        if (!use_lm_r2) return(NA_real_)
        x <- C; y <- S
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) < 3) NA_real_ else as.numeric(summary(stats::lm(y[ok] ~ x[ok]))$r.squared)
      },
      slope_total = {
        x <- C; y <- S
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) < 3) NA_real_ else as.numeric(stats::coef(stats::lm(y[ok] ~ x[ok]))[2])
      },

      # rest-based: S ~ R
      cor_rest = {
        x <- R; y <- S
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) < 3) NA_real_ else suppressWarnings(stats::cor(x[ok], y[ok], method = corr_method))
      },
      r2_rest = {
        if (!use_lm_r2) return(NA_real_)
        x <- R; y <- S
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) < 3) NA_real_ else as.numeric(summary(stats::lm(y[ok] ~ x[ok]))$r.squared)
      },
      slope_rest = {
        x <- R; y <- S
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) < 3) NA_real_ else as.numeric(stats::coef(stats::lm(y[ok] ~ x[ok]))[2])
      },

      frac_total_median = stats::median(frac_total, na.rm = TRUE),
      frac_total_iqr    = stats::IQR(frac_total, na.rm = TRUE),
      frac_parts_median = stats::median(frac_parts, na.rm = TRUE),
      frac_parts_iqr    = stats::IQR(frac_parts, na.rm = TRUE),

      log_ratio_median  = stats::median(log_ratio, na.rm = TRUE),
      log_ratio_iqr     = stats::IQR(log_ratio, na.rm = TRUE),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      decouple_total = ifelse(is.finite(r2_total), 1 - r2_total,
                              ifelse(is.finite(cor_total), 1 - abs(cor_total), NA_real_)),
      decouple_rest  = ifelse(is.finite(r2_rest),  1 - r2_rest,
                              ifelse(is.finite(cor_rest),  1 - abs(cor_rest),  NA_real_)),
      decouple = if (score_against == "rest") decouple_rest else decouple_total,
      score_against = score_against
    ) |>
    dplyr::arrange(dplyr::desc(decouple), dplyr::desc(n_samples))

  list(
    data_long  = dat,        # subclass, chain, sample, C, S, R, frac_total, frac_parts, log_ratio, class
    results    = res,
    sample_map = sample_map  # sample -> class
  )
}

# ============================================================
# 2) Single panel plotter
# ============================================================
plot_chain_vs_total_panel <- function(out,
                                      subclass_value,
                                      chain_value,
                                      add_lm = TRUE,
                                      corr_method = c("pearson","spearman"),
                                      compare_to = c("total","rest"),
                                      # outlier options
                                      drop_outliers = FALSE,
                                      outlier_method = c("none","mad_resid","iqr_resid","cook"),
                                      outlier_k = 4,
                                      point_alpha = 0.8,
                                      point_size = 2.2) {

  .require_pkgs(c("dplyr", "ggplot2"))

  corr_method    <- match.arg(corr_method)
  compare_to     <- match.arg(compare_to)
  outlier_method <- match.arg(outlier_method)

  dl <- as.data.frame(out$data_long)

  # robust coercions
  dl$subclass <- .as_chr1(dl$subclass)
  dl$chain    <- .as_chr1(dl$chain)

  sv <- .trim_chr(subclass_value)
  cv <- .trim_chr(chain_value)

  df <- dl |>
    dplyr::mutate(
      subclass = .trim_chr(subclass),
      chain    = .trim_chr(chain)
    ) |>
    dplyr::filter(.data$subclass == sv, .data$chain == cv)

  if (nrow(df) == 0) stop("No rows for subclass=", sv, " chain=", cv, call. = FALSE)

  df$C <- as.numeric(df$C)
  df$S <- as.numeric(df$S)
  df$R <- as.numeric(df$R)

  xvar <- if (compare_to == "rest") "R" else "C"
  df$X <- as.numeric(df[[xvar]])

  ok <- is.finite(df$X) & is.finite(df$S)
  df <- df[ok, , drop = FALSE]
  if (nrow(df) < 3) stop("Too few points (n<3) for subclass=", sv, " chain=", cv, call. = FALSE)

  # optional outlier removal based on S~X
  if (isTRUE(drop_outliers) && outlier_method != "none" && nrow(df) >= 6) {
    fit <- stats::lm(S ~ X, data = df)
    keep <- rep(TRUE, nrow(df))

    if (outlier_method %in% c("mad_resid", "iqr_resid")) {
      r <- stats::resid(fit)

      if (outlier_method == "mad_resid") {
        med  <- stats::median(r, na.rm = TRUE)
        madv <- stats::mad(r, constant = 1.4826, na.rm = TRUE)
        if (is.finite(madv) && madv > 0) keep <- abs(r - med) <= outlier_k * madv
      } else {
        q <- stats::quantile(r, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        if (is.finite(iqr) && iqr > 0) {
          lo <- q[1] - outlier_k * iqr
          hi <- q[2] + outlier_k * iqr
          keep <- (r >= lo) & (r <= hi)
        }
      }
    } else if (outlier_method == "cook") {
      ck  <- stats::cooks.distance(fit)
      thr <- outlier_k / nrow(df)  # default k=4 -> 4/n
      keep <- ck <= thr
    }

    if (any(!keep, na.rm = TRUE)) df <- df[keep, , drop = FALSE]
  }

  # stats (after filtering)
  fit2 <- stats::lm(S ~ X, data = df)
  corv <- suppressWarnings(stats::cor(df$X, df$S, method = corr_method))
  r2v  <- suppressWarnings(summary(fit2)$r.squared)
  ann  <- sprintf("cor = %.2f\nR² = %.2f", corv, r2v)

  # ensure class exists for fill
  if (!"class" %in% names(df)) df$class <- NA_character_

  xlab <- if (compare_to == "rest") {
    "Rest (R): subclass total excluding the chain-subset"
  } else {
    "Class total expression (C)"
  }

  ttl_suffix <- if (compare_to == "rest") "S vs R" else "S vs C"

  p <- ggplot2::ggplot(df, ggplot2::aes(x = X, y = S)) +
    ggplot2::geom_point(
      ggplot2::aes(fill = class),
      shape = 21, color = "black",
      alpha = point_alpha, size = point_size
    ) +
    ggplot2::labs(
      title = paste0(sv, " | with ", cv, " (", ttl_suffix, ")"),
      x = xlab,
      y = "Chain-subset total expression (S)",
      fill = "Class"
    ) +
    ggplot2::theme_classic() +
    ggplot2::annotate(
      "text",
      x = -Inf, y = Inf, label = ann,
      hjust = -0.05, vjust = 1.05, size = 3.2
    ) +
    ggplot2::theme(aspect.ratio = 1)

  if (isTRUE(add_lm)) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, color = "black")
  }

  p
}

# ============================================================
# 3) Export: one subclass, many chains (PDF)
# ============================================================
export_subclass_chains_pdf <- function(out,
                                       subclass,
                                       chains,
                                       out_pdf,
                                       ncol = 2,
                                       width = 11,
                                       height = 8.5,
                                       add_lm = TRUE,
                                       corr_method = "pearson",
                                       compare_to = c("total","rest"),
                                       # outlier options
                                       drop_outliers = FALSE,
                                       outlier_method = "mad_resid",
                                       outlier_k = 4,
                                       # behavior
                                       skip_missing = TRUE,
                                       point_alpha = 0.8,
                                       point_size = 2.2) {

  .require_pkgs(c("patchwork", "ggplot2"))

  compare_to <- match.arg(compare_to)

  subclass <- .trim_chr(subclass)
  chains   <- unique(.trim_chr(chains))
  chains   <- chains[nzchar(chains)]
  if (!length(chains)) stop("chains is empty.", call. = FALSE)

  plots <- list()
  for (ch in chains) {
    p <- try(
      plot_chain_vs_total_panel(
        out,
        subclass_value = subclass,
        chain_value    = ch,
        add_lm         = add_lm,
        corr_method    = corr_method,
        compare_to     = compare_to,
        drop_outliers  = drop_outliers,
        outlier_method = outlier_method,
        outlier_k      = outlier_k,
        point_alpha    = point_alpha,
        point_size     = point_size
      ),
      silent = TRUE
    )

    if (inherits(p, "try-error")) {
      msg <- paste0("[SKIP] ", subclass, " | ", ch, " : ", as.character(p))
      if (isTRUE(skip_missing)) message(msg) else stop(msg, call. = FALSE)
      next
    }
    plots[[length(plots) + 1]] <- p
  }

  if (!length(plots)) stop("No plots created. Check subclass/chain names.", call. = FALSE)

  patch <- patchwork::wrap_plots(plots, ncol = ncol)
  ggplot2::ggsave(out_pdf, patch, width = width, height = height, units = "in")
  message("[SAVED] ", out_pdf)

  invisible(out_pdf)
}

# ============================================================
# (Optional) Convenience wrapper
# ============================================================
run_decoupled_chain_panels <- function(se,
                                      subclass,
                                      chains,
                                      out_pdf,
                                      # find params
                                      find_chains = chains,
                                      assay_name = "abundance",
                                      subclass_col = "subclass",
                                      acyl_col = "acyl_chains",
                                      exclude_sample_classes = c("QC","Blank"),
                                      log1p = TRUE,
                                      pseudo = 1,
                                      min_species_total = 5,
                                      min_species_chain = 3,
                                      min_chain_fraction_median = 0.01,
                                      corr_method = "pearson",
                                      use_lm_r2 = TRUE,
                                      score_against = c("total","rest"),
                                      # plot/export params
                                      compare_to = c("total","rest"),
                                      ncol = 2,
                                      width = 11,
                                      height = 8.5,
                                      add_lm = TRUE,
                                      drop_outliers = FALSE,
                                      outlier_method = "mad_resid",
                                      outlier_k = 4,
                                      skip_missing = TRUE,
                                      point_alpha = 0.8,
                                      point_size = 2.2) {

  score_against <- match.arg(score_against)
  compare_to    <- match.arg(compare_to)

  out <- find_decoupled_chains(
    se,
    chains = find_chains,
    assay_name = assay_name,
    subclass_col = subclass_col,
    acyl_col = acyl_col,
    exclude_sample_classes = exclude_sample_classes,
    log1p = log1p,
    pseudo = pseudo,
    min_species_total = min_species_total,
    min_species_chain = min_species_chain,
    min_chain_fraction_median = min_chain_fraction_median,
    corr_method = corr_method,
    use_lm_r2 = use_lm_r2,
    score_against = score_against
  )

  export_subclass_chains_pdf(
    out,
    subclass = subclass,
    chains = chains,
    out_pdf = out_pdf,
    ncol = ncol,
    width = width,
    height = height,
    add_lm = add_lm,
    corr_method = corr_method,
    compare_to = compare_to,
    drop_outliers = drop_outliers,
    outlier_method = outlier_method,
    outlier_k = outlier_k,
    skip_missing = skip_missing,
    point_alpha = point_alpha,
    point_size = point_size
  )

  invisible(out)
}