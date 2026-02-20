# R/oplsda_ropls_utils.R -------------------------------------------------
# OPLS-DA (ropls) reusable functions (NO install/library side-effects)
#
# Caller must have packages installed.
# This file does NOT call library().

`%||%` <- function(a, b) if (is.null(a)) b else a

# ----------------------------
# 0) Small utilities
# ----------------------------
.norm_hex <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NA_character_)
  x <- trimws(as.character(x))
  if (!startsWith(x, "#")) x <- paste0("#", x)
  if (!grepl("^#([A-Fa-f0-9]{6})$", x)) return(NA_character_)
  toupper(x)
}

.make_default_palette <- function(levels_vec) {
  k <- length(levels_vec)
  if (k <= 8 && requireNamespace("RColorBrewer", quietly = TRUE)) {
    cols <- RColorBrewer::brewer.pal(max(3, k), "Set2")[seq_len(k)]
  } else {
    cols <- grDevices::hcl.colors(k, palette = "Dark 3")
  }
  stats::setNames(cols, levels_vec)
}

.align_colors <- function(levels_vec, colors_in = NULL) {
  lev <- as.character(levels_vec)
  cols_def <- .make_default_palette(lev)

  if (is.null(colors_in) || !length(colors_in) || is.null(names(colors_in))) {
    return(cols_def)
  }

  cols_in <- stats::setNames(vapply(colors_in, .norm_hex, character(1)), names(colors_in))
  cols_in <- cols_in[is.finite(match(names(cols_in), names(cols_def)))]
  cols_in <- cols_in[!is.na(cols_in)]

  cols <- cols_def
  hit <- intersect(names(cols_in), names(cols))
  if (length(hit)) cols[hit] <- cols_in[hit]
  cols
}

# ----------------------------
# 1) Generic helpers
# ----------------------------
impute_matrix_by_col_median <- function(X, verbose = FALSE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"

  n_imputed <- 0L
  for (j in seq_len(ncol(X))) {
    colj <- X[, j]
    bad <- (!is.finite(colj)) | is.na(colj)
    if (any(bad)) {
      colj[!is.finite(colj)] <- NA
      med <- stats::median(colj, na.rm = TRUE)
      if (!is.finite(med)) med <- 0
      colj[is.na(colj)] <- med
      X[, j] <- colj
      n_imputed <- n_imputed + sum(bad)
    }
  }
  if (isTRUE(verbose)) message("[IMPUTE] imputed values: ", n_imputed)
  X
}

# ----------------------------
# 3) Input preparation (SummarizedExperiment)
# ----------------------------
prepare_opls_data_from_se <- function(
    se,
    assay_name = "abundance",
    group_col = "class",
    keep_groups = c("0", "45"),
    feature_subset = NULL,
    verbose = TRUE
) {
  if (!inherits(se, "SummarizedExperiment")) stop("Input 'se' must be a SummarizedExperiment.")
  if (!(assay_name %in% SummarizedExperiment::assayNames(se))) {
    stop(sprintf("Assay '%s' was not found in the SummarizedExperiment.", assay_name))
  }

  cd <- SummarizedExperiment::colData(se)
  if (!(group_col %in% colnames(cd))) stop(sprintf("Column '%s' was not found in colData(se).", group_col))

  grp0 <- as.character(cd[[group_col]])
  if (any(is.na(grp0))) stop("Group column in colData(se) contains NA. Please fix or choose a different group_col.")

  keep_sample <- grp0 %in% keep_groups
  if (!any(keep_sample)) stop("No samples remain after filtering by keep_groups. Please check keep_groups and group_col.")

  se2 <- se[, keep_sample, drop = FALSE]
  grp <- as.character(SummarizedExperiment::colData(se2)[[group_col]])
  y <- factor(grp, levels = keep_groups)

  if (!is.null(feature_subset)) {
    missing_feat <- setdiff(feature_subset, rownames(se2))
    if (length(missing_feat)) stop("Some feature_subset entries are missing in rownames(se): ", paste(missing_feat, collapse = ", "))
    se2 <- se2[feature_subset, , drop = FALSE]
  }

  A <- SummarizedExperiment::assay(se2, assay_name)
  X <- t(as.matrix(A))
  storage.mode(X) <- "double"

  if (is.null(colnames(X))) colnames(X) <- rownames(se2)
  if (is.null(rownames(X))) rownames(X) <- colnames(se2)

  X <- impute_matrix_by_col_median(X, verbose = verbose)

  if (ncol(X) < 2) stop("Too few features after filtering.")
  if (nrow(X) < 2) stop("Too few samples after filtering.")

  if (isTRUE(verbose)) message("[DATA:SE] samples kept: ", nrow(X), "  features: ", ncol(X))

  list(se = se2, X = X, y = y)
}

# ----------------------------
# 4) Modeling
# ----------------------------
fit_oplsda <- function(
    X, y,
    predI = 1,
    orthoI = NA,
    scaleC = "standard",
    crossvalI = 7,
    permI = 50,
    seed = 123
) {
  set.seed(seed)

  # NOTE: ropls::opls does NOT have plotL in recent Bioconductor builds.
  # Use fig.pdfC / info.txtC instead.  :contentReference[oaicite:2]{index=2}
  ropls::opls(
    X, y,
    predI = predI,
    orthoI = orthoI,
    scaleC = scaleC,
    crossvalI = crossvalI,
    permI = permI,
    fig.pdfC = "none",
    info.txtC = "none"
  )
}

extract_model_stats <- function(op) {
  mdf <- op@modelDF
  req <- c("R2X(cum)", "R2Y(cum)", "Q2(cum)")
  if (!all(req %in% colnames(mdf))) stop("Required columns (R2X/R2Y/Q2) not found in op@modelDF.")

  list(
    r2x_pred_pct  = round(mdf[1, "R2X(cum)"] * 100, 1),
    r2x_ortho_pct = if (nrow(mdf) >= 2) round((mdf[2, "R2X(cum)"] - mdf[1, "R2X(cum)"]) * 100, 1) else 0,
    r2y = round(mdf[1, "R2Y(cum)"], 3),
    q2  = round(mdf[1, "Q2(cum)"], 3),
    modelDF = mdf
  )
}

# ----------------------------
# 5) Plots
# ----------------------------
plot_opls_scores <- function(
    op, y,
    colors = NULL,
    point_size = 5,
    stroke = 1,
    alpha = 0.95,
    base_family = "sans",
    use_prism = FALSE
) {
  st <- extract_model_stats(op)

  sc  <- op@scoreMN
  osc <- op@orthoScoreMN
  if (is.null(sc) || ncol(sc) < 1) stop("op@scoreMN not available.")

  df_plot <- data.frame(
    t1 = sc[, 1],
    to1 = if (!is.null(osc) && ncol(osc) >= 1) osc[, 1] else rep(0, nrow(sc)),
    Group = y
  )

  cols <- .align_colors(levels(y), colors)

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(t1, to1, fill = Group)) +
    ggplot2::geom_point(size = point_size, shape = 21, stroke = stroke, alpha = alpha, color = "black") +
    ggplot2::labs(
      title = "OPLS-DA score plot",
      x = paste0("Predictive score (t1) [", st$r2x_pred_pct, "%]"),
      y = paste0("Orthogonal score (to1) [", st$r2x_ortho_pct, "%]")
    ) +
    ggplot2::theme_classic(base_family = base_family) +
    ggplot2::theme(aspect.ratio = 1, legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = cols, breaks = names(cols), drop = FALSE) +
    ggplot2::annotate(
      "text",
      x = -Inf, y = Inf,
      hjust = -0.05, vjust = 1.1,
      label = paste0("R2Y = ", st$r2y, "\nQ2 = ", st$q2),
      size = 4
    )

  if (isTRUE(use_prism) && requireNamespace("ggprism", quietly = TRUE)) {
    p <- p + ggprism::theme_prism(base_family = base_family)
  }
  p
}

build_vip_tables <- function(op, feature_names = NULL, vip_thres = 1) {
  vip <- op@vipVn
  if (is.null(vip)) stop("op@vipVn is NULL (VIP not available).")

  loadMN <- op@loadingMN
  if (is.null(loadMN) || ncol(loadMN) < 1) stop("op@loadingMN not available.")

  vars_from_vip  <- names(vip)
  vars_from_load <- rownames(loadMN)

  vars <- NULL
  if (!is.null(vars_from_vip) && length(vars_from_vip) == length(vip)) {
    vars <- vars_from_vip
  } else if (!is.null(vars_from_load) && length(vars_from_load) == length(vip)) {
    vars <- vars_from_load
  } else if (!is.null(feature_names) && length(feature_names) == length(vip)) {
    vars <- feature_names
  } else {
    stop("VIP length does not match any available variable name source.")
  }

  vip_tbl <- tibble::tibble(variable = vars, VIP = as.numeric(vip))
  vip_tbl <- vip_tbl[vapply(vip_tbl$VIP, is.finite, logical(1)), , drop = FALSE]

  vip_tbl_filt <- vip_tbl[vip_tbl$VIP >= vip_thres & is.finite(vip_tbl$VIP), , drop = FALSE]
  vip_tbl_filt <- vip_tbl_filt[order(vip_tbl_filt$VIP, decreasing = TRUE), , drop = FALSE]

  loading1 <- loadMN[, 1]
  names(loading1) <- rownames(loadMN)

  loading_match <- loading1[match(vip_tbl_filt$variable, names(loading1))]
  vip_tbl_signed <- vip_tbl_filt
  vip_tbl_signed$loading1 <- loading_match
  vip_tbl_signed$VIP_signed <- vip_tbl_signed$VIP * sign(vip_tbl_signed$loading1)
  vip_tbl_signed <- vip_tbl_signed[is.finite(vip_tbl_signed$VIP_signed), , drop = FALSE]

  list(
    vip_tbl = vip_tbl,
    vip_tbl_filt = vip_tbl_filt,
    vip_tbl_signed = vip_tbl_signed,
    model_vars = vars
  )
}
    
plot_signed_vip <- function(
  vip_tbl_signed,
  topn = 20,
  base_family = "sans",
  legend_title = "VIP"
) {
  if (nrow(vip_tbl_signed) == 0) stop("No VIP variables available after filtering.")

  topn2 <- min(topn, nrow(vip_tbl_signed))
  vip_top <- vip_tbl_signed %>%
    dplyr::slice_max(order_by = abs(VIP_signed), n = topn2, with_ties = FALSE) %>%
    dplyr::mutate(
      Sign = ifelse(VIP_signed > 0, "Positive", "Negative"),
      Sign = factor(Sign, levels = c("Negative", "Positive"))
    )

  ggplot2::ggplot(
    vip_top,
    ggplot2::aes(
      x = stats::reorder(variable, VIP_signed),
      y = VIP_signed,
      fill = Sign
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(
      name   = "VIP",
      labels = c("Positive", "Negative"),
      values = c("Positive" = "#00ba38", "Negative" = "#f8766d")
    ) +
    ggplot2::labs(
      title = sprintf("Top %d VIP", nrow(vip_top)),
      x = "Variable",
      y = "VIP",
      fill = legend_title
    ) +
    ggplot2::theme_classic(base_size = 15)
}


make_vip_heatmap <- function(
    X, y,
    vip_tbl, vip_tbl_filt,
    topn_heat = 25,
    z_lim = 2,
    colors = NULL
) {
  cols <- .align_colors(levels(y), colors)

  sel_vars <- if (nrow(vip_tbl_filt) > 0) {
    vip_tbl_filt$variable
  } else {
    vip_tbl2 <- vip_tbl[order(vip_tbl$VIP, decreasing = TRUE), , drop = FALSE]
    vip_tbl2$variable
  }
  sel_vars <- head(sel_vars, topn_heat)

  mat <- t(X[, sel_vars, drop = FALSE])

  row_z <- function(m) {
    m2 <- m - rowMeans(m, na.rm = TRUE)
    s  <- matrixStats::rowSds(m2, na.rm = TRUE)
    s[s == 0 | !is.finite(s)] <- 1
    m2 / s
  }
  mat_z <- row_z(mat)
  mat_z[mat_z >  z_lim] <-  z_lim
  mat_z[mat_z < -z_lim] <- -z_lim

  vip_vec <- stats::setNames(vip_tbl$VIP, vip_tbl$variable)[rownames(mat_z)]
  vip_vec[!is.finite(vip_vec)] <- NA

  vip_min <- sprintf("%.2f", min(vip_vec, na.rm = TRUE))
  vip_max <- sprintf("%.2f", max(vip_vec, na.rm = TRUE))

  vip_range <- range(vip_vec, na.rm = TRUE)
  if (!all(is.finite(vip_range)) || diff(vip_range) == 0) vip_range <- c(0, 1)
  col_vip <- circlize::colorRamp2(vip_range, c("white", "green4"))

  ha_left <- ComplexHeatmap::rowAnnotation(
    VIP = ComplexHeatmap::anno_simple(vip_vec, col = col_vip, border = TRUE),
    width = grid::unit(6, "mm")
  )

  ha_top <- ComplexHeatmap::HeatmapAnnotation(
    Group = y,
    col = list(Group = cols),
    annotation_name_side = "left",
    annotation_legend_param = list(title = "Group")
  )

  col_z <- circlize::colorRamp2(c(-z_lim, 0, z_lim), c("blue4", "#f7f7f7", "#c30010"))

  ht <- ComplexHeatmap::Heatmap(
    mat_z,
    name = "Z",
    col = col_z,
    rect_gp = grid::gpar(col = "gray45", lwd = 0.5),
    cluster_rows = FALSE,
    cluster_columns = TRUE,
    show_row_names = TRUE,
    show_column_names = FALSE,
    top_annotation = ha_top,
    left_annotation = ha_left,
    heatmap_legend_param = list(border = TRUE)
  )

  list(ht = ht, vip_min = vip_min, vip_max = vip_max)
}

# ----------------------------
# 6) Shiny-friendly wrapper
# ----------------------------
run_oplsda_objects_from_se <- function(
    se,
    assay_name = "abundance",
    group_col = "class",
    keep_groups = c("0", "45"),
    feature_subset = NULL,
    seed = 123,
    predI = 1,
    orthoI = NA,
    scaleC = "standard",
    crossvalI = 7,
    permI = 50,
    vip_thres = 1,
    topn_vip = 20,
    topn_heat = 25,
    z_lim = 2,
    base_family = "sans",
    use_prism = FALSE,
    verbose = FALSE,
    colors = NULL
) {
  dat <- prepare_opls_data_from_se(
    se = se,
    assay_name = assay_name,
    group_col = group_col,
    keep_groups = keep_groups,
    feature_subset = feature_subset,
    verbose = verbose
  )
  X <- dat$X
  y <- dat$y

  op <- fit_oplsda(
    X = X, y = y,
    predI = predI,
    orthoI = orthoI,
    scaleC = scaleC,
    crossvalI = crossvalI,
    permI = permI,
    seed = seed
  )

  cols <- .align_colors(levels(y), colors)

  p_scores <- plot_opls_scores(
    op = op, y = y,
    colors = cols,
    point_size = 5,
    stroke = 1,
    alpha = 0.95,
    base_family = base_family,
    use_prism = use_prism
  )

  vips <- build_vip_tables(op, feature_names = colnames(X), vip_thres = vip_thres)

  model_vars <- intersect(colnames(X), vips$model_vars)
  if (length(model_vars) < 2) stop("Too few variables remain after aligning X to model variables.")
  X_model <- X[, model_vars, drop = FALSE]

  p_vip <- plot_signed_vip(vips$vip_tbl_signed, topn = topn_vip, base_family = base_family)

  hm <- make_vip_heatmap(
    X = X_model, y = y,
    vip_tbl = vips$vip_tbl,
    vip_tbl_filt = vips$vip_tbl_filt,
    topn_heat = topn_heat,
    z_lim = z_lim,
    colors = cols
  )

  list(
    data = dat,
    model = op,
    model_stats = extract_model_stats(op),
    colors = cols,
    vip_tables = vips,
    plots = list(score = p_scores, vip = p_vip),
    heatmap = hm
  )
}
