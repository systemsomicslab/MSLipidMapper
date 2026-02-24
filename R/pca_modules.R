# ============================================================
# PCA utilities for SummarizedExperiment
# - Run PCA (prcomp) from an assay matrix
# - Score plot (PCx vs PCy) with FIXED grouping column: colData$class
# - Loading bar plot (top/bottom N) with default top_n_each = 10
#
# Changes requested:
# 1) Fix mojibake in comments/messages (use ASCII/English)
# 2) Remove "sample_class_col" flexibility: always use colData$class
# 3) Score plot legend position: bottom
# 4) FC/variance label: show explained variance (%) correctly (not sdev)
# ============================================================

suppressPackageStartupMessages({
  library(SummarizedExperiment)
  library(dplyr)
  library(ggplot2)
  library(methods)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------
# Internal: build PCA matrix from SE and run prcomp()
# - Grouping is FIXED to colData$class (only used for exclude_classes)
# ---------------------------
run_pca_se <- function(
    se,
    assay_name      = "abundance",
    exclude_classes = NULL,
    center          = TRUE,
    scale.          = TRUE
) {
  if (!methods::is(se, "SummarizedExperiment")) {
    stop("`se` must be a SummarizedExperiment object.")
  }
  
  mat <- SummarizedExperiment::assay(se, assay_name)  # rows: features, cols: samples
  cd  <- SummarizedExperiment::colData(se)
  
  # Optional sample filtering by colData$class
  keep <- rep(TRUE, ncol(mat))
  if (!is.null(exclude_classes) && "class" %in% colnames(cd)) {
    keep <- keep & !(cd[["class"]] %in% exclude_classes)
  }
  mat <- mat[, keep, drop = FALSE]
  cd  <- cd[keep, , drop = FALSE]
  
  # PCA matrix: samples x variables
  X <- t(mat)
  
  # Replace non-finite with NA; impute NA with column mean; all-NA column -> 0
  X <- apply(X, 2, function(v) {
    v[!is.finite(v)] <- NA
    if (all(is.na(v))) return(rep(0, length(v)))
    m <- mean(v, na.rm = TRUE)
    v[is.na(v)] <- m
    v
  })
  
  # Remove zero-variance variables
  vars <- apply(X, 2, stats::var, na.rm = TRUE)
  X    <- X[, vars > 0, drop = FALSE]
  
  # PCA
  rpca <- stats::prcomp(X, center = center, scale. = scale.)
  
  list(
    rpca        = rpca,
    sample_info = as.data.frame(cd),
    feature_ids = colnames(X)
  )
}

# ---------------------------
# Internal: choose a label column from rowData and return labels
# ---------------------------
get_feature_labels_from_se <- function(
    se,
    feature_ids,
    label_col = NULL
) {
  rd <- SummarizedExperiment::rowData(se)
  cn <- colnames(rd)
  
  need_auto <- is.null(label_col) ||
    !is.character(label_col) ||
    !length(label_col) ||
    is.na(label_col[1]) ||
    !nzchar(label_col[1]) ||
    !label_col[1] %in% cn
  
  if (need_auto) {
    # Candidates (metabolite/gene-friendly)
    cand <- c(
      "Metabolite name", "Metabolite.name", "Name", "name", "Alignment ID",
      "GeneSymbol", "gene_symbol", "EnsemblID", "ENSEMBL", "gene", "Gene"
    )
    hit <- cand[cand %in% cn][1]
    label_col2 <- if (length(hit)) hit else if (length(cn)) cn[1] else {
      stop("rowData(se) has no columns; cannot choose a label column.")
    }
  } else {
    label_col2 <- label_col[1]
  }
  
  if (!label_col2 %in% cn) {
    stop(
      "rowData column '", label_col2, "' was not found.\n",
      "Available columns: ", paste(cn, collapse = ", ")
    )
  }
  
  idx <- match(feature_ids, rownames(rd))
  lbl <- rd[idx, label_col2, drop = TRUE]
  as.character(lbl)
}

# ---------------------------
# Score plot (PCx vs PCy)
# - Grouping is FIXED to colData$class
# - Legend at bottom
# - Explained variance (%) is computed correctly
# ---------------------------
plot_pca_scores_from_se <- function(
    se,
    assay_name      = "abundance",
    exclude_classes = NULL,
    pc_x            = 1,
    pc_y            = 2,
    point_size      = 3
) {
  res  <- run_pca_se(
    se = se,
    assay_name = assay_name,
    exclude_classes = exclude_classes
  )
  rpca <- res$rpca
  cd   <- res$sample_info
  
  if (!"class" %in% colnames(cd)) {
    stop("colData(se) must contain a column named 'class' (grouping is fixed).")
  }
  
  pc_x <- as.integer(pc_x)
  pc_y <- as.integer(pc_y)
  if (pc_x < 1 || pc_y < 1) stop("pc_x and pc_y must be >= 1.")
  if (pc_x > ncol(rpca$x) || pc_y > ncol(rpca$x)) stop("pc_x/pc_y exceeds available PCs.")
  
  var_pct <- (rpca$sdev^2) / sum(rpca$sdev^2) * 100
  
  df <- data.frame(
    xValues = as.numeric(rpca$x[, pc_x]),
    yValues = as.numeric(rpca$x[, pc_y]),
    class   = cd[["class"]],
    sample  = rownames(rpca$x),
    stringsAsFactors = FALSE
  )
  
  xLabel <- paste0("PC", pc_x, ": ", round(var_pct[pc_x], 2), "%")
  yLabel <- paste0("PC", pc_y, ": ", round(var_pct[pc_y], 2), "%")
  
  ggplot(df, aes(x = xValues, y = yValues)) +
    geom_point(aes(fill = class), size = point_size, shape = 21, color = "black") +
    theme_classic(base_size = 14) +
    labs(x = xLabel, y = yLabel, fill = "class") +
    theme(
      aspect.ratio = 1,
      legend.position = "bottom"
    )
}

# ---------------------------
# Loading bar plot (top/bottom N)
# - default top_n_each = 10
# - Grouping is FIXED (exclude filter only)
# ---------------------------
plot_pca_loading_from_se <- function(
    se,
    pc_index        = 1,
    assay_name      = "abundance",
    exclude_classes = NULL,
    label_col       = NULL,
    top_n_each      = 10,
    title_prefix    = NULL
) {
  res         <- run_pca_se(
    se = se,
    assay_name = assay_name,
    exclude_classes = exclude_classes
  )
  rpca        <- res$rpca
  
  pc_index <- as.integer(pc_index)
  if (pc_index < 1) stop("pc_index must be >= 1.")
  if (pc_index > ncol(rpca$rotation)) stop("pc_index exceeds the number of PCA components.")
  
  pcLoading <- rpca$rotation[, pc_index]
  
  xlabels <- get_feature_labels_from_se(
    se = se,
    feature_ids = names(pcLoading),
    label_col = label_col
  )
  
  df_loading <- data.frame(
    display_labels = as.vector(xlabels),
    values         = as.numeric(pcLoading),
    stringsAsFactors = FALSE
  )
  
  # Remove duplicated labels
  df_loading <- df_loading[!duplicated(df_loading$display_labels), , drop = FALSE]
  
  # Sign type
  df_loading$value_types <- ifelse(df_loading$values < 0, "below", "above")
  
  # Sort
  df_loading <- df_loading[order(df_loading$values), , drop = FALSE]
  df_loading$display_labels <- factor(df_loading$display_labels, levels = df_loading$display_labels)
  
  # Keep top/bottom N
  df_loading <- df_loading %>% mutate(row_number = dplyr::row_number())
  if (top_n_each > 0) {
    df_loading <- df_loading %>%
      filter(row_number <= top_n_each | row_number > dplyr::n() - top_n_each)
  }
  
  # Re-sort and refactor
  df_loading <- df_loading[order(df_loading$values), , drop = FALSE]
  df_loading$display_labels <- factor(df_loading$display_labels, levels = df_loading$display_labels)
  
  base_title <- paste0("Loading plot: PC", pc_index)
  plot_title <- if (!is.null(title_prefix) && nzchar(title_prefix)) {
    paste(title_prefix, base_title, sep = "_")
  } else {
    base_title
  }
  
  ggplot(df_loading, aes(x = display_labels, y = values)) +
    geom_bar(stat = "identity", aes(fill = value_types), width = 0.8) +
    scale_fill_manual(
      name   = "Legend",
      labels = c("Positive", "Negative"),
      values = c("above" = "#00ba38", "below" = "#f8766d")
    ) +
    theme_classic(base_size = 15) +
    labs(title = plot_title, x = "Features", y = "Loading") +
    coord_flip()+
	ggplot2::geom_hline(yintercept = 0, linetype = "dashed") 
}
make_pca_loading_heatmap_from_se <- function(
    se,
    pc_index        = 1,
    assay_name      = "abundance",
    exclude_classes = NULL,
    label_col       = NULL,
    topn_heat       = 25,
    z_lim           = 2,
    colors          = NULL
) {
  res  <- run_pca_se(
    se = se,
    assay_name = assay_name,
    exclude_classes = exclude_classes
  )
  rpca <- res$rpca
  cd   <- res$sample_info

  if (!"class" %in% colnames(cd)) {
    stop("colData(se) must contain a column named 'class' (grouping is fixed).")
  }

  pc_index <- as.integer(pc_index)
  if (pc_index < 1) stop("pc_index must be >= 1.")
  if (pc_index > ncol(rpca$rotation)) stop("pc_index exceeds the number of PCA components.")

  mat <- SummarizedExperiment::assay(se, assay_name)
  cd0 <- SummarizedExperiment::colData(se)

  keep <- rep(TRUE, ncol(mat))
  if (!is.null(exclude_classes) && "class" %in% colnames(cd0)) {
    keep <- keep & !(cd0[["class"]] %in% exclude_classes)
  }
  mat <- mat[, keep, drop = FALSE]
  cd0 <- cd0[keep, , drop = FALSE]

  X <- t(mat)

  X <- apply(X, 2, function(v) {
    v[!is.finite(v)] <- NA
    if (all(is.na(v))) return(rep(0, length(v)))
    m <- mean(v, na.rm = TRUE)
    v[is.na(v)] <- m
    v
  })

  vars <- apply(X, 2, stats::var, na.rm = TRUE)
  X    <- X[, vars > 0, drop = FALSE]

  rot_names <- rownames(rpca$rotation)
  X <- X[, rot_names, drop = FALSE]

  y <- factor(as.character(cd[["class"]]))

  .norm_hex <- function(x) {
    if (is.null(x) || !nzchar(x)) return(NA_character_)
    x <- trimws(as.character(x))
    if (!startsWith(x, "#")) x <- paste0("#", x)
    if (!grepl("^#([A-Fa-f0-9]{6})$", x)) return(NA_character_)
    toupper(x)
  }
  .make_default_palette <- function(levels_vec) {
    lev <- as.character(levels_vec)
    cols <- grDevices::hcl.colors(length(lev), palette = "Dark 3")
    stats::setNames(cols, lev)
  }
  .align_colors <- function(levels_vec, colors_in = NULL) {
    lev <- as.character(levels_vec)
    cols_def <- .make_default_palette(lev)
    if (is.null(colors_in) || !length(colors_in) || is.null(names(colors_in))) return(cols_def)

    cols_in <- stats::setNames(vapply(colors_in, .norm_hex, character(1)), names(colors_in))
    cols_in <- cols_in[is.finite(match(names(cols_in), names(cols_def)))]
    cols_in <- cols_in[!is.na(cols_in)]

    cols <- cols_def
    hit  <- intersect(names(cols_in), names(cols))
    if (length(hit)) cols[hit] <- cols_in[hit]
    cols
  }
  cols <- .align_colors(levels(y), colors)

  loading <- rpca$rotation[, pc_index]
  loading <- loading[is.finite(loading)]
  if (!length(loading)) stop("No finite loadings available.")

  topn_heat <- max(5L, as.integer(topn_heat))
  ord <- order(abs(loading), decreasing = TRUE)
  sel_ids <- names(loading)[head(ord, topn_heat)]

  sel_labels <- get_feature_labels_from_se(se = se, feature_ids = sel_ids, label_col = label_col)
  sel_labels <- make.unique(as.character(sel_labels))

  mat2 <- t(X[, sel_ids, drop = FALSE])
  rownames(mat2) <- sel_labels

  row_z <- function(m) {
    m2 <- m - rowMeans(m, na.rm = TRUE)
    s  <- matrixStats::rowSds(m2, na.rm = TRUE)
    s[s == 0 | !is.finite(s)] <- 1
    m2 / s
  }
  mat_z <- row_z(mat2)

  z_lim <- as.numeric(z_lim)
  if (!is.finite(z_lim) || z_lim <= 0) z_lim <- 2
  mat_z[mat_z >  z_lim] <-  z_lim
  mat_z[mat_z < -z_lim] <- -z_lim

  load_vec <- loading[sel_ids]
  names(load_vec) <- sel_labels

  load_min <- sprintf("%.4f", min(load_vec, na.rm = TRUE))
  load_max <- sprintf("%.4f", max(load_vec, na.rm = TRUE))

  maxabs <- max(abs(load_vec), na.rm = TRUE)
  if (!is.finite(maxabs) || maxabs == 0) maxabs <- 1
  col_load <- circlize::colorRamp2(c(-maxabs, 0, maxabs), c("blue4", "white", "#c30010"))

  ha_left <- ComplexHeatmap::rowAnnotation(
    Loading = ComplexHeatmap::anno_simple(load_vec, col = col_load, border = TRUE),
    width = grid::unit(8, "mm"),
    annotation_legend_param = list(
      Loading = list(
        title = "Loading",
        direction = "horizontal",
        title_gp = grid::gpar(fontsize = 10),
        labels_gp = grid::gpar(fontsize = 9)
      )
    )
  )

  ha_top <- ComplexHeatmap::HeatmapAnnotation(
    Group = y,
    col = list(Group = cols),
    annotation_name_side = "left",
    annotation_legend_param = list(
      Group = list(
        title = "Group",
        direction = "horizontal",
        nrow = 1,
        title_gp = grid::gpar(fontsize = 10),
        labels_gp = grid::gpar(fontsize = 9)
      )
    )
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
    heatmap_legend_param = list(
      border = TRUE,
      direction = "horizontal",
      title_gp = grid::gpar(fontsize = 10),
      labels_gp = grid::gpar(fontsize = 9)
    )
  )

  list(
    ht = ht,
    loading_min = load_min,
    loading_max = load_max,
    selected_feature_ids = sel_ids,
    selected_labels = sel_labels
  )
}