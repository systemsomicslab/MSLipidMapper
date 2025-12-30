# ======================================================================
# Helper: Class-level bar plot (for Alt view)
# ======================================================================

#' Bar plot for molecules in a selected lipid class (mean ± SE/SD)
#'
#' This function is used in mod_plot_lipid_server() as an alternative view
#' ("Bar plot") for the "Top molecules in selected lipid class" card.
#'
#' @param se SummarizedExperiment.
#' @param lipid_class Target lipid class name (e.g., "PC").
#' @param assay_name Assay name (default: "abundance").
#' @param lipid_class_col rowData column storing class (e.g., "Ontology").
#' @param feature_label_col rowData column used for x labels (e.g., "Metabolite name").
#' @param sample_class_col colData column storing sample groups (e.g., "class").
#' @param use_se If TRUE, error bars are SE; otherwise SD.
#' @param top_n If not NULL, keep top-N features by overall mean across groups.
#' @param class_colors Optional named vector for fill colors (names = class levels).
#' @param class_order Optional character vector to control class (fill/legend) order.
#' @param feature_desc If TRUE, order features by mean_overall descending.
#' @param x_text_angle X-axis label angle (default: 45).
#' @param x_text_hjust X-axis label hjust (default: 1).
#' @param bottom_margin_pt Extra bottom margin in points to avoid clipping (default: 18).
#'
#' @return ggplot object.
#' @export
plot_lipid_class_bar <- function(
    se,
    lipid_class       = "PC",
    assay_name        = "abundance",
    lipid_class_col   = "Ontology",
    feature_label_col = "Metabolite name",
    sample_class_col  = "class",
    use_se            = TRUE,
    top_n             = NULL,
    class_colors      = NULL,
    class_order       = NULL,
    feature_desc      = TRUE,
    x_text_angle      = 45,
    x_text_hjust      = 1,
    bottom_margin_pt  = 18
) {
  if (!methods::is(se, "SummarizedExperiment")) {
    stop("`se` must be a SummarizedExperiment.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2.")
  if (!requireNamespace("tidyr", quietly = TRUE))   stop("Please install tidyr.")
  if (!requireNamespace("dplyr", quietly = TRUE))   stop("Please install dplyr.")
  if (!requireNamespace("forcats", quietly = TRUE)) stop("Please install forcats.")
  
  rd  <- SummarizedExperiment::rowData(se)
  cd  <- SummarizedExperiment::colData(se)
  
  if (!assay_name %in% SummarizedExperiment::assayNames(se)) {
    stop("Assay not found: ", assay_name)
  }
  if (!lipid_class_col %in% colnames(rd))   stop("rowData has no column: ", lipid_class_col)
  if (!feature_label_col %in% colnames(rd)) stop("rowData has no column: ", feature_label_col)
  if (!sample_class_col %in% colnames(cd))  stop("colData has no column: ", sample_class_col)
  
  mat <- SummarizedExperiment::assay(se, assay_name)
  
  idx <- which(as.character(rd[[lipid_class_col]]) == lipid_class)
  if (!length(idx)) stop("No rows found for lipid_class: ", lipid_class)
  
  mat_sub       <- mat[idx, , drop = FALSE]
  feature_label <- as.character(rd[[feature_label_col]][idx])
  
  df_long <- as.data.frame(mat_sub, check.names = FALSE)
  df_long$feature_id    <- seq_len(nrow(df_long))
  df_long$feature_label <- feature_label
  
  df_long <- tidyr::pivot_longer(
    df_long,
    cols      = -c(feature_id, feature_label),
    names_to  = "sample_id",
    values_to = "value"
  )
  
  sample_info <- data.frame(
    sample_id = colnames(se),
    class     = as.character(cd[[sample_class_col]]),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(class_order) && length(class_order)) {
    sample_info$class <- factor(sample_info$class, levels = unique(class_order), ordered = TRUE)
  }
  
  df_long <- dplyr::left_join(df_long, sample_info, by = "sample_id")
  if (anyNA(df_long$class)) {
    stop("Some samples could not be matched to class (NA). Check colnames(se) and sample IDs.")
  }
  
  summary_df <- df_long |>
    dplyr::group_by(.data$feature_label, .data$class) |>
    dplyr::summarise(
      n    = dplyr::n(),
      mean = mean(.data$value, na.rm = TRUE),
      sd   = stats::sd(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$feature_label) |>
    dplyr::mutate(mean_overall = mean(.data$mean, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      se  = .data$sd / sqrt(.data$n),
      err = if (isTRUE(use_se)) .data$se else .data$sd
    )
  
  # ---- top_n (FIXED: no dplyr::n() used outside data-masking verbs) ----
  if (!is.null(top_n) && is.numeric(top_n) && top_n > 0) {
    keep_df <- summary_df |>
      dplyr::distinct(.data$feature_label, .data$mean_overall) |>
      dplyr::arrange(dplyr::desc(.data$mean_overall))
    
    n_keep <- min(as.integer(top_n), nrow(keep_df))
    
    keep_feats <- keep_df |>
      dplyr::slice_head(n = n_keep) |>
      dplyr::pull(.data$feature_label)
    
    summary_df <- dplyr::filter(summary_df, .data$feature_label %in% keep_feats)
  }
  
  summary_df <- summary_df |>
    dplyr::mutate(
      feature_label = forcats::fct_reorder(.data$feature_label, .data$mean_overall, .desc = isTRUE(feature_desc))
    )
  
  # ---- Build plot ----
  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = .data$feature_label, y = .data$mean, fill = .data$class)) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.7),
      width    = 0.6,
      color    = "black"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$mean - .data$err, ymax = .data$mean + .data$err),
      position = ggplot2::position_dodge(width = 0.7),
      width    = 0.2
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = x_text_angle, hjust = x_text_hjust, vjust = 1),
      legend.position = "right",
      plot.margin     = ggplot2::margin(t = 6, r = 6, b = bottom_margin_pt, l = 6, unit = "pt")
    ) +
    ggplot2::labs(
      title = lipid_class,
      x     = NULL,
      y     = if (isTRUE(use_se)) "Abundance (mean ± SE)" else "Abundance (mean ± SD)",
      fill  = sample_class_col
    ) +
    # Helps avoid clipping when labels are long and rotated.
    ggplot2::coord_cartesian(clip = "off")
  
  if (!is.null(class_colors)) {
    if (is.null(names(class_colors)) || any(names(class_colors) == "")) {
      stop("`class_colors` must be a named character vector: names = class levels, values = colors.")
    }
    p <- p + ggplot2::scale_fill_manual(values = class_colors, drop = FALSE)
  }
  
  p
}

# ======================================================================
# p-value annotation (no extra packages)
# ----------------------------------------------------------------------
# Add p-value layer with brackets
# - Uses p$data (the tidy data passed to ggplot)
# - Draws brackets via geom_segment + geom_text
# ======================================================================
.add_p_layer <- function(p, x_var,
                         add_p       = FALSE,
                         test        = "wilcox",
                         comparisons = NULL,
                         ref_group   = NULL,
                         p_label     = c("both","p","stars"),
                         p_adjust    = "BH",
                         bracket_tip_frac = 0.02,
                         step_frac        = 0.08,
                         text_nudge_frac  = 0.02,
                         digits          = 3) {
  
  if (!isTRUE(add_p)) return(p)
  
  p_label <- match.arg(p_label)
  df <- p$data
  if (is.null(df) || !is.data.frame(df)) return(p)
  if (!x_var %in% names(df)) return(p)
  if (!"abundance" %in% names(df)) return(p)
  
  # factor levels -> x positions
  if (!is.factor(df[[x_var]])) df[[x_var]] <- factor(df[[x_var]])
  levs <- levels(df[[x_var]])
  if (length(levs) < 2) return(p)
  
  y_all <- df$abundance
  y_all <- y_all[is.finite(y_all)]
  if (!length(y_all)) return(p)
  
  y_min <- min(y_all, na.rm = TRUE)
  y_max <- max(y_all, na.rm = TRUE)
  y_rng <- y_max - y_min
  if (!is.finite(y_rng) || y_rng == 0) y_rng <- max(abs(y_max), 1)
  
  # decide comparisons
  if (is.null(comparisons)) {
    if (!is.null(ref_group) && nzchar(ref_group) && ref_group %in% levs) {
      others <- setdiff(levs, ref_group)
      comparisons <- lapply(others, function(g) c(ref_group, g))
    } else {
      comparisons <- utils::combn(levs, 2, simplify = FALSE)
    }
  }
  
  # compute p-values
  get_p <- function(g1, g2) {
    x <- df$abundance[df[[x_var]] == g1]
    y <- df$abundance[df[[x_var]] == g2]
    x <- x[is.finite(x)]; y <- y[is.finite(y)]
    if (length(x) < 2 || length(y) < 2) return(NA_real_)
    
    if (tolower(test) %in% c("t", "t.test", "ttest")) {
      out <- try(stats::t.test(x, y), silent = TRUE)
    } else {
      out <- try(stats::wilcox.test(x, y), silent = TRUE)
    }
    if (inherits(out, "try-error")) return(NA_real_)
    as.numeric(out$p.value)
  }
  
  comp_ok <- Filter(function(v) length(v) == 2 && all(v %in% levs), comparisons)
  if (!length(comp_ok)) return(p)
  
  raw_p <- vapply(comp_ok, function(v) get_p(v[1], v[2]), numeric(1))
  adj_p <- stats::p.adjust(raw_p, method = p_adjust)
  
  p_to_stars <- function(pp) {
    if (!is.finite(pp)) return("NA")
    if (pp <= 0.0001) return("****")
    if (pp <= 0.001)  return("***")
    if (pp <= 0.01)   return("**")
    if (pp <= 0.05)   return("*")
    "ns"
  }
  
  fmt_p <- function(pp) {
    if (!is.finite(pp)) return("p=NA")
    if (pp < 10^(-digits)) return(paste0("p<", format(10^(-digits), scientific = FALSE)))
    paste0("p=", formatC(pp, digits = digits, format = "g"))
  }
  
  labels <- mapply(function(pp) {
    if (p_label == "stars") return(p_to_stars(pp))
    if (p_label == "p")     return(fmt_p(pp))
    paste0(fmt_p(pp), " (", p_to_stars(pp), ")")
  }, adj_p, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  
  # layout y positions (stack)
  step  <- step_frac * y_rng
  tip   <- bracket_tip_frac * y_rng
  nud   <- text_nudge_frac  * y_rng
  if (!is.finite(step) || step <= 0) step <- 0.2 * y_rng
  if (!is.finite(tip)  || tip  <= 0) tip  <- 0.05 * y_rng
  if (!is.finite(nud)  || nud  <= 0) nud  <- 0.05 * y_rng
  
  base_y <- y_max + step
  y_pos  <- base_y + (seq_along(comp_ok) - 1) * step
  
  x1 <- vapply(comp_ok, function(v) match(v[1], levs), integer(1))
  x2 <- vapply(comp_ok, function(v) match(v[2], levs), integer(1))
  xmin <- pmin(x1, x2); xmax <- pmax(x1, x2)
  
  df_anno <- data.frame(
    xmin  = xmin,
    xmax  = xmax,
    y     = y_pos,
    ytip  = y_pos - tip,
    label = labels,
    stringsAsFactors = FALSE
  )
  df_anno <- df_anno[is.finite(df_anno$y) & nzchar(df_anno$label), , drop = FALSE]
  if (!nrow(df_anno)) return(p)
  
  p +
    ggplot2::geom_segment(
      data = df_anno,
      ggplot2::aes(x = xmin, xend = xmax, y = y, yend = y),
      inherit.aes = FALSE, linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = df_anno,
      ggplot2::aes(x = xmin, xend = xmin, y = y, yend = ytip),
      inherit.aes = FALSE, linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = df_anno,
      ggplot2::aes(x = xmax, xend = xmax, y = y, yend = ytip),
      inherit.aes = FALSE, linewidth = 0.5
    ) +
    ggplot2::geom_text(
      data = df_anno,
      ggplot2::aes(x = (xmin + xmax) / 2, y = y + nud, label = label),
      inherit.aes = FALSE, size = 3.5, vjust = 0
    )
}

# ======================================================================
# Global helpers used by Dot/Box/Violin/Heatmap
# ======================================================================

#' Convert SummarizedExperiment to a tidy long table
#' - columns: feature_id, sample_id, abundance, + colData columns (merged)
#' @keywords internal
.tidy_from_se_global <- function(se, assay_name = "abundance") {
  if (!methods::is(se, "SummarizedExperiment")) {
    stop("`se` must be a SummarizedExperiment.")
  }
  if (!assay_name %in% SummarizedExperiment::assayNames(se)) {
    stop("Assay not found: ", assay_name)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Please install tidyr.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install dplyr.")
  
  mat <- SummarizedExperiment::assay(se, assay_name)
  if (is.null(rownames(mat))) rownames(mat) <- rownames(se)
  if (is.null(colnames(mat))) colnames(mat) <- colnames(se)
  
  df <- as.data.frame(mat, check.names = FALSE)
  df$feature_id <- rownames(df)
  
  tidy <- tidyr::pivot_longer(
    df,
    cols      = -feature_id,
    names_to  = "sample_id",
    values_to = "abundance"
  )
  
  cd <- as.data.frame(SummarizedExperiment::colData(se))
  cd$sample_id <- rownames(cd)
  if (is.null(cd$sample_id)) cd$sample_id <- colnames(se)
  
  tidy <- dplyr::left_join(tidy, cd, by = "sample_id")
  tidy
}

#' Set x-axis order for plots
#' - If x_order is given: use it (others appended)
#' - Else: order by summary of abundance per group
#' @keywords internal
.set_x_order_for_plot <- function(tidy,
                                  x_var = "class",
                                  x_order = NULL,
                                  order_by = c("none","abundance_mean","abundance_median","alphabetical"),
                                  decreasing = FALSE) {
  if (!is.data.frame(tidy)) return(tidy)
  if (!x_var %in% names(tidy)) return(tidy)
  order_by <- match.arg(order_by)
  
  x <- tidy[[x_var]]
  if (!is.factor(x)) x <- factor(x)
  levs <- levels(x)
  
  # explicit order
  if (!is.null(x_order) && length(x_order)) {
    x_order <- as.character(x_order)
    levs_new <- unique(c(x_order[x_order %in% levs], levs[!levs %in% x_order]))
    tidy[[x_var]] <- factor(as.character(tidy[[x_var]]), levels = levs_new)
    return(tidy)
  }
  
  if (order_by == "none") {
    tidy[[x_var]] <- factor(as.character(tidy[[x_var]]), levels = levs)
    return(tidy)
  }
  
  if (order_by == "alphabetical") {
    levs_new <- sort(unique(as.character(tidy[[x_var]])))
    tidy[[x_var]] <- factor(as.character(tidy[[x_var]]), levels = levs_new)
    return(tidy)
  }
  
  # abundance-based order
  if (!"abundance" %in% names(tidy)) {
    tidy[[x_var]] <- factor(as.character(tidy[[x_var]]), levels = levs)
    return(tidy)
  }
  
  stat_fun <- if (order_by == "abundance_median") stats::median else base::mean
  grp <- split(tidy$abundance, tidy[[x_var]])
  sc <- vapply(grp, function(v) stat_fun(v, na.rm = TRUE), numeric(1))
  sc <- sc[is.finite(sc)]
  
  levs_new <- names(sort(sc, decreasing = isTRUE(decreasing)))
  # include missing levels (all NA groups etc.)
  levs_new <- unique(c(levs_new, levs[!levs %in% levs_new]))
  
  tidy[[x_var]] <- factor(as.character(tidy[[x_var]]), levels = levs_new)
  tidy
}

# ======================================================================
# Dot / Box / Violin plot functions
# ======================================================================

#' Dot plot for a single feature from SE
#' @export
plot_dot_se <- function(se, feature_id,
                        x_var        = "class",
                        x_order      = NULL,
                        order_by     = c("none","abundance_mean","abundance_median","alphabetical"),
                        decreasing   = FALSE,
                        palette      = NULL,
                        add_p        = FALSE,
                        test         = "wilcox",
                        comparisons  = NULL,
                        ref_group    = NULL,
                        p_adjust     = "BH",
                        p_label      = "both",
                        point_size   = 2.0,
                        jitter_width = 0.15,
                        point_alpha  = 0.9,
                        show_median  = TRUE,
                        median_size  = 0.7,
                        median_width = 0.6,
                        median_color = "#222222") {
  
  tidy <- .tidy_from_se_global(se)
  tidy <- tidy[tidy$feature_id %in% feature_id, , drop = FALSE]
  
  if (!nrow(tidy)) {
    stop("feature_id = ", feature_id, " ????????????????????????????????????")
  }
  
  tidy <- .set_x_order_for_plot(tidy, x_var = x_var,
                                x_order = x_order,
                                order_by = order_by,
                                decreasing = decreasing)
  
  aes_base <- ggplot2::aes(
    x    = .data[[x_var]],
    y    = .data$abundance,
    fill = .data[[x_var]]
  )
  
  p <- ggplot2::ggplot(tidy, aes_base) +
    ggplot2::geom_jitter(
      width  = jitter_width,
      height = 0,
      size   = point_size,
      alpha  = point_alpha,
      shape  = 21,
      colour = "black"   # ??????(??????)
    )
  
  if (!is.null(palette) && length(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette, drop = FALSE)
  }
  
  if (isTRUE(show_median)) {
    p <- p +
      ggplot2::stat_summary(
        fun    = stats::median,
        geom   = "crossbar",
        width  = median_width,
        colour = median_color,
        size   = median_size
      )
  }
  
  p <- p +
    ggplot2::labs(x = x_var, y = "Abundance") +
    theme_lipidomics()
  
  p <- .add_p_layer(p, x_var, add_p, test, comparisons, ref_group, p_label, p_adjust)
  
  p
}

#' Box plot for a single feature from SE
#' @export
plot_box_se <- function(se, feature_id,
                        x_var        = "class",
                        x_order      = NULL,
                        order_by     = c("none","abundance_mean","abundance_median","alphabetical"),
                        decreasing   = FALSE,
                        palette      = NULL,
                        add_p        = FALSE,
                        test         = "wilcox",
                        comparisons  = NULL,
                        ref_group    = NULL,
                        p_adjust     = "BH",
                        p_label      = "both",
                        box_width    = 0.6,
                        box_alpha    = 0.6,
                        show_points  = TRUE,
                        point_size   = 1.6,
                        jitter_width = 0.12,
                        point_alpha  = 0.8) {
  
  tidy <- .tidy_from_se_global(se)
  tidy <- tidy[tidy$feature_id %in% feature_id, , drop = FALSE]
  
  if (!nrow(tidy)) {
    stop("feature_id = ", feature_id, " ????????????????????????????????????")
  }
  
  tidy <- .set_x_order_for_plot(tidy, x_var = x_var,
                                x_order = x_order,
                                order_by = order_by,
                                decreasing = decreasing)
  
  aes_base <- ggplot2::aes(x = .data[[x_var]], y = .data$abundance, fill = .data[[x_var]])
  
  p <- ggplot2::ggplot(tidy, aes_base) +
    ggplot2::geom_boxplot(
      width         = box_width,
      alpha         = box_alpha,
      outlier.shape = NA,
      linewidth     = 2
    )
  
  if (isTRUE(show_points)) {
    p <- p +
      ggplot2::geom_jitter(
        ggplot2::aes(fill = .data[[x_var]]),
        width  = jitter_width,
        height = 0,
        size   = point_size,
        alpha  = point_alpha,
        shape  = 21
      )
  }
  
  if (!is.null(palette) && length(palette)) {
    p <- p +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE)
  }
  
  p <- p +
    ggplot2::labs(x = x_var, y = "Abundance") +
    theme_lipidomics()
  
  p <- .add_p_layer(p, x_var, add_p, test, comparisons, ref_group, p_label, p_adjust)
  
  p
}

#' Violin plot for a single feature from SE
#' @export
plot_violin_se <- function(se, feature_id,
                           x_var        = "class",
                           x_order      = NULL,
                           order_by     = c("none","abundance_mean","abundance_median","alphabetical"),
                           decreasing   = FALSE,
                           palette      = NULL,
                           add_p        = FALSE,
                           test         = "wilcox",
                           comparisons  = NULL,
                           ref_group    = NULL,
                           p_adjust     = "BH",
                           p_label      = "both",
                           violin_width = 0.8,
                           violin_alpha = 0.55,
                           trim         = TRUE,
                           show_points  = TRUE,
                           point_size   = 1.6,
                           jitter_width = 0.12,
                           point_alpha  = 0.8,
                           show_median  = TRUE,
                           median_size  = 0.7,
                           median_color = "#222222") {
  
  tidy <- .tidy_from_se_global(se)
  tidy <- tidy[tidy$feature_id %in% feature_id, , drop = FALSE]
  
  if (!nrow(tidy)) {
    stop("feature_id = ", feature_id, " ????????????????????????????????????")
  }
  
  tidy <- .set_x_order_for_plot(tidy, x_var = x_var,
                                x_order = x_order,
                                order_by = order_by,
                                decreasing = decreasing)
  
  aes_base <- ggplot2::aes(x = .data[[x_var]], y = .data$abundance, fill = .data[[x_var]])
  
  p <- ggplot2::ggplot(tidy, aes_base) +
    ggplot2::geom_violin(
      width = violin_width,
      alpha = violin_alpha,
      trim  = trim
    )
  
  if (isTRUE(show_points)) {
    p <- p +
      ggplot2::geom_jitter(
        ggplot2::aes(colour = .data[[x_var]]),
        width  = jitter_width,
        height = 0,
        size   = point_size,
        alpha  = point_alpha
      )
  }
  
  if (isTRUE(show_median)) {
    p <- p +
      ggplot2::stat_summary(
        fun    = stats::median,
        geom   = "point",
        size   = median_size,
        colour = median_color
      )
  }
  
  if (!is.null(palette) && length(palette)) {
    p <- p +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
      ggplot2::scale_colour_manual(values = palette, drop = FALSE)
  }
  
  p <- p +
    ggplot2::labs(x = x_var, y = "Abundance") +
    theme_lipidomics()
  
  p <- .add_p_layer(p, x_var, add_p, test, comparisons, ref_group, p_label, p_adjust)
  
  p
}

# ======================================================================
# ComplexHeatmap-based heatmap function (outside server)
# ======================================================================

#' Make ComplexHeatmap object for one lipid class
#' @export
make_class_heatmap_CH <- function(se, class_col, class_name,
                                  x_var = "class", x_order = NULL,
                                  order_by = c("none","abundance_mean","abundance_median","alphabetical"),
                                  decreasing = FALSE,
                                  topN = 40, row_z = TRUE,
                                  row_total_fun = c("mean", "sum"),
                                  palette = NULL) {
  
  order_by      <- match.arg(order_by)
  row_total_fun <- match.arg(row_total_fun)
  
  if (!requireNamespace("dplyr", quietly = TRUE))  stop("Please install dplyr.")
  if (!requireNamespace("tidyr", quietly = TRUE))  stop("Please install tidyr.")
  if (!requireNamespace("rlang", quietly = TRUE))  stop("Please install rlang.")
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("Package 'ComplexHeatmap' is required for heatmap plotting.")
  }
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package 'circlize' is required for heatmap color mapping.")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required for annotations.")
  }
  
  # ---- tidy & class filter ----
  tidy <- .tidy_from_se_global(se)
  rd   <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!class_col %in% colnames(rd)) {
    stop("class_col = ", class_col, " ??? rowData(se) ????????????????????????")
  }
  map  <- data.frame(
    feature_id  = rownames(rd),
    lipid_class = as.character(rd[[class_col]]),
    stringsAsFactors = FALSE
  )
  tidy <- dplyr::left_join(tidy, map, by = "feature_id") |>
    dplyr::filter(.data$lipid_class == class_name)
  
  if (!nrow(tidy)) {
    return(NULL)
  }
  
  # ---- Decide x-axis order using shared helper ----
  tidy <- .set_x_order_for_plot(
    tidy,
    x_var      = x_var,
    x_order    = x_order,
    order_by   = order_by,
    decreasing = decreasing
  )
  # store x levels (reuse later)
  x_levels <- levels(tidy[[x_var]])
  
  # ---- Mean abundance: molecule × class ----
  M <- tidy |>
    dplyr::group_by(.data$feature_id, .data[[x_var]]) |>
    dplyr::summarise(mu = mean(.data$abundance, na.rm = TRUE), .groups = "drop")
  
  if (!nrow(M)) return(NULL)
  
  # ---- Pre-compute row stats (for total bar) ----
  row_stats <- M |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(
      mean_abund = mean(.data$mu, na.rm = TRUE),
      sum_abund  = sum(.data$mu,  na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Top-N selection by overall mean ----
  top_ids <- row_stats |>
    dplyr::arrange(dplyr::desc(.data$mean_abund)) |>
    dplyr::slice_head(n = topN) |>
    dplyr::pull(.data$feature_id)
  
  M <- M |> dplyr::filter(.data$feature_id %in% top_ids)
  row_stats <- row_stats |> dplyr::filter(.data$feature_id %in% top_ids)
  
  if (!nrow(M)) return(NULL)
  
  # ---- wide matrix (feature × class) ----
  x_sym <- rlang::sym(x_var)
  M_wide <- tidyr::pivot_wider(M, names_from = !!x_sym, values_from = "mu")
  rn <- M_wide$feature_id
  M_wide$feature_id <- NULL
  
  # align columns to x_levels (missing classes become NA columns)
  missing_cols <- setdiff(x_levels, colnames(M_wide))
  if (length(missing_cols)) {
    for (mc in missing_cols) M_wide[[mc]] <- NA_real_
  }
  M_wide <- M_wide[, x_levels, drop = FALSE]
  
  mat <- as.matrix(M_wide)
  rownames(mat) <- rn
  
  # ---- choose row_total (mean or sum) ----
  row_stats <- row_stats[match(rownames(mat), row_stats$feature_id), , drop = FALSE]
  row_total <- if (row_total_fun == "mean") row_stats$mean_abund else row_stats$sum_abund
  names(row_total) <- row_stats$feature_id
  
  # ---- order rows by row_total (descending) ----
  ord <- order(row_total, decreasing = TRUE, na.last = NA)
  mat <- mat[ord, , drop = FALSE]
  row_total <- row_total[ord]
  
  # ---- row_z: z-score per molecule ----
  if (isTRUE(row_z)) {
    mat <- t(scale(t(mat)))
  }
  
  # ---- palette for class annotation ----
  if (!is.null(palette) && length(palette)) {
    pal_use <- palette
    missing <- setdiff(x_levels, names(pal_use))
    if (length(missing)) pal_use[missing] <- "#BBBBBB"
    pal_use <- pal_use[x_levels]
  } else {
    hues <- seq(15, 375, length.out = length(x_levels) + 1)
    pal_use <- grDevices::hcl(h = hues, l = 65, c = 100)[1:length(x_levels)]
    names(pal_use) <- x_levels
  }
  
  # ---- column annotation (class colors) ----
  sample_class <- factor(colnames(mat), levels = x_levels)
  ha_top <- ComplexHeatmap::HeatmapAnnotation(
    class = sample_class,
    col   = list(class = pal_use),
    show_annotation_name = FALSE,
    simple_anno_size = grid::unit(3, "mm")
  )
  
  # ---- row annotation (total bar) ----
  ra_right <- ComplexHeatmap::rowAnnotation(
    total = ComplexHeatmap::anno_barplot(
      row_total,
      width  = grid::unit(2, "cm"),
      gp     = grid::gpar(fill = "grey40", col = NA),
      border = FALSE
    ),
    annotation_name_side = "top"
  )
  
  # ---- color scale ----
  rng <- range(mat, na.rm = TRUE)
  mid <- if (isTRUE(row_z)) 0 else stats::median(mat, na.rm = TRUE)
  brks <- c(rng[1], mid, rng[2])
  col_fun <- circlize::colorRamp2(brks, c("#2C7BB6", "white", "#D7191C"))
  
  # ---- heatmap body ----
  ht <- ComplexHeatmap::Heatmap(
    mat,
    name = if (row_z) "z-score" else "mean",
    col  = col_fun,
    top_annotation   = ha_top,
    right_annotation = ra_right,
    cluster_rows      = FALSE,
    cluster_columns   = FALSE,
    show_row_names    = TRUE,
    show_column_names = TRUE,
    column_title      = x_var,
    row_title         = "Molecule"
  )
  
  ht
}

# ======================================================================
# Theme
# ======================================================================
theme_lipidomics <- function(base_size = 12, x_angle = 0,
                             axis_fontsize = base_size,
                             legend_fontsize = base_size,
                             title_fontsize = base_size + 2) {
  ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = title_fontsize),
      axis.text.x  = ggplot2::element_text(
        angle = x_angle, size = axis_fontsize,
        vjust = ifelse(x_angle == 0, 0.5, 1),
        hjust = ifelse(x_angle == 0, 0.5, 1)
      ),
      axis.text.y  = ggplot2::element_text(size = axis_fontsize),
      axis.title   = ggplot2::element_text(size = title_fontsize),
      legend.text  = ggplot2::element_text(size = legend_fontsize),
      legend.title = ggplot2::element_text(size = legend_fontsize, face = "bold")
    )
}
