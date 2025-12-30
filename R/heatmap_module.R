.make_heatmap_for_class <- function(
    se, class_col, class_name,
    x_var = "class", x_order = NULL,
    order_by = c("none","abundance_mean","abundance_median","alphabetical"),
    decreasing = FALSE,
    topN = 40, row_z = TRUE,
    base_font = 22,   # フォント大きめデフォルト
    x_angle   = 45
) {
  order_by <- match.arg(order_by)

  # --- tidy_from_se の取得（.tidy_from_se 名の場合にも対応） ---
  tidy <- .tidy_from_se_global(se)

  rd  <- as.data.frame(SummarizedExperiment::rowData(se))
  map <- data.frame(
    feature_id  = rownames(rd),
    lipid_class = as.character(rd[[class_col]]),
    stringsAsFactors = FALSE
  )

  tidy <- dplyr::left_join(tidy, map, by = "feature_id") |>
    dplyr::filter(.data$lipid_class == class_name)

  # ---- x順序 ----
  tidy[[x_var]] <- factor(tidy[[x_var]])
  if (!is.null(x_order) && length(x_order)) {
    tidy[[x_var]] <- factor(tidy[[x_var]], levels = unique(x_order), ordered = TRUE)
  } else if (order_by %in% c("abundance_mean","abundance_median")) {
    stat_fun <- if (order_by == "abundance_mean") mean else stats::median
    ord <- tidy |>
      dplyr::group_by(.data[[x_var]]) |>
      dplyr::summarise(stat = stat_fun(.data$abundance, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(if (decreasing) dplyr::desc(.data$stat) else .data$stat)
    tidy[[x_var]] <- factor(tidy[[x_var]], levels = ord[[x_var]], ordered = TRUE)
  }

  # ---- 分子×クラスの平均（= クラス平均） ----
  M <- tidy |>
    dplyr::group_by(.data$feature_id, .data[[x_var]]) |>
    dplyr::summarise(mu = mean(.data$abundance, na.rm = TRUE), .groups = "drop")

  # ---- Top-N（全クラス平均） ----
  top_ids <- M |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(mu_all = mean(.data$mu, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mu_all)) |>
    dplyr::slice_head(n = topN) |>
    dplyr::pull(.data$feature_id)
  M <- M |> dplyr::filter(.data$feature_id %in% top_ids)

  # ---- 値の確定（row_z なら Z-score） ----
  if (isTRUE(row_z)) {
    w  <- tidyr::pivot_wider(M, names_from = {{x_var}}, values_from = .data$mu)
    rn <- w$feature_id; w$feature_id <- NULL
    W  <- as.matrix(w)
    W  <- t(scale(t(W)))           # 行方向 Z-score
    df <- as.data.frame(W); df$feature_id <- rn
    M  <- df |>
      tidyr::pivot_longer(-feature_id, names_to = "X", values_to = "val")
    colnames(M)[colnames(M) == "X"] <- x_var
  } else {
    M <- dplyr::rename(M, val = .data$mu)
  }

  # ---- 行順（存在量の大きい順） ----
  ord_rows <- M |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(mu_all = mean(.data$val, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mu_all)) |>
    dplyr::pull(.data$feature_id)
  M$feature_id <- factor(M$feature_id, levels = ord_rows, ordered = TRUE)

  # ---- 発散スケールの中心 ----
  midpoint_val <- if (isTRUE(row_z)) 0 else stats::median(M$val, na.rm = TRUE)

  # ---- プロット（1×1タイル & 大きめフォント）----
  p <- ggplot2::ggplot(
    M,
    ggplot2::aes(x = .data[[x_var]], y = .data$feature_id, fill = .data$val)
  ) +
    ggplot2::geom_tile(width = 1, height = 1) +  # ★ タイルを 1×1 に固定
    ggplot2::scale_fill_gradient2(
      low  = "#2C7BB6",
      mid  = "white",
      high = "#D7191C",
      midpoint = midpoint_val,
      na.value = "grey90"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = if (row_z) "Z-score" else "mean"
    ) +
    theme_lipidomics(
      base_size      = base_font,
      x_angle        = x_angle,
      axis_fontsize  = base_font * 0.9,
      legend_fontsize= base_font * 0.8,
      title_fontsize = base_font + 4
    ) +
    ggplot2::theme(
      legend.position = "right"
    ) +
    ggplot2::ggtitle(paste0("Heatmap (class mean): ", class_name))

  p
}
