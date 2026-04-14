.make_heatmap_for_class <- function(
    se, class_col, class_name,
    x_var = "class", x_order = NULL,
    order_by = c("none","abundance_mean","abundance_median","alphabetical"),
    decreasing = FALSE,
    topN = 40, row_z = TRUE,
    base_font = 22,   
    x_angle   = 45
) {
  order_by <- match.arg(order_by)

  tidy <- .tidy_from_se_global(se)
  if (!"sample_id" %in% colnames(tidy)) {
    stop("`.tidy_from_se_global(se)` must contain `sample_id`.")
  }
  if (!x_var %in% colnames(tidy)) {
    stop("Column `", x_var, "` was not found in tidy data.")
  }

  sample_order_df <- data.frame(
    sample_id = colnames(se),
    sample_index = seq_along(colnames(se)),
    stringsAsFactors = FALSE
  )

  sample_meta <- tidy |>
    dplyr::transmute(
      sample_id = as.character(.data$sample_id),
      group_var = as.character(.data[[x_var]])
    ) |>
    dplyr::distinct(.data$sample_id, .keep_all = TRUE) |>
    dplyr::left_join(sample_order_df, by = "sample_id")

  group_levels <- unique(sample_meta$group_var)
  if (!is.null(x_order) && length(x_order)) {
    x_order <- as.character(x_order)
    group_levels <- unique(c(x_order[x_order %in% group_levels], group_levels[!group_levels %in% x_order]))
  }

  sample_meta$group_var <- factor(sample_meta$group_var, levels = group_levels, ordered = TRUE)
  sample_meta <- sample_meta |>
    dplyr::arrange(.data$group_var, .data$sample_index, .data$sample_id)

  group_blocks <- lapply(seq_along(group_levels), function(i) {
    g <- group_levels[[i]]
    block <- sample_meta |>
      dplyr::filter(as.character(.data$group_var) == g) |>
      dplyr::mutate(display_id = .data$sample_id)
    if (i < length(group_levels) && nrow(block)) {
      gap_row <- data.frame(
        sample_id = NA_character_,
        group_var = NA,
        sample_index = NA_real_,
        display_id = paste0("__gap__", i),
        stringsAsFactors = FALSE
      )
      block <- dplyr::bind_rows(block, gap_row)
    }
    block
  })
  sample_meta_plot <- dplyr::bind_rows(group_blocks)

  sample_levels <- sample_meta_plot$display_id
  sample_meta_plot$sample_pos <- seq_len(nrow(sample_meta_plot))

  class_centers <- sample_meta_plot |>
    dplyr::filter(!is.na(.data$group_var), nzchar(as.character(.data$group_var))) |>
    dplyr::group_by(.data$group_var) |>
    dplyr::summarise(
      min_pos = min(.data$sample_pos),
      max_pos = max(.data$sample_pos),
      mid_pos = stats::median(.data$sample_pos),
      .groups = "drop"
    )

  class_breaks <- sample_meta_plot$sample_pos[grepl("^__gap__", sample_meta_plot$display_id)]


  rd  <- as.data.frame(SummarizedExperiment::rowData(se))
  map <- data.frame(
    feature_id  = rownames(rd),
    lipid_class = as.character(rd[[class_col]]),
    stringsAsFactors = FALSE
  )

  tidy <- dplyr::left_join(tidy, map, by = "feature_id") |>
    dplyr::filter(.data$lipid_class == class_name)


  M <- tidy |>
    dplyr::mutate(sample_id = as.character(.data$sample_id)) |>
    dplyr::filter(.data$sample_id %in% sample_meta_plot$sample_id[!is.na(sample_meta_plot$sample_id)]) |>
    dplyr::select(.data$feature_id, .data$sample_id, .data$abundance)


  top_ids <- M |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(mu_all = mean(.data$abundance, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mu_all)) |>
    dplyr::slice_head(n = topN) |>
    dplyr::pull(.data$feature_id)

  M <- M |> dplyr::filter(.data$feature_id %in% top_ids)


  if (isTRUE(row_z)) {
    w <- tidyr::pivot_wider(
      M,
      names_from  = .data$sample_id,
      values_from = .data$abundance
    )

    rn <- w$feature_id
    w$feature_id <- NULL

    W <- as.matrix(w)
    W <- t(scale(t(W)))     

    df <- as.data.frame(W)
    df$feature_id <- rn

    M <- df |>
      tidyr::pivot_longer(
        -feature_id,
        names_to  = "sample_id",
        values_to = "val"
      )
  } else {
    M <- dplyr::rename(M, val = .data$abundance)
  }

  gap_ids <- sample_meta_plot$display_id[grepl("^__gap__", sample_meta_plot$display_id)]
  if (length(gap_ids) && nrow(M)) {
    gap_df <- expand.grid(
      feature_id = unique(as.character(M$feature_id)),
      sample_id = gap_ids,
      stringsAsFactors = FALSE
    )
    gap_df$val <- NA_real_
    M <- dplyr::bind_rows(M, gap_df)
  }

  M$sample_id <- factor(as.character(M$sample_id), levels = sample_levels, ordered = TRUE)


  ord_rows <- M |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(mu_all = mean(.data$val, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mu_all)) |>
    dplyr::pull(.data$feature_id)

  M$feature_id <- factor(M$feature_id, levels = ord_rows, ordered = TRUE)
  n_rows <- length(ord_rows)

  midpoint_val <- if (isTRUE(row_z)) 0 else stats::median(M$val, na.rm = TRUE)
  fill_limits <- if (isTRUE(row_z)) c(-2, 2) else NULL


  p <- ggplot2::ggplot(
    M,
    ggplot2::aes(x = .data$sample_id, y = .data$feature_id, fill = .data$val)
  ) +
    ggplot2::geom_tile(width = 1, height = 1, color = "grey80", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(
      low  = "navy",
      mid  = "white",
      high = "firebrick",
      midpoint = midpoint_val,
      limits = fill_limits,
      na.value = "white"
    ) +
    ggplot2::scale_x_discrete(
      limits = sample_levels,
      drop = FALSE,
      labels = setNames(rep("", length(sample_levels)), sample_levels)
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = if (row_z) "Z-score" else "abundance"
    ) +
    theme_lipidomics(
      base_size       = base_font,
      x_angle         = x_angle,
      axis_fontsize   = base_font * 0.9,
      legend_fontsize = base_font * 0.8,
      title_fontsize  = base_font + 4
    ) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 34, r = 8, b = 10, l = 8)
    )

  if (length(class_breaks)) {
    p <- p + ggplot2::geom_vline(
      xintercept = class_breaks,
      linewidth = 0.35,
      colour = "grey55"
    )
  }

  if (nrow(class_centers)) {
    p <- p + ggplot2::annotate(
      "text",
      x = class_centers$mid_pos,
      y = n_rows + 0.62,
      label = as.character(class_centers$group_var),
      vjust = 0,
      size = max(4.8, base_font / 4.2),
      fontface = "bold",
      colour = "#333333"
    ) +
      ggplot2::coord_cartesian(ylim = c(0.5, n_rows + 0.9), clip = "off")
  }

  p
}
