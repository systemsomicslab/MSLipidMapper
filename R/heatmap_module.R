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


  tidy <- .set_x_order_for_plot(
    tidy,
    x_var      = x_var,
    x_order    = x_order,
    order_by   = order_by,
    decreasing = decreasing
  )


  if (is.factor(tidy[[x_var]])) {
    x_levels <- levels(tidy[[x_var]])
  } else {
    x_levels <- unique(as.character(tidy[[x_var]]))
  }


  rd  <- as.data.frame(SummarizedExperiment::rowData(se))
  map <- data.frame(
    feature_id  = rownames(rd),
    lipid_class = as.character(rd[[class_col]]),
    stringsAsFactors = FALSE
  )

  tidy <- dplyr::left_join(tidy, map, by = "feature_id") |>
    dplyr::filter(.data$lipid_class == class_name)


  M <- tidy |>
    dplyr::group_by(.data$feature_id, .data[[x_var]]) |>
    dplyr::summarise(mu = mean(.data$abundance, na.rm = TRUE), .groups = "drop")


  top_ids <- M |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(mu_all = mean(.data$mu, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mu_all)) |>
    dplyr::slice_head(n = topN) |>
    dplyr::pull(.data$feature_id)

  M <- M |> dplyr::filter(.data$feature_id %in% top_ids)


  if (isTRUE(row_z)) {
    w <- tidyr::pivot_wider(
      M,
      names_from  = !!rlang::sym(x_var),
      values_from = .data$mu
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
        names_to  = "X",
        values_to = "val"
      )

    colnames(M)[colnames(M) == "X"] <- x_var
  } else {
    M <- dplyr::rename(M, val = .data$mu)
  }


  M[[x_var]] <- factor(as.character(M[[x_var]]), levels = x_levels, ordered = TRUE)


  ord_rows <- M |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(mu_all = mean(.data$val, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mu_all)) |>
    dplyr::pull(.data$feature_id)

  M$feature_id <- factor(M$feature_id, levels = ord_rows, ordered = TRUE)

  midpoint_val <- if (isTRUE(row_z)) 0 else stats::median(M$val, na.rm = TRUE)


  p <- ggplot2::ggplot(
    M,
    ggplot2::aes(x = .data[[x_var]], y = .data$feature_id, fill = .data$val)
  ) +
    ggplot2::geom_tile(width = 1, height = 1) +
    ggplot2::scale_fill_gradient2(
      low  = "#2C7BB6",
      mid  = "white",
      high = "#D7191C",
      midpoint = midpoint_val,
      na.value = "grey90"
    ) +
    ggplot2::scale_x_discrete(limits = x_levels, drop = FALSE) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = if (row_z) "Z-score" else "mean"
    ) +
    theme_lipidomics(
      base_size       = base_font,
      x_angle         = x_angle,
      axis_fontsize   = base_font * 0.9,
      legend_fontsize = base_font * 0.8,
      title_fontsize  = base_font + 4
    ) +
    ggplot2::theme(
      legend.position = "right"
    ) +
    ggplot2::ggtitle(paste0("Heatmap (class mean): ", class_name))

  p
}
