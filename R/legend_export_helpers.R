# Helpers for exporting ggplot figures with large categorical legends.

.is_dense_categorical_legend <- function(colors) {
  labels <- names(colors)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  length(labels) > 12L ||
    (length(labels) > 0L && max(nchar(labels, type = "width"), na.rm = TRUE) > 30L) ||
    sum(nchar(labels, type = "width"), na.rm = TRUE) > 180L
}

.draw_categorical_key_pages <- function(colors, title = "Class color key") {
  labels <- names(colors)
  keep <- !is.na(labels) & nzchar(labels)
  labels <- labels[keep]
  colors <- unname(colors[keep])
  if (!length(labels)) return(invisible(NULL))

  max_chars <- max(nchar(labels, type = "width"), na.rm = TRUE)
  n_cols <- if (max_chars > 42L) 1L else if (max_chars > 25L) 2L else 3L
  rows_per_col <- 18L
  per_page <- n_cols * rows_per_col

  page_starts <- seq.int(1L, length(labels), by = per_page)
  for (page_no in seq_along(page_starts)) {
    page_start <- page_starts[[page_no]]
    idx <- page_start:min(page_start + per_page - 1L, length(labels))
    page_labels <- labels[idx]
    page_colors <- colors[idx]

    grid::grid.newpage()
    grid::grid.text(
      if (length(page_starts) > 1L) paste0(title, " (", page_no, "/", length(page_starts), ")") else title,
      x = grid::unit(0.04, "npc"), y = grid::unit(0.965, "npc"),
      just = c("left", "top"), gp = grid::gpar(fontsize = 16, fontface = "bold")
    )

    for (j in seq_along(page_labels)) {
      col_idx <- (j - 1L) %/% rows_per_col
      row_idx <- (j - 1L) %% rows_per_col
      col_width <- 0.92 / n_cols
      x0 <- 0.04 + col_idx * col_width
      y0 <- 0.89 - row_idx * (0.82 / max(1L, rows_per_col - 1L))

      grid::grid.points(
        x = grid::unit(x0 + 0.012, "npc"), y = grid::unit(y0, "npc"),
        pch = 21, size = grid::unit(3.5, "mm"),
        gp = grid::gpar(fill = page_colors[[j]], col = "black", lwd = 0.8)
      )
      grid::grid.text(
        page_labels[[j]],
        x = grid::unit(x0 + 0.035, "npc"), y = grid::unit(y0, "npc"),
        just = c("left", "center"), gp = grid::gpar(fontsize = 9)
      )
    }
  }

  invisible(NULL)
}

.export_ggplot_with_adaptive_legend <- function(
    file, plot, colors,
    width = 6.5, height = 6.5,
    key_title = "Class color key"
) {
  dense <- .is_dense_categorical_legend(colors)
  device_width <- if (dense) max(width, 11) else width
  device_height <- if (dense) max(height, 8.5) else height

  grDevices::pdf(
    file, width = device_width, height = device_height,
    onefile = TRUE, useDingbats = FALSE
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  if (dense) {
    print(plot + ggplot2::theme(legend.position = "none"))
    .draw_categorical_key_pages(colors, title = key_title)
  } else {
    print(plot)
  }

  invisible(dense)
}
