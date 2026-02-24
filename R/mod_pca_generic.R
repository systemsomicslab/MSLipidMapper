# R/mod_pca_generic.R -----------------------------------------------
# Generic PCA panel (Lipid / Transcriptome)
# - Layout matches OPLS-DA generic panel as much as possible:
#     Row1: settings (3) | score plot (4) | loading plot (5)
#     Row2: loading-heatmap (12) + PDF download
# - Event-driven (Run button) to keep outputs consistent for downloads
# - Grouping is FIXED to colData$class (NO UI for "Color by")
# - Score plot legend.position is forced to bottom
# - Point size control (sidebar)
# - Loadings Top-N default = 10  (top/bottom each)
# - Heatmap variables are GUARANTEED to be consistent with the loadings plot:
#     * same PC as the loadings plot (pc_loading)
#     * same de-duplication logic (by display label)
#     * heatmap includes at least the variables shown in the loadings plot
# - Download button style = grey (btn-default)
#
# This module is self-consistent (scores/loadings/heatmap computed from the same PCA run).
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(methods)
  library(grid)
  library(ComplexHeatmap)
  # UI uses shinydashboard::box (namespace call), so library(shinydashboard) is optional
})

`%||%` <- function(a, b) if (is.null(a)) b else a

#--------------------------------------------------
# UI: Generic PCA (Lipid / Transcriptome)
#--------------------------------------------------
#' Generic PCA UI (dataset switch + scores + loadings + loading-heatmap)
#' @export
mod_pca_generic_ui <- function(id, title = "PCA") {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .pca-plot-square { position: relative; width: 100%; padding-bottom: 100%; }
      .pca-plot-square-inner { position: absolute; inset: 0; }
      .pca-download-row { margin-top: 8px; }
    ")),

    fluidRow(
      column(
        width = 3,
        shinydashboard::box(
          title = paste(title, "- settings"),
          width = 12,
          solidHeader = TRUE,
          status = "primary",

          uiOutput(ns("dataset_ui")),
          hr(),

          tags$label("Scores axes (PC)"),
          fluidRow(
            column(6, numericInput(ns("pc_x"), "X", value = 1, min = 1, step = 1)),
            column(6, numericInput(ns("pc_y"), "Y", value = 2, min = 1, step = 1))
          ),
          hr(),

          tags$label("Score plot point size"),
          numericInput(ns("point_size"), label = NULL, min = 0.5, max = 10, value = 3, step = 0.5),
          hr(),

          tags$label("Loadings bar plot"),
          fluidRow(
            column(6, numericInput(ns("pc_loading"), "PC index", value = 1, min = 1, step = 1)),
            column(6, numericInput(ns("top_n_each"), "Top-N", value = 10, min = 1, step = 1))
          ),
          hr(),

          tags$label("Loading heatmap"),
          fluidRow(
            column(6, numericInput(ns("topn_heat"), "Heatmap top (total)", value = 20, min = 5, step = 1)),
            column(6, numericInput(ns("z_lim"),     "Heatmap Z-limit",     value = 2,  min = 0.5, step = 0.5))
          ),

          hr(),

          actionButton(ns("run_pca"), "Run PCA", icon = icon("play"),
                       class = "btn-primary", width = "100%"),
          uiOutput(ns("run_msg"))
        )
      ),

      column(
        width = 4,
        shinydashboard::box(
          title = "PCA score plot",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          div(
            class = "pca-plot-square",
            div(
              class = "pca-plot-square-inner",
              plotOutput(ns("pca_scores"), height = "100%", width = "100%")
            )
          ),
          div(
            class = "pca-download-row",
            downloadButton(
              ns("download_pca_scores_pdf"),
              "Download PDF",
              class = "btn-default",
              width = "100%"
            )
          )
        )
      ),

      column(
        width = 5,
        shinydashboard::box(
          title = "PCA loading plot",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          div(
            class = "pca-plot-square",
            div(
              class = "pca-plot-square-inner",
              plotOutput(ns("pca_loadings"), height = "100%", width = "100%")
            )
          ),
          div(
            class = "pca-download-row",
            downloadButton(
              ns("download_pca_loadings_pdf"),
              "Download PDF",
              class = "btn-default",
              width = "100%"
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        width = 12,
        shinydashboard::box(
          title = "Loading heatmap (consistent with loading plot variables)",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          plotOutput(ns("pca_heatmap"), height = "650px"),
          fluidRow(
            column(
              4,
              downloadButton(
                ns("download_pca_heatmap_pdf"),
                "Download heatmap PDF",
                class = "btn-default",
                width = "100%"
              )
            )
          )
        )
      )
    )
  )
}

#--------------------------------------------------
# server: PCA for SummarizedExperiment (lipid / tx)
#--------------------------------------------------
#' Generic PCA server
#'
#' @param id           module id
#' @param se_lipid     reactive({ SummarizedExperiment }) lipid SE
#' @param se_tx        reactive({ SummarizedExperiment }) transcriptome SE (optional; may be NULL)
#' @param assay        assay name used for PCA (default: "abundance")
#' @param adv_reactive reactive({ list }) Common advanced settings (optional)
#'                     - uses adv$palette_map (group -> "#RRGGBB") and adv$manual_order (legend order)
#' @export
mod_pca_generic_server <- function(id,
                                   se_lipid,
                                   se_tx = NULL,
                                   assay = "abundance",
                                   adv_reactive = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- shared adv fetch ------------------------------------------------
    .get_adv <- function() {
      if (is.function(adv_reactive)) {
        a <- adv_reactive()
        if (!is.null(a) && is.list(a)) return(a)
      }
      NULL
    }

    .norm_hex <- function(x) {
      if (is.null(x) || !nzchar(x)) return(NA_character_)
      x <- trimws(as.character(x))
      if (!startsWith(x, "#")) x <- paste0("#", x)
      if (!grepl("^#([A-Fa-f0-9]{6})$", x)) return(NA_character_)
      toupper(x)
    }

    .palette_from_adv <- function(values, adv) {
      v <- as.character(values)
      v[is.na(v) | v == ""] <- "NA"
      lev <- unique(v)

      cols <- grDevices::hcl.colors(length(lev), palette = "Dark 3")
      names(cols) <- lev

      if (!is.null(adv) && is.list(adv)) {
        if (!is.null(adv$palette_map) && length(adv$palette_map) && !is.null(names(adv$palette_map))) {
          pm <- stats::setNames(vapply(adv$palette_map, .norm_hex, character(1)), names(adv$palette_map))
          hit <- intersect(names(pm), lev)
          if (length(hit)) cols[hit] <- pm[hit]
        }
        if (!is.null(adv$manual_order) && length(adv$manual_order)) {
          ord  <- intersect(as.character(adv$manual_order), lev)
          rest <- setdiff(lev, ord)
          lev2 <- c(ord, rest)
          cols <- cols[lev2]
        }
      }

      list(values = v, colors = cols, breaks = names(cols))
    }

    .safe_file_stem <- function(x) {
      x <- if (is.null(x) || !nzchar(x)) "pca" else x
      x <- gsub("[^A-Za-z0-9._-]+", "_", x)
      x <- gsub("_+", "_", x)
      x
    }

    # ---------------------------
    # Feature label chooser (auto)
    # ---------------------------
    .get_feature_labels_from_se <- function(se, feature_ids, label_col = NULL) {
      rd <- SummarizedExperiment::rowData(se)
      cn <- colnames(rd)

      need_auto <- is.null(label_col) ||
        !is.character(label_col) ||
        !length(label_col) ||
        is.na(label_col[1]) ||
        !nzchar(label_col[1]) ||
        !label_col[1] %in% cn

      if (need_auto) {
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
    # PCA prep (single source of truth for scores/loadings/heatmap)
    # ---------------------------
    .prep_pca <- function(se, assay_name, exclude_classes = c("Blank", "QC"), center = TRUE, scale. = TRUE) {
      if (!methods::is(se, "SummarizedExperiment")) stop("`se` must be a SummarizedExperiment.")
      if (!(assay_name %in% SummarizedExperiment::assayNames(se))) {
        stop("Assay '", assay_name, "' was not found in the SummarizedExperiment.")
      }

      mat <- SummarizedExperiment::assay(se, assay_name)  # features x samples
      cd  <- as.data.frame(SummarizedExperiment::colData(se))
      if (!("class" %in% colnames(cd))) stop("colData(se) must contain a column named 'class'.")

      keep <- rep(TRUE, ncol(mat))
      if (!is.null(exclude_classes)) {
        keep <- keep & !(cd[["class"]] %in% exclude_classes)
      }

      mat <- mat[, keep, drop = FALSE]
      cd  <- cd[keep, , drop = FALSE]

      # X: samples x variables
      X <- t(as.matrix(mat))
      storage.mode(X) <- "double"
      X[!is.finite(X)] <- NA

      # impute NA with column mean (all-NA col -> 0)
      cm <- suppressWarnings(colMeans(X, na.rm = TRUE))
      cm[!is.finite(cm)] <- 0
      for (j in seq_len(ncol(X))) {
        bad <- is.na(X[, j])
        if (any(bad)) X[bad, j] <- cm[j]
      }

      # remove zero-variance variables
      vv <- apply(X, 2, stats::var, na.rm = TRUE)
      X  <- X[, vv > 0 & is.finite(vv), drop = FALSE]
      if (ncol(X) < 2) stop("Too few variables after filtering (zero-variance removed).")
      if (nrow(X) < 2) stop("Too few samples after filtering.")

      rpca <- stats::prcomp(X, center = center, scale. = scale.)
      var_pct <- (rpca$sdev^2) / sum(rpca$sdev^2) * 100

      list(X = X, cd = cd, rpca = rpca, var_pct = var_pct)
    }

    .plot_scores <- function(rpca, var_pct, cd, cols, pc_x, pc_y, point_size) {
      pc_x <- as.integer(pc_x); pc_y <- as.integer(pc_y)
      if (pc_x < 1 || pc_y < 1) stop("pc_x and pc_y must be >= 1.")
      if (pc_x > ncol(rpca$x) || pc_y > ncol(rpca$x)) stop("pc_x/pc_y exceeds available PCs.")

      df <- data.frame(
        xValues = as.numeric(rpca$x[, pc_x]),
        yValues = as.numeric(rpca$x[, pc_y]),
        class   = as.character(cd[["class"]]),
        sample  = rownames(rpca$x),
        stringsAsFactors = FALSE
      )

      df$class <- factor(df$class, levels = names(cols))

      xLabel <- paste0("PC", pc_x, ": ", round(var_pct[pc_x], 2), "%")
      yLabel <- paste0("PC", pc_y, ": ", round(var_pct[pc_y], 2), "%")

      ggplot2::ggplot(df, ggplot2::aes(x = xValues, y = yValues)) +
        ggplot2::geom_point(ggplot2::aes(fill = class), size = point_size, shape = 21, color = "black") +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::labs(x = xLabel, y = yLabel, fill = "class", title = "PCA score plot") +
        ggplot2::theme(aspect.ratio = 1, legend.position = "bottom") +
        ggplot2::scale_fill_manual(values = cols, breaks = names(cols), drop = FALSE)
    }

    .build_loading_table <- function(rpca, se, X_colnames, pc_index, label_col = NULL) {
      pc_index <- as.integer(pc_index)
      if (pc_index < 1) stop("pc_index must be >= 1.")
      if (pc_index > ncol(rpca$rotation)) stop("pc_index exceeds available PCs.")

      loading_all <- rpca$rotation[, pc_index]

      # ensure aligned to X columns (and keep X order)
      loading_all <- loading_all[match(X_colnames, names(loading_all))]
      names(loading_all) <- X_colnames

      labels <- .get_feature_labels_from_se(se, feature_ids = X_colnames, label_col = label_col)

      df <- data.frame(
        feature_id = X_colnames,
        label      = as.character(labels),
        loading    = as.numeric(loading_all),
        stringsAsFactors = FALSE
      )

      # de-duplication by label (same logic for barplot + heatmap)
      df <- df[!duplicated(df$label), , drop = FALSE]

      # remove non-finite
      df <- df[is.finite(df$loading), , drop = FALSE]
      if (!nrow(df)) stop("No finite loadings after filtering/dedup.")

      df
    }

    .plot_loadings <- function(df_loading, pc_index, top_n_each) {
      top_n_each <- max(1L, as.integer(top_n_each))

      df <- df_loading
      df$value_types <- ifelse(df$loading < 0, "below", "above")

      df <- df[order(df$loading), , drop = FALSE]
      df$label <- factor(df$label, levels = df$label)

      df$row_number <- seq_len(nrow(df))
      if (top_n_each > 0) {
        df <- df[df$row_number <= top_n_each | df$row_number > (nrow(df) - top_n_each), , drop = FALSE]
      }

      df <- df[order(df$loading), , drop = FALSE]
      df$label <- factor(df$label, levels = df$label)

      ggplot2::ggplot(df, ggplot2::aes(x = label, y = loading)) +
        ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = value_types), width = 0.8) +
        ggplot2::scale_fill_manual(
          name   = "Legend",
          labels = c("Positive", "Negative"),
          values = c("above" = "#00ba38", "below" = "#f8766d")
        ) +
        ggplot2::theme_classic(base_size = 15) +
        ggplot2::labs(title = paste0("Loading plot: PC", pc_index), x = "Features", y = "Loading") +
        ggplot2::coord_flip() +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
    }

    .select_ids_for_barplot <- function(df_loading, top_n_each) {
      top_n_each <- max(1L, as.integer(top_n_each))
      df <- df_loading[order(df_loading$loading), , drop = FALSE]
      ids_neg <- head(df$feature_id, top_n_each)
      ids_pos <- tail(df$feature_id, top_n_each)
      unique(c(ids_neg, ids_pos))
    }

    .select_ids_for_heatmap <- function(df_loading, top_n_each_bar, topn_heat_total) {
      topn_heat_total <- max(5L, as.integer(topn_heat_total))
      top_n_each_bar  <- max(1L, as.integer(top_n_each_bar))
      topn_heat_total <- max(topn_heat_total, 2L * top_n_each_bar)

      n_each_heat <- ceiling(topn_heat_total / 2)

      df <- df_loading[order(df_loading$loading), , drop = FALSE]
      ids_neg_heat <- head(df$feature_id, n_each_heat)
      ids_pos_heat <- tail(df$feature_id, n_each_heat)

      ids_bar <- .select_ids_for_barplot(df_loading, top_n_each_bar)

      unique(c(ids_bar, ids_neg_heat, ids_pos_heat))
    }

    .make_loading_heatmap <- function(X, cd, cols, df_loading, pc_index, top_n_each_bar, topn_heat, z_lim) {
      if (!requireNamespace("matrixStats", quietly = TRUE)) {
        stop("Package 'matrixStats' is required for PCA heatmap (rowSds).")
      }
      if (!requireNamespace("circlize", quietly = TRUE)) {
        stop("Package 'circlize' is required for PCA heatmap (colorRamp2).")
      }

      z_lim <- as.numeric(z_lim)
      if (!is.finite(z_lim) || z_lim <= 0) z_lim <- 2

      sel_ids <- .select_ids_for_heatmap(df_loading, top_n_each_bar = top_n_each_bar, topn_heat_total = topn_heat)
      sel_ids <- intersect(sel_ids, colnames(X))
      if (length(sel_ids) < 2) stop("Too few variables selected for heatmap.")

      # keep order of sel_ids (bar vars included first)
      df_sel <- df_loading[match(sel_ids, df_loading$feature_id), , drop = FALSE]
      ok <- !is.na(df_sel$feature_id) & is.finite(df_sel$loading)
      df_sel <- df_sel[ok, , drop = FALSE]
      if (!nrow(df_sel)) stop("Selected heatmap variables could not be matched to loading table.")

      sel_labels <- make.unique(as.character(df_sel$label))
      load_vec   <- as.numeric(df_sel$loading)
      names(load_vec) <- sel_labels

      # heatmap matrix: features x samples
      mat2 <- t(X[, df_sel$feature_id, drop = FALSE])
      rownames(mat2) <- sel_labels

      # row-wise Z + clip
      m2 <- mat2 - rowMeans(mat2, na.rm = TRUE)
      s  <- matrixStats::rowSds(m2, na.rm = TRUE)
      s[s == 0 | !is.finite(s)] <- 1
      mat_z <- m2 / s
      mat_z[mat_z >  z_lim] <-  z_lim
      mat_z[mat_z < -z_lim] <- -z_lim

      # group factor for samples (top annotation)
      y <- factor(as.character(cd[["class"]]), levels = names(cols))
      names(y) <- rownames(cd)
      y <- y[colnames(mat_z)]

      # Loading color scale: adapt when all-positive or all-negative
      lv <- load_vec
      if (all(lv >= 0, na.rm = TRUE)) {
        mx <- max(lv, na.rm = TRUE); if (!is.finite(mx) || mx == 0) mx <- 1
        col_load <- circlize::colorRamp2(c(0, mx), c("white", "#c30010"))
      } else if (all(lv <= 0, na.rm = TRUE)) {
        mn <- min(lv, na.rm = TRUE); if (!is.finite(mn) || mn == 0) mn <- -1
        col_load <- circlize::colorRamp2(c(mn, 0), c("blue4", "white"))
      } else {
        maxabs <- max(abs(lv), na.rm = TRUE); if (!is.finite(maxabs) || maxabs == 0) maxabs <- 1
        col_load <- circlize::colorRamp2(c(-maxabs, 0, maxabs), c("blue4", "white", "#c30010"))
      }

ha_left <- ComplexHeatmap::rowAnnotation(
  Loading = ComplexHeatmap::anno_simple(load_vec, col = col_load, border = TRUE),
  width = grid::unit(10, "mm"),
  annotation_legend_param = list(
    title = "Loading",
    direction = "horizontal",
    title_gp  = grid::gpar(fontsize = 10),
    labels_gp = grid::gpar(fontsize = 9)
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
      title_gp  = grid::gpar(fontsize = 10),
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
        row_names_gp = grid::gpar(fontsize = 9),
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
        loading_min = sprintf("%.4f", min(load_vec, na.rm = TRUE)),
        loading_max = sprintf("%.4f", max(load_vec, na.rm = TRUE)),
        selected_feature_ids = df_sel$feature_id,
        selected_labels = sel_labels,
        selected_loadings = load_vec,
        pc_index = pc_index
      )
    }

    # ---------------------------
    # Legend-safe draw (avoid overlap)
    # ---------------------------
    .draw_heatmap_safe <- function(ht) {
      grid::grid.newpage()
      ComplexHeatmap::draw(
        ht,
        heatmap_legend_side = "bottom",
        annotation_legend_side = "bottom",
        merge_legends = TRUE,
        padding = grid::unit(c(3, 3, 3, 3), "mm")
      )
    }

    #---------------------------
    # available datasets
    #---------------------------
    available_datasets <- reactive({
      labs   <- character(0)
      values <- character(0)

      s1 <- try(se_lipid(), silent = TRUE)
      if (!inherits(s1, "try-error") && !is.null(s1) && methods::is(s1, "SummarizedExperiment")) {
        labs   <- c(labs,   "Lipid")
        values <- c(values, "lipid")
      }

      if (!is.null(se_tx)) {
        s2 <- try(se_tx(), silent = TRUE)
        if (!inherits(s2, "try-error") && !is.null(s2) && methods::is(s2, "SummarizedExperiment")) {
          labs   <- c(labs,   "Transcriptome")
          values <- c(values, "tx")
        }
      }

      if (!length(values)) return(NULL)
      data.frame(label = labs, value = values, stringsAsFactors = FALSE)
    })

    output$dataset_ui <- renderUI({
      ds <- available_datasets()
      if (is.null(ds)) {
        helpText("No dataset is available yet. Please upload data first (Upload/Normalize).")
      } else {
        radioButtons(
          ns("dataset"),
          "Dataset",
          choices  = stats::setNames(ds$value, ds$label),
          selected = ds$value[1],
          inline   = FALSE
        )
      }
    })

    current_se <- reactive({
      ds <- input$dataset
      if (is.null(ds)) return(NULL)

      if (identical(ds, "lipid")) {
        se_lipid()
      } else if (identical(ds, "tx")) {
        if (is.null(se_tx)) NULL else se_tx()
      } else {
        NULL
      }
    })

    current_dataset_label <- reactive({
      ds <- input$dataset %||% "lipid"
      if (identical(ds, "tx")) "transcriptome" else "lipid"
    })

    output$run_msg <- renderUI({
      se <- current_se()
      if (is.null(se)) return(tags$div(style = "color:#b30000;", "No SummarizedExperiment available."))

      tags$div(
        style = "color:#666;",
        paste0(
          "Scores: PC", input$pc_x %||% 1, " vs PC", input$pc_y %||% 2,
          " | Loadings/Heatmap PC: PC", input$pc_loading %||% 1
        )
      )
    })

    #---------------------------
    # Event-driven run (like OPLS tab)
    #---------------------------
    fit_res <- eventReactive(input$run_pca, {
      se <- current_se()
      if (is.null(se) || !methods::is(se, "SummarizedExperiment")) {
        showNotification("SummarizedExperiment is not available.", type = "error", duration = 8)
        return(NULL)
      }

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!("class" %in% colnames(cd))) {
        showNotification("colData(se)$class is required for this module.", type = "error", duration = 8)
        return(NULL)
      }

      pc_x <- max(1L, as.integer(input$pc_x %||% 1))
      pc_y <- max(1L, as.integer(input$pc_y %||% 2))

      pt <- as.numeric(input$point_size %||% 3)
      if (!is.finite(pt) || pt <= 0) pt <- 3

      pc_load <- max(1L, as.integer(input$pc_loading %||% 1))
      top_n   <- max(1L, as.integer(input$top_n_each %||% 10))

      top_h <- max(5L, as.integer(input$topn_heat %||% 20))

      z_lim <- as.numeric(input$z_lim %||% 2)
      if (!is.finite(z_lim) || z_lim <= 0) z_lim <- 2

      adv <- .get_adv()

      withProgress(message = "Running PCA ...", value = 0, {
        incProgress(0.2)

        prep <- tryCatch({
          .prep_pca(se = se, assay_name = assay, exclude_classes = c("Blank", "QC"))
        }, error = function(e) e)

        if (inherits(prep, "error")) {
          showNotification(conditionMessage(prep), type = "error", duration = 10)
          return(NULL)
        }

        X    <- prep$X
        cd2  <- prep$cd
        rpca <- prep$rpca
        varp <- prep$var_pct

        pal  <- .palette_from_adv(cd2[["class"]], adv)
        cols <- pal$colors

        incProgress(0.25)

        p_scores <- tryCatch({
          .plot_scores(rpca = rpca, var_pct = varp, cd = cd2, cols = cols,
                       pc_x = pc_x, pc_y = pc_y, point_size = pt)
        }, error = function(e) e)

        if (inherits(p_scores, "error")) {
          showNotification(conditionMessage(p_scores), type = "error", duration = 10)
          return(NULL)
        }

        incProgress(0.25)

        df_loading <- tryCatch({
          .build_loading_table(
            rpca = rpca,
            se = se,                 # original se for rowData labels
            X_colnames = colnames(X),
            pc_index = pc_load,
            label_col = NULL
          )
        }, error = function(e) e)

        if (inherits(df_loading, "error")) {
          showNotification(conditionMessage(df_loading), type = "error", duration = 10)
          return(NULL)
        }

        p_loadings <- tryCatch({
          .plot_loadings(df_loading = df_loading, pc_index = pc_load, top_n_each = top_n)
        }, error = function(e) e)

        if (inherits(p_loadings, "error")) {
          showNotification(conditionMessage(p_loadings), type = "error", duration = 10)
          return(NULL)
        }

        incProgress(0.2)

        hm <- tryCatch({
          .make_loading_heatmap(
            X = X,
            cd = cd2,
            cols = cols,
            df_loading = df_loading,
            pc_index = pc_load,
            top_n_each_bar = top_n,
            topn_heat = top_h,
            z_lim = z_lim
          )
        }, error = function(e) e)

        if (inherits(hm, "error")) {
          showNotification(conditionMessage(hm), type = "error", duration = 10)
          return(NULL)
        }

        incProgress(0.1)

        list(
          pca = prep,
          colors = cols,
          loading_table = df_loading,
          plots = list(scores = p_scores, loadings = p_loadings),
          heatmap = hm
        )
      })

    }, ignoreInit = TRUE)

    #---------------------------
    # Render plots
    #---------------------------
    output$pca_scores <- renderPlot({
      res <- fit_res()
      req(res)
      print(res$plots$scores)
    })

    output$pca_loadings <- renderPlot({
      res <- fit_res()
      req(res)
      print(res$plots$loadings)
    })

    output$pca_heatmap <- renderPlot({
      res <- fit_res()
      req(res)

      hm <- res$heatmap
      .draw_heatmap_safe(hm$ht)

      if (!is.null(hm$loading_min) && !is.null(hm$loading_max)) {
        try({
          ComplexHeatmap::decorate_annotation("Loading", {
            grid::grid.text(hm$loading_max,
                            x = grid::unit(-2, "mm"), y = grid::unit(0.98, "npc"),
                            just = "right", gp = grid::gpar(cex = 0.8))
            grid::grid.text(hm$loading_min,
                            x = grid::unit(-2, "mm"), y = grid::unit(0.02, "npc"),
                            just = "right", gp = grid::gpar(cex = 0.8))
          })
        }, silent = TRUE)
      }
    })

    #---------------------------
    # PDF downloads
    #---------------------------
    output$download_pca_scores_pdf <- downloadHandler(
      filename = function() {
        stem <- paste0(
          "pca_scores_", current_dataset_label(),
          "_PC", input$pc_x %||% 1, "_PC", input$pc_y %||% 2
        )
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res()
        req(res)
        grDevices::pdf(file, width = 6.5, height = 6.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(res$plots$scores)
      }
    )

    output$download_pca_loadings_pdf <- downloadHandler(
      filename = function() {
        stem <- paste0("pca_loadings_", current_dataset_label(), "_PC", input$pc_loading %||% 1)
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res()
        req(res)
        grDevices::pdf(file, width = 8.5, height = 6.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(res$plots$loadings)
      }
    )

    output$download_pca_heatmap_pdf <- downloadHandler(
      filename = function() {
        stem <- paste0(
          "pca_heatmap_", current_dataset_label(),
          "_PC", input$pc_loading %||% 1,
          "_Top", input$topn_heat %||% 20
        )
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res()
        req(res)
        hm <- res$heatmap
        grDevices::pdf(file, width = 10, height = 8, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        # legend-safe draw for PDF too
        ComplexHeatmap::draw(
          hm$ht,
          heatmap_legend_side = "bottom",
          annotation_legend_side = "bottom",
          merge_legends = TRUE,
          padding = grid::unit(c(3, 3, 3, 3), "mm")
        )
      }
    )

  })
}