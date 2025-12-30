# R/mod_plot_gene.R ----------------------------------------------------------
# ======================================================================
# Gene feature module (UI + Server)
#   - Left : single gene dot/box/violin
#   - Right: high-variance gene heatmap (ComplexHeatmap)
#   - Advanced settings is shared (Feature generic module opens modal via on_open_adv)
# ======================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(methods)
  # optional (used in heatmap / downloads)
  # library(ComplexHeatmap)
  # library(grid)
  # library(svglite)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ======================================================================
# UI
# ======================================================================

#' Plot module UI for Gene features
#'
#' @param id Module ID
#' @export
mod_plot_gene_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Gene feature"),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          style = "text-align: right; margin-bottom: 6px;"
        )
      )
    ),

    shiny::fluidRow(
      # ---- Left: single gene plot -------------------------------------------
      shinydashboard::box(
        title       = "Single gene plot",
        width       = 6,
        status      = "primary",
        solidHeader = TRUE,

        shiny::uiOutput(ns("gene_ui")),

        shiny::radioButtons(
          ns("plot_type"),
          "Plot type",
          choices  = c("dot", "box", "violin"),
          selected = "violin",
          inline   = TRUE
        ),

        shiny::br(),

        shiny::plotOutput(ns("plot_gene"), height = "360px"),
        shiny::br(),
        shiny::downloadButton(ns("dl_gene_plot"), "Download SVG")
      ),

      # ---- Right: high-variance heatmap -------------------------------------
      shinydashboard::box(
        title       = "High-variance gene heatmap",
        width       = 6,
        status      = "primary",
        solidHeader = TRUE,
        shiny::p(
          "Shows Top-N high-variance genes (Top-N and row z-score are controlled by the shared Advanced settings). ",
          "If colData has 'class', columns are ordered and annotated by that group using the shared palette."
        ),
        shiny::plotOutput(ns("plot_gene_heatmap"), height = "360px"),
        shiny::br(),
        shiny::downloadButton(ns("dl_gene_heatmap"), "Download SVG")
      )
    )
  )
}

# ======================================================================
# Server
# ======================================================================

#' Plot module server for Gene features
#'
#' @param id            Module ID
#' @param se_in         reactive() SummarizedExperiment (transcriptome)
#' @param adv_reactive  reactive() list of shared advanced settings
#' @param on_live_change function(list) | NULL
#' @param on_open_adv    function() | NULL  (open shared Advanced modal)
#'
#' @export
mod_plot_gene_server <- function(
    id,
    se_in,
    adv_reactive,
    on_live_change = NULL,
    on_open_adv    = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ==== helpers =====================================================
    `%||%` <- function(a, b) if (is.null(a)) b else a

    .safe_jitter <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x) || x < 0) 0 else x
    }

    .build_comp_args <- function(mode, ref_group, manual_df) {
      if (identical(mode, "all"))
        return(list(comparisons = NULL, ref_group = NULL))
      if (identical(mode, "ref") && nzchar(ref_group))
        return(list(comparisons = NULL, ref_group = ref_group))
      if (identical(mode, "manual") && !is.null(manual_df) && nrow(manual_df)) {
        df <- manual_df
        df <- df[stats::complete.cases(df[, c("group1", "group2")]), , drop = FALSE]
        df <- df[nzchar(df$group1) & nzchar(df$group2), , drop = FALSE]
        if (nrow(df)) {
          pairs <- lapply(seq_len(nrow(df)), function(i) {
            c(as.character(df$group1[i]), as.character(df$group2[i]))
          })
          return(list(comparisons = pairs, ref_group = NULL))
        }
      }
      list(comparisons = NULL, ref_group = NULL)
    }

    # class order policy (shared with Lipid)
    .compute_class_levels <- function(cls, adv, y_for_median = NULL) {
      cls_chr <- as.character(cls)
      groups  <- unique(cls_chr)
      if (!length(groups)) return(factor(cls_chr))

      # 1) manual_order
      if (!is.null(adv$manual_order) && length(adv$manual_order)) {
        ord <- intersect(as.character(adv$manual_order), groups)
        if (length(ord)) {
          rest <- setdiff(groups, ord)
          lv   <- c(ord, rest)
          return(factor(cls_chr, levels = lv))
        }
      }

      # 2) order_by = abundance_median
      if (!is.null(adv$order_by) &&
          identical(adv$order_by, "abundance_median") &&
          !is.null(y_for_median)) {
        med <- tapply(y_for_median, cls_chr, stats::median, na.rm = TRUE)
        med <- med[is.finite(med)]
        if (length(med)) {
          dec <- isTRUE(adv$decreasing)
          lv  <- names(sort(med, decreasing = dec))
          return(factor(cls_chr, levels = lv))
        }
      }

      # 3) fallback
      factor(cls_chr, levels = groups)
    }

    .make_palette <- function(groups, adv) {
      groups <- unique(as.character(groups))
      n <- length(groups)
      if (!n) return(NULL)

      hues <- seq(15, 375, length.out = n + 1)[1:n]
      pal  <- grDevices::hcl(h = hues, l = 65, c = 100)
      names(pal) <- groups

      if (!is.null(adv$palette_map) && length(adv$palette_map)) {
        nm <- intersect(names(adv$palette_map), groups)
        pal[nm] <- adv$palette_map[nm]
      }
      pal
    }

    # ==== Advanced button -> shared modal ============================
    shiny::observeEvent(input$open_adv, {
      if (is.function(on_open_adv)) on_open_adv()
    }, ignoreInit = TRUE)

    # ==== Gene list UI ===============================================
    output$gene_ui <- shiny::renderUI({
      se <- se_in(); shiny::req(se)
      genes <- rownames(se)
      shiny::selectInput(ns("gene_id"), "Gene (feature_id)", choices = genes)
    })

    shiny::observeEvent(list(se_in(), input$gene_id), {
      se <- se_in(); shiny::req(se)
      genes <- rownames(se)
      if (is.null(input$gene_id) || !nzchar(input$gene_id) || !input$gene_id %in% genes) {
        if (length(genes)) {
          shiny::updateSelectInput(session, "gene_id", choices = genes, selected = genes[1])
        }
      }
    }, ignoreInit = TRUE)

    # ==== sync plot_type with adv =====================================
    shiny::observeEvent(adv_reactive(), {
      adv <- adv_reactive()
      pt  <- adv$plot_type %||% "violin"
      if (!is.null(input$plot_type) && identical(input$plot_type, pt)) return()
      shiny::updateRadioButtons(session, "plot_type", selected = pt)
    }, ignoreInit = TRUE)

    # ==== live notifications ==========================================
    shiny::observeEvent(input$plot_type, {
      if (is.function(on_live_change)) {
        on_live_change(list(plot_type = input$plot_type, dataset = "gene"))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$gene_id, {
      if (is.function(on_live_change)) {
        on_live_change(list(gene_id = input$gene_id, dataset = "gene"))
      }
    }, ignoreInit = TRUE)

    # ==================================================================
    # Single gene plot
    #   - uses shared palette_map + manual_order/order_by policy
    # ==================================================================
    p_gene <- shiny::reactive({
      se  <- se_in(); shiny::req(se, input$gene_id)
      adv <- adv_reactive(); shiny::req(adv)

      shiny::req(exists("theme_lipidomics", mode = "function"))
      shiny::req(exists("plot_dot_se",      mode = "function") ||
                   exists("plot_box_se",    mode = "function") ||
                   exists("plot_violin_se", mode = "function"))

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      x_var <- "class"
      if (!"class" %in% colnames(cd)) {
        stop("colData(se)$class が見つかりません (Gene feature はサンプルの group として class を想定しています)。")
      }

      y_vec <- as.numeric(SummarizedExperiment::assay(se, 1)[input$gene_id, ])

      # factorize class with shared ordering logic
      cls_factor <- .compute_class_levels(cd$class, adv, y_for_median = y_vec)
      groups     <- levels(cls_factor)
      x_order    <- groups

      # clone se and replace colData$class (avoid mutating input object)
      se2 <- se
      SummarizedExperiment::colData(se2)$class <- cls_factor

      pal <- .make_palette(groups, adv)
      cmp <- .build_comp_args(adv$comp_mode, adv$ref_group, adv$manual_pairs)

      dot_jw    <- .safe_jitter(adv$dot_jitter_width)
      box_jw    <- .safe_jitter(adv$box_jitter_width)
      violin_jw <- .safe_jitter(adv$violin_jitter_width)

      pt <- input$plot_type %||% (adv$plot_type %||% "violin")

      if (identical(pt, "dot")) {
        fun  <- get("plot_dot_se", mode = "function")
        extra <- list(
          point_size   = adv$dot_point_size,
          jitter_width = dot_jw,
          point_alpha  = adv$dot_alpha,
          show_median  = adv$dot_show_median,
          median_size  = adv$dot_median_size,
          median_width = adv$dot_median_width,
          median_color = adv$dot_median_color
        )
      } else if (identical(pt, "box")) {
        fun  <- get("plot_box_se", mode = "function")
        extra <- list(
          box_width    = adv$box_width,
          box_alpha    = adv$box_alpha,
          show_points  = adv$box_show_points,
          point_size   = adv$box_point_size,
          jitter_width = box_jw,
          point_alpha  = adv$box_point_alpha
        )
      } else {
        fun  <- get("plot_violin_se", mode = "function")
        extra <- list(
          violin_width = adv$violin_width,
          violin_alpha = adv$violin_alpha,
          trim         = adv$violin_trim,
          show_points  = adv$violin_show_points,
          point_size   = adv$violin_point_size,
          jitter_width = violin_jw,
          point_alpha  = adv$violin_point_alpha,
          show_median  = adv$violin_show_median,
          median_size  = adv$violin_median_size,
          median_color = adv$violin_median_color
        )
      }

      p <- do.call(fun, c(list(
        se         = se2,
        feature_id = input$gene_id,
        x_var      = x_var,
        x_order    = x_order,
        order_by   = "none",
        decreasing = FALSE,
        palette    = pal,
        add_p      = isTRUE(adv$add_p),
        test       = adv$test,
        comparisons= cmp$comparisons,
        ref_group  = cmp$ref_group,
        p_adjust   = "BH",
        p_label    = adv$p_label
      ), extra)) +
        theme_lipidomics(11, 0, 10, 10, 12) +
        ggplot2::theme(legend.position = "none", aspect.ratio = 1) +
        ggplot2::labs(title = paste0("Gene: ", input$gene_id))

      p
    })

    output$plot_gene <- shiny::renderPlot({
      print(p_gene())
    })

    output$dl_gene_plot <- shiny::downloadHandler(
      filename = function() paste0("gene_plot_", input$gene_id %||% "gene", ".svg"),
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        svglite::svglite(file, width = 6, height = 5)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(p_gene())
      }
    )

    # ==================================================================
    # High-variance gene heatmap
    #   - uses adv$hm_topN and adv$hm_row_z
    #   - uses class ordering + palette_map if colData$class exists
    # ==================================================================
    ht_gene <- shiny::reactive({
      se  <- se_in(); shiny::req(se)
      adv <- adv_reactive(); shiny::req(adv)

      if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) return(NULL)

      mat <- as.matrix(SummarizedExperiment::assay(se, 1))
      if (is.null(rownames(mat))) rownames(mat) <- as.character(seq_len(nrow(mat)))

      vars <- apply(mat, 1L, stats::var, na.rm = TRUE)
      ok   <- is.finite(vars) & !is.na(vars)
      if (!any(ok)) return(NULL)

      vars <- vars[ok]
      ord  <- order(vars, decreasing = TRUE)

      topN <- adv$hm_topN %||% 50L
      topN <- as.integer(topN)
      if (!is.finite(topN) || topN <= 0) topN <- 50L
      topN <- max(1L, min(length(ord), topN))
      sel  <- ord[seq_len(topN)]

      mat_sub <- mat[sel, , drop = FALSE]

      if (isTRUE(adv$hm_row_z)) {
        mat_sub <- t(scale(t(mat_sub)))
      }

      cd <- as.data.frame(SummarizedExperiment::colData(se))

      ha <- NULL

      if ("class" %in% colnames(cd)) {
        cls_factor <- .compute_class_levels(cd$class, adv, y_for_median = NULL)
        groups     <- levels(cls_factor)
        pal        <- .make_palette(groups, adv)

        ord_cols   <- order(as.numeric(cls_factor))
        mat_sub    <- mat_sub[, ord_cols, drop = FALSE]
        cls_factor <- cls_factor[ord_cols]

        ha <- ComplexHeatmap::HeatmapAnnotation(
          class = cls_factor,
          col   = list(class = pal)
        )
      }

      ComplexHeatmap::Heatmap(
        mat_sub,
        name              = "expr",
        top_annotation    = ha,
        cluster_rows      = TRUE,
        cluster_columns   = FALSE,
        show_row_names    = TRUE,
        show_column_names = TRUE
      )
    })

    output$plot_gene_heatmap <- shiny::renderPlot({
      ht <- ht_gene()
      shiny::req(ht)

      if (!requireNamespace("ComplexHeatmap", quietly = TRUE) ||
          !requireNamespace("grid", quietly = TRUE)) {
        plot.new(); text(0.5, 0.5, "ComplexHeatmap/grid is not installed.")
        return()
      }

      grid::grid.newpage()
      ComplexHeatmap::draw(ht, newpage = FALSE)
    })

    output$dl_gene_heatmap <- shiny::downloadHandler(
      filename = function() "gene_heatmap_highvar.svg",
      content  = function(file) {
        ht <- ht_gene()
        shiny::req(ht)
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) stop("Please install ComplexHeatmap.")
        if (!requireNamespace("grid", quietly = TRUE)) stop("grid is required.")

        svglite::svglite(file, width = 7, height = 6)
        on.exit(grDevices::dev.off(), add = TRUE)
        grid::grid.newpage()
        ComplexHeatmap::draw(ht, newpage = FALSE)
      }
    )
  })
}
