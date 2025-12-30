# R/mod_volcano.R
suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(DT)
  library(shinydashboard)
})

# ---- helper: pick a rowData column safely (Ontology etc.) -------------------
.pick_rowdata_col <- function(se, preferred = "Ontology",
                              candidates = c("Ontology", "SubClass", "Subclass", "Lipid subclass",
                                             "ontology", "subclass")) {
  rd <- try(as.data.frame(SummarizedExperiment::rowData(se)), silent = TRUE)
  if (inherits(rd, "try-error") || is.null(rd) || nrow(rd) == 0) return(list(name = NA_character_, vec = NULL))

  cn <- colnames(rd)
  # 1) exact preferred
  if (!is.null(preferred) && preferred %in% cn) {
    return(list(name = preferred, vec = rd[[preferred]]))
  }
  # 2) exact candidates
  hit <- candidates[candidates %in% cn]
  if (length(hit) > 0) {
    return(list(name = hit[1], vec = rd[[hit[1]]]))
  }
  # 3) case-insensitive match
  cn_low <- tolower(cn)
  for (cand in unique(tolower(c(preferred, candidates)))) {
    if (is.na(cand) || !nzchar(cand)) next
    j <- which(cn_low == cand)
    if (length(j) > 0) {
      nm <- cn[j[1]]
      return(list(name = nm, vec = rd[[nm]]))
    }
  }
  list(name = NA_character_, vec = NULL)
}

# ------------------------------------------------------------
# Volcano function (SE -> volcano; ggplot + (optional) plotly)
# - FC threshold: numeric (fc_thresh >= 1)
# - P-value mode switch:
#   * p_mode = "adj" -> use adjusted p (padj)
#   * p_mode = "raw" -> use raw p (pval)
# - Add rowData Ontology/subclass column to result table
# ------------------------------------------------------------
volcano_from_msdial_se <- function(
    se,
    group_col,
    group_a,
    group_b,
    assay_name,
    test        = c("t", "wilcox"),
    p_adj       = c("BH","bonferroni","holm","fdr","BY","hochberg"),
    p_mode      = c("adj","raw"),
    p_thresh    = 0.05,
    fc_thresh   = 2,              # numeric now (>=1)
    point_size  = 1.4,
    point_alpha = 0.8,
    cap_inf     = TRUE,
    ontology_col = "Ontology"     # <- NEW: preferred column name
) {
  stopifnot(inherits(se, "SummarizedExperiment"))
  test   <- match.arg(test)
  p_adj  <- match.arg(p_adj)
  p_mode <- match.arg(p_mode)

  if (!is.numeric(fc_thresh) || length(fc_thresh) != 1 || is.na(fc_thresh) || fc_thresh < 1) {
    stop("fc_thresh must be a numeric scalar >= 1.")
  }
  if (!is.numeric(p_thresh) || length(p_thresh) != 1 || is.na(p_thresh) || p_thresh <= 0 || p_thresh >= 1) {
    stop("p_thresh must be a numeric scalar in (0, 1).")
  }

  if (!assay_name %in% SummarizedExperiment::assayNames(se)) {
    stop("assay_name not found in assayNames(se).")
  }

  mat <- SummarizedExperiment::assay(se, assay_name)
  cd  <- as.data.frame(SummarizedExperiment::colData(se))
  if (!group_col %in% colnames(cd)) stop("group_col not found in colData(se).")

  idx_a <- which(cd[[group_col]] == group_a)
  idx_b <- which(cd[[group_col]] == group_b)
  if (length(idx_a) == 0 || length(idx_b) == 0) {
    stop("No samples found for group_a and/or group_b in colData(se)[[group_col]].")
  }

  x <- mat[, idx_a, drop = FALSE]
  y <- mat[, idx_b, drop = FALSE]

  mean_a <- rowMeans(x, na.rm = TRUE)
  mean_b <- rowMeans(y, na.rm = TRUE)

  fc <- mean_b / mean_a
  fc[mean_a == 0 & mean_b == 0] <- NA_real_
  log2fc <- log2(fc)

  nfeat <- nrow(mat)
  pval <- vapply(seq_len(nfeat), function(i) {
    xi <- as.numeric(x[i, ])
    yi <- as.numeric(y[i, ])
    xi <- xi[!is.na(xi)]
    yi <- yi[!is.na(yi)]

    if (test == "t") {
      if (length(xi) < 2 || length(yi) < 2) return(NA_real_)
      out <- try(stats::t.test(yi, xi), silent = TRUE)
    } else {
      if (length(xi) < 1 || length(yi) < 1) return(NA_real_)
      out <- try(stats::wilcox.test(yi, xi, exact = FALSE), silent = TRUE)
    }
    if (inherits(out, "try-error")) NA_real_ else out$p.value
  }, numeric(1))

  padj <- stats::p.adjust(pval, method = p_adj)

  p_use <- if (p_mode == "adj") padj else pval
  sig_ok <- !is.na(p_use) & (p_use < p_thresh)

  status <- rep("NS", length(fc))
  status[sig_ok & (fc >= fc_thresh)]     <- "Up"
  status[sig_ok & (fc <= 1 / fc_thresh)] <- "Down"
  status <- factor(status, levels = c("Up", "Down", "NS"))

  log2fc_plot <- log2fc
  if (cap_inf) {
    finite_vals <- log2fc_plot[is.finite(log2fc_plot)]
    if (length(finite_vals) > 0) {
      hi <- max(finite_vals, na.rm = TRUE)
      lo <- min(finite_vals, na.rm = TRUE)
      log2fc_plot[is.infinite(log2fc_plot) & log2fc_plot > 0] <- hi + 0.5
      log2fc_plot[is.infinite(log2fc_plot) & log2fc_plot < 0] <- lo - 0.5
    } else {
      log2fc_plot[is.infinite(log2fc_plot)] <- NA_real_
    }
  } else {
    log2fc_plot[!is.finite(log2fc_plot)] <- NA_real_
  }

  feat <- rownames(se)
  if (is.null(feat)) feat <- rownames(mat)
  if (is.null(feat)) feat <- paste0("feature_", seq_len(nfeat))

  # ---- NEW: Ontology/subclass extraction from rowData -----------------------
  ont_pick <- .pick_rowdata_col(se, preferred = ontology_col)
  ont_vec <- ont_pick$vec
  if (is.null(ont_vec)) {
    ont_vec <- rep(NA_character_, nfeat)
    ont_name <- ontology_col
  } else {
    # coerce to character for CSV/DT safety
    ont_vec <- as.character(ont_vec)
    ont_name <- ifelse(is.na(ont_pick$name), ontology_col, ont_pick$name)
  }

  mean_a_name <- paste0("mean_", group_a)
  mean_b_name <- paste0("mean_", group_b)

  res <- data.frame(
    feature = feat,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Put Ontology column early (after feature)
  res[[ont_name]]       <- ont_vec
  res[[mean_a_name]]    <- mean_a
  res[[mean_b_name]]    <- mean_b
  res[["fc_meanratio"]] <- fc
  res[["log2FC"]]       <- log2fc
  res[["p_value"]]      <- pval
  res[["p_adj"]]        <- padj
  res[["p_used"]]       <- p_use
  res[["p_mode"]]       <- p_mode
  res[["status"]]       <- status

  p_plot <- res[["p_used"]]
  p_plot <- ifelse(is.na(p_plot), NA_real_, pmax(p_plot, .Machine$double.xmin))

  df_plot <- transform(
    res,
    neglog10_p = -log10(p_plot),
    log2FC_plot = log2fc_plot
  )

  fill_map <- c(Up = "#D62728", Down = "#1F77B4", NS = "#7F7F7F")
  lfc_line <- log2(fc_thresh)
  xlab_txt <- paste0("log2FC (mean ", group_b, " / mean ", group_a, ")")
  ylab_txt <- if (p_mode == "adj") paste0("-log10(adj p) [", p_adj, "]") else "-log10(raw p)"

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = log2FC_plot, y = neglog10_p)) +
    ggplot2::geom_point(
      ggplot2::aes(fill = status),
      shape = 21, color = "black",
      size  = point_size, alpha = point_alpha,
      na.rm = TRUE
    ) +
    ggplot2::geom_vline(xintercept = c(-lfc_line, lfc_line), linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -log10(p_thresh), linetype = "dashed") +
    ggplot2::scale_fill_manual(values = fill_map, drop = FALSE) +
    ggplot2::labs(
      title = paste0("Volcano: ", group_b, " vs ", group_a),
      x = xlab_txt,
      y = ylab_txt,
      fill = NULL
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(aspect.ratio = 1)

  ply <- NULL
  if (requireNamespace("plotly", quietly = TRUE)) {
    df_plot$status_chr <- as.character(df_plot$status)
    df_plot$hover_text <- paste0(
      "feature: ", df_plot$feature,
      "<br>", ont_name, ": ", ifelse(is.na(df_plot[[ont_name]]), "", df_plot[[ont_name]]),
      "<br>log2FC: ", signif(df_plot$log2FC, 5),
      "<br>FC: ", signif(df_plot$fc_meanratio, 5),
      "<br>raw p: ", signif(df_plot$p_value, 5),
      "<br>adj p: ", signif(df_plot$p_adj, 5),
      "<br>used p: ", signif(df_plot$p_used, 5), " (", df_plot$p_mode, ")",
      "<br>status: ", df_plot$status_chr
    )

    marker_sz <- max(4, point_size * 4)

    ply <- plotly::plot_ly()
    for (st in c("Up", "Down", "NS")) {
      dsub <- df_plot[df_plot$status_chr == st, , drop = FALSE]
      ply <- plotly::add_trace(
        ply,
        data = dsub,
        x = ~log2FC_plot, y = ~neglog10_p,
        type = "scatter", mode = "markers",
        name = st,
        text = ~hover_text, hoverinfo = "text",
        marker = list(
          symbol  = "circle",
          size    = marker_sz,
          opacity = point_alpha,
          color   = fill_map[[st]],
          line    = list(color = "black", width = 1)
        )
      )
    }

    ply <- plotly::layout(
      ply,
      title = list(text = paste0("Volcano: ", group_b, " vs ", group_a)),
      xaxis = list(title = xlab_txt),
      yaxis = list(title = ylab_txt),
      shapes = list(
        list(type="line", x0=-lfc_line, x1=-lfc_line, y0=0, y1=1, xref="x", yref="paper", line=list(dash="dash")),
        list(type="line", x0= lfc_line, x1= lfc_line, y0=0, y1=1, xref="x", yref="paper", line=list(dash="dash")),
        list(type="line", x0=0, x1=1, y0=-log10(p_thresh), y1=-log10(p_thresh), xref="paper", yref="y", line=list(dash="dash"))
      )
    )
  }

  list(plot = p, plotly = ply, table = res)
}

# ------------------------------------------------------------
# Module UI
# ------------------------------------------------------------
mod_volcano_ui <- function(id, title = "Volcano") {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        shinydashboard::box(
          title = "Settings",
          width = 12,
          status = "primary",
          solidHeader = TRUE,

          radioButtons(
            ns("dataset"),
            "Dataset",
            choices = c("Lipid" = "lipid", "Gene" = "gene"),
            selected = "lipid",
            inline = TRUE
          ),

          uiOutput(ns("ui_groups")),

          selectInput(ns("test"), "Test", choices = c("wilcox", "t"), selected = "wilcox"),
          selectInput(ns("p_adj"), "P-value adjustment", choices = c("BH","bonferroni","holm","fdr","BY","hochberg"), selected = "BH"),

          radioButtons(
            ns("p_mode"),
            "P-value used in volcano",
            choices = c("Adjusted (padj)" = "adj", "Raw (p)" = "raw"),
            selected = "adj",
            inline = FALSE
          ),

          numericInput(ns("p_thresh"), "P-value threshold", value = 0.05, min = 1e-7, max = 0.9999999, step = 0.01),

          numericInput(ns("fc_thresh"), "FC threshold (>=1, numeric)", value = 2, min = 1, step = 0.1),

          numericInput(ns("point_size"), "Point size", value = 2.0, min = 0.1, step = 0.1),

          selectInput(ns("view"), "View", choices = c("ggplot", "plotly"), selected = "plotly"),

          actionButton(ns("run"), "Run", icon = icon("play"), width = "100%"),

          tags$hr(),
          downloadButton(ns("dl_plot"),  "Download plot (PDF)",  width = "100%"),
          downloadButton(ns("dl_table"), "Download table (CSV)", width = "100%")
        )
      ),

      column(
        width = 9,
        shinydashboard::box(
          title = title,
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel("Plot", uiOutput(ns("ui_plot"))),
            tabPanel("Table", DTOutput(ns("tbl")))
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------
# Module server
# ------------------------------------------------------------
mod_volcano_server <- function(
    id,
    se_lipid,
    se_tx,

    default = "class",
    default_group_col = NULL,
    group_col_fixed   = NULL,

    assay_lipid_fixed   = "abundance",
    assay_gene_priority = c("logcounts", "abundance"),

    ontology_col = "Ontology"   # <- NEW: prefer this column from rowData
) {
  group_col_use <- default
  if (!is.null(default_group_col)) group_col_use <- default_group_col
  if (!is.null(group_col_fixed))   group_col_use <- group_col_fixed

  .get_se <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.function(x)) return(x())
    x
  }

  moduleServer(id, function(input, output, session) {

    se_selected <- reactive({
      if (identical(input$dataset, "gene")) .get_se(se_tx) else .get_se(se_lipid)
    })

    assay_selected <- reactive({
      se <- se_selected()
      if (is.null(se)) return(NA_character_)
      an <- SummarizedExperiment::assayNames(se)
      if (length(an) == 0) return(NA_character_)

      if (identical(input$dataset, "lipid")) {
        if (assay_lipid_fixed %in% an) return(assay_lipid_fixed)
        return(an[1])
      }

      for (cand in assay_gene_priority) {
        if (cand %in% an) return(cand)
      }
      an[1]
    })

    groups <- reactive({
      se <- se_selected()
      if (is.null(se)) return(character(0))
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!group_col_use %in% colnames(cd)) return(character(0))
      sort(unique(as.character(cd[[group_col_use]])))
    })

    output$ui_groups <- renderUI({
      g <- groups()
      if (length(g) == 0) {
        return(helpText(paste0(
          "No groups available. Please confirm colData(se)$", group_col_use,
          " exists for the selected dataset."
        )))
      }
      tagList(
        selectInput(session$ns("group_a"), "Group A (baseline)", choices = g, selected = g[1]),
        selectInput(session$ns("group_b"), "Group B (compare)",  choices = g, selected = if (length(g) >= 2) g[2] else g[1])
      )
    })

    rv <- reactiveValues(out = NULL)

    observeEvent(input$run, {
      se <- se_selected()
      req(se)
      req(input$group_a, input$group_b)

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!group_col_use %in% colnames(cd)) {
        showNotification(paste0("colData column not found: ", group_col_use), type = "error")
        return(NULL)
      }

      assay_name <- assay_selected()
      if (is.na(assay_name) || !assay_name %in% SummarizedExperiment::assayNames(se)) {
        showNotification("No valid assay found for the selected dataset.", type = "error")
        return(NULL)
      }

      rv$out <- volcano_from_msdial_se(
        se          = se,
        group_col   = group_col_use,
        group_a     = input$group_a,
        group_b     = input$group_b,
        assay_name  = assay_name,
        test        = input$test,
        p_adj       = input$p_adj,
        p_mode      = input$p_mode,
        p_thresh    = input$p_thresh,
        fc_thresh   = input$fc_thresh,
        point_size  = input$point_size,
        point_alpha = 0.8,
        cap_inf     = TRUE,
        ontology_col = ontology_col   # <- NEW
      )
    }, ignoreInit = TRUE)

    output$ui_plot <- renderUI({
      req(rv$out)
      if (identical(input$view, "plotly")) {
        if (!requireNamespace("plotly", quietly = TRUE) || is.null(rv$out$plotly)) {
          return(helpText("plotly is not available. Install with: install.packages('plotly')"))
        }
        plotly::plotlyOutput(session$ns("ply"), height = "650px")
      } else {
        plotOutput(session$ns("gg"), height = "650px")
      }
    })

    output$gg <- renderPlot({ req(rv$out); rv$out$plot })
    output$ply <- plotly::renderPlotly({ req(rv$out); rv$out$plotly })

    output$tbl <- renderDT({
      req(rv$out)
      datatable(rv$out$table, options = list(pageLength = 25, scrollX = TRUE))
    })

    output$dl_plot <- downloadHandler(
      filename = function() {
        se_type <- if (identical(input$dataset, "gene")) "gene" else "lipid"
        paste0(
          "volcano_", se_type, "_",
          input$group_b, "_vs_", input$group_a,
          "_p-", input$p_mode, ".pdf"
        )
      },
      content = function(file) {
        req(rv$out)
        devfun <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
        ggplot2::ggsave(
          filename = file,
          plot     = rv$out$plot,
          device   = devfun,
          width    = 6,
          height   = 6,
          units    = "in"
        )
      }
    )

    output$dl_table <- downloadHandler(
      filename = function() {
        se_type <- if (identical(input$dataset, "gene")) "gene" else "lipid"
        paste0(
          "volcano_table_", se_type, "_",
          input$group_b, "_vs_", input$group_a,
          "_p-", input$p_mode, ".csv"
        )
      },
      content = function(file) {
        req(rv$out)
        utils::write.csv(rv$out$table, file = file, row.names = FALSE)
      }
    )
  })
}

