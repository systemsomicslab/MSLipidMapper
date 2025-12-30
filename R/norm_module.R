# =========================
# Shiny module: normalization + QC plot + download (data + figure)
# - Method + description aligned
# - Download controls under the plot, horizontal & vertically aligned
# - Download buttons are gray (btn-default)
# - QC log10 visualization uses pseudo offset = max(0.5 * min_positive_value, 1e-9)
# =========================

library(shiny)
library(shinydashboard)
library(SummarizedExperiment)
library(ggplot2)
library(htmltools)

# Safe %||% (in case your shiny version doesn't provide it)
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' @export
mod_normalize_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # ---- scoped ids for CSS targeting ----
  method_area_id <- ns("method_area")
  dl_id <- ns("dl_controls")
  
  css <- sprintf(
    "
    /* ===== Method row alignment ===== */
    #%s.method-row{
      display:flex;
      align-items:flex-end;
      gap:12px;
      flex-wrap:wrap;
    }
    #%s.method-row .form-group{ margin-bottom:0; }
    #%s.method-row .desc-wrap label{
      display:block;
      margin-bottom:5px;
      visibility:hidden; /* keep height, hide text */
    }

    /* ===== Download row alignment ===== */
    #%s.dl-row{
      display:flex;
      flex-wrap:wrap;
      gap:12px;
      align-items:flex-end;
    }
    #%s.dl-row .form-group{ margin-bottom:0; }
    #%s.dl-row .dl-item{ min-width:220px; }
    #%s.dl-row .btn-wrap label{
      display:block;
      margin-bottom:5px;
      visibility:hidden; /* keep height, hide text */
    }
    ",
    method_area_id, method_area_id, method_area_id,
    dl_id, dl_id, dl_id, dl_id
  )
  
  shiny::tagList(
    shiny::h3("Step 2: Lipidome data Normalization"),
    
    # Inject scoped CSS once per module instance
    shiny::tags$style(shiny::HTML(css)),
    
    # --- Upper: settings card ---
    shiny::fluidRow(
      shinydashboard::box(
        title       = "Normalization settings",
        width       = 12,
        status      = "primary",
        solidHeader = TRUE,
        
        # Method selector + description (aligned)
        shiny::tags$div(
          id = method_area_id,
          class = "method-row",
          
          shiny::div(
            style = "min-width:260px;",
            shiny::selectInput(
              ns("method"), "Method",
              c("none", "log2", "sum", "median", "zscore"),
              selected = "none"
            )
          ),
          
          shiny::div(
            class = "desc-wrap",
            style = "flex:1; min-width:280px;",
            shiny::tags$label("\u00A0"),   # invisible label line to align with "Method"
            shiny::uiOutput(ns("method_desc"))
          )
        ),
        
        # log2 offset
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'log2'", ns("method")),
          shiny::numericInput(
            ns("offset"), "log2 offset",
            value = 1e-9, min = 0, step = 1e-9
          )
        ),
        
        shiny::actionButton(
          ns("apply"),
          "Apply Normalization",
          class = "btn-success"
        )
      )
    ),
    
    # --- Lower: QC plot card (downloads under the plot) ---
    shiny::fluidRow(
      shinydashboard::box(
        title       = "Per-sample intensity after normalization (log10)",
        width       = 12,
        status      = "primary",
        solidHeader = TRUE,
        
        shiny::sliderInput(
          ns("x_angle"),
          "X label angle",
          min = 0, max = 90, value = 45, step = 5
        ),
        
        shiny::plotOutput(ns("qc_boxdot"), height = "600px"),
        
        shiny::tags$hr(),
        
        # Simple note: "half of the minimum positive value" (+ lower bound)
        shiny::tags$div(
          style = "margin-bottom:10px; color:#374151;",
          shiny::HTML(
            "Note: For log10 visualization, an offset <code>pseudo</code> is added as
             <code>pseudo = max(0.5 × (minimum positive value), 1e-9)</code>, then we plot <code>log10(value + pseudo)</code>."
          )
        ),
        
        shiny::tags$div(
          id = dl_id,
          class = "dl-row",
          
          # Plot format
          shiny::tags$div(
            class = "dl-item",
            shiny::selectInput(
              ns("plot_format"),
              "Plot format",
              choices = c("PNG" = "png", "PDF" = "pdf"),
              selected = "png"
            )
          ),
          
          # Download plot (gray)
          shiny::tags$div(
            class = "dl-item btn-wrap",
            shiny::tags$label("\u00A0"),
            shiny::downloadButton(
              ns("download_qc"),
              "Download plot",
              class = "btn-default"
            )
          ),
          
          # Data format
          shiny::tags$div(
            class = "dl-item",
            shiny::selectInput(
              ns("dl_format"),
              "Download data format",
              choices = c(
                "Matrix (CSV)" = "matrix_csv",
                "Matrix (TSV)" = "matrix_tsv",
                "SummarizedExperiment (RDS)" = "se_rds"
              ),
              selected = "matrix_csv"
            )
          ),
          
          # Download data (gray)
          shiny::tags$div(
            class = "dl-item btn-wrap",
            shiny::tags$label("\u00A0"),
            shiny::downloadButton(
              ns("download_norm"),
              "Download normalized data",
              class = "btn-default"
            )
          )
        )
      )
    )
  )
}

#' @export
mod_normalize_server <- function(id, se_in) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # ---- Method explanations ----
    .method_explain <- function(m) {
      desc <- list(
        none   = "<b>none</b>: No normalization applied (raw intensities are used as-is).",
        log2   = "<b>log2</b>: Applies <code>log2(x + offset)</code> transformation to compress scale and reduce outlier impact.",
        sum    = "<b>sum</b>: Scales each sample column so that total intensity (TIC) equals a target (default = column median).",
        median = "<b>median</b>: Scales each sample so that the column median equals a common target (robust to outliers).",
        zscore = "<b>zscore</b>: Standardizes each feature across samples to zero mean and unit variance."
      )
      html <- desc[[m]] %||% ""
      htmltools::HTML(
        sprintf(
          '<div style="background:#f8fafc;border:1px solid #e5e7eb;border-radius:8px;padding:10px;">%s</div>',
          html
        )
      )
    }
    
    output$method_desc <- shiny::renderUI({
      .method_explain(input$method %||% "none")
    })
    
    # ---- Apply normalization (button-triggered) ----
    se_norm <- shiny::eventReactive(input$apply, {
      se <- se_in()
      shiny::req(se)
      
      # Keep original if "none"
      if (identical(input$method, "none")) {
        shiny::showNotification("Normalization applied: none (raw)", type = "message", duration = 2)
        return(se)
      }
      
      ctl <- switch(input$method,
                    "log2" = list(offset = input$offset),
                    list()
      )
      
      se2 <- normalize_se(se, method = input$method, control = ctl)
      
      shiny::showNotification(
        paste0("Normalization applied: ", input$method),
        type = "message", duration = 2
      )
      
      se2
    }, ignoreInit = TRUE)
    
    # ---- Helper: get matrix from SE safely ----
    .get_norm_matrix <- function(se) {
      an <- SummarizedExperiment::assayNames(se)
      if ("abundance" %in% an) {
        as.matrix(SummarizedExperiment::assay(se, "abundance"))
      } else if (length(an) > 0) {
        as.matrix(SummarizedExperiment::assay(se, 1))
      } else {
        stop("No assays found in SummarizedExperiment.")
      }
    }
    
    # ---- Helper: build QC ggplot (shared by renderPlot + download) ----
    .make_qc_plot <- function(se, x_angle) {
      mat <- .get_norm_matrix(se)
      
      shiny::validate(
        shiny::need(ncol(mat) > 0, "No samples (columns) found."),
        shiny::need(nrow(mat) > 0, "No features (rows) found.")
      )
      
      # Performance safeguard for huge matrices
      max_feat <- 20000L
      if (nrow(mat) > max_feat) {
        set.seed(1)
        mat <- mat[sample.int(nrow(mat), max_feat), , drop = FALSE]
      }
      
      n_feat <- nrow(mat)
      samp <- colnames(mat) %||% paste0("S", seq_len(ncol(mat)))
      
      df <- data.frame(
        sample = rep(samp, each = n_feat),
        value  = as.vector(mat),
        stringsAsFactors = FALSE
      )
      df <- df[is.finite(df$value) & !is.na(df$value), , drop = FALSE]
      
      # pseudo offset: half of the minimum positive value, with a lower bound
      pos <- df$value[df$value > 0]
      pseudo <- if (length(pos)) max(min(pos, na.rm = TRUE) * 0.5, 1e-9) else 1e-9
      df$log10v <- log10(df$value + pseudo)
      
      ggplot2::ggplot(df, ggplot2::aes(x = .data$sample, y = .data$log10v, color = .data$sample)) +
        ggplot2::geom_point(
          position = ggplot2::position_jitter(width = 0.15, height = 0),
          size = 0.7, alpha = 0.6, show.legend = FALSE
        ) +
        ggplot2::geom_boxplot(
          color = "black", outlier.shape = NA, width = 0.6, alpha = 0.45, show.legend = FALSE
        ) +
        ggplot2::labs(
          x = "sample",
          y = sprintf("log10(intensity + %g)", pseudo),
          title = "Per-sample distributions after normalization"
        ) +
        {
          if (exists("theme_lipidomics", mode = "function")) {
            theme_lipidomics(x_angle = x_angle) +
              ggplot2::theme(
                legend.position = "none",
                axis.text.x = ggplot2::element_text(size = 8)
              )
          } else {
            ggplot2::theme_bw() +
              ggplot2::theme(
                legend.position = "none",
                axis.text.x = ggplot2::element_text(
                  angle = x_angle, size = 8,
                  vjust = ifelse(x_angle == 0, 0.5, 1),
                  hjust = ifelse(x_angle == 0, 0.5, 1)
                )
              )
          }
        }
    }
    
    # ---- QC plot output ----
    output$qc_boxdot <- shiny::renderPlot({
      se <- se_norm()
      shiny::req(se)
      .make_qc_plot(se, x_angle = input$x_angle)
    })
    
    # ---- Download: normalized data ----
    output$download_norm <- shiny::downloadHandler(
      filename = function() {
        ext <- switch(input$dl_format,
                      matrix_csv = "csv",
                      matrix_tsv = "tsv",
                      se_rds     = "rds"
        )
        paste0(
          "normalized_", (input$method %||% "none"), "_",
          format(Sys.Date(), "%Y%m%d"), ".", ext
        )
      },
      content = function(file) {
        se <- se_norm()
        shiny::req(se)
        
        if (input$dl_format %in% c("matrix_csv", "matrix_tsv")) {
          mat <- .get_norm_matrix(se)
          out <- data.frame(
            feature = rownames(mat) %||% paste0("F", seq_len(nrow(mat))),
            mat,
            check.names = FALSE
          )
          
          if (identical(input$dl_format, "matrix_csv")) {
            utils::write.csv(out, file, row.names = FALSE)
          } else {
            utils::write.table(out, file, sep = "\t", quote = FALSE, row.names = FALSE)
          }
          
        } else if (identical(input$dl_format, "se_rds")) {
          saveRDS(se, file)
        }
      }
    )
    
    # ---- Download: QC plot (figure) ----
    output$download_qc <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "qc_plot_", (input$method %||% "none"), "_",
          format(Sys.Date(), "%Y%m%d"), ".", (input$plot_format %||% "png")
        )
      },
      content = function(file) {
        se <- se_norm()
        shiny::req(se)
        
        p <- .make_qc_plot(se, x_angle = input$x_angle)
        ext <- input$plot_format %||% "png"
        
        if (identical(ext, "png")) {
          ggplot2::ggsave(filename = file, plot = p, width = 12, height = 6, dpi = 300)
        } else {
          if (capabilities("cairo")) {
            ggplot2::ggsave(filename = file, plot = p, width = 12, height = 6, device = grDevices::cairo_pdf)
          } else {
            ggplot2::ggsave(filename = file, plot = p, width = 12, height = 6, device = "pdf")
          }
        }
      }
    )
    
    list(
      se    = shiny::reactive(se_norm()),
      ready = shiny::reactive(!is.null(se_norm()))
    )
  })
}

