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
  
  css <- "
    .norm-settings-grid{
      display:grid;
      grid-template-columns:minmax(240px,1.1fr) 100px minmax(190px,.8fr) minmax(270px,1.2fr);
      gap:8px;
      align-items:start;
      padding:9px;
      margin-bottom:10px;
      border:1px solid #dbe3ea;
      border-radius:6px;
      background:#f7f9fb;
    }
    .norm-setting-group{
      min-width:0;
      padding:0 8px;
      border-right:1px solid #e1e7ec;
    }
    .norm-setting-group:last-child{ border-right:0; }
    .norm-setting-group > label{
      display:block;
      margin:0 0 4px;
      color:#526678;
      font-size:10px;
      font-weight:600;
    }
    .norm-control-line{ display:flex; align-items:center; gap:6px; }
    .norm-control-line .form-group{ flex:1 1 auto; min-width:0; margin:0; }
    .norm-control-line .selectize-control{ margin:0; }
    .norm-control-line .selectize-input,
    .norm-control-line .form-control{
      min-height:31px;
      padding:5px 8px;
      font-size:11px;
    }
    .norm-control-line .btn{
      min-height:31px;
      padding:5px 9px;
      white-space:nowrap;
      font-size:11px;
    }
    .norm-method-desc{
      margin-top:4px;
      color:#6a7c8d;
      font-size:10px;
      line-height:1.3;
    }
    .norm-angle .form-group{ margin:0; }
    .norm-angle input{ min-height:31px; padding:5px 7px; font-size:11px; }
    @media (max-width:1050px){
      .norm-settings-grid{ grid-template-columns:repeat(2,minmax(220px,1fr)); }
      .norm-setting-group:nth-child(2){ border-right:0; }
    }
    @media (max-width:650px){
      .norm-settings-grid{ grid-template-columns:1fr; }
      .norm-setting-group{ padding:4px; border-right:0; border-bottom:1px solid #e1e7ec; }
      .norm-setting-group:last-child{ border-bottom:0; }
    }
  "
  
  shiny::tagList(
    # Inject scoped CSS once per module instance
    shiny::tags$style(shiny::HTML(css)),

    # Settings and QC output live directly in the tab-level panel.

        shiny::tags$div(
          id = method_area_id,
          class = "norm-settings-grid",

          shiny::div(
            class = "norm-setting-group",
            shiny::tags$label("Normalization"),
            shiny::div(
              class = "norm-control-line",
              shiny::selectInput(
                ns("method"), NULL,
                c("none", "sum", "median"),
                selected = "none"
              ),
              shiny::actionButton(
                ns("apply"), "Apply",
                class = "btn-primary btn-sm"
              )
            ),
            shiny::uiOutput(ns("method_desc")),
            shiny::conditionalPanel(
              sprintf("input['%s'] == 'log2'", ns("method")),
              shiny::numericInput(
                ns("offset"), "log2 offset",
                value = 1e-9, min = 0, step = 1e-9,
                width = "130px"
              )
            )
          ),

          shiny::div(
            class = "norm-setting-group norm-angle",
            shiny::tags$label("X label angle"),
            shiny::numericInput(
              ns("x_angle"), NULL,
              value = 45, min = 0, max = 90, step = 5,
              width = "100%"
            )
          ),

          shiny::div(
            class = "norm-setting-group",
            shiny::tags$label("Plot export"),
            shiny::div(
              class = "norm-control-line",
              shiny::selectInput(
                ns("plot_format"), NULL,
                choices = c("PNG" = "png", "PDF" = "pdf"),
                selected = "png"
              ),
              shiny::downloadButton(
                ns("download_qc"), "Plot",
                icon = shiny::icon("download"),
                class = "btn-default btn-sm"
              )
            )
          ),

          shiny::div(
            id = dl_id,
            class = "norm-setting-group",
            shiny::tags$label("Data export"),
            shiny::div(
              class = "norm-control-line",
              shiny::selectInput(
                ns("dl_format"), NULL,
                choices = c(
                  "CSV matrix" = "matrix_csv",
                  "TSV matrix" = "matrix_tsv",
                  "mzTab-M" = "mztab_m",
                  "SE (RDS)" = "se_rds"
                ),
                selected = "matrix_csv"
              ),
              shiny::downloadButton(
                ns("download_norm"), "Data",
                icon = shiny::icon("download"),
                class = "btn-default btn-sm"
              )
            )
          )
        ),
        
        shiny::plotOutput(ns("qc_boxdot"), height = "600px"),
        
        # Simple note: "half of the minimum positive value" (+ lower bound)
        shiny::tags$div(
          style = "margin-top:8px; color:#667789; font-size:11px;",
          shiny::HTML(
            "Note: For log10 visualization, an offset <code>pseudo</code> is added as
             <code>pseudo = max(0.5 � (minimum positive value), 1e-9)</code>, then we plot <code>log10(value + pseudo)</code>."
          )
        )
  )
}

#' @export
mod_normalize_server <- function(id, se_in) {
  shiny::moduleServer(id, function(input, output, session) {
    
    input_version <- shiny::reactiveVal(0L)
    applied_input_version <- shiny::reactiveVal(NA_integer_)
    
    shiny::observeEvent(se_in(), {
      input_version(shiny::isolate(input_version()) + 1L)
      applied_input_version(NA_integer_)
    }, ignoreInit = FALSE)
    
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
          '<div class="norm-method-desc">%s</div>',
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
        applied_input_version(input_version())
        shiny::showNotification("Normalization applied: none (raw)", type = "message", duration = 2)
        return(se)
      }
      
      ctl <- switch(input$method,
                    "log2" = list(offset = input$offset),
                    list()
      )
      
      se2 <- normalize_se(se, method = input$method, control = ctl)
      applied_input_version(input_version())
      
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
                      mztab_m    = "mztab",
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
        } else if (identical(input$dl_format, "mztab_m")) {
          write_mztab_m(
            se,
            file,
            normalized_by = input$method %||% "none"
          )
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
      ready = shiny::reactive(!is.null(se_norm()) && identical(applied_input_version(), input_version()))
    )
  })
}

