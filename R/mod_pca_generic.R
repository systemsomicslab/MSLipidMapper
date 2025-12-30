# R/mod_pca_generic.R -----------------------------------------------
# Generic PCA panel (Lipid / Transcriptome)
# - Color grouping is FIXED to colData$class (NO UI for "Color by")
# - Score plot legend.position is forced to bottom
# - Add point size control (sidebar)
# - Add PDF download buttons under each plot
# - Loadings Top-N default = 10
# - Download button style = grey (btn-default)

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(methods)
  # UI uses shinydashboard::box with namespace, so library(shinydashboard) is optional
})

`%||%` <- function(a, b) if (is.null(a)) b else a

#--------------------------------------------------
# UI: Generic PCA (Lipid / Transcriptome)
#--------------------------------------------------

#' Generic PCA UI (dataset switch + scores + loadings)
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
          numericInput(ns("point_size"), label = NULL, min = 0.5, max = 8, value = 3, step = 0.5),
          
          hr(),
          
          tags$label("Loadings"),
          fluidRow(
            column(6, numericInput(ns("pc_loading"), "PC index", value = 1, min = 1, step = 1)),
            column(6, numericInput(ns("top_n_each"), "Top-N", value = 10, min = 1, step = 1))
          )
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
    
    .apply_shared_palette <- function(p, se, group_col_fixed = "class") {
      if (is.null(p) || !inherits(p, "ggplot")) return(p)
      if (is.null(se) || !methods::is(se, "SummarizedExperiment")) return(p)
      
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!group_col_fixed %in% colnames(cd)) return(p)
      
      adv <- .get_adv()
      if (is.null(adv)) return(p)
      
      pal <- .palette_from_adv(cd[[group_col_fixed]], adv)
      
      p +
        ggplot2::scale_color_manual(values = pal$colors, breaks = pal$breaks, drop = FALSE) +
        ggplot2::scale_fill_manual(values  = pal$colors, breaks = pal$breaks, drop = FALSE)
    }
    
    .safe_file_stem <- function(x) {
      x <- if (is.null(x) || !nzchar(x)) "pca" else x
      x <- gsub("[^A-Za-z0-9._-]+", "_", x)
      x <- gsub("_+", "_", x)
      x
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
    
    #---------------------------
    # Plots as reactives (for render + download)
    #---------------------------
    pca_scores_plot <- reactive({
      se <- current_se()
      req(se)
      
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      req("class" %in% colnames(cd))
      
      pc_x <- max(1, as.integer(input$pc_x %||% 1))
      pc_y <- max(1, as.integer(input$pc_y %||% 2))
      pt   <- as.numeric(input$point_size %||% 3)
      if (!is.finite(pt) || pt <= 0) pt <- 3
      
      p <- plot_pca_scores_from_se(
        se,
        assay_name      = assay,
        exclude_classes = c("Blank", "QC"),
        pc_x            = pc_x,
        pc_y            = pc_y
      )
      
      # Overlay points with desired size (safe override)
      p <- p + ggplot2::geom_point(aes(fill = class), size = pt, shape = 21, color = "black")
      
      p <- .apply_shared_palette(p, se, group_col_fixed = "class")
      p <- p + ggplot2::theme(legend.position = "bottom")
      
      p
    })
    
    pca_loadings_plot <- reactive({
      se <- current_se()
      req(se)
      
      pc_idx <- max(1, as.integer(input$pc_loading %||% 1))
      top_n  <- max(1, as.integer(input$top_n_each %||% 10))
      
      plot_pca_loading_from_se(
        se,
        pc_index        = pc_idx,
        assay_name      = assay,
        exclude_classes = c("Blank", "QC"),
        label_col       = NULL,
        top_n_each      = top_n,
        title_prefix    = NULL
      )
    })
    
    #---------------------------
    # Render plots
    #---------------------------
    output$pca_scores <- renderPlot({
      print(pca_scores_plot())
    })
    
    output$pca_loadings <- renderPlot({
      print(pca_loadings_plot())
    })
    
    #---------------------------
    # PDF downloads (grey buttons in UI)
    #---------------------------
    output$download_pca_scores_pdf <- downloadHandler(
      filename = function() {
        stem <- paste0("pca_scores_", current_dataset_label(), "_PC", input$pc_x %||% 1, "_PC", input$pc_y %||% 2)
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        p <- pca_scores_plot()
        grDevices::pdf(file, width = 6.5, height = 6.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(p)
      }
    )
    
    output$download_pca_loadings_pdf <- downloadHandler(
      filename = function() {
        stem <- paste0("pca_loadings_", current_dataset_label(), "_PC", input$pc_loading %||% 1)
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        p <- pca_loadings_plot()
        grDevices::pdf(file, width = 8.5, height = 6.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(p)
      }
    )
  })
}
