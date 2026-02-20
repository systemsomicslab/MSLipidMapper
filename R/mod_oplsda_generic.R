# R/mod_oplsda_generic.R -----------------------------------------------
# Generic OPLS-DA panel (Lipid / Transcriptome)
# - Uses utils in R/oplsda_ropls_utils.R
# - Event-driven fitting (Run button)
# - UI: remove MODEL params; keep PLOT params
# - Heatmap downloads: figure output only (heatmap PDF only)

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(methods)
  library(grid)
  library(ComplexHeatmap)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

#--------------------------------------------------
# UI
#--------------------------------------------------
mod_oplsda_generic_ui <- function(id, title = "OPLS-DA") {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .opls-plot-square { position: relative; width: 100%; padding-bottom: 100%; }
      .opls-plot-square-inner { position: absolute; inset: 0; }
      .opls-download-row { margin-top: 8px; }
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

          uiOutput(ns("group_ui")),
          hr(),

          # ---- keep ONLY plot params (model params removed) ----
          tags$label("Plots"),
          numericInput(ns("point_size"), "Score point size", value = 5, min = 0.5, max = 10, step = 0.5),
          numericInput(ns("vip_thres"),  "VIP threshold",    value = 1, min = 0, step = 0.1),
          fluidRow(
            column(6, numericInput(ns("topn_vip"),  "Top VIP",     value = 20, min = 5, step = 1)),
            column(6, numericInput(ns("topn_heat"), "Heatmap top", value = 25, min = 5, step = 1))
          ),
          numericInput(ns("z_lim"), "Heatmap Z-limit", value = 2, min = 0.5, step = 0.5),

          hr(),

          actionButton(ns("run_opls"), "Run OPLS-DA", icon = icon("play"),
                       class = "btn-primary", width = "100%"),
          uiOutput(ns("run_msg"))
        )
      ),

      column(
        width = 4,
        shinydashboard::box(
          title = "OPLS-DA score plot",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          div(
            class = "opls-plot-square",
            div(class = "opls-plot-square-inner",
                plotOutput(ns("opls_scores"), height = "100%", width = "100%"))
          ),
          div(class = "opls-download-row",
              downloadButton(ns("download_scores_pdf"), "Download PDF",
                             class = "btn-default", width = "100%"))
        )
      ),

      column(
        width = 5,
        shinydashboard::box(
          title = "Signed VIP plot",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          div(
            class = "opls-plot-square",
            div(class = "opls-plot-square-inner",
                plotOutput(ns("opls_vip"), height = "100%", width = "100%"))
          ),
          div(class = "opls-download-row",
              downloadButton(ns("download_vip_pdf"), "Download PDF",
                             class = "btn-default", width = "100%")),
          div(class = "opls-download-row",
              downloadButton(ns("download_vip_csv"), "Download VIP CSV",
                             class = "btn-default", width = "100%"))
        )
      )
    ),

    fluidRow(
      column(
        width = 12,
        shinydashboard::box(
          title = "VIP heatmap (top variables)",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          plotOutput(ns("opls_heatmap"), height = "650px"),
          fluidRow(
            # ---- Heatmap area: FIGURE OUTPUT ONLY ----
            column(4, downloadButton(ns("download_heatmap_pdf"), "Download heatmap PDF",
                                     class = "btn-default", width = "100%"))
          )
        )
      )
    )
  )
}

#--------------------------------------------------
# server
#--------------------------------------------------
mod_oplsda_generic_server <- function(id,
                                      se_lipid,
                                      se_tx = NULL,
                                      assay = "abundance",
                                      adv_reactive = NULL) {
  moduleServer(id, function(input, output, session) {

    # ---- fixed MODEL params (user does NOT set these) ----
    MODEL_DEFAULT <- list(
      predI     = 1L,
      orthoI    = NA_integer_,   # auto
      scaleC    = "standard",
      crossvalI = 7L,
      permI     = 50L,
      seed      = 123L
    )

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

    .palette_from_adv <- function(levels_vec, adv) {
      lev <- as.character(levels_vec)
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
      cols
    }

    .safe_file_stem <- function(x) {
      x <- if (is.null(x) || !nzchar(x)) "oplsda" else x
      x <- gsub("[^A-Za-z0-9._-]+", "_", x)
      x <- gsub("_+", "_", x)
      x
    }

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
      ns <- session$ns
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
      } else NULL
    })

    current_dataset_label <- reactive({
      ds <- input$dataset %||% "lipid"
      if (identical(ds, "tx")) "transcriptome" else "lipid"
    })

    .available_groups <- reactive({
      se <- current_se()
      if (is.null(se) || !methods::is(se, "SummarizedExperiment")) return(NULL)
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!("class" %in% colnames(cd))) return(NULL)

      g <- as.character(cd$class)
      g <- g[!is.na(g) & nzchar(g)]
      g <- unique(g)
      g <- setdiff(g, c("QC", "Blank"))
      if (!length(g)) return(NULL)
      g
    })

    output$group_ui <- renderUI({
      ns <- session$ns
      g <- .available_groups()
      if (is.null(g)) {
        helpText("Group column 'class' not found (or no valid groups).")
      } else {
        sel <- head(g, 2)
        selectizeInput(
          ns("keep_groups"),
          "Groups to compare (class)",
          choices = g,
          selected = sel,
          multiple = TRUE,
          options = list(maxItems = 8, placeholder = "Select >=2 groups")
        )
      }
    })

    output$run_msg <- renderUI({
      kg <- input$keep_groups
      if (is.null(kg) || length(kg) < 2) {
        tags$div(style="color:#b30000;", "Select at least 2 groups.")
      } else {
        tags$div(style="color:#666;", paste0("Selected: ", paste(kg, collapse = ", ")))
      }
    })

    fit_res <- eventReactive(input$run_opls, {

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

      keep_groups <- input$keep_groups
      if (is.null(keep_groups) || length(keep_groups) < 2) {
        showNotification("Select at least 2 groups.", type = "error", duration = 8)
        return(NULL)
      }

      # ---- Plot params (user keeps these) ----
      vip_thres <- as.numeric(input$vip_thres %||% 1)
      if (!is.finite(vip_thres) || vip_thres < 0) vip_thres <- 1

      topn_vip  <- max(5L, as.integer(input$topn_vip %||% 20))
      topn_heat <- max(5L, as.integer(input$topn_heat %||% 25))

      z_lim <- as.numeric(input$z_lim %||% 2)
      if (!is.finite(z_lim) || z_lim <= 0) z_lim <- 2

      pt <- as.numeric(input$point_size %||% 5)
      if (!is.finite(pt) || pt <= 0) pt <- 5

      adv  <- .get_adv()
      cols <- .palette_from_adv(keep_groups, adv)

      withProgress(message = "Running OPLS-DA ...", value = 0, {
        incProgress(0.1)

        res <- tryCatch({
          run_oplsda_objects_from_se(
            se          = se,
            assay_name  = assay,
            group_col   = "class",
            keep_groups = keep_groups,

            # ---- fixed MODEL params ----
            seed        = MODEL_DEFAULT$seed,
            predI       = MODEL_DEFAULT$predI,
            orthoI      = MODEL_DEFAULT$orthoI,
            scaleC      = MODEL_DEFAULT$scaleC,
            crossvalI   = MODEL_DEFAULT$crossvalI,
            permI       = MODEL_DEFAULT$permI,

            # ---- plot-related params remain user-controlled ----
            vip_thres   = vip_thres,
            topn_vip    = topn_vip,
            topn_heat   = topn_heat,
            z_lim       = z_lim,

            base_family = "sans",
            use_prism   = FALSE,
            verbose     = TRUE,
            colors      = cols
          )
        }, error = function(e) e)

        incProgress(0.8)

        if (inherits(res, "error")) {
          showNotification(conditionMessage(res), type = "error", duration = 10)
          return(NULL)
        }

        # Apply point size (override)
        res$plots$score <- res$plots$score +
          ggplot2::geom_point(aes(fill = Group), size = pt, shape = 21,
                              color = "black", inherit.aes = TRUE)

        incProgress(0.1)
        res
      })

    }, ignoreInit = TRUE)

    output$opls_scores <- renderPlot({
      res <- fit_res()
      req(res)
      print(res$plots$score)
    })

    output$opls_vip <- renderPlot({
      res <- fit_res()
      req(res)
      print(res$plots$vip)
    })

    output$opls_heatmap <- renderPlot({
      res <- fit_res()
      req(res)
      grid::grid.newpage()
      ComplexHeatmap::draw(res$heatmap$ht)

      if (!is.null(res$heatmap$vip_min) && !is.null(res$heatmap$vip_max)) {
        try({
          ComplexHeatmap::decorate_annotation("VIP", {
            grid::grid.text(res$heatmap$vip_max,
                            x = grid::unit(-2, "mm"), y = grid::unit(0.98, "npc"),
                            just = "right", gp = grid::gpar(cex = 0.8))
            grid::grid.text(res$heatmap$vip_min,
                            x = grid::unit(-2, "mm"), y = grid::unit(0.02, "npc"),
                            just = "right", gp = grid::gpar(cex = 0.8))
          })
        }, silent = TRUE)
      }
    })

    # ---- downloads (score/vip keep as-is) ----
    output$download_scores_pdf <- downloadHandler(
      filename = function() {
        kg <- input$keep_groups %||% c("A", "B")
        stem <- paste0("oplsda_scores_", current_dataset_label(), "_", paste(kg, collapse = "_"))
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res()
        req(res)
        grDevices::pdf(file, width = 6.5, height = 6.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(res$plots$score)
      }
    )

    output$download_vip_pdf <- downloadHandler(
      filename = function() {
        kg <- input$keep_groups %||% c("A", "B")
        stem <- paste0("oplsda_vip_", current_dataset_label(), "_", paste(kg, collapse = "_"))
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res()
        req(res)
        grDevices::pdf(file, width = 8.5, height = 6.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(res$plots$vip)
      }
    )

    output$download_vip_csv <- downloadHandler(
      filename = function() {
        kg <- input$keep_groups %||% c("A", "B")
        stem <- paste0("oplsda_vip_", current_dataset_label(), "_", paste(kg, collapse = "_"))
        paste0(.safe_file_stem(stem), ".csv")
      },
      content = function(file) {
        res <- fit_res()
        req(res)
        utils::write.csv(res$vip_tables$vip_tbl_signed, file, row.names = FALSE)
      }
    )

    # ---- heatmap: figure output only ----
    output$download_heatmap_pdf <- downloadHandler(
      filename = function() {
        kg <- input$keep_groups %||% c("A", "B")
        stem <- paste0("oplsda_heatmap_", current_dataset_label(), "_", paste(kg, collapse = "_"))
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res()
        req(res)
        grDevices::pdf(file, width = 10, height = 8, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        ComplexHeatmap::draw(res$heatmap$ht)
      }
    )

  })
}
