# R/mod_oplsda_generic.R -----------------------------------------------
# Generic OPLS-DA panel (Lipid / Transcriptome)
# - Uses utils in R/oplsda_ropls_utils.R
# - Event-driven fitting (Run button)
# - UI: remove MODEL params; keep PLOT params
# - Heatmap downloads: figure output only (heatmap PDF only)
#
# Legend-safe draw aligned to PCA tab:
#   - draw(..., heatmap_legend_side="bottom", annotation_legend_side="bottom",
#           merge_legends=TRUE, padding=...)
#
# NOTE:
#   - This file assumes `run_oplsda_objects_from_se()` is available
#     (from R/oplsda_ropls_utils.R) and now supports score_point_size.

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
      .opls-download-row { display:flex; gap:8px; margin-top:8px; }
      .opls-download-row .btn { flex:1 1 0; }
      .opls-score-controls { align-items:center; }
      .opls-score-controls .form-group { flex:0 0 auto; margin:0; }
      .opls-score-controls .checkbox { margin:0; }
      .opls-score-controls .btn {
        width:auto !important;
        height:34px;
        min-height:34px;
        padding:6px 12px;
      }
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

          tags$label("Plots"),
          numericInput(ns("point_size"), "Score point size", value = 5, min = 0.5, max = 10, step = 0.5),
          numericInput(ns("vip_thres"),  "VIP threshold",    value = 1, min = 0, step = 0.1),
          fluidRow(
            column(6, numericInput(ns("topn_vip"),  "Top VIP",     value = 20, min = 5, step = 1)),
            column(6, numericInput(ns("topn_heat"), "Heatmap top", value = 25, min = 5, step = 1))
          ),
          numericInput(ns("z_lim"), "Heatmap Z-limit", value = 2, min = 0.5, step = 0.5),

          hr(),

          actionButton(ns("run_opls"), "Run",
                       class = "btn-primary mslm-run-btn", width = "100%"),
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
          div(class = "opls-download-row opls-score-controls",
              checkboxInput(ns("show_scores_legend"), "Show legend", value = TRUE),
              downloadButton(ns("download_scores_pdf"), "Download PDF",
                             class = "btn-default"))
        )
      ),

      column(
        width = 5,
        shinydashboard::box(
          title = "VIP plot",
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
                             class = "btn-default"),
              downloadButton(ns("download_vip_csv"), "Download VIP CSV",
                             class = "btn-default"))
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

    MODEL_DEFAULT <- list(
      predI     = 1L,
      orthoI    = NA_integer_,   # auto
      scaleC    = "standard",
      crossvalI = 7L,
      permI     = 50L,
      seed      = 123L
    )

    model_failure_message <- paste(
      "An OPLS-DA model could not be built for the selected groups.",
      "This can occur when the sample size is small or the groups cannot be separated reliably."
    )

    .draw_model_failure <- function() {
      grid::grid.newpage()
      grid::grid.text(
        paste0(
          "OPLS-DA model was not created.\n\n",
          "The selected groups could not be separated reliably.\n",
          "Try adding more samples, reviewing the selected groups,\n",
          "or checking data quality and normalization."
        ),
        x = 0.5,
        y = 0.55,
        gp = grid::gpar(col = "#6b7280", fontsize = 13, lineheight = 1.35)
      )
      invisible(NULL)
    }


.draw_heatmap_safe <- function(ht) {
  if (is.null(ht)) {
    .draw_model_failure()
    return(invisible(NULL))
  }
  ComplexHeatmap::draw(
    ht,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom",
    merge_legends = TRUE,
    padding = grid::unit(c(3, 3, 3, 3), "mm")
  )
}

.decorate_vip_values <- function(hm) {
  if (is.null(hm$vip_min) || is.null(hm$vip_max)) return(invisible(NULL))
  try({
    ComplexHeatmap::decorate_annotation("VIP", {
      .vip_label <- function(label, y) {
        grid::grid.roundrect(
          x = grid::unit(0.5, "npc"), y = grid::unit(y, "npc"),
          width = grid::unit(8.5, "mm"), height = grid::unit(3.5, "mm"),
          r = grid::unit(0.6, "mm"),
          gp = grid::gpar(fill = grDevices::adjustcolor("white", alpha.f = 0.82), col = NA)
        )
        grid::grid.text(
          label,
          x = grid::unit(0.5, "npc"), y = grid::unit(y, "npc"),
          gp = grid::gpar(fontsize = 8, fontface = "bold", col = "#243342")
        )
      }
      .vip_label(hm$vip_max, 0.96)
      .vip_label(hm$vip_min, 0.04)
    })
  }, silent = TRUE)
  invisible(NULL)
}

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
          labs   <- c(labs,   "Other omics")
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
          options = list(maxItems = 2, placeholder = "Select exactly 2 groups")
        )
      }
    })

    .selected_group_info <- reactive({
      se <- current_se()
      kg <- input$keep_groups
      if (is.null(se) || !methods::is(se, "SummarizedExperiment") || length(kg) != 2) return(NULL)

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!("class" %in% colnames(cd))) return(NULL)

      counts <- table(factor(as.character(cd$class), levels = kg))
      min_n <- min(as.integer(counts))
      cv <- min(as.integer(MODEL_DEFAULT$crossvalI), min_n)

      # For two groups, do not request more permutations than distinct
      # rearrangements of the observed group labels (excluding the original).
      log_n_arrangements <- lchoose(sum(counts), counts[[1]])
      perm <- if (is.finite(log_n_arrangements) && log_n_arrangements < log(MODEL_DEFAULT$permI + 2)) {
        max(0L, as.integer(round(exp(log_n_arrangements))) - 1L)
      } else {
        as.integer(MODEL_DEFAULT$permI)
      }

      list(counts = counts, min_n = min_n, crossvalI = cv, permI = perm)
    })

    output$run_msg <- renderUI({
      kg <- input$keep_groups
      if (is.null(kg) || length(kg) != 2) {
        tags$div(style="color:#b30000;", "Select exactly 2 groups.")
      } else {
        info <- .selected_group_info()
        if (is.null(info)) return(tags$div(style="color:#b30000;", "Sample counts are unavailable."))

        count_text <- paste0(names(info$counts), ": n=", as.integer(info$counts), collapse = " / ")
        if (info$min_n < 2) {
          tags$div(style="color:#b30000;", count_text, tags$br(), "At least 2 samples per group are required.")
        } else if (info$min_n <= 4) {
          tags$div(style="color:#b30000;", count_text, tags$br(),
                   paste0("Very small sample size: a significant OPLS-DA model may not be built. CV=", info$crossvalI,
                          ", permutations=", info$permI, "."))
        } else if (info$min_n <= 6) {
          tags$div(style="color:#b36b00;", count_text, tags$br(),
                   paste0("Small-sample analysis: results may be unstable. CV=", info$crossvalI,
                          ", permutations=", info$permI, "."))
        } else {
          tags$div(style="color:#666;", count_text, tags$br(),
                   paste0("CV=", info$crossvalI, ", permutations=", info$permI, "."))
        }
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
      if (is.null(keep_groups) || length(keep_groups) != 2) {
        showNotification("Select exactly 2 groups.", type = "error", duration = 8)
        return(NULL)
      }

      group_info <- .selected_group_info()
      if (is.null(group_info) || group_info$min_n < 2) {
        showNotification("OPLS-DA requires at least 2 samples in each group.", type = "error", duration = 10)
        return(NULL)
      }
      if (group_info$min_n <= 6) {
        showNotification(
          "Only 2-6 samples are available in the smallest group. A significant model may not be built, and the model and VIP ranking may be unstable.",
          type = "warning",
          duration = 12
        )
      }

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

            seed        = MODEL_DEFAULT$seed,
            predI       = MODEL_DEFAULT$predI,
            orthoI      = MODEL_DEFAULT$orthoI,
            scaleC      = MODEL_DEFAULT$scaleC,
            crossvalI   = group_info$crossvalI,
            permI       = group_info$permI,

            vip_thres   = vip_thres,
            topn_vip    = topn_vip,
            topn_heat   = topn_heat,
            z_lim       = z_lim,

            score_point_size = pt,     # ★正式に渡す（serverでgeom追加しない）
            base_family = "sans",
            use_prism   = FALSE,
            verbose     = TRUE,
            colors      = cols
          )
        }, error = function(e) e)

        incProgress(0.8)

        if (inherits(res, "error")) {
          message("OPLS-DA model build failed: ", conditionMessage(res))
          showNotification(model_failure_message, type = "warning", duration = 10)
          return(list(
            model_failed = TRUE,
            error_detail = conditionMessage(res)
          ))
        }

        incProgress(0.1)
        res
      })

    }, ignoreInit = TRUE)

    observeEvent(fit_res(), {
      res <- fit_res()
      req(res)
      if (isTRUE(res$model_failed)) return()
      updateCheckboxInput(
        session,
        "show_scores_legend",
        value = !.is_dense_categorical_legend(res$colors)
      )
    }, ignoreInit = TRUE)

    output$opls_scores <- renderPlot({
      res <- fit_res()
      req(res)
      if (isTRUE(res$model_failed) || is.null(res$plots) || is.null(res$plots$score)) {
        .draw_model_failure()
        return(invisible(NULL))
      }
      legend_position <- if (isTRUE(input$show_scores_legend)) "bottom" else "none"
      print(res$plots$score + ggplot2::theme(legend.position = legend_position))
    })

    output$opls_vip <- renderPlot({
      res <- fit_res()
      req(res)
      if (isTRUE(res$model_failed) || is.null(res$plots) || is.null(res$plots$vip)) {
        .draw_model_failure()
        return(invisible(NULL))
      }
      print(res$plots$vip)
    })

    output$opls_heatmap <- renderPlot({
      res <- fit_res()
      req(res)
      if (isTRUE(res$model_failed) || is.null(res$heatmap) || is.null(res$heatmap$ht)) {
        .draw_model_failure()
        return(invisible(NULL))
      }

      .draw_heatmap_safe(res$heatmap$ht)
      .decorate_vip_values(res$heatmap)
    })

    output$download_scores_pdf <- downloadHandler(
      filename = function() {
        kg <- input$keep_groups %||% c("A", "B")
        stem <- paste0("oplsda_scores_", current_dataset_label(), "_", paste(kg, collapse = "_"))
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res(); req(res); req(!isTRUE(res$model_failed)); req(!is.null(res$plots$score))
        .export_ggplot_with_adaptive_legend(
          file = file,
          plot = res$plots$score,
          colors = res$colors,
          width = 6.5,
          height = 6.5,
          key_title = "OPLS-DA class color key"
        )
      }
    )

    output$download_vip_pdf <- downloadHandler(
      filename = function() {
        kg <- input$keep_groups %||% c("A", "B")
        stem <- paste0("oplsda_vip_", current_dataset_label(), "_", paste(kg, collapse = "_"))
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res(); req(res); req(!isTRUE(res$model_failed)); req(!is.null(res$plots$vip))
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
        res <- fit_res(); req(res); req(!isTRUE(res$model_failed)); req(!is.null(res$vip_tables$vip_tbl_ranked))
        out <- as.data.frame(res$vip_tables$vip_tbl_ranked, stringsAsFactors = FALSE)
        rd <- SummarizedExperiment::rowData(res$data$se)
        feature_match <- match(as.character(out$variable), rownames(res$data$se))

        metabolite_name <- as.character(out$variable)
        name_candidates <- c("Metabolite name", "Metabolite.name", "Name", "name")
        name_col <- name_candidates[name_candidates %in% colnames(rd)][1]
        if (length(name_col)) {
          metabolite_name <- as.character(rd[[name_col]][feature_match])
        }

        alignment_id <- rep(NA_character_, nrow(out))
        if ("Alignment ID" %in% colnames(rd)) {
          alignment_id <- as.character(rd[["Alignment ID"]][feature_match])
        }

        out <- data.frame(
          `Alignment ID` = alignment_id,
          `Metabolite name` = metabolite_name,
          out[, setdiff(colnames(out), "variable"), drop = FALSE],
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        utils::write.csv(out, file, row.names = FALSE, na = "")
      }
    )

    output$download_heatmap_pdf <- downloadHandler(
      filename = function() {
        kg <- input$keep_groups %||% c("A", "B")
        stem <- paste0("oplsda_heatmap_", current_dataset_label(), "_", paste(kg, collapse = "_"))
        paste0(.safe_file_stem(stem), ".pdf")
      },
      content = function(file) {
        res <- fit_res(); req(res); req(!isTRUE(res$model_failed)); req(!is.null(res$heatmap$ht))
        grDevices::pdf(file, width = 10, height = 8, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        .draw_heatmap_safe(res$heatmap$ht)
        .decorate_vip_values(res$heatmap)
      }
    )

  })
}
