# Test-only Shiny app for chain-subset-vs-class specificity metrics.
# This file is intentionally standalone and not wired into the main app.

.tcs_app_find_function <- function(name) {
  if (exists(name, mode = "function", inherits = TRUE)) {
    return(get(name, mode = "function", inherits = TRUE))
  }

  pkg_ns <- tryCatch(asNamespace("MSLipidMapper"), error = function(e) NULL)
  if (!is.null(pkg_ns) && exists(name, envir = pkg_ns, mode = "function", inherits = FALSE)) {
    return(get(name, envir = pkg_ns, mode = "function", inherits = FALSE))
  }

  NULL
}

.tcs_app_source_core <- function() {
  core_fun <- .tcs_app_find_function("compute_test_class_specificity_metrics")
  write_fun <- .tcs_app_find_function("write_test_class_specificity_outputs")
  if (is.function(core_fun) && is.function(write_fun)) {
    return(invisible(TRUE))
  }

  path <- file.path(getwd(), "R", "test_class_specificity_metrics.R")
  if (!file.exists(path)) {
    stop("Could not locate R/test_class_specificity_metrics.R", call. = FALSE)
  }
  source(path, chdir = TRUE)
  invisible(TRUE)
}

.tcs_app_safe_classes <- function(se, class_col) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!class_col %in% names(rd)) {
    return(character(0))
  }
  vals <- sort(unique(as.character(rd[[class_col]])))
  vals[!is.na(vals) & nzchar(vals)]
}

run_test_class_specificity_app <- function() {
  .tcs_app_source_core()

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required.", call. = FALSE)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Package 'SummarizedExperiment' is required.", call. = FALSE)
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("Test-only chain subset specificity viewer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::fileInput("lipid_file", "Lipidomics CSV", accept = c(".csv")),
        shiny::fileInput("metadata_file", "Metadata CSV", accept = c(".csv")),
        shiny::textInput("metadata_sample_id_col", "metadata sample_id col", value = "sample_id"),
        shiny::textInput("metadata_class_col", "metadata class col", value = "class"),
        shiny::textInput("assay_name", "assay_name", value = "abundance"),
        shiny::textInput("class_col", "class_col", value = "subclass"),
        shiny::uiOutput("ui_target_class"),
        shiny::textInput("group_col", "group_col", value = ""),
        shiny::numericInput("pseudocount", "pseudocount", value = 1, min = 0, step = 0.1),
        shiny::checkboxInput("annotate_acyl", "annotate_acyl", value = FALSE),
        shiny::textInput("lipid_col", "lipid_col", value = "Metabolite.name"),
        shiny::textInput("rules_yaml", "rules_yaml", value = ""),
        shiny::actionButton("run_metrics", "Run", icon = shiny::icon("play")),
        shiny::tags$hr(),
        shiny::downloadButton("download_csv_bundle", "Export CSVs")
      ),
      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          shiny::tabPanel("Summary", DT::DTOutput("summary_tbl")),
          shiny::tabPanel("Metrics", DT::DTOutput("metric_tbl")),
          shiny::tabPanel("Plots",
            shiny::fluidRow(
              shiny::column(width = 6, shiny::plotOutput("plot_specificity", height = "420px")),
              shiny::column(width = 6, shiny::plotOutput("plot_logratio", height = "420px"))
            )
          ),
          shiny::tabPanel("Samplewise", DT::DTOutput("sample_tbl")),
          shiny::tabPanel("Subset members", DT::DTOutput("chain_tbl")),
          shiny::tabPanel("Run log", shiny::verbatimTextOutput("run_log"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    `%||%` <- function(a, b) {
      if (is.null(a) || (is.character(a) && !length(a))) b else a
    }

    rv <- shiny::reactiveValues(se = NULL, result = NULL)

    shiny::observeEvent(list(input$lipid_file, input$metadata_file), {
      req <- input$lipid_file
      if (is.null(req) || !nzchar(req$datapath)) {
        return()
      }

      metadata_path <- NULL
      if (!is.null(input$metadata_file) && nzchar(input$metadata_file$datapath %||% "")) {
        metadata_path <- input$metadata_file$datapath
      }

      se <- tryCatch(
        build_test_specificity_se_from_csv(
          lipid_csv = req$datapath,
          metadata_csv = metadata_path,
          metadata_sample_id_col = input$metadata_sample_id_col,
          metadata_class_col = input$metadata_class_col
        ),
        error = function(e) e
      )
      if (inherits(se, "error")) {
        shiny::showNotification(conditionMessage(se), type = "error", duration = 8)
        return()
      }
      rv$se <- se
    })

    output$ui_target_class <- shiny::renderUI({
      se <- rv$se
      shiny::validate(shiny::need(!is.null(se), "Upload lipidomics CSV and optional metadata CSV first."))
      cls <- .tcs_app_safe_classes(se, input$class_col %||% "LipidClass")
      shiny::selectInput(
        "target_class",
        "target_class",
        choices = c("All classes" = "", cls),
        selected = ""
      )
    })

    shiny::observeEvent(input$run_metrics, {
      se <- rv$se
      shiny::validate(shiny::need(!is.null(se), "Upload lipidomics CSV and optional metadata CSV first."))

      result <- tryCatch(
        compute_test_class_specificity_metrics(
          se = se,
          assay_name = input$assay_name,
          class_col = input$class_col,
          target_class = if (nzchar(input$target_class %||% "")) input$target_class else NULL,
          group_col = if (nzchar(input$group_col %||% "")) input$group_col else NULL,
          pseudocount = as.numeric(input$pseudocount),
          annotate_acyl = isTRUE(input$annotate_acyl),
          lipid_col = input$lipid_col,
          rules_yaml = if (nzchar(input$rules_yaml %||% "")) input$rules_yaml else NULL,
          verbose = TRUE
        ),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        shiny::showNotification(conditionMessage(result), type = "error", duration = 10)
        return()
      }
      rv$result <- result
    })

    output$summary_tbl <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
      DT::datatable(rv$result$summary_table, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$metric_tbl <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
      DT::datatable(rv$result$metric_table, options = list(pageLength = 20, scrollX = TRUE))
    })

    output$sample_tbl <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
      DT::datatable(rv$result$samplewise_long, options = list(pageLength = 20, scrollX = TRUE))
    })

    output$chain_tbl <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
      DT::datatable(rv$result$chain_membership, options = list(pageLength = 20, scrollX = TRUE))
    })

    output$run_log <- shiny::renderText({
      shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
      paste(rv$result$run_log, collapse = "\n")
    })

    output$plot_specificity <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
      df <- rv$result$metric_table
      shiny::validate(shiny::need(nrow(df) > 0, "No metric rows available."))

      ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$pearson_cor_leave1out, y = .data$specificity_score, color = .data$class)
      ) +
        ggplot2::geom_point(size = 2.6, alpha = 0.8) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          x = "Pearson cor with leave-one-out total",
          y = "Subset specificity score (1 - R^2)",
          color = "Class"
        )
    })

    output$plot_logratio <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
      df <- rv$result$metric_table
      shiny::validate(shiny::need(nrow(df) > 0, "No metric rows available."))

      ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$logratio_mean, y = .data$residual_mad, color = .data$class)
      ) +
        ggplot2::geom_point(size = 2.6, alpha = 0.8) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          x = "Subset mean logratio",
          y = "Residual MAD",
          color = "Class"
        )
    })

    output$download_csv_bundle <- shiny::downloadHandler(
      filename = function() {
        paste0("class_specificity_test_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        shiny::validate(shiny::need(!is.null(rv$result), "Run the test metrics first."))
        out_dir <- file.path(tempdir(), paste0("class_specificity_test_", Sys.getpid()))
        write_test_class_specificity_outputs(rv$result, out_dir)

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(out_dir)

        files <- c(
          "metric_table.csv",
          "samplewise_long.csv",
          "chain_membership.csv",
          "summary_table.csv",
          "run_log.txt"
        )
        utils::zip(zipfile = file, files = files)
      }
    )
  }

  shiny::runApp(shiny::shinyApp(ui = ui, server = server), launch.browser = interactive())
}
