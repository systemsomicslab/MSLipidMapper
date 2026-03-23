# R/mod_ccd_analysis.R --------------------------------------------------------
# Class x chain-subset divergence analysis module.

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(SummarizedExperiment)
  library(ggplot2)
  library(DT)
  library(dplyr)
})

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 0)) b else a

.parse_custom_subset_rules_text <- function(txt) {
  txt <- paste(txt %||% "", collapse = "\n")
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(NULL)

  out <- list()
  for (ln in lines) {
    parts <- strsplit(ln, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    nm <- trimws(parts[[1]])
    rx <- trimws(paste(parts[-1], collapse = "="))
    if (!nzchar(nm) || !nzchar(rx)) next
    out[[nm]] <- local({
      pattern <- rx
      function(chains) any(grepl(pattern, chains, perl = TRUE))
    })
  }
  if (length(out)) out else NULL
}

.ccd_profile_from_result_row <- function(se,
                                         result_row,
                                         assay_name,
                                         class_col,
                                         chain_col,
                                         custom_subset_rules = NULL) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se), check.names = FALSE)
  A <- as.matrix(SummarizedExperiment::assay(se, assay_name))

  cls <- as.character(result_row$class[[1]])
  subset_label <- as.character(result_row$subset[[1]])
  subset_mode <- as.character(result_row$subset_mode[[1]])

  subset_defs <- generate_chain_subsets(
    row_annot = rd,
    class_value = cls,
    subset_mode = subset_mode,
    chain_col = chain_col,
    class_col = class_col,
    custom_subset_rules = custom_subset_rules
  )
  member_local <- subset_defs[[subset_label]]
  if (is.null(member_local)) return(NULL)

  class_idx <- which(as.character(rd[[class_col]]) == cls)
  if (!length(class_idx)) return(NULL)

  subset_idx <- class_idx[as.logical(member_local)]
  if (!length(subset_idx)) return(NULL)

  class_profile <- colSums(A[class_idx, , drop = FALSE], na.rm = TRUE)
  subset_profile <- colSums(A[subset_idx, , drop = FALSE], na.rm = TRUE)

  tibble::tibble(
    sample = colnames(A),
    class_total = as.numeric(class_profile),
    subset_total = as.numeric(subset_profile)
  ) %>%
    tidyr::pivot_longer(
      cols = c("class_total", "subset_total"),
      names_to = "profile",
      values_to = "abundance"
    )
}

#' CCD analysis UI
#'
#' @param id Module ID.
#' @param title Module title.
#'
#' @return UI for CCD analysis.
#' @export
mod_ccd_analysis_ui <- function(id, title = "Class x chain-subset divergence") {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 3,
      shinydashboard::box(
        title = title,
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        shiny::uiOutput(ns("ui_assay")),
        shiny::uiOutput(ns("ui_class_col")),
        shiny::uiOutput(ns("ui_chain_col")),

        shiny::selectInput(
          ns("subset_mode"),
          "Subset mode",
          choices = c(
            "Exact chain" = "exact_chain",
            "Carbon only" = "carbon_only",
            "Custom" = "custom"
          ),
          selected = "exact_chain"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("subset_mode")),
          shiny::textAreaInput(
            ns("custom_rules"),
            "Custom rules (label = regex, one per line)",
            rows = 5,
            placeholder = "DHA-containing = 22:6\nPUFA-containing = :[4-9]"
          ),
          shiny::helpText("A subset matches when any chain in a lipid matches the regex.")
        ),

        shiny::numericInput(ns("min_subset_size"), "Minimum subset size", value = 3, min = 1, step = 1),
        shiny::selectInput(
          ns("cor_method"),
          "Correlation method",
          choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
          selected = "pearson"
        ),
        shiny::numericInput(ns("ccd_threshold"), "CCD threshold", value = 0.5, min = 0, max = 2, step = 0.05),
        shiny::numericInput(ns("scr_threshold"), "SCR threshold", value = 0.05, min = 0, max = 1, step = 0.01),
        shiny::numericInput(ns("top_classes"), "Top classes in heatmap", value = 20, min = 5, step = 1),
        shiny::numericInput(ns("top_subsets"), "Top subsets in heatmap", value = 20, min = 5, step = 1),
        shiny::checkboxInput(ns("verbose"), "Verbose", value = FALSE),
        shiny::actionButton(ns("run"), "Run", icon = shiny::icon("play"), class = "btn btn-primary")
      )
    ),

    shiny::column(
      width = 9,
      shinydashboard::tabBox(
        width = 12,
        shiny::tabPanel(
          "Results",
          shiny::br(),
          DT::DTOutput(ns("summary_tbl")),
          shiny::tags$hr(),
          DT::DTOutput(ns("main_tbl"))
        ),
        shiny::tabPanel(
          "Plots",
          shiny::br(),
          shiny::plotOutput(ns("ccd_heatmap"), height = "520px"),
          shiny::tags$hr(),
          shiny::plotOutput(ns("ccd_scatter"), height = "420px"),
          shiny::tags$hr(),
          shiny::plotOutput(ns("profile_plot"), height = "320px")
        )
      )
    )
  )
}

#' CCD analysis server
#'
#' @param id Module ID.
#' @param se_lipid Reactive lipid SummarizedExperiment.
#'
#' @return Invisibly, a list of CCD reactives.
#' @export
mod_ccd_analysis_server <- function(id, se_lipid) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    se_r <- if (shiny::is.reactive(se_lipid)) se_lipid else shiny::reactive(se_lipid)

    output$ui_assay <- shiny::renderUI({
      se <- se_r()
      shiny::req(se)
      shiny::selectInput(
        ns("assay_name"),
        "Assay",
        choices = SummarizedExperiment::assayNames(se),
        selected = .ccd_resolve_assay_name(se, NULL)
      )
    })

    output$ui_class_col <- shiny::renderUI({
      se <- se_r()
      shiny::req(se)
      rd_names <- colnames(SummarizedExperiment::rowData(se))
      sel <- .ccd_resolve_class_col(se, "LipidClass")
      shiny::selectInput(ns("class_col"), "Class column", choices = rd_names, selected = sel)
    })

    output$ui_chain_col <- shiny::renderUI({
      se <- se_r()
      shiny::req(se)
      rd_names <- colnames(SummarizedExperiment::rowData(se))
      sel <- .ccd_resolve_chain_col(se, "acyl_chains") %||% rd_names[[1]]
      shiny::selectInput(ns("chain_col"), "Chain column", choices = rd_names, selected = sel)
    })

    ccd_result <- shiny::eventReactive(input$run, {
      se <- se_r()
      shiny::validate(
        shiny::need(!is.null(se), "No lipid SummarizedExperiment is available."),
        shiny::need(nrow(se) > 0, "The lipid SummarizedExperiment has no rows.")
      )

      custom_rules <- NULL
      if (identical(input$subset_mode, "custom")) {
        custom_rules <- .parse_custom_subset_rules_text(input$custom_rules)
      }

      main_tbl <- compute_ccd_table(
        se = se,
        assay_name = input$assay_name,
        class_col = input$class_col,
        chain_col = input$chain_col,
        subset_mode = input$subset_mode,
        min_subset_size = input$min_subset_size,
        cor_method = input$cor_method,
        custom_subset_rules = custom_rules,
        verbose = isTRUE(input$verbose)
      )

      summary_tbl <- compute_ccd_class_summary(
        ccd_table = main_tbl,
        ccd_threshold = input$ccd_threshold,
        scr_threshold = input$scr_threshold
      )

      list(
        main = main_tbl,
        summary = summary_tbl,
        custom_rules = custom_rules
      )
    }, ignoreInit = TRUE)

    output$summary_tbl <- DT::renderDT({
      res <- ccd_result()
      shiny::req(res)

      DT::datatable(
        res$summary,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$main_tbl <- DT::renderDT({
      res <- ccd_result()
      shiny::req(res)

      DT::datatable(
        res$main,
        rownames = FALSE,
        filter = "top",
        selection = "single",
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$ccd_heatmap <- shiny::renderPlot({
      res <- ccd_result()
      shiny::req(res)
      df <- res$main
      shiny::validate(shiny::need(nrow(df) > 0, "No CCD results to plot."))

      top_classes <- max(1L, as.integer(input$top_classes %||% 20L))
      top_subsets <- max(1L, as.integer(input$top_subsets %||% 20L))
      .safe_max <- function(x) {
        x <- x[is.finite(x)]
        if (!length(x)) NA_real_ else max(x)
      }

      class_keep <- df %>%
        dplyr::group_by(.data$class) %>%
        dplyr::summarise(score = .safe_max(.data$CCD), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.data$score)) %>%
        dplyr::slice_head(n = top_classes) %>%
        dplyr::pull(.data$class)

      subset_keep <- df %>%
        dplyr::group_by(.data$subset) %>%
        dplyr::summarise(score = .safe_max(.data$CCD), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.data$score)) %>%
        dplyr::slice_head(n = top_subsets) %>%
        dplyr::pull(.data$subset)

      df2 <- df %>%
        dplyr::filter(.data$class %in% class_keep, .data$subset %in% subset_keep)

      ggplot2::ggplot(df2, ggplot2::aes(x = .data$subset, y = .data$class, fill = .data$CCD)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2) +
        ggplot2::scale_fill_gradientn(
          colours = c("#f7fbff", "#6baed6", "#08519c", "#67000d"),
          limits = c(0, 2)
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::labs(x = "Subset", y = "Class", fill = "CCD")
    })

    output$ccd_scatter <- shiny::renderPlot({
      res <- ccd_result()
      shiny::req(res)
      df <- res$main
      shiny::validate(shiny::need(nrow(df) > 0, "No CCD results to plot."))

      ggplot2::ggplot(df, ggplot2::aes(
        x = .data$SCR,
        y = .data$CCD,
        color = .data$direction,
        size = .data$n_species
      )) +
        ggplot2::geom_point(alpha = 0.8) +
        ggplot2::geom_hline(yintercept = as.numeric(input$ccd_threshold %||% 0.5), linetype = "dashed", color = "#555555") +
        ggplot2::geom_vline(xintercept = as.numeric(input$scr_threshold %||% 0.05), linetype = "dashed", color = "#555555") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(x = "SCR", y = "CCD", color = "Direction", size = "n_species")
    })

    output$profile_plot <- shiny::renderPlot({
      res <- ccd_result()
      shiny::req(res)
      df <- res$main
      shiny::validate(shiny::need(nrow(df) > 0, "No CCD results to plot."))

      sel <- input$main_tbl_rows_selected
      shiny::validate(shiny::need(length(sel) == 1, "Select one row in the main table to inspect the sample-wise profile."))

      prof <- .ccd_profile_from_result_row(
        se = se_r(),
        result_row = df[sel, , drop = FALSE],
        assay_name = input$assay_name,
        class_col = input$class_col,
        chain_col = input$chain_col,
        custom_subset_rules = res$custom_rules
      )
      shiny::validate(shiny::need(!is.null(prof) && nrow(prof) > 0, "Could not build class/subset profiles for the selected row."))

      ggplot2::ggplot(prof, ggplot2::aes(x = .data$sample, y = .data$abundance, color = .data$profile, group = .data$profile)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 2) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(x = "Sample", y = "Total abundance", color = "Profile")
    })

    invisible(list(
      ccd_table = shiny::reactive(ccd_result()$main),
      class_summary = shiny::reactive(ccd_result()$summary)
    ))
  })
}
