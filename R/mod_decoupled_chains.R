# R/mod_decoupled_chains.R
# ============================================================
# Decoupled chain scatter panels (Shiny module)
#  - run find_decoupled_chains()
#  - show subclass × chains panels (patchwork)
#  - export multi-panel PDF
#  - chains are selectable (multi-select) instead of comma text
# ============================================================

mod_decoupled_chains_ui <- function(id, title = "Decoupled chains") {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shinydashboard::box(
      title = title, width = 4, status = "primary", solidHeader = TRUE,

      shiny::uiOutput(ns("ui_subclass")),
      shiny::uiOutput(ns("ui_chains")),   # <- NEW: selectable chains

      shiny::tags$hr(),

      shiny::selectInput(ns("corr_method"), "cor method",
                         choices = c("pearson","spearman"), selected = "pearson"),
      shiny::numericInput(ns("point_size"), "Point size", value = 3, min = 0.5, step = 0.5),
      shiny::numericInput(ns("point_alpha"), "Point alpha", min = 0.1, max = 1.0, value = 0.8, step = 0.05),
      shiny::checkboxInput(ns("use_lm_r2"), "use lm R2 (decouple=1-R2)", value = TRUE),
      shiny::checkboxInput(ns("log1p"), "log10(raw + pseudo)", value = FALSE),
      shiny::numericInput(ns("pseudo"), "pseudo", value = 1, min = 0, step = 0.1),
	  shiny::numericInput(ns("min_species_total"), "min_species_total", value = 1, min = 1, step = 1),
      shiny::numericInput(ns("min_species_chain"), "min_species_chain", value = 1, min = 1, step = 1),
      shiny::numericInput(ns("min_chain_frac_med"), "min_chain_fraction_median", value = 0.01, min = 0, step = 0.001),
      shiny::tags$hr(),

      shiny::actionButton(ns("run"), "Run", icon = shiny::icon("play")),
      shiny::downloadButton(ns("download_pdf"), "Export PDF")
    ),

    shinydashboard::box(
      title = "Panels",
      width = 8, status = "primary", solidHeader = TRUE,
      shiny::helpText("Selected subclass × chains panels are shown below."),
      shiny::plotOutput(ns("all_panels_plot"), height = "860px")
    )
  )
}

mod_decoupled_chains_server <- function(id, se_lipid, assay = "abundance", adv_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 0)) b else a

    # ============================================================
    # (A) PCAタブと同じ palette 取得ロジック（“色の取り方”）
    # ============================================================
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

    .apply_palette_snapshot <- function(p, pal) {
      if (is.null(p) || !inherits(p, "ggplot")) return(p)
      if (is.null(pal) || is.null(pal$colors) || is.null(pal$breaks)) return(p)

      p +
        ggplot2::scale_color_manual(values = pal$colors, breaks = pal$breaks, drop = FALSE) +
        ggplot2::scale_fill_manual(values  = pal$colors, breaks = pal$breaks, drop = FALSE)
    }

    # group_col 固定は PCA 同様 "class" を優先（なければ "Class" にフォールバック）
    .infer_group_col <- function(cd, fixed = "class") {
      if (fixed %in% colnames(cd)) return(fixed)
      if ("Class" %in% colnames(cd)) return("Class")
      NULL
    }

    # ============================================================
    # subclass choices from SE
    # ============================================================
    output$ui_subclass <- shiny::renderUI({
      se <- se_lipid()
      shiny::validate(shiny::need(!is.null(se), "No SE loaded yet."))

      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      shiny::validate(shiny::need("subclass" %in% names(rd), "rowData$subclass is missing."))

      sc <- sort(unique(trimws(as.character(rd$subclass))))
      sc <- sc[nzchar(sc)]
      shiny::validate(shiny::need(length(sc) > 0, "No subclass values found."))

      shiny::selectInput(ns("subclass"), "Subclass", choices = sc, selected = sc[1])
    })

    # ============================================================
    # chain choices UI (multi-select)
    # ============================================================
    output$ui_chains <- shiny::renderUI({
      se <- se_lipid()
      shiny::validate(shiny::need(!is.null(se), "No SE loaded yet."))

      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      shiny::validate(
        shiny::need("acyl_chains" %in% names(rd), "rowData$acyl_chains is missing."),
        shiny::need(is.list(rd$acyl_chains), "rowData$acyl_chains must be a list-column.")
      )

      sc <- trimws(as.character(input$subclass %||% ""))
      idx <- rep(TRUE, nrow(rd))
      if (nzchar(sc) && "subclass" %in% names(rd)) {
        idx <- trimws(as.character(rd$subclass)) == sc
      }

      al <- rd$acyl_chains[idx]
      cand <- unique(trimws(as.character(unlist(al, use.names = FALSE))))
      cand <- cand[nzchar(cand)]
      cand <- sort(cand)

      shiny::validate(shiny::need(length(cand) > 0, "No chains found in this subclass."))

      default <- intersect(c("18:2","20:4","22:6"), cand)
      if (!length(default)) default <- cand[seq_len(min(3, length(cand)))]

      shiny::selectizeInput(
        ns("chains"),
        "Chains (multi-select)",
        choices  = cand,
        selected = default,
        multiple = TRUE,
        options  = list(
          placeholder = "Type to search chains…",
          plugins = list("remove_button"),
          persist = TRUE,
          maxItems = 200
        )
      )
    })

    # ============================================================
    # helpers
    # ============================================================
    .parse_chains <- function(x) {
      chains <- as.character(x %||% character(0))
      chains <- trimws(chains)
      chains <- chains[nzchar(chains)]
      unique(chains)
    }

    .placeholder_plot <- function(title, msg) {
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 4) +
        ggplot2::labs(title = title) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0))
    }

    .rank_chains_by_subsetS <- function(out, subclass, chains) {
      chains <- unique(trimws(chains))
      chains <- chains[nzchar(chains)]
      if (!length(chains)) return(character(0))

      dl <- as.data.frame(out$data_long)
      if (!nrow(dl)) return(chains)

      dl$subclass <- trimws(as.character(dl$subclass))
      dl$chain    <- trimws(as.character(dl$chain))
      dl$S        <- suppressWarnings(as.numeric(dl$S))

      dl2 <- dl[dl$subclass == subclass & dl$chain %in% chains, , drop = FALSE]
      if (!nrow(dl2)) return(chains)

      rank_tbl <- dl2 |>
        dplyr::group_by(.data$chain) |>
        dplyr::summarise(score = sum(.data$S, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$score))

      ranked  <- as.character(rank_tbl$chain)
      missing <- setdiff(chains, ranked)  # pair無しは末尾
      c(ranked, missing)
    }

    # ============================================================
    # state (snapshot after Run)
    # ============================================================
    rv <- shiny::reactiveValues(
      out = NULL,
      subclass = NULL,
      chains_ranked = character(0),
      plot_opts = list(corr_method = "pearson", add_lm = TRUE, point_size = 2.2),
      pal = NULL
    )

    # ============================================================
    # run computation on demand
    # ============================================================
    shiny::observeEvent(input$run, {
      se <- se_lipid()
      shiny::validate(shiny::need(!is.null(se), "No SE loaded yet."))

      subclass <- trimws(as.character(input$subclass %||% ""))
      shiny::validate(shiny::need(nzchar(subclass), "Pick subclass."))

      chains <- .parse_chains(input$chains)
      shiny::validate(shiny::need(length(chains) > 0, "chains is empty."))

      out <- NULL
      tryCatch({
        out <- find_decoupled_chains(
          se,
          chains = chains,
          assay_name = assay,
          subclass_col = "subclass",
          acyl_col = "acyl_chains",
          exclude_sample_classes = c("QC", "Blank"),
          log1p = isTRUE(input$log1p),
          pseudo = as.numeric(input$pseudo),
          min_species_total = as.integer(input$min_species_total),
          min_species_chain = as.integer(input$min_species_chain),
          min_chain_fraction_median = as.numeric(input$min_chain_frac_med),
          corr_method = as.character(input$corr_method),
          use_lm_r2 = isTRUE(input$use_lm_r2)
        )
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
      })
      if (is.null(out)) return()

      # --- snapshot palette exactly like PCA (but snapshot at Run to avoid auto-update)
      pal <- NULL
      try({
        cd <- as.data.frame(SummarizedExperiment::colData(se))
        gcol <- .infer_group_col(cd, fixed = "class")
        adv <- .get_adv()
        if (!is.null(gcol) && !is.null(adv)) {
          pal <- .palette_from_adv(cd[[gcol]], adv)
        }
      }, silent = TRUE)

      rv$out <- out
      rv$subclass <- subclass
      rv$chains_ranked <- .rank_chains_by_subsetS(out, subclass, chains)
      rv$plot_opts <- list(
        corr_method = as.character(input$corr_method),
        add_lm = TRUE,
        point_size = {
          ps <- as.numeric(input$point_size %||% 2.2)
          if (!is.finite(ps) || ps <= 0) 2.2 else ps
        }
      )
      rv$pal <- pal
    }, ignoreInit = TRUE)

    # ============================================================
    # render ALL panels (patchwork ncol=2)
    #  - no input refs => no update until Run
    # ============================================================
    output$all_panels_plot <- shiny::renderPlot({
      out <- rv$out
      shiny::validate(shiny::need(!is.null(out), "Press Run to compute."))

      sc <- rv$subclass %||% ""
      chains <- rv$chains_ranked
      shiny::validate(
        shiny::need(nzchar(sc), "Pick subclass."),
        shiny::need(length(chains) > 0, "chains is empty.")
      )

      if (!requireNamespace("patchwork", quietly = TRUE)) {
        return(.placeholder_plot(
          title = "patchwork is missing",
          msg = "Install 'patchwork' (CRAN): install.packages('patchwork')"
        ))
      }

      corr_method <- rv$plot_opts$corr_method %||% "pearson"
      add_lm      <- isTRUE(rv$plot_opts$add_lm)
      pt_size     <- as.numeric(rv$plot_opts$point_size %||% 2.2)
      pal         <- rv$pal

      plots <- lapply(chains, function(ch) {
        p <- try(
          plot_chain_vs_total_panel(
            out,
            subclass_value = sc,
            chain_value    = ch,
            add_lm         = add_lm,
            corr_method    = corr_method,
            drop_outliers  = FALSE,
            point_size     = pt_size
          ),
          silent = TRUE
        )

        if (inherits(p, "try-error")) {
          .placeholder_plot(
            title = paste0(sc, " | with ", ch),
            msg = "No data for this pair\n(or filtered out by thresholds)."
          )
        } else {
          p <- .apply_palette_snapshot(p, pal)
          p
        }
      })

      patch <- patchwork::wrap_plots(plots, ncol = 2, guides = "collect")
      patch <- patch & ggplot2::theme(legend.position = "bottom")
      patch
    }, res = 120)

    # ============================================================
    # download PDF (same as screen: ranked + palette)
    # ============================================================
    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0("decoupled_chain_panels_", Sys.Date(), ".pdf"),
      content = function(file) {
        out <- rv$out
        shiny::validate(shiny::need(!is.null(out), "Press Run first."))

        sc <- rv$subclass %||% ""
        chains <- rv$chains_ranked
        shiny::validate(shiny::need(nzchar(sc), "Pick subclass."))
        shiny::validate(shiny::need(length(chains) > 0, "chains is empty."))

        if (!requireNamespace("patchwork", quietly = TRUE)) {
          stop("patchwork is missing.", call. = FALSE)
        }

        corr_method <- rv$plot_opts$corr_method %||% "pearson"
        add_lm      <- isTRUE(rv$plot_opts$add_lm)
        pt_size     <- as.numeric(rv$plot_opts$point_size %||% 2.2)
        pal         <- rv$pal

        plots <- lapply(chains, function(ch) {
          p <- try(
            plot_chain_vs_total_panel(
              out,
              subclass_value = sc,
              chain_value    = ch,
              add_lm         = add_lm,
              corr_method    = corr_method,
              drop_outliers  = FALSE,
              point_size     = pt_size
            ),
            silent = TRUE
          )
          if (inherits(p, "try-error")) {
            .placeholder_plot(
              title = paste0(sc, " | with ", ch),
              msg = "No data for this pair\n(or filtered out by thresholds)."
            )
          } else {
            .apply_palette_snapshot(p, pal)
          }
        })

        patch <- patchwork::wrap_plots(plots, ncol = 2, guides = "collect")
        patch <- patch & ggplot2::theme(legend.position = "bottom")

        ggplot2::ggsave(file, patch, width = 11, height = 8.5, units = "in")
      }
    )

    invisible(NULL)
  })
}