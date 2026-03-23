# R/mod_decoupled_chains.R
# ============================================================
# Decoupled chain scatter panels (Shiny module)
#  - run find_decoupled_chains()
#  - show subclass × chains panels (patchwork)
#  - export multi-panel PDF
#  - chains are selectable (multi-select)
#  - NEW: Compare to switch:
#        total: x=C (class total) vs y=S (subset total)
#        rest : x=R (class total minus subset) vs y=S
#
# Requirements (in your package / app environment):
#  - find_decoupled_chains()
#  - plot_chain_vs_total_panel()
#    (Both can be the "rest-enabled" versions; wrappers below also work if old versions)
# ============================================================

mod_decoupled_chains_ui <- function(id, title = "Decoupled chains") {
  ns <- shiny::NS(id)

  return(
    shinydashboard::tabBox(
      width = 12,
      shiny::tabPanel(
        "Heatmap",
        shiny::br(),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Heatmap settings", width = 4, status = "primary", solidHeader = TRUE,
            shiny::uiOutput(ns("ui_heatmap_chains")),
            shiny::selectInput(
              ns("heatmap_corr_method"),
              "Correlation method",
              choices = c("pearson", "spearman"),
              selected = "pearson"
            ),
            shiny::helpText("The heatmap always uses class-total correlation and colors tiles by 1 - cor.")
          ),
          shinydashboard::box(
            title = title, width = 8, status = "primary", solidHeader = TRUE,
            shiny::plotOutput(ns("cor_heatmap"), height = "760px")
          )
        )
      ),
      shiny::tabPanel(
        "Panels",
        shiny::br(),
        shiny::fluidRow(
          shinydashboard::box(
            title = title, width = 4, status = "primary", solidHeader = TRUE,
            shiny::uiOutput(ns("ui_subclass")),
            shiny::uiOutput(ns("ui_chains")),
            shiny::tags$hr(),
            shiny::selectInput(
              ns("corr_method"),
              "cor method",
              choices = c("pearson", "spearman"),
              selected = "pearson"
            ),
            shiny::selectInput(
              ns("compare_to"),
              "Compare to (x-axis)",
              choices = c(
                "Total (C): subset vs class total" = "total",
                "Rest  (R): subset vs (class total minus subset)" = "rest"
              ),
              selected = "total"
            ),
            shiny::helpText("Total: x=C, y=S. Rest: x=R=T-S, y=S."),
            shiny::numericInput(ns("point_size"), "Point size", value = 3, min = 0.5, step = 0.5),
            shiny::numericInput(ns("point_alpha"), "Point alpha", min = 0.1, max = 1.0, value = 0.8, step = 0.05),
            shiny::tags$hr(),
            shiny::checkboxInput(ns("drop_outliers"), "Drop outliers (per panel)", value = FALSE),
            shiny::selectInput(
              ns("outlier_method"),
              "Outlier method",
              choices = c("mad_resid", "iqr_resid", "cook"),
              selected = "mad_resid"
            ),
            shiny::numericInput(ns("outlier_k"), "Outlier k", value = 4, min = 1, step = 0.5),
            shiny::helpText("mad_resid/iqr_resid: |residual| > k*MAD(IQR). cook: Cook's D > k/n."),
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
            title = "Scatter panels", width = 8, status = "primary", solidHeader = TRUE,
            shiny::helpText("Selected subclass-chain panels are shown below."),
            shiny::plotOutput(ns("all_panels_plot"), height = "860px")
          )
        )
      )
    )
  )

  shiny::fluidRow(
    shinydashboard::box(
      title = title, width = 4, status = "primary", solidHeader = TRUE,

      shiny::uiOutput(ns("ui_subclass")),
      shiny::uiOutput(ns("ui_chains")),

      shiny::tags$hr(),

      shiny::selectInput(ns("corr_method"), "cor method",
                         choices = c("pearson","spearman"), selected = "pearson"),

      # --- NEW: switch x-axis target (Total vs Rest) ---
      shiny::selectInput(
        ns("compare_to"),
        "Compare to (x-axis)",
        choices = c(
          "Total (C): subset vs class total" = "total",
          "Rest  (R): subset vs (class total minus subset)" = "rest"
        ),
        selected = "total"
      ),
      shiny::helpText("Total: x=C, y=S.  Rest: x=R=T−S, y=S (reduces coupling)."),

      shiny::numericInput(ns("point_size"), "Point size", value = 3, min = 0.5, step = 0.5),
      shiny::numericInput(ns("point_alpha"), "Point alpha", min = 0.1, max = 1.0, value = 0.8, step = 0.05),

      shiny::tags$hr(),

      shiny::checkboxInput(ns("drop_outliers"), "Drop outliers (per panel)", value = FALSE),
      shiny::selectInput(
        ns("outlier_method"),
        "Outlier method",
        choices = c("mad_resid", "iqr_resid", "cook"),
        selected = "mad_resid"
      ),
      shiny::numericInput(ns("outlier_k"), "Outlier k", value = 4, min = 1, step = 0.5),
      shiny::helpText("mad_resid/iqr_resid: |residual| > k×MAD(IQR).  cook: Cook's D > k/n."),

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

    shiny::column(
      width = 8,
      shinydashboard::tabBox(
        width = 12,
        shiny::tabPanel(
          "Heatmap",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 7,
              shiny::uiOutput(ns("ui_heatmap_chains"))
            ),
            shiny::column(
              width = 3,
              shiny::selectInput(
                ns("heatmap_corr_method"),
                "Correlation method",
                choices = c("pearson", "spearman"),
                selected = "pearson"
              )
            )
          ),
          shiny::helpText("Heatmap uses class-total correlation and colors tiles by 1 - cor."),
          shiny::plotOutput(ns("cor_heatmap"), height = "760px")
        ),
        shiny::tabPanel(
          "Panels",
          shiny::br(),
          shiny::helpText("Selected subclass × chains panels are shown below."),
          shiny::plotOutput(ns("all_panels_plot"), height = "860px")
        )
      )
    )
  )
}


mod_decoupled_chains_server <- function(id, se_lipid, assay = "abundance", adv_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 0)) b else a

    # ----------------------------
    # adv helpers (palette snapshot)
    # ----------------------------
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

    .infer_group_col <- function(cd, fixed = "class") {
      if (fixed %in% colnames(cd)) return(fixed)
      if ("Class" %in% colnames(cd)) return("Class")
      NULL
    }

    # ----------------------------
    # basic utils
    # ----------------------------
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
      missing <- setdiff(chains, ranked)
      c(ranked, missing)
    }

    .sanitize_compare_to <- function(x) {
      v <- trimws(as.character(x %||% "total"))
      if (!v %in% c("total","rest")) v <- "total"
      v
    }

    # ----------------------------
    # SAFE CALLERS (argument-gated)
    #   - important fix: se/out are always passed as named args
    # ----------------------------
    .call_find_decoupled <- function(se, ...) {
      args <- c(list(se = se), list(...))
      fm <- names(formals(find_decoupled_chains))
      args <- args[names(args) %in% fm]
      do.call(find_decoupled_chains, args)
    }

    .call_plot_panel <- function(out, ...) {
      args <- c(list(out = out), list(...))
      fm <- names(formals(plot_chain_vs_total_panel))
      args <- args[names(args) %in% fm]
      do.call(plot_chain_vs_total_panel, args)
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
    # heatmap chain choices from the full SE
    # ============================================================
    output$ui_heatmap_chains <- shiny::renderUI({
      se <- se_lipid()
      shiny::validate(shiny::need(!is.null(se), "No SE loaded yet."))

      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      shiny::validate(
        shiny::need("acyl_chains" %in% names(rd), "rowData$acyl_chains is missing."),
        shiny::need(is.list(rd$acyl_chains), "rowData$acyl_chains must be a list-column.")
      )

      cand <- unique(trimws(as.character(unlist(rd$acyl_chains, use.names = FALSE))))
      cand <- cand[nzchar(cand)]
      cand <- sort(cand)

      shiny::validate(shiny::need(length(cand) > 0, "No chains found in rowData$acyl_chains."))

      default <- isolate(input$heatmap_chains %||% character(0))
      if (!length(default)) {
        default <- intersect(c("16:0", "18:1", "18:2", "20:4", "22:6"), cand)
      }
      if (!length(default)) default <- cand[seq_len(min(12, length(cand)))]

      shiny::selectizeInput(
        ns("heatmap_chains"),
        "Chains for heatmap",
        choices = cand,
        selected = default,
        multiple = TRUE,
        options = list(
          placeholder = "Select chains to include in the heatmap",
          plugins = list("remove_button"),
          persist = TRUE,
          maxItems = 500
        )
      )
    })

    # ============================================================
    # state (snapshot after Run)
    # ============================================================
    rv <- shiny::reactiveValues(
      out = NULL,
      subclass = NULL,
      chains_ranked = character(0),
      plot_opts = list(
        corr_method = "pearson",
        add_lm = TRUE,
        point_size = 2.2,
        point_alpha = 0.8,
        compare_to = "total",
        drop_outliers = FALSE,
        outlier_method = "mad_resid",
        outlier_k = 4
      ),
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

      compare_to <- .sanitize_compare_to(input$compare_to)

      out <- NULL
      tryCatch({
        out <- .call_find_decoupled(
          se = se,
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
          use_lm_r2 = isTRUE(input$use_lm_r2),
          # if supported by your find_decoupled_chains() version:
          score_against = compare_to
        )
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
      })
      if (is.null(out)) return()

      # palette snapshot (avoid auto-update)
      pal <- NULL
      try({
        cd <- as.data.frame(SummarizedExperiment::colData(se))
        gcol <- .infer_group_col(cd, fixed = "class")
        adv <- .get_adv()
        if (!is.null(gcol) && !is.null(adv)) {
          pal <- .palette_from_adv(cd[[gcol]], adv)
        }
      }, silent = TRUE)

      # outlier snapshot
      drop_out <- isTRUE(input$drop_outliers %||% FALSE)

      om <- as.character(input$outlier_method %||% "mad_resid")
      if (!om %in% c("none", "mad_resid", "iqr_resid", "cook")) om <- "mad_resid"

      ok <- suppressWarnings(as.numeric(input$outlier_k %||% 4))
      if (!is.finite(ok) || ok <= 0) ok <- 4

      # point snapshot
      ps <- suppressWarnings(as.numeric(input$point_size %||% 2.2))
      if (!is.finite(ps) || ps <= 0) ps <- 2.2

      pa <- suppressWarnings(as.numeric(input$point_alpha %||% 0.8))
      if (!is.finite(pa) || pa <= 0) pa <- 0.8
      if (pa > 1) pa <- 1

      rv$out <- out
      rv$subclass <- subclass
      rv$chains_ranked <- .rank_chains_by_subsetS(out, subclass, chains)
      rv$pal <- pal

      rv$plot_opts <- list(
        corr_method = as.character(input$corr_method),
        add_lm = TRUE,
        point_size = ps,
        point_alpha = pa,
        compare_to = compare_to,
        drop_outliers = drop_out,
        outlier_method = om,
        outlier_k = ok
      )
    }, ignoreInit = TRUE)

    # ============================================================
    # allow compare_to switching AFTER Run (no recompute)
    # ============================================================
    shiny::observeEvent(input$compare_to, {
      if (is.null(rv$out)) return()
      rv$plot_opts$compare_to <- .sanitize_compare_to(input$compare_to)
    }, ignoreInit = TRUE)

    # ============================================================
    # heatmap (subclass x chain)
    # ============================================================
    output$cor_heatmap <- shiny::renderPlot({
      se <- se_lipid()
      shiny::validate(shiny::need(!is.null(se), "No SE loaded yet."))

      hm_chains <- .parse_chains(input$heatmap_chains)
      shiny::validate(shiny::need(length(hm_chains) > 0, "Select one or more chains for the heatmap."))

      out <- NULL
      tryCatch({
        out <- .call_find_decoupled(
          se = se,
          chains = hm_chains,
          assay_name = assay,
          subclass_col = "subclass",
          acyl_col = "acyl_chains",
          exclude_sample_classes = c("QC", "Blank"),
          log1p = FALSE,
          pseudo = 1,
          min_species_total = 1L,
          min_species_chain = 1L,
          min_chain_fraction_median = 0.01,
          corr_method = as.character(input$heatmap_corr_method %||% "pearson"),
          use_lm_r2 = FALSE,
          score_against = "total"
        )
      }, error = function(e) {
        shiny::validate(shiny::need(FALSE, conditionMessage(e)))
      })

      shiny::validate(shiny::need(!is.null(out), "No heatmap data available."))
      res <- as.data.frame(out$results)
      shiny::validate(shiny::need(nrow(res) > 0, "No heatmap data available."))

      shiny::validate(shiny::need("cor_total" %in% names(res), "Correlation column cor_total is missing in the decoupled-chain results."))

      res$subclass <- trimws(as.character(res$subclass))
      res$chain    <- trimws(as.character(res$chain))
      res$cor_val  <- suppressWarnings(as.numeric(res[["cor_total"]]))
      res$fill_val <- ifelse(is.finite(res$cor_val), 1 - res$cor_val, NA_real_)

      ggplot2::ggplot(res, ggplot2::aes(x = .data$chain, y = .data$subclass, fill = .data$fill_val)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.3) +
        ggplot2::geom_text(
          ggplot2::aes(label = ifelse(is.finite(.data$cor_val), sprintf("%.2f", .data$cor_val), "NA")),
          size = 3
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#2166ac",
          mid = "#f7f7f7",
          high = "#b2182b",
          midpoint = 1,
          limits = c(0, 2),
          na.value = "#d9d9d9",
          name = "1 - cor"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          x = "Chain",
          y = "Subclass",
          title = "Decoupled chain heatmap (1 - cor_total)"
        )
    }, res = 120)

    # ============================================================
    # render ALL panels (patchwork ncol=2)
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

      pt_size  <- suppressWarnings(as.numeric(rv$plot_opts$point_size %||% 2.2))
      if (!is.finite(pt_size) || pt_size <= 0) pt_size <- 2.2

      pt_alpha <- suppressWarnings(as.numeric(rv$plot_opts$point_alpha %||% 0.8))
      if (!is.finite(pt_alpha) || pt_alpha <= 0) pt_alpha <- 0.8
      if (pt_alpha > 1) pt_alpha <- 1

      compare_to <- .sanitize_compare_to(rv$plot_opts$compare_to %||% "total")

      drop_out    <- isTRUE(rv$plot_opts$drop_outliers)
      out_method  <- rv$plot_opts$outlier_method %||% "mad_resid"
      out_k       <- suppressWarnings(as.numeric(rv$plot_opts$outlier_k %||% 4))
      if (!is.finite(out_k) || out_k <= 0) out_k <- 4

      pal <- rv$pal

      plots <- lapply(chains, function(ch) {
        p <- try(
          .call_plot_panel(
            out = out,
            subclass_value = sc,
            chain_value    = ch,
            add_lm         = add_lm,
            corr_method    = corr_method,
            compare_to     = compare_to,
            drop_outliers  = drop_out,
            outlier_method = out_method,
            outlier_k      = out_k,
            point_alpha    = pt_alpha,
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
      patch
    }, res = 120)

    # ============================================================
    # download PDF (same as screen)
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

        pt_size  <- suppressWarnings(as.numeric(rv$plot_opts$point_size %||% 2.2))
        if (!is.finite(pt_size) || pt_size <= 0) pt_size <- 2.2

        pt_alpha <- suppressWarnings(as.numeric(rv$plot_opts$point_alpha %||% 0.8))
        if (!is.finite(pt_alpha) || pt_alpha <= 0) pt_alpha <- 0.8
        if (pt_alpha > 1) pt_alpha <- 1

        compare_to <- .sanitize_compare_to(rv$plot_opts$compare_to %||% "total")

        drop_out    <- isTRUE(rv$plot_opts$drop_outliers)
        out_method  <- rv$plot_opts$outlier_method %||% "mad_resid"
        out_k       <- suppressWarnings(as.numeric(rv$plot_opts$outlier_k %||% 4))
        if (!is.finite(out_k) || out_k <= 0) out_k <- 4

        pal <- rv$pal

        plots <- lapply(chains, function(ch) {
          p <- try(
            .call_plot_panel(
              out = out,
              subclass_value = sc,
              chain_value    = ch,
              add_lm         = add_lm,
              corr_method    = corr_method,
              compare_to     = compare_to,
              drop_outliers  = drop_out,
              outlier_method = out_method,
              outlier_k      = out_k,
              point_alpha    = pt_alpha,
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
