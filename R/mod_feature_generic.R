# R/mod_feature_generic.R
# ======================================================================
# Feature generic module (Global Advanced Settings hub)
#   - Owns shared "Advanced settings" state (adv) + palette table
#   - Provides adv_reactive() to other modules (PCA/Heatmap/Network/Correlation)
#   - Provides open_adv(dataset) to open the common modal from anywhere
#   - FIX: avoid freeze by guarding palette sync loop (picker <-> table)
#   - FIX: initialize palette_map early (before modal is opened) to keep colors consistent
# ======================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(SummarizedExperiment)
})

# Utilities ----------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

.idify <- function(x) gsub("[^A-Za-z0-9_]+", "_", as.character(x))

# scalar-safe hex normalizer (NEVER returns length>1)
.norm_hex1 <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  if (length(x) != 1L) x <- x[1]
  x <- trimws(x)
  if (!nzchar(x)) return(NA_character_)
  if (!startsWith(x, "#")) x <- paste0("#", x)
  if (!grepl("^#([A-Fa-f0-9]{6})$", x)) return(NA_character_)
  toupper(x)
}

# vectorized normalizer (preserves length)
.norm_hex_vec <- function(x) vapply(x, .norm_hex1, FUN.VALUE = character(1))

.default_palette <- function(n) {
  n <- as.integer(n %||% 0)
  if (!is.finite(n) || n <= 0) return(character(0))
  hues <- seq(15, 375, length.out = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

# Merge palette_map with current groups:
# - keep existing colors for common groups
# - fill missing with defaults
.merge_palette <- function(groups, palette_map) {
  groups <- as.character(groups %||% character(0))
  groups <- groups[nzchar(groups) & !is.na(groups)]
  groups <- unique(groups)

  if (!length(groups)) return(character(0))

  pal_old <- palette_map %||% character(0)
  if (length(pal_old) && is.null(names(pal_old))) names(pal_old) <- rep("", length(pal_old))
  pal_old <- pal_old[nzchar(names(pal_old))]

  def <- .default_palette(length(groups))
  names(def) <- groups

  out <- def
  if (length(pal_old)) {
    keep <- intersect(names(pal_old), groups)
    out[keep] <- pal_old[keep]
  }
  out <- .norm_hex_vec(out)
  names(out) <- groups
  out
}

# Default advanced settings -------------------------------------------------
.default_feature_adv <- function() {
  list(
    # --- X-axis ordering -----------------------------------------------------
    order_by     = "abundance_median",  # "abundance_median" / "none"
    decreasing   = FALSE,
    manual_order = character(0),        # manual x-axis order (group names)
    palette_map  = character(0),        # group -> "#RRGGBB"

    # --- Statistical annotation ---------------------------------------------
    plot_type    = "violin",  # "dot" / "box" / "violin"
    add_p        = FALSE,
    test         = "wilcox",  # "wilcox" / "t.test"
    p_label      = "both",    # "p" / "stars" / "both"
    comp_mode    = "ref",     # "all" / "ref" / "manual"
    ref_group    = "",
    manual_pairs = data.frame(
      group1 = NA_character_,
      group2 = NA_character_,
      stringsAsFactors = FALSE
    ),

    # --- Dot plot options ---------------------------------------------------
    dot_point_size   = 2.0,
    dot_jitter_width = 0.15,
    dot_alpha        = 0.9,
    dot_show_median  = TRUE,
    dot_median_size  = 0.7,
    dot_median_width = 0.6,
    dot_median_color = "#222222",

    # --- Box plot options ---------------------------------------------------
    box_width        = 0.6,
    box_alpha        = 0.6,
    box_show_points  = TRUE,
    box_point_size   = 1.6,
    box_jitter_width = 0.12,
    box_point_alpha  = 0.8,

    # --- Violin plot options ------------------------------------------------
    violin_width        = 0.8,
    violin_alpha        = 0.55,
    violin_trim         = TRUE,
    violin_show_points  = TRUE,
    violin_point_size   = 1.6,
    violin_jitter_width = 0.12,
    violin_point_alpha  = 0.8,
    violin_show_median  = TRUE,
    violin_median_size  = 0.7,
    violin_median_color = "#222222",

    # --- Heatmap options -----------------------------------------------------
    hm_row_z = TRUE,
    hm_topN  = 50L
  )
}

# ======================================================================
# Feature generic SERVER (Global settings hub)
# ======================================================================

#' Feature generic server
#'
#' @param id Module ID
#' @param se_lipid reactive or SummarizedExperiment
#' @param se_tx    reactive or SummarizedExperiment (can be NULL)
#' @param on_live_change function(list) | NULL
#' @param on_adv_commit  function(adv_list) | NULL
#'
#' @return list(adv_reactive, open_adv, apply_live)
#' @export
mod_feature_generic_server <- function(
    id,
    se_lipid,
    se_tx = NULL,
    on_live_change = NULL,
    on_adv_commit  = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    se_lipid_r <- if (shiny::is.reactive(se_lipid)) se_lipid else shiny::reactive(se_lipid)
    se_tx_r    <- if (!is.null(se_tx)) {
      if (shiny::is.reactive(se_tx)) se_tx else shiny::reactive(se_tx)
    } else {
      shiny::reactive(NULL)
    }

    # Get groups (class column in colData) -----------------------------------
    .get_groups <- function(dataset = c("lipid", "gene")) {
      dataset <- match.arg(dataset)
      se <- if (dataset == "lipid") se_lipid_r() else se_tx_r()
      if (is.null(se)) return(character(0))
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!nrow(cd) || !"class" %in% colnames(cd)) return(character(0))
      g <- as.character(cd$class)
      g <- g[!is.na(g) & nzchar(g)]
      unique(g)
    }

    # Shared state ------------------------------------------------------------
    rv <- shiny::reactiveValues(
      dataset = "lipid",
      adv     = .default_feature_adv(),
      pal_tbl = NULL
    )

    # ---- IMPORTANT: initialize palette_map early (before modal is opened) ---
    shiny::observe({
      dataset <- rv$dataset %||% "lipid"
      groups  <- .get_groups(dataset)
      adv     <- rv$adv

      if (length(groups)) {
        adv$palette_map <- .merge_palette(groups, adv$palette_map)
        if (!nzchar(adv$ref_group %||% "") || !(adv$ref_group %||% "") %in% groups) {
          adv$ref_group <- groups[1]
        }
      } else {
        # keep palette_map empty if no groups
        adv$palette_map <- adv$palette_map %||% character(0)
      }
      rv$adv <- adv
    })

    # Dataset selection -> update state --------------------------------------
    shiny::observeEvent(input$feature_dataset, {
      rv$dataset <- input$feature_dataset %||% "lipid"
      if (is.function(on_live_change)) on_live_change(list(dataset = rv$dataset))
    }, ignoreInit = FALSE)

    # Shared advanced settings reactive --------------------------------------
    adv_reactive <- shiny::reactive(rv$adv)

    # Local live-update handler (from child modules or outer modules) ----------
    apply_live <- function(live) {
      if (is.null(live) || !is.list(live)) return(invisible(NULL))
      adv <- rv$adv

      if (!is.null(live$plot_type)) adv$plot_type <- as.character(live$plot_type)
      if (!is.null(live$dataset))   rv$dataset <- as.character(live$dataset)
      if (!is.null(live$hm_topN))   adv$hm_topN <- as.integer(live$hm_topN)
      if (!is.null(live$hm_row_z))  adv$hm_row_z <- isTRUE(live$hm_row_z)

      rv$adv <- adv
      if (is.function(on_live_change)) on_live_change(live)
      invisible(NULL)
    }

    # =================================================================
    # Advanced settings modal (Feature-shared)
    # =================================================================

    # --- anti-freeze guard for picker <-> table sync
    syncing <- shiny::reactiveVal(FALSE)

    output$adv_ref_ui <- shiny::renderUI({
      shiny::req((rv$adv$comp_mode %||% "ref") == "ref")
      groups <- .get_groups(rv$dataset)
      shiny::selectInput(
        ns("adv_ref_group"),
        "Reference group",
        choices  = groups,
        selected = if ((rv$adv$ref_group %||% "") %in% groups) rv$adv$ref_group else (groups[1] %||% "")
      )
    })

    output$adv_pairs_ui <- shiny::renderUI({
      shiny::req((rv$adv$comp_mode %||% "ref") == "manual")
      df <- rv$adv$manual_pairs
      txt <- ""
      if (!is.null(df) && nrow(df)) {
        txt <- paste(
          apply(df, 1L, function(z) paste0(z[1], "\t", z[2])),
          collapse = "\n"
        )
      }
      shiny::textAreaInput(
        ns("adv_pairs_txt"),
        "Manual comparisons (one pair per line: group1\\tgroup2)",
        value = txt,
        rows  = 5,
        width = "100%"
      )
    })

    output$adv_color_pickers <- shiny::renderUI({
      df <- rv$pal_tbl
      if (is.null(df) || !nrow(df)) {
        return(shiny::helpText("No 'class' groups found in colData."))
      }
      shiny::tagList(lapply(seq_len(nrow(df)), function(i) {
        gid <- df$group[i]
        col <- df$color[i]
        inputId <- ns(paste0("adv_col_", .idify(gid)))
        if (requireNamespace("colourpicker", quietly = TRUE)) {
          colourpicker::colourInput(
            inputId,
            label = gid,
            value = col,
            showColour = "both"
          )
        } else {
          shiny::textInput(inputId, label = gid, value = col, placeholder = "#RRGGBB")
        }
      }))
    })

    output$adv_palette_table <- if (requireNamespace("rhandsontable", quietly = TRUE)) {
      rhandsontable::renderRHandsontable({
        df <- rv$pal_tbl
        if (is.null(df) || !nrow(df)) return(NULL)
        rhandsontable::rhandsontable(df, rowHeaders = NULL, stretchH = "all") |>
          rhandsontable::hot_col("group", readOnly = TRUE) |>
          rhandsontable::hot_col(
            "color",
            type      = "text",
            validator = "function(value){return /^#?[0-9A-Fa-f]{6}$/.test(value);}"
          ) |>
          rhandsontable::hot_col("level", type = "numeric")
      })
    } else {
      shiny::renderUI({
        df <- rv$pal_tbl
        txt <- if (!is.null(df) && nrow(df)) {
          paste(apply(df, 1, function(r) paste(r[1], r[2], r[3], sep = "\t")), collapse = "\n")
        } else ""
        shiny::textAreaInput(
          ns("adv_palette_text"),
          "Palette table (group\\t#RRGGBB\\tlevel), one row per line",
          value = txt,
          rows  = 10,
          width = "100%"
        )
      })
    }

    # rhandsontable -> update rv$pal_tbl --------------------------------------
    if (requireNamespace("rhandsontable", quietly = TRUE)) {
      shiny::observeEvent(input$adv_palette_table, {
        if (isTRUE(syncing())) return()
        newdf <- try(rhandsontable::hot_to_r(input$adv_palette_table), silent = TRUE)
        if (!inherits(newdf, "try-error") && !is.null(newdf) && nrow(newdf)) {
          newdf$color <- .norm_hex_vec(newdf$color)
          newdf$level <- suppressWarnings(as.numeric(newdf$level))

          # keep row order stable by group
          old <- rv$pal_tbl
          if (!is.null(old) && nrow(old)) {
            keep <- intersect(old$group, newdf$group)
            newdf <- newdf[match(keep, newdf$group), , drop = FALSE]
          }

          syncing(TRUE); on.exit(syncing(FALSE), add = TRUE)
          rv$pal_tbl <- newdf
        }
      }, ignoreInit = TRUE)
    } else {
      shiny::observeEvent(input$adv_palette_text, {
        if (isTRUE(syncing())) return()
        txt <- input$adv_palette_text %||% ""
        rows <- strsplit(txt, "\n", fixed = TRUE)[[1]]
        rows <- rows[nzchar(trimws(rows))]

        if (!length(rows)) return()

        parts <- strsplit(rows, "\t", fixed = TRUE)
        df <- do.call(rbind, lapply(parts, function(p) {
          p <- c(p, NA, NA)[1:3]
          data.frame(
            group = trimws(p[1]),
            color = .norm_hex1(p[2]),
            level = suppressWarnings(as.numeric(p[3])),
            stringsAsFactors = FALSE
          )
        }))
        df <- df[nzchar(df$group), , drop = FALSE]
        if (nrow(df)) {
          syncing(TRUE); on.exit(syncing(FALSE), add = TRUE)
          rv$pal_tbl <- df
        }
      }, ignoreInit = TRUE)
    }

    # Color picker -> table sync ----------------------------------------------
    shiny::observe({
      if (isTRUE(syncing())) return()
      df <- rv$pal_tbl
      if (is.null(df) || !nrow(df)) return()

      pick <- vapply(df$group, function(g) {
        id <- paste0("adv_col_", .idify(g))
        .norm_hex1(input[[id]] %||% "")
      }, FUN.VALUE = character(1))

      idx <- which(!is.na(pick) & pick != df$color)
      if (length(idx)) {
        syncing(TRUE); on.exit(syncing(FALSE), add = TRUE)
        df$color[idx] <- pick[idx]
        rv$pal_tbl <- df
      }
    })

    # table -> Color picker sync ----------------------------------------------
    shiny::observeEvent(rv$pal_tbl, {
      if (isTRUE(syncing())) return()
      df <- rv$pal_tbl
      if (is.null(df) || !nrow(df)) return()

      syncing(TRUE); on.exit(syncing(FALSE), add = TRUE)

      for (i in seq_len(nrow(df))) {
        gid <- df$group[i]
        val <- df$color[i] %||% ""
        id  <- paste0("adv_col_", .idify(gid))

        if (requireNamespace("colourpicker", quietly = TRUE)) {
          if (!is.null(input[[id]])) colourpicker::updateColourInput(session, inputId = id, value = val)
        } else {
          if (!is.null(input[[id]])) shiny::updateTextInput(session, inputId = id, value = val)
        }
      }
    }, ignoreInit = TRUE)

    # Build modal dialog -------------------------------------------------------
    feature_adv_modal <- function() {
      adv    <- rv$adv
      groups <- .get_groups(rv$dataset)

      # init palette table from adv (already initialized before modal)
      pal_map <- .merge_palette(groups, adv$palette_map)

      # init level from manual_order
      lvl <- if (length(adv$manual_order)) {
        ord <- intersect(adv$manual_order, groups)
        if (length(ord)) {
          rest <- setdiff(groups, ord)
          match(groups, c(ord, rest))
        } else seq_along(groups)
      } else seq_along(groups)

      rv$pal_tbl <- if (length(groups)) {
        data.frame(
          group = groups,
          color = unname(pal_map[groups]),
          level = lvl,
          stringsAsFactors = FALSE
        )
      } else NULL

      shiny::modalDialog(
        title     = "Advanced settings (Shared)",
        size      = "l",
        easyClose = TRUE,
        footer    = shiny::tagList(
          shiny::actionButton(ns("adv_apply"), "Apply", class = "btn-primary"),
          shiny::modalButton("Cancel")
        ),
        shiny::tabsetPanel(
          id = ns("adv_tabs"),

          shiny::tabPanel(
            "Colors / order",
            shiny::fluidRow(
              shiny::column(6, shiny::h4("Color / order"), shiny::uiOutput(ns("adv_color_pickers"))),
              shiny::column(
                6,
                shiny::h4("Palette table (edit colors / order)"),
                if (requireNamespace("rhandsontable", quietly = TRUE)) {
                  rhandsontable::rHandsontableOutput(ns("adv_palette_table"), height = 420)
                } else {
                  shiny::uiOutput(ns("adv_palette_table"))
                }
              )
            ),
            shiny::helpText("Use the 'level' column to define a manual x-axis order (smaller comes first).")
          ),

          shiny::tabPanel(
            "Statistics",
            shiny::h4("p-value annotation (Wilcoxon / t-test)"),
            shiny::fluidRow(
              shiny::column(3, shiny::checkboxInput(ns("adv_add_p"), "Show p-values", value = isTRUE(adv$add_p))),
              shiny::column(3, shiny::selectInput(ns("adv_test"), "Test", choices = c("wilcox", "t.test"), selected = adv$test)),
              shiny::column(3, shiny::selectInput(ns("adv_p_label"), "Label", choices = c("p", "stars", "both"), selected = adv$p_label))
            ),
            shiny::hr(),
            shiny::radioButtons(
              ns("adv_comp_mode"),
              "Comparison mode",
              choices  = c("All pairwise comparisons" = "all",
                           "Reference vs others"      = "ref",
                           "Manual pairs"             = "manual"),
              selected = adv$comp_mode,
              inline   = TRUE
            ),
            shiny::uiOutput(ns("adv_ref_ui")),
            shiny::uiOutput(ns("adv_pairs_ui"))
          ),

          shiny::tabPanel(
            "Plot styling",
            shiny::h4("Plot styling parameters (Dot / Box / Violin)"),
            shiny::fluidRow(
              shiny::column(
                4, shiny::h5("Dot"),
                shiny::numericInput(ns("adv_dot_point_size"),   "point size",        adv$dot_point_size,   min = 0.1, step = 0.1),
                shiny::numericInput(ns("adv_dot_jitter_width"), "jitter width",      adv$dot_jitter_width, min = 0,   step = 0.01),
                shiny::numericInput(ns("adv_dot_alpha"),        "alpha (0-1)",        adv$dot_alpha,        min = 0, max = 1, step = 0.05),
                shiny::checkboxInput(ns("adv_dot_show_median"), "Show median line",  adv$dot_show_median),
                shiny::numericInput(ns("adv_dot_median_size"),  "median line size",  adv$dot_median_size,  min = 0.1, step = 0.1),
                shiny::numericInput(ns("adv_dot_median_width"), "median line width", adv$dot_median_width, min = 0,   step = 0.05),
                shiny::textInput(ns("adv_dot_median_color"),    "median line color", adv$dot_median_color)
              ),
              shiny::column(
                4, shiny::h5("Box"),
                shiny::numericInput(ns("adv_box_width"),        "box width",          adv$box_width,        min = 0.1, step = 0.05),
                shiny::numericInput(ns("adv_box_alpha"),        "box alpha (0-1)",    adv$box_alpha,        min = 0, max = 1, step = 0.05),
                shiny::checkboxInput(ns("adv_box_show_points"), "Show points",        adv$box_show_points),
                shiny::numericInput(ns("adv_box_point_size"),   "point size",         adv$box_point_size,   min = 0.1, step = 0.1),
                shiny::numericInput(ns("adv_box_jitter_width"), "jitter width",       adv$box_jitter_width, min = 0,   step = 0.01),
                shiny::numericInput(ns("adv_box_point_alpha"),  "point alpha (0-1)",  adv$box_point_alpha,  min = 0, max = 1, step = 0.05)
              ),
              shiny::column(
                4, shiny::h5("Violin"),
                shiny::numericInput(ns("adv_violin_width"),        "violin width",        adv$violin_width,        min = 0.1, step = 0.05),
                shiny::numericInput(ns("adv_violin_alpha"),        "violin alpha (0-1)",  adv$violin_alpha,        min = 0, max = 1, step = 0.05),
                shiny::checkboxInput(ns("adv_violin_trim"),        "trim",                adv$violin_trim),
                shiny::checkboxInput(ns("adv_violin_show_points"), "Show points",         adv$violin_show_points),
                shiny::numericInput(ns("adv_violin_point_size"),   "point size",          adv$violin_point_size,   min = 0.1, step = 0.1),
                shiny::numericInput(ns("adv_violin_jitter_width"), "jitter width",        adv$violin_jitter_width, min = 0,   step = 0.01),
                shiny::numericInput(ns("adv_violin_point_alpha"),  "point alpha (0-1)",   adv$violin_point_alpha,  min = 0, max = 1, step = 0.05),
                shiny::checkboxInput(ns("adv_violin_show_median"), "Show median line",    adv$violin_show_median),
                shiny::numericInput(ns("adv_violin_median_size"),  "median line size",    adv$violin_median_size,  min = 0.1, step = 0.1),
                shiny::textInput(ns("adv_violin_median_color"),    "median line color",   adv$violin_median_color)
              )
            )
          ),

          shiny::tabPanel(
            "Heatmap",
            shiny::h4("High-variance gene heatmap / Lipid Top-N"),
            shiny::fluidRow(
              shiny::column(4, shiny::checkboxInput(ns("adv_hm_row_z"), "row z-score", value = isTRUE(adv$hm_row_z))),
              shiny::column(4, shiny::numericInput(ns("adv_hm_topN"), "Top-N genes / molecules", value = adv$hm_topN, min = 5, step = 5))
            )
          )
        )
      )
    }

    # Apply advanced settings --------------------------------------------------
    shiny::observeEvent(input$adv_apply, {
      # guard: avoid sync loops while committing
      syncing(TRUE); on.exit(syncing(FALSE), add = TRUE)

      adv <- rv$adv

      # Palette table -> adv$palette_map / adv$manual_order
      df <- rv$pal_tbl
      if (!is.null(df) && nrow(df)) {
        df$color <- .norm_hex_vec(df$color)

        ok <- !is.na(df$color) & nzchar(df$color)
        pal_map <- df$color[ok]
        names(pal_map) <- as.character(df$group[ok])
        adv$palette_map <- pal_map

        if ("level" %in% names(df) && any(!is.na(df$level))) {
          df2 <- df[!is.na(df$level), , drop = FALSE]
          df2 <- df2[order(df2$level, decreasing = FALSE), , drop = FALSE]
          adv$manual_order <- as.character(df2$group)
          if (length(adv$manual_order)) {
            adv$order_by   <- "none"
            adv$decreasing <- FALSE
          }
        }
      }

      # Statistics
      adv$add_p     <- isTRUE(input$adv_add_p)
      adv$test      <- input$adv_test    %||% "wilcox"
      adv$p_label   <- input$adv_p_label %||% "both"
      adv$comp_mode <- input$adv_comp_mode %||% "ref"
      adv$ref_group <- input$adv_ref_group %||% ""

      # Manual pairs (tab-separated, ignore empty lines)
      txt <- input$adv_pairs_txt %||% ""
      txt <- trimws(txt)
      if (nzchar(txt)) {
        rows <- strsplit(txt, "\n", fixed = TRUE)[[1]]
        rows <- rows[nzchar(trimws(rows))]
        parts <- strsplit(rows, "\t", fixed = TRUE)
        dfp <- do.call(rbind, lapply(parts, function(p) {
          p <- c(p, NA, NA)
          data.frame(
            group1 = trimws(p[1]),
            group2 = trimws(p[2]),
            stringsAsFactors = FALSE
          )
        }))
        dfp <- dfp[nzchar(dfp$group1) & nzchar(dfp$group2), , drop = FALSE]
        adv$manual_pairs <- if (nrow(dfp)) dfp else data.frame(group1 = NA_character_, group2 = NA_character_, stringsAsFactors = FALSE)
      } else {
        adv$manual_pairs <- data.frame(group1 = NA_character_, group2 = NA_character_, stringsAsFactors = FALSE)
      }

      # Dot
      adv$dot_point_size   <- input$adv_dot_point_size
      adv$dot_jitter_width <- input$adv_dot_jitter_width
      adv$dot_alpha        <- input$adv_dot_alpha
      adv$dot_show_median  <- isTRUE(input$adv_dot_show_median)
      adv$dot_median_size  <- input$adv_dot_median_size
      adv$dot_median_width <- input$adv_dot_median_width
      adv$dot_median_color <- input$adv_dot_median_color %||% "#222222"

      # Box
      adv$box_width        <- input$adv_box_width
      adv$box_alpha        <- input$adv_box_alpha
      adv$box_show_points  <- isTRUE(input$adv_box_show_points)
      adv$box_point_size   <- input$adv_box_point_size
      adv$box_jitter_width <- input$adv_box_jitter_width
      adv$box_point_alpha  <- input$adv_box_point_alpha

      # Violin
      adv$violin_width        <- input$adv_violin_width
      adv$violin_alpha        <- input$adv_violin_alpha
      adv$violin_trim         <- isTRUE(input$adv_violin_trim)
      adv$violin_show_points  <- isTRUE(input$adv_violin_show_points)
      adv$violin_point_size   <- input$adv_violin_point_size
      adv$violin_jitter_width <- input$adv_violin_jitter_width
      adv$violin_point_alpha  <- input$adv_violin_point_alpha
      adv$violin_show_median  <- isTRUE(input$adv_violin_show_median)
      adv$violin_median_size  <- input$adv_violin_median_size
      adv$violin_median_color <- input$adv_violin_median_color %||% "#222222"

      # Heatmap
      adv$hm_row_z <- isTRUE(input$adv_hm_row_z)
      topN_in <- input$adv_hm_topN
      if (is.null(topN_in) || !is.finite(topN_in) || topN_in <= 0) topN_in <- 50
      adv$hm_topN <- as.integer(topN_in)

      # Commit state
      rv$adv <- adv

      if (is.function(on_adv_commit)) on_adv_commit(adv)
      shiny::removeModal()
    }, ignoreInit = TRUE)

    # Public "hub" API: open_adv() --------------------------------------------
    open_adv <- function(dataset = NULL) {
      if (!is.null(dataset)) rv$dataset <- as.character(dataset)
      shiny::showModal(feature_adv_modal())
      invisible(NULL)
    }

    # Optional: local button in this module UI
    shiny::observeEvent(input$open_adv_btn, {
      open_adv(rv$dataset %||% "lipid")
    }, ignoreInit = TRUE)

    # Optional: child plot modules (only if they exist) -----------------------
    if (exists("mod_plot_lipid_server", inherits = TRUE) && is.function(get("mod_plot_lipid_server", inherits = TRUE))) {
      mod_plot_lipid_server(
        id             = "lipid",
        se_in          = se_lipid_r,
        adv_reactive   = adv_reactive,
        on_live_change = apply_live,
        on_open_adv    = function() open_adv("lipid")
      )
    }
    if (exists("mod_plot_gene_server", inherits = TRUE) && is.function(get("mod_plot_gene_server", inherits = TRUE))) {
      mod_plot_gene_server(
        id             = "gene",
        se_in          = se_tx_r,
        adv_reactive   = adv_reactive,
        on_live_change = apply_live,
        on_open_adv    = function() open_adv("gene")
      )
    }

    # Return hub handles ------------------------------------------------------
    list(
      adv_reactive = adv_reactive,
      open_adv     = open_adv,
      apply_live   = apply_live
    )
  })
}

# ======================================================================
# Feature generic UI
# ======================================================================

#' Feature generic UI
#'
#' @param id Module ID
#' @param title Panel title
#' @param show_adv_button logical (show a local Advanced button)
#'
#' @export
mod_feature_generic_ui <- function(id, title = "Feature") {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          style = "display:flex; align-items:center; justify-content:space-between; gap:10px;",
          shiny::h3(title, style = "margin: 6px 0;")
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::radioButtons(
            ns("feature_dataset"),
            label    = "Dataset",
            choices  = c("Lipid" = "lipid", "Gene" = "gene"),
            selected = "lipid",
            inline   = TRUE
          )
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'lipid'", ns("feature_dataset")),
          if (exists("mod_plot_lipid_ui", inherits = TRUE)) mod_plot_lipid_ui(ns("lipid")) else shiny::helpText("mod_plot_lipid_ui() is not available.")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'gene'", ns("feature_dataset")),
          if (exists("mod_plot_gene_ui", inherits = TRUE)) mod_plot_gene_ui(ns("gene")) else shiny::helpText("mod_plot_gene_ui() is not available.")
        )
      )
    )
  )
}
