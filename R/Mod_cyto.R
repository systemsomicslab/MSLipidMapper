# R/mod_cyto.R ---------------------------------------------------------------
# Shiny module: Cytoscape network + plot panels + SVG mapping + export.
# (NO fallbacks: if plot/heatmap generation fails, throw error)

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(jsonlite)
  library(htmltools)  # for htmlEscape
})

# Depends:
# - source("R/cyto_utils.R")      # must define:
#     %||%, .safe_id, .cyjs_to_elements, .parse_gml_yed, .parse_gpml,
#     .ensure_positions_for_preset, .any_node_has_pos,
#     .get_svg_dir, .CY_IMG_BASE_URL, .get_svg_size, .save_svg_file,
#     .prep_for_node_svg, aggregate_to_class_se, get_plot_fun,
#     get_valid_classes_from_se, resolve_class_col,
#     .get_adv_or_default, .pvalue_args_from_adv, .plot_style_args_from_adv,
#     .apply_palette_if_missing, .has_chain_col, .get_unique_chain_codes,
#     .filter_lipid_se_by_chains, .make_ensembl_to_rowname_map,
#     .make_heatmap_for_class
# - source("R/cyto_js_deps.R")    # must define: cyto_js_deps()
#
# IMPORTANT:
# - cyto_js_deps.R must implement handlers:
#     "cy_delete_selected", "cy_push_cyjs", "cy_set_node_images", "cy_loading",
#     "cy_set_node_borders", "cy_set_node_meta", "cy_set_node_borderstyle", "cy_resize".
#
# Enrichment:
# - CSV input with columns: ID, Description, pvalue, p.adjust, qvalue, Count, type, lipidID
# - Mapping rule: term is NOT mapped to "chain nodes". Instead:
#     lipidID -> molecules -> lipid class -> class node popup (Enrichment tab).
# - Highlight class nodes with DOUBLE border style (no color change).

# ---- SAFE CSS formatter (NO sprintf; robust to '%' in CSS) ------------------
.fill_css_id <- function(fmt, id) {
  gsub("#%s", paste0("#", id), fmt, fixed = TRUE)
}

.cy_details <- function(title, ..., open = FALSE) {
  shiny::tags$details(
    open = if (isTRUE(open)) NA else NULL,
    class = "cy-acc",
    shiny::tags$summary(shiny::tags$span(title)),
    shiny::div(class = "cy-acc-body", ...)
  )
}

mod_cyto_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar_id <- ns("cy_sidebar")

    css_fmt <- "
#%s{ background: transparent; }

#%s .well,
#%s .well.well,
#%s .panel,
#%s .panel-body{
  padding: 10px 10px 6px 10px !important;
  background-color: #f6f7f9 !important;
  border: 1px solid #e5e7eb !important;
  border-radius: 12px !important;
  box-shadow: none !important;
}

#%s .cy-acc{
  border: 1px solid #e5e7eb;
  border-radius: 10px;
  margin: 8px 0;
  background: #fff;
  overflow: hidden;
}

#%s .cy-acc > summary{
  list-style: none !important;
  cursor: pointer;
  padding: 10px 10px;
  font-weight: 700;
  font-size: 13px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  user-select: none;
  background: #f8fafc;
  outline: none;
}

/* ---- FIX: remove default marker (prevents '???' in some browsers) ---- */
#%s .cy-acc > summary::-webkit-details-marker{
  display: none !important;
}
#%s .cy-acc > summary::marker{
  content: '' !important;
}
/* まれに他CSSがsummaryに疑似要素で記号を挿す場合の保険（あなたはafterを使うのでbeforeのみ潰す） */
#%s .cy-acc > summary::before{
  content: '' !important;
}

#%s .cy-acc > summary::after{
  content: '▸';
  font-size: 12px;
  opacity: 0.7;
  transform: rotate(0deg);
  transition: transform .15s ease;
}
#%s .cy-acc[open] > summary::after{ transform: rotate(90deg); }

#%s .cy-acc-body{ padding: 10px; }

#%s .btn-xs{ padding: 4px 8px; font-size: 11px; }

#%s .help-block{
  font-size: 11px;
  opacity: 0.85;
  margin: 4px 0 0 0;
}

#%s hr{ margin: 10px 0; }

#%s .cy-actions{
  display: flex;
  gap: 6px;
  flex-wrap: wrap;
}

#%s .radio-inline{ margin-right: 10px; }

.plot-card{
  border: 1px solid #e5e7eb;
  border-radius: 12px;
  background: #fff;
  padding: 8px 10px 10px 10px;
}
.plot-card h5{ margin-top: 4px; margin-bottom: 6px; }
.dl-row{ margin-top: 6px; }
"


  shiny::tagList(
    cyto_js_deps(container_id_css = ns("cy")),

    shiny::tags$style(shiny::HTML(.fill_css_id(css_fmt, sidebar_id))),

    shiny::fluidRow(
      shiny::column(
        width = 2,
        class = "cy-sidebar",
        id = sidebar_id,
        shiny::wellPanel(

          .cy_details(
            "Networks",
            shiny::fileInput(
              ns("netfiles"),
              "Import networks (.cyjs/.gml)",
              accept = c(".cyjs", ".json", ".gml", ".gpml", ".xml"),
              multiple = TRUE
            ),
            shiny::selectInput(
              ns("net_select"),
              label = NULL,
              choices = character(0),
              size = 8,
              selectize = FALSE
            ),
            shiny::div(
              class = "cy-actions",
              shiny::actionButton(ns("net_delete"), "Delete network", class = "btn-xs"),
              shiny::actionButton(ns("fit"), "Fit network", class = "btn-xs"),
              shiny::actionButton(ns("map_local_svgs"), "Pathway Mapping", class = "btn-primary btn-sm")
            ),
            br(),
            shiny::radioButtons(
              ns("plot_type"),
              "Plot type",
              choices  = c("dot", "violin", "box"),
              selected = "dot",
              inline   = TRUE
            ),
            open = TRUE
          ),

          .cy_details(
            "Enrichment (CSV)",
            shiny::fileInput(ns("enrich_csv"), "Import enrichment CSV (.csv)", accept = ".csv", multiple = FALSE),
            shiny::div(
              class = "cy-actions",
              shiny::actionButton(ns("apply_enrich"), "Apply", class = "btn-xs btn-primary"),
              shiny::actionButton(ns("clear_enrich"), "Clear", class = "btn-xs")
            ),
            shiny::helpText("Mapping: lipidID → molecules → lipid class → class node popup (Enrichment tab)."),
            shiny::helpText("Class nodes with any enriched molecules are highlighted with DOUBLE borders."),
            open = FALSE
          ),

          .cy_details(
            "Differential (2-group)",
            shiny::uiOutput(ns("diff_ui")),
            shiny::div(
              class = "cy-actions",
              shiny::actionButton(ns("apply_diff_borders"), "Apply", class = "btn-xs btn-primary"),
              shiny::actionButton(ns("clear_diff_borders"), "Clear", class = "btn-xs")
            ),
            shiny::helpText("Group column is fixed to colData$class. Direction is B − A."),
            shiny::helpText("Significant increase = red border; decrease = blue border."),
            open = FALSE
          ),

          .cy_details(
            "Export",
            shiny::actionButton(ns("export_pdf"), "Export PDF", class = "btn-xs"),
            shiny::actionButton(ns("export_pdf_popups"), "Export PDF (with popups)", class = "btn-xs"),
            open = FALSE
          ),

          .cy_details(
            "Map edit",
            shiny::actionButton(ns("delete_selected"), "Delete selected", class = "btn-xs btn-danger"),
            shiny::helpText("Select nodes/edges then press to delete."),
            open = FALSE
          ),

          .cy_details(
            "Image option",
            shiny::selectInput(
              ns("agg_fun"),
              "Aggregation (class-level)",
              choices = c("sum", "mean", "median"),
              selected = "sum"
            ),
            shiny::numericInput(
              ns("hm_top_n"),
              "Heatmap: #molecules (top by abundance)",
              value = 20, min = 5, max = 200, step = 5
            ),
            open = FALSE
          ),

          .cy_details(
            "Acyl-chain filter",
            shiny::uiOutput(ns("chain_filter_ui")),
            open = FALSE
          ),

          shiny::helpText("Click a node to draw plots below. Right-click a node for popup image.")
        )
      ),

      shiny::column(
        width = 10,

        shiny::div(
          id = ns("cy"),
          style = "width:100%; height:720px; border:1px solid #e5e7eb; border-radius:12px;"
        ),

        shiny::div(
          style = "margin-top:10px; padding:10px; border:1px solid #e5e7eb; border-radius:12px; background:#fff;",
          shiny::h4("Selected node"),

          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::div(
                class = "plot-card",
                shiny::uiOutput(ns("plot_title_left")),
                shiny::plotOutput(ns("plot_left"), height = "360px"),
                shiny::div(
                  class = "dl-row",
                  shiny::downloadButton(ns("dl_left"), "Download", class = "btn btn-default btn-sm")
                )
              )
            ),
            shiny::column(
              width = 6,
              shiny::div(
                class = "plot-card",
                shiny::uiOutput(ns("plot_title_right")),
                shiny::uiOutput(ns("molecule_ui")),
                shiny::plotOutput(ns("plot_right"), height = "360px"),
                shiny::div(
                  class = "dl-row",
                  shiny::downloadButton(ns("dl_right"), "Download", class = "btn btn-default btn-sm")
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_cyto_server <- function(id,
                            se_lipid_reactive,
                            se_tx_reactive = NULL,
                            settings_reactive = NULL,
                            elements = NULL,              # reactive() or list
                            elements_name = "external",
                            default_layout = "cose") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    container_id <- ns("cy")

    rv_nets        <- reactiveValues(map = list())
    rv_selected    <- reactiveVal(NULL)
    selected_node  <- reactiveVal(NULL)

    # ---- Image URL builder (IMPORTANT: /static prefix) -----------------------
    CY_IMG_PORT   <- 7310L
    CY_IMG_PREFIX <- "/static"

    .infer_img_origin <- function(session, port = CY_IMG_PORT) {
      proto <- session$clientData$url_protocol %||% "http:"
      host  <- session$clientData$url_hostname %||% "127.0.0.1"
      host  <- trimws(as.character(host))
      if (!nzchar(host)) host <- "127.0.0.1"
      paste0(proto, "//", host, ":", as.integer(port))
    }

    .img_url <- function(fname, session, ts = NULL, prefix = CY_IMG_PREFIX) {
      origin <- .infer_img_origin(session, port = CY_IMG_PORT)
      u <- paste0(origin, prefix, "/", utils::URLencode(fname, reserved = TRUE))
      if (!is.null(ts)) u <- paste0(u, "?t=", ts)
      u
    }

    # per-network differential border styles (persist across cy_init)
    rv_diff_border <- reactiveValues(map = list())  # map[[net_id]] = list(items=..., reset_ids=...)

    # per-network enrichment state (persist across cy_init)
    rv_enrich_state <- reactiveValues(map = list()) # map[[net_id]] = list(meta=..., borderstyle=...)

    make_id <- function() paste0(
      "net_",
      as.integer(as.numeric(Sys.time()) * 1000), "_",
      paste(sample(c(letters, 0:9), 4, TRUE), collapse = "")
    )

    add_network <- function(name, elements) {
      id <- make_id()
      rv_nets$map[[id]] <- list(id = id, name = name %||% id, elements_work = elements)
      if (is.null(rv_selected())) rv_selected(id)

      updateSelectInput(
        session, "net_select",
        choices  = setNames(names(rv_nets$map), vapply(rv_nets$map, `[[`, character(1), "name")),
        selected = rv_selected()
      )
    }

    remove_network <- function(id) {
      if (is.null(id) || is.null(rv_nets$map[[id]])) return()
      rv_nets$map[[id]] <- NULL
      rv_diff_border$map[[id]] <- NULL
      rv_enrich_state$map[[id]] <- NULL

      ids <- names(rv_nets$map)
      rv_selected(if (length(ids)) ids[[1]] else NULL)

      updateSelectInput(
        session, "net_select",
        choices  = setNames(names(rv_nets$map), vapply(rv_nets$map, `[[`, character(1), "name")),
        selected = rv_selected()
      )
    }

    .coerce_elements <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.function(x)) x <- x()
      if (is.null(x)) return(NULL)

      if (is.list(x) && !is.null(x$nodes) && !is.null(x$edges)) return(x)

      if (is.list(x) && (!is.null(x$elements) || !is.null(x$nodes) || !is.null(x$edges))) {
        return(.cyjs_to_elements(x))
      }

      stop("elements must be a Cytoscape elements list (nodes/edges) or a cyjs-like list.", call. = FALSE)
    }

    observeEvent(
      {
        if (is.null(elements)) NULL else {
          if (is.function(elements)) elements() else elements
        }
      },
      {
        el <- .coerce_elements(elements)
        req(el)

        el <- .ensure_positions_for_preset(el)

        id_hit <- NULL
        for (nid in names(rv_nets$map)) {
          if (identical(rv_nets$map[[nid]]$name, elements_name)) { id_hit <- nid; break }
        }

        if (is.null(id_hit)) {
          add_network(elements_name, el)
        } else {
          rv_nets$map[[id_hit]]$elements_work <- el
          rv_selected(id_hit)
          updateSelectInput(
            session, "net_select",
            choices  = setNames(names(rv_nets$map), vapply(rv_nets$map, `[[`, character(1), "name")),
            selected = rv_selected()
          )
        }
      },
      ignoreInit = FALSE
    )

    observeEvent(input$netfiles, {
      df <- input$netfiles
      req(df)

      for (i in seq_len(nrow(df))) {
        nm   <- df$name[i]
        path <- df$datapath[i]
        ext  <- tolower(tools::file_ext(nm))

        els <- NULL
        if (ext %in% c("cyjs", "json")) {
          obj <- jsonlite::read_json(path, simplifyVector = FALSE)
          els <- .cyjs_to_elements(obj)
        } else if (ext %in% c("gml")) {
          els <- .parse_gml_yed(path)
        } else if (ext %in% c("gpml", "xml")) {
          els <- .parse_gpml(path)
        } else {
          next
        }

        els <- .ensure_positions_for_preset(els)
        base <- tools::file_path_sans_ext(nm)
        add_network(base, els)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$net_select, { rv_selected(input$net_select) }, ignoreInit = TRUE)
    observeEvent(input$net_delete, { remove_network(rv_selected()) }, ignoreInit = TRUE)

    # ---- apply node images to cy from elements (for existing path) ------------
    .send_node_images_from_elements <- function(el, thin_bw = 1L, clear_all_first = TRUE, show_loading = FALSE) {
      if (is.null(el) || is.null(el$nodes)) return(invisible(NULL))

      items     <- list()
      clear_ids <- character(0)
      all_ids   <- character(0)

      for (n in el$nodes) {
        d <- n$data
        if (is.null(d$id)) next
        all_ids <- c(all_ids, d$id)

        bw <- as.integer(if (!is.null(d$BorderWidth)) d$BorderWidth else thin_bw)

        if (!is.null(d$path) && nzchar(d$path)) {
          hm <- d$heatmap_path
          if (is.null(hm)) hm <- ""
          items[[length(items) + 1L]] <- list(
            id            = d$id,
            url           = d$path,
            hm_url        = as.character(hm),
            borderWidthPx = bw
          )
        }
      }

      if (clear_all_first) clear_ids <- all_ids

      session$sendCustomMessage(
        "cy_set_node_images",
        list(
          container_id  = container_id,
          items         = items,
          clear_ids     = clear_ids,
          thinBorderPx  = as.integer(thin_bw),
          show_loading  = isTRUE(show_loading)
        )
      )
      invisible(NULL)
    }

    # ---- Deletion: Shiny -> JS ------------------------------------------------
    observeEvent(input$delete_selected, {
      session$sendCustomMessage("cy_delete_selected", list(
        container_id = container_id,
        fit_after    = FALSE
      ))
    }, ignoreInit = TRUE)

    # ---- Deletion sync: JS -> Shiny ------------------------------------------
    observeEvent(input$cy_changed, {
      msg <- input$cy_changed
      if (is.null(msg) || is.null(msg$action)) return()

      id <- rv_selected()
      if (is.null(id) || is.null(rv_nets$map[[id]])) return()

      if (identical(msg$action, "delete_selected")) {
        session$sendCustomMessage("cy_push_cyjs", list(container_id = container_id))
        return()
      }

      if (identical(msg$action, "cyjs") && !is.null(msg$cyjs)) {
        new_el <- .cyjs_to_elements(msg$cyjs)
        new_el <- .ensure_positions_for_preset(new_el)

        info <- rv_nets$map[[id]]
        info$elements_work <- new_el
        rv_nets$map[[id]] <- info
      }
    }, ignoreInit = TRUE)

    # ---- Differential (2-group) UI (fixed group_col = "class") ---------------
    DIFF_GROUP_COL <- "class"

    se_group_ref <- reactive({
      se <- tryCatch(se_lipid_reactive(), error = function(e) NULL)
      if (!is.null(se)) return(se)
      if (!is.null(se_tx_reactive)) {
        se2 <- tryCatch(se_tx_reactive(), error = function(e) NULL)
        return(se2)
      }
      NULL
    })

    .class_levels_from_se <- function(se, class_col = DIFF_GROUP_COL) {
      if (is.null(se)) return(character(0))
      cd <- as.data.frame(colData(se))
      if (!class_col %in% names(cd)) return(character(0))
      v <- as.character(cd[[class_col]])
      v <- v[!is.na(v)]
      v <- trimws(v)
      v <- v[nzchar(v)]
      sort(unique(v))
    }

    output$diff_ui <- renderUI({
      se <- se_group_ref()
      if (is.null(se)) {
        return(shiny::helpText("Differential: unavailable (no SE provided)."))
      }
      levs <- .class_levels_from_se(se, DIFF_GROUP_COL)
      if (length(levs) < 2) {
        return(shiny::helpText(paste0("Differential: unavailable (colData$", DIFF_GROUP_COL, " has <2 levels).")))
      }

      a0 <- isolate(input$diff_group_a)
      b0 <- isolate(input$diff_group_b)

      if (is.null(a0) || !(a0 %in% levs)) a0 <- levs[[1]]
      if (is.null(b0) || !(b0 %in% levs)) b0 <- levs[[min(2, length(levs))]]
      if (identical(a0, b0) && length(levs) >= 2) b0 <- levs[[2]]

      adj0 <- isTRUE(isolate(input$diff_use_padj))

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("diff_group_a"), "Group A", choices = levs, selected = a0)),
          shiny::column(6, shiny::selectInput(ns("diff_group_b"), "Group B", choices = levs, selected = b0))
        ),
        shiny::selectInput(
          ns("diff_method"),
          "Test",
          choices = c("Wilcoxon" = "wilcox", "t-test" = "t"),
          selected = isolate(input$diff_method) %||% "wilcox"
        ),
        shiny::checkboxInput(
          ns("diff_use_padj"),
          "Use BH-adjusted p-value (FDR)",
          value = adj0
        ),
        shiny::numericInput(
          ns("diff_alpha"),
          "Alpha",
          value = isolate(input$diff_alpha) %||% 0.05,
          min = 1e-6, max = 0.5, step = 0.01
        )
      )
    })

    .idx_from_se <- function(se, gA, gB, class_col = DIFF_GROUP_COL) {
      cd <- as.data.frame(colData(se))
      if (!class_col %in% names(cd)) stop("Group column not found in colData: ", class_col, call. = FALSE)
      grp <- trimws(as.character(cd[[class_col]]))
      iA <- which(grp == gA)
      iB <- which(grp == gB)
      if (!length(iA) || !length(iB)) stop("Group A/B has no samples in SE.", call. = FALSE)
      list(iA = iA, iB = iB)
    }

    .test_two_group <- function(xA, xB, method = c("wilcox", "t")) {
      method <- match.arg(method)
      xA <- as.numeric(xA); xB <- as.numeric(xB)
      xA <- xA[is.finite(xA)]
      xB <- xB[is.finite(xB)]
      if (length(xA) < 2 || length(xB) < 2) return(list(p = NA_real_, lfc = NA_real_))

      muA <- mean(xA, na.rm = TRUE)
      muB <- mean(xB, na.rm = TRUE)
      lfc <- log2(muB + 1e-8) - log2(muA + 1e-8)

      p <- tryCatch({
        if (method == "wilcox") stats::wilcox.test(xB, xA)$p.value else stats::t.test(xB, xA)$p.value
      }, error = function(e) NA_real_)

      list(p = p, lfc = lfc)
    }

    .extract_net_keys <- function(el) {
      cls_keys <- unique(vapply(el$nodes, function(n) {
        d <- n$data %||% list()
        trimws(as.character(d$shared_name %||% ""))
      }, FUN.VALUE = character(1)))
      cls_keys <- cls_keys[nzchar(cls_keys)]

      ens_keys <- unique(vapply(el$nodes, function(n) {
        d <- n$data %||% list()
        e <- trimws(as.character(d$Ensembl_ID %||% ""))
        e <- sub("\\..*$", "", e)
        e
      }, FUN.VALUE = character(1)))
      ens_keys <- ens_keys[nzchar(ens_keys)]

      list(class = cls_keys, ens = ens_keys)
    }

    .compute_diff_stats <- function(el, se_lipid, se_tx, gA, gB, method = "wilcox") {
      stats_map <- list()

      if (!is.null(se_lipid)) {
        ii <- .idx_from_se(se_lipid, gA, gB, DIFF_GROUP_COL)

        valid_lipid <- get_valid_classes_from_se(se_lipid)
        keys <- .extract_net_keys(el)$class
        target <- intersect(keys, valid_lipid)

        if (length(target)) {
          agg_fun <- tolower(as.character(input$agg_fun %||% "sum"))[1]
          se_cls <- aggregate_to_class_se(se_lipid, fun = agg_fun)
          mat <- as.matrix(assay(se_cls, 1))

          for (cls in target) {
            if (!cls %in% rownames(mat)) next
            r <- .test_two_group(mat[cls, ii$iA], mat[cls, ii$iB], method = method)
            stats_map[[paste0("CLASS__", cls)]] <- r
          }
        }
      }

      if (!is.null(se_tx)) {
        ii <- .idx_from_se(se_tx, gA, gB, DIFF_GROUP_COL)

        ens_map <- .make_ensembl_to_rowname_map(se_tx)
        keys <- .extract_net_keys(el)$ens
        target <- intersect(keys, names(ens_map))

        if (length(target)) {
          mat <- as.matrix(assay(se_tx, 1))
          for (ens in target) {
            fid <- unname(ens_map[[ens]])
            if (!fid %in% rownames(mat)) next
            r <- .test_two_group(mat[fid, ii$iA], mat[fid, ii$iB], method = method)
            stats_map[[paste0("ENS__", ens)]] <- r
          }
        }
      }

      stats_map
    }

    .border_items_from_stats <- function(el, stats_map, alpha = 0.05, use_padj = FALSE) {
      if (!length(stats_map)) return(list(items = list(), reset_ids = character(0)))

      keys  <- names(stats_map)
      pvals <- vapply(stats_map, function(x) x$p, numeric(1))

      p_used <- pvals
      if (isTRUE(use_padj)) {
        p_used <- stats::p.adjust(pvals, method = "BH")
      }
      names(p_used) <- keys

      col_up   <- "#ef4444"
      col_down <- "#3b82f6"
      col_ns   <- "#9ca3af"

      items <- list()

      for (n in el$nodes) {
        d <- n$data %||% list()
        nid <- as.character(d$id %||% "")
        if (!nzchar(nid)) next

        cls <- trimws(as.character(d$shared_name %||% ""))
        ens <- trimws(as.character(d$Ensembl_ID %||% ""))
        ens <- sub("\\..*$", "", ens)

        k <- NULL
        if (nzchar(ens)) k <- paste0("ENS__", ens)
        else if (nzchar(cls)) k <- paste0("CLASS__", cls)

        if (!is.null(k) && k %in% names(p_used) && is.finite(p_used[[k]])) {
          lfc <- stats_map[[k]]$lfc
          if (isTRUE(p_used[[k]] <= alpha) && is.finite(lfc)) {
            col <- if (lfc > 0) col_up else col_down
          } else {
            col <- col_ns
          }
          items[[length(items) + 1L]] <- list(id = nid, color = col)
        }
      }

      list(items = items, reset_ids = character(0))
    }

    observeEvent(input$apply_diff_borders, {
      id <- rv_selected()
      req(id)
      el <- rv_nets$map[[id]]$elements_work
      req(el)

      gA <- trimws(as.character(input$diff_group_a %||% ""))
      gB <- trimws(as.character(input$diff_group_b %||% ""))
      req(nzchar(gA), nzchar(gB))
      if (identical(gA, gB)) {
        showNotification("Group A and Group B must be different.", type = "error")
        return()
      }

      method <- tolower(as.character(input$diff_method %||% "wilcox"))[1]

      alpha <- suppressWarnings(as.numeric(input$diff_alpha %||% 0.05))
      if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

      use_padj <- isTRUE(input$diff_use_padj)

      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)
      se_tx <- if (!is.null(se_tx_reactive)) tryCatch(se_tx_reactive(), error = function(e) NULL) else NULL

      if (!is.null(se_lipid)) {
        cd <- as.data.frame(colData(se_lipid))
        if (!DIFF_GROUP_COL %in% names(cd)) {
          showNotification(paste0("Lipid SE: colData has no column '", DIFF_GROUP_COL, "'. Lipid nodes will be skipped."), type = "warning")
          se_lipid <- NULL
        }
      }
      if (!is.null(se_tx)) {
        cd <- as.data.frame(colData(se_tx))
        if (!DIFF_GROUP_COL %in% names(cd)) {
          showNotification(paste0("Gene SE: colData has no column '", DIFF_GROUP_COL, "'. Gene nodes will be skipped."), type = "warning")
          se_tx <- NULL
        }
      }

      if (is.null(se_lipid) && is.null(se_tx)) {
        showNotification(paste0("No applicable SE found (missing colData$", DIFF_GROUP_COL, ")."), type = "error")
        return()
      }

      stats_map <- tryCatch(
        .compute_diff_stats(el, se_lipid, se_tx, gA, gB, method = method),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(stats_map) || !length(stats_map)) {
        showNotification("No testable nodes found in this network for the selected groups.", type = "warning")
        return()
      }

      payload <- .border_items_from_stats(el, stats_map, alpha = alpha, use_padj = use_padj)
      rv_diff_border$map[[id]] <- payload

      session$sendCustomMessage("cy_set_node_borders", c(list(container_id = container_id), payload))
    }, ignoreInit = TRUE)

    observeEvent(input$clear_diff_borders, {
      id <- rv_selected()
      req(id)
      el <- rv_nets$map[[id]]$elements_work
      req(el)

      reset_ids <- unique(vapply(el$nodes, function(n) as.character((n$data %||% list())$id %||% ""), character(1)))
      reset_ids <- reset_ids[nzchar(reset_ids)]

      payload <- list(items = list(), reset_ids = reset_ids)
      rv_diff_border$map[[id]] <- NULL

      session$sendCustomMessage("cy_set_node_borders", c(list(container_id = container_id), payload))
    }, ignoreInit = TRUE)

    # ---- Enrichment (CSV -> molecules -> lipid class -> class nodes) ----------
    rv_enrich_raw <- reactiveVal(NULL)

    .read_enrich_csv <- function(path){
      df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      need <- c("ID","Description","p.adjust","type","lipidID")
      miss <- setdiff(need, names(df))
      if (length(miss)) stop("Enrichment CSV is missing columns: ", paste(miss, collapse=", "), call. = FALSE)
      df
    }

    .split_lipid_ids <- function(x){
      if (is.null(x) || !nzchar(trimws(x))) return(character(0))
      v <- unlist(strsplit(x, "/", fixed = TRUE), use.names = FALSE)
      v <- trimws(v)
      v[nzchar(v)]
    }

    .guess_class_from_lipid_string <- function(lip){
      s <- trimws(as.character(lip))
      if (!nzchar(s)) return(NA_character_)
      m <- regexpr("[0-9]", s)
      if (m[1] > 1) {
        out <- trimws(substr(s, 1, m[1] - 1))
        out <- sub("\\s+$", "", out)
        return(out)
      }
      sp <- regexpr("\\s", s)
      if (sp[1] > 1) return(substr(s, 1, sp[1] - 1))
      NA_character_
    }

    .map_lipid_to_class <- function(lip_vec, se_lipid){
      lip_vec <- unique(trimws(lip_vec))
      lip_vec <- lip_vec[nzchar(lip_vec)]
      if (!length(lip_vec)) return(setNames(character(0), character(0)))

      out <- setNames(rep(NA_character_, length(lip_vec)), lip_vec)

      if (!is.null(se_lipid)) {
        class_col <- resolve_class_col(se_lipid)
        rd <- as.data.frame(rowData(se_lipid))
        rn <- rownames(se_lipid)

        hit <- intersect(lip_vec, rn)
        if (length(hit)) {
          cls <- as.character(rd[match(hit, rn), class_col])
          cls <- trimws(cls)
          out[hit] <- cls
        }
      }

      miss <- names(out)[!nzchar(out)]
      if (length(miss)) {
        out[miss] <- vapply(miss, .guess_class_from_lipid_string, character(1))
      }

      out <- out[nzchar(out)]
      out
    }

    .build_enrich_html_for_class <- function(class_name, rows){
      esc <- function(x) htmltools::htmlEscape(as.character(x))

      parts <- c(
        sprintf("<div class='cy-enrich'>"),
        sprintf("<div style='font-weight:700; margin-bottom:6px;'>%s</div>", esc(class_name))
      )

      pa <- suppressWarnings(as.numeric(rows[["p.adjust"]]))
      if (all(is.finite(pa))) rows <- rows[order(pa, decreasing = FALSE), , drop = FALSE]

      for (i in seq_len(nrow(rows))) {
        term_id <- rows$ID[i]
        desc    <- rows$Description[i]
        padj    <- rows[["p.adjust"]][i]
        mols    <- rows$mols_in_class[[i]]
        nmols   <- length(mols)

        show_mols <- mols[seq_len(min(nmols, 80))]

        parts <- c(parts,
                   "<hr/>",
                   sprintf("<div><b>%s</b> &nbsp; <span class='muted'>%s</span></div>", esc(term_id), esc(desc)),
                   sprintf("<div class='muted'>p.adjust: %s &nbsp; (#molecules in this class: %d)</div>", esc(padj), nmols),
                   sprintf("<div class='mono' style='margin-top:6px; white-space:normal; word-break:break-word;'>%s</div>",
                           esc(paste(show_mols, collapse = ", ")))
        )
      }

      parts <- c(parts, "</div>")
      paste0(parts, collapse = "\n")
    }

    observeEvent(input$enrich_csv, {
      req(input$enrich_csv$datapath)
      df <- .read_enrich_csv(input$enrich_csv$datapath)
      rv_enrich_raw(df)
      showNotification(paste0("Enrichment CSV loaded: ", nrow(df), " rows"), type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$apply_enrich, {
      id <- rv_selected()
      req(id)
      el <- rv_nets$map[[id]]$elements_work
      req(el)

      df <- rv_enrich_raw()
      if (is.null(df) || !nrow(df)) {
        showNotification("No enrichment CSV loaded.", type = "error")
        return()
      }

      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)

      node_by_class <- list()
      for (n in el$nodes) {
        d <- n$data %||% list()
        nid <- as.character(d$id %||% "")
        cls <- trimws(as.character(d$shared_name %||% ""))
        if (nzchar(nid) && nzchar(cls)) node_by_class[[cls]] <- nid
      }

      if (!length(node_by_class)) {
        showNotification("No class nodes found (data$shared_name is empty).", type = "error")
        return()
      }

      df$.__mols <- lapply(df$lipidID, .split_lipid_ids)
      all_mols <- unique(unlist(df$.__mols, use.names = FALSE))
      mol_to_class <- .map_lipid_to_class(all_mols, se_lipid)

      out_list <- list()
      for (i in seq_len(nrow(df))) {
        mols <- df$.__mols[[i]]
        if (!length(mols)) next

        cls_vec <- unname(mol_to_class[mols])
        ok <- !is.na(cls_vec) & nzchar(cls_vec)
        mols <- mols[ok]; cls_vec <- cls_vec[ok]
        if (!length(mols)) next

        split_m <- split(mols, cls_vec)
        for (cls in names(split_m)) {
          if (is.null(node_by_class[[cls]])) next

          row <- df[i, intersect(c("ID","Description","p.adjust","type","Count","qvalue","pvalue"), names(df)), drop = FALSE]
          row$mols_in_class <- list(split_m[[cls]])

          if (is.null(out_list[[cls]])) out_list[[cls]] <- row else out_list[[cls]] <- rbind(out_list[[cls]], row)
        }
      }

      all_node_ids <- unique(vapply(el$nodes, function(n) as.character((n$data %||% list())$id %||% ""), character(1)))
      all_node_ids <- all_node_ids[nzchar(all_node_ids)]

      items_meta <- list()
      hi_ids <- character(0)

      for (cls in names(out_list)) {
        node_id <- node_by_class[[cls]]
        html <- .build_enrich_html_for_class(cls, out_list[[cls]])
        items_meta[[length(items_meta) + 1L]] <- list(id = node_id, enrich_html = html)
        hi_ids <- c(hi_ids, node_id)
      }
      hi_ids <- unique(hi_ids)
      reset_ids <- setdiff(all_node_ids, hi_ids)

      payload_meta <- list(items = items_meta, reset_ids = reset_ids)
      payload_borderstyle <- list(items = lapply(hi_ids, function(x) list(id = x, borderWidthPx = 6)),
                                  reset_ids = reset_ids)

      rv_enrich_state$map[[id]] <- list(meta = payload_meta, borderstyle = payload_borderstyle)

      session$sendCustomMessage("cy_set_node_meta", c(list(container_id = container_id), payload_meta))
      session$sendCustomMessage("cy_set_node_borderstyle", c(list(container_id = container_id), payload_borderstyle))

      showNotification(paste0("Enrichment applied to ", length(hi_ids), " class node(s)."), type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$clear_enrich, {
      id <- rv_selected()
      req(id)
      el <- rv_nets$map[[id]]$elements_work
      req(el)

      all_node_ids <- unique(vapply(el$nodes, function(n) as.character((n$data %||% list())$id %||% ""), character(1)))
      all_node_ids <- all_node_ids[nzchar(all_node_ids)]

      session$sendCustomMessage("cy_set_node_meta", list(
        container_id = container_id,
        items = list(),
        reset_ids = all_node_ids
      ))
      session$sendCustomMessage("cy_set_node_borderstyle", list(
        container_id = container_id,
        items = list(),
        reset_ids = all_node_ids
      ))

      rv_enrich_state$map[[id]] <- NULL
      showNotification("Enrichment cleared.", type = "message")
    }, ignoreInit = TRUE)

    # ---- render selected network ---------------------------------------------
    observe({
      id <- rv_selected()
      req(id)

      el <- rv_nets$map[[id]]$elements_work
      req(el)

      use_preset <- .any_node_has_pos(el)
      if (use_preset) el <- .ensure_positions_for_preset(el)
      lay <- if (use_preset) "preset" else default_layout

      session$sendCustomMessage(
        "cy_init",
        list(
          container_id      = container_id,
          elements          = el,
          layout            = lay,
          style             = NULL,
          wheelSensitivity  = 0.2,
          selected_input    = ns("node_selected"),
          changed_input     = ns("cy_changed"),
          allow_delete      = TRUE
        )
      )

      .send_node_images_from_elements(el, thin_bw = 1L, clear_all_first = FALSE, show_loading = FALSE)

      payload <- rv_diff_border$map[[id]]
      if (!is.null(payload) && is.list(payload)) {
        session$sendCustomMessage("cy_set_node_borders", c(list(container_id = container_id), payload))
      }

      est <- rv_enrich_state$map[[id]]
      if (!is.null(est) && is.list(est)) {
        if (!is.null(est$meta)) {
          session$sendCustomMessage("cy_set_node_meta", c(list(container_id = container_id), est$meta))
        }
        if (!is.null(est$borderstyle)) {
          session$sendCustomMessage("cy_set_node_borderstyle", c(list(container_id = container_id), est$borderstyle))
        }
      }

      session$sendCustomMessage("cy_resize", list(container_id = container_id, fit = TRUE))
    })

    observeEvent(input$fit, {
      session$sendCustomMessage("cy_fit", list(container_id = container_id))
    }, ignoreInit = TRUE)

    observeEvent(input$node_selected, {
      selected_node(input$node_selected)
    }, ignoreInit = TRUE)

    # ---- Export ---------------------------------------------------------------
    .ts_name <- function(prefix = "network", ext = "pdf") {
      paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    }

    observeEvent(input$export_pdf, {
      session$sendCustomMessage("cy_export_pdf", list(
        container_id = container_id,
        filename     = .ts_name("network", "pdf"),
        scale        = 2,
        bg           = "white",
        format       = "a4",
        orientation  = "l"
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$export_pdf_popups, {
      session$sendCustomMessage("cy_export_pdf_with_popups", list(
        container_id = container_id,
        filename     = .ts_name("network_with_popups", "pdf"),
        scale        = 2
      ))
    }, ignoreInit = TRUE)

    # ---- Acyl-chain filter UI ------------------------------------------------
    has_chain_info <- reactive({
      se <- tryCatch(se_lipid_reactive(), error = function(e) NULL)
      .has_chain_col(se, "acyl_chains")
    })

    output$chain_filter_ui <- renderUI({
      if (!isTRUE(has_chain_info())) {
        return(shiny::helpText("Acyl-chain filter: unavailable (rowData$acyl_chains is missing)."))
      }

      se <- se_lipid_reactive()
      codes <- .get_unique_chain_codes(se, "acyl_chains")

      selected0 <- isolate(input$chain_codes)
      if (is.null(selected0)) selected0 <- character(0)
      selected0 <- intersect(as.character(selected0), codes)

      shiny::tagList(
        shiny::checkboxInput(ns("chain_exclude_odd"), "Exclude molecules with any odd-carbon chain", value = FALSE),
        shiny::selectizeInput(
          ns("chain_codes"),
          "Chain code(s) (multiple)",
          choices  = codes,
          selected = selected0,
          multiple = TRUE,
          options  = list(
            plugins     = list("remove_button"),
            placeholder = "Select one or more (e.g., 18:0, 22:6;O2)",
            maxOptions  = 5000
          )
        ),
        shiny::checkboxInput(ns("chain_require_all"), "Require ALL selected chain codes", value = FALSE),
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(ns("chain_carbon"), "C",  value = "")),
          shiny::column(4, shiny::textInput(ns("chain_db"),     "DB", value = "")),
          shiny::column(4, shiny::textInput(ns("chain_oxygen"), "O",  value = ""))
        ),
        shiny::actionButton(ns("chain_reset"), "Reset", class = "btn-xs")
      )
    })

    observeEvent(input$chain_reset, {
      updateCheckboxInput(session, "chain_exclude_odd", value = FALSE)
      updateSelectizeInput(session, "chain_codes", selected = character(0), server = TRUE)
      updateCheckboxInput(session, "chain_require_all", value = FALSE)
      updateTextInput(session, "chain_carbon", value = "")
      updateTextInput(session, "chain_db", value = "")
      updateTextInput(session, "chain_oxygen", value = "")
    }, ignoreInit = TRUE)

    se_lipid_effective <- reactive({
      se <- se_lipid_reactive()
      req(se)

      if (!isTRUE(.has_chain_col(se, "acyl_chains"))) return(se)

      chain_codes <- input$chain_codes %||% character(0)
      require_all <- isTRUE(input$chain_require_all)
      exclude_odd <- isTRUE(input$chain_exclude_odd)

      carbon <- suppressWarnings(as.integer(trimws(input$chain_carbon %||% "")))
      db     <- suppressWarnings(as.integer(trimws(input$chain_db %||% "")))
      oxygen <- suppressWarnings(as.integer(trimws(input$chain_oxygen %||% "")))

      if (!is.finite(carbon)) carbon <- NULL
      if (!is.finite(db))     db     <- NULL
      if (!is.finite(oxygen)) oxygen <- NULL

      any_filter_on <- exclude_odd ||
        (length(chain_codes) > 0) ||
        (!is.null(carbon) || !is.null(db) || !is.null(oxygen))

      if (!any_filter_on) return(se)

      .filter_lipid_se_by_chains(
        se,
        chain_codes = chain_codes,
        require_all = require_all,
        exclude_odd = exclude_odd,
        carbon = carbon, db = db, oxygen = oxygen,
        chain_col = "acyl_chains"
      )
    })

    # ---- Map local SVGs (lipid + gene + heatmap) ------------------------------
    observeEvent(input$map_local_svgs, {
      id <- rv_selected()
      req(id)

      info <- rv_nets$map[[id]]
      req(info)

      se_lipid <- se_lipid_effective()
      req(se_lipid)

      se_tx <- if (!is.null(se_tx_reactive)) se_tx_reactive() else NULL

      svg_dir <- .get_svg_dir()

      session$sendCustomMessage("cy_loading", list(container_id = container_id, on = TRUE))
      on.exit(session$sendCustomMessage("cy_loading", list(container_id = container_id, on = FALSE)), add = TRUE)

      el <- info$elements_work
      ts_now <- as.integer(as.numeric(Sys.time()))

      classes_in_net <- unique(vapply(el$nodes, function(n) {
        d <- n$data %||% list()
        as.character(d$shared_name %||% "")
      }, FUN.VALUE = character(1)))
      classes_in_net <- classes_in_net[nzchar(classes_in_net)]

      ens_in_net <- unique(vapply(el$nodes, function(n) {
        d <- n$data %||% list()
        e <- as.character(d$Ensembl_ID %||% "")
        e <- trimws(e)
        e <- sub("\\..*$", "", e)
        e
      }, FUN.VALUE = character(1)))
      ens_in_net <- ens_in_net[nzchar(ens_in_net)]

      url_map_lipid_class <- list()
      url_map_lipid_hm    <- list()

      valid_lipid  <- get_valid_classes_from_se(se_lipid)
      target_lipid <- intersect(classes_in_net, valid_lipid)

      if (length(target_lipid)) {
        plot_type <- tolower(as.character(input$plot_type %||% "dot"))[1]
        agg_fun   <- tolower(as.character(input$agg_fun   %||% "sum"))[1]

        adv <- .get_adv_or_default(se_lipid, settings_reactive = settings_reactive)
        x_order <- adv$manual_order %||% character(0)
        pal     <- if (length(adv$palette_map)) adv$palette_map else NULL
        extra_p     <- .pvalue_args_from_adv(adv)
        extra_style <- .plot_style_args_from_adv(adv, plot_type)

        se_cls <- aggregate_to_class_se(se_lipid, fun = agg_fun)
        fun    <- get_plot_fun(plot_type)
        sz     <- .get_svg_size("class")

        files_map_class <- list()
        for (cls in target_lipid) {
          safe <- .safe_id(cls)
          f    <- file.path(svg_dir, paste0("class_", safe, ".svg"))

          p <- do.call(fun, c(list(
            se = se_cls, feature_id = cls, x_var = "class",
            x_order = if (length(x_order)) x_order else NULL,
            palette = pal
          ), extra_style, extra_p))

          p <- .apply_palette_if_missing(p, pal)
          p <- p + ggplot2::theme(legend.position = "none", aspect.ratio = 1)
          p <- .prep_for_node_svg(p)

          .save_svg_file(p, f, width = sz[["w"]], height = sz[["h"]])
          files_map_class[[cls]] <- f
        }

        for (cls in names(files_map_class)) {
          fname <- basename(files_map_class[[cls]])
          url_map_lipid_class[[cls]] <- .img_url(fname, session, ts = ts_now)  # ★ /static 付き
        }

        top_n <- as.integer(input$hm_top_n %||% 40)
        if (!is.finite(top_n) || top_n <= 0) top_n <- 40L

        class_col <- resolve_class_col(se_lipid)
        sz_hm <- .get_svg_size("hm")

        files_map_hm <- list()
        for (cls in target_lipid) {
          safe <- .safe_id(cls)
          f    <- file.path(svg_dir, paste0("hm_class_", safe, ".svg"))

          p <- .make_heatmap_for_class(
            se_lipid,
            class_col  = class_col,
            class_name = cls,
            topN       = top_n,
            x_order    = if (length(x_order)) x_order else NULL
          )
          p <- .prep_for_node_svg(p)

          .save_svg_file(p, f, width = sz_hm[["w"]], height = sz_hm[["h"]])
          files_map_hm[[cls]] <- f
        }

        for (cls in names(files_map_hm)) {
          fname <- basename(files_map_hm[[cls]])
          url_map_lipid_hm[[cls]] <- .img_url(fname, session, ts = ts_now)     # ★ /static 付き
        }
      }

      url_map_gene <- list()
      if (!is.null(se_tx) && length(ens_in_net)) {
        ens_map <- .make_ensembl_to_rowname_map(se_tx)
        target_ens <- intersect(ens_in_net, names(ens_map))

        if (length(target_ens)) {
          plot_type <- tolower(as.character(input$plot_type %||% "dot"))[1]
          fun <- get_plot_fun(plot_type)

          adv_tx <- .get_adv_or_default(se_tx, settings_reactive = settings_reactive)
          x_order <- adv_tx$manual_order %||% character(0)
          pal     <- if (length(adv_tx$palette_map)) adv_tx$palette_map else NULL
          extra_p     <- .pvalue_args_from_adv(adv_tx)
          extra_style <- .plot_style_args_from_adv(adv_tx, plot_type)

          sz_gene <- .get_svg_size("gene")

          out_files <- list()
          for (ens_id in target_ens) {
            fid <- unname(ens_map[[ens_id]])
            safe <- .safe_id(fid)
            f <- file.path(svg_dir, paste0("gene_", safe, ".svg"))

            p <- do.call(fun, c(list(
              se = se_tx, feature_id = fid, x_var = "class",
              x_order = if (length(x_order)) x_order else NULL,
              palette = pal
            ), extra_style, extra_p))

            p <- .apply_palette_if_missing(p, pal)
            p <- p + ggplot2::theme(legend.position = "none", aspect.ratio = 1)
            p <- .prep_for_node_svg(p)

            .save_svg_file(p, f, width = sz_gene[["w"]], height = sz_gene[["h"]])
            out_files[[ens_id]] <- f
          }

          for (ens_id in names(out_files)) {
            fname <- basename(out_files[[ens_id]])
            url_map_gene[[ens_id]] <- .img_url(fname, session, ts = ts_now)     # ★ /static 付き
          }
        }
      }

      thick_bw <- 4L
      thin_bw  <- 1L

      el$nodes <- lapply(el$nodes, function(n) {
        d <- n$data %||% list()

        cls_key <- trimws(as.character(d$shared_name %||% ""))
        ens_key <- trimws(as.character(d$Ensembl_ID %||% ""))
        ens_key <- sub("\\..*$", "", ens_key)

        if (nzchar(ens_key) && !is.null(url_map_gene[[ens_key]])) {
          d$path        <- url_map_gene[[ens_key]]
          d$BorderWidth <- thick_bw
        } else if (nzchar(cls_key) && !is.null(url_map_lipid_class[[cls_key]])) {
          d$path        <- url_map_lipid_class[[cls_key]]
          d$BorderWidth <- thick_bw
        } else {
          d$BorderWidth <- d$BorderWidth %||% thin_bw
        }

        if (nzchar(cls_key) && !is.null(url_map_lipid_hm[[cls_key]])) {
          d$heatmap_path <- url_map_lipid_hm[[cls_key]]
        }

        out <- list(data = d)
        if (!is.null(n$position)) out$position <- n$position
        out
      })

      rv_nets$map[[id]]$elements_work <- el
      .send_node_images_from_elements(el, thin_bw = thin_bw, clear_all_first = TRUE, show_loading = TRUE)
    }, ignoreInit = TRUE)

    # ---- Plot panel logic -----------------------------------------------------
    .empty_plot <- function(msg = "No node selected / not applicable.") {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = msg) +
        ggplot2::theme_void()
    }

    .get_ens_key <- function(node) {
      if (is.null(node)) return("")
      ens <- node$Ensembl_ID %||% ""
      ens <- trimws(as.character(ens))
      sub("\\..*$", "", ens)
    }

    .get_gene_label <- function(node) {
      if (is.null(node)) return("")
      lab <- trimws(as.character(node$label %||% ""))
      if (!nzchar(lab)) lab <- .get_ens_key(node)
      lab
    }

    .is_gene_node <- function(node) nzchar(.get_ens_key(node))

    .get_class_key <- function(node) {
      if (is.null(node)) return("")
      trimws(as.character(node$shared_name %||% ""))
    }

    .is_valid_lipid_class_node <- function(node, se_lipid) {
      cls <- .get_class_key(node)
      if (!nzchar(cls)) return(FALSE)
      cls %in% get_valid_classes_from_se(se_lipid)
    }

    output$molecule_ui <- renderUI({
      node <- selected_node()
      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)

      if (!is.null(node) && !is.null(se_lipid) && .is_valid_lipid_class_node(node, se_lipid)) {
        shiny::selectizeInput(
          ns("molecule_select"),
          label = NULL,
          choices = character(0),
          selected = NULL,
          multiple = FALSE,
          options = list(placeholder = "Select a molecule...", maxOptions = 5000)
        )
      } else {
        shiny::tags$div(style = "height: 34px;")
      }
    })

    output$plot_title_left <- renderUI({
      node <- selected_node()
      if (is.null(node)) return(shiny::tags$h5("Primary plot"))
      if (.is_gene_node(node)) return(shiny::tags$h5(paste0("Gene plot (", .get_gene_label(node), ")")))

      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)
      if (!is.null(se_lipid) && .is_valid_lipid_class_node(node, se_lipid)) {
        cls <- .get_class_key(node)
        return(shiny::tags$h5(paste0("Lipid class plot (", cls, ")")))
      }
      shiny::tags$h5("Primary plot")
    })

    output$plot_title_right <- renderUI({
      node <- selected_node()
      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)
      if (!is.null(node) && !is.null(se_lipid) && .is_valid_lipid_class_node(node, se_lipid)) {
        shiny::tags$h5("Molecule plot (user selected)")
      } else {
        shiny::tags$h5("Secondary plot (N/A)")
      }
    })

    observeEvent(list(selected_node(), se_lipid_effective()), {
      node <- selected_node()
      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)

      if (is.null(node) || is.null(se_lipid) || !.is_valid_lipid_class_node(node, se_lipid)) {
        if (!is.null(input$molecule_select)) {
          updateSelectizeInput(session, "molecule_select",
                               choices = character(0), selected = NULL, server = TRUE
          )
        }
        return()
      }

      cls <- .get_class_key(node)
      class_col <- resolve_class_col(se_lipid)
      rd <- as.data.frame(rowData(se_lipid))
      mat <- as.matrix(assay(se_lipid, 1))

      ix <- which(as.character(rd[[class_col]]) == cls)
      if (!length(ix)) {
        updateSelectizeInput(session, "molecule_select",
                             choices = character(0), selected = NULL, server = TRUE
        )
        return()
      }

      ids <- rownames(mat)[ix]
      mu  <- rowMeans(mat[ix, , drop = FALSE], na.rm = TRUE)
      ord <- order(mu, decreasing = TRUE)
      ids <- ids[ord]

      if (length(ids) > 500) ids <- ids[seq_len(500)]
      updateSelectizeInput(session, "molecule_select", choices = ids, selected = ids[[1]], server = TRUE)
    }, ignoreInit = TRUE)

    left_plot_obj <- reactive({
      node <- selected_node()
      if (is.null(node)) return(.empty_plot("Select a node in the network."))

      plot_type <- tolower(as.character(input$plot_type %||% "dot"))[1]
      fun <- get_plot_fun(plot_type)

      if (.is_gene_node(node)) {
        se_tx <- if (!is.null(se_tx_reactive)) se_tx_reactive() else NULL
        if (is.null(se_tx)) return(.empty_plot("Gene SE is not available."))

        ens <- .get_ens_key(node)
        ens_map <- .make_ensembl_to_rowname_map(se_tx)
        if (!ens %in% names(ens_map)) return(.empty_plot(paste0("Ensembl_ID not found in se_tx: ", ens)))
        fid <- unname(ens_map[[ens]])

        adv <- .get_adv_or_default(se_tx, settings_reactive = settings_reactive)
        x_order <- adv$manual_order %||% character(0)
        pal     <- if (length(adv$palette_map)) adv$palette_map else NULL
        extra_style <- .plot_style_args_from_adv(adv, plot_type)
        extra_p     <- .pvalue_args_from_adv(adv)

        p <- do.call(fun, c(list(
          se = se_tx, feature_id = fid, x_var = "class",
          x_order = if (length(x_order)) x_order else NULL,
          palette = pal
        ), extra_style, extra_p))

        p <- .apply_palette_if_missing(p, pal)
        p + ggplot2::labs(title = paste0("Gene: ", .get_gene_label(node))) +
          ggplot2::theme(legend.position = "none", aspect.ratio = 1)

      } else {
        se_lipid <- se_lipid_effective()
        if (!.is_valid_lipid_class_node(node, se_lipid)) {
          return(.empty_plot("Unsupported node type (not a valid lipid class, and no Ensembl ID)."))
        }

        cls <- .get_class_key(node)
        agg_fun <- tolower(as.character(input$agg_fun %||% "sum"))[1]
        se_cls <- aggregate_to_class_se(se_lipid, fun = agg_fun)

        adv <- .get_adv_or_default(se_lipid, settings_reactive = settings_reactive)
        x_order <- adv$manual_order %||% character(0)
        pal     <- if (length(adv$palette_map)) adv$palette_map else NULL
        extra_style <- .plot_style_args_from_adv(adv, plot_type)
        extra_p     <- .pvalue_args_from_adv(adv)

        p <- do.call(fun, c(list(
          se = se_cls, feature_id = cls, x_var = "class",
          x_order = if (length(x_order)) x_order else NULL,
          palette = pal
        ), extra_style, extra_p))

        p <- .apply_palette_if_missing(p, pal)
        p + ggplot2::labs(title = paste0("Class: ", cls)) +
          ggplot2::theme(legend.position = "none", aspect.ratio = 1)
      }
    })

    right_plot_obj <- reactive({
      node <- selected_node()
      if (is.null(node)) return(.empty_plot("Select a node in the network."))

      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)
      if (is.null(se_lipid) || !.is_valid_lipid_class_node(node, se_lipid)) {
        return(.empty_plot("N/A for this node type."))
      }

      cls <- .get_class_key(node)
      class_col <- resolve_class_col(se_lipid)
      rd <- as.data.frame(rowData(se_lipid))
      mat <- as.matrix(assay(se_lipid, 1))

      ix <- which(as.character(rd[[class_col]]) == cls)
      if (!length(ix)) return(.empty_plot(paste("No molecules found in class:", cls)))

      fid <- trimws(as.character(input$molecule_select %||% ""))
      if (!nzchar(fid) || !(fid %in% rownames(mat)[ix])) {
        mu <- rowMeans(mat[ix, , drop = FALSE], na.rm = TRUE)
        fid <- rownames(mat)[ix][which.max(mu)]
      }

      plot_type <- tolower(as.character(input$plot_type %||% "dot"))[1]
      fun <- get_plot_fun(plot_type)

      adv <- .get_adv_or_default(se_lipid, settings_reactive = settings_reactive)
      x_order <- adv$manual_order %||% character(0)
      pal     <- if (length(adv$palette_map)) adv$palette_map else NULL
      extra_style <- .plot_style_args_from_adv(adv, plot_type)
      extra_p     <- .pvalue_args_from_adv(adv)

      p <- do.call(fun, c(list(
        se = se_lipid, feature_id = fid, x_var = "class",
        x_order = if (length(x_order)) x_order else NULL,
        palette = pal
      ), extra_style, extra_p))

      p <- .apply_palette_if_missing(p, pal)
      p + ggplot2::labs(title = paste0("Molecule in ", cls, ": ", fid)) +
        ggplot2::theme(legend.position = "none", aspect.ratio = 1)
    })

    output$plot_left  <- renderPlot({ left_plot_obj() })
    output$plot_right <- renderPlot({ right_plot_obj() })

    .safe_filename <- function(x) {
      x <- gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x))
      x <- substr(x, 1, 80)
      if (!nzchar(x)) x <- "plot"
      x
    }

    output$dl_left <- downloadHandler(
      filename = function() {
        node <- selected_node()
        if (is.null(node)) return("plot_left.png")
        if (.is_gene_node(node)) return(paste0("gene_", .safe_filename(.get_gene_label(node)), ".png"))

        se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)
        if (!is.null(se_lipid) && .is_valid_lipid_class_node(node, se_lipid)) {
          return(paste0("lipid_class_", .safe_filename(.get_class_key(node)), ".png"))
        }
        "plot_left.png"
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = left_plot_obj(),
                        width = 6, height = 6, units = "in", dpi = 200, bg = "white"
        )
      }
    )

    output$dl_right <- downloadHandler(
      filename = function() {
        node <- selected_node()
        se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)
        if (!is.null(node) && !is.null(se_lipid) && .is_valid_lipid_class_node(node, se_lipid)) {
          fid <- trimws(as.character(input$molecule_select %||% "molecule"))
          return(paste0("molecule_", .safe_filename(fid), ".png"))
        }
        "plot_right.png"
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = right_plot_obj(),
                        width = 6, height = 6, units = "in", dpi = 200, bg = "white"
        )
      }
    )
  })
}
