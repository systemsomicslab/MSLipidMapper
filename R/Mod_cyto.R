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

# Per-species effects for the movable pathway FC panels. No class aggregation:
# use the same (possibly acyl-chain filtered) assay as the existing node plots.
.mslm_fc_panel_data <- function(se, class_col, group_col, group_a, group_b,
                                scale = "abundance", top_n = 5L) {
  if (identical(group_a, group_b)) stop("Choose two different groups.")
  cd <- as.data.frame(SummarizedExperiment::colData(se))
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!group_col %in% names(cd)) stop("Group column is missing.")
  if (!class_col %in% names(rd)) stop("Lipid class column is missing.")
  groups <- trimws(as.character(cd[[group_col]]))
  ia <- which(groups == group_a); ib <- which(groups == group_b)
  if (length(ia) < 2L || length(ib) < 2L) stop("FC panels need at least two samples per group.")
  mat <- as.matrix(SummarizedExperiment::assay(se, 1))
  ids <- rownames(mat)
  if (is.null(ids)) ids <- paste0("Feature ", seq_len(nrow(mat)))
  lfc <- vapply(seq_len(nrow(mat)), function(i) {
    a <- as.numeric(mat[i, ia]); b <- as.numeric(mat[i, ib])
    a <- a[is.finite(a)]; b <- b[is.finite(b)]
    if (length(a) < 2L || length(b) < 2L) return(NA_real_)
    if (identical(scale, "log2")) return(mean(b) - mean(a))
    if (any(a < 0) || any(b < 0)) return(NA_real_)
    log2(mean(b) + 1e-8) - log2(mean(a) + 1e-8)
  }, numeric(1))
  classes <- trimws(as.character(rd[[class_col]]))
  keys <- unique(classes[!is.na(classes) & nzchar(classes)])
  top_n <- max(1L, min(30L, as.integer(top_n)))
  out <- lapply(keys, function(key) {
    all <- which(!is.na(classes) & classes == key)
    valid <- all[is.finite(lfc[all])]
    up <- valid[lfc[valid] > 0]; down <- valid[lfc[valid] < 0]
    pick <- c(head(up[order(lfc[up], decreasing = TRUE)], top_n),
              head(down[order(lfc[down], decreasing = TRUE)], top_n))
    list(class = key, total = length(all), valid = length(valid),
         excluded = length(all) - length(valid),
         rows = lapply(pick, function(i) list(name = ids[[i]], fc = unname(lfc[[i]]))))
  })
  # A shared symmetric domain across ALL valid species, including hidden rows.
  values <- abs(lfc[is.finite(lfc)])
  list(classes = out, domain = if (length(values)) max(1, ceiling(max(values))) else 1)
}

.mslm_clear_projection_elements <- function(elements) {
  elements$nodes <- lapply(elements$nodes, function(node) {
    for (key in c("path", "heatmap_path", "hm_path", "pathpng", "enrich_html")) node$data[[key]] <- ""
    node
  })
  elements
}

mod_cyto_ui <- function(id) {
  ns <- shiny::NS(id)
  tabs <- c(display = "Display", compare = "Comparison / Panels", filter = "Filters", manage = "Manage")
  pane <- function(key, ...) shiny::tags$section(
    class = "mslm-settings-pane", `data-pane` = key,
    hidden = if (key != "display") NA else NULL, ...)
  settings_button <- function(label, key, class = "btn btn-default")
    shiny::tags$button(type = "button", class = class, `data-settings-open` = key, label)
  shiny::tagList(
    cyto_js_deps(container_id_css = ns("cy")),
    shiny::div(id = ns("workspace"), class = "mslm-pathway-workspace", `data-ns` = ns(""),
      shiny::div(class = "mslm-pathway-header",
        shiny::div(class = "mslm-pathway-controls",
      shiny::div(class = "mslm-pathway-toolbar",
        shiny::uiOutput(ns("current_pathway"))),
      shiny::uiOutput(ns("settings_summary")),
      shiny::div(class = "mslm-pathway-context",
        shiny::checkboxInput(ns("fc_enabled"), "Popup panels", value = FALSE),
        shiny::checkboxInput(ns("diff_borders_enabled"), "Highlight significant nodes", value = FALSE),
        shiny::conditionalPanel(condition = "input.diff_borders_enabled", ns = ns,
          shiny::uiOutput(ns("diff_legend"), inline = TRUE)),
        shiny::div(class = "mslm-toolbar-spacer"),
        shiny::actionButton(ns("map_local_svgs"), "Pathway Mapping", class = "btn-primary btn-sm"),
        shiny::tags$details(class = "mslm-pathway-save",
          shiny::tags$summary("Save"),
          shiny::div(class = "mslm-save-menu",
            shiny::actionButton(ns("fc_svg"), "Figure SVG (popup panels)"),
            shiny::actionButton(ns("export_pdf"), "Network PDF"),
            shiny::actionButton(ns("export_pdf_popups"), "PDF with panels / popups"))),
        shiny::actionButton(ns("fit"), "Fit network", class = "btn-sm"),
        shiny::conditionalPanel(condition = "input.fc_enabled", ns = ns,
          shiny::div(class = "mslm-context-actions",
            shiny::actionButton(ns("fc_arrange"), "Arrange panels", class = "btn-sm"),
            shiny::actionButton(ns("fc_clear"), "Close all panels", class = "btn-sm"))),
        settings_button("Panel settings", "compare", "btn btn-default btn-sm")))),
      shiny::div(id = ns("cy"), class = "mslm-pathway-canvas",
        style = "width:100%;height:720px;"),
      shiny::tags$p(class = "mslm-pathway-hint", "Enable Popup panels, then click a node. Choose panel content in Panel settings; drag headers to move and corners to resize. Ctrl + right-click a node or edge to delete it."),
      shiny::div(class = "mslm-selected-node",
        shiny::tags$h4("Selected node"),
        shiny::tags$style(shiny::HTML("
          .mslm-selected-node-plots {
            display:grid; grid-template-columns:repeat(2,minmax(0,1fr));
            grid-template-rows:auto auto auto; gap:12px 16px;
          }
          .mslm-selected-node-plots > .plot-card {
            display:grid; grid-template-rows:subgrid; grid-row:span 3;
            min-width:0; align-items:start;
          }
          .mslm-selected-node-plots > .plot-card > .btn { justify-self:start; }
          .mslm-selected-node-plots .mslm-node-plot-header .form-group { margin-bottom:0; }
          @media (max-width:767px) {
            .mslm-selected-node-plots { grid-template-columns:1fr; grid-template-rows:none; }
            .mslm-selected-node-plots > .plot-card { grid-template-rows:auto auto auto; grid-row:auto; }
          }
        ")),
        shiny::div(class = "mslm-selected-node-plots",
          shiny::div(class = "plot-card",
            shiny::div(class = "mslm-node-plot-header", shiny::uiOutput(ns("plot_title_left"))),
            shiny::div(class = "cy-plot-frame", shiny::div(class = "cy-plot-square",
              shiny::plotOutput(ns("plot_left"), height = "100%"))),
            shiny::downloadButton(ns("dl_left"), "Download", class = "btn-sm")),
          shiny::div(class = "plot-card",
            shiny::div(class = "mslm-node-plot-header",
              shiny::uiOutput(ns("plot_title_right")), shiny::uiOutput(ns("molecule_ui"))),
            shiny::div(class = "cy-plot-frame", shiny::div(class = "cy-plot-square",
              shiny::plotOutput(ns("plot_right"), height = "100%"))),
            shiny::downloadButton(ns("dl_right"), "Download", class = "btn-sm")))),
      shiny::tags$aside(id = ns("settings_drawer"), class = "mslm-pathway-settings",
        hidden = NA, role = "dialog", `aria-modal` = "true", `aria-labelledby` = ns("settings_title"),
        shiny::div(class = "mslm-settings-header",
          shiny::tags$h3(id = ns("settings_title"), "Pathway settings"),
          shiny::tags$button(type = "button", class = "mslm-settings-close", `data-settings-close` = "true", `aria-label` = "Close settings", "\u00d7")),
        shiny::div(class = "mslm-settings-tabs", role = "tablist",
          lapply(names(tabs), function(key) {
            shiny::tags$button(type = "button", role = "tab", id = ns(paste0("settings_tab_", key)),
              `data-tab` = key, `aria-controls` = ns(paste0("settings_pane_", key)),
              `aria-selected` = if (key == "display") "true" else "false", tabs[[key]])
          })),
        shiny::div(class = "mslm-settings-body",
          pane("display", id = ns("settings_pane_display"), role = "tabpanel", `aria-labelledby` = ns("settings_tab_display"),
            shiny::tags$h4("Pathway"),
            shiny::div(class = "mslm-pathway-picker",
              shiny::selectInput(ns("net_select"), "Select pathway", choices = character(0), size = 3, selectize = FALSE)),
            shiny::helpText("Select a pathway, then Apply settings. Applying settings clears existing projections; run Pathway Mapping again."),
            shiny::tags$hr(),
            shiny::tags$h4("Projected plots"),
            shiny::radioButtons(ns("plot_type"), "Plot type", choices = c("dot", "violin", "box"), selected = "dot", inline = TRUE),
            shiny::checkboxInput(ns("include_multiomics"), "Overlay multi-omics data", FALSE),
            shiny::tags$hr(), shiny::tags$h4("Aggregation and heatmap"),
            shiny::selectInput(ns("agg_fun"), "Class aggregation", choices = c("sum", "mean", "median"), selected = "sum"),
            shiny::numericInput(ns("hm_top_n"), "Heatmap: top molecules by abundance", 20, min = 5, max = 200, step = 5),
            shiny::helpText("Apply settings, then use Pathway Mapping to regenerate projected images.")),
          pane("compare", id = ns("settings_pane_compare"), role = "tabpanel", `aria-labelledby` = ns("settings_tab_compare"),
            shiny::tags$h4("Group comparison"), shiny::uiOutput(ns("diff_ui")),
            shiny::helpText("Direction: B / A. Red = increase; blue = decrease. FC bars show effect size, not significance."),
            shiny::tags$hr(), shiny::tags$h4("Popup panels"),
            shiny::selectInput(ns("popup_content"), "Panel content", choices = c("Species fold changes" = "fc", "Enlarged plot" = "plot", "Heatmap" = "heatmap", "Enrichment" = "enrichment"), selected = "fc"),
            shiny::helpText("Panel content switches immediately without remapping. FC axes adapt independently to each panel; compare the tick values."),
            shiny::selectInput(ns("fc_scale"), "Input assay scale", choices = c("Non-negative abundance" = "abundance", "Already log2 transformed" = "log2")),
            shiny::numericInput(ns("fc_top_n"), "Top species per direction", 5, min = 1, max = 30, step = 1),
            shiny::tags$details(shiny::tags$summary("Calculation and export details"),
              shiny::helpText("Abundance: log2(mean B + 1e-8) - log2(mean A + 1e-8). Log2 input: mean B - mean A. Need at least 2 finite values per group; negative abundance values are excluded."),
              shiny::helpText("SVG has vector FC panels and a raster network snapshot. PDF with panels / popups is a bitmap."))),
          pane("filter", id = ns("settings_pane_filter"), role = "tabpanel", `aria-labelledby` = ns("settings_tab_filter"),
            shiny::tags$h4("Acyl-chain / sphingoid-base filter"), shiny::uiOutput(ns("chain_filter_ui")),
            shiny::tags$hr(), shiny::tags$h4("Enrichment overlay"),
            shiny::fileInput(ns("enrich_csv"), "Import enrichment CSV", accept = ".csv"),
            shiny::actionButton(ns("clear_enrich"), "Clear enrichment", class = "btn-sm"),
            shiny::helpText("Apply settings to map enrichment molecules to lipid classes. Double borders indicate enriched classes.")),
          pane("manage", id = ns("settings_pane_manage"), role = "tabpanel", `aria-labelledby` = ns("settings_tab_manage"),
            shiny::tags$h4("Import pathways"),
            shiny::fileInput(ns("netfiles"), "CYJS / GML / GPML", accept = c(".cyjs", ".json", ".gml", ".gpml", ".xml"), multiple = TRUE),
            shiny::helpText("Imports are added immediately. Select a pathway in the Display tab."),
            shiny::tags$hr(), shiny::tags$h4("Edit pathway"),
            shiny::actionButton(ns("delete_selected"), "Delete selected elements", class = "btn-danger btn-sm"),
            shiny::helpText("Ctrl + right-click a node or edge to delete it. You can also select elements and press Delete."),
            shiny::tags$hr(),
            shiny::tags$button(type = "button", class = "btn btn-default btn-sm", `data-network-delete` = "true", "Delete current pathway..."))),
        shiny::div(class = "mslm-settings-footer",
          shiny::tags$span(class = "mslm-settings-dirty", role = "status", "Changes take effect after Apply."),
          shiny::actionButton(ns("settings_apply"), "Apply settings", class = "btn-primary")))
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
    examples_loaded <- FALSE
    project_loaded <- reactiveVal(FALSE)
    projection_state <- reactiveValues(map = list())
    last_multiomics <- FALSE
    enrichment_enabled <- reactiveVal(FALSE)

    # Settings in the drawer are drafts. All analytical consumers use this
    # snapshot, so closing the drawer never silently applies half-edited values.
    applied_settings <- reactiveVal(list(
      plot_type = "dot", include_multiomics = FALSE, agg_fun = "sum", hm_top_n = 20L,
      diff_group_a = NULL, diff_group_b = NULL, diff_method = "wilcox",
      diff_use_padj = FALSE, diff_alpha = 0.05,
      fc_scale = "abundance", fc_top_n = 5L, chain_codes = character(0),
      chain_require_all = FALSE, chain_exclude_odd = FALSE,
      chain_carbon = "", chain_db = "", chain_oxygen = ""))
    default_settings <- shiny::isolate(applied_settings())
    .cfg <- function(name) {
      value <- applied_settings()[[name]]
      if (name %in% c("diff_group_a", "diff_group_b")) {
        se <- se_group_ref()
        levels <- if (is.null(se)) character(0) else .class_levels_from_se(se, .selected_group_col())
        if (is.null(value) || !value %in% levels) {
          index <- if (name == "diff_group_a") 1L else 2L
          return(if (length(levels) >= index) levels[[index]] else "")
        }
      }
      value
    }
    observeEvent(input$settings_apply, {
      next_settings <- applied_settings()
      for (name in names(next_settings)) {
        value <- input[[name]]
        if (!is.null(value)) next_settings[name] <- list(value)
        else if (name == "chain_codes") next_settings[name] <- list(character(0))
      }
      a <- next_settings$diff_group_a %||% ""
      b <- next_settings$diff_group_b %||% ""
      if (nzchar(a) && nzchar(b) && identical(a, b)) {
        showNotification("Choose different groups for A and B.", type = "error")
        return()
      }
      valid_number <- function(x, lo, hi) length(x) == 1L && is.finite(x) && x >= lo && x <= hi
      if (!valid_number(next_settings$fc_top_n, 1, 30) ||
          !valid_number(next_settings$hm_top_n, 5, 200) ||
          !valid_number(next_settings$diff_alpha, 1e-6, 0.5)) {
        showNotification("Check FC species count (1-30), heatmap count (5-200), and alpha (0.000001-0.5).", type = "error")
        return()
      }
      target <- input$net_select %||% rv_selected()
      if (!is.null(target) && !target %in% names(rv_nets$map)) {
        showNotification("The selected pathway is no longer available.", type = "error")
        return()
      }
      applied_settings(next_settings)
      shiny::updateCheckboxInput(session, "diff_borders_enabled", value = FALSE)
      .invalidate_projection()
      if (!is.null(target)) rv_selected(target)
      session$sendCustomMessage("cy_settings_applied", list(
        root_id = ns("workspace"), prefix = ns(""),
        enrichment = FALSE))
    }, ignoreInit = TRUE, priority = 100)

    output$settings_summary <- renderUI({
      badge <- function(label, tab) shiny::tags$button(type = "button",
        class = "mslm-settings-badge", `data-settings-open` = tab, label)
      a <- .cfg("diff_group_a"); b <- .cfg("diff_group_b")
      filters <- .cfg("chain_codes")
      if (isTRUE(.cfg("chain_exclude_odd"))) filters <- c(filters, "Exclude odd chains")
      if (isTRUE(.cfg("chain_require_all")) && length(.cfg("chain_codes"))) filters <- c(filters, "ALL")
      chain_labels <- c(chain_carbon = "C", chain_db = "DB", chain_oxygen = "O")
      for (key in names(chain_labels)) {
        value <- .cfg(key)
        if (nzchar(value)) filters <- c(filters, paste0(chain_labels[[key]], ":", value))
      }
      shiny::div(class = "mslm-settings-summary",
        badge(if (nzchar(a) && nzchar(b)) paste0(b, " / ", a) else "Choose comparison groups", "compare"),
        badge(paste(c(.cfg("plot_type"), .cfg("agg_fun"), if (isTRUE(.cfg("include_multiomics"))) "multi-omics"), collapse = " · "), "display"),
        badge(if (length(filters)) paste(filters, collapse = ", ") else "No chain filter", "filter"))
    })
    output$current_pathway <- renderUI({
      id <- rv_selected()
      info <- if (!is.null(id)) rv_nets$map[[id]] else NULL
      if (is.null(info)) return(shiny::tags$span("No pathway selected"))
      el <- info$elements_work
      mapped <- identical(projection_state$map[[id]], "mapped")
      shiny::div(class = "mslm-current-pathway",
        shiny::tags$strong(info$name),
        shiny::tags$span(class = "mslm-pathway-counts", paste(length(el$nodes), "nodes /", length(el$edges), "connections")),
        shiny::tags$span(class = if (mapped) "mslm-projection-ready" else "mslm-projection-pending",
          if (mapped) "Projected" else "Not projected - run Pathway Mapping"))
    })
    output$diff_legend <- renderUI({
      shiny::tags$span(class = "mslm-significance-legend",
        shiny::tags$span(style = "color:#ef4444", "\u25cf Increase"),
        shiny::tags$span(style = "color:#3b82f6", "\u25cf Decrease"),
        shiny::tags$span(style = "color:#687887", "\u25cf Other / untested"),
        shiny::tags$span(paste0(.cfg("diff_method"), " · ",
          if (isTRUE(.cfg("diff_use_padj"))) "FDR" else "p", " \u2264 ", .cfg("diff_alpha"))))
    })

    # ---- Image URL builder (IMPORTANT: /static prefix) -----------------------
    image_context <- session$userData$mslm_images
    CY_IMG_PORT   <- image_context$port %||% 7310L
    CY_IMG_PREFIX <- image_context$prefix %||% "/static"

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

    .invalidate_projection <- function(ids = names(rv_nets$map)) {
      nets <- rv_nets$map
      diff <- rv_diff_border$map
      enrich <- rv_enrich_state$map
      status <- projection_state$map
      for (id in intersect(ids, names(nets))) {
        nets[[id]]$elements_work <- .mslm_clear_projection_elements(nets[[id]]$elements_work)
        diff[[id]] <- NULL
        enrich[[id]] <- NULL
        status[[id]] <- "needs_mapping"
      }
      rv_nets$map <- nets
      rv_diff_border$map <- diff
      rv_enrich_state$map <- enrich
      projection_state$map <- status
      selected_node(NULL)
      session$sendCustomMessage("cy_fc_action", list(container_id = container_id, action = "clear"))
      session$sendCustomMessage("cy_reset_projection", list(container_id = container_id))
    }

    make_id <- function() paste0(
      "net_",
      as.integer(as.numeric(Sys.time()) * 1000), "_",
      paste(sample(c(letters, 0:9), 4, TRUE), collapse = "")
    )

    add_network <- function(name, elements) {
      id <- make_id()
      rv_nets$map[[id]] <- list(id = id, name = name %||% id, elements_work = elements)
      current_selected <- shiny::isolate(rv_selected())
      if (is.null(current_selected)) {
        rv_selected(id)
        current_selected <- id
      }

      updateSelectInput(
        session, "net_select",
        choices  = setNames(names(rv_nets$map), vapply(rv_nets$map, `[[`, character(1), "name")),
        selected = current_selected
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

    .read_default_network <- function(path) {
      obj <- jsonlite::read_json(path, simplifyVector = FALSE)
      .mslm_clear_projection_elements(.ensure_positions_for_preset(.cyjs_to_elements(obj)))
    }

    .default_network_specs <- function(include_multiomics = FALSE) {
      include_multiomics <- isTRUE(include_multiomics)
      list(
        list(
          name = "glycerophospholipids",
          file = if (include_multiomics) "remodeling.cyjs" else "remodeling_lipidonly.cyjs"
        ),
        list(
          name = "sphingolipids",
          file = if (include_multiomics) "ceramidepathway.cyjs" else "ceramidepathway_lipidonly.cyjs"
        ),
        list(
          name = "global pathway",
          file = "global.cyjs"
        )
      )
    }

    .upsert_default_networks <- function(include_multiomics = FALSE, only_if_empty = FALSE) {
      ex_dir <- .mslm_example_dir()
      if (!nzchar(ex_dir) || !dir.exists(ex_dir)) return(invisible(FALSE))

      should_skip <- isTRUE(only_if_empty) && length(rv_nets$map) > 0
      if (should_skip) return(invisible(FALSE))

      loaded_any <- FALSE
      for (spec in .default_network_specs(include_multiomics = include_multiomics)) {
        path <- file.path(ex_dir, spec$file)
        if (!file.exists(path)) next

        els <- .read_default_network(path)

        id_hit <- NULL
        for (nid in names(rv_nets$map)) {
          if (identical(rv_nets$map[[nid]]$name, spec$name)) {
            id_hit <- nid
            break
          }
        }

        if (is.null(id_hit)) {
          add_network(spec$name, els)
        } else {
          rv_nets$map[[id_hit]]$elements_work <- els
        }
        loaded_any <- TRUE
      }

      if (loaded_any) {
        examples_loaded <<- TRUE
      }

      invisible(loaded_any)
    }

    observe({
      if (is.null(elements) && !project_loaded()) {
        .upsert_default_networks(
          include_multiomics = isTRUE(.cfg("include_multiomics")),
          only_if_empty = TRUE
        )
      }
    })

    session$onFlushed(function() {
      if (is.null(elements) && !shiny::isolate(project_loaded()) && length(shiny::isolate(rv_nets$map)) == 0) {
        .upsert_default_networks(
          include_multiomics = isTRUE(shiny::isolate(.cfg("include_multiomics"))),
          only_if_empty = TRUE
        )
      }
    }, once = TRUE)

    observeEvent(.cfg("include_multiomics"), {
      req(is.null(elements))
      overlay <- isTRUE(.cfg("include_multiomics"))
      if (identical(overlay, last_multiomics)) return()
      last_multiomics <<- overlay
      if (!isTRUE(examples_loaded) && length(rv_nets$map) == 0) {
        .upsert_default_networks(
          include_multiomics = isTRUE(.cfg("include_multiomics")),
          only_if_empty = TRUE
        )
      } else {
        .upsert_default_networks(
          include_multiomics = isTRUE(.cfg("include_multiomics")),
          only_if_empty = FALSE
        )
      }
    }, ignoreInit = TRUE)

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

    # net_select is a draft in the settings drawer; commit it with settings_apply.
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

    # Non-reactive mirror of the graph already displayed by the client. Updating
    # stored elements after a client deletion must not rebuild that same graph.
    client_graph <- new.env(parent = emptyenv())
    client_graph$id <- NULL
    client_graph$elements <- NULL

    # ---- Deletion sync: JS -> Shiny ------------------------------------------
    observeEvent(input$cy_changed, {
      msg <- input$cy_changed
      if (is.null(msg) || is.null(msg$action)) return()

      id <- msg$network_id %||% rv_selected()
      if (is.null(id) || is.null(rv_nets$map[[id]])) return()

      if (identical(msg$action, "delete_selected") && is.null(msg$cyjs)) {
        session$sendCustomMessage("cy_push_cyjs", list(container_id = container_id))
        return()
      }

      if (msg$action %in% c("cyjs", "delete_selected") && !is.null(msg$cyjs)) {
        new_el <- .cyjs_to_elements(msg$cyjs)
        new_el <- .ensure_positions_for_preset(new_el)

        info <- rv_nets$map[[id]]
        info$elements_work <- new_el
        if (identical(id, rv_selected())) {
          client_graph$id <- id
          client_graph$elements <- new_el
        }
        rv_nets$map[[id]] <- info
      }
    }, ignoreInit = TRUE)

    se_group_ref <- reactive({
      se <- tryCatch(se_lipid_reactive(), error = function(e) NULL)
      if (!is.null(se)) return(se)
      if (!is.null(se_tx_reactive)) {
        se2 <- tryCatch(se_tx_reactive(), error = function(e) NULL)
        return(se2)
      }
      NULL
    })

    # Primary grouping is fixed to colData$class.
    # Optional panel split is read from shared Feature advanced settings.
    .selected_group_col <- reactive("class")

    .palette_for_groups <- function(groups, adv = NULL) {
      groups <- unique(as.character(groups))
      groups <- groups[!is.na(groups) & nzchar(groups)]
      if (!length(groups)) return(NULL)
      hues <- seq(15, 375, length.out = length(groups) + 1)[seq_along(groups)]
      pal <- grDevices::hcl(h = hues, l = 65, c = 100)
      names(pal) <- groups
      if (!is.null(adv$palette_map) && length(adv$palette_map)) {
        nm <- intersect(names(adv$palette_map), groups)
        pal[nm] <- adv$palette_map[nm]
      }
      pal
    }

    .class_levels_from_se <- function(se, class_col) {
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
      group_col <- .selected_group_col()
      levs <- .class_levels_from_se(se, group_col)
      if (length(levs) < 2) {
        return(shiny::helpText(paste0("Differential: unavailable (colData$", group_col, " has <2 levels).")))
      }

      a0 <- .cfg("diff_group_a")
      b0 <- .cfg("diff_group_b")

      if (is.null(a0) || !(a0 %in% levs)) a0 <- levs[[1]]
      if (is.null(b0) || !(b0 %in% levs)) b0 <- levs[[min(2, length(levs))]]
      if (identical(a0, b0) && length(levs) >= 2) b0 <- levs[[2]]

      adj0 <- isTRUE(.cfg("diff_use_padj"))

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("diff_group_a"), "Group A", choices = levs, selected = a0)),
          shiny::column(6, shiny::selectInput(ns("diff_group_b"), "Group B", choices = levs, selected = b0))
        ),
        shiny::selectInput(
          ns("diff_method"),
          "Test",
          choices = c("Wilcoxon" = "wilcox", "t-test" = "t"),
          selected = .cfg("diff_method") %||% "wilcox"
        ),
        shiny::checkboxInput(
          ns("diff_use_padj"),
          "Use BH-adjusted p-value (FDR)",
          value = adj0
        ),
        shiny::numericInput(
          ns("diff_alpha"),
          "Alpha",
          value = .cfg("diff_alpha") %||% 0.05,
          min = 1e-6, max = 0.5, step = 0.01
        )
      )
    })

    # These inputs live in a custom drawer, rather than a Bootstrap tab. Bind
    # their controls even while closed; only the downstream settings are drafts.
    shiny::outputOptions(output, "diff_ui", suspendWhenHidden = FALSE)

    .idx_from_se <- function(se, gA, gB, class_col) {
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

    .compute_diff_stats <- function(el, se_lipid, se_tx, gA, gB, class_col, method = "wilcox") {
      stats_map <- list()

      if (!is.null(se_lipid)) {
        ii <- .idx_from_se(se_lipid, gA, gB, class_col)

        valid_lipid <- get_valid_classes_from_se(se_lipid)
        keys <- .extract_net_keys(el)$class
        target <- intersect(keys, valid_lipid)

        if (length(target)) {
          agg_fun <- tolower(as.character(.cfg("agg_fun") %||% "sum"))[1]
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
        ii <- .idx_from_se(se_tx, gA, gB, class_col)

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
      reset_ids <- vapply(el$nodes, function(n) as.character(n$data$id), character(1))
      if (!length(stats_map)) return(list(items = list(), reset_ids = reset_ids))

      keys  <- names(stats_map)
      pvals <- vapply(stats_map, function(x) x$p, numeric(1))

      p_used <- pvals
      if (isTRUE(use_padj)) {
        p_used <- stats::p.adjust(pvals, method = "BH")
      }
      names(p_used) <- keys

      col_up   <- "#ef4444"
      col_down <- "#3b82f6"
      col_ns   <- "#b8c1cb"

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
          significant <- isTRUE(p_used[[k]] <= alpha) && is.finite(lfc)
          if (significant) {
            col <- if (lfc > 0) col_up else col_down
          } else {
            col <- col_ns
          }
          items[[length(items) + 1L]] <- list(id = nid, color = col,
            width = if (significant) 4 else 1)
        }
      }

      list(items = items, reset_ids = reset_ids)
    }

    observeEvent(list(input$apply_diff_borders, input$diff_borders_enabled,
                      rv_selected(), applied_settings(), se_group_ref(), projection_state$map), {
      req(isTRUE(input$diff_borders_enabled))
      id <- rv_selected()
      req(id)
      el <- rv_nets$map[[id]]$elements_work
      req(el)

      gA <- trimws(as.character(.cfg("diff_group_a") %||% ""))
      gB <- trimws(as.character(.cfg("diff_group_b") %||% ""))
      req(nzchar(gA), nzchar(gB))
      if (identical(gA, gB)) {
        showNotification("Group A and Group B must be different.", type = "error")
        return()
      }

      method <- tolower(as.character(.cfg("diff_method") %||% "wilcox"))[1]
      group_col <- .selected_group_col()

      alpha <- suppressWarnings(as.numeric(.cfg("diff_alpha") %||% 0.05))
      if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

      use_padj <- isTRUE(.cfg("diff_use_padj"))

      se_lipid <- tryCatch(se_lipid_effective(), error = function(e) NULL)
      se_tx <- if (!is.null(se_tx_reactive)) tryCatch(se_tx_reactive(), error = function(e) NULL) else NULL

      if (!is.null(se_lipid)) {
        cd <- as.data.frame(colData(se_lipid))
        if (!group_col %in% names(cd)) {
          showNotification(paste0("Lipid SE: colData has no column '", group_col, "'. Lipid nodes will be skipped."), type = "warning")
          se_lipid <- NULL
        }
      }
      if (!is.null(se_tx)) {
        cd <- as.data.frame(colData(se_tx))
        if (!group_col %in% names(cd)) {
          showNotification(paste0("Gene SE: colData has no column '", group_col, "'. Gene nodes will be skipped."), type = "warning")
          se_tx <- NULL
        }
      }

      if (is.null(se_lipid) && is.null(se_tx)) {
        showNotification(paste0("No applicable SE found (missing colData$", group_col, ")."), type = "error")
        return()
      }

      stats_map <- tryCatch(
        .compute_diff_stats(el, se_lipid, se_tx, gA, gB, class_col = group_col, method = method),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(stats_map) || !length(stats_map)) {
        showNotification("No testable nodes found in this network for the selected groups.", type = "warning")
        stats_map <- list()
      }

      payload <- .border_items_from_stats(el, stats_map, alpha = alpha, use_padj = use_padj)
      rv_diff_border$map[[id]] <- payload

      session$sendCustomMessage("cy_set_node_borders", c(list(container_id = container_id), payload))
    }, ignoreInit = TRUE)

    observeEvent(list(input$clear_diff_borders, input$diff_borders_enabled, rv_selected()), {
      req(!isTRUE(input$diff_borders_enabled))
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
      enrichment_enabled(TRUE)
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
      enrichment_enabled(FALSE)
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
      if (!project_loaded() && is.null(rv_selected()) && is.null(elements) && length(rv_nets$map) == 0) {
        .upsert_default_networks(
          include_multiomics = isTRUE(.cfg("include_multiomics")),
          only_if_empty = TRUE
        )
      }

      id <- rv_selected()
      req(id)

      el <- rv_nets$map[[id]]$elements_work
      req(el)
      if (identical(client_graph$id, id) && identical(client_graph$elements, el)) return()
      client_graph$id <- id
      client_graph$elements <- el

      use_preset <- .any_node_has_pos(el)
      if (use_preset) el <- .ensure_positions_for_preset(el)
      lay <- if (use_preset) "preset" else default_layout

      session$sendCustomMessage(
        "cy_init",
        list(
          container_id      = container_id,
          network_id        = id,
          elements          = el,
          layout            = lay,
          style             = NULL,
          wheelSensitivity  = 0.2,
          selected_input    = ns("node_selected"),
          changed_input     = ns("cy_changed"),
          error_input       = ns("cy_render_error"),
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
    observeEvent(input$cy_render_error, {
      message <- as.character(input$cy_render_error$message %||% "Unknown Cytoscape error")
      warning(paste0("Pathway renderer: ", message), call. = FALSE)
      shiny::showNotification(paste0("Network rendering failed: ", message), type = "error", duration = NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$node_selected, {
      selected_node(input$node_selected)
    }, ignoreInit = TRUE)

    observe({
      enabled <- isTRUE(input$fc_enabled)
      mode <- input$popup_content %||% "fc"
      id <- rv_selected()
      # Track the network identity, but not plot-image/style updates.
      if (!enabled) {
        session$sendCustomMessage("cy_fc_panels", list(container_id = container_id, enabled = FALSE, network_id = id, mode = mode))
        return()
      }
      payload <- tryCatch({
        if (mode != "fc") {
          list(container_id = container_id, enabled = TRUE, network_id = id, mode = mode, classes = list(), error = "")
        } else {
        se <- se_lipid_effective()
        if (is.null(se)) stop("Lipid data are not available.")
        a <- trimws(as.character(.cfg("diff_group_a") %||% ""))
        b <- trimws(as.character(.cfg("diff_group_b") %||% ""))
        result <- .mslm_fc_panel_data(se, resolve_class_col(se), .selected_group_col(),
          a, b, scale = .cfg("fc_scale") %||% "abundance", top_n = .cfg("fc_top_n") %||% 5L)
        c(list(container_id = container_id, enabled = TRUE, network_id = id,
          mode = mode, contrast = paste0(b, " / ", a), error = ""), result)
        }
      }, error = function(e) list(container_id = container_id, enabled = TRUE,
        network_id = id, mode = mode, classes = list(), error = conditionMessage(e)))
      session$sendCustomMessage("cy_fc_panels", payload)
    })
    observeEvent(input$fc_arrange, {
      session$sendCustomMessage("cy_fc_action", list(container_id = container_id, action = "arrange"))
    }, ignoreInit = TRUE)
    observeEvent(input$fc_clear, {
      session$sendCustomMessage("cy_fc_action", list(container_id = container_id, action = "clear"))
    }, ignoreInit = TRUE)
    observeEvent(input$fc_svg, {
      session$sendCustomMessage("cy_fc_action", list(container_id = container_id, action = "export"))
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
      .has_chain_col(se, "acyl_chains") || .has_chain_col(se, "sphingoid_bases")
    })

    output$chain_filter_ui <- renderUI({
      if (!isTRUE(has_chain_info())) {
        return(shiny::helpText("Chain/SPB filter: unavailable (rowData$acyl_chains and rowData$sphingoid_bases are missing)."))
      }

      se <- se_lipid_reactive()
      codes <- .get_combined_chain_codes(se, c("acyl_chains", "sphingoid_bases"))

      selected0 <- .cfg("chain_codes")
      if (is.null(selected0)) selected0 <- character(0)
      selected0 <- intersect(as.character(selected0), codes)

      shiny::tagList(
        shiny::checkboxInput(ns("chain_exclude_odd"), "Exclude molecules with any odd-carbon chain or SPB", value = isTRUE(.cfg("chain_exclude_odd"))),
        shiny::selectizeInput(
          ns("chain_codes"),
          "Chain / SPB code(s) (multiple)",
          choices  = codes,
          selected = selected0,
          multiple = TRUE,
          options  = list(
            plugins     = list("remove_button"),
            placeholder = "Select one or more (e.g., 18:0, 22:6;O2, SPB18:1;O2)",
            maxOptions  = 5000
          )
        ),
        shiny::checkboxInput(ns("chain_require_all"), "Require ALL selected chain codes", value = isTRUE(.cfg("chain_require_all"))),
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(ns("chain_carbon"), "C",  value = .cfg("chain_carbon") %||% "")),
          shiny::column(4, shiny::textInput(ns("chain_db"),     "DB", value = .cfg("chain_db") %||% "")),
          shiny::column(4, shiny::textInput(ns("chain_oxygen"), "O",  value = .cfg("chain_oxygen") %||% ""))
        ),
        shiny::actionButton(ns("chain_reset"), "Reset", class = "btn-xs")
      )
    })

    shiny::outputOptions(output, "chain_filter_ui", suspendWhenHidden = FALSE)

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

      if (!isTRUE(.has_chain_col(se, "acyl_chains") || .has_chain_col(se, "sphingoid_bases"))) return(se)

      chain_codes <- .cfg("chain_codes") %||% character(0)
      require_all <- isTRUE(.cfg("chain_require_all"))
      exclude_odd <- isTRUE(.cfg("chain_exclude_odd"))

      carbon <- suppressWarnings(as.integer(trimws(.cfg("chain_carbon") %||% "")))
      db     <- suppressWarnings(as.integer(trimws(.cfg("chain_db") %||% "")))
      oxygen <- suppressWarnings(as.integer(trimws(.cfg("chain_oxygen") %||% "")))

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
        chain_cols = c("acyl_chains", "sphingoid_bases")
      )
    })

    # ---- Map local SVGs (lipid + gene + heatmap) ------------------------------
    observeEvent(input$map_local_svgs, {
      id <- rv_selected()
      req(id)

      info <- rv_nets$map[[id]]
      req(info)

      .invalidate_projection(id)
      info <- rv_nets$map[[id]]

      se_lipid <- se_lipid_effective()
      req(se_lipid)

      se_tx <- if (!is.null(se_tx_reactive)) se_tx_reactive() else NULL

      svg_dir <- image_context$directory %||% .get_svg_dir()

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
        plot_type <- tolower(as.character(.cfg("plot_type") %||% "dot"))[1]
        agg_fun   <- tolower(as.character(.cfg("agg_fun")   %||% "sum"))[1]
        group_col <- .selected_group_col()

        adv <- .get_adv_or_default(se_lipid, settings_reactive = settings_reactive)
        facet_var <- adv$facet_var %||% ""
        x_order <- adv$manual_order %||% character(0)
        groups  <- .class_levels_from_se(se_lipid, group_col)
        pal     <- .palette_for_groups(groups, adv)
        plot_font   <- .safe_plot_font_size(adv$plot_font_size %||% 12)
        strip_font  <- .safe_strip_font_size(adv$strip_font_size %||% plot_font, default = plot_font)
        p_label_font <- .safe_p_label_font_size(adv$p_label_font_size %||% 3.5)
        y_axis_label <- trimws(as.character(adv$lipid_y_axis_label %||% "Abundance"))
        if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"
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
            se = se_cls, feature_id = cls, x_var = group_col,
            facet_var = if (nzchar(facet_var)) facet_var else NULL,
            x_order = if (length(x_order)) x_order else NULL,
            palette = pal
          ), extra_style, extra_p))

          p <- .apply_palette_if_missing(p, pal)
          p <- .apply_plot_font_size(p, plot_font, strip_size = strip_font)
          p <- .apply_p_label_size(p, p_label_font)
          p <- p + ggplot2::labs(y = y_axis_label)
          p <- p + ggplot2::theme(legend.position = "none")
          p <- .prep_for_node_svg(p)

          .save_svg_file(p, f, width = sz[["w"]], height = sz[["h"]])
          files_map_class[[cls]] <- f
        }

        for (cls in names(files_map_class)) {
          fname <- basename(files_map_class[[cls]])
          url_map_lipid_class[[cls]] <- .img_url(fname, session, ts = ts_now)  # ★ /static 付き
        }

        top_n <- as.integer(.cfg("hm_top_n") %||% 40)
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
            x_var      = group_col,
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
          plot_type <- tolower(as.character(.cfg("plot_type") %||% "dot"))[1]
          fun <- get_plot_fun(plot_type)
          group_col <- .selected_group_col()

          adv_tx <- .get_adv_or_default(se_tx, settings_reactive = settings_reactive)
          facet_var <- adv_tx$facet_var %||% ""
          x_order <- adv_tx$manual_order %||% character(0)
          groups  <- .class_levels_from_se(se_tx, group_col)
          pal     <- .palette_for_groups(groups, adv_tx)
          plot_font   <- .safe_plot_font_size(adv_tx$plot_font_size %||% 12)
          strip_font  <- .safe_strip_font_size(adv_tx$strip_font_size %||% plot_font, default = plot_font)
          p_label_font <- .safe_p_label_font_size(adv_tx$p_label_font_size %||% 3.5)
          y_axis_label <- trimws(as.character(adv_tx$gene_y_axis_label %||% "Abundance"))
          if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"
          extra_p     <- .pvalue_args_from_adv(adv_tx)
          extra_style <- .plot_style_args_from_adv(adv_tx, plot_type)

          sz_gene <- .get_svg_size("gene")

          out_files <- list()
          for (ens_id in target_ens) {
            fid <- unname(ens_map[[ens_id]])
            safe <- .safe_id(fid)
            f <- file.path(svg_dir, paste0("gene_", safe, ".svg"))

            p <- do.call(fun, c(list(
              se = se_tx, feature_id = fid, x_var = group_col,
              facet_var = if (nzchar(facet_var)) facet_var else NULL,
              x_order = if (length(x_order)) x_order else NULL,
              palette = pal
            ), extra_style, extra_p))

            p <- .apply_palette_if_missing(p, pal)
            p <- .apply_plot_font_size(p, plot_font, strip_size = strip_font)
            p <- .apply_p_label_size(p, p_label_font)
            p <- p + ggplot2::labs(y = y_axis_label)
            p <- p + ggplot2::theme(legend.position = "none")
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

      thin_bw  <- 1L

      el$nodes <- lapply(el$nodes, function(n) {
        d <- n$data %||% list()

        cls_key <- trimws(as.character(d$shared_name %||% ""))
        ens_key <- trimws(as.character(d$Ensembl_ID %||% ""))
        ens_key <- sub("\\..*$", "", ens_key)

        if (nzchar(ens_key) && !is.null(url_map_gene[[ens_key]])) {
          d$path <- url_map_gene[[ens_key]]
        } else if (nzchar(cls_key) && !is.null(url_map_lipid_class[[cls_key]])) {
          d$path <- url_map_lipid_class[[cls_key]]
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
      projection_state$map[[id]] <- "mapped"
      session$sendCustomMessage("cy_projection_ready", list(prefix = ns(""), enrichment = isTRUE(enrichment_enabled())))
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

      plot_type <- tolower(as.character(.cfg("plot_type") %||% "dot"))[1]
      fun <- get_plot_fun(plot_type)

      if (.is_gene_node(node)) {
        se_tx <- if (!is.null(se_tx_reactive)) se_tx_reactive() else NULL
        if (is.null(se_tx)) return(.empty_plot("Gene SE is not available."))
        group_col <- .selected_group_col()

        ens <- .get_ens_key(node)
        ens_map <- .make_ensembl_to_rowname_map(se_tx)
        if (!ens %in% names(ens_map)) return(.empty_plot(paste0("Ensembl_ID not found in se_tx: ", ens)))
        fid <- unname(ens_map[[ens]])

        adv <- .get_adv_or_default(se_tx, settings_reactive = settings_reactive)
        facet_var <- adv$facet_var %||% ""
        x_order <- adv$manual_order %||% character(0)
        groups  <- .class_levels_from_se(se_tx, group_col)
        pal     <- .palette_for_groups(groups, adv)
        plot_font   <- .safe_plot_font_size(adv$plot_font_size %||% 12)
        strip_font  <- .safe_strip_font_size(adv$strip_font_size %||% plot_font, default = plot_font)
        p_label_font <- .safe_p_label_font_size(adv$p_label_font_size %||% 3.5)
        y_axis_label <- trimws(as.character(adv$gene_y_axis_label %||% "Abundance"))
        if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"
        extra_style <- .plot_style_args_from_adv(adv, plot_type)
        extra_p     <- .pvalue_args_from_adv(adv)

        p <- do.call(fun, c(list(
          se = se_tx, feature_id = fid, x_var = group_col,
          facet_var = if (nzchar(facet_var)) facet_var else NULL,
          x_order = if (length(x_order)) x_order else NULL,
          palette = pal
        ), extra_style, extra_p))

        p <- .apply_palette_if_missing(p, pal)
        p <- .apply_plot_font_size(p, plot_font, strip_size = strip_font)
        p <- .apply_p_label_size(p, p_label_font)
        p + ggplot2::labs(title = NULL, y = y_axis_label) +
          ggplot2::theme(legend.position = "none")

      } else {
        se_lipid <- se_lipid_effective()
        if (!.is_valid_lipid_class_node(node, se_lipid)) {
          return(.empty_plot("Unsupported node type (not a valid lipid class, and no Ensembl ID)."))
        }

        cls <- .get_class_key(node)
        agg_fun <- tolower(as.character(.cfg("agg_fun") %||% "sum"))[1]
        se_cls <- aggregate_to_class_se(se_lipid, fun = agg_fun)
        group_col <- .selected_group_col()

        adv <- .get_adv_or_default(se_lipid, settings_reactive = settings_reactive)
        facet_var <- adv$facet_var %||% ""
        x_order <- adv$manual_order %||% character(0)
        groups  <- .class_levels_from_se(se_lipid, group_col)
        pal     <- .palette_for_groups(groups, adv)
        plot_font   <- .safe_plot_font_size(adv$plot_font_size %||% 12)
        strip_font  <- .safe_strip_font_size(adv$strip_font_size %||% plot_font, default = plot_font)
        p_label_font <- .safe_p_label_font_size(adv$p_label_font_size %||% 3.5)
        y_axis_label <- trimws(as.character(adv$lipid_y_axis_label %||% "Abundance"))
        if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"
        extra_style <- .plot_style_args_from_adv(adv, plot_type)
        extra_p     <- .pvalue_args_from_adv(adv)

        p <- do.call(fun, c(list(
          se = se_cls, feature_id = cls, x_var = group_col,
          facet_var = if (nzchar(facet_var)) facet_var else NULL,
          x_order = if (length(x_order)) x_order else NULL,
          palette = pal
        ), extra_style, extra_p))

        p <- .apply_palette_if_missing(p, pal)
        p <- .apply_plot_font_size(p, plot_font, strip_size = strip_font)
        p <- .apply_p_label_size(p, p_label_font)
        p + ggplot2::labs(title = NULL, y = y_axis_label) +
          ggplot2::theme(legend.position = "none")
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

      plot_type <- tolower(as.character(.cfg("plot_type") %||% "dot"))[1]
      fun <- get_plot_fun(plot_type)
      group_col <- .selected_group_col()

      adv <- .get_adv_or_default(se_lipid, settings_reactive = settings_reactive)
      facet_var <- adv$facet_var %||% ""
      x_order <- adv$manual_order %||% character(0)
      groups  <- .class_levels_from_se(se_lipid, group_col)
      pal     <- .palette_for_groups(groups, adv)
      plot_font   <- .safe_plot_font_size(adv$plot_font_size %||% 12)
      strip_font  <- .safe_strip_font_size(adv$strip_font_size %||% plot_font, default = plot_font)
      p_label_font <- .safe_p_label_font_size(adv$p_label_font_size %||% 3.5)
      y_axis_label <- trimws(as.character(adv$lipid_y_axis_label %||% "Abundance"))
      if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"
      extra_style <- .plot_style_args_from_adv(adv, plot_type)
      extra_p     <- .pvalue_args_from_adv(adv)

      p <- do.call(fun, c(list(
        se = se_lipid, feature_id = fid, x_var = group_col,
        facet_var = if (nzchar(facet_var)) facet_var else NULL,
        x_order = if (length(x_order)) x_order else NULL,
        palette = pal
      ), extra_style, extra_p))

      p <- .apply_palette_if_missing(p, pal)
      p <- .apply_plot_font_size(p, plot_font, strip_size = strip_font)
      p <- .apply_p_label_size(p, p_label_font)
      p + ggplot2::labs(title = NULL, y = y_axis_label) +
        ggplot2::theme(legend.position = "none")
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
    list(
      project_export = function(graph = NULL, network_id = NULL) {
        nets <- rv_nets$map
        id <- rv_selected()
        if (!is.null(graph) && !is.null(network_id) && !is.null(nets[[network_id]]))
          nets[[network_id]]$elements_work <- .ensure_positions_for_preset(.cyjs_to_elements(graph))
        nets <- lapply(nets, function(item) {
          list(id = item$id, name = item$name,
               elements_work = .mslm_clear_projection_elements(item$elements_work))
        })
        list(map = nets, selected = id, settings = applied_settings())
      },
      project_restore = function(state) {
        # Prevent the multiomics default-network observer from replacing saved edits.
        project_loaded(TRUE)
        last_multiomics <<- isTRUE(state$settings$include_multiomics)
        examples_loaded <<- TRUE
        selected_node(NULL)
        client_graph$id <- NULL
        client_graph$elements <- NULL
        projection_state$map <- list()
        rv_diff_border$map <- list()
        rv_enrich_state$map <- list()
        enrichment_enabled(FALSE)
        applied_settings(utils::modifyList(default_settings, state$settings, keep.null = TRUE))
        rv_nets$map <- state$map
        rv_selected(state$selected)
        session$sendCustomMessage("cy_fc_action", list(container_id = container_id, action = "clear"))
        session$sendCustomMessage("cy_reset_projection", list(container_id = container_id))
        if (is.null(state$selected)) session$sendCustomMessage("cy_init", list(
          container_id = container_id, elements = list(nodes = list(), edges = list()),
          layout = "preset", changed_input = ns("cy_changed"), selected_input = ns("node_selected")))
        shiny::updateSelectInput(session, "net_select",
          choices = setNames(names(state$map) %||% character(0), vapply(state$map, `[[`, character(1), "name")),
          selected = state$selected %||% character(0))
        for (key in names(state$settings)) {
          value <- state$settings[[key]]
          if (is.null(value)) next
          if (is.logical(value)) shiny::updateCheckboxInput(session, key, value = value)
          else if (key == "plot_type") shiny::updateRadioButtons(session, key, selected = value)
          else if (key == "chain_codes") shiny::updateSelectizeInput(session, key, selected = value)
          else if (is.numeric(value)) shiny::updateNumericInput(session, key, value = value)
          else if (key %in% c("chain_carbon", "chain_db", "chain_oxygen"))
            shiny::updateTextInput(session, key, value = value)
          else shiny::updateSelectInput(session, key, selected = value)
        }
      }
    )

  })
}
