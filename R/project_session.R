# Portable snapshots contain data, never Shiny environments or temporary paths.
.mslm_project_version <- function() {
  tryCatch(as.character(utils::packageVersion("MSLipidMapper")),
           error = function(e) "development")
}

.mslm_project_validate <- function(x) {
  fail <- function(message) stop(paste("Invalid project:", message), call. = FALSE)
  if (!is.list(x) || !identical(x$format, "MSLipidMapper-project")) fail("unrecognized format.")
  if (!identical(x$schema_version, 1L)) fail("unsupported schema version.")
  if (!is.character(x$app_version) || length(x$app_version) != 1L ||
      !is.character(x$saved_at) || length(x$saved_at) != 1L) fail("missing metadata.")
  sections <- c("upload", "normalization", "feature", "network")
  if (!is.list(x$state) || !all(sections %in% names(x$state))) fail("missing state sections.")
  if (!all(vapply(x$state[sections], is.list, logical(1)))) fail("invalid state sections.")
  check_se <- function(se) {
    if (is.null(se)) return(invisible(NULL))
    if (!methods::is(se, "SummarizedExperiment")) fail("expected SummarizedExperiment data.")
    methods::validObject(se)
    if (!"abundance" %in% SummarizedExperiment::assayNames(se)) fail("abundance assay missing.")
  }
  u <- x$state$upload
  if (!all(c("full", "selected", "tx", "tx_raw") %in% names(u))) fail("incomplete upload state.")
  invisible(lapply(u[c("full", "selected", "tx", "tx_raw")], check_se))
  n <- x$state$normalization
  check_se(n$se)
  if (!is.logical(n$ready) || length(n$ready) != 1L || is.na(n$ready)) fail("normalization readiness.")
  if (isTRUE(n$ready) && (is.null(n$se) || is.null(u$selected))) fail("normalized data missing.")
  if (isTRUE(n$ready) && (!identical(dimnames(n$se), dimnames(u$selected)) ||
                         !identical(dim(n$se), dim(u$selected)))) fail("normalized data dimensions.")
  plain <- function(v, depth = 0L) {
    if (depth > 100L) fail("settings nesting too deep.")
    if (is.null(v)) return(invisible(NULL))
    if (is.object(v) && !is.data.frame(v)) fail("unsupported settings object.")
    if (is.list(v)) { for (child in v) plain(child, depth + 1L) }
    else if (!is.atomic(v)) fail("settings must contain only data.")
  }
  plain(n$settings)
  if (isTRUE(n$ready) && is.null(n$settings)) fail("normalization settings missing.")
  if (!is.null(n$settings)) {
    if (!is.list(n$settings) || !is.character(n$settings$method) ||
        length(n$settings$method) != 1L || is.na(n$settings$method) ||
        !n$settings$method %in% c("none", "log2", "sum", "median", "zscore")) fail("normalization method.")
    if (!is.null(n$settings$offset) && (!is.numeric(n$settings$offset) ||
        length(n$settings$offset) != 1L || !is.finite(n$settings$offset) ||
        n$settings$offset <= 0)) fail("normalization offset.")
  }
  f <- x$state$feature
  if (!identical(f$dataset, "lipid") && !identical(f$dataset, "gene")) fail("feature dataset.")
  if (!is.list(f$adv)) fail("advanced settings.")
  plain(f)
  check_settings <- function(values, defaults) {
    if (length(values) && (is.null(names(values)) || anyDuplicated(names(values)))) fail("setting names.")
    for (key in intersect(names(values), names(defaults))) {
      value <- values[[key]]; default <- defaults[[key]]
      if (is.null(value)) next
      if (is.data.frame(default)) {
        if (!is.data.frame(value) || !all(names(default) %in% names(value))) fail(paste("setting", key))
      } else {
        same_type <- (is.numeric(default) && is.numeric(value)) ||
          (is.character(default) && is.character(value)) ||
          (is.logical(default) && is.logical(value))
        if (!same_type || (length(default) == 1L && length(value) != 1L) ||
            anyNA(value) || (is.numeric(value) && any(!is.finite(value)))) fail(paste("setting", key))
      }
    }
  }
  check_settings(f$adv, .default_feature_adv())
  if (!is.null(f$panel_styles)) {
    if (!is.list(f$panel_styles) || (length(f$panel_styles) &&
        (is.null(names(f$panel_styles)) || anyDuplicated(names(f$panel_styles))))) fail("panel styles.")
    for (style in f$panel_styles) {
      if (!is.list(style)) fail("panel style.")
      check_settings(style, .default_feature_adv())
    }
  }
  net <- x$state$network
  if (!is.list(net$map) || !is.list(net$settings)) fail("network state.")
  plain(net)
  network_template <- list(
    plot_type = "dot", include_multiomics = FALSE, agg_fun = "sum", hm_top_n = 20,
    diff_group_a = "", diff_group_b = "", diff_method = "wilcox", diff_use_padj = FALSE,
    diff_alpha = 0.05, fc_scale = "abundance", fc_top_n = 5, chain_codes = character(0),
    chain_require_all = FALSE, chain_exclude_odd = FALSE, chain_carbon = "", chain_db = "", chain_oxygen = "")
  if (length(setdiff(names(net$settings), names(network_template)))) fail("unknown network setting.")
  check_settings(net$settings, network_template)
  ids <- names(net$map)
  if (length(net$map) && (is.null(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))) fail("network IDs.")
  for (id in ids) {
    item <- net$map[[id]]
    if (!is.list(item) || !identical(item$id, id) ||
        !is.character(item$name) || length(item$name) != 1L ||
        !is.list(item$elements_work) || !is.list(item$elements_work$nodes) ||
        !is.list(item$elements_work$edges)) fail("network elements.")
    nodes <- item$elements_work$nodes
    node_ids <- vapply(nodes, function(node) {
      if (!is.list(node) || !is.list(node$data) || !is.character(node$data$id) ||
          length(node$data$id) != 1L || is.na(node$data$id) || !nzchar(node$data$id)) fail("node ID.")
      if (!is.null(node$position) && (!is.list(node$position) ||
          !is.numeric(node$position$x) || length(node$position$x) != 1L || !is.finite(node$position$x) ||
          !is.numeric(node$position$y) || length(node$position$y) != 1L || !is.finite(node$position$y))) fail("node position.")
      node$data$id
    }, character(1))
    if (anyDuplicated(node_ids)) fail("duplicate node IDs.")
    for (edge in item$elements_work$edges) {
      if (!is.list(edge) || !is.list(edge$data) ||
          length(edge$data$source) != 1L || length(edge$data$target) != 1L ||
          !edge$data$source %in% node_ids || !edge$data$target %in% node_ids) fail("edge endpoints.")
    }
  }
  if (!is.null(net$selected) && (length(net$selected) != 1L || !net$selected %in% ids)) fail("selected network.")
  invisible(x)
}

.mslm_project_create <- function(upload, normalization, feature, network) {
  x <- list(format = "MSLipidMapper-project", schema_version = 1L,
            app_version = .mslm_project_version(),
            saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            state = list(upload = upload, normalization = normalization,
                         feature = feature, network = network))
  .mslm_project_validate(x)
  x
}

.mslm_project_read <- function(file) {
  x <- readRDS(file)
  .mslm_project_validate(x)
  x
}

.mslm_project_write <- function(x, file) {
  .mslm_project_validate(x)
  saveRDS(x, file, compress = "gzip", version = 3)
}

.mslm_project_ui <- function() {
  shiny::tagList(
    shiny::p("Save input data, normalization, shared Advanced settings and pathway networks/settings. Plots and projections are regenerated. Other analysis panels and Utility editor state are not included."),
    shiny::actionButton("project_prepare", "Prepare project file"),
    shiny::uiOutput("project_download_ui"),
    shiny::fileInput("project_file", "Restore project (.mslm.rds)", accept = ".rds"),
    shiny::helpText("Restore replaces the saved data/settings in this workspace. Other panel settings are not restored. Load only project files from trusted sources. Original source files are not required."),
    shiny::verbatimTextOutput("project_status"),
    shiny::tags$script(shiny::HTML('
      $(document).on("shiny:connected", function() {
        if (window.__mslm_project_handler) return;
        window.__mslm_project_handler = true;
        Shiny.addCustomMessageHandler("mslm_project_capture", function(p) {
          var cy = (window.__cy_instances || {})["net-cy"];
          Shiny.setInputValue("project_capture", {
            request: p.request, graph: cy ? cy.json() : null,
            network_id: ((window.__cy_state || {})["net-cy"] || {}).network_id || null
          }, {priority: "event"});
        });
      });
    '))
  )
}

.mslm_project_server <- function(input, output, session, upload, normalization, feature, network) {
  status <- shiny::reactiveVal("No project file prepared.")
  prepared <- shiny::reactiveVal(NULL)
  request <- 0L
  output$project_status <- shiny::renderText(status())
  output$project_download_ui <- shiny::renderUI({
    if (is.null(prepared())) return(NULL)
    shiny::tagList(shiny::downloadButton("project_download", "Download prepared project"),
      shiny::helpText("This is a snapshot at preparation time. Prepare again to include later changes."))
  })
  shiny::observeEvent(input$project_prepare, {
    request <<- request + 1L
    prepared(NULL)
    status("Capturing current pathway layout...")
    session$sendCustomMessage("mslm_project_capture", list(request = request))
  })
  shiny::observeEvent(input$project_capture, {
    if (!identical(as.integer(input$project_capture$request), request)) return()
    tryCatch({
      x <- .mslm_project_create(upload$project_export(), normalization$project_export(),
                               feature$project_export(), network$project_export(input$project_capture$graph, input$project_capture$network_id))
      prepared(x)
      status(paste("Prepared:", x$saved_at))
    }, error = function(e) status(paste("Save failed:", conditionMessage(e))))
  })
  output$project_download <- shiny::downloadHandler(
    filename = function() paste0("MSLipidMapper_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".mslm.rds"),
    contentType = "application/octet-stream",
    content = function(file) { shiny::req(prepared()); .mslm_project_write(prepared(), file) })
  shiny::observeEvent(input$project_file, {
    tryCatch({
      x <- .mslm_project_read(input$project_file$datapath)
      # Validation finishes before touching the active workspace. Data consumers
      # must see the new input before the normalized snapshot is marked ready.
      request <<- request + 1L
      restore_request <- request
      prepared(NULL)
      upload$project_restore(x$state$upload)
      session$onFlushed(function() {
        if (!identical(request, restore_request)) return()
        tryCatch(shiny::isolate({
          normalization$project_restore(x$state$normalization)
          feature$project_restore(x$state$feature)
          network$project_restore(x$state$network)
          prepared(NULL)
          version_note <- if (!identical(x$app_version, .mslm_project_version()))
            paste(" (created with app", x$app_version, ")") else ""
          status(paste0("Restored project saved at ", x$saved_at, version_note,
                        ". Regenerate plots and pathway projections as needed."))
        }), error = function(e) status(paste("Restore failed:", conditionMessage(e))))
      }, once = TRUE)
    }, error = function(e) status(paste("Restore failed:", conditionMessage(e))))
  })
}
