# Standalone REST service. No Shiny state is shared with these jobs.
.mslm_api_abort <- function(status, message) {
  stop(structure(list(message = message, call = NULL, status = status),
                 class = c("mslm_api_error", "error", "condition")))
}

.mslm_api_json <- function(res, value, status = 200L) {
  res$status <- status
  res$setHeader("Content-Type", "application/json; charset=utf-8")
  res$setHeader("Cache-Control", "no-store")
  res$body <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA)
  res
}

# State files are immutable. Publishing by rename keeps readers from seeing
# partially written JSON and works on both Windows and Unix.
.mslm_api_record <- function(dir, state, values = list()) {
  record <- c(list(status = state,
                   time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")), values)
  target <- file.path(dir, paste0(state, ".json"))
  tmp <- tempfile("record-", tmpdir = dir)
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(record, tmp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  if (!file.rename(tmp, target)) stop("Could not publish job state.")
  invisible(record)
}

.mslm_api_state <- function(dir) {
  for (state in c("failed", "succeeded", "running", "queued")) {
    path <- file.path(dir, paste0(state, ".json"))
    if (file.exists(path)) return(jsonlite::read_json(path, simplifyVector = FALSE))
  }
  NULL
}

.mslm_api_config <- function(bytes, extension) {
  bad <- function(message) .mslm_api_abort(400L, message)
  cfg <- tryCatch(yaml::yaml.load(rawToChar(bytes), eval.expr = FALSE),
                  error = function(e) bad("config must be valid YAML."))
  mapping <- function(x) is.list(x) && (length(x) == 0L ||
    (!is.null(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))))
  if (!mapping(cfg)) bad("config must contain a YAML mapping.")
  allowed <- list(
    input = c("format", "lipidomics", "file", "mztab", "annotation_cols", "header_rows",
              "data_start_row", "identifier", "use_rgoslin_if_no_ontology"),
    analysis = c("normalization", "aggregation", "group_column", "include_groups",
                 "exclude_groups", "acyl_chain_filter", "y_axis_label"),
    pathway = c("timeout_seconds", "plot_type"),
    plot = c("type", "x_order", "font_size", "strip_font_size", "p_label_font_size",
             "y_axis_label", "svg_width", "svg_height", "palette", "dot", "box", "violin"),
    output = c("directory", "pdf", "paper_size", "orientation", "palette", "format", "llm_features"),
    measurements = c("include_subsets", "acyl_chains", "unit")
  )
  if (length(setdiff(names(cfg), names(allowed)))) bad("Unknown config section.")
  for (section in names(allowed)) {
    if (is.null(cfg[[section]])) cfg[[section]] <- list()
    if (!mapping(cfg[[section]])) bad(paste(section, "must be a mapping."))
    unknown <- setdiff(names(cfg[[section]]), allowed[[section]])
    if (length(unknown)) bad(paste0("Unsupported API setting: ", section, ".", unknown[1]))
  }
  acyl <- cfg$analysis$acyl_chain_filter
  if (!is.null(acyl) && (!mapping(acyl) ||
      length(setdiff(names(acyl), c("chains", "match", "lipid_name_column"))))) {
    bad("Unsupported analysis.acyl_chain_filter settings (custom rules are not accepted).")
  }
  format <- if (extension == "csv") "msdial" else "mztab-m"
  supplied <- cfg$input$format
  if (!is.null(supplied) && (!is.character(supplied) || length(supplied) != 1L ||
      !tolower(gsub("_", "-", supplied)) %in% c(format, if (format == "mztab-m") "mztab"))) {
    bad("input.format does not match the uploaded file extension.")
  }
  cfg$input$format <- format
  # CLI paths are intentionally discarded; the API owns every input/output path.
  cfg$input[c("lipidomics", "file", "mztab")] <- NULL
  cfg$output[c("directory", "pdf")] <- NULL
  cfg$output$format <- cfg$output$format %||% "pdf"
  fmt <- cfg$output$format
  if (!is.character(fmt) || !length(fmt) || anyNA(fmt) || anyDuplicated(fmt) ||
      !all(fmt %in% c("pdf", "jsonld", "llm", "both")) || (length(fmt) > 1L && "both" %in% fmt)) {
    bad("output.format must be pdf, jsonld, llm, both, or a list such as [pdf, jsonld, llm].")
  }
  if (any(fmt %in% c("jsonld", "both")) && !exists("mztab_pathway_measurements", mode = "function")) {
    bad("Requested output format is not available in this distribution.")
  }
  if (!is.null(cfg$output$llm_features) && (!is.logical(cfg$output$llm_features) ||
      length(cfg$output$llm_features) != 1L || is.na(cfg$output$llm_features))) {
    bad("output.llm_features must be true or false.")
  }
  cfg$measurements$include_subsets <- cfg$measurements$include_subsets %||% FALSE
  enabled <- cfg$measurements$include_subsets
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    bad("measurements.include_subsets must be true or false.")
  }
  chains <- cfg$measurements$acyl_chains
  if (!is.null(chains)) {
    if (!is.character(chains) || !length(chains) || anyNA(chains) ||
        any(!grepl("^(?:O-|P-)?[0-9]+:[0-9]+(?:;O[0-9]*)?$", chains, perl = TRUE))) {
      bad("measurements.acyl_chains must contain chain strings such as '18:0'.")
    }
    if (!enabled) bad("measurements.acyl_chains requires include_subsets: true.")
    cfg$measurements$acyl_chains <- unique(chains)
  }
  if (!is.null(cfg$measurements$unit) && (!is.character(cfg$measurements$unit) ||
      length(cfg$measurements$unit) != 1L || is.na(cfg$measurements$unit) ||
      !nzchar(cfg$measurements$unit))) bad("measurements.unit must be a non-empty string.")
  bounded <- function(x, lower, upper, label) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < lower || x > upper) {
      bad(paste(label, "must be between", lower, "and", upper))
    }
  }
  if (!is.null(cfg$pathway$timeout_seconds)) bounded(cfg$pathway$timeout_seconds, 1, 300, "pathway.timeout_seconds")
  for (key in c("svg_width", "svg_height")) {
    if (!is.null(cfg$plot[[key]])) bounded(cfg$plot[[key]], 0.5, 20, paste0("plot.", key))
  }
  cfg
}

.mslm_api_upload <- function(req, max_bytes) {
  type <- req$HTTP_CONTENT_TYPE
  if (is.null(type)) type <- req$CONTENT_TYPE
  if (is.null(type) || !grepl("^multipart/form-data\\s*;", type, ignore.case = TRUE)) {
    .mslm_api_abort(415L, "Use multipart/form-data with input and config files.")
  }
  if (length(req$bodyRaw) > max_bytes) .mslm_api_abort(413L, "Upload exceeds the configured limit.")
  match <- regexec('boundary=(?:"([^"]+)"|([^;[:space:]]+))', type, perl = TRUE, ignore.case = TRUE)
  captures <- regmatches(type, match)[[1]]
  if (length(captures) < 3L) .mslm_api_abort(400L, "Missing multipart boundary.")
  boundary <- if (nzchar(captures[2])) captures[2] else captures[3]
  parts <- tryCatch(webutils::parse_multipart(req$bodyRaw, boundary),
                    error = function(e) .mslm_api_abort(400L, "Malformed multipart upload."))
  # Parse only bytes; never invoke automatic YAML/RDS/CSV parsers on file parts.
  labels <- vapply(parts, function(x) if (is.null(x$name)) "" else x$name, character(1))
  if (length(parts) != 2L || anyDuplicated(labels) || !setequal(labels, c("input", "config"))) {
    .mslm_api_abort(400L, "Send exactly one input file and one config file.")
  }
  names(parts) <- labels
  for (part in parts) {
    if (is.null(part$filename) || !nzchar(part$filename) || !is.raw(part$value) || !length(part$value)) {
      .mslm_api_abort(400L, "input and config must be non-empty file uploads.")
    }
  }
  ext <- tolower(tools::file_ext(parts$input$filename))
  if (!ext %in% c("csv", "mztab")) .mslm_api_abort(400L, "input must be an MS-DIAL .csv or .mzTab file.")
  if (length(parts$config$value) > 1024^2) .mslm_api_abort(413L, "config must not exceed 1 MiB.")
  list(input = parts$input$value, extension = ext,
       config = .mslm_api_config(parts$config$value, ext))
}

.mslm_api_worker <- function(dir) {
  request <- readRDS(file.path(dir, "request.rds"))
  config_path <- file.path(dir, "config.yml")
  cfg <- yaml::read_yaml(config_path)
  format <- .mslm_api_formats(cfg$output$format)
  input_path <- file.path(dir, request$input)
  artifact_dir <- file.path(dir, "artifacts")
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  result <- list(artifacts = character())
  if ("pdf" %in% format) {
    # Only server-generated paths and sanitized configuration reach the CLI.
    result <- render_pathway_from_config(config_path, input_path = input_path,
      output_path = artifact_dir)
  }
  prepared <- if (any(c("jsonld", "llm") %in% format)) {
    .mslm_prepare_pathway_data(cfg$input, cfg$analysis, cfg$input$format, input_path)
  }
  if ("jsonld" %in% format) {
    exported <- .mslm_api_measurements(cfg, input_path, artifact_dir, basename(dir), prepared)
    result$artifacts <- c(result$artifacts, exported$artifacts)
    result$measurements <- exported$summary
  }
  if ("llm" %in% format) {
    networks <- .mslm_api_networks()
    result$llm <- list()
    for (pw in names(networks)) {
      out <- file.path(artifact_dir, paste0(pw, "-llm-summary.json"))
      summary <- .mslm_llm_summary(prepared, networks[[pw]], pw, out,
        dataset = list(id = basename(dir), input_format = cfg$input$format,
          normalization = prepared$normalization, aggregation = cfg$analysis$aggregation %||% "sum",
          unit = cfg$measurements$unit %||% "unspecified", group_column = prepared$group_col,
          ignored_exclude_groups = as.list(prepared$ignored_exclude_groups)),
        aggregate = cfg$analysis$aggregation %||% "sum",
        include_subsets = isTRUE(cfg$measurements$include_subsets),
        acyl_chains = cfg$measurements$acyl_chains %||% character(),
        include_features = isTRUE(cfg$output$llm_features))
      result$artifacts <- c(result$artifacts, out)
      result$llm[[pw]] <- list(lipids = length(summary$lipids),
        measured_lipids = sum(vapply(summary$lipids, function(l) is.null(l$status), logical(1))),
        reactions = length(summary$reactions),
        annotated_reactions = sum(vapply(summary$reactions, function(r) !is.null(r$enzymes), logical(1))))
    }
  }
  if (is.null(result$ignored_exclude_groups) && !is.null(prepared)) {
    result$ignored_exclude_groups <- prepared$ignored_exclude_groups
  }
  result$artifacts <- basename(result$artifacts)
  result$pdf <- NULL
  jsonlite::write_json(result, file.path(dir, "result.json"), auto_unbox = TRUE,
                       null = "null", pretty = TRUE)
  invisible(NULL)
}

# "both" is shorthand for PDF plus JSON-LD; lists select any combination.
.mslm_api_formats <- function(format) {
  format <- format %||% "pdf"
  unique(unlist(lapply(format, function(f) if (identical(f, "both")) c("pdf", "jsonld") else f)))
}

.mslm_api_networks <- function() {
  networks <- c(remodeling = "remodeling_lipidonly.cyjs",
    ceramide = "ceramidepathway_lipidonly.cyjs", global = "global.cyjs")
  stats::setNames(file.path(.mslm_example_dir(), networks), names(networks))
}

.mslm_api_measurements <- function(cfg, input_path, artifact_dir, dataset_id, prepared = NULL) {
  if (is.null(prepared)) {
    prepared <- .mslm_prepare_pathway_data(cfg$input, cfg$analysis, cfg$input$format, input_path)
  }
  networks <- .mslm_api_networks()
  enabled <- isTRUE(cfg$measurements$include_subsets)
  chains <- if (enabled) cfg$measurements$acyl_chains else character()
  outputs <- file.path(artifact_dir, paste0(names(networks), "-measurements.json"))
  summaries <- list()
  for (i in seq_along(networks)) {
    out <- mztab_pathway_measurements(prepared$se, networks[[i]],
      output_file = outputs[i], group_col = prepared$group_col,
      acyl_chains = chains, aggregate = cfg$analysis$aggregation %||% "sum",
      dataset_id = paste0(dataset_id, "-", names(networks)[i]),
      normalization = prepared$normalization, unit = cfg$measurements$unit %||% "unspecified")
    summaries[[names(networks)[i]]] <- list(
      measured_nodes = sum(out$node_mapping$status == "measured"),
      subset_chains = unname(as.list(out$acyl_coverage$acyl_chain)))
  }
  list(artifacts = outputs, summary = list(format = "jsonld", include_subsets = enabled,
    samples = ncol(prepared$se), features = nrow(prepared$se),
    aggregation = cfg$analysis$aggregation %||% "sum", normalization = prepared$normalization,
    ignored_exclude_groups = prepared$ignored_exclude_groups, pathways = summaries))
}

.mslm_api_manager <- function(data_dir, job_timeout, max_pending, worker_script) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  lock <- filelock::lock(file.path(root, "service.lock"), timeout = 0)
  if (is.null(lock)) stop("Another API service is already using data_dir.")
  ready <- FALSE
  on.exit(if (!ready) filelock::unlock(lock), add = TRUE)
  active <- NULL
  closed <- FALSE
  jobs <- function() {
    dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
    dirs[grepl("^job_[a-f0-9]{32}$", basename(dirs)) &
           file.exists(file.path(dirs, "queued.json"))]
  }
  # A job that was running at shutdown is never silently rerun.
  for (dir in jobs()) {
    state <- .mslm_api_state(dir)
    if (!is.null(state) && identical(state$status, "running")) {
      .mslm_api_record(dir, "failed", list(error = "Service restarted during analysis; submit the job again."))
    }
  }
  tick <- function() {
    if (closed) return(invisible(NULL))
    if (!is.null(active)) {
      p <- active$process
      expired <- as.numeric(difftime(Sys.time(), active$started, units = "secs")) > job_timeout
      if (expired || !p$is_alive()) {
        if (expired && p$is_alive()) p$kill_tree()
        result <- file.path(active$dir, "result.json")
        if (!expired && identical(p$get_exit_status(), 0L) && file.exists(result)) {
          .mslm_api_record(active$dir, "succeeded")
        } else {
          .mslm_api_record(active$dir, "failed", list(error = if (expired)
            "Analysis exceeded the job timeout." else "Analysis failed; consult the server worker.log."))
        }
        active <<- NULL
      }
    }
    if (is.null(active)) {
      pending <- Filter(function(dir) identical(.mslm_api_state(dir)$status, "queued"), jobs())
      if (length(pending)) {
        times <- vapply(pending, function(dir) .mslm_api_state(dir)$time, character(1))
        dir <- pending[order(times)][1]
        .mslm_api_record(dir, "running")
        tryCatch({
          # Preserve native runtime settings (PATH, temporary directories and
          # R environment), overriding only the selected library paths.
          worker_env <- Sys.getenv()
          worker_env["R_LIBS"] <- paste(.libPaths(), collapse = .Platform$path.sep)
          p <- processx::process$new(file.path(R.home("bin"), "Rscript"),
            c("--vanilla", worker_script, dir),
            env = worker_env,
            wd = dir, stdout = file.path(dir, "worker.log"), stderr = "2>&1",
            cleanup = TRUE, cleanup_tree = TRUE, supervise = TRUE)
          active <<- list(process = p, dir = dir, started = Sys.time())
        }, error = function(e) {
          .mslm_api_record(dir, "failed", list(error = "Could not start analysis worker."))
          message(conditionMessage(e))
        })
      }
    }
    invisible(NULL)
  }
  submit <- function(upload) {
    force(upload)
    n <- sum(vapply(jobs(), function(dir) .mslm_api_state(dir)$status %in% c("queued", "running"), logical(1)))
    if (n >= max_pending) .mslm_api_abort(429L, "Job queue is full; retry later.")
    repeat {
      id <- paste0("job_", paste(sample(c(letters[1:6], 0:9), 32L, replace = TRUE), collapse = ""))
      dir <- file.path(root, id)
      if (dir.create(dir, showWarnings = FALSE)) break
      if (!dir.exists(dir)) stop("Cannot create job directory.")
    }
    published <- FALSE
    on.exit(if (!published) unlink(dir, recursive = TRUE), add = TRUE)
    input <- paste0("input.", upload$extension)
    writeBin(upload$input, file.path(dir, input))
    yaml::write_yaml(upload$config, file.path(dir, "config.yml"))
    saveRDS(list(input = input), file.path(dir, "request.rds"))
    .mslm_api_record(dir, "queued")
    published <- TRUE
    id
  }
  lookup <- function(id) {
    if (!grepl("^job_[a-f0-9]{32}$", id)) .mslm_api_abort(404L, "Job not found.")
    dir <- file.path(root, id)
    if (is.null(.mslm_api_state(dir))) .mslm_api_abort(404L, "Job not found.")
    dir
  }
  close <- function() {
    if (closed) return(invisible(NULL))
    closed <<- TRUE
    on.exit(filelock::unlock(lock), add = TRUE)
    if (!is.null(active)) {
      if (active$process$is_alive()) active$process$kill_tree()
      .mslm_api_record(active$dir, "failed", list(error = "Service stopped during analysis; submit the job again."))
      active <<- NULL
    }
    invisible(NULL)
  }
  ready <- TRUE
  list(tick = tick, submit = submit, lookup = lookup, close = close)
}

# Browser console for the API. Only these bundled files are ever served.
.mslm_api_ui_files <- c("index.html" = "text/html; charset=utf-8",
                        "js-yaml.min.js" = "application/javascript; charset=utf-8")

.mslm_api_ui_path <- function(req) {
  identical(req$PATH_INFO, "/") || grepl("^/ui/", req$PATH_INFO)
}

.mslm_api_router <- function(manager, token, max_bytes,
    ui_dir = system.file("api-ui", package = "MSLipidMapper")) {
  pr <- plumber::pr()
  # Authentication and size checks precede manual multipart parsing.
  # Some Plumber releases register parser_none with the invalid regex "*".
  # A private catch-all parser also prevents file contents becoming route args.
  plumber::register_parser("mslm_raw_upload", function() function(value, ...) list(),
                           regex = ".*", verbose = FALSE)
  plumber::pr_set_parsers(pr, "mslm_raw_upload")
  guard <- function(fun) function(req, res, ...) {
    tryCatch(fun(req, res, ...), mslm_api_error = function(e) {
      .mslm_api_json(res, list(error = conditionMessage(e)), e$status)
    }, error = function(e) {
      message("REST API error: ", conditionMessage(e))
      .mslm_api_json(res, list(error = "Internal server error."), 500L)
    })
  }
  pr$filter("auth", guard(function(req, res) {
    # The console page itself holds no data; the API calls it makes are authenticated.
    if (nzchar(token) && !identical(req$PATH_INFO, "/healthz") && !.mslm_api_ui_path(req) &&
        !identical(req$HTTP_AUTHORIZATION, paste("Bearer", token))) {
      res$setHeader("WWW-Authenticate", "Bearer")
      .mslm_api_abort(401L, "A valid Bearer token is required.")
    }
    if (length(req$bodyRaw) > max_bytes) .mslm_api_abort(413L, "Upload exceeds the configured limit.")
    plumber::forward()
  }))
  ui <- function(res, name) {
    path <- file.path(ui_dir, name)
    if (!nzchar(ui_dir) || !name %in% names(.mslm_api_ui_files) || !file.exists(path)) {
      .mslm_api_abort(404L, "Not found.")
    }
    res$setHeader("Content-Type", .mslm_api_ui_files[[name]])
    res$setHeader("Cache-Control", "no-cache")
    res$setHeader("X-Content-Type-Options", "nosniff")
    res$body <- readBin(path, "raw", n = file.info(path)$size)
    res
  }
  pr$handle("GET", "/", guard(function(req, res) ui(res, "index.html")))
  pr$handle("GET", "/ui/<name>", guard(function(req, res, name) ui(res, name)))
  pr$handle("GET", "/healthz", guard(function(req, res) .mslm_api_json(res, list(ok = TRUE))))
  pr$handle("POST", "/v1/jobs", guard(function(req, res) {
    id <- manager$submit(.mslm_api_upload(req, max_bytes))
    url <- paste0("/v1/jobs/", id)
    res$setHeader("Location", url)
    .mslm_api_json(res, list(job_id = id, status = "queued", status_url = url), 202L)
  }))
  pr$handle("GET", "/v1/jobs/<id>", guard(function(req, res, id) {
    dir <- manager$lookup(id)
    state <- .mslm_api_state(dir)
    value <- c(list(job_id = id), state)
    if (identical(state$status, "succeeded")) {
      value$result <- jsonlite::read_json(file.path(dir, "result.json"))
      value$artifacts_url <- paste0("/v1/jobs/", id, "/artifacts")
    }
    .mslm_api_json(res, value)
  }))
  artifacts <- function(id) {
    dir <- manager$lookup(id)
    if (!identical(.mslm_api_state(dir)$status, "succeeded")) {
      .mslm_api_abort(409L, "Artifacts are available only after successful analysis.")
    }
    files <- list.files(file.path(dir, "artifacts"), pattern = "\\.(pdf|json)$", full.names = FALSE)
    list(dir = dir, files = files)
  }
  pr$handle("GET", "/v1/jobs/<id>/artifacts", guard(function(req, res, id) {
    a <- artifacts(id)
    items <- lapply(a$files, function(name) list(name = name,
      size_bytes = file.info(file.path(a$dir, "artifacts", name))$size,
      url = paste0("/v1/jobs/", id, "/artifacts/", name)))
    .mslm_api_json(res, list(job_id = id, artifacts = items))
  }))
  pr$handle("GET", "/v1/jobs/<id>/artifacts/<name>", guard(function(req, res, id, name) {
    a <- artifacts(id)
    if (!name %in% a$files || !grepl("^[A-Za-z0-9_-]+\\.(pdf|json)$", name)) {
      .mslm_api_abort(404L, "Artifact not found.")
    }
    path <- file.path(a$dir, "artifacts", name)
    res$setHeader("Content-Type", if (endsWith(name, "-measurements.json")) "application/ld+json" else
      if (endsWith(name, ".json")) "application/json" else "application/pdf")
    res$setHeader("Content-Disposition", paste0('attachment; filename="', name, '"'))
    res$setHeader("Cache-Control", "no-store")
    res$body <- readBin(path, "raw", n = file.info(path)$size)
    res
  }))
  pr
}

#' Run the standalone file-upload REST API
#'
#' Runs one analysis worker at a time. Queued jobs and results are persisted;
#' interrupted running jobs are marked failed on restart. An empty token disables
#' authentication for lab-network operation. One service may own a data directory.
#' @param host Interface to listen on; defaults to loopback.
#' @param port HTTP port.
#' @param data_dir Persistent directory for jobs, inputs, logs and artifacts.
#' @param token Empty string for no authentication (default), or a shared API
#'   token of at least 16 characters to enable Bearer authentication.
#' @param max_upload_mb Maximum total request body size in MiB. Also configure
#'   a matching body limit at the reverse proxy to bound ingress memory.
#' @param job_timeout Maximum elapsed seconds per analysis worker.
#' @param max_pending Maximum combined queued and running jobs.
#' @return Invisibly, the Plumber run result.
#' @export
run_mslipidmapper_api <- function(host = "127.0.0.1", port = 8000L,
    data_dir = "mslipidmapper-jobs", token = Sys.getenv("MSLIPIDMAPPER_API_TOKEN"),
    max_upload_mb = 100, job_timeout = 900, max_pending = 32L) {
  if (!is.character(token) || length(token) != 1L || is.na(token) ||
      (nzchar(token) && nchar(token) < 16L)) {
    stop("MSLIPIDMAPPER_API_TOKEN must be empty (no authentication) or at least 16 characters.")
  }
  for (value in list(port, max_upload_mb, job_timeout, max_pending)) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value <= 0) {
      stop("Port and resource limits must be positive finite numbers.")
    }
  }
  if (port > 65535 || port != as.integer(port) || max_pending != floor(max_pending)) {
    stop("Invalid port or max_pending.")
  }
  script <- system.file("scripts", "mslipidmapper-api-worker.R", package = "MSLipidMapper")
  if (!nzchar(script)) stop("Install this version of MSLipidMapper before starting the API.")
  manager <- .mslm_api_manager(data_dir, job_timeout, max_pending, script)
  on.exit(manager$close(), add = TRUE)
  alive <- TRUE
  on.exit(alive <- FALSE, add = TRUE)
  pump <- function() {
    if (!alive) return(invisible(NULL))
    tryCatch(manager$tick(), error = function(e) message("Job scheduler: ", conditionMessage(e)))
    later::later(pump, delay = 0.5)
  }
  pump()
  pr <- .mslm_api_router(manager, token, max_upload_mb * 1024^2)
  pr$run(host = host, port = as.integer(port), swagger = FALSE)
}
