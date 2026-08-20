#' Render a pathway projection from a YAML parameter file
#'
#' This is the non-interactive entry point used by the MSLipidMapper command
#' line tool. Paths in the YAML file are resolved relative to that file.
#'
#' @param config Path to a YAML configuration file.
#' @param input_path Optional lipidomics input file that overrides the input
#'   path in the YAML file. Relative paths are resolved from the current
#'   working directory.
#' @param network_path Optional CYJS/JSON pathway file that overrides
#'   `pathway.network` in the YAML file. Relative paths are resolved from the
#'   current working directory.
#' @param output_path Optional output override. It is a PDF path for a custom
#'   network and a directory path for the three default pathways. Relative
#'   paths are resolved from the current working directory.
#' @return Invisibly, a summary list containing the output path and mapping
#'   counts.
#' @export
render_pathway_from_config <- function(config, input_path = NULL, network_path = NULL, output_path = NULL) {
  config <- normalizePath(config, winslash = "/", mustWork = TRUE)
  cfg <- yaml::read_yaml(config)
  if (!is.list(cfg)) stop("The parameter file must contain a YAML mapping.", call. = FALSE)

  base_dir <- dirname(config)
  resolve_cli_path <- function(x, must_exist) {
    x <- path.expand(as.character(x))
    if (!grepl("^(?:[A-Za-z]:[/\\\\]|/)", x)) x <- file.path(getwd(), x)
    normalizePath(x, winslash = "/", mustWork = must_exist)
  }
  resolve_path <- function(x, label, must_exist = TRUE) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop("Missing parameter: ", label, call. = FALSE)
    }
    x <- path.expand(as.character(x))
    if (!grepl("^(?:[A-Za-z]:[/\\\\]|/)", x)) x <- file.path(base_dir, x)
    x <- normalizePath(x, winslash = "/", mustWork = FALSE)
    if (must_exist && !file.exists(x)) stop(label, " does not exist: ", x, call. = FALSE)
    x
  }

  input <- cfg$input %||% list()
  analysis <- cfg$analysis %||% list()
  pathway <- cfg$pathway %||% list()
  plot_cfg <- cfg$plot %||% list()
  output <- cfg$output %||% list()

  input_format <- tolower(as.character(input$format %||% "msdial"))[1]
  input_format <- gsub("_", "-", input_format, fixed = TRUE)
  if (input_format %in% c("mztab", "mztab-m")) input_format <- "mztab-m"
  if (!input_format %in% c("msdial", "mztab-m")) {
    stop("Supported input.format values are: msdial, mztab-m.", call. = FALSE)
  }
  input_file <- input$lipidomics %||% input$file
  if (identical(input_format, "mztab-m")) input_file <- input$mztab %||% input_file
  if (!is.null(input_path)) {
    if (length(input_path) != 1L || is.na(input_path) || !nzchar(trimws(input_path))) {
      stop("input_path must be one non-empty file path.", call. = FALSE)
    }
    # CLI overrides are intentionally resolved from the caller's working
    # directory, not from the directory containing the YAML file.
    input_file <- resolve_cli_path(input_path, must_exist = TRUE)
  }
  input_label <- if (identical(input_format, "mztab-m")) "input.mztab (or input.file)" else "input.lipidomics (or input.file)"
  lipidomics_path <- resolve_path(input_file, input_label)
  if (!is.null(network_path)) {
    if (length(network_path) != 1L || is.na(network_path) || !nzchar(trimws(network_path))) {
      stop("network_path must be one non-empty CYJS/JSON file path.", call. = FALSE)
    }
    # Like the input override, a CLI-supplied network is relative to the
    # caller's working directory.
    pathway$network <- resolve_cli_path(network_path, must_exist = TRUE)
  }
  if (!is.null(output_path)) {
    if (length(output_path) != 1L || is.na(output_path) || !nzchar(trimws(output_path))) {
      stop("output_path must be one non-empty path.", call. = FALSE)
    }
    output_path <- resolve_cli_path(output_path, must_exist = FALSE)
  }
  custom_network <- !is.null(pathway$network) && length(pathway$network) == 1L && nzchar(trimws(pathway$network))
  if (custom_network) {
    network_paths <- c(custom = resolve_path(pathway$network, "pathway.network"))
    if (!is.null(output_path)) {
      # `--output result.pdf` selects a file, while `--output results/`
      # selects a directory and derives the PDF name from the CYJS file.
      output_is_pdf <- identical(tolower(tools::file_ext(output_path)), "pdf") && !dir.exists(output_path)
      selected_output <- if (output_is_pdf) {
        output_path
      } else {
        network_stem <- tools::file_path_sans_ext(basename(pathway$network))
        file.path(output_path, paste0(network_stem, ".pdf"))
      }
    } else {
      selected_output <- output$pdf %||% "pathway_projection.pdf"
    }
    output_paths <- c(custom = resolve_path(selected_output, "output.pdf", FALSE))
  } else {
    example_dir <- .mslm_example_dir()
    if (!nzchar(example_dir)) stop("Bundled pathway directory was not found.", call. = FALSE)
    defaults <- c(
      remodeling = "remodeling_lipidonly.cyjs",
      ceramide = "ceramidepathway_lipidonly.cyjs",
      global = "global.cyjs"
    )
    network_paths <- stats::setNames(file.path(example_dir, defaults), names(defaults))
    missing <- network_paths[!file.exists(network_paths)]
    if (length(missing)) stop("Bundled pathway file not found: ", paste(basename(missing), collapse = ", "), call. = FALSE)
    selected_output <- output_path %||% output$directory %||% "pathway_projection"
    output_dir <- resolve_path(selected_output, "output.directory", FALSE)
    output_paths <- file.path(output_dir, paste0(names(defaults), ".pdf"))
    names(output_paths) <- names(defaults)
  }
  dir.create(unique(dirname(output_paths)), recursive = TRUE, showWarnings = FALSE)

  if (identical(input_format, "msdial")) {
    annotation_cols <- input$annotation_cols %||% 1:35
    if (length(annotation_cols) == 1L && is.character(annotation_cols) && grepl("^[0-9]+:[0-9]+$", annotation_cols)) {
      bounds <- as.integer(strsplit(annotation_cols, ":", fixed = TRUE)[[1]])
      annotation_cols <- seq.int(bounds[1], bounds[2])
    }
    annotation_cols <- as.integer(unlist(annotation_cols))
    if (!length(annotation_cols) || anyNA(annotation_cols) || any(annotation_cols < 1L)) {
      stop("input.annotation_cols must be positive column numbers.", call. = FALSE)
    }
    se <- load_lipidomics_se(
      lipidomics_path,
      annotation_cols = annotation_cols,
      header_rows = as.integer(input$header_rows %||% 5L),
      data_start_row = as.integer(input$data_start_row %||% 6L)
    )
  } else {
    identifier <- tolower(as.character(input$identifier %||% "name"))[1]
    identifier <- match.arg(identifier, c("name", "mass", "sml_id", "feature"))
    parsed <- mztab_to_se(lipidomics_path, identifier = identifier, verbose = FALSE)
    use_rgoslin <- input$use_rgoslin_if_no_ontology %||% TRUE
    if (!is.logical(use_rgoslin) || length(use_rgoslin) != 1L || is.na(use_rgoslin)) {
      stop("input.use_rgoslin_if_no_ontology must be true or false.", call. = FALSE)
    }
    se <- mztab_se_to_lipidomics_se(
      parsed,
      use_rgoslin_if_no_ontology = use_rgoslin,
      verbose = FALSE
    )
  }
  group_col <- as.character(analysis$group_column %||% "class")[1]
  cd <- as.data.frame(SummarizedExperiment::colData(se), check.names = FALSE)
  if (!group_col %in% names(cd)) {
    stop("analysis.group_column not found in sample metadata: ", group_col, call. = FALSE)
  }
  groups <- as.character(cd[[group_col]])
  if (anyNA(groups) || any(!nzchar(trimws(groups)))) {
    stop("analysis.group_column contains missing or empty values.", call. = FALSE)
  }
  available_groups <- unique(groups)
  parse_group_filter <- function(x, label) {
    if (is.null(x)) return(character(0))
    value <- as.character(unlist(x, use.names = FALSE))
    if (!length(value) || anyNA(value) || any(!nzchar(trimws(value)))) {
      stop(label, " must contain non-empty group names.", call. = FALSE)
    }
    value <- unique(value)
    unknown <- setdiff(value, available_groups)
    if (length(unknown)) {
      stop(label, " contains unknown groups: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    value
  }
  include_groups <- parse_group_filter(analysis$include_groups, "analysis.include_groups")
  exclude_groups <- parse_group_filter(analysis$exclude_groups, "analysis.exclude_groups")
  keep <- rep(TRUE, length(groups))
  if (length(include_groups)) keep <- groups %in% include_groups
  if (length(exclude_groups)) keep <- keep & !groups %in% exclude_groups
  if (!any(keep)) stop("Group filtering removed every sample.", call. = FALSE)
  se <- se[, keep, drop = FALSE]
  groups <- groups[keep]

  normalization <- tolower(as.character(analysis$normalization %||% "none"))[1]
  se <- normalize_se(se, method = normalization)

  network_ext <- tolower(tools::file_ext(network_paths))
  if (any(!network_ext %in% c("cyjs", "json"))) stop("Only CYJS/JSON pathway files are supported.", call. = FALSE)
  elements_list <- lapply(network_paths, function(path) {
    .cyjs_to_elements(jsonlite::fromJSON(path, simplifyVector = FALSE))
  })

  agg_fun <- tolower(as.character(analysis$aggregation %||% "sum"))[1]
  se_class <- aggregate_to_class_se(se, fun = agg_fun)
  abundance <- as.matrix(SummarizedExperiment::assay(se_class, 1))
  requested_order <- as.character(unlist(plot_cfg$x_order %||% character(0)))
  requested_order <- requested_order[requested_order %in% unique(groups)]
  group_levels <- unique(c(requested_order, sort(setdiff(unique(groups), requested_order))))

  # Build exactly the same class-level node plots as the Shiny Pathway Mapping
  # module. The CLI only replaces the browser/Cytoscape composition step.
  plot_type <- tolower(as.character(plot_cfg$type %||% pathway$plot_type %||% "dot"))[1]
  plot_type <- match.arg(plot_type, c("dot", "violin", "box"))
  adv <- .get_adv_or_default(se)
  adv$manual_order <- group_levels
  adv$plot_font_size <- plot_cfg$font_size %||% adv$plot_font_size
  adv$strip_font_size <- plot_cfg$strip_font_size %||% adv$strip_font_size
  adv$p_label_font_size <- plot_cfg$p_label_font_size %||% adv$p_label_font_size
  if (is.list(plot_cfg$dot)) {
    adv$dot_point_size <- plot_cfg$dot$point_size %||% adv$dot_point_size
    adv$dot_jitter_width <- plot_cfg$dot$jitter_width %||% adv$dot_jitter_width
    adv$dot_alpha <- plot_cfg$dot$alpha %||% adv$dot_alpha
    adv$dot_show_median <- plot_cfg$dot$show_median %||% adv$dot_show_median
    adv$dot_median_size <- plot_cfg$dot$median_size %||% adv$dot_median_size
    adv$dot_median_width <- plot_cfg$dot$median_width %||% adv$dot_median_width
    adv$dot_median_color <- plot_cfg$dot$median_color %||% adv$dot_median_color
  }
  if (is.list(plot_cfg$box)) {
    adv$box_width <- plot_cfg$box$width %||% adv$box_width
    adv$box_alpha <- plot_cfg$box$alpha %||% adv$box_alpha
    adv$box_show_points <- plot_cfg$box$show_points %||% adv$box_show_points
    adv$box_point_size <- plot_cfg$box$point_size %||% adv$box_point_size
    adv$box_jitter_width <- plot_cfg$box$jitter_width %||% adv$box_jitter_width
    adv$box_point_alpha <- plot_cfg$box$point_alpha %||% adv$box_point_alpha
  }
  if (is.list(plot_cfg$violin)) {
    adv$violin_width <- plot_cfg$violin$width %||% adv$violin_width
    adv$violin_alpha <- plot_cfg$violin$alpha %||% adv$violin_alpha
    adv$violin_trim <- plot_cfg$violin$trim %||% adv$violin_trim
    adv$violin_show_points <- plot_cfg$violin$show_points %||% adv$violin_show_points
    adv$violin_point_size <- plot_cfg$violin$point_size %||% adv$violin_point_size
    adv$violin_jitter_width <- plot_cfg$violin$jitter_width %||% adv$violin_jitter_width
    adv$violin_point_alpha <- plot_cfg$violin$point_alpha %||% adv$violin_point_alpha
    adv$violin_show_median <- plot_cfg$violin$show_median %||% adv$violin_show_median
    adv$violin_median_size <- plot_cfg$violin$median_size %||% adv$violin_median_size
    adv$violin_median_color <- plot_cfg$violin$median_color %||% adv$violin_median_color
  }
  hues <- seq(15, 375, length.out = length(group_levels) + 1)[seq_along(group_levels)]
  plot_palette <- grDevices::hcl(h = hues, l = 65, c = 100)
  names(plot_palette) <- group_levels
  if (!is.null(adv$palette_map) && length(adv$palette_map)) {
    hits <- intersect(names(adv$palette_map), group_levels)
    plot_palette[hits] <- adv$palette_map[hits]
  }
  palette_cfg <- plot_cfg$palette %||% output$palette
  if (!is.null(palette_cfg)) {
    supplied <- unlist(palette_cfg, use.names = TRUE)
    if (!is.null(names(supplied)) && any(nzchar(names(supplied)))) {
      hits <- intersect(names(supplied), group_levels)
      plot_palette[hits] <- as.character(supplied[hits])
    } else {
      plot_palette[] <- rep(as.character(supplied), length.out = length(group_levels))
    }
  }

  labels_in_net <- unique(unlist(lapply(elements_list, function(elements) {
    vapply(elements$nodes, function(n) {
      d <- n$data %||% list()
      trimws(as.character(d$shared_name %||% d$label %||% ""))
    }, character(1))
  }), use.names = FALSE))
  target_classes <- intersect(labels_in_net, rownames(abundance))
  plot_fun <- get_plot_fun(plot_type)
  extra_style <- .plot_style_args_from_adv(adv, plot_type)
  extra_p <- .pvalue_args_from_adv(adv)
  node_plots <- stats::setNames(vector("list", length(target_classes)), target_classes)
  for (cls in target_classes) {
    p <- do.call(plot_fun, c(list(
      se = se_class,
      feature_id = cls,
      x_var = group_col,
      x_order = if (length(adv$manual_order)) adv$manual_order else NULL,
      palette = plot_palette
    ), extra_style, extra_p))
    p <- .apply_plot_font_size(
      p,
      .safe_plot_font_size(adv$plot_font_size %||% 12),
      .safe_strip_font_size(adv$strip_font_size %||% 12)
    )
    p <- .apply_p_label_size(p, adv$p_label_font_size %||% 3.5)
    p <- p + ggplot2::labs(y = as.character(plot_cfg$y_axis_label %||% analysis$y_axis_label %||% "Abundance"))
    p <- p + ggplot2::theme(legend.position = "none")
    node_plots[[cls]] <- .prep_for_node_svg(p)
  }
  mapped <- integer(length(elements_list))
  total_nodes <- integer(length(elements_list))
  for (i in seq_along(elements_list)) {
    mapped[i] <- .render_pathway_cytoscape_pdf(
      elements = elements_list[[i]], node_plots = node_plots, file = output_paths[[i]],
      paper_size = as.character(output$paper_size %||% "A4"),
      orientation = as.character(output$orientation %||% "LANDSCAPE"),
      browser = pathway$browser %||% NULL,
      timeout = as.numeric(pathway$timeout_seconds %||% 90),
      svg_width = as.numeric(plot_cfg$svg_width %||% 4.5),
      svg_height = as.numeric(plot_cfg$svg_height %||% 4.5)
    )
    total_nodes[i] <- length(elements_list[[i]]$nodes)
  }
  names(mapped) <- names(total_nodes) <- names(elements_list)

  summary <- list(
    status = "success",
    mode = if (custom_network) "custom" else "default_three_pathways",
    artifacts = unname(as.character(output_paths)),
    mapped_nodes = as.list(mapped),
    total_nodes = as.list(total_nodes),
    groups = group_levels,
    excluded_groups = setdiff(available_groups, unique(groups))
  )
  if (custom_network) summary$pdf <- unname(output_paths[[1]])
  invisible(summary)
}

.find_headless_browser <- function(explicit = NULL) {
  candidates <- c(
    explicit,
    Sys.getenv("MSLIPIDMAPPER_BROWSER", unset = NA_character_),
    Sys.which(c("google-chrome", "chromium", "chromium-browser", "chrome", "msedge")),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe"
  )
  candidates <- unique(as.character(candidates))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)][1]
  if (!length(hit) || is.na(hit)) {
    stop("Chrome/Chromium was not found. Set pathway.browser or MSLIPIDMAPPER_BROWSER.", call. = FALSE)
  }
  normalizePath(hit, winslash = "/", mustWork = TRUE)
}

.render_pathway_cytoscape_pdf <- function(elements, node_plots, file,
                                          paper_size = "A4", orientation = "LANDSCAPE",
                                          browser = NULL, timeout = 90,
                                          svg_width = 4.5, svg_height = 4.5) {
  work <- tempfile("mslm-cytoscape-")
  dir.create(file.path(work, "static"), recursive = TRUE)
  dir.create(file.path(work, "vendor"), recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

  vendor <- file.path(.mslm_app_www_dir(), "vendor")
  required <- c("cytoscape-pdf-export.js", "mslm-cytoscape-style.js", "mslm-cytoscape-export.html")
  sources <- file.path(vendor, required)
  if (any(!file.exists(sources))) stop("Missing Cytoscape CLI assets: ", paste(required[!file.exists(sources)], collapse = ", "), call. = FALSE)
  file.copy(sources, file.path(work, "vendor", required), overwrite = TRUE)
  file.copy(file.path(work, "vendor", "mslm-cytoscape-export.html"), file.path(work, "export.html"), overwrite = TRUE)

  mapped <- 0L
  elements$nodes <- lapply(elements$nodes, function(n) {
    d <- n$data %||% list()
    cls <- trimws(as.character(d$shared_name %||% d$label %||% ""))
    p <- node_plots[[cls]]
    if (!is.null(p)) {
      name <- paste0("class_", .safe_id(cls), ".svg")
      .save_svg_file(p, file.path(work, "static", name), width = svg_width, height = svg_height)
      d$path <- paste0("/static/", name)
      mapped <<- mapped + 1L
    } else {
      d$BorderWidth <- d$BorderWidth %||% 1L
    }
    out <- list(data = d)
    if (!is.null(n$position)) out$position <- n$position
    out
  })
  jsonlite::write_json(
    list(elements = elements, layout = "preset", paper_size = toupper(paper_size), orientation = toupper(orientation)),
    file.path(work, "job.json"), auto_unbox = TRUE, pretty = FALSE, null = "null"
  )

  server_script <- system.file("scripts", "mslipidmapper-plumber-static.R", package = "MSLipidMapper")
  if (!nzchar(server_script)) server_script <- file.path(getwd(), "inst", "scripts", "mslipidmapper-plumber-static.R")
  if (!file.exists(server_script)) stop("Plumber static-server script not found.", call. = FALSE)
  port <- httpuv::randomPort()
  log_file <- file.path(work, "plumber.log")
  server <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(server_script, work, as.character(port)),
    stdout = log_file, stderr = "2>&1", cleanup = TRUE
  )
  on.exit(if (server$is_alive()) server$kill(), add = TRUE)
  url <- sprintf("http://127.0.0.1:%d", port)
  ready <- FALSE
  for (i in seq_len(100)) {
    ready <- isTRUE(tryCatch({ readLines(paste0(url, "/healthz"), warn = FALSE); TRUE }, error = function(e) FALSE))
    if (ready) break
    if (!server$is_alive()) break
    Sys.sleep(0.1)
  }
  if (!ready) stop("Plumber server did not start. Log: ", paste(readLines(log_file, warn = FALSE), collapse = " "), call. = FALSE)

  chrome <- .find_headless_browser(browser)
  profile <- file.path(work, "chrome-profile")
  result <- processx::run(
    chrome,
    c("--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
      paste0("--user-data-dir=", profile), "--virtual-time-budget=30000", "--dump-dom", paste0(url, "/export.html")),
    error_on_status = FALSE, timeout = timeout * 1000, echo = FALSE
  )
  dom <- result$stdout
  match <- regexec("EXPORT_BASE64:([A-Za-z0-9+/=]+)", dom)
  value <- regmatches(dom, match)[[1]]
  if (result$status != 0L || length(value) < 2L) {
    err <- sub(".*EXPORT_ERROR:", "", dom)
    stop("Cytoscape PDF export failed: ", substr(err, 1, 2000), " ", result$stderr, call. = FALSE)
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeBin(jsonlite::base64_dec(value[2]), file)
  if (!file.exists(file) || file.info(file)$size < 1000) stop("Cytoscape returned an invalid PDF.", call. = FALSE)
  mapped
}

#' Command-line entry point for MSLipidMapper
#'
#' @param args Command-line arguments, normally from `commandArgs(TRUE)`.
#' @return Process-style exit status (zero on success).
#' @export
mslipidmapper_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  usage <- paste(
    "Usage:",
    "  mslipidmapper pathway --config parameters.yml [--input data.csv] [--network pathway.cyjs] [--output result.pdf]",
    "  mslipidmapper pathway --config parameters.yml [--input data.mzTab] [--network pathway.cyjs] [--output result.pdf]",
    "",
    "Commands:",
    "  pathway    Render a pathway projection PDF from a YAML parameter file.",
    "",
    "Options:",
    "  -c, --config FILE    YAML parameter file (required).",
    "  -i, --input FILE     Override the lipidomics input path in the YAML file.",
    "  -n, --network FILE   Override pathway.network with a CYJS/JSON file.",
    "      --cyjs FILE      Alias for --network.",
    "  -o, --output PATH    Output PDF (custom network) or directory (default pathways).",
    sep = "\n"
  )
  if (!length(args) || args[1] %in% c("-h", "--help", "help")) {
    cat(usage, "\n")
    return(0L)
  }
  if (!identical(args[1], "pathway")) stop("Unknown command: ", args[1], "\n", usage, call. = FALSE)

  parse_option <- function(names, label, required = FALSE) {
    exact <- which(args %in% names)
    long_names <- names[startsWith(names, "--")]
    equals <- unique(unlist(lapply(long_names, function(nm) which(startsWith(args, paste0(nm, "="))))))
    if (length(exact) + length(equals) > 1L) stop(label, " may be specified only once.\n", usage, call. = FALSE)
    if (length(exact)) {
      pos <- exact[1]
      if (pos == length(args) || startsWith(args[pos + 1L], "-")) {
        stop(label, " requires a file path.\n", usage, call. = FALSE)
      }
      return(list(value = args[pos + 1L], consumed = c(pos, pos + 1L)))
    }
    if (length(equals)) {
      pos <- equals[1]
      matched_name <- long_names[vapply(long_names, function(nm) startsWith(args[pos], paste0(nm, "=")), logical(1))][1]
      value <- substring(args[pos], nchar(matched_name) + 2L)
      if (!nzchar(value)) stop(label, " requires a file path.\n", usage, call. = FALSE)
      return(list(value = value, consumed = pos))
    }
    if (required) stop(label, " is required.\n", usage, call. = FALSE)
    list(value = NULL, consumed = integer(0))
  }

  config_opt <- parse_option(c("--config", "-c"), "--config", required = TRUE)
  input_opt <- parse_option(c("--input", "-i"), "--input")
  network_opt <- parse_option(c("--network", "--cyjs", "-n"), "--network")
  output_opt <- parse_option(c("--output", "-o"), "--output")
  consumed <- unique(c(1L, config_opt$consumed, input_opt$consumed, network_opt$consumed, output_opt$consumed))
  unknown <- args[setdiff(seq_along(args), consumed)]
  if (length(unknown)) stop("Unknown option: ", unknown[1], "\n", usage, call. = FALSE)

  input_override <- input_opt$value
  absolute_cli_path <- function(x, must_exist) {
    x <- path.expand(as.character(x))
    if (!grepl("^(?:[A-Za-z]:[/\\\\]|/)", x)) x <- file.path(getwd(), x)
    normalizePath(x, winslash = "/", mustWork = must_exist)
  }
  if (!is.null(input_override)) {
    input_override <- absolute_cli_path(input_override, must_exist = TRUE)
  }
  network_override <- network_opt$value
  if (!is.null(network_override)) {
    network_override <- absolute_cli_path(network_override, must_exist = TRUE)
  }
  output_override <- output_opt$value
  if (!is.null(output_override)) {
    output_override <- absolute_cli_path(output_override, must_exist = FALSE)
  }
  result <- render_pathway_from_config(
    config_opt$value,
    input_path = input_override,
    network_path = network_override,
    output_path = output_override
  )
  cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n")
  0L
}
