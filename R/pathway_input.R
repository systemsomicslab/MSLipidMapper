# Shared preparation for PDF and comparison-independent JSON-LD jobs.
.mslm_prepare_pathway_data <- function(input, analysis, input_format, lipidomics_path,
    acyl_chains = NULL, acyl_match = NULL, resolve_path = function(x, ...) x) {
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
  cd <- as.data.frame(SummarizedExperiment::colData(se), optional = TRUE)
  if (!group_col %in% names(cd)) {
    stop("analysis.group_column not found in sample metadata: ", group_col, call. = FALSE)
  }
  groups <- as.character(cd[[group_col]])
  if (anyNA(groups) || any(!nzchar(trimws(groups)))) {
    stop("analysis.group_column contains missing or empty values.", call. = FALSE)
  }
  available_groups <- unique(groups)
  ignored_exclude_groups <- character(0)
  parse_group_filter <- function(x, label, ignore_unknown = FALSE) {
    if (is.null(x)) return(character(0))
    value <- as.character(unlist(x, use.names = FALSE))
    if (!length(value) || anyNA(value) || any(!nzchar(trimws(value)))) {
      stop(label, " must contain non-empty group names.", call. = FALSE)
    }
    value <- unique(value)
    unknown <- setdiff(value, available_groups)
    if (length(unknown) && ignore_unknown) {
      # Excluding a group that is absent from this dataset is a no-op, so one
      # parameter file (e.g. excluding Blank) can be reused across datasets.
      message(label, ": ignoring groups not present in the data: ", paste(unknown, collapse = ", "))
      ignored_exclude_groups <<- unknown
      return(intersect(value, available_groups))
    }
    if (length(unknown)) {
      stop(label, " contains unknown groups: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    value
  }
  include_groups <- parse_group_filter(analysis$include_groups, "analysis.include_groups")
  exclude_groups <- parse_group_filter(analysis$exclude_groups, "analysis.exclude_groups",
                                       ignore_unknown = TRUE)
  keep <- rep(TRUE, length(groups))
  if (length(include_groups)) keep <- groups %in% include_groups
  if (length(exclude_groups)) keep <- keep & !groups %in% exclude_groups
  if (!any(keep)) stop("Group filtering removed every sample.", call. = FALSE)
  se <- se[, keep, drop = FALSE]
  groups <- groups[keep]

  features_before_acyl_filter <- nrow(se)
  acyl_cfg <- analysis$acyl_chain_filter %||% list()
  if (!is.list(acyl_cfg)) stop("analysis.acyl_chain_filter must be a YAML mapping.", call. = FALSE)
  selected_acyl_chains <- acyl_chains %||% acyl_cfg$chains %||% character(0)
  selected_acyl_chains <- trimws(as.character(unlist(selected_acyl_chains, use.names = FALSE)))
  selected_acyl_chains <- unique(selected_acyl_chains[!is.na(selected_acyl_chains) & nzchar(selected_acyl_chains)])
  selected_acyl_match <- tolower(as.character(acyl_match %||% acyl_cfg$match %||% "any"))[1]
  selected_acyl_match <- match.arg(selected_acyl_match, c("any", "all"))
  if (length(selected_acyl_chains)) {
    rules_path <- acyl_cfg$rules %||% .mslm_rules_yaml_path()
    if (!is.null(acyl_cfg$rules)) rules_path <- resolve_path(rules_path, "analysis.acyl_chain_filter.rules")
    if (is.null(rules_path) || !nzchar(rules_path) || !file.exists(rules_path)) {
      stop("Lipid rules YAML was not found for acyl-chain filtering.", call. = FALSE)
    }
    rules <- load_lipid_rules(rules_path)
    lipid_col <- as.character(acyl_cfg$lipid_name_column %||% .resolve_lipid_name_col(se))[1]
    if (is.na(lipid_col) || !nzchar(lipid_col)) {
      stop("No lipid-name column was found for acyl-chain filtering.", call. = FALSE)
    }
    se <- add_chain_list_to_se(se, rules, lipid_col = lipid_col, out_col = "acyl_chains")
    chain_values <- as.list(SummarizedExperiment::rowData(se)[["acyl_chains"]])
    chain_values <- lapply(chain_values, function(x) unique(trimws(as.character(x))))
    available_chains <- sort(unique(unlist(chain_values, use.names = FALSE)))
    unavailable <- setdiff(selected_acyl_chains, available_chains)
    if (length(unavailable)) {
      stop(
        "Requested acyl chains were not found in molecule-level annotations: ",
        paste(unavailable, collapse = ", "),
        call. = FALSE
      )
    }
    keep_features <- vapply(chain_values, function(chains) {
      if (identical(selected_acyl_match, "all")) {
        all(selected_acyl_chains %in% chains)
      } else {
        any(selected_acyl_chains %in% chains)
      }
    }, logical(1))
    if (!any(keep_features)) stop("Acyl-chain filtering removed every lipid molecule.", call. = FALSE)
    se <- se[keep_features, , drop = FALSE]
  }
  features_after_acyl_filter <- nrow(se)

  normalization <- tolower(as.character(analysis$normalization %||% "none"))[1]
  se <- normalize_se(se, method = normalization)

  list(se = se, group_col = group_col, groups = groups, available_groups = available_groups, include_groups = include_groups,
    exclude_groups = exclude_groups, ignored_exclude_groups = ignored_exclude_groups,
    features_before_acyl_filter = features_before_acyl_filter,
    features_after_acyl_filter = features_after_acyl_filter,
    selected_acyl_chains = selected_acyl_chains, selected_acyl_match = selected_acyl_match,
    normalization = normalization)
}
