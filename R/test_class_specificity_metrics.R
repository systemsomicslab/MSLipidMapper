# Test-only utilities for molecule-vs-class specificity metrics.
# These helpers are intentionally isolated so they can be removed
# without affecting the main app flow.

.tcs_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

.tcs_add_log <- function(log_lines, message) {
  c(log_lines, sprintf("[%s] %s", .tcs_timestamp(), message))
}

.tcs_as_flag <- function(x) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    return(x)
  }
  x_chr <- tolower(trimws(as.character(x)[1]))
  isTRUE(x_chr %in% c("true", "t", "1", "yes", "y"))
}

.tcs_find_function <- function(name) {
  if (exists(name, mode = "function", inherits = TRUE)) {
    return(get(name, mode = "function", inherits = TRUE))
  }

  pkg_ns <- NULL
  if ("MSLipidMapper" %in% loadedNamespaces()) {
    pkg_ns <- asNamespace("MSLipidMapper")
  } else {
    pkg_ns <- tryCatch(asNamespace("MSLipidMapper"), error = function(e) NULL)
  }

  if (!is.null(pkg_ns) && exists(name, envir = pkg_ns, mode = "function", inherits = FALSE)) {
    return(get(name, envir = pkg_ns, mode = "function", inherits = FALSE))
  }

  NULL
}

.tcs_resolve_rules_yaml <- function(rules_yaml = NULL) {
  if (!is.null(rules_yaml) && nzchar(as.character(rules_yaml)[1]) && file.exists(rules_yaml)) {
    return(normalizePath(rules_yaml, winslash = "/", mustWork = FALSE))
  }

  path_fun <- .tcs_find_function("mslipidmapper_rules_path")
  if (is.function(path_fun)) {
    path <- tryCatch(path_fun(), error = function(e) "")
    if (nzchar(path) && file.exists(path)) {
      return(normalizePath(path, winslash = "/", mustWork = FALSE))
    }
  }

  ""
}

.tcs_safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

.tcs_safe_log <- function(x, pseudocount) {
  x_num <- .tcs_safe_numeric(x)
  out <- rep(NA_real_, length(x_num))
  ok <- is.finite(x_num) & is.finite(pseudocount) & ((x_num + pseudocount) > 0)
  out[ok] <- log(x_num[ok] + pseudocount)
  out
}

.tcs_safe_cor <- function(x, y, method = "pearson") {
  x_num <- .tcs_safe_numeric(x)
  y_num <- .tcs_safe_numeric(y)
  ok <- is.finite(x_num) & is.finite(y_num)
  if (sum(ok) < 3L) {
    return(list(value = NA_real_, n = sum(ok), flag = "too_few_points"))
  }
  if (stats::sd(x_num[ok]) == 0 || stats::sd(y_num[ok]) == 0) {
    return(list(value = NA_real_, n = sum(ok), flag = "zero_variance"))
  }

  val <- tryCatch(
    suppressWarnings(stats::cor(x_num[ok], y_num[ok], method = method)),
    error = function(e) NA_real_
  )
  list(value = as.numeric(val), n = sum(ok), flag = if (is.finite(val)) "" else "cor_failed")
}

.tcs_safe_ttest <- function(values, groups) {
  ok <- is.finite(values) & !is.na(groups)
  if (sum(ok) < 3L) {
    return(NA_real_)
  }
  g <- droplevels(factor(groups[ok]))
  if (nlevels(g) != 2L) {
    return(NA_real_)
  }
  split_vals <- split(values[ok], g)
  if (length(split_vals[[1]]) < 2L || length(split_vals[[2]]) < 2L) {
    return(NA_real_)
  }
  tryCatch(stats::t.test(split_vals[[2]], split_vals[[1]])$p.value, error = function(e) NA_real_)
}

.tcs_group_shift <- function(values, groups) {
  ok <- is.finite(values) & !is.na(groups)
  if (sum(ok) < 2L) {
    return(NA_real_)
  }
  g <- droplevels(factor(groups[ok]))
  if (nlevels(g) != 2L) {
    return(NA_real_)
  }
  split_vals <- split(values[ok], g)
  mean(split_vals[[2]], na.rm = TRUE) - mean(split_vals[[1]], na.rm = TRUE)
}

.tcs_safe_interaction_p <- function(log_molecule, log_leave1out, groups) {
  ok <- is.finite(log_molecule) & is.finite(log_leave1out) & !is.na(groups)
  if (sum(ok) < 5L) {
    return(NA_real_)
  }
  g <- droplevels(factor(groups[ok]))
  if (nlevels(g) != 2L) {
    return(NA_real_)
  }
  if (stats::sd(log_leave1out[ok]) == 0 || stats::sd(log_molecule[ok]) == 0) {
    return(NA_real_)
  }

  fit <- tryCatch(
    stats::lm(log_molecule[ok] ~ log_leave1out[ok] * g),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NA_real_)
  }

  cf <- summary(fit)$coefficients
  rn <- rownames(cf)
  hit <- grep(":", rn, fixed = TRUE)
  if (!length(hit)) {
    return(NA_real_)
  }
  as.numeric(cf[hit[1], 4])
}

.tcs_fit_leave1out_model <- function(log_molecule, log_leave1out) {
  ok <- is.finite(log_molecule) & is.finite(log_leave1out)
  residuals <- rep(NA_real_, length(log_molecule))

  if (sum(ok) < 3L) {
    return(list(r2 = NA_real_, residuals = residuals, flag = "too_few_points"))
  }
  if (stats::sd(log_leave1out[ok]) == 0 || stats::sd(log_molecule[ok]) == 0) {
    return(list(r2 = NA_real_, residuals = residuals, flag = "zero_variance"))
  }

  fit <- tryCatch(stats::lm(log_molecule[ok] ~ log_leave1out[ok]), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(r2 = NA_real_, residuals = residuals, flag = "lm_failed"))
  }

  residuals[ok] <- stats::residuals(fit)
  list(
    r2 = as.numeric(summary(fit)$r.squared),
    residuals = residuals,
    flag = ""
  )
}

.tcs_feature_ids <- function(se) {
  ids <- rownames(se)
  if (is.null(ids) || !length(ids)) {
    ids <- sprintf("feature_%s", seq_len(nrow(se)))
  }
  as.character(ids)
}

.tcs_lipid_labels <- function(rd, feature_ids, lipid_col = "Metabolite.name") {
  if (!lipid_col %in% names(rd)) {
    return(feature_ids)
  }
  lbl <- as.character(rd[[lipid_col]])
  lbl[is.na(lbl) | !nzchar(lbl)] <- feature_ids[is.na(lbl) | !nzchar(lbl)]
  lbl
}

.tcs_unique_chains <- function(x) {
  ch <- unique(trimws(as.character(x)))
  ch[nzchar(ch)]
}

.tcs_feature_has_chain <- function(acyl_list, chain) {
  target <- trimws(as.character(chain)[1])
  vapply(acyl_list, function(x) {
    vals <- .tcs_unique_chains(x)
    any(vals == target)
  }, logical(1))
}

.tcs_subset_membership_table <- function(classes, feature_ids, labels, acyl_chains) {
  if (!is.list(acyl_chains)) {
    return(data.frame())
  }

  out <- vector("list", length(feature_ids))
  for (i in seq_along(feature_ids)) {
    chains_i <- .tcs_unique_chains(acyl_chains[[i]])
    if (!length(chains_i)) {
      next
    }
    out[[i]] <- data.frame(
      class = classes[i],
      subset = paste0(chains_i, "-containing"),
      feature_id = feature_ids[i],
      lipid_label = labels[i],
      chain = chains_i,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out[!vapply(out, is.null, logical(1))])
}

build_test_specificity_se_from_csv <- function(lipid_csv,
                                               metadata_csv = NULL,
                                               metadata_sample_id_col = "sample_id",
                                               metadata_class_col = "class") {
  load_fun <- .tcs_find_function("load_lipidomics_se")
  if (!is.function(load_fun)) {
    stop("Function 'load_lipidomics_se()' is not available.", call. = FALSE)
  }

  se <- load_fun(lipid_csv)

  if (is.null(metadata_csv) || !nzchar(as.character(metadata_csv)[1])) {
    return(se)
  }
  if (!file.exists(metadata_csv)) {
    stop("metadata_csv was not found.", call. = FALSE)
  }

  meta <- utils::read.csv(metadata_csv, check.names = FALSE, stringsAsFactors = FALSE)
  if (!metadata_sample_id_col %in% names(meta)) {
    stop(sprintf("Metadata file is missing '%s'.", metadata_sample_id_col), call. = FALSE)
  }
  if (!metadata_class_col %in% names(meta)) {
    stop(sprintf("Metadata file is missing '%s'.", metadata_class_col), call. = FALSE)
  }

  cd <- as.data.frame(SummarizedExperiment::colData(se))
  if (!"sample_id" %in% names(cd)) {
    cd$sample_id <- colnames(SummarizedExperiment::assay(se, SummarizedExperiment::assayNames(se)[1]))
  }
  cd$sample_id <- as.character(cd$sample_id)

  meta[[metadata_sample_id_col]] <- as.character(meta[[metadata_sample_id_col]])
  idx <- match(cd$sample_id, meta[[metadata_sample_id_col]])
  hit <- !is.na(idx)

  for (nm in names(meta)) {
    vals <- rep(NA, nrow(cd))
    vals[hit] <- meta[[nm]][idx[hit]]
    cd[[nm]] <- vals
  }

  if (!"class" %in% names(cd) || metadata_class_col != "class") {
    cd$class <- cd[[metadata_class_col]]
  }

  rownames(cd) <- cd$sample_id
  SummarizedExperiment::colData(se) <- S4Vectors::DataFrame(cd, row.names = cd$sample_id)
  se
}

annotate_acyl_chains_if_possible <- function(se,
                                             annotate_acyl = FALSE,
                                             lipid_col = "Metabolite.name",
                                             rules_yaml = NULL,
                                             verbose = FALSE,
                                             log_lines = character()) {
  rd <- S4Vectors::DataFrame(SummarizedExperiment::rowData(se))
  if ("acyl_chains" %in% names(rd) && is.list(rd[["acyl_chains"]])) {
    log_lines <- .tcs_add_log(log_lines, "Using existing rowData$acyl_chains.")
    return(list(se = se, annotated = FALSE, log = log_lines))
  }

  if (!isTRUE(annotate_acyl)) {
    msg <- "rowData$acyl_chains is missing and annotate_acyl = FALSE; continuing without chain annotation."
    warning(msg, call. = FALSE)
    log_lines <- .tcs_add_log(log_lines, msg)
    return(list(se = se, annotated = FALSE, log = log_lines))
  }

  load_fun <- .tcs_find_function("load_lipid_rules")
  add_fun  <- .tcs_find_function("add_chain_list_to_se")
  if (!is.function(load_fun) || !is.function(add_fun)) {
    msg <- "Acyl-chain helpers are unavailable; continuing without chain annotation."
    warning(msg, call. = FALSE)
    log_lines <- .tcs_add_log(log_lines, msg)
    return(list(se = se, annotated = FALSE, log = log_lines))
  }

  rules_path <- .tcs_resolve_rules_yaml(rules_yaml)
  if (!nzchar(rules_path)) {
    msg <- "No lipid rules YAML was found; continuing without chain annotation."
    warning(msg, call. = FALSE)
    log_lines <- .tcs_add_log(log_lines, msg)
    return(list(se = se, annotated = FALSE, log = log_lines))
  }

  if (!lipid_col %in% names(rd)) {
    msg <- sprintf("lipid_col '%s' is missing; continuing without chain annotation.", lipid_col)
    warning(msg, call. = FALSE)
    log_lines <- .tcs_add_log(log_lines, msg)
    return(list(se = se, annotated = FALSE, log = log_lines))
  }

  rules <- tryCatch(load_fun(rules_path), error = function(e) e)
  if (inherits(rules, "error")) {
    msg <- paste("Failed to load lipid rules:", conditionMessage(rules))
    warning(msg, call. = FALSE)
    log_lines <- .tcs_add_log(log_lines, msg)
    return(list(se = se, annotated = FALSE, log = log_lines))
  }

  se2 <- tryCatch(
    add_fun(se, lipid_col = lipid_col, rules = rules),
    error = function(e) e
  )
  if (inherits(se2, "error")) {
    msg <- paste("Failed to annotate acyl chains:", conditionMessage(se2))
    warning(msg, call. = FALSE)
    log_lines <- .tcs_add_log(log_lines, msg)
    return(list(se = se, annotated = FALSE, log = log_lines))
  }

  if (verbose) {
    message("Acyl-chain annotation added for test metrics.")
  }
  log_lines <- .tcs_add_log(log_lines, sprintf("Annotated acyl chains using %s.", rules_path))
  list(se = se2, annotated = TRUE, log = log_lines)
}

compute_test_class_specificity_metrics <- function(se,
                                                   assay_name = NULL,
                                                   class_col = "LipidClass",
                                                   target_class = NULL,
                                                   group_col = NULL,
                                                   pseudocount = 1,
                                                   annotate_acyl = FALSE,
                                                   lipid_col = "Metabolite.name",
                                                   rules_yaml = NULL,
                                                   verbose = FALSE) {
  if (!inherits(se, "SummarizedExperiment")) {
    stop("Input must be a SummarizedExperiment.", call. = FALSE)
  }

  assay_names <- SummarizedExperiment::assayNames(se)
  if (is.null(assay_name) || !nzchar(as.character(assay_name)[1])) {
    assay_name <- if (length(assay_names)) assay_names[1] else "abundance"
  }
  if (!assay_name %in% assay_names) {
    stop(sprintf("Assay '%s' was not found in the SummarizedExperiment.", assay_name), call. = FALSE)
  }

  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!class_col %in% names(rd)) {
    stop(sprintf("rowData is missing class_col '%s'.", class_col), call. = FALSE)
  }

  log_lines <- .tcs_add_log(character(), sprintf("Started specificity test metrics for assay '%s'.", assay_name))
  log_lines <- .tcs_add_log(log_lines, sprintf("Using class_col '%s'.", class_col))

  ann <- annotate_acyl_chains_if_possible(
    se = se,
    annotate_acyl = annotate_acyl,
    lipid_col = lipid_col,
    rules_yaml = rules_yaml,
    verbose = verbose,
    log_lines = log_lines
  )
  se <- ann$se
  log_lines <- ann$log

  A <- SummarizedExperiment::assay(se, assay_name)
  if (!is.matrix(A)) {
    A <- as.matrix(A)
  }
  storage.mode(A) <- "numeric"

  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  classes <- as.character(rd[[class_col]])
  feature_ids <- .tcs_feature_ids(se)
  lipid_labels <- .tcs_lipid_labels(rd, feature_ids, lipid_col = lipid_col)
  sample_ids <- colnames(A)
  if (is.null(sample_ids) || !length(sample_ids)) {
    sample_ids <- sprintf("sample_%s", seq_len(ncol(A)))
  }

  target_classes <- unique(classes[!is.na(classes) & nzchar(classes)])
  if (!is.null(target_class) && nzchar(as.character(target_class)[1])) {
    target_classes <- intersect(target_classes, as.character(target_class))
  }
  if (!length(target_classes)) {
    stop("No features were found for target_class.", call. = FALSE)
  }

  cd <- as.data.frame(SummarizedExperiment::colData(se))
  group_values <- rep(NA_character_, ncol(A))
  group_levels <- character(0)
  group_enabled <- FALSE
  if (!is.null(group_col) && nzchar(as.character(group_col)[1]) && group_col %in% names(cd)) {
    group_values <- as.character(cd[[group_col]])
    group_levels <- unique(group_values[!is.na(group_values) & nzchar(group_values)])
    group_levels <- sort(group_levels)
    if (length(group_levels) == 2L) {
      group_enabled <- TRUE
      log_lines <- .tcs_add_log(log_lines, sprintf("Enabled two-group comparisons using '%s': %s vs %s.", group_col, group_levels[1], group_levels[2]))
    } else {
      msg <- sprintf("group_col '%s' does not contain exactly 2 groups; group tests were skipped.", group_col)
      warning(msg, call. = FALSE)
      log_lines <- .tcs_add_log(log_lines, msg)
    }
  }

  metric_rows <- vector("list", 0L)
  sample_rows <- vector("list", 0L)
  subset_membership <- vector("list", 0L)

  for (cls in target_classes) {
    idx_class <- which(classes == cls)
    if (!length(idx_class)) {
      next
    }

    class_total <- colSums(A[idx_class, , drop = FALSE], na.rm = TRUE)
    class_acyl <- if ("acyl_chains" %in% names(rd)) rd$acyl_chains[idx_class] else NULL
    class_chains <- unique(unlist(lapply(class_acyl, .tcs_unique_chains), use.names = FALSE))
    class_chains <- sort(unique(class_chains[nzchar(class_chains)]))
    if (!length(class_chains)) {
      next
    }

    for (chain_value in class_chains) {
      has_chain <- .tcs_feature_has_chain(class_acyl, chain_value)
      idx_subset <- idx_class[has_chain]
      if (!length(idx_subset)) {
        next
      }

      subset_total <- colSums(A[idx_subset, , drop = FALSE], na.rm = TRUE)
      leave1out_total <- class_total - subset_total

      cor_total_pearson <- .tcs_safe_cor(subset_total, class_total, method = "pearson")
      cor_total_spearman <- .tcs_safe_cor(subset_total, class_total, method = "spearman")
      cor_leave_pearson <- .tcs_safe_cor(subset_total, leave1out_total, method = "pearson")
      cor_leave_spearman <- .tcs_safe_cor(subset_total, leave1out_total, method = "spearman")

      log_subset <- .tcs_safe_log(subset_total, pseudocount)
      log_leave1out <- .tcs_safe_log(leave1out_total, pseudocount)
      logratio <- log_subset - log_leave1out

      fit <- .tcs_fit_leave1out_model(log_subset, log_leave1out)
      residuals <- fit$residuals

      warning_flags <- unique(c(
        cor_total_pearson$flag,
        cor_total_spearman$flag,
        cor_leave_pearson$flag,
        cor_leave_spearman$flag,
        fit$flag
      ))
      warning_flags <- warning_flags[nzchar(warning_flags)]

      subset_label <- paste0(chain_value, "-containing")
      member_ids <- feature_ids[idx_subset]
      member_labels <- lipid_labels[idx_subset]

      metric_row <- data.frame(
        class = cls,
        subset = subset_label,
        chain = chain_value,
        n_molecules = length(idx_subset),
        member_feature_ids = paste(member_ids, collapse = ";"),
        member_lipid_labels = paste(member_labels, collapse = ";"),
        pearson_cor_class_total = cor_total_pearson$value,
        pearson_cor_leave1out = cor_leave_pearson$value,
        spearman_cor_class_total = cor_total_spearman$value,
        spearman_cor_leave1out = cor_leave_spearman$value,
        lm_r2_leave1out = fit$r2,
        specificity_score = if (is.finite(fit$r2)) 1 - fit$r2 else NA_real_,
        residual_sd = stats::sd(residuals, na.rm = TRUE),
        residual_mad = stats::mad(residuals, constant = 1.4826, na.rm = TRUE),
        logratio_mean = mean(logratio, na.rm = TRUE),
        logratio_sd = stats::sd(logratio, na.rm = TRUE),
        logratio_mad = stats::mad(logratio, constant = 1.4826, na.rm = TRUE),
        subset_total_sum = sum(subset_total, na.rm = TRUE),
        class_total_sum = sum(class_total, na.rm = TRUE),
        n_samples_total = ncol(A),
        n_samples_valid_lm = sum(is.finite(log_subset) & is.finite(log_leave1out)),
        warning_flag = if (length(warning_flags)) paste(warning_flags, collapse = ";") else "",
        stringsAsFactors = FALSE
      )

      if (group_enabled) {
        g <- factor(group_values, levels = group_levels)
        metric_row$logratio_group_shift <- .tcs_group_shift(logratio, g)
        metric_row$logratio_group_pvalue <- .tcs_safe_ttest(logratio, g)
        metric_row$residual_group_shift <- .tcs_group_shift(residuals, g)
        metric_row$residual_group_pvalue <- .tcs_safe_ttest(residuals, g)
        metric_row$interaction_pvalue <- .tcs_safe_interaction_p(log_subset, log_leave1out, g)
      }

      metric_rows[[length(metric_rows) + 1L]] <- metric_row

      sample_rows[[length(sample_rows) + 1L]] <- data.frame(
        class = cls,
        subset = subset_label,
        chain = chain_value,
        n_molecules = length(idx_subset),
        sample_id = sample_ids,
        group = if (!is.null(group_col) && group_col %in% names(cd)) group_values else NA_character_,
        subset_total = .tcs_safe_numeric(subset_total),
        class_total = .tcs_safe_numeric(class_total),
        leave1out_total = .tcs_safe_numeric(leave1out_total),
        residual = .tcs_safe_numeric(residuals),
        logratio = .tcs_safe_numeric(logratio),
        stringsAsFactors = FALSE
      )

      subset_membership[[length(subset_membership) + 1L]] <- data.frame(
        class = cls,
        subset = subset_label,
        chain = chain_value,
        feature_id = member_ids,
        lipid_label = member_labels,
        stringsAsFactors = FALSE
      )
    }
  }

  metric_table <- if (length(metric_rows)) do.call(rbind, metric_rows) else data.frame()
  samplewise_long <- if (length(sample_rows)) do.call(rbind, sample_rows) else data.frame()
  chain_membership <- if (length(subset_membership)) do.call(rbind, subset_membership) else data.frame()

  summary_table <- data.frame()
  if (nrow(metric_table)) {
    split_metrics <- split(metric_table, metric_table$class)
    summary_rows <- lapply(split_metrics, function(df) {
      ord <- order(df$specificity_score, decreasing = TRUE, na.last = TRUE)
      top <- df[ord[1], , drop = FALSE]
      data.frame(
        class = df$class[1],
        n_subsets = nrow(df),
        mean_specificity_score = mean(df$specificity_score, na.rm = TRUE),
        median_specificity_score = stats::median(df$specificity_score, na.rm = TRUE),
        max_specificity_score = suppressWarnings(max(df$specificity_score, na.rm = TRUE)),
        mean_lm_r2_leave1out = mean(df$lm_r2_leave1out, na.rm = TRUE),
        mean_pearson_cor_leave1out = mean(df$pearson_cor_leave1out, na.rm = TRUE),
        top_subset = top$subset[1],
        top_chain = top$chain[1],
        top_n_molecules = top$n_molecules[1],
        top_specificity_score = top$specificity_score[1],
        stringsAsFactors = FALSE
      )
    })
    summary_table <- do.call(rbind, summary_rows)
  }

  log_lines <- .tcs_add_log(log_lines, sprintf("Processed %d classes and %d chain-containing subsets.", length(unique(metric_table$class)), nrow(metric_table)))

  list(
    metric_table = metric_table,
    samplewise_long = samplewise_long,
    chain_membership = chain_membership,
    summary_table = summary_table,
    run_log = log_lines
  )
}

write_test_class_specificity_outputs <- function(result, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  utils::write.csv(result$metric_table, file.path(output_dir, "metric_table.csv"), row.names = FALSE, na = "")
  utils::write.csv(result$samplewise_long, file.path(output_dir, "samplewise_long.csv"), row.names = FALSE, na = "")
  utils::write.csv(result$chain_membership, file.path(output_dir, "chain_membership.csv"), row.names = FALSE, na = "")
  utils::write.csv(result$summary_table, file.path(output_dir, "summary_table.csv"), row.names = FALSE, na = "")
  writeLines(result$run_log, con = file.path(output_dir, "run_log.txt"), sep = "\n")

  invisible(output_dir)
}
