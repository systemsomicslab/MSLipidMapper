# R/ccd_analysis.R ------------------------------------------------------------
# Class x chain-subset divergence analysis utilities.

suppressPackageStartupMessages({
  library(SummarizedExperiment)
  library(S4Vectors)
  library(dplyr)
  library(tibble)
  library(stringr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.ccd_first_existing_col <- function(available, candidates) {
  hit <- candidates[candidates %in% available]
  if (length(hit)) hit[[1]] else NULL
}

.ccd_resolve_class_col <- function(se, class_col = "LipidClass") {
  rd_names <- colnames(SummarizedExperiment::rowData(se))
  if (!is.null(class_col) && nzchar(class_col) && class_col %in% rd_names) {
    return(class_col)
  }

  .ccd_first_existing_col(rd_names, c("LipidClass", "class", "lipid_class", "Class", "Ontology")) %||%
    resolve_class_col(se)
}

.ccd_resolve_chain_col <- function(se, chain_col = "Chains") {
  rd_names <- colnames(SummarizedExperiment::rowData(se))
  if (!is.null(chain_col) && nzchar(chain_col) && chain_col %in% rd_names) {
    return(chain_col)
  }

  .ccd_first_existing_col(rd_names, c("Chains", "acyl_chains", "chains", "AcylChains"))
}

.ccd_resolve_assay_name <- function(se, assay_name = NULL) {
  if (!is.null(assay_name) && nzchar(assay_name) && assay_name %in% SummarizedExperiment::assayNames(se)) {
    return(assay_name)
  }
  if ("abundance" %in% SummarizedExperiment::assayNames(se)) return("abundance")
  SummarizedExperiment::assayNames(se)[1]
}

.ccd_as_chain_list <- function(x) {
  if (is.list(x) || inherits(x, c("List", "SimpleList", "CompressedList"))) {
    out <- as.list(x)
  } else {
    out <- as.list(x)
  }

  lapply(out, function(v) {
    if (is.null(v)) return(character(0))
    vv <- as.character(v)
    vv <- vv[!is.na(vv) & nzchar(vv)]
    if (!length(vv)) return(character(0))

    if (length(vv) == 1L) {
      tok <- stringr::str_extract_all(
        vv,
        "\\d+:\\d+(?:;O\\d+)?(?:\\(\\d+OH\\))?"
      )[[1]]
      tok <- tok[!is.na(tok) & nzchar(tok)]
      if (length(tok)) return(unique(tok))
    }

    unique(vv)
  })
}

.ccd_chain_carbon <- function(chain_code) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(chain_code), "^\\d+")))
}

.ccd_safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  n_ok <- sum(ok)
  if (n_ok < 3L) {
    return(list(
      cor = NA_real_,
      n_samples_used = n_ok,
      warning_flag = "too_few_complete_samples"
    ))
  }

  x2 <- x[ok]
  y2 <- y[ok]

  if (all(is.na(x2)) || all(is.na(y2))) {
    return(list(
      cor = NA_real_,
      n_samples_used = n_ok,
      warning_flag = "all_na_profile"
    ))
  }

  if (all(x2 == 0, na.rm = TRUE) || all(y2 == 0, na.rm = TRUE)) {
    return(list(
      cor = NA_real_,
      n_samples_used = n_ok,
      warning_flag = "all_zero_profile"
    ))
  }

  if (stats::sd(x2, na.rm = TRUE) == 0 || stats::sd(y2, na.rm = TRUE) == 0) {
    return(list(
      cor = NA_real_,
      n_samples_used = n_ok,
      warning_flag = "zero_variance_profile"
    ))
  }

  cor_val <- tryCatch(
    stats::cor(x2, y2, use = "pairwise.complete.obs", method = method),
    error = function(e) NA_real_
  )

  flag <- if (is.na(cor_val)) "correlation_failed" else ""

  list(
    cor = as.numeric(cor_val),
    n_samples_used = n_ok,
    warning_flag = flag
  )
}

.ccd_direction_label <- function(cor_val) {
  if (!is.finite(cor_val)) return("not_testable")
  if (cor_val > 0.7) return("concordant")
  if (cor_val > 0.3) return("weak_concordant")
  if (cor_val >= -0.3) return("decoupled")
  "opposite"
}

.ccd_warn_join <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[nzchar(vals) & !is.na(vals)]
  if (!length(vals)) "" else paste(unique(vals), collapse = ";")
}

#' Generate chain subsets for one lipid class
#'
#' @param row_annot A data.frame-like annotation object for one or more lipids.
#' @param class_value Target class value.
#' @param subset_mode Subset generation mode.
#' @param chain_col Chain annotation column name.
#' @param class_col Class annotation column name.
#' @param custom_subset_rules Named list of custom subset functions.
#'
#' @return A named list of logical vectors, one per subset.
#' @export
generate_chain_subsets <- function(row_annot,
                                   class_value,
                                   subset_mode = "exact_chain",
                                   chain_col = "Chains",
                                   class_col = "LipidClass",
                                   custom_subset_rules = NULL) {
  df <- as.data.frame(row_annot)
  if (!class_col %in% colnames(df)) {
    stop("row_annot does not contain class_col: ", class_col, call. = FALSE)
  }
  if (!chain_col %in% colnames(df)) {
    stop("row_annot does not contain chain_col: ", chain_col, call. = FALSE)
  }

  idx <- which(as.character(df[[class_col]]) == as.character(class_value))
  if (!length(idx)) return(list())

  chains_list <- .ccd_as_chain_list(df[[chain_col]][idx])
  n <- length(chains_list)
  subset_mode <- match.arg(subset_mode, c("exact_chain", "carbon_only", "custom"))

  make_membership <- function(labels, predicate) {
    out <- list()
    for (lab in labels) {
      member <- vapply(chains_list, function(chains) {
        if (!length(chains)) return(FALSE)
        isTRUE(predicate(chains, lab))
      }, logical(1))
      out[[lab]] <- member
    }
    out
  }

  if (subset_mode == "exact_chain") {
    all_codes <- sort(unique(unlist(chains_list, use.names = FALSE)))
    all_codes <- all_codes[nzchar(all_codes)]
    if (!length(all_codes)) return(list())
    labels <- paste0(all_codes, "-containing")
    codes <- stats::setNames(all_codes, labels)
    return(make_membership(names(codes), function(chains, lab) {
      target <- codes[[lab]]
      any(chains %in% target)
    }))
  }

  if (subset_mode == "carbon_only") {
    carbons <- sort(unique(stats::na.omit(vapply(unlist(chains_list, use.names = FALSE), .ccd_chain_carbon, integer(1)))))
    if (!length(carbons)) return(list())
    labels <- paste0("C", carbons, "-containing")
    carbon_map <- stats::setNames(carbons, labels)
    return(make_membership(names(carbon_map), function(chains, lab) {
      target <- carbon_map[[lab]]
      any(vapply(chains, function(cc) identical(.ccd_chain_carbon(cc), target), logical(1)))
    }))
  }

  if (is.null(custom_subset_rules) || !length(custom_subset_rules)) {
    return(list())
  }

  out <- list()
  for (nm in names(custom_subset_rules)) {
    rule <- custom_subset_rules[[nm]]
    if (!is.function(rule)) next
    out[[nm]] <- vapply(chains_list, function(chains) {
      if (!length(chains)) return(FALSE)
      isTRUE(tryCatch(rule(chains), error = function(e) FALSE))
    }, logical(1))
  }
  out
}

#' Compute class x chain-subset CCD table
#'
#' @param se A SummarizedExperiment.
#' @param assay_name Assay name. Defaults to `abundance` or the first assay.
#' @param class_col Lipid class column name in rowData.
#' @param chain_col Chain annotation column name in rowData.
#' @param subset_mode Subset mode: `exact_chain`, `carbon_only`, or `custom`.
#' @param min_subset_size Minimum number of lipids in a subset.
#' @param cor_method Correlation method passed to `stats::cor`.
#' @param custom_subset_rules Named list of custom subset predicates.
#' @param verbose If TRUE, emit progress messages.
#'
#' @return A tibble with one row per class x subset pair.
#' @export
compute_ccd_table <- function(se,
                              assay_name = NULL,
                              class_col = "LipidClass",
                              chain_col = "Chains",
                              subset_mode = "exact_chain",
                              min_subset_size = 3,
                              cor_method = "pearson",
                              custom_subset_rules = NULL,
                              verbose = FALSE) {
  stopifnot(methods::is(se, "SummarizedExperiment"))

  assay_name <- .ccd_resolve_assay_name(se, assay_name)
  class_col <- .ccd_resolve_class_col(se, class_col)
  chain_col <- .ccd_resolve_chain_col(se, chain_col)

  if (is.null(chain_col)) {
    stop("No chain annotation column found. Expected one of: Chains, acyl_chains.", call. = FALSE)
  }

  A <- as.matrix(SummarizedExperiment::assay(se, assay_name))
  rd <- as.data.frame(SummarizedExperiment::rowData(se), check.names = FALSE)
  classes <- as.character(rd[[class_col]])
  chains <- .ccd_as_chain_list(rd[[chain_col]])

  valid_class_idx <- which(!is.na(classes) & nzchar(classes))
  classes_u <- sort(unique(classes[valid_class_idx]))
  min_subset_size <- as.integer(min_subset_size %||% 3L)
  min_subset_size <- max(1L, min_subset_size)
  cor_method <- match.arg(cor_method, c("pearson", "spearman"))

  out <- list()
  row_id <- 1L

  for (cls in classes_u) {
    class_idx <- which(classes == cls)
    if (length(class_idx) == 0) next

    if (isTRUE(verbose)) {
      message("[CCD] Processing class: ", cls)
    }

    class_profile <- colSums(A[class_idx, , drop = FALSE], na.rm = TRUE)
    class_total <- sum(class_profile, na.rm = TRUE)

    subset_defs <- generate_chain_subsets(
      row_annot = rd,
      class_value = cls,
      subset_mode = subset_mode,
      chain_col = chain_col,
      class_col = class_col,
      custom_subset_rules = custom_subset_rules
    )

    if (!length(subset_defs)) next

    class_chain_list <- chains[class_idx]

    for (subset_label in names(subset_defs)) {
      member_local <- subset_defs[[subset_label]]
      member_local <- as.logical(member_local %||% FALSE)
      if (length(member_local) != length(class_idx)) next

      n_species <- sum(member_local, na.rm = TRUE)
      if (n_species < min_subset_size) next

      subset_idx <- class_idx[member_local]
      subset_profile <- colSums(A[subset_idx, , drop = FALSE], na.rm = TRUE)
      subset_total <- sum(subset_profile, na.rm = TRUE)

      cor_info <- .ccd_safe_cor(class_profile, subset_profile, method = cor_method)
      cor_val <- cor_info$cor
      ccd_val <- if (is.finite(cor_val)) 1 - cor_val else NA_real_
      scr_val <- if (is.finite(class_total) && class_total != 0) subset_total / class_total else NA_real_

      warning_flag <- cor_info$warning_flag
      if (!is.finite(scr_val)) {
        warning_flag <- .ccd_warn_join(warning_flag, "invalid_scr")
      }
      if (n_species == length(class_idx)) {
        warning_flag <- .ccd_warn_join(warning_flag, "subset_equals_full_class")
      }

      out[[row_id]] <- tibble::tibble(
        class = as.character(cls),
        subset = as.character(subset_label),
        subset_mode = as.character(subset_mode),
        n_species = as.integer(n_species),
        cor = as.numeric(cor_val),
        CCD = as.numeric(ccd_val),
        SCR = as.numeric(scr_val),
        direction = .ccd_direction_label(cor_val),
        class_total_all_samples = as.numeric(class_total),
        subset_total_all_samples = as.numeric(subset_total),
        n_samples_used = as.integer(cor_info$n_samples_used %||% 0L),
        warning_flag = as.character(warning_flag %||% "")
      )
      row_id <- row_id + 1L
    }
  }

  if (!length(out)) {
    return(tibble::tibble(
      class = character(0),
      subset = character(0),
      subset_mode = character(0),
      n_species = integer(0),
      cor = numeric(0),
      CCD = numeric(0),
      SCR = numeric(0),
      direction = character(0),
      class_total_all_samples = numeric(0),
      subset_total_all_samples = numeric(0),
      n_samples_used = integer(0),
      warning_flag = character(0)
    ))
  }

  dplyr::bind_rows(out) %>%
    dplyr::arrange(dplyr::desc(.data$CCD), dplyr::desc(.data$SCR), .data$class, .data$subset)
}

#' Compute one-row-per-class CCD summary
#'
#' @param ccd_table Output of [compute_ccd_table()].
#' @param ccd_threshold CCD threshold used for interpretation.
#' @param scr_threshold SCR threshold used for interpretation.
#'
#' @return A tibble with one row per class.
#' @export
compute_ccd_class_summary <- function(ccd_table,
                                      ccd_threshold = 0.5,
                                      scr_threshold = 0.05) {
  df <- as.data.frame(ccd_table, check.names = FALSE)
  if (!nrow(df)) {
    return(tibble::tibble(
      class = character(0),
      top_subset = character(0),
      max_CCD = numeric(0),
      cor_top = numeric(0),
      SCR_top = numeric(0),
      n_subsets_tested = integer(0),
      interpretation = character(0)
    ))
  }

  split_df <- split(df, df$class)
  out <- lapply(split_df, function(d) {
    tested <- d[is.finite(d$CCD), , drop = FALSE]

    if (!nrow(tested)) {
      return(tibble::tibble(
        class = as.character(d$class[[1]]),
        top_subset = NA_character_,
        max_CCD = NA_real_,
        cor_top = NA_real_,
        SCR_top = NA_real_,
        n_subsets_tested = 0L,
        interpretation = "not_testable"
      ))
    }

    ord <- order(-tested$CCD, -tested$SCR, tested$subset)
    top <- tested[ord[1], , drop = FALSE]

    interpretation <- if (is.na(top$CCD[[1]])) {
      "not_testable"
    } else if (top$CCD[[1]] < ccd_threshold) {
      "stable class"
    } else if (top$CCD[[1]] >= ccd_threshold && (is.na(top$SCR[[1]]) || top$SCR[[1]] < scr_threshold)) {
      "minor divergent subset"
    } else {
      "major divergent subset"
    }

    tibble::tibble(
      class = as.character(top$class[[1]]),
      top_subset = as.character(top$subset[[1]]),
      max_CCD = as.numeric(top$CCD[[1]]),
      cor_top = as.numeric(top$cor[[1]]),
      SCR_top = as.numeric(top$SCR[[1]]),
      n_subsets_tested = as.integer(nrow(tested)),
      interpretation = interpretation
    )
  })

  dplyr::bind_rows(out) %>%
    dplyr::arrange(dplyr::desc(.data$max_CCD), .data$class)
}
