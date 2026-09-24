#' Compute pairwise lipid-subclass statistics and map them to RDF
#'
#' Accepts an mzTab-M file or a lipidomics-style `SummarizedExperiment` and
#' computes all pairwise sample-class comparisons. Statistics are calculated
#' independently for complete lipid subclasses and for molecular-species
#' subsets containing specified acyl chains. No substrate/product ratio is
#' calculated.
#'
#' @param x Path to mzTab-M or a `SummarizedExperiment`.
#' @param group_col Column in `colData` defining sample classes.
#' @param subclass_col Column in `rowData` containing MS-DIAL Ontology labels.
#' @param name_col Column in `rowData` containing lipid names.
#' @param assay_name Assay containing abundance values.
#' @param exclude_sample_classes Sample classes excluded before pair generation.
#' @param acyl_chains Character vector such as `c("18:0", "24:1")`.
#' @param aggregate Sum, mean, or median across molecular species per sample.
#' @param test Two-sample Welch t-test or Wilcoxon rank-sum test.
#' @param pseudocount Positive value used only for log2 fold-change.
#' @param p_adjust_method Method passed to `p.adjust` within each comparison and scope.
#' @param dataset_id Identifier used in generated RDF.
#' @param output_file Optional Turtle output path.
#' @param rules_file Optional lipid-chain parsing rules YAML.
#' @return A list containing `statistics`, `turtle`, and the imported `se`.
#' @export
mztab_pairwise_subclass_rdf <- function(
    x,
    group_col = "class",
    subclass_col = "subclass",
    name_col = "Metabolite name",
    assay_name = "abundance",
    exclude_sample_classes = c("Blank", "QC"),
    acyl_chains = character(),
    aggregate = c("sum", "mean", "median"),
    test = c("welch_t", "wilcox"),
    pseudocount = 1,
    p_adjust_method = "BH",
    dataset_id = NULL,
    output_file = NULL,
    rules_file = NULL
) {
  aggregate <- match.arg(aggregate)
  test <- match.arg(test)
  if (!is.numeric(pseudocount) || length(pseudocount) != 1L ||
      !is.finite(pseudocount) || pseudocount <= 0) {
    stop("pseudocount must be one positive finite number.", call. = FALSE)
  }

  input_path <- NULL
  if (is.character(x) && length(x) == 1L) {
    input_path <- normalizePath(x, winslash = "/", mustWork = TRUE)
    parsed <- mztab_to_se(input_path, verbose = FALSE)
    se <- mztab_se_to_lipidomics_se(parsed, verbose = FALSE)
  } else if (inherits(x, "SummarizedExperiment")) {
    se <- x
  } else {
    stop("x must be an mzTab-M path or SummarizedExperiment.", call. = FALSE)
  }

  assay_names <- SummarizedExperiment::assayNames(se)
  assay_index <- if (assay_name %in% assay_names) assay_name else 1L
  mat <- as.matrix(SummarizedExperiment::assay(se, assay_index))
  storage.mode(mat) <- "double"
  rd <- as.data.frame(SummarizedExperiment::rowData(se), stringsAsFactors = FALSE)
  cd <- as.data.frame(SummarizedExperiment::colData(se), stringsAsFactors = FALSE)
  if (!group_col %in% names(cd)) stop("group_col not found in colData: ", group_col, call. = FALSE)
  if (!subclass_col %in% names(rd)) {
    fallback <- c("Ontology", "opt_global_Ontology")
    hit <- fallback[fallback %in% names(rd)]
    if (!length(hit)) stop("subclass_col not found in rowData: ", subclass_col, call. = FALSE)
    subclass_col <- hit[1]
  }
  if (!name_col %in% names(rd)) {
    names_vec <- rownames(mat)
  } else {
    names_vec <- as.character(rd[[name_col]])
  }
  subclasses <- trimws(as.character(rd[[subclass_col]]))
  subclasses[is.na(subclasses) | !nzchar(subclasses)] <- "Unknown"
  groups <- trimws(as.character(cd[[group_col]]))
  valid_sample <- !is.na(groups) & nzchar(groups) & !groups %in% exclude_sample_classes
  mat <- mat[, valid_sample, drop = FALSE]
  groups <- groups[valid_sample]
  levels_present <- sort(unique(groups))
  if (length(levels_present) < 2L) stop("At least two non-empty sample classes are required.", call. = FALSE)
  comparisons <- utils::combn(levels_present, 2L, simplify = FALSE)

  chain_lists <- vector("list", nrow(mat))
  if (length(acyl_chains)) {
    if (is.null(rules_file) && exists(".mslm_rules_yaml_path", mode = "function")) {
      rules_file <- .mslm_rules_yaml_path()
    }
    if (!is.null(rules_file) && file.exists(rules_file) &&
        exists("load_lipid_rules", mode = "function") &&
        exists("extract_acyl_chains_from_name", mode = "function")) {
      rules <- load_lipid_rules(rules_file)
      chain_lists <- extract_acyl_chains_from_name(names_vec, rules)
    } else {
      chain_lists <- lapply(names_vec, function(nm) {
        hit <- regmatches(nm, gregexpr("(?:O-|P-)?[0-9]+:[0-9]+(?:;O[0-9]*)?", nm, perl = TRUE))[[1]]
        if (identical(hit, character(0)) || identical(hit, "")) character() else unique(sub("^(O-|P-)", "", hit))
      })
    }
  }

  aggregate_rows <- function(feature_index) {
    split_index <- split(feature_index, subclasses[feature_index])
    fun <- switch(aggregate,
      sum = function(z) colSums(z, na.rm = TRUE),
      mean = function(z) colMeans(z, na.rm = TRUE),
      median = function(z) apply(z, 2L, stats::median, na.rm = TRUE)
    )
    out <- t(vapply(split_index, function(ii) fun(mat[ii, , drop = FALSE]), numeric(ncol(mat))))
    rownames(out) <- names(split_index)
    out
  }

  scopes <- list(subclass = seq_len(nrow(mat)))
  acyl_chains <- unique(trimws(as.character(acyl_chains)))
  acyl_chains <- acyl_chains[nzchar(acyl_chains)]
  for (chain in acyl_chains) {
    keep <- which(vapply(chain_lists, function(z) chain %in% z, logical(1)))
    if (length(keep)) scopes[[paste0("acyl:", chain)]] <- keep
  }
  acyl_coverage <- data.frame(
    acyl_chain = acyl_chains,
    n_features = vapply(acyl_chains, function(chain) {
      sum(vapply(chain_lists, function(z) chain %in% z, logical(1)))
    }, integer(1)),
    stringsAsFactors = FALSE
  )

  safe_test <- function(a, b) {
    a <- a[is.finite(a)]; b <- b[is.finite(b)]
    if (length(a) < 2L || length(b) < 2L) return(NA_real_)
    tryCatch(if (test == "welch_t") stats::t.test(a, b)$p.value else
      stats::wilcox.test(a, b, exact = FALSE)$p.value, error = function(e) NA_real_)
  }

  results <- list()
  k <- 0L
  for (scope_name in names(scopes)) {
    agg <- aggregate_rows(scopes[[scope_name]])
    chain <- if (startsWith(scope_name, "acyl:")) substring(scope_name, 6L) else NA_character_
    scope_type <- if (is.na(chain)) "subclass" else "acyl_subset"
    for (pair in comparisons) {
      ia <- which(groups == pair[1]); ib <- which(groups == pair[2])
      rows <- lapply(seq_len(nrow(agg)), function(i) {
        a <- as.numeric(agg[i, ia]); b <- as.numeric(agg[i, ib])
        mean_a <- if (any(is.finite(a))) mean(a, na.rm = TRUE) else NA_real_
        mean_b <- if (any(is.finite(b))) mean(b, na.rm = TRUE) else NA_real_
        data.frame(
          group_a = pair[1], group_b = pair[2], scope = scope_type,
          acyl_chain = chain, subclass = rownames(agg)[i],
          mean_a = mean_a, mean_b = mean_b,
          effect_size = log2((mean_a + pseudocount) / (mean_b + pseudocount)),
          effect_size_type = "log2_fold_change", p_value = safe_test(a, b),
          sample_size_a = sum(is.finite(a)), sample_size_b = sum(is.finite(b)),
          n_molecular_species = length(split(scopes[[scope_name]], subclasses[scopes[[scope_name]]])[[rownames(agg)[i]]]),
          aggregation_method = aggregate, stringsAsFactors = FALSE
        )
      })
      block <- do.call(rbind, rows)
      block$adjusted_p_value <- stats::p.adjust(block$p_value, method = p_adjust_method)
      k <- k + 1L; results[[k]] <- block
    }
  }
  statistics <- do.call(rbind, results)
  rownames(statistics) <- NULL

  mapping <- utils::read.csv(
    mslipidmapper_pathway_knowledge_path("mappings", "msdial-lipidmaps.csv"),
    stringsAsFactors = FALSE, colClasses = "character", na.strings = character()
  )
  statistics$canonical_uri <- mapping$canonical_uri[match(statistics$subclass, mapping$msdial_ontology)]
  statistics$mapping_status <- ifelse(is.na(statistics$canonical_uri), "unmapped", "mapped")
  if (is.null(dataset_id) || !nzchar(dataset_id)) {
    dataset_id <- if (!is.null(input_path)) tools::file_path_sans_ext(basename(input_path)) else "in_memory_dataset"
  }
  turtle <- .mslm_subclass_statistics_turtle(statistics, dataset_id)
  if (!is.null(output_file)) writeLines(turtle, output_file, useBytes = TRUE)
  list(statistics = statistics, acyl_coverage = acyl_coverage, turtle = turtle, se = se)
}

.mslm_subclass_statistics_turtle <- function(statistics, dataset_id) {
  esc <- function(x) gsub('"', '\\"', gsub("[\r\n\t]+", " ", as.character(x)))
  slug <- function(x) gsub("[^A-Za-z0-9._~-]+", "_", as.character(x))
  num <- function(predicate, value, datatype = "decimal") {
    if (!is.finite(value)) "" else sprintf("  mslm:%s \"%s\"^^xsd:%s ;\n", predicate, format(value, digits = 15), datatype)
  }
  ds <- slug(dataset_id)
  header <- paste0(
    "@prefix mslm: <https://systemomicslab.github.io/MSLipidMapper/ontology/> .\n",
    "@prefix res: <https://systemomicslab.github.io/MSLipidMapper/resource/> .\n",
    "@prefix state: <https://systemomicslab.github.io/MSLipidMapper/state/", ds, "/> .\n",
    "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n",
    "state:dataset a mslm:Dataset .\n\n"
  )
  comparisons <- unique(statistics[c("group_a", "group_b")])
  comparison_lines <- vapply(seq_len(nrow(comparisons)), function(i) {
    id <- paste0("comparison_", slug(comparisons$group_a[i]), "_vs_", slug(comparisons$group_b[i]))
    sprintf("state:%s a mslm:Comparison ; mslm:conditionA \"%s\" ; mslm:conditionB \"%s\" .",
      id, esc(comparisons$group_a[i]), esc(comparisons$group_b[i]))
  }, character(1))
  observation_lines <- vapply(seq_len(nrow(statistics)), function(i) {
    z <- statistics[i, ]
    cmp <- paste0("comparison_", slug(z$group_a), "_vs_", slug(z$group_b))
    obs <- paste0("obs_", i, "_", slug(z$subclass), if (!is.na(z$acyl_chain)) paste0("_", slug(z$acyl_chain)) else "")
    lipid_uri <- if (!is.na(z$canonical_uri) && nzchar(z$canonical_uri)) {
      paste0("<", z$canonical_uri, ">")
    } else {
      paste0("res:unmapped_", slug(z$subclass))
    }
    paste0(
      "state:", obs, " a mslm:LipidSubclassObservation ;\n",
      "  mslm:inDataset state:dataset ; mslm:inComparison state:", cmp, " ;\n",
      "  mslm:observedLipidSubclass ", lipid_uri, " ; mslm:detectionStatus mslm:Observed ;\n",
      if (!is.na(z$acyl_chain)) paste0("  mslm:acylChainSignature \"", esc(z$acyl_chain), "\" ;\n") else "",
      num("meanA", z$mean_a), num("meanB", z$mean_b), num("effectSize", z$effect_size),
      "  mslm:effectSizeType \"", z$effect_size_type, "\" ;\n",
      num("pValue", z$p_value), num("adjustedPValue", z$adjusted_p_value),
      num("sampleSizeA", z$sample_size_a, "integer"), num("sampleSizeB", z$sample_size_b, "integer"),
      "  mslm:aggregationMethod \"", esc(z$aggregation_method), "\" ."
    )
  }, character(1))
  paste(c(header, comparison_lines, "", observation_lines), collapse = "\n")
}
