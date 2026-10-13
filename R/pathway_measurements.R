#' Export pathway structure and sample-level lipid measurements as JSON-LD
#'
#' Uses cyjs only as pathway input. Exports RDF entities, connections, samples,
#' subsets and measurements; no Cytoscape display attributes or comparisons.
#' Missing values remain missing, including all-missing sums.
#' @param x mzTab-M path or lipidomics SummarizedExperiment.
#' @param network cyjs path or parsed Cytoscape JSON list.
#' @param output_file Optional output JSON-LD (.json) path.
#' @param group_col Sample group column in colData.
#' @param subclass_col Lipid subclass column in rowData.
#' @param name_col Lipid name column in rowData.
#' @param assay_name Assay containing the supplied abundance values.
#' @param acyl_chains Chains for independent, overlapping subsets. NULL selects
#'   all parsed chains; character() disables subsets.
#' @param aggregate Within-sample aggregation: sum, mean or median.
#' @param exclude_sample_classes Exact sample groups to exclude; default none.
#' @param node_subclasses Optional named character vector: node ID to subclass.
#' @param dataset_id Dataset identifier.
#' @param normalization Description of upstream normalization, default unknown.
#' @param unit Abundance unit, default unspecified.
#' @param rules_file Optional chain-parser rules file.
#' @return List with document (JSON-LD), node_mapping, acyl_coverage and se.
#' @export
mztab_pathway_measurements <- function(
    x, network, output_file = NULL, group_col = "class",
    subclass_col = "subclass", name_col = "Metabolite name",
    assay_name = "abundance", acyl_chains = character(),
    aggregate = c("sum", "mean", "median"),
    exclude_sample_classes = character(), node_subclasses = NULL,
    dataset_id = NULL, normalization = "unknown", unit = "unspecified",
    rules_file = NULL
) {
  aggregate <- match.arg(aggregate)
  input_path <- NULL
  if (is.character(x) && length(x) == 1L) {
    input_path <- normalizePath(x, winslash = "/", mustWork = TRUE)
    se <- mztab_se_to_lipidomics_se(mztab_to_se(input_path, verbose = FALSE), verbose = FALSE)
  } else if (inherits(x, "SummarizedExperiment")) {
    se <- x
  } else stop("x must be an mzTab-M path or SummarizedExperiment.", call. = FALSE)
  if (!assay_name %in% SummarizedExperiment::assayNames(se)) {
    stop("assay_name not found: ", assay_name, call. = FALSE)
  }
  mat <- as.matrix(SummarizedExperiment::assay(se, assay_name))
  storage.mode(mat) <- "double"
  rd <- as.data.frame(SummarizedExperiment::rowData(se), optional = TRUE)
  cd <- as.data.frame(SummarizedExperiment::colData(se), optional = TRUE)
  if (!group_col %in% names(cd)) stop("group_col not found: ", group_col, call. = FALSE)
  if (!subclass_col %in% names(rd)) {
    hits <- c("Ontology", "opt_global_Ontology")
    hits <- hits[hits %in% names(rd)]
    if (!length(hits)) stop("No lipid subclass column found.", call. = FALSE)
    subclass_col <- hits[1]
  }
  groups <- trimws(as.character(cd[[group_col]]))
  keep <- !(groups %in% exclude_sample_classes)
  mat <- mat[, keep, drop = FALSE]; cd <- cd[keep, , drop = FALSE]; groups <- groups[keep]
  if (!nrow(mat) || !ncol(mat)) stop("At least one feature and sample are required.", call. = FALSE)
  sample_ids <- colnames(mat)
  if (is.null(sample_ids) || anyNA(sample_ids) || any(!nzchar(sample_ids)) || anyDuplicated(sample_ids)) {
    stop("Sample IDs must be non-empty and unique.", call. = FALSE)
  }
  feature_ids <- rownames(mat)
  if (is.null(feature_ids)) feature_ids <- paste0("feature_", seq_len(nrow(mat)))
  if (anyNA(feature_ids) || any(!nzchar(feature_ids)) || anyDuplicated(feature_ids)) {
    stop("Feature IDs must be non-empty and unique.", call. = FALSE)
  }
  subclasses <- trimws(as.character(rd[[subclass_col]]))
  subclasses[is.na(subclasses) | !nzchar(subclasses)] <- "Unknown"
  lipid_names <- if (name_col %in% names(rd)) as.character(rd[[name_col]]) else feature_ids
  mat[!is.finite(mat)] <- NA_real_
  chains <- rep(list(character()), nrow(mat))
  auto_chains <- is.null(acyl_chains)
  acyl_chains <- unique(trimws(as.character(acyl_chains)))
  acyl_chains <- acyl_chains[!is.na(acyl_chains) & nzchar(acyl_chains)]
  # Use the established parser; do not guess chains from sum-composition names.
  if ("acyl_chains" %in% names(rd) && is.list(rd$acyl_chains)) {
    chains <- lapply(rd$acyl_chains, function(z) unique(as.character(z[!is.na(z)])))
  } else if (exists("extract_acyl_chains_from_name", mode = "function")) {
    if (is.null(rules_file)) rules_file <- .mslm_rules_yaml_path()
    if (!is.null(rules_file) && file.exists(rules_file)) {
      rules <- load_lipid_rules(rules_file)
      # The parser returns a vector for one name and a list for several names.
      chains <- lapply(lipid_names, function(nm) extract_acyl_chains_from_name(nm, rules))
      # Some existing phospholipid rules accept sum-only names. Such a token
      # does not identify an individual chain in a multi-chain lipid.
      sum_classes <- c("PC", "PE", "PS", "PG", "PI", "PA", "DG", "TG", "CL",
        "BMP", "HBMP", "MLCL", "DLCL", "MGDG", "DGDG", "SQDG", "DGTS", "DGCC")
      for (j in seq_along(chains)) {
        nm <- lipid_names[j]
        if (!is.na(nm) && subclasses[j] %in% sum_classes &&
            grepl("^[A-Za-z0-9-]+[ (]+(?:O-|P-)?[0-9]+:[0-9]+(?:;O[0-9]*)?\\)?$", nm, perl = TRUE)) {
          chains[[j]] <- character()
        }
      }
    } else if (length(acyl_chains) || auto_chains) stop("Chain parsing rules not found.", call. = FALSE)
  } else if (length(acyl_chains) || auto_chains) stop("Chain parser not available.", call. = FALSE)
  if (auto_chains) acyl_chains <- sort(unique(unlist(chains, use.names = FALSE)))
  obj <- if (is.character(network) && length(network) == 1L) {
    jsonlite::fromJSON(network, simplifyVector = FALSE)
  } else network
  if (!is.list(obj) || !is.list(obj$elements) || is.null(obj$elements$nodes)) {
    stop("network must contain elements.nodes and elements.edges arrays.", call. = FALSE)
  }
  if (is.null(obj$elements$edges)) obj$elements$edges <- list()
  ids <- vapply(obj$elements$nodes, function(n) as.character(n$data$id), character(1))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) stop("Network node IDs must be unique.", call. = FALSE)
  for (e in obj$elements$edges) {
    if (!all(c(e$data$source, e$data$target) %in% ids) ||
        is.null(e$data$source) || is.null(e$data$target)) {
      stop("Network edge has an unknown or missing endpoint.", call. = FALSE)
    }
  }
  if (!is.null(node_subclasses) && (is.null(names(node_subclasses)) ||
      anyDuplicated(names(node_subclasses)) || !all(names(node_subclasses) %in% ids))) {
    stop("node_subclasses must be named by existing unique node IDs.", call. = FALSE)
  }
  # Values as named objects, even for a single sample; NA becomes JSON null.
  values <- function(v) stats::setNames(as.list(as.numeric(v)), sample_ids)
  coverage <- function(ii) colSums(is.finite(mat[ii, , drop = FALSE]))
  measure <- function(ii) {
    if (!length(ii)) return(rep(NA_real_, ncol(mat)))
    apply(mat[ii, , drop = FALSE], 2L, function(z) {
      z <- z[is.finite(z)]
      if (!length(z)) NA_real_ else switch(aggregate, sum = sum(z), mean = mean(z), median = stats::median(z))
    })
  }
  scope <- function(ii, type, signature = character()) list(
    scope = type, definition = list(acyl_chains = unname(as.list(signature)), match = "any"),
    member_feature_ids = unname(as.list(feature_ids[ii])),
    aggregation_method = aggregate, measurements = values(measure(ii)),
    valid_feature_counts = stats::setNames(as.list(as.integer(coverage(ii))), sample_ids),
    detection_status = if (length(ii)) "Observed" else "NotMeasured"
  )
  map <- utils::read.csv(mslipidmapper_pathway_knowledge_path("mappings", "msdial-lipidmaps.csv"),
    stringsAsFactors = FALSE, colClasses = "character", na.strings = character())
  mapping_rows <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    d <- obj$elements$nodes[[i]]$data
    explicit <- !is.null(node_subclasses) && ids[i] %in% names(node_subclasses)
    candidates <- if (explicit) node_subclasses[[ids[i]]] else
      unlist(d[c("subclass", "Ontology", "lipid_class", "label", "name", "shared_name")], use.names = FALSE)
    candidates <- trimws(as.character(candidates))
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    hits <- candidates[candidates %in% unique(c(subclasses, map$msdial_ontology))]
    cls <- if (explicit && length(candidates)) candidates[1] else if (length(hits)) hits[1] else NA_character_
    mapping_rows[[i]] <- data.frame(node_id = ids[i], subclass = cls,
      status = if (is.na(cls)) "unmatched" else if (cls %in% subclasses) "measured" else "not_measured")
    if (!is.na(cls)) {
      ii <- which(subclasses == cls)
      uri <- map$canonical_uri[match(cls, map$msdial_ontology)]
      subsets <- lapply(acyl_chains, function(chain) {
        jj <- ii[vapply(chains[ii], function(z) chain %in% z, logical(1))]
        scope(jj, "acyl_subset", chain)
      })
      d$mslm <- list(entity_type = "lipid_subclass", subclass = cls,
        canonical_uri = if (is.na(uri)) NA_character_ else uri,
        whole_subclass = scope(ii, "subclass"), subsets = unname(subsets))
      obj$elements$nodes[[i]]$data <- d
    } else {
      # Clear only our measurements when reusing an enriched network.
      obj$elements$nodes[[i]]$data$mslm <- NULL
    }
  }
  if (is.null(dataset_id)) dataset_id <- if (is.null(input_path)) "in_memory_dataset" else tools::file_path_sans_ext(basename(input_path))
  samples <- lapply(seq_along(sample_ids), function(i) list(
    id = sample_ids[i], group = if (is.na(groups[i]) || !nzchar(groups[i])) NA_character_ else groups[i],
    metadata = as.list(cd[i, , drop = FALSE])))
  features <- lapply(seq_along(feature_ids), function(i) list(
    id = feature_ids[i], lipid_name = lipid_names[i], subclass = subclasses[i],
    acyl_chains = unname(as.list(chains[[i]])), measurements = values(mat[i, ])))
  if (is.null(obj$data)) obj$data <- list()
  obj$data$mslm <- list(schema_version = "1.0.0", dataset_id = dataset_id,
    group_column = group_col, assay_name = assay_name, normalization = normalization,
    unit = unit, aggregation_method = aggregate,
    missing_value_policy = "null; aggregate finite values; all missing remains null",
    pathway_semantics = "existing graph connections; no chain-specific reaction inference",
    excluded_sample_classes = unname(as.list(exclude_sample_classes)),
    samples = samples, features = features,
    unmapped_subclasses = unname(as.list(setdiff(unique(subclasses),
      vapply(mapping_rows, function(z) z$subclass, character(1))))))
  document <- .mslm_measurements_jsonld(obj)
  if (!is.null(output_file)) {
    jsonlite::write_json(document, output_file, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null", digits = NA)
  }
  list(document = document, node_mapping = do.call(rbind, mapping_rows),
    acyl_coverage = data.frame(acyl_chain = acyl_chains,
      n_features = vapply(acyl_chains, function(ch) sum(vapply(chains, function(z) ch %in% z, logical(1))), integer(1))),
    se = se)
}

# Explicit RDF node references avoid treating IDs as literals. Every value has
# its own identified measurement resource, preserving sample and scope identity.
.mslm_measurements_jsonld <- function(obj) {
  meta <- obj$data$mslm
  vocab <- "https://systemomicslab.github.io/MSLipidMapper/ontology/"
  encode <- function(x) utils::URLencode(enc2utf8(as.character(x)), reserved = TRUE)
  base <- paste0("https://systemomicslab.github.io/MSLipidMapper/dataset/", encode(meta$dataset_id), "/")
  uri <- function(kind, id) paste0(base, kind, "/", encode(id))
  ref <- function(id) list("@id" = id)
  node <- function(id, type, ...) c(list("@id" = id, "@type" = paste0("mslm:", type)), list(...))
  graph <- list()
  add <- function(n) { graph[[length(graph) + 1L]] <<- n; invisible(n[["@id"]]) }
  ds <- uri("dataset", meta$dataset_id)
  pw <- uri("pathway", "imported")
  add(node(ds, "Dataset", schemaVersion = "2.0.0", identifier = meta$dataset_id,
    groupColumn = meta$group_column, assayName = meta$assay_name,
    normalization = meta$normalization, unit = meta$unit,
    aggregationMethod = meta$aggregation_method, missingValuePolicy = meta$missing_value_policy,
    excludedSampleClass = meta$excluded_sample_classes, pathway = ref(pw)))
  add(node(pw, "Pathway", label = "Imported metabolic pathway",
    assertionMethod = ref(paste0(vocab, "ImportedPathway"))))
  for (s in meta$samples) {
    n <- node(uri("sample", s$id), "Sample", identifier = s$id, inDataset = ref(ds))
    if (!is.na(s$group)) n$sampleGroup <- s$group
    # JSON literal keeps original metadata losslessly, without inventing RDF
    # predicates from arbitrary user-provided column names.
    n$sampleMetadata <- list("@value" = s$metadata, "@type" = "@json")
    add(n)
  }
  subclass_uri <- function(cls) uri("subclass", cls)
  known <- unique(c(vapply(meta$features, function(f) f$subclass, character(1)),
    vapply(Filter(function(n) !is.null(n$data$mslm), obj$elements$nodes),
      function(n) n$data$mslm$subclass, character(1))))
  for (cls in known) {
    n <- node(subclass_uri(cls), "MSDialMappableLipidEntity", label = cls,
      msdialOntologyLabel = cls, resolutionLevel = ref(paste0(vocab, "SubclassLevel")))
    mapped <- Filter(function(z) !is.null(z$data$mslm) && identical(z$data$mslm$subclass, cls), obj$elements$nodes)
    if (length(mapped)) {
      canonical <- mapped[[1]]$data$mslm$canonical_uri
      if (!is.na(canonical)) n$canonicalEntity <- ref(canonical)
    }
    add(n)
  }
  add_measurements <- function(owner, vals, counts = NULL) {
    for (s in names(vals)) {
      val <- vals[[s]]
      id <- paste0(owner, "/sample/", encode(s))
      n <- node(id, "SampleMeasurement", measuredEntity = ref(owner),
        inSample = ref(uri("sample", s)), inDataset = ref(ds),
        valueStatus = ref(paste0(vocab, if (is.finite(val)) "AvailableValue" else "MissingValue")))
      # null is omitted by RDF processors; explicit MissingValue retains the
      # missing observation as RDF even though its numerical value is absent.
      n$value <- if (is.finite(val)) val else NA_real_
      if (!is.null(counts)) n$validFeatureCount <- counts[[s]]
      add(n)
    }
  }
  for (f in meta$features) {
    id <- uri("feature", f$id)
    add(node(id, "MolecularFeature", identifier = f$id, label = f$lipid_name,
      lipidSubclass = ref(subclass_uri(f$subclass)), acylChainSignature = f$acyl_chains,
      inDataset = ref(ds)))
    add_measurements(id, f$measurements)
  }
  # Layout-only junction nodes have no biological identity but are retained as
  # PathwayJunction resources to preserve topology without invented reactions.
  for (n in obj$elements$nodes) {
    d <- n$data
    id <- uri("pathway-node", d$id)
    label <- unlist(d[c("label", "name", "shared_name")], use.names = FALSE)
    label <- as.character(label[!is.na(label) & nzchar(label)])
    lipid <- d$mslm
    type <- if (!is.null(lipid)) "PathwayLipidNode" else if (!length(label) ||
      grepl("^Node[0-9]*$", label[1])) "PathwayJunction" else "PathwayEntity"
    entity <- node(id, type, inPathway = ref(pw))
    if (length(label) && type != "PathwayJunction") entity$label <- label[1]
    if (!is.null(lipid)) entity$representsEntity <- ref(subclass_uri(lipid$subclass))
    # Allowlisted biological annotations only; never copy path/style/SUID.
    for (key in c("Ensembl_ID", "LipidMAPS_ID", "geneSymbol", "enzymeName", "reactionId")) {
      val <- d[[key]]
      if (!is.null(val) && length(val) == 1L && !is.na(val) && nzchar(as.character(val))) {
        prop <- switch(key, Ensembl_ID = "ensemblId", LipidMAPS_ID = "lipidMapsId", key)
        entity[[prop]] <- as.character(val)
      }
    }
    add(entity)
    if (!is.null(lipid)) {
      scopes <- c(list(lipid$whole_subclass), lipid$subsets)
      for (scope in scopes) {
        sig <- unlist(scope$definition$acyl_chains, use.names = FALSE)
        sid <- paste0(id, "/scope/", if (scope$scope == "subclass") "whole" else paste0("acyl-", encode(sig)))
        observation <- node(sid, "LipidMeasurementScope", inDataset = ref(ds),
          onPathwayNode = ref(id), observedLipidSubclass = ref(subclass_uri(lipid$subclass)),
          scopeType = scope$scope, matchMode = scope$definition$match,
          acylChainSignature = scope$definition$acyl_chains,
          aggregationMethod = scope$aggregation_method,
          memberFeature = lapply(scope$member_feature_ids, function(f) ref(uri("feature", f))),
          detectionStatus = ref(paste0(vocab, scope$detection_status)))
        add(observation)
        add_measurements(sid, scope$measurements, scope$valid_feature_counts)
      }
    }
  }
  for (i in seq_along(obj$elements$edges)) {
    d <- obj$elements$edges[[i]]$data
    # Source/target describe the imported graph's connection, not a proven
    # stoichiometric or chain-preserving biochemical reaction.
    connection <- node(uri("connection", as.character(i)), "PathwayConnection",
      inPathway = ref(pw), sourceEntity = ref(uri("pathway-node", d$source)),
      targetEntity = ref(uri("pathway-node", d$target)),
      assertionMethod = ref(paste0(vocab, "ImportedPathway")))
    for (key in c("interaction", "reactionId", "enzymeName", "geneSymbol", "sourceDatabase", "sourceIdentifier")) {
      val <- d[[key]]
      if (!is.null(val) && length(val) == 1L && !is.na(val) && nzchar(as.character(val))) connection[[key]] <- as.character(val)
    }
    add(connection)
  }
  list("@context" = list("@version" = 1.1, "@vocab" = vocab, mslm = vocab,
    xsd = "http://www.w3.org/2001/XMLSchema#",
    value = list("@id" = paste0(vocab, "value"), "@type" = "xsd:double"),
    validFeatureCount = list("@id" = paste0(vocab, "validFeatureCount"), "@type" = "xsd:integer")),
    "@graph" = graph)
}
