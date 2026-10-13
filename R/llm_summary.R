# Compact export intended as LLM input. Unlike the JSON-LD measurements it uses
# plain names instead of URIs, stores values as arrays aligned to one sample
# order, and represents conversions as explicit reaction nodes.

# Reactions from a Cytoscape pathway. Junction nodes (no label, "Node 1"-style
# or purely numeric labels) are reaction points; junctions joined to each other
# form one reaction. Direct entity-to-entity edges are single-step reactions.
.mslm_network_reactions <- function(obj) {
  nodes <- obj$elements$nodes
  edges <- obj$elements$edges %||% list()
  label_of <- function(d) {
    v <- d$label
    if (is.null(v) || !length(v) || is.na(v[1])) "" else trimws(as.character(v[1]))
  }
  ids <- vapply(nodes, function(n) as.character(n$data$id), character(1))
  labels <- stats::setNames(vapply(nodes, function(n) label_of(n$data), character(1)), ids)
  junction <- stats::setNames(!nzchar(labels) | grepl("^(Node ?[0-9]*|[0-9]+)$", labels), ids)
  src <- vapply(edges, function(e) as.character(e$data$source), character(1))
  tgt <- vapply(edges, function(e) as.character(e$data$target), character(1))

  parent <- stats::setNames(ids, ids)
  root <- function(x) { while (parent[[x]] != x) x <- parent[[x]]; x }
  for (k in which(junction[src] & junction[tgt])) {
    a <- root(src[k]); b <- root(tgt[k])
    if (a != b) parent[[a]] <- b
  }

  reactions <- list()
  add <- function(subs, prods) {
    subs <- unique(subs); prods <- unique(prods)
    if (length(subs) && length(prods)) reactions[[length(reactions) + 1L]] <<- list(substrates = subs, products = prods)
  }
  for (k in which(!junction[src] & !junction[tgt])) add(labels[[src[k]]], labels[[tgt[k]]])
  roots <- vapply(ids[junction], root, character(1))
  for (r in unique(roots)) {
    members <- ids[junction][roots == r]
    add(unname(labels[src[tgt %in% members & !junction[src]]]),
        unname(labels[tgt[src %in% members & !junction[tgt]]]))
  }
  list(entities = unique(unname(labels[!junction])), reactions = reactions)
}

# Reaction metadata maintained as a CSV; multi-valued cells use ";".
.mslm_reaction_annotations <- function(path = mslipidmapper_pathway_knowledge_path("reaction-annotations.csv")) {
  if (!nzchar(path) || !file.exists(path)) return(data.frame())
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = character(), check.names = FALSE)
}

.mslm_annotate_reaction <- function(reaction, annotations) {
  if (!nrow(annotations)) return(reaction)
  hits <- annotations[annotations$substrate %in% reaction$substrates &
                      annotations$product %in% reaction$products, , drop = FALSE]
  cells <- function(x) { x <- trimws(unlist(strsplit(x, ";", fixed = TRUE))); x[nzchar(x)] }
  for (i in seq_len(nrow(hits))) {
    row <- hits[i, ]
    enzyme <- list(name = row$enzyme, genes = as.list(cells(row$genes)))
    if (nzchar(row$compartment)) enzyme$compartment <- row$compartment
    reaction$enzymes <- c(reaction$enzymes, list(Filter(function(v) length(v) && !identical(v, ""), enzyme)))
    reaction$cosubstrates <- as.list(unique(c(unlist(reaction$cosubstrates), cells(row$cosubstrates))))
    reaction$coproducts <- as.list(unique(c(unlist(reaction$coproducts), cells(row$coproducts))))
    if (nzchar(row$note)) reaction$notes <- as.list(unique(c(unlist(reaction$notes), row$note)))
  }
  Filter(length, reaction)
}

.mslm_llm_summary <- function(prepared, networks, output_file = NULL, dataset = list(),
                              aggregate = "sum", include_subsets = FALSE,
                              acyl_chains = character(), include_features = FALSE,
                              annotations = .mslm_reaction_annotations()) {
  se <- prepared$se
  assay <- if ("abundance" %in% SummarizedExperiment::assayNames(se)) "abundance" else 1L
  mat <- as.matrix(SummarizedExperiment::assay(se, assay))
  storage.mode(mat) <- "double"
  mat[!is.finite(mat)] <- NA_real_
  rd <- as.data.frame(SummarizedExperiment::rowData(se), optional = TRUE)
  subclass_col <- intersect(c("subclass", "Ontology", "opt_global_Ontology"), names(rd))[1]
  if (is.na(subclass_col)) stop("No lipid subclass column found.", call. = FALSE)
  subclasses <- trimws(as.character(rd[[subclass_col]]))
  subclasses[is.na(subclasses) | !nzchar(subclasses)] <- "Unknown"
  feature_ids <- rownames(mat) %||% paste0("feature_", seq_len(nrow(mat)))
  lipid_names <- if ("Metabolite name" %in% names(rd)) as.character(rd[["Metabolite name"]]) else feature_ids
  samples <- colnames(mat)
  groups <- as.character(prepared$groups)

  num <- function(v) as.list(signif(unname(as.numeric(v)), 6))
  agg <- function(ii) apply(mat[ii, , drop = FALSE], 2L, function(z) {
    z <- z[is.finite(z)]
    if (!length(z)) NA_real_ else switch(aggregate, sum = sum(z), mean = mean(z), median = stats::median(z))
  })
  by_group <- function(v) lapply(split(v, factor(groups, levels = unique(groups))), function(z) {
    z <- z[is.finite(z)]
    list(mean = if (length(z)) signif(mean(z), 6) else NA_real_,
         sd = if (length(z) > 1L) signif(stats::sd(z), 6) else NA_real_, n = length(z))
  })
  measured <- function(ii) {
    v <- agg(ii)
    list(values = num(v), n_features = length(ii), groups = by_group(v))
  }

  chains <- NULL
  if (isTRUE(include_subsets) || isTRUE(include_features)) {
    chains <- .mslm_feature_chains(rd, lipid_names, subclasses)
  }
  if (is.null(chains)) chains <- rep(list(character()), nrow(mat))
  if (isTRUE(include_subsets) && !length(acyl_chains)) {
    acyl_chains <- sort(unique(unlist(chains, use.names = FALSE)))
  }

  parsed <- lapply(networks, function(path) .mslm_network_reactions(jsonlite::fromJSON(path, simplifyVector = FALSE)))
  pathways_of <- function(name) as.list(names(parsed)[vapply(parsed, function(p) name %in% p$entities, logical(1))])

  lipids <- list()
  for (cls in unique(c(sort(unique(subclasses)), unlist(lapply(parsed, `[[`, "entities"))))) {
    ii <- which(subclasses == cls)
    entry <- list(pathways = pathways_of(cls))
    if (length(ii)) {
      entry <- c(entry, measured(ii))
      if (isTRUE(include_subsets)) {
        subsets <- lapply(acyl_chains, function(ch) {
          jj <- ii[vapply(chains[ii], function(z) ch %in% z, logical(1))]
          if (length(jj)) measured(jj)
        })
        names(subsets) <- acyl_chains
        subsets <- Filter(Negate(is.null), subsets)
        if (length(subsets)) entry$acyl_subsets <- subsets
      }
    } else {
      entry$status <- "not_measured"
    }
    lipids[[cls]] <- entry
  }

  # Identical conversions appearing in several pathways are listed once.
  reactions <- list()
  for (pw in names(parsed)) for (r in parsed[[pw]]$reactions) {
    key <- paste(paste(sort(r$substrates), collapse = "+"), paste(sort(r$products), collapse = "+"), sep = ">")
    if (is.null(reactions[[key]])) {
      reactions[[key]] <- list(substrates = as.list(r$substrates), products = as.list(r$products), pathways = list(pw))
    } else {
      reactions[[key]]$pathways <- c(reactions[[key]]$pathways, list(pw))
    }
  }
  reactions <- unname(lapply(reactions, function(r) {
    r <- .mslm_annotate_reaction(r, annotations)
    r$pathways <- unique(r$pathways)
    r
  }))
  reactions <- Map(function(r, i) c(list(id = paste0("R", i)), r), reactions, seq_along(reactions))

  summary <- list(
    format = "mslipidmapper-llm-summary/1",
    about = paste(
      "Lipidomics measurements mapped onto lipid pathways.",
      "'lipids' holds per-sample values aligned to samples.ids, per-group mean/sd/n, and the pathways containing each lipid;",
      "lipids with status not_measured appear in a pathway but not in the data.",
      "'reactions' convert substrates into products and may list enzymes.",
      "Values are after the stated normalization and aggregation; null means missing."),
    dataset = dataset,
    samples = list(ids = as.list(samples), groups = as.list(groups)),
    lipids = lipids,
    reactions = reactions,
    unmapped_classes = as.list(setdiff(sort(unique(subclasses)), unlist(lapply(parsed, `[[`, "entities"))))
  )
  if (isTRUE(include_features)) {
    summary$features <- lapply(seq_len(nrow(mat)), function(i) Filter(length, list(
      name = lipid_names[i], class = subclasses[i], chains = as.list(chains[[i]]), values = num(mat[i, ]))))
  }
  if (!is.null(output_file)) {
    jsonlite::write_json(summary, output_file, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
  }
  invisible(summary)
}
