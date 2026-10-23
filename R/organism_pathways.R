#' List built organism-specific class pathway datasets
#' @param release Knowledge release identifier (not the R package version).
#' @return Catalog data frame with coverage status and counts.
#' @export
list_organism_pathway_datasets <- function(release = "0.4.0") {
  root <- .organism_release_path(release)
  catalog <- jsonlite::read_json(file.path(root, "catalog.json"), simplifyVector = FALSE)
  do.call(rbind, lapply(catalog$organisms, function(x) data.frame(
    taxon_id = x$taxon_id, scientific_name = x$scientific_name,
    status = x$status, n_classes = x$counts$classes,
    n_reactions = x$counts$reactions, stringsAsFactors = FALSE)))
}

.organism_release_path <- function(release) {
  if (length(release) != 1L || is.na(release) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", release) || release %in% c(".", "..")) {
    stop("Invalid release identifier.", call. = FALSE)
  }
  mslipidmapper_pathway_knowledge_path("datasets", release)
}

#' Read an organism-specific RDF pathway dataset and its map-making tables
#' @param taxon_id Exact configured NCBI taxonomy identifier.
#' @param release Knowledge release identifier.
#' @return A list containing manifest, classes, reactions, participants and RDF JSON-LD.
#' @export
read_organism_pathway <- function(taxon_id, release = "0.4.0") {
  taxon_id <- as.character(taxon_id)
  catalog <- list_organism_pathway_datasets(release)
  if (length(taxon_id) != 1L || is.na(taxon_id) || !taxon_id %in% catalog$taxon_id) {
    stop("Unknown organism dataset.", call. = FALSE)
  }
  folder <- file.path(.organism_release_path(release), "organisms", taxon_id)
  read_table <- function(name) utils::read.csv(file.path(folder, paste0(name, ".csv")),
    stringsAsFactors = FALSE, colClasses = "character", na.strings = character())
  list(manifest = jsonlite::read_json(file.path(folder, "manifest.json")),
    classes = read_table("classes"), reactions = read_table("reactions"),
    participants = read_table("participants"),
    graph = jsonlite::read_json(file.path(folder, "pathways.jsonld")), path = folder)
}

#' Search a prebuilt organism dataset without an external service
#' @param query Literal class label, URI fragment, reaction label or Rhea ID.
#' @param taxon_id Exact NCBI taxonomy identifier.
#' @param release Knowledge release identifier.
#' @return Matching class/reaction tables and release provenance.
#' @export
search_organism_pathway <- function(query, taxon_id, release = "0.4.0") {
  if (length(query) != 1L || is.na(query) || !nzchar(trimws(query))) {
    stop("query must be one non-empty string.", call. = FALSE)
  }
  dataset <- read_organism_pathway(taxon_id, release)
  query <- tolower(trimws(query))
  contains <- function(x) grepl(query, tolower(x), fixed = TRUE)
  cls <- dataset$classes
  classification <- if ("lipidmaps_class_id" %in% names(cls)) tolower(cls$lipidmaps_class_id) == query else rep(FALSE, nrow(cls))
  classes <- cls[contains(cls$label) | contains(cls$uri) | classification, , drop = FALSE]
  exact <- cls$uri[tolower(cls$label) == query | classification]
  related <- dataset$participants$reaction[dataset$participants$entity %in% exact]
  rxn <- dataset$reactions
  reactions <- rxn[contains(rxn$label) | contains(rxn$rhea) | rxn$uri %in% related | tolower(rxn$uri) == query, , drop = FALSE]
  list(taxon_id = as.character(taxon_id), release = release,
    status = dataset$manifest$status, classes = classes, reactions = reactions)
}

#' Map measurements to a prebuilt organism RDF pathway, without a cyjs file
#' @param x mzTab-M path or lipidomics SummarizedExperiment.
#' @param taxon_id Exact NCBI taxonomy identifier; no species inference is performed.
#' @param output_file Optional JSON-LD output path.
#' @param release Knowledge release identifier.
#' @param ... Measurement settings passed to mztab_pathway_measurements, except network
#'   and node_subclasses. No normalization is performed by this wrapper.
#' @return Measurement result with organism manifest and canonical RDF knowledge attached.
#' @export
organism_pathway_measurements <- function(x, taxon_id, output_file = NULL,
                                          release = "0.4.0", ...) {
  dataset <- read_organism_pathway(taxon_id, release)
  if (!identical(dataset$manifest$status, "available")) {
    stop("No supported reactions in the selected organism dataset.", call. = FALSE)
  }
  dots <- list(...)
  if (any(c("network", "node_subclasses", "lipidmaps_mapping") %in% names(dots))) {
    stop("network and node_subclasses are fixed by the organism dataset.", call. = FALSE)
  }
  cls <- dataset$classes
  # A geometry-free internal adapter reuses the existing measurement aggregation.
  # It creates no reaction edges: canonical reaction nodes come from RDF below.
  adapter <- list(elements = list(nodes = lapply(seq_len(nrow(cls)), function(i)
    list(data = list(id = cls$uri[i], label = cls$label[i]))), edges = list()))
  result <- do.call(mztab_pathway_measurements, c(list(x = x, network = adapter,
    node_subclasses = stats::setNames(cls$label, cls$uri), lipidmaps_mapping = cls), dots))
  vocab <- "https://systemomicslab.github.io/MSLipidMapper/ontology/"
  canonical_dataset <- dataset$manifest$dataset_uri
  if (is.null(canonical_dataset)) canonical_dataset <- paste0(
    "https://systemomicslab.github.io/MSLipidMapper/resource/organism/", taxon_id, "/release/", release)
  graph <- result$document[["@graph"]]
  for (i in seq_along(graph)) {
    node <- graph[[i]]
    if (identical(node[["@type"]], "mslm:Dataset")) {
      node$knowledgeRelease <- release
      node$mappingRulesVersion <- "ontology-first-lipidmaps-fallback/3"
      node$organism <- list("@id" = paste0("http://purl.uniprot.org/taxonomy/", taxon_id))
      node$knowledgeDataset <- list("@id" = canonical_dataset)
    }
    if (identical(node[["@type"]], "mslm:Pathway")) {
      node$label <- paste(dataset$manifest$scientific_name, "class pathway measurement view")
      node$assertionMethod <- list("@id" = paste0(vocab, "KnowledgeProjection"))
      node$knowledgeDataset <- list("@id" = canonical_dataset)
    }
    if (identical(node[["@type"]], "mslm:MSDialMappableLipidEntity")) {
      hit <- match(node$msdialOntologyLabel, cls$label)
      if (!is.na(hit)) node$canonicalEntity <- list("@id" = cls$uri[hit])
    }
    graph[[i]] <- node
  }
  result$document[["@graph"]] <- c(graph, dataset$graph)
  result$knowledge_manifest <- dataset$manifest
  result$node_mapping$canonical_uri <- cls$uri[match(result$node_mapping$node_id, cls$uri)]
  result$node_mapping$mapping_rule <- "ontology-first-lipidmaps-fallback/3"
  if ("lipidmaps_class_id" %in% names(cls)) {
    result$node_mapping$lipidmaps_class_id <- cls$lipidmaps_class_id[match(result$node_mapping$node_id, cls$uri)]
  }
  if (!is.null(output_file)) jsonlite::write_json(result$document, output_file,
    auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null", digits = NA)
  result
}
