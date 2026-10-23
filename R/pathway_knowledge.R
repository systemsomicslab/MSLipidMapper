#' Locate the bundled MSLipidMapper pathway knowledge base
#'
#' RDF/Turtle is the canonical pathway representation. This helper returns the
#' installed path, with a source-tree fallback for development.
#'
#' @param ... Optional path components below the pathway-knowledge root.
#' @return A normalized file or directory path.
#' @export
mslipidmapper_pathway_knowledge_path <- function(...) {
  parts <- as.character(list(...))
  root <- system.file("extdata", "pathway_knowledge", package = "MSLipidMapper")
  candidates <- c(
      file.path(getwd(), "inst", "extdata", "pathway_knowledge"),
      file.path(getwd(), "..", "inst", "extdata", "pathway_knowledge"),
      file.path(getwd(), "..", "..", "inst", "extdata", "pathway_knowledge"), root
  )
  candidates <- candidates[nzchar(candidates)]
  paths <- vapply(candidates, function(candidate) {
    if (length(parts)) do.call(file.path, c(list(candidate), as.list(parts))) else candidate
  }, character(1))
  hits <- paths[file.exists(paths)]
  if (!length(hits)) stop("MSLipidMapper pathway knowledge asset was not found.", call. = FALSE)
  path <- hits[1]
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Read the pathway knowledge source manifest
#'
#' @return A list parsed from `source/source-manifest.yml`.
#' @export
read_pathway_knowledge_manifest <- function() {
  yaml::read_yaml(mslipidmapper_pathway_knowledge_path("source", "source-manifest.yml"))
}

#' List bundled pathway knowledge assets
#'
#' @return A data frame containing relative paths and asset roles.
#' @export
list_pathway_knowledge_assets <- function() {
  root <- mslipidmapper_pathway_knowledge_path()
  files <- list.files(root, pattern = "\\.(ttl|trig|rq|csv)$", recursive = TRUE, full.names = TRUE)
  rel <- substring(normalizePath(files, winslash = "/"), nchar(root) + 2L)
  role <- ifelse(startsWith(rel, "ontology/"), "ontology",
    ifelse(startsWith(rel, "curated/"), "curated",
      ifelse(startsWith(rel, "releases/"), "release",
        ifelse(startsWith(rel, "mappings/"), "mapping",
          ifelse(startsWith(rel, "organisms/"), "organism",
            ifelse(startsWith(rel, "queries/"), "query",
              ifelse(startsWith(rel, "examples/"), "example", "other")))))))
  data.frame(path = rel, role = role, stringsAsFactors = FALSE)
}

#' Map MS-DIAL Ontology labels to pathway lipid entities
#'
#' Matching is deliberately exact after trimming whitespace. Classification
#' codes are returned separately from individual LIPID MAPS structure IDs.
#' Empty `lipidmaps_id` values mean that the mapped entity is a generic lipid
#' subclass and therefore has no single defensible LM_ID.
#'
#' @param ontology Character vector from the MS-DIAL `Ontology` column.
#' @return A data frame preserving input order and reporting mapping status.
#' @export
map_msdial_ontology_to_pathway <- function(ontology) {
  key <- trimws(as.character(ontology))
  mapping <- utils::read.csv(
    mslipidmapper_pathway_knowledge_path("mappings", "msdial-lipidmaps.csv"),
    stringsAsFactors = FALSE, na.strings = character(), colClasses = "character"
  )
  mapping$canonical_uri <- .mslm_registered_uri(mapping$canonical_uri)
  index <- match(key, mapping$msdial_ontology)
  out <- mapping[index, , drop = FALSE]
  rownames(out) <- NULL
  out$input_ontology <- key
  out$matched <- !is.na(index)
  out[, c("input_ontology", "matched", setdiff(names(mapping), "msdial_ontology"),
    "msdial_ontology"), drop = FALSE]
}

# Resolve feature classes once for measurements and statistics. Never override
# a supplied ontology, even when its label is not in the selected pathway.
.mslm_pathway_feature_classes <- function(rd, subclass_col = "subclass", mapping = NULL) {
  missing <- function(x) is.na(x) | tolower(trimws(x)) %in% c("", "unknown", "na", "null", "n/a")
  out <- rep(NA_character_, nrow(rd))
  for (column in unique(c("Ontology", "opt_global_Ontology", "ontology", subclass_col))) {
    if (!column %in% names(rd)) next
    value <- trimws(as.character(rd[[column]]))
    take <- missing(out) & !missing(value)
    out[take] <- value[take]
  }
  if (is.null(mapping)) {
    m <- utils::read.csv(mslipidmapper_pathway_knowledge_path("mappings", "msdial-lipidmaps.csv"),
      colClasses = "character", na.strings = character())
    mapping <- data.frame(label = m$msdial_ontology, lipidmaps_class_id = m$lipidmaps_subclass_code)
  }
  # A classification shared by distinct pathway entities cannot select one.
  codes <- split(mapping$label, mapping$lipidmaps_class_id)
  codes <- lapply(codes, unique)
  codes <- codes[lengths(codes) == 1L & nzchar(names(codes))]
  id_columns <- names(rd)[tolower(gsub("[^a-z0-9]", "", tolower(names(rd)))) %in%
    c("lipidmaps", "lipidmapsid", "lmid", "lipidmapsclassid", "optgloballipidmaps",
      "optgloballipidmapsid", "databaseidentifier")]
  for (i in which(missing(out))) {
    ids <- toupper(trimws(as.character(unlist(rd[i, id_columns, drop = FALSE], use.names = FALSE))))
    # Accept bare classification codes, LM structure accessions and database URLs.
    ids <- unlist(strsplit(ids, "[|;,[:space:]]+"))
    ids <- sub("^.*[/#:](LM[A-Z]{2}[0-9]{8})$", "\\1", ids)
    ids <- sub("^LM([A-Z]{2}[0-9]{4})[0-9]{4}$", "\\1", ids)
    hits <- unique(unlist(codes[intersect(ids, names(codes))], use.names = FALSE))
    if (length(hits) == 1L) out[i] <- hits
  }
  out[missing(out)] <- "Unknown"
  out
}

#' List organisms configured for Rhea-UniProt graph materialization
#'
#' @return A character-only data frame containing NCBI taxonomy identifiers.
#' @export
list_pathway_organisms <- function() {
  utils::read.csv(
    mslipidmapper_pathway_knowledge_path("source", "organisms.csv"),
    stringsAsFactors = FALSE, colClasses = "character"
  )
}

#' Render the federated Rhea-UniProt query for one organism
#'
#' The returned SPARQL keeps Rhea reactions canonical and obtains the
#' organism-specific enzyme link from reviewed UniProtKB catalytic activities.
#'
#' @param taxon_id NCBI taxonomy identifier configured in `organisms.csv`.
#' @return A single SPARQL query string.
#' @export
render_rhea_uniprot_query <- function(taxon_id) {
  taxon_id <- as.character(taxon_id)
  organisms <- list_pathway_organisms()
  if (length(taxon_id) != 1L || !taxon_id %in% organisms$taxon_id) {
    stop("taxon_id must be one configured NCBI taxonomy identifier.", call. = FALSE)
  }
  template <- paste(readLines(
    mslipidmapper_pathway_knowledge_path("queries", "rhea-uniprot-by-taxon.rq"),
    warn = FALSE
  ), collapse = "\n")
  gsub("__TAXON_ID__", taxon_id, template, fixed = TRUE)
}

#' Perform lightweight pathway knowledge integrity checks
#'
#' This check deliberately has no triplestore dependency. It verifies the
#' package data contract and required pilot assertions. Full RDF and SHACL
#' validation should additionally be run by a standards-compliant RDF tool.
#'
#' @param error If `TRUE`, stop when a check fails.
#' @return A data frame with one row per check.
#' @export
validate_pathway_knowledge <- function(error = TRUE) {
  root <- mslipidmapper_pathway_knowledge_path()
  required <- c(
    "ontology/mslm-core.ttl",
    "ontology/mslm-shapes.ttl",
    "source/source-manifest.yml",
    "curated/ceramide-pilot.ttl",
    "curated/phospholipid-pilot.ttl",
    "curated/acyl-selectivity-pilot.ttl",
    "curated/pemt-stages.ttl",
    "mappings/msdial-lipidmaps.csv",
    "organisms/homo-sapiens.trig",
    "queries/rhea-uniprot-by-taxon.rq",
    "queries/observed-subclasses-through-gaps.rq",
    "examples/subclass-statistics-example.ttl",
    "source/organisms.csv",
    "releases/mslm-pathway-knowledge-v0.1.ttl"
  )
  required_ok <- file.exists(file.path(root, required))
  checks <- data.frame(
    check = paste0("file:", required),
    ok = required_ok,
    detail = ifelse(required_ok, "present", "missing"),
    stringsAsFactors = FALSE
  )

  if (all(required_ok)) {
    pilot <- paste(readLines(file.path(root, "curated", "ceramide-pilot.ttl"), warn = FALSE), collapse = "\n")
    tokens <- c(
      "a mslm:LipidReaction",
      "mslm:participantRole mslm:Substrate",
      "mslm:participantRole mslm:Product",
      "mslm:hasSourceAssertion",
      "R-HSA-1638104",
      "RHEA:12088"
    )
    token_ok <- vapply(tokens, grepl, logical(1), x = pilot, fixed = TRUE)
    checks <- rbind(checks, data.frame(
      check = paste0("pilot-token:", tokens),
      ok = token_ok,
      detail = ifelse(token_ok, "present", "missing"),
      stringsAsFactors = FALSE
    ))
    manifest <- read_pathway_knowledge_manifest()
    source_ids <- vapply(manifest$sources, function(x) {
      if (is.null(x$id) || !length(x$id)) "" else as.character(x$id[[1]])
    }, character(1))
    for (id in c("reactome", "rhea", "msdial", "lipidmaps")) {
      ok <- id %in% source_ids
      checks <- rbind(checks, data.frame(
        check = paste0("manifest-source:", id), ok = ok,
        detail = if (ok) "present" else "missing", stringsAsFactors = FALSE
      ))
    }

    phospholipid <- paste(readLines(
      file.path(root, "curated", "phospholipid-pilot.ttl"), warn = FALSE
    ), collapse = "\n")
    phospholipid_tokens <- c(
      "res:reaction_pc_to_ps",
      "res:reaction_pe_to_ps",
      "res:reaction_ps_to_pe",
      "res:reaction_pe_to_pc",
      "R-HSA-1483186",
      "RHEA:45088",
      "mslm:msdialOntologyLabel \"PC\"",
      "mslm:lipidMapsSubClassCode \"GP0101\""
    )
    phospholipid_ok <- vapply(
      phospholipid_tokens, grepl, logical(1), x = phospholipid, fixed = TRUE
    )
    checks <- rbind(checks, data.frame(
      check = paste0("phospholipid-token:", phospholipid_tokens),
      ok = phospholipid_ok,
      detail = ifelse(phospholipid_ok, "present", "missing"),
      stringsAsFactors = FALSE
    ))
  }

  if (isTRUE(error) && any(!checks$ok)) {
    stop("Pathway knowledge validation failed: ",
      paste(checks$check[!checks$ok], collapse = ", "), call. = FALSE)
  }
  checks
}
