# Persistent accession ledger. Reference IDs use the committed JSON registry;
# experimental IDs use a writable SQLite registry with transactional allocation.
.mslm_id_session <- new.env(parent = emptyenv())
.mslm_id_session$con <- NULL
.mslm_id_path <- function() getOption("MSLipidMapper.id_registry", file.path(
  tools::R_user_dir("MSLipidMapper", which = "data"), "identifiers.sqlite"))
.mslm_open_id_connection <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA busy_timeout=30000")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS identifiers (key TEXT PRIMARY KEY, accession TEXT UNIQUE NOT NULL)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS counters (prefix TEXT PRIMARY KEY, high_water INTEGER NOT NULL)")
  con
}
.mslm_begin_id_session <- function() {
  if (!is.null(.mslm_id_session$con)) return(FALSE)
  .mslm_id_session$con <- .mslm_open_id_connection(.mslm_id_path())
  ready <- FALSE
  on.exit(if (!ready) {
    try(DBI::dbRollback(.mslm_id_session$con), silent = TRUE)
    DBI::dbDisconnect(.mslm_id_session$con)
    .mslm_id_session$con <- NULL
  }, add = TRUE)
  DBI::dbExecute(.mslm_id_session$con, "BEGIN IMMEDIATE")
  .mslm_id_session$seed <- jsonlite::read_json(mslipidmapper_pathway_knowledge_path("source", "id-registry.json"))
  ready <- TRUE
  TRUE
}
.mslm_end_id_session <- function(owner, commit = FALSE) {
  if (!owner || is.null(.mslm_id_session$con)) return(invisible(NULL))
  con <- .mslm_id_session$con
  .mslm_id_session$con <- NULL
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (commit) DBI::dbCommit(con) else DBI::dbRollback(con)
  invisible(NULL)
}
.mslm_resource_uri <- function(kind, ...) {
  keys <- vapply(list(...), function(x) {
    if (length(x) != 1L || is.na(x)) stop("Identifier keys must be scalar and non-missing.")
    as.character(x)
  }, character(1))
  values <- enc2utf8(c("mslm-accession-v1", kind, keys))
  key <- paste0(nchar(values, type = "bytes"), ":", values, collapse = "")
  owns <- is.null(.mslm_id_session$con)
  con <- if (owns) .mslm_open_id_connection(.mslm_id_path()) else .mslm_id_session$con
  if (owns) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "BEGIN IMMEDIATE")
  }
  completed <- FALSE
  on.exit(if (owns && !completed) DBI::dbRollback(con), add = TRUE, after = FALSE)
  hit <- DBI::dbGetQuery(con, "SELECT accession FROM identifiers WHERE key = ?", params = list(key))
  if (nrow(hit)) {
    accession <- hit$accession[1]
  } else {
    prefixes <- c("measurement-dataset" = "D", "measurement-pathway" = "X",
      "measurement-sample" = "S", "measurement-subclass" = "U", "measurement-feature" = "F",
      "measurement-pathway-node" = "X", "measurement-connection" = "X", "measurement-scope" = "O",
      "sample-measurement" = "M", "statistics-dataset" = "D", "statistics-comparison" = "I",
      "statistics-observation" = "O", "unmapped-class" = "U")
    prefix <- unname(prefixes[kind])
    if (is.na(prefix)) prefix <- "Z"
    seed <- if (owns) jsonlite::read_json(mslipidmapper_pathway_knowledge_path("source", "id-registry.json")) else .mslm_id_session$seed
    baseline <- seed$counters[[prefix]]
    if (is.null(baseline)) baseline <- 0L
    old <- DBI::dbGetQuery(con, "SELECT high_water FROM counters WHERE prefix = ?", params = list(prefix))
    number <- max(baseline, if (nrow(old)) old$high_water[1] else 0L) + 1L
    accession <- sprintf("MSLM%s%06d", prefix, number)
    DBI::dbExecute(con, "INSERT OR REPLACE INTO counters VALUES (?, ?)", params = list(prefix, number))
    DBI::dbExecute(con, "INSERT INTO identifiers VALUES (?, ?)", params = list(key, accession))
  }
  if (owns) DBI::dbCommit(con)
  completed <- TRUE
  paste0("https://systemomicslab.github.io/MSLipidMapper/resource/", accession)
}

.mslm_registered_uri <- function(uri) {
  registry <- jsonlite::read_json(mslipidmapper_pathway_knowledge_path("source", "id-registry.json"))
  lookup <- c(registry$entries, registry$aliases)
  vapply(uri, function(value) {
    if (is.na(value) || !nzchar(value)) return(value)
    if (!is.null(lookup[[value]])) lookup[[value]] else value
  }, character(1), USE.NAMES = FALSE)
}

.mslm_classification_info <- function(label) {
  config <- jsonlite::read_json(mslipidmapper_pathway_knowledge_path("source", "class-pathways.json"))
  keys <- names(config$class_labels)[vapply(config$class_labels, identical, logical(1), label)]
  mapping <- jsonlite::read_json(mslipidmapper_pathway_knowledge_path("source", "lipidmaps-class-mappings.json"))
  hits <- Filter(function(item) item$source_key %in% keys, mapping$classes)
  if (length(hits)) hits[[1]] else NULL
}
