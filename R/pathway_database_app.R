# Standalone, read-only browser. Published releases and drafts remain separate.
.pathway_browser_catalog <- function() {
  root <- mslipidmapper_pathway_knowledge_path()
  rows <- list()
  for (folder in list.dirs(file.path(root, "datasets"), recursive = FALSE)) {
    if (!file.exists(file.path(folder, "catalog.json"))) next
    release <- basename(folder)
    organisms <- list_organism_pathway_datasets(release)
    for (i in seq_len(nrow(organisms))) rows[[length(rows) + 1L]] <- data.frame(
      key = paste("release", release, organisms$taxon_id[i], sep = ":"),
      label = paste(organisms$scientific_name[i], release, "[release]"),
      kind = "release", release = release, taxon = organisms$taxon_id[i],
      folder = file.path(folder, "organisms", organisms$taxon_id[i]))
  }
  for (folder in list.dirs(file.path(root, "drafts"), recursive = FALSE)) {
    if (!all(file.exists(file.path(folder, c("report.json", "candidates.json", "class-mappings.json"))))) next
    report <- jsonlite::read_json(file.path(folder, "report.json"))
    rows[[length(rows) + 1L]] <- data.frame(key = paste0("draft:", basename(folder)),
      label = paste(basename(folder), paste0("(", report$scientific_name, "; ", report$taxon_id, ")"), if (identical(report$status, "integrated-source-catalog")) "[integrated / source evidence]" else if (identical(report$status, "source-catalog-with-explicit-class-mapping-status")) "[source catalog]" else "[draft / unreviewed]"), kind = "draft",
      release = "", taxon = as.character(report$taxon_id), folder = folder)
  }
  if (!length(rows)) stop("No bundled pathway datasets were found.", call. = FALSE)
  do.call(rbind, rows)
}

.pathway_browser_read <- function(entry) {
  if (entry$kind == "release") {
    x <- read_organism_pathway(entry$taxon, entry$release)
    return(list(reactions = x$reactions, classes = x$classes,
      details = lapply(x$reactions$uri, function(id) list(
        reaction = x$reactions[x$reactions$uri == id, , drop = FALSE],
        participants = x$participants[x$participants$reaction == id, , drop = FALSE])),
      metadata = x$manifest, rdf = file.path(x$path, "pathways.nt"), source_rdf = file.path(x$path, "pathways.nt")))
  }
  x <- jsonlite::read_json(file.path(entry$folder, "candidates.json"))
  report <- jsonlite::read_json(file.path(entry$folder, "report.json"))
  snapshot <- file.path(dirname(dirname(entry$folder)), "snapshots", basename(entry$folder))
  if (file.exists(file.path(snapshot, "annotations.json")) && file.exists(file.path(snapshot, "participants.json"))) {
    annotations <- jsonlite::read_json(file.path(snapshot, "annotations.json"))
    participants <- jsonlite::read_json(file.path(snapshot, "participants.json"))
    by_reaction <- split(vapply(participants, function(p) sub(".*/", "", p$annotated), character(1)),
      vapply(participants, function(p) p$reaction, character(1)))
    x <- lapply(x, function(r) {
      ids <- unique(by_reaction[[r$reaction]])
      r$protein_evidence <- Filter(function(a) sub("^RHEA:", "", a$rhea) %in% ids, annotations)
      r$source <- list(database = "Rhea", identifier = r$reaction, release = report$rhea_release)
      r
    })
  }
  mappings <- jsonlite::read_json(file.path(entry$folder, "class-mappings.json"))
  classes <- unique(do.call(rbind, lapply(names(mappings), function(id) data.frame(
    uri = paste0("http://purl.obolibrary.org/obo/CHEBI_", id),
    label = mappings[[id]][[1]], lipidmaps_class_id = mappings[[id]][[2]]))))
  if (file.exists(file.path(entry$folder, "classes.csv"))) classes <- utils::read.csv(
    file.path(entry$folder, "classes.csv"), stringsAsFactors = FALSE, colClasses = "character")
  aliases <- c(GP0101 = "PC", GP0105 = "LPC", GP0201 = "PE", GP0205 = "LPE", GP0301 = "PS", GP0305 = "LPS",
    GP0401 = "PG", GP0405 = "LPG", GP0601 = "PI", GP0605 = "LPI", GP1001 = "PA", GP1005 = "LPA",
    GP1201 = "CL", GP1301 = "CDP-DG", GL0101 = "MG", GL0201 = "DG", GL0301 = "TG",
    SP0201 = "Cer", SP0202 = "DHCer", SP0205 = "CerP", SP0301 = "SM", SP0105 = "S1P DHS1P",
    SP0501 = "GlcCer HexCer", SP0509 = "GalCer HexCer", ST0101 = "Cholesterol", ST0102 = "CE",
    GP0102 = "PC-O", GP0103 = "PC-P", GP0202 = "PE-O", GP0203 = "PE-P")
  classes$aliases <- unname(aliases[classes$lipidmaps_class_id])
  classes$aliases[is.na(classes$aliases)] <- ""
  reactions <- if (length(x)) do.call(rbind, lapply(x, function(r) data.frame(
    uri = r$reaction,
    source_database = if (!is.null(r$source$database)) r$source$database else "Rhea",
    source_id = if (!is.null(r$source$identifier)) r$source$identifier else r$reaction,
    inferred = if (!is.null(r$is_inferred)) as.character(r$is_inferred) else if (identical(r$source$database, "Reactome")) "unknown" else "not applicable",
    pathways = paste(unlist(c(r$pathway_ids, r$pathway_names)), collapse = "; "),
    match_kinds = paste(unlist(r$match_kinds), collapse = "; "),
    related_source_ids = paste(unlist(r$related_source_ids), collapse = "; "),
    label = if (!is.null(r$name)) r$name else paste(paste(unlist(r$left), collapse = " + "), "|", paste(unlist(r$right), collapse = " + ")),
    equation = if (!is.null(r$equation)) r$equation else "",
    status = if (!is.null(r$class_mapping_status)) r$class_mapping_status else "unreviewed",
    direction = if (!is.null(r$direction)) r$direction else "unknown",
    kind = if (!is.null(r$kind)) r$kind else "class-conversion"))) else data.frame(uri = character(), rhea = character(), label = character())
  list(reactions = reactions, classes = classes, details = x,
    mapping_features = if (file.exists(file.path(entry$folder, "mapping-reactions.csv")))
      utils::read.csv(file.path(entry$folder, "mapping-reactions.csv"), stringsAsFactors = FALSE) else data.frame(),
    groups = if (file.exists(file.path(entry$folder, "pathway-groups.csv")))
      utils::read.csv(file.path(entry$folder, "pathway-groups.csv"), stringsAsFactors = FALSE) else data.frame(),
    matches = if (file.exists(file.path(entry$folder, "cross-source-matches.csv")))
      utils::read.csv(file.path(entry$folder, "cross-source-matches.csv"), stringsAsFactors = FALSE) else data.frame(),
    pathways = if (file.exists(file.path(entry$folder, "pathways.json")))
      jsonlite::fromJSON(file.path(entry$folder, "pathways.json")) else data.frame(),
    metadata = report,
    rdf = file.path(entry$folder, if (file.exists(file.path(entry$folder, "class-pathways.ttl"))) "class-pathways.ttl" else "candidates.ttl"),
    source_rdf = file.path(entry$folder, "candidates.ttl"))
}

.pathway_mapping_turtle <- function(x, ids) {
  iri <- function(value) {
    if (length(value) != 1L || grepl('[<>"[:space:]]', value)) stop("Invalid RDF IRI")
    paste0("<", value, ">")
  }
  literal <- function(value) as.character(jsonlite::toJSON(as.character(value), auto_unbox = TRUE))
  lines <- c('@prefix mslm: <https://systemomicslab.github.io/MSLipidMapper/ontology/> .',
    '@prefix dct: <http://purl.org/dc/terms/> .', '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .',
    '# Selected lipid class projections; source reaction IDs and taxon are retained.')
  classes <- character()
  for (r in Filter(function(r) r$reaction %in% ids, x$details)) {
    projection <- if (!is.null(r$projection_uri)) r$projection_uri else r$projection
    if (is.null(projection)) next
    subject <- iri(projection)
    lines <- c(lines, paste(subject, 'a mslm:ReactionTraversalProjection ; dct:source', iri(r$reaction), ';',
      'mslm:organism', iri(paste0('http://purl.uniprot.org/taxonomy/', x$metadata$taxon_id)), ';',
      'mslm:sourceDatabase', literal(r$source$database), '; mslm:sourceRelease', literal(r$source$release), ';',
      'mslm:resolutionLevel mslm:ClassLevel .'))
    direction <- if (identical(r$source$database, "Reactome")) {
      if (identical(r$direction, "reversible")) "mslm:Reversible" else "mslm:SourceForward"
    } else "mslm:UnknownDirection"
    lines <- c(lines, paste(subject, 'mslm:directionality', direction, '; mslm:curationStatus mslm:Draft ; mslm:classMappingComplete',
      tolower(as.character(identical(r$class_mapping_status, "complete"))), '.'))
    if (!is.null(r$is_inferred)) lines <- c(lines, paste(subject, 'mslm:isInferred', tolower(as.character(r$is_inferred)), '.'))
    for (cls in r$classes) {
      predicate <- if (cls$side_code == "L") "mslm:leftClass" else "mslm:rightClass"
      lines <- c(lines, paste(subject, predicate, iri(cls$canonical_class), '.'))
      classes <- c(classes, cls$canonical_class)
    }
  }
  for (uri in unique(classes)) {
    cls <- x$classes[x$classes$uri == uri, , drop = FALSE]
    if (nrow(cls)) lines <- c(lines, paste(iri(uri), 'a mslm:LipidEntity ; rdfs:label', literal(cls$label[1]),
      '; mslm:lipidmapsClassId', literal(cls$lipidmaps_class_id[1]), '.'))
  }
  unique(lines)
}

.pathway_mapping_filter <- function(x, ids, pathways = character(), candidates = "none", profile = "all",
                                    measured = character(), measured_mode = "adjacent", bridge_depth = 2L,
                                    include_inferred = TRUE) {
  features <- x$mapping_features
  if (profile == "mapping") ids <- intersect(ids, features$reaction[tolower(as.character(features$mapping_eligible)) %in% "true"])
  if (length(pathways)) {
    levels <- c("direct", "ancestor", "verified-derived")
    if (candidates %in% c("equation", "all")) levels <- c(levels, "equation-candidate")
    if (candidates == "all") levels <- c(levels, "class-candidate", "reference-candidate")
    groups <- x$groups
    ids <- intersect(ids, groups$reaction[groups$pathway %in% pathways & groups$evidence %in% levels])
  }
  if (!include_inferred) ids <- setdiff(ids, features$reaction[features$inferred %in% "true"])
  if (!length(measured)) return(ids)
  rows <- Filter(function(r) r$reaction %in% ids, x$details)
  sides <- function(r, side) unique(vapply(Filter(function(c) identical(c$side_code, side), r$classes), function(c) c$canonical_class, character(1)))
  if (measured_mode != "bridge") return(vapply(Filter(function(r) {
    left <- sides(r, "L"); right <- sides(r, "R")
    if (measured_mode == "both") any(left %in% measured) && any(right %in% measured) else any(c(left, right) %in% measured)
  }, rows), function(r) r$reaction, character(1)))
  # Undirected class connectivity is a display aid, not physiological direction.
  edges <- lapply(rows, function(r) {
    left <- sides(r, "L"); right <- sides(r, "R")
    if (!length(left) || !length(right)) return(data.frame(a = character(), b = character()))
    e <- expand.grid(a = left, b = right, stringsAsFactors = FALSE)
    e[e$a != e$b, , drop = FALSE]
  })
  all_edges <- do.call(rbind, edges)
  if (is.null(all_edges) || !nrow(all_edges) || length(measured) < 2L) return(character())
  nodes <- unique(c(all_edges$a, all_edges$b))
  measured <- intersect(measured, nodes)
  if (length(measured) < 2L) return(character())
  adjacency <- split(c(all_edges$b, all_edges$a), c(all_edges$a, all_edges$b))
  edge_key <- function(a, b) paste(sort(c(a, b)), collapse = "|")
  bridge_edges <- new.env(hash = TRUE, parent = emptyenv())
  walk <- function(node, path) {
    if (length(path) > 1L && node %in% measured) {
      for (i in seq_len(length(path) - 1L)) assign(edge_key(path[i], path[i + 1L]), TRUE, envir = bridge_edges)
    }
    if (length(path) - 1L >= bridge_depth) return(invisible(NULL))
    for (other in setdiff(unique(adjacency[[node]]), path)) walk(other, c(path, other))
    invisible(NULL)
  }
  for (seed in measured) walk(seed, seed)
  keep <- vapply(edges, function(e) {
    if (!nrow(e)) return(FALSE)
    any(vapply(seq_len(nrow(e)), function(i) exists(edge_key(e$a[i], e$b[i]), envir = bridge_edges, inherits = FALSE), logical(1)))
  }, logical(1))
  vapply(rows[keep], function(r) r$reaction, character(1))
}

.pathway_browser_filter <- function(data, query) {
  query <- tolower(trimws(query))
  if (grepl("^rhea:[0-9]+$", query)) query <- sub("^rhea:", "", query)
  if (!nzchar(query) || !nrow(data)) return(data)
  keep <- Reduce(`|`, lapply(data, function(column) grepl(query, tolower(as.character(column)), fixed = TRUE)))
  data[keep, , drop = FALSE]
}

#' Browse the bundled pathway database in a standalone Shiny application
#'
#' Searches local releases and provisional organism reaction candidates without
#' uploading measurements or calling external services. Drafts are explicitly
#' marked and are not promoted to published datasets.
#' @param launch.browser Open the system browser when running the application.
#' @param host Listening address; defaults to localhost.
#' @param port Optional Shiny listening port.
#' @return `pathway_database_app()` returns a Shiny application object.
#'   `run_pathway_database_app()` runs the application.
#' @export
pathway_database_app <- function() {
  catalog <- .pathway_browser_catalog()
  preferred <- tail(catalog$key[catalog$kind == "draft"], 1)
  integrated <- catalog$key[grepl("\\[integrated / source evidence\\]", catalog$label)]
  if (length(integrated)) preferred <- tail(integrated, 1)
  if (!length(preferred)) preferred <- tail(catalog$key, 1)
  ui <- shiny::fluidPage(
    shiny::titlePanel("MSLipidMapper | Pathway database"),
    shiny::sidebarLayout(shiny::sidebarPanel(
      shiny::selectInput("dataset", "Organism / dataset", choices = stats::setNames(catalog$key, catalog$label), selected = preferred),
      shiny::selectInput("source", "Source", choices = c("All", "Rhea", "Reactome")),
      shiny::selectizeInput("pathway", "Pathways (includes descendants)", choices = NULL, multiple = TRUE),
      shiny::selectInput("membership", "Rhea pathway candidates", choices = c("Confirmed membership only" = "none", "Include equation matches" = "equation", "Include class / reference candidates" = "all")),
      shiny::selectInput("profile", "Reaction view", choices = c("Lipid class mapping" = "mapping", "All source events" = "all"), selected = "mapping"),
      shiny::checkboxInput("include_inferred", "Include source-inferred events", value = TRUE),
      shiny::selectizeInput("measured", "Measured lipid classes (optional)", choices = NULL, multiple = TRUE),
      shiny::selectInput("measured_mode", "Measured class connections", choices = c("At least one measured side" = "adjacent", "Both sides measured" = "both", "Connect through intermediate classes" = "bridge")),
      shiny::sliderInput("bridge_depth", "Maximum reactions through intermediates", min = 1, max = 4, value = 2, step = 1),
      shiny::textInput("query", "Search", placeholder = "PC, GP0101, Reactome / RHEA ID, pathway..."),
      shiny::helpText("Literal, case-insensitive search. Class matches also show related reactions."),
      shiny::uiOutput("status"),
      shiny::downloadButton("csv", "Download search results (CSV)"), shiny::br(), shiny::br(),
      shiny::downloadButton("rdf", "Download class pathway RDF"),
      shiny::br(), shiny::br(), shiny::downloadButton("selected_rdf", "Download selected class RDF"),
      shiny::helpText("Mapping view requires complete, distinct classes on both sides and excludes known transport, binding, omitted and unknown events. Intermediate links show connectivity only. Original dataset downloads retain all records."),
      shiny::uiOutput("source_download")
    ), shiny::mainPanel(shiny::tabsetPanel(
      shiny::tabPanel("Reactions", DT::DTOutput("reactions"), shiny::h4("Selected reaction / participants / evidence"), shiny::verbatimTextOutput("detail")),
      shiny::tabPanel("Lipid classes", DT::DTOutput("classes")),
      shiny::tabPanel("Pathways", DT::DTOutput("pathways")),
      shiny::tabPanel("Cross-source matches", shiny::p("Class signatures and complete equation matches are related records. Verified equivalence additionally requires a direct source reference. Source reactions remain separate."), DT::DTOutput("matches")),
      shiny::tabPanel("Dataset / sources", shiny::verbatimTextOutput("metadata"))
    )))
  )
  server <- function(input, output, session) {
    dataset <- shiny::reactive({
      shiny::req(input$dataset)
      entry <- catalog[catalog$key == input$dataset, , drop = FALSE]
      shiny::req(nrow(entry) == 1L)
      .pathway_browser_read(entry)
    })
    query <- shiny::reactive(if (is.null(input$query)) "" else input$query)
    shiny::observeEvent(input$dataset, {
      x <- dataset(); paths <- x$pathways
      choices <- if (!is.null(paths) && nrow(paths)) stats::setNames(paths$stId, paste(paths$name, paths$stId)) else character()
      shiny::updateSelectizeInput(session, "pathway", choices = choices, selected = character(), server = TRUE)
      shiny::updateSelectizeInput(session, "measured", choices = stats::setNames(x$classes$uri, paste(x$classes$label, x$classes$lipidmaps_class_id)), selected = character(), server = TRUE)
      shiny::updateSelectInput(session, "profile", selected = if (!is.null(x$mapping_features) && nrow(x$mapping_features) > 0L) "mapping" else "all")
    })
    classes <- shiny::reactive(.pathway_browser_filter(dataset()$classes, query()))
    reactions <- shiny::reactive({
      x <- dataset()
      direct <- .pathway_browser_filter(x$reactions, query())$uri
      related <- character()
      if (nzchar(trimws(query())) && nrow(classes())) {
        ids <- classes()$uri
        related <- x$reactions$uri[vapply(x$details, function(detail) {
          if (is.data.frame(detail$participants)) return(any(detail$participants$entity %in% ids))
          any(vapply(detail$classes, function(cls) {
            term <- if (!is.null(cls$canonical_class)) cls$canonical_class else cls$chebi
            term %in% ids
          }, logical(1)))
        }, logical(1))]
      }
      selected <- x$reactions[x$reactions$uri %in% c(direct, related), , drop = FALSE]
      if (!is.null(input$source) && input$source != "All") {
        if (!"source_database" %in% names(selected)) return(selected[FALSE, , drop = FALSE])
        selected <- selected[selected$source_database == input$source, , drop = FALSE]
      }
      ids <- .pathway_mapping_filter(x, selected$uri,
        pathways = if (is.null(input$pathway)) character() else input$pathway,
        candidates = if (is.null(input$membership)) "none" else input$membership,
        profile = if (is.null(input$profile)) "all" else input$profile,
        measured = if (is.null(input$measured)) character() else input$measured,
        measured_mode = if (is.null(input$measured_mode)) "adjacent" else input$measured_mode,
        bridge_depth = if (is.null(input$bridge_depth)) 2L else as.integer(input$bridge_depth),
        include_inferred = !identical(input$include_inferred, FALSE))
      selected <- selected[selected$uri %in% ids, , drop = FALSE]
      groups <- x$groups
      if (!is.null(groups) && nrow(groups)) {
        if (length(input$pathway)) groups <- groups[groups$pathway %in% input$pathway, , drop = FALSE]
        selected$pathway_membership_evidence <- vapply(selected$uri, function(id)
          paste(sort(unique(groups$evidence[groups$reaction == id])), collapse = "; "), character(1))
      }
      selected
    })
    visible_classes <- shiny::reactive({
      x <- dataset(); selected <- reactions()$uri
      details <- Filter(function(r) {
        id <- if (is.character(r$reaction)) r$reaction else r$reaction$uri
        id %in% selected
      }, x$details)
      ids <- unique(unlist(lapply(details, function(r) {
        if (is.data.frame(r$participants)) return(r$participants$entity)
        unlist(lapply(r$classes, function(cls) if (!is.null(cls$canonical_class)) cls$canonical_class else cls$chebi), use.names = FALSE)
      }), use.names = FALSE))
      data <- classes()
      data[data$uri %in% ids, , drop = FALSE]
    })
    shiny::observeEvent(list(input$dataset, input$query, input$source, input$pathway, input$membership, input$profile, input$measured, input$measured_mode, input$bridge_depth, input$include_inferred), {
      DT::selectRows(DT::dataTableProxy("reactions", session = session), NULL)
    }, ignoreInit = TRUE)
    output$reactions <- DT::renderDT(DT::datatable(reactions(), rownames = FALSE,
      escape = TRUE, selection = "single", options = list(pageLength = 10, scrollX = TRUE)), server = TRUE)
    output$classes <- DT::renderDT(DT::datatable(visible_classes(), rownames = FALSE,
      escape = TRUE, options = list(pageLength = 10, scrollX = TRUE)), server = FALSE)
    output$pathways <- DT::renderDT({
      data <- dataset()$pathways
      if (is.null(data)) data <- data.frame()
      groups <- dataset()$groups
      if (nrow(data) && !is.null(groups) && nrow(groups)) {
        data$confirmed_source_events <- vapply(data$stId, function(id) length(unique(groups$reaction[
          groups$pathway == id & groups$evidence %in% c("direct", "ancestor", "verified-derived")])), integer(1))
        data$candidate_reactions <- vapply(data$stId, function(id) length(unique(groups$reaction[
          groups$pathway == id & grepl("candidate$", groups$evidence)])), integer(1))
      }
      DT::datatable(.pathway_browser_filter(data, query()), rownames = FALSE,
        escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))
    }, server = FALSE)
    output$matches <- DT::renderDT({
      data <- dataset()$matches
      if (is.null(data)) data <- data.frame()
      if (nrow(data)) data <- data[data$rhea %in% reactions()$uri | data$reactome %in% reactions()$uri, , drop = FALSE]
      DT::datatable(data, rownames = FALSE, escape = TRUE,
        options = list(pageLength = 10, scrollX = TRUE))
    }, server = TRUE)
    output$status <- shiny::renderUI({
      draft <- startsWith(input$dataset, "draft:")
      source_catalog <- identical(dataset()$metadata$status, "source-catalog-with-explicit-class-mapping-status")
      shiny::tagList(shiny::p(if (identical(dataset()$metadata$status, "integrated-source-catalog"))
        "Integrated Rhea / Reactome catalog. Source reactions and organism evidence are preserved separately. Matching lipid class signatures do not establish reaction identity."
        else if (identical(dataset()$metadata$source_database, "Reactome"))
        paste0("Reactome ", dataset()$metadata$scientific_name, " lipid pathway events, including binding and regulatory events. Inferred flags are source evidence; SBML direction does not establish physiological flux. Sets and complexes are retained without expanding components into free reactants.")
        else if (source_catalog) "Source catalog - all source-supported organism lipid reactions are retained, including reactions with incomplete class mapping. Equation sides do not establish biological direction." else if (draft) "DRAFT - unreviewed class projections. Left/right does not establish biological direction." else "Published dataset - coverage and provenance are shown in Dataset / sources."),
        shiny::p(sprintf("%d reactions / %d classification codes / %d class records matched", nrow(reactions()),
          length(unique(visible_classes()$lipidmaps_class_id)), nrow(visible_classes()))))
    })
    output$detail <- shiny::renderText({
      selected <- input$reactions_rows_selected
      if (length(selected) != 1L || selected > nrow(reactions())) return("Select a reaction row to inspect its participants and source information.")
      id <- reactions()$uri[selected]
      detail <- dataset()$details[[match(id, dataset()$reactions$uri)]]
      as.character(jsonlite::toJSON(detail, pretty = TRUE, auto_unbox = TRUE, na = "null"))
    })
    output$metadata <- shiny::renderText(as.character(jsonlite::toJSON(dataset()$metadata, pretty = TRUE, auto_unbox = TRUE)))
    output$csv <- shiny::downloadHandler(filename = function() "pathway-search.csv",
      content = function(file) utils::write.csv(reactions(), file, row.names = FALSE, fileEncoding = "UTF-8"))
    output$selected_rdf <- shiny::downloadHandler(filename = function() "selected-class-pathways.ttl",
      content = function(file) {
        x <- dataset(); selected <- Filter(function(r) r$reaction %in% reactions()$uri, x$details)
        writeLines(.pathway_mapping_turtle(x, reactions()$uri), file, useBytes = TRUE)
      })
    output$rdf <- shiny::downloadHandler(filename = function() basename(dataset()$rdf),
      content = function(file) file.copy(dataset()$rdf, file, overwrite = TRUE))
    output$source_download <- shiny::renderUI({
      if (!identical(dataset()$rdf, dataset()$source_rdf)) shiny::tagList(shiny::br(), shiny::br(),
        shiny::downloadButton("source_rdf", "Download source reaction RDF"))
    })
    output$source_rdf <- shiny::downloadHandler(filename = function() basename(dataset()$source_rdf),
      content = function(file) file.copy(dataset()$source_rdf, file, overwrite = TRUE))
  }
  shiny::shinyApp(ui, server)
}

#' @rdname pathway_database_app
#' @export
run_pathway_database_app <- function(launch.browser = interactive(), host = "127.0.0.1", port = NULL) {
  shiny::runApp(pathway_database_app(), launch.browser = launch.browser, host = host, port = port)
}
