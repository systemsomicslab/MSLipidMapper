# R/mod_utility_ontology_builder.R -----------------------------------------
# Utility: Ontology builder for Generic lipidomics input
# - Input: pasted lipid names OR CSV/TSV with a lipid-name column
# - Parse: rgoslin (version-robust wrapper)
# - Output: feature table (lipid name -> Ontology) + failed list
#
# Exports:
#   mod_utility_ontology_builder_ui(id, title="Ontology builder")
#   mod_utility_ontology_builder_server(id)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(htmltools)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- rgoslin wrapper (robust across versions) -----------------------------
.rgoslin_parse <- function(lipids) {
  if (!requireNamespace("rgoslin", quietly = TRUE)) {
    stop("Package 'rgoslin' is not installed. Please install it first.")
  }
  # Try common function names across versions
  cand <- c("parse_lipid_names", "parseLipidNames", "parse_lipidnames", "parseLipidnames")
  fn <- NULL
  for (nm in cand) {
    if (exists(nm, where = asNamespace("rgoslin"), mode = "function")) {
      fn <- get(nm, envir = asNamespace("rgoslin"))
      break
    }
  }
  if (is.null(fn)) {
    stop("Could not find a parser function in 'rgoslin'. Please check your rgoslin version.")
  }
  
  res <- fn(lipids)
  
  # Ensure data.frame/tibble
  if (is.list(res) && !is.data.frame(res)) {
    # some versions might return list with $results
    if (!is.null(res$results) && is.data.frame(res$results)) res <- res$results
  }
  if (!is.data.frame(res)) {
    stop("rgoslin returned an unexpected object (not a data.frame).")
  }
  res
}

.pick1 <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

.clean_lipid_vec <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)
}

# ---- UI ------------------------------------------------------------------
#' @export
mod_utility_ontology_builder_ui <- function(id, title = "Ontology builder") {
  ns <- NS(id)
  
  tagList(
    h3(title),
    
    fluidRow(
      column(
        width = 4,
        box(
          width = 12, status = "primary", solidHeader = TRUE, title = "Input",
          radioButtons(
            ns("input_mode"),
            "Input mode",
            choices = c("Paste lipid names" = "paste", "Upload CSV/TSV" = "file"),
            selected = "paste",
            inline = TRUE
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'paste'", ns("input_mode")),
            textAreaInput(
              ns("paste_txt"),
              "Lipid names (one per line)",
              value = paste(
                c("PC 34:1", "PC(16:0/18:1)", "PE 36:2", "TG(16:0/18:1/18:2)",
                  "Cer(d18:1/16:0)", "PC O-34:1", "PE P-16:0/18:1"),
                collapse = "\n"
              ),
              rows = 10
            )
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'file'", ns("input_mode")),
            fileInput(ns("infile"), "CSV/TSV file", accept = c(".csv", ".tsv", ".txt")),
            radioButtons(ns("sep"), "Separator", choices = c("CSV (,)" = ",", "TSV (tab)" = "\t"), inline = TRUE),
            uiOutput(ns("name_col_ui"))
          ),
          
          hr(),
          selectInput(
            ns("ontology_source"),
            "Ontology source (preferred column)",
            choices = c(
              "LIPIDMAPS Main Class"      = "main",
              "LIPIDMAPS Category"  = "cat"
            ),
            selected = "abbr"
          ),
          
          actionButton(ns("run"), "Parse & build ontology", class = "btn btn-primary"),
          br(), br(),
          downloadButton(ns("download_feature"), "Download feature CSV (name + Ontology)"),
          downloadButton(ns("download_failed"),  "Download failed list")
        )
      ),
      
      column(
        width = 8,
        box(
          width = 12, status = "primary", solidHeader = TRUE, title = "Status",
          uiOutput(ns("status_ui"))
        ),
        box(
          width = 12, status = "primary", solidHeader = TRUE, title = "Preview (feature table)",
          tableOutput(ns("preview_tbl"))
        ),
        box(
          width = 12, status = "primary", solidHeader = TRUE, title = "Failed inputs (if any)",
          tableOutput(ns("failed_tbl"))
        )
      )
    )
  )
}

# ---- Server --------------------------------------------------------------
#' @export
mod_utility_ontology_builder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # name column chooser for file mode
    output$name_col_ui <- renderUI({
      if (!identical(input$input_mode, "file")) return(NULL)
      req(input$infile)
      
      sep <- if (identical(input$sep, "\t")) "\t" else ","
      df <- try(
        utils::read.delim(
          input$infile$datapath,
          sep = sep,
          header = TRUE,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        silent = TRUE
      )
      if (inherits(df, "try-error") || is.null(df) || !ncol(df)) {
        return(helpText("Failed to read file header. Please check delimiter / format."))
      }
      selectInput(ns("name_col"), "Column containing lipid names", choices = names(df), selected = names(df)[1])
    })
    
    # main event
    res_r <- eventReactive(input$run, {
      # ---- collect lipids ----
      lipids <- character()
      
      if (identical(input$input_mode, "paste")) {
        txt <- input$paste_txt %||% ""
        # split by newline
        lipids <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
        lipids <- .clean_lipid_vec(lipids)
      } else {
        req(input$infile)
        req(input$name_col)
        sep <- if (identical(input$sep, "\t")) "\t" else ","
        df <- utils::read.delim(
          input$infile$datapath,
          sep = sep,
          header = TRUE,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        if (!input$name_col %in% names(df)) {
          stop("Selected name column not found in uploaded file.")
        }
        lipids <- .clean_lipid_vec(df[[input$name_col]])
      }
      
      if (!length(lipids)) {
        stop("No lipid names found. Please paste or upload at least one name.")
      }
      
      # ---- parse ----
      parsed <- .rgoslin_parse(lipids)
      
      # Normalize error column name differences
      # Many versions use "error" column; keep it if present, otherwise create empty
      if (!"input" %in% names(parsed)) {
        # some versions might use "Original.Name" or similar; try to restore
        orig_col <- .pick1(parsed, c("Original.Name", "original_name", "lipid", "lipid_name"))
        if (!is.na(orig_col)) parsed$input <- as.character(parsed[[orig_col]])
      }
      if (!"input" %in% names(parsed)) {
        parsed$input <- lipids
      }
      if (!"error" %in% names(parsed)) parsed$error <- ""
      
      # ---- choose ontology column ----
      abbr_col <- .pick1(parsed, c("Functional.Class.Abbr", "functional_class_abbr", "functionalClassAbbr"))
      main_col <- .pick1(parsed, c("Lipid.Maps.Main.Class", "lipid_maps_main_class", "lipidMapsMainClass"))
      cat_col  <- .pick1(parsed, c("Lipid.Maps.Category", "lipid_maps_category", "lipidMapsCategory"))
      
      src <- input$ontology_source %||% "abbr"
      ont_col <- NA_character_
      if (identical(src, "abbr")) ont_col <- abbr_col
      if (identical(src, "main")) ont_col <- main_col
      if (identical(src, "cat"))  ont_col <- cat_col
      
      # fallback chain: abbr -> main -> cat
      if (is.na(ont_col)) ont_col <- abbr_col
      if (is.na(ont_col)) ont_col <- main_col
      if (is.na(ont_col)) ont_col <- cat_col
      
      if (is.na(ont_col)) {
        # As last resort: try "Grammar" or "Lipid.Maps.Main.Class" not found, make Unknown
        parsed$Ontology <- "Unknown"
      } else {
        parsed$Ontology <- as.character(parsed[[ont_col]])
        parsed$Ontology[is.na(parsed$Ontology) | parsed$Ontology == ""] <- "Unknown"
      }
      
      # ---- create outputs ----
      ok <- parsed[is.na(parsed$error) | parsed$error == "", , drop = FALSE]
      ng <- parsed[!is.na(parsed$error) & parsed$error != "", , drop = FALSE]
      
      feature_df <- data.frame(
        `Metabolite name` = as.character(ok$input),
        Ontology          = as.character(ok$Ontology),
        stringsAsFactors  = FALSE,
        check.names       = FALSE
      )
      
      failed_df <- if (nrow(ng)) {
        data.frame(
          input = as.character(ng$input),
          error = as.character(ng$error),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        data.frame(input = character(), error = character(), stringsAsFactors = FALSE)
      }
      
      list(
        feature_df = feature_df,
        failed_df  = failed_df,
        parsed_n   = nrow(ok),
        failed_n   = nrow(ng),
        total_n    = length(lipids),
        ont_used   = ont_col
      )
    }, ignoreInit = TRUE)
    
    # status UI (NO validate() to avoid the error you hit)
    output$status_ui <- renderUI({
      if (is.null(res_r())) {
        return(HTML("<em>Click <b>Parse & build ontology</b> to generate the feature table.</em>"))
      }
      res <- res_r()
      
      tags$div(
        tags$p(tags$b("Summary")),
        tags$ul(
          tags$li(sprintf("Input: %d", res$total_n)),
          tags$li(sprintf("Parsed: %d", res$parsed_n)),
          tags$li(sprintf("Failed: %d", res$failed_n)),
          tags$li(sprintf("Ontology column used: %s", res$ont_used %||% "Unknown"))
        ),
        if (res$failed_n > 0) {
          tags$div(
            style = "margin-top:8px;color:#b45309;background:#fffbeb;border:1px solid #f59e0b;border-radius:8px;padding:8px;",
            tags$b("Note: Some inputs failed. Download the failed list for inspection.")
          )
        }
      )
    })
    
    output$preview_tbl <- renderTable({
      req(res_r())
      head(res_r()$feature_df, 30)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    
    output$failed_tbl <- renderTable({
      req(res_r())
      head(res_r()$failed_df, 30)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    
    output$download_feature <- downloadHandler(
      filename = function() {
        sprintf("feature_ontology_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        req(res_r())
        utils::write.csv(res_r()$feature_df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    output$download_failed <- downloadHandler(
      filename = function() {
        sprintf("failed_inputs_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        req(res_r())
        utils::write.csv(res_r()$failed_df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    invisible(NULL)
  })
}
