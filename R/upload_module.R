# R/mod_upload.R
# Upload & Edit module (navlistPanel + rhandsontable + class mapping + robust checks)
# - Step 1: Upload lipidome data (MS-DIAL or Generic: wide + ontology)
# - Step 2: Edit colData (sample_id, class, use) & subset by use -> analysis SE
# - Step 3: Transcriptome CSV -> SummarizedExperiment (auto-align to Lipidomics sample IDs if possible)
# - Optional: Import class mapping (CSV/TSV) and overwrite colData$class by sample_id
# - Summary shows only {samples, features} (prioritize the analysis SE if present)
#
# Depends: shiny, shinydashboard, rhandsontable, SummarizedExperiment, S4Vectors, utils, methods, htmltools

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(rhandsontable)
  library(SummarizedExperiment)
  library(S4Vectors)
  library(htmltools)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# Summarize only key SE stats (samples / features / groups) and warnings
# ============================================================
.summarize_se <- function(se, assay_name = "abundance", group_col = "class") {
  stopifnot(methods::is(se, "SummarizedExperiment"))
  a  <- SummarizedExperiment::assay(se, assay_name)
  cd <- as.data.frame(SummarizedExperiment::colData(se))

  n_samp  <- ncol(a)
  n_feat  <- nrow(a)
  has_grp <- group_col %in% names(cd)
  n_group <- if (has_grp) length(unique(cd[[group_col]])) else NA_integer_

  samp_ids <- colnames(a)
  feat_ids <- rownames(a)
  dup_samp <- unique(samp_ids[duplicated(samp_ids)])
  dup_feat <- unique(feat_ids[duplicated(feat_ids)])

  warns <- character()
  if (length(dup_samp)) warns <- c(
    warns,
    sprintf("Duplicated sample_id: %s", paste(head(dup_samp, 5), collapse = ", "))
  )
  if (length(dup_feat)) warns <- c(
    warns,
    sprintf("Duplicated feature_id: %s", paste(head(dup_feat, 5), collapse = ", "))
  )
  if (has_grp) {
    n_empty_class <- sum(is.na(cd[[group_col]]) | cd[[group_col]] == "")
    if (n_empty_class > 0) warns <- c(
      warns,
      sprintf("Samples with empty '%s': %d", group_col, n_empty_class)
    )
  }

  list(
    n_samples  = n_samp,
    n_features = n_feat,
    n_groups   = n_group,
    warnings   = warns
  )
}

.render_kpi_cards <- function(x) {
  htmltools::HTML(sprintf('
    <div style="display:flex; gap:12px; flex-wrap:wrap;">
      <div style="flex:1; min-width:160px; background:#ffffff;border:1px solid #e5e7eb;border-radius:10px;padding:12px;">
        <div style="font-size:12px;color:#64748b;">Samples</div>
        <div style="font-size:22px;font-weight:700;">%s</div>
      </div>
      <div style="flex:1; min-width:160px; background:#ffffff;border:1px solid #e5e7eb;border-radius:10px;padding:12px;">
        <div style="font-size:12px;color:#64748b;">Features</div>
        <div style="font-size:22px;font-weight:700;">%s</div>
      </div>
      <div style="flex:1; min-width:160px; background:#ffffff;border:1px solid #e5e7eb;border-radius:10px;padding:12px;">
        <div style="font-size:12px;color:#64748b;">Groups (class)</div>
        <div style="font-size:22px;font-weight:700;">%s</div>
      </div>
    </div>',
    x$n_samples,
    x$n_features,
    ifelse(is.na(x$n_groups), "-", x$n_groups)
  ))
}

.render_kpi_cards_tx <- function(x) {
  htmltools::HTML(sprintf('
    <div style="display:flex; gap:12px; flex-wrap:wrap;">
      <div style="flex:1; min-width:160px; background:#ffffff;border:1px solid #e5e7eb;border-radius:10px;padding:12px;">
        <div style="font-size:12px;color:#64748b;">Samples</div>
        <div style="font-size:22px;font-weight:700;">%s</div>
      </div>
      <div style="flex:1; min-width:160px; background:#ffffff;border:1px solid #e5e7eb;border-radius:10px;padding:12px;">
        <div style="font-size:12px;color:#64748b;">Genes</div>
        <div style="font-size:22px;font-weight:700;">%s</div>
      </div>
    </div>',
    x$n_samples,
    x$n_features
  ))
}

# ============================================================
# Helper: add acyl-chain metadata (if available)
# ============================================================
LIPID_RULES_PATH <- getOption(
  "mslipidmapper.lipid_rules",
  default = "./R/lipid_rules.yaml"  # Change this path if needed.
)

.add_acyl_chains_if_available <- function(se) {
  if (!exists("add_chain_list_to_se", mode = "function") ||
      !exists("load_lipid_rules",     mode = "function")) {
    return(se)
  }
  if (is.null(LIPID_RULES_PATH) || !nzchar(LIPID_RULES_PATH) ||
      !file.exists(LIPID_RULES_PATH)) {
    return(se)
  }
  rules <- try(load_lipid_rules(LIPID_RULES_PATH), silent = TRUE)
  if (inherits(rules, "try-error") || is.null(rules)) {
    warning("Failed to load lipid rules from: ", LIPID_RULES_PATH)
    return(se)
  }
  out <- try(
    add_chain_list_to_se(
      se,
      rules,
      lipid_col = "Metabolite.name",
      out_col   = "acyl_chains"
    ),
    silent = TRUE
  )
  if (inherits(out, "try-error")) {
    warning("Failed to add acyl_chains: ", as.character(out))
    return(se)
  }
  out
}

# ============================================================
# Generic (column-name based): wide + ontology -> SummarizedExperiment
# - measurement columns are ALWAYS: setdiff(names(assay_df), c(sample_id_col, class_col))
# ============================================================
.build_lipidomics_se_generic_cols <- function(
    assay_df,
    feature_df,
    sample_id_col,
    sample_class_col = NULL,
    feature_name_col,
    ontology_col,
    assay_name = "abundance"
) {
  stopifnot(is.data.frame(assay_df), is.data.frame(feature_df))

  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) stop("Please install 'SummarizedExperiment'.")
  if (!requireNamespace("S4Vectors", quietly = TRUE))            stop("Please install 'S4Vectors'.")

  if (is.null(sample_id_col) || !nzchar(sample_id_col) || !(sample_id_col %in% names(assay_df))) {
    stop("sample_id_col is invalid (not found in assay_df).")
  }
  if (!is.null(sample_class_col) && nzchar(sample_class_col) && !(sample_class_col %in% names(assay_df))) {
    stop("sample_class_col is invalid (not found in assay_df).")
  }

  if (is.null(feature_name_col) || !nzchar(feature_name_col) || !(feature_name_col %in% names(feature_df))) {
    stop("feature_name_col is invalid (not found in feature_df).")
  }
  if (is.null(ontology_col) || !nzchar(ontology_col) || !(ontology_col %in% names(feature_df))) {
    stop("ontology_col is invalid (not found in feature_df).")
  }

  # fixed: measurement columns
  exclude_cols <- c(sample_id_col, sample_class_col)
  exclude_cols <- exclude_cols[!is.null(exclude_cols) & nzchar(exclude_cols)]
  value_cols <- setdiff(names(assay_df), exclude_cols)

  if (!length(value_cols)) {
    stop("No measurement columns left after excluding sample_id/class. Check your assay CSV.")
  }

  sample_ids <- as.character(assay_df[[sample_id_col]])
  if (any(!nzchar(sample_ids))) stop("Some sample_id values are empty.")
  if (anyDuplicated(sample_ids)) stop("Duplicated sample_id values were found in assay CSV. Make them unique.")

  sample_classes <- if (!is.null(sample_class_col) && nzchar(sample_class_col)) {
    as.character(assay_df[[sample_class_col]])
  } else {
    rep(NA_character_, length(sample_ids))
  }

  # assay_df is samples x features (wide); convert to matrix features x samples
  assay_data <- assay_df[, value_cols, drop = FALSE]
  assay_mat_wide <- as.matrix(
    data.frame(
      lapply(assay_data, function(x) suppressWarnings(as.numeric(x))),
      check.names = FALSE
    )
  )
  rownames(assay_mat_wide) <- sample_ids
  colnames(assay_mat_wide) <- value_cols

  if (all(is.na(assay_mat_wide))) stop("All measurement values are NA after numeric conversion. Check your CSV.")

  assay_mat <- t(assay_mat_wide)

  col_data <- S4Vectors::DataFrame(
    sample_id = sample_ids,
    class     = sample_classes
  )
  rownames(col_data) <- sample_ids

  feature_names <- value_cols

  rd <- S4Vectors::DataFrame(
    `Metabolite name` = feature_names,
    Ontology          = rep(NA_character_, length(feature_names)),
    check.names       = FALSE
  )

  fn_vec  <- as.character(feature_df[[feature_name_col]])
  ont_vec <- as.character(feature_df[[ontology_col]])

  idx <- match(feature_names, fn_vec)
  hit <- !is.na(idx)
  rd$Ontology[hit] <- ont_vec[idx[hit]]

  n_unmatched <- sum(!hit)
  if (n_unmatched > 0) {
    message("Warning: ", n_unmatched, " features have no matching Ontology in feature_df.")
  }

  rd$subclass <- ifelse(
    is.na(rd$Ontology) | rd$Ontology == "",
    "Unknown",
    as.character(rd$Ontology)
  )

  rownames(rd) <- make.unique(as.character(rd[["Metabolite name"]]))

  SummarizedExperiment::SummarizedExperiment(
    assays  = setNames(list(assay_mat), assay_name),
    rowData = rd,
    colData = col_data
  )
}

# ============================================================
# Minimal preflight check for an MS-DIAL alignment CSV
# ============================================================
.check_msdial_csv <- function(path, annotation_cols = 1:35, header_rows = 5, data_start_row = 6) {
  out <- list(ok = TRUE, reasons = character())

  hdr <- try(
    utils::read.csv(path, header = FALSE, nrows = header_rows, check.names = FALSE),
    silent = TRUE
  )
  if (inherits(hdr, "try-error")) {
    out$ok <- FALSE
    out$reasons <- c(out$reasons, "Cannot read the file (CSV parse failed).")
    return(out)
  }
  if (nrow(hdr) < header_rows) {
    out$ok <- FALSE
    out$reasons <- c(out$reasons, sprintf("Header rows < %d.", header_rows))
    return(out)
  }
  true_colnames <- as.character(unlist(hdr[header_rows, ]))
  if (!any(nzchar(true_colnames))) {
    out$ok <- FALSE
    out$reasons <- c(out$reasons, "Header row appears empty (no column names detected).")
  }

  required_cols <- c("Metabolite name","Average Rt(min)","Average Mz","Adduct type","Alignment ID")
  missing <- setdiff(required_cols, true_colnames)
  if (length(missing) > 0) {
    out$ok <- FALSE
    out$reasons <- c(out$reasons, paste("Missing required columns:", paste(missing, collapse = ", ")))
  }

  if (max(annotation_cols) >= length(true_colnames)) {
    out$ok <- FALSE
    out$reasons <- c(out$reasons, "annotation_cols range exceeds number of columns.")
  }

  body_row <- try(
    utils::read.csv(path, header = FALSE, skip = data_start_row - 1, nrows = 1, check.names = FALSE),
    silent = TRUE
  )
  if (!inherits(body_row, "try-error")) {
    first_assay_col <- max(annotation_cols) + 1
    if (first_assay_col <= ncol(body_row)) {
      v <- suppressWarnings(as.numeric(body_row[[first_assay_col]]))
      if (is.na(v)) {
        out$ok <- FALSE
        out$reasons <- c(out$reasons, "First assay column is not numeric at data_start_row.")
      }
    }
  }
  out
}

# ---------------- UI ----------------
#' @export
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Step 1: Upload data"),

    shiny::navlistPanel(
      widths = c(2, 10),

      # ---- Page 1: Upload lipidome data ----
      shiny::tabPanel(
        "1) Upload lipidome data",

        shiny::div(
          style = "background:#f8f9fa;border:1px solid #e5e7eb;border-radius:10px;padding:18px;margin-bottom:16px;",
          shiny::h2("Welcome to MSLipidMapper"),
          shiny::p("MSLipidMapper supports pathway-based exploration of lipidomics and multi-omics data."),
          shiny::h3("What you can do"),
          shiny::tags$ul(
            shiny::tags$li("Import lipidomics data as the primary input."),
            shiny::tags$li("Optionally add other omics layers (e.g., gene expression) for integrated analysis."),
            shiny::tags$li("Explore results with interactive plots and pathway-based views.")
          )
        ),

        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::radioButtons(
              ns("lipid_format"),
              "Lipidomics data format",
              choices = c(
                "MS-DIAL alignment CSV"     = "msdial",
                "Generic (wide + ontology)" = "generic"
              ),
              selected = "msdial"
            )
          ),
          shiny::column(
            width = 9,

            # ---------------- MS-DIAL ----------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'msdial'", ns("lipid_format")),
              shiny::fluidRow(
                shiny::column(
                  width = 3,
                  shiny::helpText("Upload an MS-DIAL Alignment CSV."),
                  shiny::fileInput(
                    ns("file"), NULL, accept = ".csv",
                    buttonLabel = "Browse...", placeholder = "No file selected"
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 35px;",
                  shiny::actionButton(ns("build"), "Submit (MS-DIAL)", class = "btn btn-primary")
                )
              )
            ),

            # ---------------- Generic ----------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'generic'", ns("lipid_format")),

              shiny::div(
                style = "margin-bottom:10px; padding:10px; border:1px solid #fde68a; background:#fffbeb; border-radius:10px;",
                shiny::tags$b("Generic input requires an Ontology file (lipid name ??? Ontology). "),
                shiny::span("If you do not have one, create it in Utility ??? Ontology builder."),
                shiny::div(style = "margin-top:8px;",
                  shiny::actionButton(
                    ns("go_ontology_builder"),
                    "Go to Ontology builder",
                    icon = shiny::icon("table"),
                    class = "btn btn-warning btn-sm"
                  )
                )
              ),

              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shinydashboard::box(
                    width = 12, status = "primary", solidHeader = TRUE,
                    title = "Assay CSV (wide: samples × lipids)",
                    shiny::fileInput(ns("generic_assay_file"), "Assay CSV", accept = ".csv"),
                    shiny::uiOutput(ns("generic_assay_col_ui")),
                    shiny::tags$hr(),
                    shiny::helpText("Measurement columns are automatically: all columns except sample_id/class.")
                  )
                ),
                shiny::column(
                  width = 6,
                  shinydashboard::box(
                    width = 12, status = "primary", solidHeader = TRUE,
                    title = "Feature/Ontology CSV (lipid name ??? Ontology)",
                    shiny::fileInput(ns("generic_feature_file"), "Feature/Ontology CSV", accept = ".csv"),
                    shiny::uiOutput(ns("generic_feature_col_ui")),
                    shiny::tags$hr(),
                    shiny::helpText("This table maps each lipid column name to its Ontology (subclass).")
                  )
                )
              ),

              shiny::div(style = "margin-top: 10px;",
                shiny::actionButton(ns("build_generic"), "Submit (Generic)", class = "btn btn-primary")
              )
            )
          )
        ),

        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Summary (analysis samples)"),
            shiny::htmlOutput(ns("summary_html")),
            shiny::br()
          )
        ),
        shiny::hr()
      ),

      # ---- Page 2: Edit & Select Samples ----
      shiny::tabPanel(
        "2) Edit & Select Samples",
        shiny::fluidRow(
          shiny::column(
            width = 7,

            shinydashboard::box(
              width = 12,
              title = "Master sample table (all samples)",
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              rhandsontable::rHandsontableOutput(ns("cd_hot"), height = 400)
            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,
                style = "display:flex; justify-content:center; margin: 10px 0 14px 0; clear: both;",
                shiny::actionButton(
                  ns("apply_edits"),
                  "Apply edits & update analysis samples (use == TRUE) ???",
                  class = "btn-warning",
                  style = "min-width: 420px; white-space: normal;"
                )
              )
            ),

            shinydashboard::box(
              width = 12,
              title = "Samples currently used for analysis (use == TRUE)",
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              rhandsontable::rHandsontableOutput(ns("cd_hot_selected"), height = 400),
              shiny::br()
            )
          ),

          shiny::column(
            width = 5,

            shinydashboard::box(
              width = 12,
              title = "Import class file (CSV/TSV)",
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::radioButtons(
                    ns("class_sep"), "Separator",
                    c(CSV = ",", TSV = "\t"), inline = TRUE
                  ),
                  shiny::fileInput(
                    ns("class_file"),
                    "Class mapping file",
                    accept = c(".csv", ".tsv", ".txt")
                  ),
                  shiny::checkboxInput(
                    ns("reset_use"),
                    "Reset 'use' by class after import\n(turn OFF for Blank/QC)",
                    TRUE
                  ),
                  shiny::uiOutput(ns("class_colpick_ui")),
                  shiny::actionButton(
                    ns("apply_class_map"),
                    "Apply class mapping",
                    class = "btn-info"
                  ),
                  shiny::br(),
                  shiny::helpText("Required columns: sample_id and class")
                ),
                shiny::column(
                  width = 8,
                  shiny::div(
                    style = "width:100%; overflow-x:auto;",
                    shiny::tableOutput(ns("class_preview"))
                  )
                )
              )
            ),

            shinydashboard::box(
              width = 12,
              title = "Class filter",
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(
                  width = 7,
                  rhandsontable::rHandsontableOutput(ns("class_use_hot"), height = 300)
                ),
                shiny::column(
                  width = 5,
                  shiny::helpText(
                    "Each row corresponds to one class.\n",
                    "Edit 'use_class' (TRUE/FALSE) and click the button\n",
                    "to update colData$use for all samples in each class."
                  ),
                  shiny::br(),
                  shiny::actionButton(
                    ns("apply_class_use"),
                    "Filtering class",
                    class = "btn btn-primary",
                    style = "white-space:normal;"
                  )
                )
              )
            )
          )
        ),

        shiny::hr(),

        shinydashboard::box(
          width = 12,
          title = "Variable data (rowData preview)",
          solidHeader = TRUE,
          collapsible = TRUE,
          rhandsontable::rHandsontableOutput(ns("rd_hot"), height = 300)
        )
      ),

      # ---- Page 3: Transcriptome ----
      shiny::tabPanel(
        "3) Upload multiomics data",
        shiny::helpText("Upload a transcriptome count table. Must contain a 'GeneID' column."),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::fileInput(ns("tx_file"), "Transcriptome CSV", accept = ".csv")
          ),
          shiny::column(
            width = 4,
            shiny::selectInput(
              ns("tx_organism"), "Organism",
              choices = c(
                "Human (hsapiens)"  = "hsapiens",
                "Mouse (mmusculus)" = "mmusculus",
                "Rat (rnorvegicus)" = "rnorvegicus"
              ),
              selected = "mmusculus"
            ),
            shiny::div(
              style = "margin-top: 6px;",
              shiny::actionButton(ns("tx_build"), "Submit Transcriptome data", class = "btn btn-primary")
            )
          ),
          shiny::column(
            width = 4,
            shiny::h4("Summary"),
            shiny::htmlOutput(ns("tx_summary_html"))
          )
        ),
        shiny::br(),
        shiny::hr(),
        shiny::h4("Sample table (Transcriptome data)"),
        rhandsontable::rHandsontableOutput(ns("tx_col_preview"), height = 220),
        shiny::br(),
        shiny::h4("Preview"),
        rhandsontable::rHandsontableOutput(ns("tx_row_preview"), height = 240)
      )
    )
  )
}

# ---------------- Server ----------------
#' @export
mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    se_full_r <- shiny::reactiveVal(NULL)
    se_sel_r  <- shiny::reactiveVal(NULL)
    se_tx_r   <- shiny::reactiveVal(NULL)

    .update_selected_from_use <- function(se_full) {
      cd0 <- as.data.frame(SummarizedExperiment::colData(se_full))
      if (!"use" %in% colnames(cd0)) {
        return(se_full)
      }
      keep <- rownames(cd0)[which(cd0$use %in% TRUE)]
      if (length(keep) == 0L) {
        shiny::showNotification(
          "No samples selected (use == TRUE is 0). Analysis SE falls back to all samples.",
          type = "warning"
        )
        return(se_full)
      }
      se_full[, keep, drop = FALSE]
    }

    # ---- Generic CSV preview readers (for column-name dropdowns) ----------
    generic_assay_df <- shiny::reactive({
      req(input$generic_assay_file)
      df <- try(
        utils::read.csv(
          input$generic_assay_file$datapath,
          header = TRUE,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        silent = TRUE
      )
      shiny::validate(shiny::need(!inherits(df, "try-error") && is.data.frame(df), "Failed to read assay CSV."))
      shiny::validate(shiny::need(ncol(df) >= 2, "Assay CSV must have at least 2 columns."))
      df
    })

    generic_feature_df <- shiny::reactive({
      req(input$generic_feature_file)
      df <- try(
        utils::read.csv(
          input$generic_feature_file$datapath,
          header = TRUE,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        silent = TRUE
      )
      shiny::validate(shiny::need(!inherits(df, "try-error") && is.data.frame(df), "Failed to read feature CSV."))
      shiny::validate(shiny::need(ncol(df) >= 2, "Feature/Ontology CSV must have at least 2 columns."))
      df
    })

    # ---- UI: assay column pickers (names) --------------------------------
    output$generic_assay_col_ui <- shiny::renderUI({
      df <- generic_assay_df()
      cols <- names(df)

      default_sample <- if ("sample_id" %in% cols) "sample_id" else cols[1]
      default_class  <- if ("class" %in% cols) "class" else if (length(cols) >= 2) cols[2] else ""

      shiny::tagList(
        shiny::selectInput(
          ns("generic_sample_id_colname"),
          "Sample ID column",
          choices = cols,
          selected = default_sample
        ),
        shiny::selectInput(
          ns("generic_sample_class_colname"),
          "Class column (optional)",
          choices = c("- none -" = "", stats::setNames(cols, cols)),
          selected = default_class
        )
      )
    })

    # ---- UI: feature column pickers (names) ------------------------------
    output$generic_feature_col_ui <- shiny::renderUI({
      df <- generic_feature_df()
      cols <- names(df)

      default_name <- if (any(tolower(cols) %in% c("lipid", "lipid_name", "metabolite", "metabolite name", "metabolite.name", "name"))) {
        cols[which(tolower(cols) %in% c("lipid", "lipid_name", "metabolite", "metabolite name", "metabolite.name", "name"))[1]]
      } else {
        cols[1]
      }
      default_ont <- if ("Ontology" %in% cols) "Ontology" else if ("ontology" %in% cols) "ontology" else cols[min(2, length(cols))]

      shiny::tagList(
        shiny::selectInput(
          ns("generic_feature_name_colname"),
          "Lipid name column (feature CSV)",
          choices = cols,
          selected = default_name
        ),
        shiny::selectInput(
          ns("generic_ontology_colname"),
          "Ontology column (feature CSV)",
          choices = cols,
          selected = default_ont
        )
      )
    })

    # ---- Build SE (MS-DIAL) ----------------------------------------------
    shiny::observeEvent(input$build, {
      req(input$file)
      if (!identical(input$lipid_format, "msdial")) return()

      path <- input$file$datapath

      chk <- .check_msdial_csv(path, annotation_cols = 1:35, header_rows = 5, data_start_row = 6)
      if (!isTRUE(chk$ok)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "File format error",
            easyClose = TRUE, footer = shiny::modalButton("OK"),
            shiny::p("This file does not look like an MS-DIAL alignment table with the expected format."),
            shiny::hr(),
            shiny::p("Please check the file format.")
          )
        )
        return(invisible(NULL))
      }

      se <- try(
        load_lipidomics_se(
          csv_path        = path,
          annotation_cols = 1:35,
          header_rows     = 5,
          data_start_row  = 6
        ),
        silent = TRUE
      )
      if (inherits(se, "try-error")) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Failed to build SE",
            easyClose = TRUE, footer = shiny::modalButton("OK"),
            shiny::p("An error occurred while parsing the file:"),
            shiny::code(as.character(se))
          )
        )
        return(invisible(NULL))
      }
      if (!methods::is(se, "SummarizedExperiment") ||
          !"abundance" %in% SummarizedExperiment::assayNames(se)) {
        shiny::showNotification("Invalid SummarizedExperiment structure.", type = "error")
        return(invisible(NULL))
      }

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!"sample_id" %in% names(cd)) cd$sample_id <- rownames(cd)
      if (!"class"     %in% names(cd)) cd$class     <- NA_character_

      default_off <- c("Blank", "Quality control", "QC")
      cd$use <- !(cd$class %in% default_off)

      others <- setdiff(colnames(cd), c("sample_id", "class", "use"))
      cd <- cd[, c("sample_id", "class", "use", others), drop = FALSE]
      SummarizedExperiment::colData(se) <- S4Vectors::DataFrame(cd, row.names = cd$sample_id)

      se <- .add_acyl_chains_if_available(se)

      se_full_r(se)
      se_sel_r(.update_selected_from_use(se))

      shiny::showNotification("SE built successfully (MS-DIAL).", type = "message")
    })

    # ---- Build SE (Generic; fixed measurement columns) -------------------
    shiny::observeEvent(input$build_generic, {
      if (!identical(input$lipid_format, "generic")) return()
      req(input$generic_assay_file, input$generic_feature_file)

      assay_df   <- generic_assay_df()
      feature_df <- generic_feature_df()

      req(input$generic_sample_id_colname)
      req(input$generic_feature_name_colname, input$generic_ontology_colname)

      sample_id_col <- input$generic_sample_id_colname
      class_col     <- input$generic_sample_class_colname %||% ""
      if (!nzchar(class_col)) class_col <- NULL

      # (fixed) measurement columns are computed inside builder

      se <- try(
        .build_lipidomics_se_generic_cols(
          assay_df         = assay_df,
          feature_df       = feature_df,
          sample_id_col    = sample_id_col,
          sample_class_col = class_col,
          feature_name_col = input$generic_feature_name_colname,
          ontology_col     = input$generic_ontology_colname,
          assay_name       = "abundance"
        ),
        silent = TRUE
      )

      if (inherits(se, "try-error")) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Failed to build SE (Generic)",
            easyClose = TRUE, footer = shiny::modalButton("OK"),
            shiny::p("An error occurred while parsing generic lipidomics files:"),
            shiny::code(as.character(se))
          )
        )
        return(invisible(NULL))
      }
      if (!methods::is(se, "SummarizedExperiment") ||
          !"abundance" %in% SummarizedExperiment::assayNames(se)) {
        shiny::showNotification("Invalid SummarizedExperiment structure (Generic).", type = "error")
        return(invisible(NULL))
      }

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!"sample_id" %in% names(cd)) cd$sample_id <- rownames(cd)
      if (!"class"     %in% names(cd)) cd$class     <- NA_character_

      default_off <- c("Blank", "Quality control", "QC")
      cd$use <- !(cd$class %in% default_off)

      others <- setdiff(colnames(cd), c("sample_id", "class", "use"))
      cd <- cd[, c("sample_id", "class", "use", others), drop = FALSE]
      SummarizedExperiment::colData(se) <- S4Vectors::DataFrame(cd, row.names = cd$sample_id)

      se <- .add_acyl_chains_if_available(se)

      se_full_r(se)
      se_sel_r(.update_selected_from_use(se))

      shiny::showNotification("SE built successfully (Generic).", type = "message")
    })

    # ---- Summary: prioritize analysis SE (fallback to full SE) -----------
    output$summary_html <- shiny::renderUI({
      se_sel  <- se_sel_r()
      se_full <- se_full_r()
      se <- se_sel %||% se_full
      if (is.null(se)) return(shiny::HTML("<em>No data loaded.</em>"))

      sx <- .summarize_se(se, assay_name = "abundance", group_col = "class")
      htmltools::tagList(
        .render_kpi_cards(sx),
        if (length(sx$warnings))
          htmltools::tags$div(
            style = "margin-top:8px;color:#b45309;background:#fffbeb;border:1px solid #f59e0b;border-radius:8px;padding:8px;",
            htmltools::tags$strong("Warnings: "),
            htmltools::tags$ul(lapply(sx$warnings, htmltools::tags$li))
          )
      )
    })

    # ---- Master colData as rhandsontable (all samples) -------------------
    output$cd_hot <- rhandsontable::renderRHandsontable({
      se_full <- se_full_r(); req(se_full)
      df <- as.data.frame(SummarizedExperiment::colData(se_full))
      others <- setdiff(colnames(df), c("sample_id", "class", "use"))
      df <- df[, c("sample_id", "class", "use", others), drop = FALSE]

      rhandsontable::rhandsontable(
        df,
        readOnly   = FALSE,
        rowHeaders = NULL,
        stretchH   = "all"
      ) |>
        rhandsontable::hot_col("sample_id", readOnly = TRUE) |>
        rhandsontable::hot_col("use", type = "checkbox")
    })

    # ---- Selected colData as rhandsontable (analysis; use==TRUE) ----------
    output$cd_hot_selected <- rhandsontable::renderRHandsontable({
      se_sel  <- se_sel_r()
      se_full <- se_full_r()
      se <- se_sel %||% se_full
      req(se)

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!"sample_id" %in% names(cd)) cd$sample_id <- rownames(cd)
      others <- setdiff(colnames(cd), c("sample_id", "class", "use"))
      cd <- cd[, c("sample_id", "class", "use", others), drop = FALSE]

      rhandsontable::rhandsontable(
        cd,
        readOnly   = TRUE,
        rowHeaders = NULL,
        stretchH   = "all"
      ) |>
        rhandsontable::hot_col("sample_id", readOnly = TRUE)
    })

    # ---- rowData head as rhandsontable ----------------------------------
    output$rd_hot <- rhandsontable::renderRHandsontable({
      se_sel  <- se_sel_r()
      se_full <- se_full_r()
      se <- se_sel %||% se_full
      req(se)

      rdfull <- as.data.frame(SummarizedExperiment::rowData(se))
      n_show <- min(200, nrow(rdfull))
      df <- utils::head(rdfull, n_show)
      df$feature_id <- rownames(rdfull)[seq_len(n_show)]
      df <- df[, c("feature_id", setdiff(colnames(df), "feature_id")), drop = FALSE]

      rhandsontable::rhandsontable(
        df,
        readOnly   = TRUE,
        rowHeaders = NULL,
        stretchH   = "all"
      ) |>
        rhandsontable::hot_col("feature_id", readOnly = TRUE)
    })

    # ---- Apply edits & update analysis samples --------------------------
    shiny::observeEvent(input$apply_edits, {
      se_full <- se_full_r(); req(se_full)
      if (!is.null(input$cd_hot)) {
        df_cd <- rhandsontable::hot_to_r(input$cd_hot)
        others <- setdiff(colnames(df_cd), c("sample_id", "class", "use"))
        df_cd <- df_cd[, c("sample_id", "class", "use", others), drop = FALSE]
        stopifnot("sample_id" %in% names(df_cd))
        rownames(df_cd) <- df_cd$sample_id
        if ("use" %in% names(df_cd)) {
          df_cd$use <- as.logical(df_cd$use)
          df_cd$use[is.na(df_cd$use)] <- FALSE
        }

        cd0 <- as.data.frame(SummarizedExperiment::colData(se_full))
        if (!"sample_id" %in% names(cd0)) cd0$sample_id <- rownames(cd0)
        rownames(cd0) <- cd0$sample_id

        common_cols <- intersect(colnames(cd0), colnames(df_cd))
        cd0[rownames(df_cd), common_cols] <- df_cd[rownames(df_cd), common_cols]

        others2 <- setdiff(colnames(cd0), c("sample_id", "class", "use"))
        cd0 <- cd0[, c("sample_id", "class", "use", others2), drop = FALSE]

        SummarizedExperiment::colData(se_full) <-
          S4Vectors::DataFrame(cd0, row.names = cd0$sample_id)

        se_full_r(se_full)
        se_sel_r(.update_selected_from_use(se_full))
      }
      shiny::showNotification("Edits applied and analysis samples updated from 'use'.", type = "message")
    })

    # ==== Class-level 'use' control: table (master) =======================
    output$class_use_hot <- rhandsontable::renderRHandsontable({
      se_full <- se_full_r(); req(se_full)
      cd <- as.data.frame(SummarizedExperiment::colData(se_full))

      if (!"class" %in% names(cd)) return(NULL)

      classes <- sort(unique(cd$class))
      classes <- classes[!is.na(classes) & nzchar(classes)]
      if (!length(classes)) return(NULL)

      if ("use" %in% names(cd)) {
        class_use <- vapply(
          classes,
          function(cl) {
            vals <- cd$use[cd$class == cl]
            any(isTRUE(as.logical(vals)))
          },
          logical(1)
        )
      } else {
        class_use <- rep(TRUE, length(classes))
      }

      tbl <- data.frame(
        class     = classes,
        use_class = class_use,
        stringsAsFactors = FALSE
      )

      rhandsontable::rhandsontable(
        tbl,
        readOnly   = FALSE,
        rowHeaders = NULL,
        stretchH   = "all"
      ) |>
        rhandsontable::hot_col("class", readOnly = TRUE) |>
        rhandsontable::hot_col("use_class", type = "checkbox")
    })

    shiny::observeEvent(input$apply_class_use, {
      se_full <- se_full_r(); req(se_full)

      if (is.null(input$class_use_hot)) {
        shiny::showNotification("No class table available.", type = "warning")
        return()
      }

      tbl <- rhandsontable::hot_to_r(input$class_use_hot)
      if (is.null(tbl) || !nrow(tbl)) {
        shiny::showNotification("Class table is empty.", type = "warning")
        return()
      }

      if (!all(c("class", "use_class") %in% names(tbl))) {
        shiny::showNotification("Class table must have 'class' and 'use_class' columns.", type = "error")
        return()
      }

      tbl$class     <- as.character(tbl$class)
      tbl$use_class <- as.logical(tbl$use_class)
      tbl$use_class[is.na(tbl$use_class)] <- FALSE

      cd <- as.data.frame(SummarizedExperiment::colData(se_full))
      if (!"class" %in% names(cd)) {
        shiny::showNotification("No 'class' column in colData.", type = "error")
        return()
      }
      if (!"sample_id" %in% names(cd)) cd$sample_id <- rownames(cd)
      rownames(cd) <- cd$sample_id

      key <- match(cd$class, tbl$class)
      cd$use <- ifelse(is.na(key), FALSE, tbl$use_class[key])

      others <- setdiff(colnames(cd), c("sample_id", "class", "use"))
      cd <- cd[, c("sample_id", "class", "use", others), drop = FALSE]

      SummarizedExperiment::colData(se_full) <-
        S4Vectors::DataFrame(cd, row.names = cd$sample_id)

      se_full_r(se_full)
      se_sel_r(.update_selected_from_use(se_full))

      shiny::showNotification(
        sprintf("Updated colData$use from class table: %d TRUE, %d FALSE.", sum(cd$use), sum(!cd$use)),
        type = "message", duration = 5
      )
    })

    # ===== Class mapping (CSV/TSV) =======================================
    class_df <- shiny::reactive({
      req(input$class_file)
      sep <- if (identical(input$class_sep, "\t")) "\t" else ","
      df <- try({
        utils::read.delim(
          input$class_file$datapath,
          sep = sep, header = TRUE,
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }, silent = TRUE)
      shiny::validate(shiny::need(!inherits(df, "try-error") && !is.null(df),
                                  "Failed to read class file."))
      df
    })

    output$class_colpick_ui <- shiny::renderUI({
      df <- class_df(); req(df)
      cols <- colnames(df)
      shiny::tagList(
        shiny::selectInput(
          ns("col_sample"), "Column for sample_id", choices = cols,
          selected = if ("sample_id" %in% cols) "sample_id" else cols[1]
        ),
        shiny::selectInput(
          ns("col_class"), "Column for class", choices = cols,
          selected = if ("class" %in% cols) "class" else cols[min(2, length(cols))]
        )
      )
    })

    output$class_preview <- shiny::renderTable({
      df <- class_df(); req(df)
      utils::head(df, 8)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$class_import_msg <- shiny::renderText({ "" })

    shiny::observeEvent(input$apply_class_map, {
      se_full <- se_full_r(); req(se_full)
      df <- class_df(); req(df)
      req(input$col_sample, input$col_class)

      map <- df[, c(input$col_sample, input$col_class), drop = FALSE]
      colnames(map) <- c("sample_id", "class")
      map$sample_id <- as.character(map$sample_id)

      cd <- as.data.frame(SummarizedExperiment::colData(se_full))
      if (!"sample_id" %in% names(cd)) cd$sample_id <- rownames(cd)
      cd$sample_id <- as.character(cd$sample_id)
      rownames(cd) <- cd$sample_id

      if (any(duplicated(map$sample_id))) {
        map <- map[!duplicated(map$sample_id, fromLast = TRUE), , drop = FALSE]
      }

      key <- match(cd$sample_id, map$sample_id)
      matched <- !is.na(key)
      n_match <- sum(matched)
      cd$class[matched] <- map$class[key[matched]]

      if (!"use" %in% names(cd)) {
        cd$use <- TRUE
      }

      others <- setdiff(colnames(cd), c("sample_id", "class", "use"))
      cd <- cd[, c("sample_id", "class", "use", others), drop = FALSE]
      SummarizedExperiment::colData(se_full) <- S4Vectors::DataFrame(cd, row.names = cd$sample_id)

      se_full_r(se_full)
      se_sel_r(.update_selected_from_use(se_full))

      not_found <- setdiff(map$sample_id, cd$sample_id)
      msg <- sprintf("Mapped class to %d/%d samples. Unmatched in SE: %d",
                     n_match, nrow(cd), length(not_found))
      if (length(not_found)) {
        msg <- paste0(msg, "\nUnmatched sample_id (first 5): ",
                      paste(utils::head(not_found, 5), collapse = ", "))
      }
      output$class_import_msg <- shiny::renderText(msg)
      shiny::showNotification("Class mapping applied to colData.", type = "message")
    })

    # ===== Transcriptome: build & align ==================================
    shiny::observeEvent(input$tx_build, {
      req(input$tx_file)
      org <- input$tx_organism %||% "mmusculus"

      se_tx <- try(
        {
          if (is.function(load_transcriptome_se_from_symbol_ensembl)) {
            load_transcriptome_se_from_symbol_ensembl(
              csv_path      = input$tx_file$datapath,
              organism      = org,
              symbol_col    = "Gene",
              ensembl_col   = "EnsemblID",
              none_tokens   = "none",
              keep_unmapped = "keep_na"
            )
          } else {
            load_transcriptome_se(
              csv_path = input$tx_file$datapath,
              organism = org, target = "ENSG"
            )
          }
        },
        silent = TRUE
      )

      if (inherits(se_tx, "try-error")) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Failed to build Transcriptome SE",
            easyClose = TRUE, footer = shiny::modalButton("OK"),
            shiny::p("An error occurred while parsing the transcriptome CSV:"),
            shiny::code(as.character(se_tx))
          )
        )
        return(invisible(NULL))
      }
      if (!methods::is(se_tx, "SummarizedExperiment") ||
          !"abundance" %in% SummarizedExperiment::assayNames(se_tx)) {
        shiny::showNotification("Invalid transcriptome SE structure.", type = "error")
        return(invisible(NULL))
      }

      se_lip_sel  <- se_sel_r()
      se_lip_full <- se_full_r()
      se_lip <- se_lip_sel %||% se_lip_full

      if (!is.null(se_lip)) {
        lip_ids <- colnames(SummarizedExperiment::assay(se_lip, "abundance"))
        tx_ids  <- colnames(SummarizedExperiment::assay(se_tx,  "abundance"))
        keep    <- intersect(lip_ids, tx_ids)

        if (length(keep) == 0L) {
          shiny::showNotification(
            "No overlapping samples between Lipidomics and Transcriptome.",
            type = "warning", duration = 8
          )
        } else {
          keep_ordered <- lip_ids[lip_ids %in% keep]
          se_tx <- se_tx[, keep_ordered, drop = FALSE]

          cd_tx <- as.data.frame(SummarizedExperiment::colData(se_tx))
          if (!"sample_id" %in% names(cd_tx)) cd_tx$sample_id <- rownames(cd_tx)
          cd_tx <- cd_tx[keep_ordered, , drop = FALSE]

          cd_lip <- as.data.frame(SummarizedExperiment::colData(se_lip))
          if (!"sample_id" %in% names(cd_lip)) cd_lip$sample_id <- rownames(cd_lip)
          cd_lip <- cd_lip[match(keep_ordered, cd_lip$sample_id), , drop = FALSE]

          if (!"class" %in% names(cd_lip)) cd_lip$class <- NA_character_
          cd_tx$class <- cd_lip$class

          SummarizedExperiment::colData(se_tx) <-
            S4Vectors::DataFrame(cd_tx, row.names = cd_tx$sample_id)

          dropped <- setdiff(tx_ids, keep)
          shiny::showNotification(
            sprintf("Transcriptome aligned to Lipidomics (analysis SE): kept %d, dropped %d.",
                    length(keep_ordered), length(dropped)),
            type = "message"
          )
        }
      } else {
        shiny::showNotification(
          "Lipidomics SE not loaded yet: Transcriptome kept as-is.",
          type = "warning"
        )
      }

      se_tx_r(se_tx)
      shiny::showNotification("Transcriptome SE built successfully.", type = "message")
    })

    output$tx_summary_html <- shiny::renderUI({
      se_tx <- se_tx_r()
      if (is.null(se_tx)) return(shiny::HTML("<em>No transcriptome loaded.</em>"))
      sx <- .summarize_se(se_tx, assay_name = "abundance", group_col = "class")
      htmltools::tagList(
        .render_kpi_cards_tx(sx),
        if (length(sx$warnings))
          htmltools::tags$div(
            style = "margin-top:8px;color:#b45309;background:#fffbeb;border:1px solid #f59e0b;border-radius:8px;padding:8px;",
            htmltools::tags$strong("Warnings: "),
            htmltools::tags$ul(lapply(sx$warnings, htmltools::tags$li))
          )
      )
    })

    output$tx_row_preview <- rhandsontable::renderRHandsontable({
      se_tx <- se_tx_r(); req(se_tx)
      rd <- as.data.frame(SummarizedExperiment::rowData(se_tx))
      n_show <- min(200, nrow(rd))
      df <- utils::head(rd, n_show)
      df$feature_id <- rownames(rd)[seq_len(n_show)]
      df <- df[, c("feature_id", setdiff(colnames(df), "feature_id")), drop = FALSE]
      rhandsontable::rhandsontable(df, readOnly = TRUE, rowHeaders = NULL, stretchH = "all")
    })

    output$tx_col_preview <- rhandsontable::renderRHandsontable({
      se_tx <- se_tx_r(); req(se_tx)
      cd <- as.data.frame(SummarizedExperiment::colData(se_tx))
      if (!"sample_id" %in% names(cd)) cd$sample_id <- rownames(cd)
      others <- setdiff(colnames(cd), "sample_id")
      cd <- cd[, c("sample_id", others), drop = FALSE]
      rhandsontable::rhandsontable(cd, readOnly = TRUE, rowHeaders = NULL, stretchH = "all")
    })

    list(
      se_all   = shiny::reactive(se_full_r()),
      se       = shiny::reactive(se_sel_r() %||% se_full_r()),
      se_tx    = shiny::reactive(se_tx_r()),
      ready    = shiny::reactive(!is.null(se_full_r())),
      ready_tx = shiny::reactive(!is.null(se_tx_r()))
    )
  })
}

