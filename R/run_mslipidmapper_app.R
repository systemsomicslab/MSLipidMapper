# R/run_mslipidmapper_app.R
# ============================================================
# Run MSLipidMapper dashboard app
# - Dashboard navigation + per-panel "Advanced" buttons hub (Feature module)
# - Prefer normalized SE when ready
# - Utility includes Ontology builder + Pathway editor
# - Inline plumber static server (optional)
#
# FIX:
# - SAFE icon wrapper to avoid startup failure due to invalid FontAwesome names
# ============================================================

#' Run MSLipidMapper dashboard app
#'
#' @param launch.browser logical. If TRUE, open browser.
#' @param max_request_size integer. shiny.maxRequestSize (bytes).
#' @param api_enable logical. If TRUE, try to start inline API static server.
#' @param api_host character. Host for inline API listen (Docker: "0.0.0.0").
#' @param api_port integer. Port for inline API.
#' @param api_public_host character. Hostname embedded into browser URL (local: "localhost").
#' @param api_static_dir character|NULL. If NULL, use tempdir()/cyimg_cache.
#' @param rules_yaml_path character|NULL. Lipid rules YAML path for enrichment.
#' @param skin character. shinydashboard skin name.
#' @param title character. App title.
#' @param verbose_api logical. Verbose for API.
#' @return (invisibly) result of shiny::runApp().
#' @export
run_mslipidmapper_app <- function(
  launch.browser = TRUE,
  max_request_size = 1024 * 1024 * 1024,
  api_enable = TRUE,
  api_host = "0.0.0.0",
  api_port = 7310,
  api_public_host = "localhost",
  api_static_dir = NULL,
  rules_yaml_path = "./R/lipid_rules.yaml",
  skin = "blue",
  title = "MSLipidMapper",
  verbose_api = TRUE
) {

  options(shiny.maxRequestSize = max_request_size)
  try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE)

  suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(SummarizedExperiment)
    library(shinybusy)
  })

  `%||%` <- function(a, b) {
    if (is.null(a) || (is.character(a) && length(a) == 0)) b else a
  }

  # ---- SAFE icon wrapper (prevents app crash if icon name is unsupported) ---
  .ic <- function(name, ...) {
    tryCatch(
      shiny::icon(name, ...),
      error = function(e) shiny::icon("circle")
    )
  }

  # ============================================================
  # 0) Robust loader
  # ============================================================

  .debug_file <- function(f) {
    message("--- DEBUG FILE: ", f, " ---")
    message("exists: ", file.exists(f))
    if (file.exists(f)) {
      fi <- file.info(f)
      message("size: ", fi$size, " bytes")
      txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
      message("lines: ", length(txt))
      message("has plot_dot_se token: ", any(grepl("plot_dot_se", txt, fixed = TRUE)))
      message("has plot_dot_se <- function: ",
              any(grepl("plot_dot_se\\s*<-\\s*function", txt, perl = TRUE)))
      message("has local(: ", any(grepl("^\\s*local\\s*\\(", txt, perl = TRUE)))
      message("has rm(: ", any(grepl("^\\s*rm\\s*\\(", txt, perl = TRUE)))
      message("has detach(: ", any(grepl("^\\s*detach\\s*\\(", txt, perl = TRUE)))
      message("has return( at line-start: ", any(grepl("^\\s*return\\s*\\(", txt, perl = TRUE)))
      message("HEAD(40):\n", paste(head(txt, 40), collapse = "\n"))
    }
    message("--- END DEBUG FILE ---")
    invisible(TRUE)
  }

  .read_parse_exprs <- function(f, enc) {
    txt <- readLines(f, warn = FALSE, encoding = enc)
    parse(text = txt, keep.source = TRUE)
  }

  .eval_capture_must <- function(exprs, envir_eval, must_have = character(0)) {
    saved <- list()
    saved_names <- character(0)

    for (i in seq_along(exprs)) {
      ex <- exprs[[i]]
      tryCatch(
        eval(ex, envir = envir_eval),
        error = function(e) {
          stop(sprintf("Error while evaluating expression #%d: %s", i, conditionMessage(e)), call. = FALSE)
        }
      )

      if (length(must_have)) {
        for (nm in must_have) {
          if (!(nm %in% saved_names) && exists(nm, envir = envir_eval, mode = "function", inherits = FALSE)) {
            saved[[nm]] <- get(nm, envir = envir_eval, inherits = FALSE)
            saved_names <- c(saved_names, nm)
            message("  [CAPTURE] ", nm, " (expr #", i, ")")
          }
        }
      }
    }

    list(saved = saved, saved_names = saved_names)
  }

  .load_file_strict <- function(f, must_have = NULL,
                               encs = c("UTF-8", "UTF-8-BOM", "CP932", "Shift_JIS", "latin1")) {

    message("Sourcing: ", f)
    if (!file.exists(f)) {
      .debug_file(f)
      stop("File not found: ", f, call. = FALSE)
    }

    before <- ls(envir = globalenv(), all.names = TRUE)

    last_err <- NULL
    enc_used <- NA_character_
    saved <- list()
    saved_names <- character(0)

    ok <- FALSE
    for (enc in encs) {
      ok <- tryCatch({
        tmp <- new.env(parent = globalenv())
        exprs <- .read_parse_exprs(f, enc = enc)

        cap <- .eval_capture_must(exprs, envir_eval = tmp, must_have = must_have %||% character(0))
        saved <- cap$saved
        saved_names <- cap$saved_names

        for (nm in ls(tmp, all.names = TRUE)) {
          assign(nm, get(nm, envir = tmp, inherits = FALSE), envir = globalenv())
        }

        if (!is.null(must_have) && length(must_have)) {
          for (nm in must_have) {
            if (!exists(nm, envir = globalenv(), mode = "function", inherits = FALSE) && !is.null(saved[[nm]])) {
              assign(nm, saved[[nm]], envir = globalenv())
              message("  [RESTORE] ", nm, " (restored from captured)")
            }
          }
        }

        enc_used <<- enc
        TRUE
      }, error = function(e) {
        last_err <<- e
        FALSE
      })

      if (isTRUE(ok)) break
    }

    if (!isTRUE(ok)) {
      .debug_file(f)
      stop("Failed to load: ", f, "\nLast error: ", conditionMessage(last_err), call. = FALSE)
    }

    after <- ls(envir = globalenv(), all.names = TRUE)
    newobj <- setdiff(after, before)
    message("  -> OK (encoding=", enc_used, ")  new objects: ",
            if (length(newobj)) paste(newobj, collapse = ", ") else "(none)")

    if (!is.null(must_have) && length(must_have)) {
      miss <- must_have[!vapply(must_have, exists, logical(1),
                                envir = globalenv(), mode = "function", inherits = FALSE)]
      if (length(miss)) {
        message("  -> BUT missing functions after load: ", paste(miss, collapse = ", "))
        .debug_file(f)
        stop("Loaded but functions were not defined: ",
             paste(miss, collapse = ", "), " in ", f, call. = FALSE)
      } else {
        message("  -> Verified functions: ", paste(must_have, collapse = ", "))
      }
    }

    invisible(TRUE)
  }

  .load_app_sources <- function(dir = "R") {
    if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)

    helpers <- file.path(dir, "plot_helpers_lipid.R")
    must <- c(
      "plot_dot_se", "plot_box_se", "plot_violin_se",
      "make_class_heatmap_CH", "theme_lipidomics", "plot_lipid_class_bar"
    )
    .load_file_strict(helpers, must_have = must)

    rfiles <- list.files(dir, pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
    rfiles <- sort(normalizePath(rfiles, winslash = "/", mustWork = FALSE))

    exclude <- c(
      normalizePath(file.path(dir, "run_mslipidmapper_app.R"), winslash = "/", mustWork = FALSE),
      normalizePath(file.path(dir, "plot_helpers_lipid.R"), winslash = "/", mustWork = FALSE),
      normalizePath(file.path(dir, "run_module.R"), winslash = "/", mustWork = FALSE)
    )
    rfiles <- rfiles[!rfiles %in% exclude]

    for (f in rfiles) .load_file_strict(f)

    miss2 <- must[!vapply(must, exists, logical(1),
                         envir = globalenv(), mode = "function", inherits = FALSE)]
    if (length(miss2)) stop("After loading all R/ files, missing: ", paste(miss2, collapse = ", "), call. = FALSE)

    message("All plot helper functions are present.")
    invisible(TRUE)
  }

  .load_app_sources("R")

  # ============================================================
  # 1) rules yaml path resolve
  # ============================================================
  .resolve_rules_yaml <- function(path_in = NULL) {

    norm_if_exists <- function(p) {
      if (is.null(p) || !nzchar(p)) return(NA_character_)
      if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
      NA_character_
    }

    cand <- path_in

    if (is.null(cand) || !nzchar(cand)) {
      envp <- Sys.getenv("MSLM_RULES_YAML", unset = "")
      if (nzchar(envp)) cand <- envp
    }

    out <- norm_if_exists(cand)
    if (!is.na(out)) return(out)

    candidates <- c(
      file.path("R", "lipid_rules.yaml"),
      file.path("R", "lipid_rules.yml"),
      file.path("R", "rules.yaml"),
      file.path("R", "rules.yml")
    )

    if (dir.exists("R")) {
      any_yaml <- list.files("R", pattern = "\\.(ya?ml)$", full.names = TRUE, ignore.case = TRUE)
      candidates <- c(candidates, any_yaml)
    }

    for (p in unique(candidates)) {
      out <- norm_if_exists(p)
      if (!is.na(out)) return(out)
    }

    NULL
  }

  rules_yaml_path2 <- .resolve_rules_yaml(rules_yaml_path)

  # ---- Safe UI/server callers -------------------------------------------
  maybe_ui <- function(fun_name, ...) {
    if (exists(fun_name, mode = "function")) {
      do.call(get(fun_name, mode = "function"), list(...))
    } else {
      shiny::tagList(
        shiny::h3(sprintf("%s not found", fun_name)),
        shiny::helpText(sprintf("Define %s() in your R/ modules.", fun_name))
      )
    }
  }

  maybe_server <- function(fun_name, ...) {
    if (exists(fun_name, mode = "function")) {
      do.call(get(fun_name, mode = "function"), list(...))
    } else {
      NULL
    }
  }

  .boxTitleWithAdv <- function(title_text, btn_id) {
    shiny::div(
      style = "display:flex; align-items:center; width:100%; gap:8px;",
      shiny::span(title_text, style = "font-weight:600;"),
      shiny::div(
        style = "margin-left:auto;",
        shiny::actionButton(
          inputId = btn_id,
          label   = "Advanced",
          icon    = .ic("sliders-h"),
          class   = "btn btn-default btn-xs"
        )
      )
    )
  }

  # ============================================================
  # 2) UI
  # ============================================================
  ui <- shinydashboard::dashboardPage(
    skin = skin,
    shinydashboard::dashboardHeader(title = title),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",

        shinydashboard::menuItem("1. Upload",    tabName = "upload",    icon = .ic("upload")),
        shinydashboard::menuItem("2. Normalize", tabName = "normalize", icon = .ic("sliders")),

        shinydashboard::menuItem(
          "3. Analysis",
          tabName       = "analysis_home",
          icon          = .ic("chart-bar"),
          startExpanded = FALSE,
          shinydashboard::menuSubItem("Analysis hub",     tabName = "analysis_home",       icon = .ic("home")),
          shinydashboard::menuSubItem("PCA",              tabName = "analysis_pca",        icon = .ic("project-diagram")),
          shinydashboard::menuSubItem("OPLS-DA",          tabName = "analysis_oplsda",     icon = .ic("project-diagram")),
          shinydashboard::menuSubItem("Feature",          tabName = "analysis_lipid_feat", icon = .ic("chart-column")),
          shinydashboard::menuSubItem("Heatmap",          tabName = "analysis_heatmap",    icon = .ic("th")),
          shinydashboard::menuSubItem("Correlation",      tabName = "analysis_cor",        icon = .ic("chart-line")),
          shinydashboard::menuSubItem("Volcano",          tabName = "analysis_volcano",    icon = .ic("fire")),
          shinydashboard::menuSubItem("Enrichment",       tabName = "analysis_enrich",     icon = .ic("magnifying-glass")),
          shinydashboard::menuSubItem("Decoupled chains", tabName = "analysis_decoupled",  icon = .ic("shuffle")),
          shinydashboard::menuSubItem("Pathway analysis", tabName = "analysis_network",    icon = .ic("code-branch"))
        ),

        shinydashboard::menuItem(
          "4. Utility",
          tabName       = "utility_home",
          icon          = .ic("tools"),
          startExpanded = FALSE,
          shinydashboard::menuSubItem("Utility hub",      tabName = "utility_home",     icon = .ic("home")),
          shinydashboard::menuSubItem("Ontology builder", tabName = "utility_ontology", icon = .ic("table")),
          shinydashboard::menuSubItem("Pathway editor",   tabName = "utility_pathway",  icon = .ic("code-branch"))
        )
      )
    ),

    shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
/* Overall background */
.content-wrapper, .right-side { background-color: #f5f7fb; }

/* Header */
.skin-blue .main-header .logo { background-color: #143554; color: #ffffff; }
.skin-blue .main-header .navbar { background-color: #143554; }

/* Sidebar */
.skin-blue .main-sidebar { background-color: #ffffff; }
.skin-blue .sidebar-menu > li > a { color: #333333; }
.skin-blue .sidebar-menu > li.active > a,
.skin-blue .sidebar-menu > li:hover > a {
  background-color: #143554; color: #ffffff;
}

/* box (card-like panels) */
.box { border-radius: 8px; border: none; box-shadow: 0 2px 8px rgba(0,0,0,0.05); }
.box.box-solid.box-primary > .box-header { background-color: #143554; color: #ffffff; }
.box.box-solid.box-primary { border: none; }
        "))
      ),

      shinybusy::add_busy_spinner(
        spin = "fading-circle",
        position = "bottom-right",
        margins = c(20, 20),
        height = "50px",
        width  = "50px"
      ),

      shinydashboard::tabItems(

        # ---------------- 1. Upload ----------------
        shinydashboard::tabItem(
          tabName = "upload",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Step 1: Upload",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              maybe_ui("mod_upload_ui", "u")
            )
          )
        ),

        # ---------------- 2. Normalize ----------------
        shinydashboard::tabItem(
          tabName = "normalize",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Step 2: Normalize",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              maybe_ui("mod_normalize_ui", "n")
            )
          )
        ),

        # ---------------- 3-A. Analysis hub ----------------
        shinydashboard::tabItem(
          tabName = "analysis_home",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::h2("Step 3: Analysis"),
              shiny::p("Select an analysis module from the cards below or from the left sidebar.")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "PCA", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Run PCA for an overview of sample structure."),
                shiny::actionButton("go_pca", "Go to PCA", width = "100%", icon = .ic("project-diagram"))
              )
            ),
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "OPLS-DA", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Discriminant analysis (OPLS-DA) and VIPs."),
                shiny::actionButton("go_oplsda", "Go to OPLS-DA", width = "100%", icon = .ic("project-diagram"))
              )
            ),
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Feature", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Visualize feature expression."),
                shiny::actionButton("go_lipid_feat", "Go to Feature", width = "100%", icon = .ic("chart-column"))
              )
            ),
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Heatmap", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Class-level and molecule-level heatmaps."),
                shiny::actionButton("go_hm", "Go to Heatmap", width = "100%", icon = .ic("th"))
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Correlation", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Explore correlations."),
                shiny::actionButton("go_cor", "Go to Correlation", width = "100%", icon = .ic("chart-line"))
              )
            ),
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Volcano", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Volcano plot."),
                shiny::actionButton("go_volcano", "Go to Volcano", width = "100%", icon = .ic("fire"))
              )
            ),
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Enrichment", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("ORA enrichment for lipid class / acyl chains."),
                shiny::actionButton("go_enrich", "Go to Enrichment", width = "100%", icon = .ic("magnifying-glass"))
              )
            ),
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Decoupled chains", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Scatter panels for chain vs class total (decoupling)."),
                shiny::actionButton("go_decoupled", "Go to Decoupled chains", width = "100%", icon = .ic("shuffle"))
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Pathway analysis", width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Exploring metabolic pathway."),
                shiny::actionButton("go_net", "Go to Pathway analysis", width = "100%", icon = .ic("code-branch"))
              )
            )
          )
        ),

        # ---------------- 3-B. PCA tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_pca",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - PCA", "open_adv_pca"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_pca_generic_ui", "pca", title = "PCA")
            )
          )
        ),

        # ---------------- 3-B2. OPLS-DA tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_oplsda",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - OPLS-DA", "open_adv_oplsda"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_oplsda_generic_ui", "opls", title = "OPLS-DA")
            )
          )
        ),

        # ---------------- 3-C. Feature tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_lipid_feat",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - Feature", "open_adv_feat"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_feature_generic_ui", "feat", title = "Feature")
            )
          )
        ),

        # ---------------- 3-D. Heatmap tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_heatmap",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - Heatmap", "open_adv_hm"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_heatmap_ui", "hm", title = "Heatmap")
            )
          )
        ),

        # ---------------- 3-E. Correlation tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_cor",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - Correlation", "open_adv_cor"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_plot_cor_ui", "cor")
            )
          )
        ),

        # ---------------- 3-F. Volcano tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_volcano",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - Volcano", "open_adv_volcano"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_volcano_ui", "volc", title = "Volcano")
            )
          )
        ),

        # ---------------- 3-F2. Enrichment tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_enrich",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - Enrichment", "open_adv_enrich"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_lipid_enrich_ui", "enrich", title = "Enrichment")
            )
          )
        ),

        # ---------------- 3-F3. Decoupled chains tab (NEW) ----------------
       shinydashboard::tabItem(
          tabName = "analysis_decoupled",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - Decoupled chains", "open_adv_decoupled"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_decoupled_chains_ui", "dc", title = "Decoupled chains")
            )
          )
        ),

        # ---------------- 3-G. Pathway analysis tab ----------------
        shinydashboard::tabItem(
          tabName = "analysis_network",
          shiny::fluidRow(
            shinydashboard::box(
              title = .boxTitleWithAdv("Step 3: Analysis - Pathway analysis", "open_adv_net"),
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_cyto_ui", "net")
            )
          )
        ),

        # ---------------- 4-A. Utility hub ----------------
        shinydashboard::tabItem(
          tabName = "utility_home",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::h2("Step 4: Utility"),
              shiny::p("Select a utility module from the cards below or from the left sidebar.")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Ontology builder",
                width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Generate lipid ontology (lipid name -> subclass)."),
                shiny::actionButton("go_util_ontology", "Go to Ontology builder", width = "100%", icon = .ic("table"))
              )
            ),
            shiny::column(
              width = 3,
              shinydashboard::box(
                title = "Pathway editor",
                width = 12, status = "primary", solidHeader = TRUE,
                shiny::p("Edit Pathway map file (.cyjs export)."),
                shiny::actionButton("go_util_pathway", "Go to Pathway editor", width = "100%", icon = .ic("code-branch"))
              )
            )
          )
        ),

        # ---------------- 4-B. Utility: Ontology builder ----------------
        shinydashboard::tabItem(
          tabName = "utility_ontology",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Step 4: Utility - Ontology builder",
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_utility_ontology_builder_ui", "util_ont", title = "Ontology builder")
            )
          )
        ),

        # ---------------- 4-C. Utility: Pathway editor ----------------
        shinydashboard::tabItem(
          tabName = "utility_pathway",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Step 4: Utility - Pathway editor",
              width = 12, status = "primary", solidHeader = TRUE,
              maybe_ui("mod_utility_pathway_editor_ui", "util_path", title = "Pathway editor")
            )
          )
        )
      )
    )
  )

  # ============================================================
  # 3) Server
  # ============================================================
  server <- function(input, output, session) {

    # ---- Step 1: Upload --------------------------------------------------
    step1 <- maybe_server("mod_upload_server", "u")
    if (is.null(step1)) {
      se0    <- shiny::reactiveVal(NULL)
      se_tx0 <- shiny::reactiveVal(NULL)
      step1 <- list(se = shiny::reactive(se0()), se_tx = shiny::reactive(se_tx0()))
    } else {
      if (is.null(step1$se))    step1$se    <- shiny::reactive(NULL)
      if (is.null(step1$se_tx)) step1$se_tx <- shiny::reactive(NULL)
    }

    # ---- Step 2: Normalize ----------------------------------------------
    step2 <- maybe_server("mod_normalize_server", "n", se_in = step1$se)
    if (is.null(step2)) {
      step2 <- list(
        se = shiny::reactive(NULL),
        ready = shiny::reactive(FALSE),
        se_tx = shiny::reactive(NULL),
        ready_tx = shiny::reactive(FALSE)
      )
    } else {
      if (is.null(step2$se))       step2$se       <- shiny::reactive(NULL)
      if (is.null(step2$ready))    step2$ready    <- shiny::reactive(FALSE)
      if (is.null(step2$se_tx))    step2$se_tx    <- shiny::reactive(NULL)
      if (is.null(step2$ready_tx)) step2$ready_tx <- shiny::reactive(FALSE)
    }

    # ---- Select SE for downstream plots (prefer normalized when ready) ---
    se_lipid_for_plot <- shiny::reactive({
      if (!is.null(step2$ready) && isTRUE(step2$ready())) step2$se() else step1$se()
    })

    se_tx_for_plot <- shiny::reactive({
      if (!is.null(step2$se_tx) && !is.null(step2$ready_tx) && isTRUE(step2$ready_tx())) {
        step2$se_tx()
      } else if (!is.null(step1$se_tx)) {
        step1$se_tx()
      } else {
        NULL
      }
    })

    # ---- Navigation buttons (hub cards) ----------------------------------
    shiny::observeEvent(input$go_pca,       { shinydashboard::updateTabItems(session, "tabs", "analysis_pca") })
    shiny::observeEvent(input$go_oplsda,    { shinydashboard::updateTabItems(session, "tabs", "analysis_oplsda") })
    shiny::observeEvent(input$go_lipid_feat,{ shinydashboard::updateTabItems(session, "tabs", "analysis_lipid_feat") })
    shiny::observeEvent(input$go_hm,        { shinydashboard::updateTabItems(session, "tabs", "analysis_heatmap") })
    shiny::observeEvent(input$go_cor,       { shinydashboard::updateTabItems(session, "tabs", "analysis_cor") })
    shiny::observeEvent(input$go_volcano,   { shinydashboard::updateTabItems(session, "tabs", "analysis_volcano") })
    shiny::observeEvent(input$go_enrich,    { shinydashboard::updateTabItems(session, "tabs", "analysis_enrich") })
    shiny::observeEvent(input$go_decoupled, { shinydashboard::updateTabItems(session, "tabs", "analysis_decoupled") })
    shiny::observeEvent(input$go_net,       { shinydashboard::updateTabItems(session, "tabs", "analysis_network") })
    shiny::observeEvent(input$go_util_pathway,  { shinydashboard::updateTabItems(session, "tabs", "utility_pathway") })
    shiny::observeEvent(input$go_util_ontology, { shinydashboard::updateTabItems(session, "tabs", "utility_ontology") })

    # ---- jump to ontology builder from Upload module ---------------------
    .bind_jump <- function(full_input_id) {
      shiny::observeEvent(input[[full_input_id]], {
        shinydashboard::updateTabItems(session, "tabs", "utility_ontology")
      }, ignoreInit = TRUE)
    }
    .bind_jump("u-go_ontology_builder")
    .bind_jump("u-go_util_ontology")
    .bind_jump("u-go_ontology")
    .bind_jump("u-go_to_ontology")

    # ---- Shared state for API/Network integration ------------------------
    api_state <- new.env(parent = emptyenv())
    api_state$se   <- NULL
    api_state$live <- list(
      plot_type   = "violin",
      agg_fun     = "sum",
      lipid_class = NULL,
      molecule_id = NULL
    )
    api_state$adv  <- NULL

    shiny::observeEvent(se_lipid_for_plot(), {
      api_state$se <- se_lipid_for_plot()
    }, ignoreInit = FALSE)

    # ---- Feature module = Advanced Settings hub --------------------------
    feat <- maybe_server(
      "mod_feature_generic_server",
      "feat",
      se_lipid = se_lipid_for_plot,
      se_tx    = se_tx_for_plot,
      on_live_change = function(live) {
        api_state$live <- utils::modifyList(api_state$live, live)
      },
      on_adv_commit = function(adv) {
        api_state$adv <- adv
      }
    )

    settings_reactive <- NULL
    if (!is.null(feat) && is.list(feat) && is.function(feat$adv_reactive)) {
      settings_reactive <- feat$adv_reactive
    } else {
      settings_reactive <- shiny::reactive(list())
    }

    # ---- Per-panel Advanced buttons --------------------------------------
    adv_btn_ids <- c(
      "open_adv_pca",
      "open_adv_oplsda",
      "open_adv_feat",
      "open_adv_hm",
      "open_adv_cor",
      "open_adv_volcano",
      "open_adv_enrich",
      "open_adv_decoupled",
      "open_adv_net"
    )
    if (!is.null(feat) && is.list(feat) && is.function(feat$open_adv)) {
      lapply(adv_btn_ids, function(id2) {
        shiny::observeEvent(input[[id2]], {
          feat$open_adv()
        }, ignoreInit = TRUE)
      })
    }

    # ---- PCA module ------------------------------------------------------
    maybe_server(
      "mod_pca_generic_server",
      "pca",
      se_lipid     = se_lipid_for_plot,
      se_tx        = se_tx_for_plot,
      assay        = "abundance",
      adv_reactive = settings_reactive
    )

    # ---- OPLS-DA module --------------------------------------------------
    maybe_server(
      "mod_oplsda_generic_server",
      "opls",
      se_lipid     = se_lipid_for_plot,
      se_tx        = se_tx_for_plot,
      assay        = "abundance",
      adv_reactive = settings_reactive
    )

    # ---- Heatmap module --------------------------------------------------
    maybe_server(
      "mod_heatmap_server",
      "hm",
      se_lipid     = se_lipid_for_plot,
      adv_reactive = settings_reactive
    )

    # ---- Correlation module ---------------------------------------------
    maybe_server(
      "mod_plot_cor_server",
      "cor",
      se_lipid = se_lipid_for_plot,
      se_tx    = se_tx_for_plot,
      adv_reactive = settings_reactive
    )

    # ---- Volcano module --------------------------------------------------
    maybe_server(
      "mod_volcano_server",
      "volc",
      se_lipid = se_lipid_for_plot,
      se_tx    = se_tx_for_plot,
      default_group_col = "class"
    )

    # ---- Enrichment module ----------------------------------------------
    maybe_server(
      "mod_lipid_enrich_server",
      "enrich",
      se_lipid        = se_lipid_for_plot,
      adv_reactive    = settings_reactive,
      rules_yaml_path = rules_yaml_path2
    )

    # ---- Decoupled chains module (NEW) ----------------------------------
    maybe_server(
      "mod_decoupled_chains_server",
      "dc",
      se_lipid     = se_lipid_for_plot,
      adv_reactive = settings_reactive
    )

    # ---- Pathway analysis (Network) module -------------------------------
    maybe_server(
      "mod_cyto_server",
      "net",
      se_lipid_reactive = se_lipid_for_plot,
      se_tx_reactive    = se_tx_for_plot,
      settings_reactive = settings_reactive,
      default_layout    = "cose"
    )

    # ---- Utility: Ontology builder module --------------------------------
    maybe_server("mod_utility_ontology_builder_server", "util_ont")

    # ---- Utility: Pathway editor module ---------------------------------
    maybe_server(
      "mod_utility_pathway_editor_server",
      "util_path",
      elements = NULL,
      export_filename = "pathway.cyjs"
    )

    # ---- Inline API (Plumber): static mount for SVGs ----------------------
    if (isTRUE(api_enable) && exists("start_inline_api2_static", mode = "function")) {
      static_dir_raw <- api_static_dir %||% file.path(tempdir(), "cyimg_cache")
      dir.create(static_dir_raw, recursive = TRUE, showWarnings = FALSE)
      svg_dir <- normalizePath(static_dir_raw, winslash = "/", mustWork = FALSE)

      img_base_url <- sprintf("http://%s:%d/static", api_public_host, api_port)

      api_handle <- NULL
      api_handle <- try(
        start_inline_api2_static(
          se_provider    = function() api_state$se,
          state_provider = function() list(live = api_state$live, adv = api_state$adv),
          static_dir     = svg_dir,
          host           = api_host,
          port           = api_port,
          verbose        = isTRUE(verbose_api),
          enable_plot_endpoints = FALSE
        ),
        silent = TRUE
      )

      try(shiny::updateTextInput(session, "net-svg_dir",      value = svg_dir), silent = TRUE)
      try(shiny::updateTextInput(session, "net-img_base_url", value = img_base_url), silent = TRUE)

      shiny::onStop(function() {
        try({
          if (!inherits(api_handle, "try-error") &&
              !is.null(api_handle) &&
              is.function(api_handle$stop)) {
            api_handle$stop()
          }
        }, silent = TRUE)
        try(unlink(svg_dir, recursive = TRUE, force = TRUE), silent = TRUE)
      })
    }
  }

  shiny::runApp(
    list(ui = ui, server = server),
    launch.browser = launch.browser,
    host = "0.0.0.0",
    port = 3838
  )
}