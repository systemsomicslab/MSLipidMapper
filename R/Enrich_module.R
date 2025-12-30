# R/mod_lipid_enrich.R -------------------------------------------------------
# Shiny module: Lipid enrichment (ORA) using clusterProfiler::enricher()
#
# YAML-driven acyl-chain extraction (read from local YAML file; no UI for YAML):
# - Always read rules from local YAML file (rules_yaml_path argument)
# - "sum-only" notation (e.g., "GM3 34:1;2O") can be ignored per-rule: ignore_sum_only: true
# - Cer / sphingolipids: drop sphingoid base (first chain) ONLY when >=2 chains (exclude_first_chain)
# - Steryl ester: drop sterol backbone (first token) ONLY when >=2 chains (exclude_first_chain)
# - BA / ST / NGcGM3 / SPB: no_chain: true
# - Supports suffix FA in parentheses: (FA 16:0)
# - Supports tail FA WITHOUT parentheses: "...;FA 20:4" (detect_fa_tail: true)  <-- for ASG
# - Keep isotopic suffix like (d7) as-is
# - Preserve "(2OH)" parentheses
# - Stable list-column generation (purrr::map) so mutate never crashes
#
# Download:
# - plot PDF
# - significant terms CSV
# - background table with generated acyl_chains CSV
#
# Depends: shiny, shinydashboard, dplyr, tidyr, stringr, readr, yaml, purrr,
#          clusterProfiler, ggplot2, htmltools

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(yaml)
  library(purrr)
  library(clusterProfiler)
  library(ggplot2)
  library(htmltools)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ------------------------------------------------------------
# Collapsible section helper
# ------------------------------------------------------------
.cy_details <- function(title, ..., open = FALSE) {
  shiny::tags$details(
    open = if (isTRUE(open)) NA else NULL,
    class = "cy-acc",
    shiny::tags$summary(shiny::tags$span(title)),
    shiny::div(class = "cy-acc-body", ...)
  )
}

# ------------------------------------------------------------
# Utils
# ------------------------------------------------------------
.norm_chr <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  x
}

.safe_num01 <- function(x, default = 0.05) {
  xn <- suppressWarnings(as.numeric(x))
  if (length(xn) == 0 || is.na(xn) || !is.finite(xn)) return(default)
  if (xn < 0) xn <- 0
  if (xn > 1) xn <- 1
  xn
}

.safe_int <- function(x, default = 15L, min = 2L, max = 100L) {
  xn <- suppressWarnings(as.numeric(x))
  if (length(xn) == 0 || is.na(xn) || !is.finite(xn)) return(as.integer(default))
  xn <- floor(xn)
  if (xn < min) xn <- min
  if (xn > max) xn <- max
  as.integer(xn)
}

.safe_num_range <- function(x, default = 1.0, min = 0.5, max = 4.0) {
  xn <- suppressWarnings(as.numeric(x))
  if (length(xn) == 0 || is.na(xn) || !is.finite(xn)) return(default)
  if (xn < min) xn <- min
  if (xn > max) xn <- max
  xn
}

.term_type <- function(term) {
  dplyr::case_when(
    str_starts(term, "Ontology:") ~ "Ontology",
    str_starts(term, "Chain:") ~ "Chain",
    TRUE ~ "Other"
  )
}

.read_table_auto <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("tsv", "txt")) {
    readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
  } else {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  }
}

.collapse_ids_html <- function(id_str, max_chars = 120) {
  id_str <- as.character(id_str %||% "")
  id_str <- gsub("\\s+", " ", id_str)
  if (!nzchar(id_str)) return("")
  preview <- if (nchar(id_str) <= max_chars) id_str else paste0(substr(id_str, 1, max_chars), " ...")
  paste0(
    "<details style='max-width:100%;'>",
    "<summary style='cursor:pointer; color:#2c6fbb;'>", htmlEscape(preview), "</summary>",
    "<div style='white-space:normal; word-break:break-word; padding-top:6px;'>",
    htmlEscape(id_str),
    "</div></details>"
  )
}

.split_geneid <- function(x) {
  x <- as.character(x %||% "")
  x <- str_trim(x)
  if (!nzchar(x)) return(character(0))
  out <- unlist(str_split(x, fixed("/")))
  out <- str_trim(out)
  out <- out[nzchar(out)]
  unique(out)
}

# ------------------------------------------------------------
# YAML rules
# ------------------------------------------------------------
load_lipid_rules <- function(yaml_file) {
  if (is.null(yaml_file) || !nzchar(yaml_file)) {
    stop("rules_yaml_path is NULL/empty. Provide a path to acyl_rules.yaml.", call. = FALSE)
  }
  if (!file.exists(yaml_file)) {
    stop("YAML rules file not found: ", yaml_file, call. = FALSE)
  }
  y <- yaml::read_yaml(yaml_file)
  if (is.null(y$patterns) || !length(y$patterns)) {
    stop("YAML rules has no 'patterns': ", yaml_file, call. = FALSE)
  }
  y
}

.score_rule <- function(rule) {
  pri <- suppressWarnings(as.numeric(rule$priority %||% 0))
  pri <- ifelse(is.na(pri), 0, pri)
  rx_len <- nchar(rule$class_regex %||% "")
  bonus <- 0
  bonus <- bonus + ifelse(isTRUE(rule$no_chain), 80, 0)
  bonus <- bonus + ifelse(isTRUE(rule$exclude_first_chain), 25, 0)
  bonus <- bonus + ifelse(isTRUE(rule$detect_fa_suffix), 10, 0)
  bonus <- bonus + ifelse(isTRUE(rule$detect_fa_tail), 12, 0)
  bonus <- bonus + ifelse(isTRUE(rule$drop_zero_chains), 5, 0)
  bonus <- bonus + ifelse(isTRUE(rule$ignore_sum_only), 20, 0)
  pri * 1000 + rx_len + bonus
}

select_rule <- function(lipid_class, rules) {
  pats <- rules$patterns
  if (is.null(pats)) return(NULL)
  hits <- list()
  for (r in pats) {
    rx <- r$class_regex %||% ""
    if (nzchar(rx) && str_detect(lipid_class, rx)) hits[[length(hits) + 1]] <- r
  }
  if (!length(hits)) return(NULL)
  scores <- vapply(hits, .score_rule, numeric(1))
  hits[[which.max(scores)]]
}

# ------------------------------------------------------------
# Split helper
# ------------------------------------------------------------
.split_by_rule <- function(x, split, split_mode = NULL) {
  if (is.null(split) || !nzchar(split)) return(x)
  
  if (identical(split, "auto")) {
    if (str_detect(x, fixed("/"))) return(unlist(str_split(x, fixed("/"))))
    if (str_detect(x, fixed("_"))) return(unlist(str_split(x, fixed("_"))))
    # also allow whitespace for "CAR 8:0" / "CE 24:1(d7)" / "ASG ... FA 20:4"
    if (str_detect(x, "\\s+")) return(unlist(str_split(x, "\\s+")))
    return(x)
  }
  
  if (!is.null(split_mode)) {
    m <- tolower(as.character(split_mode))
    if (m %in% c("literal", "fixed")) return(unlist(str_split(x, fixed(split))))
    if (m %in% c("regex", "re"))      return(unlist(str_split(x, split)))
  }
  
  is_regex <- grepl("[\\[\\]\\(\\)\\?\\+\\*\\\\\\|\\^\\$\\{\\}]", split)
  if (is_regex) unlist(str_split(x, split)) else unlist(str_split(x, fixed(split)))
}

# ------------------------------------------------------------
# Best alt in "total|molecular" notations
# - default: choose the part with most chain tokens
# - optional: prefer "molecular" notation (contains "_" or "/") if rule$prefer_molecular_notation
# ------------------------------------------------------------
.count_chain_tokens <- function(x) {
  if (is.na(x) || !nzchar(x)) return(0L)
  pat <- "(?:^|[^0-9])(?:FA\\s*)?(?:O-|P-)?\\d+:\\d+"
  length(str_extract_all(x, pat, simplify = FALSE)[[1]])
}

.pick_best_alt <- function(x, prefer_molecular_notation = FALSE) {
  if (is.na(x) || !nzchar(x)) return(x)
  if (!str_detect(x, fixed("|"))) return(x)
  
  parts <- str_split(x, fixed("|"), simplify = TRUE) |> as.character() |> str_trim()
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(x)
  
  base_score <- vapply(parts, .count_chain_tokens, integer(1))
  
  if (isTRUE(prefer_molecular_notation)) {
    # +2 if contains "_" or "/" (more likely molecular/sn notation)
    bonus <- ifelse(str_detect(parts, "[/_]"), 2L, 0L)
    base_score <- base_score * 10L + bonus
  }
  
  parts[which.max(base_score)]
}

# ------------------------------------------------------------
# Sum-only detector (ignore_sum_only)
# e.g., "GM3 34:1;2O" : has one token but no delimiter "_" or "/" or "(FA ...)" and no extra FA tokens.
# We treat "sum-only" as: contains exactly one chain-like token and lacks evidence of multiple chains.
# ------------------------------------------------------------
.is_sum_only_like <- function(core) {
  if (is.na(core) || !nzchar(core)) return(TRUE)
  has_delim <- str_detect(core, "[/_]")
  has_fa_paren <- str_detect(core, "\\((?:FA\\s*)?(?:O-|P-)?\\d+:\\d+")
  has_fa_tail  <- str_detect(core, "(?:^|[;\\s])FA\\s*(?:O-|P-)?\\d+:\\d+")
  n_tok <- .count_chain_tokens(core)
  (!has_delim) && (!has_fa_paren) && (!has_fa_tail) && (n_tok <= 1L)
}

# ------------------------------------------------------------
# Normalize chain token (keep (2OH) parentheses intact)
# ------------------------------------------------------------
.norm_chain_token <- function(tok) {
  tok <- as.character(tok) %||% ""
  tok <- str_trim(tok)
  if (!nzchar(tok)) return(NA_character_)
  
  # remove outer parentheses only when fully wrapped
  if (str_starts(tok, fixed("(")) && str_ends(tok, fixed(")"))) {
    tok <- str_sub(tok, 2, -2) |> str_trim()
  }
  
  tok <- str_replace(tok, "^FA\\s*", "") |> str_trim()
  tok <- str_remove(tok, "^(O-|P-)")
  
  # normalize ;2O -> ;O2 (space/paren OK)
  tok <- str_replace_all(tok, ";(\\d+)O(?=\\D|$)", ";O\\1")
  
  # 18:1(2OH) -> 18:1;(2OH)
  if (str_detect(tok, "^\\d+:\\d+\\(\\d+OH\\)$")) {
    tok <- str_replace(tok, "\\(\\d+OH\\)$", function(m) paste0(";", m))
  }
  
  # drop 0:0
  if (identical(tok, "0:0")) return(NA_character_)
  if (str_detect(tok, "^0:0(;|$)")) return(NA_character_)
  
  tok
}

# ------------------------------------------------------------
# Extract acyl chains from one lipid name (ALWAYS character vec)
# ------------------------------------------------------------
extract_acyl_chains_from_name <- function(lipid_name, rules) {
  nm <- as.character(lipid_name %||% "")
  nm <- str_trim(nm)
  if (!nzchar(nm)) return(character(0))
  
  # class token (allow + - ; _)
  class_raw <- str_extract(nm, "^[A-Za-z0-9\\-\\+;_]+") %||% ""
  class_raw <- str_trim(class_raw)
  
  rule <- select_rule(class_raw, rules)
  
  # if no rule, still try minimal extraction
  if (is.null(rule)) {
    rule <- list(
      no_chain = FALSE,
      split = "auto",
      split_mode = NULL,
      detect_fa_suffix = TRUE,
      detect_fa_tail = FALSE,
      exclude_first_chain = FALSE,
      drop_zero_chains = FALSE,
      ignore_sum_only = FALSE,
      prefer_molecular_notation = FALSE
    )
  }
  
  if (isTRUE(rule$no_chain %||% FALSE)) return(character(0))
  
  # choose best alt for "A|B" notations
  nm2 <- .pick_best_alt(nm, prefer_molecular_notation = isTRUE(rule$prefer_molecular_notation %||% FALSE))
  
  core <- nm2
  
  # remove class prefix:
  # support "PC(16:0/18:1)" AND "PC 16:0/18:1"
  if (nzchar(class_raw) && str_detect(core, paste0("^", class_raw, "\\("))) {
    core <- str_remove(core, paste0("^", class_raw, "\\("))
    core <- str_remove(core, "\\)$")
  } else if (nzchar(class_raw)) {
    core <- str_remove(core, paste0("^", class_raw, "\\s+"))
  }
  core <- str_trim(core)
  
  # ignore sum-only notation if requested
  if (isTRUE(rule$ignore_sum_only %||% FALSE) && .is_sum_only_like(core)) {
    return(character(0))
  }
  
  # suffix extraction in parentheses: (FA 16:0) etc.
  suffix_vec <- character(0)
  if (isTRUE(rule$detect_fa_suffix %||% FALSE)) {
    suffix_pat <- "\\((?:FA\\s*)?(?:O-|P-)?\\d+:\\d+(?:(?:;O\\d*)|(?:;\\d+O))?(?:;?\\(\\d+OH\\))?(?:\\(d\\d+\\))?\\)"
    suf_raw <- str_extract_all(core, suffix_pat, simplify = FALSE)[[1]]
    if (length(suf_raw) > 0) {
      suffix_vec <- gsub("^\\(|\\)$", "", suf_raw)
      suffix_vec <- str_replace(suffix_vec, "^FA\\s*", "") |> str_trim()
      suffix_vec <- vapply(suffix_vec, .norm_chain_token, character(1), USE.NAMES = FALSE)
      suffix_vec <- suffix_vec[!is.na(suffix_vec) & nzchar(suffix_vec)]
    }
    core <- str_remove_all(core, suffix_pat)
    core <- str_trim(core)
  }
  
  # ---- NEW: tail extraction like "...;FA 20:4" (no parentheses) ----
  if (isTRUE(rule$detect_fa_tail %||% FALSE)) {
    tail_pat <- "(?:^|[;\\s])FA\\s*(?:O-|P-)?\\d+:\\d+(?:(?:;O\\d*)|(?:;\\d+O))?(?:;?\\(\\d+OH\\))?(?:\\(d\\d+\\))?"
    tail_raw <- str_extract_all(core, tail_pat, simplify = FALSE)[[1]]
    
    if (length(tail_raw) > 0) {
      tail_vec <- str_trim(tail_raw)
      tail_vec <- str_replace(tail_vec, "^(?:;|\\s)+", "")
      tail_vec <- str_replace(tail_vec, "^FA\\s*", "")
      tail_vec <- vapply(tail_vec, .norm_chain_token, character(1), USE.NAMES = FALSE)
      tail_vec <- tail_vec[!is.na(tail_vec) & nzchar(tail_vec)]
      suffix_vec <- c(suffix_vec, tail_vec)
    }
    
    core <- str_remove_all(core, tail_pat)
    core <- str_trim(core)
  }
  
  # split main chains
  split <- rule$split %||% "auto"
  split_mode <- rule$split_mode %||% NULL
  parts <- .split_by_rule(core, split, split_mode)
  
  parts <- str_trim(parts)
  parts <- parts[nzchar(parts)]
  
  # keep only chain-like tokens
  is_chain_like <- function(x) str_detect(x, "^(?:FA\\s*)?(?:O-|P-)?\\d+:\\d+")
  parts <- parts[is_chain_like(parts)]
  parts <- vapply(parts, .norm_chain_token, character(1), USE.NAMES = FALSE)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  
  if (isTRUE(rule$drop_zero_chains %||% FALSE)) {
    parts <- parts[!str_detect(parts, "^0:0(?:$|[^0-9])")]
  }
  
  chains <- c(parts, suffix_vec)
  chains <- chains[!is.na(chains) & nzchar(chains)]
  if (!length(chains)) return(character(0))
  
  # drop first chain ONLY when >=2
  if (isTRUE(rule$exclude_first_chain %||% FALSE) && length(chains) >= 2) {
    chains <- chains[-1]
  }
  
  unique(chains)
}

# ------------------------------------------------------------
# Stable list-column add (never breaks mutate)
# ------------------------------------------------------------
add_acyl_chains_to_table <- function(df, lipid_col, rules) {
  stopifnot(is.data.frame(df))
  stopifnot(lipid_col %in% colnames(df))
  
  df %>%
    mutate(
      .lipid_tmp__ = as.character(.data[[lipid_col]]),
      acyl_chains  = purrr::map(.lipid_tmp__, ~{
        out <- try(extract_acyl_chains_from_name(.x, rules = rules), silent = TRUE)
        if (inherits(out, "try-error") || is.null(out)) character(0) else as.character(out)
      })
    ) %>%
    select(-.lipid_tmp__)
}

# ------------------------------------------------------------
# TERM2GENE
# ------------------------------------------------------------
build_term2gene_from_bg <- function(df_bg, lipid_col, ontology_col, include_chain = TRUE) {
  df_bg <- df_bg %>%
    mutate(
      lipid_id = as.character(.data[[lipid_col]]),
      Ontology = as.character(.data[[ontology_col]])
    ) %>%
    filter(!is.na(lipid_id), nzchar(lipid_id)) %>%
    mutate(Ontology = ifelse(is.na(Ontology) | !nzchar(Ontology), NA_character_, Ontology))
  
  df_onto <- df_bg %>%
    filter(!is.na(Ontology), nzchar(Ontology)) %>%
    transmute(term = paste0("Ontology:", Ontology), gene = lipid_id) %>%
    distinct(term, gene)
  
  term2gene <- df_onto
  
  if (isTRUE(include_chain)) {
    df_chain <- df_bg %>%
      select(lipid_id, acyl_chains) %>%
      tidyr::unnest(acyl_chains, keep_empty = FALSE) %>%
      filter(!is.na(acyl_chains), nzchar(acyl_chains)) %>%
      transmute(term = paste0("Chain:", acyl_chains), gene = lipid_id) %>%
      distinct(term, gene)
    
    term2gene <- bind_rows(term2gene, df_chain)
  }
  
  distinct(term2gene, term, gene)
}

# ------------------------------------------------------------
# ORA runner
# ------------------------------------------------------------
run_enricher <- function(sig_ids, term2gene, universe_ids, p_adjust_method = "BH", padj_cut = 0.05) {
  universe_ids <- .norm_chr(universe_ids)
  sig_ids      <- .norm_chr(sig_ids)
  sig_in <- intersect(sig_ids, universe_ids)
  
  if (length(sig_in) == 0) {
    return(list(obj = NULL, table_sig = data.frame(), sig_in = sig_in))
  }
  
  obj <- clusterProfiler::enricher(
    gene          = sig_in,
    universe      = universe_ids,
    TERM2GENE     = term2gene,
    pvalueCutoff  = 1,
    qvalueCutoff  = 1,
    pAdjustMethod = p_adjust_method
  )
  
  if (is.null(obj) || nrow(as.data.frame(obj)) == 0) {
    return(list(obj = obj, table_sig = data.frame(), sig_in = sig_in))
  }
  
  tbl <- as.data.frame(obj)
  tbl$type <- .term_type(tbl$ID)
  tbl <- tbl[order(tbl$p.adjust, tbl$pvalue), , drop = FALSE]
  
  padj_cut <- .safe_num01(padj_cut, default = 0.05)
  
  tbl_sig <- tbl %>%
    filter(p.adjust <= padj_cut) %>%
    arrange(p.adjust, pvalue)
  
  list(obj = obj, table_sig = tbl_sig, sig_in = sig_in)
}

make_lipid_table <- function(tbl_sig) {
  if (!is.data.frame(tbl_sig) || nrow(tbl_sig) == 0) return(data.frame())
  tbl_sig %>%
    mutate(
      LipidRatio   = GeneRatio,
      BgLipidRatio = BgRatio,
      lipidID_raw  = geneID
    ) %>%
    select(-GeneRatio, -BgRatio, -geneID)
}

# ------------------------------------------------------------
# UpSet prep
# ------------------------------------------------------------
make_upset_from_tbl <- function(tbl_sig, max_terms = 15L) {
  if (!is.data.frame(tbl_sig) || nrow(tbl_sig) == 0) return(NULL)
  
  max_terms <- .safe_int(max_terms, default = 15L, min = 2L, max = 100L)
  
  tbl_use <- tbl_sig %>%
    arrange(p.adjust, pvalue) %>%
    head(max_terms)
  
  sets_list <- lapply(seq_len(nrow(tbl_use)), function(i) .split_geneid(tbl_use$geneID[i]))
  names(sets_list) <- tbl_use$ID
  
  sets_list <- sets_list[vapply(sets_list, length, integer(1)) > 0]
  if (length(sets_list) < 2) return(NULL)
  if (!requireNamespace("UpSetR", quietly = TRUE)) return("NO_UPSETR")
  
  df <- UpSetR::fromList(sets_list)
  actual_cols <- colnames(df)
  
  orig <- names(sets_list)
  cols_vec <- ifelse(.term_type(orig) == "Ontology", "seagreen3",
                     ifelse(.term_type(orig) == "Chain", "orange2", "grey70"))
  
  cols_map_raw  <- cols_vec; names(cols_map_raw)  <- orig
  cols_map_made <- cols_vec; names(cols_map_made) <- make.names(orig, unique = TRUE)
  cols_map <- c(cols_map_raw, cols_map_made)
  
  set_cols <- unname(cols_map[actual_cols])
  set_cols[is.na(set_cols)] <- "grey60"
  
  list(input = df, sets = actual_cols, cols = set_cols)
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
#' @export
mod_lipid_enrich_ui <- function(id, title = "Lipid enrichment (file input)") {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      details.cy-acc{
        border:1px solid #e5e5e5;
        border-radius:6px;
        padding:6px 8px;
        margin-bottom:8px;
        background:#fff;
      }
      details.cy-acc > summary{
        cursor:pointer;
        font-weight:600;
        outline:none;
      }
      details.cy-acc > summary::-webkit-details-marker { display:none; }
      .cy-acc-body{ margin-top:8px; }
      .cy-acc-body .form-group{ margin-bottom:10px; }
      .cy-acc-body hr{ margin:10px 0; }
    ")),
    
    fluidRow(
      column(
        width = 3,
        shinydashboard::box(
          title = "Settings",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          .cy_details(
            "Input files",
            fileInput(ns("bg_file"), "Background lipids (universe) CSV/TSV", accept = c(".csv", ".tsv", ".txt")),
            fileInput(ns("sig_file"), "Significant lipids CSV/TSV", accept = c(".csv", ".tsv", ".txt")),
            open = TRUE
          ),
          
          .cy_details(
            "Variable column",
            uiOutput(ns("ui_bg_lipid_col")),
            uiOutput(ns("ui_bg_onto_col")),
            uiOutput(ns("ui_sig_lipid_col")),
            open = FALSE
          ),
          
          .cy_details(
            "Metadata generation",
            checkboxInput(ns("use_chain_terms"), "Generate acyl_chains and include Chain terms", value = TRUE),
            checkboxInput(ns("drop_unknown_onto"), "Drop rows with missing Ontology (background)", value = TRUE),
            open = FALSE
          ),
          
          .cy_details(
            "Enrichment settings",
            selectInput(
              ns("padj_method"),
              "p-adjust method",
              choices = c("BH", "bonferroni", "holm", "hochberg", "hommel", "BY", "fdr", "none"),
              selected = "BH"
            ),
            numericInput(ns("padj_cut"), "adjusted p cutoff", value = 0.05, min = 0, max = 1, step = 0.001),
            open = FALSE
          ),
          
          .cy_details(
            "Plot type",
            radioButtons(
              ns("plot_type"),
              label = NULL,
              choices = c("Barplot", "UpSet"),
              selected = "Barplot",
              inline = TRUE
            ),
            open = FALSE
          ),
          
          .cy_details(
            "Font size",
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Barplot'", ns("plot_type")),
              numericInput(ns("bar_font_size"), "Barplot font size (ggplot)", value = 12, min = 6, max = 30, step = 1)
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'UpSet'", ns("plot_type")),
              numericInput(ns("upset_text_scale"), "UpSet text scale", value = 2.0, min = 0.5, max = 4.0, step = 0.1),
              numericInput(ns("upset_point_size"), "UpSet point size", value = 3.0, min = 1.0, max = 8.0, step = 0.5)
            ),
            open = FALSE
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'UpSet'", ns("plot_type")),
            .cy_details(
              "UpSet options",
              numericInput(ns("upset_terms_n"), "UpSet: max terms", value = 15, min = 2, max = 100, step = 1),
              tags$div(style = "margin-top:8px; color:#666;",
                       HTML("<b>Set color:</b> Ontology = green, Chain = orange")),
              tags$div(style = "margin-top:4px; color:#666;",
                       HTML("<b>Intersection bars:</b> blue")),
              open = FALSE
            )
          ),
          
          tags$hr(),
          uiOutput(ns("summary_box")),
          
          tags$hr(),
          actionButton(ns("run"), "Run enrichment", icon = icon("play"), width = "100%", class = "btn-primary"),
          
          tags$hr(),
          downloadButton(ns("download_plot"), "Download plot (PDF)", width = "100%"),
          downloadButton(ns("download_enrich_sig"), "Download significant terms (CSV)", width = "100%"),
          downloadButton(ns("download_bg_with_acyl"), "Download background+acyl_chains (CSV)", width = "100%")
        )
      ),
      
      column(
        width = 9,
        shinydashboard::box(
          title = title,
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              "Metadata",
              tags$p(style = "margin:0 0 8px 0; color:#666;",
                     "Preview (first 200 rows)."),
              tableOutput(ns("meta_table"))
            ),
            tabPanel(
              "Plot",
              plotOutput(ns("plot_sig"), height = "560px")
            ),
            tabPanel(
              "Table",
              uiOutput(ns("table_sig_ui"))
            )
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
#' @export
mod_lipid_enrich_server <- function(id, rules_yaml_path, adv_reactive = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    
    if (!is.null(adv_reactive)) {
      shiny::observe({ adv_reactive() }, priority = -1000)
    }
    
    # Load YAML rules ONCE (no UI input)
    rules <- load_lipid_rules(rules_yaml_path)
    
    bg_tbl_raw <- reactive({
      req(input$bg_file)
      .read_table_auto(input$bg_file$datapath)
    })
    
    sig_tbl_raw <- reactive({
      req(input$sig_file)
      .read_table_auto(input$sig_file$datapath)
    })
    
    bg_cols <- reactive({
      if (is.null(input$bg_file)) return(character(0))
      colnames(bg_tbl_raw())
    })
    
    sig_cols <- reactive({
      if (is.null(input$sig_file)) return(character(0))
      colnames(sig_tbl_raw())
    })
    
    output$ui_bg_lipid_col <- renderUI({
      cols <- bg_cols()
      selectInput(session$ns("bg_lipid_col"), "Background: lipid name/ID column",
                  choices = cols,
                  selected = {
                    if ("lipid_id" %in% cols) "lipid_id"
                    else if ("Metabolite.name" %in% cols) "Metabolite.name"
                    else if ("feature" %in% cols) "feature"
                    else if ("name" %in% cols) "name"
                    else cols[1] %||% ""
                  }
      )
    })
    
    output$ui_bg_onto_col <- renderUI({
      cols <- bg_cols()
      selectInput(session$ns("bg_onto_col"), "Background: Ontology column",
                  choices = cols,
                  selected = {
                    if ("Ontology" %in% cols) "Ontology"
                    else if ("ontology" %in% cols) "ontology"
                    else if ("subclass" %in% cols) "subclass"
                    else if ("Class" %in% cols) "Class"
                    else cols[1] %||% ""
                  }
      )
    })
    
    output$ui_sig_lipid_col <- renderUI({
      cols <- sig_cols()
      selectInput(session$ns("sig_lipid_col"), "Significant: lipid name/ID column",
                  choices = cols,
                  selected = {
                    if ("lipid_id" %in% cols) "lipid_id"
                    else if ("Metabolite.name" %in% cols) "Metabolite.name"
                    else if ("feature" %in% cols) "feature"
                    else if ("name" %in% cols) "name"
                    else cols[1] %||% ""
                  }
      )
    })
    
    # Background with generated metadata
    bg_tbl <- reactive({
      df <- bg_tbl_raw()
      lipid_col <- input$bg_lipid_col
      onto_col  <- input$bg_onto_col
      
      shiny::validate(
        shiny::need(!is.null(lipid_col) && nzchar(lipid_col), "Select background lipid column."),
        shiny::need(!is.null(onto_col) && nzchar(onto_col), "Select background Ontology column."),
        shiny::need(lipid_col %in% colnames(df), paste0("Background missing column: ", lipid_col)),
        shiny::need(onto_col %in% colnames(df), paste0("Background missing column: ", onto_col))
      )
      
      df2 <- add_acyl_chains_to_table(df, lipid_col = lipid_col, rules = rules)
      
      if (isTRUE(input$drop_unknown_onto)) {
        df2 <- df2 %>%
          mutate(.onto__ = as.character(.data[[onto_col]])) %>%
          filter(!is.na(.onto__), nzchar(.onto__)) %>%
          select(-.onto__)
      }
      
      df2
    })
    
    universe_ids <- reactive({
      df <- bg_tbl()
      lipid_col <- input$bg_lipid_col
      .norm_chr(df[[lipid_col]])
    })
    
    sig_ids <- reactive({
      df <- sig_tbl_raw()
      lipid_col <- input$sig_lipid_col
      
      shiny::validate(
        shiny::need(!is.null(lipid_col) && nzchar(lipid_col), "Select significant lipid column."),
        shiny::need(lipid_col %in% colnames(df), paste0("Significant missing column: ", lipid_col))
      )
      
      .norm_chr(df[[lipid_col]])
    })
    
    term2gene_reactive <- reactive({
      df_bg <- bg_tbl()
      lipid_col <- input$bg_lipid_col
      onto_col  <- input$bg_onto_col
      
      build_term2gene_from_bg(
        df_bg = df_bg,
        lipid_col = lipid_col,
        ontology_col = onto_col,
        include_chain = isTRUE(input$use_chain_terms)
      )
    })
    
    output$summary_box <- renderUI({
      if (is.null(input$bg_file) || is.null(input$sig_file)) {
        return(tags$p(style = "margin:0; color:#666;", "Upload background and significant files."))
      }
      
      if (is.null(input$bg_lipid_col) || !nzchar(input$bg_lipid_col) ||
          is.null(input$sig_lipid_col) || !nzchar(input$sig_lipid_col)) {
        return(tags$p(style = "margin:0; color:#666;", "Select column mappings."))
      }
      
      u <- try(universe_ids(), silent = TRUE)
      s <- try(sig_ids(), silent = TRUE)
      if (inherits(u, "try-error") || inherits(s, "try-error")) {
        return(tags$p(style = "margin:0; color:#666;", "Waiting for valid inputs..."))
      }
      
      in_u <- intersect(s, u)
      not_in_u <- setdiff(s, u)
      
      tagList(
        tags$p(
          style = "margin:0;",
          sprintf("Universe: %d | Significant: %d | In universe: %d",
                  length(u), length(s), length(in_u))
        ),
        tags$p(style = "margin:6px 0 0 0; color:#666;",
               paste0("Rules YAML: ", rules_yaml_path)),
        if (length(not_in_u) > 0) {
          tags$p(
            style = "margin:6px 0 0 0; color:#b00;",
            sprintf("Warning: %d significant IDs are not in universe (ID mismatch).", length(not_in_u))
          )
        }
      )
    })
    
    output$meta_table <- renderTable({
      df <- bg_tbl()
      lipid_col <- input$bg_lipid_col
      onto_col  <- input$bg_onto_col
      
      show_df <- df %>%
        mutate(acyl_chains = vapply(acyl_chains, function(v) paste(v, collapse = " | "), character(1))) %>%
        select(all_of(unique(c(lipid_col, onto_col, "acyl_chains"))))
      
      head(show_df, 200)
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
    enrich_result <- eventReactive(input$run, {
      u <- universe_ids()
      s <- sig_ids()
      t2g <- term2gene_reactive()
      
      shiny::validate(
        shiny::need(length(u) > 0, "Universe is empty."),
        shiny::need(length(s) > 0, "Significant list is empty."),
        shiny::need(nrow(t2g) > 0, "TERM2GENE is empty (check Ontology / Chain toggle).")
      )
      
      run_enricher(
        sig_ids = s,
        term2gene = t2g,
        universe_ids = u,
        p_adjust_method = input$padj_method %||% "BH",
        padj_cut = input$padj_cut %||% 0.05
      )
    }, ignoreInit = TRUE)
    
    .print_upset <- function(p, for_pdf = FALSE) {
      pr <- try(getS3method("print", "upset", optional = TRUE), silent = TRUE)
      has_newpage <- FALSE
      if (!inherits(pr, "try-error") && !is.null(pr)) {
        fml <- try(formals(pr), silent = TRUE)
        if (!inherits(fml, "try-error") && is.list(fml)) {
          has_newpage <- "newpage" %in% names(fml)
        }
      }
      if (has_newpage) {
        print(p, newpage = !for_pdf)
      } else {
        print(p)
      }
      invisible(NULL)
    }
    
    .draw_current_plot <- function(tbl, for_pdf = FALSE) {
      plot_type <- input$plot_type %||% "Barplot"
      
      if (identical(plot_type, "UpSet")) {
        max_terms <- .safe_int(input$upset_terms_n %||% 15, default = 15, min = 2, max = 100)
        up <- make_upset_from_tbl(tbl, max_terms = max_terms)
        
        if (is.null(up)) {
          plot.new()
          text(0.5, 0.5, "UpSet needs at least 2 terms with non-empty lipid sets.", cex = 1.2)
          return(invisible(NULL))
        }
        if (is.character(up) && up == "NO_UPSETR") {
          plot.new()
          text(0.5, 0.55, "Package 'UpSetR' is not installed.", cex = 1.2)
          text(0.5, 0.45, "Install: install.packages('UpSetR')", cex = 1.1)
          return(invisible(NULL))
        }
        
        text_scale <- .safe_num_range(input$upset_text_scale %||% 2.0, default = 2.0, min = 0.5, max = 4.0)
        point_size <- .safe_num_range(input$upset_point_size %||% 3.0, default = 3.0, min = 1.0, max = 8.0)
        ts_vec <- c(1.15, 0.95, 0.95, 0.90, 0.95, 0.90) * text_scale
        
        set_cols <- up$cols
        if (length(set_cols) != length(up$sets)) set_cols <- rep("grey60", length(up$sets))
        set_cols[is.na(set_cols)] <- "grey60"
        
        tryCatch({
          p <- UpSetR::upset(
            up$input,
            sets = up$sets,
            nsets = length(up$sets),
            keep.order = TRUE,
            order.by = "freq",
            sets.bar.color = set_cols,
            main.bar.color = "steelblue3",
            matrix.color   = "steelblue",
            text.scale     = ts_vec,
            point.size     = point_size
          )
          if (inherits(p, "upset")) .print_upset(p, for_pdf = for_pdf)
        }, error = function(e) {
          plot.new()
          text(0.5, 0.60, "UpSet failed to render.", cex = 1.2)
          text(0.5, 0.45, conditionMessage(e), cex = 0.9)
        })
        
      } else {
        base_size <- .safe_int(input$bar_font_size %||% 12, default = 12L, min = 6L, max = 30L)
        
        dfp <- tbl %>%
          mutate(log10_padj = -log10(p.adjust + 1e-300)) %>%
          arrange(p.adjust, pvalue)
        
        p <- ggplot(dfp, aes(x = reorder(ID, log10_padj), y = log10_padj, fill = log10_padj)) +
          geom_col() +
          coord_flip() +
          labs(x = NULL, y = "-log10(adjusted p)", fill = "-log10(p.adjust)", title = NULL) +
          scale_fill_gradient(low = "grey90", high = "steelblue") +
          theme_classic(base_size = base_size)
        
        print(p)
      }
      
      invisible(NULL)
    }
    
    output$plot_sig <- renderPlot({
      res <- enrich_result()
      if (is.null(res) || !is.list(res) || !is.data.frame(res$table_sig) || nrow(res$table_sig) == 0) {
        plot.new()
        text(0.5, 0.5, "Click 'Run enrichment' to compute results.", cex = 1.1)
        return(invisible(NULL))
      }
      .draw_current_plot(res$table_sig, for_pdf = FALSE)
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("lipid_enrichment_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        res <- try(enrich_result(), silent = TRUE)
        
        pdf(file, width = 12, height = 7, onefile = TRUE)
        on.exit(dev.off(), add = TRUE)
        
        if (inherits(res, "try-error") || is.null(res) || !is.list(res) ||
            !is.data.frame(res$table_sig) || nrow(res$table_sig) == 0) {
          plot.new()
          text(0.5, 0.5, "No plot (run enrichment first).", cex = 1.2)
          return()
        }
        
        .draw_current_plot(res$table_sig, for_pdf = TRUE)
      }
    )
    
    output$table_sig_ui <- renderUI({
      res <- enrich_result()
      if (is.null(res) || !is.list(res) || !is.data.frame(res$table_sig) || nrow(res$table_sig) == 0) {
        return(tags$p(style = "margin:0; color:#666;", "No significant terms at this cutoff (or run enrichment first)."))
      }
      
      tbl <- res$table_sig
      out <- make_lipid_table(tbl)
      
      disp <- out
      disp$lipidID <- vapply(disp$lipidID_raw, .collapse_ids_html, character(1))
      disp$lipidID_raw <- NULL
      
      show_cols <- intersect(
        c("ID", "Description", "type", "Count",
          "LipidRatio", "BgLipidRatio",
          "pvalue", "p.adjust", "qvalue", "lipidID"),
        colnames(disp)
      )
      
      header <- tags$tr(lapply(show_cols, function(nm) tags$th(nm)))
      rows <- lapply(seq_len(nrow(disp)), function(i) {
        tags$tr(lapply(show_cols, function(nm) {
          val <- disp[[nm]][i]
          if (nm == "lipidID") tags$td(HTML(val)) else tags$td(htmlEscape(as.character(val)))
        }))
      })
      
      tags$div(
        style = "overflow-x:auto;",
        tags$table(
          class = "table table-striped table-bordered table-condensed",
          tags$thead(header),
          tags$tbody(rows)
        )
      )
    })
    
    output$download_enrich_sig <- downloadHandler(
      filename = function() {
        paste0("lipid_enrichment_significant_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        res <- try(enrich_result(), silent = TRUE)
        if (inherits(res, "try-error") || !is.list(res) || !is.data.frame(res$table_sig)) {
          write.csv(data.frame(), file, row.names = FALSE)
          return()
        }
        tbl <- res$table_sig
        if (!is.data.frame(tbl) || nrow(tbl) == 0) {
          write.csv(data.frame(), file, row.names = FALSE)
          return()
        }
        out <- make_lipid_table(tbl) %>% rename(lipidID = lipidID_raw)
        write.csv(out, file, row.names = FALSE)
      }
    )
    
    output$download_bg_with_acyl <- downloadHandler(
      filename = function() {
        paste0("background_with_acyl_chains_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- try(bg_tbl(), silent = TRUE)
        if (inherits(df, "try-error") || !is.data.frame(df)) {
          write.csv(data.frame(), file, row.names = FALSE)
          return()
        }
        out <- df
        if ("acyl_chains" %in% colnames(out)) {
          out$acyl_chains <- vapply(out$acyl_chains, function(v) paste(as.character(v), collapse = " | "), character(1))
        }
        write.csv(out, file, row.names = FALSE)
      }
    )
    
    list(
      bg_table_with_acyl = bg_tbl,
      term2gene = term2gene_reactive,
      enrich_table_sig = reactive({
        res <- try(enrich_result(), silent = TRUE)
        if (inherits(res, "try-error") || !is.list(res)) return(data.frame())
        res$table_sig %||% data.frame()
      })
    )
  })
}
