# ============================================================
# Lipid chain parser utilities (MS-DIAL style)  --- YAML-unified FULL SCRIPT
#
# Unified algorithm with mod_lipid_enrich.R:
# - Always read rules from local YAML file (load_lipid_rules(yaml_path))
# - Rule-based parsing:
#   * no_chain: TRUE => always returns character(0)
#   * ignore_sum_only: TRUE => if "sum-only like" (1 chain token, no delimiter/suffix evidence) => character(0)
#   * exclude_first_chain: TRUE => drop 1st chain ONLY when length(chains) >= 2
# - Supports delimiter logic:
#   * split="auto": "/" if present else "_" if present else whitespace else unsplit
# - Handles "A|B" notation by choosing the alternative with the most chain tokens
#   * optional prefer_molecular_notation: true => add bonus for "_" or "/" in alt
# - Suffix extraction:
#   * detect_fa_suffix: TRUE => (FA 22:6), (O-16:0), etc.
#   * detect_fa_tail : TRUE => "...;FA 20:4" (NO parentheses)  <-- for ASG-like
# - Preserves "(2OH)" parentheses and normalizes ";2O" -> ";O2"
# - Stable list-column generation (never NULL): list(character(0)) if none
#
# Provides:
# - load_lipid_rules()
# - extract_acyl_chains_from_name()
# - add_chain_list_to_se()
# - build_chain_dict_from_se()
# - filter_se_by_chain()
# - filter_se_remove_odd_chains()
# ============================================================

suppressPackageStartupMessages({
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(yaml)
  library(rlang)
  library(purrr)
  library(SummarizedExperiment)
  library(S4Vectors)
})

# ------------------------------------------------------------
# Safe helpers
# ------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Ensure list-column: NULL/NA -> list(character(0))
.ensure_listcol <- function(x) {
  if (is.list(x)) {
    x[vapply(x, is.null, logical(1))] <- list(character(0))
    x[vapply(x, function(v) length(v) == 1 && is.na(v), logical(1))] <- list(character(0))
    return(x)
  }
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    xi <- tryCatch(x[[i]], error = function(e) NA)
    if (is.null(xi) || (length(xi) == 1 && is.na(xi))) out[[i]] <- character(0)
    else out[[i]] <- as.character(xi)
  }
  out
}

# ------------------------------------------------------------
# YAML loader (strict; user should maintain YAML)
# ------------------------------------------------------------
load_lipid_rules <- function(yaml_file) {
  if (is.null(yaml_file) || !nzchar(yaml_file)) {
    stop("yaml_file is NULL/empty. Provide a path to acyl_rules.yaml.", call. = FALSE)
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

# ------------------------------------------------------------
# Rule selection with scoring (unified)
# ------------------------------------------------------------
.score_rule <- function(rule) {
  pri <- suppressWarnings(as.numeric(rule$priority %||% 0))
  pri <- ifelse(is.na(pri), 0, pri)
  rx_len <- nchar(rule$class_regex %||% "")
  bonus <- 0
  bonus <- bonus + ifelse(isTRUE(rule$no_chain), 80, 0)
  bonus <- bonus + ifelse(isTRUE(rule$ignore_sum_only), 20, 0)
  bonus <- bonus + ifelse(isTRUE(rule$exclude_first_chain), 25, 0)
  bonus <- bonus + ifelse(isTRUE(rule$detect_fa_suffix), 10, 0)
  bonus <- bonus + ifelse(isTRUE(rule$detect_fa_tail), 12, 0)
  bonus <- bonus + ifelse(isTRUE(rule$drop_zero_chains), 5, 0)
  bonus <- bonus + ifelse(isTRUE(rule$prefer_molecular_notation), 2, 0)
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
# Split helper (unified)
# ------------------------------------------------------------
.split_by_rule <- function(x, split, split_mode = NULL) {
  if (is.null(split) || !nzchar(split)) return(x)
  
  if (identical(split, "auto")) {
    if (str_detect(x, fixed("/"))) return(unlist(str_split(x, fixed("/"))))
    if (str_detect(x, fixed("_"))) return(unlist(str_split(x, fixed("_"))))
    if (str_detect(x, "\\s+"))     return(unlist(str_split(x, "\\s+")))
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
# Best alternative for "A|B" notations
# Choose the alternative with the most chain tokens
# (optional) prefer_molecular_notation: bonus if "_" or "/" exists
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
  
  score <- vapply(parts, .count_chain_tokens, integer(1))
  
  if (isTRUE(prefer_molecular_notation)) {
    bonus <- ifelse(str_detect(parts, "[/_]"), 2L, 0L)
    score <- score * 10L + bonus
  }
  
  parts[which.max(score)]
}

# ------------------------------------------------------------
# Sum-only detector (ignore_sum_only)
# "sum-only": exactly one chain token, and no evidence of molecular/sn notation or FA suffix.
# ------------------------------------------------------------
.is_sum_only_like <- function(core) {
  if (is.na(core) || !nzchar(core)) return(TRUE)
  
  has_delim   <- str_detect(core, "[/_]")
  has_parenFA <- str_detect(core, "\\((?:FA\\s*)?(?:O-|P-)?\\d+:\\d+")
  has_tailFA  <- str_detect(core, "(?:^|[;\\s])FA\\s*(?:O-|P-)?\\d+:\\d+")
  n_tok       <- .count_chain_tokens(core)
  
  (!has_delim) && (!has_parenFA) && (!has_tailFA) && (n_tok <= 1L)
}

# ------------------------------------------------------------
# Normalize chain token
# - keep "(2OH)" parentheses
# - normalize ;2O -> ;O2
# - drop 0:0
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
  
  # normalize ;2O -> ;O2
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
# Extract acyl chains from ONE lipid name (ALWAYS character vec)
# - obeys: no_chain / ignore_sum_only / exclude_first_chain (>=2 only)
# - suffix extraction:
#     detect_fa_suffix: parentheses
#     detect_fa_tail  : "... FA 20:4" (no parentheses)
# ------------------------------------------------------------
extract_acyl_chains_from_name <- function(lipid_name, rules) {
  nm <- as.character(lipid_name %||% "")
  nm <- str_trim(nm)
  if (!nzchar(nm)) return(character(0))
  
  # class token (allow + - ; _)
  class_raw <- str_extract(nm, "^[A-Za-z0-9\\-\\+;_]+") %||% ""
  class_raw <- str_trim(class_raw)
  
  rule <- select_rule(class_raw, rules)
  
  # minimal fallback (still safe)
  if (is.null(rule)) {
    rule <- list(
      no_chain = FALSE,
      ignore_sum_only = FALSE,
      split = "auto",
      split_mode = NULL,
      detect_fa_suffix = TRUE,
      detect_fa_tail = FALSE,
      exclude_first_chain = FALSE,
      drop_zero_chains = FALSE,
      prefer_molecular_notation = FALSE
    )
  }
  
  if (isTRUE(rule$no_chain %||% FALSE)) return(character(0))
  
  # choose best alt (A|B)
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
  
  # ignore_sum_only (stronger)
  if (isTRUE(rule$ignore_sum_only %||% FALSE) && .is_sum_only_like(core)) {
    return(character(0))
  }
  
  # ---- suffix extraction in parentheses: (FA 16:0) etc. ----
  suffix_vec <- character(0)
  if (isTRUE(rule$detect_fa_suffix %||% FALSE)) {
    # allow isotopic suffix like (d7) inside final parentheses, keep as-is
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
  
  # ---- tail extraction: "...;FA 20:4" (NO parentheses) ----
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
  if (isTRUE(rule$exclude_first_chain %||% FALSE) && length(chains) >= 2L) {
    chains <- chains[-1]
  }
  
  unique(chains)
}

# ------------------------------------------------------------
# Oxygen extractor for chain strings (for dictionary/filter)
# ------------------------------------------------------------
.count_keywords_fixed <- function(x, keywords) {
  if (is.null(keywords) || length(keywords) == 0) return(0L)
  sum(vapply(keywords, function(k) {
    if (is.na(k) || !nzchar(k)) return(0L)
    str_count(x, fixed(k))
  }, integer(1)))
}

.oxygen_from_chain_code <- function(x) {
  if (is.na(x) || !nzchar(x)) return(0L)
  ox <- 0L
  
  # ;O or ;O2
  m1 <- str_match_all(x, ";O(\\d*)")[[1]]
  if (nrow(m1) > 0) {
    vals <- m1[, 2]
    add  <- ifelse(vals == "" | is.na(vals), 1L, suppressWarnings(as.integer(vals)))
    add[is.na(add)] <- 0L
    ox <- ox + sum(add)
  }
  
  # ;2O, ;3O, ...
  m2 <- str_match_all(x, ";(\\d+)O")[[1]]
  if (nrow(m2) > 0) {
    add <- suppressWarnings(as.integer(m2[, 2]))
    add[is.na(add)] <- 0L
    ox <- ox + sum(add)
  }
  
  # (2OH)
  m3 <- str_match_all(x, "\\((\\d+)OH\\)")[[1]]
  if (nrow(m3) > 0) {
    add <- suppressWarnings(as.integer(m3[, 2]))
    add[is.na(add)] <- 0L
    ox <- ox + sum(add)
  }
  
  # (OH)
  m4 <- str_match_all(x, "\\((OH)\\)")[[1]]
  if (nrow(m4) > 0) ox <- ox + nrow(m4)
  
  as.integer(ox)
}

# Parse a single chain string into carbon/db/oxygen
parse_chain <- function(chain, oxygen_keywords = c(";OH", "(OH)")) {
  raw_chain <- str_trim(chain %||% "")
  
  normalized_chain <- raw_chain
  normalized_chain <- str_remove(normalized_chain, "^FA\\s*")
  normalized_chain <- str_remove(normalized_chain, "^(O-|P-)")
  
  normalized_chain <- str_replace_all(normalized_chain, ";(\\d+)O(?=\\D|$)", ";O\\1")
  if (str_detect(normalized_chain, "^\\d+:\\d+\\(\\d+OH\\)$")) {
    normalized_chain <- str_replace(normalized_chain, "\\(\\d+OH\\)$", function(m) paste0(";", m))
  }
  
  carbon <- NA_integer_
  db     <- NA_integer_
  oxygen <- 0L
  
  if (str_detect(normalized_chain, "^\\d+:\\d+")) {
    carbon <- suppressWarnings(as.integer(str_extract(normalized_chain, "^\\d+")))
    db     <- suppressWarnings(as.integer(str_extract(normalized_chain, "(?<=:)\\d+")))
  } else if (str_detect(normalized_chain, "^\\d+")) {
    carbon <- suppressWarnings(as.integer(str_extract(normalized_chain, "^\\d+")))
    db     <- 0L
  }
  
  oxygen <- .oxygen_from_chain_code(normalized_chain)
  if (is.na(oxygen) || oxygen == 0L) {
    oxygen <- .count_keywords_fixed(normalized_chain, oxygen_keywords)
  }
  
  data.frame(
    chain  = normalized_chain,
    carbon = carbon,
    db     = db,
    oxygen = as.integer(oxygen),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Add acyl_chains (list col) to SummarizedExperiment rowData
# - YAML rules unify with enrich.R
# - Collision-proof
# ------------------------------------------------------------
add_chain_list_to_se <- function(se,
                                 rules,
                                 lipid_col = "Metabolite.name",
                                 out_col   = "acyl_chains") {
  stopifnot(methods::is(se, "SummarizedExperiment"))
  
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!lipid_col %in% colnames(rd)) {
    stop("rowData does not contain column: '", lipid_col, "'.", call. = FALSE)
  }
  
  if (out_col %in% colnames(rd)) {
    rd[[out_col]] <- NULL
  }
  
  lipid_names <- rd[[lipid_col]]
  
  chains_list <- purrr::map(as.character(lipid_names), ~{
    out <- try(extract_acyl_chains_from_name(.x, rules = rules), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) character(0) else as.character(out)
  })
  
  rd[[out_col]] <- .ensure_listcol(chains_list)
  
  SummarizedExperiment::rowData(se) <- S4Vectors::DataFrame(rd)
  se
}

# ------------------------------------------------------------
# Build chain dictionary from rowData list column
# - chain_code -> carbon/db/oxygen
# ------------------------------------------------------------
build_chain_dict_from_se <- function(se, chain_col = "acyl_chains", rules = NULL) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!chain_col %in% colnames(rd)) {
    stop("rowData does not contain column: '", chain_col, "'.", call. = FALSE)
  }
  
  rd[[chain_col]] <- .ensure_listcol(rd[[chain_col]])
  
  chains <- rd %>%
    mutate(row_index = dplyr::row_number()) %>%
    tidyr::unnest(!!rlang::sym(chain_col), keep_empty = FALSE) %>%
    rename(chain_code = !!rlang::sym(chain_col)) %>%
    distinct(chain_code) %>%
    filter(!is.na(chain_code), nzchar(chain_code))
  
  oxygen_kw <- if (!is.null(rules)) (rules$defaults$oxygen_detect_keywords %||% c(";OH", "(OH)")) else c(";OH", "(OH)")
  
  parsed <- bind_rows(lapply(chains$chain_code, function(cc) {
    parse_chain(cc, oxygen_keywords = oxygen_kw)
  }))
  
  bind_cols(chains, parsed) %>%
    select(chain_code, carbon, db, oxygen) %>%
    distinct()
}

# ------------------------------------------------------------
# Filter SE rows by chain conditions (carbon/db/oxygen)
# - Row kept if >=1 chain satisfies all specified conditions
# - Rows with NO chain info are dropped if filtering is requested
# ------------------------------------------------------------
filter_se_by_chain <- function(se,
                               carbon = NULL,
                               db     = NULL,
                               oxygen = NULL,
                               chain_col = "acyl_chains",
                               rules = NULL) {
  if (is.null(carbon) && is.null(db) && is.null(oxygen)) return(se)
  
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!chain_col %in% colnames(rd)) {
    stop("rowData does not contain column: '", chain_col, "'.", call. = FALSE)
  }
  rd[[chain_col]] <- .ensure_listcol(rd[[chain_col]])
  
  oxygen_kw <- if (!is.null(rules)) (rules$defaults$oxygen_detect_keywords %||% c(";OH", "(OH)")) else c(";OH", "(OH)")
  
  rd_long <- rd %>%
    mutate(row_index = dplyr::row_number()) %>%
    tidyr::unnest(!!rlang::sym(chain_col), keep_empty = FALSE) %>%
    rename(chain_code = !!rlang::sym(chain_col)) %>%
    filter(!is.na(chain_code), nzchar(chain_code))
  
  if (nrow(rd_long) == 0) {
    return(se[integer(0), , drop = FALSE])
  }
  
  parsed <- bind_rows(lapply(rd_long$chain_code, function(cc) {
    parse_chain(cc, oxygen_keywords = oxygen_kw)
  }))
  
  rd_long <- bind_cols(rd_long, parsed) %>%
    rename(carbon_long = carbon, db_long = db, oxygen_long = oxygen)
  
  if (!is.null(carbon)) rd_long <- rd_long %>% filter(!is.na(carbon_long), carbon_long == carbon)
  if (!is.null(db))     rd_long <- rd_long %>% filter(!is.na(db_long), db_long == db)
  if (!is.null(oxygen)) rd_long <- rd_long %>% filter(!is.na(oxygen_long), oxygen_long == oxygen)
  
  idx <- sort(unique(rd_long$row_index))
  se[idx, , drop = FALSE]
}

# ------------------------------------------------------------
# Remove molecules that contain ANY odd-carbon chain
# - Rows with NO chain info are kept.
# ------------------------------------------------------------
filter_se_remove_odd_chains <- function(se, chain_col = "acyl_chains", rules = NULL) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!chain_col %in% colnames(rd)) {
    stop("rowData does not contain column: '", chain_col, "'.", call. = FALSE)
  }
  
  rd[[chain_col]] <- .ensure_listcol(rd[[chain_col]])
  
  rd_long <- rd %>%
    mutate(row_index = dplyr::row_number()) %>%
    tidyr::unnest(!!rlang::sym(chain_col), keep_empty = FALSE) %>%
    rename(chain_code = !!rlang::sym(chain_col)) %>%
    filter(!is.na(chain_code), nzchar(chain_code)) %>%
    mutate(
      chain_code2 = str_trim(chain_code),
      chain_code2 = str_remove(chain_code2, "^FA\\s*"),
      chain_code2 = str_remove(chain_code2, "^(O-|P-)")
    )
  
  if (nrow(rd_long) == 0) {
    return(se)
  }
  
  oxygen_kw <- if (!is.null(rules)) (rules$defaults$oxygen_detect_keywords %||% c(";OH", "(OH)")) else c(";OH", "(OH)")
  
  parsed <- bind_rows(lapply(rd_long$chain_code2, function(cc) {
    parse_chain(cc, oxygen_keywords = oxygen_kw)
  }))
  
  rd_long <- bind_cols(rd_long, parsed) %>%
    rename(carbon_long = carbon)
  
  odd_info <- rd_long %>%
    group_by(row_index) %>%
    summarise(
      has_odd = any(!is.na(carbon_long) & (carbon_long %% 2L == 1L)),
      .groups = "drop"
    )
  
  keep_idx <- setdiff(seq_len(nrow(rd)), odd_info$row_index[odd_info$has_odd])
  se[keep_idx, , drop = FALSE]
}
