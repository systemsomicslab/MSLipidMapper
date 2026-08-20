# ============================================================
# msdialSE - mzTab-M (MS-DIAL) import helpers
#
# This file provides:
#   1) read_mztab_m():
#      - Reads an mzTab-M file exported by MS-DIAL
#      - Splits the file into section-wise tables and returns them as data.frames:
#          * MTD (metadata)
#          * SMH/SML (Small Molecule header/table), if present
#          * SFH/SMF (Small Molecule Feature header/table), if present
#      - Extracts helper mappings from metadata (assay map, study_variable map)
#
#   2) mztab_to_se():
#      - Parses MS-DIAL mzTab-M and builds a base SummarizedExperiment
#
#   3) mztab_se_to_lipidomics_se():
#      - Converts the mzTab-derived SummarizedExperiment into a "lipidomics-like" SE
#        compatible with the structure of load_lipidomics_se()
#
# Notes:
# - rgoslin is OPTIONAL (Suggests). If installed, it can infer subclass when no ontology exists.
# - The conversion utilities are robust against "pick = NA" and length-mismatch replacements.
# - "chemical_name" is preferred as the primary metabolite name source when available.
# ============================================================


#' Read an MS-DIAL mzTab-M file and split into each table (MTD/SMH/SML/SFH/SMF)
#'
#' This function reads an mzTab-M file exported by MS-DIAL and returns each
#' major section as a separate data.frame:
#' - `mtd`: metadata lines (MTD)
#' - `smh`/`sml`: small molecule table header/body (SMH/SML) if present
#' - `sfh`/`smf`: small molecule feature header/body (SFH/SMF) if present
#'
#' In addition, it extracts useful helper mappings from metadata:
#' - `assay_map`: mapping of assay id (e.g., "assay[1]") to assay name/file label (V3)
#' - `study_variables`: a list of study_variable groups, their names, and assay_refs
#'
#' @param filename Path to the mzTab-M file.
#' @param sep Field separator (default tab).
#' @param na_strings Value(s) treated as NA when reading the file (default `"null"`).
#' @param quote Quote argument for `read.table` (default `""`).
#' @param fill Logical. Passed to `read.table` (default TRUE).
#' @param verbose If TRUE, prints messages.
#'
#' @return A list with elements:
#'   - `raw`: full raw table as read (data.frame with V1..Vn)
#'   - `mtd`: metadata rows (data.frame) or NULL
#'   - `smh`: SMH header (named 1-row data.frame) or NULL
#'   - `sml`: SML table (data.frame) or NULL
#'   - `sfh`: SFH header (named 1-row data.frame) or NULL
#'   - `smf`: SMF table (data.frame) or NULL
#'   - `assay_map`: named character vector (names=assay id, values=display name), possibly length 0
#'   - `study_variables`: list with:
#'        * `groups`: character vector like "study_variable[1]"
#'        * `names`:  character vector of group names (V3 for each group)
#'        * `assay_refs`: named list mapping each group -> character vector of assay ids
#'   - `has_sml`: TRUE/FALSE
#'   - `has_smf`: TRUE/FALSE
#'
#' @export
read_mztab_m <- function(
    filename,
    sep = "\t",
    na_strings = "null",
    quote = "",
    fill = TRUE,
    verbose = TRUE
) {
  .msg <- function(...) if (isTRUE(verbose)) message(...)
  .stop <- function(txt) stop(txt, call. = FALSE)

  if (!file.exists(filename)) .stop(paste0("File not found: ", filename))

  # ---- read mzTab (compute max columns first) ----
  ncol <- max(stats::na.omit(utils::count.fields(file = filename, sep = sep)))
  raw <- utils::read.table(
    file = filename,
    header = FALSE,
    row.names = NULL,
    dec = ".",
    fill = fill,
    col.names = paste0("V", seq_len(ncol)),
    sep = sep,
    na.strings = na_strings,
    quote = quote,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!nrow(raw)) .stop("Empty mzTab file.")

  # ---- detect which blocks exist ----
  tags <- unique(as.character(raw$V1))
  has_sml <- sum(sapply(c("MTD", "SMH", "SML"), grepl, tags)) == 3
  has_smf <- sum(sapply(c("MTD", "SFH", "SMF"), grepl, tags)) == 3

  # ---- split MTD ----
  mtd <- raw[startsWith(as.character(raw$V1), "MTD"), , drop = FALSE]
  if (!nrow(mtd)) mtd <- NULL

  # ---- helper: build a named 1-row header data.frame ----
  .build_header <- function(hdr_row) {
    # hdr_row is a 1-row data.frame (raw subset)
    out <- as.character(hdr_row[1, , drop = TRUE])
    out <- out[!is.na(out) & nzchar(out)]
    # make sure we keep the order
    out_df <- as.data.frame(t(out), stringsAsFactors = FALSE)
    colnames(out_df) <- out
    out_df[0, , drop = FALSE] # return 0-row with correct colnames
  }

  # ---- helper: block extraction (tag -> header_tag + body_tag) ----
  .extract_block <- function(raw_df, header_tag, body_tag) {
    hdr <- raw_df[startsWith(as.character(raw_df$V1), header_tag), , drop = FALSE]
    body <- raw_df[startsWith(as.character(raw_df$V1), body_tag), , drop = FALSE]

    if (!nrow(hdr) || !nrow(body)) {
      return(list(header = NULL, body = NULL))
    }

    # mzTab uses header row as the true column names
    hdr_names <- as.character(hdr[1, ])
    df <- data.frame(body, stringsAsFactors = FALSE, check.names = FALSE)
    colnames(df) <- hdr_names

    # drop the tag column if present (usually first col is "SML"/"SMF")
    # but keep it if it is needed; many users prefer to keep it.
    # Here we KEEP all columns as-is; caller can drop later.

    list(header = hdr, body = df)
  }

  # ---- extract SMH/SML and SFH/SMF ----
  sm <- .extract_block(raw, "SMH", "SML")
  sf <- .extract_block(raw, "SFH", "SMF")

  smh <- sm$header
  sml <- sm$body
  sfh <- sf$header
  smf <- sf$body

  # ---- parse assay mapping and study_variable mapping from MTD ----
  assay_map <- character(0)
  study_variables <- list(groups = character(0), names = character(0), assay_refs = list())

  if (!is.null(mtd)) {
    # assay[n] -> V3 (display name)
    assay_rows <- mtd[grepl("^assay\\[[0-9]+\\]$", mtd$V2), , drop = FALSE]
    if (nrow(assay_rows)) {
      assay_map <- setNames(as.character(assay_rows$V3), as.character(assay_rows$V2))
    }

    # study_variable
    variables <- mtd[grepl("study_variable", mtd$V2), , drop = FALSE]
    if (nrow(variables)) {
      # Group IDs like "study_variable[1]" (strip "-..." suffix if exists)
      groups <- unique(gsub("-.*", "", as.character(variables$V2)))
      group_names <- as.character(mtd$V3[match(groups, mtd$V2)])
      names(group_names) <- groups

      assay_refs <- vector("list", length(groups))
      names(assay_refs) <- groups

      for (g in groups) {
        # Escape [] so grepl matches literally
        g2 <- gsub("\\[", "\\\\[", g)
        g2 <- gsub("\\]", "\\\\]", g2)
        all_info <- variables[grepl(g2, variables$V2), , drop = FALSE]

        refs <- all_info$V3[grepl("assay_refs", all_info$V2)]
        refs <- trimws(unlist(strsplit(as.character(refs), "\\|")))
        refs <- refs[nzchar(refs)]
        assay_refs[[g]] <- refs
      }

      study_variables <- list(
        groups = groups,
        names = unname(group_names),
        assay_refs = assay_refs
      )
      names(study_variables$names) <- groups
    }
  }

  .msg("Read mzTab-M: ", basename(filename))
  .msg("  has_sml: ", has_sml, " | has_smf: ", has_smf)
  if (length(assay_map)) .msg("  assays: ", length(assay_map))
  if (length(study_variables$groups)) .msg("  study_variables: ", length(study_variables$groups))

  list(
    raw = raw,
    mtd = mtd,
    smh = smh,
    sml = sml,
    sfh = sfh,
    smf = smf,
    assay_map = assay_map,
    study_variables = study_variables,
    has_sml = has_sml,
    has_smf = has_smf
  )
}

# ------------------------------------------------------------
# 1) mzTab -> SE
# ------------------------------------------------------------

#' Build a SummarizedExperiment from an MS-DIAL mzTab-M file (Small Molecule / Feature)
#'
#' @param filename Path to the mzTab file.
#' @param identifier Preferred identifier for rownames. One of:
#'   `"name"`, `"mass"`, `"sml_id"`, `"feature"`.
#' @param na_strings Value(s) treated as NA when reading the file.
#' @param sep Field separator (default tab).
#' @param quote Quote argument for read.table (default "").
#' @param add_err_msg Optional error callback. If NULL, errors will be raised via stop().
#' @param verbose If TRUE, prints messages.
#'
#' @return A list with:
#'   - `se`: SummarizedExperiment
#'   - `msg`: character vector of messages
#'   - `identifier_used`: identifier actually used (after fallbacks)
#'   - `mztab_table`: raw table (data.frame)
#'
#' @export
mztab_to_se <- function(
    filename,
    identifier = c("name", "mass", "sml_id", "feature"),
    na_strings = "null",
    sep = "\t",
    quote = "",
    add_err_msg = NULL,
    verbose = TRUE
) {
  identifier <- match.arg(identifier)

  # ---- helpers ----
  .msg <- function(...) if (isTRUE(verbose)) message(...)
  .fatal <- function(text) {
    if (is.function(add_err_msg)) {
      add_err_msg(text)
      return(invisible(0))
    } else {
      stop(text, call. = FALSE)
    }
  }

  # ---- input checks ----
  if (!file.exists(filename)) {
    return(.fatal(paste0("File not found: ", filename)))
  }

  # ---- read mzTab (compute max columns first) ----
  ncol <- max(stats::na.omit(utils::count.fields(file = filename, sep = sep)))

  mztab.table <- utils::read.table(
    file = filename,
    header = FALSE,
    row.names = NULL,
    dec = ".",
    fill = TRUE,
    col.names = paste0("V", seq_len(ncol)),
    sep = sep,
    na.strings = na_strings,
    quote = quote,
    stringsAsFactors = FALSE
  )

  msg <- character(0)

  # ---- sanity check for required sections ----
  tags <- unique(mztab.table$V1)
  has_sml <- sum(sapply(c("MTD", "SMH", "SML"), grepl, tags)) == 3
  has_smf <- sum(sapply(c("MTD", "SFH", "SMF"), grepl, tags)) == 3

  if (has_sml) {
    msg <- c(msg, "mzTab format ok!")
  } else if (has_smf) {
    msg <- c(msg, "Small molecule table not found. Using Small Molecule Feature table!")
    identifier <- "feature"
  } else {
    return(.fatal("Invalid mzTab format! Make sure mzTab file has been validated!"))
  }

  # ---- set up metadata ----
  metadata <- mztab.table[startsWith(as.character(mztab.table$V1), "MTD"), , drop = FALSE]
  variables <- metadata[grepl("study_variable", metadata$V2), , drop = FALSE]

  if (nrow(variables) < 1) {
    return(.fatal("Invalid mzTab format! Make sure mzTab file has been validated!"))
  }

  # Group IDs are like "study_variable[1]" (strip "-..." suffix)
  variables.groups <- unique(gsub("-.*", "", variables$V2))
  group.names <- metadata$V3[match(variables.groups, metadata$V2)]

  variables.list <- vector("list", length = length(variables.groups))
  names(variables.list) <- variables.groups

  for (i in seq_along(variables.groups)) {
    # Escape "[" and "]" so grepl works literally
    group2match <- gsub("\\[", "\\\\[", variables.groups[i])
    group2match <- gsub("\\]", "\\\\]", group2match)

    all.info <- variables[grepl(group2match, variables$V2), , drop = FALSE]
    assay.refs <- all.info$V3[grepl("assay_refs", all.info$V2)]
    variables.list[[i]] <- assay.refs
  }

  # ---- choose Small Molecule vs Feature block ----
  if (identifier != "feature") {
    smh <- mztab.table[startsWith(as.character(mztab.table$V1), "SMH"), , drop = FALSE]
    sml <- mztab.table[startsWith(as.character(mztab.table$V1), "SML"), , drop = FALSE]

    # Switch to feature table if SML is empty or single-row
    if (nrow(sml) < 2) {
      msg <- c(msg, "Small molecule table is empty or single-row. Using Small Molecule Feature table!")
      identifier <- "feature"
    }
  }
  if (identifier == "feature") {
    smh <- mztab.table[startsWith(as.character(mztab.table$V1), "SFH"), , drop = FALSE]
    sml <- mztab.table[startsWith(as.character(mztab.table$V1), "SMF"), , drop = FALSE]
  }

  if (nrow(sml) < 1) {
    return(.fatal("Invalid mzTab format! Make sure mzTab file has been validated!"))
  }

  sml.data.frame <- data.frame(sml, stringsAsFactors = FALSE)
  colnames(sml.data.frame) <- as.character(smh[1, ])

  # ---- fallback identifier if too many missing values ----
  if (identifier == "name") {
    miss_rate <- mean(is.na(sml.data.frame$chemical_name) | sml.data.frame$chemical_name == "")
    if (is.finite(miss_rate) && miss_rate > 0.5) {
      msg <- c(msg, "Too many missing chemical names, will use theoretical neutral mass instead!")
      identifier <- "mass"
    }
  }
  if (identifier == "mass") {
    miss_rate <- mean(is.na(sml.data.frame$theoretical_neutral_mass) | sml.data.frame$theoretical_neutral_mass == "")
    if (is.finite(miss_rate) && miss_rate > 0.5) {
      msg <- c(msg, "Too many missing m/z, will use mzTab SML_ID instead!")
      identifier <- "sml_id"
    }
  }

  # ---- build feature IDs and resolve duplicates ----
  if (identifier == "name") {
    id.og <- id <- sml.data.frame$chemical_name
    dup.id <- paste(sml.data.frame$chemical_name, sml.data.frame$adduct_ions, sep = "_")
    dup_idx <- which(duplicated(id, fromLast = TRUE) | duplicated(id))
    id[dup_idx] <- dup.id[dup_idx]
  } else if (identifier == "mass") {
    mass <- round(as.numeric(sml.data.frame$theoretical_neutral_mass), 5)
    id.og <- id <- mass
    dup.id <- paste(mass, sml.data.frame$adduct_ions, sep = "_")
    dup_idx <- which(duplicated(id, fromLast = TRUE) | duplicated(id))
    id[dup_idx] <- dup.id[dup_idx]
  } else if (identifier == "sml_id") {
    id <- sml.data.frame$SML_ID
  } else if (identifier == "feature") {
    id <- sml.data.frame$SMF_ID
  }

  # If still duplicated, append SML_ID (only possible if id.og exists)
  if (sum(duplicated(id)) > 1) {
    if (exists("id.og")) {
      if (!("SML_ID" %in% colnames(sml.data.frame))) {
        return(.fatal("Duplicate IDs detected but SML_ID column not found to disambiguate."))
      }
      id <- paste(id.og, sml.data.frame$SML_ID, sep = "_")
    } else {
      return(.fatal("Duplicate sample IDs are not allowed!"))
    }
  }

  # ---- get assay columns referenced by study_variable assay_refs ----
  assay_data <- trimws(unlist(lapply(variables.list, function(x) strsplit(x, "\\|"))))
  assay_cols <- paste0("abundance_", assay_data)

  idx <- match(assay_cols, colnames(sml.data.frame))
  if (anyNA(idx)) {
    missing_cols <- assay_cols[is.na(idx)]
    return(.fatal(paste0(
      "Some abundance columns referenced in metadata are missing: ",
      paste(missing_cols, collapse = ", ")
    )))
  }

  assay_df <- sml.data.frame[, idx, drop = FALSE]
  rowdata  <- sml.data.frame[, -idx, drop = FALSE]
  rownames(rowdata) <- id

  assay_matrix <- as.matrix(assay_df)
  assay_matrix <- apply(assay_matrix, 2, as.numeric)
  rownames(assay_matrix) <- id

  # ---- sample-level metadata: class + sample_id (display name) ----
  samples <- colnames(assay_matrix)                  # e.g., "abundance_assay[1]"
  samples_base <- gsub("^abundance_", "", samples)   # e.g., "assay[1]"

  # Expand assay_refs lists into clean vectors
  variables.list <- lapply(variables.list, function(x) trimws(unlist(strsplit(x, "\\|"))))

  # Map each sample to its group/class
  samples2groups <- character(length(samples_base))
  for (i in seq_along(samples_base)) {
    hit <- sapply(variables.list, function(x) samples_base[i] %in% x)
    samples2groups[i] <- if (any(hit)) group.names[which(hit)[1]] else NA_character_
  }

  # Map each assay[n] to the display name stored in metadata V3
  assay_rows <- metadata[grepl("^assay\\[[0-9]+\\]$", metadata$V2), , drop = FALSE]
  assay_id_to_name <- setNames(as.character(assay_rows$V3), as.character(assay_rows$V2))
  file.names <- unname(assay_id_to_name[samples_base])  # may contain NAs if missing

  sample_metadata <- data.frame(
    class = samples2groups,
    sample_id = as.character(file.names),
    stringsAsFactors = FALSE
  )
  rownames(sample_metadata) <- samples

  # ---- build SummarizedExperiment ----
  se <- SummarizedExperiment::SummarizedExperiment(
    assays  = list(abundance = assay_matrix),
    colData = S4Vectors::DataFrame(sample_metadata, check.names = FALSE),
    rowData = S4Vectors::DataFrame(rowdata, check.names = FALSE)
  )

  # Ensure SE rownames are set (helps downstream conversion)
  rownames(se) <- rownames(assay_matrix)

  .msg("Done: ", basename(filename), " (identifier_used = ", identifier, ")")

  list(
    se = se,
    msg = msg,
    identifier_used = identifier,
    mztab_table = mztab.table
  )
}

# ------------------------------------------------------------
# 2) mzTab SE -> lipidomics-like SE (rgoslin-based subclass fill; partial accept)
# ------------------------------------------------------------

#' Convert mzTab-derived SE (from mztab_to_se()) into a lipidomics-like SE
#'
#' This produces an SE compatible with the structure of `load_lipidomics_se()`:
#'  - assays$abundance: numeric matrix
#'  - colData: DataFrame with sample_id and class
#'  - rowData: keeps mzTab annotations and ensures `Metabolite name` and `subclass`
#'
#' If no Ontology-like column exists in rowData, this function can try to infer
#' subclass from lipid names using the rgoslin R package.
#'
#' NOTE: This version ACCEPTS PARTIAL rgoslin results:
#'   - parsed rows get inferred subclass
#'   - failed rows become "Unknown"
#'
#' @param x Output list from `mztab_to_se()` OR a SummarizedExperiment (`x$se`).
#' @param sample_id_col Column in mzTab colData used as sample_id (default "sample_id").
#' @param class_col Column in mzTab colData used as class (default "class").
#' @param name_priority Candidate rowData columns used to create "Metabolite name".
#' @param subclass_candidates Candidate rowData columns used to create subclass (if any).
#'   MS-DIAL mzTab-M exports commonly use `opt_global_Ontology`.
#' @param keep_original_sample_names If TRUE, keep original mzTab assay colnames in colData$mz_sample_col.
#' @param make_unique_sample_ids If TRUE, make sample_id unique.
#' @param use_rgoslin_if_no_ontology If TRUE, try rgoslin to infer subclass when no ontology-like info exists.
#' @param rgoslin_name_col Column used as input names for rgoslin (default "Metabolite name").
#' @param verbose If TRUE, prints messages about fallbacks.
#'
#' @return SummarizedExperiment
#' @export
mztab_se_to_lipidomics_se <- function(
    x,
    sample_id_col = "sample_id",
    class_col = "class",
    name_priority = c("Metabolite name", "chemical_name", "SML_ID", "SMF_ID_REFS"),
    subclass_candidates = c("opt_global_Ontology", "Ontology", "ontology", "subclass"),
    keep_original_sample_names = TRUE,
    make_unique_sample_ids = TRUE,
    use_rgoslin_if_no_ontology = TRUE,
    rgoslin_name_col = "Metabolite name",
    verbose = FALSE
) {
  .msg <- function(...) if (isTRUE(verbose)) message(...)

  # ---- normalize input ----
  if (inherits(x, "SummarizedExperiment")) {
    se0 <- x
  } else if (is.list(x) && !is.null(x$se) && inherits(x$se, "SummarizedExperiment")) {
    se0 <- x$se
  } else {
    stop("`x` must be a SummarizedExperiment or the list returned by mztab_to_se().", call. = FALSE)
  }

  # ---- extract matrix / metadata ----
  assay_mat <- SummarizedExperiment::assay(se0, "abundance")
  cd0 <- as.data.frame(SummarizedExperiment::colData(se0))
  rd0 <- as.data.frame(SummarizedExperiment::rowData(se0))

  colnames(cd0) <- trimws(colnames(cd0))
  colnames(rd0) <- trimws(colnames(rd0))

  # Align colData vs assay columns
  if (!identical(rownames(cd0), colnames(assay_mat))) {
    common <- intersect(rownames(cd0), colnames(assay_mat))
    if (length(common) < 1) stop("colData rownames and assay colnames do not match.", call. = FALSE)
    cd0 <- cd0[common, , drop = FALSE]
    assay_mat <- assay_mat[, common, drop = FALSE]
  }

  # ---- build lipidomics-style colData ----
  if (!(sample_id_col %in% colnames(cd0))) {
    stop(paste0("colData is missing `", sample_id_col, "`."), call. = FALSE)
  }
  if (!(class_col %in% colnames(cd0))) {
    stop(paste0("colData is missing `", class_col, "`."), call. = FALSE)
  }

  sample_id <- as.character(cd0[[sample_id_col]])
  miss_sid <- is.na(sample_id) | sample_id == ""
  sample_id[miss_sid] <- rownames(cd0)[miss_sid]

  if (isTRUE(make_unique_sample_ids)) {
    sample_id <- make.unique(sample_id)
  }

  if (isTRUE(keep_original_sample_names)) {
    cd0$mz_sample_col <- rownames(cd0)
  }

  col_data <- S4Vectors::DataFrame(
    sample_id   = sample_id,
    class       = as.character(cd0[[class_col]]),
    check.names = FALSE
  )

  extra_cols <- setdiff(colnames(cd0), c(sample_id_col, class_col))
  for (nm in extra_cols) {
    v <- cd0[[nm]]
    if (is.null(v) || length(v) == 0) next
    if (length(v) != nrow(cd0)) next
    col_data[[nm]] <- v
  }

  rownames(col_data) <- sample_id
  colnames(assay_mat) <- sample_id

  # ---- build lipidomics-style rowData ----
  rd <- S4Vectors::DataFrame(rd0, check.names = FALSE)
  names(rd) <- trimws(names(rd))

  # Create/ensure "Metabolite name"
  {
    rd_cols <- names(rd)
    pick <- name_priority[name_priority %in% rd_cols]
    pick <- if (length(pick)) pick[1] else NA_character_

    if (is.na(pick)) {
      rd[["Metabolite name"]] <- rownames(se0)
    } else {
      v <- rd[[pick]]
      if (is.null(v) || length(v) == 0 || length(v) != nrow(rd)) {
        rd[["Metabolite name"]] <- rownames(se0)
      } else {
        rd[["Metabolite name"]] <- as.character(v)
        miss <- is.na(rd[["Metabolite name"]]) | rd[["Metabolite name"]] == ""
        rd[["Metabolite name"]][miss] <- rownames(se0)[which(miss)]
      }
    }
  }

  # Helper: check if ontology-like columns exist and non-empty
  .has_useful_ontology <- function(rd_df, candidates, min_nonempty = 0.05) {
    cand <- candidates[candidates %in% names(rd_df)]
    if (!length(cand)) return(FALSE)
    for (nm in cand) {
      v <- rd_df[[nm]]
      if (is.null(v) || length(v) == 0) next
      v <- as.character(v)
      frac <- mean(!(is.na(v) | trimws(v) == ""))
      if (is.finite(frac) && frac >= min_nonempty) return(TRUE)
    }
    FALSE
  }

  # Create/ensure subclass (partial accept from rgoslin)
  {
    # If subclass exists but invalid length -> drop it
    if ("subclass" %in% names(rd)) {
      if (is.null(rd$subclass) || length(rd$subclass) == 0 || length(rd$subclass) != nrow(rd)) {
        rd$subclass <- NULL
      }
    }

    if ("subclass" %in% names(rd)) {
      rd$subclass <- as.character(rd$subclass)
      miss <- is.na(rd$subclass) | rd$subclass == ""
      if (any(miss)) rd$subclass[miss] <- "Unknown"
    } else {
      use_ont <- .has_useful_ontology(as.data.frame(rd), subclass_candidates, min_nonempty = 0.05)

      if (isTRUE(use_ont)) {
        src <- subclass_candidates[subclass_candidates %in% names(rd)]
        src <- if (length(src)) src[1] else NA_character_

        if (!is.na(src)) {
          vv <- rd[[src]]
          if (!is.null(vv) && length(vv) == nrow(rd)) {
            rd$subclass <- as.character(vv)
            miss <- is.na(rd$subclass) | rd$subclass == ""
            if (any(miss)) rd$subclass[miss] <- "Unknown"
          } else {
            rd$subclass <- rep("Unknown", nrow(rd))
          }
        } else {
          rd$subclass <- rep("Unknown", nrow(rd))
        }
      } else {
        # No useful ontology -> try rgoslin (partial accept)
        rd$subclass <- rep("Unknown", nrow(rd))

        if (isTRUE(use_rgoslin_if_no_ontology) && requireNamespace("rgoslin", quietly = TRUE)) {
          nm_col <- rgoslin_name_col
          if (!(nm_col %in% names(rd))) nm_col <- "Metabolite name"

          lipid_names <- as.character(rd[[nm_col]])
          lipid_names[is.na(lipid_names)] <- ""

          inferred <- rep(NA_character_, length(lipid_names))

          if (exists("parseLipidNames", where = asNamespace("rgoslin"), mode = "function")) {
            lipid_names2 <- as.character(lipid_names)

            parsed <- try(rgoslin::parseLipidNames(lipid_names2), silent = TRUE)
            if (!inherits(parsed, "try-error") && is.data.frame(parsed)) {
              cand_cols <- c("lipid_class", "lipidClass", "class", "category",
                             "main_class", "mainClass", "lipid_category")
              hit <- cand_cols[cand_cols %in% colnames(parsed)]
              if (length(hit)) {
                inferred <- as.character(parsed[[hit[1]]])
              }
            } else {
              .msg("rgoslin::parseLipidNames() failed; keep subclass as Unknown.")
            }
          } else if (exists("parseLipidName", where = asNamespace("rgoslin"), mode = "function")) {
            inferred <- vapply(lipid_names, function(s) {
              if (!nzchar(s)) return(NA_character_)
              out <- try(rgoslin::parseLipidName(as.character(s)), silent = TRUE)
              if (inherits(out, "try-error") || is.null(out)) return(NA_character_)
              if (is.list(out)) {
                for (k in c("lipid_class", "lipidClass", "class", "category",
                            "main_class", "mainClass")) {
                  if (!is.null(out[[k]]) && length(out[[k]]) == 1) return(as.character(out[[k]]))
                }
              }
              NA_character_
            }, character(1))
          } else {
            .msg("rgoslin is installed but no parse function was found; keep subclass as Unknown.")
          }

          ok <- !(is.na(inferred) | inferred == "")
          success_rate <- mean(ok)
          if (any(ok)) {
            rd$subclass[ok] <- inferred[ok]
          }
          .msg(sprintf("subclass inferred by rgoslin (partial accept; success_rate=%.3f).", success_rate))
        } else {
          if (isTRUE(use_rgoslin_if_no_ontology)) {
            .msg("rgoslin not available; subclass set to Unknown.")
          }
        }
      }
    }
  }

  # Heatmap and other existing MSLipidMapper modules use the canonical
  # `Ontology` column, while MS-DIAL mzTab-M exports `opt_global_Ontology`.
  # Keep both `Ontology` and `subclass` available with identical fallbacks.
  if (!("Ontology" %in% names(rd))) {
    ontology_source <- if ("opt_global_Ontology" %in% names(rd)) {
      as.character(rd[["opt_global_Ontology"]])
    } else {
      as.character(rd$subclass)
    }
    ontology_source[is.na(ontology_source) | ontology_source == ""] <- "Unknown"
    rd[["Ontology"]] <- ontology_source
  } else {
    rd[["Ontology"]] <- as.character(rd[["Ontology"]])
    missing_ontology <- is.na(rd[["Ontology"]]) | rd[["Ontology"]] == ""
    rd[["Ontology"]][missing_ontology] <- as.character(rd$subclass[missing_ontology])
  }

  # Set rownames from "Metabolite name" (unique)
  new_rn <- make.unique(as.character(rd[["Metabolite name"]]))
  rownames(rd) <- new_rn
  rownames(assay_mat) <- new_rn

  SummarizedExperiment::SummarizedExperiment(
    assays  = list(abundance = assay_mat),
    rowData = rd,
    colData = col_data
  )
}
