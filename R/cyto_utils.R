# R/cyto_utils.R ---------------------------------------------------------------
# Utilities for mod_cyto (SE helpers, parsers, SVG helpers, chain filter, etc.)

suppressPackageStartupMessages({
  library(jsonlite)
  library(ggplot2)
  library(SummarizedExperiment)
  library(S4Vectors)
  library(svglite)
  library(stringr)
  library(xml2)
})

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 0)) b else a
.safe_id <- function(x) gsub("[^A-Za-z0-9_\\-]+","_", as.character(x))

# Fixed plumber base URL
.CY_IMG_BASE_URL <- "http://127.0.0.1:7314/static"

# ---- SVG canvas sizes --------------------------------------------------------
.get_svg_size <- function(kind = c("class","gene","hm")) {
  kind <- match.arg(kind)
  opt_name <- switch(kind,
                     class = "mslm_cy_svg_size_class",
                     gene  = "mslm_cy_svg_size_gene",
                     hm    = "mslm_cy_svg_size_hm"
  )
  v <- getOption(opt_name, NULL)
  
  if (is.null(v) || length(v) < 2L) {
    v <- switch(kind,
                class = c(w = 5.4, h = 4.5),
                gene  = c(w = 5.4, h = 4.5),
                hm    = c(w = 10,  h = 8)
    )
  } else {
    if (is.null(names(v))) {
      v <- c(w = as.numeric(v[1]), h = as.numeric(v[2]))
    } else {
      v <- c(
        w = as.numeric(v[["w"]] %||% v[["width"]]  %||% v[[1]]),
        h = as.numeric(v[["h"]] %||% v[["height"]] %||% v[[2]])
      )
    }
  }
  
  v[["w"]] <- max(1, v[["w"]])
  v[["h"]] <- max(1, v[["h"]])
  v
}

.get_svg_dir <- function() {
  d <- getOption("mslm_cy_svg_dir", NULL)
  if (is.null(d) || !nzchar(d)) {
    d <- normalizePath(file.path(tempdir(), "cyimg_cache"), winslash = "/", mustWork = FALSE)
  } else {
    d <- normalizePath(d, winslash = "/", mustWork = FALSE)
  }
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}


.tidy_from_se_global <- function(se, assay_name = "abundance") {
  if (!methods::is(se, "SummarizedExperiment")) {
    stop("`se` must be a SummarizedExperiment.")
  }
  if (!assay_name %in% SummarizedExperiment::assayNames(se)) {
    stop("Assay not found: ", assay_name)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Please install tidyr.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install dplyr.")
  
  mat <- SummarizedExperiment::assay(se, assay_name)
  if (is.null(rownames(mat))) rownames(mat) <- rownames(se)
  if (is.null(colnames(mat))) colnames(mat) <- colnames(se)
  
  df <- as.data.frame(mat, check.names = FALSE)
  df$feature_id <- rownames(df)
  
  tidy <- tidyr::pivot_longer(
    df,
    cols      = -feature_id,
    names_to  = "sample_id",
    values_to = "abundance"
  )
  
  cd <- as.data.frame(SummarizedExperiment::colData(se))
  cd$sample_id <- rownames(cd)
  if (is.null(cd$sample_id)) cd$sample_id <- colnames(se)
  
  tidy <- dplyr::left_join(tidy, cd, by = "sample_id")
  tidy
}
# ---- Node SVG padding + x-title drop ----------------------------------------
.get_svg_pad4_pt <- function() {
  t0 <- getOption("mslm_cy_svg_pad_top_pt",    NULL)
  r0 <- getOption("mslm_cy_svg_pad_right_pt",  NULL)
  b0 <- getOption("mslm_cy_svg_pad_bottom_pt", NULL)
  l0 <- getOption("mslm_cy_svg_pad_left_pt",   NULL)
  
  v <- getOption("mslm_cy_svg_pad_pt", NULL)
  if (is.null(v)) v <- getOption("mslm_cy_svg_pad_pt_legacy", NULL)
  
  pad_default <- c(top = 50, right = 18, bottom = 18, left = 18)
  
  if (!is.null(v)) {
    vv <- suppressWarnings(as.numeric(v))
    if (length(vv) == 1 && is.finite(vv[1]) && vv[1] >= 0) {
      pad_default[] <- vv[1]
    } else if (length(vv) >= 4) {
      vv <- vv[1:4]
      if (all(is.finite(vv)) && all(vv >= 0)) {
        pad_default <- c(top = vv[1], right = vv[2], bottom = vv[3], left = vv[4])
      }
    }
  }
  
  if (!is.null(t0)) { tt <- suppressWarnings(as.numeric(t0)); if (is.finite(tt) && tt >= 0) pad_default["top"] <- tt }
  if (!is.null(r0)) { rr <- suppressWarnings(as.numeric(r0)); if (is.finite(rr) && rr >= 0) pad_default["right"] <- rr }
  if (!is.null(b0)) { bb <- suppressWarnings(as.numeric(b0)); if (is.finite(bb) && bb >= 0) pad_default["bottom"] <- bb }
  if (!is.null(l0)) { ll <- suppressWarnings(as.numeric(l0)); if (is.finite(ll) && ll >= 0) pad_default["left"] <- ll }
  
  pad_default
}

.prep_for_node_svg <- function(p) {
  if (!inherits(p, c("gg", "ggplot"))) return(p)
  pad <- .get_svg_pad4_pt()
  p +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = pad[["top"]], r = pad[["right"]], b = pad[["bottom"]], l = pad[["left"]],
        unit = "pt"
      ),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

.save_svg_file <- function(plt, file, width, height) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  svglite::svglite(file, width = width, height = height, bg = "white")
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
  print(plt)
  invisible(file)
}

# ---- SE helpers --------------------------------------------------------------
resolve_class_col <- function(se) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  keys <- c("class","lipid_class","Class","LipidClass","subclass","ontology","Ontology")
  hit  <- keys[keys %in% names(rd)]
  if (!length(hit)) stop("rowData(se) must contain a class column (class/lipid_class/subclass/ontology, etc.).")
  hit[1]
}

get_valid_classes_from_se <- function(se) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  v <- as.character(rd[[resolve_class_col(se)]])
  sort(unique(v[nzchar(v) & !is.na(v)]))
}

# ---- REQUIRED: aggregate to class-level SummarizedExperiment -----------------
# Used by mod_cyto.R for class-level plotting.
aggregate_to_class_se <- function(se, fun = c("sum","mean","median"), assay_name = NULL) {
  if (is.null(se) || !methods::is(se, "SummarizedExperiment")) {
    stop("aggregate_to_class_se(): 'se' must be a SummarizedExperiment.", call. = FALSE)
  }
  fun <- match.arg(tolower(fun), c("sum","mean","median"))
  
  # assay
  if (is.null(assay_name)) {
    A <- SummarizedExperiment::assay(se, 1)
  } else {
    if (!assay_name %in% SummarizedExperiment::assayNames(se)) {
      stop("aggregate_to_class_se(): assay_name not found in se.", call. = FALSE)
    }
    A <- SummarizedExperiment::assay(se, assay_name)
  }
  A <- as.matrix(A)
  
  # class labels (per feature/row)
  class_col <- resolve_class_col(se)
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  cls <- as.character(rd[[class_col]])
  cls[is.na(cls)] <- ""
  ok <- nzchar(cls)
  
  if (!any(ok)) {
    stop("aggregate_to_class_se(): no non-empty class labels in rowData.", call. = FALSE)
  }
  
  A2   <- A[ok, , drop = FALSE]
  cls2 <- cls[ok]
  
  # aggregate rows by class -> matrix (#class x #samples)
  if (fun == "sum") {
    Agg <- rowsum(A2, group = cls2, reorder = TRUE, na.rm = TRUE)
  } else if (fun == "mean") {
    AggSum <- rowsum(A2, group = cls2, reorder = TRUE, na.rm = TRUE)
    n_per  <- as.numeric(table(cls2)[rownames(AggSum)])
    Agg <- AggSum / n_per
  } else { # median
    u <- sort(unique(cls2))
    Agg <- matrix(NA_real_, nrow = length(u), ncol = ncol(A2),
                  dimnames = list(u, colnames(A2)))
    for (k in seq_along(u)) {
      idx <- which(cls2 == u[[k]])
      if (length(idx) == 1) {
        Agg[k, ] <- A2[idx, ]
      } else {
        Agg[k, ] <- apply(A2[idx, , drop = FALSE], 2, stats::median, na.rm = TRUE)
      }
    }
  }
  
  # build SE (keep colData)
  cd <- SummarizedExperiment::colData(se)
  
  # rowData: ensure resolve_class_col() works on the aggregated SE too
  rd_new <- S4Vectors::DataFrame(
    class     = rownames(Agg),
    Ontology  = rownames(Agg),  # helps if downstream expects "Ontology"
    stringsAsFactors = FALSE
  )
  rownames(rd_new) <- rownames(Agg)
  
  se_out <- SummarizedExperiment::SummarizedExperiment(
    assays  = S4Vectors::SimpleList(abundance = Agg),
    rowData = rd_new,
    colData = cd
  )
  
  se_out
}

# ---- Acyl-chain filter helpers ----------------------------------------------
.has_chain_col <- function(se, chain_col = "acyl_chains") {
  if (is.null(se) || !methods::is(se, "SummarizedExperiment")) return(FALSE)
  rd <- SummarizedExperiment::rowData(se)
  if (!chain_col %in% colnames(rd)) return(FALSE)
  x <- rd[[chain_col]]
  is.list(x) || inherits(x, c("List", "SimpleList", "CompressedList"))
}

.as_chain_list <- function(se, chain_col = "acyl_chains") {
  rd <- SummarizedExperiment::rowData(se)
  x  <- rd[[chain_col]]
  xs <- as.list(x)
  lapply(xs, function(v) {
    if (is.null(v)) return(character(0))
    v <- as.character(v)
    v <- v[!is.na(v) & nzchar(v)]
    v
  })
}

.get_unique_chain_codes <- function(se, chain_col = "acyl_chains") {
  if (!.has_chain_col(se, chain_col)) return(character(0))
  xs <- .as_chain_list(se, chain_col)
  codes <- unique(unlist(xs, use.names = FALSE))
  sort(codes)
}

.chain_code_triplet <- function(code) {
  code <- as.character(code)
  carbon <- suppressWarnings(as.integer(stringr::str_extract(code, "^\\d+")))
  db     <- suppressWarnings(as.integer(stringr::str_extract(code, "(?<=:)\\d+")))
  oxygen <- if (stringr::str_detect(code, ";O\\d+")) {
    suppressWarnings(as.integer(stringr::str_extract(code, "(?<=;O)\\d+")))
  } else 0L
  list(carbon = carbon, db = db, oxygen = oxygen)
}

.keep_by_numeric_chain <- function(chain_vec, carbon = NULL, db = NULL, oxygen = NULL) {
  if (length(chain_vec) == 0) return(FALSE)
  for (cc in chain_vec) {
    tri <- .chain_code_triplet(cc)
    if (!is.null(carbon) && !is.na(carbon) && !identical(tri$carbon, carbon)) next
    if (!is.null(db)     && !is.na(db)     && !identical(tri$db, db))         next
    if (!is.null(oxygen) && !is.na(oxygen) && !identical(tri$oxygen, oxygen)) next
    return(TRUE)
  }
  FALSE
}

.filter_lipid_se_by_chains <- function(se,
                                       chain_codes = character(0),
                                       require_all = FALSE,
                                       exclude_odd = FALSE,
                                       carbon = NULL, db = NULL, oxygen = NULL,
                                       chain_col = "acyl_chains") {
  if (is.null(se) || !methods::is(se, "SummarizedExperiment")) return(se)
  if (!.has_chain_col(se, chain_col)) return(se)
  
  xs <- .as_chain_list(se, chain_col)
  keep <- rep(TRUE, length(xs))
  
  if (isTRUE(exclude_odd)) {
    keep_odd <- vapply(xs, function(v){
      if (!length(v)) return(TRUE)
      carb <- suppressWarnings(as.integer(stringr::str_extract(v, "^\\d+")))
      !any(!is.na(carb) & (carb %% 2L == 1L))
    }, logical(1))
    keep <- keep & keep_odd
  }
  
  chain_codes <- as.character(chain_codes)
  chain_codes <- chain_codes[!is.na(chain_codes) & nzchar(chain_codes)]
  
  if (length(chain_codes)) {
    keep_cc <- vapply(xs, function(v){
      if (!length(v)) return(FALSE)
      if (isTRUE(require_all)) all(chain_codes %in% v) else any(v %in% chain_codes)
    }, logical(1))
    keep <- keep & keep_cc
  }
  
  has_numeric <- !(is.null(carbon) && is.null(db) && is.null(oxygen))
  if (has_numeric) {
    keep_num <- vapply(xs, function(v){
      .keep_by_numeric_chain(v, carbon = carbon, db = db, oxygen = oxygen)
    }, logical(1))
    keep <- keep & keep_num
  }
  
  se[keep, , drop = FALSE]
}

# ---- Plot fun resolution -----------------------------------------------------
get_plot_fun <- function(plot_type){
  plot_type <- match.arg(plot_type, c("dot","violin","box"))
  if (plot_type == "dot"    && exists("plot_dot_se", inherits = TRUE))    return(get("plot_dot_se", inherits=TRUE))
  if (plot_type == "violin" && exists("plot_violin_se", inherits = TRUE)) return(get("plot_violin_se", inherits=TRUE))
  if (plot_type == "box"    && exists("plot_box_se", inherits = TRUE))    return(get("plot_box_se", inherits=TRUE))
  
  # fallback: basic dot plot
  function(se, feature_id, x_var="class", x_order=NULL, palette=NULL, ...) {
    se_cls <- aggregate_to_class_se(se, "sum")
    A <- as.matrix(SummarizedExperiment::assay(se_cls, 1))
    if (!feature_id %in% rownames(A)) {
      ggplot2::ggplot() + ggplot2::annotate("text", x=0, y=0, label=paste("No data:", feature_id)) + ggplot2::theme_void()
    } else {
      df <- data.frame(value = as.numeric(A[feature_id, ]), sample = colnames(A))
      cd <- as.data.frame(SummarizedExperiment::colData(se_cls))
      df$group <- cd[["class"]] %||% cd[["Class"]] %||% "G"
      
      if (!is.null(x_order) && length(x_order)) {
        df$group <- factor(as.character(df$group), levels = x_order)
      } else {
        df$group <- as.factor(df$group)
      }
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = value, color = group)) +
        ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.15), alpha = 0.85) +
        ggplot2::stat_summary(fun = stats::median, geom = "crossbar", width = 0.6, fatten = 0.8) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::labs(x = "Class", y = "Abundance", title = paste("Class:", feature_id))
      
      if (!is.null(palette) && length(palette)) {
        p <- p + ggplot2::scale_color_manual(values = palette, drop = FALSE)
      }
      p
    }
  }
}

# ---- Style/palette helpers ---------------------------------------------------
.default_palette <- function(n) {
  hues <- seq(15, 375, length.out = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

.get_x_groups <- function(se) {
  cd <- as.data.frame(SummarizedExperiment::colData(se))
  if ("class" %in% names(cd)) return(unique(as.character(cd$class)))
  if ("Class" %in% names(cd)) return(unique(as.character(cd$Class)))
  character(0)
}

.get_adv_or_default <- function(se, settings_reactive = NULL) {
  adv <- if (!is.null(settings_reactive)) settings_reactive() else NULL
  if (!is.null(adv)) return(adv)
  
  groups <- .get_x_groups(se)
  pal <- if (length(groups)) { p <- .default_palette(length(groups)); names(p) <- groups; p } else character(0)
  
  list(
    manual_order = character(0),
    palette_map  = pal,
    
    add_p       = FALSE,
    test        = "wilcox",
    comparisons = NULL,
    ref_group   = NULL,
    p_adjust    = "BH",
    p_label     = "both",
    
    dot_point_size   = 2.0,
    dot_jitter_width = 0.15,
    dot_alpha        = 0.9,
    dot_show_median  = TRUE,
    dot_median_size  = 0.7,
    dot_median_width = 0.6,
    dot_median_color = "#222222",
    
    box_width        = 0.6,
    box_alpha        = 0.6,
    box_show_points  = TRUE,
    box_point_size   = 1.6,
    box_jitter_width = 0.12,
    box_point_alpha  = 0.8,
    
    violin_width        = 0.8,
    violin_alpha        = 0.55,
    violin_trim         = TRUE,
    violin_show_points  = TRUE,
    violin_point_size   = 1.6,
    violin_jitter_width = 0.12,
    violin_point_alpha  = 0.8,
    violin_show_median  = TRUE,
    violin_median_size  = 0.7,
    violin_median_color = "#222222"
  )
}

.plot_style_args_from_adv <- function(adv, plot_type) {
  plot_type <- match.arg(plot_type, c("dot","violin","box"))
  if (plot_type == "dot") {
    return(list(
      point_size   = adv$dot_point_size,
      jitter_width = adv$dot_jitter_width,
      point_alpha  = adv$dot_alpha,
      show_median  = adv$dot_show_median,
      median_size  = adv$dot_median_size,
      median_width = adv$dot_median_width,
      median_color = adv$dot_median_color
    ))
  }
  if (plot_type == "box") {
    return(list(
      box_width    = adv$box_width,
      box_alpha    = adv$box_alpha,
      show_points  = adv$box_show_points,
      point_size   = adv$box_point_size,
      jitter_width = adv$box_jitter_width,
      point_alpha  = adv$box_point_alpha
    ))
  }
  list(
    violin_width = adv$violin_width,
    violin_alpha = adv$violin_alpha,
    trim         = adv$violin_trim,
    show_points  = adv$violin_show_points,
    point_size   = adv$violin_point_size,
    jitter_width = adv$violin_jitter_width,
    point_alpha  = adv$violin_point_alpha,
    show_median  = adv$violin_show_median,
    median_size  = adv$violin_median_size,
    median_color = adv$violin_median_color
  )
}

.pvalue_args_from_adv <- function(adv) {
  list(
    add_p       = isTRUE(adv$add_p),
    test        = adv$test        %||% "wilcox",
    comparisons = adv$comparisons %||% NULL,
    ref_group   = adv$ref_group   %||% NULL,
    p_adjust    = adv$p_adjust    %||% "BH",
    p_label     = adv$p_label     %||% "both"
  )
}

.has_scale <- function(p, aes_name) {
  if (!inherits(p, c("gg","ggplot"))) return(FALSE)
  if (is.null(p$scales) || is.null(p$scales$scales)) return(FALSE)
  any(vapply(p$scales$scales, function(s) {
    if (is.null(s$aesthetics)) FALSE else aes_name %in% s$aesthetics
  }, logical(1)))
}

.apply_palette_if_missing <- function(p, pal) {
  if (!inherits(p, c("gg","ggplot"))) return(p)
  if (is.null(pal) || !length(pal)) return(p)
  if (!.has_scale(p, "fill"))  p <- p + ggplot2::scale_fill_manual(values = pal, drop = FALSE)
  if (!.has_scale(p, "colour") && !.has_scale(p, "color")) p <- p + ggplot2::scale_color_manual(values = pal, drop = FALSE)
  p
}

# ---- Ensembl mapping ---------------------------------------------------------
.make_ensembl_to_rowname_map <- function(se_tx) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se_tx))
  cand <- c("EnsemblID","Ensembl_ID","ensembl_id","ENSEMBL","Ensembl")
  col  <- cand[cand %in% names(rd)][1]
  
  if (!is.na(col) && nzchar(col)) {
    ens <- as.character(rd[[col]])
    rn  <- rownames(rd)
  } else {
    ens <- rownames(rd)
    rn  <- rownames(rd)
  }
  
  ens <- trimws(ens)
  ens <- sub("\\..*$", "", ens)
  ok  <- !is.na(ens) & nzchar(ens)
  
  ens <- ens[ok]; rn <- rn[ok]
  keep <- !duplicated(ens)
  stats::setNames(rn[keep], ens[keep])
}

# ---- Network import parsers --------------------------------------------------
# cyjs/json -> Cytoscape elements (KEEP existing path/hm_path if present)
.cyjs_to_elements <- function(obj) {
  els <- if (!is.null(obj$elements)) obj$elements else obj
  if (is.null(els$nodes)) els$nodes <- list()
  if (is.null(els$edges)) els$edges <- list()
  
  nodes <- lapply(els$nodes, function(n) {
    d <- n$data %||% list()
    lab <- d$label %||% d$name %||% d$id %||% ""
    d$label <- lab
    out <- list(data = d)
    if (!is.null(n$position)) out$position <- n$position
    out
  })
  
  edges <- lapply(seq_along(els$edges), function(i) {
    e <- els$edges[[i]]
    d <- e$data %||% list()
    if (is.null(d$id)) d$id <- paste0("e", i)
    list(data = d)
  })
  
  list(nodes = nodes, edges = edges)
}

.read_lines <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")

.parse_gml_yed <- function(path) {
  lines <- .read_lines(path)
  lines <- gsub("#.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  
  collect_blocks <- function(keyword) {
    blocks <- list()
    i <- 1
    n <- length(lines)
    while (i <= n) {
      if (grepl(paste0("^", keyword, "\\s*\\["), lines[i])) {
        depth <- 0L
        start <- i
        j <- i
        while (j <= n) {
          depth <- depth + stringr::str_count(lines[j], "\\[") - stringr::str_count(lines[j], "\\]")
          if (j > start && depth <= 0L) break
          j <- j + 1
        }
        blocks[[length(blocks) + 1L]] <- lines[start:j]
        i <- j + 1
      } else {
        i <- i + 1
      }
    }
    blocks
  }
  
  get_num <- function(x) suppressWarnings(as.numeric(stringr::str_extract(x, "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")))
  get_str <- function(x) {
    m <- stringr::str_match(x, "\"(.*)\"")
    if (!is.na(m[1,2])) return(m[1,2])
    m2 <- stringr::str_match(x, "\\s([^\\s]+)$")
    if (!is.na(m2[1,2])) return(m2[1,2])
    ""
  }
  
  node_blocks <- collect_blocks("node")
  edge_blocks <- collect_blocks("edge")
  
  nodes <- lapply(node_blocks, function(bl) {
    id_line  <- bl[grepl("^id\\s+", bl)][1] %||% ""
    lab_line <- bl[grepl("^label\\s+", bl)][1] %||% ""
    
    gx <- bl[grepl("^x\\s+", bl)][1] %||% ""
    gy <- bl[grepl("^y\\s+", bl)][1] %||% ""
    gw <- bl[grepl("^w\\s+", bl)][1] %||% ""
    gh <- bl[grepl("^h\\s+", bl)][1] %||% ""
    
    idv <- get_str(id_line)
    if (!nzchar(idv)) idv <- paste0("n", sample.int(1e9, 1))
    
    lab <- get_str(lab_line)
    if (!nzchar(lab)) lab <- idv
    
    x <- get_num(gx); y <- get_num(gy)
    w <- get_num(gw); h <- get_num(gh)
    
    d <- list(
      id = as.character(idv),
      label = as.character(lab),
      shared_name = as.character(lab),
      Width = if (is.finite(w)) w else 180,
      Height = if (is.finite(h)) h else 140
    )
    
    out <- list(data = d)
    if (is.finite(x) && is.finite(y)) out$position <- list(x = x, y = y)
    out
  })
  
  edges <- lapply(seq_along(edge_blocks), function(i) {
    bl <- edge_blocks[[i]]
    src_line <- bl[grepl("^source\\s+", bl)][1] %||% ""
    tgt_line <- bl[grepl("^target\\s+", bl)][1] %||% ""
    src <- get_str(src_line); tgt <- get_str(tgt_line)
    if (!nzchar(src) || !nzchar(tgt)) return(NULL)
    list(data = list(id = paste0("e", i), source = as.character(src), target = as.character(tgt)))
  })
  edges <- Filter(Negate(is.null), edges)
  
  list(nodes = nodes, edges = edges)
}

.parse_gpml <- function(path) {
  doc <- xml2::read_xml(path)
  
  dnodes <- xml2::xml_find_all(doc, ".//DataNode")
  nodes <- lapply(dnodes, function(n) {
    gid <- xml2::xml_attr(n, "GraphId") %||% xml2::xml_attr(n, "GraphID") %||% xml2::xml_attr(n, "graphId")
    if (is.null(gid) || !nzchar(gid)) gid <- paste0("dn_", sample.int(1e9, 1))
    
    lab <- xml2::xml_attr(n, "TextLabel") %||% xml2::xml_attr(n, "textlabel") %||% ""
    if (!nzchar(lab)) {
      lab <- xml2::xml_text(xml2::xml_find_first(n, "./Label")) %||% ""
    }
    if (!nzchar(lab)) lab <- gid
    
    g <- xml2::xml_find_first(n, "./Graphics")
    cx <- suppressWarnings(as.numeric(xml2::xml_attr(g, "CenterX")))
    cy <- suppressWarnings(as.numeric(xml2::xml_attr(g, "CenterY")))
    w  <- suppressWarnings(as.numeric(xml2::xml_attr(g, "Width")))
    h  <- suppressWarnings(as.numeric(xml2::xml_attr(g, "Height")))
    
    d <- list(
      id = as.character(gid),
      label = as.character(lab),
      shared_name = as.character(lab),
      Width  = if (is.finite(w)) w else 180,
      Height = if (is.finite(h)) h else 140
    )
    
    out <- list(data = d)
    if (is.finite(cx) && is.finite(cy)) out$position <- list(x = cx, y = cy)
    out
  })
  
  ints <- xml2::xml_find_all(doc, ".//Interaction")
  edges <- list()
  k <- 0L
  for (it in ints) {
    pts <- xml2::xml_find_all(it, ".//Graphics//Point")
    refs <- xml2::xml_attr(pts, "GraphRef")
    refs <- refs[!is.na(refs) & nzchar(refs)]
    if (length(refs) < 2) next
    src <- refs[[1]]
    tgt <- refs[[length(refs)]]
    k <- k + 1L
    edges[[k]] <- list(data = list(id = paste0("e", k), source = as.character(src), target = as.character(tgt)))
  }
  
  list(nodes = nodes, edges = edges)
}

# ---- Position handling -------------------------------------------------------
.any_node_has_pos <- function(el) {
  if (is.null(el) || is.null(el$nodes)) return(FALSE)
  any(vapply(el$nodes, function(n) !is.null(n$position) &&
               is.finite(suppressWarnings(as.numeric(n$position$x))) &&
               is.finite(suppressWarnings(as.numeric(n$position$y))), logical(1)))
}

.fill_missing_positions <- function(el) {
  if (is.null(el) || is.null(el$nodes)) return(el)
  
  xs <- vapply(el$nodes, function(n) {
    if (is.null(n$position)) return(NA_real_)
    suppressWarnings(as.numeric(n$position$x))
  }, numeric(1))
  ys <- vapply(el$nodes, function(n) {
    if (is.null(n$position)) return(NA_real_)
    suppressWarnings(as.numeric(n$position$y))
  }, numeric(1))
  
  has <- is.finite(xs) & is.finite(ys)
  if (!any(has)) return(el)
  
  cx <- mean(xs[has]); cy <- mean(ys[has])
  rx <- stats::sd(xs[has]); ry <- stats::sd(ys[has])
  r <- max(200, 2.0 * max(rx, ry, na.rm = TRUE))
  
  miss <- which(!has)
  if (!length(miss)) return(el)
  
  m <- length(miss)
  ang <- seq(0, 2*pi, length.out = m + 1)[1:m]
  
  for (j in seq_along(miss)) {
    i <- miss[[j]]
    el$nodes[[i]]$position <- list(
      x = cx + r * cos(ang[[j]]),
      y = cy + r * sin(ang[[j]])
    )
  }
  el
}

.ensure_positions_for_preset <- function(el) {
  if (.any_node_has_pos(el)) .fill_missing_positions(el) else el
}
