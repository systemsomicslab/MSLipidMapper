# api_static.R -----------------------------------------------------------------
# API: (A) /static にローカルSVGをマウント（ネットワーク背景用）
#      (B) /plot_class.svg /plot_molecule.svg を提供（ノード選択プレビュー用）

suppressPackageStartupMessages({
  requireNamespace("plumber")
  requireNamespace("httpuv")
})

`%||%` <- function(a,b) if (is.null(a) || (is.character(a) && length(a) == 0)) b else a

.check_pkgs <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "))
}

# plumber のバージョン差吸収して /static をマウント
.mount_static <- function(pr, static_dir, path = "/static") {
  fm <- try(names(formals(plumber::pr_static)), silent = TRUE)
  if (!inherits(fm, "try-error") && length(fm) && fm[1] %in% c("dir","path")) {
    pr$mount(path, plumber::pr_static(dir = static_dir))
    return(pr)
  }
  f <- get("pr_static", asNamespace("plumber"))
  f(pr, dir = static_dir, path = path)
}

# ---------------- (Optional) Plot helpers ----------------
.theme_lipidomics <- function(base_size = 12, x_angle = 0,
                              axis_fontsize = base_size,
                              legend_fontsize = base_size,
                              title_fontsize = base_size + 2) {
  ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = title_fontsize),
      axis.text.x  = ggplot2::element_text(angle = x_angle, size = axis_fontsize,
                                           vjust = ifelse(x_angle == 0, 0.5, 1),
                                           hjust = ifelse(x_angle == 0, 0.5, 1)),
      axis.text.y  = ggplot2::element_text(size = axis_fontsize),
      axis.title   = ggplot2::element_text(size = title_fontsize),
      legend.text  = ggplot2::element_text(size = legend_fontsize),
      legend.title = ggplot2::element_text(size = legend_fontsize, face = "bold")
    )
}

.resolve_class_col <- function(se) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  keys <- c("class","lipid_class","Class","LipidClass","subclass","ontology")
  hit  <- keys[keys %in% names(rd)]
  if (!length(hit)) stop("rowData(se) に class/lipid_class/subclass/ontology 等の列がありません。")
  hit[1]
}

.aggregate_to_class_se <- function(se, class_col, fun = c("sum","mean","median")) {
  fun <- match.arg(fun)
  rd  <- as.data.frame(SummarizedExperiment::rowData(se))
  grp <- as.character(rd[[class_col]]); grp[!nzchar(grp) | is.na(grp)] <- "UNK"
  mat <- as.matrix(SummarizedExperiment::assay(se, 1)); rownames(mat) <- rownames(rd)
  if (fun == "sum") {
    agg <- rowsum(mat, group = grp, reorder = FALSE, na.rm = TRUE)
  } else {
    idx_list <- split(seq_len(nrow(mat)), grp)
    agg <- vapply(idx_list, function(idx){
      X <- mat[idx, , drop = FALSE]
      if (fun == "mean") colMeans(X, na.rm = TRUE) else apply(X, 2, stats::median, na.rm = TRUE)
    }, FUN.VALUE = numeric(ncol(mat)))
    agg <- t(agg)
  }
  SummarizedExperiment::SummarizedExperiment(
    assays  = list(abundance = agg),
    rowData = S4Vectors::DataFrame(feature = rownames(agg)),
    colData = SummarizedExperiment::colData(se)
  )
}

.top_feature_in_class <- function(se, class_col, class_name) {
  rd <- as.data.frame(SummarizedExperiment::rowData(se))
  ids <- rownames(rd)[as.character(rd[[class_col]]) == class_name]
  if (!length(ids)) return(NA_character_)
  tidy <- try(as_long_table(se, level_label = "molecule"), silent = TRUE)
  if (inherits(tidy, "try-error")) return(ids[1])
  tidy$abundance <- suppressWarnings(as.numeric(tidy$abundance))
  tidy <- tidy[is.finite(tidy$abundance), , drop = FALSE]
  if (!nrow(tidy)) return(ids[1])
  top <- tidy |>
    dplyr::filter(.data$feature_id %in% ids) |>
    dplyr::group_by(.data$feature_id) |>
    dplyr::summarise(mu = mean(.data$abundance, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$mu)) |>
    dplyr::slice_head(n = 1)
  if (nrow(top)) as.character(top$feature_id[1]) else ids[1]
}

.build_comp_args <- function(mode, ref_group, manual_df) {
  if (identical(mode, "all"))  return(list(comparisons = NULL, ref_group = NULL))
  if (identical(mode, "ref") && nzchar(ref_group))
    return(list(comparisons = NULL, ref_group = ref_group))
  if (identical(mode, "manual") && !is.null(manual_df) && nrow(manual_df)) {
    df <- manual_df
    df <- df[stats::complete.cases(df[, c("group1","group2")]), , drop = FALSE]
    df <- df[nzchar(df$group1) & nzchar(df$group2), , drop = FALSE]
    if (nrow(df)) {
      pairs <- lapply(seq_len(nrow(df)), function(i) c(as.character(df$group1[i]), as.character(df$group2[i])))
      return(list(comparisons = pairs, ref_group = NULL))
    }
  }
  list(comparisons = NULL, ref_group = NULL)
}

.save_plot_svg_text <- function(p, width = 12, height = 10) {
  tf <- tempfile(fileext = ".svg")
  svglite::svglite(tf, width = width, height = height, bg = "white")
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
  print(p)
  try(grDevices::dev.off(), silent = TRUE)
  paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ---------------- Main ----------------
# ---------------- Main ----------------
start_inline_api2_static <- function(
    se_provider,
    state_provider,
    static_dir,
    host = "127.0.0.1",
    port = 7301,
    verbose = TRUE,
    enable_plot_endpoints = TRUE,
    heatmap_dir = NULL  # ★ 追加: ヒートマップ用ディレクトリ（NULLなら static_dir と同じ）
) {
  stopifnot(is.function(se_provider), is.function(state_provider))
  static_dir  <- normalizePath(static_dir, winslash = "/", mustWork = FALSE)
  heatmap_dir <- (heatmap_dir %||% static_dir)
  heatmap_dir <- normalizePath(heatmap_dir, winslash = "/", mustWork = FALSE)

  .check_pkgs(c("plumber","httpuv"))
  if (isTRUE(enable_plot_endpoints)) {
    .check_pkgs(c("SummarizedExperiment","S4Vectors","ggplot2","svglite"))
  }

  pr <- plumber::pr()

  # ---- CORS (+ preflight) ----
  pr$filter("cors", function(req, res) {
    res$setHeader("Access-Control-Allow-Origin",  "*")
    res$setHeader("Access-Control-Allow-Methods", "GET, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Requested-With")
    res$setHeader("Access-Control-Max-Age",      "86400")
    if (identical(req$REQUEST_METHOD, "OPTIONS")) {
      res$status <- 204
      return(list())
    }
    plumber::forward()
  })

  # ---- Cache（常に上書き：getHeader は使わない）----
  pr$filter("cache", function(req, res) {
    res$setHeader("Cache-Control", "public, max-age=15, must-revalidate")
    plumber::forward()
  })

  # ---- メタ ----
  pr$handle("GET", "/ping",    function(){ list(ok = TRUE) })
  pr$handle("GET", "/healthz", function(){
    se <- try(se_provider(), silent = TRUE)
    list(ok = TRUE, se_ready = (!inherits(se, "try-error") && !is.null(se)))
  })
  pr$handle("GET", "/status", function(){
    st <- state_provider()
    list(
      live = st$live %||% NULL,
      adv  = if (!is.null(st$adv)) {
        a <- st$adv
        a$manual_pairs <- if (is.null(a$manual_pairs)) NULL else utils::head(a$manual_pairs, 5L)
        a
      } else NULL,
      static_dir  = static_dir,
      heatmap_dir = heatmap_dir   # ★ ここも返しておくとデバッグに便利
    )
  })

  # ---- static mount ----
  if (!dir.exists(static_dir)) dir.create(static_dir, recursive = TRUE, showWarnings = FALSE)
  pr <- .mount_static(pr, static_dir, path = "/static")
  # 例: http://127.0.0.1:7311/static/class_PC.svg

  # ---- heatmap mount （別ディレクトリを使う場合のみ）----
  if (!dir.exists(heatmap_dir)) dir.create(heatmap_dir, recursive = TRUE, showWarnings = FALSE)

  # static_dir と heatmap_dir が同じなら /static からそのまま提供されるので、
  # わざわざ /heatmap を作らない
  if (!identical(static_dir, heatmap_dir)) {
    pr <- .mount_static(pr, heatmap_dir, path = "/heatmap")
    # 例: http://127.0.0.1:7311/heatmap/hm_class_PC.svg
  }

  # ---- /plot_*.svg ----
  if (isTRUE(enable_plot_endpoints)) {

    pr$handle(
      "GET", "/plot_class.svg",
      function(lipid_class = NULL, plot_type = NULL, agg_fun = NULL) {
        se <- se_provider(); if (is.null(se)) plumber::halt(503, "SE not ready.")
        st <- state_provider(); live <- st$live %||% list()
        plot_type <- (plot_type %||% live$plot_type) %||% "violin"
        plot_type <- match.arg(plot_type, c("violin","box","dot"))
        agg_fun   <- (agg_fun   %||% live$agg_fun)   %||% "sum"
        agg_fun   <- match.arg(agg_fun, c("sum","mean","median"))
        lipid_class <- (lipid_class %||% live$lipid_class)
        if (is.null(lipid_class) || !nzchar(lipid_class)) plumber::halt(400, "lipid_class is required")

        class_col <- .resolve_class_col(se)
        se_cls <- .aggregate_to_class_se(se, class_col, fun = agg_fun)

        fun_ok <- exists("plot_dot_se", inherits = TRUE)
        p <- if (fun_ok) {
          fun <- switch(plot_type,
                        violin = get("plot_violin_se", inherits = TRUE),
                        box    = get("plot_box_se",    inherits = TRUE),
                        dot    = get("plot_dot_se",    inherits = TRUE))
          do.call(fun, c(list(se = se_cls, feature_id = lipid_class, x_var = "class", add_p = FALSE))) +
            .theme_lipidomics(12, 0, 12, 12, 14) +
            ggplot2::theme(legend.position = "none", aspect.ratio = 1) +
            ggplot2::labs(title = paste0("Class: ", lipid_class))
        } else {
          ggplot2::ggplot(data.frame(x=1:10, y=rnorm(10)),
                          ggplot2::aes(x, y)) +
            ggplot2::geom_point() +
            ggplot2::labs(title = paste0("Class: ", lipid_class)) +
            .theme_lipidomics()
        }
        .save_plot_svg_text(p)
      },
      serializer = plumber::serializer_content_type("image/svg+xml; charset=utf-8")
    )

    pr$handle(
      "GET", "/plot_molecule.svg",
      function(feature_id = NULL, lipid_class = NULL, plot_type = NULL) {
        se <- se_provider(); if (is.null(se)) plumber::halt(503, "SE not ready.")
        st <- state_provider(); live <- st$live %||% list()
        plot_type <- (plot_type %||% live$plot_type) %||% "violin"
        plot_type <- match.arg(plot_type, c("violin","box","dot"))
        class_col <- .resolve_class_col(se)

        if (is.null(feature_id) || !nzchar(feature_id)) {
          lipid_class <- (lipid_class %||% live$lipid_class)
          if (is.null(lipid_class) || !nzchar(lipid_class))
            plumber::halt(400, "feature_id または lipid_class が必要です")
          feature_id <- .top_feature_in_class(se, class_col, lipid_class)
          if (is.na(feature_id)) plumber::halt(404, "指定クラスに分子がありません")
        }

        fun_ok <- exists("plot_dot_se", inherits = TRUE)
        p <- if (fun_ok) {
          fun <- switch(plot_type,
                        violin = get("plot_violin_se", inherits = TRUE),
                        box    = get("plot_box_se",    inherits = TRUE),
                        dot    = get("plot_dot_se",    inherits = TRUE))
          do.call(fun, c(list(se = se, feature_id = feature_id, x_var = "class", add_p = FALSE))) +
            .theme_lipidomics(11, 0, 10, 10, 12) +
            ggplot2::theme(legend.position = "none", aspect.ratio = 1) +
            ggplot2::labs(title = paste0("Molecule: ", feature_id))
        } else {
          ggplot2::ggplot(data.frame(x=1:10, y=rnorm(10)),
                          ggplot2::aes(x, y)) +
            ggplot2::geom_point() +
            ggplot2::labs(title = paste0("Molecule: ", feature_id)) +
            .theme_lipidomics()
        }
        .save_plot_svg_text(p)
      },
      serializer = plumber::serializer_content_type("image/svg+xml; charset=utf-8")
    )
  }

  # ---- start server ----
  srv <- httpuv::startServer(host, port, list(call = pr$call))
  if (isTRUE(verbose)) {
    message(sprintf(">> API listening at: http://%s:%d/", host, port))
    message(sprintf(">> Static mount:    http://%s:%d/static/  (dir: %s)", host, port, static_dir))
    if (!identical(static_dir, heatmap_dir)) {
      message(sprintf(">> Heatmap mount:   http://%s:%d/heatmap/ (dir: %s)", host, port, heatmap_dir))
    } else {
      message(">> Heatmaps: served from /static/ (same dir as static_dir).")
    }
    if (isTRUE(enable_plot_endpoints)) {
      message(">> Endpoints: /plot_class.svg , /plot_molecule.svg , /healthz , /status")
    }
  }

  stop_api <- local({
    s <- srv
    function() {
      if (!is.null(s)) {
        try(httpuv::stopServer(s), silent = TRUE)
        s <<- NULL
        if (isTRUE(verbose)) message(">> API stopped.")
      }
    }
  })
  if (requireNamespace("shiny", quietly = TRUE)) {
    try(shiny::onStop(stop_api), silent = TRUE)
  }

  structure(list(server = srv, router = pr, stop = stop_api,
                 host = host, port = port,
                 static_dir = static_dir,
                 heatmap_dir = heatmap_dir),
            class = "inline_api_handle")
}


