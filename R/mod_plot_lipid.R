# R/mod_plot_lipid.R ---------------------------------------------------------
# Plot module server (Lipid only, no PCA)
#   - adv_reactive: reactive() returning shared advanced settings list
#   - on_live_change: optional callback to notify outside modules (e.g., Network)
#   - on_open_adv: optional callback to open shared Advanced modal
#
# Added in this version:
#   - Acyl-chain filtering (chain code multi-select)
#   - "Require ALL selected chain codes" (AND/OR switch)
#   - "Remove odd-carbon chains" checkbox
#   - Auto-OFF if rowData has no valid acyl_chains
#   - Heatmap card header text updated + toggle label "Bar plot"
#   - Bar plot uses the same palette logic as other plots (adv$palette_map)
#   - FIX: top_n slicing no longer uses dplyr::n() outside data-masking verbs
#   - NEW: Bar plot adds extra bottom margin (and supports ggsave safety via coord_cartesian(clip="off"))
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(S4Vectors)
  library(methods)
  # optional (used in downloads / combined)
  # library(ComplexHeatmap)
  # library(grid)
  # library(patchwork)
  # library(svglite)
})



#' Plot module server for Lipid features
#'
#' @param id            module id
#' @param se_in         reactive() SummarizedExperiment (lipid)
#' @param adv_reactive  reactive() list (advanced settings)
#' @param on_live_change function(list) | NULL
#' @param on_open_adv    function()    | NULL
#'
#' @export
mod_plot_lipid_server <- function(
    id,
    se_in,
    adv_reactive,
    on_live_change = NULL,
    on_open_adv    = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    `%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 0)) b else a
    
    .resolve_class_col <- function(se) {
      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      keys <- c("class","lipid_class","Class","LipidClass","subclass","ontology","Ontology")
      hit  <- keys[keys %in% colnames(rd)]
      if (!length(hit)) stop("rowData(se) must contain a class column (class/lipid_class/subclass/ontology/Ontology etc.).")
      hit[1]
    }
    
    .resolve_label_col <- function(se) {
      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      cn <- colnames(rd)
      cand <- c("Metabolite name", "Metabolite", "Name","Metabolite.name")
      hit  <- cand[cand %in% cn][1]
      if (length(hit)) hit else cn[1]
    }
    
    .get_x_groups <- function(se) {
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!"class" %in% colnames(cd)) return(character(0))
      unique(as.character(cd$class))
    }
    
    aggregate_to_class_se <- function(se, class_col, fun = c("sum","mean","median"), assay_name = NULL) {
      fun <- match.arg(fun)
      rd  <- as.data.frame(SummarizedExperiment::rowData(se))
      grp <- as.character(rd[[class_col]])
      grp[!nzchar(grp) | is.na(grp)] <- "UNK"
      
      mat <- if (is.null(assay_name)) as.matrix(SummarizedExperiment::assay(se, 1))
      else as.matrix(SummarizedExperiment::assay(se, assay_name))
      rownames(mat) <- rownames(rd)
      
      if (fun == "sum") {
        agg <- rowsum(mat, group = grp, reorder = FALSE, na.rm = TRUE)
      } else {
        idx_list <- split(seq_len(nrow(mat)), grp)
        agg <- vapply(idx_list, function(idx) {
          X <- mat[idx, , drop = FALSE]
          if (fun == "mean") colMeans(X, na.rm = TRUE)
          else apply(X, 2, stats::median, na.rm = TRUE)
        }, FUN.VALUE = numeric(ncol(mat)))
        agg <- t(agg)
      }
      
      SummarizedExperiment::SummarizedExperiment(
        assays  = list(abundance = agg),
        rowData = S4Vectors::DataFrame(feature = rownames(agg)),
        colData = SummarizedExperiment::colData(se)
      )
    }
    
    .pick_molecule_in_class <- function(se, class_col, class_name, preferred_id = NULL) {
      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      in_class <- rownames(rd)[as.character(rd[[class_col]]) == class_name]
      if (!length(in_class)) return(NA_character_)
      
      if (!is.null(preferred_id) && nzchar(preferred_id) && preferred_id %in% in_class) {
        return(preferred_id)
      }
      
      if (!exists(".tidy_from_se_global", mode = "function")) return(in_class[1])
      
      tidy <- .tidy_from_se_global(se)
      
      if (!requireNamespace("dplyr", quietly = TRUE)) return(in_class[1])
      
      top <- tidy |>
        dplyr::filter(.data$feature_id %in% in_class) |>
        dplyr::group_by(.data$feature_id) |>
        dplyr::summarise(mu = mean(.data$abundance, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$mu)) |>
        dplyr::slice_head(n = 1)
      
      if (nrow(top)) top$feature_id[1] else in_class[1]
    }
    
    .safe_jitter <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x) || x < 0) 0 else x
    }
    
    .build_comp_args <- function(mode, ref_group, manual_df) {
      if (identical(mode, "all"))
        return(list(comparisons = NULL, ref_group = NULL))
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
    
    .make_palette <- function(groups, adv) {
      groups <- unique(groups)
      n <- length(groups)
      if (!n) return(NULL)
      
      hues <- seq(15, 375, length.out = n + 1)[1:n]
      pal  <- grDevices::hcl(h = hues, l = 65, c = 100)
      names(pal) <- groups
      
      if (!is.null(adv$palette_map) && length(adv$palette_map)) {
        nm <- intersect(names(adv$palette_map), groups)
        pal[nm] <- adv$palette_map[nm]
      }
      pal
    }
    
    # ==== Acyl-chain filtering helpers =================================
    .has_acyl_chains <- function(se, chain_col = "acyl_chains") {
      rd <- SummarizedExperiment::rowData(se)
      chain_col %in% colnames(rd)
    }
    
    .parse_chain_string <- function(s) {
      s <- as.character(s)
      if (is.na(s) || !nzchar(s)) return(character(0))
      parts <- unlist(strsplit(s, "[,;|\\s]+", perl = TRUE), use.names = FALSE)
      parts <- trimws(parts)
      parts[!is.na(parts) & nzchar(parts)]
    }
    
    .get_chain_list <- function(se, chain_col = "acyl_chains") {
      rd <- SummarizedExperiment::rowData(se)
      if (!(chain_col %in% colnames(rd))) return(NULL)
      
      x <- rd[[chain_col]]
      
      if (methods::is(x, "List")) {
        lst <- as.list(x)
      } else if (is.list(x)) {
        lst <- as.list(x)
      } else if (is.character(x)) {
        lst <- lapply(x, .parse_chain_string)
      } else {
        return(NULL)
      }
      
      lst <- lapply(lst, function(v) {
        if (is.null(v)) return(character(0))
        vv <- as.character(v)
        vv <- vv[!is.na(vv) & nzchar(vv)]
        trimws(vv)
      })
      lst
    }
    
    .get_chain_choices <- function(se, chain_col = "acyl_chains") {
      lst <- .get_chain_list(se, chain_col = chain_col)
      if (is.null(lst)) return(character(0))
      u <- unique(unlist(lst, use.names = FALSE))
      u <- u[!is.na(u) & nzchar(u)]
      sort(u)
    }
    
    .row_has_odd_carbon_chain <- function(chains_vec) {
      if (!length(chains_vec)) return(FALSE)
      carb <- suppressWarnings(as.integer(sub(":.*$", "", chains_vec)))
      any(!is.na(carb) & (carb %% 2L == 1L))
    }
    
    .filter_se_by_chain_codes <- function(se,
                                          selected_codes = character(0),
                                          require_all = FALSE,
                                          remove_odd = FALSE,
                                          chain_col = "acyl_chains") {
      if (is.null(se) || !methods::is(se, "SummarizedExperiment")) return(se)
      
      chains_list <- .get_chain_list(se, chain_col = chain_col)
      if (is.null(chains_list)) return(se)
      
      selected_codes <- as.character(selected_codes %||% character(0))
      selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
      require_all <- isTRUE(require_all)
      remove_odd  <- isTRUE(remove_odd)
      
      keep <- rep(TRUE, length(chains_list))
      
      if (length(selected_codes)) {
        if (require_all) {
          keep <- vapply(chains_list, function(chs) all(selected_codes %in% chs), logical(1))
        } else {
          keep <- vapply(chains_list, function(chs) any(selected_codes %in% chs), logical(1))
        }
      }
      
      if (remove_odd) {
        has_odd <- vapply(chains_list, .row_has_odd_carbon_chain, logical(1))
        keep <- keep & !has_odd
      }
      
      se[keep, , drop = FALSE]
    }
    
    # ==== Advanced button -> open shared modal =========================
    observeEvent(input$open_adv, {
      if (is.function(on_open_adv)) on_open_adv()
    }, ignoreInit = TRUE)
    
    # ==== Top-N molecules: sync adv -> UI ==============================
    observeEvent(adv_reactive(), {
      adv <- adv_reactive()
      topN <- adv$hm_topN %||% 40L
      if (!is.null(input$n_top_mol) && identical(as.integer(input$n_top_mol), as.integer(topN))) return()
      updateNumericInput(session, "n_top_mol", value = as.integer(topN))
    }, ignoreInit = TRUE)
    
    observeEvent(input$n_top_mol, {
      val <- input$n_top_mol
      if (!is.null(val) && is.finite(val) && val > 0) {
        if (is.function(on_live_change)) on_live_change(list(hm_topN = as.integer(val)))
      }
    }, ignoreInit = TRUE)
    
    # ==== Chain filter UI (auto-OFF if invalid) ========================
    output$chain_filter_ui <- renderUI({
      se <- se_in()
      if (is.null(se) || !methods::is(se, "SummarizedExperiment")) {
        return(tags$div(style = "font-size:12px; color:#777;", "Chain filtering: (SE not ready)"))
      }
      
      if (!.has_acyl_chains(se, "acyl_chains")) {
        return(tags$div(style = "font-size:12px; color:#777;",
                        "Chain filtering is OFF (rowData has no 'acyl_chains')."))
      }
      
      choices <- .get_chain_choices(se, "acyl_chains")
      if (!length(choices)) {
        return(tags$div(style = "font-size:12px; color:#777;",
                        "Chain filtering is OFF ('acyl_chains' format is not supported / empty)."))
      }
      
      sel <- isolate(input$chain_codes)
      sel <- sel[sel %in% choices]
      
      tagList(
        selectizeInput(
          ns("chain_codes"),
          "Chain codes (multi-select)",
          choices  = choices,
          selected = sel,
          multiple = TRUE,
          options = list(
            placeholder = "e.g., 18:0, 22:6;O2 ...",
            maxOptions  = 5000,
            plugins     = list("remove_button")
          )
        ),
        checkboxInput(
          ns("chain_require_all"),
          "Require ALL selected chain codes (AND)",
          value = isTRUE(isolate(input$chain_require_all))
        ),
        checkboxInput(
          ns("chain_remove_odd"),
          "Remove molecules with odd-carbon chains",
          value = isTRUE(isolate(input$chain_remove_odd))
        ),
        tags$div(style = "font-size:11px; color:#777; margin-top:4px;",
                 "Note: Filtering affects class plot / molecule plot / heatmap / bar plot.")
      )
    })
    
    # ==== SE with chain filters ========================================
    se_work <- reactive({
      se <- se_in()
      req(se)
      
      if (!methods::is(se, "SummarizedExperiment")) return(se)
      if (!.has_acyl_chains(se, "acyl_chains")) return(se)
      
      choices <- .get_chain_choices(se, "acyl_chains")
      if (!length(choices)) return(se)
      
      codes   <- input$chain_codes %||% character(0)
      req_all <- isTRUE(input$chain_require_all)
      rm_odd  <- isTRUE(input$chain_remove_odd)
      
      .filter_se_by_chain_codes(
        se,
        selected_codes = codes,
        require_all    = req_all,
        remove_odd     = rm_odd,
        chain_col      = "acyl_chains"
      )
    })
    
    # ==== Class UI (based on filtered SE) ==============================
    output$class_ui <- renderUI({
      se <- se_work(); req(se)
      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      class_col <- .resolve_class_col(se)
      
      classes <- sort(unique(as.character(rd[[class_col]])))
      classes <- classes[!is.na(classes) & nzchar(classes)]
      
      if (!length(classes)) {
        return(tags$div(style = "color:#b91c1c; font-size:12px;",
                        "No lipid classes available after filtering."))
      }
      
      cur <- isolate(input$lipid_class)
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% classes) cur else classes[1]
      
      selectInput(ns("lipid_class"), "Lipid class", classes, selected = sel)
    })
    
    # ==== Molecule candidates (based on filtered SE) ====================
    observeEvent(list(se_work(), input$lipid_class), {
      se <- se_work(); req(se)
      cls <- input$lipid_class
      if (is.null(cls) || !nzchar(cls)) {
        updateSelectInput(session, "molecule_id", choices = character(0), selected = character(0))
        return()
      }
      
      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      class_col <- .resolve_class_col(se)
      mol_ids <- rownames(rd)[as.character(rd[[class_col]]) == cls]
      mol_ids <- mol_ids[!is.na(mol_ids) & nzchar(mol_ids)]
      
      cur <- isolate(input$molecule_id)
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% mol_ids) cur else (if (length(mol_ids)) mol_ids[1] else character(0))
      
      updateSelectInput(session, "molecule_id", choices = mol_ids, selected = sel)
    }, ignoreInit = TRUE)
    
    # ==================================================================
    # Plot components (class / molecule / heatmap) shared reactive
    # ==================================================================
    plot_components <- reactive({
      se  <- se_work(); req(se)
      rd0 <- as.data.frame(SummarizedExperiment::rowData(se))
      req(nrow(rd0) > 0)
      
      req(input$lipid_class, input$plot_type, input$agg_fun)
      adv <- adv_reactive(); req(adv)
      
      req(
        exists("plot_dot_se",      mode = "function") ||
          exists("plot_box_se",    mode = "function") ||
          exists("plot_violin_se", mode = "function")
      )
      req(exists("make_class_heatmap_CH", mode = "function"))
      req(exists("theme_lipidomics",      mode = "function"))
      
      x_var        <- "class"
      x_order      <- adv$manual_order
      order_by_eff <- if (length(x_order)) "none" else adv$order_by
      
      groups <- .get_x_groups(se)
      pal    <- .make_palette(groups, adv)
      
      class_col <- .resolve_class_col(se)
      
      se_cls <- aggregate_to_class_se(se, class_col = class_col, fun = input$agg_fun)
      picked_mol <- .pick_molecule_in_class(se, class_col, input$lipid_class, preferred_id = input$molecule_id)
      
      arg <- .build_comp_args(adv$comp_mode, adv$ref_group, adv$manual_pairs)
      
      dot_jw    <- .safe_jitter(adv$dot_jitter_width)
      box_jw    <- .safe_jitter(adv$box_jitter_width)
      violin_jw <- .safe_jitter(adv$violin_jitter_width)
      
      if (identical(input$plot_type, "dot")) {
        fun  <- get("plot_dot_se", mode = "function")
        extra <- list(
          point_size   = adv$dot_point_size,
          jitter_width = dot_jw,
          point_alpha  = adv$dot_alpha,
          show_median  = adv$dot_show_median,
          median_size  = adv$dot_median_size,
          median_width = adv$dot_median_width,
          median_color = adv$dot_median_color
        )
      } else if (identical(input$plot_type, "box")) {
        fun  <- get("plot_box_se", mode = "function")
        extra <- list(
          box_width    = adv$box_width,
          box_alpha    = adv$box_alpha,
          show_points  = adv$box_show_points,
          point_size   = adv$box_point_size,
          jitter_width = box_jw,
          point_alpha  = adv$box_point_alpha
        )
      } else {
        fun  <- get("plot_violin_se", mode = "function")
        extra <- list(
          violin_width = adv$violin_width,
          violin_alpha = adv$violin_alpha,
          trim         = adv$violin_trim,
          show_points  = adv$violin_show_points,
          point_size   = adv$violin_point_size,
          jitter_width = violin_jw,
          point_alpha  = adv$violin_point_alpha,
          show_median  = adv$violin_show_median,
          median_size  = adv$violin_median_size,
          median_color = adv$violin_median_color
        )
      }
      
      p_class <- do.call(fun, c(list(
        se         = se_cls,
        feature_id = input$lipid_class,
        x_var      = x_var,
        x_order    = if (length(x_order)) x_order else NULL,
        order_by   = order_by_eff,
        decreasing = isTRUE(adv$decreasing),
        palette    = pal,
        add_p      = isTRUE(adv$add_p),
        test       = adv$test,
        comparisons= arg$comparisons,
        ref_group  = arg$ref_group,
        p_adjust   = "BH",
        p_label    = adv$p_label
      ), extra)) +
        theme_lipidomics(12, 0, 12, 12, 14) +
        ggplot2::theme(legend.position = "none", aspect.ratio = 1) +
        ggplot2::labs(title = paste0("Class: ", input$lipid_class))
      
      x_levels_from_plot <- NULL
      if (!is.null(p_class$data) && x_var %in% names(p_class$data)) {
        x_col <- p_class$data[[x_var]]
        x_levels_from_plot <- if (is.factor(x_col)) levels(x_col) else unique(as.character(x_col))
      }
      
      if (is.na(picked_mol) || !nzchar(picked_mol)) {
        p_mol <- ggplot() +
          annotate("text", x = 0, y = 0, label = "No molecule available in this class (after filtering).") +
          theme_void()
      } else {
        p_mol <- do.call(fun, c(list(
          se         = se,
          feature_id = picked_mol,
          x_var      = x_var,
          x_order    = if (length(x_order)) x_order else NULL,
          order_by   = order_by_eff,
          decreasing = isTRUE(adv$decreasing),
          palette    = pal,
          add_p      = isTRUE(adv$add_p),
          test       = adv$test,
          comparisons= arg$comparisons,
          ref_group  = arg$ref_group,
          p_adjust   = "BH",
          p_label    = adv$p_label
        ), extra)) +
          theme_lipidomics(11, 0, 10, 10, 12) +
          ggplot2::theme(legend.position = "none", aspect.ratio = 1) +
          ggplot2::labs(title = paste0("Molecule: ", picked_mol))
      }
      
      x_order_hm <- NULL
      if (!is.null(x_levels_from_plot) && length(x_levels_from_plot)) x_order_hm <- x_levels_from_plot
      else if (length(x_order)) x_order_hm <- x_order
      
      order_by_hm   <- if (!is.null(x_order_hm) && length(x_order_hm)) "none" else order_by_eff
      decreasing_hm <- if (identical(order_by_hm, "none")) FALSE else isTRUE(adv$decreasing)
      
      ht <- make_class_heatmap_CH(
        se         = se,
        class_col  = class_col,
        class_name = input$lipid_class,
        x_var      = x_var,
        x_order    = x_order_hm,
        order_by   = order_by_hm,
        decreasing = decreasing_hm,
        topN       = adv$hm_topN,
        row_z      = isTRUE(adv$hm_row_z),
        row_total_fun = "mean",
        palette    = pal
      )
      
      list(p_class = p_class, p_mol = p_mol, ht = ht, pal = pal, x_order = x_order)
    })
    
    # ==== Outputs =======================================================
    output$plot_class <- renderPlot({
      pcs <- plot_components(); req(pcs$p_class)
      print(pcs$p_class)
    })
    
    output$plot_molecule <- renderPlot({
      pcs <- plot_components(); req(pcs$p_mol)
      print(pcs$p_mol)
    })
    
    output$dl_class_plot <- downloadHandler(
      filename = function() paste0("class_plot_", input$lipid_class, ".svg"),
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        svglite::svglite(file, width = 6, height = 5)
        on.exit(grDevices::dev.off(), add = TRUE)
        pcs <- plot_components(); req(pcs$p_class)
        print(pcs$p_class)
      }
    )
    
    output$dl_molecule_plot <- downloadHandler(
      filename = function() paste0("molecule_plot_", input$molecule_id %||% "molecule", ".svg"),
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        svglite::svglite(file, width = 6, height = 5)
        on.exit(grDevices::dev.off(), add = TRUE)
        pcs <- plot_components(); req(pcs$p_mol)
        print(pcs$p_mol)
      }
    )
    
    # ==== Bar plot helper (for the toggle) =============================
    .make_bar_plot <- function() {
      se  <- se_work();      req(se, input$lipid_class)
      adv <- adv_reactive(); req(adv)
      
      class_col <- .resolve_class_col(se)
      label_col <- .resolve_label_col(se)
      top_n     <- adv$hm_topN
      
      groups  <- .get_x_groups(se)
      pal     <- .make_palette(groups, adv)
      x_order <- adv$manual_order
      
      req(exists("plot_lipid_class_bar", mode = "function"))
      plot_lipid_class_bar(
        se,
        lipid_class       = input$lipid_class,
        assay_name        = "abundance",
        lipid_class_col   = class_col,
        feature_label_col = label_col,
        sample_class_col  = "class",
        use_se            = TRUE,
        top_n             = top_n,
        class_colors      = pal,
        class_order       = if (length(x_order)) x_order else NULL,
        x_text_angle      = 45,
        x_text_hjust      = 1,
        bottom_margin_pt  = 26
      )
    }
    
    # ==== Heatmap / Bar plot output ====================================
    output$plot_heatmap <- renderPlot({
      mode <- input$heatmap_mode %||% "heatmap"
      pcs  <- plot_components()
      
      if (identical(mode, "heatmap")) {
        req(pcs$ht)
        if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
          plot.new(); text(0.5, 0.5, "ComplexHeatmap is not installed.")
          return()
        }
        if (!requireNamespace("grid", quietly = TRUE)) {
          plot.new(); text(0.5, 0.5, "grid is not available.")
          return()
        }
        grid::grid.newpage()
        ComplexHeatmap::draw(
          pcs$ht,
          newpage = FALSE,
          heatmap_legend_side    = "bottom",
          annotation_legend_side = "bottom",
          merge_legends          = TRUE
        )
      } else {
        p_bar <- .make_bar_plot()
        print(p_bar)
      }
    })
    
    output$dl_heatmap <- downloadHandler(
      filename = function() {
        mode <- input$heatmap_mode %||% "heatmap"
        if (identical(mode, "heatmap")) paste0("heatmap_", input$lipid_class, ".svg")
        else paste0("barplot_", input$lipid_class, ".svg")
      },
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        mode <- input$heatmap_mode %||% "heatmap"
        
        svglite::svglite(file, width = 7, height = 6)
        on.exit(grDevices::dev.off(), add = TRUE)
        
        if (identical(mode, "heatmap")) {
          pcs <- plot_components(); req(pcs$ht)
          if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) stop("Please install ComplexHeatmap.")
          if (!requireNamespace("grid", quietly = TRUE)) stop("grid is required.")
          grid::grid.newpage()
          ComplexHeatmap::draw(
            pcs$ht,
            newpage = FALSE,
            heatmap_legend_side    = "bottom",
            annotation_legend_side = "bottom",
            merge_legends          = TRUE
          )
        } else {
          p_bar <- .make_bar_plot()
          print(p_bar)
        }
      }
    )
    
    output$dl_svg <- downloadHandler(
      filename = function() "plot_class_molecule_heatmap.svg",
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        if (!requireNamespace("patchwork", quietly = TRUE)) stop("Please install patchwork.")
        if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) stop("Please install ComplexHeatmap.")
        if (!requireNamespace("grid", quietly = TRUE)) stop("grid is required.")
        
        svglite::svglite(file, width = 12, height = 10)
        on.exit(grDevices::dev.off(), add = TRUE)
        
        pcs <- plot_components()
        req(pcs$p_class, pcs$p_mol, pcs$ht)
        
        p_top <- patchwork::wrap_plots(pcs$p_class, pcs$p_mol, ncol = 2, widths = c(1, 1))
        
        grid::grid.newpage()
        lay <- grid::grid.layout(nrow = 2, ncol = 1, heights = grid::unit(c(1, 1.1), "null"))
        grid::pushViewport(grid::viewport(layout = lay))
        
        grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
        print(p_top, newpage = FALSE)
        grid::upViewport()
        
        grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
        ComplexHeatmap::draw(
          pcs$ht,
          newpage = FALSE,
          heatmap_legend_side    = "bottom",
          annotation_legend_side = "bottom",
          merge_legends          = TRUE
        )
        grid::upViewport(2)
      }
    )
    
    # ==== Live notify ===================================================
    observeEvent(input$plot_type, {
      if (is.function(on_live_change)) on_live_change(list(plot_type = input$plot_type, dataset = "lipid"))
    }, ignoreInit = TRUE)
    
    observeEvent(input$agg_fun, {
      if (is.function(on_live_change)) on_live_change(list(agg_fun = input$agg_fun, dataset = "lipid"))
    }, ignoreInit = TRUE)
    
    observeEvent(input$lipid_class, {
      if (is.function(on_live_change)) on_live_change(list(lipid_class = input$lipid_class, dataset = "lipid"))
    }, ignoreInit = TRUE)
    
    observeEvent(input$molecule_id, {
      if (is.function(on_live_change)) on_live_change(list(molecule_id = input$molecule_id, dataset = "lipid"))
    }, ignoreInit = TRUE)
    
    observeEvent(list(input$chain_codes, input$chain_require_all, input$chain_remove_odd), {
      if (is.function(on_live_change)) {
        on_live_change(list(
          chain_codes       = input$chain_codes %||% character(0),
          chain_require_all = isTRUE(input$chain_require_all),
          chain_remove_odd  = isTRUE(input$chain_remove_odd),
          dataset = "lipid"
        ))
      }
    }, ignoreInit = TRUE)
  })
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
# Note: this UI uses shinydashboard::box; ensure shinydashboard is attached
# where you source this module, or prefix with shinydashboard:: everywhere.
# -----------------------------------------------------------------------------

.cy_details <- function(title, ..., open = FALSE) {
  shiny::tags$details(
    open = if (isTRUE(open)) NA else NULL,
    class = "cy-acc",
    shiny::tags$summary(shiny::tags$span(title)),
    shiny::div(class = "cy-acc-body", ...)
  )
}

#' @export
mod_plot_lipid_ui <- function(id) {
  ns <- shiny::NS(id)
  
  js_heatmap_toggle <- "
  document.addEventListener('click', function(e){
    var t = e.target;
    if (!t.classList.contains('hm-toggle-link')) return;
    e.preventDefault();
    var inputId = t.getAttribute('data-input');
    var mode    = t.getAttribute('data-mode');
    var header = t.closest('.box-header');
    if (header) {
      header.querySelectorAll('.hm-toggle-link').forEach(function(el){
        el.classList.remove('hm-active');
      });
      t.classList.add('hm-active');
    }
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(inputId, mode, {priority: 'event'});
    }
  });
  "
  
  css_heatmap_toggle <- "
  .hm-toggle-link { cursor: pointer; padding: 0 6px; color: #e1ecf4; }
  .hm-toggle-link.hm-active { font-weight: bold; text-decoration: underline; }
  "
  
  # details/summary accordion style
css_details <- "
details.cy-acc {
  border: 1px solid rgba(0,0,0,0.12);
  border-radius: 6px;
  margin-bottom: 8px;
  background: #fff;
  overflow: hidden;
}

details.cy-acc > summary {
  padding: 8px 10px;
  cursor: pointer;
  list-style: none !important;
  user-select: none;
  display: flex;
  align-items: center;
  justify-content: space-between;
}

/* remove browser default marker */
details.cy-acc > summary::-webkit-details-marker { display: none !important; }
details.cy-acc > summary::marker { content: '' !important; }

/* IMPORTANT: do NOT inject '???' */
details.cy-acc > summary:before { content: '' !important; }

/* optional: custom chevron */
details.cy-acc > summary:after {
  content: '▸';
  display: inline-block;
  margin-left: 8px;
  opacity: 0.7;
  transform: rotate(0deg);
  transition: transform .15s ease;
}
details.cy-acc[open] > summary:after { transform: rotate(90deg); }

details.cy-acc > summary span { font-weight: 600; }
details.cy-acc[open] > summary { border-bottom: 1px solid rgba(0,0,0,0.10); }

.cy-acc-body { padding: 10px; }
.cy-acc-body .form-group { margin-bottom: 10px; }
"

  
  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(css_heatmap_toggle, css_details))),
    shiny::tags$script(shiny::HTML(js_heatmap_toggle)),
    
    shiny::fluidRow(
      shinydashboard::box(
        title       = "Class-level plot",
        width       = 6,
        status      = "primary",
        solidHeader = TRUE,
        shiny::plotOutput(ns("plot_class"), height = "360px"),
        shiny::br(),
        shiny::downloadButton(ns("dl_class_plot"), "Download SVG")
      ),
      shinydashboard::box(
        title       = "Molecule-level plot",
        width       = 6,
        status      = "primary",
        solidHeader = TRUE,
        shiny::plotOutput(ns("plot_molecule"), height = "360px"),
        shiny::br(),
        shiny::downloadButton(ns("dl_molecule_plot"), "Download SVG")
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        title       = "Plot settings",
        width       = 3,
        status      = "primary",
        solidHeader = TRUE,
        
          open = TRUE,
          shiny::radioButtons(
            ns("plot_type"),
            "Plot type",
            choices  = c("dot", "box", "violin"),
            selected = "dot",
            inline   = TRUE
          ),
        .cy_details(
          "Variable",
          open = TRUE,
          shiny::uiOutput(ns("class_ui")),
          shiny::selectInput(ns("molecule_id"), "Molecule (in selected class)", choices = NULL),
		  shiny::tags$div(style = "height: 100px;")
        ),
        
        .cy_details(
          "Aggregation / Top-N",
          open = FALSE,
          shiny::selectInput(
            ns("agg_fun"),
            "Class aggregation",
            choices  = c("sum", "mean", "median"),
            selected = "sum"
          ),
          shiny::numericInput(
            ns("n_top_mol"),
            "Top-N molecules (heatmap / bar plot)",
            value = 40,
            min   = 5,
            step  = 5
          )
        ),
        
        .cy_details(
          "Chain filter",
          open = FALSE,
          shiny::uiOutput(ns("chain_filter_ui"))
        )
      ),
      
      shinydashboard::box(
        title       = shiny::div(
          "Top molecules in selected lipid class",
          shiny::span(
            class = "pull-right",
            shiny::tags$a(
              "Heatmap",
              class        = "hm-toggle-link hm-active",
              `data-input` = ns("heatmap_mode"),
              `data-mode`  = "heatmap"
            ),
            "|",
            shiny::tags$a(
              "Bar plot",
              class        = "hm-toggle-link",
              `data-input` = ns("heatmap_mode"),
              `data-mode`  = "bar"
            )
          )
        ),
        width       = 9,
        status      = "primary",
        solidHeader = TRUE,
        shiny::plotOutput(ns("plot_heatmap"), height = "380px"),
        shiny::br(),
        shiny::downloadButton(ns("dl_heatmap"), "Download SVG")
      )
    )
  )
}
