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
#   - Heatmap / bar plot / acyl-chain composition are shown as tabs
#   - Bar plot uses the same palette logic as other plots (adv$palette_map)
#   - FIX: top_n slicing no longer uses dplyr::n() outside data-masking verbs
#   - NEW: Bar plot adds extra bottom margin (and supports ggsave safety via coord_cartesian(clip="off"))
#   - NEW: Acyl-chain composition plot shows within-class chain proportions
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(SummarizedExperiment)
  library(S4Vectors)
  library(methods)
  library(dplyr)
  library(tidyr)
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
      cand <- c("Metabolite name", "Metabolite.name", "Metabolite", "Name")
      hit  <- cand[cand %in% cn][1]
      if (length(hit)) hit else cn[1]
    }
    
    .get_group_choices <- function(se) {
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!ncol(cd)) return(character(0))
      keep <- vapply(cd, function(x) {
        is.atomic(x) || is.factor(x)
      }, logical(1))
      cols <- names(cd)[keep]
      cols <- setdiff(cols, c("sample_id", "use"))
      cols
    }

    .get_x_groups <- function(se, x_var = "class") {
      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!x_var %in% colnames(cd)) return(character(0))
      unique(as.character(cd[[x_var]]))
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

    .safe_font_size <- function(x, default = 12) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x) || !is.finite(x) || x < 6) default else x
    }

    .apply_p_label_size <- function(p, size = 3.5) {
      if (!inherits(p, c("gg", "ggplot"))) return(p)
      size <- suppressWarnings(as.numeric(size))
      if (!length(size) || is.na(size) || !is.finite(size) || size <= 0) return(p)
      for (i in seq_along(p$layers)) {
        lyr <- p$layers[[i]]
        if (inherits(lyr$geom, "GeomText")) {
          lyr$aes_params$size <- size
          p$layers[[i]] <- lyr
        }
      }
      p
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

    .make_chain_palette <- function(chains) {
      chains <- unique(as.character(chains))
      chains <- chains[!is.na(chains) & nzchar(chains)]
      if (!length(chains)) return(NULL)

      other_idx <- which(chains == "Other")
      base_chains <- setdiff(chains, "Other")

      pal_base <- unname(grDevices::palette.colors(palette = "R3"))
      pal <- rep(pal_base, length.out = max(length(base_chains), 1L))
      pal <- pal[seq_len(length(base_chains))]
      names(pal) <- base_chains

      if (length(other_idx)) pal <- c(pal, Other = "#BDBDBD")
      pal[chains]
    }
    
    # ==== Chain/SPB filtering helpers ==================================
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

    .get_combined_chain_list <- function(se, chain_cols = c("acyl_chains", "sphingoid_bases")) {
      n <- nrow(SummarizedExperiment::rowData(se))
      out <- rep(list(character(0)), n)
      for (col in unique(chain_cols)) {
        lst <- .get_chain_list(se, chain_col = col)
        if (is.null(lst)) next
        out <- Map(function(a, b) unique(c(a, b)), out, lst)
      }
      out
    }

    .get_combined_chain_choices <- function(se, chain_cols = c("acyl_chains", "sphingoid_bases")) {
      u <- unique(unlist(.get_combined_chain_list(se, chain_cols = chain_cols), use.names = FALSE))
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
                                          chain_col = "acyl_chains",
                                          chain_cols = NULL) {
      if (is.null(se) || !methods::is(se, "SummarizedExperiment")) return(se)
      
      chains_list <- if (is.null(chain_cols)) {
        .get_chain_list(se, chain_col = chain_col)
      } else {
        .get_combined_chain_list(se, chain_cols = chain_cols)
      }
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
      
      if (!(.has_acyl_chains(se, "acyl_chains") || .has_acyl_chains(se, "sphingoid_bases"))) {
        return(tags$div(style = "font-size:12px; color:#777;",
                        "Chain filtering is OFF (rowData has no 'acyl_chains' or 'sphingoid_bases')."))
      }
      
      choices <- .get_combined_chain_choices(se, c("acyl_chains", "sphingoid_bases"))
      if (!length(choices)) {
        return(tags$div(style = "font-size:12px; color:#777;",
                        "Chain/SPB filtering is OFF (metadata format is not supported / empty)."))
      }
      
      sel <- isolate(input$chain_codes)
      sel <- sel[sel %in% choices]
      
      tagList(
        selectizeInput(
          ns("chain_codes"),
          "Chain / SPB codes (multi-select)",
          choices  = choices,
          selected = sel,
          multiple = TRUE,
          options = list(
            placeholder = "e.g., 18:0, 22:6;O2, SPB18:1;O2 ...",
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
          "Remove molecules with odd-carbon chains or SPB",
          value = isTRUE(isolate(input$chain_remove_odd))
        ),
        tags$div(style = "font-size:11px; color:#777; margin-top:4px;",
                 "Note: Filtering affects class plot / molecule plot / heatmap / bar plot / chain composition.")
      )
    })
    
    # ==== SE with chain filters ========================================
    se_work <- reactive({
      se <- se_in()
      req(se)
      
      if (!methods::is(se, "SummarizedExperiment")) return(se)
      if (!(.has_acyl_chains(se, "acyl_chains") || .has_acyl_chains(se, "sphingoid_bases"))) return(se)
      
      choices <- .get_combined_chain_choices(se, c("acyl_chains", "sphingoid_bases"))
      if (!length(choices)) return(se)
      
      codes   <- input$chain_codes %||% character(0)
      req_all <- isTRUE(input$chain_require_all)
      rm_odd  <- isTRUE(input$chain_remove_odd)
      
      .filter_se_by_chain_codes(
        se,
        selected_codes = codes,
        require_all    = req_all,
        remove_odd     = rm_odd,
        chain_cols     = c("acyl_chains", "sphingoid_bases")
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
      facet_var    <- adv$facet_var %||% ""
      x_order      <- adv$manual_order
      order_by_eff <- if (length(x_order)) "none" else adv$order_by
      
      groups <- .get_x_groups(se, x_var = x_var)
      pal    <- .make_palette(groups, adv)
      
      class_col <- .resolve_class_col(se)
      
      se_cls <- aggregate_to_class_se(se, class_col = class_col, fun = input$agg_fun)
      picked_mol <- .pick_molecule_in_class(se, class_col, input$lipid_class, preferred_id = input$molecule_id)
      
      arg <- .build_comp_args(adv$comp_mode, adv$ref_group, adv$manual_pairs)
      plot_font <- .safe_font_size(adv$plot_font_size, default = 12)
      strip_font <- .safe_font_size(adv$strip_font_size %||% plot_font, default = plot_font)
      p_label_font <- suppressWarnings(as.numeric(adv$p_label_font_size %||% 3.5))
      if (!is.finite(p_label_font) || p_label_font <= 0) p_label_font <- 3.5
      y_axis_label <- trimws(as.character(adv$lipid_y_axis_label %||% "Abundance"))
      if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"
      
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
        facet_var  = if (nzchar(facet_var)) facet_var else NULL,
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
        theme_lipidomics(plot_font, 0, plot_font, plot_font, plot_font + 2, strip_font) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(title = paste0("Class: ", input$lipid_class), y = y_axis_label)
      p_class <- .apply_p_label_size(p_class, p_label_font)
      
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
          facet_var  = if (nzchar(facet_var)) facet_var else NULL,
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
          theme_lipidomics(plot_font, 0, plot_font, plot_font, plot_font + 1, strip_font) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(title = paste0("Molecule: ", picked_mol), y = y_axis_label)
        p_mol <- .apply_p_label_size(p_mol, p_label_font)
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
        svglite::svglite(file, width = 6, height = 6)
        on.exit(grDevices::dev.off(), add = TRUE)
        pcs <- plot_components(); req(pcs$p_class)
        print(pcs$p_class)
      }
    )
    
    output$dl_molecule_plot <- downloadHandler(
      filename = function() paste0("molecule_plot_", input$molecule_id %||% "molecule", ".svg"),
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        svglite::svglite(file, width = 6, height = 6)
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
      top_n     <- adv$hm_topN
      plot_font <- .safe_font_size(adv$plot_font_size, default = 12)
      y_axis_label <- trimws(as.character(adv$lipid_y_axis_label %||% "Abundance"))
      if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"
      
      x_var   <- "class"
      facet_var <- adv$facet_var %||% ""
      groups  <- .get_x_groups(se, x_var = x_var)
      pal     <- .make_palette(groups, adv)
      x_order <- adv$manual_order
      
      req(exists("plot_lipid_class_bar", mode = "function"))
      plot_lipid_class_bar(
        se,
        lipid_class       = input$lipid_class,
        assay_name        = "abundance",
        lipid_class_col   = class_col,
        #feature_label_col = label_col,
        sample_class_col  = x_var,
        facet_var         = if (nzchar(facet_var)) facet_var else NULL,
        use_se            = TRUE,
        top_n             = top_n,
        class_colors      = pal,
        class_order       = if (length(x_order)) x_order else NULL,
        x_text_angle      = 45,
        x_text_hjust      = 1,
        bottom_margin_pt  = 26
      ) +
        ggplot2::labs(y = y_axis_label) +
        ggplot2::theme_classic(base_size = plot_font) +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_text(size = plot_font, angle = 45, hjust = 1, vjust = 1),
          axis.text.y  = ggplot2::element_text(size = plot_font),
          axis.title   = ggplot2::element_text(size = plot_font + 1),
          legend.text  = ggplot2::element_text(size = plot_font),
          legend.title = ggplot2::element_text(size = plot_font, face = "bold"),
          plot.title   = ggplot2::element_text(size = plot_font + 2, face = "bold")
        )
    }

    .make_chain_composition_plot <- function() {
      se  <- se_work();      req(se, input$lipid_class)
      adv <- adv_reactive(); req(adv)

      .fail_plot <- function(msg) {
        stop(as.character(msg), call. = FALSE)
      }

      class_col <- .resolve_class_col(se)
      facet_var <- adv$facet_var %||% ""
      x_var     <- "class"
      plot_font <- .safe_font_size(adv$plot_font_size, default = 12)
      y_axis_label <- trimws(as.character(adv$lipid_y_axis_label %||% "Abundance"))
      if (!nzchar(y_axis_label)) y_axis_label <- "Abundance"

      rd <- as.data.frame(SummarizedExperiment::rowData(se))
      idx <- which(as.character(rd[[class_col]]) == as.character(input$lipid_class))
      if (!length(idx)) .fail_plot("No molecules are available in the selected lipid class.")

      chain_list_all <- .get_chain_list(se, chain_col = "acyl_chains")
      if (is.null(chain_list_all)) .fail_plot("rowData must contain a usable 'acyl_chains' column.")
      chain_list <- chain_list_all[idx]

      has_chain <- vapply(chain_list, function(ch) length(ch) > 0, logical(1))
      if (!any(has_chain)) .fail_plot("No acyl-chain annotations are available in the selected lipid class.")

      idx <- idx[has_chain]
      chain_list <- chain_list[has_chain]

      assay_name <- if ("abundance" %in% SummarizedExperiment::assayNames(se)) "abundance" else SummarizedExperiment::assayNames(se)[1]
      mat <- as.matrix(SummarizedExperiment::assay(se, assay_name))[idx, , drop = FALSE]
      storage.mode(mat) <- "double"

      feature_chain_map <- dplyr::bind_rows(lapply(seq_along(chain_list), function(i) {
        chains <- unique(trimws(as.character(chain_list[[i]])))
        chains <- chains[!is.na(chains) & nzchar(chains)]
        if (!length(chains)) return(NULL)
        data.frame(
          feature_id = rownames(mat)[i],
          chain = chains,
          chain_weight = 1 / length(chains),
          stringsAsFactors = FALSE
        )
      }))
      if (is.null(feature_chain_map) || !nrow(feature_chain_map)) {
        .fail_plot("No valid acyl-chain entries were found.")
      }

      abundance_long <- as.data.frame(mat)
      abundance_long$feature_id <- rownames(mat)
      abundance_long <- tidyr::pivot_longer(
        abundance_long,
        cols = -feature_id,
        names_to = "sample_id",
        values_to = "abundance"
      )

      cd <- as.data.frame(SummarizedExperiment::colData(se))
      if (!(x_var %in% names(cd))) .fail_plot("colData must contain 'class'.")
      sample_meta <- data.frame(
        sample_id = colnames(se),
        class = as.character(cd[[x_var]]),
        stringsAsFactors = FALSE
      )
      if (nzchar(facet_var) && facet_var %in% names(cd)) {
        sample_meta[[facet_var]] <- as.character(cd[[facet_var]])
      }

      dat <- abundance_long |>
        dplyr::left_join(feature_chain_map, by = "feature_id") |>
        dplyr::filter(!is.na(.data$chain), nzchar(.data$chain)) |>
        dplyr::mutate(chain_abundance = .data$abundance * .data$chain_weight) |>
        dplyr::left_join(sample_meta, by = "sample_id") |>
        dplyr::filter(!is.na(.data[[x_var]]), nzchar(.data[[x_var]]))

      if (!nrow(dat)) .fail_plot("No chain abundance data could be calculated.")

      group_vars <- c(x_var, if (nzchar(facet_var) && facet_var %in% names(dat)) facet_var else character(0), "chain")
      summarised <- dat |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
        dplyr::summarise(value = stats::median(.data$chain_abundance, na.rm = TRUE), .groups = "drop")

      summarised <- summarised |>
        dplyr::group_by(.data[[x_var]], !!!rlang::syms(setdiff(group_vars, c(x_var, "chain")))) |>
        dplyr::mutate(prop = .data$value / sum(.data$value, na.rm = TRUE)) |>
        dplyr::ungroup()

      summarised <- summarised |>
        dplyr::filter(is.finite(.data$prop), !is.na(.data$prop), .data$prop > 0)
      if (!nrow(summarised)) .fail_plot("No non-zero chain proportions are available for this class.")

      top_n_chain <- suppressWarnings(as.integer(input$n_top_chain %||% 12L))
      if (!is.finite(top_n_chain) || top_n_chain < 1) top_n_chain <- 12L

      top_chains <- summarised |>
        dplyr::group_by(.data$chain) |>
        dplyr::summarise(score = mean(.data$prop, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$score), .data$chain) |>
        dplyr::slice_head(n = top_n_chain) |>
        dplyr::pull(.data$chain)

      summarised <- summarised |>
        dplyr::mutate(chain = ifelse(.data$chain %in% top_chains, .data$chain, "Other")) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(group_vars, "chain"))), .data$chain) |>
        dplyr::summarise(prop = sum(.data$prop, na.rm = TRUE), .groups = "drop")

      chain_order <- summarised |>
        dplyr::group_by(.data$chain) |>
        dplyr::summarise(score = mean(.data$prop, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$score), .data$chain) |>
        dplyr::pull(.data$chain)
      if ("Other" %in% chain_order) {
        chain_order <- c(setdiff(chain_order, "Other"), "Other")
      }
      summarised$chain <- factor(summarised$chain, levels = chain_order, ordered = TRUE)

      x_order <- adv$manual_order %||% character(0)
      x_levels <- unique(as.character(summarised[[x_var]]))
      x_levels <- x_levels[!is.na(x_levels) & nzchar(x_levels)]
      if (length(x_order)) {
        x_levels <- c(intersect(x_order, x_levels), setdiff(x_levels, x_order))
      }
      summarised[[x_var]] <- factor(as.character(summarised[[x_var]]), levels = x_levels, ordered = TRUE)

      chain_pal <- .make_chain_palette(levels(summarised$chain))

      p <- ggplot2::ggplot(
        summarised,
        ggplot2::aes(x = .data[[x_var]], y = .data$prop, fill = .data$chain)
      ) +
        ggplot2::geom_col(width = 0.8, color = "gray34", linewidth = 0.2) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.2),
          labels = function(x) paste0(round(x * 100), "%"),
          expand = ggplot2::expansion(mult = c(0, 0.02))
        ) +
        ggplot2::scale_fill_manual(values = chain_pal, drop = FALSE) +
        ggplot2::labs(
          title = paste0("Acyl chain composition: ", input$lipid_class),
          x = NULL,
          y = y_axis_label,
          fill = "Acyl chain"
        ) +
        ggplot2::theme_classic(base_size = plot_font) +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_text(size = plot_font),
          axis.text.y  = ggplot2::element_text(size = plot_font),
          axis.title   = ggplot2::element_text(size = plot_font + 1),
          legend.text  = ggplot2::element_text(size = plot_font),
          legend.title = ggplot2::element_text(size = plot_font, face = "bold"),
          plot.title   = ggplot2::element_text(size = plot_font + 2, face = "bold")
        )

      if (nzchar(facet_var) && facet_var %in% names(summarised)) {
        p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)))
      }

      p
    }
    
    # ==== Heatmap / Bar plot / chain composition output ================
    output$plot_heatmap <- renderPlot({
      pcs  <- plot_components()
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
    })

    output$plot_bar <- renderPlot({
      p_bar <- .make_bar_plot()
      print(p_bar)
    })

    output$plot_chain_comp <- renderPlot({
      p_chain <- .make_chain_composition_plot()
      print(p_chain)
    })

    output$dl_heatmap <- downloadHandler(
      filename = function() paste0("heatmap_", input$lipid_class, ".svg"),
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")

        svglite::svglite(file, width = 7, height = 7)
        on.exit(grDevices::dev.off(), add = TRUE)

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
      }
    )

    output$dl_bar <- downloadHandler(
      filename = function() paste0("barplot_", input$lipid_class, ".svg"),
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        svglite::svglite(file, width = 7, height = 7)
        on.exit(grDevices::dev.off(), add = TRUE)
        p_bar <- .make_bar_plot()
        print(p_bar)
      }
    )

    output$dl_chain_comp <- downloadHandler(
      filename = function() paste0("acyl_chain_composition_", input$lipid_class, ".svg"),
      content  = function(file) {
        if (!requireNamespace("svglite", quietly = TRUE)) stop("Please install svglite.")
        svglite::svglite(file, width = 8, height = 7)
        on.exit(grDevices::dev.off(), add = TRUE)
        p_chain <- .make_chain_composition_plot()
        print(p_chain)
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
  
  css_heatmap_toggle <- "
  .mslm-plot-frame {
    width: 100%;
    height: 360px;
    display: flex;
    align-items: center;
    justify-content: center;
    overflow: hidden;
  }
  .mslm-plot-frame-lg {
    width: 100%;
    height: 380px;
    overflow: hidden;
  }
  .mslm-plot-square {
    width: min(100%, 360px);
    height: 360px;
  }
  .mslm-plot-square .shiny-plot-output,
  .mslm-plot-square-lg .shiny-plot-output {
    width: 100% !important;
    height: 100% !important;
  }
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
    
    shiny::fluidRow(
      shinydashboard::box(
        title       = "Class-level plot",
        width       = 6,
        status      = "primary",
        solidHeader = TRUE,
        shiny::div(
          class = "mslm-plot-frame",
          shiny::div(
            class = "mslm-plot-square",
            shiny::plotOutput(ns("plot_class"), height = "100%")
          )
        ),
        shiny::br(),
        shiny::downloadButton(ns("dl_class_plot"), "Download SVG")
      ),
      shinydashboard::box(
        title       = "Molecule-level plot",
        width       = 6,
        status      = "primary",
        solidHeader = TRUE,
        shiny::div(
          class = "mslm-plot-frame",
          shiny::div(
            class = "mslm-plot-square",
            shiny::plotOutput(ns("plot_molecule"), height = "100%")
          )
        ),
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
            "Top-N molecules (heatmap / bar plot tabs)",
            value = 40,
            min   = 5,
            step  = 5
          ),
          shiny::numericInput(
            ns("n_top_chain"),
            "Top-N chains (composition)",
            value = 12,
            min   = 3,
            step  = 1
          )
        ),
        
        .cy_details(
          "Chain filter",
          open = FALSE,
          shiny::uiOutput(ns("chain_filter_ui"))
        )
      ),
      
      shinydashboard::box(
        title       = "Top molecules in selected lipid class",
        width       = 9,
        status      = "primary",
        solidHeader = TRUE,
        shiny::tabsetPanel(
          id = ns("heatmap_mode"),
          type = "tabs",
          shiny::tabPanel(
            title = "Heatmap",
            value = "heatmap",
            shiny::div(
              class = "mslm-plot-frame-lg",
              shiny::plotOutput(ns("plot_heatmap"), height = "100%")
            ),
            shiny::br(),
            shiny::downloadButton(ns("dl_heatmap"), "Download SVG")
          ),
          shiny::tabPanel(
            title = "Bar plot",
            value = "bar",
            shiny::div(
              class = "mslm-plot-frame-lg",
              shiny::plotOutput(ns("plot_bar"), height = "100%")
            ),
            shiny::br(),
            shiny::downloadButton(ns("dl_bar"), "Download SVG")
          ),
          shiny::tabPanel(
            title = "Acyl chain composition",
            value = "chain",
            shiny::div(
              class = "mslm-plot-frame-lg",
              shiny::plotOutput(ns("plot_chain_comp"), height = "100%")
            ),
            shiny::br(),
            shiny::downloadButton(ns("dl_chain_comp"), "Download SVG")
          )
        )
      )
    )
  )
}
