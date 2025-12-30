# R/heatmap.R
# Heatmap module for MSLipidMapper
# - Auto-initial draw when SE becomes available
# - Manual update button still works

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(SummarizedExperiment)
  library(S4Vectors)
  library(ComplexHeatmap)
  library(circlize)
  library(grid)
})

# ---- fixed settings -----------------------------------------------------
.HM_ASSAY_NAME       <- "abundance"
.HM_SUBCLASS_COL     <- "Ontology"
.HM_SAMPLE_GROUP_COL <- "class"

# ---- helpers ------------------------------------------------------------

.hm_norm_hex <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NA_character_)
  x <- trimws(as.character(x))
  if (!startsWith(x, "#")) x <- paste0("#", x)
  if (!grepl("^#([A-Fa-f0-9]{6})$", x)) return(NA_character_)
  toupper(x)
}

.hm_quantile_col_fun <- function(mat) {
  v <- as.numeric(mat)
  v <- v[is.finite(v)]
  
  if (!length(v)) {
    brk <- c(-1, 0, 1)
    return(circlize::colorRamp2(brk, c("navy", "white", "firebrick")))
  }
  
  q <- as.numeric(stats::quantile(v, probs = c(0.02, 0.5, 0.98), na.rm = TRUE, names = FALSE))
  if (any(!is.finite(q))) q <- c(min(v), stats::median(v), max(v))
  
  eps <- max(1e-6, 1e-6 * max(abs(q), na.rm = TRUE))
  if (q[1] >= q[2]) q[1] <- q[2] - eps
  if (q[2] >= q[3]) q[3] <- q[2] + eps
  if (q[1] >= q[2]) q[1] <- q[2] - eps
  
  circlize::colorRamp2(q, c("navy", "white", "firebrick"))
}

.hm_scale_base <- function(base_mat, mode = c("by_feature", "by_sample")) {
  mode <- match.arg(mode)
  if (mode == "by_feature") {
    m <- scale(base_mat)
    m[is.na(m)] <- 0
    return(m)
  }
  m <- t(scale(t(base_mat)))
  m[is.na(m)] <- 0
  m
}

.hm_palette_for_levels <- function(x, palette = "Dark 3") {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "NA"
  lev <- unique(x)
  pal <- grDevices::hcl.colors(length(lev), palette = palette)
  names(pal) <- lev
  list(values = x, colors = pal)
}

.hm_palette_from_adv <- function(x, adv, default_palette = "Dark 3") {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "NA"
  
  lev <- unique(x)
  pal <- grDevices::hcl.colors(length(lev), palette = default_palette)
  names(pal) <- lev
  
  if (!is.null(adv) && is.list(adv)) {
    if (!is.null(adv$palette_map) && length(adv$palette_map)) {
      pm <- adv$palette_map
      if (!is.null(names(pm))) {
        pm_norm <- vapply(pm, .hm_norm_hex, character(1))
        pm_norm <- pm_norm[!is.na(pm_norm)]
        hit <- intersect(names(pm_norm), lev)
        if (length(hit)) pal[hit] <- pm_norm[hit]
      }
    }
    if (!is.null(adv$manual_order) && length(adv$manual_order)) {
      ord <- intersect(as.character(adv$manual_order), lev)
      rest <- setdiff(lev, ord)
      lev2 <- c(ord, rest)
      pal <- pal[lev2]
    }
  }
  
  list(values = x, colors = pal)
}

.hm_make_subclass_base_matrix <- function(
    se,
    assay_name = .HM_ASSAY_NAME,
    subclass_col = .HM_SUBCLASS_COL,
    agg_fun = c("sum", "mean")
) {
  agg_fun <- match.arg(agg_fun)
  X <- as.matrix(SummarizedExperiment::assay(se, assay_name)) # features x samples
  storage.mode(X) <- "double"
  
  rd <- SummarizedExperiment::rowData(se)
  if (!subclass_col %in% colnames(rd)) stop("rowData(se)$", subclass_col, " not found.")
  subclass <- as.character(rd[[subclass_col]])
  subclass[is.na(subclass) | subclass == ""] <- "Unknown"
  
  if (agg_fun == "sum") {
    sub_by_sample <- rowsum(X, group = subclass, reorder = TRUE)  # subclass x sample
  } else {
    sub_sum <- rowsum(X, group = subclass, reorder = TRUE)
    sub_n   <- as.numeric(table(subclass)[rownames(sub_sum)])
    sub_by_sample <- sub_sum / sub_n
  }
  t(sub_by_sample) # samples x subclass
}

.hm_make_topvar_base_matrix <- function(
    se,
    assay_name = .HM_ASSAY_NAME,
    top_n = 50,
    var_method = c("var", "mad")
) {
  var_method <- match.arg(var_method)
  
  X <- as.matrix(SummarizedExperiment::assay(se, assay_name)) # features x samples
  storage.mode(X) <- "double"
  
  mol <- rownames(se)
  mol[is.na(mol) | mol == ""] <- "NA"
  mol <- make.unique(mol)
  
  score <- if (var_method == "mad") {
    apply(X, 1, stats::mad, na.rm = TRUE)
  } else {
    apply(X, 1, stats::var, na.rm = TRUE)
  }
  score[is.na(score)] <- -Inf
  
  n_use <- min(as.integer(top_n), nrow(X))
  n_use <- max(n_use, 1L)
  idx <- order(score, decreasing = TRUE)[seq_len(n_use)]
  
  base_mat <- t(X[idx, , drop = FALSE])  # samples x molecules
  colnames(base_mat) <- mol[idx]
  list(base_mat = base_mat, idx = idx, score = score)
}

.hm_orient <- function(base_mat, samples_on = c("rows", "columns")) {
  samples_on <- match.arg(samples_on)
  if (samples_on == "rows") {
    return(list(plot_mat = base_mat, sample_on_rows = TRUE))
  }
  list(plot_mat = t(base_mat), sample_on_rows = FALSE)
}

.hm_square_dims <- function(mat, cell_mm = 4.0, max_mm = 260) {
  nr <- max(1L, nrow(mat))
  nc <- max(1L, ncol(mat))
  
  w_mm <- nc * cell_mm
  h_mm <- nr * cell_mm
  
  scale_factor <- min(1, max_mm / max(w_mm, h_mm))
  w_mm <- w_mm * scale_factor
  h_mm <- h_mm * scale_factor
  
  list(
    width  = grid::unit(w_mm, "mm"),
    height = grid::unit(h_mm, "mm"),
    w_mm = w_mm,
    h_mm = h_mm
  )
}

.hm_export_dims <- function(hm_w_mm, hm_h_mm, extra_w_mm = 25, extra_h_mm = 105) {
  list(
    dev_w_mm = hm_w_mm + extra_w_mm,
    dev_h_mm = hm_h_mm + extra_h_mm
  )
}

.hm_mm_to_in <- function(mm) mm / 25.4
.hm_mm_to_px <- function(mm, dpi) round(.hm_mm_to_in(mm) * dpi)

# ---- module UI ----------------------------------------------------------

mod_heatmap_ui <- function(id, title = "Heatmap") {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3(title),
    
    shiny::tabsetPanel(
      id = ns("hm_tabs"),
      
      shiny::tabPanel(
        "Class heatmap",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shinydashboard::box(
              title = "Settings",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              
              shiny::selectInput(ns("agg_fun"), "Aggregation", choices = c("sum", "mean"), selected = "sum"),
              
              shiny::selectInput(
                ns("samples_on_class"),
                "Samples on",
                choices = c("rows (y-axis)" = "rows", "columns (x-axis)" = "columns"),
                selected = "rows"
              ),
              
              shiny::selectInput(
                ns("scale_mode_class"),
                "Scaling (required)",
                choices = c("Scale by feature (recommended)" = "by_feature",
                            "Scale by sample" = "by_sample"),
                selected = "by_feature"
              ),
              
              shiny::numericInput(ns("cell_mm_class"), "Tile size (mm)", value = 4.2, min = 1, max = 20, step = 0.2),
              shiny::numericInput(ns("max_mm_class"),  "Max heatmap size (mm)", value = 260, min = 100, max = 600, step = 10),
              
              shiny::checkboxInput(ns("use_shared_palette_class"), "Use shared class colors (Feature Advanced)", value = TRUE),
              
              shiny::checkboxInput(ns("cluster_samples_class"),  "Cluster samples", value = TRUE),
              shiny::checkboxInput(ns("cluster_features_class"), "Cluster subclasses", value = TRUE),
              
              shiny::actionButton(ns("update_class"), "Run", width = "100%", icon = shiny::icon("sync")),
              shiny::tags$hr(),
              shiny::downloadButton(ns("dl_class_pdf"), "Download PDF", width = "100%"),
              shiny::downloadButton(ns("dl_class_png"), "Download PNG", width = "100%")
            )
          ),
          shiny::column(
            width = 9,
            shinydashboard::box(
              title = "Heatmap",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              shiny::plotOutput(ns("plot_class"), height = "1150px")
            )
          )
        )
      ),
      
      shiny::tabPanel(
        "Molecule TopVar heatmap",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shinydashboard::box(
              title = "Settings",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              
              shiny::numericInput(ns("top_n"), "Top N molecules", value = 80, min = 5, step = 5),
              shiny::selectInput(ns("var_method"), "Variability metric", choices = c("var", "mad"), selected = "var"),
              
              shiny::selectInput(
                ns("samples_on_mol"),
                "Samples on",
                choices = c("rows (y-axis)" = "rows", "columns (x-axis)" = "columns"),
                selected = "rows"
              ),
              
              shiny::selectInput(
                ns("scale_mode_mol"),
                "Scaling (required)",
                choices = c("Scale by feature (recommended)" = "by_feature",
                            "Scale by sample" = "by_sample"),
                selected = "by_feature"
              ),
              
              shiny::numericInput(ns("cell_mm_mol"), "Tile size (mm)", value = 3.6, min = 1, max = 20, step = 0.2),
              shiny::numericInput(ns("max_mm_mol"),  "Max heatmap size (mm)", value = 260, min = 100, max = 600, step = 10),
              
              shiny::checkboxInput(ns("use_shared_palette_mol"), "Use shared class colors (Feature Advanced)", value = TRUE),
              
              shiny::checkboxInput(ns("cluster_samples_mol"),  "Cluster samples", value = TRUE),
              shiny::checkboxInput(ns("cluster_features_mol"), "Cluster molecules", value = TRUE),
              
              shiny::checkboxInput(ns("add_mol_subclass_anno"), "Add molecule subclass annotation", value = TRUE),
              
              shiny::actionButton(ns("update_mol"), "Run", width = "100%", icon = shiny::icon("sync")),
              shiny::tags$hr(),
              shiny::downloadButton(ns("dl_mol_pdf"), "Download PDF", width = "100%"),
              shiny::downloadButton(ns("dl_mol_png"), "Download PNG", width = "100%")
            )
          ),
          shiny::column(
            width = 9,
            shinydashboard::box(
              title = "Heatmap",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              shiny::plotOutput(ns("plot_mol"), height = "1150px")
            )
          )
        )
      )
    )
  )
}

# ---- module server ------------------------------------------------------

mod_heatmap_server <- function(id, se_lipid, adv_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    .get_adv <- function() {
      if (is.function(adv_reactive)) {
        a <- adv_reactive()
        if (!is.null(a) && is.list(a)) return(a)
      }
      NULL
    }
    
    .req_se <- function() {
      se <- se_lipid()
      shiny::validate(shiny::need(!is.null(se), "No data yet. Load SE first."))
      shiny::validate(shiny::need(methods::is(se, "SummarizedExperiment"), "Input is not a SummarizedExperiment."))
      se
    }
    
    .validate_fixed_inputs <- function(se) {
      shiny::validate(
        shiny::need(.HM_ASSAY_NAME %in% SummarizedExperiment::assayNames(se),
                    paste0("Assay '", .HM_ASSAY_NAME, "' not found in se.")),
        shiny::need(.HM_SUBCLASS_COL %in% colnames(SummarizedExperiment::rowData(se)),
                    paste0("rowData(se)$", .HM_SUBCLASS_COL, " not found.")),
        shiny::need(.HM_SAMPLE_GROUP_COL %in% colnames(SummarizedExperiment::colData(se)),
                    paste0("colData(se)$", .HM_SAMPLE_GROUP_COL, " not found."))
      )
      invisible(TRUE)
    }
    
    rect_gp_grey <- grid::gpar(col = "grey80", lwd = 0.5)
    
    .draw_bottom_legends <- function(ht) {
      grid::grid.newpage()
      ComplexHeatmap::draw(
        ht,
        heatmap_legend_side = "bottom",
        annotation_legend_side = "bottom",
        merge_legends = TRUE,
        padding = grid::unit(c(10, 10, 10, 10), "mm")
      )
    }
    
    .sample_group_colors <- function(groups_vec, use_shared = TRUE) {
      adv <- if (isTRUE(use_shared)) .get_adv() else NULL
      if (!is.null(adv)) {
        .hm_palette_from_adv(groups_vec, adv, default_palette = "Dark 3")
      } else {
        .hm_palette_for_levels(groups_vec, palette = "Dark 3")
      }
    }
    
    # ---- "nonce" triggers (auto initial draw + manual redraw) ------------
    class_nonce <- shiny::reactiveVal(0L)
    mol_nonce   <- shiny::reactiveVal(0L)
    
    class_autodrawn <- shiny::reactiveVal(FALSE)
    mol_autodrawn   <- shiny::reactiveVal(FALSE)
    
    # Auto initial draw when SE becomes available (once per session)
    shiny::observeEvent(se_lipid(), {
      se <- se_lipid()
      if (is.null(se)) return()
      
      if (!isTRUE(class_autodrawn())) {
        class_autodrawn(TRUE)
        class_nonce(class_nonce() + 1L)
      }
      if (!isTRUE(mol_autodrawn())) {
        mol_autodrawn(TRUE)
        mol_nonce(mol_nonce() + 1L)
      }
    }, ignoreInit = TRUE)
    
    # Manual redraw buttons
    shiny::observeEvent(input$update_class, {
      class_nonce(class_nonce() + 1L)
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$update_mol, {
      mol_nonce(mol_nonce() + 1L)
    }, ignoreInit = TRUE)
    
    # ---- Class heatmap ---------------------------------------------------
    class_obj <- shiny::eventReactive(class_nonce(), {
      se <- .req_se()
      .validate_fixed_inputs(se)
      
      base_mat <- .hm_make_subclass_base_matrix(
        se = se,
        assay_name = .HM_ASSAY_NAME,
        subclass_col = .HM_SUBCLASS_COL,
        agg_fun = input$agg_fun
      )
      base_mat <- .hm_scale_base(base_mat, input$scale_mode_class)
      
      cd <- SummarizedExperiment::colData(se)
      grp <- as.character(cd[[.HM_SAMPLE_GROUP_COL]])
      grp_info <- .sample_group_colors(grp, use_shared = isTRUE(input$use_shared_palette_class))
      
      ori <- .hm_orient(base_mat, input$samples_on_class)
      mat <- ori$plot_mat
      sample_on_rows <- ori$sample_on_rows
      
      cluster_rows <- if (sample_on_rows) isTRUE(input$cluster_samples_class) else isTRUE(input$cluster_features_class)
      cluster_cols <- if (sample_on_rows) isTRUE(input$cluster_features_class) else isTRUE(input$cluster_samples_class)
      
      left_anno <- NULL
      top_anno  <- NULL
      if (sample_on_rows) {
        left_anno <- ComplexHeatmap::rowAnnotation(
          SampleClass = grp_info$values,
          col = list(SampleClass = grp_info$colors)
        )
      } else {
        top_anno <- ComplexHeatmap::HeatmapAnnotation(
          SampleClass = grp_info$values,
          col = list(SampleClass = grp_info$colors),
          which = "column"
        )
      }
      
      col_fun <- .hm_quantile_col_fun(mat)
      dims <- .hm_square_dims(mat, cell_mm = input$cell_mm_class, max_mm = input$max_mm_class)
      
      ht <- ComplexHeatmap::Heatmap(
        mat,
        name = paste0(.HM_SUBCLASS_COL, "_", input$agg_fun, "_scaled"),
        col = col_fun,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_cols,
        left_annotation = left_anno,
        top_annotation  = top_anno,
        rect_gp = rect_gp_grey,
        width  = dims$width,
        height = dims$height,
        row_title = if (sample_on_rows) "Samples" else .HM_SUBCLASS_COL,
        column_title = if (sample_on_rows) .HM_SUBCLASS_COL else "Samples"
      )
      
      list(ht = ht, hm_w_mm = dims$w_mm, hm_h_mm = dims$h_mm)
    }, ignoreInit = TRUE)
    
    output$plot_class <- shiny::renderPlot({
      shiny::req(class_nonce() > 0)  # ?????????????????????????????????????????????????????????
      obj <- class_obj()
      .draw_bottom_legends(obj$ht)
    })
    
    output$dl_class_pdf <- shiny::downloadHandler(
      filename = function() sprintf("heatmap_%s_scaled_%s.pdf", tolower(.HM_SUBCLASS_COL), Sys.Date()),
      content = function(file) {
        shiny::req(class_nonce() > 0)
        obj <- class_obj()
        d <- .hm_export_dims(obj$hm_w_mm, obj$hm_h_mm, extra_w_mm = 25, extra_h_mm = 105)
        grDevices::pdf(file, width = .hm_mm_to_in(d$dev_w_mm), height = .hm_mm_to_in(d$dev_h_mm))
        .draw_bottom_legends(obj$ht)
        grDevices::dev.off()
      }
    )
    
    output$dl_class_png <- shiny::downloadHandler(
      filename = function() sprintf("heatmap_%s_scaled_%s.png", tolower(.HM_SUBCLASS_COL), Sys.Date()),
      content = function(file) {
        shiny::req(class_nonce() > 0)
        obj <- class_obj()
        d <- .hm_export_dims(obj$hm_w_mm, obj$hm_h_mm, extra_w_mm = 25, extra_h_mm = 105)
        dpi <- 220
        grDevices::png(file,
                       width  = .hm_mm_to_px(d$dev_w_mm, dpi),
                       height = .hm_mm_to_px(d$dev_h_mm, dpi),
                       res = dpi)
        .draw_bottom_legends(obj$ht)
        grDevices::dev.off()
      }
    )
    
    # ---- Molecule TopVar heatmap ----------------------------------------
    mol_obj <- shiny::eventReactive(mol_nonce(), {
      se <- .req_se()
      .validate_fixed_inputs(se)
      
      top <- .hm_make_topvar_base_matrix(
        se = se,
        assay_name = .HM_ASSAY_NAME,
        top_n = input$top_n,
        var_method = input$var_method
      )
      
      base_mat <- .hm_scale_base(top$base_mat, input$scale_mode_mol)
      
      cd <- SummarizedExperiment::colData(se)
      grp <- as.character(cd[[.HM_SAMPLE_GROUP_COL]])
      grp_info <- .sample_group_colors(grp, use_shared = isTRUE(input$use_shared_palette_mol))
      
      ori <- .hm_orient(base_mat, input$samples_on_mol)
      mat <- ori$plot_mat
      sample_on_rows <- ori$sample_on_rows
      
      cluster_rows <- if (sample_on_rows) isTRUE(input$cluster_samples_mol) else isTRUE(input$cluster_features_mol)
      cluster_cols <- if (sample_on_rows) isTRUE(input$cluster_features_mol) else isTRUE(input$cluster_samples_mol)
      
      left_anno <- NULL
      top_anno  <- NULL
      if (sample_on_rows) {
        left_anno <- ComplexHeatmap::rowAnnotation(
          SampleClass = grp_info$values,
          col = list(SampleClass = grp_info$colors)
        )
      } else {
        top_anno <- ComplexHeatmap::HeatmapAnnotation(
          SampleClass = grp_info$values,
          col = list(SampleClass = grp_info$colors),
          which = "column"
        )
      }
      
      feat_anno_top  <- NULL
      feat_anno_left <- NULL
      if (isTRUE(input$add_mol_subclass_anno)) {
        rd  <- SummarizedExperiment::rowData(se)
        sub <- as.character(rd[[.HM_SUBCLASS_COL]])[top$idx]
        sub[is.na(sub) | sub == ""] <- "Unknown"
        sub_info <- .hm_palette_for_levels(sub, palette = "Set 2")
        
        if (sample_on_rows) {
          feat_anno_top <- ComplexHeatmap::HeatmapAnnotation(
            MoleculeSubclass = sub_info$values,
            col = list(MoleculeSubclass = sub_info$colors),
            which = "column"
          )
        } else {
          feat_anno_left <- ComplexHeatmap::rowAnnotation(
            MoleculeSubclass = sub_info$values,
            col = list(MoleculeSubclass = sub_info$colors)
          )
        }
      }
      
      final_left_anno <- left_anno
      final_top_anno  <- top_anno
      if (!is.null(feat_anno_left)) {
        final_left_anno <- if (is.null(final_left_anno)) feat_anno_left else final_left_anno + feat_anno_left
      }
      if (!is.null(feat_anno_top)) {
        final_top_anno <- if (is.null(final_top_anno)) feat_anno_top else final_top_anno + feat_anno_top
      }
      
      col_fun <- .hm_quantile_col_fun(mat)
      dims <- .hm_square_dims(mat, cell_mm = input$cell_mm_mol, max_mm = input$max_mm_mol)
      
      ht <- ComplexHeatmap::Heatmap(
        mat,
        name = paste0("TopVar_", input$var_method, "_scaled"),
        col = col_fun,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_cols,
        left_annotation = final_left_anno,
        top_annotation  = final_top_anno,
        rect_gp = rect_gp_grey,
        width  = dims$width,
        height = dims$height,
        row_title = if (sample_on_rows) "Samples (clustered)" else "Molecules (TopVar)",
        column_title = if (sample_on_rows) "Molecules (TopVar)" else "Samples (clustered)",
        show_column_names = TRUE
      )
      
      list(ht = ht, hm_w_mm = dims$w_mm, hm_h_mm = dims$h_mm)
    }, ignoreInit = TRUE)
    
    output$plot_mol <- shiny::renderPlot({
      shiny::req(mol_nonce() > 0)
      obj <- mol_obj()
      .draw_bottom_legends(obj$ht)
    })
    
    output$dl_mol_pdf <- shiny::downloadHandler(
      filename = function() sprintf("heatmap_topvar_scaled_%s.pdf", Sys.Date()),
      content = function(file) {
        shiny::req(mol_nonce() > 0)
        obj <- mol_obj()
        d <- .hm_export_dims(obj$hm_w_mm, obj$hm_h_mm, extra_w_mm = 25, extra_h_mm = 115)
        grDevices::pdf(file, width = .hm_mm_to_in(d$dev_w_mm), height = .hm_mm_to_in(d$dev_h_mm))
        .draw_bottom_legends(obj$ht)
        grDevices::dev.off()
      }
    )
    
    output$dl_mol_png <- shiny::downloadHandler(
      filename = function() sprintf("heatmap_topvar_scaled_%s.png", Sys.Date()),
      content = function(file) {
        shiny::req(mol_nonce() > 0)
        obj <- mol_obj()
        d <- .hm_export_dims(obj$hm_w_mm, obj$hm_h_mm, extra_w_mm = 25, extra_h_mm = 115)
        dpi <- 220
        grDevices::png(file,
                       width  = .hm_mm_to_px(d$dev_w_mm, dpi),
                       height = .hm_mm_to_px(d$dev_h_mm, dpi),
                       res = dpi)
        .draw_bottom_legends(obj$ht)
        grDevices::dev.off()
      }
    )
  })
}
