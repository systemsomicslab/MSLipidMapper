# ======================================================================
# Correlation module (UI + Server)  [Tabbed UI version]
#   Tabs (order):
#     - Single
#     - Focus
#     - All pair
#
# Updates in this revision:
#   - Fix Shiny downloadHandler error in module:
#       DO NOT do: output$id2 <- output$id1
#       Instead: define one download handler per output ID
#     - Focus CSV download button is unified (one button works from any Focus subtab)
#     - All pair header color unified to match other cards
# ======================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(SummarizedExperiment)
  library(ggplot2)
  library(DT)
  library(ComplexHeatmap)
  library(circlize)
  library(plotly)
})

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.character(a) && length(a) == 0) return(b)
  a
}

# ----------------------------------------------------------------------
# Helper: log2 transform with a pseudo-count for zeros
# ----------------------------------------------------------------------
.log2_with_pseudo <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(x)
  positive <- x[x > 0 & !is.na(x)]
  if (length(positive) == 0) return(x)
  x_min <- min(positive)
  x[x == 0] <- x_min / 2
  log2(x)
}

# ----------------------------------------------------------------------
# Compute all lipid × gene correlations
# ----------------------------------------------------------------------
compute_all_lipid_gene_cor <- function(
    se_lipid,
    se_tx,
    assay_name_lipid = "abundance",
    assay_name_tx    = "abundance",
    sample_id_col    = "sample_id",
    method           = c("pearson", "spearman")
) {
  method <- match.arg(method)
  
  cd_lip <- SummarizedExperiment::colData(se_lipid)
  cd_tx  <- SummarizedExperiment::colData(se_tx)
  
  key_lip <- if (!is.null(cd_lip) && sample_id_col %in% colnames(cd_lip)) {
    as.character(cd_lip[[sample_id_col]])
  } else {
    colnames(se_lipid)
  }
  names(key_lip) <- colnames(se_lipid)
  
  key_tx <- if (!is.null(cd_tx) && sample_id_col %in% colnames(cd_tx)) {
    as.character(cd_tx[[sample_id_col]])
  } else {
    colnames(se_tx)
  }
  names(key_tx) <- colnames(se_tx)
  
  if (anyDuplicated(key_lip)) warning("Duplicate sample_id values detected in the lipid SE. Matching may be ambiguous.")
  if (anyDuplicated(key_tx))  warning("Duplicate sample_id values detected in the transcript SE. Matching may be ambiguous.")
  
  common_ids <- intersect(key_lip, key_tx)
  if (length(common_ids) < 3) stop("Fewer than 3 shared samples. Correlation cannot be computed.")
  
  idx_lip <- match(common_ids, key_lip)
  idx_tx  <- match(common_ids, key_tx)
  
  mat_lip <- SummarizedExperiment::assay(se_lipid, assay_name_lipid)[, idx_lip, drop = FALSE]
  mat_tx  <- SummarizedExperiment::assay(se_tx,    assay_name_tx)[,  idx_tx,  drop = FALSE]
  
  X <- t(mat_lip)  # n_sample × n_lipid
  Y <- t(mat_tx)   # n_sample × n_gene
  
  cor_mat <- stats::cor(X, Y, use = "pairwise.complete.obs", method = method)
  
  X_ok <- !is.na(X)
  Y_ok <- !is.na(Y)
  n_mat <- crossprod(X_ok, Y_ok)
  
  r_vec  <- as.vector(cor_mat)
  n_vec  <- as.vector(n_mat)
  df_vec <- pmax(0, n_vec - 2L)
  
  denom <- pmax(1e-12, 1 - r_vec^2)
  t_vec <- r_vec * sqrt(df_vec / denom)
  t_vec[df_vec < 1] <- NA_real_
  
  p_vec <- 2 * stats::pt(-abs(t_vec), df = df_vec)
  p_vec[df_vec < 1] <- NA_real_
  
  fdr <- stats::p.adjust(p_vec, method = "BH")
  
  df_long <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  colnames(df_long) <- c("lipid_id", "gene_id", "r")
  df_long$p_value <- p_vec
  df_long$fdr     <- fdr
  df_long$n       <- n_vec
  
  df_long$lipid_id <- as.character(df_long$lipid_id)
  df_long$gene_id  <- as.character(df_long$gene_id)
  
  df_long
}

# ----------------------------------------------------------------------
# Compute correlations between one target lipid and all other lipids
# ----------------------------------------------------------------------
compute_focus_lipid_lipid_cor <- function(
    se_lipid,
    target_lipid,
    assay_name_lipid = "abundance",
    sample_id_col    = "sample_id",
    method           = c("pearson", "spearman")
) {
  method <- match.arg(method)
  
  mat_lip <- SummarizedExperiment::assay(se_lipid, assay_name_lipid)
  if (!(target_lipid %in% rownames(mat_lip))) {
    stop(sprintf("Lipid '%s' is not found in the lipid SE.", target_lipid))
  }
  
  X <- t(mat_lip)  # n_sample × n_lipid
  lipid_ids <- colnames(X)
  
  target_idx <- match(target_lipid, lipid_ids)
  if (is.na(target_idx)) stop(sprintf("Lipid '%s' is not found in the lipid SE.", target_lipid))
  
  r_vec <- as.numeric(stats::cor(
    X[, target_idx],
    X,
    use    = "pairwise.complete.obs",
    method = method
  ))
  names(r_vec) <- lipid_ids
  
  x_ok <- !is.na(X[, target_idx])
  Y_ok <- !is.na(X)
  n_vec <- as.numeric(crossprod(x_ok, Y_ok))  # 1 × n_lipid
  
  df_vec <- pmax(0, n_vec - 2L)
  denom  <- pmax(1e-12, 1 - r_vec^2)
  t_vec  <- r_vec * sqrt(df_vec / denom)
  t_vec[df_vec < 1] <- NA_real_
  
  p_vec <- 2 * stats::pt(-abs(t_vec), df = df_vec)
  p_vec[df_vec < 1] <- NA_real_
  
  fdr_vec <- stats::p.adjust(p_vec, method = "BH")
  
  df <- data.frame(
    lipid_id = lipid_ids,
    r        = r_vec,
    p_value  = p_vec,
    fdr      = fdr_vec,
    n        = n_vec,
    stringsAsFactors = FALSE
  )
  
  df <- df[df$lipid_id != target_lipid, , drop = FALSE]
  df
}

# ======================================================================
# UI
# ======================================================================
mod_plot_cor_ui <- function(id) {
  ns <- shiny::NS(id)
  
  wrap_id <- ns("scatter_wrap")
  fn_id   <- gsub("[^A-Za-z0-9_]", "_", wrap_id)
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .box.single-cor-box > .box-header { padding: 6px 10px; }
        .box.single-cor-box > .box-header .box-title { width: 100%; padding: 0; }
        .plot-box-body { padding: 8px; }
        .plotly.html-widget { width:100% !important; }

        /* ---- FORCE same dark-navy header for status=info ---- */
        .box.box-solid.box-info > .box-header{
          background:#163a59 !important;
          color:#fff !important;
        }
        .box.box-solid.box-info{
          border-top-color:#163a59 !important;
        }
      ")),
      shiny::tags$script(shiny::HTML(sprintf("
  function resize_%s(){
    var wrap = document.getElementById('%s');
    if(!wrap) return;
    var w = wrap.getBoundingClientRect().width;
    var size = Math.min(w, 520);
    if(!size || size <= 0) return;

    var gg = document.getElementById('%s');
    if(gg) gg.style.height = size + 'px';

    var pl = document.getElementById('%s');
    if(pl){
      pl.style.height = size + 'px';
      if(window.Plotly){
        try { Plotly.relayout(pl, {height: size}); } catch(e){}
      }
    }
  }
  $(window).on('resize', function(){ resize_%s(); });
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(){ setTimeout(resize_%s, 80); });
  $(document).on('shiny:connected', function(){ setTimeout(resize_%s, 200); });
", fn_id, wrap_id, ns("scatter_plot"), ns("scatter_plotly"), fn_id, fn_id, fn_id)))
    ),
    
    shinydashboard::tabBox(
      width = 12,
      
      # -------------------- Single --------------------
      shiny::tabPanel(
        title = "Single",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shinydashboard::box(
              width       = 12,
              status      = "primary",
              solidHeader = TRUE,
              class       = "single-cor-box",
              title       = "Controls",
              shiny::div(
                style = "max-height:620px; overflow:auto;",
                
                shiny::selectInput(
                  ns("single_mode"),
                  label   = "Mode",
                  choices = c(
                    "Lipid vs gene"       = "lipid_gene",
                    "Lipid ratio vs gene" = "lipid_ratio_gene",
                    "Lipid vs lipid"      = "lipid_lipid",
                    "Gene vs gene"        = "gene_gene"
                  ),
                  selected  = "lipid_lipid",
                  selectize = FALSE
                ),
                
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'lipid_gene'", ns("single_mode")),
                  shiny::selectInput(ns("single_lipid_lg"), "Lipid (X)", choices = NULL),
                  shiny::selectInput(ns("single_gene_lg"),  "Gene (Y)",  choices = NULL)
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'lipid_ratio_gene'", ns("single_mode")),
                  shiny::selectInput(ns("single_lipid_ratio_num"), "Lipid numerator (A)", choices = NULL),
                  shiny::selectInput(ns("single_lipid_ratio_den"), "Lipid denominator (B)", choices = NULL),
                  shiny::selectInput(ns("single_gene_rg"), "Gene (Y)", choices = NULL),
                  shiny::helpText("If Transform=Log2: X = log2(A)-log2(B) / If None: X = A/B")
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'lipid_lipid'", ns("single_mode")),
                  shiny::selectInput(ns("single_lipid_ll_x"), "Lipid (X)", choices = NULL),
                  shiny::selectInput(ns("single_lipid_ll_y"), "Lipid (Y)", choices = NULL)
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'gene_gene'", ns("single_mode")),
                  shiny::selectInput(ns("single_gene_gg_x"), "Gene (X)", choices = NULL),
                  shiny::selectInput(ns("single_gene_gg_y"), "Gene (Y)", choices = NULL)
                ),
                
                shiny::hr(),
                shiny::radioButtons(
                  ns("corr_method_single"), "Method",
                  choices  = c("Pearson" = "pearson", "Spearman" = "spearman"),
                  selected = "pearson"
                ),
                shiny::radioButtons(
                  ns("single_transform"), "Transform",
                  choices  = c("Log2 (pseudo-count for zeros)" = "log2", "None (raw values)" = "none"),
                  selected = "log2"
                ),
                shiny::selectInput(
                  ns("single_plot_type"),
                  label   = "View",
                  choices = c("Static" = "ggplot", "Interactive" = "plotly"),
                  selected  = "ggplot",
                  selectize = FALSE
                ),
                
                shiny::actionButton(ns("run_cor_single"), "Run", class = "btn btn-primary btn-sm"),
                shiny::tags$div(style = "height:6px;"),
                shiny::downloadButton(ns("download_scatter_gg"), "Download ggplot (PDF)", class = "btn btn-default btn-sm")
              )
            )
          ),
          
          shiny::column(
            width = 9,
            shinydashboard::box(
              width       = 12,
              status      = "primary",
              solidHeader = TRUE,
              title       = "Scatter plot",
              shiny::div(
                id    = wrap_id,
                class = "plot-box-body",
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'ggplot'", ns("single_plot_type")),
                  shiny::plotOutput(ns("scatter_plot"), height = "400px")
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'plotly'", ns("single_plot_type")),
                  plotly::plotlyOutput(ns("scatter_plotly"), height = "400px")
                )
              )
            )
          )
        )
      ),
      
      # -------------------- Focus --------------------
      shiny::tabPanel(
        title = "Focus",
        shinydashboard::tabBox(
          width = 12,
          
          shiny::tabPanel(
            title = "Lipid to lipids",
            shinydashboard::box(
              width=12, title="Focus lipid to correlated lipids (lipid × lipid)",
              status="info", solidHeader=TRUE,
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("focus_lipid_ll"), "Focus lipid (row in lipid SE)", choices=NULL)),
                shiny::column(3, shiny::numericInput(ns("focus_lipid_ll_abs_r_min"), "Min |r|", value=0.6, min=0, max=1, step=0.05)),
                shiny::column(3, shiny::numericInput(ns("focus_lipid_ll_fdr_max"), "Max FDR", value=0.1, min=0, max=1, step=0.01))
              ),
              shiny::div(style="margin-bottom:5px; display:flex; gap:8px; align-items:center;",
                         shiny::actionButton(ns("run_focus_lipid_ll"), "List correlated lipids", class="btn btn-primary btn-sm"),
                         shiny::downloadButton(ns("download_focus_csv"), "Download focus results (CSV)", class="btn btn-default btn-sm")
              ),
              DT::dataTableOutput(ns("tbl_focus_lipid_ll"))
            )
          ),
          
          shiny::tabPanel(
            title = "Lipid to genes",
            shinydashboard::box(
              width=12, title="Focus lipid to correlated genes (lipid × gene)",
              status="info", solidHeader=TRUE,
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("focus_lipid"), "Focus lipid (row in lipid SE)", choices=NULL)),
                shiny::column(3, shiny::numericInput(ns("focus_abs_r_min"), "Min |r|", value=0.6, min=0, max=1, step=0.05)),
                shiny::column(3, shiny::numericInput(ns("focus_fdr_max"), "Max FDR", value=0.1, min=0, max=1, step=0.01))
              ),
              shiny::div(style="margin-bottom:5px;",
                         shiny::actionButton(ns("run_focus_lipid"), "List correlated genes", class="btn btn-primary btn-sm")
              ),
              DT::dataTableOutput(ns("tbl_focus_lipid"))
            )
          ),
          
          shiny::tabPanel(
            title = "Gene to lipids",
            shinydashboard::box(
              width=12, title="Focus gene to correlated lipids (gene × lipid)",
              status="info", solidHeader=TRUE,
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("focus_gene"), "Focus gene (row in tx SE)", choices=NULL)),
                shiny::column(3, shiny::numericInput(ns("focus_gene_abs_r_min"), "Min |r|", value=0.6, min=0, max=1, step=0.05)),
                shiny::column(3, shiny::numericInput(ns("focus_gene_fdr_max"), "Max FDR", value=0.1, min=0, max=1, step=0.01))
              ),
              shiny::div(style="margin-bottom:5px;",
                         shiny::actionButton(ns("run_focus_gene"), "List correlated lipids", class="btn btn-primary btn-sm")
              ),
              DT::dataTableOutput(ns("tbl_focus_gene"))
            )
          )
        )
      ),
      
      # -------------------- All pair --------------------
      shiny::tabPanel(
        title = "All pair",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinydashboard::box(
              width       = 12,
              title       = "All pair correlation (lipid × gene, heatmap + table)",
              status      = "info",
              solidHeader = TRUE,
              
              shiny::fluidRow(
                shiny::column(4, shiny::selectInput(ns("method_screen"), "Method",
                                                    choices = c("Pearson"="pearson","Spearman"="spearman"),
                                                    selected="pearson")),
                shiny::column(4, shiny::numericInput(ns("abs_r_min"), "Min |r|",
                                                     value=0.5, min=0, max=1, step=0.05)),
                shiny::column(4, shiny::numericInput(ns("fdr_max"), "Max FDR",
                                                     value=0.05, min=0, max=1, step=0.01))
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("top_n_pairs"), "Top N (|r|)",
                                                     value=200, min=1, step=50)),
                shiny::column(4, shiny::numericInput(ns("max_lipids_heat"), "Max lipids",
                                                     value=30, min=1, step=5)),
                shiny::column(4, shiny::numericInput(ns("max_genes_heat"), "Max genes",
                                                     value=30, min=1, step=5))
              ),
              
              shiny::div(
                style="margin-bottom:5px; display:flex; gap:8px; align-items:center;",
                shiny::actionButton(ns("run_screen"), "Run all-pair scan", class="btn btn-info btn-sm"),
                shiny::downloadButton(ns("download_allpair_csv"), "Download all-pair results (CSV)", class="btn btn-primary btn-sm")
              ),
              
              shiny::plotOutput(ns("cor_heatmap"), height="320px"),
              shiny::hr(style="margin-top:10px; margin-bottom:5px;"),
              DT::dataTableOutput(ns("cor_table"))
            )
          )
        )
      )
    )
  )
}

# ======================================================================
# Server
# ======================================================================
mod_plot_cor_server <- function(
    id,
    se_lipid,
    se_tx = NULL,
    assay_name_lipid = "abundance",
    assay_name_tx    = "abundance",
    sample_id_col    = "sample_id",
    group_col        = "class",
    adv_reactive     = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    se_lipid_reactive <- if (shiny::is.reactive(se_lipid)) se_lipid else shiny::reactive(se_lipid)
    se_tx_reactive    <- if (!is.null(se_tx) && shiny::is.reactive(se_tx)) {
      se_tx
    } else if (!is.null(se_tx)) {
      shiny::reactive(se_tx)
    } else {
      shiny::reactive(NULL)
    }
    
    adv_get <- function() {
      if (is.null(adv_reactive)) return(NULL)
      if (shiny::is.reactive(adv_reactive)) return(adv_reactive())
      NULL
    }
    
    # --- group colors from adv$palette_map (fallback if missing) ---
    get_group_colors <- function(groups_chr) {
      g <- as.character(groups_chr)
      g[is.na(g) | !nzchar(g)] <- "NA"
      g <- trimws(g)
      lev <- sort(unique(g))
      
      adv <- adv_get()
      pal_adv <- if (!is.null(adv) && !is.null(adv$palette_map) && length(adv$palette_map)) adv$palette_map else NULL
      
      if (!is.null(pal_adv) && length(pal_adv)) {
        names(pal_adv) <- trimws(names(pal_adv))
        pal <- pal_adv
        miss <- setdiff(lev, names(pal))
        if (length(miss)) {
          add <- grDevices::hcl.colors(length(miss), "Dark 3")
          names(add) <- miss
          pal <- c(pal, add)
        }
        cols <- pal[lev]
        names(cols) <- lev
        return(cols)
      }
      
      cols <- grDevices::hcl.colors(length(lev), "Dark 3")
      names(cols) <- lev
      cols
    }
    
    # All-pair cache
    rv_all <- shiny::reactiveValues(df = NULL, method = NULL)
    
    # Keep latest focus results (for CSV export)
    rv_focus <- shiny::reactiveValues(
      lipid_ll   = NULL,
      lipid_gene = NULL,
      gene_lipid = NULL
    )
    
    # --------------------------------------------------------------
    # Init select choices
    # --------------------------------------------------------------
    shiny::observe({
      se_lip <- se_lipid_reactive()
      se_tx_ <- se_tx_reactive()
      if (is.null(se_lip)) return(NULL)
      
      lipid_rows <- rownames(se_lip)
      if (is.null(lipid_rows) || !length(lipid_rows)) return(NULL)
      gene_rows <- if (!is.null(se_tx_)) rownames(se_tx_) else NULL
      
      shiny::updateSelectInput(session, "single_lipid_lg", choices = lipid_rows, selected = lipid_rows[1])
      shiny::updateSelectInput(session, "single_lipid_ratio_num", choices = lipid_rows, selected = lipid_rows[1])
      shiny::updateSelectInput(session, "single_lipid_ratio_den", choices = lipid_rows, selected = lipid_rows[min(2, length(lipid_rows))])
      shiny::updateSelectInput(session, "single_lipid_ll_x", choices = lipid_rows, selected = lipid_rows[1])
      shiny::updateSelectInput(session, "single_lipid_ll_y", choices = lipid_rows, selected = lipid_rows[min(2, length(lipid_rows))])
      
      shiny::updateSelectInput(session, "focus_lipid", choices = lipid_rows, selected = lipid_rows[1])
      shiny::updateSelectInput(session, "focus_lipid_ll", choices = lipid_rows, selected = lipid_rows[1])
      
      if (!is.null(gene_rows) && length(gene_rows) > 0) {
        shiny::updateSelectInput(session, "single_gene_lg", choices = gene_rows, selected = gene_rows[1])
        shiny::updateSelectInput(session, "single_gene_rg", choices = gene_rows, selected = gene_rows[1])
        shiny::updateSelectInput(session, "single_gene_gg_x", choices = gene_rows, selected = gene_rows[1])
        shiny::updateSelectInput(session, "single_gene_gg_y", choices = gene_rows, selected = gene_rows[min(2, length(gene_rows))])
        shiny::updateSelectInput(session, "focus_gene", choices = gene_rows, selected = gene_rows[1])
      } else {
        shiny::updateSelectInput(session, "single_gene_lg", choices = character(0))
        shiny::updateSelectInput(session, "single_gene_rg", choices = character(0))
        shiny::updateSelectInput(session, "single_gene_gg_x", choices = character(0))
        shiny::updateSelectInput(session, "single_gene_gg_y", choices = character(0))
        shiny::updateSelectInput(session, "focus_gene", choices = character(0))
      }
    })
    
    # --------------------------------------------------------------
    # Single: compute
    # --------------------------------------------------------------
    cor_res_single <- shiny::eventReactive(input$run_cor_single, {
      se_lip <- se_lipid_reactive()
      se_tx_ <- se_tx_reactive()
      shiny::req(se_lip)
      
      mode      <- input$single_mode %||% "lipid_lipid"
      method    <- input$corr_method_single %||% "pearson"
      transform <- input$single_transform %||% "log2"
      
      cd_lip <- SummarizedExperiment::colData(se_lip)
      cd_tx  <- if (!is.null(se_tx_)) SummarizedExperiment::colData(se_tx_) else NULL
      
      key_lip <- if (!is.null(cd_lip) && sample_id_col %in% colnames(cd_lip)) {
        as.character(cd_lip[[sample_id_col]])
      } else colnames(se_lip)
      names(key_lip) <- colnames(se_lip)
      
      key_tx <- if (!is.null(se_tx_) && !is.null(cd_tx) && sample_id_col %in% colnames(cd_tx)) {
        as.character(cd_tx[[sample_id_col]])
      } else if (!is.null(se_tx_)) colnames(se_tx_) else NULL
      if (!is.null(key_tx)) names(key_tx) <- colnames(se_tx_)
      
      mat_lip <- SummarizedExperiment::assay(se_lip, assay_name_lipid)
      mat_tx  <- if (!is.null(se_tx_)) SummarizedExperiment::assay(se_tx_, assay_name_tx) else NULL
      
      do_tr  <- identical(transform, "log2")
      tr_fun <- function(v) if (do_tr) .log2_with_pseudo(v) else as.numeric(v)
      
      if (mode %in% c("lipid_gene", "lipid_ratio_gene", "gene_gene") && is.null(se_tx_)) {
        shiny::validate(shiny::need(FALSE, "Transcript SummarizedExperiment is not provided, so this mode is not available."))
      }
      
      if (mode %in% c("lipid_gene", "lipid_ratio_gene")) {
        common_ids <- intersect(key_lip, key_tx)
        shiny::validate(shiny::need(length(common_ids) >= 3, "Fewer than 3 shared samples. Correlation cannot be computed."))
        
        idx_lip <- match(common_ids, key_lip)
        idx_tx  <- match(common_ids, key_tx)
        
        group <- if (!is.null(cd_lip) && group_col %in% colnames(cd_lip)) {
          as.character(cd_lip[[group_col]])[idx_lip]
        } else rep("all", length(idx_lip))
        
        if (mode == "lipid_gene") {
          lipid_id <- input$single_lipid_lg
          gene_id  <- input$single_gene_lg
          shiny::req(lipid_id, gene_id)
          
          shiny::validate(
            shiny::need(lipid_id %in% rownames(mat_lip), sprintf("Lipid '%s' is not found in the lipid SE.", lipid_id)),
            shiny::need(gene_id  %in% rownames(mat_tx),  sprintf("Gene '%s' is not found in the transcript SE.", gene_id))
          )
          
          x <- tr_fun(mat_lip[lipid_id, idx_lip])
          y <- tr_fun(mat_tx[gene_id,  idx_tx])
          
          x_label <- lipid_id
          y_label <- gene_id
          
        } else {
          lipid_A <- input$single_lipid_ratio_num
          lipid_B <- input$single_lipid_ratio_den
          gene_id <- input$single_gene_rg
          shiny::req(lipid_A, lipid_B, gene_id)
          
          shiny::validate(
            shiny::need(lipid_A %in% rownames(mat_lip), sprintf("Lipid '%s' is not found in the lipid SE.", lipid_A)),
            shiny::need(lipid_B %in% rownames(mat_lip), sprintf("Lipid '%s' is not found in the lipid SE.", lipid_B)),
            shiny::need(gene_id  %in% rownames(mat_tx),  sprintf("Gene '%s' is not found in the transcript SE.", gene_id))
          )
          
          if (do_tr) {
            a <- .log2_with_pseudo(mat_lip[lipid_A, idx_lip])
            b <- .log2_with_pseudo(mat_lip[lipid_B, idx_lip])
            x <- a - b
            x_label <- sprintf("%s / %s (log2)", lipid_A, lipid_B)
          } else {
            a <- as.numeric(mat_lip[lipid_A, idx_lip])
            b <- as.numeric(mat_lip[lipid_B, idx_lip])
            x <- a / b
            x_label <- sprintf("%s / %s", lipid_A, lipid_B)
          }
          
          y <- tr_fun(mat_tx[gene_id, idx_tx])
          y_label <- gene_id
        }
        
      } else if (mode == "lipid_lipid") {
        common_ids <- key_lip
        idx_lip <- seq_along(common_ids)
        
        group <- if (!is.null(cd_lip) && group_col %in% colnames(cd_lip)) {
          as.character(cd_lip[[group_col]])[idx_lip]
        } else rep("all", length(idx_lip))
        
        lipid_x <- input$single_lipid_ll_x
        lipid_y <- input$single_lipid_ll_y
        shiny::req(lipid_x, lipid_y)
        
        shiny::validate(
          shiny::need(lipid_x %in% rownames(mat_lip), sprintf("Lipid '%s' is not found in the lipid SE.", lipid_x)),
          shiny::need(lipid_y %in% rownames(mat_lip), sprintf("Lipid '%s' is not found in the lipid SE.", lipid_y))
        )
        
        x <- tr_fun(mat_lip[lipid_x, idx_lip])
        y <- tr_fun(mat_lip[lipid_y, idx_lip])
        
        x_label <- lipid_x
        y_label <- lipid_y
        
      } else {
        shiny::req(se_tx_)
        
        common_ids <- key_tx
        idx_tx <- seq_along(common_ids)
        
        group <- if (!is.null(cd_tx) && group_col %in% colnames(cd_tx)) {
          as.character(cd_tx[[group_col]])[idx_tx]
        } else rep("all", length(idx_tx))
        
        gene_x <- input$single_gene_gg_x
        gene_y <- input$single_gene_gg_y
        shiny::req(gene_x, gene_y)
        
        shiny::validate(
          shiny::need(gene_x %in% rownames(mat_tx), sprintf("Gene '%s' is not found in the transcript SE.", gene_x)),
          shiny::need(gene_y %in% rownames(mat_tx), sprintf("Gene '%s' is not found in the transcript SE.", gene_y))
        )
        
        x <- tr_fun(mat_tx[gene_x, idx_tx])
        y <- tr_fun(mat_tx[gene_y, idx_tx])
        
        x_label <- gene_x
        y_label <- gene_y
      }
      
      data <- data.frame(
        X     = as.numeric(x),
        Y     = as.numeric(y),
        Group = as.character(group),
        stringsAsFactors = FALSE
      )
      
      filtered <- data[stats::complete.cases(data$X, data$Y), , drop = FALSE]
      shiny::validate(shiny::need(nrow(filtered) >= 3, "Fewer than 3 valid points after removing NAs."))
      
      ct <- stats::cor.test(filtered$X, filtered$Y, method = method)
      
      list(
        data        = data,
        filtered    = filtered,
        x_label     = x_label,
        y_label     = y_label,
        group_col   = group_col,
        cor_test    = ct,
        corr_method = method,
        transform   = transform
      )
    })
    
    scatter_gg <- shiny::reactive({
      res <- cor_res_single()
      shiny::req(res)
      
      data     <- res$data
      filtered <- res$filtered
      
      cols_fill <- get_group_colors(data$Group)
      
      data$Group <- as.character(data$Group)
      data$Group[is.na(data$Group) | !nzchar(data$Group)] <- "NA"
      data$Group <- trimws(data$Group)
      data$Group <- factor(data$Group, levels = names(cols_fill))
      
      r_val <- unname(res$cor_test$estimate)
      p_val <- res$cor_test$p.value
      label_txt <- sprintf("r = %.3f\np = %.3g", r_val, p_val)
      
      tr_tag <- if (identical(res$transform, "log2")) "Log2-transformed" else "Raw"
      xlab <- paste(tr_tag, res$x_label, "level")
      ylab <- paste(tr_tag, res$y_label, "level")
      
      ggplot(data, aes(x = X, y = Y)) +
        geom_point(aes(fill = Group), size = 3, shape = 21, alpha = 0.85, colour = "#000000") +
        geom_smooth(data = filtered, method = "lm", formula = y ~ x, level = 0.95, colour = "blue", se = TRUE) +
        annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.2, label = label_txt, size = 3.5) +
        scale_fill_manual(values = cols_fill, drop = FALSE) +
        labs(x = xlab, y = ylab, fill = res$group_col) +
        guides(fill = guide_legend(override.aes = list(fill = unname(cols_fill), colour = "#000000", shape = 21, alpha = 1))) +
        theme_classic(base_size = 10) +
        theme(legend.position = "bottom", aspect.ratio = 1)
    })
    
    output$scatter_plot <- shiny::renderPlot({
      p <- scatter_gg()
      shiny::req(p)
      p
    })
    
    output$scatter_plotly <- plotly::renderPlotly({
      res <- cor_res_single()
      shiny::req(res)
      
      data     <- res$data
      filtered <- res$filtered
      
      cols_fill <- get_group_colors(data$Group)
      
      data$Group <- as.character(data$Group)
      data$Group[is.na(data$Group) | !nzchar(data$Group)] <- "NA"
      data$Group <- trimws(data$Group)
      data$Group <- factor(data$Group, levels = names(cols_fill))
      
      tr_tag <- if (identical(res$transform, "log2")) "Log2-transformed" else "Raw"
      xlab <- paste(tr_tag, res$x_label, "level")
      ylab <- paste(tr_tag, res$y_label, "level")
      
      fit <- stats::lm(Y ~ X, data = filtered)
      xs <- seq(min(filtered$X, na.rm = TRUE), max(filtered$X, na.rm = TRUE), length.out = 100)
      pred <- stats::predict(fit, newdata = data.frame(X = xs), interval = "confidence", level = 0.95)
      df_line <- data.frame(X = xs, Y = pred[, "fit"], lwr = pred[, "lwr"], upr = pred[, "upr"])
      
      r_val <- unname(res$cor_test$estimate)
      p_val <- res$cor_test$p.value
      ann_txt <- sprintf("r = %.3f<br>p = %.3g", r_val, p_val)
      
      plot_ly(
        data = data,
        x = ~X, y = ~Y,
        type = "scatter", mode = "markers",
        color = ~Group,
        colors = unname(cols_fill),
        marker = list(size = 8, line = list(width = 1, color = "#000000")),
        hoverinfo = "text",
        text = ~paste0("Group: ", Group, "<br>X: ", signif(X, 4), "<br>Y: ", signif(Y, 4))
      ) |>
        add_lines(data = df_line, x = ~X, y = ~Y, inherit = FALSE,
                  line = list(color = "blue"), showlegend = FALSE, hoverinfo = "skip") |>
        add_ribbons(data = df_line, x = ~X, ymin = ~lwr, ymax = ~upr, inherit = FALSE,
                    showlegend = FALSE, hoverinfo = "skip", opacity = 0.2) |>
        layout(
          xaxis = list(title = xlab),
          yaxis = list(title = ylab),
          annotations = list(list(
            x = 1, y = 1, xref = "paper", yref = "paper",
            text = ann_txt, showarrow = FALSE,
            xanchor = "right", yanchor = "top"
          )),
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    })
    
    output$download_scatter_gg <- shiny::downloadHandler(
      filename = function() paste0("scatter_", Sys.Date(), ".pdf"),
      content = function(file) {
        p <- scatter_gg()
        shiny::req(p)
        grDevices::pdf(file, width = 7, height = 7, useDingbats = FALSE)
        print(p)
        grDevices::dev.off()
      }
    )
    
    # --------------------------------------------------------------
    # All pair scan
    # --------------------------------------------------------------
    shiny::observeEvent(input$run_screen, {
      se_lip <- se_lipid_reactive()
      se_tx_ <- se_tx_reactive()
      shiny::req(se_lip)
      
      if (is.null(se_tx_)) {
        shiny::showNotification(
          "Transcript SummarizedExperiment is not provided, so all-pair (lipid × gene) scan is not available.",
          type = "warning"
        )
        return(NULL)
      }
      
      method <- input$method_screen %||% "pearson"
      df_all <- compute_all_lipid_gene_cor(
        se_lipid         = se_lip,
        se_tx            = se_tx_,
        assay_name_lipid = assay_name_lipid,
        assay_name_tx    = assay_name_tx,
        sample_id_col    = sample_id_col,
        method           = method
      )
      rv_all$df     <- df_all
      rv_all$method <- method
    })
    
    filtered_res <- shiny::reactive({
      df <- rv_all$df
      shiny::req(df)
      
      thr_r   <- input$abs_r_min   %||% 0
      thr_fdr <- input$fdr_max     %||% 1
      top_n   <- input$top_n_pairs %||% NA_integer_
      
      df2 <- df[!is.na(df$r), , drop = FALSE]
      df2 <- df2[abs(df2$r) >= thr_r & (is.na(df2$fdr) | df2$fdr <= thr_fdr), , drop = FALSE]
      ord <- order(-abs(df2$r), df2$fdr, df2$lipid_id, df2$gene_id)
      df2 <- df2[ord, , drop = FALSE]
      
      if (is.finite(top_n) && top_n > 0 && nrow(df2) > top_n) {
        df2 <- df2[seq_len(top_n), , drop = FALSE]
      }
      df2
    })
    
    output$cor_table <- DT::renderDataTable({
      df <- filtered_res()
      shiny::req(df)
      
      view <- df[, c("lipid_id", "gene_id", "r", "p_value", "fdr", "n")]
      colnames(view) <- c("Lipid", "Gene", "r", "p_value", "FDR", "n")
      DT::datatable(view, selection = "single",
                    options = list(pageLength = 20, scrollX = TRUE))
    })
    
    output$cor_heatmap <- shiny::renderPlot({
      df <- filtered_res()
      shiny::req(df)
      
      max_lip <- input$max_lipids_heat %||% 30
      max_gen <- input$max_genes_heat  %||% 30
      
      if (nrow(df) == 0) {
        plot.new(); text(0.5, 0.5, "No correlation hits under the current thresholds.")
        return(invisible(NULL))
      }
      
      lipids <- unique(df$lipid_id)
      genes  <- unique(df$gene_id)
      if (length(lipids) > max_lip) lipids <- lipids[seq_len(max_lip)]
      if (length(genes)  > max_gen) genes  <- genes[seq_len(max_gen)]
      
      df_sub <- df[df$lipid_id %in% lipids & df$gene_id %in% genes, , drop = FALSE]
      if (nrow(df_sub) == 0) {
        plot.new()
        text(0.5, 0.5, "No pairs available for the heatmap.")
        return(invisible(NULL))
      }
      
      mat <- matrix(NA_real_, nrow = length(lipids), ncol = length(genes),
                    dimnames = list(lipids, genes))
      for (i in seq_len(nrow(df_sub))) {
        mat[df_sub$lipid_id[i], df_sub$gene_id[i]] <- df_sub$r[i]
      }
      
      keep_rows <- rowSums(!is.na(mat)) > 0
      keep_cols <- colSums(!is.na(mat)) > 0
      mat <- mat[keep_rows, keep_cols, drop = FALSE]
      if (nrow(mat) == 0 || ncol(mat) == 0) {
        plot.new(); text(0.5, 0.5, "No non-NA cells remain for the heatmap.")
        return(invisible(NULL))
      }
      
      mat[is.na(mat)] <- 0
      col_fun <- circlize::colorRamp2(c(-1, 0, 1), c("navy", "white", "firebrick"))
      ht <- ComplexHeatmap::Heatmap(
        mat,
        name = "r",
        col  = col_fun,
        cluster_rows    = nrow(mat) > 1,
        cluster_columns = ncol(mat) > 1,
        show_row_names  = TRUE,
        show_column_names = TRUE,
        rect_gp = grid::gpar(col = "grey80", lwd = 0.5)  # ?????????
      )
      
      ComplexHeatmap::draw(ht)
    })
    
    output$download_allpair_csv <- shiny::downloadHandler(
      filename = function() paste0("all_pair_results_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- filtered_res()
        shiny::req(df)
        utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # --------------------------------------------------------------
    # Focus: Lipid???genes
    # --------------------------------------------------------------
    focus_lipid_res <- shiny::eventReactive(input$run_focus_lipid, {
      se_lip <- se_lipid_reactive()
      se_tx_ <- se_tx_reactive()
      shiny::req(se_lip)
      
      if (is.null(se_tx_)) {
        shiny::showNotification(
          "Transcript SummarizedExperiment is not provided, so 'Focus lipid ??? genes' is not available.",
          type = "warning"
        )
        return(NULL)
      }
      
      method <- input$method_screen %||% "pearson"
      
      if (is.null(rv_all$df) || !identical(rv_all$method, method)) {
        df_all <- compute_all_lipid_gene_cor(
          se_lipid         = se_lip,
          se_tx            = se_tx_,
          assay_name_lipid = assay_name_lipid,
          assay_name_tx    = assay_name_tx,
          sample_id_col    = sample_id_col,
          method           = method
        )
        rv_all$df     <- df_all
        rv_all$method <- method
      } else {
        df_all <- rv_all$df
      }
      
      L       <- input$focus_lipid
      thr_r   <- input$focus_abs_r_min %||% 0
      thr_fdr <- input$focus_fdr_max   %||% 1
      shiny::req(L)
      
      df_L <- df_all[df_all$lipid_id == L & !is.na(df_all$r), , drop = FALSE]
      df_L <- df_L[abs(df_L$r) >= thr_r & (is.na(df_L$fdr) | df_L$fdr <= thr_fdr), , drop = FALSE]
      df_L <- df_L[order(-abs(df_L$r), df_L$fdr, df_L$gene_id), , drop = FALSE]
      
      rv_focus$lipid_gene <- df_L
      df_L
    })
    
    output$tbl_focus_lipid <- DT::renderDataTable({
      df <- focus_lipid_res()
      if (is.null(df)) return(NULL)
      view <- df[, c("gene_id", "r", "p_value", "fdr", "n")]
      colnames(view) <- c("Gene", "r", "p_value", "FDR", "n")
      DT::datatable(view, options = list(pageLength = 20, scrollX = TRUE), selection = "single")
    })
    
    # --------------------------------------------------------------
    # Focus: Gene???lipids
    # --------------------------------------------------------------
    focus_gene_res <- shiny::eventReactive(input$run_focus_gene, {
      se_lip <- se_lipid_reactive()
      se_tx_ <- se_tx_reactive()
      shiny::req(se_lip)
      
      if (is.null(se_tx_)) {
        shiny::showNotification(
          "Transcript SummarizedExperiment is not provided, so 'Focus gene ??? lipids' is not available.",
          type = "warning"
        )
        return(NULL)
      }
      
      method <- input$method_screen %||% "pearson"
      
      if (is.null(rv_all$df) || !identical(rv_all$method, method)) {
        df_all <- compute_all_lipid_gene_cor(
          se_lipid         = se_lip,
          se_tx            = se_tx_,
          assay_name_lipid = assay_name_lipid,
          assay_name_tx    = assay_name_tx,
          sample_id_col    = sample_id_col,
          method           = method
        )
        rv_all$df     <- df_all
        rv_all$method <- method
      } else {
        df_all <- rv_all$df
      }
      
      G       <- input$focus_gene
      thr_r   <- input$focus_gene_abs_r_min %||% 0
      thr_fdr <- input$focus_gene_fdr_max   %||% 1
      shiny::req(G)
      
      df_G <- df_all[df_all$gene_id == G & !is.na(df_all$r), , drop = FALSE]
      df_G <- df_G[abs(df_G$r) >= thr_r & (is.na(df_G$fdr) | df_G$fdr <= thr_fdr), , drop = FALSE]
      df_G <- df_G[order(-abs(df_G$r), df_G$fdr, df_G$lipid_id), , drop = FALSE]
      
      rv_focus$gene_lipid <- df_G
      df_G
    })
    
    output$tbl_focus_gene <- DT::renderDataTable({
      df <- focus_gene_res()
      if (is.null(df)) return(NULL)
      view <- df[, c("lipid_id", "r", "p_value", "fdr", "n")]
      colnames(view) <- c("Lipid", "r", "p_value", "FDR", "n")
      DT::datatable(view, options = list(pageLength = 20, scrollX = TRUE), selection = "single")
    })
    
    # --------------------------------------------------------------
    # Focus: Lipid???lipids
    # --------------------------------------------------------------
    focus_lipid_ll_res <- shiny::eventReactive(input$run_focus_lipid_ll, {
      se_lip <- se_lipid_reactive()
      shiny::req(se_lip)
      
      L       <- input$focus_lipid_ll
      thr_r   <- input$focus_lipid_ll_abs_r_min %||% 0
      thr_fdr <- input$focus_lipid_ll_fdr_max   %||% 1
      shiny::req(L)
      
      method <- input$corr_method_single %||% "pearson"
      
      df_all <- compute_focus_lipid_lipid_cor(
        se_lipid         = se_lip,
        target_lipid     = L,
        assay_name_lipid = assay_name_lipid,
        sample_id_col    = sample_id_col,
        method           = method
      )
      
      df_all <- df_all[!is.na(df_all$r), , drop = FALSE]
      df_all <- df_all[abs(df_all$r) >= thr_r & (is.na(df_all$fdr) | df_all$fdr <= thr_fdr), , drop = FALSE]
      df_all <- df_all[order(-abs(df_all$r), df_all$fdr, df_all$lipid_id), , drop = FALSE]
      
      rv_focus$lipid_ll <- df_all
      df_all
    })
    
    output$tbl_focus_lipid_ll <- DT::renderDataTable({
      df <- focus_lipid_ll_res()
      shiny::req(df)
      view <- df[, c("lipid_id", "r", "p_value", "fdr", "n")]
      colnames(view) <- c("Lipid", "r", "p_value", "FDR", "n")
      DT::datatable(view, options = list(pageLength = 20, scrollX = TRUE), selection = "single")
    })
    
    # --------------------------------------------------------------
    # Focus CSV download (ONE handler)
    # --------------------------------------------------------------
    focus_csv_data <- shiny::reactive({
      d1 <- rv_focus$lipid_ll
      d2 <- rv_focus$lipid_gene
      d3 <- rv_focus$gene_lipid
      
      out <- list()
      
      if (!is.null(d1) && nrow(d1) > 0) {
        tmp <- d1
        tmp$type <- "lipid_to_lipids"
        out[[length(out) + 1]] <- tmp[, c("type", "lipid_id", "r", "p_value", "fdr", "n")]
      }
      if (!is.null(d2) && nrow(d2) > 0) {
        tmp <- d2
        tmp$type <- "lipid_to_genes"
        out[[length(out) + 1]] <- tmp[, c("type", "lipid_id", "gene_id", "r", "p_value", "fdr", "n")]
      }
      if (!is.null(d3) && nrow(d3) > 0) {
        tmp <- d3
        tmp$type <- "gene_to_lipids"
        out[[length(out) + 1]] <- tmp[, c("type", "lipid_id", "gene_id", "r", "p_value", "fdr", "n")]
      }
      
      if (!length(out)) return(NULL)
      
      all_cols <- unique(unlist(lapply(out, colnames)))
      out2 <- lapply(out, function(d) {
        miss <- setdiff(all_cols, colnames(d))
        if (length(miss)) for (m in miss) d[[m]] <- NA
        d[, all_cols, drop = FALSE]
      })
      do.call(rbind, out2)
    })
    
    output$download_focus_csv <- shiny::downloadHandler(
      filename = function() paste0("focus_results_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- focus_csv_data()
        shiny::req(df)
        utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
  })
}
