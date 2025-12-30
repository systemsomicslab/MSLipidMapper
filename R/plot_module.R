# ======================================================================
# Plot module server  （R/mod_plot_wrapper.R）
# ======================================================================
#' Plot module server
#' @export
mod_plot_server <- function(id, se_lipid, se_tx = NULL, ...) {
  shiny::moduleServer(id, function(input, output, session) {

    # se_tx が未指定のときは、当面は脂質 SE を流用（仮）
    se_tx_reactive <- if (!is.null(se_tx)) se_tx else se_lipid

    # --- 脂質側モジュール ---
    mod_plot_lipid_server(
      id    = "lipid",
      se_in = se_lipid,
      ...
    )

    # --- 遺伝子側モジュール ---
    mod_plot_tx_server(
      id            = "tx",
      se_tx         = se_tx_reactive,
      assay_name_tx = "abundance"
    )

    # --- 相関解析モジュール ---
    mod_plot_cor_server(
      id       = "cor",
      se_lipid = se_lipid,
      se_tx    = se_tx_reactive
    )
  })
}

# ======================================================================
# Plot tab UI  （R/mod_plot_wrapper.R）
# ======================================================================

#' Plot tab UI
#' @export
mod_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tabsetPanel(
      id   = ns("plot_mode"),
      type = "tabs",

      shiny::tabPanel(
        title = "Lipid",
        value = "lipid",
        # ★ 脂質側 UI モジュール
        mod_plot_lipid_ui(ns("lipid"))
      ),

      shiny::tabPanel(
        title = "Transcriptome",
        value = "tx",
        # ★ 遺伝子側 UI モジュール
        mod_plot_tx_ui(ns("tx"))
      ),

      shiny::tabPanel(
        title = "Correlation",
        value = "cor",
        # ★ 新規: 脂質 × 遺伝子 相関モジュール
        mod_plot_cor_ui(ns("cor"))
      )
    )
  )
}
