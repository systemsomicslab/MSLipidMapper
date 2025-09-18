# ---- Dependencies ----
pkgs <- c("shiny","Spectra","dplyr","ggplot2","DT","mzR","lubridate")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(shiny); library(Spectra); library(dplyr); library(ggplot2); library(DT)
library(mzR); library(lubridate)

# ---- Helpers ----
# mzMLの計測開始日時（startTimeStamp; POSIXct）を取得
get_start_timestamp <- function(mzml_path) {
  ts <- NA
  suppressWarnings({
    con <- try(openMSfile(mzml_path), silent = TRUE)
    if (!inherits(con, "try-error")) {
      ri <- try(runInfo(con), silent = TRUE)  # data.frame
      close(con)
      if (!inherits(ri, "try-error") && "startTimeStamp" %in% names(ri)) {
        ts <- ri$startTimeStamp[1]
      }
    }
  })
  if (is.character(ts) && nzchar(ts)) {
    parsed <- suppressWarnings(ymd_hms(ts, tz = "UTC"))
    if (is.na(parsed)) parsed <- suppressWarnings(ymd_hms(ts))
    return(parsed)
  }
  as_datetime(file.info(mzml_path)$mtime)
}

# XICのピーク高さ(=RT範囲内のXIC最大値)
extract_peak_height <- function(mzml_path, mz_target, ppm = 10,
                                rt_center_min, rt_window_min = 0.5,
                                ms_level = 1L) {
  sps <- tryCatch(Spectra(mzml_path, backend = MsBackendMzR()),
                  error = function(e) return(NA_real_))
  if (!length(sps)) return(NA_real_)
  sps <- filterMsLevel(sps, ms_level)
  if (!length(sps)) return(NA_real_)
  tol_abs <- mz_target * ppm / 1e6
  sps <- filterMzRange(sps, c(mz_target - tol_abs, mz_target + tol_abs))
  if (!length(sps)) return(NA_real_)
  rt_center_sec <- rt_center_min * 60; half_win_sec <- rt_window_min * 60
  sps <- filterRt(sps, c(rt_center_sec - half_win_sec, rt_center_sec + half_win_sec))
  if (!length(sps)) return(NA_real_)
  xic_vals <- vapply(intensity(sps), function(iv) if (length(iv)) sum(iv, na.rm = TRUE) else 0, numeric(1))
  if (!length(xic_vals)) return(NA_real_)
  max(xic_vals, na.rm = TRUE)
}

# XICの曲線（rt_min, intensity）のデータフレームを返す
extract_xic_curve <- function(mzml_path, mz_target, ppm = 10,
                              rt_center_min, rt_window_min = 0.5,
                              ms_level = 1L) {
  sps <- tryCatch(Spectra(mzml_path, backend = MsBackendMzR()),
                  error = function(e) return(NULL))
  if (is.null(sps) || !length(sps)) return(NULL)
  sps <- filterMsLevel(sps, ms_level)
  if (!length(sps)) return(NULL)
  tol_abs <- mz_target * ppm / 1e6
  sps <- filterMzRange(sps, c(mz_target - tol_abs, mz_target + tol_abs))
  if (!length(sps)) return(NULL)
  rt_center_sec <- rt_center_min * 60; half_win_sec <- rt_window_min * 60
  sps <- filterRt(sps, c(rt_center_sec - half_win_sec, rt_center_sec + half_win_sec))
  if (!length(sps)) return(NULL)

  # 各スペクトルの合計強度と RT（min）
  rt_min <- rtime(sps) / 60
  xic    <- vapply(intensity(sps), function(iv) if (length(iv)) sum(iv, na.rm = TRUE) else 0, numeric(1))
  data.frame(rt_min = rt_min, intensity = xic)
}

list_mzml <- function(dir_path) {
  if (!dir.exists(dir_path)) return(character(0))
  list.files(dir_path, pattern = "\\.mzML$", full.names = TRUE, ignore.case = TRUE)
}

# ---- Shiny App ----
ui <- fluidPage(
  titlePanel("XIC Monitor: peak heights & multi-file chromatogram overlay"),
  sidebarLayout(
    sidebarPanel(
      textInput("dir", "フォルダパス（mzMLが入ったディレクトリ）", value = ""),
      actionButton("scan", "スキャン / 再読込"),
      tags$hr(),
      numericInput("mz", "Target m/z", value = 272.101, step = 0.001),
      numericInput("ppm", "ppm ウィンドウ (±)", value = 10, min = 1, step = 1),
      numericInput("rt_center", "中心RT (min)", value = 8.884, step = 0.001),
      numericInput("rt_win", "RT 幅 (±min)", value = 0.5, min = 0.01, step = 0.01),
      selectInput("mslevel", "MS level", choices = c(1,2), selected = 1),
      tags$hr(),
      selectizeInput("files_sel", "クロマトグラム表示ファイル",
                     choices = NULL, multiple = TRUE, options = list(placeholder = "スキャン後に選択")),
      downloadButton("dl_csv", "結果をCSVで保存")
    ),
    mainPanel(
      h4("A) Peak height trend"),
      plotOutput("trend", height = 300),
      DTOutput("tbl"),
      tags$hr(),
      h4("B) Chromatogram overlay"),
      plotOutput("chrom", height = 340),
      tags$br(),
      verbatimTextOutput("log")
    )
  )
)

server <- function(input, output, session) {
  r <- reactiveValues(files = character(0), res = NULL, log = "")

  observeEvent(input$scan, {
    req(nzchar(input$dir))
    files <- list_mzml(input$dir)
    r$log <- paste0("[", Sys.time(), "] Found ", length(files), " mzML file(s).")
    r$files <- files

    if (!length(files)) { r$res <- NULL; updateSelectizeInput(session, "files_sel", choices = NULL, server = TRUE); return() }

    withProgress(message = "Extracting peak heights ...", value = 0, {
      vals <- vector("list", length(files))
      for (i in seq_along(files)) {
        incProgress(1/length(files),
                    detail = paste0(basename(files[i]), " (", i, "/", length(files), ")"))
        acq_time <- tryCatch(get_start_timestamp(files[i]), error = function(e) NA)
        ph <- tryCatch(
          extract_peak_height(
            mzml_path = files[i],
            mz_target = input$mz,
            ppm = input$ppm,
            rt_center_min = input$rt_center,
            rt_window_min = input$rt_win,
            ms_level = as.integer(input$mslevel)
          ),
          error = function(e) NA_real_
        )
        vals[[i]] <- data.frame(
          file = basename(files[i]),
          path = files[i],
          acquisition_start = acq_time,
          peak_height = ph,
          stringsAsFactors = FALSE
        )
      }
      r$res <- dplyr::bind_rows(vals) |>
        arrange(acquisition_start)
    })

    # 選択肢を更新（ラベルは日時、値はフルパス）
    if (!is.null(r$res) && nrow(r$res) > 0) {
      lab <- paste0(format(r$res$acquisition_start, "%Y-%m-%d %H:%M"), " | ", r$res$file)
      names_vec <- lab; vals_vec <- r$res$path
      names(vals_vec) <- names_vec
      updateSelectizeInput(session, "files_sel", choices = vals_vec, server = TRUE)
    } else {
      updateSelectizeInput(session, "files_sel", choices = NULL, server = TRUE)
    }
  })

  # A) トレンド（x軸は startTimeStamp をラベルにして離散化）
  output$trend <- renderPlot({
    req(r$res, nrow(r$res) > 0)
    df <- r$res |> arrange(acquisition_start)
    df$acq_label <- format(df$acquisition_start, "%Y-%m-%d %H:%M")
    df$acq_label <- factor(df$acq_label, levels = df$acq_label)
    ggplot(df, aes(x = acq_label, y = peak_height)) +
      geom_point(fill = "steelblue", color = "black", alpha = 0.7, stroke = 0.5, shape = 21, size = 5) +
      geom_text(aes(label = round(peak_height, 0)), vjust = -0.3, size = 3) +
      labs(
        title = sprintf("XIC peak height (m/z=%.6f ± %d ppm, RT=%.3f ± %.3f min, MS%s)",
                        input$mz, input$ppm, input$rt_center, input$rt_win, input$mslevel),
        x = "Acquisition start time (startTimeStamp)", y = "Peak height (XIC max)"
      ) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$tbl <- renderDT({
    req(r$res, nrow(r$res) > 0)
    datatable(r$res, extensions = "Buttons",
              options = list(scrollX = TRUE, pageLength = 10))
  })

  # B) 選択ファイルのクロマトグラム重ね描き
  output$chrom <- renderPlot({
    req(length(input$files_sel) >= 1)
    sel_paths <- input$files_sel
    # 各ファイルのXICを集める
    dfs <- lapply(sel_paths, function(p) {
      df <- extract_xic_curve(
        mzml_path = p,
        mz_target = input$mz,
        ppm = input$ppm,
        rt_center_min = input$rt_center,
        rt_window_min = input$rt_win,
        ms_level = as.integer(input$mslevel)
      )
      if (is.null(df)) return(NULL)
      # ライン名：startTimeStamp + ファイル名
      st <- tryCatch(format(get_start_timestamp(p), "%Y-%m-%d %H:%M"), error = function(e) basename(p))
      df$label <- paste0(st, " | ", basename(p))
      df
    })
    dfs <- dfs[!vapply(dfs, is.null, logical(1))]
    req(length(dfs) > 0)
    d <- bind_rows(dfs)

    ggplot(d, aes(x = rt_min, y = intensity, color = label)) +
      geom_line(linewidth = 0.7, alpha = 0.9) +
      labs(
        title = sprintf("Chromatogram overlay: m/z=%.6f ± %d ppm, RT=%.3f ± %.3f min, MS%s",
                        input$mz, input$ppm, input$rt_center, input$rt_win, input$mslevel),
        x = "Retention time (min)", y = "XIC intensity", color = "File (startTimeStamp | name)"
      ) +
      theme_classic() +
      theme(legend.position = "bottom")
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      sprintf("xic_monitor_by_startTimeStamp_mz%.6f_ppm%d_rt%.3f±%.3f_MS%s_%s.csv",
              input$mz, input$ppm, input$rt_center, input$rt_win, input$mslevel,
              format(Sys.time(), "%Y%m%d-%H%M%S"))
    },
    content = function(file) {
      req(r$res); write.csv(r$res, file, row.names = FALSE)
    }
  )

  output$log <- renderText({ r$log })
}

shinyApp(ui, server)
