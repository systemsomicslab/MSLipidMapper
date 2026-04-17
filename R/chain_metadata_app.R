#' Annotate lipid chain metadata in a data frame
#'
#' @param data A data frame containing lipid names.
#' @param lipid_col Optional column name containing lipid names. When `NULL`,
#'   known lipid-name columns are auto-detected.
#' @param rules_yaml_path Optional path to the lipid rules YAML file.
#' @param acyl_col Output column name for acyl-chain metadata.
#' @param sphingoid_col Output column name for sphingoid-base metadata.
#'
#' @return A data frame with appended metadata columns.
#' @export
annotate_lipid_metadata <- function(
  data,
  lipid_col = NULL,
  rules_yaml_path = .mslm_rules_yaml_path(),
  acyl_col = "acyl_chains",
  sphingoid_col = "sphingoid_bases"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!nrow(data)) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }
  acyl_col <- if (is.null(acyl_col) || !nzchar(acyl_col)) "acyl_chains" else acyl_col
  sphingoid_col <- if (is.null(sphingoid_col) || !nzchar(sphingoid_col)) "sphingoid_bases" else sphingoid_col
  
  lipid_col <- lipid_col %||% .resolve_lipid_name_col(data)
  if (is.null(lipid_col) || !nzchar(lipid_col) || !lipid_col %in% colnames(data)) {
    stop(
      "Could not resolve a lipid name column. Set `lipid_col` explicitly.",
      call. = FALSE
    )
  }
  
  rules_yaml_path <- .mslm_rules_yaml_path(rules_yaml_path)
  if (is.null(rules_yaml_path) || !nzchar(rules_yaml_path) || !file.exists(rules_yaml_path)) {
    stop("Lipid rules YAML was not found.", call. = FALSE)
  }
  
  rules <- load_lipid_rules(rules_yaml_path)
  lipid_names <- as.character(data[[lipid_col]])
  metadata_list <- purrr::map(lipid_names, ~{
    out <- try(.parse_lipid_chain_metadata(.x, rules = rules), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) {
      list(acyl_chains = character(0), sphingoid_bases = character(0))
    } else {
      out
    }
  })
  
  acyl_list <- .ensure_listcol(purrr::map(metadata_list, ~ as.character(.x$acyl_chains %||% character(0))))
  sphingoid_list <- .ensure_listcol(purrr::map(metadata_list, ~ as.character(.x$sphingoid_bases %||% character(0))))
  
  out <- data
  out[[acyl_col]] <- acyl_list
  out[[sphingoid_col]] <- sphingoid_list
  out[[paste0(acyl_col, "_text")]] <- vapply(acyl_list, paste, collapse = " | ", character(1))
  out[[paste0(sphingoid_col, "_text")]] <- vapply(sphingoid_list, paste, collapse = " | ", character(1))
  out[[paste0(acyl_col, "_n")]] <- lengths(acyl_list)
  out[[paste0(sphingoid_col, "_n")]] <- lengths(sphingoid_list)
  out
}

.lipid_metadata_example_df <- function() {
  data.frame(
    Metabolite.name = c(
      "Hex2Cer 18:1;O2/24:1",
      "HexCer 18:1;O2/16:0",
      "Cer 18:1;O2/24:0",
      "SM 18:1;O2/16:0",
      "PC 16:0/18:1",
      "TG 16:0/18:1/18:2",
      "SPB 18:1;O2"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.combine_chain_metadata <- function(acyl, sphingoid) {
  acyl <- as.character(acyl %||% character(0))
  sphingoid <- as.character(sphingoid %||% character(0))
  out <- c(sphingoid, acyl)
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

.format_lipid_metadata_output <- function(
  df,
  lipid_col,
  acyl_col,
  sphingoid_col,
  output_format = c("long", "wide")
) {
  output_format <- match.arg(output_format)
  
  if (identical(output_format, "wide")) {
    list_cols <- vapply(df, is.list, logical(1))
    df[list_cols] <- lapply(df[list_cols], function(col) {
      vapply(col, paste, collapse = " | ", character(1))
    })
    return(df)
  }
  
  lipid_names <- as.character(df[[lipid_col]] %||% character(nrow(df)))
  acyl_list <- if (acyl_col %in% colnames(df)) .ensure_listcol(df[[acyl_col]]) else rep(list(character(0)), nrow(df))
  sphingoid_list <- if (sphingoid_col %in% colnames(df)) .ensure_listcol(df[[sphingoid_col]]) else rep(list(character(0)), nrow(df))
  
  long_rows <- Map(
    function(name, acyl, sphingoid) {
      chains <- .combine_chain_metadata(acyl = acyl, sphingoid = sphingoid)
      if (!length(chains)) {
        return(NULL)
      }
      data.frame(
        name = rep(name, length(chains)),
        chain_index = seq_along(chains),
        chain = chains,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },
    name = lipid_names,
    acyl = acyl_list,
    sphingoid = sphingoid_list
  )
  
  long_rows <- Filter(Negate(is.null), long_rows)
  if (!length(long_rows)) {
    return(data.frame(
      name = character(0),
      chain_index = integer(0),
      chain = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  
  out <- do.call(rbind, long_rows)
  rownames(out) <- NULL
  out
}

#' Launch a small Shiny app for lipid chain metadata extraction
#'
#' @param data A data frame containing a lipid name column. When `NULL`, the
#'   app starts with a small example table and also accepts CSV/TSV upload.
#' @param lipid_col Optional default column name containing lipid names.
#' @param rules_yaml_path Optional path to the lipid rules YAML file.
#' @param launch.browser Logical. If `TRUE`, open the app in a browser.
#' @param title App title shown in the UI.
#'
#' @return Invisibly, the result of [shiny::runApp()].
#' @export
run_lipid_metadata_app <- function(
  data = NULL,
  lipid_col = NULL,
  rules_yaml_path = .mslm_rules_yaml_path(),
  launch.browser = interactive(),
  title = "Lipid Chain Metadata"
) {
  data <- data %||% .lipid_metadata_example_df()
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!nrow(data)) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }
  
  default_lipid_col <- lipid_col %||% .resolve_lipid_name_col(data) %||% colnames(data)[1]
  rules_yaml_path <- .mslm_rules_yaml_path(rules_yaml_path)
  
  app_ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput(
          "input_file",
          "Input data frame (CSV/TSV)",
          accept = c(".csv", ".tsv", ".txt")
        ),
        shiny::selectInput(
          "file_sep",
          "Delimiter",
          choices = c("Comma (,)" = ",", "Tab" = "\t"),
          selected = ","
        ),
        shiny::selectInput(
          "lipid_col",
          "Lipid name column",
          choices = colnames(data),
          selected = default_lipid_col
        ),
        shiny::textInput(
          "acyl_col",
          "Acyl-chain column name",
          value = "acyl_chains"
        ),
        shiny::textInput(
          "sphingoid_col",
          "Sphingoid-base column name",
          value = "sphingoid_bases"
        ),
        shiny::selectInput(
          "output_format",
          "Output format",
          choices = c(
            "Long chain table" = "long",
            "Annotated wide table" = "wide"
          ),
          selected = "long"
        ),
        shiny::helpText("Preview and download a table annotated with chain metadata."),
        shiny::helpText("If no file is uploaded, the example data frame is used."),
        shiny::downloadButton("download_csv", "Download annotated CSV", width = "100%")
      ),
      shiny::mainPanel(
        shiny::verbatimTextOutput("input_summary"),
        shiny::verbatimTextOutput("summary"),
        DT::DTOutput("table")
      )
    )
  )
  
  app_server <- function(input, output, session) {
    current_data <- shiny::reactive({
      if (is.null(input$input_file)) {
        return(data)
      }
      
      sep <- input$file_sep %||% ","
      df <- try(
        utils::read.delim(
          input$input_file$datapath,
          sep = sep,
          header = TRUE,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        silent = TRUE
      )
      shiny::validate(
        shiny::need(!inherits(df, "try-error") && is.data.frame(df), "Failed to read the uploaded file."),
        shiny::need(nrow(df) > 0, "The uploaded file is empty.")
      )
      df
    })
    
    shiny::observe({
      df <- current_data()
      cols <- colnames(df)
      selected <- input$lipid_col
      if (is.null(selected) || !selected %in% cols) {
        selected <- .resolve_lipid_name_col(df) %||% cols[1]
      }
      shiny::updateSelectInput(session, "lipid_col", choices = cols, selected = selected)
    })
    
    output$input_summary <- shiny::renderText({
      df <- current_data()
      src <- if (is.null(input$input_file)) "Example data frame" else paste0("Uploaded file: ", input$input_file$name)
      paste0(
        src,
        "\nRows: ", nrow(df),
        "\nColumns: ", ncol(df)
      )
    })
    
    annotated <- shiny::reactive({
      shiny::req(input$lipid_col)
      acyl_col <- if (nzchar(input$acyl_col)) input$acyl_col else "acyl_chains"
      sphingoid_col <- if (nzchar(input$sphingoid_col)) input$sphingoid_col else "sphingoid_bases"
      annotate_lipid_metadata(
        data = current_data(),
        lipid_col = input$lipid_col,
        rules_yaml_path = rules_yaml_path,
        acyl_col = acyl_col,
        sphingoid_col = sphingoid_col
      )
    })
    
    output$summary <- shiny::renderText({
      df <- annotated()
      acyl_col <- if (nzchar(input$acyl_col)) input$acyl_col else "acyl_chains"
      sphingoid_col <- if (nzchar(input$sphingoid_col)) input$sphingoid_col else "sphingoid_bases"
      acyl_n <- sum(df[[paste0(acyl_col, "_n")]] > 0, na.rm = TRUE)
      sphingoid_n <- sum(df[[paste0(sphingoid_col, "_n")]] > 0, na.rm = TRUE)
      paste0(
        "Rows: ", nrow(df),
        "\nAnnotated with acyl chains: ", acyl_n,
        "\nAnnotated with sphingoid bases: ", sphingoid_n
      )
    })
    
    output$table <- DT::renderDT({
      df <- annotated()
      acyl_col <- if (nzchar(input$acyl_col)) input$acyl_col else "acyl_chains"
      sphingoid_col <- if (nzchar(input$sphingoid_col)) input$sphingoid_col else "sphingoid_bases"
      df <- .format_lipid_metadata_output(
        df = df,
        lipid_col = input$lipid_col,
        acyl_col = acyl_col,
        sphingoid_col = sphingoid_col,
        output_format = input$output_format %||% "long"
      )
      DT::datatable(
        df,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("lipid_metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- annotated()
        acyl_col <- if (nzchar(input$acyl_col)) input$acyl_col else "acyl_chains"
        sphingoid_col <- if (nzchar(input$sphingoid_col)) input$sphingoid_col else "sphingoid_bases"
        df <- .format_lipid_metadata_output(
          df = df,
          lipid_col = input$lipid_col,
          acyl_col = acyl_col,
          sphingoid_col = sphingoid_col,
          output_format = input$output_format %||% "long"
        )
        utils::write.csv(df, file, row.names = FALSE, na = "")
      }
    )
  }
  
  shiny::runApp(
    shiny::shinyApp(ui = app_ui, server = app_server),
    launch.browser = launch.browser
  )
}

#' Launch the lipid metadata mini app
#'
#' @param data Optional data frame containing lipid names. When `NULL`, a small
#'   bundled example-like data frame is used.
#' @param lipid_col Optional default column name containing lipid names.
#' @param rules_yaml_path Optional path to the lipid rules YAML file.
#' @param launch.browser Logical. If `TRUE`, open the app in a browser.
#' @param title App title shown in the UI.
#'
#' @return Invisibly, the result of [shiny::runApp()].
#' @export
launch_lipid_metadata_app <- function(
  data = NULL,
  lipid_col = NULL,
  rules_yaml_path = .mslm_rules_yaml_path(),
  launch.browser = interactive(),
  title = "Lipid Chain Metadata"
) {
  data <- data %||% .lipid_metadata_example_df()
  lipid_col <- lipid_col %||% .resolve_lipid_name_col(data) %||% colnames(data)[1]
  
  run_lipid_metadata_app(
    data = data,
    lipid_col = lipid_col,
    rules_yaml_path = rules_yaml_path,
    launch.browser = launch.browser,
    title = title
  )
}
