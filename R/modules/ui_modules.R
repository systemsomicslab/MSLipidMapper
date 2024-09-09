library(shinyjs)
plotTabAppearanceUI <- function(
    title, imgSrc, width, height, outputId, outputId2, textOutputId
) {
  mainPanel(
    width = 10,
    h3(title),
    wellPanel(
      plotlyOutput(outputId, width = "100%", height = "600px")
    ),
    selectInput(
      inputId = "mol",
      label = "molecule:",
      choices = NULL
    ),
    wellPanel(
      plotlyOutput(outputId2, width = "100%", height = "600px")
    ),
    textOutput(outputId = textOutputId),
    br(), br(), br(), br()
  )
}


footer <- dashboardFooter(
  left = tagList(
    # Define shared style for dropdown buttons and container
    tags$head(tags$style(HTML("
      #Colorpicker, #exportgraph {
        background-color: #3D3D3D;
        border-color: #3D3D3D;
      }
      .button-container {
        display: flex;
        flex-direction: row;
        align-items: center;
        gap: 10px;  /* 間隔を調整 */
      }
    "))),
    
    # Container for buttons
    div(class = "button-container",
      # Color Picker Dropdown Button
      dropdownButton(
        inputId = "Colorpicker",
        circle = TRUE, 
        status = "danger",
        icon = icon("palette"), 
        width = "500px", 
        tooltip = tooltipOptions(title = "Color option!")
      ),
      
      # Export Graph Dropdown Button
      dropdownButton(
        inputId = "exportgraph",
        circle = TRUE, 
        status = "danger",
        icon = icon("diagram-project"), 
        width = "500px", 
        tooltip = tooltipOptions(title = "Mapping option!")
      )
    )
  )
)

         
createCustomSidebar <- function(y_id, w_id, z_id, alpha_id, size_id, fontsize_id,orderInputId, pValueCheckId, qInputId) {
  sidebarPanel(
    width = 2,
    tags$h2("Plot setting"),
	h3("Plot type"),
    esquisse::dropInput(
      inputId = "mydrop",
      choicesNames = tagList(
        list(img(
          src = "./boxplot.png",
          style = "margin-left: 5px; margin-bottom: 4px; margin-top: 0px; width: 50px; height: 50px;"
        ), style = "width: 100px;"),
        list(img(
          src = "./barplot.png",
          style = "margin-left: 5px; margin-bottom: 4px; margin-top: 0px; width: 50px; height: 50px;"
        ), style = "width: 100px;"),
        list(img(
          src = "./violin2.png",
          style = "margin-left: 5px; margin-bottom: 4px; margin-top: 0px; width: 50px; height: 50px;"
        ), style = "width: 100px;"),
        list(img(
          src = "./dotplot.png",
          style = "margin-left: 5px; margin-bottom: 4px; margin-top: 0px; width: 50px; height: 50px;"
        ), style = "width: 100px;")
      ),
      choicesValues = c("box", "bar", "violin", "coding"),
      dropWidth = "220px"
    ),
    selectInput(inputId = y_id, label = "Y-axis:", choices = NULL),
    selectInput(inputId = w_id, label = "X-axis:", choices = c("Class"), selected = "Class"),
    sliderInput(inputId = alpha_id, label = "Alpha:", min = 0, max = 1, value = 1),
    chooseSliderSkin("Round", color = "DarkTurquoise"),
    sliderInput(inputId = size_id, label = "Size:", min = 0, max = 10, value = 5),
    numericInput(inputId = fontsize_id, label = "Fontsize:", value = 20, min = 1, max = 30),
	textInput("yAxisLabel", "Enter y-axis label:", "Normalized expression"),
    orderInput(
      inputId = orderInputId,
      label = "Factor level order",
      items = c(""),
      width = "100%"
    ),
    checkboxInput(
      inputId = pValueCheckId,
      label = "Add p-value",
      value = FALSE
    ),
    selectInput(
      inputId = qInputId,
      label = "p-value (Tukey-Kramer test) from:",
      choices = c(""),
      selected = ""
    ),
    br(),
    br(),
    h5("Ver 1.20240906"),
    br(),
    footer
  )
}

transcriptomeuploadpanel <- wellPanel(
  h3("Other Omics Data", style = "color: #2c3e50; border-bottom: 2px solid #2c3e50; padding-bottom: 10px;"),
  selectInput(
    inputId = "transcriptomefiletype",
    label = "Input format:",
    choices = c("Sample in cols"),
    selected = "Sample in cols"
  ),
  fileInput(
    inputId = "transcriptomefile",
    label = "Input transcriptome data:",
    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
  ),
  hr(),
  h3("Input Meta Data", style = "color: #2c3e50; border-bottom: 2px solid #2c3e50; padding-bottom: 10px;"),
  h5("If necessary, you can add class information.", style = "font-style: italic; color: #7f8c8d;"),
  fileInput(
    inputId = "file2",
    label = "Input meta data file:",
    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
  )
)

uploadpanel <- tabPanel(
  value = "uploadtab",
  tagList(
    div(
      style = "display: flex; align-items: center; justify-content: center; margin-bottom: 15px;",
      img(src = "./upload.png", style = "width: 50px; height: 50px; margin-right: 10px;"),
      div(style = "font-size: 18px; color: #000000; font-weight: bold;", "Data Upload")
    )
  ),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        h3("Input Metabolome Table (.csv)", style = "color: #2c3e50; border-bottom: 2px solid #2c3e50; padding-bottom: 10px;"),
        selectInput(
          inputId = "filetype",
          label = "Input format:",
          choices = c("MS-DIAL export", "Sample in rows"),
          selected = "MS-DIAL export"
        ),
        fileInput(
          inputId = "file1",
          label = "Input peak table:",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        fileInput(
          inputId = "ontfile",
          label = "Input ontology file:",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
		hr(),
		h3("Try Our Test Data", style = "color: #2c3e50; border-bottom: 2px solid #2c3e50; padding-bottom: 10px;"),
        fluidRow(
          column(2, radioButtons("radio", "", choices = c(" "))),
          column(10, downloadLink(outputId = "downloadData", label = "Download Aging Mouse Data", class = "btn btn-info btn-block"))
        ),
        div(style = "margin-top: 20px;",
          actionButton(
            inputId = "submit",
            label = "Submit Data",
            class = "btn-primary",
            style = "font-size: 16px; padding: 8px 24px; width: auto;"
          )
        )
      )
    ),
    column(
      width = 3,
      transcriptomeuploadpanel
    )
  )
)

originalplotTabAppearanceUI <- tabPanel(
  value = "test",
  tagList(
    img(
      src = "./cord.png",
      style = "margin-left: 5px; margin-bottom: 4px; margin-top: 0px; width: 50px; height: 50px;"
    ),
    div(
      style = "font-size: 10px; color: #000000; text-align: center;",
      HTML(' '), "coding", HTML(' ')
    )
  ),
  mainPanel(
    width = 10,
    h3("original"),
    fluidRow(
      column(
        width = 6,
        aceEditor(
          "code",
          value = "p <- ggplot(data = data, aes(y = Yvariable, x = Class)) + 
                  ggbeeswarm::geom_beeswarm(aes(fill = Class), 
                                            dodge.width = 0.1, 
                                            shape = 21,
                                            size = 3,
                                            cex = 1
                  ) +
                  stat_summary(
                    geom = \"crossbar\",
                    aes(fill = Class),
                    fun = mean,
                    position = position_dodge(0.9),
                    colour = \"red\",
                    linewidth = 0.5, width = 0.7,
                    show.legend = FALSE
                  ) +
                  theme_prism(
                    base_fontface = \"plain\", 
                    base_line_size = 0.9, 
                    base_family = \"Arial\"
                  ) +
                  scale_x_discrete(
                    guide = guide_prism_bracket(width = 0.1),
                    labels = scales::wrap_format(5)
                  ) +
                  theme(aspect.ratio = 1.0)
          p"
        ),
        actionButton("runButton", "Run Code")
      ),
      column(
        width = 6,
        plotOutput("plot"),
        downloadButton("downloadPlot", "Download Plot")
      )
    )
  )
)








