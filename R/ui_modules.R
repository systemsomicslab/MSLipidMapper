library(shinyjs)
plotTabAppearanceUI <- function(title, imgSrc, width, height, outputId, outputId2, textOutputId) {
 card(
    h3(title),
    
    # コンテナ領域の設定
    div(
      style = "position: relative; height: 1500px;",
      
      # 1つ目のプロット
      jqui_draggable(
        jqui_resizable(
          div(style = "width: 600px; position: absolute; left: 0px; top: 20px;",
            wellPanel(
              style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              plotOutput(outputId, width = "100%", height = "600px")
            )
          ),
          options = list(
            handles = "e, w",
            minWidth = 200,
            maxWidth = 2000,
            grid = 50,
            animate = FALSE
          )
        ),
        options = list(
          grid = c(50, 50),
          containment = "parent",
          snap = TRUE,
          snapTolerance = 20
        )
      ),
	  
	  jqui_draggable(
        jqui_resizable(
          div(style = "width: 600px; position: absolute; left: 1300px; top: 20px;",
            wellPanel(
               style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
               verbatimTextOutput("stat_results"),
               tableOutput("summary_stats")
            )
          ),
          options = list(
            handles = "e, w",
            minWidth = 200,
            maxWidth = 2000,
            grid = 50,
            animate = FALSE
          )
        ),
        options = list(
          grid = c(50, 50),
          containment = "parent",
          snap = TRUE,
          snapTolerance = 20
        )
      ),
      
      # 2つ目のプロット
      jqui_draggable(
        jqui_resizable(
          div(style = "width: 600px; position: absolute; left: 650px; top: 20px;",
            wellPanel(
              style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              plotOutput(outputId2, width = "100%", height = "600px")
            )
          ),
          options = list(
            handles = "e, w",
            minWidth = 200,
            maxWidth = 2000,
            grid = 50,
            animate = FALSE
          )
        ),
        options = list(
          grid = c(50, 50),
          containment = "parent",
          snap = TRUE,
          snapTolerance = 20
        )
      )
    ),
    br(), br(), br(), br()
  )
}


         
createCustomSidebar <- function(y_id, w_id, z_id, alpha_id, size_id, fontsize_id,orderInputId, pValueCheckId, qInputId) {
   card(
    full_screen = TRUE,
    card_header(""),
    card_body(
    # カスタムCSSの追加
    # CSSの修正
tags$head(
  tags$style(HTML("
    .plot-type-container {
      display: flex;
      align-items: center;  /* 中央揃えに変更 */
      justify-content: flex-start; /* 左寄せに変更 */
      width: 100%;
      margin-bottom: 20px;
      padding: 10px 0;
    }

    .plot-buttons-container {
      display: flex;
      gap: 40px;  /* ボタン間の間隔を調整 */
      margin-left: 40px;  /* dropInputとの間隔 */
    }

    .round-button {
      border-radius: 50%;
      width: 55px;
      height: 55px;
      padding: 0;
      border: 2px solid #007bff;
      background-color: white;
      color: #007bff;
      transition: all 0.3s;
    }

    .button-with-label {
      display: flex;
      flex-direction: column;
      align-items: center;
      width: 70px;  /* 幅を固定 */
    }

    .button-label {
      text-align: center;
      font-size: 12px;  /* フォントサイズを小さく */
      margin-top: 8px;  /* ボタンとの間隔 */
      line-height: 1.3;  /* 行間を調整 */
      color: #666;
    }
  "))
),

# HTML構造の修正
div(
  class = "plot-type-container",
  # dropInput
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
  
  # ボタンコンテナ
   div(
    class = "plot-buttons-container",
    # 保存ボタン
    div(
      class = "button-with-label",
      actionButton("Colorpicker", 
                  label = icon("palette", class = "fa-lg"),  # アイコンサイズ調整 
                  class = "round-button"),
      div(
        class = "button-label",
        "Color",
        tags$br(),
        "setting"
      )
    ),
    
    # ダウンロードボタン
    div(
      class = "button-with-label",
      actionButton("exportgraph", 
                  label = icon("diagram-project", class = "fa-lg"),  # アイコンサイズ調整
                  class = "round-button"),
      div(
        class = "button-label",
        "Pathway",
        tags$br(),
        "mapping"
      )
    )
  )
),
accordion(
        accordion_panel(
		"Plot Settings",
    selectInput(inputId = y_id, label = "Y-axis:", choices = NULL),
	      selectInput(
        inputId = "mol",
        label = "molecule:",
        choices = NULL
      ),
    selectInput(inputId = w_id, label = "X-axis:", choices = c("Class"), selected = "Class"),
	numericInput("x_rotation", "X-axis Label Rotation:",
                  value = 0, min = -90, max = 90),
    sliderInput(inputId = alpha_id, label = "Transparency:", min = 0, max = 1, value = 1),
    chooseSliderSkin("Round", color = "DarkTurquoise"),
    sliderInput(inputId = size_id, label = "Size:", min = 0, max = 10, value = 5),
    numericInput(inputId = fontsize_id, label = "Fontsize:", value = 20, min = 1, max = 30),
	textInput("AxisLabel", "Enter x-axis label:", ""),
	textInput("yAxisLabel", "Enter y-axis label:", "Normalized expression"),
    orderInput(
      inputId = orderInputId,
      label = "Factor level order",
      items = c(""),
      width = "100%"
    ),
    selectInput(
      inputId = qInputId,
      label = "p-value (Tukey-Kramer test) from:",
      choices = c(""),
      selected = ""
    )),
	accordion_panel(
          "Statistical Test Settings",
		  checkboxInput(
      inputId = pValueCheckId,
      label = "Add p-value",
      value = FALSE
                         ),
          selectInput("test_method", "Statistical Test:",
                     choices = c("t-test" = "t_test",
                               "Wilcoxon" = "wilcox_test",
                               "ANOVA" = "anova_test")),
          
          conditionalPanel(
            condition = "input.test_method == 'anova_test'",
            selectInput("post_hoc", "Post-hoc test:",
                       choices = c("Tukey" = "tukey",
                                 "Bonferroni" = "bonferroni"))
          ),
          
          conditionalPanel(
            condition = "input.test_method != 'anova_test'",
            selectInput("p_adjust", "P-value adjustment:",
                       choices = c("Bonferroni" = "bonferroni",
                                 "Holm" = "holm",
                                 "BH" = "BH",
                                 "Hochberg" = "hochberg",
                                 "None" = "none"),
                       selected = "bonferroni")
          ),
          
          numericInput("significance", "Significance Level:",
                      value = 0.05, min = 0.01, max = 0.1, step = 0.01),
		  numericInput("pvaluefontsize", "Fontsize for pvalue symbol:",
                      value = 10, min = 1, max = 30),		  
        br(),
          div(
            class = "d-grid gap-2 d-md-flex",
            actionButton("select_all", "Select All", 
                        class = "btn-secondary"),
            actionButton("clear_all", "Clear All", 
                        class = "btn-secondary")),
						uiOutput("comparison_choices"))
          
        ),
    br(),
    h5("Ver 1.20241114"),
    br()
	)
  )
}

transcriptomeuploadpanel <- 
div(
          class = "input-panel",
          style = "
            background: white;
            padding: 25px;
            border-radius: 12px;
            box-shadow: 0 4px 15px rgba(0,0,0,0.08);
          ",
  div(
            class = "section-title",
            style = "
              margin-bottom: 25px;
              padding-bottom: 15px;
              border-bottom: 2px solid #2b2b2b;
            ",
            h3("Other Omics Data", 
               style = "
                 color: #2b2b2b;
                 font-weight: 500;
                 margin: 0;
               ")
          ),
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
  div(
            class = "section-title",
            style = "
              margin-bottom: 25px;
              padding-bottom: 15px;
              border-bottom: 2px solid #2b2b2b;
            ",
            h3("Input Meta Data", 
               style = "
                 color: #2b2b2b;
                 font-weight: 500;
                 margin: 0;
               ")
          ),
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
      # メタボローム入力パネル
      column(
        width = 3,
        div(
          class = "input-panel",
          style = "
            background: white;
            padding: 25px;
            border-radius: 12px;
            box-shadow: 0 4px 15px rgba(0,0,0,0.08);
          ",
          
          # メタボロームテーブル入力セクション
          div(
            class = "section-title",
            style = "
              margin-bottom: 25px;
              padding-bottom: 15px;
              border-bottom: 2px solid #2b2b2b;
            ",
            h3("Input Metabolome Table (.csv)", 
               style = "
                 color: #2b2b2b;
                 font-weight: 500;
                 margin: 0;
               ")
          ),
          
          # 入力フォーマット選択
          div(
            class = "input-group",
            style = "margin-bottom: 20px;",
            selectInput(
              inputId = "filetype",
              label = tags$span(
                style = "color: #5f6368; font-weight: 500;",
                "Input format:"
              ),
              choices = c("MS-DIAL export", "Sample in rows"),
              selected = "MS-DIAL export"
            )
          ),
          
          # ファイル入力フィールド
          div(
            class = "file-input-group",
            style = "margin-bottom: 20px;",
            fileInput(
              inputId = "file1",
              label = tags$span(
                style = "color: #5f6368; font-weight: 500;",
                "Input peak table:"
              ),
              accept = c("text/csv", 
                        "text/comma-separated-values,text/plain", 
                        ".csv"),
              width = "100%"
            )
          ),
          
          fileInput(
            inputId = "ontfile",
            label = tags$span(
              style = "color: #5f6368; font-weight: 500;",
              "Input ontology file:"
            ),
            accept = c("text/csv", 
                      "text/comma-separated-values,text/plain", 
                      ".csv"),
            width = "100%"
          ),
          
          tags$hr(style = "
            border: 0;
            height: 1px;
            background: #e0e0e0;
            margin: 25px 0;
          "),
          
          # テストデータセクション
          div(
            class = "section-title",
            style = "margin-bottom: 25px;",
            h3("Try Our Test Data", 
               style = "
                 color: #2b2b2b;
                 font-weight: 500;
                 border-bottom: 2px solid #2b2b2b;
                 padding-bottom: 15px;
               ")
          ),
          
          div(
    style = "text-align: left; margin: 20px;",
    downloadButton(
      outputId = "downloadData",
      label = "Download Demo Data",
      style = "
        background: transparent;
        color: #1a1a1a;
        border: 2px solid #1a1a1a;
        border-radius: 6px;
        font-size: 16px;
        font-weight: 500;
        padding: 10px 30px;
        transition: all 0.3s ease;
        &:hover {
          background: #1a1a1a;
          color: #f5f5f5;
          box-shadow: 0 2px 8px rgba(26,115,232,0.2);
        }
      "
    )
  ),
            div(
            class = "section-title",
            style = "
              margin-bottom: 25px;
              padding-bottom: 15px;
              border-bottom: 2px solid #2b2b2b;
            ",
            h3("Submit", 
               style = "
                 color: #2b2b2b;
                 font-weight: 500;
                 margin: 0;
               ")
          ),
          
          # 送信ボタン
          div(
            style = "margin: 20px; text-align: left;",
            actionButton(
              inputId = "submit",
              label = "Submit Data",
              class = "btn-submit",
              style = "
                background: transparent;
                color: #1a1a1a;
                border: 2px solid #1a1a1a;
                border-radius: 6px;
                font-size: 16px;
                font-weight: 500;
                padding: 10px 30px;
                transition: all 0.3s ease;
                &:hover {
                  background: #1a1a1a;
                  color: #f5f5f5;
                  box-shadow: 0 2px 8px rgba(26,115,232,0.2);
                }
              "
            )
          )
        )
      ),
      
      # トランスクリプトームアップロードパネル
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








