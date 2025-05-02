source("./modules/librarys.R")
source("./modules/data_processing.R")
source("./modules/plot_utils.R")
source("./modules/heatmap_utils.R")
source("./modules/modaldialog_utils.R")
source("./modules/selectcolor_utils.R")
source("./modules/ui_modules.R")
source("./modules/20191008 LIONweb functions.R")

shinyUI(
  fluidPage(
  titlePanel("MSLipidMapper"),
  useShinyjs(),
  # Create tabsetPanel with 3 tabs
  waiter::useWaiter(),
  uiOutput("dynamicSidebar"),
  uiOutput("dynamicSidebarToggle"),
  tabsetPanel(
    # Tab 1
    tabPanel(
      "Overview",
      fluidRow(
        column(12, 
               h2("Welcome to MSLipidMapper"),
               p("This application helps you visualize and analyze metabolic pathways."),
               h3("Available Features:"),
               tags$ul(
                 tags$li("Pathway Visualization - Explore different pathway types using interactive network graphs"),
                 tags$li("Network Customization - Change colors, export visualizations, and analyze relationships"),
                 tags$li("Data Analysis - Select nodes to see detailed information about pathway elements")
               ),
               h3("How to Use:"),
               p("Navigate through the tabs to access different functionalities:"),
               tags$ol(
                 tags$li(strong("Overview"), " - Introduction and general information"),
                 tags$li(strong("Data Import"), " - Tools for importing and managing data"),
                 tags$li(strong("Network Visualization"), " - The main pathway visualization interface")
               )
        )
      )
    ),
    # Tab 2
    tabPanel(
      "Data Import",
      value = "uploadtab",
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
        )
      )
    ),
    tabPanel(
      "Data Analysis",
      fluidPage(
        # フレックスボックスでレイアウトを作成
        tags$div(
          style = "display: flex; width: 100%; height: calc(100vh - 80px);",
          
          # サイドバー領域
          tags$div(
            style = "flex: 0 0 400px; background-color: #f8f9fa; border-right: 1px solid #dee2e6; padding: 15px; overflow-y: auto;",
            createCustomSidebar("y", "w", "z", "alpha", "size", "Fontsize", "levels", "pvaluecheck", "q")
          ),
          
          # メイン領域
          tags$div(
            style = "flex: 1; position: relative; padding: 20px; overflow: hidden;",
            
            # First plot
            jqui_draggable(
              jqui_resizable(
                div(style = "width: 600px; position: absolute; left: 10px; top: 20px;",
                    wellPanel(
                      style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                      plotOutput("plottest", width = "100%", height = "600px")
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
            
            # Stats results
            jqui_draggable(
              jqui_resizable(
                div(style = "width: 600px; position: absolute; left: 10px; top: 700px;",
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
            
            # Second plot
            jqui_draggable(
              jqui_resizable(
                div(style = "width: 600px; position: absolute; left: 700px; top: 20px;",
                    wellPanel(
                      style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                      plotOutput("plottest2", width = "100%", height = "600px")
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
          )
        )
      )
    ),
    
    # Tab 4 - Network Visualization with added ggplot figures
    tabPanel(
      "Network Visualization",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            h4("Pathway projection"),
            selectInput("pathwaytype", "Select Pathway Type:",
                        choices = c("Global pathway", 
                                    "Ceramide pathway",
                                    "Remodeling pathway"),
                        selected = "Global pathway"),
            
            hr(),
            
            # エクスポート機能
            h4("Export Network"),
            fluidRow(
              column(6, selectInput("export_format", "Format:", 
                                    choices = c("PNG" = "png", "JPG" = "jpg", "SVG" = "svg", "PDF" = "pdf"),
                                    selected = "png")),
              column(6, numericInput("export_scale", "Scale:", 2, min = 1, max = 10, step = 0.5))
            ),
            fluidRow(
              column(12, textInput("export_filename", "Filename:", "network_export"))
            ),
            actionButton("export_btn", "Export Network", class = "btn-success btn-block"),
            br(),
            downloadButton("exportCYJS", "Export CYJS", class = "btn-block"),
            downloadButton("exportStyles", "Export styles.xml", class = "btn-block"),
            hr(),
            
            # Selection information panel
            h4("Selected Elements Information"),
            verbatimTextOutput("selection_info"),
            
            # Node relationship information
            conditionalPanel(
              condition = "input.selected_nodes && input.selected_nodes.length == 2",
              h4("Node Relationship"),
              verbatimTextOutput("node_relationship")
            ),
            
            hr(),
            
            # ランダムなノード色変更セクション
            h4("Random Node Colors"),
            numericInput("num_nodes", "Number of nodes to change:", 5, min = 1, max = 50),
            actionButton("highlight_significant", "Apply Random Colors", class = "btn-primary"),
            actionButton("reset_colors", "Reset Colors", class = "btn-warning"),
            
            # New controls for ggplot visualization
            width = 3
          ),
          
          mainPanel(
            width = 9,
            # Network visualization
            div(
              style = "margin-bottom: 30px;",  # Add space between network and plots
              tags$div(id = "cy", style = "height:600px; border:1px solid #ccc;")
            ),
            
            # New section for ggplot visualizations
fluidPage(
  tabsetPanel(
    id = "mainTabset",
    tabPanel("Lipidomics Analysis", 
      # 最初のタブセットパネル全体をここに移動
      tabsetPanel(
        # 最初のタブ - 現在の可視化パネル
        tabPanel("Visualization", 
          div(
            # 元の2つのパネルを含むコンテナ
            fluidRow(
              column(4, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("Lipid species expression", style = "text-align: center;"),
                  plotOutput("corselect2", height = "400px"),
                  selectInput(
                    inputId = "selectmol",
                    label = "Select molecule to display",
                    choices = c(" "),
                    selected = " "
                  )
                )
              ),
              column(8, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("Heatmap", style = "text-align: center;"),
                  plotOutput("heatmap", height = "400px"),
                  checkboxInput(inputId = "acylfilter", label = "Filtering common acylchains (16:0, 16:1, 18:0, 18:1, 18:2, 18:3, 20:3, 20:4, 20:5, 22:4, 22:5, 22:6)", value = TRUE),
                  checkboxInput(inputId = "sn", label = "sn", value = FALSE)
                )
              )
            )
          )
        ),
        
        # 2つ目のタブ - 新しい可視化パネル1
        tabPanel("Corrlation",
          div(
            fluidRow(
              column(4, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("グラフ1", style = "text-align: center;"),
                  plotlyOutput("corselect", height = "400px"),
                  selectInput(
                    inputId = "new_select1",
                    label = "変数を選択",
                    choices = c("変数1", "変数2", "変数3"),
                    selected = "変数1"
                  )
                )
              ),
              column(6, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("グラフ2", style = "text-align: center;"),
                  plotOutput("new_plot2", height = "400px"),
                  sliderInput(
                    inputId = "new_slider1",
                    label = "パラメータ調整",
                    min = 0,
                    max = 100,
                    value = 50
                  )
                )
              )
            )
          )
        ),
        
        # 3つ目のタブ - 新しい可視化パネル2
        tabPanel("Enrichment",
          div(
            fluidRow(
              column(4, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("総合分析", style = "text-align: center;"),
                  actionButton("settingsButton", "Settings", class = "settings-button"),
                  actionButton("runAnalysisButton", "Run Analysis", class = "run-button"),
                  tabsetPanel(id = "leftTabs",
                    tabPanel("グラフ", 
                      plotOutput("volcanoPlot", height = "400px")
                    ),
                    tabPanel("情報", 
                      plotlyOutput("volcanoPlotly", height = "400px")
                    )
                  )
                )
              ),
              column(8, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("総合分析", style = "text-align: center;"),
                  DT::dataTableOutput("resultsTable"),
                  downloadButton("downloadResults", "Download Results as CSV")
                )
              ),
            ),
            
            fluidRow(
              column(4,
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-top: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  # ボタンとNumeric Inputはタブの外側に配置し、どのタブからでもアクセス可能にする
                  h4("統計情報コントロール", style = "text-align: center;"),
                  
                  # 横並びにするためのネストされたfluidRow
                  fluidRow(
                    column(6, # 幅を調整して2つのコントロールが横に並ぶようにする
                      numericInput("lionPvalThreshold", "LION p-value Threshold:",
                               value = 0.05, min = 0, max = 1, step = 0.01)
                    ),
                    column(6,
                      selectInput(
                              inputId = "enrichtarget",
                              label = "target variable",
                              choices = c("Up", "Down"),
                              selected = "Up"
                      )
                    )
                  ),
                  
                  # ボタンは全幅で配置
                  div(style = "margin-top: 10px;",
                    actionButton("runLIONButton", "Run LION Enrichment",
                             class = "btn-primary", width = "100%")
                  ),
                  
                  br(),
                  
                  # タブセットパネルを追加
                  tabsetPanel(id = "leftTabsEnrich",
                    tabPanel("グラフ", 
                      plotOutput("enrichbarplot", height = "400px")
                    ),
                    tabPanel("情報", 
                      plotlyOutput("enrich", height = "400px")
                    )
                  )
                )
              ),
              column(8,
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-top: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("統計情報2", style = "text-align: center;"),
                  DT::dataTableOutput("enrichtable")
                )
              )
            )
          ),
          # Modal dialog for settings
          bsModal(
            id = "settingsModal",
            title = "Analysis Settings",
            trigger = "settingsButton",
            size = "large",
            
            fluidRow(
              column(4,
                    # Data section
                    h4("Data Selection"),
                    selectInput("dataset", "Sample Dataset:",
                                choices = c("Lipid subclass" = "myData", "Lipid species" = "moldata")),
                    
              ),
              
              column(4,
                    # Analysis settings
                    h4("Analysis Settings"),
                    # Group selection
                    uiOutput("groupColumnSelector"),
                    uiOutput("group1Selector"),
                    uiOutput("group2Selector"),
                    
                    # Statistical settings
                    selectInput("testMethod", "Test Method:",
                                choices = c("t-test" = "t.test", "Wilcoxon test" = "wilcox"),
                                selected = "t.test"),
                    
                    selectInput("pAdjustMethod", "p-value Adjustment Method:",
                                choices = c("None" = "none", 
                                            "Bonferroni" = "bonferroni", 
                                            "Holm" = "holm", 
                                            "Hochberg" = "hochberg", 
                                            "Hommel" = "hommel", 
                                            "BH (FDR)" = "BH", 
                                            "BY" = "BY"),
                                selected = "none")
              ),
              
              column(4,
                    # Threshold and visualization settings
                    h4("Threshold Settings"),
                    numericInput("fcThresholdUp", "FC Threshold (Up):", 
                                value = 0.5, min = 0, max = 10, step = 0.1),
                    numericInput("fcThresholdDown", "FC Threshold (Down):", 
                                value = -0.5, min = -10, max = 0, step = 0.1),
                    numericInput("pThreshold", "p-value Threshold:", 
                                value = 0.05, min = 0, max = 1, step = 0.01),
                    
                    h4("Visualization Settings"),
                    numericInput("pointSize", "Point Size:", 
                                value = 3, min = 1, max = 10),
                    numericInput("pointAlpha", "Point Transparency:", 
                                value = 0.8, min = 0.1, max = 1, step = 0.1),
                    numericInput("fontSize", "Font Size:", 
                                value = 15, min = 8, max = 20),
                    
                    h4("Color Settings"),
                    textInput("colorNS", "Non-significant:", value = "grey50"),
                    textInput("colorUp", "Up-regulated:", value = "#FF4B4B"),
                    textInput("colorDown", "Down-regulated:", value = "#4B4BFF")
              )
            )
          )
        )
      )
    ),
    
    # 新しいメインタブ
    tabPanel("Advanced Tools", 
      # 2つ目のタブセットパネル全体をここに移動
      tabsetPanel(
        # 1つ目のタブ
        tabPanel("Advanced Analysis", 
          div(
            fluidRow(
              column(4, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("総合分析", style = "text-align: center;"),
                  tabsetPanel(id = "leftTabs",
                    tabPanel("グラフ", 
                    ),
                    tabPanel("情報", 
                    )
                  )
                )
              ),
              column(8, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("総合分析", style = "text-align: center;"),
                  DT::dataTableOutput("resultsTable2"),
                  downloadButton("downloadResults2", "Download Results as CSV")
                )
              )
            )
          )
        ),
        
        # 2つ目のタブ
        tabPanel("Batch Processing",
          div(
            fluidRow(
              column(4, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("バッチ処理設定", style = "text-align: center;"),
                  fileInput("batchFile", "バッチファイル:",
                         accept = c(".zip", ".xlsx")),
                  checkboxInput("batchParallel", "並列処理を使用", value = TRUE),
                  sliderInput(
                    inputId = "batchCores",
                    label = "使用コア数",
                    min = 1,
                    max = 8,
                    value = 4
                  )
                )
              ),
              column(8, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("バッチ処理状況", style = "text-align: center;"),
                  verbatimTextOutput("batchLog"),
                  # ここでshinyWidgetsのprogressBarを使用
                  # 使えない場合はshinyのprogressBarを使用するか、別の方法で進捗を表示
                  # progressBar(id = "batchProgress", value = 0),
                  # 代替として：
                  div(
                    style = "width: 100%; margin: 10px 0;",
                    div(
                      id = "batchProgress",
                      style = "width: 0%; height: 20px; background-color: #4CAF50; text-align: center; line-height: 20px; color: white; border-radius: 3px;"
                    )
                  ),
                  actionButton("startBatchButton", "バッチ処理開始", class = "btn-primary"),
                  actionButton("stopBatchButton", "処理中止", class = "btn-danger")
                )
              )
            ),
            fluidRow(
              column(12,
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-top: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("バッチ処理結果", style = "text-align: center;"),
                  DT::dataTableOutput("batchResults"),
                  downloadButton("downloadBatchResults", "結果ダウンロード")
                )
              )
            )
          )
        ),
        
        # 3つ目のタブ
        tabPanel("Report Generation",
          div(
            fluidRow(
              column(4, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("レポート設定", style = "text-align: center;"),
                  textInput("reportTitle", "レポートタイトル", value = "分析レポート"),
                  textInput("reportAuthor", "著者名", value = ""),
                  dateInput("reportDate", "レポート日付", value = Sys.Date()),
                  selectInput(
                    inputId = "reportFormat",
                    label = "出力形式",
                    choices = c("PDF", "HTML", "Word"),
                    selected = "PDF"
                  ),
                  checkboxInput("includeCode", "Rコードを含める", value = FALSE)
                )
              ),
              column(8, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("レポートプレビュー", style = "text-align: center;"),
                  
                  # タブセットパネルをネスト
                  tabsetPanel(
                    tabPanel("構成", 
                      verbatimTextOutput("reportStructure")
                    ),
                    tabPanel("プレビュー", 
                      htmlOutput("reportPreview")
                    )
                  ),
                  
                  actionButton("generateReportButton", "レポート生成", class = "btn-success"),
                  downloadButton("downloadReport", "レポートダウンロード")
                )
              )
            )
          )
        )
      )
    ),
    
    # ここに新しいメインタブをさらに追加することができます
    tabPanel("Experimental Features",
      tabsetPanel(
        tabPanel("実験機能1", 
          div(
            fluidRow(
              column(12, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("実験的な機能1", style = "text-align: center;"),
                  plotOutput("expPlot1", height = "300px"),
                  p("この機能は開発中です。将来のバージョンで機能が拡張される予定です。")
                )
              )
            )
          )
        ),
        tabPanel("実験機能2", 
          div(
            fluidRow(
              column(12, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("実験的な機能2", style = "text-align: center;"),
                  plotOutput("expPlot2", height = "300px"),
                  p("この機能は開発中です。将来のバージョンで機能が拡張される予定です。")
                )
              )
            )
          )
        ),
        tabPanel("実験機能3", 
          div(
            fluidRow(
              column(12, 
                div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  h4("実験的な機能3", style = "text-align: center;"),
                  plotOutput("expPlot3", height = "300px"),
                  p("この機能は開発中です。将来のバージョンで機能が拡張される予定です。")
                )
              )
            )
          )
        )
      )
    )
  )
),
            
            # 隠しダウンロードリンク
            tags$a(id = "download-link", style = "display:none;"),
            
            tags$head(
              tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.26.0/cytoscape.min.js"),
              tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
              tags$style(HTML("
              #cy {
                width: 100%;
                height: 800px;
                position: relative;
                border: 1px solid #ddd;
                border-radius: 4px;
              }
            "))
            ),
            
            tags$script(HTML("
            var cy = null;
            var originalNodeStyles = {}; // ノードの元のスタイルを保存
            var originalEdgeStyles = {}; // エッジの元のスタイルを保存
            
            function initCytoscape(data, styleData) {
              if (cy) {
                cy.destroy();
              }
              
              const defaultStyles = [
                {
                  selector: 'node',
                  style: {
                    'border-width': '1px',
                    'border-color': 'gray',
                    'border-style': 'solid',
                    'background-color': 'white',
                    'shape': 'rectangle',
                    'background-image': 'data(path)',
                    'label': 'data(shared_name)',
                    'height': 'data(Height)',
                    'width': 'data(Width)',
                    'font-size': 'data(Label_size)'
                  }
                },
                {
                  selector: 'node:selected',
                  style: {
                    'border-width': '3px',
                    'border-color': '#FFA500'
                  }
                },
                {
                  selector: 'edge',
                  style: {
                    'width': 2,
                    'line-color': '#888',
                    'curve-style': 'bezier',
                    'target-arrow-shape': 'triangle',
                    'target-arrow-color': '#888',
                    'arrow-scale': 1.5
                  }
                },
                {
                  selector: 'edge:selected',
                  style: {
                    'width': 3,
                    'line-color': '#ff0000',
                    'target-arrow-color': '#ff0000'
                  }
                }
              ];
              
              const combinedStyles = defaultStyles.concat(styleData || []);
              
              cy = cytoscape({
                container: document.getElementById('cy'),
                elements: data,
                style: combinedStyles,
                layout: {
                  name: 'preset',
                  animate: false,
                  padding: 30,
                  positions: function(node) {
                    var position = node.position();
                    return position && position.x && position.y ? position : null;
                  }
                },
                minZoom: 0.1,
                maxZoom: 3,
                wheelSensitivity: 0.2,
                selectionType: 'additive'
              });

              cy.on('select unselect', function(evt) {
                var selectedNodes = cy.nodes(':selected').map(function(node) {
                  return {
                    id: node.id(),
                    data: node.data()
                  };
                });
                
                var selectedEdges = cy.edges(':selected').map(function(edge) {
                  return {
                    id: edge.id(),
                    source: edge.source().id(),
                    target: edge.target().id(),
                    data: edge.data()
                  };
                });
                
                Shiny.setInputValue('selected_elements', {
                  nodes: selectedNodes,
                  edges: selectedEdges
                });
              });
              
              // 元のノードスタイルを保存
              originalNodeStyles = {};
              cy.nodes().forEach(function(node) {
                originalNodeStyles[node.id()] = {
                  'background-color': node.style('background-color'),
                  'border-color': node.style('border-color'),
                  'border-width': node.style('border-width')
                };
              });
              
              // 元のエッジスタイルを保存
              originalEdgeStyles = {};
              cy.edges().forEach(function(edge) {
                originalEdgeStyles[edge.id()] = {
                  'line-color': edge.style('line-color'),
                  'width': edge.style('width'),
                  'target-arrow-color': edge.style('target-arrow-color')
                };
              });
              
              setTimeout(function() {
                cy.fit(undefined, 50);
              }, 100);
            }

            // ランダム色を生成する関数
            function getRandomColor() {
              var letters = '0123456789ABCDEF';
              var color = '#';
              for (var i = 0; i < 6; i++) {
                color += letters[Math.floor(Math.random() * 16)];
              }
              return color;
            }
            
            // ネットワークをエクスポートする関数
function exportNetwork(format, filename, scale) {
  if (!cy) return;
  
  // 形式に基づいてMIMEタイプを設定
  let mimeType = 'image/png';
  if (format === 'jpg') mimeType = 'image/jpeg';
  else if (format === 'svg') mimeType = 'image/svg+xml';
  else if (format === 'pdf') mimeType = 'application/pdf';
  
  try {
    if (format === 'pdf') {
      // PDFの場合はsvgを介してPDFを生成
      const svgContent = cy.svg({scale: scale, full: true});
      
      // SVGをBlobに変換
      const svgBlob = new Blob([svgContent], {type: 'image/svg+xml;charset=utf-8'});
      
      // PDFとして保存
      saveAs(svgBlob, filename + '.pdf');
    } else if (format === 'svg') {
      // SVGの場合
      const svgContent = cy.svg({scale: scale, full: true});
      const svgBlob = new Blob([svgContent], {type: mimeType});
      saveAs(svgBlob, filename + '.' + format);
    } else {
      // PNG, JPGの場合
      const blob = cy.png({
        output: 'blob',
        bg: '#ffffff', // 背景色
        scale: scale,
        full: true
      });
      
      // 直接Blobオブジェクトを保存
      saveAs(blob, filename + '.' + format);
    }
  } catch (e) {
    console.error('Export error:', e);
    alert('Error exporting image: ' + e.message);
  }
}
            
            // 複数のノードの色をランダムに変更
Shiny.addCustomMessageHandler('changeRandomNodeColors', function(message) {
  if (cy) {
    const nodeIds = message.nodeIds;
    const color = message.color || getRandomColor(); // メッセージから色を取得、なければランダム色を使用
    
    nodeIds.forEach(function(nodeId) {
      var node = cy.$id(nodeId);
      if (node) {
        node.style({
          'border-color': color,
          'border-width': '6px'
        });
      }
    });
  }
});
            
            // ノードの色を元に戻す
            Shiny.addCustomMessageHandler('resetNodeColors', function(message) {
              if (cy) {
                cy.nodes().forEach(function(node) {
                  const nodeId = node.id();
                  if (originalNodeStyles[nodeId]) {
                    node.style(originalNodeStyles[nodeId]);
                  } else {
                    node.style({
                      'background-color': 'white',
                      'border-color': 'gray',
                      'border-width': '1px'
                    });
                  }
                });
              }
            });
            
            // ネットワークをエクスポート
            Shiny.addCustomMessageHandler('exportNetwork', function(message) {
              exportNetwork(message.format, message.filename, message.scale);
            });
            
            Shiny.addCustomMessageHandler('updateNetwork', function(message) {
              initCytoscape(message.data, message.style);
            });
          "))
          )
        )
      )
    )
  )
)
)



