source("./modules/librarys.R")
source("./modules/data_processing.R")
source("./modules/plot_utils.R")
source("./modules/heatmap_utils.R")
source("./modules/modaldialog_utils.R")
source("./modules/selectcolor_utils.R")
source("./modules/ui_modules.R")
source("./modules/20191008 LIONweb functions.R")

shinyUI(
  fixedPage(
   tags$head(
   tags$link(rel = "shortcut icon", href = "favicon.ico"),
	  tags$style(HTML(" 
		.container, .container-fluid {
          max-width: 5000px !important;
          min-width: 3000px !important;
          font-size: 16px;
        }	  
      body {
        background-color: #f8f5f2;
        color: #333333;
		zoom: 1.1;
      }
      .navbar-default {
        background-color: #2c3e50;
        border-color: #2c3e50;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #ffffff;
      }
      .navbar-default .navbar-nav > li > a:hover {
        background-color: #1a2733;
      }
      .sidebar-panel {
        background-color: #e9ecef;
        padding: 15px;
        border-right: 1px solid #ccc;
        height: 100%;
      }
      .network-panel {
        background-color: #fdfdfd;
        border: 1px solid #ccc;
        border-radius: 4px;
        height: 500px;
        margin-bottom: 20px;
        color: #333333;
      }
      .legend-box {
        background-color: #f5f5f5;
        border-left: 4px solid #2c3e50;
        padding: 10px;
        border-radius: 5px;
        font-size: 12px;
        margin-bottom: 15px;
        color: #333333;
      }
       .btn-primary, .btn-warning {
        background-color: #3c6e71;
        border-color: #3c6e71;
        color: #ffffff;
      }
	  .form-control {
          font-size: 16px;
        }
        .btn {
          font-size: 16px;
          padding: 8px 16px;
        }
		.custom-navbar {
  background-color: #2b3e50;
  border-bottom: 1px solid #2b3e50;
  margin-bottom: 15px;
}

    "))
    ),
  tags$div(
  class = "custom-navbar",
  tags$div(
    style = "padding: 10px 20px; font-size: 20px; color: white;",
	tags$img(
    src = "MSLipidMapper-icon.png",  # www フォルダにある画像ファイル
    height = "30px",       # 高さ指定（必要に応じて調整）
    style = "margin-right: 10px;"  # テキストとの余白
  ),
    "MSLipidMapper"
  )
),
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
  fixedPage(  # ← fluidPage から fixedPage に変更
    tags$div(
      style = "
        display: flex; 
        flex-wrap: nowrap;
        min-width: 1280px;
        height: calc(100vh - 80px); 
        overflow-x: auto;
      ",
      
      # サイドバー領域
      tags$div(
        style = "
          flex: 0 0 350px; 
          background-color: #f8f9fa; 
          border-right: 1px solid #dee2e6; 
          padding: 15px; 
          overflow-y: auto;
        ",
        createCustomSidebar("y", "w", "z", "alpha", "size", "Fontsize", "levels", "pvaluecheck", "q")
      ),
      
      # メイン領域
      tags$div(
        style = "flex: 1; position: relative; padding: 20px; overflow: hidden; min-width: 900px;",
        
        jqui_draggable(
          jqui_resizable(
            div(style = "width: 600px; position: absolute; left: 10px; top: 20px;",
                wellPanel(
                  style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  plotOutput("plottest", width = "100%", height = "600px")
                )
            )
          )
        ),
        
        jqui_draggable(
          jqui_resizable(
            div(style = "width: 600px; position: absolute; left: 10px; top: 700px;",
                wellPanel(
                  style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  verbatimTextOutput("stat_results"),
                  tableOutput("summary_stats")
                )
            )
          )
        ),
        
        jqui_draggable(
          jqui_resizable(
            div(style = "width: 600px; position: absolute; left: 700px; top: 20px;",
                wellPanel(
                  style = "border: 2px solid #ddd; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
                  plotOutput("plottest2", width = "100%", height = "600px")
                )
            )
          )
        )
      )
    )
  )
)
,
    
    # Tab 4 - Network Visualization with added ggplot figures
# Tab 4 - Network Visualization with added ggplot figures
    tabPanel(
      "Network Visualization",
      fixedPage(
	  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/split.js/1.6.0/split.min.js"),
    tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
        background-color: #f8f9fa;
        font-family: 'Segoe UI', sans-serif;
      }
      .toolbar {
        background-color: #e9ecef;
        padding: 10px;
        border-bottom: 1px solid #ccc;
      }
      #mainSplit {
        display: flex;
        height: calc(100vh - 130px);
      }
      #cy {
        width: 100%;
        height: 100%;
        background-color: #ffffff;
        border-right: 1px solid #ccc;
        overflow: auto;
      }
      .gutter {
        background-color: #ccc;
        cursor: col-resize;
        width: 8px;
        height: 100%;
      }
      #rightPanel {
        width: 300px;
        padding: 10px;
        background-color: #ffffff;
        overflow-y: auto;
      }
      .filter-panel {
        background-color: #f0f4f8;
        padding: 15px;
        margin: 0 10px 10px 10px;
        border: 1px solid #ddd;
        border-radius: 5px;
      }
	  .floating-panel {
        position: absolute;
        top: 50px;
        left: 50px;
        width: 800px;
        height: 600px;
        background: #ffffff;
        border: 2px solid #2c3e50;
        padding: 0;
        z-index: 100;
        box-shadow: 0 4px 15px rgba(0,0,0,0.3);
        display: none;
        overflow: auto;
        resize: both;
        min-width: 300px;
        min-height: 200px;
      }

      .floating-panel.fullscreen {
        display: block;
      }

      .floating-header {
        cursor: move;
        background-color: #2c3e50;
        color: white;
        padding: 8px 12px;
        font-weight: bold;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      .floating-body {
        padding: 15px;
        overflow: auto;
        height: calc(100% - 45px);
        min-width: 700px;
      }

      .overlay-toggle {
        margin-top: 10px;
        margin-bottom: 20px;
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        Split(['#cy', '#rightPanel'], {
          direction: 'horizontal',
          sizes: [70, 30],
          minSize: [200, 200],
          gutterSize: 8,
          elementStyle: function (dimension, size, gutterSize) {
            return { 'width': 'calc(' + size + '% - ' + gutterSize + 'px)' };
          },
          gutterStyle: function () {
            return { 'width': '8px' };
          }
        });
      });
    "))
  ),
  	  div(class = "toolbar",
    fluidRow(
      column(2, actionButton("btn_filter_name", "Pathway mapping", class = "btn btn-primary btn-sm btn-block")),
      column(2, actionButton("btn_filter_class", "Export", class = "btn btn-primary btn-sm btn-block")),
      column(2, actionButton("btn_filter_species", "Edit", class = "btn btn-primary btn-sm btn-block"))
    )
  ),
    uiOutput("filter_ui"),

  div(id = "mainSplit",
    div(id = "cy",
			  div(id = "legend-overlay",
      h5("Legend"),
      tags$ul(
		tags$li(HTML("<span style='display:inline-block;width:14px;height:14px;border:2px solid blue;margin-right:6px;'></span>Lipid")),
		tags$li(HTML("<span style='display:inline-block;width:14px;height:14px;border:2px solid black;margin-right:6px;'></span>Gene"))

      )), style = "height:800px;"),
    div(id = "rightPanel",
      verbatimTextOutput("info"),
tabsetPanel(
    id = "mainTabs",
    
    tabPanel("Lipid expression",
      div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px;
                   box-shadow: 0 1px 3px rgba(0,0,0,0.12); margin-bottom: 20px;
                   width:600px; height:600px;",
        h4("Lipid species expression", style = "text-align: center;"),
        plotOutput("corselect2", height = "500px"),
      selectInput(
        inputId = "selectmol",
        label = "Select molecule to display",
        choices = c(" "),
        selected = " "
      ),
	  actionLink("toggle_heatmap", "Heatmap"),
      )
    ),
    
    tabPanel("corrlation plot",
      div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); width:600px; height:600px;",
                  h4("corrlation plot", style = "text-align: center;"),
                  plotlyOutput("corselect", height = "500px"),
                  selectInput(
                    inputId = "new_select1",
                    label = "select variables",
                    choices = c("変数1", "変数2", "変数3"),
                    selected = "変数1"
                  )
                )
    ),
    
    tabPanel("Volcano plot",
      div(style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); width:600px; height:600px;",
                  h4("Volcano plot", style = "text-align: center;"),
                  actionButton("settingsButton", "Settings", class = "settings-button"),
                  actionButton("runAnalysisButton", "Run Analysis", class = "run-button"),
				  plotOutput("volcanoPlot", height = "400px"),
				  actionLink("toggle_other", "Result table"),
				  actionLink("toggle_enrich", "Enrichment analysis"),
				  actionButton("runLIONButton", "Run LION Enrichment",
                             class = "btn-primary", width = "100%"),
							 numericInput("lionPvalThreshold", "p-value threshold", value = 0.05),
							 selectInput("enrichtarget", "Enrichment Direction", choices = c("Up", "Down"))
                )
    )
  ),
  div(id = "enrich_wrapper", class = "floating-panel",
        div(id = "enrich_wrapper_header", class = "floating-header",
            "\ud83e\uddea Enrichment Analysis",
            actionLink("close_enrich", "\u274c Close", style = "color:white;")
        ),
        div(id = "enrich_wrapper_body", class = "floating-body",
            fluidRow(
              column(6,plotOutput("enrichbarplot")),
			  column(6,DT::dataTableOutput("enrichtable"),
			  downloadButton("downloadenrichResults", "Download Results as CSV"))
			  
            ),
            hr(),
            fluidRow(
			
			  #column(4,actionButton("runLIONButton", "Run LION Enrichment",
              #               class = "btn-primary", width = "100%")),
              #column(4, numericInput("p_threshold", "p-value threshold", value = 0.05)),
              #column(4, selectInput("direction", "Enrichment Direction", choices = c("Up", "Down")))
            )
        )
    ),
    
    # 別のパネル（例: Other Analysis）
    div(id = "other_wrapper", class = "floating-panel",
        div(id = "other_wrapper_header", class = "floating-header",
            "\ud83d\udd2c Other Analysis",
            actionLink("close_other", "\u274c Close", style = "color:white;")
        ),
        div(id = "other_wrapper_body", class = "floating-body",
            DT::dataTableOutput("resultsTable"),
            downloadButton("downloadResults", "Download Results as CSV")
        )
    ),
	div(id = "heatmap_wrapper", class = "floating-panel",
  div(id = "heatmap_wrapper_header", class = "floating-header",
    "🔬 Heatmap",
    actionLink("close_heatmap", "❌ Close", style = "color:white;")
  ),
  div(id = "heatmap_wrapper_body", class = "floating-body",
    div(style = "display: flex; flex-direction: row; gap: 20px;",
      # ヒートマップ表示
      div(style = "height: 1000px",
        plotOutput(outputId = "heatmap", height = "100%")
      ),
      # オプションボックス
      div(style = "min-width: 200px; height: 250px; padding: 10px; border: 2px solid #444; border-radius: 6px; background-color: #f9f9f9;",
        tags$h4("Options", style = "margin-top: 0;"),
        
        # acylfilter（ツールチップ付き）
        div(title = "Filtering common acylchains: 16:0, 16:1, 18:0, 18:1, 18:2, 18:3, 20:3, 20:4, 20:5, 22:4, 22:5, 22:6",
          checkboxInput("acylfilter", "Filter common acyl chains", value = TRUE)
        ),
        
        # sn（そのまま）
        checkboxInput("sn", "test", value = FALSE)
      )
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
          ),

            
            tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function () {
      const top = document.getElementById('cy');
      const bottom = document.getElementById('bottomPanel');

      const gutter = document.createElement('div');
      gutter.style.height = '8px';
      gutter.style.cursor = 'row-resize';
      gutter.style.background = '#ccc';

      top.parentNode.insertBefore(gutter, bottom);

      let isDragging = false;

      gutter.addEventListener('mousedown', function(e) {
        isDragging = true;
        document.body.style.cursor = 'row-resize';
      });

      document.addEventListener('mousemove', function(e) {
        if (!isDragging) return;
        const containerTop = top.parentNode.getBoundingClientRect().top;
        const newHeight = e.clientY - containerTop;
        top.style.height = newHeight + 'px';
      });

      document.addEventListener('mouseup', function() {
        isDragging = false;
        document.body.style.cursor = 'default';
      });
    });
  ")),
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function () {
      document.getElementById('toggle_enrich').onclick = function() {
        document.getElementById('enrich_wrapper').classList.add('fullscreen');
        setTimeout(() => {
          Shiny.setInputValue('trigger_redraw_ENRICH', new Date().getTime());
        }, 100);
      };
      document.getElementById('close_enrich').onclick = function() {
        document.getElementById('enrich_wrapper').classList.remove('fullscreen');
      };

      document.getElementById('toggle_other').onclick = function() {
        document.getElementById('other_wrapper').classList.add('fullscreen');
        setTimeout(() => {
          Shiny.setInputValue('trigger_redraw_DT', new Date().getTime());
        }, 100);
      };
      document.getElementById('close_other').onclick = function() {
        document.getElementById('other_wrapper').classList.remove('fullscreen');
      };
	  document.getElementById('toggle_heatmap').onclick = function() {
        document.getElementById('heatmap_wrapper').classList.add('fullscreen');
      };
      document.getElementById('close_heatmap').onclick = function() {
        document.getElementById('heatmap_wrapper').classList.remove('fullscreen');
      };

      function enableDrag(wrapperId, headerId) {
        const wrapper = document.getElementById(wrapperId);
        const header = document.getElementById(headerId);
        let offsetX = 0, offsetY = 0, isDown = false;

        header.addEventListener('mousedown', function(e) {
          isDown = true;
          offsetX = wrapper.offsetLeft - e.clientX;
          offsetY = wrapper.offsetTop - e.clientY;
          document.body.style.userSelect = 'none';
        });

        document.addEventListener('mouseup', function() {
          isDown = false;
          document.body.style.userSelect = '';
        });

        document.addEventListener('mousemove', function(e) {
          if (!isDown) return;
          wrapper.style.left = (e.clientX + offsetX) + 'px';
          wrapper.style.top = (e.clientY + offsetY) + 'px';
        });
      }

      enableDrag('enrich_wrapper', 'enrich_wrapper_header');
      enableDrag('other_wrapper', 'other_wrapper_header');
	  enableDrag('heatmap_wrapper', 'heatmap_wrapper_header');
    });
  ")),
            
            tags$head(
              tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.26.0/cytoscape.min.js"),
              tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
              tags$style(HTML("
              #cy {
                width: 100%;
				overflow: auto;
				padding: 10px;
                position: relative;
				background-color: #f8f9fa;
				border: 2px solid #2c3e50;
				border-radius: 18px;
				box-shadow: 0 8px 20px rgba(44, 62, 80, 0.2);
				margin-top: 10px;
              }
			  #legend-overlay {
				position: absolute;
				top: 10px;
				right: 10px;
				z-index: 10;
				background-color: #ffffffdd;
				padding: 10px;
				border: 1px solid #ccc;
				border-radius: 10px;
				font-size: 13px;
				box-shadow: 0 2px 5px rgba(0,0,0,0.1);
				}

			 #legend-overlay ul {
			list-style: none;
			padding-left: 0;
			margin-bottom: 0;
			}

			#legend-overlay li {
			margin-bottom: 4px;
			}
              
              /* ツールチップのスタイル */
              .cy-tooltip {
  position: absolute;
  display: none;
  z-index: 10000;
  padding: 12px;
  border-radius: 6px;
  font-size: 14px;
  max-width: 350px;
  min-width: 200px;
  pointer-events: auto; /* noneからautoに変更 */
  background-color: rgba(20, 30, 50, 0.95);
  color: #ffffff;
  border: 1px solid rgba(100, 150, 200, 0.3);
  box-shadow: 0 3px 10px rgba(0, 0, 0, 0.3);
  transition: opacity 0.2s;
  line-height: 1.4;
}
                           
              /* ツールチップの見出し */
              .cy-tooltip-header {
                margin-bottom: 8px;
                padding-bottom: 5px;
                font-weight: bold;
                font-size: 16px;
                display: flex;
                justify-content: space-between;
                align-items: center;
              }
              
              /* ツールチップ内の水平線 */
              .cy-tooltip hr {
                border: 0;
                height: 1px;
                margin: 8px 0;
                background-color: rgba(255, 255, 255, 0.2);
              }
              
              /* ツールチップ内の項目ラベル */
              .cy-tooltip-label {
                font-weight: bold;
                margin-right: 5px;
              }
              
              /* ツールチップ内の項目値 */
              .cy-tooltip-value {
                float: right;
              }
              
              /* ツールチップ内のフッター */
              .cy-tooltip-footer {
                margin-top: 10px;
                font-size: 12px;
                opacity: 0.7;
              }
              
              /* ノードとエッジのホバー状態 */
              node:hover, edge:hover {
                cursor: pointer;
              }
            "))
            ),
            
            tags$script(HTML("
            var cy = null;
            var originalNodeStyles = {}; // ノードの元のスタイルを保存
            var originalEdgeStyles = {}; // エッジの元のスタイルを保存
            var tooltip = null; // ツールチップの要素
            var tooltipTimeout = null; // ツールチップの表示/非表示のタイミング調整用
            var tooltipDelay = 200; // ツールチップの表示遅延（ミリ秒）
            var tooltipShowing = false; // ツールチップが表示中かどうか
            var tooltipEnabled = true; // ツールチップが有効かどうか
            
            // ツールチップを初期化する関数
            function initTooltip() {
  if (!tooltip) {
    tooltip = document.createElement('div');
    tooltip.className = 'cy-tooltip dark';
    tooltip.style.opacity = '0.9';
    tooltip.style.display = 'none';
    
    // ここが重要: pointer-eventsをautoに設定
    tooltip.style.pointerEvents = 'auto';
    
    // ツールチップ自身のマウスイベントを追加
    tooltip.addEventListener('mouseenter', function() {
      // ツールチップ上にマウスがある場合は非表示にしない
      tooltipShowing = true;
    });
    
    tooltip.addEventListener('mouseleave', function() {
      // ツールチップからマウスが離れたら非表示
      hideTooltip();
    });
    
    document.body.appendChild(tooltip);
  }
  return tooltip;
}
            
            // ツールチップのスタイルを更新する
            function updateTooltipStyle(theme, opacity) {
              var tooltip = initTooltip();
              
              // 現在のクラスをクリア
              tooltip.classList.remove('dark', 'light', 'blue');
              
              // 新しいテーマを適用
              tooltip.classList.add(theme);
              
              // 透明度を設定
              tooltip.style.opacity = opacity;
            }
            
            // ノードにマウスを乗せた時のツールチップ表示
            function showNodeTooltip(event) {
  if (!tooltipEnabled) return;
  
  var node = event.target;
  var nodeData = node.data();
  
  // 現在のターゲットを保存
  currentTooltipTarget = node;
  
  clearTimeout(tooltipTimeout);
  tooltipTimeout = setTimeout(function() {
    var tooltip = initTooltip();
    
    // HTMLコンテンツを構築
    var content = '';
    
    // ヘッダー部分
    content += '<div class=\"cy-tooltip-header\">';
    if (nodeData.shared_name) {
      content += nodeData.shared_name;
    } else if (nodeData.label) {
      content += nodeData.label;
    } else {
      content += 'Node ' + nodeData.id;
    }
    content += '</div>';
    
    // 区切り線
    content += '<hr>';
    
    // Term_IDsを表示（存在する場合）
    if (nodeData.Term_IDs) {
      content += '<div style=\"margin: 8px 0;\">';
      content += '<span style=\"font-weight: bold; color: #c4e2ff;\">Term IDs:</span><br/>';
      content += '<div style=\"margin-left: 5px; margin-top: 5px;\">' + nodeData.Term_IDs + '</div>';
      content += '</div>';
    }
    
    // Molecule_Namesを表示（存在する場合）
    if (nodeData.Molecule_Names) {
      content += '<hr style=\"border: 0; height: 1px; background-color: rgba(255,255,255,0.2); margin: 8px 0;\">';
      content += '<div style=\"margin: 8px 0;\">';
      content += '<span style=\"font-weight: bold; color: #c4e2ff;\">Molecules:</span><br/>';
      
      // 長いテキストの場合はスクロール可能なボックスにする
      content += '<div style=\"max-height: 90px; overflow-y: auto; margin-left: 5px; margin-top: 5px; padding: 5px; font-size: 13px;\">';
      
      // カンマで分割して箇条書きにする
      var molecules = nodeData.Molecule_Names.split(', ');
      for (var i = 0; i < molecules.length; i++) {
        content += '<div style=\"margin-bottom: 3px;\">• ' + molecules[i] + '</div>';
      }
      content += '</div></div>';
    }
    
    // ツールチップの内容を設定
    tooltip.innerHTML = content;
    
    // ツールチップを表示
    tooltip.style.display = 'block';
    tooltipShowing = true;
    
    // ノードの位置を取得
    var position = node.renderedPosition();
    var nodeHeight = node.renderedHeight();
    
    // ネットワークコンテナの位置を取得
    var container = cy.container();
    var containerRect = container.getBoundingClientRect();
    
    // ツールチップの幅と高さを取得
    var tooltipWidth = tooltip.offsetWidth;
    var tooltipHeight = tooltip.offsetHeight;
    
    // ツールチップの位置を設定（ノードの上に表示）
    tooltip.style.left = (containerRect.left + position.x - tooltipWidth / 2) + 'px';
    tooltip.style.top = (containerRect.top + position.y - nodeHeight/2 - tooltipHeight - 10) + 'px';
  }, tooltipDelay);
}
function showEdgeTooltip(event) {
  if (!tooltipEnabled) return;
  
  var edge = event.target;
  var edgeData = edge.data();
  
  clearTimeout(tooltipTimeout);
  tooltipTimeout = setTimeout(function() {
    var tooltip = initTooltip();
    
    // HTMLコンテンツを構築
    var content = '';
    
    // ヘッダー部分
    content += '<div class=\"cy-tooltip-header\">';
    if (edgeData.type) {
      content += edgeData.type;
    } else {
      content += 'Edge';
    }
    content += '</div>';
    
    // 区切り線
    content += '<hr>';
    
    // 接続情報
    var sourceId = edge.source().id();
    var targetId = edge.target().id();
    
    // ソースとターゲットのノードのラベルを取得（あれば）
    var sourceNode = cy.$id(sourceId);
    var targetNode = cy.$id(targetId);
    var sourceLabel = sourceNode.data('shared_name') || sourceNode.data('label') || sourceId;
    var targetLabel = targetNode.data('shared_name') || targetNode.data('label') || targetId;
    
    content += '<div><span class=\"cy-tooltip-label\">接続:</span> ' + sourceLabel + ' → ' + targetLabel + '</div>';
    
    // ツールチップの内容を設定
    tooltip.innerHTML = content;
    
    // ツールチップを表示
    tooltip.style.display = 'block';
    tooltipShowing = true;
    
    // エッジの中点を計算
    var sourcePos = edge.source().renderedPosition();
    var targetPos = edge.target().renderedPosition();
    var midX = (sourcePos.x + targetPos.x) / 2;
    var midY = (sourcePos.y + targetPos.y) / 2;
    
    // ネットワークコンテナの位置を取得
    var container = cy.container();
    var containerRect = container.getBoundingClientRect();
    
    // ツールチップの幅と高さを取得
    var tooltipWidth = tooltip.offsetWidth;
    var tooltipHeight = tooltip.offsetHeight;
    
    // ツールチップの位置を設定（エッジの中点の上に表示）
    tooltip.style.left = (containerRect.left + midX - tooltipWidth / 2) + 'px';
    tooltip.style.top = (containerRect.top + midY - tooltipHeight - 10) + 'px';
  }, tooltipDelay);
}
            // ツールチップを隠す関数
            function hideTooltip() {
  clearTimeout(tooltipTimeout);
  // すぐに消さずに少し遅延を入れる
  setTimeout(function() {
    // この時点でマウスがツールチップの上にない場合のみ非表示
    if (tooltip && tooltipShowing) {
      tooltip.style.display = 'none';
      tooltipShowing = false;
      currentTooltipTarget = null;
    }
  }, 200); // 短い遅延
}
            
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
                },
                // ホバー状態のスタイルを追加
                {
                  selector: 'node:hover',
                  style: {
                    'border-width': '2px',
                    'border-color': '#33A8FF'
                  }
                },
                {
                  selector: 'edge:hover',
                  style: {
                    'line-color': '#33A8FF',
                    'target-arrow-color': '#33A8FF',
                    'width': function(ele) {
                      return parseFloat(ele.style('width')) + 1;
                    }
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

              // 元のコードに加えて、ツールチップ用のイベントリスナーを追加
              initTooltip();
              Shiny.addCustomMessageHandler('deleteSelectedNodes', function(_) {
        if (cy) {
          const selected = cy.$(':selected');
          selected.remove();
        }
      });
              // ノードとエッジのマウスオーバーイベント
              cy.on('mouseover', 'node', showNodeTooltip);
              cy.on('mouseover', 'edge', showEdgeTooltip);
              
              // マウスアウトイベント
              cy.on('mouseout', 'node, edge', hideTooltip);
              
              // ツールチップをキャンバス（空白部分）のクリックで非表示
              cy.on('tap', function(event) {
                if (event.target === cy) {
                  hideTooltip();
                }
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
			  // 削除ボタンによるノード削除
      
            
            // ネットワークをエクスポートする関数
function exportNetwork(filename) {
  if (!cy) return;

  const format = 'png'; // 固定
  const mimeType = 'image/png';
  const scale = 1; // 固定スケール

  try {
    const blob = cy.png({
      output: 'blob',
      bg: '#ffffff',
      scale: scale,
      full: true
    });

    saveAs(blob, filename + '.png');
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
            
            // ツールチップの表示/非表示切り替え
            Shiny.addCustomMessageHandler('toggleTooltips', function(message) {
              tooltipEnabled = message.show;
              
              if (tooltipEnabled) {
                updateTooltipStyle(message.theme, message.opacity);
              } else {
                hideTooltip();
              }
            });
            
            // ツールチップのスタイル更新
            Shiny.addCustomMessageHandler('updateTooltipStyle', function(message) {
              updateTooltipStyle(message.theme, message.opacity);
            });
            
            // ツールチップの遅延時間更新
            Shiny.addCustomMessageHandler('updateTooltipDelay', function(message) {
              tooltipDelay = message.delay;
            });
          "))
          )
        )
      )
  )
)
)