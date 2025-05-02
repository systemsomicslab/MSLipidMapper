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


         
createCustomSidebar <- function(y_id, w_id, z_id, alpha_id, size_id, fontsize_id, orderInputId, pValueCheckId, qInputId) {
  tagList(
    tags$head(
      tags$style(HTML("
        .plot-type-container {
          display: flex;
          align-items: center;
          justify-content: flex-start;
          width: 100%;
          margin-bottom: 20px;
          padding: 10px 0;
        }
        .sidebar-container {
          max-width: 280px; /* サイドバーの最大幅を制限 */
          width: 100%;
          overflow-x: hidden; /* 横方向のスクロールを防止 */
        }
        .plot-buttons-container {
          display: flex;
          gap: 20px; /* ボタン間の間隔を調整（小さくした） */
          margin-left: 20px; /* dropInputとの間隔（小さくした） */
        }
        .round-button {
          border-radius: 50%;
          width: 45px; /* ボタンサイズを小さくした */
          height: 45px; /* ボタンサイズを小さくした */
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
          width: 60px; /* 幅を固定（小さくした） */
        }
        .button-label {
          text-align: center;
          font-size: 11px; /* フォントサイズを小さく */
          margin-top: 4px; /* ボタンとの間隔（小さくした） */
          line-height: 1.2; /* 行間を調整 */
          color: #666;
        }
        /* 入力コントロールのサイズを調整 */
        .sidebar-container .form-control,
        .sidebar-container .selectize-input,
        .sidebar-container .slider-container {
          font-size: 12px;
        }
        /* ラベルのサイズを調整 */
        .sidebar-container label {
          font-size: 12px;
          margin-bottom: 2px;
        }
      "))
    ),

    # 全体をdivでラップして幅を制限
    div(
      class = "sidebar-container",
      
      # HTML構造の修正
      div(
        class = "plot-type-container",
        # dropInput（幅を小さく）
        esquisse::dropInput(
          inputId = "mydrop",
          choicesNames = tagList(
            list(img(
              src = "./boxplot.png",
              style = "margin-left: 2px; margin-bottom: 2px; margin-top: 0px; width: 40px; height: 40px;"
            ), style = "width: 80px;"),
            list(img(
              src = "./barplot.png",
              style = "margin-left: 2px; margin-bottom: 2px; margin-top: 0px; width: 40px; height: 40px;"
            ), style = "width: 80px;"),
            list(img(
              src = "./violin2.png",
              style = "margin-left: 2px; margin-bottom: 2px; margin-top: 0px; width: 40px; height: 40px;"
            ), style = "width: 80px;"),
            list(img(
              src = "./dotplot.png",
              style = "margin-left: 2px; margin-bottom: 2px; margin-top: 0px; width: 40px; height: 40px;"
            ), style = "width: 80px;")
          ),
          choicesValues = c("box", "bar", "violin", "coding"),
          dropWidth = "180px" # 幅を小さく
        ),
        
        # ボタンコンテナ
        div(
          class = "plot-buttons-container",
          # 保存ボタン
          div(
            class = "button-with-label",
            actionButton("Colorpicker", 
                        label = icon("palette"), # アイコンサイズ調整
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
                        label = icon("diagram-project"), # アイコンサイズ調整
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
      
      # アコーディオンパネル - プロット設定
      bsCollapse(id = "collapse_panels", open = "panel1_content",
        bsCollapsePanel("Plot Settings", value = "panel1_content", status = "primary",
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
          )
        )
      ),
      
      # アコーディオンパネル - 統計テスト設定
      bsCollapse(id = "collapse_panels2", open = "panel2_content",
        bsCollapsePanel("Statistical Test Settings", value = "panel2_content", status = "primary",
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
            class = "d-grid gap-2 d-md-flex justify-content-center", # 中央寄せに変更
            actionButton("select_all", "Select All", 
                        class = "btn-secondary btn-sm"), # ボタンサイズ小さく
            actionButton("clear_all", "Clear All", 
                        class = "btn-secondary btn-sm") # ボタンサイズ小さく
          ),
          uiOutput("comparison_choices")
        )
      ),
      
      h5("Ver 1.20241114", style = "font-size: 11px; text-align: center; margin-top: 10px;") # バージョン表記のスタイルを調整
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


create_volcano_plot <- function(
    data,                    # Data frame
    group_column,            # Column name for grouping
    group1,                  # First group to compare
    group2,                  # Second group to compare
    test_method = "t.test",  # Test method: "t.test" or "wilcox"
    p_adjust_method = "none",# p-value adjustment method
    exclude_columns = NULL,  # Columns to exclude from analysis
    fc_threshold_up = 0.5,   # Fold change threshold for upregulation
    fc_threshold_down = -0.5,# Fold change threshold for downregulation
    p_threshold = 0.05,      # p-value threshold
    point_size = 3,          # Size of plot points
    point_alpha = 0.8,       # Transparency of plot points
    Fontsize = 15,
    colors = c(              # Color settings
      "NS" = "grey50", 
      "Up" = "#FF4B4B",      # Color for upregulation (red)
      "Down" = "#4B4BFF"     # Color for downregulation (blue)
    )
) {

  
  # Validate test method
  if (!test_method %in% c("t.test", "wilcox")) {
    stop("test_method must be either 't.test' or 'wilcox'")
  }
  
  # Validate p-value adjustment method
  valid_p_adjust <- c("none", "holm", "hochberg", "hommel", 
                      "bonferroni", "BH", "BY", "fdr")
  if (!p_adjust_method %in% valid_p_adjust) {
    stop(paste("p_adjust_method must be one of:", 
               paste(valid_p_adjust, collapse = ", ")))
  }
  
  # Data preprocessing
  if (!group_column %in% names(data)) {
    stop("Group column not found in data")
  }
  
  # Extract only the two specified groups
  data_subset <- data %>%
    filter(!!sym(group_column) %in% c(group1, group2))
  
  # Identify numeric columns
  if (is.null(exclude_columns)) {
    exclude_columns <- c(group_column)
  } else {
    exclude_columns <- c(group_column, exclude_columns)
  }
  
  numeric_cols <- names(data_subset)[sapply(data_subset, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, exclude_columns)
  
  if (length(numeric_cols) == 0) {
    stop("No numeric columns found for analysis")
  }
  
  # Function for statistical calculation
  calculate_stats <- function(data, var, group_col, group1, group2, 
                              test_method) {
    # Calculate means between groups
    means <- tapply(data[[var]], data[[group_col]], mean)
    
    # Calculate log2 fold change
    log2FC <- log2(means[group2] / means[group1])
    
    # Perform statistical test
    if (test_method == "t.test") {
      test_result <- t.test(data[[var]] ~ data[[group_col]])
    } else {
      test_result <- wilcox.test(data[[var]] ~ data[[group_col]])
    }
    
    p_value <- test_result$p.value
    
    # Return results as data frame
    data.frame(
      variable = var,
      log2FoldChange = as.numeric(log2FC),
      pvalue = as.numeric(p_value),
      mean_group1 = as.numeric(means[group1]),
      mean_group2 = as.numeric(means[group2])
    )
  }
  
  # Calculate statistics for each numeric variable
  results <- do.call(rbind, lapply(numeric_cols, function(var) {
    calculate_stats(data_subset, var, group_column, group1, group2, 
                    test_method)
  }))
  
  # Adjust p-values
  results <- results %>%
    mutate(
      pvalue_adj = if(p_adjust_method != "none") {
        p.adjust(pvalue, method = p_adjust_method)
      } else {
        pvalue
      },
      significance = case_when(
        log2FoldChange > fc_threshold_up & pvalue_adj < p_threshold ~ "Up",
        log2FoldChange < fc_threshold_down & pvalue_adj < p_threshold ~ "Down",
        TRUE ~ "NS"
      ),
      significance = factor(significance, levels = c("NS", "Down", "Up"))
    )
  
  # Create volcano plot
  volcano_plot <- ggplot(results, 
                         aes(x = log2FoldChange, 
                             y = -log10(pvalue_adj),
                             fill = significance,
                             label = variable)) +
    # Add vertical threshold lines
    geom_vline(xintercept = c(fc_threshold_down, fc_threshold_up), 
               linetype = "dashed", color = "grey80") +
    # Add horizontal p-value threshold line
    geom_hline(yintercept = -log10(p_threshold), 
               linetype = "dashed", color = "grey80") +
    geom_point(size = point_size, alpha = point_alpha, shape = 21) +
    geom_text_repel()+
    scale_fill_manual(values = colors,
                      labels = c("Not Significant", 
                                 paste("Down in", group2),
                                 paste("Up in", group2))) +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      text = element_text(size = Fontsize),
	  aspect.ratio = 1
    ) +
    labs(
      title = paste("Volcano Plot:", group2, "vs", group1,
                    "\nTest:", test_method,
                    "| Adjustment:", p_adjust_method),
      subtitle = paste0("FC threshold: ", fc_threshold_down, 
                        " and ", fc_threshold_up,
                        " | p-value < ", p_threshold),
      x = "log2 Fold Change",
      y = paste("-log10(p-value", 
                if(p_adjust_method != "none") " adjusted" else "",
                ")", sep=""),
      fill = "Significance"
    )
  
  # Return results as a list
  return(list(
    plot = volcano_plot,
    results = results
  ))
}

source("./pathwaymap/20191008 LIONweb functions.R")
runLIONenrichment_targetList <- function(sublist, backgroundlist, 
                                         association_file = "./pathwaymap/20190704 LION_association.txt",
                                         remove_redundant = TRUE,
                                         smart_matching = TRUE,
                                         fa_prediction = TRUE,
                                         LIONterms_rules = NULL,
                                         LIONterms_FAs = NULL,
                                         FA_composition_table = NULL) {
  # LION初期化
  topOnto::initONT('LION')
  
  # アソシエーションの読み込み
  lipidID2TERM <- readMappings(file = association_file)
  
  # リピドIDの処理
  sublist_ids <- strsplit(sublist, "\n")[[1]]
  background_ids <- strsplit(backgroundlist, "\n")[[1]]
  
  # 背景リストにサブリストの要素が含まれていない場合は追加
  background_ids <- unique(c(background_ids, sublist_ids[!sublist_ids %in% background_ids]))
  
  # ID番号付け
  background_df <- data.frame(ID = background_ids)
  background_df$IDnr <- 1:length(background_df$ID)
  background_df$input <- paste(sprintf("[#%04d]", background_df$IDnr), background_df$ID)
  
  # サブリストとバックグラウンドのマッピング
  sublist_input <- background_df$input[background_df$ID %in% sublist_ids]
  background_input <- background_df$input
  
  # 脂質名の変換と標準化
  lipidExistance <- data.frame(input = background_input)
  lipidExistance$'simplified input' <- convertLipidNames(gsub("^\\[#\\d+\\] ","", lipidExistance$input))
  
  # マッチングとスマートマッチング
  matched_lipidID2TERM <- performLipidMatching(
    lipidExistance,
    lipidID2TERM,
    smart_matching,
    fa_prediction,
    LIONterms_rules,
    LIONterms_FAs,
    FA_composition_table
  )
  
  # リピドIDの論理ベクトル作成
  lipidIDlogical <- factor(as.integer(background_input %in% sublist_input), levels = c(0,1))
  names(lipidIDlogical) <- background_input
  
  # Ontologyデータオブジェクトの作成
  ONTdata <- new(
    "topONTdata",
    ontology = "LION",
    allGenes = lipidIDlogical,
    annot = annFUN.gene2GO,
    gene2GO = matched_lipidID2TERM
  ) 
  
  # Fisherの正確確率検定を実行
  resultFis <- runTest(ONTdata, algorithm = "classic", statistic = "fisher")

  # 結果テーブルの生成
  result_table <- GenTable(ONTdata, 'p-value' = resultFis, topNodes = 2000)

  # LIONタームのみをフィルタリング
  result_table <- result_table[grep("LION", result_table$TERM.ID), ]
  
  # ターム名の追加
  LUT <- data.frame(ID = names(ONTdata@termName), Description = ONTdata@termName)
  result_table$Term <- LUT$Description[match(result_table$TERM.ID, LUT$ID)]

  # 少なくとも3つのリピドを持つタームのみを残す
  result_table <- result_table[result_table$Annotated > 2, ]

  # FDRの計算
  result_table$'p-value' <- gsub("< ","", result_table$'p-value')
  result_table$'FDR q-value' <- format(p.adjust(as.numeric(result_table$'p-value'), "fdr"), digits = 3)
  
  # 結果の整形
  colnames(result_table) <- c(
    "Term ID", "Description", "Annotated", "Significant", "Expected", "p-value", "FDR q-value"
  )
  
  # 各タームに関連するリピドの取得
  lipids_in_terms <- genesInTerm(ONTdata)
  term_lipids <- lapply(result_table$'Term ID', function(ID) {
    lipids_in_terms[[ID]]
  })
  # ヒートマップ用データの準備
  plot_data <- result_table
  plot_data$color <- -log(as.numeric(plot_data$`FDR q-value`), base = 10)
  plot_data$color[plot_data$color > 6] <- 6
  
  # エンリッチメントプロットの作成
  enrichment_plot <- ggplot(data = plot_data[1:min(nrow(plot_data), 40), ],
                            aes(y = -log(as.numeric(`FDR q-value`), base = 10),
                                x = reorder(Description, -log(as.numeric(`FDR q-value`), base = 10)))) +
    geom_hline(yintercept = -log(0.05, base = 10), alpha = .3) +
    geom_bar(stat = 'identity', aes(fill = color), width = .70) +
    scale_fill_gradient2(limits = c(0, 6),
                         midpoint = -log(0.05, base = 10),
                         low = "grey", mid = "grey", high = "red") +
    labs(title = "LION enrichment analysis - Target-list mode",
         subtitle = "") +
    xlab("") + ylab("-LOG(FDR q-value)") +
    guides(fill = "none") +
    coord_flip() +
    theme_pander()
  
  # 脂質マッピング統計
  matching_statistics <- calculateMatchingStatistics(lipidExistance)
  
  # 結果を返す
  return(list(
    result_table = result_table,
    term_lipids = term_lipids,
    enrichment_plot = enrichment_plot,
    ONTdata = ONTdata,
    lipidExistance = lipidExistance,
    matching_statistics = matching_statistics
  ))
}

# LIONエンリッチメント解析（ランキングモード）に脂質ID変換ロジックを追加
runLIONenrichment_ranking <- function(lipid_pvalues, 
                                      association_file = "./pathwaymap/20190704 LION_association.txt",
                                      ranking_direction = "ascending", 
                                      ks_test_type = "ks",
                                      remove_redundant = TRUE,
                                      smart_matching = TRUE,
                                      fa_prediction = TRUE,
                                      LIONterms_rules = NULL,
                                      LIONterms_FAs = NULL,
                                      FA_composition_table = NULL) {
  # LION初期化
  topOnto::initONT('LION')
  
  # アソシエーションの読み込み
  lipidID2TERM <- readMappings(file = association_file)
  
  # p値形式の入力処理
  lines <- strsplit(lipid_pvalues, "\n")[[1]]
  entries <- strsplit(lines, "\t")
  
  lipid_ids <- sapply(entries, function(x) x[1])
  pvalues <- as.numeric(sapply(entries, function(x) x[2]))
  
  # ID番号付け
  lipid_ids_with_numbers <- paste(sprintf("[#%04d]", 1:length(lipid_ids)), lipid_ids)
  
  # 脂質名の変換と標準化
  lipidExistance <- data.frame(input = lipid_ids_with_numbers)
  lipidExistance$'simplified input' <- convertLipidNames(gsub("^\\[#\\d+\\] ","", lipidExistance$input))
  
  # マッチングとスマートマッチング
  matched_lipidID2TERM <- performLipidMatching(
    lipidExistance,
    lipidID2TERM,
    smart_matching,
    fa_prediction,
    LIONterms_rules,
    LIONterms_FAs,
    FA_composition_table
  )
  
  # ランキング方向の設定
  direction <- if(ranking_direction == "ascending") 1 else -1
  
  # リピドのランキング
  lipidIDrank <- rank(pvalues * direction)
  names(lipidIDrank) <- lipid_ids_with_numbers
  
  # 選択関数の定義
  mySel <- function(allScore) {
    return(rep(TRUE, length(lipidIDrank)))
  }
  
  # Ontologyデータオブジェクトの作成
  ONTdata <- new(
    "topONTdata",
    ontology = "LION",
    allGenes = lipidIDrank,
    annot = annFUN.gene2GO,
    gene2GO = matched_lipidID2TERM,
    geneSelectionFun = mySel
  )
  
  # KS検定の実行
  resultFis <- runTest(ONTdata, algorithm = "classic", statistic = ks_test_type)
  resultFis@score <- abs(resultFis@score)
  
  # エンリッチメントスコアの計算
  ES <- runTest(ONTdata, algorithm = "classic", statistic = "ks.score")@score
  
  # スコアの符号情報
  sign_i_df <- data.frame(LION = names(ES), sign = sign(ES), ES = ES)
  
  # 結果テーブルの生成
  result_table <- GenTable(ONTdata, 'p-value' = resultFis, topNodes = 2000)
  
  # LIONタームのみをフィルタリング
  result_table <- result_table[grep("LION", result_table$TERM.ID), ]
  
  # ターム名の追加
  LUT <- data.frame(ID = names(ONTdata@termName), Description = ONTdata@termName)
  result_table$Term <- LUT$Description[match(result_table$TERM.ID, LUT$ID)]
  
  # 少なくとも3つのリピドを持つタームのみを残す
  result_table <- result_table[result_table$Annotated > 2, ]
  
  # 冗長なタームの削除（オプション）
  if(remove_redundant) {
    result_table <- removeRedundantTerms(result_table, ONTdata)
  }
  
  # FDRの計算
  result_table$'p-value' <- gsub("< 1e", "< 1.0e", result_table$'p-value')
  result_table$'p-value' <- gsub("< ", "", result_table$'p-value')
  result_table$'FDR q-value' <- format(p.adjust(as.numeric(result_table$'p-value'), "fdr"), digits = 3)
  
  # 結果の整形
  colnames(result_table) <- c(
    "Term ID", "Description", "Annotated", "Significant", "Expected", "p-value", "FDR q-value"
  )
  result_table <- result_table[, c(1, 2, 3, 6, 7)]
  
  # 2側KS検定の場合、エンリッチメントスコアを追加
  if(ks_test_type == "ks2") {
    result_table$ES <- sign_i_df$ES[match(result_table$`Term ID`, sign_i_df$LION)]
    result_table$Regulated <- as.character(factor(sign_i_df$sign[match(result_table$`Term ID`, sign_i_df$LION)], 
                                                  levels = c(1,-1), labels = c("UP","DOWN")))
  }
  
  # 可視化用データの準備
  plot_data <- result_table
  plot_data$color <- -log(as.numeric(plot_data$`FDR q-value`), base = 10)
  plot_data$color[plot_data$color > 6] <- 6
  
  # 可視化タイプの選択
  if(ks_test_type == "ks2") {
    # ボルケーノプロット
    enrichment_plot <- ggplot(data = plot_data,
                              aes(x = ES, 
                                  y = -log(as.numeric(`FDR q-value`), base = 10),
                                  size = Annotated)) +
      geom_hline(yintercept = -log(0.05, base = 10), alpha = .8, linetype = 2) +
      geom_vline(xintercept = 0, alpha = .8, linetype = 2) +
      geom_point(shape = 21, aes(fill = color)) +
      geom_text_repel(data = plot_data[as.numeric(plot_data$`FDR q-value`) < 0.1, ],
                      aes(label = Description), size = 4) +
      scale_fill_gradient2(limits = c(0, 6),
                           midpoint = -log(0.05, base = 10),
                           low = "grey", mid = "grey", high = "red") +
      labs(x = "LION-enrichment score (ES)", y = "-LOG(FDR q-value)", 
           size = "# of lipids", title = "LION enrichment analysis - Ranking mode") +
      guides(fill = "none") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 1))
  } else {
    # 通常のバーチャート
    enrichment_plot <- ggplot(data = plot_data[1:min(nrow(plot_data), 40), ],
                              aes(y = -log(as.numeric(`FDR q-value`), base = 10),
                                  x = reorder(Description, -log(as.numeric(`FDR q-value`), base = 10)))) +
      geom_hline(yintercept = -log(0.05, base = 10), alpha = .3) +
      geom_bar(stat = 'identity', aes(fill = color), width = .70) +
      scale_fill_gradient2(limits = c(0, 6),
                           midpoint = -log(0.05, base = 10),
                           low = "grey", mid = "grey", high = "red") +
      labs(title = "LION enrichment analysis - Ranking mode") +
      xlab("") + ylab("-LOG(FDR q-value)") +
      guides(fill = "none") +
      coord_flip() +
      theme_pander()
  }
  
  # 脂質マッピング統計
  matching_statistics <- calculateMatchingStatistics(lipidExistance)
  
  # 結果を返す
  return(list(
    result_table = result_table,
    enrichment_plot = enrichment_plot,
    ONTdata = ONTdata,
    ES = ES,
    lipidExistance = lipidExistance,
    matching_statistics = matching_statistics
  ))
}

# メイン関数の更新：LIONエンリッチメント解析
runLIONenrichment <- function(input_data, method = "target_list", 
                              association_file = "./pathwaymap/20190704 LION_association.txt",
                              ranking_direction = "ascending", 
                              ks_test_type = "ks",
                              remove_redundant = TRUE,
                              smart_matching = TRUE,
                              fa_prediction = TRUE,
                              LIONterms_rules_file = NULL,
                              LIONterms_FAs_file = NULL,
                              FA_composition_table_file = NULL) {
  
  # LIONterms_rulesの読み込み
  if(is.null(LIONterms_rules_file)) {
    LIONterms_rules <- NULL
  } else {
    LIONterms_rules <- read.csv(file = LIONterms_rules_file, header = TRUE)
    LIONterms_rules$RULE1[LIONterms_rules$RULE1 == ""] <- "-"
  }
  
  # LIONterms_FAsの読み込み
  if(is.null(LIONterms_FAs_file)) {
    LIONterms_FAs <- NULL
  } else {
    LIONterms_FAs <- read.csv(file = LIONterms_FAs_file, header = TRUE)
  }
  
  # FA_composition_tableの読み込み
  if(is.null(FA_composition_table_file)) {
    FA_composition_table <- NULL
  } else {
    FA_composition_table <- read.csv(file = FA_composition_table_file)
  }
  
  # 解析方法に応じた処理
  if (method == "target_list") {
    # ターゲットリストとバックグラウンドリストの解析
    if (!all(c("sublist", "background") %in% names(input_data))) {
      stop("For target_list mode, input_data must contain 'sublist' and 'background'")
    }
    
    result <- runLIONenrichment_targetList(
      input_data$sublist,
      input_data$background,
      association_file,
      remove_redundant,
      smart_matching,
      fa_prediction,
      LIONterms_rules,
      LIONterms_FAs,
      FA_composition_table
    )
  } else if (method == "ranking") {
    # ランキングモードの解析
    if (!("lipid_pvalues" %in% names(input_data))) {
      stop("For ranking mode, input_data must contain 'lipid_pvalues'")
    }
    
    result <- runLIONenrichment_ranking(
      input_data$lipid_pvalues,
      association_file,
      ranking_direction,
      ks_test_type,
      remove_redundant,
      smart_matching,
      fa_prediction,
      LIONterms_rules,
      LIONterms_FAs,
      FA_composition_table
    )
  } else {
    stop("Invalid method. Choose 'target_list' or 'ranking'.")
  }
  
  return(result)
}

# 脂質マッピング処理のためのヘルパー関数
performLipidMatching <- function(lipidExistance, lipidID2TERM, 
                                 smart_matching = TRUE, fa_prediction = TRUE,
                                 LIONterms_rules = NULL, LIONterms_FAs = NULL,
                                 FA_composition_table = NULL) {
  
  # 各脂質のマッチング処理
  lipidExistance_list <- lapply(1:nrow(lipidExistance), function(row_i) {
    lipid_i <- lipidExistance[row_i, 2]  # simplified input
    
    # 直接マッチングを試みる
    LION_ID <- unlist(lipidID2TERM[lipid_i == names(lipidID2TERM)])
    
    if (is.null(LION_ID)) {
      # LION:xxxxフォーマットの場合
      LION_ID <- unlist(lipidID2TERM[lipid_i == unlist(lipidID2TERM)])
      LION_ID <- LION_ID[!grepl("^SLM:|^LM..\\d+", names(LION_ID))]  # SwissLipids/LIPIDMAPS IDを除外
    }
    
    if (!is.null(LION_ID)) {
      # 直接マッチングが見つかった場合
      output_df <- data.frame(
        input = lipidExistance[row_i, 1],
        'simplified input' = lipid_i,
        name = names(LION_ID),
        LION = LION_ID,
        match = "direct"
      )
    } else {
      # 直接マッチングが見つからなかった場合
      if (smart_matching && !is.null(LIONterms_rules) && !is.null(LIONterms_FAs)) {
        # スマートマッチングの実行
        lipid_index <- list(
          generalized = simplifyLipidnames(lipid_i),
          headgroup = getLipidHeadgroup(lipid_i),
          linkage = getLipidLinkage(lipid_i),
          FAs = getFAs(lipid_i)
        )
        
        # 一般化された脂質名でのマッチングを試みる
        generalized_LION_ID <- unlist(lipidID2TERM[lipid_index$generalized == names(lipidID2TERM)])
        
        if (!is.null(generalized_LION_ID)) {
          # 一般化された脂質名でマッチした場合
          terms <- data.frame(
            name = names(lipidID2TERM)[lipid_index$generalized == names(lipidID2TERM)],
            LION = generalized_LION_ID
          )
        } else {
          # ルールベースのマッチングを試みる
          LIONterms_rules_i <- LIONterms_rules[lipid_index$headgroup == LIONterms_rules$RULE1, ]
          
          if (all(LIONterms_rules_i$RULE2 == "")) {
            terms <- LIONterms_rules_i[, 1:2]
          } else {
            terms <- LIONterms_rules_i[lipid_index$linkage == LIONterms_rules_i$RULE2, ][, 1:2]
          }
        }
        
        # 脂肪酸に基づくマッチングを追加
        terms <- rbind(terms, LIONterms_FAs[LIONterms_FAs$name %in% lipid_index$FAs, ])
        
        if (nrow(terms) > 0) {
          # スマートマッチングが成功した場合
          output_df <- data.frame(
            input = lipidExistance[row_i, 1],
            'simplified input' = lipid_i,
            terms,
            match = "smart matching"
          )
        } else {
          # スマートマッチングが失敗した場合
          output_df <- data.frame(
            input = lipidExistance[row_i, 1],
            'simplified input' = lipid_i,
            name = "not found",
            LION = "not found",
            match = ""
          )
        }
      } else {
        # スマートマッチングがオフの場合
        output_df <- data.frame(
          input = lipidExistance[row_i, 1],
          'simplified input' = lipid_i,
          name = "not found",
          LION = "not found",
          match = ""
        )
      }
    }
    
    # FA予測の実行（オプション）
    if (fa_prediction && !is.null(FA_composition_table)) {
      lipid_index <- list(
        generalized = simplifyLipidnames(lipid_i),
        headgroup = getLipidHeadgroup(lipid_i),
        FAs = getFAs(lipid_i)
      )
      
      # FA予測の条件を確認
      if (lipid_index$generalized == lipid_i && length(lipid_index$FAs) == 1) {
        # 予測FAの取得
        predicted_FAs <- predict_FAs(
          headgroup = lipid_index$headgroup,
          summedFA = gsub("C", "", lipid_index$FAs),
          composition_table = FA_composition_table
        )
        
        # 予測FAに基づくマッチング
        if (!is.null(LIONterms_FAs) && nrow(LIONterms_FAs[LIONterms_FAs$name %in% predicted_FAs, ]) > 0) {
          output_df <- rbind(
            output_df,
            data.frame(
              input = lipidExistance[row_i, 1],
              'simplified input' = lipid_i,
              LIONterms_FAs[LIONterms_FAs$name %in% predicted_FAs, ],
              match = "FA-prediction"
            )
          )
        }
      }
    }
    
    return(output_df)
  })
  
  # リスト内のデータフレームを結合
  lipidExistance_combined <- do.call("rbind", lipidExistance_list)
  
  # マッチングされた脂質IDのみ抽出
  lipidExistance_feasable <- lipidExistance_combined[lipidExistance_combined$LION != "not found", ]
  
  # 脂質IDとLIONタームのマッピング
  matched_lipidID2TERM <- sapply(unique(lipidExistance_feasable$input), function(input) {
    lipidExistance_feasable$LION[lipidExistance_feasable$input == input]
  }, simplify = FALSE)
  
  return(matched_lipidID2TERM)
}

# マッチング統計の計算
calculateMatchingStatistics <- function(lipidExistance) {
  # ユニークな入力脂質の取得
  unique_inputs <- unique(lipidExistance$input)
  
  # 各脂質がマッチングされたかの確認
  matched_status <- sapply(unique_inputs, function(input) {
    lipid_rows <- lipidExistance[lipidExistance$input == input, ]
    !all(lipid_rows$'LION ID' == "not found" | is.na(lipid_rows$'LION ID'))
  })
  
  # 統計の計算
  total_lipids <- length(unique_inputs)
  matched_lipids <- sum(matched_status)
  match_percentage <- (matched_lipids / total_lipids) * 100
  
  return(data.frame(
    total = total_lipids,
    matched = matched_lipids,
    percent = match_percentage
  ))
}

# 冗長なタームの削除
removeRedundantTerms <- function(result_table, ONTdata) {
  # グラフ構造の取得
  ONT_DAG <- ONTdata@graph
  ONT_DAG_lev <- buildLevels(ONT_DAG)
  DAG.env <- ONT_DAG_lev$nodes2level
  
  # 各タームのレベル取得
  DAGlevel <- sapply(result_table$`Term ID`, function(id) {
    DAG.env[[id]]
  })
  
  # 各タームに関連する脂質の取得
  lipidsInTerms <- genesInTerm(ONTdata)
  TermContent <- sapply(result_table$`Term ID`, function(id) {
    lipidsInTerms[[id]]
  })
  
  # 冗長性チェック
  test_similarity <- sapply(names(TermContent), function(term_i) {
    sapply(names(TermContent), function(term_j) {
      if (term_i != term_j) {
        # 内容が同じかチェック
        if (length(TermContent[[term_i]]) == length(TermContent[[term_j]])) {
          same <- all(TermContent[[term_i]] %in% TermContent[[term_j]])
        } else {
          same <- FALSE
        }
        
        # 結果リストの作成
        outputList <- list(
          term_a = term_i,
          term_b = term_j,
          isSame = same,
          term_a_level = DAGlevel[names(DAGlevel) == term_i],
          term_b_level = DAGlevel[names(DAGlevel) == term_j]
        )
        
        # 削除すべきタームの決定
        outputList$remove <- ""
        if ((outputList$term_a_level - outputList$term_b_level) == 1) {
          outputList$remove <- outputList$term_b
        }
        if ((outputList$term_a_level - outputList$term_b_level) == -1) {
          outputList$remove <- outputList$term_a
        }
        
        return(outputList)
      } else {
        # 同じタームの場合
        return(list(
          term_a = term_i,
          term_b = term_j,
          remove = "",
          isSame = FALSE
        ))
      }
    }, simplify = FALSE)
  })
  
  # 削除すべきタームの抽出
  test_similarity <- test_similarity[unlist(lapply(test_similarity, function(n) {
    n[['isSame']]
  }))]
  
  terms_to_remove <- unique(unlist(lapply(test_similarity, function(n) {
    n[['remove']]
  })))
  
  # 冗長なタームを除外
  filtered_table <- result_table[!(result_table$`Term ID` %in% terms_to_remove), ]
  
  return(filtered_table)
}



generate_ontology_result <- function(vo_df, term_lipids, ontology_mapping, sublist_ids, p_value_threshold = 0.05) {
  # 必要なライブラリをロード
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr package is required")
  }
  
  # データフレームに分子リストを追加
  vo_df$molecules <- term_lipids
  
  # 分子名と対応するOntologyのマッピング作成
  ontology_dict <- setNames(ontology_mapping$Ontology, ontology_mapping$variable)
  print(sublist_ids)
  mapping_dict <- setNames(names(sublist_ids),unname(sublist_ids))
  
  vo_df$`FDR q-value` <- as.numeric(vo_df$`FDR q-value`)
  # P<0.05の行をフィルタリング
  significant_terms <- vo_df %>%
    dplyr::filter(`FDR q-value` < p_value_threshold)
  
  # 結果を保存するためのベクトルを用意
  term_ids <- c()
  molecule_names <- c()
  clean_molecule_names <- c()
  matched_variables <- c()
  ontologies <- c()
  print("ont")
  print(mapping_dict)
  print("sig")
  print(head(significant_terms))
  # 有意なtermごとに処理
  for (i in 1:nrow(significant_terms)) {
    term_id <- significant_terms$Description[i]
    molecules <- significant_terms$molecules[[i]]
    
    # 空のリストをチェック
    if (length(molecules) == 0 || is.null(molecules)) {
      next  # 空のリストの場合はスキップ
    }
    
    # 各分子ごとに処理
    for (m in molecules) {
      # "[#0156] " のような部分を削除して正規化
      clean_m <- gsub("^\\[#\\d+\\]\\s+", "", m)
      norm_key <- gsub("\\s+", " ", trimws(clean_m))
      
# 仮に対応辞書が mapping_dict という名前で、
# norm_key -> ontology_mapping_key の対応を持っているとします
if (norm_key %in% names(mapping_dict)) {
  ontology_mapping_key <- mapping_dict[norm_key]
  
  # 対応するキーを使ってオントロジー辞書を検索
  if (ontology_mapping_key %in% names(ontology_dict)) {
    ontology <- ontology_dict[ontology_mapping_key]
    if (!is.na(ontology)) {
      # 一致があった場合の処理
      term_ids <- c(term_ids, term_id)
      molecule_names <- c(molecule_names, m)
      clean_molecule_names <- c(clean_molecule_names, norm_key)
      matched_variables <- c(matched_variables, ontology_mapping_key)  # 実際にマッチしたキーを記録
      ontologies <- c(ontologies, ontology)
      found_ontology <- TRUE
    }
  }
}
    }
  }
  
  # ベクトルから最終的なデータフレームを作成
  if (length(term_ids) > 0) {
    final_result <- data.frame(
      Term_ID = term_ids,
      Molecule = molecule_names,
      Clean_Molecule = clean_molecule_names,
      Matched_Variable = matched_variables,
      Ontology = ontologies,
      stringsAsFactors = FALSE
    )
    
    # Ontologyごとの分子数をカウント
    ontology_counts <- final_result %>%
      dplyr::group_by(Ontology) %>%
      dplyr::summarise(Molecule_Count = dplyr::n(), .groups = 'drop')
    
    # Term_IDごとにOntologyの分布を確認
    term_ontology_distribution <- final_result %>%
      dplyr::group_by(Term_ID, Ontology) %>%
      dplyr::summarise(Count = dplyr::n(), .groups = 'drop') %>%
      dplyr::group_by(Term_ID) %>%
      dplyr::mutate(Percentage = Count / sum(Count) * 100)
    
    # 結果をリストとして返す
    return(list(
      result = final_result,
      ontology_counts = ontology_counts,
      term_distribution = term_ontology_distribution
    ))
  } else {
    # 結果が空の場合
    return(NULL)
  }
}



validate_dataframe <- function(df, validation_rules = c("structure", "peak_headers", "metadata")
) {
  expected_headers <- c(
    "Alignment ID", "Average Rt(min)", "Average Mz", "Metabolite name",
    "Adduct type", "Post curation result", "Fill %", "MS/MS assigned",
    "Reference RT", "Reference m/z", "Formula", "Ontology", "INCHIKEY",
    "SMILES", "Annotation tag (VS1.0)", "RT matched", "m/z matched",
    "MS/MS matched", "Comment", "Manually modified for quantification",
    "Manually modified for annotation", "Isotope tracking parent ID", 
    "Isotope tracking weight number", "RT similarity", "m/z similarity",
    "Simple dot product", "Weighted dot product", "Reverse dot product",
    "Matched peaks count", "Matched peaks percentage", "Total score",
    "S/N average", "Spectrum reference file name", "MS1 isotopic spectrum",
    "MS/MS spectrum"
  )
  # Input check
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (!is.character(validation_rules)) {
    stop("validation_rules must be a character vector")
  }
  
  if (!all(validation_rules %in% c("structure", "peak_headers", "metadata"))) {
    stop("Invalid validation_rules specified")
  }
  
  validation_results <- list()
  
  # Check data structure  
  if ("structure" %in% validation_rules) {
    if (nrow(df) < 5) {
      validation_results$insufficient_rows <- 
        "Data has less than 5 rows. At least 5 rows are required."
    }
    if (ncol(df) < 34) {
      validation_results$insufficient_cols <- 
        "Missing 34 columns required for peak information."
    }
  }
  
  # Check peak information headers (row 5)
  if ("peak_headers" %in% validation_rules) {
    if (is.null(expected_headers)) {
      stop("expected_headers required for peak_headers validation")
    }
    
    if (nrow(df) >= 5) {
      headers <- df[5, 1:min(34, ncol(df))]
      missing_headers <- expected_headers[1:length(headers)][headers != expected_headers[1:length(headers)]]
      
      if (length(missing_headers) > 0) {
        validation_results$incorrect_headers <- 
          paste("Headers in row 5 differ from expected headers:",
                paste(missing_headers, collapse = ", "))
      }
    }
  }
  
  # Check metadata (rows 1-2, columns 35+)
  if ("metadata" %in% validation_rules && ncol(df) > 34) {
    # Check for Sample in columns 36+
    if (ncol(df) > 35) {
      file_types <- df[2, 36:ncol(df)]
      if (!"Sample" %in% file_types) {
        validation_results$no_sample <- 
          "'Sample' not found in columns 36 onward"
      }
    }
  }
  
  return(validation_results)
}


# Ontology情報をネットワークデータに統合し、全てのTerm_IDとmolecules情報も追加する関数
integrate_ontology_preserving_format <- function(network_content, ontology_result) {
  # JSONデータを解析 - 文字列として正しく結合
  if (is.character(network_content) && length(network_content) > 1) {
    network_json <- paste(network_content, collapse = "\n")
  } else {
    network_json <- network_content
  }
  
  # JSONをデータ構造に変換
  network_data <- jsonlite::fromJSON(network_json, simplifyVector = FALSE)
  
  # ノード情報を取得
  nodes <- network_data$elements$nodes
  
  # ノード情報のデータフレームを作成
  nodes_df <- data.frame(
    id = sapply(nodes, function(x) if (!is.null(x$data$id)) x$data$id else NA),
    label = sapply(nodes, function(x) if (!is.null(x$data$label)) x$data$label else ""),
    shared_name = sapply(nodes, function(x) if (!is.null(x$data$shared_name)) x$data$shared_name else ""),
    stringsAsFactors = FALSE
  )
  
  # shared_nameが空の場合は代わりにlabelを使用
  nodes_df$shared_name <- ifelse(nodes_df$shared_name == "", nodes_df$label, nodes_df$shared_name)
  
  # 簡易マッピング: shared_nameをシンプルな形式に変換してマッチング
  simplified_nodes <- nodes_df %>%
    mutate(simple_name = stringr::str_extract(shared_name, "^[A-Za-z0-9]+"))
  
  # Ontologyマッピングテーブルを作成
  ontology_mapping <- ontology_result %>%
    mutate(simple_matched = stringr::str_extract(Clean_Molecule, "^[A-Za-z0-9]+")) %>%
    dplyr::select(Term_ID, Molecule, Clean_Molecule, simple_matched, Matched_Variable, Ontology) %>%
    filter(!is.na(simple_matched) & simple_matched != "")
  
  # ノードとマッピングの結合 - 複数のマッチングを保持
  nodes_with_ontology <- inner_join(simplified_nodes,
                                   ontology_mapping,
                                   by = c("shared_name" = "Ontology"),
                                   relationship = "many-to-many")
  
  # 各ノードに対して、関連するすべてのTerm_IDとMoleculeを収集
  node_term_mapping <- list()
  
  for (node_id in unique(nodes_with_ontology$id)) {
    node_data <- nodes_with_ontology[nodes_with_ontology$id == node_id, ]
    
    # このノードに関連するすべてのTerm_IDとMoleculeを収集
    terms <- unique(node_data$Term_ID)
    molecules <- unique(node_data$Molecule)
    ontologies <- unique(node_data$Ontology)
    
    # 結果を保存
    node_term_mapping[[node_id]] <- list(
      terms = terms,
      molecules = molecules,
      ontologies = ontologies
    )
  }
  
  # Ontologyが付与されたノードに適用する統一色とボーダー幅
  highlight_color <- "#008080"  # 青色系の強調色
  highlight_border_width <- 5   # 太いボーダー
  
  # ネットワークデータに情報を統合
  for (i in seq_along(nodes)) {
    # 安全に値を取得
    node_id <- if (!is.null(nodes[[i]]$data$id)) nodes[[i]]$data$id else NA
    
    # ノードIDが有効で、マッピングに存在する場合
    if (!is.na(node_id) && node_id %in% names(node_term_mapping)) {
      mapping_data <- node_term_mapping[[node_id]]
      
      # Terms情報を追加 - JSONとして保存
      if (length(mapping_data$terms) > 0) {
        network_data$elements$nodes[[i]]$data$Terms <- jsonlite::toJSON(mapping_data$terms)
        network_data$elements$nodes[[i]]$data$Term_IDs <- paste(mapping_data$terms, collapse = ", ")
      }
      
      # Molecules情報を追加
      if (length(mapping_data$molecules) > 0) {
        network_data$elements$nodes[[i]]$data$Molecules <- jsonlite::toJSON(mapping_data$molecules)
        network_data$elements$nodes[[i]]$data$Molecule_Names <- paste(mapping_data$molecules, collapse = ", ")
		        # 元のスタイル情報を保存
        original_color <- if (!is.null(network_data$elements$nodes[[i]]$data$Color)) 
          network_data$elements$nodes[[i]]$data$Color else "gray"
        original_fillcolor <- if (!is.null(network_data$elements$nodes[[i]]$data$Fillcolor)) 
          network_data$elements$nodes[[i]]$data$Fillcolor else "#ffffff"
        original_width <- if (!is.null(network_data$elements$nodes[[i]]$data$BorderWidth)) 
          network_data$elements$nodes[[i]]$data$BorderWidth else 1
        
        network_data$elements$nodes[[i]]$data$OriginalColor <- original_color
        network_data$elements$nodes[[i]]$data$OriginalFillcolor <- original_fillcolor
        network_data$elements$nodes[[i]]$data$OriginalBorderWidth <- original_width
        
        # 統一された強調色とボーダー幅を適用
        network_data$elements$nodes[[i]]$data$borderstyle <- "double"
        network_data$elements$nodes[[i]]$data$Fillcolor <- highlight_color
        network_data$elements$nodes[[i]]$data$BorderWidth <- highlight_border_width
        
        # 有意なノードであることをフラグで示す
        network_data$elements$nodes[[i]]$data$IsSignificant <- TRUE
      }
    }
  }
  
  # 同じフォーマットで出力するため、JSONを文字列に変換して返す
  network_json <- jsonlite::toJSON(network_data, auto_unbox = TRUE, pretty = TRUE)
  return(strsplit(as.character(network_json), "\n")[[1]])
}