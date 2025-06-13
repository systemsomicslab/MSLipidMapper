server <- function(input, output,session) {
  options(shiny.maxRequestSize=150*1024^100)
   session$onSessionEnded(function() {
    cat("セッション終了：アプリを停止します\n")
    q("no")  # Rごと終了 → コンテナも終了（--rmなら自動削除）
  })
  filter_name_toggle <- reactiveVal(FALSE)
  filter_class_toggle <- reactiveVal(FALSE)
  filter_species_toggle <- reactiveVal(FALSE)

  observeEvent(input$btn_filter_name, {
    filter_name_toggle(!filter_name_toggle())
    filter_class_toggle(FALSE)
    filter_species_toggle(FALSE)
  })

  observeEvent(input$btn_filter_class, {
    filter_class_toggle(!filter_class_toggle())
    filter_name_toggle(FALSE)
    filter_species_toggle(FALSE)
  })

  observeEvent(input$btn_filter_species, {
    filter_species_toggle(!filter_species_toggle())
    filter_name_toggle(FALSE)
    filter_class_toggle(FALSE)
  })
 
  output$filter_ui <- renderUI({
    if (filter_name_toggle()) {
      return(div(class = "filter-panel",
        h5("Filter the reactions on lipid name"),
        fluidRow(
          column(2, checkboxInput("showTooltips", "Show Tooltips", value = TRUE)),
          column(10, selectInput("pathwaytype", "Select Pathway Type:",
                        choices = c("Global pathway", 
                                    "Ceramide pathway",
                                    "Remodeling pathway"),
                        selected = "Global pathway"))
        )
      ))
    }

    if (filter_class_toggle()) {
      return(div(class = "filter-panel",
        h5("Filter by lipid classification"),
		fluidRow(
              column(2, textInput("export_filename", "Filename:", "network_export")),
			  column(2, actionButton("export_btn", "Export Network", class = "btn-success btn-block")),
			  column(2, downloadButton("exportCYJS", "Export CYJS", class = "btn-block")),
			  column(2, downloadButton("exportStyles", "Export styles.xml", class = "btn-block"))
            )
      ))
    }

    if (filter_species_toggle()) {
      return(div(class = "filter-panel",
        h5("Filter by species"),
        fluidRow(
          column(6, actionButton("delete_node", "Remove selected nodes", icon = icon("trash"))),
          column(6, numericInput("num_nodes", "Number of nodes to change:", 5, min = 1, max = 50)),
		  column(6, actionButton("reset_colors", "Reset Colors", class = "btn-warning")),
		  column(6, actionButton("highlight_significant", "Apply Random Colors", class = "btn-primary"))
        )
      ))
    }

    return(NULL)
  })
  
  originaldir <- reactiveValues(datapath = getwd()) # directry of shiny R script
  global <- reactiveValues(datapath = getwd()) # directory of file path in lipidomics tab
  col = reactiveValues(col = col)
  plotTrigger <- reactiveVal(0)
  validation_status <- reactiveVal(FALSE)
  svg_path <- paste(getwd(),"/svg",sep = "")
  output$downloadData <- downloadHandler(
    filename = function() {
      "Demo_data.zip"  # ダウンロード時のファイル名
    },
    content = function(file) {
      file.copy("./pathwaymap/demodata.zip", file)
    },
    contentType = "application/zip"
  )

volcanoResults <- reactiveVal(
  list(results = data.frame(
    variable = character(), log2FoldChange = numeric(), pvalue = numeric(), pvalue_adj = numeric(), significance = character(),
    mean_group1 = numeric(), mean_group2 = numeric()
  ))
)

  observeEvent(input$filetype,{
    if(input$filetype =="Sample in rows"){
      shinyjs::show("ontfile")
    }else{
      shinyjs::hide("ontfile")
    }
  })
  observeEvent(input$file1, {	
    tryCatch({
      originaldata <- read.csv(input$file1$datapath, header = F, check.names = F, fileEncoding = "UTF-8-BOM")
    }, error = function(e) {
      showNotification(paste("Failed to load the file", e$message), type = "error")
    })
    if(input$filetype == "Sample in rows"){
      
    }else if(input$filetype == "MS-DIAL export"){
      validateresults <- validate_dataframe(originaldata, validation_rules = c("structure", "peak_headers", "metadata"))
      if (length(validateresults) == 0) {
        validation_status(TRUE)
        shinyalert(
          "Success",
          "All validation checks passed successfully!",
          type = "success"
        )
      } else {
        validation_status(FALSE)
        error_message <- "<div style='text-align: left;'><strong>Validation Errors:</strong><br><br>"
        
        # 各エラーメッセージの処理
        for (error_name in names(validateresults)) {
          error_message <- paste0(
            error_message,
            "<span style='color: #dc3545;'>• ",  # 赤色のブレット
            switch(error_name,
                   "insufficient_cols" = "<strong>Column Error:</strong> ",
                   "incorrect_headers" = "<strong>Header Error:</strong> ",
                   "no_sample" = "<strong>Sample Error:</strong> "),
            validateresults[[error_name]],
            "</span><br><br>"
          )
        }
        error_message <- paste0(error_message, "</div>")
        
        # エラーメッセージの表示
        shinyalert(
          "Validation Failed",
          html = TRUE,
          text = error_message,
          type = "error",
          size = "l",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          showConfirmButton = TRUE,
          confirmButtonText = "OK",
          timer = FALSE
        )
        
      }
    }
    observeEvent(input$submit,{
     # tryCatch({
        if(input$filetype == "Sample in rows"){
          
          Lipidomedata <- processSampleInRows(originaldata, session, input)[[1]]
          lipidont <- read.csv(input$ontfile$datapath, sep = ",", check.names = FALSE) 
		  colnames(lipidont) <- c("variable","Ontology")
          metadata <- processSampleInRows(originaldata, session, input)[[2]]
          alitable <- read.csv(input$ontfile$datapath, check.names = FALSE, fileEncoding = "UTF-8-BOM")
          colnames(alitable) <- c("Metabolite name","Ontology")
          letchoice <- c(colnames(Lipidomedata)[!colnames(Lipidomedata) %in% colnames(metadata)],"Others")
          moldata <- processSampleInRowstomoldata(originaldata, session, input)[[1]]
        }else if(input$filetype == "MS-DIAL export"){
          Lipidomedata <- processMSDIALExport(originaldata, session, input)[[1]]
		  lipidont <- processMSDIALExport(originaldata, session, input)[[3]] %>% dplyr::select(c(1,2))
		  colnames(lipidont) <- c("variable","Ontology")
          metadata <- processMSDIALExport(originaldata, session, input)[[2]]
          alitable <- process_alignment_file(originaldata)[[1]] %>% dplyr::select(c(1,2))
          moldata <- processMSDIALExporttomoldata(originaldata, session, input)[[1]] 
        } 
        if(length(input$transcriptomefile) !=0){
          transcriptome <- read.csv(input$transcriptomefile$datapath,check.names = F,fileEncoding ="UTF-8-BOM") %>% t()
          colnames(transcriptome) <- transcriptome[1,]
          transcriptome <- transcriptome[-1,] %>% data.frame()
          transcriptome[,-c(1)] <- apply(transcriptome[,-c(1)],2,as.numeric) %>% data.frame()
          transcriptome <-  rownames_to_column(transcriptome,"name")
          transcriptome[,2] <- as.character(transcriptome[,2])
        }
        
        if(length(input$file1) !=0 & length(input$transcriptomefile) != 0){
          
          Ensembl <- gconvert(query = colnames(transcriptome)[-c(1:2)] , organism = "mmusculus",#sapiens
                              target="ENSG", mthreshold = Inf, filter_na = TRUE) %>% dplyr::select(input,target)
          transcriptome <- transcriptome %>% dplyr::select(-2)
          myData <- inner_join(Lipidomedata,transcriptome,by =c("name"))
          moldata <- inner_join(moldata,transcriptome,by =c("name"))
        }
        else{
          myData <- Lipidomedata
        }
     # }, error = function(e) {
      #  showNotification(paste("Error:Invalid CSV file format. Please ensure your file is a properly formatted CSV.", e$message), type = "error")
      #})
      
      metainfocol <- ncol(metadata)
      lipidclassproperties <- read_csv("./pathwaymap/lipidclassproperties.csv")
      processAndUpdateInputs(myData, session, metadata, metainfocol)
      processAndUpdateInputs2(moldata, session, metadata, metainfocol)
      shinyalert(
        "Success",
        "GO to Plot tab",
        type = "success"
      )
      observeEvent(input$y,{
        targetclass <- filter(alitable,Ontology %in% input$y)
        shiny::updateSelectInput(session, "mol", selected = targetclass$`Metabolite name`[1], choices = targetclass$`Metabolite name`)
      })
      
      #observeEvent(input$Colorpicker,{
      #print("boton")
      #  ShowtestModaldialog4(data,input,col$col)
      #}
      #)
      observeEvent(input$exportgraph,{
        ShowtestModaldialog2(myData,input,col$col)
      }
      )
      observeEvent(input$Colorpicker, {
        ShowtestModaldialog(myData,input,col$col)
      })
      
      observeEvent(input$saveConfirm, {
        removeModal()
        showNotification("保存が完了しました", type = "success")
      })
      
      observeEvent(input$w,{
        updateOrderInput(session, "levels",
                         items = c(unique(as.matrix(dplyr::select(myData,input$w)))),
                         item_class = "success")
        shiny::updateSelectInput(session, "q", selected = c(unique(as.matrix(dplyr::select(myData,input$w))))[1], choices = c(unique(as.matrix(dplyr::select(myData,input$w)))))
        col$col <<- rainbow_hcl(length(unique(as.matrix(dplyr::select(myData,input$w)))))
        col$col <<- setNames(col$col,unique(as.matrix(dplyr::select(myData,input$w))))
      })
      
      observeEvent(input$levels,{
        if (!is.null(input$levels) && any(input$levels != "")) {
          dataa <<- mutate(myData, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
          moldataa <<- mutate(moldata, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
          
        }
      })
      
      observe({
        if (length(grep("selectcolor", names(input), value = TRUE)) != 0) {
          col$col <<-  process_select_color_input(input,myData)
        }
      })
      
      # 可能な比較の組み合わせを生成
      possible_comparisons <- reactive({
        groups <- unique(myData[[input$w]])
        combs <- combn(groups, 2, simplify = FALSE)
        setNames(
          lapply(combs, function(x) paste(x, collapse = "vs")),
          lapply(combs, function(x) paste(x, collapse = " vs "))
        )
      })
      
      # 比較選択用のUI生成
      output$comparison_choices <- renderUI({
        checkboxGroupInput("comparisons", "Select comparisons:",
                           choices = possible_comparisons(),
                           selected = possible_comparisons()[1])
      })
      
      # Select All ボタンの処理
      observeEvent(input$select_all, {
        updateCheckboxGroupInput(session, "comparisons",
                                 selected = possible_comparisons())
      })
      
      # Clear All ボタンの処理
      observeEvent(input$clear_all, {
        updateCheckboxGroupInput(session, "comparisons",
                                 selected = character(0))
      })
      
      # 統計検定の実行
      stat_test <- reactive({
        req(input$comparisons, input$test_method)
        
        # 検定関数の選択
        test_func <- get(input$test_method, asNamespace("rstatix"))
        
        # 結果を格納するデータフレーム
        results <- data.frame()
        
        for(comp in input$comparisons) {
          # グループを分割
          groups <- strsplit(comp, "vs")[[1]]
          
          # データのサブセット作成
          subset_data <- myData %>%
            dplyr::filter(!!sym(input$w) %in% groups)
          
          # 検定実行
          test_result <- test_func(subset_data, as.formula(paste(input$y, "~", input$w))) %>%
            adjust_pvalue(method = input$p_adjust) %>%
            add_significance()
          
          results <- rbind(results, test_result)
        }
        
        results
      })
      
      stat_testmol <- reactive({
        req(input$comparisons, input$test_method)
        
        # 検定関数の選択
        test_func <- get(input$test_method, asNamespace("rstatix"))
        
        # 結果を格納するデータフレーム
        results <- data.frame()
        
        for(comp in input$comparisons) {
          # グループを分割
          groups <- strsplit(comp, "vs")[[1]]
          
          # データのサブセット作成
          subset_data <- moldata %>%
            dplyr::filter(!!sym(input$w) %in% groups)
          
          # 検定実行
          test_result <- test_func(subset_data, as.formula(paste(paste("`",input$mol,"`",sep =""), "~", input$w))) %>%
            adjust_pvalue(method = input$p_adjust) %>%
            add_significance()
          
          results <- rbind(results, test_result)
        }
        
        results
      })
      
      if(input$pvaluecheck == TRUE){
      # サマリー統計の表示
      
      output$stat_results <- renderPrint({
        req(stat_test())
        results <- stat_test() 
        cat("Results:\n")
        print(summary(results))
      })
      }  
	  
	  observeEvent(input$selected_elements, {
if(!is.null(input$selected_elements)){
	selected <- input$selected_elements
	if(length(selected$nodes) != 0){
      moldataa <- mutate(moldata, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
      targetclass <- filter(alitable,Ontology %in% selected$nodes[[1]]$data$label)
        shiny::updateSelectInput(session, "selectmol", selected = targetclass$`Metabolite name`[1], choices = targetclass$`Metabolite name`)
		}}	
	  },ignoreNULL = TRUE)
	  
	  if(!is.null(input$selectmol)){
	  output$corselect2 <- renderPlot({
        if(!is.null(input$selectmol) && input$selectmol != " "){
          if(input$mydrop == "box"){
			g <- process_boxplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col,stat_testmol())
          }
          else if(input$mydrop == "bar"){
			g <- process_barplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col,stat_testmol())
          }
          else if(input$mydrop == "violin"){
			g <- process_violinplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col,stat_testmol())
          }
          else if(input$mydrop == "polar"){
			g <- process_polar_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col,stat_testmol())
          }
          else if(input$mydrop == "coding"){

			g <- process_dotplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col,stat_testmol())
          }
          plot(g)
        }
        
      })
	  }
	  
      observeEvent(input$levels,{
        observe({
          if (!is.null(input$mol) && input$mol != "") {
            if(!is.null(input$levels) && any(input$levels != "")){
              
              output$plottest <- renderPlot({
                
                if(input$mydrop == "box"){
                  process_boxplot(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "bar"){
                  process_barplot(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "violin"){
                  process_violinplot(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "polar"){
                  process_polarplot(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "coding"){
                  process_dotplot(input,input$y,dataa,col$col,stat_test())
                }
              }
              )
              output$mappingraph <- renderPlot({
                
                if(input$mydrop == "box"){
                  process_boxplot_diagram(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "bar"){
                  process_barplot_diagram(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "violin"){
                  process_violinplot_diagram(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "polar"){
                  process_polar_diagram(input,input$y,dataa,col$col,stat_test())
                }
                else if(input$mydrop == "coding"){
                  process_dotplot_diagram(input,input$y,dataa,col$col,stat_test())
                }
              }
              )
              
              observeEvent(input$mol,{
                
                output$plottest2 <- renderPlot({
                  if(input$mydrop == "box"){
                    process_boxplot(input, paste("`",input$mol,"`",sep =""),moldataa,col$col,stat_testmol())
                  }
                  else if(input$mydrop == "bar"){
                    process_barplot(input, paste("`",input$mol,"`",sep =""),moldataa,col$col,stat_testmol())
                  }
                  else if(input$mydrop == "violin"){
                    process_violinplot(input,paste("`",input$mol,"`",sep =""),moldataa,col$col,stat_testmol())
                  }
                  else if(input$mydrop == "polar"){
                    process_polarplot(input,paste("`",input$mol,"`",sep =""),moldataa,col$col,stat_testmol())
                  }
                  else if(input$mydrop == "coding"){
                    process_dotplot(input,paste("`",input$mol,"`",sep =""),moldataa,col$col,stat_testmol())
                  }
                  
                })
              })
            }
          }
        })
      })
	  
	   output$corselect <- renderPlotly({
	   if(!is.null(input$selected_elements)){
	    selected <- input$selected_elements
        if(length(selected$nodes) == 2){
		 cor_value <- cor(myData[,selected$nodes[[1]]$data$label], myData[,selected$nodes[[2]]$data$label],method = "spearman")
         g <- ggplot(myData,aes_string(selected$nodes[[1]]$data$label,selected$nodes[[2]]$data$label,fill = input$w,size = input$size))+geom_point(shape =21,color = "black")+
         scale_fill_manual(values = unlist(col$col)) + ggtitle(paste("r = ",round(cor_value, 2))) + theme_classic() + theme(aspect.ratio = 1.0)
	     plotly::ggplotly(g)
		 }
          }		 
          })
		  
  
    # Update group column choices
  output$groupColumnSelector <- renderUI({
    req(myData)
    
    # Use factor or character columns as group column candidates
    factor_cols <- names(myData)[sapply(myData, function(x) is.factor(x) || is.character(x))]
    
    if (length(factor_cols) == 0) {
      return(HTML("<div class='alert alert-warning'>No character or factor columns found for grouping.</div>"))
    }
    
    selectInput("groupColumn", "Group Column:", 
                choices = factor_cols, 
                selected = factor_cols[1])
  })
  
    # Update group value choices
  observeEvent(input$groupColumn, {
    req(myData, input$groupColumn)
    
    # Get group column values
    group_values <- unique(myData[[input$groupColumn]])
    
    if (length(group_values) < 2) {
      showNotification("At least 2 groups are needed for comparison", type = "warning")
      return()
    }
    
    # Update group 1 choices
    output$group1Selector <- renderUI({
      selectInput("group1", "Group 1:", 
                  choices = group_values, 
                  selected = group_values[1])
    })
    
    # Update group 2 choices
    output$group2Selector <- renderUI({
      # Safely select the second value, or use the first if there's no second value
      second_group <- ifelse(length(group_values) >= 2, group_values[2], group_values[1])
      
      selectInput("group2", "Group 2:", 
                  choices = group_values, 
                  selected = second_group)
    })
  })
  
  	  get_selected_df <- reactive({
    switch(input$dataset,
           "myData" = myData,
           "moldata" = moldata)
  })
  
    volcanoResults <- eventReactive(input$runAnalysisButton, {
    req(myData, input$groupColumn, input$group1, input$group2)
    
    # Input validation
    if (input$group1 == input$group2) {
      showNotification("Please select different groups", type = "error")
      return(NULL)
    }
    
    # Close the settings modal if open
    toggleModal(session, "settingsModal", toggle = "close")
    
    # Color settings
    colors <- c(
      "NS" = input$colorNS,
      "Up" = input$colorUp,
      "Down" = input$colorDown
    )
	
    # Run Volcano Plot analysis
    withProgress(message = 'Analyzing...', {
      tryCatch({

        create_volcano_plot(
          data = get_selected_df(),
          group_column = input$groupColumn,
          group1 = input$group1,
          group2 = input$group2,
          test_method = input$testMethod,
		  exclude_columns = "name",
          p_adjust_method = input$pAdjustMethod,
          fc_threshold_up = input$fcThresholdUp,
          fc_threshold_down = input$fcThresholdDown,
          p_threshold = input$pThreshold,
          point_size = input$pointSize,
          point_alpha = input$pointAlpha,
          Fontsize = input$fontSize,
          colors = colors
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        NULL
      })
    })
  })
  
  
    # Display Volcano Plot
  output$volcanoPlotly <- renderPlotly({
    req(volcanoResults())
    g <- volcanoResults()$plot
	plotly::ggplotly(g)
  })
  output$volcanoPlot <- renderPlot({
    req(volcanoResults())
    volcanoResults()$plot
  })
    # ランダムに選択したノードの色を変更
observeEvent(input$highlight_significant, {
  data <- network_data()
  if (is.null(data)) {
    showNotification("Network data not loaded", type = "error")
    return()
  }
  
  # 有意に増加/減少している変数名のリスト (仮の例)
  significantly_increased <- volcanoResults()$results %>% filter(significance == "Up") %>% pull(variable) 
  significantly_decreased <- volcanoResults()$results %>% filter(significance == "Down") %>% pull(variable)
  
  # 増加している変数に対応するノードIDを取得
  increased_node_ids <- c()
  for (node in data$nodes) {
    if (node$data$label %in% significantly_increased) {
      increased_node_ids <- c(increased_node_ids, node$data$id)
    }
  }
  
  # 減少している変数に対応するノードIDを取得
  decreased_node_ids <- c()
  for (node in data$nodes) {
    if (node$data$label %in% significantly_decreased) {
      decreased_node_ids <- c(decreased_node_ids, node$data$id)
    }
  }
  
if (length(increased_node_ids) > 0) {
  session$sendCustomMessage("changeRandomNodeColors", list(
    nodeIds = increased_node_ids,
    color = "#FF4500"  # 赤色
  ))
  
  showNotification(
    sprintf("Highlighted %d nodes with increased expression", length(increased_node_ids)),
    type = "message"
  )
}

# 減少しているノードの色を変更 (青色)
if (length(decreased_node_ids) > 0) {
  session$sendCustomMessage("changeRandomNodeColors", list(
    nodeIds = decreased_node_ids,
    color = "#1E90FF"  # 青色
  ))
  
  showNotification(
    sprintf("Highlighted %d nodes with decreased expression", length(decreased_node_ids)),
    type = "message"
  )
}
  
  # ハイライトされたノードがない場合の通知
  if (length(increased_node_ids) == 0 && length(decreased_node_ids) == 0) {
    showNotification("No significant nodes found in the network", type = "warning")
  }
})
  
  # Display results table
  output$resultsTable <- DT::renderDataTable({
    req(volcanoResults())
    
    # Format results table
    results_df <- volcanoResults()$results %>%
      mutate(
        pvalue = formatC(pvalue, digits = 6, format = "g"),
        pvalue_adj = formatC(pvalue_adj, digits = 6, format = "g")
      )
    
    DT::datatable(results_df, 
                  options = list(
                    pageLength = 7,
                    scrollX = TRUE,
                    order = list(list(3, 'desc')) # Sort by adjusted p-value
                  )) %>%
      DT::formatRound(c("log2FoldChange", "mean_group1", "mean_group2"), 4) %>%
      DT::formatStyle(
        "significance",
        backgroundColor = styleEqual(
          c("NS", "Up", "Down"),
          c(input$colorNS, input$colorUp, input$colorDown)
        ),
        color = styleEqual(
          c("NS", "Up", "Down"),
          c("white", "white", "white")
        )
      )
  })
  outputOptions(output, "resultsTable", suspendWhenHidden = FALSE)
  observeEvent(input$trigger_redraw_DT, {
    DT::dataTableProxy("resultsTable") %>% DT::reloadData()
  })
     
	output$downloadResults <- downloadHandler(
    filename = function() {
      paste("volcano_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(volcanoResults()$results, file, row.names = FALSE)
    }
  )
  # Extract target and background variables for LION
  targetVariables <- reactive({
    req(volcanoResults())
    
    # Get significant variables (Up or Down)
	if(input$enrichtarget == "Up"){
    significant_vars <- volcanoResults()$results %>%
      filter(significance %in% c("Up")) %>%
      pull(variable)
    }else{
	    significant_vars <- volcanoResults()$results %>%
      filter(significance %in% c("Down")) %>%
      pull(variable)
	  }
    # Return as newline-separated string
    paste(significant_vars, collapse = "\n")
  })
  
  backgroundVariables <- reactive({
    req(volcanoResults())
    print("test111")
    # Get all variables
    all_vars <- volcanoResults()$results$variable
    
    # Return as newline-separated string
    paste(all_vars, collapse = "\n")
  })
  
  
    lionResults <- eventReactive(input$runLIONButton, {
    req(targetVariables(),backgroundVariables())
	sublist_ids <- strsplit(targetVariables(), "\n")[[1]] %>% convertLipidNames()
    background_ids <-strsplit(backgroundVariables(), "\n")[[1]] %>% convertLipidNames()
    # Prepare input data for LION
    input_data <- list(
    sublist = paste(sublist_ids, collapse = "\n"),
    background = paste(background_ids, collapse = "\n")
    )
    # Run LION enrichment analysis
    withProgress(message = 'Running LION enrichment...', {
      tryCatch({   
		d <- runLIONenrichment(input_data = input_data,
		LIONterms_rules_file = "./pathwaymap/20191008 LIONterms_rules.csv",
		LIONterms_FAs_file = "./pathwaymap/20191008 LIONterms_FAs.csv",
		FA_composition_table = "./pathwaymap/FA_composition_table.csv" )
      }, error = function(e) {
        showNotification(paste("LION Error:", e$message), type = "error")
        NULL
      })
    })
  })
  ontology_result <- reactive({
  req(lionResults(),volcanoResults())
  volcanoResult <- inner_join(lipidont, volcanoResults()$results, by = "variable")
  sublist_ids <- strsplit(targetVariables(), "\n")[[1]] %>% convertLipidNames()
  print("aaaa")
  a <- generate_ontology_result(lionResults()$result_table, lionResults()$term_lipids, volcanoResult, sublist_ids )
  return(a)
})
  # すぐ後に強制評価用のobserveを配置
  observe({
    result <- ontology_result()
    print("ontology_result was observed")
  })
  
    
  # Original functionality for tab 3
  output$selection_info <- renderPrint({
    if (is.null(input$selected_elements)) {
      return("No elements selected")
    }
    
    selected <- input$selected_elements
    
    if (length(selected$nodes) > 0) {
      cat("Selected Nodes:\n")
      for (node in selected$nodes) {
        cat("\nNode:\n")
        cat("ID:", node$id, "\n")
        cat("Label:", node$data$label, "\n")
        cat("Name:", node$data$name, "\n")
        if (!is.null(node$data$Ensembl_ID) && node$data$Ensembl_ID != "") {
          cat("Ensembl ID:", node$data$Ensembl_ID, "\n")
        }
      }
    }
    
    if (length(selected$edges) > 0) {
      cat("\nSelected Edges:\n")
      for (edge in selected$edges) {
        cat("\nEdge:\n")
        cat("ID:", edge$id, "\n")
        cat("Source:", edge$source, "\n")
        cat("Target:", edge$target, "\n")
        cat("Interaction:", edge$data$interaction, "\n")
      }
    }
  })
  
  # ネットワークエクスポート
  observeEvent(input$export_btn, {
    # ファイル名にデフォルト値を設定
    filename <- if (input$export_filename == "") "network_export" else input$export_filename
    
    # フロントエンドにメッセージを送信してエクスポート
    session$sendCustomMessage("exportNetwork", list(
      format = input$export_format,
      filename = filename,
      scale = input$export_scale
    ))
    
    showNotification(
      sprintf("Exporting network as %s...", toupper(input$export_format)),
      type = "message"
    )
  })
  
  
  # ノードの色をリセット
  observeEvent(input$reset_colors, {
    session$sendCustomMessage("resetNodeColors", list())
    showNotification("Reset all node colors", type = "message")
  })
  
  observeEvent(input$delete_node, {
  session$sendCustomMessage("deleteSelectedNodes", list())
})

  output$node_relationship <- renderPrint({
    if (is.null(input$selected_elements) || 
        length(input$selected_elements$nodes) != 2) {
      return(NULL)
    }
    
    nodes <- input$selected_elements$nodes
    node1 <- nodes[[1]]
    node2 <- nodes[[2]]
    
    cat("Analyzing relationship between:\n")
    cat(sprintf("Node 1: %s (%s)\n", node1$data$name, node1$id))
    cat(sprintf("Node 2: %s (%s)\n", node2$data$name, node2$id))
  })
  
  style_data <- reactive({
    if (!is.null(input$style_file)) {
      tryCatch({
        style_content <- readLines(input$style_file$datapath, warn = FALSE)
        style_json <- paste(style_content, collapse = "")
        fromJSON(style_json, simplifyVector = FALSE)
      }, error = function(e) {
        showNotification(
          paste("Error reading style file:", e$message),
          type = "error"
        )
        NULL
      })
    } else {
      list(
        list(
          selector = "node",
          css = list(
            "border-width" = "data(BorderWidth)",
            "border-color" = "data(Color)",
			"border-style"=  "data(borderstyle)",
            "content" = "data(id)",
            "background-color" = "white",
            "shape" = "rectangle",
            "background-image" = "data(path)",
            "background-fit" = "cover cover",
            "label" = "data(label)",
            "height" = "data(Height)",
            "width" = "data(Width)",
            "font-size" = "18",
            "text-valign" = "top",
            "text-halign" = "center",
			"text-margin-y" = "22"
          )
        ),
        list(
          selector = "node:selected",
          css = list(
            "border-width" = "3px",
            "border-color" = "red"
          )
        ),
        list(
          selector = "edge",
          css = list(
            "width" = 2,
            "line-color" = "#888",
            "curve-style" = "bezier",
            "target-arrow-shape" = "triangle",
            "target-arrow-color" = "#888",
            "arrow-scale" = 1.5
          )
        ),
        list(
          selector = "edge:selected",
          css = list(
            "width" = 3,
            "line-color" = "#ff0000",
            "target-arrow-color" = "#ff0000"
          )
        )
      )
    }
  })
  
     
network_data <- reactive({
  # pathway typeが変更されたときに実行されるようにinput$pathwaytypeを要求
  req(input$pathwaytype)
  
  print("network_data is being evaluated")
  
  content <- if (input$pathwaytype == "Global pathway") {
    readLines("./pathwaymap/globalpathway.cyjs", warn = FALSE)
  } else if (input$pathwaytype == "Ceramide pathway") {
    readLines("./pathwaymap/ceramidepathway.cyjs", warn = FALSE)
  } else if (input$pathwaytype == "Remodeling pathway") {
    readLines("./pathwaymap/remodeling.cyjs", warn = FALSE)
  }
  
  # コンテンツが正常に読み込まれたか確認
  if (is.null(content) || length(content) == 0) {
    showNotification("ネットワークファイルが空か読み込めませんでした", type = "error")
    return(NULL)
  }
  
  # 有効なontology結果があるかをチェック - エラーをキャッチするためにtry()を使用
  ont_data <- try(ontology_result(), silent = TRUE)
  
  print(paste("Ontology data is a try-error?", inherits(ont_data, "try-error")))
  print(paste("Ontology data is NULL?", is.null(ont_data)))
  
  # ontologyデータが有効で結果を持っているか確認
  if (!inherits(ont_data, "try-error") && !is.null(ont_data) && 
      !is.null(ont_data$result) && nrow(ont_data$result) > 0) {
    
    print("Ontology data is valid, integrating with network data")
    
    # ontologyデータをネットワークコンテンツと統合
    integrated_content <- tryCatch({
      integrate_ontology_preserving_format(content, ont_data$result)
    }, error = function(e) {
      print(paste("Error in integrate_ontology_preserving_format:", e$message))
      showNotification(
        paste("ontologyデータの統合中にエラーが発生しました:", e$message),
        type = "error"
      )
      # 統合に失敗した場合は元のコンテンツを返す
      content
    })
    
    # 統合されたコンテンツを使用
    content <- integrated_content
    print("Integration completed")
  } else {
    print("Ontology data not valid or not available yet")
  }
  
  # JSONデータの解析
  tryCatch({
    json_text <- paste(content, collapse = "")
    json_data <- fromJSON(json_text, simplifyVector = FALSE)
    print("Network data parsed successfully")
    json_data$elements
  }, error = function(e) {
    print(paste("Error parsing network data:", e$message))
    showNotification(
      paste("ネットワークファイルの読み込み中にエラーが発生しました:", e$message),
      type = "error"
    )
    NULL
  })
})

observeEvent(input$showTooltips, {
  session$sendCustomMessage("toggleTooltips", list(
    show = input$showTooltips,
    theme = input$tooltipTheme,
    opacity = input$tooltipOpacity
  ))
})
# ontology結果が変更されたときにネットワークを更新するオブザーバーを追加
observe({
  print("Checking if ontology data has changed...")
  # 有効なontology結果がある場合のみ続行
  ont_data <- try(ontology_result(), silent = TRUE)
  if (inherits(ont_data, "try-error") || is.null(ont_data)) {
    print("Ontology data not available yet, skipping network update")
    return(NULL)
  }
  
  print("Ontology data available, updating network...")
  
  # network_data リアクティブを再実行させる
  data <- network_data()
  style <- style_data()
  
  # 有効なデータがある場合はネットワークを更新
  if (!is.null(data) && !is.null(style)) {
    print("Sending updated network data to UI")
    session$sendCustomMessage("updateNetwork", list(
      data = data,
      style = style
    ))
  } else {
    print("Network data or style is NULL, cannot update UI")
  }
})
  
  observe({
    data <- network_data()
    style <- style_data()
    req(data, style)
    
    session$sendCustomMessage("updateNetwork", list(
      data = data,
      style = style
    ))
  })
  
    output$enrich <- renderPlotly({
    req(lionResults())
    category <- read.csv("./pathwaymap/LIONOntology.csv")
    lion_data <- lionResults()$result_table 
	lion_data[,-c(1,2)] <- apply(lion_data[,-c(1,2)], 2, as.numeric)
	res <- inner_join(lion_data,category,by=c("Term ID"= "LION.ID"))
	
	g <- ggplot(res, aes(x = Category, y = -log(res$`FDR q-value`), color = Category,size = Significant, text = paste("Description:", Description,
                                                                                                                 "<br>FDR q-value:", `FDR q-value`,
                                                                                                                 "<br>Count:", Significant))) +
    geom_jitter(width = 0.3, alpha = 0.7) +
    scale_color_bright() +
    geom_hline(yintercept = -log10(0.05), linetype = "dash", color = "gray44",linewidth = 0.3) +
    theme_classic() +
    theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
    plotly::ggplotly(g, tooltip = "text")
  })
    observeEvent(input$trigger_redraw_ENRICH, {
  plotTrigger(plotTrigger() + 1)  # ← これで再描画を強制
})

	output$enrichbarplot <- renderPlot({
    req(lionResults())
	plotTrigger()
	#volcanoResult <- inner_join(lipidont, volcanoResults()$results, by = "variable")
    lionResults()$enrichment_plot
  },height = 800)
 outputOptions(output, "enrichbarplot", suspendWhenHidden = FALSE)
 
    output$enrichtable <- DT::renderDataTable({
    req(lionResults())
	    results_df <- lionResults()$result_table
    
    DT::datatable(results_df, 
                  options = list(
                    pageLength = 7,
                    scrollX = TRUE,
					order = list(list(6, 'desc'))
                  ))
  })		
  outputOptions(output, "enrichtable", suspendWhenHidden = FALSE)
  


  observeEvent(input$trigger_redraw_ENRICH, {
    DT::dataTableProxy("enrichtable") %>% DT::reloadData()
  })

        
      #Creating heatmap
      observeEvent(input$actionplottest, {
        waiter::waiter_show(
          id = NULL,
          html = tagList(waiter::spin_loader(),
                         "Loading ..."),
          color = "#333e48",
          logo = "",
          image = ""
        )
        file_list <- list.files(svg_path)
        if (length(unlist(file_list)) > 0) {
          file.remove(file.path(svg_path, file_list))
        } else {
        }
        if(is.null(input$transcriptomefile) == FALSE){
          Ensembl <- gconvert(query = colnames(transcriptome)[-c(1:2)] , organism = "mmusculus",#sapiens
                              target="ENSG", mthreshold = Inf, filter_na = TRUE) %>% dplyr::select(input,target)
          graph_json1 <- paste(readLines("./pathwaymap/ceramidepathway.cyjs"), collapse="")  %>% fromJSON()
          graph_json2 <- paste(readLines("./pathwaymap/remodeling.cyjs"), collapse="") %>% fromJSON()
          Ensemblinmap <- c(graph_json1$elements$nodes$data$Ensembl_ID,graph_json2$elements$nodes$data$Ensembl_ID)
          Ensemblinmap <- Ensemblinmap[-which(Ensemblinmap %in% "")]
          
          geneinmap <- Ensembl[Ensembl[,2] %in% Ensemblinmap,]
          
          transcriptome2 <- colnames(transcriptome)[colnames(transcriptome) %in% geneinmap$input]
          
          myData <- myData[,colnames(myData) %in% c(colnames(myData)[1:metainfocol],colnames(Lipidomedata),transcriptome2)]
          dataa <- mutate(myData, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
          names(dataa)[match(geneinmap$input, names(dataa))] <- geneinmap$target
          
        } else {
          dataa <- mutate(myData, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
        }
        
        # if (length(grep("selectcolor", names(input), value = TRUE)) != 0) {
        #   col <<-  process_select_color_input(input,data)
        # }
        if(input$mydrop == "box"){
          process_action_boxplot(input,dataa,metainfocol,svg_path,col$col,output,myData) 
        }
        else if(input$mydrop == "bar"){
          process_action_barplot(input,dataa,metainfocol,svg_path,col$col,output,myData) 
        }
        else if(input$mydrop == "violin"){
          process_action_violinplot(input,dataa,metainfocol,svg_path,col$col,output,myData) 
        }
        else if(input$mydrop == "polar"){
          process_action_polarplot(input,dataa,metainfocol,svg_path,col$col,output,myData) 
        }
        else if(input$mydrop == "coding"){
          process_action_dotplot(input,dataa,metainfocol,svg_path,col$col,output,myData) 
        }
        # if(input$filetype == "MS-DIAL export"){
        #   exportgroupnodeheatmap(originaldata,metadata,paste(global$datapath,"",sep =""),input$w,input$levels,originaldir,input)}
        Sys.sleep(3)
        waiter::waiter_hide()
        shinyalert(
          "Success",
          "GO to Pathway analysis tab",
          type = "success"
        )
      }
      )
      observeEvent(input$w,{
 	
        output$heatmap <- renderPlot({
		 if(!is.null(input$selected_elements)){
	     selected <- input$selected_elements
	     if(length(selected$nodes) != 0){	
            groupnodeheatmap(lipidclassproperties,originaldata,metadata,input$w,input$levels,selected$nodes[[1]]$data$label,input)
          }}
        },height = 1000)
		outputOptions(output, "heatmap", suspendWhenHidden = FALSE)
      })
	   
	  
    } )})
}