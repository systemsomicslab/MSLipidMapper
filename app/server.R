server <- function(input, output,session) {
  options(shiny.maxRequestSize=150*1024^100)
  originaldir <- reactiveValues(datapath = getwd()) # directry of shiny R script
  global <- reactiveValues(datapath = getwd()) # directory of file path in lipidomics tab
  col = reactiveValues(col = col)
  svg_path <- paste(getwd(),"/svg",sep = "")

    observeEvent(input$filetype,{
    if(input$filetype =="Sample in rows"){
      shinyjs::show("ontfile")
    }else{
      shinyjs::hide("ontfile")
    }
	if(input$ClassorMol =="TRUE"){
      shinyjs::show("X1")
	  shinyjs::show("X2")
    }else{
      shinyjs::hide("X1")
	  shinyjs::hide("X2")
    }
	})
	
	observeEvent(input$ClassorMol,{
	if(input$ClassorMol =="TRUE"){
      shinyjs::show("X1")
	  shinyjs::show("X2")
    }else{
      shinyjs::hide("X1")
	  shinyjs::hide("X2")
    }
	})
	
 observeEvent(input$file1, {	
 tryCatch({
    originaldata <- read.csv(input$file1$datapath, header = F, check.names = F, fileEncoding = "UTF-8-BOM")
  }, error = function(e) {
    showNotification(paste("Failed to load the file", e$message), type = "error")
  })
	observeEvent(input$submit,{
  tryCatch({
  if(input$filetype == "Sample in rows"){
    
    Lipidomedata <- processSampleInRows(originaldata, session, input)[[1]]
    lipidont <- read.csv(input$ontfile$datapath, sep = ",", check.names = FALSE)
    metadata <- processSampleInRows(originaldata, session, input)[[2]]
    alitable <- read.csv(input$ontfile$datapath, check.names = FALSE, fileEncoding = "UTF-8-BOM")
    colnames(alitable) <- c("Metabolite name","Ontology")
    letchoice <- c(colnames(Lipidomedata)[!colnames(Lipidomedata) %in% colnames(metadata)],"Others")
    moldata <- processSampleInRowstomoldata(originaldata, session, input)[[1]]
  }else if(input$filetype == "MS-DIAL export"){
  Lipidomedata <- processMSDIALExport(originaldata, session, input)[[1]]
  metadata <- processMSDIALExport(originaldata, session, input)[[2]]
  alitable <- process_alignment_file(originaldata)[[1]] %>% select(c(1,2))
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
                       target="ENSG", mthreshold = Inf, filter_na = TRUE) %>% select(input,target)
    transcriptome <- transcriptome %>% select(-2)
    data <- inner_join(Lipidomedata,transcriptome,by =c("name"))
	moldata <- inner_join(moldata,transcriptome,by =c("name"))
  }
  else{
    data <- Lipidomedata
  }
  }, error = function(e) {
    showNotification(paste("Error:Invalid CSV file format. Please ensure your file is a properly formatted CSV.", e$message), type = "error")
  })
  
  metainfocol <- ncol(metadata)
  lipidclassproperties <- read_csv("./pathwaymap/lipidclassproperties.csv")
  
  processAndUpdateInputs(data, session, metadata, metainfocol)
  processAndUpdateInputs2(moldata, session, metadata, metainfocol)
  observeEvent(input$y,{
    targetclass <- filter(alitable,Ontology %in% input$y)
    shiny::updateSelectInput(session, "mol", selected = targetclass$`Metabolite name`[1], choices = targetclass$`Metabolite name`)
  })

  shinyjs::onclick(id = "Colorpicker",{
    ShowtestModaldialog(data,input,col$col)
  }
  )
  shinyjs::onclick(id = "exportgraph",{
    ShowtestModaldialog2(data,input,col$col)
  }
  )

  observeEvent(input$w,{
    updateOrderInput(session, "levels",
                     items = c(unique(as.matrix(select(data,input$w)))),
                     item_class = "success")
    shiny::updateSelectInput(session, "q", selected = c(unique(as.matrix(select(data,input$w))))[1], choices = c(unique(as.matrix(select(data,input$w)))))
    col$col <<- rainbow_hcl(length(unique(as.matrix(select(data,input$w)))))
    col$col <<- setNames(col$col,unique(as.matrix(select(data,input$w))))
  })
  
  observeEvent(input$levels,{
    if (!is.null(input$levels) && any(input$levels != "")) {
      dataa <<- mutate(data, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
      moldataa <<- mutate(moldata, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
      
    }
  })
  
  observe({
    if (length(grep("selectcolor", names(input), value = TRUE)) != 0) {
      col$col <<-  process_select_color_input(input,data)
    }
  })
  
  observeEvent(input$levels,{
    observe({
      if (!is.null(input$mol) && input$mol != "") {
        if(!is.null(input$levels) && any(input$levels != "")){
          
          output$plottest <- renderPlotly({
            
            if(input$mydrop == "box"){
              process_boxplot(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "bar"){
              process_barplot(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "violin"){
              process_violinplot(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "polar"){
              process_polarplot(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "coding"){
              process_dotplot(input,input$y,dataa,col$col)
            }
          }
          )
          output$mappingraph <- renderPlot({
            
            if(input$mydrop == "box"){
              process_boxplot_diagram(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "bar"){
              process_barplot_diagram(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "violin"){
              process_violinplot_diagram(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "polar"){
              process_polar_diagram(input,input$y,dataa,col$col)
            }
            else if(input$mydrop == "coding"){
              process_dotplot_diagram(input,input$y,dataa,col$col)
            }
          }
          )
          
          observeEvent(input$mol,{
            
            output$plottest2 <- renderPlotly({
              if(input$mydrop == "box"){
                process_boxplot(input, paste("`",input$mol,"`",sep =""),moldataa,col$col)
              }
              else if(input$mydrop == "bar"){
                process_barplot(input, paste("`",input$mol,"`",sep =""),moldataa,col$col)
              }
              else if(input$mydrop == "violin"){
                process_violinplot(input,paste("`",input$mol,"`",sep =""),moldataa,col$col)
              }
              else if(input$mydrop == "polar"){
                process_polarplot(input,paste("`",input$mol,"`",sep =""),moldataa,col$col)
              }
              else if(input$mydrop == "coding"){
                process_dotplot(input,paste("`",input$mol,"`",sep =""),moldataa,col$col)
              }
              
            })
          })
		   }
      }
    })
  })
  
  graph_json <- reactive({
    if (input$pathwaytype == "Global pathway") {
      read_graph_json("./pathwaymap/globalpathway.cyjs")
    } else if (input$pathwaytype == "Ceramide pathway") {
      read_graph_json("./pathwaymap/ceramidepathway.cyjs")
    } else if (input$pathwaytype == "Remodeling pathway") {
      read_graph_json("./pathwaymap/remodeling.cyjs")
    }
  })
  
  style_file_path <- "./pathwaymap/nodestyle1.js"
  styles_xml_path <- "./pathwaymap/styles.xml"
  # CYJSファイルのダウンロード
  output$exportCYJS <- downloadHandler(
    filename = function() {
      paste0(input$pathwaytype, "_", format(Sys.Date(), "%Y%m%d"), ".cyjs")
    },
    content = function(file) {
      writeLines(graph_json, file)
    },
    contentType = "application/json"
  )
  
  # styles.xmlファイルのダウンロード
  output$exportStyles <- downloadHandler(
    filename = function() {
      "styles.xml"
    },
    content = function(file) {
      file.copy(styles_xml_path, file)
    },
    contentType = "application/xml"
  )
  #observeEvent(input$sidebarCollapse, {
  #  toggleClass(id = "content", class = "active")
  #})
  
   observeEvent(input$toggle_sidebar, {
    shinyjs::runjs("toggleSidebar()")
  })
  
  observeEvent(input$pathway, {
    output$graphContainer <- renderUI({
      cyjShinyOutput("cyjShinytest", height = "90%", width = "90%")
    })
  })
          output$cyjShinytest <- renderCyjShiny({
            #p <- input$pathway           
            #if (p != 0) {
              if (input$pathwaytype == "Global pathway") {
                graph_json <<- paste(readLines("./pathwaymap/globalpathway.cyjs"), collapse = "")
              } else if (input$pathwaytype == "Ceramide pathway") {
                graph_json <<- paste(readLines("./pathwaymap/ceramidepathway.cyjs"), collapse = "")    
              } else if (input$pathwaytype == "Remodeling pathway") {
                graph_json <<- paste(readLines("./pathwaymap/remodeling.cyjs"), collapse = "")   
              }
              
              test <- fromJSON(graph_json)
              test <- as.data.frame(test$elements$nodes)
              
              if (input$viewacyllevel == TRUE) {
                cyjShiny(graph_json, layoutName = "preset", styleFile = "./pathwaymap/nodestyle1.js")
              } else {
                cyjShiny(graph_json, layoutName = "preset", styleFile = "./pathwaymap/nodestyle2.js")
              }
           # } else {
              
            #}
          })
		  
		  
		  
          observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
            output$selectedNodesDisplay <- renderText({" "})
            getSelectedNodes(session)
          })
          
          
          output$corselect <- renderPlotly({
            if(input$pathwaytype == "Global pathway"){
              graph_json <<- paste(readLines("./pathwaymap/globalpathway.cyjs"), collapse="")
            }else if(input$pathwaytype == "Ceramide pathway"){
              graph_json <<- paste(readLines("./pathwaymap/ceramidepathway.cyjs"), collapse="")    
            }else if(input$pathwaytype == "Remodeling pathway"){
              graph_json <<- paste(readLines("./pathwaymap/remodeling.cyjs"), collapse="")      
            }
            test <- fromJSON(graph_json)
            test <- as.data.frame(test$elements$nodes)
            test1 <- test[test$data$id %in% unlist(input$selectedNodes),]
            dataa <- mutate(data, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
            
            if(length(test1$data$shared_name) == 2){
            moldataa <- mutate(moldata, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
            targetclass <- filter(alitable,Ontology %in% test1$data$shared_name)
              if(! test1$data$shared_name[1] %in% colnames(data)) {
                a <- Ensembl[Ensembl$target %in% test1$data$Ensembl_ID[1],]
                test1$data$shared_name[1] <- a$input
              }
              if(! test1$data$shared_name[2] %in% colnames(data)) {
                b <- Ensembl[Ensembl$target %in% test1$data$Ensembl_ID[2],]
                test1$data$shared_name[2] <- b$input
              }
			  if(input$ClassorMol == FALSE){
			  cor_value <- cor(dataa[,test1$data$shared_name[1]], dataa[,test1$data$shared_name[2]],method = "spearman")
              g <- ggplot(dataa,aes_string(test1$data$shared_name[1],test1$data$shared_name[2],fill = input$w,size = input$size))+geom_point(shape =21,color = "black")+
    scale_fill_manual(values = unlist(col$col)) + ggtitle(paste("r = ",round(cor_value, 2)))
			  }else{
			  moldataa <- mutate(moldata, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
	cor_value <- cor(moldataa[,input$X1], moldataa[,input$X2],method = "spearman")
			  g <- ggplot(moldataa,aes_string(paste("`",input$X1,"`",sep =""),paste("`",input$X2,"`",sep =""),fill = input$w,size = input$size))+geom_point(shape =21,color = "black")+
    scale_fill_manual(values = unlist(col$col))+ ggtitle(paste("r = ",round(cor_value, 2)))
			  }
			  plotly::ggplotly(g)
            }
            
          })
		  output$textOutput <- renderText({
		  if(input$ClassorMol == FALSE){
    "Select two nodes from the network to analyze their correlation."}
	else{
	"Select two features to analyze their correlation."
	}
  })
		  


		  
		     observeEvent(input$selectedNodes, {
      if(input$pathwaytype == "Global pathway"){
        graph_json <<- paste(readLines("./pathwaymap/globalpathway.cyjs"), collapse="")
      }else if(input$pathwaytype == "Ceramide pathway"){
        graph_json <<- paste(readLines("./pathwaymap/ceramidepathway.cyjs"), collapse="")    
      }else if(input$pathwaytype == "Remodeling pathway"){
        graph_json <<- paste(readLines("./pathwaymap/remodeling.cyjs"), collapse="")      
      }
      test <- fromJSON(graph_json)
      test <- as.data.frame(test$elements$nodes)
      test1 <- test[test$data$id %in% unlist(input$selectedNodes),]
      moldataa <- mutate(moldata, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
      targetclass <- filter(alitable,Ontology %in% test1$data$shared_name)
        shiny::updateSelectInput(session, "selectmol", selected = targetclass$`Metabolite name`[1], choices = targetclass$`Metabolite name`)
      },ignoreNULL = TRUE)
	  
		  output$corselect2 <- renderPlot({
        if(input$pathwaytype == "Global pathway"){
          graph_json <<- paste(readLines("./pathwaymap/globalpathway.cyjs"), collapse="")
        }else if(input$pathwaytype == "Ceramide pathway"){
          graph_json <<- paste(readLines("./pathwaymap/ceramidepathway.cyjs"), collapse="")    
        }else if(input$pathwaytype == "Remodeling pathway"){
          graph_json <<- paste(readLines("./pathwaymap/remodeling.cyjs"), collapse="")      
        }
        test <- fromJSON(graph_json)
        test <- as.data.frame(test$elements$nodes)
        test1 <- test[test$data$id %in% unlist(input$selectedNodes),]
        moldataa <- mutate(moldata, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
        targetclass <- filter(alitable,Ontology %in% test1$data$shared_name)
        if(length(test1$data$shared_name) > 0 && input$selectmol != " "){
          if(test1$data$shared_name[1] %in% colnames(data)) {
          if(input$mydrop == "box"){
			g <- process_boxplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col)
          }
          else if(input$mydrop == "bar"){
			g <- process_barplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col)
          }
          else if(input$mydrop == "violin"){
			g <- process_violinplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col)
          }
          else if(input$mydrop == "polar"){
			g <- process_polar_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col)
          }
          else if(input$mydrop == "coding"){

			g <- process_dotplot_diagram(input,paste("`",input$selectmol,"`",sep =""),moldataa,col$col)
          }}
          plot(g)
        }
        
      })
          

  observeEvent(input$save_pdf,{
            shinyscreenshot::screenshot(selector="#cyjShinytest")
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
                                   target="ENSG", mthreshold = Inf, filter_na = TRUE) %>% select(input,target)
              graph_json1 <- paste(readLines("./pathwaymap/ceramidepathway.cyjs"), collapse="")  %>% fromJSON()
              graph_json2 <- paste(readLines("./pathwaymap/remodeling.cyjs"), collapse="") %>% fromJSON()
              Ensemblinmap <- c(graph_json1$elements$nodes$data$Ensembl_ID,graph_json2$elements$nodes$data$Ensembl_ID)
              Ensemblinmap <- Ensemblinmap[-which(Ensemblinmap %in% "")]
	
              geneinmap <- Ensembl[Ensembl[,2] %in% Ensemblinmap,]

              transcriptome2 <- colnames(transcriptome)[colnames(transcriptome) %in% geneinmap$input]
			  
              data <- data[,colnames(data) %in% c(colnames(data)[1:metainfocol],colnames(Lipidomedata),transcriptome2)]
			  dataa <- mutate(data, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
			  names(dataa)[match(geneinmap$input, names(dataa))] <- geneinmap$target
			  
			  } else {
			  dataa <- mutate(data, !!as.symbol(input$w) := !!as.symbol(input$w) %>% factor(levels = input$levels))
			 }
            
           # if (length(grep("selectcolor", names(input), value = TRUE)) != 0) {
           #   col <<-  process_select_color_input(input,data)
           # }
            if(input$mydrop == "box"){
              process_action_boxplot(input,dataa,metainfocol,svg_path,col$col,output) 
            }
            else if(input$mydrop == "bar"){
              process_action_barplot(input,dataa,metainfocol,svg_path,col$col,output) 
            }
            else if(input$mydrop == "violin"){
              process_action_violinplot(input,dataa,metainfocol,svg_path,col$col,output) 
            }
            else if(input$mydrop == "polar"){
              process_action_polarplot(input,dataa,metainfocol,svg_path,col$col,output) 
            }
            else if(input$mydrop == "coding"){
              process_action_dotplot(input,dataa,metainfocol,svg_path,col$col,output) 
            }
            # if(input$filetype == "MS-DIAL export"){
            #   exportgroupnodeheatmap(originaldata,metadata,paste(global$datapath,"",sep =""),input$w,input$levels,originaldir,input)}
            Sys.sleep(3)
             waiter::waiter_hide()
        }
          )
  observeEvent(input$w,{
   
  output$heatmap <- renderPlot({
  if(length(unlist(input$selectedNodes)) > 0 ) {
        test <- fromJSON(graph_json)
      test <- as.data.frame(test$elements$nodes)
      test1 <- test[test$data$id %in% unlist(input$selectedNodes),]
     groupnodeheatmap(lipidclassproperties,originaldata,metadata,input$w,input$levels,test1$data$shared_name[1],input)
	 }
  })
  })
} )})}
