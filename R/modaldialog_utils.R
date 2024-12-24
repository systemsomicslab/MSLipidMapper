ShowtestModaldialog = function(data,input,col){
  showModal(modalDialog(
    h2("Appearance setting"),
    
    div( class = "dynamicSI",
         lapply(1:length(names(col)), function(i){
           column(3,   
                  colorPickr(paste0(names(col)[i],"_selectcolor"),theme = "monolith",label = paste0(names(col)[i],""),  selected  = col[i],interaction =  list(
                    clear = T,
                    save = T,
                    RGBA = TRUE,
                    input=TRUE,
                    hex =TRUE
                    
                  ))       
                  
           )})),
    br(),
    br(),
    br(),
    br(),
    br(),
    easyClose = TRUE,
    footer = NULL
  ))
}

ShowtestModaldialog2 = function(data,input,col){
  showModal(modalDialog(
    title = "Plot in Dialog",
    plotOutput("mappingraph"),
    actionButton("actionplottest","pathway mapping",onclick = "$(tab).removeClass('disabled')"),
    easyClose = TRUE,
    footer = NULL
  ))
}


ShowtestModaldialog4 = function(data,input,col){
showModal(modalDialog(
      h2("Appearance setting"),
    
    div( class = "dynamicSI",
         lapply(1:length(names(col)), function(i){
           column(3,   
                  colorPickr(paste0(names(col)[i],"_selectcolor"),theme = "monolith",label = paste0(names(col)[i],""),  selected  = col[i],interaction =  list(
                    clear = T,
                    save = T,
                    RGBA = TRUE,
                    input=TRUE,
                    hex =TRUE
                    
                  ))       
                  
           )})),
    br(),
    br(),
    br(),
    br(),
    br(),
    easyClose = TRUE,
    footer = NULL
    ))
	}