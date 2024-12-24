collist <- list()
colset <- function(input,col){
  if(length(grep("selectcolor", names(input), value = TRUE)) != 0){
    inputlist<- grep("selectcolor", names(input), value = TRUE) 
    for (input_name in inputlist) {
      
      collist[str_remove_all(input_name,pattern = "_selectcolor")] <- input[[input_name]]
      
    }
    col <<- unlist(collist)
    col <<- col[c(unique(data[[input$x]]))]
  }}