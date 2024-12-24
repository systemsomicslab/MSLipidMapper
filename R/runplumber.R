library(plumber)
# 'plumber.R' is the location of the file shown above
#
 # RUN = "TRUE"
 # while(RUN == "TRUE"){
 #   a <- readLines("./runShinyApp.Rout")
 #   b <- grepl("Close the program", a)
 #   if(TRUE %in% b) {
 #     RUN = "FALSE"
 #     stop()
 #   }
 # }
pr("./shiny/plumber.R") %>%
  pr_run()


# library(plumbplumb)
# library(plumber)
# 
# .state <- new.env(parent = emptyenv()) #create .state when package is first loaded
# 
# start_plumber3 <- function(path) {
#   trml <- rstudioapi::terminalCreate(show = FALSE)
#   rstudioapi::terminalSend(trml, "R\n") 
#   Sys.sleep(2)
#   cmd <- sprintf('plumber::plumb("%s")$run()\n', path)
#   rstudioapi::terminalSend(trml, cmd)
#   
#   .state[["trml"]] <- trml #store terminal name
#   invisible(trml)
# }
# 
# kill_plumber <- function() {
#   rstudioapi::terminalKill(.state[["trml"]]) #access terminal name
# }
# 
# 
# 
# plumber_path <- system.file("./shiny/plumber.R", package = "plumbplumb")
# start_plumber3(plumber_path)

# while (TRUE) {
#   a <- readLines("./runShinyApp.Rout")
#   b <- grepl("Close the program", a)
#   print("ccccc")
#   if (TRUE %in% b) {
#     message("Stopping the loop...")
#     future::plan("sequential")
#     quit()
#   }
#   Sys.sleep(1)
# }