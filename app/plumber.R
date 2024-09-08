
library(plumber)


# Title
#* @apiTitle resr4cyjs
# Description
#* @apiDescription This is a resr4cyjs API.
# Version
#* @apiVersion 0.0.1
# Tag Description
#* @apiTag resr4cyjs "resr4cyjs API"


#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
   res$setHeader("Access-Control-Allow-Methods", "GET")
    res$setHeader(
      "Access-Control-Allow-Headers",
      req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
    )
   res$status <- 200
   return(list())
  } else {
    plumber::forward()
  }
}



#* @serializer contentType list(type="image/svg+xml")
#* @get /plot
#* @arg str1
function(str1) {
  thefolder <- "/srv/app/svg/"
  svg_file_path <- paste(thefolder,str1,sep = "")
  if(file.exists(svg_file_path) == TRUE){
  readBin(svg_file_path, "raw", file.info(svg_file_path)$size)}
  else{
    svg_file_path <- "/srv/app/www/background.svg"
    readBin(svg_file_path, "raw", file.info(svg_file_path)$size)
  }
}



