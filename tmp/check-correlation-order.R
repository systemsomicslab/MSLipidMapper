.libPaths(c("C:/Users/takak/AppData/Local/R/win-library/4.5", .libPaths()))
library(shiny)
library(ggplot2)
source("R/corrlation_module.R")
lines <- readLines("R/corrlation_module.R", warn=FALSE)
start <- grep("    get_group_colors <-", lines)
end <- grep("    # All-pair cache", lines)-1L
eval(parse(text=lines[start:end]))
settings <- list(manual_order=c("2", "12", "19", "24"),
                 palette_map=c("12"="#66bb11", "19"="#22bbaa", "2"="#ee7777", "24"="#bb77ee"))
adv_get <- function() settings
groups <- c("12", "19", "2", "24")
cols <- get_group_colors(groups)
stopifnot(identical(names(cols), settings$manual_order), cols[["2"]]=="#ee7777")
data <- data.frame(X=1:12, Y=c(3,2,4,1,5,8,6,7,10,9,11,12), Group=rep(groups,3))
cor_res_single <- function() list(data=data, filtered=data, cor_test=cor.test(data$X,data$Y), transform="none", x_label="X", y_label="Y", group_col="class")
correlation_font_size <- function() 12
start <- grep("    scatter_gg <-", lines)
end <- grep("    shiny::observeEvent\\(cor_res_single", lines)-1L
eval(parse(text=lines[start:end]))
p <- isolate(scatter_gg())
scale <- ggplot_build(p)$plot$scales$get_scales("fill")
stopifnot(identical(as.character(scale$get_breaks()), settings$manual_order))
settings$manual_order <- c("24", "2")
cols <- get_group_colors(groups)
stopifnot(identical(names(cols), c("24", "2", "12", "19")), cols[["2"]]=="#ee7777")
settings$palette_map <- NULL
stopifnot(identical(names(get_group_colors(groups)), c("24", "2", "12", "19")))
cat("Correlation legend order, partial order and color identity verified\n")
