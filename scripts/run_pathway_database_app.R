# Run from the repository root without installing the package.
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")), .libPaths()))
source("R/pathway_knowledge.R")
source("R/organism_pathways.R")
source("R/pathway_database_app.R")
args <- commandArgs(trailingOnly = TRUE)
run_pathway_database_app(launch.browser = FALSE,
  port = if (length(args)) as.integer(args[1]) else 3847L)
