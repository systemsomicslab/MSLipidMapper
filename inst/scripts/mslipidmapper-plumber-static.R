#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: mslipidmapper-plumber-static.R ROOT PORT")
root <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
port <- as.integer(args[2])
pr <- plumber::pr()
pr$handle("GET", "/healthz", function() list(ok = TRUE))
fm <- try(names(formals(plumber::pr_static)), silent = TRUE)
if (!inherits(fm, "try-error") && length(fm) && fm[1] %in% c("dir", "path")) {
  pr$mount("/", plumber::pr_static(dir = root))
} else {
  pr <- get("pr_static", asNamespace("plumber"))(pr, dir = root, path = "/")
}
pr$run(host = "127.0.0.1", port = port, swagger = FALSE, quiet = TRUE)
