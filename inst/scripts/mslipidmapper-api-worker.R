#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: mslipidmapper-api-worker.R JOB_DIRECTORY")
# Load the installed package explicitly before resolving the worker.
suppressPackageStartupMessages(library(MSLipidMapper))
worker <- getFromNamespace(".mslm_api_worker", "MSLipidMapper")
worker(normalizePath(args[1], mustWork = TRUE))
