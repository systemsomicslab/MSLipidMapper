#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: mslipidmapper-api-worker.R JOB_DIRECTORY")
MSLipidMapper:::.mslm_api_worker(normalizePath(args[1], mustWork = TRUE))
