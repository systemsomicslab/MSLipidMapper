#!/usr/bin/env Rscript

status <- tryCatch(
  MSLipidMapper::mslipidmapper_cli(commandArgs(trailingOnly = TRUE)),
  error = function(e) {
    message("MSLipidMapper error: ", conditionMessage(e))
    1L
  }
)
quit(status = status, save = "no")
