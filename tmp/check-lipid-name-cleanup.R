source("R/load_lipidomics_se.R")
row_data <- data.frame(check.names = FALSE, "Metabolite name" = c("SHexCer 36:1;O2|SHexCer 18:1;O2/18:0", "PC 16:0/18:1", NA_character_))
for (expression in as.list(body(load_lipidomics_se))[-1]) {
  code <- paste(deparse(expression), collapse = " ")
  if (startsWith(code, "name_cols <-") || startsWith(code, "for (name_col")) eval(expression)
}
stopifnot(identical(row_data[["Metabolite name"]], c("SHexCer 18:1;O2/18:0", "PC 16:0/18:1", NA_character_)))
cat("Name cleanup verified\n")