from pathlib import Path
p=Path('R/Mod_cyto.R')
s=p.read_bytes().replace(b'mod_cyto_ui <- function(id) {\r\n  ns <- shiny::NS(id)\r\n', b'mod_cyto_ui <- function(id) {\n  ns <- shiny::NS(id)\n')
p.write_bytes(s)
