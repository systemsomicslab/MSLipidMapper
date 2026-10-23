from pathlib import Path
p=Path('R/Mod_cyto.R')
s=p.read_bytes()
start=s.index(b'mod_cyto_ui <- function(id) {')
end=s.index(b'mod_cyto_server <- function(id,',start)
replacement=Path('tmp/pathway_ui_replacement.R').read_bytes().replace(b'\r\n',b'\n').rstrip()+b'\n\n'
p.write_bytes(s[:start]+replacement+s[end:])
