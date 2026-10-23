from pathlib import Path
import re
p=Path('R/Mod_cyto.R')
s=p.read_bytes().decode('utf-8')
# Keep draft selections inside renderUI/isolate() and action inputs intact.
names=['plot_type','include_multiomics','agg_fun','hm_top_n','diff_group_a','diff_group_b','diff_method','diff_use_padj','diff_alpha','fc_scale','fc_top_n','chain_codes','chain_require_all','chain_exclude_odd','chain_carbon','chain_db','chain_oxygen']
for name in names:
    s=re.sub(r'(?<!isolate\()input\$'+name+r'\b', '.cfg("'+name+'")', s)
# The onFlushed initializer also uses the applied setting, not the draft UI.
s=s.replace('shiny::isolate(input$include_multiomics)', 'shiny::isolate(.cfg("include_multiomics"))')
p.write_bytes(s.encode('utf-8'))
