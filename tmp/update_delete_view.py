from pathlib import Path
p=Path('R/Mod_cyto.R');s=p.read_text(encoding='utf-8')
a=s.index('        shiny::tags$details(class = "mslm-pathway-save",')
b=s.index('      shiny::uiOutput(ns("settings_summary")),',a)
block=s[a:b]
block=block.replace('"PDF with panels / popups")))),','"PDF with panels / popups"))),')
s=s[:a]+'        NULL),\n'+s[b:]
anchor='        shiny::actionButton(ns("map_local_svgs"), "Pathway Mapping", class = "btn-primary btn-sm"),\n'
s=s.replace(anchor,anchor+block)
s=s.replace('    # ---- Deletion sync: JS -> Shiny ------------------------------------------', '''    # Non-reactive mirror of the graph already displayed by the client. Updating
    # stored elements after a client deletion must not rebuild that same graph.
    client_graph <- new.env(parent = emptyenv())
    client_graph$id <- NULL
    client_graph$elements <- NULL

    # ---- Deletion sync: JS -> Shiny ------------------------------------------''')
s=s.replace('        info$elements_work <- new_el\n        rv_nets$map[[id]] <- info', '        info$elements_work <- new_el\n        client_graph$id <- id\n        client_graph$elements <- new_el\n        rv_nets$map[[id]] <- info')
s=s.replace('      req(el)\n\n      use_preset', '''      req(el)
      if (identical(client_graph$id, id) && identical(client_graph$elements, el)) return()
      client_graph$id <- id
      client_graph$elements <- el

      use_preset''')
p.write_text(s,encoding='utf-8')
