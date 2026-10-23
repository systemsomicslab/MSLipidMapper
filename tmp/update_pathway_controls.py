from pathlib import Path
p=Path('R/Mod_cyto.R');s=p.read_text(encoding='utf-8')
button='        shiny::actionButton(ns("map_local_svgs"), "Pathway Mapping", class = "btn-primary"),\n'
s=s.replace(button,'').replace('        shiny::actionButton(ns("fit"),',button.replace('btn-primary','btn-primary btn-sm')+'        shiny::actionButton(ns("fit"),')
s=s.replace('            shiny::checkboxInput(ns("map_edit_enabled"), "Enable node / edge deletion", FALSE),\n            shiny::conditionalPanel(condition = "input.map_edit_enabled", ns = ns,\n              shiny::actionButton(ns("delete_selected"), "Delete selected elements", class = "btn-danger btn-sm")),\n            shiny::helpText("When enabled: select elements and press Delete, or Ctrl + right-click an element."),', '            shiny::actionButton(ns("delete_selected"), "Delete selected elements", class = "btn-danger btn-sm"),\n            shiny::helpText("Ctrl + right-click a node or edge to delete it. You can also select elements and press Delete."),')
s=s.replace('      req(isTRUE(input$map_edit_enabled))\n','').replace('allow_delete      = isTRUE(isolate(input$map_edit_enabled))','allow_delete      = TRUE')
s=s.replace('    observeEvent(input$map_edit_enabled, {\n      session$sendCustomMessage("cy_edit_mode", list(container_id = container_id,\n        enabled = isTRUE(input$map_edit_enabled)))\n    }, ignoreInit = FALSE)\n','')
s=s.replace('      applied_settings(next_settings)\n      .invalidate_projection()', '      applied_settings(next_settings)\n      shiny::updateCheckboxInput(session, "diff_borders_enabled", value = FALSE)\n      .invalidate_projection()')
s=s.replace('click or right-click a node', 'click a node').replace('corners to resize.")','corners to resize. Ctrl + right-click a node or edge to delete it.")')
p.write_text(s,encoding='utf-8')
p=Path('R/cyto_js_deps.R');s=p.read_text(encoding='utf-8')
a=s.index('      cy.on("cxttap", "node,edge", function(evt){')
b=s.index('    })();\n\n    (function(){',a)
s=s[:a]+'''      cy.on("cxttap", "node,edge", function(evt){
        const oe = evt.originalEvent;
        if (oe && oe.preventDefault) oe.preventDefault();
        if (oe && (oe.ctrlKey || oe.metaKey)) {
          // Delete the targeted element, rather than other selected elements.
          cy.elements().unselect();
          evt.target.select();
          deleteSelected(id, {fit_after:false});
        }
      });
'''+s[b:]
p.write_text(s,encoding='utf-8')
p=Path('inst/app/www/vendor/mslm-fc-panels.js');s=p.read_text(encoding='utf-8').replace('Click or right-click a node.','Click a node.');p.write_text(s,encoding='utf-8')
