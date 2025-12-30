# R/mod_utility_pathway_editor.R
# ======================================================================
# Utility module: Pathway editor (Cytoscape editing)
#   - Add/Delete nodes/edges
#   - Insert junction node on edge (add-node mode)
#   - Right click popup for label / Ensembl_ID / Width / Height / LipidMAPS_ID
#   - Fit / Reset layout / Align / Export .cyjs
#   - Import existing network file (.cyjs/.json/.gml/.gpml/.xml)  [REPLACE ONLY]
#
# NOTE:
# - DT table output & sync are REMOVED
# - Network MERGE import is REMOVED (replace only)
# - GML/GPML/CYJS parsing uses cyto_utils.R functions:
#     .cyjs_to_elements(), .parse_gml_yed(), .parse_gpml(),
#     .ensure_positions_for_preset(), .any_node_has_pos()
# - (No endpoints; do not touch path/pathpng)
# ======================================================================

#' @import shiny
NULL

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------- helpers (R) ------------------------------------------

.utilpe_as_elements_empty <- function() list(nodes = list(), edges = list())

.utilpe_stop <- function(...) stop(paste0(...), call. = FALSE)

.utilpe_require_cyto_utils <- function() {
  req_funs <- c(".cyjs_to_elements", ".parse_gml_yed", ".parse_gpml",
                ".ensure_positions_for_preset", ".any_node_has_pos")
  miss <- req_funs[!vapply(req_funs, exists, logical(1), mode = "function", inherits = TRUE)]
  if (length(miss)) {
    .utilpe_stop(
      "Required functions not found: ", paste(miss, collapse = ", "),
      "\nPlease source('R/cyto_utils.R') before using this module."
    )
  }
  invisible(TRUE)
}

.utilpe_read_network_file <- function(path) {
  .utilpe_require_cyto_utils()
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("cyjs", "json")) {
    obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    el  <- .cyjs_to_elements(obj)
    el  <- .ensure_positions_for_preset(el)
    return(el)
  }
  
  if (ext %in% c("gml")) {
    el <- .parse_gml_yed(path)
    el <- .ensure_positions_for_preset(el)
    return(el)
  }
  
  if (ext %in% c("gpml", "xml")) {
    el <- .parse_gpml(path)
    el <- .ensure_positions_for_preset(el)
    return(el)
  }
  
  .utilpe_stop("Unsupported file extension: .", ext, " (supported: .cyjs .json .gml .gpml/.xml)")
}

.utilpe_normalize_elements <- function(el) {
  if (is.null(el)) return(.utilpe_as_elements_empty())
  if (is.null(el$nodes)) el$nodes <- list()
  if (is.null(el$edges)) el$edges <- list()
  
  # nodes
  el$nodes <- lapply(el$nodes, function(n) {
    d <- n$data %||% list()
    if (is.null(d$id) || !nzchar(as.character(d$id))) d$id <- paste0("n", sample.int(1e9, 1))
    lab <- d$label %||% d$name %||% d$shared_name %||% d$id
    d$label       <- as.character(lab)
    d$name        <- as.character(d$name %||% lab)
    d$shared_name <- as.character(d$shared_name %||% lab)
    
    if (is.null(d$Color))       d$Color <- "Blue"
    if (is.null(d$Label_size))  d$Label_size <- 18
    if (is.null(d$Height))      d$Height <- 140
    if (is.null(d$Width))       d$Width  <- 140
    if (is.null(d$BorderWidth)) d$BorderWidth <- 1
    if (is.null(d$Ensembl_ID))  d$Ensembl_ID <- ""
    if (is.null(d$IsJunction))  d$IsJunction <- 0
    
    # future field
    if (is.null(d$LipidMAPS_ID)) d$LipidMAPS_ID <- ""
    
    n$data <- d
    n
  })
  
  # edges
  el$edges <- Filter(Negate(is.null), lapply(seq_along(el$edges), function(i) {
    e <- el$edges[[i]]
    d <- e$data %||% list()
    if (is.null(d$source) || is.null(d$target)) return(NULL)
    if (is.null(d$id) || !nzchar(as.character(d$id))) d$id <- paste0("e", i)
    if (is.null(d$label)) d$label <- ""
    e$data <- d
    e
  }))
  
  el
}

#' Pathway editor UI
#'
#' @param id module id
#' @param title panel title
#' @param cy_height cytoscape height (px)
#'
#' @export
mod_utility_pathway_editor_ui <- function(
    id,
    title = "Pathway editor",
    cy_height = 800
) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "https://unpkg.com/cytoscape@3.26.0/dist/cytoscape.min.js"),
        shiny::tags$style(shiny::HTML(sprintf("
/* scoped by wrapper */
#%s { position: relative; }
#%s .cy-edge-add-source { border-color:#e11d48 !important; border-width:3px !important; border-style:solid !important; }

/* popup */
#%s .cy-popup{
  position:absolute; display:block; background:white; border:1px solid #aaa;
  padding:8px 10px 10px 10px; box-shadow:0 2px 6px rgba(0,0,0,0.3);
  min-width:240px; max-width:420px; border-radius:6px; z-index:20000;
  pointer-events:auto; font-size:11px;
}
#%s .cy-popup-header{ display:flex; justify-content:space-between; align-items:center; margin-bottom:6px; cursor:move; user-select:none; }
#%s .cy-popup-title{ font-weight:600; font-size:12px; }
#%s .cy-popup-close{ border:none; background:transparent; font-size:14px; cursor:pointer; line-height:1; }
#%s .cy-popup label{ display:block; margin-bottom:3px; }
#%s .cy-popup input{ width:100%%; box-sizing:border-box; font-size:11px; padding:2px 4px; margin-bottom:6px; }
#%s .cy-popup button{ font-size:11px; padding:2px 8px; }
#%s .cy-popup .cy-grid{ display:grid; grid-template-columns:1fr 1fr; gap:8px; }
#%s .cy-popup .cy-grid > div{ min-width:0; }
        ",
ns("wrap"), ns("wrap"),
ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"),
ns("wrap"), ns("wrap")
        ))),

shiny::tags$script(shiny::HTML("
(function(){
  if (!window.__utilpe) window.__utilpe = { instances:{}, state:{}, installed:false };
  var U = window.__utilpe;

  function getState(container_id){
    if (!U.state[container_id]) {
      U.state[container_id] = {
        editMode: 'move',
        edgeAddSourceId: null,
        nodeCounter: 0,
        edgeCounter: 0
      };
    }
    return U.state[container_id];
  }

  function isNonEmptyEns(val){
    if (val == null) return false;
    var s = String(val).trim();
    if (!s) return false;
    if (s.toUpperCase() === 'NA') return false;
    return true;
  }

  function midpoint(edge){
    var s = edge.source().position();
    var t = edge.target().position();
    return { x: (s.x + t.x)/2.0, y: (s.y + t.y)/2.0 };
  }

  function removePopups(containerEl){
    if (!containerEl) return;
    containerEl.querySelectorAll('.cy-popup').forEach(function(el){
      if (el && el.parentNode) el.parentNode.removeChild(el);
    });
  }

  function escAttr(s){
    return String(s)
      .replace(/&/g,'&amp;')
      .replace(/</g,'&lt;')
      .replace(/>/g,'&gt;')
      .replace(/\\\"/g,'&quot;')
      .replace(/'/g,'&#39;');
  }

  function clearEdgeAddSource(container_id){
    var cy = U.instances[container_id];
    var st = getState(container_id);
    if (!cy) { st.edgeAddSourceId = null; return; }
    if (st.edgeAddSourceId){
      var n = cy.getElementById(String(st.edgeAddSourceId));
      if (n && n.length) n.removeClass('cy-edge-add-source');
    }
    st.edgeAddSourceId = null;
  }

  function parseIntStrict(x){
    if (x == null) return null;
    var s = String(x).trim();
    if (!s) return null;
    if (!/^\\d+$/.test(s)) return null;
    return parseInt(s, 10);
  }

  function parseEdgeSuffix(id){
    if (id == null) return null;
    var s = String(id).trim();
    var m = s.match(/^e(\\d+)$/);
    if (m) return parseInt(m[1], 10);
    if (/^\\d+$/.test(s)) return parseInt(s, 10);
    return null;
  }

  function asFiniteNumber(x, fallback){
    if (x == null) return fallback;
    var v = parseFloat(String(x).trim());
    if (!isFinite(v)) return fallback;
    return v;
  }

  function clamp(v, lo, hi){
    if (!isFinite(v)) return lo;
    if (v < lo) return lo;
    if (v > hi) return hi;
    return v;
  }

  function initCountersFromExisting(cy, st){
    var maxNode = 0;
    cy.nodes().forEach(function(n){
      var d = n.data() || {};
      var v1 = parseIntStrict(d.SUID);
      var v2 = parseIntStrict(d.id || n.id());
      if (v1 != null && v1 > maxNode) maxNode = v1;
      if (v2 != null && v2 > maxNode) maxNode = v2;
    });

    var maxEdge = 0;
    cy.edges().forEach(function(e){
      var d = e.data() || {};
      var v = parseEdgeSuffix(d.id || e.id());
      if (v != null && v > maxEdge) maxEdge = v;
    });

    st.nodeCounter = maxNode;
    st.edgeCounter = maxEdge;
    st.edgeAddSourceId = null;
  }

  function nextNodeId(cy, st){
    while (true) {
      st.nodeCounter += 1;
      var idStr = String(st.nodeCounter);
      if (!cy.getElementById(idStr).length) return idStr;
    }
  }

  function nextEdgeId(cy, st){
    while (true) {
      st.edgeCounter += 1;
      var idStr = 'e' + String(st.edgeCounter);
      if (!cy.getElementById(idStr).length) return idStr;
    }
  }

  if (U.installed) return;
  U.installed = true;

  Shiny.addCustomMessageHandler('utilpe_initCy', function(msg){
    var container_id = msg.container_id;
    var el = document.getElementById(container_id);
    if (!el) return;

    removePopups(el);

    if (U.instances[container_id]) {
      try { U.instances[container_id].destroy(); } catch(e){}
      delete U.instances[container_id];
    }

    var st = getState(container_id);
    st.editMode = msg.init_mode || st.editMode || 'move';
    st.edgeAddSourceId = null;

    var cy = cytoscape({
      container: el,
      elements: msg.elements || { nodes: [], edges: [] },

      // zoom sensitivity: smaller = less sensitive
      wheelSensitivity: 0.15,
      minZoom: 0.03,
      maxZoom: 6,

      style: [
        { selector:'node', style:{
          'shape':'rectangle',
          'background-color':'white',
          'border-width':'data(BorderWidth)',
          'border-color':'data(Color)',
          'border-style':'solid',
          'width':'data(Width)',
          'height':'data(Height)',
          'label':'data(label)',
          'font-size':'data(Label_size)',
          'text-valign':'top',
          'text-halign':'center',
          'text-margin-y':22
        }},
        { selector:'node[IsJunction = 1]', style:{
          'shape':'ellipse',
          'width':24,
          'height':24,
          'label':'',
          'border-width':2,
          'border-color':'#666',
          'background-color':'white'
        }},
        { selector:'node:selected', style:{ 'border-width':4, 'border-color':'#e11d48' }},
        { selector:'edge', style:{
          'width':2,
          'line-color':'#888',
          'target-arrow-color':'#888',
          'target-arrow-shape':'triangle',
          'curve-style':'bezier',
          'arrow-scale':1.5,
          'label':'data(label)',
          'font-size':8
        }},
        { selector:'edge[edgeType = \"toJunction\"]', style:{ 'target-arrow-shape':'none' } }
      ],
      layout: { name:'grid' }
    });

    U.instances[container_id] = cy;
    initCountersFromExisting(cy, st);

    cy.on('tap', function(evt){
      var mode = st.editMode || 'move';
      var target = evt.target;

      if (mode === 'move') return;

      if (mode === 'add-node') {
        if (target === cy) {
          var idStr = nextNodeId(cy, st);
          var suid  = parseInt(idStr, 10);
          var initLabel = idStr;

          cy.add({
            group:'nodes',
            data:{
              id: idStr, SUID: suid,
              Color:'Blue',
              label:initLabel, Label_size:18,
              shared_name:initLabel, name:initLabel,
              Ensembl_ID:'',
              LipidMAPS_ID:'',
              Height:140, Width:140, BorderWidth:1,
              IsJunction:0
            },
            position: evt.position
          });
        }
        return;
      }

      if (mode === 'delete') {
        if (target !== cy) {
          if (target.isNode && target.isNode() && st.edgeAddSourceId && String(st.edgeAddSourceId) === String(target.id())) {
            clearEdgeAddSource(container_id);
          }
          target.remove();
        }
        return;
      }
    });

    // edge tap (add-node mode): insert junction
    cy.on('tap', 'edge', function(evt){
      if (st.editMode !== 'add-node') return;

      var edge = evt.target;
      var mid  = midpoint(edge);
      var data = edge.data() || {};

      cy.batch(function(){
        var newId = nextNodeId(cy, st);
        var suid  = parseInt(newId, 10);
        var label = newId;

        var newNode = cy.add({
          group:'nodes',
          data:{
            id:newId, SUID:suid,
            Color:'Blue',
            label:label, Label_size:12,
            shared_name:label, name:label,
            Ensembl_ID:'',
            LipidMAPS_ID:'',
            Height:140, Width:140, BorderWidth:1,
            IsJunction:1
          },
          position: mid
        });

        var src = data.source;
        var tgt = data.target;
        var oldLabel = data.label || '';

        edge.remove();

        var e1 = nextEdgeId(cy, st);
        var e2 = nextEdgeId(cy, st);

        cy.add({ group:'edges', data:{
          id: e1, source: src, target: newNode.id(),
          label: oldLabel, edgeType: 'toJunction'
        }});
        cy.add({ group:'edges', data:{
          id: e2, source: newNode.id(), target: tgt, label: ''
        }});
      });
    });

    // node tap (add-edge mode)
    cy.on('tap', 'node', function(evt){
      if (st.editMode !== 'add-edge') return;

      var node = evt.target;
      if (st.edgeAddSourceId && !cy.getElementById(String(st.edgeAddSourceId)).length) {
        st.edgeAddSourceId = null;
      }

      if (!st.edgeAddSourceId) {
        st.edgeAddSourceId = node.id();
        node.addClass('cy-edge-add-source');
        return;
      }

      if (String(st.edgeAddSourceId) === String(node.id())) {
        clearEdgeAddSource(container_id);
        return;
      }

      var srcNode = cy.getElementById(String(st.edgeAddSourceId));
      if (srcNode && srcNode.length) srcNode.removeClass('cy-edge-add-source');

      var newEdgeId = nextEdgeId(cy, st);
      var edgeData  = { id:newEdgeId, source:String(st.edgeAddSourceId), target:node.id(), label:'' };

      var isJunc = node.data('IsJunction');
      if (isJunc === 1 || isJunc === '1') edgeData.edgeType = 'toJunction';

      cy.add({ group:'edges', data: edgeData });
      st.edgeAddSourceId = null;
    });

    // right click popup edit: label / Ensembl_ID / Width / Height / LipidMAPS_ID
    cy.on('cxttap', 'node', function(evt){
      var node = evt.target;
      var pos  = node.renderedPosition();
      var data = node.data() || {};

      var nameVal = data.label || data.name || node.id();
      var ensVal  = data.Ensembl_ID || '';
      var lmVal   = data.LipidMAPS_ID || '';

      // Width/Height: junction is fixed size in style, but we still allow editing for future use.
      var wVal = (data.Width != null ? data.Width : 140);
      var hVal = (data.Height != null ? data.Height : 140);

      removePopups(el);

      var pop = document.createElement('div');
      pop.className = 'cy-popup';
      pop.innerHTML =
        '<div class=\"cy-popup-header\">' +
          '<span class=\"cy-popup-title\">Edit node: ' + escAttr(node.id()) + '</span>' +
          '<button type=\"button\" class=\"cy-popup-close\">&#x2715;</button>' +
        '</div>' +
        '<div>' +
          '<label>Name<br><input type=\"text\" class=\"cy-meta-name\" value=\"' + escAttr(nameVal) + '\"></label>' +
          '<label>Ensembl_ID<br><input type=\"text\" class=\"cy-meta-ens\" value=\"' + escAttr(ensVal) + '\"></label>' +
          '<label>LipidMAPS ID<br><input type=\"text\" class=\"cy-meta-lm\" value=\"' + escAttr(lmVal) + '\"></label>' +
          '<div class=\"cy-grid\">' +
            '<div><label>Width<br><input type=\"number\" class=\"cy-meta-w\" value=\"' + escAttr(wVal) + '\" step=\"1\" min=\"10\"></label></div>' +
            '<div><label>Height<br><input type=\"number\" class=\"cy-meta-h\" value=\"' + escAttr(hVal) + '\" step=\"1\" min=\"10\"></label></div>' +
          '</div>' +
          '<div style=\"text-align:right; margin-top:6px;\"><button type=\"button\" class=\"cy-meta-save\">Save</button></div>' +
        '</div>';

      pop.style.left = (pos.x + 20) + 'px';
      pop.style.top  = (pos.y + 20) + 'px';
      el.appendChild(pop);

      ['mousedown','click','dblclick','wheel','touchstart','touchmove','touchend','pointerdown'].forEach(function(evName){
        pop.addEventListener(evName, function(e){ e.stopPropagation(); });
      });

      var closeBtn = pop.querySelector('.cy-popup-close');
      if (closeBtn){
        closeBtn.onclick = function(e){
          e.stopPropagation();
          if (pop && pop.parentNode) pop.parentNode.removeChild(pop);
        };
      }

      // drag
      var header = pop.querySelector('.cy-popup-header');
      if (header){
        var dragging=false, startX=0, startY=0, startLeft=0, startTop=0;
        header.addEventListener('mousedown', function(e){
          dragging = true;
          e.stopPropagation(); e.preventDefault();
          var rect  = pop.getBoundingClientRect();
          var crect = el.getBoundingClientRect();
          startX = e.clientX; startY = e.clientY;
          startLeft = rect.left - crect.left;
          startTop  = rect.top  - crect.top;

          function onMove(ev){
            if (!dragging) return;
            pop.style.left = (startLeft + (ev.clientX - startX)) + 'px';
            pop.style.top  = (startTop  + (ev.clientY - startY)) + 'px';
          }
          function onUp(){
            dragging=false;
            document.removeEventListener('mousemove', onMove);
            document.removeEventListener('mouseup', onUp);
          }
          document.addEventListener('mousemove', onMove);
          document.addEventListener('mouseup', onUp);
        });
      }

      var saveBtn = pop.querySelector('.cy-meta-save');
      if (saveBtn){
        saveBtn.onclick = function(e){
          e.stopPropagation(); e.preventDefault();

          var inpName = pop.querySelector('.cy-meta-name');
          var inpEns  = pop.querySelector('.cy-meta-ens');
          var inpLM   = pop.querySelector('.cy-meta-lm');
          var inpW    = pop.querySelector('.cy-meta-w');
          var inpH    = pop.querySelector('.cy-meta-h');

          var newName = inpName ? inpName.value : nameVal;
          var newEns  = inpEns  ? inpEns.value  : ensVal;
          var newLM   = inpLM   ? inpLM.value   : lmVal;

          // clamp to reasonable range to avoid zero-size nodes
          var newW = asFiniteNumber(inpW ? inpW.value : wVal, 140);
          var newH = asFiniteNumber(inpH ? inpH.value : hVal, 140);
          newW = clamp(newW, 10, 2000);
          newH = clamp(newH, 10, 2000);

          node.data('label', newName);
          node.data('name',  newName);
          node.data('shared_name', newName);

          node.data('Ensembl_ID', newEns);
          node.data('LipidMAPS_ID', newLM);

          // apply width/height to data (style reads data(Width/Height) for normal nodes)
          node.data('Width',  newW);
          node.data('Height', newH);

          // Ensembl coloring rule (keep existing behavior)
          if (isNonEmptyEns(newEns)) node.data('Color','Black'); else node.data('Color','Blue');

          if (pop && pop.parentNode) pop.parentNode.removeChild(pop);
        };
      }
    });
  });

  Shiny.addCustomMessageHandler('utilpe_loadElementsReplace', function(msg){
    var cy = U.instances[msg.container_id];
    if (!cy) return;

    var st = getState(msg.container_id);
    var el = document.getElementById(msg.container_id);
    if (el) removePopups(el);
    clearEdgeAddSource(msg.container_id);

    var incoming = msg.elements || {nodes:[], edges:[]};
    var inNodes = incoming.nodes || [];
    var inEdges = incoming.edges || [];

    cy.json({ elements: { nodes: inNodes, edges: inEdges } });
    initCountersFromExisting(cy, st);

    if (msg.fit) { try { cy.fit(); cy.center(); } catch(e){} }
  });

  Shiny.addCustomMessageHandler('utilpe_setMode', function(msg){
    var st = getState(msg.container_id);
    st.editMode = msg.mode || 'move';
    clearEdgeAddSource(msg.container_id);
  });

  Shiny.addCustomMessageHandler('utilpe_resetLayout', function(msg){
    var cy = U.instances[msg.container_id];
    if (!cy) return;
    cy.layout({ name:'cose' }).run();
  });

  Shiny.addCustomMessageHandler('utilpe_fitView', function(msg){
    var cy = U.instances[msg.container_id];
    if (!cy) return;
    cy.fit(); cy.center();
  });

  Shiny.addCustomMessageHandler('utilpe_alignSelected', function(msg){
    var cy = U.instances[msg.container_id];
    if (!cy) return;
    var mode  = msg.mode || 'horizontal';
    var nodes = cy.nodes(':selected');
    if (nodes.length < 2) return;

    cy.batch(function(){
      if (mode === 'horizontal') {
        var sumY = 0; nodes.forEach(function(n){ sumY += n.position('y'); });
        var y0 = sumY / nodes.length;
        nodes.forEach(function(n){ var p=n.position(); n.position({x:p.x, y:y0}); });
      } else {
        var sumX = 0; nodes.forEach(function(n){ sumX += n.position('x'); });
        var x0 = sumX / nodes.length;
        nodes.forEach(function(n){ var p=n.position(); n.position({x:x0, y:p.y}); });
      }
    });
  });

  Shiny.addCustomMessageHandler('utilpe_exportCy', function(msg){
    var cy = U.instances[msg.container_id];
    if (!cy) return;
    var cyjs = cy.json();
    var blob = new Blob([JSON.stringify(cyjs, null, 2)], { type:'application/json;charset=utf-8' });
    var a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = (msg.filename || 'pathway.cyjs');
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    setTimeout(function(){ URL.revokeObjectURL(a.href); }, 0);
  });

})();
        "))
      )
    ),

shiny::div(
  id = ns("wrap"),
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::wellPanel(
        shiny::h4(title),
        
        shiny::h4("Import network"),
        shiny::fileInput(
          ns("netfile"),
          label = "Upload (.cyjs/.gml)",
          accept = c(".cyjs",".json",".gml",".gpml",".xml")
        ),
        shiny::checkboxInput(ns("import_fit"), "Fit after import", value = TRUE),
        shiny::actionButton(ns("import_apply"), "Load file (replace)", class = "btn btn-default btn-xs"),
        shiny::tags$hr(),
        
        shiny::h4("Edit mode"),
        shiny::radioButtons(
          ns("edit_mode"),
          label = NULL,
          choices = c(
            "Move (drag only)" = "move",
            "Add node"         = "add-node",
            "Add edge"         = "add-edge",
            "Delete"           = "delete"
          ),
          selected = "move"
        ),
        shiny::tags$hr(),
        
        shiny::h4("Layout / Export / Align"),
        shiny::actionButton(ns("reset_layout"), "Reset layout (cose)", class = "btn btn-default btn-xs"),
        shiny::tags$br(), shiny::tags$br(),
        shiny::actionButton(ns("fit_view"), "Fit to view", class = "btn btn-default btn-xs"),
        shiny::tags$br(), shiny::tags$br(),
        shiny::actionButton(ns("align_h"), "Align selected: Horizontal", class = "btn btn-default btn-xs"),
        shiny::tags$br(), shiny::tags$br(),
        shiny::actionButton(ns("align_v"), "Align selected: Vertical", class = "btn btn-default btn-xs"),
        shiny::tags$hr(),
        
        shiny::h4("Export (.cyjs)"),
        shiny::textInput(ns("export_filename"), "File name", value = "pathway.cyjs"),
        shiny::actionButton(ns("export_cyjs"), "Export as .cyjs", class = "btn btn-primary btn-xs"),
        
        shiny::tags$hr()
      )
    ),
    shiny::column(
      width = 9,
      shiny::div(
        id = ns("cy"),
        style = sprintf(
          "height:%dpx; border:1px solid #e5e7eb; border-radius:12px; margin-bottom:10px; background:white;",
          as.integer(cy_height)
        )
      )
    )
  )
)
  )
}

#' Pathway editor server
#'
#' @param id module id
#' @param elements reactive/list for initial cytoscape elements (list(nodes=..., edges=...)).
#'        - NULL/empty -> empty network.
#' @param export_filename default filename (used only as fallback)
#'
#' @return list(fit, reset_layout, export_cyjs, set_mode, load_file)
#' @export
mod_utility_pathway_editor_server <- function(
    id,
    elements = NULL,
    export_filename = "pathway.cyjs"
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    .utilpe_require_cyto_utils()
    
    ns <- session$ns
    container_id <- ns("cy")
    
    .send_init <- function(elm) {
      if (is.null(elm)) elm <- .utilpe_as_elements_empty()
      elm <- .utilpe_normalize_elements(elm)
      session$sendCustomMessage(
        "utilpe_initCy",
        list(
          container_id = container_id,
          elements     = elm,
          init_mode    = input$edit_mode %||% "move"
        )
      )
    }
    
    if (shiny::is.reactive(elements)) {
      shiny::observeEvent(elements(), {
        .send_init(elements())
      }, ignoreInit = FALSE)
    } else {
      shiny::observeEvent(TRUE, {
        .send_init(elements)
      }, once = TRUE)
    }
    
    shiny::observeEvent(input$edit_mode, {
      session$sendCustomMessage("utilpe_setMode", list(container_id = container_id, mode = input$edit_mode))
    }, ignoreInit = FALSE)
    
    shiny::observeEvent(input$reset_layout, {
      session$sendCustomMessage("utilpe_resetLayout", list(container_id = container_id))
    })
    shiny::observeEvent(input$fit_view, {
      session$sendCustomMessage("utilpe_fitView", list(container_id = container_id))
    })
    shiny::observeEvent(input$align_h, {
      session$sendCustomMessage("utilpe_alignSelected", list(container_id = container_id, mode = "horizontal"))
    })
    shiny::observeEvent(input$align_v, {
      session$sendCustomMessage("utilpe_alignSelected", list(container_id = container_id, mode = "vertical"))
    })
    
    shiny::observeEvent(input$export_cyjs, {
      fn <- input$export_filename %||% export_filename
      fn <- trimws(as.character(fn))
      if (!nzchar(fn)) fn <- export_filename
      if (!grepl("\\.cyjs$", fn, ignore.case = TRUE)) fn <- paste0(fn, ".cyjs")
      session$sendCustomMessage("utilpe_exportCy", list(container_id = container_id, filename = fn))
    })
    
    shiny::observeEvent(input$import_apply, {
      shiny::req(input$netfile)
      path <- input$netfile$datapath
      shiny::req(file.exists(path))
      
      elm <- .utilpe_read_network_file(path)
      elm <- .utilpe_normalize_elements(elm)
      
      fit <- isTRUE(input$import_fit)
      
      session$sendCustomMessage(
        "utilpe_loadElementsReplace",
        list(container_id = container_id, elements = elm, fit = fit)
      )
    })
    
    list(
      fit = function() session$sendCustomMessage("utilpe_fitView", list(container_id = container_id)),
      reset_layout = function() session$sendCustomMessage("utilpe_resetLayout", list(container_id = container_id)),
      export_cyjs = function(filename = export_filename) session$sendCustomMessage("utilpe_exportCy", list(container_id = container_id, filename = filename)),
      set_mode = function(mode = "move") session$sendCustomMessage("utilpe_setMode", list(container_id = container_id, mode = mode)),
      load_file = function(filepath, fit = TRUE) {
        elm <- .utilpe_read_network_file(filepath)
        elm <- .utilpe_normalize_elements(elm)
        session$sendCustomMessage("utilpe_loadElementsReplace", list(container_id = container_id, elements = elm, fit = isTRUE(fit)))
      }
    )
  })
}
