# R/cyto_js_deps.R -------------------------------------------------------------
# JS/CSS dependencies + custom message handlers for Cytoscape module.
# NOTE:
# - "Processing..." overlay is shown ONLY when:
#     (1) R explicitly calls cy_loading(on=TRUE), OR
#     (2) cy_set_node_images payload has show_loading === true
# - Network switching / cy_init NEVER triggers the overlay.
#
# Handlers:
# - cy_init
# - cy_fit
# - cy_resize                 (NEW; safe for mod_cyto.R)
# - cy_set_node_images
# - cy_loading
# - cy_set_node_borders       (border-color changes; existing)
# - cy_set_node_borderstyle   (NEW; double border highlight; no color change)
# - cy_set_node_meta          (NEW; set node.data(enrich_html) etc.)
# - cy_delete_selected
# - cy_push_cyjs
# - cy_export_pdf
# - cy_export_pdf_with_popups

suppressPackageStartupMessages({
  library(shiny)
})

cyto_js_deps <- function(container_id_css) {
  shiny::tagList(
    shiny::tags$script(src = "https://unpkg.com/cytoscape@3.26.0/dist/cytoscape.min.js"),
    shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"),
    shiny::tags$script(src = "https://unpkg.com/html2canvas@1.4.1/dist/html2canvas.min.js"),
    
    shiny::tags$style(shiny::HTML(sprintf("
      #%s { position: relative; }

      .cy-loading{
        position:absolute; inset:0; display:flex; align-items:center; justify-content:center;
        background:rgba(255,255,255,0.6); backdrop-filter:blur(1px);
        z-index:30000;
        font:600 14px/1.4 system-ui,-apple-system,Segoe UI,Roboto,sans-serif;
        visibility:hidden; opacity:0; transition:opacity .2s ease; border-radius:12px;
        pointer-events:auto;
      }
      .cy-loading.show{ visibility:visible; opacity:1; }
      .cy-spinner{
        width:28px; height:28px; border:3px solid #e5e7eb; border-top-color:#4b5563; border-radius:50%%;
        animation:cyspin 1s linear infinite; margin-right:10px;
      }
      @keyframes cyspin { to { transform: rotate(360deg); } }

      .cy-popup{
        position:absolute;
        display:block;
        background:white;
        border:1px solid #aaa;
        padding:8px 10px 10px 10px;
        box-shadow:0 2px 6px rgba(0,0,0,0.3);
        min-width:240px;
        max-width:680px;
        min-height:160px;
        max-height:540px;
        resize: both;
        overflow:auto;
        border-radius:4px;
        z-index:20000;
        pointer-events:auto;
      }
      .cy-popup-header{
        display:flex;
        justify-content:space-between;
        align-items:center;
        margin-bottom:4px;
        cursor:move;
        user-select:none;
      }
      .cy-popup-title{ font-weight:600; font-size:13px; }
      .cy-popup-close{ border:none; background:transparent; font-size:16px; cursor:pointer; line-height:1; }
      .cy-popup-tabs{ display:flex; gap:6px; margin-bottom:6px; }
      .cy-tab-btn{
        flex:1 1 auto;
        border:1px solid #ccc;
        padding:3px 6px;
        font-size:11px;
        border-radius:4px;
        background-color:#f5f5f5;
        cursor:pointer;
      }
      .cy-tab-btn.active{ background-color:#2563eb; color:#fff; border-color:#1d4ed8; }
      .cy-tab-btn:disabled{ opacity:0.4; cursor:default; }
      .cy-popup-img-wrap{ max-height:360px; overflow:auto; }
      .cy-popup-img{ width:100%%; height:auto; display:block; }

      /* Enrichment HTML helpers (optional) */
      .cy-enrich{ font-size:12px; line-height:1.35; }
      .cy-enrich hr{ margin:8px 0; }
      .cy-enrich .muted{ opacity:.75; font-size:11px; }
      .cy-enrich .mono{ font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace; font-size:11px; }
    ", container_id_css))),
    
    shiny::tags$script(shiny::HTML('
(function(){
  if (!window.__cy_instances) window.__cy_instances = {};
  if (!window.__cy_state) window.__cy_state = {};

  function normalizeStyle(arr){
    if (!Array.isArray(arr)) return arr;
    return arr.map(function(it){
      if (it && it.css && !it.style) { it.style = it.css; delete it.css; }
      return it;
    });
  }

  function defaultStyle(){
    return [
      { selector: "node", style: {
          "border-width": "data(BorderWidth)",
          "border-color": "data(Color)",
          "border-style": "data(borderstyle)",
          "label": "data(label)", "background-color": "white", "shape": "rectangle",
          "background-image": "data(path)",
          "background-image-crossorigin": "anonymous",
          "background-fit": "cover cover",
          "height": "data(Height)", "width": "data(Width)",
          "font-size": "data(Label_size)", "text-valign": "top", "text-halign": "center", "text-margin-y": 22
      }},
      { selector: "node:selected", style: { "border-width": 3 } },
      { selector: "edge", style: {
          "width": 2, "line-color": "#888", "curve-style": "bezier",
          "target-arrow-shape": "triangle", "target-arrow-color": "#888", "arrow-scale": 1.5
      } }
    ];
  }

  function overlayEl(container_id){
    const el = document.getElementById(container_id);
    if (!el) return null;
    let ov = el.querySelector(".cy-loading");
    if (!ov){
      ov = document.createElement("div");
      ov.className = "cy-loading";
      ov.innerHTML = "<div class=\\"cy-spinner\\"></div><div>Processing...</div>";
      el.appendChild(ov);
    }
    return ov;
  }
  function showLoading(container_id){ const ov = overlayEl(container_id); if (ov) ov.classList.add("show"); }
  function hideLoading(container_id){ const ov = overlayEl(container_id); if (ov) ov.classList.remove("show"); }

  function withOverlayHidden(container_id, fn){
    const ov = overlayEl(container_id);
    const wasShown = ov ? ov.classList.contains("show") : false;
    if (ov) ov.classList.remove("show");

    let ret;
    try { ret = fn(); }
    catch(e){
      if (ov && wasShown) ov.classList.add("show");
      throw e;
    }

    if (ret && typeof ret.then === "function"){
      return ret.finally(function(){
        if (ov && wasShown) ov.classList.add("show");
      });
    } else {
      if (ov && wasShown) ov.classList.add("show");
      return ret;
    }
  }

  // ---- Deletion helpers ----------------------------------------------------
  function isTypingContext(){
    const ae = document.activeElement;
    if (!ae) return false;
    const tag = (ae.tagName || "").toLowerCase();
    if (tag === "input" || tag === "textarea" || tag === "select") return true;
    if (ae.isContentEditable) return true;
    return false;
  }

  function sendChanged(container_id, payload){
    const st = window.__cy_state[container_id] || {};
    const inId = st.changed_input;
    if (!inId || !window.Shiny) return;
    Shiny.setInputValue(inId, payload, {priority:"event"});
  }

  function deleteSelected(container_id, opts){
    opts = opts || {};
    const cy = window.__cy_instances[container_id];
    if (!cy) return;

    const sel = cy.$(":selected");
    if (!sel || sel.length === 0) return;

    const removedNodes = sel.nodes().map(n => n.id());
    const removedEdges = sel.edges().map(e => e.id());

    sel.remove();

    sendChanged(container_id, {
      action: "delete_selected",
      removed: { nodes: removedNodes, edges: removedEdges },
      counts: { nodes: cy.nodes().length, edges: cy.edges().length },
      ts: Date.now()
    });

    if (opts.fit_after){
      try{ cy.fit(); cy.center(); }catch(e){}
    }
  }

  function installDeleteHotkeys(container_id){
    const el = document.getElementById(container_id);
    if (!el) return;
    if (el.__cy_delete_hotkeys_installed) return;
    el.__cy_delete_hotkeys_installed = true;

    if (!el.hasAttribute("tabindex")) el.setAttribute("tabindex", "0");
    el.addEventListener("mousedown", function(){ try{ el.focus(); }catch(e){} });

    let lastInside = 0;
    el.addEventListener("mouseenter", () => { lastInside = Date.now(); });
    el.addEventListener("mousemove",  () => { lastInside = Date.now(); });

    document.addEventListener("keydown", function(e){
      if (isTypingContext()) return;

      const key = e.key;
      if (key !== "Delete" && key !== "Backspace") return;

      const active = document.activeElement;
      const focusedOnContainer = (active === el) || (active && el.contains(active));
      const recentlyInside = (Date.now() - lastInside) < 800;

      if (!focusedOnContainer && !recentlyInside) return;

      e.preventDefault();
      e.stopPropagation();

      deleteSelected(container_id, {fit_after:false});
    }, true);
  }

  // ---- Core ----------------------------------------------------------------
  function ensureCy(payload){
    const id = payload.container_id;
    const el = document.getElementById(id);
    if (!el) return null;

    window.__cy_state[id] = window.__cy_state[id] || {};
    window.__cy_state[id].selected_input = payload.selected_input || null;
    window.__cy_state[id].changed_input  = payload.changed_input  || null;
    window.__cy_state[id].allow_delete   = (payload.allow_delete !== false);

    // IMPORTANT: never auto-show loading on cy_init / network updates
    hideLoading(id);

    // remove popups
    el.querySelectorAll(".cy-popup").forEach(function(p){
      try{ p.remove(); }catch(e){}
    });

    // reset old instance
    if (window.__cy_instances[id]) {
      try{ window.__cy_instances[id].destroy(); }catch(e){}
      delete window.__cy_instances[id];
    }

    const style = normalizeStyle(payload.style) || defaultStyle();
    const cy = cytoscape({
      container: el,
      elements: payload.elements || { nodes: [], edges: [] },
      layout: { name: payload.layout || "cose" },
      style: style,
      wheelSensitivity: payload.wheelSensitivity || 0.2,
      minZoom: 0.1,
      maxZoom: 3
    });
    window.__cy_instances[id] = cy;

    if (window.__cy_state[id].allow_delete) installDeleteHotkeys(id);

    // Selection bridge -> Shiny input
    (function(){
      const inId = window.__cy_state[id] && window.__cy_state[id].selected_input;
      if (!inId || !window.Shiny) return;

      const sendNode = function(nodeOrNull){
        if (!nodeOrNull) {
          Shiny.setInputValue(inId, null, {priority:"event"});
          return;
        }
        const d = nodeOrNull.data() || {};
        const ens = d.Ensembl_ID || d.EnsemblID || d.ensembl_id || d["Ensembl ID"] || "";
        Shiny.setInputValue(inId, {
          id: nodeOrNull.id(),
          label: d.label || d.name || nodeOrNull.id(),
          shared_name: d.shared_name || "",
          Ensembl_ID: ens,
          path: d.path || "",
          heatmap_path: d.heatmap_path || d.hm_path || ""
        }, {priority:"event"});
      };

      cy.on("select", "node", function(evt){ sendNode(evt.target); });
      cy.on("unselect", "node", function(){
        if (cy.$("node:selected").length === 0) sendNode(null);
      });
    })();

    // Right-click popup (Plot / Heatmap / Enrichment)
    (function(){
      const containerEl = document.getElementById(id);
      if (!containerEl) return;

      if (!window.__cy_popup_z) window.__cy_popup_z = 20000;
      function bringToFront(pop){ window.__cy_popup_z++; pop.style.zIndex = String(window.__cy_popup_z); }

      function stopAllEvents(pop){
        ["mousedown","click","dblclick","wheel","touchstart","touchmove","touchend","pointerdown"].forEach(function(evName){
          pop.addEventListener(evName, function(e){ e.stopPropagation(); });
        });
        pop.addEventListener("mousedown", function(e){ bringToFront(pop); e.stopPropagation(); });
      }

      cy.on("cxttap", "node,edge", function(evt){
        const oe = evt.originalEvent;
        const st = window.__cy_state[id] || {};
        const allowDelete = !!st.allow_delete;

        // Ctrl/Meta + rightclick: delete target immediately
        if (allowDelete && oe && (oe.ctrlKey || oe.metaKey)){
          if (oe.preventDefault) oe.preventDefault();
          evt.target.select();
          deleteSelected(id, {fit_after:false});
          return;
        }

        if (!evt.target.isNode || !evt.target.isNode()) return;
        if (oe && oe.preventDefault) oe.preventDefault();

        const node = evt.target;
        const pos  = node.renderedPosition();

        const plotSrc  = node.data("path") || "";
        const hmSrc    = node.data("heatmap_path") || node.data("hm_path") || "";
        const enrichHtml = node.data("enrich_html") || "";
        const hasPlot  = !!plotSrc;
        const hasHm    = !!hmSrc;
        const hasEn    = !!enrichHtml;

        const label    = node.data("label") || node.id();

        const tabsHtml =
          "<div class=\\"cy-popup-tabs\\">" +
            "<button type=\\"button\\" class=\\"cy-tab-btn cy-tab-plot" + (hasPlot ? " active" : "") + "\\">Plot</button>" +
            "<button type=\\"button\\" class=\\"cy-tab-btn cy-tab-hm\\"" + (hasHm ? ">" : " disabled>") + "Heatmap</button>" +
            "<button type=\\"button\\" class=\\"cy-tab-btn cy-tab-en\\"" + (hasEn ? ">" : " disabled>") + "Enrichment</button>" +
          "</div>";

        // two content areas: imageWrap + enrichWrap
        const imageWrapHtml = (
          (hasPlot || hasHm)
            ? ("<div class=\\"cy-popup-img-wrap cy-wrap-img\\">" +
                "<img class=\\"cy-popup-img\\" src=\\"" + (hasPlot ? plotSrc : hmSrc) + "\\" />" +
               "</div>")
            : "<div class=\\"cy-popup-img-wrap cy-wrap-img\\">No image available for this node.</div>"
        );

        const enrichWrapHtml =
          "<div class=\\"cy-popup-img-wrap cy-wrap-en cy-enrich\\" style=\\"display:none;\\">" +
            (hasEn ? enrichHtml : "No enrichment data.") +
          "</div>";

        const pop = document.createElement("div");
        pop.className = "cy-popup";
        pop.innerHTML =
          "<div class=\\"cy-popup-header\\">" +
            "<span class=\\"cy-popup-title\\">" + label + "</span>" +
            "<button type=\\"button\\" class=\\"cy-popup-close\\">&#x2715;</button>" +
          "</div>" +
          tabsHtml +
          imageWrapHtml +
          enrichWrapHtml;

        pop.style.width  = "380px";
        pop.style.height = "300px";
        pop.style.left = (pos.x + 20) + "px";
        pop.style.top  = (pos.y + 20) + "px";

        bringToFront(pop);
        containerEl.appendChild(pop);
        stopAllEvents(pop);

        const closeBtn = pop.querySelector(".cy-popup-close");
        if (closeBtn){
          closeBtn.onclick = function(e){
            e.stopPropagation();
            if (pop && pop.parentNode) pop.parentNode.removeChild(pop);
          };
        }

        const imgWrap  = pop.querySelector(".cy-wrap-img");
        const enWrap   = pop.querySelector(".cy-wrap-en");
        const imgEl    = pop.querySelector(".cy-popup-img");
        const btnPlot  = pop.querySelector(".cy-tab-plot");
        const btnHm    = pop.querySelector(".cy-tab-hm");
        const btnEn    = pop.querySelector(".cy-tab-en");

        function setActive(btn){
          [btnPlot, btnHm, btnEn].forEach(function(b){ if (b) b.classList.remove("active"); });
          if (btn) btn.classList.add("active");
        }
        function showImg(){
          if (enWrap) enWrap.style.display = "none";
          if (imgWrap) imgWrap.style.display = "block";
        }
        function showEn(){
          if (imgWrap) imgWrap.style.display = "none";
          if (enWrap)  enWrap.style.display = "block";
        }

        if (btnPlot){
          btnPlot.onclick = function(e){
            e.stopPropagation();
            if (!hasPlot) return;
            showImg();
            if (imgEl) imgEl.src = plotSrc;
            setActive(btnPlot);
          };
        }
        if (btnHm){
          btnHm.onclick = function(e){
            e.stopPropagation();
            if (!hasHm || btnHm.disabled) return;
            showImg();
            if (imgEl) imgEl.src = hmSrc;
            setActive(btnHm);
          };
        }
        if (btnEn){
          btnEn.onclick = function(e){
            e.stopPropagation();
            if (!hasEn || btnEn.disabled) return;
            showEn();
            setActive(btnEn);
          };
        }

        // draggable
        const headerEl = pop.querySelector(".cy-popup-header");
        if (headerEl){
          let dragging=false, startX=0, startY=0, startLeft=0, startTop=0;
          headerEl.addEventListener("mousedown", function(e){
            dragging = true;
            bringToFront(pop);
            e.stopPropagation(); e.preventDefault();

            const rect = pop.getBoundingClientRect();
            const contRect2 = containerEl.getBoundingClientRect();
            startX = e.clientX; startY = e.clientY;
            startLeft = rect.left - contRect2.left;
            startTop  = rect.top  - contRect2.top;

            function onMove(ev){
              if (!dragging) return;
              const dx = ev.clientX - startX;
              const dy = ev.clientY - startY;
              pop.style.left = (startLeft + dx) + "px";
              pop.style.top  = (startTop  + dy) + "px";
            }
            function onUp(){
              dragging=false;
              document.removeEventListener("mousemove", onMove);
              document.removeEventListener("mouseup", onUp);
            }
            document.addEventListener("mousemove", onMove);
            document.addEventListener("mouseup", onUp);
          });
        }
      });
    })();

    // resize/fit after render
    (function(){
      let tries = 0;
      const tick = function(){
        const r = el.getBoundingClientRect();
        if (r.width > 0 && r.height > 0) {
          try{ cy.resize(); }catch(e){}
          try{ cy.fit(); cy.center(); }catch(e){}
        } else if (tries++ < 30) {
          setTimeout(tick, 120);
        }
      };
      setTimeout(tick, 60);
    })();

    return cy;
  }

  // ---- Border color handler (existing) --------------------------------------
  function isColorString(x){
    return (typeof x === "string") && x.length > 0;
  }

  function applyNodeBorderColors(container_id, items, reset_ids){
    const cy = window.__cy_instances[container_id];
    if (!cy) return;

    if (Array.isArray(reset_ids) && reset_ids.length){
      reset_ids.forEach(function(id){
        const n = cy.getElementById(id);
        if (!n.empty()){
          try{ n.removeStyle("border-color"); }catch(e){ n.style("border-color", ""); }
        }
      });
    }

    if (!Array.isArray(items) || items.length === 0) return;

    items.forEach(function(it){
      if (!it || typeof it.id !== "string") return;
      if (!isColorString(it.color)) return;
      const n = cy.getElementById(it.id);
      if (n.empty()) return;
      n.style("border-color", it.color);
    });
  }

  // ---- NEW: set node meta (enrich_html) -------------------------------------
  function applyNodeMeta(container_id, items, reset_ids){
    const cy = window.__cy_instances[container_id];
    if (!cy) return;

    if (Array.isArray(reset_ids) && reset_ids.length){
      reset_ids.forEach(function(id){
        const n = cy.getElementById(id);
        if (!n.empty()){
          n.data("enrich_html", null);
        }
      });
    }

    if (!Array.isArray(items) || items.length === 0) return;

    items.forEach(function(it){
      if (!it || typeof it.id !== "string") return;
      const n = cy.getElementById(it.id);
      if (n.empty()) return;
      if (typeof it.enrich_html === "string" && it.enrich_html.length){
        n.data("enrich_html", it.enrich_html);
      } else {
        n.data("enrich_html", null);
      }
    });
  }

  // ---- NEW: double border highlight (no color change) ------------------------
  function applyNodeBorderStyle(container_id, items, reset_ids){
    const cy = window.__cy_instances[container_id];
    if (!cy) return;

    if (Array.isArray(reset_ids) && reset_ids.length){
      reset_ids.forEach(function(id){
        const n = cy.getElementById(id);
        if (!n.empty()){
          try{ n.removeStyle("border-style"); }catch(e){ n.style("border-style",""); }
          try{ n.removeStyle("border-width"); }catch(e){ n.style("border-width",""); }
        }
      });
    }

    if (!Array.isArray(items) || items.length === 0) return;

    items.forEach(function(it){
      if (!it || typeof it.id !== "string") return;
      const n = cy.getElementById(it.id);
      if (n.empty()) return;
      n.style("border-style", "double");
      const bw = (typeof it.borderWidthPx === "number" && it.borderWidthPx > 0) ? it.borderWidthPx : 6;
      n.style("border-width", bw + "px");
    });
  }

  function registerHandlers(){
    Shiny.addCustomMessageHandler("cy_init", function(p){ ensureCy(p); });

    Shiny.addCustomMessageHandler("cy_fit", function(p){
      const cy = window.__cy_instances[p.container_id];
      if (cy){ cy.fit(); cy.center(); }
    });

    // NEW: safe resize handler
    Shiny.addCustomMessageHandler("cy_resize", function(p){
      const cy = window.__cy_instances[p.container_id];
      if (!cy) return;
      try{ cy.resize(); }catch(e){}
      if (p && p.fit){
        try{ cy.fit(); cy.center(); }catch(e){}
      }
    });

    Shiny.addCustomMessageHandler("cy_set_node_images", function(p){
      const cy = window.__cy_instances[p.container_id];
      if (!cy || !p || !Array.isArray(p.items)) return;

      const wantLoading = !!p.show_loading;
      if (wantLoading) showLoading(p.container_id);

      let pending = 0;

      if (Array.isArray(p.clear_ids) && p.clear_ids.length){
        p.clear_ids.forEach(function(id){
          const n = cy.getElementById(id);
          if (!n.empty()){
            n.style("background-image","none");
            n.data("path", null);
            n.data("heatmap_path", null);
            n.data("hm_path", null);
          }
        });
      }

      p.items.forEach(function(it){
        if (!it || typeof it.id !== "string") return;
        if (typeof it.url !== "string" || !it.url.length) return;

        const n = cy.getElementById(it.id);
        if (n.empty()) return;

        n.data("path", it.url);

        if (typeof it.hm_url === "string" && it.hm_url.length){
          n.data("heatmap_path", it.hm_url);
          n.data("hm_path", it.hm_url);
        }
        if (typeof it.borderWidthPx === "number"){
          n.style("border-width", String(it.borderWidthPx) + "px");
        }

        pending++;
        const img = new Image();
        img.crossOrigin = "anonymous";
        img.onload = img.onerror = function(){
          pending = Math.max(0, pending - 1);
          if (pending === 0 && wantLoading) hideLoading(p.container_id);
        };

        n.style("background-image", "url(\\"" + it.url + "\\")");
        img.src = it.url;
      });

      if (pending === 0 && wantLoading) hideLoading(p.container_id);
    });

    Shiny.addCustomMessageHandler("cy_loading", function(p){
      if (!p || !p.container_id) return;
      if (p.on) showLoading(p.container_id); else hideLoading(p.container_id);
    });

    Shiny.addCustomMessageHandler("cy_set_node_borders", function(p){
      if (!p || !p.container_id) return;
      applyNodeBorderColors(p.container_id, p.items, p.reset_ids);
    });

    Shiny.addCustomMessageHandler("cy_set_node_meta", function(p){
      if (!p || !p.container_id) return;
      applyNodeMeta(p.container_id, p.items, p.reset_ids);
    });

    Shiny.addCustomMessageHandler("cy_set_node_borderstyle", function(p){
      if (!p || !p.container_id) return;
      applyNodeBorderStyle(p.container_id, p.items, p.reset_ids);
    });

    Shiny.addCustomMessageHandler("cy_delete_selected", function(p){
      if (!p || !p.container_id) return;
      const st = window.__cy_state[p.container_id] || {};
      if (st.allow_delete === false) return;
      deleteSelected(p.container_id, {fit_after: !!p.fit_after});
    });

    Shiny.addCustomMessageHandler("cy_push_cyjs", function(p){
      if (!p || !p.container_id) return;
      const cy = window.__cy_instances[p.container_id];
      if (!cy) return;
      sendChanged(p.container_id, {
        action: "cyjs",
        cyjs: cy.json(),
        ts: Date.now()
      });
    });

    Shiny.addCustomMessageHandler("cy_export_pdf", function(p){
      try{
        if (!p || !p.container_id) return;
        const cy = window.__cy_instances[p.container_id];
        if (!cy){ alert("Cytoscape instance not found."); return; }

        if (!window.jspdf || !window.jspdf.jsPDF){ alert("jsPDF not loaded."); return; }
        const jsPDF = window.jspdf.jsPDF;

        withOverlayHidden(p.container_id, function(){
          const scale = (typeof p.scale === "number" && p.scale > 0) ? p.scale : 2;
          const bg = p.bg || "white";
          const dataUrl = cy.png({ full: true, scale: scale, bg: bg });

          const doc = new jsPDF({
            orientation: (p.orientation === "p" || p.orientation === "portrait") ? "p" : "l",
            unit: "pt",
            format: p.format || "a4"
          });

          const pageW = doc.internal.pageSize.getWidth();
          const pageH = doc.internal.pageSize.getHeight();

          const img = new Image();
          img.onload = function(){
            const iw = img.width, ih = img.height;
            const r = Math.min(pageW / iw, pageH / ih);
            const w = iw * r, h = ih * r;
            const x = (pageW - w) / 2;
            const y = (pageH - h) / 2;
            doc.addImage(dataUrl, "PNG", x, y, w, h);
            doc.save(p.filename || "network.pdf");
          };
          img.onerror = function(){ alert("Failed to load PNG from cy.png()."); };
          img.src = dataUrl;
        });

      } catch(e){
        console.error(e);
        alert("PDF export failed: " + (e && e.message ? e.message : String(e)));
      }
    });

    Shiny.addCustomMessageHandler("cy_export_pdf_with_popups", function(p){
      try{
        if (!p || !p.container_id) return;

        const el = document.getElementById(p.container_id);
        if (!el){ alert("Container not found."); return; }

        if (!window.html2canvas){ alert("html2canvas not loaded."); return; }
        if (!window.jspdf || !window.jspdf.jsPDF){ alert("jsPDF not loaded."); return; }
        const jsPDF = window.jspdf.jsPDF;

        return withOverlayHidden(p.container_id, async function(){
          const imgs = Array.from(el.querySelectorAll("img"));
          await Promise.all(imgs.map(img => new Promise(res => {
            if (img.complete) return res();
            img.onload = img.onerror = () => res();
          })));

          const scale = (typeof p.scale === "number" && p.scale > 0) ? p.scale : 2;

          const canvas = await html2canvas(el, {
            backgroundColor: "#ffffff",
            scale: scale,
            useCORS: true,
            allowTaint: false
          });

          const dataUrl = canvas.toDataURL("image/png");
          const w = canvas.width;
          const h = canvas.height;

          const doc = new jsPDF({
            orientation: (w > h) ? "l" : "p",
            unit: "pt",
            format: [w, h]
          });

          doc.addImage(dataUrl, "PNG", 0, 0, w, h);
          doc.save(p.filename || "network_with_popups.pdf");
        });

      } catch(e){
        console.error(e);
        alert("PDF export (with popups) failed: " + (e && e.message ? e.message : String(e)));
      }
    });
  }

  function whenShinyReady(cb){
    if (window.Shiny && typeof Shiny.addCustomMessageHandler === "function"){
      cb();
    } else {
      setTimeout(function(){ whenShinyReady(cb); }, 50);
    }
  }

  whenShinyReady(registerHandlers);

})();
    '))
  )
}
