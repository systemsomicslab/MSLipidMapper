/* Movable per-species FC panels for the existing Cytoscape/Shiny viewer.
 * No remote dependencies. All labels enter the DOM through textContent.
 */
(function () {
  'use strict';
  if (window.MSLMFC) return;
  const NS = 'http://www.w3.org/2000/svg';
  const states = new Map(), pending = new Map();
  const css = document.createElement('style');
  css.textContent = `
    .mslm-fc-layer{position:absolute;inset:0;pointer-events:none;z-index:1000;overflow:hidden}
    .mslm-fc-lines{position:absolute;inset:0;width:100%;height:100%;pointer-events:none}
    .mslm-fc-panel{position:absolute;pointer-events:auto;background:white;border:1px solid #b9cdd4;border-radius:8px;box-shadow:0 3px 12px #16334418;min-width:270px;min-height:240px;overflow:hidden;display:flex;flex-direction:column}
    .mslm-fc-header{display:flex;align-items:center;gap:8px;padding:8px 10px;background:#edf5f6;color:#183e4c;cursor:grab;touch-action:none;flex-shrink:0}
    .mslm-fc-title{font:600 13px/1.4 system-ui,sans-serif;flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
    .mslm-fc-close{border:0;background:transparent;font:20px/1 system-ui;cursor:pointer;color:#345363;padding:0 4px}
    .mslm-fc-chart{display:block;width:100%;flex:1;min-height:0}
    .mslm-popup-content{flex:1;min-height:0;overflow:auto;padding:8px;box-sizing:border-box}.mslm-popup-content img{width:100%;height:100%;object-fit:contain}.mslm-popup-empty{font:13px/1.6 system-ui;color:#657f8e;padding:16px}.mslm-fc-resize{position:absolute;bottom:0;right:0;width:20px;height:20px;cursor:nwse-resize;touch-action:none;background:linear-gradient(135deg,transparent 65%,#8ca8b5 66%,#8ca8b5 70%,transparent 71%)}
    .mslm-fc-status{position:absolute;bottom:6px;left:8px;max-width:85%;padding:4px 8px;background:#fffffff0;color:#345363;font:12px/1.4 system-ui,sans-serif;pointer-events:none}
  `;
  document.head.append(css);
  function svg(tag, attrs, parent) {
    const e = document.createElementNS(NS, tag);
    for (const [k, v] of Object.entries(attrs || {})) e.setAttribute(k, v);
    if (parent) parent.append(e);
    return e;
  }
  function text(parent, x, y, value, attrs) {
    const e = svg('text', {x, y, fill:'#253b4b', 'font-family':'Arial, sans-serif', 'font-size':11, ...attrs}, parent);
    e.textContent = value;
    return e;
  }
  function say(s, value) { s.status.textContent = value; }
  function bounds(s, p) {
    p.w = Math.min(Math.max(270, p.w), Math.max(270, s.el.clientWidth));
    p.h = Math.min(Math.max(240, p.h), Math.max(240, s.el.clientHeight - 30));
    p.x = Math.max(0, Math.min(p.x, s.el.clientWidth - p.w));
    p.y = Math.max(0, Math.min(p.y, s.el.clientHeight - p.h - 25));
  }
  function place(s, p) {
    bounds(s, p);
    Object.assign(p.el.style, {left:p.x+'px', top:p.y+'px', width:p.w+'px', height:p.h+'px'});
  }
  function connectors(s) {
    s.lines.replaceChildren();
    for (const [id, p] of s.panels) {
      const n = s.cy.getElementById(id);
      if (n.empty() || !n.visible()) continue;
      const q = n.renderedPosition();
      const ex = Math.max(p.x, Math.min(q.x, p.x+p.w));
      const ey = Math.max(p.y, Math.min(q.y, p.y+p.h));
      svg('path', {d:`M${q.x},${q.y} Q${(q.x+ex)/2},${q.y} ${ex},${ey}`,
        stroke:'#7399a6', 'stroke-width':1.2, 'stroke-dasharray':'4 4', fill:'none'}, s.lines);
    }
  }
  function mode(s) { return s.payload.mode || 'fc'; }
  const labels = {fc:'species FC', plot:'enlarged plot', heatmap:'heatmap', enrichment:'enrichment'};
  function panelTitle(s,p) { return p.key+' · '+labels[mode(s)]; }
  function axisDomain(rows) {
    const max = Math.max(0, ...rows.map(r=>Math.abs(Number(r.fc))).filter(Number.isFinite));
    if (!max) return 1;
    const power = Math.pow(10,Math.floor(Math.log10(max)));
    return ([1,1.25,1.5,2,2.5,3,4,5,6,8,10].find(v=>v*power>=max*(1-1e-12)) || 10)*power;
  }
  function chart(s, p) {
    p.title.textContent = panelTitle(s,p);
    p.close.setAttribute('aria-label','Close '+p.key+' popup panel');
    const m=mode(s), node=s.cy.getElementById(p.id);
    p.chart.style.display=m==='fc'?'':'none';
    p.content.style.display=m==='fc'?'none':'';
    if(m!=='fc') {
      const d=node.empty()?{}:node.data();
      const source=m==='plot'?d.path:m==='heatmap'?(d.heatmap_path||d.hm_path):d.enrich_html;
      const signature=m+'|'+(source||'');
      if(p.signature!==signature) {
        p.signature=signature;p.content.replaceChildren();p.image=null;
        if(source && m==='enrichment') p.content.innerHTML=source;
        else if(source) {
          const img=document.createElement('img');img.alt=p.key+' '+labels[m];img.src=source;p.image=img;
          img.onerror=()=>{p.content.textContent='Image could not be loaded. Run Pathway Mapping again.';};
          p.content.append(img);
        } else {
          const empty=document.createElement('div');empty.className='mslm-popup-empty';
          empty.textContent=m==='enrichment'?'No enrichment results for this node.':'No projected image for this node. Run Pathway Mapping first.';
          p.content.append(empty);
        }
      }
      return;
    }
    const c = s.classes.get(p.key), g = p.chart;
    g.replaceChildren();
    if (!c) { g.setAttribute('viewBox',`0 0 ${p.w} ${p.h-38}`);text(g,10,30,s.payload.error || 'No lipid-class FC measurements for this node.');return; }
    const w = p.w, h = p.h-38, domain = axisDomain(c.rows || []);
    p.domain=domain;
    g.setAttribute('viewBox', `0 0 ${w} ${h}`);
    // Long names remain available in tooltips; a bounded label area keeps bars visible.
    const left = Math.min(178, w*.53), right = w-15, mid = (left+right)/2, span = (right-left)/2;
    text(g, 10, 16, s.payload.contrast, {'font-size':10});
    const rows = Array.isArray(c.rows) ? c.rows : [];
    const top = 49, bottom = h-36;
    for (const t of [-domain, 0, domain]) {
      const x = mid+t/domain*span;
      svg('line', {x1:x, y1:40, x2:x, y2:bottom, stroke:t===0?'#8fa4b0':'#e5edf1'}, g);
      text(g, x, 32, (t>0?'+':'')+Number(t.toPrecision(3)), {'text-anchor':'middle', 'font-size':10});
    }
    if (!rows.length) text(g, 10, 70, 'No non-zero finite FC values.');
    rows.forEach((row, i) => {
      const step = (bottom-top)/Math.max(1, rows.length), y = top+(i+.5)*step;
      const label = text(g, 10, y+3, row.name.length>25?row.name.slice(0,24)+'…':row.name,
        {'font-size':Math.min(11, Math.max(8, step*.75))});
      const title = svg('title', {}, label); title.textContent = row.name+'; log2FC '+Number(row.fc).toPrecision(4);
      const width = Math.abs(row.fc)/domain*span;
      const bar = svg('rect', {x:row.fc>=0?mid:mid-width, y:y-Math.min(7,step*.35), width,
        height:Math.min(14,step*.7), fill:row.fc>0?'#dc765e':'#648fca'}, g);
      const tip = svg('title', {}, bar); tip.textContent = row.name+'; log2FC '+row.fc;
    });
    text(g, 10, h-18, `${c.total} mapped · ${c.valid} valid · ${rows.length} shown`, {'font-size':10});
    text(g, 10, h-5, `${c.excluded} excluded · independent axis per panel`, {'font-size':9, fill:'#657f8e'});
    text(g, right, h-18, 'log₂FC', {'text-anchor':'end', 'font-size':10});
  }
  function drag(s, p, handle, resize) {
    handle.addEventListener('pointerdown', function(e) {
      if (e.button!==0 || e.target.closest('button')) return;
      e.preventDefault(); e.stopPropagation();
      const x=e.clientX, y=e.clientY, original={x:p.x,y:p.y,w:p.w,h:p.h};
      handle.setPointerCapture(e.pointerId);
      p.el.style.zIndex = String(++s.z);
      function move(ev) {
        if (resize) { p.w=original.w+ev.clientX-x; p.h=original.h+ev.clientY-y; }
        else { p.x=original.x+ev.clientX-x; p.y=original.y+ev.clientY-y; }
        place(s,p); chart(s,p); connectors(s);
      }
      function end() { handle.removeEventListener('pointermove',move); handle.removeEventListener('pointerup',end); handle.removeEventListener('pointercancel',end); }
      handle.addEventListener('pointermove',move); handle.addEventListener('pointerup',end); handle.addEventListener('pointercancel',end);
    });
  }
  function open(s, node) {
    if (!s.payload || !s.payload.enabled) return;
    if (mode(s)==='fc' && s.payload.error) { say(s,s.payload.error); return; }
    const id=node.id(), d=node.data();
    // Protein nodes must never acquire a lipid panel just because a label matches.
    if (mode(s)==='fc' && (d.Ensembl_ID || d.EnsemblID || d.ensembl_id)) {say(s,'Choose Enlarged plot or Heatmap for a gene node.');return;}
    const key=String(d.shared_name || d.label || d.name || '').trim();
    if (mode(s)==='fc' && !s.classes.has(key)) { say(s,'No lipid-class measurements for '+key+'.'); return; }
    if (s.panels.has(id)) { s.panels.get(id).el.style.zIndex=String(++s.z); return; }
    const box=document.createElement('div'); box.className='mslm-fc-panel';
    const head=document.createElement('div'); head.className='mslm-fc-header';
    const title=document.createElement('span'); title.className='mslm-fc-title'; title.textContent=key+' · species FC';
    const close=document.createElement('button'); close.className='mslm-fc-close'; close.textContent='×'; close.setAttribute('aria-label','Close '+key+' FC panel');close.setAttribute('data-html2canvas-ignore','true');
    head.append(title,close); box.append(head);
    const c=svg('svg', {class:'mslm-fc-chart','aria-label':key+' species log2 fold changes',role:'img'}, box);
    const content=document.createElement('div');content.className='mslm-popup-content';box.append(content);
    const grip=document.createElement('div'); grip.className='mslm-fc-resize';grip.setAttribute('data-html2canvas-ignore','true');box.append(grip);
    // Reserve the next free peripheral slot without moving existing cards.
    let i=0, x=8, y=8;
    for (; i<100; i++) {
      x=i%2?Math.max(8,s.el.clientWidth-338):8; y=8+Math.floor(i/2)*295;
      if (y+285>s.el.clientHeight-25) { x=20+(i*23)%Math.max(30,s.el.clientWidth-355); y=20+(i*19)%Math.max(30,s.el.clientHeight-320); break; }
      if (![...s.panels.values()].some(p=>x<p.x+p.w && x+330>p.x && y<p.y+p.h && y+285>p.y)) break;
    }
    const p={id,key,el:box,title,close,chart:c,content,x,y,w:330,h:285};
    s.panels.set(id,p); s.layer.append(box); place(s,p); chart(s,p);
    close.onclick=()=>{box.remove();s.panels.delete(id);connectors(s);};
    ['pointerdown','click','dblclick','wheel','contextmenu'].forEach(type=>box.addEventListener(type,e=>e.stopPropagation()));
    drag(s,p,head,false); drag(s,p,grip,true); connectors(s);
    say(s,'Drag headers to move; drag bottom-right corners to resize. '+(s.payload.contrast||''));
  }
  function clear(s) { for(const p of s.panels.values())p.el.remove();s.panels.clear();connectors(s); }
  function update(s,payload) {
    if (s.payload && s.payload.network_id!==payload.network_id) clear(s);
    s.payload=payload;
    const classes=Array.isArray(payload.classes)?payload.classes:[];
    s.classes=new Map(classes.map(c=>[c.class,c]));
    s.layer.style.display=payload.enabled?'':'none';
    for (const [id,p] of s.panels) {
      if (s.cy.getElementById(id).empty()) { p.el.remove();s.panels.delete(id); }
      else chart(s,p);
    }
    say(s,payload.error || (payload.enabled?'Popup panels · '+labels[mode(s)]+(payload.contrast?' · '+payload.contrast:'')+'. Click a node.':''));
    connectors(s);
    if(s.requested && payload.enabled) { const node=s.cy.getElementById(s.requested);s.requested=null;if(!node.empty())open(s,node); }
  }
  function mount(id,cy) {
    const old=states.get(id);
    if (old) { old.observer.disconnect(); old.layer.remove(); }
    const el=document.getElementById(id); if(!el)return;
    const layer=document.createElement('div');layer.className='mslm-fc-layer';layer.style.display='none';
    const lines=svg('svg',{class:'mslm-fc-lines'},layer);
    const status=document.createElement('div');status.className='mslm-fc-status';status.setAttribute('data-html2canvas-ignore','true');status.setAttribute('role','status');status.setAttribute('aria-live','polite');layer.append(status);el.append(layer);
    const s={el,cy,layer,lines,status,panels:new Map(),classes:new Map(),payload:null,z:1};states.set(id,s);
    cy.on('tap','node',e=>open(s,e.target));
    cy.on('data','node',e=>{const p=s.panels.get(e.target.id());if(p)chart(s,p)});
    cy.on('pan zoom position resize',()=>connectors(s));
    cy.on('remove',()=>{for(const[n,p]of s.panels)if(cy.getElementById(n).empty()){p.el.remove();s.panels.delete(n)}connectors(s)});
    s.observer=new ResizeObserver(()=>{for(const p of s.panels.values()){place(s,p);chart(s,p)}connectors(s)});s.observer.observe(el);
    if (pending.has(id))update(s,pending.get(id));
    // Preserve positions during re-rendering of the same network (e.g. mapping).
    if(old && old.payload && s.payload && old.payload.network_id===s.payload.network_id) {
      for(const[n,p]of old.panels){const node=cy.getElementById(n);if(node.empty())continue;open(s,node);const q=s.panels.get(n);if(q){Object.assign(q,{x:p.x,y:p.y,w:p.w,h:p.h});place(s,q);chart(s,q)}}connectors(s);
    }
  }
  async function exportFigure(s) {
    if(!s.payload || !s.payload.enabled){say(s,'Enable Popup panels first.');return;}
    const w=s.el.clientWidth,h=s.el.clientHeight;
    const out=svg('svg',{xmlns:NS,width:w,height:h,viewBox:`0 0 ${w} ${h}`});
    svg('rect',{width:w,height:h,fill:'white'},out);
    svg('image',{x:0,y:0,width:w,height:h,href:s.cy.png({full:false,scale:2,bg:'white'}),preserveAspectRatio:'none'},out);
    const lines=s.lines.cloneNode(true);lines.setAttribute('width',w);lines.setAttribute('height',h);out.append(lines);
    // z-order matches the interactive view, rather than insertion order.
    for(const p of [...s.panels.values()].sort((a,b)=>(+a.el.style.zIndex||0)-(+b.el.style.zIndex||0))){
      svg('rect',{x:p.x,y:p.y,width:p.w,height:p.h,rx:8,fill:'white',stroke:'#b9cdd4'},out);
      text(out,p.x+10,p.y+23,panelTitle(s,p),{'font-size':13,'font-weight':600});
      if(mode(s)!=='fc') {
        if(p.image) {
          try {
            const response=await fetch(p.image.src);if(!response.ok)throw new Error('HTTP '+response.status);
            const blob=await response.blob();
            const data=await new Promise((resolve,reject)=>{const r=new FileReader();r.onload=()=>resolve(r.result);r.onerror=reject;r.readAsDataURL(blob)});
            svg('image',{x:p.x+8,y:p.y+38,width:p.w-16,height:p.h-46,href:data,preserveAspectRatio:'xMidYMid meet'},out);
          } catch(e) {say(s,'Export stopped: unable to embed '+p.key+' image. '+e.message);return;}
        } else {
          const fo=svg('foreignObject',{x:p.x+8,y:p.y+38,width:p.w-16,height:p.h-46},out);
          const content=p.content.cloneNode(true);content.setAttribute('xmlns','http://www.w3.org/1999/xhtml');fo.append(content);
        }
        continue;
      }
      const c=p.chart.cloneNode(true);c.setAttribute('x',p.x);c.setAttribute('y',p.y+38);c.setAttribute('width',p.w);c.setAttribute('height',p.h-38);out.append(c);
    }
    const blob=new Blob([new XMLSerializer().serializeToString(out)],{type:'image/svg+xml;charset=utf-8'});
    const url=URL.createObjectURL(blob),a=document.createElement('a');a.href=url;a.download='pathway_popup_figure.svg';a.click();setTimeout(()=>URL.revokeObjectURL(url),1000);
    say(s,'SVG saved: popup panels + raster network snapshot.');
  }
  function register() {
    if(!window.Shiny || !window.Shiny.addCustomMessageHandler){setTimeout(register,50);return;}
    window.Shiny.addCustomMessageHandler('cy_fc_panels',p=>{pending.set(p.container_id,p);const s=states.get(p.container_id);if(s)update(s,p)});
    window.Shiny.addCustomMessageHandler('cy_fc_action',p=>{const s=states.get(p.container_id);if(!s)return;
      if(p.action==='clear')clear(s);
      if(p.action==='arrange'){let i=0;for(const panel of s.panels.values()){panel.x=i%2?s.el.clientWidth-panel.w-8:8;panel.y=8+Math.floor(i/2)*295;i++;place(s,panel);chart(s,panel)}connectors(s);}
      if(p.action==='export')exportFigure(s);
    });
  }
  window.MSLMFC={mount,open:(id,node)=>{
    const s=states.get(id);if(!s)return;
    if(!s.payload || !s.payload.enabled) {
      const toggle=document.getElementById(id.replace(/cy$/,'fc_enabled'));
      if(toggle && !toggle.checked){s.requested=node.id();toggle.click();return;}
    }
    open(s,node);
  }};register();
})();
