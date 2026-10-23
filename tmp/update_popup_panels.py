from pathlib import Path
p = Path('R/Mod_cyto.R')
s = p.read_text(encoding='utf-8')
s = s.replace('        if (mode != "fc") return_payload <- list() else return_payload <- NULL\n', '')
s = s.replace('Comparison / FC', 'Comparison / Panels').replace('Figure SVG (FC panels)', 'Figure SVG (popup panels)').replace('"FC panels", value', '"Popup panels", value')
s = s.replace('Click a node for its plots. Enable FC panels to add movable species-level charts; right-click for the existing popup.', 'Enable Popup panels, then click or right-click a node. Choose panel content in Panel settings; drag headers to move and corners to resize.')
p.write_text(s, encoding='utf-8')
p = Path('inst/app/www/vendor/mslm-fc-panels.js')
s = p.read_text(encoding='utf-8')
s = s.replace('.mslm-fc-resize{', '.mslm-popup-content{flex:1;min-height:0;overflow:auto;padding:8px;box-sizing:border-box}.mslm-popup-content img{width:100%;height:100%;object-fit:contain}.mslm-popup-empty{font:13px/1.6 system-ui;color:#657f8e;padding:16px}.mslm-fc-resize{')
s = s.replace('function chart(s, p) {', '''function mode(s) { return s.payload.mode || 'fc'; }
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
    }''')
s = s.replace('const w = p.w, h = p.h-38, domain = s.payload.domain || 1;', 'const w = p.w, h = p.h-38, domain = axisDomain(c.rows || []);\n    p.domain=domain;')
s = s.replace("(t>0?'+':'')+t", "(t>0?'+':'')+Number(t.toPrecision(3))")
s = s.replace("`${c.excluded} excluded · + increase / − decrease`", "`${c.excluded} excluded · independent axis per panel`")
s = s.replace("if (s.payload.error) { say(s,s.payload.error); return; }", "if (mode(s)==='fc' && s.payload.error) { say(s,s.payload.error); return; }")
s = s.replace('if (d.Ensembl_ID || d.EnsemblID || d.ensembl_id) return;', "if (mode(s)==='fc' && (d.Ensembl_ID || d.EnsemblID || d.ensembl_id)) {say(s,'Choose Enlarged plot or Heatmap for a gene node.');return;}")
s = s.replace("if (!s.classes.has(key))", "if (mode(s)==='fc' && !s.classes.has(key))")
s = s.replace("const grip=document.createElement('div');", "const content=document.createElement('div');content.className='mslm-popup-content';box.append(content);\n    const grip=document.createElement('div');")
s = s.replace('const p={key,el:box,chart:c,x,y,w:330,h:285};', 'const p={id,key,el:box,title,close,chart:c,content,x,y,w:330,h:285};')
s = s.replace("+s.payload.contrast);", "+(s.payload.contrast||''));")
s = s.replace('if (!s.classes.has(p.key) || s.cy.getElementById(id).empty())', 'if (s.cy.getElementById(id).empty())')
s = s.replace("say(s,payload.error || (payload.enabled?'FC panels: '+payload.contrast+'. Click a lipid class to add a panel.':''));", "say(s,payload.error || (payload.enabled?'Popup panels · '+labels[mode(s)]+(payload.contrast?' · '+payload.contrast:'')+'. Click or right-click a node.':''));")
s = s.replace("cy.on('pan zoom position resize'", "cy.on('data','node',e=>{const p=s.panels.get(e.target.id());if(p)chart(s,p)});\n    cy.on('pan zoom position resize'")
s = s.replace('function exportFigure(s)', 'async function exportFigure(s)')
s = s.replace("if(!s.payload || !s.payload.enabled || s.payload.error){say(s,'Enable FC panels and choose valid Group A/B first.');return;}", "if(!s.payload || !s.payload.enabled){say(s,'Enable Popup panels first.');return;}")
s = s.replace("p.key+' · species FC',{'font-size':13", "panelTitle(s,p),{'font-size':13")
s = s.replace("const c=p.chart.cloneNode(true);", """if(mode(s)!=='fc') {
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
      const c=p.chart.cloneNode(true);""")
s = s.replace('pathway_fc_figure.svg', 'pathway_popup_figure.svg').replace('SVG saved: editable FC panels + raster network snapshot.', 'SVG saved: popup panels + raster network snapshot.')
s = s.replace('window.MSLMFC={mount};', "window.MSLMFC={mount,open:(id,node)=>{const s=states.get(id);if(s)open(s,node)}};")
p.write_text(s,encoding='utf-8')
