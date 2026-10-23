const {chromium}=require('C:/Users/takak/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
(async()=>{
 const b=await chromium.launch({headless:true,executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'}),p=await b.newPage({viewport:{width:1600,height:1100}}),errors=[];p.on('pageerror',e=>errors.push(e.message));
 await p.goto('http://127.0.0.1:3868');await p.waitForFunction(()=>window.__cy_instances?.['path-cy']&&window.Shiny?.shinyapp?.isConnected());
 await p.waitForTimeout(500);
 if(!await p.evaluate(()=>document.getElementById('path-map_local_svgs').nextElementSibling.classList.contains('mslm-pathway-save')))throw Error('Save placement wrong');
 await p.check('#path-fc_enabled');await p.waitForFunction(()=>document.querySelector('.mslm-fc-status').textContent.includes('Treatment / Control'));
 await p.evaluate(()=>{
  const cy=window.__cy_instances['path-cy'];window.testCy=cy;
  const pc=cy.nodes().filter(n=>n.data('shared_name')==='PC').first();pc.position({x:345,y:678});cy.zoom(0.72);cy.pan({x:123,y:-56});pc.emit('tap');
 });await p.waitForSelector('.mslm-fc-panel');
 const before=await p.evaluate(()=>{const cy=window.testCy;return {zoom:cy.zoom(),pan:cy.pan(),pos:cy.nodes().filter(n=>n.data('shared_name')==='PC').first().position(),count:cy.nodes().length,panel:document.querySelector('.mslm-fc-panel').getAttribute('style')}});
 const deleted=await p.evaluate(()=>{const cy=window.testCy;const edge=cy.edges().first(),node=cy.nodes().filter(n=>n.data('shared_name')==='PE').first(),ids=[edge.id(),node.id()];for(const target of [edge,node])target.emit({type:'cxttap',originalEvent:{ctrlKey:true,preventDefault(){}}});return ids});
 await p.waitForFunction(()=>Shiny.shinyapp.$inputValues['path-cy_changed']?.action==='delete_selected');await p.waitForTimeout(1000);
 const after=await p.evaluate(ids=>{const cy=window.__cy_instances['path-cy'];return {same:cy===window.testCy,zoom:cy.zoom(),pan:cy.pan(),pos:cy.nodes().filter(n=>n.data('shared_name')==='PC').first().position(),count:cy.nodes().length,panel:document.querySelector('.mslm-fc-panel')?.getAttribute('style'),deleted:ids.every(id=>cy.getElementById(id).empty())}},deleted);
 if(!after.same||!after.deleted||after.count!==before.count-1)throw Error('Graph recreated or deletion lost '+JSON.stringify(after));
 for(const key of ['zoom','pan','pos','panel'])if(JSON.stringify(before[key])!==JSON.stringify(after[key]))throw Error(key+' changed');
 // Force an unrelated server update, then switch pathways away/back: stored deletion and positions must survive.
 await p.locator('button').filter({hasText:/^Panel settings$/}).click();await p.click('[data-tab="display"]');
 await p.selectOption('#path-net_select',{label:'sphingolipids'});await p.click('#path-settings_apply');await p.waitForFunction(()=>document.getElementById('path-current_pathway').textContent.includes('sphingolipids'));
 await p.locator('button').filter({hasText:/^Panel settings$/}).click();await p.click('[data-tab="display"]');await p.selectOption('#path-net_select',{label:'glycerophospholipids'});await p.click('#path-settings_apply');await p.waitForFunction(()=>document.getElementById('path-current_pathway').textContent.includes('glycerophospholipids'));
 if(!await p.evaluate(({ids,pos})=>{const cy=window.__cy_instances['path-cy'];return ids.every(id=>cy.getElementById(id).empty())&&JSON.stringify(cy.nodes().filter(n=>n.data('shared_name')==='PC').first().position())===JSON.stringify(pos)},{ids:deleted,pos:before.pos}))throw Error('Stored graph lost edits');
 if(errors.length)throw Error(errors.join(';'));console.log('PASS: Save immediately right of Mapping; node+edge deletion keeps same Cytoscape instance, zoom, pan, moved node, and popup position; stored edits survive pathway switching.');await b.close();
})().catch(e=>{console.error(e);process.exit(1)});
