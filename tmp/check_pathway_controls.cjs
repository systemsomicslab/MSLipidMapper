const {chromium}=require('C:/Users/takak/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
(async()=>{
 const b=await chromium.launch({headless:true,executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'}),p=await b.newPage();const errors=[];p.on('pageerror',e=>errors.push(e.message));
 await p.goto('http://127.0.0.1:3868');await p.waitForFunction(()=>window.__cy_instances?.['path-cy']&&window.Shiny?.shinyapp?.isConnected());
 if(await p.locator('#path-map_edit_enabled').count())throw Error('Deletion toggle remains');
 if(!await p.evaluate(()=>document.getElementById('path-fit').previousElementSibling.id==='path-map_local_svgs'))throw Error('Mapping button misplaced');
 await p.check('#path-fc_enabled');await p.waitForFunction(()=>document.querySelector('.mslm-fc-status').textContent.includes('Treatment / Control'));
 await p.evaluate(()=>window.__cy_instances['path-cy'].nodes().filter(n=>n.data('shared_name')==='PC').first().emit('cxttap'));
 if(await p.locator('.mslm-fc-panel,.cy-popup').count())throw Error('Right click opened popup');
 await p.evaluate(()=>window.__cy_instances['path-cy'].nodes().filter(n=>n.data('shared_name')==='PC').first().emit('tap'));await p.waitForSelector('.mslm-fc-panel');
 await p.check('#path-diff_borders_enabled');await p.locator('button').filter({hasText:/^Panel settings$/}).click();
 await p.click('[data-tab="display"]');await p.evaluate(()=>document.getElementById('path-agg_fun').selectize.setValue('mean'));await p.click('#path-settings_apply');
 await p.waitForFunction(()=>!document.getElementById('path-diff_borders_enabled').checked&&document.querySelector('.mslm-projection-pending'));
 await p.waitForTimeout(700);
 const result=await p.evaluate(()=>{
   const cy=window.__cy_instances['path-cy'];const edge=cy.edges().first(),edgeId=edge.id();
   const unrelated=cy.nodes().filter(n=>n.data('shared_name')==='PC').first();unrelated.select();
   edge.emit({type:'cxttap',originalEvent:{ctrlKey:true,preventDefault(){}}});
   const edgeDeleted=cy.getElementById(edgeId).empty(),unrelatedKept=!cy.getElementById(unrelated.id()).empty();
   const node=cy.nodes().filter(n=>n.data('shared_name')==='PE').first(),nodeId=node.id();
   node.emit({type:'cxttap',originalEvent:{ctrlKey:true,preventDefault(){}}});
   return {edgeDeleted,unrelatedKept,nodeDeleted:cy.getElementById(nodeId).empty(),nodeId,edgeId};
 });
 if(!result.edgeDeleted||!result.nodeDeleted||!result.unrelatedKept)throw Error(JSON.stringify(result));
 await p.waitForTimeout(700);
 if(!await p.evaluate(r=>{const cy=window.__cy_instances['path-cy'];return cy.getElementById(r.nodeId).empty()&&cy.getElementById(r.edgeId).empty()},result))throw Error('Deleted elements reappeared '+JSON.stringify(result)+' '+await p.evaluate(()=>window.__cy_instances['path-cy'].nodes().map(n=>n.id()).join(',')));
 if(errors.length)throw Error(errors.join(';'));console.log('PASS: Mapping left of Fit; no deletion toggle; regular right click does nothing; click opens popup; Apply unchecks highlighting; Ctrl-right-click deletes target node/edge and preserves unrelated selection; changes persist.');await b.close();
})().catch(e=>{console.error(e);process.exit(1)});
