const {chromium}=require('C:/Users/takak/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs=require('fs'),path=require('path');
(async()=>{
 const b=await chromium.launch({headless:true,executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'});
 const p=await b.newPage({viewport:{width:1550,height:1100}}),errors=[];
 p.on('pageerror',e=>errors.push(e.message));
 await p.route('http://127.0.0.1:7310/static/**',async route=>{const f=path.join(process.cwd(),'output/fc-integration-plots',path.basename(new URL(route.request().url()).pathname));if(fs.existsSync(f))await route.fulfill({contentType:'image/svg+xml',body:fs.readFileSync(f)});else await route.abort()});
 await p.goto('http://127.0.0.1:3868');await p.waitForFunction(()=>window.__cy_instances?.['path-cy']&&window.Shiny?.shinyapp?.isConnected());
 await p.click('#path-map_local_svgs');await p.waitForSelector('.mslm-projection-ready',{timeout:60000});
 await p.evaluate(()=>window.__cy_instances['path-cy'].nodes().filter(n=>n.data('shared_name')==='PC').first().emit('cxttap'));
 await p.waitForSelector('.mslm-fc-panel');if(await p.locator('.cy-popup').count())throw Error('Legacy popup still created');
 await p.locator('button').filter({hasText:/^Panel settings$/}).click();
 const setMode=async mode=>{await p.evaluate(m=>document.getElementById('path-popup_content').selectize.setValue(m),mode);await p.waitForFunction(m=>document.querySelector('.mslm-fc-title')?.textContent.includes(m==='plot'?'enlarged plot':m==='fc'?'species FC':m),mode)};
 const initial=await p.locator('.mslm-fc-panel').getAttribute('style');
 for(const mode of ['plot','heatmap']){await setMode(mode);await p.waitForFunction(()=>{const i=document.querySelector('.mslm-popup-content img');return i&&i.complete&&i.naturalWidth>0});if(await p.locator('.mslm-fc-panel').getAttribute('style')!==initial)throw Error('Mode switch changed layout');}
 const download=p.waitForEvent('download');
 await p.evaluate(()=>Shiny.shinyapp.dispatchMessage(JSON.stringify({custom:{cy_fc_action:{container_id:'path-cy',action:'export'}}})));
 const exported=await download;await exported.saveAs('output/popup-heatmap-export.svg');
 if(!fs.readFileSync('output/popup-heatmap-export.svg','utf8').includes('data:image/svg+xml;base64,'))throw Error('Panel image not embedded');
 await setMode('enrichment');await p.waitForFunction(()=>document.querySelector('.mslm-popup-content').textContent.includes('No enrichment'));
 await setMode('fc');
 if(!(await p.locator('.mslm-projection-ready').count()))throw Error('Content switch invalidated projection');
 await p.evaluate(()=>{
   const cy=window.__cy_instances['path-cy'];
   const msg={container_id:'path-cy',enabled:true,network_id:'axis-test',mode:'fc',contrast:'B / A',domain:100,classes:[{class:'PC',total:2,valid:2,excluded:0,rows:[{name:'PC small',fc:0.12},{name:'PC down',fc:-0.08}]},{class:'PE',total:1,valid:1,excluded:0,rows:[{name:'PE large',fc:12}]}]};
   Shiny.shinyapp.dispatchMessage(JSON.stringify({custom:{cy_fc_panels:msg}}));
   cy.nodes().filter(n=>n.data('shared_name')==='PC').first().emit('tap');cy.nodes().filter(n=>n.data('shared_name')==='PE').first().emit('tap');
 });
 await p.waitForFunction(()=>document.querySelectorAll('.mslm-fc-panel').length===2);
 const axes=await p.locator('.mslm-fc-chart').evaluateAll(es=>es.map(e=>[...e.querySelectorAll('text')].map(t=>t.textContent)));
 if(!axes[0].includes('+0.125')||!axes[1].includes('+12.5'))throw Error('Independent domains wrong: '+JSON.stringify(axes));
 await p.keyboard.press('Escape');
 const head=p.locator('.mslm-fc-header').first(),rect=await head.boundingBox();await p.mouse.move(rect.x+80,rect.y+15);await p.mouse.down();await p.mouse.move(rect.x+220,rect.y+70);await p.mouse.up();
 await p.screenshot({path:'output/popup-panels-axis-check.png',fullPage:true});
 if(errors.length)throw Error(errors.join(';'));console.log('PASS: right click auto-enables unified panel; real plot and heatmap; enrichment empty state; immediate mode switch retains positions and projection; independent axes 0.125 / 12.5; drag; no JS errors.');await b.close();
})().catch(e=>{console.error(e);process.exit(1)});


