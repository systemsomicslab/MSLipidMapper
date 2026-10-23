const {chromium}=require('C:/Users/takak/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs=require('fs');
(async()=>{
 const b=await chromium.launch({headless:true,executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'});
 for(const stale of [false,true]){
  const p=await b.newPage({viewport:{width:1550,height:1100}}),errors=[];
  p.on('pageerror',e=>errors.push(e.message));
  if(stale){let code=fs.readFileSync('inst/app/www/vendor/mslm-cytoscape-style.js','utf8').replace('"background-image": "none"','"background-image": "data(path)"');await p.route('**/vendor/mslm-cytoscape-style.js*',r=>r.fulfill({contentType:'application/javascript',body:code}));}
  await p.goto('http://127.0.0.1:3869');await p.waitForFunction(()=>window.Shiny?.shinyapp?.isConnected());
  await p.evaluate(()=>Shiny.setInputValue('go_net',1,{priority:'event'}));
  await p.waitForSelector('#net-cy',{state:'visible'});
  await p.waitForFunction(()=>window.__cy_instances?.['net-cy'],{},{timeout:8000}).catch(()=>{});
  console.log(stale?'STALE ASSET':'FRESH ASSET',await p.evaluate(()=>{const cy=window.__cy_instances?.['net-cy'];return cy?{nodes:cy.nodes().length,width:cy.width(),height:cy.height(),pan:cy.pan(),zoom:cy.zoom()}:null}),errors);
  if(!stale)await p.locator('#net-cy').screenshot({path:'output/installed-network-before-fix.png'});
  await p.close();
 }
 await b.close();
})().catch(e=>{console.error(e);process.exit(1)});
