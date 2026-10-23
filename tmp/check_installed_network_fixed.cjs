const {chromium}=require('C:/Users/takak/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
(async()=>{const b=await chromium.launch({headless:true,executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'}),p=await b.newPage({viewport:{width:1550,height:1100}});const errors=[];p.on('pageerror',e=>errors.push(e.message));
await p.route('**/vendor/mslm-cytoscape-style.js*',async r=>{if(!new URL(r.request().url()).searchParams.has('v'))throw Error('Old unversioned style requested');await r.continue()});
await p.goto('http://127.0.0.1:3869');await p.waitForFunction(()=>window.__cy_instances?.['net-cy']);
// Reproduce entering the pathway tab after the original 3.6-second fit retry expires.
await p.waitForTimeout(4200);
await p.evaluate(()=>Shiny.setInputValue('go_net',1,{priority:'event'}));await p.waitForSelector('#net-cy',{state:'visible'});
await p.waitForFunction(()=>{const cy=window.__cy_instances['net-cy'];return cy.width()>0&&cy.height()>0&&cy.nodes().some(n=>{const r=n.renderedBoundingBox();return r.x2>0&&r.y2>0&&r.x1<cy.width()&&r.y1<cy.height()})});
const count=await p.evaluate(()=>window.__cy_instances['net-cy'].nodes().length);if(count!==37)throw Error('Unexpected node count '+count);
if(await p.locator('.cy-render-error').count())throw Error('Visible renderer error');if(errors.length)throw Error(errors.join(';'));
await p.locator('#net-cy').screenshot({path:'output/installed-network-fixed.png'});
console.log('PASS: installed full app; versioned assets, delayed hidden-tab display, 37 visible network nodes, no JS errors.');await b.close()})().catch(e=>{console.error(e);process.exit(1)});
