const {chromium}=require('C:/Users/takak/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
(async()=>{
const b=await chromium.launch({headless:true,executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'}),p=await b.newPage({viewport:{width:1550,height:1100},acceptDownloads:true});
const errors=[];p.on('pageerror',e=>{errors.push(e.message);console.log('JS ERROR:',e.message)});
await p.goto('http://127.0.0.1:3868');await p.waitForFunction(()=>window.__cy_instances?.['path-cy']);
const drawer=p.locator('#path-settings_drawer');if(await drawer.isVisible())throw Error('Drawer initially visible');
await p.check('#path-fc_enabled');
await p.waitForFunction(()=>document.querySelector('.mslm-fc-status')?.textContent.includes('Treatment / Control'));
async function tap(cls){await p.evaluate(cls=>{const n=window.__cy_instances['path-cy'].nodes().filter(n=>n.data('shared_name')===cls).first();n.emit('tap')},cls)}
await tap('PC');await p.waitForSelector('.mslm-fc-panel');
const oldChart=await p.locator('.mslm-fc-chart').textContent();
const cyInstance=await p.evaluate(()=>{window.testCy=window.__cy_instances['path-cy'];return true});
await p.locator('[data-settings-open="compare"]').first().click();await p.waitForFunction(()=>document.getElementById('path-diff_group_a')?.selectize);
await p.evaluate(()=>{document.getElementById('path-diff_group_a').selectize.setValue('Treatment');document.getElementById('path-diff_group_b').selectize.setValue('Control')});
await p.fill('#path-fc_top_n','3');await p.click('[data-settings-close]');
if((await p.locator('.mslm-fc-chart').textContent())!==oldChart)throw Error('Drafts applied before commit');
await p.locator('[data-settings-open="compare"]').first().click();await p.click('#path-settings_apply');
await p.waitForFunction(()=>document.querySelector('#path-settings_drawer').hidden);
await p.waitForFunction(()=>document.querySelector('.mslm-fc-chart').textContent.includes('Control / Treatment'));
if(!(await p.locator('#path-settings_summary').textContent()).includes('Control / Treatment'))throw Error('Summary');
if(!(await p.locator('.mslm-fc-chart').textContent()).includes('6 shown'))throw Error('FC count not committed');
// Verify that changing FC settings did not recreate the pathway or reset positions.
if(!await p.evaluate(()=>window.testCy===window.__cy_instances['path-cy']))throw Error('Unrelated settings reset pathway');
await p.locator('[data-settings-open="display"]').first().click();await p.click('[data-tab="compare"]');
await p.evaluate(()=>document.getElementById('path-diff_group_a').selectize.setValue('Control'));
await p.click('#path-settings_apply');await p.waitForSelector('.shiny-notification-error');if(!await drawer.isVisible())throw Error('Invalid settings accepted');
await p.evaluate(()=>document.getElementById('path-diff_group_a').selectize.setValue('Treatment'));
await p.click('[data-tab="display"]');await p.evaluate(()=>document.getElementById('path-agg_fun').selectize.setValue('mean'));await p.click('#path-settings_apply');await p.waitForFunction(()=>document.querySelector('#path-settings_drawer').hidden);
await p.screenshot({path:'output/pathway-workspace-integrated.png',fullPage:true});
await p.locator('[data-settings-open="compare"]').first().click();await p.screenshot({path:'output/pathway-workspace-settings-integrated.png',fullPage:true});
await p.keyboard.press('Escape');
await p.locator('.mslm-pathway-save summary').click();const dl=p.waitForEvent('download');await p.click('#path-fc_svg');await(await dl).saveAs('output/pathway-workspace-integrated.svg');
await p.locator('[data-settings-open="display"]').first().click();await p.click('[data-tab="manage"]');await p.check('#path-map_edit_enabled');await p.click('[data-settings-close]');
await p.waitForFunction(()=>window.__cy_state['path-cy'].allow_delete===true);
await p.locator('[data-settings-open="display"]').first().click();await p.click('[data-tab="manage"]');await p.uncheck('#path-map_edit_enabled');await p.click('[data-settings-close]');await p.waitForFunction(()=>window.__cy_state['path-cy'].allow_delete===false);
await p.selectOption('#path-net_select',{label:'sphingolipids'});await p.waitForFunction(()=>document.querySelectorAll('.mslm-fc-panel').length===0);
await p.setViewportSize({width:390,height:844});await p.locator('[data-settings-open="display"]').first().click();
try{await p.waitForFunction(()=>document.documentElement.scrollWidth<=innerWidth,{},{timeout:6000})}catch(e){console.log(await p.evaluate(()=>[...document.querySelectorAll('body *')].filter(e=>e.getBoundingClientRect().right>innerWidth+2).slice(0,15).map(e=>({tag:e.tagName,cls:e.className,id:e.id,right:e.getBoundingClientRect().right}))));throw Error('Page overflows at narrow width')}
if(errors.length)throw Error(errors.join('\n'));console.log('PASS: draft isolation, commit, validation, summaries, FC, unchanged network, tabs, SVG, deletion mode, network switch, narrow layout.');
await b.close();
})().catch(e=>{console.error(e);process.exit(1)});
