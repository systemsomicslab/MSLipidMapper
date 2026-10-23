/* Compact pathway toolbar and transactional settings drawer. */
(function () {
  'use strict';
  if (window.MSLMPathwayWorkspace) return;
  window.MSLMPathwayWorkspace = true;
  const css=document.createElement('style');
  css.textContent=`
  .mslm-pathway-workspace{position:relative;color:#263e50}
  .mslm-pathway-header{padding:12px 14px;margin-bottom:10px;background:#fff;border:1px solid #dce5eb;border-radius:10px}
  .mslm-current-pathway{display:flex;gap:8px;align-items:baseline;flex-wrap:wrap;max-width:100%}
  .mslm-current-pathway strong{font-size:15px;overflow-wrap:anywhere}.mslm-pathway-counts{font-size:11px;color:#637e8d}
  .mslm-projection-pending{font-size:11px;color:#99642b}.mslm-projection-ready{font-size:11px;color:#137b70}
  .mslm-pathway-controls{min-width:0}
  .mslm-pathway-toolbar{display:flex;gap:10px;align-items:center;flex-wrap:wrap;padding:0;background:#fff}
  .mslm-pathway-picker{width:100%;min-width:0}.mslm-pathway-picker .form-group{margin:0}.mslm-pathway-picker label{font-size:12px;color:#627d8e}.mslm-pathway-picker select{height:auto!important;width:100%;max-height:150px}
  .mslm-toolbar-spacer{flex:1}.mslm-pathway-toolbar .btn-primary,.mslm-settings-footer .btn-primary{background:#177f85;border-color:#177f85}
  .mslm-settings-summary{display:flex;align-items:center;flex-wrap:wrap;gap:8px;margin:8px 0}
  .mslm-settings-badge{border:0;background:#e6f0f2;color:#375e6b;border-radius:5px;font-size:12px;padding:5px 10px;max-width:100%;overflow-wrap:anywhere;text-align:left}
  .mslm-pathway-context{display:flex;align-items:center;gap:10px;flex-wrap:wrap;background:#f7fafb;padding:7px 9px;border-radius:7px}
  .mslm-pathway-context .form-group{margin:0}.mslm-pathway-context .checkbox{margin:0}.mslm-context-actions{display:flex;gap:6px;flex-wrap:wrap}
  .mslm-pathway-context>.shiny-input-container{width:auto!important;flex:0 0 auto}
  .mslm-significance-legend{display:flex;gap:10px;flex-wrap:wrap;font-size:11px;color:#627d8e}
  .mslm-pathway-canvas{border:1px solid #dce5eb;border-radius:10px;background:#fff}
  .mslm-pathway-hint{color:#627d8e;font-size:12px;margin:8px 0 16px}
  .mslm-selected-node{padding:14px;background:#fff;border:1px solid #dce5eb;border-radius:10px}
  .mslm-pathway-workspace .plot-card{border:1px solid #e1e8ed;border-radius:9px;padding:12px;margin-bottom:10px}
  .mslm-pathway-workspace .cy-plot-frame{width:100%;height:360px;display:flex;align-items:center;justify-content:center;overflow:hidden}
  .mslm-pathway-workspace .cy-plot-square{width:min(100%,360px);height:360px}
  .mslm-pathway-workspace .cy-plot-square .shiny-plot-output{width:100%!important;height:100%!important}
  .mslm-pathway-settings{position:fixed;top:0;right:0;bottom:0;width:410px;max-width:100vw;background:white;border-left:1px solid #d9e4eb;box-shadow:-8px 0 35px #18334522;z-index:40010;display:flex;flex-direction:column}
  .mslm-pathway-settings[hidden],.mslm-settings-pane[hidden],.mslm-settings-backdrop[hidden]{display:none!important}
  .mslm-settings-header{display:flex;justify-content:space-between;align-items:center;padding:20px 22px;border-bottom:1px solid #e4ecf1}
  .mslm-settings-header h3{font-size:18px;margin:0}.mslm-settings-close{border:0;background:none;font-size:25px;color:#536e7f;padding:0 6px}
  .mslm-settings-tabs{display:grid;grid-template-columns:repeat(4,1fr);padding:0 12px;border-bottom:1px solid #e4ecf1}
  .mslm-settings-tabs button{background:none;border:0;border-bottom:2px solid transparent;padding:13px 2px;color:#667f90;font-size:12px}
  .mslm-settings-tabs button[aria-selected=true]{color:#147a82;border-color:#147a82;font-weight:600}
  .mslm-settings-body{flex:1;overflow:auto;padding:18px 22px;min-height:0}.mslm-settings-body h4{font-size:14px;font-weight:600;margin:0 0 16px}
  .mslm-settings-body .help-block{font-size:12px;color:#647f91}.mslm-settings-body hr{margin:22px 0;border-color:#e5edf2}
  .mslm-settings-body .form-control{border-radius:6px}.mslm-settings-body details{font-size:12px;color:#637e90}.mslm-settings-body summary{cursor:pointer}
  .mslm-settings-footer{display:flex;align-items:center;justify-content:space-between;gap:12px;padding:15px 22px;border-top:1px solid #e4ecf1;background:white}
  .mslm-settings-dirty{font-size:12px;color:#657f8f}.mslm-settings-dirty.pending{color:#a15f23}
  .mslm-settings-backdrop{position:fixed;inset:0;background:#17394a12;z-index:40000}
  .mslm-pathway-save{position:relative}.mslm-pathway-save>summary{cursor:pointer;padding:6px 14px;border:1px solid #ccc;border-radius:4px;background:white}
  .mslm-save-menu{position:absolute;right:0;top:38px;width:260px;background:#fff;border:1px solid #dce5eb;border-radius:8px;box-shadow:0 6px 25px #16334422;z-index:30001;padding:7px}
  .mslm-save-menu .btn{display:block;width:100%;text-align:left;border:0;background:none;padding:9px;font-size:13px}.mslm-save-menu .btn:hover{background:#edf5f6}
  .mslm-pathway-workspace button:focus-visible{outline:3px solid #258f97;outline-offset:2px}
  @media(max-width:800px){.mslm-pathway-header{grid-template-columns:1fr;gap:10px}.mslm-pathway-toolbar .mslm-toolbar-spacer{display:none}.mslm-pathway-settings{width:min(410px,100vw)}.mslm-context-actions{gap:4px}.mslm-pathway-context{gap:8px}}
  `;
  document.head.append(css);
  const instances=new Map();
  function init(root){
    if(root.dataset.settingsReady)return;
    const drawer=root.querySelector('.mslm-pathway-settings');
    if(!drawer || !drawer.querySelector('.mslm-settings-footer .action-button'))return;
    root.dataset.settingsReady='true';
    const picker=root.querySelector('.mslm-pathway-picker select');
    if(picker){
      const fitCandidates=()=>{picker.size=Math.max(2,Math.min(5,picker.options.length))};
      fitCandidates();new MutationObserver(fitCandidates).observe(picker,{childList:true});
    }
    const backdrop=document.createElement('div');backdrop.className='mslm-settings-backdrop';backdrop.hidden=true;root.append(backdrop);
    const dirty=drawer.querySelector('.mslm-settings-dirty');
    let lastFocus=null,active='display';
    const state={root,drawer,backdrop,close,markApplied};instances.set(root.id,state);
    function tab(key){active=key;drawer.querySelectorAll('[data-tab]').forEach(b=>{const selected=b.dataset.tab===key;b.setAttribute('aria-selected',selected);b.tabIndex=selected?0:-1});drawer.querySelectorAll('[data-pane]').forEach(p=>p.hidden=p.dataset.pane!==key)}
    function open(key){lastFocus=document.activeElement;tab(key||active);drawer.hidden=false;backdrop.hidden=false;Array.from(root.children).forEach(e=>{if(e!==drawer&&e!==backdrop)e.inert=true});drawer.querySelector('[data-settings-close]').focus();window.dispatchEvent(new Event('resize'))}
    function close(){drawer.hidden=true;backdrop.hidden=true;Array.from(root.children).forEach(e=>e.inert=false);if(lastFocus&&lastFocus.isConnected)lastFocus.focus()}
    function markApplied(){dirty.textContent='Settings applied.';dirty.classList.remove('pending');close()}
    root.addEventListener('click',e=>{
      const opener=e.target.closest('[data-settings-open]');if(opener){open(opener.dataset.settingsOpen);return}
      if(e.target.closest('[data-settings-close]')){close();return}
      const t=e.target.closest('[data-tab]');if(t){tab(t.dataset.tab);return}
      if(e.target.closest('[data-network-delete]')&&confirm('Delete the current pathway from this session?'))window.Shiny.setInputValue(root.dataset.ns+'net_delete',Date.now(),{priority:'event'});
      if(e.target.closest('.mslm-save-menu .btn'))root.querySelector('.mslm-pathway-save').open=false;
    });
    root.addEventListener('change',e=>{if(!drawer.hidden&&e.target.closest('.mslm-settings-body')&&!e.target.closest('[data-pane="manage"]')){dirty.textContent='Unapplied changes';dirty.classList.add('pending')}});
    backdrop.onclick=close;
    drawer.addEventListener('keydown',e=>{
      if(e.key==='Escape'){e.preventDefault();close();return}
      if(e.target.matches('[data-tab]')&&['ArrowLeft','ArrowRight','Home','End'].includes(e.key)){
        e.preventDefault();const tabs=Array.from(drawer.querySelectorAll('[data-tab]'));let i=tabs.indexOf(e.target);
        i=e.key==='Home'?0:e.key==='End'?tabs.length-1:(i+(e.key==='ArrowRight'?1:-1)+tabs.length)%tabs.length;tab(tabs[i].dataset.tab);tabs[i].focus();return;
      }
      if(e.key!=='Tab')return;
      const focusable=Array.from(drawer.querySelectorAll('button,select,input,a[href],[tabindex]')).filter(x=>!x.disabled&&x.tabIndex>=0&&x.getClientRects().length);
      if(!focusable.length)return;
      const first=focusable[0],last=focusable[focusable.length-1];
      if(e.shiftKey&&document.activeElement===first){e.preventDefault();last.focus()}
      else if(!e.shiftKey&&document.activeElement===last){e.preventDefault();first.focus()}
    });
    tab('display');
  }
  function scan(){document.querySelectorAll('.mslm-pathway-workspace').forEach(init)}
  if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',scan);else scan();
  new MutationObserver(()=>{for(const[id,s]of instances)if(!s.root.isConnected)instances.delete(id);scan()}).observe(document.documentElement,{childList:true,subtree:true});
  function register(){
    if(!window.Shiny||!window.Shiny.addCustomMessageHandler){setTimeout(register,50);return}
    window.Shiny.addCustomMessageHandler('cy_settings_applied',p=>{
      const s=instances.get(p.root_id);if(s)s.markApplied();
      if(p.enrichment)window.Shiny.setInputValue(p.prefix+'apply_enrich',Date.now(),{priority:'event'});
    });
    window.Shiny.addCustomMessageHandler('cy_projection_ready',p=>{
      if(p.enrichment)window.Shiny.setInputValue(p.prefix+'apply_enrich',Date.now(),{priority:'event'});
    });
  }
  register();
})();
