// Run after exporting .mslm_project_ui() HTML; no browser dependencies required.
const fs = require('node:fs');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const html = fs.readFileSync(process.argv[2], 'utf8');
const scripts = [...html.matchAll(/<script[^>]*>([\s\S]*?)<\/script>/g)].map(m => m[1]);
const events = {};
const handlers = {};
const calls = [];
let registrations = 0;
const context = {
  window: {}, document: {},
  $: () => ({on: (event, fn) => {events[event] = fn;}}),
  Shiny: {
    addCustomMessageHandler(name, fn) {registrations++; handlers[name] = fn;},
    setInputValue(...args) {calls.push(args);}
  }
};
for (const script of scripts) vm.runInNewContext(script, context);
events['shiny:connected']();
events['shiny:connected']();
assert.equal(registrations, 1, 'reconnection must not register a duplicate handler');
handlers.mslm_project_capture({request: 1});
assert.equal(calls[0][1].graph, null);
const graph = {elements: {nodes: [{data: {id: 'PC'}, position: {x: 7, y: 9}}]}};
context.window.__cy_instances = {'net-cy': {json: () => graph}};
context.window.__cy_state = {'net-cy': {network_id: 'second'}};
handlers.mslm_project_capture({request: 2});
assert.equal(calls[1][0], 'project_capture');
assert.equal(calls[1][1].request, 2);
assert.equal(calls[1][1].network_id, 'second');
assert.equal(calls[1][1].graph, graph);
assert.equal(calls[1][2].priority, 'event');
if (process.argv[3]) {
  const cyto = fs.readFileSync(process.argv[3], 'utf8');
  for (const match of cyto.matchAll(/<script[^>]*>([\s\S]*?)<\/script>/g)) new vm.Script(match[1]);
}
console.log('PASS: browser snapshot, graph ID, reconnection, and generated JavaScript syntax');
