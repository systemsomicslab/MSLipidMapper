const fs = require('fs');
const assert = require('assert');
const source = fs.readFileSync('R/upload_module.R', 'utf8');
const js = source.match(/htmlwidgets::onRender\("([\s\S]*?)"\)/)[1];
const setup = new Function('return (' + js + ')')();
let data = [['sample1', 'A', true], ['sample2', 'B', false]];
const hooks = {};
const notice = {style: {}, id: 'test-sample_pending_notice'};
const button = {setAttribute() {}};
global.document = {getElementById(id) {return id.endsWith('notice') ? notice : button;}};
const hot = {
  getData: () => data, countRows: () => data.length, countCols: () => 3,
  getDataAtCell: (r,c) => data[r][c],
  addHook: (k,v) => hooks[k] = v, removeHook: k => delete hooks[k], render() {}
};
global.HTMLWidgets = {getInstance: () => ({hot})};
setup({id:'test-cd_hot'});
assert.equal(notice.textContent, '');
assert.equal(button.disabled, true);
data[0][1] = 'C'; data[1][2] = true;
hooks.afterChange();
assert(notice.textContent.startsWith('2 unapplied'));
assert.equal(button.disabled, false);
let highlighted;
const td = {classList: {toggle: (_,v) => highlighted = v}, setAttribute() {}, getAttribute() {}, removeAttribute() {}};
hooks.afterRenderer(td, 0, 1);
assert.equal(highlighted, true);
hooks.afterRenderer(td, 0, 0);
assert.equal(highlighted, false);
data[0][1] = 'A'; hooks.afterChange();
assert(notice.textContent.startsWith('1 unapplied'));
assert.equal(button.disabled, false);
data[1][2] = false; hooks.afterChange();
assert.equal(notice.textContent, '');
assert.equal(button.disabled, true);
data[0][1] = 'C'; hooks.afterChange();
setup({id:'test-cd_hot'});
assert.equal(notice.textContent, '');
assert.equal(button.disabled, true);
hooks.afterRenderer(td, 0, 1);
assert.equal(highlighted, false);
console.log('Pending edits: cell highlighting, checkbox edits, revert, and re-render reset verified');
