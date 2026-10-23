const {chromium} = require('C:/Users/takak/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const assert = require('assert/strict');
(async () => {
  const browser = await chromium.launch({headless:true, executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'});
  try {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    await page.goto('http://127.0.0.1:3877');
    await page.waitForFunction(() => document.getElementById('echo').textContent.trim() === '2 plotly 12');
    assert.equal(await page.locator('#volc-point_size').isVisible(), false);
    assert.equal(await page.locator('#volc-p_thresh').isVisible(), true);
    await page.click('#open_volcano');
    await page.waitForSelector('.modal.in #volc-point_size');
    await page.locator('#volc-point_size').fill('4');
    await page.locator('#volc-point_size').press('Tab');
    await page.locator('#feat-adv_plot_font_size').fill('18');
    await page.locator('#volc-view').evaluate(el => {el.selectize.setValue('ggplot'); el.selectize.close();});
    await page.locator('#feat-adv_apply').click();
    await page.waitForFunction(() => document.getElementById('echo').textContent.trim() === '4 ggplot 18');
    await page.waitForFunction(() => !document.querySelector('.modal.in'));
    assert.equal(await page.locator('#volc-point_size').count(), 1);
    assert.equal(await page.locator('#volc-point_size').isVisible(), false);
    for (let i=0;i<3;i++) {
      await page.click('#open_volcano');
      await page.waitForSelector('.modal.in #volc-point_size');
      assert.equal(await page.locator('#volc-point_size').inputValue(), '4');
      await page.getByRole('button', {name:'Close', exact:true}).click();
      await page.waitForFunction(() => !document.querySelector('.modal.in'));
      assert.equal(await page.locator('#volc-point_size').count(),1);
    }
    await page.click('#open_shared');
    await page.waitForSelector('.modal.in');
    assert.equal(await page.locator('#volc-point_size').isVisible(), false);
    assert.equal(await page.locator('.modal.in #feat-adv_plot_font_size').count(),0);
    assert.deepEqual(errors, []);
    console.log('Browser: initial inputs bound, settings moved, values retained over repeated close/reopen, shared dialog isolated');
  } finally {await browser.close();}
})().catch(e => {console.error(e);process.exitCode=1;});
