// Recorrido bs4Dash real: apertura, carga diferida, Plotly y estimación de modelos con datos ficticios.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport: {width: 1440, height: 1000}});
    page.setDefaultTimeout(20000);
    const errors = []; page.on('pageerror', e => errors.push(e.message));
    await page.goto(process.env.CRM_ANALITICO_URL || 'http://127.0.0.1:3880');
    await page.waitForFunction(() => window.Shiny && Shiny.shinyapp.isConnected());
    const idle = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') &&
      (!document.querySelector('#rsh-loader-host') || document.querySelector('#rsh-loader-host').hidden));
    await idle();
    // Cada apertura debe sobrevivir al estado controlbar enviado por Shiny y al preloader sintético.
    for (let i = 0; i < 2; i++) {
      await page.locator('#controlbar-toggle').click();
      await page.waitForFunction(() => document.body.classList.contains('control-sidebar-slide-open'));
      await idle();
      assert(await page.locator('.control-sidebar').isVisible());
      await page.locator('#controlbar-toggle').click();
      await page.waitForFunction(() => !document.body.classList.contains('control-sidebar-slide-open'));
      await idle();
    }
    for (const index of [0, 1, 0]) {
      await page.locator('.rmh-trigger').nth(index).click(); await idle();
      assert.equal(await page.locator('.rmh-menu.show').count(), 1);
      await page.locator('.content-wrapper').click({position: {x: 30, y: 30}});
      await page.waitForFunction(() => !document.querySelector('.rmh-menu.show'));
    }
    // La primera visita debe llenar los selectores sin hacer clic en Actualizar.
    await page.locator('a[data-value="cartera"]').click();
    await page.waitForFunction(() => document.querySelector('#cartera-corte')?.value === '2026-11-05');
    await page.locator('#cartera-tabla-tabla .gt_table').waitFor();
    assert.match(await page.locator('#cartera-tabla-tabla').innerText(), /\$1,000/);
    await page.locator('a[data-value="productos"]').click();
    for (const id of ['ranking', 'pareto', 'tendencia', 'rentabilidad']) {
      await page.waitForSelector(`#productos-${id}.js-plotly-plot,#productos-${id} .js-plotly-plot`);
      const info = await page.locator(`#productos-${id}.js-plotly-plot,#productos-${id} .js-plotly-plot`)
        .evaluate(p => p.data.some(t => t.text && t.text.length));
      assert(info, `Falta hover de ${id}`);
    }
    await page.locator('a[data-value="fidelidad"]').click();
    await page.locator('#fidelidad-rfm-tabla .gt_table').waitFor();
    await page.locator('#fidelidad-modelar').click(); await idle();
    await page.locator('#ui_fidelidad a[data-value="BTYD y CLV"]').click();
    await page.locator('#fidelidad-btyd-tabla .gt_table').waitFor();
    assert.match(await page.locator('#fidelidad-estado_modelo').innerText(), /Modelo ajustado/);
    await page.locator('#ui_fidelidad a[data-value="Diagnóstico del modelo"]').click();
    await page.locator('#fidelidad-modelo_parametros-tabla .gt_table').waitFor();
    await page.evaluate(() => Shiny.setInputValue('fidelidad-corte:shiny.date', '2026-10-01', {priority: 'event'}));
    await page.locator('#ui_fidelidad a[data-value="BTYD y CLV"]').click();
    await page.waitForFunction(() => document.querySelector('#fidelidad-estado_modelo').innerText.includes('cambiaron'));
    assert.equal(await page.locator('#fidelidad-btyd-tabla:visible').count(), 0);
    await page.locator('a[data-value="potencial"]').click();
    await page.locator('#potencial-tabla-tabla .gt_table').waitFor();
    assert.match(await page.locator('#potencial-tabla-tabla').innerText(), /Entidad/);
    await page.evaluate(() => Shiny.setInputValue('potencial-mes:shiny.date', '2026-01-01', {priority: 'event'}));
    await page.waitForFunction(() => document.querySelector('#potencial-tabla-tabla').innerText.includes('70.0%'));

    await page.locator('a[data-value="basket"]').click();
    const guide = page.getByText('Qué es Market Basket y cómo usar este tablero', {exact: true});
    await guide.waitFor();
    assert.equal(await guide.evaluate(el => el.closest('details').open), false);
    await guide.click();
    assert.equal(await guide.evaluate(el => el.closest('details').open), true);
    await page.locator('a[data-value="cohortes"]').click();
    await page.waitForFunction(() => document.querySelector('#cohortes-fecha_fin')?.value === '08/2026');
    assert.equal(await page.locator('#cohortes-fecha_ini').inputValue(), '01/2026');
    await page.evaluate(() => Shiny.setInputValue('fecha_tope:shiny.date', '2026-05-15', {priority: 'event'}));
    await page.waitForFunction(() => document.querySelector('#cohortes-fecha_fin')?.value === '05/2026');
    assert.equal(await page.locator('#cohortes-fecha_ini').inputValue(), '01/2026');
    assert.deepEqual(errors, []);
    await page.screenshot({path: '/tmp/crm-analitico.png', fullPage: true});
    console.log('OK: controlbar/header al primer clic, Cartera diferida, cuatro Plotly, RFM/BG-NBD/CLV y guía Basket.');
  } finally {await browser.close();}
})().catch(e => {console.error(e); process.exitCode = 1;});
