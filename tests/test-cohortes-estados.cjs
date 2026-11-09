const assert = require('node:assert/strict');
const {chromium} = require('playwright');

(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ['--no-sandbox'] : []});
  const errors = [];
  try {
    const page = await browser.newPage({viewport: {width: 1440, height: 1000}});
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(process.env.CRM_COHORTES_URL || 'http://127.0.0.1:3875');
    await page.waitForFunction(() =>
      document.querySelector('#cohortes-kpi_retencion-valor_display')?.textContent.includes('50.00%'));
    await page.locator('#cohortes-tbl_resumen_mensual-tabla .gt_table').waitFor();
    assert.equal(await page.locator('.rmcv-card').count(), 22);

    await page.locator('#cohortes-kpi_ini_activo-btn_abrir').click();
    let table = page.locator('#cohortes-tbl_mod_ini_activo-tbl-tabla');
    await table.locator('.gt_table').waitFor();
    let text = await table.innerText();
    assert.ok(text.includes('Cliente de prueba 1') && text.includes('Cliente de prueba 2'));
    assert.ok(!text.includes('Cliente de prueba 3'));
    const download = page.waitForEvent('download');
    await page.locator('#cohortes-tbl_mod_ini_activo-btn_descarga').click();
    assert.ok((await download).suggestedFilename().endsWith('.xlsx'));
    await page.locator('.modal-footer').getByRole('button', {name: 'Cerrar', exact: true}).click();
    await page.locator('#shiny-modal').waitFor({state: 'hidden'});

    await page.locator('#cohortes-kpi_vig_activo-btn_abrir').click();
    table = page.locator('#cohortes-tbl_mod_vig_activo-tbl-tabla');
    await table.locator('.gt_table').waitFor();
    text = await table.innerText();
    assert.ok(text.includes('Cliente de prueba 1') && text.includes('Cliente de prueba 3'));
    assert.ok(!text.includes('Cliente de prueba 2'));
    await page.locator('.modal-footer').getByRole('button', {name: 'Cerrar', exact: true}).click();
    await page.locator('#shiny-modal').waitFor({state: 'hidden'});

    await page.locator('#cohortes-tbl_matriz_trans_abs-tabla').getByRole('button',
      {name: 'ACTIVO', exact: true}).click();
    table = page.locator('#cohortes-tbl_mod_matriz_total-tbl-tabla');
    await table.locator('.gt_table').waitFor();
    assert.ok((await table.innerText()).includes('Cliente de prueba 2'));
    await page.locator('.modal-footer').getByRole('button', {name: 'Cerrar', exact: true}).click();
    await page.locator('#shiny-modal').waitFor({state: 'hidden'});

    const cell = page.locator('#cohortes-tbl_resumen_mensual-tabla').getByRole('button',
      {name: /INACTIVO febrero \d{4} 3/, exact: true});
    for (let i = 0; i < 2; i++) {
      await cell.click();
      table = page.locator('#cohortes-tbl_mod_resumen_celda-tbl-tabla');
      await table.locator('.gt_table').waitFor();
      text = await table.innerText();
      assert.ok([2, 3, 4].every(n => text.includes(`Cliente de prueba ${n}`)));
      assert.ok(!text.includes('Cliente de prueba 1') && !text.includes('Cliente de prueba 5'));
      assert.equal(await page.locator('#shiny-modal').count(), 1);
      await page.locator('.modal-footer').getByRole('button', {name: 'Cerrar', exact: true}).click();
      await page.locator('#shiny-modal').waitFor({state: 'hidden'});
    }

    await page.locator('#cohortes-fil_ppto').selectOption('sin');
    await page.waitForFunction(() =>
      document.querySelector('#cohortes-kpi_vig_total-valor_display')?.textContent.trim() === '2.00');
    await page.locator('#cohortes-fil_ppto').selectOption('todos');
    await page.waitForFunction(() =>
      document.querySelector('#cohortes-kpi_vig_total-valor_display')?.textContent.trim() === '5.00');
    await page.setViewportSize({width: 390, height: 844});
    await page.locator('#cohortes-kpi_ini_total-btn_abrir').click();
    await page.locator('#cohortes-tbl_mod_ini_total-tbl-tabla .gt_table').waitFor();
    assert.equal(await page.locator('.shiny-output-error').count(), 0);
    assert.deepEqual(errors, []);
    console.log('OK: 22 tarjetas, detalles por corte/fila/celda, reapertura, XLSX, filtros y vista móvil.');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
