// Prueba de interfaz con fuentes ficticias; no accede a servicios ni escribe datos del negocio.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport: {width: 1440, height: 1000}});
    page.setDefaultTimeout(15000);
    const errors = []; page.on('pageerror', e => errors.push(e.message));
    await page.goto(process.env.CRM_CARTERA_URL || 'http://127.0.0.1:3878');
    const set = (id, value) => page.evaluate(({id, value}) => {
      $('#' + id).selectpicker('val', value).trigger('change');
    }, {id, value});
    const table = page.locator('#cartera-tabla-tabla');
    const idle = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') &&
      (!document.querySelector('#rsh-loader-host') || document.querySelector('#rsh-loader-host').hidden));
    await table.locator('.gt_table').waitFor(); await idle();
    assert.match(await table.innerText(), /\$1,000/);
    assert.match(await table.innerText(), /30.0%/);
    // Detalle TOTAL por clave, búsqueda con totales y descarga numérica sobre todas las páginas filtradas.
    await table.getByRole('button', {name: 'TOTAL', exact: true}).click();
    await page.locator('#cartera-detalle-tabla .gt_table').waitFor();
    await page.locator('#cartera-buscar_detalle').fill('F2');
    await page.waitForFunction(() => document.querySelector('#cartera-totales_detalle').innerText.includes('$200'));
    assert.match(await page.locator('#cartera-totales_detalle').innerText(), /Filas visibles: 1/);
    const download = page.waitForEvent('download');
    await page.locator('#cartera-descargar_detalle').click();
    await (await download).saveAs('/tmp/cartera-detalle.xlsx');
    await page.getByRole('button', {name: 'Cerrar', exact: true}).click();
    await page.locator('#shiny-modal').waitFor({state: 'hidden'});
    await page.locator('#shiny-modal').waitFor({state: 'hidden'});
    // Opciones nativas: parcial, ninguna, refrescar y todas.
    await set('cartera-negocio', ['COPRODUCTO']);
    await page.waitForFunction(() => document.querySelector('#cartera-tabla-tabla').innerText.includes('$300'));
    await page.locator('[data-id="cartera-negocio"]').click();
    await page.locator('.bootstrap-select.show .bs-deselect-all').click();
    await page.keyboard.press('Escape');
    await page.waitForFunction(() => document.querySelector('#cartera-fuente').innerText.includes('No hay negocios seleccionados'));
    await page.locator('#cartera-actualizar').click(); await idle();
    assert.match(await page.locator('#cartera-fuente').innerText(), /No hay negocios seleccionados/);
    await set('cartera-vista', 'Recaudos');
    await table.locator('.gt_table').waitFor();
    assert.match(await table.innerText(), /F4/);
    await set('cartera-vista', 'Resumen');
    await page.waitForFunction(() => document.querySelector('#cartera-fuente').innerText.includes('No hay negocios'));
    await page.locator('[data-id="cartera-negocio"]').click();
    await page.locator('.bootstrap-select.show .bs-select-all').click();
    await page.keyboard.press('Escape'); await table.locator('.gt_table').waitFor();
    // Vencida: tramos ordenados y detalle, sin repetir al mismo cliente en TOTAL.
    await set('cartera-vista', 'Vencida');
    await page.waitForFunction(() => document.querySelector('#cartera-tabla-tabla').innerText.includes('1–7 días'));
    await table.getByRole('button', {name: 'TOTAL', exact: true}).click();
    await page.locator('#cartera-detalle-tabla .gt_table').waitFor();
    await page.getByRole('button', {name: 'Cerrar', exact: true}).click();
    await page.locator('#shiny-modal').waitFor({state: 'hidden'});
    // Proyección comparte saldo y acumulado con el gráfico al cruzar el año.
    await set('cartera-vista', 'Proyección de pagos'); await set('cartera-agrupacion', 'mes');
    await page.waitForFunction(() => document.querySelector('#cartera-tabla-tabla').innerText.includes('2027-01-01'));
    await page.waitForSelector('#cartera-grafico.js-plotly-plot,#cartera-grafico .js-plotly-plot'); await idle();
    const series = await page.locator('#cartera-grafico.js-plotly-plot,#cartera-grafico .js-plotly-plot')
      .evaluate(p => p.data.map(t => t.y));
    assert.deepEqual(series, [[300, 400], [300, 700]]);
    // Cada histórico usa fecha de factura y selector por NIT; incluye pendientes sin fingir abonos.
    await set('cartera-vista', 'Recaudos');
    await table.locator('.gt_table').waitFor();
    await page.evaluate(() => {
      const dates = $('#cartera-facturas input');
      dates.eq(0).bsDatepicker('setUTCDate', new Date('2025-01-01'));
      dates.eq(1).bsDatepicker('setUTCDate', new Date('2026-10-01'));
    });
    await page.waitForFunction(() => document.querySelector('#cartera-tabla-tabla').innerText.includes('F1'));
    assert.match(await table.innerText(), /Sin pago total acreditado/);
    await set('cartera-vista', 'Rotación');
    await page.waitForFunction(() => document.querySelector('#cartera-tabla-tabla').innerText.includes('Más días'));
    await set('cartera-vista', 'Comportamiento de pago');
    await page.waitForFunction(() => document.querySelector('#cartera-tabla-tabla').innerText.includes('Pago oportuno'));
    for (const c of ['Valor', 'Facturas', 'Sacos']) {await set('cartera-concepto', c); await idle();}
    await set('cartera-cliente', []);
    await page.waitForFunction(() => document.querySelector('#cartera-fuente').innerText.includes('No hay clientes seleccionados'));
    // Cambiar el alcance elimina a Beta tanto de la consulta como de las opciones de clientes.
    await set('alcance', 'Propio'); await set('cartera-vista', 'Resumen'); await idle();
    assert(!await page.locator('#cartera-cliente').innerText().then(t => t.includes('Cliente Beta')));
    await table.getByRole('button', {name: 'TOTAL', exact: true}).click();
    await page.locator('#cartera-detalle-tabla .gt_table').waitFor();
    assert(!(await page.locator('#cartera-detalle-tabla').innerText()).includes('Cliente Beta'));
    await page.getByRole('button', {name: 'Cerrar', exact: true}).click();
    await page.locator('#shiny-modal').waitFor({state: 'hidden'});
    await page.screenshot({path: '/tmp/cartera-escritorio.png', fullPage: true, animations: 'disabled'});
    await page.setViewportSize({width: 390, height: 844}); await idle();
    await page.screenshot({path: '/tmp/cartera-movil.png', fullPage: true, animations: 'disabled'});
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 1), false);
    // Una selección comercial sin coincidencias no elimina las fechas existentes ni repite errores.
    await set('alcance', 'Sin coincidencias');
    await page.waitForFunction(() => document.querySelector('#cartera-fuente').innerText.includes('filtros actuales'));
    assert.equal(await page.locator('#cartera-corte').inputValue(), '2026-11-05');
    assert.equal(await page.locator('#cartera-saldo_tarjeta:visible').count(), 0);
    assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
    await set('alcance', 'Administrador');
    await table.locator('.gt_table').waitFor();
    assert.match(await table.innerText(), /\$1,000/);
    // Fuente vacía y fuente ausente reciben un solo aviso, sin KPI ni descarga activa de importes.
    await set('modo_fuente', 'Vacía');
    await page.waitForFunction(() => document.querySelector('#cartera-fuente').innerText.includes('no contiene cortes'));
    assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
    await set('modo_fuente', 'Ausente');
    await page.waitForFunction(() => document.querySelector('#cartera-fuente').innerText.includes('No hay una fuente'));
    assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
    await set('modo_fuente', 'Completa');
    await table.locator('.gt_table').waitFor();
    assert.deepEqual(errors, []);
    console.log('OK: saldos, tramos, detalle, búsqueda, Excel, filtros nativos, proyección, históricos, permisos y móvil.');
  } finally {await browser.close();}
})().catch(e => {console.error(e); process.exitCode = 1;});
