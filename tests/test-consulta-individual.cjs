const {chromium} = require('playwright');
const assert = require('node:assert/strict');
(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport: {width: 1440, height: 1000}});
    page.setDefaultTimeout(20000);
    const errors = []; page.on('pageerror', e => errors.push(e.message));
    await page.goto(process.env.CRM_INDIVIDUAL_URL || 'http://127.0.0.1:3881');
    const idle = () => page.waitForFunction(() => window.Shiny && Shiny.shinyapp.isConnected() &&
      !document.documentElement.classList.contains('shiny-busy') &&
      (!document.querySelector('#rsh-loader-host') || document.querySelector('#rsh-loader-host').hidden));
    await page.locator('#consulta-ficha-tabla .gt_table').waitFor(); await idle();
    assert.match(await page.locator('#consulta-ficha-tabla').innerText(), /900123456/);
    assert(!/900,123,456|900\.123\.456/.test(await page.locator('#consulta-identidad').innerText()));
    assert.equal(await page.locator('#consulta-contenido a[data-value="Ejecución y productos"]').count(), 0);
    assert.match(await page.locator('#consulta-telefonos-tabla').innerText(), /111111/);
    assert(!/999999/.test(await page.locator('#consulta-telefonos-tabla').innerText()));
    const seleccionar = async valor => {
      await page.evaluate(v => {
        const el = $('#consulta-entidad'); el.selectpicker('val', v); el.trigger('change');
      }, valor); await idle();
    };
    await seleccionar('contacto:C2');
    await page.locator('#consulta-contenido a[data-value="Potencial y oportunidades"]').click();
    await page.locator('#consulta-potencial-tabla .gt_table').waitFor();
    assert.match(await page.locator('#consulta-potencial-tabla').innerText(), /100/);
    await page.locator('#consulta-contenido a[data-value="Ejecución y productos"]').click();
    await page.locator('#consulta-productos-tabla .gt_table').waitFor();
    assert.equal((await page.locator('#alcance').innerText()).trim(), '1 | 3');
    await seleccionar('principal:101');
    await page.locator('#consulta-contenido a[data-value="Ejecución y productos"]').click();
    await page.locator('#consulta-sacos-valor_display').waitFor();
    await page.waitForFunction(() => document.querySelector('#alcance').innerText.trim() === '2 | 5');
    assert.match(await page.locator('#consulta-movimientos-tabla').innerText(), /101/);
    assert(!/303/.test(await page.locator('#consulta-movimientos-tabla').innerText()));
    await page.locator('#consulta-contenido a[data-value="Pendientes"]').click();
    await page.locator('#consulta-pendientes-tabla .gt_table').waitFor();
    assert.match(await page.locator('#consulta-pendientes-tabla').innerText(), /Futuros|Por facturar/);
    await page.locator('#consulta-contenido a[data-value="Cartera"]').click();
    await page.locator('#consulta-cartera-tabla .gt_table').waitFor();
    assert.match(await page.locator('#consulta-corte_cartera').innerText(), /2026-11-05/);
    await page.locator('#consulta-contenido a[data-value="Fidelidad y valor"]').click();
    await page.locator('#consulta-rfm-tabla .gt_table').waitFor();
    await page.locator('#consulta-modelar').click(); await idle();
    assert.match(await page.locator('#consulta-modelo').innerText(), /20 clientes/);
    // El cambio de fecha actualiza los resultados sin fechas locales en el módulo.
    await page.evaluate(() => Shiny.setInputValue('fecha:shiny.date', '2026-10-31', {priority: 'event'}));
    await page.waitForFunction(() => document.querySelector('#alcance').innerText.trim() === '1 | 2');
    // Selección vacía de asesores: ningún dato de la entidad anterior permanece visible.
    await page.evaluate(() => {$('#asesor').selectpicker('val', []); $('#asesor').trigger('change');});
    await page.waitForFunction(() => document.querySelector('#consulta-identidad').innerText.includes('Seleccione'));
    assert.equal(await page.locator('#consulta-contenido .gt_table').count(), 0);
    await page.evaluate(() => {$('#asesor').selectpicker('val', ['ANA']); $('#asesor').trigger('change');});
    await page.locator('#consulta-ficha-tabla .gt_table').waitFor();
    await seleccionar('contacto:C2');
    await page.evaluate(() => Shiny.setInputValue('fallar', true, {priority: 'event'}));
    await page.locator('#consulta-contenido a[data-value="Potencial y oportunidades"]').click();
    await page.waitForFunction(() => document.querySelector('#consulta-potencial-tabla').innerText.includes('no está disponible'));
    await idle();
    await page.locator('a[data-value="Cajas de prospección"]').click();
    await page.locator('#pr_numero-valor_display').waitFor();
    assert.match(await page.locator('#pr_numero-valor_display').innerText(), /75/);
    assert.match(await page.locator('#pr_html-valor_display').innerText(), /12.*50%/);
    await page.evaluate(() => Shiny.setInputValue('fecha:shiny.date', '2026-11-09', {priority: 'event'}));
    await idle();
    await page.locator('a[data-value="Basket"]').click();
    assert.equal(await page.locator('#basket-periodo').count(), 0);
    await page.locator('#basket-analizar').click(); await idle();
    await page.locator('#basket-tabla-tabla .gt_table').waitFor();
    assert(!/Revise soporte/.test(await page.locator('#basket-estado').innerText()));
    await page.setViewportSize({width: 390, height: 844}); await idle();
    assert(await page.locator('#basket-analizar').isVisible());
    assert.deepEqual(errors, []);
    console.log('OK: consulta sin ventas, lead, cuenta consolidada, pendientes, cartera, permisos y Basket relajado.');
  } finally {await browser.close();}
})().catch(e => {console.error(e); process.exitCode = 1;});
