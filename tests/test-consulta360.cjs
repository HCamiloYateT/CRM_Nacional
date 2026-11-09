const {chromium} = require('playwright');
const assert = require('node:assert/strict');
(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport: {width: 1440, height: 1000}});
    page.setDefaultTimeout(30000);
    const errors = []; page.on('pageerror', e => errors.push(e.message));
    await page.goto(process.env.CRM_CONSULTA360_URL || 'http://127.0.0.1:3901');
    const idle = () => page.waitForFunction(() => window.Shiny && Shiny.shinyapp.isConnected() &&
      !document.documentElement.classList.contains('shiny-busy') &&
      (!document.querySelector('#rsh-loader-host') || document.querySelector('#rsh-loader-host').hidden));
    await page.locator('#consulta-ficha-tabla .gt_table').waitFor(); await idle();
    const elegir = async nit => {
      await page.evaluate(v => {$('#consulta-entidad').selectpicker('val', v).trigger('change');}, nit);
      await idle();
    };
    await elegir('nit:102');
    await page.waitForFunction(() => document.querySelector('#consulta-identidad').innerText.includes('SEDE NORTE'));
    assert.match(await page.locator('#consulta-identidad').innerText(), /Con presupuesto 2026/);
    assert.match(await page.locator('#consulta-identidad').innerText(), /Cuenta hija de CASA MATRIZ/);
    await page.locator('#consulta-contenido a[data-value="Ficha"]').click();
    await page.locator('#consulta-contacto_sistema-tabla .gt_table').waitFor();
    const contacto = await page.locator('#consulta-contacto_sistema-tabla').innerText();
    assert.match(contacto, /222222/); assert(!/111111/.test(contacto));
    assert.match(await page.locator('#consulta-geografia-tabla').innerText(), /Medellín/);
    assert.match(await page.locator('#consulta-antiguedad-tabla').innerText(), /2015-06-01/);
    assert.match(await page.locator('#consulta-antiguedad-tabla').innerText(), /11 años/);
    await page.locator('#consulta-contenido a[data-value="Potencial y oportunidades"]').click();
    await page.locator('#consulta-oportunidades-tabla .gt_table').waitFor();
    assert.match(await page.locator('#consulta-oportunidades-tabla').innerText(), /2024/);
    await page.locator('#consulta-contenido a[data-value="Ejecución y productos"]').click();
    await page.waitForFunction(() => document.querySelector('#alcance').innerText.trim() === '1 | 3');
    await page.locator('#consulta-contenido a[data-value="Histórico completo"]').click();
    await page.locator('#consulta-historico_anual-tabla .gt_table').waitFor();
    const anual = await page.locator('#consulta-historico_anual-tabla').innerText();
    for (const year of ['2023', '2024', '2025', '2026']) assert(anual.includes(year));
    assert(!/2,023|2,024|2,025/.test(anual));
    await page.waitForSelector('#consulta-historico_grafico.js-plotly-plot,#consulta-historico_grafico .js-plotly-plot');
    await page.locator('#consulta-contenido a[data-value="Preferencias de compra"]').click();
    await page.locator('#consulta-preferencias-tabla .gt_table').waitFor();
    assert.match(await page.locator('#consulta-preferencias-tabla').innerText(), /B/);
    await page.locator('#consulta-contenido a[data-value="Fidelidad y valor"]').click();
    await page.locator('#consulta-rfm-tabla .gt_table').waitFor();
    await page.locator('#consulta-modelo .gt_table').waitFor();
    assert.match(await page.locator('#consulta-modelo').innerText(), /Margen estimado/);
    assert(!/Pulse Estimar/.test(await page.locator('#consulta-modelo').innerText()));
    await page.locator('#consulta-contenido a[data-value="Venta cruzada"]').click();
    await page.locator('#consulta-venta_cruzada .gt_table').waitFor();
    assert.match(await page.locator('#consulta-venta_cruzada').innerText(), /Café.*C|C/);
    assert.match(await page.locator('#consulta-venta_cruzada').innerText(), /Ya registrada/);
    await page.locator('#consulta-contenido a[data-value="Gestión y seguimiento"]').click();
    await page.locator('#consulta-cronologia-tabla .gt_table').waitFor();
    const cronologia = await page.locator('#consulta-cronologia-tabla').innerText();
    assert.match(cronologia, /2024/); assert.match(cronologia, /Cambio de etapa/);
    await elegir('principal:101');
    await page.waitForFunction(() => document.querySelector('#consulta-identidad').innerText.includes('Cuenta padre'));
    assert.match(await page.locator('#consulta-identidad').innerText(), /1 cuenta hija visible/);
    await elegir('nit:500');
    await page.waitForFunction(() => document.querySelector('#consulta-identidad').innerText.includes('Sin presupuesto'));
    await page.setViewportSize({width: 390, height: 844}); await idle();
    await page.locator('#consulta-contenido a[data-value="Ficha"]').click();
    await page.locator('#consulta-antiguedad-tabla .gt_table').waitFor();
    assert.deepEqual(errors, []);
    await page.screenshot({path: '/tmp/consulta360-final.png', fullPage: true});
    console.log('OK: presupuesto, familia, contacto/geografía, antigüedad, historia anterior, RFM/CLV por hija y venta cruzada.');
  } finally {await browser.close();}
})().catch(e => {console.error(e); process.exitCode = 1;});
