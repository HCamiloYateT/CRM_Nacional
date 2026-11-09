const {chromium} = require('playwright');
const assert = require('node:assert/strict');
(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport: {width: 1366, height: 1000}});
    page.setDefaultTimeout(20000);
    const errors = []; page.on('pageerror', e => errors.push(e.message));
    await page.goto(process.env.CRM_AJUSTES_URL || 'http://127.0.0.1:3891');
    const idle = () => page.waitForFunction(() => window.Shiny && Shiny.shinyapp.isConnected() &&
      !document.documentElement.classList.contains('shiny-busy') &&
      (!document.querySelector('#rsh-loader-host') || document.querySelector('#rsh-loader-host').hidden));
    await page.locator('#consulta-ficha-tabla .gt_table').waitFor(); await idle();
    const opciones = await page.locator('#consulta-entidad option').evaluateAll(xs => xs.map(x => x.value));
    assert(opciones.includes('nit:101') && opciones.includes('nit:102') && opciones.includes('principal:101'));
    assert(!opciones.some(x => x.startsWith('contacto:')));
    assert.equal(await page.locator("#obligatorio").inputValue(), "");
    assert.equal(await page.locator("#opcional").inputValue(), "");
    const none = await page.locator('select option').evaluateAll(xs => xs.some(x => /^ningun[oa]$/i.test(x.text)));
    assert.equal(none, false);
    assert(await page.locator('.crm-identidad-badges .badge').count() >= 4);
    // Bootstrap 4: los tabs contienen nav-item/nav-link, sin enlaces pegados ni pestañas vacías.
    assert(await page.locator('#consulta-contenido .nav-item .nav-link').count() >= 2);
    const vacias = await page.locator('#consulta-contenido .nav-link').evaluateAll(xs => xs.some(x => !x.textContent.trim()));
    assert.equal(vacias, false);
    await page.locator('a[data-value="Basket"]').click();
    const guia = page.getByText('Qué es Market Basket y cómo usar este tablero', {exact: true});
    await guia.click();
    const tipografia = await page.locator('.crm-basket-guia .rf-notice-list').evaluate(el => getComputedStyle(el).fontSize);
    assert(parseFloat(tipografia) >= 13);
    const notas = await page.locator('.crm-basket-guia').innerText();
    assert(!/\n1\.\n|\n2\.\n/.test(notas));
    await page.locator('a[data-value="Clasificación"]').click();
    await page.locator('#clasificacion-referencias-tabla .gt_table').waitFor();
    assert.match(await page.locator('#clasificacion-referencias-tabla').innerText(), /CAFÉ NUEVO/);
    await page.locator('#clasificacion-referencias-tabla .crm-tabla-enlace').filter({hasText: 'RACAFÉ'}).click();
    await page.locator('#clasificacion-producto').waitFor(); await idle();
    assert.equal(await page.locator('#clasificacion-categoria').inputValue(), 'REGIONAL');
    assert.equal(await page.locator('#clasificacion-producto').inputValue(), 'HUILA');
    await page.locator('#clasificacion-guardar').click();
    await page.locator('#clasificacion-confirmar').waitFor();
    assert.match(await page.locator('.modal').innerText(), /REGIONAL.*HUILA/);
    await page.locator('#clasificacion-cancelar').click(); await idle();
    assert.match(await page.locator('#clasificacion-referencias-tabla').innerText(), /CAFÉ NUEVO/);
    await page.locator('#clasificacion-guardar').click();
    await page.locator('#clasificacion-confirmar').click(); await idle();
    await page.waitForFunction(() => document.querySelector('#clasificacion-referencias-tabla').innerText.includes('Sin registros'));
    assert.equal(await page.locator('#clasificacion-edicion button').count(), 0);
    await page.getByText('Prueba del menú de detalle', {exact: true}).click();
    await page.locator('.rc-boton-acciones').click();
    const panel = page.locator('#detalle-acc_C1');
    assert(await panel.isVisible());
    assert(await panel.evaluate(el => el.parentNode === document.body));
    await panel.locator('button').hover();
    await page.locator('.tooltip.show').waitFor();
    const nivel = await page.locator('.tooltip.show').evaluate(el => Number(getComputedStyle(el).zIndex));
    assert(nivel > 99999);
    await page.setViewportSize({width: 390, height: 844});
    await page.locator('a[data-value="Consulta"]').click(); await idle();
    const bodyWidth = await page.evaluate(() => document.documentElement.scrollWidth);
    assert(bodyWidth <= 410, `Desborde horizontal de ${bodyWidth}px`);
    assert.deepEqual(errors, []);
    await page.screenshot({path: '/tmp/ajustes-comerciales.png', fullPage: true});
    console.log('OK: búsqueda por cuentas hijas, badges, tabs bs4Dash, sin Ninguno, guía legible y guardar clasificación.');
  } finally {await browser.close();}
})().catch(e => {console.error(e); process.exitCode = 1;});
