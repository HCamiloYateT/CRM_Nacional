const assert = require('node:assert/strict');
const { chromium } = require('playwright');
(async () => {
  const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH,
    args:process.env.CI ? ['--no-sandbox'] : []});
  let checks = 0;
  const check = (condition, message) => { assert.ok(condition, message); checks++; };
  try {
    const page = await browser.newPage({viewport:{width:1440,height:1000}}), errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(process.env.CRM_TEST_URL || 'http://127.0.0.1:4080');
    await page.locator('#valor-valor_display').filter({hasText:'1,234.56'}).waitFor();
    check(await page.locator('.main-header .navbar-right > li.rmh-menu').count() === 5, 'Cinco li directos');
    check(await page.locator('.shiny-output-error').count() === 0, 'UI sin errores de R');
    check(await page.locator('#legacy-valor_display').textContent() === '1,234.56', 'Alias anterior renderiza');
    await page.locator('#cantidad').fill('0');
    await page.locator('#cantidad').press('Tab');
    await page.waitForFunction(() => document.querySelector('#valor-valor_display').textContent === '0.00');
    check(await page.locator('#legacy-valor_display').textContent() === '0.00', 'Ambos módulos actualizan cero');
    await page.locator('#valor-detalle').click();
    await page.waitForFunction(() => /\$detalle\s+\[1\] 1/.test(document.querySelector('#state').textContent));
    check(!/\$legacy\s+\[1\] 1/.test(await page.locator('#state').textContent()), 'Clics independientes');
    const clients = page.locator('[data-rmh-select="MenuClientes-item_click"]');
    await clients.locator('.rmh-trigger').click();
    await clients.locator('.rsh-list-item').first().click();
    await page.locator('.rf-modal-detail.show').waitFor();
    check(await clients.evaluate(el=>el.classList.contains('show')), 'Lista real del CRM conserva menú durante modal');
    check((await page.locator('.rf-modal-detail').textContent()).includes('900123'), 'La clave del cliente llega al servidor');
    await page.locator('#activities input[value="review"]').check();
    await page.waitForFunction(()=>document.querySelector('#state').textContent.includes('review'));
    check(true, 'Checklist transmite claves');
    await page.locator('.modal.show [data-dismiss="modal"]').click();
    await page.waitForFunction(()=>!document.querySelector('.modal.show'));
    await clients.locator('.rmh-trigger').click();
    await page.locator('[data-widget="control-sidebar"]').click();
    const sidebar = page.locator('aside.control-sidebar');
    await sidebar.locator('.nav-link').filter({hasText:'Notas'}).click();
    await page.locator('#CrearNota-NOT_Titulo').fill('Revisión Café & Cía.');
    await page.waitForFunction(()=>document.querySelector('#state').textContent.includes('Revisión Café & Cía.'));
    check(true, 'Input real de notas transmite al servidor');
    await sidebar.locator('.nav-link').filter({hasText:'Cotizador'}).click();
    check(await page.locator('#Cotizador-COT_FechaIni').count() > 0, 'Cotizador conserva fecha y namespace');
    await sidebar.locator('.nav-link').filter({hasText:'Competencia'}).click();
    check(await page.locator('#RegistrarCompetencia-COMP_Guardar').isVisible(), 'Formulario de competencia visible');
    await page.locator('[data-widget="control-sidebar"]').click();
    check((await page.locator('.cot-resumen-tabla').textContent()).includes('15,425'), 'Tabla y total corporativos');
    check(await page.locator('.cot-resumen-tabla script').count() === 0, 'Producto con HTML queda escapado');
    const duplicates = await page.evaluate(()=>{
      const counts = new Map(); document.querySelectorAll('[id]').forEach(el=>counts.set(el.id,(counts.get(el.id)||0)+1));
      return [...counts].filter(([id,n])=>id && n>1);
    });
    assert.deepEqual(duplicates, []); checks++;
    check(errors.length === 0, errors.join('; '));
    console.log(`${checks} comprobaciones de navegador del CRM superadas, sin servidores ni operaciones BD del CRM.`);
  } finally { await browser.close(); }
})().catch(error=>{console.error(error);process.exitCode=1;});
