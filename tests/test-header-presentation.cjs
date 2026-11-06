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
    await page.goto(process.env.CRM_TEST_URL || 'http://127.0.0.1:4082');
    await page.locator('#valor-valor_display').filter({hasText:'1,234.56'}).waitFor();
    const menu = id => page.locator(`[data-rmh-select="${id}-item_click"]`);
    for (const width of [1440, 390]) {
      await page.setViewportSize({width,height:900});
      for (const id of ['MenuClientes','MenuTareas','MenuClientesAnt','MenuIndicadores']) {
        const root = menu(id);
        await root.locator('.rmh-trigger').click();
        const panel = root.locator('.rmh-panel');
        await panel.waitFor();
        if (id === 'MenuIndicadores') await root.locator('.gt_table').first().waitFor();
        else await root.locator('.rsh-list-item').first().waitFor();
        if (id === 'MenuIndicadores') check(await root.locator('.gt_table td').last().evaluate(
          el=>getComputedStyle(el).whiteSpace === 'nowrap'), 'Importe sin separar símbolo y valor');
        const bounds = await panel.boundingBox();
        check(bounds.x >= -1 && bounds.x+bounds.width <= width+1, `${id}: ancho en ${width}`);
        const overflow = await panel.evaluate(el => [...el.querySelectorAll('*'),el].filter(node =>
          ['auto','scroll'].includes(getComputedStyle(node).overflowY) && node.scrollHeight > node.clientHeight+1));
        check(overflow.length <= 1, `${id}: una única barra vertical`);
        const horizontal = await panel.evaluate(el => el.scrollWidth > el.clientWidth+1 ||
          [...el.querySelectorAll('.rsh-list-item,.gt_table')].some(node => node.scrollWidth > node.clientWidth+1));
        check(!horizontal, `${id}: sin desbordamiento horizontal`);
        if (id !== 'MenuIndicadores') {
          const row = root.locator('.rsh-list-item').first();
          const titleWidth = await row.locator('.rsh-list-title').evaluate(el=>el.getBoundingClientRect().width);
          check(titleWidth > bounds.width*.8, `${id}: título con ancho completo`);
        }
        if (id === 'MenuClientes') {
          const row = root.locator('.rsh-list-item').first();
          check(((await row.textContent()).match(/NIT:/g)||[]).length === 1, 'NIT visible una sola vez');
          const footer = root.locator('.rmh-footer');
          await footer.locator('a').waitFor();
          const before = await footer.boundingBox();
          await root.locator('.rmh-content').evaluate(el=>el.scrollTop=el.scrollHeight);
          const after = await footer.boundingBox();
          check(Math.abs(before.y-after.y)<1, 'Pie fijo durante el desplazamiento');
          await root.locator('.rmh-content').evaluate(el=>el.scrollTop=0);
        }
        if (id === 'MenuTareas') {
          const row = root.locator('.rsh-list-item').first();
          check(await row.locator('.rf-status-badge').count() === 2, 'Solo prioridad y estado como badges');
          const copy = await row.locator('.rsh-list-copy').boundingBox();
          const badges = await row.locator('.rsh-list-badges').boundingBox();
          check(badges.y >= copy.y+copy.height, 'Badges debajo del contenido');
          await row.click();
          await page.waitForFunction(()=> /\$tarea\s+\[1\] "1"/.test(document.querySelector('#state').textContent));
          check(await root.evaluate(el=>el.classList.contains('show')), 'Selección conserva menú');
        }
        if (id === 'MenuClientesAnt') {
          const row = root.locator('.rsh-list-item').first();
          check((await row.locator('.rsh-list-metadata').textContent()).includes('Actualización: 01/01/2025'),
            'Fecha de actualización en metadatos');
          check(await row.locator('.rf-status-badge').count() === 0, 'Sin pastillas de datos duplicados');
        }
        if (width === 1440 && process.env.CRM_SCREENSHOTS) await panel.screenshot({
          path:`${process.env.CRM_SCREENSHOTS}/${id}.png`});
        await root.locator('.rmh-trigger').click();
      }
    }
    check(await page.locator('.shiny-output-error').count() === 0, 'Sin errores R');
    check(errors.length === 0, errors.join('; '));
    console.log(`${checks} comprobaciones de presentación de encabezados superadas.`);
  } finally { await browser.close(); }
})().catch(error=>{console.error(error);process.exitCode=1;});
