const assert = require('node:assert/strict');
const {chromium} = require('playwright');
(async()=>{
  const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH,
    args:process.env.CI ? ['--no-sandbox'] : []});
  let checks = 0;
  const check=(value,message)=>{assert.ok(value,message);checks++;};
  try {
    const page=await browser.newPage({viewport:{width:1440,height:1000}}), errors=[];
    page.on('pageerror',error=>errors.push(error.message));
    await page.goto(process.env.CRM_TEST_URL || 'http://127.0.0.1:4092');
    await page.locator('#default_table .gt_table').waitFor();
    const styles=async id=>page.locator(`#${id} .gt_table`).evaluate(table=>{
      const headers=[...table.querySelectorAll('thead th')], rows=[...table.querySelectorAll('tbody tr')];
      const pick=el=>{const s=getComputedStyle(el);return {background:s.backgroundColor,align:s.textAlign,
        weight:s.fontWeight,top:s.borderTopWidth,topStyle:s.borderTopStyle,topColor:s.borderTopColor,
        bottom:s.borderBottomWidth,bottomStyle:s.borderBottomStyle,bottomColor:s.borderBottomColor,
        left:s.borderLeftStyle,right:s.borderRightStyle,padding:s.paddingTop};};
      return {headerLine:pick(table.querySelector("thead tr:last-child")),headers:headers.map(pick),rows:rows.map(row=>[...row.querySelectorAll('td')].map(pick)),
        width:table.getBoundingClientRect().width,parent:table.parentElement.getBoundingClientRect().width};
    });
    const def=await styles('default_table'), custom=await styles('custom_table'), old=await styles('existing_table');
    check(def.headerLine.bottom==='1px' && def.headerLine.bottomColor==='rgb(218, 41, 28)' && def.headerLine.bottomStyle==='solid',
      'Línea roja continua de 1px bajo todos los encabezados');
    check(def.headers.every(h=>h.topStyle==='none' && h.left==='none' && h.right==='none'), 'Encabezado sin otros bordes');
    check(def.rows[0][0].topStyle==='none' && def.rows[0][0].bottomStyle==='none', 'Cuerpo sin cuadrícula');
    check(def.rows.flat().every(c=>c.left==='none' && c.right==='none'), 'Sin bordes verticales');
    check(def.rows[0][0].align==='left' && def.rows[0][1].align==='right', 'Alineación automática texto/cifras');
    check(def.rows[2].every(c=>Number(c.weight)>=600), 'Fila Total en negrita');
    check(def.rows[2].every(c=>c.top==='1px' && c.topColor==='rgb(221, 225, 230)'), 'Fila Total con línea gris superior');
    check(def.rows[2].every(c=>c.background==='rgb(245, 246, 247)'), 'Fondo uniforme de la fila Total');
    check(def.rows.every(row=>row[2].background==='rgb(245, 246, 247)') &&
      def.headers[2].background==='rgb(245, 246, 247)', 'Columna Total incluye encabezado');
    check(def.rows[2][2].background===def.rows[2][0].background, 'Intersección de totales sin oscurecer');
    check(Math.abs(def.width-def.parent)<2, 'Ancho del contenedor por defecto');
    check(custom.headerLine.bottom==='3px' && custom.headerLine.bottomColor==='rgb(36, 90, 129)', 'Color y grosor parametrizables');
    check(custom.rows[0][1].align==='center' && custom.rows[0][1].padding==='9px', 'Alineación y espaciado configurables');
    check(old.headers.every(h=>h.bottomStyle==='none'), 'La función anterior conserva sus bordes');
    const clients=page.locator('[data-rmh-select="MenuClientes-item_click"]');
    async function openClient(index){
      if(!await clients.evaluate(el=>el.classList.contains('show'))) await clients.locator('.rmh-trigger').click();
      await clients.locator('.rsh-list-item').nth(index).click();
      await page.locator('.modal.show #header_cli_pedidos-valor_display').filter({hasText:index===0?'2':'5'}).waitFor();
    }
    async function closeModal(){await page.locator('#header_cliente_cerrar').click();
      await page.waitForFunction(()=>!document.querySelector('.modal.show'));}
    await openClient(0);
    const modal=page.locator('.modal.show'), dialog=await modal.locator('.modal-dialog').boundingBox();
    check(dialog.width>=1100 && dialog.width<=1201, 'Modal ampliado hasta 1200px');
    check(await modal.locator('.racafe-caja-valor').count()===6, 'Seis módulos de indicadores');
    const cards=await modal.locator('.racafe-caja-valor').evaluateAll(nodes=>nodes.map(el=>{
      const style=getComputedStyle(el), icon=el.querySelector('i');
      return {border:style.borderLeftColor,width:style.borderLeftWidth,top:style.borderTopWidth,
        icon:!!icon,color:icon&&getComputedStyle(icon).color};}));
    const heights=await modal.locator('.racafe-caja-valor').evaluateAll(nodes=>nodes.map(el=>el.getBoundingClientRect().height));
    check(Math.max(...heights)-Math.min(...heights)<2, 'Cajas de altura uniforme');
    check(Math.max(...heights)<100, 'Cajas compactas de menos de 100px');
    check(cards.every(c=>c.border==='rgb(218, 41, 28)' && c.width==='3px' && c.top==='1px' && c.icon && c.color==='rgb(218, 41, 28)'),
      'Todas las cajas con borde lateral e icono rojos');
    check(await modal.locator('.rmcv-footer .rmcv-button').count()===3, 'Tres acciones con detalle; las otras cajas son informativas');
    await modal.locator('#header_cli_pedidos-detalle').click();
    await page.locator('.modal.show .modal-title').filter({hasText:'Detalle: Pedidos'}).waitFor();
    check(await page.locator('.modal.show .gt_table').count()===1, 'Botón abre el detalle operativo');
    await page.locator('#header_cliente_volver').click();
    await page.locator('#header_cli_pedidos-valor_display').filter({hasText:'2'}).waitFor();
    check((await modal.textContent()).includes('Creación: 20/10/2026'), 'Conserva fecha de creación');
    await modal.locator('#header_detalle_pedidos > summary').click();
    const detail=modal.locator('#header_detalle_pedidos .gt_table');
    await detail.waitFor();
    check(await detail.evaluate(el=>el.parentElement.scrollWidth>el.parentElement.clientWidth), 'Detalle con scroll horizontal');
    check(await modal.locator('.modal-body').evaluate(el=>el.scrollWidth<=el.clientWidth+1), 'Sin desbordamiento del modal');
    if(process.env.CRM_SCREENSHOTS) await modal.screenshot({path:`${process.env.CRM_SCREENSHOTS}/cliente-desktop.png`});
    await closeModal();
    await openClient(1);
    check((await page.locator('#header_cli_lotes-valor_display').textContent())==='5', 'Los módulos cambian de cliente');
    check((await page.locator('#header_cli_produccion-valor_display').textContent())==='5.00', 'Actualiza cantidades reactivas');
    await closeModal();
    const older=page.locator('[data-rmh-select="MenuClientesAnt-item_click"]');
    await older.locator('.rmh-trigger').click();await older.locator('.rsh-list-item').first().click();
    await page.locator('.modal.show #resumen_cli_ant_modal .gt_table').waitFor();
    check((await page.locator('#resumen_cli_ant_modal').textContent()).includes('Historial comercial'), 'Historial usa la tabla nueva');
    await closeModal();
    await page.setViewportSize({width:390,height:844});
    await openClient(0);
    const mobile=await page.locator('.modal.show .modal-dialog').boundingBox();
    check(mobile.width<=390 && mobile.x>=0, 'Modal adaptable en móvil');
    await page.locator('.modal.show #header_detalle_pedidos > summary').click();
    check(await page.locator('.modal.show .modal-body').evaluate(el=>el.scrollWidth<=el.clientWidth+1), 'Móvil sin scroll horizontal exterior');
    const duplicates=await page.evaluate(()=>{const ids=new Map();document.querySelectorAll('[id]').forEach(el=>
      ids.set(el.id,(ids.get(el.id)||0)+1));return [...ids].filter(([id,n])=>id&&n>1);});
    assert.deepEqual(duplicates,[]);checks++;
    check(await page.locator('.shiny-output-error').count()===0, 'Sin errores R');
    check(errors.length===0,errors.join('; '));
    console.log(`${checks} comprobaciones de Vancouver y modal de clientes superadas.`);
  } finally {await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
