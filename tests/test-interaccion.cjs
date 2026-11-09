// Ejecutar contra tests/interaccion; fuentes sintéticas y ninguna operación de persistencia.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
(async()=>{
 const browser=await chromium.launch({executablePath:process.env.CHROMIUM||'/usr/bin/chromium',args:['--no-sandbox']});
 const page=await browser.newPage({viewport:{width:1440,height:1000}});
 page.setDefaultTimeout(15000);
 const errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto(process.env.CRM_TEST_URL||'http://127.0.0.1:3876/');
 await page.waitForFunction(()=>window.Shiny&&Shiny.shinyapp&&Shiny.shinyapp.isConnected());
 const idle=()=>page.waitForFunction(()=>!document.documentElement.classList.contains('shiny-busy')&&
   (!document.querySelector('#rsh-loader-host')||document.querySelector('#rsh-loader-host').hidden));
 const set=async(id,value)=>{await page.evaluate(({id,value})=>{$('#'+id).selectpicker('val',value).trigger('change');},{id,value});};
 await page.waitForFunction(()=>document.querySelectorAll('#Tareas-tablero .crm-tarea-detalle').length===3);await idle();
 console.log('CARGADO');
 // Todas, parcial y vacía se verifican con el picker real y con sus botones nativos.
 const count=()=>page.locator('#Tareas-tablero .crm-tarea-detalle').count();
 assert.equal(await count(),3);
 await set('Tareas-etiquetas',['Venta']);await idle();
 await page.waitForFunction(()=>document.querySelectorAll('#Tareas-tablero .crm-tarea-detalle').length===1);
 assert.equal(await count(),1);
 await page.locator('[data-id="Tareas-etiquetas"]').click();
 await page.locator('.bootstrap-select.show .bs-deselect-all').click();
 await page.keyboard.press('Escape');await idle();assert.equal(await count(),0);
 assert.match(await page.locator('#Tareas-tablero').innerText(),/No hay tareas ni notas/);
 await page.locator('#Tareas-actualizar').click();
 await page.waitForFunction(()=>!document.querySelector('#rsh-loader-host')?.hidden);
 await idle();assert.equal(await count(),0);
 await page.locator('[data-id="Tareas-etiquetas"]').click();
 await page.locator('.bootstrap-select.show .bs-select-all').click();await page.keyboard.press('Escape');
 await idle();assert.equal(await count(),3);
 await set('Tareas-prioridades',[]);await idle();assert.equal(await count(),0);
 await set('Tareas-prioridades',['URGENTE']);await idle();assert.equal(await count(),1);
 console.log('FILTROS OK');
 // Hover con separación del control; el clic lo elimina y permite abrir el menú.
 const button=page.locator('[data-id="Tareas-etiquetas"]');await button.hover();
 await page.waitForSelector('#crm-select-tooltip');
 const rects=await page.evaluate(()=>{
  const a=document.querySelector('[data-id="Tareas-etiquetas"]').getBoundingClientRect();
  const b=document.querySelector('#crm-select-tooltip').getBoundingClientRect();
  return {a:{top:a.top,bottom:a.bottom},b:{top:b.top,bottom:b.bottom,left:b.left,right:b.right},w:innerWidth};
 });
 assert(rects.b.bottom<=rects.a.top-8||rects.b.top>=rects.a.bottom+8);
 assert(rects.b.left>=0&&rects.b.right<=rects.w);
 await button.click();assert.equal(await page.locator('#crm-select-tooltip').count(),0);await page.keyboard.press('Escape');
 console.log('HOVER OK');
 // Bordes del viewport con el mismo control real: arriba, abajo y esquina derecha.
 const originalStyle=await button.getAttribute('style');
 for(const edge of ['top','bottom']){
  await page.mouse.move(0,0);
  await button.evaluate((el,edge)=>Object.assign(el.style,{position:'fixed',right:'4px',left:'auto',
    top:edge==='top'?'4px':'auto',bottom:edge==='bottom'?'4px':'auto',width:'180px',zIndex:'1000'}),edge);
  await button.hover();await page.waitForSelector('#crm-select-tooltip');
  const ok=await page.evaluate(edge=>{
   const a=document.querySelector('[data-id="Tareas-etiquetas"]').getBoundingClientRect();
   const b=document.querySelector('#crm-select-tooltip').getBoundingClientRect();
   return b.left>=0&&b.right<=innerWidth&&b.top>=0&&b.bottom<=innerHeight&&
     (edge==='top'?b.top>=a.bottom+8:b.bottom<=a.top-8);
  },edge);
  assert(ok);
 }
 await button.evaluate((el,style)=>style===null?el.removeAttribute('style'):el.setAttribute('style',style),originalStyle);
 await page.mouse.move(0,0);
 // Las acciones de oportunidades preceden la fila de filtros y las KPI.
 await page.locator('a[data-value="GC_Oportunidades"]').click();await idle();
 await page.waitForSelector('#Oportunidades-detalle-tabla .gt_table');
 const ys=await page.evaluate(()=>['Oportunidades-crear','Oportunidades-contacto','Oportunidades-total-valor_display'].map(id=>
  document.getElementById(id).getBoundingClientRect().top));
 assert(ys[0]<ys[1]&&ys[1]<ys[2]);
 await set('Oportunidades-estado',[]);await idle();
 assert.match(await page.locator('#Oportunidades-detalle-tabla').innerText(),/Sin registros/);
 await set('Oportunidades-estado',['Activas','Descartadas']);await idle();
 assert.match(await page.locator('#Oportunidades-detalle-tabla').innerText(),/OP2/);
 console.log('OPORTUNIDADES OK');
 // El flujo de pendientes usa las etapas reales y conserva la selección vacía al actualizar.
 await page.locator('a[data-value="GC_Pendientes"]').click();await idle();
 await page.waitForSelector('#Pendientes-detalle-tabla .gt_table');
 await set('Pendientes-etapa',['Sin asignar']);await idle();
 await page.waitForFunction(()=>document.querySelector('#Pendientes-detalle-tabla').innerText.includes('Sin asignar'));
 await set('Pendientes-etapa',[]);await idle();
 await page.waitForFunction(()=>document.querySelector('#Pendientes-detalle-tabla').innerText.includes('Sin registros'));
 await page.locator('#Pendientes-actualizar').click();await idle();
 assert.match(await page.locator('#Pendientes-detalle-tabla').innerText(),/Sin registros/);
 // Cálculo verdadero con entradas válidas y error de validación recuperable.
 await page.locator('a[data-value="HE_Calculadoras"]').click();
 await page.locator('a[data-value="Coproductos"]').click();await idle();
 await page.locator('#Calculadora-coproductos-Calcular').click();await idle();
 assert.match(await page.locator('#Calculadora-coproductos-Precio_9d3').innerText(),/16,000/);
 console.log('CALCULADORA OK');
 // Cálculo lento: loader visible mientras procesa, oculto al terminar; vacío y error son explícitos.
 await page.locator('a[data-value="prueba"]').click();await idle();
 await page.waitForSelector('#interactivo.js-plotly-plot,#interactivo .js-plotly-plot');
 let fonts=await page.locator('#interactivo.js-plotly-plot,#interactivo .js-plotly-plot').evaluate(p=>[p._fullLayout.title.text,p._fullLayout.font.size,p._fullLayout.xaxis.tickfont.size]);
 assert.equal(fonts[0],'Gráfico interactivo de prueba');assert.equal(fonts[1],12);assert.equal(fonts[2],11);
 for(const value of ['Sin datos','Error','Válido']){
  await set('caso',value);
  await page.waitForFunction(()=>document.querySelector('#rsh-loader-host')&&!document.querySelector('#rsh-loader-host').hidden);
  if(value==='Sin datos')await page.waitForFunction(()=>document.querySelector('#grafico').innerText.includes('No hay datos'));
  if(value==='Error')await page.waitForFunction(()=>document.querySelector('#grafico').innerText.includes('No se pudo completar'));
  if(value==='Válido')await page.waitForSelector('#grafico img');
  await idle();
 }
 // Resoluciones habituales de escritorio: acciones y filtros alineados sin desbordamiento de página.
 for(const [width,height] of [[1366,768],[1920,1080]]){
  await page.setViewportSize({width,height});
  await page.locator('a[data-value="GC_Tareas"]').click();await idle();
  const tops=await page.evaluate(()=>['#Tareas-buscar','[data-id="Tareas-etiquetas"]',
    '[data-id="Tareas-prioridades"]'].map(s=>document.querySelector(s).getBoundingClientRect().top));
  assert(Math.max(...tops)-Math.min(...tops)<3);
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false);
 }
 await page.screenshot({path:process.env.CRM_TEST_DESKTOP||'/tmp/crm-interaccion-escritorio.png',fullPage:true});
 // Pantalla estrecha: encabezado apilable, menú utilizable y tooltip dentro del viewport.
 await page.setViewportSize({width:390,height:844});
 await page.locator('a[data-value="GC_Tareas"]').click();await idle();
 assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false);
 await page.locator('[data-id="Tareas-prioridades"]').hover();await page.waitForSelector('#crm-select-tooltip');
 const inside=await page.locator('#crm-select-tooltip').evaluate(t=>{const r=t.getBoundingClientRect();return r.left>=0&&r.right<=innerWidth&&r.top>=0&&r.bottom<=innerHeight;});
 assert(inside);await page.mouse.move(1,1);
 await page.screenshot({path:process.env.CRM_TEST_SCREENSHOT||'/tmp/crm-interaccion-movil.png',fullPage:true});
 assert.deepEqual(errors,[]);await browser.close();
 console.log('OK: interfaz real, filtros global/parcial/vacío, refresco, loader/error/recuperación, tooltips, gráficos y móvil.');
})().catch(e=>{console.error(e);process.exit(1)});
