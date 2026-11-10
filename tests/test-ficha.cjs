const assert = require('node:assert/strict');
const {chromium} = require('playwright');
(async()=>{
 const browser=await chromium.launch({executablePath:process.env.CHROMIUM||'/usr/bin/chromium',args:['--no-sandbox']});
 try {
  const page=await browser.newPage({viewport:{width:1440,height:1100}}), errors=[];
  page.on('pageerror',e=>errors.push(e.message));page.setDefaultTimeout(25000);
  await page.goto(process.env.CRM_FICHA_URL||'http://127.0.0.1:3910');
  await page.locator('#consulta-identidad h4').waitFor();
  const elegir=async valor=>{
   await page.evaluate(v=>$('#consulta-entidad').selectpicker('val',v).trigger('change'),valor);
   await page.waitForFunction(v=>$('#consulta-entidad').val()===v,valor);
  };
  await elegir('nit:860076919');
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('CREPES Y WAFFLES'));
  const ficha=page.locator('.in-ficha');
  await page.waitForFunction(()=>document.querySelector('#consulta-presupuesto_ficha').innerText.includes('235.200.000'));
  assert.match(await page.locator('#consulta-identidad .rf-status-badge--success').first().innerText(),/Activo en CRM/);
  assert.match(await page.locator('#consulta-familia_contador').innerText(),/4 hijas visibles/);
  assert.match(await page.locator('#consulta-presupuesto_ficha').innerText(),/960/);
  assert.match(await page.locator('#consulta-antiguedad-tabla').innerText(),/23 años y 3 meses/);
  assert.match(await page.locator('#consulta-antiguedad-tabla').innerText(),/5 años y 7 meses/);
  assert.match(await page.locator('#consulta-ficha-tabla').innerText(),/14\/02\/2018 10:35:22/);
  assert.match(await page.locator('#consulta-contacto_sistema-tabla').innerText(),/6011234567/);
  assert.doesNotMatch(await page.locator('#consulta-contacto_sistema-tabla').innerText(),/SIN DATO \|/);
  const relacionados=ficha.locator('details').filter({has:page.locator('summary',{hasText:'Cuentas relacionadas'})}).first();
  assert.equal(await relacionados.evaluate(el=>el.open),false);
  await relacionados.locator(':scope > summary').click();
  await page.locator('#consulta-familia-tabla .in-ficha-cuenta').first().waitFor();
  assert.equal(await relacionados.locator(':scope > summary > .rsh-collapsible-header-badge').count(),1);
  assert.match(await page.locator('#consulta-familia-tabla .in-ficha-cuenta').first().innerText(),/Cuenta padre/);
  assert.equal(await page.locator('#consulta-familia-tabla .in-ficha-cuenta').count(),5);
  assert.doesNotMatch(await page.locator('#consulta-familia-tabla').innerText(),/Sede 5/);
  const complementos=ficha.locator('details').filter({has:page.locator('summary',{hasText:'Registros complementarios'})}).first();
  assert.equal(await complementos.getAttribute('open'),null);
  assert.match(await complementos.locator(':scope > summary').innerText(),/0 registros/);
  // Enter y Espacio sobre summary usan el comportamiento accesible nativo.
  await complementos.locator(':scope > summary').focus();await page.keyboard.press('Enter');
  assert.equal(await complementos.evaluate(el=>el.open),true);
  await page.waitForFunction(()=>Shiny.shinyapp.$inputValues['consulta-ficha_box_complementarios']===true);
  await page.waitForFunction(()=>document.querySelector('#consulta-complementarios').innerText.includes('Sin registros'));
  await complementos.locator(':scope > summary').focus();await page.keyboard.press('Space');
  assert.equal(await complementos.getAttribute('open'),null);
  await page.waitForFunction(()=>!document.documentElement.classList.contains('shiny-busy') &&
    (!document.querySelector('#rsh-loader-host') || document.querySelector('#rsh-loader-host').hidden));
  await page.evaluate(()=>document.activeElement.blur());
  const cajas=page.locator('.in-ficha details.in-ficha-caja');
  assert.equal(await cajas.count(),6);
  const ids=await cajas.evaluateAll(ns=>ns.map(n=>n.id));
  assert.equal(new Set(ids).size,6);
  assert(ids.every(id=>id.startsWith('consulta-ficha_box_')&&!id.includes('consulta-consulta')));
  const estilos=await cajas.evaluateAll(nodes=>nodes.map(n=>{const s=getComputedStyle(n);return {
    border:s.borderTopWidth,color:s.borderTopColor};}));
  assert(estilos.every(x=>x.border==='0px'));
  const filas=await page.locator('.in-ficha-grid > details').evaluateAll(nodes=>nodes.map(n=>{
    const r=n.getBoundingClientRect();return {x:r.x,y:r.y,height:r.height};}));
  assert.equal(filas.length,6);
  assert(Math.abs(filas[0].y-filas[1].y)<2&&Math.abs(filas[2].y-filas[3].y)<2&&Math.abs(filas[4].y-filas[5].y)<2);
  assert.equal(await page.locator('.in-ficha .racafe-caja-valor').count(),4);
  assert.equal(await page.locator('.in-ficha .rmcv-button').count(),0);
  assert.match(await page.locator('#consulta-identidad').innerText(),/Estado comercial: Activo/);
  assert(Math.abs(filas[0].height-filas[1].height)<2&&Math.abs(filas[2].height-filas[3].height)<2);
  assert.equal(await page.locator('.in-ficha-grid').evaluate(n=>getComputedStyle(n,'::before').width),'1px');
  const filasCuenta=await page.locator('.in-ficha-cuenta').evaluateAll(nodes=>nodes.map(n=>{
    const b=n.querySelector('.rf-status-badge');return {h:n.getBoundingClientRect().height,
      w:n.getBoundingClientRect().width,b:b.getBoundingClientRect().width};}));
  assert(filasCuenta.every(r=>r.h<85&&r.b<r.w/2));
  assert.equal(await page.locator('.in-ficha-asesor-comun').count(),1);
  assert.equal(await page.locator('#consulta-identidad .rf-status-badge--info').first()
    .evaluate(n=>getComputedStyle(n).backgroundColor),'rgb(231, 239, 249)');
  await page.evaluate(()=>window.__fichaCajas=[...document.querySelectorAll('.in-ficha-caja')]);
  await relacionados.locator(':scope > summary').click();
  await page.screenshot({path:'/tmp/ficha-desktop.png',fullPage:true});
  await page.setViewportSize({width:390,height:844});
  const movil=await page.locator('.in-ficha-grid > details').evaluateAll(nodes=>{
    const r=nodes.map(n=>n.getBoundingClientRect());return r.every((x,i)=>!i||x.y>=r[i-1].bottom);});
  assert(movil);
  assert.equal(await page.locator('.in-ficha-grid').evaluate(n=>getComputedStyle(n,'::before').display),'none');
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false);
  await page.screenshot({path:'/tmp/ficha-mobile.png',fullPage:true});
  await page.locator('#consulta-ficha_box_antiguedad > summary').click();
  await elegir('nit:102');
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('SEDE NORTE'));
  await page.waitForFunction(()=>document.querySelector('#consulta-complementarios_contador').innerText.includes('0 registros'));
  assert.equal(await page.locator('#consulta-ficha_box_antiguedad').evaluate(n=>n.open),false);
  assert.equal(await page.evaluate(()=>window.__fichaCajas.every((n,i)=>
    n===document.querySelectorAll('.in-ficha-caja')[i])),true);
  await page.locator('#consulta-ficha_box_complementarios > summary').click();
  await page.waitForFunction(()=>document.querySelector('#consulta-complementarios').textContent.includes('Sin registros'));
  assert.equal((await page.locator('#consulta-complementarios').textContent()).match(/Sin registros para los filtros seleccionados/g).length,1);
  assert.match(await page.locator('#consulta-ficha-tabla').innerText(),/202/);
  assert.match(await page.locator('#consulta-identidad').innerText(),/102/);
  assert.doesNotMatch(await page.locator('#consulta-presupuesto_ficha').innerText(),/235.200.000/);
  await elegir('principal:101');
  await page.locator('#consulta-presupuesto-tabla .gt_table').waitFor();
  assert.equal(await page.locator('#consulta-presupuesto_ficha .racafe-caja-valor').count(),0);
  assert.equal(await page.locator('#consulta-presupuesto-tabla .gt_table tbody tr').count(),2);
  await page.locator('#consulta-contenido a[data-value="Ejecución y productos"]').click();
  await elegir('nit:900123456');
  await page.waitForFunction(()=>document.querySelector('#consulta-detalle_tabs a[data-value="Ficha"]').classList.contains('active'));
  assert.equal(await page.locator('#consulta-contenido a[data-value="Ejecución y productos"]:visible').count(),0);
  await page.waitForFunction(()=>document.querySelector('#consulta-complementarios_contador').innerText.includes('3 registros'));
  await page.locator('#consulta-telefonos-tabla .gt_table').waitFor();
  await page.locator('#consulta-telefonos-buscar').fill('300');
  await page.waitForFunction(()=>document.querySelector('#consulta-telefonos-tabla').innerText.includes('3001234567') &&
    !document.querySelector('#consulta-telefonos-tabla').innerText.includes('6011234567'));
  assert.equal(await page.evaluate(()=>window.__fichaCajas.every((n,i)=>
    n===document.querySelectorAll('.in-ficha-caja')[i])),true);
  await elegir('nit:500');
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('Estado comercial: Inactivo'));
  await elegir('nit:501');
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('Cliente nuevo'));
  await page.waitForFunction(()=>document.querySelector('#consulta-presupuesto_ficha').innerText.includes('Sin presupuesto'));
  await elegir('nit:860076919');
  await page.evaluate(()=>$('#linea').selectpicker('val',['CONVENCIONALES']).trigger('change'));
  await page.waitForFunction(()=>document.querySelector('#consulta-presupuesto_ficha').innerText.includes('Sin presupuesto'));
  await page.waitForFunction(()=>document.querySelector('#consulta-familia_contador').innerText.includes('0 hijas'));
  await page.evaluate(()=>$('#asesor').selectpicker('val',[]).trigger('change'));
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('Seleccione'));
  assert.equal(await page.locator('.in-ficha .racafe-caja-valor:visible').count(),0);
  assert.deepEqual(errors,[]);
  console.log('OK: grid de tres filas, cajasValor, dos cierres por defecto, estados mensuales y móvil.');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
