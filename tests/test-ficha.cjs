const assert = require('node:assert/strict');
const {chromium} = require('playwright');
(async()=>{
 const browser=await chromium.launch({executablePath:process.env.CHROMIUM||'/usr/bin/chromium',args:['--no-sandbox']});
 try {
  const page=await browser.newPage({viewport:{width:1440,height:1100}}), errors=[];
  page.on('pageerror',e=>errors.push(e.message));page.setDefaultTimeout(25000);
  await page.goto(process.env.CRM_FICHA_URL||'http://127.0.0.1:3903');
  await page.locator('#consulta-identidad h4').waitFor();
  const elegir=async valor=>{
   await page.evaluate(v=>$('#consulta-entidad').selectpicker('val',v).trigger('change'),valor);
   await page.waitForFunction(v=>$('#consulta-entidad').val()===v,valor);
  };
  await elegir('nit:860076919');
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('CREPES Y WAFFLES'));
  const ficha=page.locator('.in-ficha');
  await page.waitForFunction(()=>document.querySelector('#consulta-presupuesto_ficha').innerText.includes('235.200.000'));
  assert.match(await page.locator('#consulta-identidad .rf-status-badge--success').innerText(),/Activo en CRM/);
  assert.match(await page.locator('#consulta-familia_contador').innerText(),/4 cuentas hijas visibles/);
  assert.match(await page.locator('#consulta-presupuesto_ficha').innerText(),/960/);
  assert.match(await page.locator('#consulta-antiguedad-tabla').innerText(),/23 años y 3 meses/);
  assert.match(await page.locator('#consulta-antiguedad-tabla').innerText(),/5 años y 7 meses/);
  assert.match(await page.locator('#consulta-ficha-tabla').innerText(),/14\/02\/2018 10:35:22/);
  assert.match(await page.locator('#consulta-contacto_sistema-tabla').innerText(),/6011234567/);
  assert.doesNotMatch(await page.locator('#consulta-contacto_sistema-tabla').innerText(),/SIN DATO \|/);
  const relacionados=ficha.locator('details').filter({has:page.locator('summary',{hasText:'Cuentas relacionadas'})}).first();
  assert.equal(await relacionados.getAttribute('open'),'');
  assert.match(await page.locator('#consulta-familia-tabla .rf-panel').first().innerText(),/Cuenta padre/);
  assert.equal(await page.locator('#consulta-familia-tabla .rf-panel').count(),5);
  assert.doesNotMatch(await page.locator('#consulta-familia-tabla').innerText(),/Sede 5/);
  const complementos=ficha.locator('details').filter({has:page.locator('summary',{hasText:'Registros complementarios'})}).first();
  assert.equal(await complementos.getAttribute('open'),null);
  assert.match(await complementos.locator(':scope > summary').innerText(),/3 registros visibles/);
  // Enter y Espacio sobre summary usan el comportamiento accesible nativo.
  await complementos.locator(':scope > summary').focus();await page.keyboard.press('Enter');
  assert.equal(await complementos.getAttribute('open'),'');
  await complementos.locator(':scope > summary').focus();await page.keyboard.press('Space');
  assert.equal(await complementos.getAttribute('open'),null);
  await page.waitForFunction(()=>!document.documentElement.classList.contains('shiny-busy') &&
    (!document.querySelector('#rsh-loader-host') || document.querySelector('#rsh-loader-host').hidden));
  const filas=await ficha.locator(':scope > .rf-grid').evaluateAll(nodes=>nodes.map(n=>{
   const r=[...n.children].map(x=>x.getBoundingClientRect());return r.map(x=>({x:x.x,y:x.y,width:x.width}));}));
  assert(filas.every(r=>r.length===2&&Math.abs(r[0].y-r[1].y)<2));
  await page.screenshot({path:'/tmp/ficha-desktop.png',fullPage:true});
  await page.setViewportSize({width:390,height:844});
  const movil=await ficha.locator(':scope > .rf-grid').evaluateAll(nodes=>nodes.map(n=>{
   const r=[...n.children].map(x=>x.getBoundingClientRect());return r[1].y>=r[0].bottom;}));
  assert(movil.every(Boolean));
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false);
  await page.screenshot({path:'/tmp/ficha-mobile.png',fullPage:true});
  await elegir('nit:102');
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('SEDE NORTE'));
  await page.waitForFunction(()=>document.querySelector('#consulta-complementarios').textContent.includes('Sin registros'));
  assert.equal((await page.locator('#consulta-complementarios').textContent()).match(/Sin registros para los filtros seleccionados/g).length,1);
  assert.match(await page.locator('#consulta-ficha-tabla').innerText(),/202/);
  assert.match(await page.locator('#consulta-identidad').innerText(),/102/);
  assert.doesNotMatch(await page.locator('#consulta-presupuesto_ficha').innerText(),/235.200.000/);
  await elegir('principal:101');
  await page.locator('#consulta-presupuesto-tabla .gt_table').waitFor();
  assert.equal(await page.locator('#consulta-presupuesto_ficha .rf-metric').count(),0);
  assert.equal(await page.locator('#consulta-presupuesto-tabla .gt_table tbody tr').count(),2);
  await elegir('nit:500');
  await page.waitForFunction(()=>document.querySelector('#consulta-presupuesto_ficha').innerText.includes('Sin presupuesto'));
  await elegir('nit:860076919');
  await page.evaluate(()=>$('#linea').selectpicker('val',['CONVENCIONALES']).trigger('change'));
  await page.waitForFunction(()=>document.querySelector('#consulta-presupuesto_ficha').innerText.includes('Sin presupuesto'));
  await page.waitForFunction(()=>document.querySelector('#consulta-familia_contador').innerText.includes('0 cuentas hijas'));
  await page.evaluate(()=>$('#asesor').selectpicker('val',[]).trigger('change'));
  await page.waitForFunction(()=>document.querySelector('#consulta-identidad').innerText.includes('Seleccione'));
  assert.equal(await page.locator('.in-ficha .rf-metric').count(),0);
  assert.deepEqual(errors,[]);
  console.log('OK: referencia CREPES ficticia, cuatro hijas autorizadas, edades, metas, datos parciales, cambios, teclado y móvil.');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
