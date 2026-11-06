const assert=require('node:assert/strict');
const {chromium}=require('playwright');
(async()=>{
  const browser=await chromium.launch({executablePath:process.env.CHROMIUM_PATH,
    args:process.env.CI?['--no-sandbox']:[]});
  let checks=0;const check=(value,message)=>{assert.ok(value,message);checks++;};
  try{
    const page=await browser.newPage({viewport:{width:1600,height:1000}}),errors=[];
    page.on('pageerror',error=>errors.push(error.message));
    await page.goto(process.env.CRM_ACTIONS_URL||'http://127.0.0.1:4104');
    const summary=page.locator('.resumen-ejecutivo');
    await summary.locator('.rmcv-value span').first().waitFor();
    await page.waitForFunction(()=>document.querySelector('.resumen-ejecutivo .re-loader').style.display==='none');
    const cards=await summary.locator('.rmcv-card').count();
    check(cards>10,'Resumen con tarjetas reales');
    check(await summary.locator('.rmcv-footer button').count()===cards,'Botones de navegación en todas las tarjetas');
    check(await summary.locator('.rmcv-value span').evaluateAll(nodes=>nodes.every(el=>
      getComputedStyle(el).color==='rgb(0, 0, 0)')),'Resumen sin meta: números negros');
    check(await summary.locator('.rmcv-card').evaluateAll(nodes=>nodes.every(el=>
      getComputedStyle(el).borderRightWidth==='1px')),'Contorno derecho visible en resumen');
    if(process.env.CRM_SCREENSHOTS) await page.screenshot({path:`${process.env.CRM_SCREENSHOTS}/resumen.png`});
    await summary.locator('.rmcv-button').first().click();
    const financial=page.locator('[id="fin-vf_contenido"]');
    await financial.waitFor();
    await page.waitForFunction(()=>document.querySelector('#fin-vf_preloader').style.display==='none');
    check(await financial.locator('.rmcv-button').count()===8,'Ocho botones financieros visibles');
    check(await financial.locator('.rmcv-value span').evaluateAll(nodes=>nodes.every(el=>
      getComputedStyle(el).color==='rgb(0, 0, 0)')),'Financiera sin meta: números negros');
    check(await financial.locator('.rmcv-card').evaluateAll(nodes=>nodes.every(el=>
      getComputedStyle(el).borderRightWidth==='1px'&&getComputedStyle(el).borderLeftWidth==='3px')),
      'Contorno completo y acento lateral en financiera');
    if(process.env.CRM_SCREENSHOTS) await page.screenshot({path:`${process.env.CRM_SCREENSHOTS}/financiera.png`});
    const keys=['kpi_cuadre','kpi_lotes','kpi_fact_sin_pyg','kpi_pyg_sin_fact','kpi_pyg_sin_lote',
      'kpi_linea','kpi_segmentos','kpi_calidad'];
    for(const key of keys){
      await page.locator(`#fin-${key}-btn_abrir`).click();
      await page.locator('.modal.show .modal-title').filter({hasText:key}).waitFor();
      check((await page.locator('.modal.show').textContent()).includes('Detalle financiero simulado'),
        `Botón abre su modal existente: ${key}`);
      await page.locator('.modal.show [data-dismiss="modal"]').click();
      await page.waitForFunction(()=>!document.querySelector('.modal.show'));
    }
    const ids=await page.evaluate(()=>{const counts=new Map();document.querySelectorAll('[id]').forEach(el=>
      counts.set(el.id,(counts.get(el.id)||0)+1));return[...counts].filter(([id,n])=>id&&n>1);});
    assert.deepEqual(ids,[]);checks++;
    check(await page.locator('.shiny-output-error').count()===0,'Sin errores R');
    check(errors.length===0,errors.join('; '));
    console.log(`${checks} comprobaciones de resumen y botones financieros superadas.`);
  }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
