# Cartera financiera nacional

La sección existente **Gestión Comercial → Cartera** conserva su ruta `GC_Cartera` y su ID `CarteraComercial`.
La entrega sustituye el tablero parcial por las siete vistas del paquete adjunto: resumen, vencida,
proyección de pagos, comportamiento, rotación, recaudos y detalle. No modifica la migración administrativa
de cuentas entre asesores, que es otra función del CRM.

## Fuente definitiva y ETL

`APP/misc/datos/04_cartera.R` implementa la extracción indicada en el data prep adjunto mediante
`racafeBD::ConsultaSistema("syscafe", query = ...)`, reutilizando la conexión administrada del proyecto.
No copia conexiones, credenciales, rutas personales, despliegues ni código ejecutable de la app antigua.
`DataPrep.R` incorpora el resultado en `homologaciones$cartera_financiera`, dentro del mismo `data.RData`.
La app consume esa fuente sin consultar Tesorería desde cada gráfico, modal o descarga.

| Objeto exportado | Fuente | Grano y criterio |
|---|---|---|
| cartera | TESHCARE, compañía 10, CN | Corte/NIT/factura/tipo/línea/producto/pedido/línea de pedido |
| facturacion | FCTFACNA, compañía 10, contabilizadas | Cabecera por NIT/prefijo/factura; desde 2018, sacos y pedido no cero |
| clientes | Directorio de la facturación | NIT como valor; nombre y NIT como etiqueta |
| última gestión | TESGESCA, CN | Una gestión por NIT/factura, mayor secuencia y fecha |
| última fecha de recibo | TESRECPA, compañía 10 | MAX de fecha por número de factura; no es un registro de abonos |
| vencimiento de factura | NFORPAG / FCTCTBPA | Fecha de factura + plazo de la forma de pago; plazo general solo si es único |
| maestros | NPERSONA, NLINEANE, NTIPPROD, EXPPEDID | Unicidad verificada antes de unir importes |

Se conserva el histórico completo de cortes que devuelve TESHCARE. La app nunca suma cortes diferentes.
`ValorCOP` y `SaldoCOP` convierten USD por `TCETrm` una sola vez; COP conserva su importe original.
Una moneda o TRM no soportada deja importe desconocido, en lugar de asumir cero.
La proyección usa `SaldoCOP`, no `TCESal/1000`: tanto tabla como gráfico están en **COP**.
Las fechas ambiguas por prefijo en el enlace de cartera quedan sin homologar; no multiplican saldos.
Una fecha de recibo cuyo número corresponde a varios NIT/prefijos no se atribuye a ninguno de ellos.

Si falla la nueva extracción, el ETL registra su estado y el CRM identifica explícitamente el archivo
histórico autorizado cuando está disponible (`CRM_DATOS_COMERCIALES` o `comercial_historico.rds`).
Sin fuente disponible se muestra un mensaje; no se fabrican saldos ni pagos.
La extracción nueva queda preparada y probada con consultas simuladas; no se ejecutó contra el sistema de producción.
**Actualizar** recalcula el archivo cargado en la sesión; para obtener otro corte se ejecuta el ETL y se recarga su exportación.

## Alcance y decisiones funcionales

- El catálogo vigente del CRM y los filtros autorizados definen el universo por NIT principal y línea.
  Se homologa el NIT de facturación antes de restringir el acceso; un administrador no evita los filtros seleccionados.
  Facturas históricas sin línea se restringen por cliente principal, sin inventar una distribución por negocio.
- Corte y negocio afectan saldo, KPI, resumen, vencida, proyección y detalle. Los históricos tienen su propio
  rango de **fecha de factura** y selector múltiple de clientes, independientes del corte/negocio.
  Los filtros de asesor/segmento/línea del CRM siguen restringiendo ambas fuentes.
- Los cortes son opciones existentes, con el último seleccionado inicialmente. Negocios y clientes tienen
  selección global nativa; limpiar todas las opciones significa cero registros, incluso tras actualizar.
- Facturas distintas: NIT + factura en cartera; NIT + prefijo + factura en el histórico.
  No se cuentan llaves faltantes. TOTAL recalcula distintos, no suma los subtotales de clientes/facturas.
  Un cliente o factura puede ocupar varios tramos; sus participaciones usan el universo distinto completo.
- Mora al corte: corte menos vencimiento, con tramos 1–7, 8–15, 16–30 y más de 30 días.
  Se separan los saldos a favor y los documentos sin vencimiento.
  Se omiten gestiones posteriores a un corte histórico porque la fuente importada solo trae la última gestión.
- El calendario contractual proyecta saldo positivo por vencer, por lunes de semana o primer día de mes,
  manteniendo el año en la fecha. Una única serie ordenada produce saldo y acumulado de tabla y gráfico.
- Comportamiento usa únicamente facturas con saldo cero, fecha de pago válida y fecha de vencimiento conocida.
  Se completan todos los tramos y se mantienen separados los NIT homónimos.
  Valor usa FcnVal en millones de COP; Facturas y Sacos conservan sus unidades.
- Rotación: promedio de días factura→pago ponderado por FcnVal positivo, agrupado por NIT/año de factura.
  Se excluyen fechas centinela, pagos negativos y pendientes. La comparación usa solo el año anterior;
  sin comparable, permanece neutra y las columnas siguen siendo numéricas.
- Recaudos es una consulta de facturas, incluidas las que no acreditan pago total. Se muestran por separado
  días hasta el pago, atraso al pagar y mora actual. No se interpreta cada fila como un abono.
  No se inventa fecha de pago para una factura con saldo cero y sin recibo válido.

## Interfaz y detalles

Controles y acciones usan racafeForms; KPI usan cajaValor de racafeModulos; tablas principales y modales
usan racafeTableVancouver a través de los adaptadores del CRM. Los importes y participaciones conservan
sus valores numéricos hasta presentarse, y las notas usan racafeNoticeList.
Se reutilizan el preloader global, los tooltips y la tipografía compartida.

Los detalles se abren por clave de tipo/tramo, incluida TOTAL. La búsqueda del modal actualiza los totales
y el Excel sobre todas las filas filtradas, no solo la página visible. Se registra el servidor de detalle una vez.
Los NIT se exportan como texto y los importes como números. `writexl` se incluye en las dependencias de inicio. La búsqueda general limita las filas visibles;
la descarga general contiene la vista completa según sus controles y la descarga del modal respeta su búsqueda.

## Verificación

Se leyó en un entorno aislado la copia histórica previamente disponible: 45 980 líneas de cartera,
709 cortes y 9 688 cabeceras de factura. No se encontraron duplicados del grano de cartera ni del histórico
por NIT/prefijo/factura. El último corte concilia exactamente en saldo, sacos, facturas y subtotales.
La conversión COP coincide con la fórmula del data prep suministrado, incluida la cartera en USD.
La muestra histórica confirma que FcnVal de las facturas vinculadas en USD está expresado en COP;
ValorCOP puede diferir de FcnVal por el valor tributario del documento, por lo que no se intercambian.
No se incorpora ese snapshot ni sus datos al repositorio.

Las diferencias respecto al código anterior son deliberadas: proyección en COP, periodos cronológicos,
conteos distintos en TOTAL, selección vacía real, distribución de tramos completa, identificación por NIT,
filtro por fecha de factura y exclusión de fechas de pago fabricadas.

```sh
Rscript tests/test_cartera.R
Rscript tests/test_comercial_bono_cartera.R
Rscript -e "shiny::runApp('tests/cartera', host='127.0.0.1', port=3878)"
# En otra terminal, con Playwright y Chromium:
node tests/test-cartera.cjs
```

El arnés usa únicamente datos ficticios y no habilita escrituras. Cubre selección completa/parcial/vacía,
tramos, detalle por TOTAL, búsqueda, exportación numérica, cambio de alcance, proyección diciembre/enero,
históricos y vista móvil. `CRM_CARTERA_URL` y `CHROMIUM` permiten cambiar URL y navegador.

## Cortes disponibles y filtros sin coincidencias

Los selectores de corte y negocio usan los metadatos del archivo, antes de aplicar los filtros comerciales.
Una selección de asesor, segmento o línea sin coincidencias no elimina las fechas que sí contiene la fuente.
Los importes, clientes, detalles y descargas continúan restringidos mediante el catálogo autorizado del CRM.

La interfaz distingue fuente ausente, archivo sin cortes válidos, filtros sin coincidencias y selección vacía.
Muestra el motivo una sola vez y oculta las KPI y el detalle que no puedan calcularse. Las consultas históricas
mantienen su independencia del selector de negocios del saldo. Al corregir los filtros se recuperan las salidas
sin reiniciar la app. El arnés de navegador incluye estas regresiones.
