# Integración de PreparacionDatos.zip

## Ubicación de los elementos

| Elementos de la app entregada | Ubicación en el CRM actual | Tratamiento |
| --- | --- | --- |
| Margen/volumen general, por negocio y asesor; comparativos mensuales, acumulados y de cinco años | Generalidades → Evolución del Negocio | Comparte hechos del maestro; selector de dimensión, mes, acumulado y datos abiertos; comparación interanual del mismo periodo |
| Desgloses por negocio, asesor, cliente, región, categorías y marcas; anillos y sunburst | Generalidades → Participación y Composición | Barras y tabla completa con detalle Vancouver; soporta negativos y evita particiones circulares engañosas |
| Cumplimiento de volumen y margen por cliente/línea, segmento, asesor, departamento y municipio | Gestión Comercial → Presupuesto (existente) y desglose comercial | Se conserva el módulo de presupuesto actual; no se crea otro resumen comercial en el sidebar |
| Margen por kilo y por libra en centavos de dólar | Evolución y Participación | Ratio con totales; US¢/lb utiliza la tasa anual importada, no la TRM actual de otro año |
| Componentes de bono por negocio/segmento y cuatrimestres | Gestión Comercial → Variables (Bono) | Esquemas históricos y simulación provisional 2026, trazabilidad mensual y exportación |
| Presupuestados, nuevos, nuevas líneas, no presupuestados/recuperados | Gestión Comercial → Fidelidad y Riesgo | Separa clasificación presupuestal y señal de compra; no llama recuperado a todo cliente sin presupuesto |
| Saldo, facturas y vencida por rango | Gestión Comercial → Cartera | Un corte explícito, importes COP del archivo y documentos identificados por cliente/tipo/factura |
| Reincidencia/comportamiento de pago | Cartera → Comportamiento de pago | Clientes y facturas pagadas según fecha de vencimiento; no confundir saldo vivo con valor original de factura |
| Proyección semanal/mensual | Cartera → Vencimientos | Calendario contractual, no pronóstico de cobro; fecha de semana completa para no mezclar años |
| Rotación histórica y recaudos por cliente | Cartera → Rotación / Recaudos | Promedio ponderado por valor; solo pagos totales inferidos con fecha válida y saldo cero |
| Helpers de DT, cajas azules, preloaders independientes, aplicaciones de demostración y enlaces a otra app | Componentes existentes | No se ejecutan scripts antiguos: RF, cajas racafeModulos, Vancouver, Compartido y preloader global |

La fecha máxima comercial del archivo recibido es marzo de 2026; la cartera llega al 5 de noviembre de 2026.
No se representa ninguna de esas copias como información en tiempo real.

## Datos del archivo en la app

El repositorio es público. Los registros reales se importan localmente y quedan fuera del PR y de Git.
Desde la raíz del repo:

```sh
Rscript scripts/importar_comercial_historico.R /ruta/PreparacionDatos.zip
```

El importador genera `APP/data/comercial_historico.rds`. Reiniciar Shiny basta para usarlo: el servidor lo lee como
fuente local cuando no está incorporado al maestro. La siguiente ejecución de DataPrep lo agrega dentro de
`homologaciones$comercial_historico`, conservando los dos objetos de exportación habituales.
También puede configurarse la ruta con `CRM_DATOS_COMERCIALES` (ZIP, RData o RDS).
Si el maestro ya contiene la copia incorporada, esa copia tiene prioridad: regenerar DataPrep para actualizarla.

Se extrae únicamente `APP/data/datos.Rdata`; no se sourcean `global.R`, `Funciones.R`, módulos, UI ni servidor
antiguos. Solo se conservan las tablas reconocidas. Fechas POSIX se normalizan a Date y sentinelas de 1753 se
transforman en fechas desconocidas. Las tasas anuales también se conservan como referencia histórica.
Un archivo no configurado muestra una indicación de importación en la sección dependiente, sin inventar saldo cero.

## Presupuesto y ejecución

- El presupuesto anual se toma una sola vez por NIT de facturación/línea en su último snapshot hasta el corte.
  Después se agrega por NIT principal/línea para no multiplicar las uniones de ejecución.
  Las sucursales pueden tener cortes distintos: se conserva el último de cada una, sin perder metas antiguas vigentes.
  Las diferencias de nombre o ubicación no son presupuestos duplicados. Sus dimensiones ambiguas quedan sin clasificar.
  Una contradicción real de importe en el mismo corte deja solo esa meta como desconocida y marca
  `PresupuestoEnRevision`, sin escoger el máximo ni detener todas las vistas. Una meta de sacos coherente se conserva
  aunque la de margen esté en revisión, y viceversa.
  Los filtros se aplican tras resolver los empates, para no elegir una versión contradictoria por el asesor filtrado.
  El desglose conserva las metas de sucursales por asesor/segmento/ubicación; el bono agrega esos planes por segmento
  antes de unirlos con la ejecución, sin repetir ventas. No se usa presupuesto de 2026 en años sin snapshot.
- Los filtros de asesor/segmento/línea se aplican también al catálogo de objetivos, incluidos clientes sin ejecución.
- Las cantidades facturadas son sacos de 70 kg. Margen/kg = margen agregado / kilos agregados.
- El margen US¢/lb = margen/kg × 0,453592 × 100 / tasa COP/USD anual de referencia. Sin tasa válida no se calcula.
- Categoría, producto y marca no reciben un presupuesto de cliente repartido artificialmente. Se muestra su ejecución.
- Región/departamento/municipio usan metadatos disponibles del CRM. Sin soporte se muestran sin clasificar;
  no se inventa una región por la razón social.
- Los meses abiertos están desactivados inicialmente y se distinguen como provisionales. La vista acumulada también
  acumula las series; las variaciones comparan los mismos meses y evitan dividir por una base cero.

## Bono 2026 provisional

El usuario indicó que existe una nueva política y autorizó valores quemados mientras consulta los reales.
La configuración `.BON_PROVISIONAL_2026` en `gestion_comercial/01_variables_bono.R` contiene:

| Cumplimiento | Factor provisional |
| --- | --- |
| Menor de 70% | 0% |
| Desde 70% hasta menos de 85% | 33% |
| Desde 85% hasta menos de 100% | 67% |
| Desde 100% | 100% |

Los pesos de referencia, también provisionales, son los de los perfiles 2025: 20/60/20 para Carlos,
47/53 para Jonathan y periodos independientes para Gustavo/Luis. Cambiar pesos y umbrales cuando llegue la política.
Los perfiles Carlos/Jonathan evalúan negocios; Gustavo/Luis evalúan al asesor seleccionado. El alcance efectivo
siempre está limitado por los filtros y permisos disponibles. No hay liquidación ni escritura de pagos.
Los cuatrimestres se presentan separados: no se suman tres porcentajes de incentivo como si fueran uno anual.
Un periodo sin presupuesto o sin iniciar conserva valor vacío.

Se preservan además las reglas históricas: binarios 80%/100% y escala 70/85/100%. En el legado de Jonathan 2024
la función de total y el indicador individual aplicaban pesos diferentes a MEDIDA. La tabla muestra ambos aportes
para que la discrepancia sea visible. Los objetivos NUEVOS del archivo usan sus filas presupuestales especiales
1/2 solo como objetivos, no como clientes reales. Homologaciones no únicas no se resuelven por nombre.
Los esquemas históricos usados en otro año quedan identificados como simulación, no como política vigente.

## Cartera y riesgo

Los saldos se filtran por un único `TCEFch` y utilizan `SaldoCOP` ya convertido: no hay segunda conversión ni
escala arbitraria de miles. Los sacos de cartera mantienen la unidad original, sin asumir que son de 70 kg.
La fecha de vencimiento determina el atraso al corte. La programación de vencimientos no incluye deuda ya vencida.

El alcance por NIT principal y asesor se obtiene del CRM; sin homologación no se muestra al usuario como cartera
asignada. Las líneas 20000/21000 se homologan para permisos conforme a la convención comercial actual, conservando
el código de origen. Categoría/producto no se usan para atribuir deuda sin datos.
Negocio filtra saldos. Los pagos del archivo carecen de detalle confiable de línea y se filtran por cliente/periodo.

La rotación pondera días factura→pago por valor original de las facturas totalmente pagadas. Se excluyen sentinelas,
fechas futuras, días negativos y saldos pendientes; no se inventan abonos parciales. Recaudo se llama inferido porque
el archivo no es un libro de movimientos de caja. El gráfico y las tablas se pueden consultar con cortes históricos.

Fidelidad utiliza primera/última compra observada y un umbral de revisión modificable (90 días iniciales).
No cambia estados del CRM ni supone alta real por primera observación dentro de un histórico truncado.

## Verificación

```sh
Rscript tests/test_comercial_bono_cartera.R
```

Pruebas sintéticas: bordes de umbrales, pesos, presupuesto cero/desconocido, snapshots repetidos,
no reparto de metas a productos, margen/kg ponderado, cortes de saldos, permisos NIT, pagos inválidos/parciales,
rotación ponderada e importación sin ejecución de scripts. Se verificó localmente el saldo del último corte contra
el ZIP real, y se calcularon rotación y la política histórica sin publicar importes ni nombres del archivo.
El arnés de Shiny renderizó cinco vistas con componentes reales; Chromium comprobó namespaces, controles RF,
Vancouver y adaptación móvil. No se consultaron ni escribieron sistemas de negocio ni se publicaron datos privados.
