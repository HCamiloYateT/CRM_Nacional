# Oportunidades, pendientes y cohortes

## Instalación

1. Usar las versiones Racafé exigidas actualmente por `APP/global.R` y los assets de Compartido.
2. Verificar si `CRMNALCLOPT` ya tiene `FrecuenciaDias`. Si falta, aplicar una vez el SQL revisable en
   `docs/migrations/2026-11-08_oportunidades_frecuencia.sql` antes de habilitar creación de oportunidades.
   Es una columna nullable: no inventa frecuencias de oportunidades históricas. El script no se ejecuta desde Shiny.
3. Regenerar `APP/data/data.RData` ejecutando DataPrep en el entorno habitual y reiniciar la app.
   Se conservan los dos objetos publicados: `crm_maestro` y `homologaciones`.

DataPrep ahora retiene también pedidos activos anteriores al inicio del histórico, aunque no tengan fecha estimada
ni facturación reciente. El resto de los filtros transaccionales del universo nacional se conserva.
No se ejecutó ETL ni migración SQL contra los servicios reales durante el desarrollo.

## Oportunidades

`gestion_comercial/03_oportunidades.R` sustituye la pantalla vacía del sidebar. El tablero usa el catálogo vigente
`CRMNALCLOPT` y reutiliza `CrearOportunidad` / `DescartarOportunidad`, sin otro formulario de persistencia paralelo.
La selección para crear permite contactos propios/asignados; administradores acceden al catálogo completo.
La vista incluye oportunidades creadas por el usuario o asociadas a sus contactos. Las comprobaciones se repiten
al resolver el contacto o ID para las acciones; los cambios requieren la confirmación del formulario vigente.

La creación conserva la frecuencia pedida por el formulario. El servidor captura un snapshot de los valores revisados
y consume una sola vez la confirmación; editar después o repetir el evento no cambia ni duplica ese guardado.
Si falta la columna de frecuencia, la creación falla antes de escribir con una indicación de la migración necesaria.

El tablero normaliza los campos antiguos (`SacosOP`, `MargenOP`, `Usuario`, `Descartada`, `LineaNegocio`) y los actuales.
Una razón social antigua solo se homologa cuando identifica un contacto único. Registros sin ID vigente único se
presentan en modo de lectura: el identificador visual HIST no se usa para modificar BD.

Cumplimiento: ventas del mismo contacto, línea, categoría y producto, entre creación y fecha objetivo, hasta hoy.
No hay asignación exclusiva de facturas: dos oportunidades solapadas pueden compartir ejecución, y por eso los
indicadores globales agregan objetivos, no suman ejecución atribuida. Los sacos de objetivos se convierten a 70 kg
usando el peso de la línea del formulario: convencionales 62,5 kg; a la medida/diferenciados 70 kg.
El margen objetivo usa margen por kilo × kilos. El margen abierto no se presenta como ejecución contable.
La normalización mensual se informa solo cuando hay frecuencia persistida.

El tablero tiene sus filtros locales y permisos. El cruce de cumplimiento conserva toda la historia disponible;
no se recorta por el rango de facturación global, lo que produciría falsos incumplimientos al consultar un periodo.

## Pendientes de Gestión

`gestion_comercial/04_pendientes.R` parte del maestro completo y los filtros dimensionales del controlbar,
sin usar su rango de facturación. La consulta completa de facturas se ejecuta una vez: su agregado de vida del lote
alimenta `FacturadosOperativos70` y los saldos/proyecciones; la base financiera mantiene el inicio del histórico.
Esto evita pendientes falsos por facturas antiguas. Se mantienen separados tres granos:

- Pedido/línea: cantidad pedida menos asignada; cantidad pedida menos facturada; futuro, vencido o sin fecha.
- Lote: asignado menos producido; producido menos despachado; despachado menos facturado.
- Financiero: factura sin lote, margen contable ausente o mes abierto pendiente de cierre.

También aparecen pedidos pendientes sin estimación de margen, lotes sin pedido y cantidades inconsistentes.
Las cantidades operativas se toman una vez por lote, aunque tenga varias facturas; la ejecución se suma excluyendo
proyecciones. Una cantidad operativa contradictoria o desconocida conserva NA y se envía a revisión.
Un pedido sin lote tiene asignación cero. Los pedidos inactivos no generan nuevos saldos operativos por gestionar.

Los indicadores y el detalle usan sacos equivalentes de 70 kg y tolerancia de 0,1. Los estados no son sumables:
un pedido futuro también puede estar sin asignar. El filtro de futuros está activado inicialmente.
La tabla permite abrir la trazabilidad y exportar el conjunto filtrado en XLSX, con NIT como texto.
La fecha de datos corresponde a la última exportación de DataPrep; no se hacen consultas transaccionales adicionales.

La asignación de órdenes de compra del archivo antiguo se conserva en el detalle del lote, con formulario RF,
bitácora Vancouver y confirmación con snapshot. Se verifica que el lote siga visible para el usuario antes de guardar;
la bitácora conserva el usuario de sesión, sin el HCYATE fijo del adjunto. Se añade un registro a CRMNALORDCMP y se
refresca la bitácora; la homologación usada por el resto del maestro se actualizará al regenerar DataPrep.

## Cohortes de Clientes

`cohortes_clientes/00_cohortes.R` alimenta las cuatro entradas existentes: Retención, Ventas, Margen y Frecuencia.
La unidad es NIT principal + línea. La cohorte es la primera compra positiva **observada en toda la historia
exportada**, antes de recortar el rango del informe. No equivale al alta comercial y está limitada por el inicio
del histórico; el módulo lo explica al pie.

El panel completa meses observados después de M0 para mantener el denominador original, incluso sin compras.
No crea observaciones después del corte seleccionado/hoy. Las celdas futuras quedan vacías; los meses sin compra
son cero. El último mes puede estar en curso. Las vistas incluyen curva por cohorte, matriz y panel por cliente,
con nombre, asesor y transición de compra recurrente/reactivada/sin compra. Todas usan la misma transformación.

- Retención: clientes con compra positiva / base original de la cohorte. Es retención de compra mensual, distinta
  del estado activo/inactivo comercial del antiguo script.
- Ventas: sacos facturados de 70 kg; no incluye proyecciones.
- Margen: contable en meses cerrados. Meses abiertos conservan NA, incluso si traen estimación; no se sustituyen por cero.
- Frecuencia: meses con compra / meses observados desde M0. Es una medida acumulada de regularidad mensual.
  No infiere días de compra ni número de facturas a partir de agregados por lote/mes.

## Validación

Desde la raíz del repo, con las dependencias habituales instaladas:

```sh
Rscript tests/test_flujo_cohortes_oportunidades.R
Rscript tests/test_confirmacion_oportunidad.R
```

Se probaron granos, asignación parcial, facturas múltiples, cantidades desconocidas, futuros, inactivos,
ceros observados frente a censura futura, cohortes recortadas, reactivación, permisos, lectura antigua,
conversión de unidades, frecuencia y confirmación única con snapshot. Las fuentes/escrituras se simulan.
El arnés local también renderizó servidores y tablas con componentes reales; Chromium comprobó controles RF,
tablas Vancouver, namespaces y adaptación móvil. La app completa con credenciales reales, ETL y esquema productivo
requiere verificación en el entorno de negocio. No se copiaron credenciales, demos ni envíos de los adjuntos.
