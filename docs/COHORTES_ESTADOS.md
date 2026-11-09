# Estados y Transiciones

La nueva entrada `Cohortes de Clientes > Estados y Transiciones` integra el módulo aprobado de `Cohortes.R`.
Las cuatro vistas de primera compra observada conservan sus rutas y cálculos.

## Contenido

- Mes inicial/final y filtro Total / Con Ppto / Sin Ppto, compartidos por toda la pantalla.
- Resumen de periodo: total, activos, inactivos y nuevos en ambos cortes.
- Alertas: riesgo de inactivación y recuperación, con detalle por cliente.
- Matrices absoluta y porcentual por fila, columna y total general; detalle al seleccionar una fila de origen.
- Retención, churn, reactivación, cambio neto y estabilidad.
- Duración de actividad, tiempo de reactivación, balance neto, equilibrio y sacos/margen en riesgo.
- Resumen mensual con detalle por celda, gráfico de crecimiento y descargas XLSX en los detalles.

## Fuentes y reglas conservadas

El panel usa el bloque `data_cohortes` suministrado junto al script aprobado:

- `CRMNALSEGR` aporta los estados históricos y sus fechas `FecProceso`, sin sustituirlos por primera compra.
- `CLIENTE` se transforma en ACTIVO; `CLIENTE A RECUPERAR`, en INACTIVO.
- Un mes con ventas y sin estado se clasifica NUEVO. Si ocurre en cualquier mes del año, la UC mantiene NUEVO
  en toda su trayectoria anual, incluso en los meses en que ya tiene estado registrado.
- El último snapshot de `CRMNALCLIENTE` por NIT principal/línea aporta asesor, segmento, umbral y presupuesto.
  Se exige snapshot del año en curso y `Excluir != "SI"`, como en el bloque suministrado.
- Las metas anuales se dividen entre 12 y permanecen invariantes por UC. El acumulado usa los meses del panel
  seleccionado, tal como lo hace el módulo aprobado.
- Las ventas y la última factura provienen del maestro completo; los filtros dimensionales restringen el universo
  visible mediante `maestro_comercial`. El rango de facturación del controlbar no recorta el panel mensual.
- `SacosFact` del maestro ya está en sacos equivalentes de 70 kg. No se incluyen proyecciones.

`CRMNALSEGR` se carga al consultar el módulo y se invalida con `FT_Actualizar`. Se reutiliza el cache de clientes
que ya tiene la aplicación. No se agregan escrituras de negocio ni cambios en DataPrep o en el esquema de BD.

Para respetar el contrato financiero actual, el margen abierto/desconocido se mantiene como NA; no se transforma
en cero ni en ejecución cerrada. El detalle y el margen en riesgo muestran un guion cuando falta ese dato.
Los cambios de razón social no multiplican unidades comerciales al asociar la última factura.

## Presentación

Se usan paneles y filtros `racafeForms`, tarjetas `cajaValor_ui`, modales corporativos y tablas Vancouver del CRM.
Los detalles se registran una sola vez por módulo. Las filas TOTAL no abren modales; cada selección de celda se
valida contra el conjunto vigente. Los NIT se exportan como texto. La app de prueba incluida en el adjunto no se carga.

## Validación

Desde la raíz del repositorio, con las dependencias habituales del CRM:

```sh
Rscript tests/test_cohortes_estados.R
Rscript tests/test_flujo_cohortes_oportunidades.R
Rscript -e 'shiny::runApp(source("tests/cohortes-estados/app.R")$value)'
```

Para la prueba de navegador, iniciar el arnés en el puerto 3875 y ejecutar
`node tests/test-cohortes-estados.cjs` con Playwright instalado; `CHROMIUM_PATH` permite usar Chromium del sistema.
La prueba verifica 22 tarjetas, detalles por corte/fila/celda, reapertura de modales, XLSX, filtros y vista móvil.

El arnés visual usa datos ficticios y no accede a los servicios del negocio. La aplicación completa requiere
la exportación habitual y las credenciales de consulta ya configuradas en el entorno de ejecución.
