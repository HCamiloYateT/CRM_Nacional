# Cierre financiero y revisión comercial de segmentos

- El calendario usa `ultimo_periodo_pyg` exportado por DataPrep. La fecha se limita
  al último mes calendario terminado. Los movimientos provisionales del mes en
  curso no definen el cierre. Sin cierre válido se muestran 12 meses pendientes.
- La ejecución de presupuesto se corta en ese cierre, tanto en tablas como en
  indicadores y proyecciones. En noviembre de 2026, con cierre a octubre, quedan
  noviembre y diciembre: dos meses. Un cierre más antiguo conserva ese rezago.
- El presupuesto oculta las cuatro columnas de comparación con el período anterior;
  conserva el año anterior y las columnas de ejecución, presupuesto y cumplimiento.
- Las convenciones aparecen en una fila y se adaptan a pantallas pequeñas.
  Se retiran las columnas Nota de las tablas por dimensión.
- El switch de racafeForms tiene pista roja al activarse, gris al desactivarse,
  área clicable definida y acceso por teclado.
- Los títulos de modales no incluyen iconos. Las descargas se alinean a la derecha
  con 16 px de separación antes de las cajas.
- Cuadre mensual: gt rechaza `inherit` como color. Se sustituye por gris #495057
  y el adaptador omite ese valor CSS si aparece en otras definiciones de columna.
- Segmentos: la comparación principal usa PyG financiero frente al snapshot de
  CRMNALCLIENTE por NIT principal y línea. La segmentación de creación no determina
  diferencias. Las matrices se reemplazan por una lista de clientes, con asesor,
  segmento PyG, segmento CRM y estado. Inicialmente se muestran los casos por
  revisar; se puede consultar el universo completo y descargar el detalle.

## Validación

Con las dependencias corporativas instaladas, ejecutar desde la raíz:

```sh
Rscript tests/test_presentation.R
Rscript tests/test_tables_presentation.R
Rscript tests/test_closed_months_segments.R
Rscript tests/test_monthly_reconciliation.R
```

Las pruebas locales verificaron cajas y acciones, formato y búsqueda de tablas,
cuadre con diferencias cero y meses sin movimiento, selección de octubre, cierre
con dos meses pendientes y comparación PyG/CRM sin segmentación de creación.
Se usaron las fuentes de las librerías y de colDef/colFormat de reactable 0.4.4,
con gt 0.7.0 adaptando en el arnés dos opciones de padding no disponibles en esa
versión. Se simuló el registro de assets y el formateador numérico de caja.

Chromium verificó convenciones en una fila, cambio de estado y colores del switch,
y alineación y separación de las descargas con el widget real de shinyWidgets 0.9.0.
No se ejecutó la aplicación completa con servicios y datos de negocio.
