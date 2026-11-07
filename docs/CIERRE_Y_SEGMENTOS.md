# Cierre financiero y revisión comercial de segmentos

- El cierre financiero usa `ultimo_periodo_pyg` exportado por DataPrep, limitado al
  último mes calendario terminado. Define la validación financiera y los indicadores
  cerrados; los movimientos provisionales no lo adelantan.
- La planificación del presupuesto usa el mes vigente del calendario. En noviembre
  de 2026 quedan noviembre y diciembre: dos meses, aunque el cierre financiero sea septiembre.
- La tabla anual y las series muestran toda la facturación disponible dentro de los
  filtros, incluidos meses abiertos y futuros. El cierre no elimina esas filas.
- La meta roja solo se dibuja para el mes vigente y los siguientes. Su base de
  ejecución no incorpora facturas futuras.
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
  clientes del CRM por NIT principal y línea. La segmentación de creación no determina
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


La prueba `tests/test_budget_open_billing.R` verifica la fuente real del módulo con
facturas en septiembre, octubre, noviembre y diciembre, incluido un mes futuro.


## Presentación y cargadores

Los NIT se convierten a texto únicamente al presentar las tablas, sin separadores,
decimales ni notación científica. Los valores utilizados por filtros, selección y
operaciones de datos conservan su tipo original. Las columnas cuyo título sea NIT
también reciben esa regla.

Los enlaces del sidebar colapsado tienen hovers grises y etiquetas accesibles;
se limpian al expandir el menú y se incluyen las opciones generadas dinámicamente.

La actividad de toda la app utiliza un único `racafeShiny::racafeLoaderUI` con
`busy = TRUE`. El inicio conserva el preset `racafePreloader` de la misma librería.
Se eliminan spinners propios, overlays locales y llamadas manuales a waiter en los
módulos para evitar cargadores simultáneos.

Pruebas adicionales: `tests/test_budget_calendar_nit.R` y
`tests/test_budget_red_trace.R`. El arnés local verificó 278 llamadas públicas de
las librerías y preservó las 23 operaciones de datos de 17 scripts.
Chromium probó hovers al colapsar/expandir y con menú dinámico, el cargador único
busy/idle con racafeShiny y waiter reales, y NIT completos. La conexión Shiny del
navegador se simuló; no se ejecutó la app completa con servicios de negocio.
