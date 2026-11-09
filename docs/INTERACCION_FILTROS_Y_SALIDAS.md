# Interacción, filtros y salidas

La revisión reproduce el flujo con los módulos reales de Tareas, Oportunidades, Pendientes y Calculadoras,
además de gráficos ggplot/base y Plotly, con fuentes sintéticas y sin escrituras de producción.

## Causas corregidas

- `Compartido/Styles/style.css` oculta `.shiny-output-error`. Shiny también usa esa clase para «sin datos»:
  el mensaje se generaba pero no era visible. El CSS del CRM restablece su visibilidad; las validaciones
  no reciben un prefijo técnico de error. Los widgets fallidos siguen ocultando su gráfico anterior.
- En tareas, listas vacías omitían el filtro y el refresco restauraba «Todas». Ahora vacío significa cero
  registros; el refresco conserva la selección y las etiquetas se obtienen solo de registros autorizados.
- En filtros dependientes y controlbar se distinguió la primera carga del vacío explícito. No se vuelve
  a seleccionar todo al actualizar una fuente. Al reaparecer opciones tras vaciar un filtro padre,
  la cascada puede recuperar su selección global anterior.
- En presupuesto, NULL y una combinación especial de segmentos se trataban como selección completa.
  Se eliminaron esos comodines. El servidor principal también conserva una selección vacía como un
  data.frame sin filas, en lugar de suspender silenciosamente la cadena de datos.
- Los gráficos que devolvían un widget vacío o `req()` sin mensaje ahora muestran validación explícita.
  Los fallos inesperados muestran un aviso legible y registran la causa original en los logs de R.
  No se convierten fallos de cálculo en resultados cero.

## Preloader

`racafeShiny::racafeLoaderUI` sigue siendo el único preloader de actividad. Un cambio real de campo o
navegación lo activa en el navegador antes del cálculo. El observador `crm_recalcular` asegura un ciclo
busy/idle incluso para una interacción sin outputs invalidados. No consulta datos ni escribe en base.
Las entradas repetidas se deduplican y no se añaden temporizadores que oculten un cálculo todavía activo.
La librería lo cierra al terminar el ciclo o desconectarse, incluidos los errores de cálculo.

## Encabezados, notas y gráficos

Oportunidades: Nueva oportunidad/Actualizar, después Contacto/Mostrar/Agrupación y luego KPI y detalle.
Tareas: Nueva tarea o nota/Actualizar, después Buscar/Etiquetas/Prioridad y debajo los dos switches.
Pendientes conserva el flujo de pedidos/lotes, añade esas acciones y sitúa sus filtros antes de las KPI.
Las filas corporativas se adaptan al ancho disponible; las etiquetas y controles quedan alineados.

Las listas con selección global usan picker múltiple y sus acciones nativas; se retiraron opciones
artificiales «Todos/Todas». «Solo total» continúa siendo una opción de agrupación, no un filtro global.
Los tooltips consultan los valores seleccionados, se sitúan arriba o abajo con 10 px de separación,
se ajustan al viewport y se ocultan al abrir el menú, desplazarse, cambiar de foco o pulsar una tecla.

Las notas usan `racafeForms::racafeNoticeList`, pequeñas, grises y con elementos separados. Adaptadores
para CajaModal/TablaReactable2 conservan los módulos y sus IDs, pero sitúan las notas de Forms fuera
del componente anterior. Las notas de tablas Vancouver y las del detalle del cliente siguen el mismo patrón.

Los temas compartidos fijan tamaños coherentes de título, ejes y leyenda, sin cambiar datos, rangos ni
unidades. Plotly conserva sus títulos y configura márgenes automáticos; las etiquetas pequeñas se elevan
al mínimo legible. Se revisaron 1440 px, 1366×768, 1920×1080 y móvil de 390 px.

## Reproducir

Con las librerías corporativas y los paquetes de desarrollo instalados, desde la raíz:

```sh
Rscript -e "shiny::runApp('tests/interaccion', host='127.0.0.1', port=3876)"
# En otra terminal con Playwright/Chromium:
node tests/test-interaccion.cjs
```

`CRM_TEST_URL` y `CHROMIUM` permiten cambiar URL y ejecutable. Los screenshots se guardan en `/tmp`
y pueden redirigirse con `CRM_TEST_DESKTOP` y `CRM_TEST_SCREENSHOT`.
El fixture usa la hoja de estilos de Compartido; `options(crm.test.shared_css = '/ruta/style.css')`
permite usar una copia local. Las creaciones y escrituras están deshabilitadas en este fixture.

Pruebas específicas: `tests/test_filtros_multiples.R`, `tests/test_salidas_estados.R` y
`tests/test_tasks_board.R`. Las regresiones de contrato, presupuesto, cálculos, productos y oportunidades
siguen verificando las fórmulas y los permisos. La base de producción no se utilizó para estas pruebas.
