# Validación de integración — 2026-11-06

Referencias iniciales: CRM Nacional `4e33801`, Libreria-Racafe `4351c16`, racafeModulos `ef449ec`.
Entorno: R 4.5.0, Shiny 1.10.0, bs4Dash 2.3.5, gt 0.11.1, Chromium/Playwright.

| Ejecución | Resultado |
| --- | --- |
| Parse de los 39 scripts de módulos | Correcto |
| Verificador de la migración del encabezado | 40 comprobaciones correctas |
| tests/check_ui.R sobre los archivos reales | 33 comprobaciones correctas |
| Navegador sobre tests/ui con interfaces reales | 15 comprobaciones correctas |
| testthat racafeModulos completo | 91 PASS, 0 FAIL, 0 WARN |
| testthat racafeShiny CajaValor/navbar/loaders/UI | 227 PASS, 0 FAIL, 0 WARN |
| testthat racafeShiny completo | 343 PASS, mismos 28 fallos preexistentes, 0 WARN |
| R CMD build racafeShiny 1.3.1 y racafeModulos 0.3.0 | Correcto |
| R CMD check racafeModulos --no-manual | Sin errores, 4 advertencias y 1 nota históricas |
| R CMD check racafeShiny --no-manual --no-tests | Sin errores, 1 advertencia y 2 notas históricas |

Las advertencias de paquetes corresponden a licencia, NEWS, caracteres no ASCII, llamadas históricas
library/require y documentación pendiente de objetos antiguos; los objetos nuevos tienen ayuda Rd y no
presentan discrepancias entre código y documentación. La suite completa de racafeShiny se ejecutó por
separado: mantiene los 28 fallos sobre formatos y clases de modales ya comprobados en main en la tarea
anterior. El check de empaquetado de ese paquete se ejecutó explícitamente con --no-tests.

La comparación estática conserva literales SQL, llamadas DBI/racafeBD, transacciones y cuerpos de funciones
comerciales protegidas, además de los comentarios originales. Se revisaron también filtros y formulario
personal; no necesitan modificaciones. El cambio del resumen ejecutivo se limita al namespace de CajaValor.

El navegador verificó los cinco li del navbar, selección real de clientes y su clave, conservación de menú
al abrir un modal, checklist reactivo, los formularios reales del controlbar, actualización de valores incluido
cero mediante módulo nuevo y alias anterior, eventos de detalle independientes, números 15,425, escaping de
productos y ausencia de IDs duplicados y errores JavaScript.

La prueba aislada no carga global.R ni inicia los servidores de negocio. No dispone de los servicios ni de
la dependencia racafeBD del despliegue. No se probaron conexiones, transacciones, permisos reales, guardados
ni generación/descarga PDF. Esos flujos requieren staging antes de desplegar. No se ejecutaron operaciones BD,
no se modificaron credenciales y no se desplegó la aplicación.

Orden de integración: racafeModulos 0.3.0 y racafeShiny 1.3.1, después CRM Nacional. Los módulos nuevos de
racafeModulos funcionan con racafeShiny >= 1.3.0; para alias de racafeShiny 1.3.1 instalar racafeModulos 0.3.0.
No hay import obligatorio inverso de racafeShiny a racafeModulos.

## Ajuste visual de encabezados — 2026-11-06

- racafeModulos 0.3.1: 91 pruebas testthat correctas; instalación y construcción correctas.
- R CMD check sin errores; conserva las 4 advertencias y 1 nota históricas descritas arriba.
- CRM: 41 comprobaciones de SQL, funciones protegidas, comentarios y construcción de UI correctas.
- Navegador: 48 comprobaciones de presentación en anchos 1440 y 390, con nombres largos y listas extensas.
- Regresión de integración CRM: 15 comprobaciones correctas.
- Regresión de la galería navbar: 30 comprobaciones correctas.

Se verifican títulos a todo el ancho, NIT único, metadatos de actualización, dos badges por tarea,
selección con menú abierto, una sola zona de desplazamiento, pie fijo, importes sin saltos y ausencia de
desbordamiento horizontal y errores R/JavaScript. Se inspeccionaron capturas de clientes, tareas e indicadores.
Se mantienen los alcances de staging y las restricciones de pruebas de negocio descritos arriba.

## Modal de clientes y Vancouver — 2026-11-06

Referencias iniciales: CRM `f71acf7`, Libreria-Racafe `694b04d`, racafeModulos `78dd9cb`.
Versiones resultantes: racafeShiny 1.4.0 y racafeModulos 0.4.0; racafeForms no cambia.

| Validación | Resultado |
| --- | --- |
| testthat de tabla Vancouver | 19 PASS, 0 FAIL/WARN |
| testthat racafeModulos completo | 98 PASS, 0 FAIL/WARN |
| testthat racafeShiny completo | 362 PASS; mismos 28 fallos preexistentes, 0 WARN |
| Comparación estática y construcción UI CRM | 43 comprobaciones correctas |
| Navegador: tabla y modal real con datos ficticios | 29 comprobaciones correctas |
| Navegador: integración CRM anterior | 15 comprobaciones correctas |
| Navegador: presentación de encabezados | 48 comprobaciones correctas |
| Construcción/instalación de paquetes | Correctas |
| R CMD check racafeModulos | Sin errores, 4 WARNINGs/1 NOTE históricas |
| R CMD check racafeShiny --no-tests | Sin errores, 1 WARNING/2 NOTEs históricas |

Se verificó la línea roja continua de 2px, ausencia de cuadrícula/bordes verticales, alineación automática y
por columna, reconocimiento de Total, fondos iguales en su intersección, opciones de color/grosor/espaciado,
compatibilidad con las tablas anteriores y escaping. La ayuda Rd nueva se generó y verificó con R CMD check.
El check de racafeShiny omite tests explícitamente: la suite completa se ejecutó por separado y mantiene los
28 fallos anteriores de formatos/clases de modales, documentados en las tareas previas.

En el modal se verificaron 1200px máximos, seis cajas de altura uniforme con borde/icono rojo, fechas,
actualización al cambiar cliente, formato numérico, historial, detalle con scroll horizontal y adaptación a
390px sin desbordamiento exterior. Tras abrir varios clientes no hay IDs duplicados ni errores R/JavaScript.
El módulo distingue cantidades desconocidas y cero cuando se solicita empty.

El verificador conserva SQL, transacciones y funciones comerciales protegidas. Para las dos funciones que
componen ahora las UI se comparan las transformaciones de datos y cálculos originales, permitiendo sustituir
exclusivamente su presentación. Se conserva el trabajo previo de main y sus cambios de formulación.
No se ejecutaron servidores comerciales, consultas, guardados, transacciones ni PDF. Mantener la revisión
con servicios y permisos reales en staging antes del despliegue.

## Compactación de cajas y línea fina

Versiones: racafeModulos 0.4.1 y racafeShiny 1.4.1. Las cajas del cliente activan compact: padding 8px,
gap vertical 3px, icono/valor/detalle reducidos. El tamaño anterior se conserva por defecto en otras cajas.
Vancouver usa línea inferior de encabezado de 1px por defecto; CRM la solicita explícitamente y reduce
el padding de fila a 3px. No hay cambios de consultas, cálculos ni módulos de servidor.

Validación: 98 pruebas de racafeModulos, 19 de Vancouver, 43 comprobaciones estáticas/UI del CRM y 30
comprobaciones de navegador correctas. Se verifica altura uniforme inferior a 100px, línea roja de 1px,
actualización entre clientes, totales, scroll y vista móvil. Paquetes instalados y ayuda roxygen2 actualizada.
No se ejecutaron operaciones BD ni se desplegó la aplicación.

## Tarjetas con borde lateral y footer configurable

Requiere racafeModulos 0.5.0. Las cajas mantienen valores, fechas e IDs, con borde lateral, título/icono,
sombra suave y fechas en el footer. color_kpi/color_footer controlan sus colores de forma independiente.
El CRM mantiene las seis cajas sin botón porque no tienen una acción definida. La biblioteca permite
mostrarlo abajo a la derecha y la aplicación decide qué hacer con click_detalle.
Los overrides anteriores del resumen ejecutivo se aplican solo a tarjetas heredadas para que la nueva
apariencia sea responsabilidad del componente compartido. Los valores de sus KPIs se mantienen oscuros.

Validación: 98 pruebas testthat correctas; 43 comprobaciones estáticas/UI del CRM correctas; 30 de navegador
del modal de clientes y 25 de tarjetas/acciones correctas. Se prueban navegación de pestaña, apertura de modal,
teclado sin eventos duplicados, botón ausente, colores independientes, cero reactivo, namespaces, guías sin KPI
y vista móvil sin desbordamiento. R CMD build/instalación correctos; R CMD check sin errores, con las mismas
4 advertencias y 1 nota históricas de licencia, ASCII, código anterior y documentación pendiente de otros objetos.
No se ejecutaron consultas, transacciones ni guardados reales.
