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
