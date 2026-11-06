# CRM Nacional — integración de componentes corporativos

La UI utiliza racafeForms, racafeShiny y racafeModulos. Las consultas, reglas comerciales y transacciones
permanecen en CRM Nacional. No se instalan paquetes ni se ejecutan migraciones de datos al iniciar la aplicación.

## Actualizar dependencias antes de desplegar

Se requieren racafeForms >= 0.7.0, racafeShiny >= 1.3.0 y racafeModulos >= 0.3.1.
Para el ajuste de presentación usar `ref = "codex/header-list-presentation"` al instalar racafeModulos
mientras se revisa el PR; después de integrarlo puede omitirse ese argumento.

```r
install.packages("remotes")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeForms")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeShiny")
remotes::install_github("HCamiloYateT/racafeModulos", ref = "codex/header-list-presentation")
```

Instalar las librerías antes de actualizar los scripts del CRM. racafeShiny 1.3.1 conserva los nombres antiguos de
CajaValor como alias opcionales; el resumen ejecutivo llama directamente a racafeModulos.

## Cambios por área

| Área | Integración |
| --- | --- |
| Encabezado | ListItem/ListGroup, CompactTable, badges y ModalDetail; elimina onclick y CSS/JS específicos del navbar |
| Indicadores | Estilo gt compartido y overlay restringido al panel, con estado inicial de carga |
| Tareas | Checklist nativo con claves estables y fecha compartida; conserva persistencia y confirmaciones |
| Cartera | Overlay local con trigger de confirmación y limpieza mediante on.exit |
| Cotizador | Tabla compacta con texto escapado, badges, total monetario y modal corporativo de detalle |
| Crear notas y competencia | ModalDetail y overlay local de confirmación; conserva validaciones y escrituras |
| Filtros y formulario de persona | Revisados; ya utilizan componentes corporativos, sin cambios |
| Resumen ejecutivo | Cambia únicamente el propietario de cajaValor_ui/server a racafeModulos |

Se mantienen los loaders existentes del PDF y de la consulta de forma de pago: representan operaciones
independientes del guardado local. No se cambian el PDF, las fórmulas de precios ni sus consultas.
Los overlays locales cierran en on.exit, incluso si falla la operación o una validación retorna temprano.
No convierten tareas síncronas en asíncronas.

## Pruebas aisladas

```sh
Rscript tests/check_ui.R .
```

Compara SQL, llamadas a BD y funciones comerciales protegidas con el commit base `4e33801`, y verifica
conservación de comentarios y construcción de UI. Puede indicarse otra referencia como segundo argumento.
No carga global.R ni ejecuta servidores de negocio. Requiere shiny, bs4Dash y los paquetes corporativos instalados.

```r
shiny::runApp("tests/ui", port = 4080, launch.browser = FALSE)
```

La aplicación de prueba compone las UI reales con datos ficticios y un servidor de demostración sin consultas
ni escrituras. Incluye encabezado, cuatro formularios del controlbar y ambos nombres de CajaValor.
Con Playwright instalado y Chromium disponible:

```sh
CRM_TEST_URL=http://127.0.0.1:4080 CHROMIUM_PATH=/ruta/chromium node tests/test-ui.cjs
```

NODE_PATH debe permitir resolver Playwright. En CI usar CI=1 cuando Chromium requiera --no-sandbox.
Resultados y limitaciones en [docs/VALIDATION.md](docs/VALIDATION.md).

Antes del despliegue, validar en staging los guardados, permisos, transacciones y descarga PDF con los servicios
reales. Este cambio no modifica datos, credenciales ni servicios FastAPI y no despliega la aplicación.

## Listados de encabezados

Los nombres y títulos ocupan el ancho del menú. NIT, línea, responsable, vencimiento y avance se muestran
como texto secundario; solo prioridad y estado usan badges. La fecha de actualización de clientes antiguos
es metadato y el NIT se presenta una sola vez. Los indicadores tienen mayor fuente/espaciado y sus importes
se mantienen en una línea. El contenido usa una sola barra vertical; el footer permanece visible.

Para revisar los casos de nombres largos, listas extensas, clientes antiguos e indicadores en escritorio y móvil,
ejecutar `tests/ui` y `node tests/test-header-presentation.cjs`, con las mismas variables que la prueba anterior.
Este cambio afecta presentación; conserva filtros, orden de datos, selección y operaciones comerciales.
