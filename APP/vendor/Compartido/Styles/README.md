# Guía de estilos compartidos

El archivo generado `style.css` reúne los estilos corporativos en un único CSS compatible
con includeCSS(). No debe editarse directamente.

## Bloques principales incluidos

- Configuración general de la app.
- Tipografía base y títulos.
- Componentes UI (tabs, modales, popups, tarjetas).
- Clases utilitarias para footers de cajas modales.
- Estilos globales para tablas `reactable`, `TablaPresupuesto` y `gt`.


## Tamaños de modales

Para evitar que todos los modales compartan el mismo ancho, el CSS expone clases de tamaño reutilizables. Pueden aplicarse al contenedor `.modal` o directamente a `.modal-dialog`, según cómo construya el modal cada app; también existen alias con prefijo `modal-` para integraciones que siguen la convención de Bootstrap (`modal-ventana`, `modal-subventana1`, etc.).

| Clase | Uso sugerido | Ancho máximo |
| --- | --- | --- |
| `ventana` | Modal principal o vista amplia | `1200px` |
| `subventana1` | Submodal amplio | `1050px` |
| `subventana2` | Submodal medio-amplio | `900px` |
| `subventana3` | Submodal medio | `760px` |
| `subventana4` | Submodal compacto | `620px` |
| `subventana5` | Submodal pequeño | `500px` |
| `aviso` | Confirmaciones, alertas o mensajes breves | `380px` |

Todos los tamaños usan límites responsivos en `vw` para no desbordar pantallas pequeñas.

## Cómo extender sin romper compatibilidad

- Agregar nuevas clases en lugar de modificar comportamiento existente cuando sea posible.
- Si se cambia una clase ampliamente usada, documentar impacto en el `README.md` principal.
- Evitar selectores excesivamente específicos que dificulten sobrescritura local.

## Checklist antes de publicar cambios

1. Confirmar que no se degradan pantallas existentes.
2. Revisar que colores y tipografía mantengan coherencia visual.
3. Verificar en resolución desktop (mínimo) y, si aplica, móvil.

## Componentes racafeForms (recursos 1.0.0)

`style.css` contiene tokens, componentes RF, tamaños RF (`racafe-modal-sizes.css`)
y reglas generales (`application.css`), en ese orden y sin imports relativos.
Este módulo añade exclusivamente variables `--rf-*` y clases `rf-*` para paneles,
tarjetas, métricas, cuadrículas informativas, barras de acciones, validación, campos
readonly y mensajes. Los colores y las familias de fuente proceden de las variables
y tipografías corporativas del propio `style.css`; no descarga fuentes ni recursos externos.

Los modales usan las clases de tamaño de la tabla anterior directamente. No se
mantiene una segunda tabla de anchos en racafeForms ni se busca contenido mediante JS.
Las nuevas reglas de contenido están acotadas a `.rf-modal`. Las reglas globales
anteriores se conservan para no cambiar las aplicaciones existentes.

La adopción es explícita con `racafeForms::racafeSharedAssets()`. Las aplicaciones
que continúen usando solo los assets base de racafeForms conservan su apariencia.
Dentro de aplicaciones que adopten Compartido, el acento usa `--color-red-primary`
y la tipografía corporativa, con alternativas locales si no están instaladas las fuentes.

## Construcción y regresión

| Archivo | Función |
| --- | --- |
| tokens.css | Variables corporativas; rojo principal #DA291C |
| racafe-forms.css | Componentes limitados a clases rf- y variables --rf-* |
| racafe-modal-sizes.css | Variables y tamaños RF precargables, sin modificar modales ajenos |
| application.css | Reglas generales anteriores, sin cambios de selectores |
| style.css | Resultado generado autocontenido para includeCSS o link |
| components.css | Entrada independiente mediante imports, para recursos servidos por URL |
| ../utils/build_styles.R | Construcción y verificación sin dependencias adicionales |

Desde la raíz del repositorio:

```sh
Rscript utils/build_styles.R
Rscript utils/build_styles.R --check
Rscript tests/test-build-styles.R --reference=/tmp/compartido-reference.css
```

También puede invocar el script por ruta absoluta desde otro directorio, o pasar una raíz
alternativa como último argumento. --check devuelve un error si falta style.css o no coincide
con sus fuentes; no modifica el archivo. No usa timestamps ni el contenido del destino para
generar la salida, por lo que no acumula reglas. La construcción elimina únicamente imports
incondicionales de las fuentes consolidadas; rechaza otros imports para evitar un bundle
incompleto. No modifica comentarios ni literales que contengan el texto @import.

La referencia de navegador utiliza el commit `38b65d4`, anterior a esta corrección, expandiendo
sus imports en el orden solicitado. Se genera fuera del repositorio: no se mantiene una
segunda copia manual de CSS. Requiere disponer de ese commit en Git.

Instale shiny, bs4Dash, htmltools y la revisión de racafeForms que incluye variantes y
racafeInputDateTime. Para las pruebas de navegador instale Playwright y Chromium. La app
mínima usa el mismo `includeCSS()` inline que los consumidores; no llama al loader de
Compartido ni añade CSS de corrección local.

```sh
COMPARTIDO_REFERENCE_CSS=/tmp/compartido-reference.css Rscript -e \
  'shiny::runApp("tests/styles-regression", port=4011, launch.browser=FALSE)'
COMPARTIDO_TEST_URL=http://127.0.0.1:4011 node tests/browser/test-styles.cjs
```

El test admite NODE_PATH, CHROMIUM_PATH y CI=1. `?mode=reference` abre el CSS de referencia;
la URL predeterminada usa el generado. `?forms=0` permite comprobar que añadir formularios
no altera el sidebar. La referencia y el generado deben producir las mismas reglas CSSOM,
estilos calculados y captura del sidebar. Se prueban menús activos/inactivos, submenús,
marca, header, content y footer, y se repite la comparación en móvil. Se verifican controles,
calendario fecha-hora, mensajes, modal y las nueve variantes/densidades del formulario.

No se demuestra una comparación con CRM Nacional: no se modificó ni ejecutó esa aplicación.
Las reglas generales heredadas, incluida su tipografía, navegación y modales, se conservan;
la corrección restaura su acceso a las variables al cargar inline. Las nuevas reglas rf-
no se extienden a iconos, menús ni controles ajenos. components.css sigue requiriendo un
contexto URL para sus imports; use style.css con includeCSS().

Resultados de esta corrección: **26 comprobaciones R** y **69 comprobaciones de navegador**
superadas con Chromium. La captura del sidebar coincide píxel a píxel con la referencia
expandida de 38b65d4; las reglas CSSOM y los estilos calculados de los selectores indicados
son iguales. No se registraron errores JavaScript, fallos HTTP ni solicitudes de los imports
relativos incorporados. Se conserva íntegramente el contenido de las tres fuentes originales,
exceptuando los dos imports retirados al separar application.css.

## Precarga y fuente canónica de modales

racafe-modal-sizes.css reúne las dimensiones existentes en variables --rf-modal-max-* y
reglas limitadas a .rf-modal. El constructor lee esa cuarta fuente antes de application.css;
los selectores heredados de tamaño usan las mismas variables, sin cambiar sus límites.
El resultado generado sigue siendo autocontenido. No se cambian los estilos de navegación.

racafeForms distribuye este archivo idéntico en inst/www y gallery-assets/Styles, cargándolo
en su dependencia inicial. La galería usa esa precarga; showModal tiene además una espera
segura cuando una aplicación todavía no incluyó componentes RF al iniciar.

La regresión compara las declaraciones generales con las anteriores resolviendo las nuevas
variables de dimensiones y excluyendo únicamente los nuevos tokens/reglas RF de tamaño.
Los estilos calculados y la imagen del sidebar siguen comparándose íntegramente. Pasan
27 comprobaciones de construcción y 69 de navegador; el nuevo caso modal/controlbar pasa
58 verificaciones. La revisión se entrega mediante PR sin cambiar ASSETS_VERSION.

## Controles y resúmenes corporativos

`racafe-controls.css` centraliza menús picker, mayúsculas visuales, checks rojos, tablas compactas,
totales, separación vertical, listas de avisos y advertencias de validación. Solo utiliza clases rf-.
Los tamaños de modal permanecen en su archivo canónico racafe-modal-sizes.css.

`style.css` consolida tokens, racafe-forms, racafe-modal-sizes, racafe-controls y application,
en ese orden. Sigue siendo autocontenido para includeCSS(URL); no edite el resultado generado.
`components.css` importa los mismos componentes sin application.css. Se conservan los recursos
individuales y las reglas de navegación anteriores. Reconstruya con `Rscript utils/build_styles.R`
y compruebe con `--check` y `Rscript tests/test-build-styles.R`.

racafeForms distribuye copias exactas para funcionamiento local; sincronice desde Libreria-Racafe
con `Rscript racafeForms/tools/sync_shared.R /ruta/Compartido`. No edite las copias de la galería.
Esta revisión conserva ASSETS_VERSION 1.1.0 y no constituye una publicación.
