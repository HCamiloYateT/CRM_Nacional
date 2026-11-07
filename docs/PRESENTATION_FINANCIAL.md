# Presentación financiera y presupuestos

## Dependencias

Instalar primero racafeForms 0.7.1 y racafeShiny 1.4.2 del [PR #114](https://github.com/HCamiloYateT/Libreria-Racafe/pull/114)
que acompaña este cambio. Se conserva racafeModulos 0.5.1 y la conexión remota
existente a Compartido/main.

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeForms")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeShiny")
```

## Comportamiento

- Pies de caja fuera del borde, alineados a la izquierda y con separación en líneas.
- Notas de tabla externas al área de desplazamiento, en gris #495057 y 11 px,
  mediante racafeShiny::racafeTableNote.
- Convenciones de presupuesto con cuadros de color para año anterior, mensual y acumulado.
- racafeForms::racafeNoticeList permite font_size, text_color y accent_color;
  la metodología usa texto pequeño gris y acento rojo.
- Vista acumulada usa racafeForms::racafeInputSwitch.
- Cumplimiento por dimensión y detalles usan racafeShiny::racafeTableVancouver,
  cabecera gris #495057 y ancho CSS de 1.5 px. Los detalles tienen filas compactas.
- Las tablas conservan búsqueda, ordenamiento por cabecera, paginación y selección
  por clave original. TOTAL permanece al final al ordenar. Se puede reabrir la misma fila.
- Validación financiera usa columnas 7/5. El título redundante se sustituye por
  una nota sobre meses cerrados financieramente.
- Descargas con racafeForms::racafeButtonDownload, variante secondary.
- Títulos de modales con iconos Font Awesome. Hovers grises activados al pasar
  el cursor; se ocultan al hacer clic, desplazarse o cerrar un modal.

## Validación

Desde la raíz, con las dependencias corporativas instaladas:

```sh
Rscript tests/test_presentation.R
Rscript tests/test_tables_presentation.R
```

Se verificaron 57 expresiones reactivas de negocio idénticas a main, el parseo R
y 54 llamadas a las APIs públicas de las librerías.

En este entorno se ejecutaron las 19 comprobaciones de cajas y acciones,
pruebas de búsqueda, ordenamiento, formatos, selección estable y reapertura de
modal, y las nuevas pruebas de notas de ambas librerías. Se usaron sus fuentes;
se simularon el registro de assets y el formateador numérico de caja. El
renderizado de tablas usó gt 0.7.0 con adaptación de pruebas para omitir dos opciones
de padding de contenedor que esa versión no admite. No se cambia esa API en producción.

Chromium verificó seis pies externos alineados al borde izquierdo, fuente de
11 px, texto gris, convenciones de color y hovers grises que desaparecen al hacer
clic o cerrar el modal. El CSS declara bordes de 1.5 px; el navegador puede
redondear su medida calculada según la escala de pantalla.

La app completa con datos y servicios de negocio no se ejecutó en este entorno.
