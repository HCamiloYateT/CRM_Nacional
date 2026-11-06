# Presentación financiera y presupuestos

Los adaptadores de `APP/misc/00_presentacion.R` usan `racafeModulos` y `racafeForms`
para cajas, notas y modales; las tablas estáticas usan `racafeShiny`.
Se mantienen los recursos remotos de Compartido configurados en `global.R`.

- Botones de 22 × 22 px, solo icono, con tooltip y nombre accesible.
- Las cajas informativas no tienen acción. En el detalle de cliente solo pedidos,
  lotes y facturación abren detalle; las seis cajas tienen pie explicativo.
- Las tablas Vancouver y las cabeceras de TablaReactable2 usan `#495057`
  (gris oscuro de Compartido) y una línea de 1.5 px.
- Validación financiera oculta el mensaje de integridad cuando no hay diferencias;
  conserva la alerta cuando existen diferencias. Conserva los enlaces de las matrices.
- Presupuestos mantiene sus formatos, filtros y ordenamiento. Los detalles usan
  TablaReactable2 y los modales y notas usan racafeForms.
- Una tabla con detalle limpia su selección después de abrirlo para permitir
  consultar nuevamente la misma fila.

Versiones mínimas ya exigidas por la app: racafeForms 0.7.0,
racafeShiny 1.4.1 y racafeModulos 0.5.1.

## Validación

Con las dependencias corporativas instaladas, ejecutar desde la raíz:

```r
Rscript tests/test_presentation.R
```

En el entorno de edición se verificó:

- Parseo de todos los archivos R modificados.
- Conservación exacta de 31 expresiones reactivas de validación financiera
  y 26 de presupuestos respecto de main.
- 19 comprobaciones de HTML, pies, acciones informativas y apertura única de modal,
  ejecutadas con las fuentes actuales de las librerías; se simularon el formateador
  numérico y el registro de dependencias estáticas.
- Chromium con el HTML y CSS reales de las cajas: tres botones sin texto,
  con tooltip, dimensiones de 22 × 22 px y seis pies explicativos.
- Revisión de las firmas públicas usadas contra las fuentes de las librerías.

No se ejecutó la app completa con servicios de negocio, datos productivos ni la
suite integrada de navegador. Las pruebas existentes de cliente y KPI se ajustaron
al nuevo número de acciones y al adaptador de cajas.
