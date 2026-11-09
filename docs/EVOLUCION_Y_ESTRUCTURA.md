# Evolución del Negocio y estructura modular

`GE_Evolucion` usa `EvolucionNegocioUI()` y `EvolucionNegocio()`, en
`APP/misc/modules/generalidades/evolucion_negocio/`.

## Lectura de la presentación

- Láminas 5–7: historia anual de Industria Nacional, A la medida y Coproductos. Dos gráficas separadas
  para volumen y margen, tabla de cumplimiento y crecimiento entre años comparables.
- Láminas 8–10: desgloses dinámicos por segmento o asesor, a partir de los hechos y snapshots disponibles.
  El selector de línea distingue A la medida y Coproductos. La referencia original se puede consultar aparte.
- Lámina 11: primeras compras observadas en el año consultado, clientes únicos, volumen y margen.
  La fecha del sistema evita contar como nuevo un cliente con facturación previa conocida.
- Lámina 3: escenario estratégico 2026–2029, con metas separadas de la ejecución y del presupuesto vigente.
- Láminas 13–15: fotografía original del pipeline y potencial, con acceso a los tableros vigentes de
  Oportunidades y Prospección. No se suman oportunidades con potenciales ni se confunden con ejecución.

La presentación no es una especificación de permisos ni se ejecuta código de los adjuntos.
Sus valores se extrajeron de las series de los gráficos, no de los porcentajes decorativos.
Los importes en millones se convirtieron a COP. Se conservan la lámina, año y naturaleza del dato.
Los gráficos usan millones COP; las tablas y descargas conservan COP y sacos equivalentes de 70 kg.

## Fuentes, prioridad y comparabilidad

1. El ETL es la fuente de ejecución desde su primer mes cerrado por línea. Los filtros se aplican después
   de establecer ese límite: el histórico nunca repone ventas que el usuario acaba de excluir.
2. Los Excel aportan meses anteriores a ese límite, homologados por NIT de facturación/línea con el
   catálogo disponible. No se incorporan clientes sin una atribución comercial única ni se publica
   detalle de clientes en GitHub. Asesor y segmento son los del catálogo disponible, no su trayectoria histórica.
3. Los agregados anuales de la presentación completan años anteriores al ETL solo si no hay detalle anual,
   en vistas por línea, para administradores con todo el alcance comercial seleccionado. Se identifican
   como referencia redondeada. No se reparten entre meses, asesores, segmentos ni productos.

Al filtrar categoría o producto no se usa el Excel sin homologación de producto y no se prorratea un
presupuesto de cliente: esas vistas muestran ejecución del ETL y presupuesto vacío.
Los escenarios corporativos y la referencia integral son de solo lectura para administradores.

Por defecto el corte queda limitado al cierre financiero. La opción de abiertos incorpora facturación
provisional y distingue su margen. Los pendientes/proyectados del maestro no se suman como facturación.
El presupuesto viene de los snapshots de cada año, una vez por unidad comercial. Sin snapshot de ese año
no se inventa presupuesto cero ni se reutiliza el presupuesto vigente para toda la historia.
En la vista anual completa y con todo el alcance, los presupuestos originales de la presentación
pueden completar años hasta 2025 sin snapshot; la columna FuentePresupuesto lo identifica.
No se usan para comparaciones mensuales ni para ocultar snapshots contradictorios.

El año consultado se compara con el presupuesto proporcional a sus meses. Para crecimiento interanual
contra un año parcial hay que activar «Comparar los mismos meses de cada año». Las variaciones quedan
vacías ante meses distintos, años no consecutivos o denominador cero. CAGR exige bases positivas.
La proyección opcional es acumulado × 12 / meses; aparece como un símbolo separado, sin reemplazar el ejecutado.

## Instalar el histórico Excel

El archivo ya preparado `evolucion_historica.rds` se entrega por separado para colocarlo en
`APP/data/evolucion_historica.rds`. También puede configurarse `CRM_EVOLUCION_HISTORICA` con su ruta absoluta.
La app lo lee inmediatamente; al volver a ejecutar DataPrep queda incluido en
`homologaciones$evolucion_historica`, sin modificar los hechos operativos ni el contrato obligatorio existente.

Para regenerarlo desde los dos Excel, desde la raíz del repositorio:

```sh
Rscript scripts/importar_evolucion_historica.R \
  'P Y G coproductos de 2016 a 2022.xlsx' \
  'p y g 2023 coproductos.xlsx' \
  APP/data/evolucion_historica.rds
```

El importador requiere `readxl` y `dplyr`. Lee solo las columnas financieras de `BASE PYG`/`BASE`.
Usa fecha de factura, `SS 70 KLS` y `MNFCC$`, no sacos físicos ni utilidad después de impuestos.
Excluye `EXT` y `TRASLADOS`; conserva ajustes negativos y magnitudes desconocidas.
Homologa 20000 a 21000 siguiendo la convención vigente del ETL.
El libro «2016 a 2022» contiene fechas de 2023: por mes/línea prevalece el libro dedicado a 2023,
antes de agregar por cliente. No se deduplican filas legítimas de lotes ni se suman dos libros del mismo periodo.
Con los adjuntos se generaron 3.621 filas mensuales entre enero de 2016 y diciembre de 2023.
No se necesita regenerar el ETL para consultar el RDS local; sin él siguen disponibles el ETL y la referencia anual.

## Estructura de la app

`APP/misc` contiene únicamente carpetas:

- `config/`: parámetros y constantes generales.
- `compartido/`: presentación, utilidades y análisis reutilizados entre capítulos.
- `datos/`: contrato de exportación e importadores históricos.
- `modules/`: capítulos del sidebar y una carpeta por opción.
- `ui/`: composición del shell, encabezado, barra lateral, pie y controlbar.

Las 35 opciones del sidebar tienen un punto de entrada propio. Cohortes conserva un motor compartido con
cuatro entradas específicas. Las opciones que ya eran marcadores pendientes mantienen ese comportamiento;
reubicarlas no implementa sus funcionalidades. Acciones, encabezado y controlbar son piezas transversales.

`global.R` carga configuración, compartidos y datos antes de los módulos y construye la UI al final.
Si un archivo no carga, el arranque falla con su ruta en lugar de continuar con un módulo incompleto.
Se actualizaron DataPrep, importadores, documentación y pruebas que referencian los scripts reubicados.
El resolvedor de APP funciona desde la raíz o desde APP con la nueva ubicación del contrato.

## Verificación

`tests/test_evolucion_negocio.R` comprueba prioridad de fuentes, alcance, datos ambiguos, filtros,
meses comparables, presupuestos, proyección separada y referencias con datos sintéticos.
`tests/test_estructura_modular.R` comprueba las 35 carpetas, las fuentes literales y la ausencia de R sueltos en misc.
Las pruebas de contrato, comerciales, oportunidades, productos y presupuesto usan las rutas nuevas.
La validación de UI usa componentes Racafé reales y datos sintéticos, sin consultas ni escrituras de producción.
