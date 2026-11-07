# Tablero de tareas y calculadoras

## Instalación

Actualizar racafeShiny a 1.4.3 con [Libreria-Racafe #116](https://github.com/HCamiloYateT/Libreria-Racafe/pull/116) y
reiniciar la app. El cambio corrige el relevo del preloader inicial al global si
Shiny sigue calculando. Un overlay oculto ya no bloquea el global. Los botones de
cálculo y actualización registrados muestran anticipadamente el preloader.

## Tareas

`APP/misc/modules/gestion_comercial/05_tareas.R` integra el tablero en Notas y Tareas.
El dropdown y el tablero comparten una fuente reactiva de notas. Crear un registro,
actualizar o guardar un cambio refresca ambas vistas. El botón Nueva tarea abre el
formulario existente de Crear Tareas/Notas del controlbar.

La vista incluye cuatro prioridades, búsqueda por texto, filtros de etiquetas y
prioridad, e inclusión opcional de archivadas y cumplidas. Se muestran registros
creados por el usuario o con su nombre exacto en la lista de responsables; ANA no
coincide con ANABEL. El detalle y las acciones se reutilizan desde el dropdown.

Actividades, cumplimiento, archivo, comentarios y eliminación requieren
confirmación antes de escribir. El borrado se permite al creador. La transacción
comprueba que el registro siga disponible para el usuario y que no haya cambiado
desde que se abrió; las consultas usan parámetros. Los textos se presentan escapados.

Se reutiliza la creación existente; no se importa la segunda implementación de
creación, la conexión directa ni el envío SMTP del archivo de referencia.

## Calculadoras

`APP/misc/modules/herramientas/01_calculadoras.R` contiene las calculadoras de pesos
excelso, dólares excelso y coproductos, siguiendo la ubicación del sidebar.
Sus tres expresiones de cálculo se compararon con el adjunto y se conservan.
Los controles y actualizaciones usan racafeForms; las tarjetas y los resultados
usan los componentes y estilos corporativos.

Los indicadores se comparten con el menú del encabezado y los datos de clientes
con el cache existente. El diferencial usa el indicador actual de compra. Se
permite costo cero; las divisiones exigen TRM positiva. Los defaults de pesos y dólares se inicializan una vez; coproductos actualiza los
costos al cambiar de segmento o recibir nuevos indicadores.

Calcular no escribe márgenes. Guardar márgenes es una acción separada, confirmada,
solo para los dos usuarios autorizados en el script original. Se guarda el snapshot
revisado, no cambios posteriores del formulario, y una confirmación no se reutiliza.

La opción Resumen de Gestión Comercial se retira del sidebar, body, servidor y
Resumen Ejecutivo. Se conserva el Resumen Ejecutivo de Generalidades.

## Verificación

```sh
Rscript tests/test_tasks_board.R
Rscript tests/test_calculators.R
```

Pruebas locales: fórmulas del adjunto idénticas; resultados numéricos de las tres
calculadoras; confirmación y snapshot de márgenes; pertenencia exacta y filtros de
tareas; sincronización de la fuente compartida. Las fuentes de BD y persistencia
se simularon/injectaron, y el arnés simuló el transporte de confirmación de márgenes.
Se usaron las fuentes reales de componentes y shinyWidgets.

Chromium verificó el tablero de cuatro prioridades, apertura por clave, namespaces
únicos de las tres calculadoras, vista móvil apilada sin desbordamiento y el relevo
del preloader con waiter real. No se ejecutaron escrituras ni correos reales ni la
app completa conectada a los servicios de negocio.
