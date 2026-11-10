# CRM Nacional — integración de componentes corporativos

La UI utiliza racafeForms, racafeShiny y racafeModulos. Las consultas, reglas comerciales y transacciones
permanecen en CRM Nacional. No se instalan paquetes ni se ejecutan migraciones de datos al iniciar la aplicación.

## Actualizar dependencias antes de desplegar

Se requieren racafeForms >= 0.7.0, racafeShiny >= 1.4.1 y racafeModulos >= 0.5.1.
Las referencias revisadas y los cambios de contrato están documentados en
[docs/DATA_PREP_INTEGRATION.md](docs/DATA_PREP_INTEGRATION.md).

```r
install.packages("remotes")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeForms")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeShiny")
remotes::install_github("HCamiloYateT/racafeModulos", ref = "a13fd7e06b628c8ef72c4e1c201a3d9084c78aa8")
```

Instalar las librerías antes de actualizar los scripts del CRM. racafeShiny 1.3.1 conserva los nombres antiguos de
CajaValor como alias opcionales; el resumen ejecutivo llama directamente a racafeModulos.

## Integración con la exportación actual

Después de instalar las dependencias, ejecutar `DataPrep.R` en el entorno que ya tiene acceso a las fuentes.
La app requiere regenerar `APP/data/data.RData`. Se conservan los dos objetos de la nueva exportación:
`crm_maestro` y `homologaciones`. Este último incluye las referencias de clientes, facturación, PyG y lotes
necesarias para los módulos. La carga valida ambos objetos y deriva las vistas que consumen los módulos;
`data` conserva sus 67 columnas de ejecución y excluye las filas proyectadas del maestro.
Las proyecciones y sus métricas permanecen disponibles en `crm_maestro`.

Los controles de acciones y prospección utilizan racafeForms; las tablas, menús y cajas se llaman desde
racafeModulos. Los estilos y scripts se leen desde `HCamiloYateT/Compartido`, rama `main`, al arrancar la app.
Las apps de prueba de los módulos se abren mediante funciones `Demo*()`; no se ejecutan durante el arranque.

Iniciar con `shiny::runApp("APP")` desde la raíz del repositorio o con `shiny::runApp(".")` desde `APP`.
No se requiere `vendor/Compartido`: `includeCSS()` e `includeScript()` leen los archivos de GitHub y los
incluyen en la UI. Los logos usan URLs del mismo repositorio; para el PDF se descarga un logo temporal que
se elimina al terminar. El servidor necesita acceso HTTPS a `raw.githubusercontent.com` al iniciar la app
y generar el PDF; el navegador también debe poder cargar los logos desde ese dominio.


Prueba de contrato y enriquecimiento con datos ficticios, sin conexiones de negocio:

```sh
Rscript tests/test_data_contract.R .
```

La plantilla `APP/misc/modules/controlbar/cotizacion.Rmd` no está versionada en este repositorio. El cotizador
comprueba su presencia al generar el PDF; esa ausencia ya no impide cargar el resto de la aplicación.

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

Antes del despliegue, validar en staging los guardados, permisos, transacciones y descarga PDF con los servicios
reales. Este cambio no modifica datos, credenciales ni servicios FastAPI y no despliega la aplicación.

## Listados de encabezados

Los nombres y títulos ocupan el ancho del menú. NIT, línea, responsable, vencimiento y avance se muestran
como texto secundario; solo prioridad y estado usan badges. La fecha de actualización de clientes antiguos
es metadato y el NIT se presenta una sola vez. Los indicadores tienen mayor fuente/espaciado y sus importes
se mantienen en una línea. El contenido usa una sola barra vertical; el footer permanece visible.

## Modal de clientes y tablas Vancouver

El modal usa el tamaño corporativo ventana (hasta 1200px, adaptable a móvil). Las seis cajas de clientes
sin información usan cajaValor_ui/server con iconos y borde rojo, conservando fechas y cantidades.
Los servidores se registran una sola vez y reciben los valores del resumen reactivo compartido.
Una cantidad desconocida muestra — y cero conserva su valor numérico.

Productos, detalle e historial usan racafeTableVancouver. El detalle tiene ancho mínimo de 1500px y scroll
horizontal dentro de su contenedor para evitar cortes de identificadores y fechas. La altura de contenido
continúa adaptándose al viewport con desplazamiento vertical del cuerpo del modal.

Las cajas del cliente usan `compact = TRUE`: padding de 8px y separación vertical de 3px.
Las tablas del modal usan padding de fila de 3px y línea roja de encabezado de 1px.


Las cajas usan el diseño 0.5.0 de racafeModulos: borde lateral rojo, sombra suave y footer para las fechas.
El valor usa color_kpi y el pie color_footer. Las seis métricas tienen Ver detalle para abrir las operaciones del cliente, con Volver para restaurar el resumen.
Los ajustes locales del resumen ejecutivo se limitan a tarjetas heredadas para que la presentación nueva
conserve sus estilos corporativos. No cambia la navegación ni el cálculo de sus indicadores.

## Colores y botones de KPI (0.5.1)

Valores negros y textos neutrales sin meta; el rojo decorativo queda en iconos y acento lateral.
La meta opcional colorea el valor: prop TRUE mayor/mejor y FALSE menor/mejor. No se asignan metas nuevas
a los indicadores financieros del CRM. El Resumen Ejecutivo muestra Abrir módulo usando sus rutas existentes;
los ocho KPIs financieros muestran Ver detalle y usan el evento btn_abrir de sus CajaModal existentes.
Se elimina el clic sobre toda la tarjeta para evitar acciones duplicadas. El borde derecho vuelve a ser visible.

Actualizar racafeModulos, aplicar los cambios del CRM y reiniciar la aplicación para cargar el código nuevo.

## Evolución del Negocio y organización modular

El capítulo **Generalidades → Evolución del Negocio** incorpora la lectura histórica y el plan de crecimiento.
Los scripts se organizan por opción del sidebar; configuración, contratos y utilidades tienen carpetas propias.
Vea [fuentes, instalación del histórico Excel y estructura](docs/EVOLUCION_Y_ESTRUCTURA.md).
