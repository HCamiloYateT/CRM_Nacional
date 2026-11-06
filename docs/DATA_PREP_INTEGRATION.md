# Integración DataPrep / Shiny

Revisión del 6 de noviembre de 2026 sobre CRM_Nacional `21de5d5ce4d565b613b6d1105d0d71bf65fd33a8`.
Se revisaron los 58 scripts R de la revisión original, incluidos sus ejemplos y pruebas.
La resolución de conflictos incorpora el ETL de `main` en `9ed885903fdd73fad19cb5b5995811d8f2ba446b`,
conservando sus proyecciones, trazabilidad, auditorías y exportación maestra.

## Fuentes revisadas

| Repositorio | Revisión |
| --- | --- |
| racafeModulos | `a13fd7e06b628c8ef72c4e1c201a3d9084c78aa8` — 0.5.1 |
| Libreria-Racafe | `61c7efda5f468c91f8185ac49adfa604901b5843` — racafeForms 0.7.0 |
| Compartido | Rama `main`, recursos enlazados por URL al arrancar |

## Contrato de datos

- `data` conserva las 67 columnas comprobadas por DataPrep. No se sustituyen ni se agregan hechos financieros.
- El RData conserva exactamente dos objetos: `crm_maestro` y `homologaciones`, con compresión xz.
  Se agregan dentro de `homologaciones` cuatro referencias ya calculadas por el ETL: `clientes_sistema`,
  `facturacion`, `margenes_pyg` y `lotes`. No se agregan consultas ni se modifica su cálculo.
- El cargador adapta el maestro a las vistas usadas por los módulos. `data` contiene los 67 campos originales
  y solo registros de ejecución; las proyecciones se conservan sin cambios en `crm_maestro`.
  El cierre se obtiene de `FechaCortePyG`. Las exportaciones planas anteriores siguen siendo compatibles.
- La conciliación conserva fuentes independientes: facturas reales y PyG provienen de sus referencias,
  no se reconstruyen ambas a partir de la misma base financiera.
- La primera compra por NIT utiliza `FecPrimerFact` de la facturación independiente, enlazada con los lotes;
  se limita al universo histórico que define el nuevo ETL.
- `APP/misc/data_contract.R` comparte el contrato entre el ETL y la app. La carga valida nombres, fechas,
  cierre PyG y claves dimensionales antes de publicar los objetos en el entorno de la app.
- Los cruces rechazan dimensiones duplicadas. Las órdenes de compra se consolidan por lote conservando sus textos.
- Las filas PyG sin marca/empaque pueden heredar un atributo de producto solo si es único para línea y código.
  Los atributos ambiguos permanecen sin dato. Los filtros incluyen las categorías y productos observados,
  para que seleccionar todas las opciones no descarte esos hechos por ausencia de catálogo.
- La conversión de leads toma `MinFecFact` de `fact_cliente`, sin inferir la primera compra de las fechas mensuales PyG.
- Las alianzas consultan los clientes actuales y sus nombres maestros; su serie usa `SacosFact`, no la columna
  inexistente `SacosPYG` de la base principal. Se conserva `SacosPYG` como nombre de salida de esa serie.

## Detalle operativo del encabezado

`crm_detalle_operaciones` se deriva del maestro, con una fila por combinación pedido/línea/lote y
facturación independiente acumulada por lote. Conserva pedidos proyectados sin lote sin incorporarlos
como ejecución financiera. Reutiliza `SacosLote70`, `SacosProducidos70`, `SacosDespachados70` y
`SacosPedido70Linea`: no aplica nuevamente UMeFac. La fecha de despacho es `FechaDespachoReal`,
sin confundirla con la instrucción de despacho. Este objeto se pasa explícitamente a `HeaderClientesServer`.
Debe verificarse con ejemplos reales de las unidades disponibles en staging.

## Componentes

| Área | Ajuste |
| --- | --- |
| acciones | Inputs, botones, descargas, notificaciones y actualizadores racafeForms; IDs conservados |
| admon | Revisado: ya utiliza formularios, validación, confirmación y componentes corporativos actuales |
| controlbar | Filtros incluyen dimensiones observadas; el PDF valida su plantilla durante la descarga |
| encabezado | Consume el detalle operativo exportado; conserva los componentes corporativos existentes |
| generalidades | Referencias a racafeCore/racafeShiny; descargas y selector compatibles con racafeForms |
| prospeccion | Selectores, radio, descargas y modales racafeForms; llamadas explícitas a racafeModulos |
| gestion_comercial | Revisado: ya consume SacosFact/Margen y completa las dimensiones opcionales; recibe el cierre exportado |
| global / UI | Validación de versiones y datos, estilos/scripts/logos enlazados a Compartido/main |

Las confirmaciones de escritura siguen usando `racafeShiny::MostrarModalConfirmacion` y sus observers existentes.
El campo Calidades conserva selectize porque admite valores nuevos; el picker de racafeForms no ofrece esa operación.
Los recordatorios usan el control integrado de fecha/hora y validan en el servidor la fecha futura y el horario 07:00–17:59.
Los ejemplos `Demo*()` conservan su contenido dentro de funciones: cargar los módulos no crea ni ejecuta sus apps.

## Verificación

Verificado en este entorno:

- Compatibilidad estática de argumentos de 843 llamadas a racafeForms, racafeModulos, racafeShiny y racafeCore
  frente a sus fuentes y exportaciones revisadas: sin incompatibilidades detectadas.
- Parseo real de los 60 scripts R con R 4.5.0.
- 820 comprobaciones de contrato y regresión con dplyr 1.1.4, datos ficticios y stubs.
- `git diff --check` y comprobación de sintaxis JavaScript con Node.
- Lectura real con Shiny del CSS y los cuatro scripts desde Compartido/main, y generación del HTML.
- Descarga del logo del PDF desde el mismo repositorio y verificación de su cabecera PNG.
  Se retiró la copia local de vendor y la llamada racafeSharedAssets, que solo acepta carpetas locales.
  Los recursos remotos se incluyen con includeCSS/includeScript, evitando depender del MIME de los scripts en el navegador.

Se habilitó un R aislado dentro del workspace, sin cambiar la instalación del sistema.
No se ejecutó el ETL contra servicios de negocio ni la aplicación Shiny completa: no se dispone del RData real
ni de las credenciales de las fuentes. Las pruebas no realizan consultas ni escrituras de negocio.

Comandos de verificación con R y dplyr disponibles:

```sh
Rscript -e 'invisible(lapply(list.files(".", "[.]R$", recursive = TRUE, full.names = TRUE), parse))'
Rscript tests/test_data_contract.R .
```

El segundo comando comprueba conservación de filas/importes/fechas, rechazo de claves duplicadas,
atributos de producto ambiguos, carga sin reemplazo parcial, detalle operativo, primera factura y ausencia
de demos ejecutadas en el nivel global. También verifica la carga de exactamente dos objetos, la separación de
proyecciones, los snapshots de productos, las fuentes independientes y las cantidades operativas sin doble conversión.
Usa datos ficticios y stubs; no llama a servicios de negocio.
Las pruebas existentes de UI/navegador siguen pendientes de ejecución con sus dependencias instaladas.

Antes de publicar: regenerar la exportación, ejecutar las pruebas R, iniciar Shiny y comprobar filtros,
resumen operativo, conversión de leads y formularios con datos reales. El repositorio no contiene
`APP/misc/modules/controlbar/cotizacion.Rmd`; la descarga PDF requiere esa plantilla del despliegue existente.

## Inventario de scripts originales

| Script | Resultado |
| --- | --- |
| `APP/global.R` | Modificado |
| `APP/misc/factores.R` | Revisado; conserva implementación |
| `APP/misc/functions.R` | Revisado; conserva implementación |
| `APP/misc/modules/acciones/00_Constantes.R` | Revisado; conserva implementación |
| `APP/misc/modules/acciones/00_Funciones.R` | Modificado |
| `APP/misc/modules/acciones/01_editar.R` | Modificado |
| `APP/misc/modules/acciones/02_relacionamiento.R` | Modificado |
| `APP/misc/modules/acciones/03_promover.R` | Modificado |
| `APP/misc/modules/acciones/04_descartar.R` | Modificado |
| `APP/misc/modules/acciones/05_reactivar.R` | Modificado |
| `APP/misc/modules/acciones/06_crear_oportunidad.R` | Modificado |
| `APP/misc/modules/acciones/07_descartar_oportunidad.R` | Modificado |
| `APP/misc/modules/admon/00_constantes.R` | Revisado; conserva implementación |
| `APP/misc/modules/admon/00_funciones.R` | Revisado; conserva implementación |
| `APP/misc/modules/admon/01_choices.R` | Revisado; conserva implementación |
| `APP/misc/modules/admon/02_administrar.R` | Revisado; conserva implementación |
| `APP/misc/modules/controlbar/01_filtros.R` | Modificado |
| `APP/misc/modules/controlbar/02_cotizador.R` | Modificado |
| `APP/misc/modules/controlbar/03_crear_nota.R` | Revisado; conserva implementación |
| `APP/misc/modules/controlbar/04_registrar_competencia.R` | Revisado; conserva implementación |
| `APP/misc/modules/encabezado/00_header.R` | Modificado |
| `APP/misc/modules/encabezado/01_tabla_indicadores.R` | Revisado; conserva implementación |
| `APP/misc/modules/encabezado/02_notificaciones_tareas.R` | Revisado; conserva implementación |
| `APP/misc/modules/encabezado/03_form_persona.R` | Modificado |
| `APP/misc/modules/encabezado/04_migrar_cartera.R` | Revisado; conserva implementación |
| `APP/misc/modules/generalidades/00_Constantes.R` | Revisado; conserva implementación |
| `APP/misc/modules/generalidades/00_Funciones.R` | Revisado; conserva implementación |
| `APP/misc/modules/generalidades/01_resumen_ejecutivo.R` | Revisado; conserva implementación |
| `APP/misc/modules/generalidades/02_validacion_financiera.R` | Modificado |
| `APP/misc/modules/generalidades/03_comparacion_indicadores.R` | Modificado |
| `APP/misc/modules/gestion_comercial/02_presupuesto.R` | Revisado; conserva implementación |
| `APP/misc/modules/prospeccion/00_Constantes.R` | Revisado; conserva implementación |
| `APP/misc/modules/prospeccion/00_Funciones.R` | Modificado |
| `APP/misc/modules/prospeccion/01_crear_contacto.R` | Revisado; conserva implementación |
| `APP/misc/modules/prospeccion/02_panel.R` | Modificado |
| `APP/misc/modules/prospeccion/03_kanban.R` | Modificado |
| `APP/misc/modules/prospeccion/04_detalle_contacto.R` | Modificado |
| `APP/misc/modules/prospeccion/05_detalle_prospecto.R` | Modificado |
| `APP/misc/modules/prospeccion/06_detalle_leads.R` | Modificado |
| `APP/misc/modules/prospeccion/07_detalle_cliente.R` | Modificado |
| `APP/misc/modules/prospeccion/08_detalle_descartados.R` | Modificado |
| `APP/misc/modules/prospeccion/09_embudo_conversion.R` | Modificado |
| `APP/misc/parametros.R` | Revisado; conserva implementación |
| `APP/misc/ui/body.R` | Modificado |
| `APP/misc/ui/controlbar.R` | Revisado; conserva implementación |
| `APP/misc/ui/footer.R` | Modificado |
| `APP/misc/ui/header.R` | Modificado |
| `APP/misc/ui/preloader.R` | Revisado; conserva implementación |
| `APP/misc/ui/sidebar.R` | Revisado; conserva implementación |
| `APP/misc/values.R` | Revisado; conserva implementación |
| `APP/server.R` | Modificado |
| `APP/ui.R` | Revisado; conserva implementación |
| `DataPrep.R` | Modificado |
| `tests/check_ui.R` | Revisado; conserva implementación |
| `tests/client-detail/app.R` | Revisado; conserva implementación |
| `tests/helpers.R` | Revisado; conserva implementación |
| `tests/kpi-actions/app.R` | Revisado; conserva implementación |
| `tests/ui/app.R` | Revisado; conserva implementación |
