# Cuentas de facturación y presentación comercial

La búsqueda individual usa `CLCliNit`: cada NIT de facturación tiene una sola entrada, combinada con su ficha CRM
cuando existe un vínculo explícito. Las cuentas hijas conservan su propio nombre desde el directorio del sistema.
Seleccionar una hija filtra exclusivamente su NIT, aunque comparta código CRM con otras cuentas.
La opción de consolidación aparece únicamente cuando hay varios NIT hijos dentro del alcance seleccionado.

La identidad se presenta con badges de tipo, vista, NIT y asesores. Se retiraron las notas de implementación
indicadas y el bloque de cobertura de Evolución del Negocio. Pedidos y años son identificadores: no llevan
separadores ni decimales. Las etiquetas compartidas explican las unidades de sacos, margen y presupuesto.

Todos los tabsets de la app usan `bs4Dash::tabsetPanel`; `tabPanel` permanece sin prefijo. El helper filtra paneles
nulos para no crear pestañas vacías. Los selectores de racafeForms se centralizan para impedir opciones artificiales
«Ninguno», también cuando sus listas cambian. La selección vacía continúa siendo válida en filtros múltiples.

Kanban usa asesores, líneas y segmentos del controlbar como límite de su alcance, con opciones locales inicialmente
seleccionadas por completo. Los contactos todavía sin clasificación permanecen consultables; deseleccionar todas
las opciones produce un tablero vacío. Descartados permite varias etapas a la vez. El embudo se distribuye en
columnas 7/5, con indicadores apilados a la derecha.

La guía de Market Basket usa `racafeForms::racafeDisclosure` y una lista de instrucciones legible, sin separar los
números de sus textos. Los menús de acciones se llevan al cuerpo del documento para salir de las capas de las
tablas; sus IDs incluyen el namespace y los tooltips quedan por encima del menú y del texto.

## Productos sin clasificar

La nueva opción de Productos lista referencias del catálogo `CRMNALPRODS` sin categoría o producto, con marca,
nombre comercial y presentación. El usuario selecciona una referencia y una combinación válida para su línea,
según el catálogo de productos y las opciones jerárquicas de Administración.

Guardar muestra la referencia y la clasificación para confirmar. La escritura usa la conexión corporativa,
una transacción y bloqueo del registro; solo actualiza clasificación y campos de modificación ya existentes.
Verifica nuevamente los valores originales dentro del bloqueo. La clave incluye la fecha de snapshot cuando
existe: no borra historia ni modifica cantidades, facturas o márgenes.

Después de guardar, la sesión refresca su caché de productos y sus tableros. Actualizar desde el controlbar también
relee el catálogo para incorporar cambios de otras sesiones. Basket exige recalcular si cambió la clasificación.
Las opciones nuevas de categoría/producto se mantienen en Administración, no se escriben como texto libre.

## Validación

- `tests/test_ajustes_comerciales.R`: identificadores, alcance Kanban, clasificación, historia y conflictos de edición.
- `tests/test_consulta_individual.R`: cuentas hijas, nombres, consolidación, vínculos, selección vacía y permisos.
- `tests/test-ajustes-comerciales.cjs`: badges, tabs Bootstrap 4, selectores, guía, cancelar/confirmar guardado y tooltips.
- Regresión de consulta, navegación/header/controlbar, gráficos, RFM/CLV, Apriori, evolución y filtros múltiples.

Los recorridos Shiny usan Chromium y fuentes ficticias con las dependencias UI disponibles. El guardado se simula
mediante el adaptador de persistencia del módulo: no se realizaron escrituras ni conexiones a producción.
