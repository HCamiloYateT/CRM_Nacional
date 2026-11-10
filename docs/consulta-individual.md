# Consulta individual

La selección incluye entidades registradas y cada NIT de facturación con catálogo o ejecución visible.
La ficha no requiere facturación. Las coincidencias se resuelven por código de entidad, NIT de facturación
vinculado o NIT principal; nunca por razón social. Una cuenta hija se consulta por su NIT propio.

- **Entidad:** directorio, personas, teléfonos, redes, ubicaciones, sucursales, gestiones, recordatorios y recorrido.
- **Prospecto, lead o cliente:** también potencial registrado y oportunidades.
- **Cliente o entidad con movimientos:** ejecución, productos, metas mensuales, flujo de pendientes y cartera.
- **Cuenta principal:** ofrece una vista consolidada si hay varias cuentas hijas y permite consultar RFM y estimar
  compras futuras y CLV usando la población visible, sin ajustar el modelo a un cliente aislado.

Las tareas se muestran únicamente cuando tienen vínculo explícito `CodContacto`, además de respetar su privacidad
por creador/responsable. Si el contrato de tareas no contiene ese vínculo, se indica consultar Notas y Tareas.
No se intenta deducir una entidad por texto libre.

## Filtros y fechas

El controlbar determina el periodo de ejecución y las dimensiones comerciales. Vaciar los asesores elimina
la selección y su detalle. Las fichas sin clasificación comercial conservan sus datos operativos propios;
los filtros comerciales restringen la ejecución, no inventan una clasificación para contactos nuevos.
Los permisos de contactos por asesor/creador y los de tareas se aplican también en la consulta.

Los pendientes conservan los pedidos futuros y sin factura: la fecha final del controlbar es referencia de
vencimiento, no un filtro de fecha de factura. Cartera toma el último corte disponible hasta esa fecha, con
su origen explícito; no se atribuye por categoría/producto. RFM y CLV usan la historia hasta el corte, con las
dimensiones seleccionadas, para no confundir un filtro de periodo corto con abandono del cliente.

Las metas son referencias mensuales del catálogo al corte. No se asignan a productos. El margen abierto es
provisional; importes desconocidos y snapshots contradictorios conservan valores vacíos y marcas de revisión.
Los NIT se presentan y exportan como texto, sin separadores ni decimales.

## Fuentes y actualización

El maestro ETL aporta ejecución y pendientes; `CRMNALCLIENTE`, metas y clasificación. El catálogo de entidades
proviene de `CRMNALCONTACTO`; se complementa con historia de etapas y vínculos explícitos de NIT. El directorio,
potencial, oportunidades y seguimiento consultan sus tablas CRM mediante el cargador existente de la app.
Los cambios de registro se consultan con **Actualizar**. La consulta no escribe en esas tablas.

Una fuente opcional caída presenta un aviso de indisponibilidad en su propia sección y deja funcionar las otras.
Una tabla disponible pero vacía muestra el estado sin registros. No se ejecuta el script antiguo adjunto,
que incluía escrituras y una app de demostración independientes.

## Verificación

- `Rscript tests/test_consulta_individual.R`: identidad, privacidad, sucursales, NIT, alcance, fechas, fuentes
  opcionales, RFM vacío y cuentas sin presupuesto.
- `tests/consulta-individual/app.R` y `node tests/test-consulta-individual.cjs`: recorrido Shiny con datos ficticios,
  incluyendo selección por tipo, pendientes, cartera, error opcional, filtros vacíos y valores iniciales de Basket.
- Regresión: `tests/test_productos_market_basket.R`, `tests/test_crm_analitico.R`,
  `tests/test_estructura_modular.R` y `tests/test-crm-analitico.cjs`.

El arnés requiere las dependencias de la app; no consulta bases de datos de producción.

## Perfil e historia comercial completa

La ficha indica presupuesto del año consultado, parentesco y número de cuentas hijas visibles para el usuario.
Contacto, teléfono, dirección y ciudad pertenecen al NIT seleccionado. La antigüedad distingue la fecha de
creación del cliente en el directorio, el registro del contacto CRM y la primera compra observada.

**Histórico completo** reúne los años disponibles hasta la fecha final del controlbar. Las tarjetas del periodo
conservan sus fechas inicial y final. Las preferencias incluyen categoría, producto, marca y nombre comercial.
Las cuentas hijas mantienen sus propias ventas y modelos; la vista consolidada agrupa la cuenta principal.
Si una hija solo tiene seguimiento CRM a través del padre, se informa ese origen sin atribuirle ventas del padre.

RFM describe las compras disponibles. CLV y BG/NBD se calculan automáticamente al abrir fidelidad, con horizonte
de doce meses y descuento anual del 12 %. Si falta población o repetición suficiente se indica la limitación.
Son estimaciones, no promesas de compra. La comparación respeta el alcance comercial autorizado.

El recorrido reúne contactos, conversiones, relaciones, alianzas, descartes y oportunidades con sus fechas
registradas. Los registros antiguos de conversión se deduplican frente a la historia de etapas. Una preferencia
u oportunidad sobrescrita conserva únicamente su última versión disponible: no se reconstruye una auditoría
inexistente. Las fuentes no disponibles se señalan en la interfaz.

Venta cruzada propone productos aún no observados en la cuenta, comprados junto a sus productos por al menos
dos cuentas comparables y con afinidad superior a la habitual. Identifica oportunidades ya registradas para
consultar su seguimiento. No infiere vínculos por nombre ni amplía los permisos del usuario.

Verificación adicional: `tests/test_consulta360.R` y `node tests/test-consulta360.cjs`, con datos ficticios,
modelos reales, años anteriores, presupuesto, padre/hija, geografía, conversiones y propuestas de venta cruzada.
La interfaz se comprueba en escritorio y a 390 px; no se accedió a datos de producción.

## Secciones colapsables y precios de pedido

Directorio, seguimiento y potencial usan secciones corporativas colapsables con un badge de cantidad de registros.
Un fallo de fuente indica «No disponible» y no se presenta como cero registros. Market Basket usa el mismo adaptador
compatible si la versión instalada de racafeForms todavía no exporta `racafeDisclosure`.

Ejecución muestra marcas y nombres comerciales. El detalle de pedidos utiliza el histórico completo de la cuenta
hasta el corte y respeta los filtros comerciales. Muestra sacos facturados, margen y el precio por kilo de cabecera;
no calcula precio de venta a partir del margen. Los precios mínimo y máximo excluyen valores ausentes o no positivos.

Un precio ingresado encuentra todas las referencias con la menor diferencia absoluta. Para consultar indicadores
se utiliza el pedido más reciente entre los empatados y su fecha real de creación. No se sustituye una cotización
faltante por la de otro día. Los indicadores conservan sus unidades y la cuenta hija mantiene su propio NIT.
DataPrep enriquece el histórico financiero con cabeceras inequívocas de pedidos, sin multiplicar filas ni cantidades.
Para completar precios ausentes en exportaciones anteriores hay que ejecutar el ETL actualizado.

Los labels de línea usan «Coproductos»; los códigos y valores internos originales se conservan para filtros y joins.
La validación de segmentos usa el último corte de archivo por línea cuando está disponible. En exportaciones antiguas
sin ese dato usa la última observación de cada cliente del año más reciente. Nunca rellena un vacío reciente con una
clasificación anterior. El detalle muestra la fecha utilizada. El siguiente ETL conserva el corte de la carpeta PyG.

Verificación: `tests/test_precios_segmentos.R` comprueba precios, indicadores del día exacto, snapshots recientes,
labels y enriquecimiento ETL sin duplicación. `tests/test-perfil-precios.cjs` recorre los paneles, conteos, referencias,
comparación y Market Basket con fuentes ficticias y una versión anterior instalada de racafeForms.

## Rediseño de Ficha

El punto de entrada vigente es `ConsultaIndividual` en `IN_Consulta`. La nueva presentación está aislada en
`04_ficha_presentacion.R`; el servidor conserva argumentos, retorno, fuentes, filtros y cálculos. Se conservan
los IDs de salida `ficha-tabla`, `antiguedad-tabla`, `contacto_sistema-tabla`, `geografia-tabla`, `familia-tabla`
y la tabla de presupuesto. Las pruebas que esperaban una tabla GT para los bloques informativos ahora comprueban
sus componentes corporativos; las tablas operativas de las demás pestañas se conservan.

La ficha presenta razón social, NIT de facturación, asesor, estado CRM, tipo, relación y disponibilidad de presupuesto
sin duplicar el encabezado. «Activo en CRM» requiere un estado ACTIVO explícito del registro propio (o de los
registros visibles de la vista consolidada); una hija con seguimiento heredado no obtiene ese estado por su padre.

Dos filas adaptables muestran antigüedad/metas y contacto/registro CRM. Con una meta se destacan sus dos cifras;
con varias se conserva el orden y el detalle por registro, sin sumar ni escoger una cifra para el encabezado.
Los valores desconocidos se muestran como «Sin dato», y un cero real se mantiene. El formato de fechas y cifras
se aplica al HTML; el timestamp de creación del contacto conserva su precisión en la fuente.

Las cuentas relacionadas inician colapsadas y muestran primero la cuenta padre cuando es visible. El contador usa
las hijas del catálogo autorizado. Los seis registros complementarios se agrupan en un desplegable inicialmente
cerrado: si todos están vacíos se muestra un solo aviso. Una fuente caída se distingue de una fuente vacía.
Las seis cajas visibles usan `racafeShiny::racafeCollapsible` y se montan una sola vez. Sus contenidos y badges
se actualizan sin sustituir los contenedores. El CSS de bordes, cuadrícula, separador y colores suaves afecta
únicamente a la ficha y a su encabezado.

`tests/fixtures/ficha.R` reproduce la referencia CREPES con información ficticia: 960 sacos y $235.200.000 COP al
mes, edades de 23 años y 3 meses y de 5 años y 7 meses al corte de prueba, cuatro hijas visibles y una quinta fuera
del alcance. Esas cifras no están fijadas en producción. `tests/test-ficha.cjs` verifica escritorio de 1440 px,
móvil de 390 px, teclado, cambio de cliente/línea, visibilidad y casos sin presupuesto y con varias metas.
`tests/test_ficha_presentacion.R` comprueba datos parciales, formato, timestamp, estados, vacíos y metas por fila.

`tests/test-client-detail.cjs` corresponde al modal del header y a los estilos de Vancouver. No valida la pestaña
Ficha; sus aserciones se conservan. Los recorridos de Consulta Individual, consulta360 y precios siguen cubriendo
las tablas, permisos y cálculos de las otras pestañas.

Resultado de verificación del rediseño: pruebas R de presentación, consulta y estructura aprobadas; recorridos
`test-ficha.cjs`, `test-consulta360.cjs`, `test-consulta-individual.cjs` y `test-perfil-precios.cjs` aprobados con fuentes
ficticias. Se revisaron capturas de escritorio y móvil. No se accedió a los datos reales de CREPES ni a producción.
`test-client-detail.cjs` se ejecutó sin cambiar sus aserciones: falla en «Línea roja continua de 1px bajo todos los
encabezados». El mismo fallo se reprodujo contra la base `c0d4bde` de main, con `gt 0.7.0`; esta comprobación de
Vancouver falla antes del recorrido del modal. No se atribuye a Ficha ni se presenta como una prueba aprobada.


## Ajuste visual de las cajas corporativas

Las seis cajas forman una cuadrícula de tres filas y dos columnas. Las cajas usan el contenedor nativo sin borde rojo exterior. Las abiertas igualan su altura por fila; la línea vertical queda en el espacio entre columnas y desaparece
en móvil. Las cuatro primeras cajas abren inicialmente; Cuentas relacionadas y Registros complementarios quedan cerradas. Los valores principales
usan cajaValor_ui de racafeModulos como tarjetas informativas sin botones; las metas muestran explícitamente su periodicidad mensual.

NIT y asesor se muestran en una línea secundaria. Los badges usan verde suave para activo, azul suave para cliente
y presupuesto, rosado suave para padre, gris para hija y atributos, y ámbar para revisión. El nombre de la cuenta
padre de una hija se conserva en el título accesible de su pill de relación. Las cuentas relacionadas son filas
compactas con separadores, razón social/NIT a la izquierda y relación a la derecha. Si el asesor es común se muestra
una sola vez al pie; si difiere se mantiene por registro. Los contadores usan las mismas fuentes y permisos.

El contenedor de pestañas permanece montado. Se conserva la lógica previa de disponibilidad de navegación y
se vuelve a Ficha cuando la pestaña seleccionada deja de aplicar. Esto evita cerrar los desplegables al cambiar
de cliente o filtros. Las tablas y sus servidores conservan sus IDs, búsquedas y acciones.

`racafeShiny` 1.4.4 añade `header_badge` al final de la firma de racafeCollapsible. El badge es hermano del título
en summary; el título sigue escapado y el binding no cambia. Para instalaciones anteriores, el adaptador local
inserta el mismo espacio de badge en el summary inicial mediante tagQuery, sin modificar el binding ni reinstanciar
las cajas. Los contadores cambian en outputs hijos del encabezado.

Verificación del ajuste: pruebas R de presentación, identidad, permisos y estructura; cuatro recorridos Chromium
vigentes de consulta, histórico/RFM, precios y Ficha. El recorrido de Ficha se ejecuta con la API nueva y con la
anterior, verificando seis IDs únicos, nodos DOM persistentes, estado abierto/cerrado, binding Shiny y teclado,
colores, bordes, alturas por fila, separador y vista móvil. La referencia CREPES usa datos ficticios: cuatro hijas
visibles y cero complementos; otro contacto permite verificar búsquedas y detalle con datos. No se accedió a staging.


## Grid y clasificación comercial mensual

Las seis secciones siguen usando racafeShiny::racafeCollapsible, sin borde exterior rojo, en tres filas de dos
columnas. Cuentas relacionadas y Registros complementarios comparten la última fila y comienzan colapsadas;
conservan su estado al cambiar de cliente. En móvil se apilan. Las cuatro cifras de antigüedad/presupuesto utilizan
`racafeModulos::cajaValor_ui`, exportada por la versión instalada, en modo informativo (`mostrar_kpi = FALSE`),
con valor visual en detalle y sin botón. Esto permite mostrar edades en años/meses sin forzarlas a un formato numérico.
Se conserva el acento neutro de borde y las notas externas. Cero/una/varias metas mantienen su tratamiento previo.

El encabezado diferencia estado del registro CRM de estado comercial. El nuevo argumento opcional `estados`
recibe el mismo `data_cohortes` que alimenta el panel aprobado de Cohortes. Para cada unidad comercial visible
(cuenta principal y línea) se lee `EstadoPanel` del mes del corte del controlbar, con la última observación disponible
hasta ese corte. Se muestran Activo, Inactivo o Cliente nuevo; para hijas se identifica en el hover la cuenta principal.
NUEVO conserva la regla vigente del panel, incluida su propagación anual: el badge lo ubica en el mes consultado,
sin redefinirlo como primera compra. No se deduce actividad de ventas ni se arrastran estados de otros meses.

Si falta clasificación se muestra Sin clasificación/Sin dato; si hay estados contradictorios en la misma fecha,
Requiere revisión. Si el proveedor falla, Clasificación comercial no disponible y el resto de la ficha sigue funcionando.
Se conservan únicamente las unidades visibles del historial/metas de la entidad y de su cuenta principal actual.
El argumento es opcional para conservar los consumidores existentes del módulo. No cambia el cálculo de Cohortes.

Validación adicional: `tests/test_estado_comercial_individual.R` prueba los tres estados, unidad de la hija,
mes ausente, contradicciones, corte futuro y otra línea; el recorrido `test-ficha.cjs` comprueba la grid 3 × 2,
cuatro cajas de racafeModulos sin acciones, dos secciones cerradas y badges mensuales. Validación local con fixtures;
no se accedió a staging ni se ejecutó ETL.
