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
