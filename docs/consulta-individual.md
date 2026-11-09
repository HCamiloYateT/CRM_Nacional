# Consulta individual

La selección incluye entidades comerciales registradas y cuentas principales con catálogo o ejecución visible.
La ficha no requiere facturación. Las coincidencias se resuelven por código de entidad, NIT de facturación
vinculado o NIT principal; nunca por razón social.

- **Entidad:** directorio, personas, teléfonos, redes, ubicaciones, sucursales, gestiones, recordatorios y recorrido.
- **Prospecto, lead o cliente:** también potencial registrado y oportunidades.
- **Cliente o entidad con movimientos:** ejecución, productos, metas mensuales, flujo de pendientes y cartera.
- **Cuenta principal:** consolida sucursales dentro del alcance autorizado y permite consultar RFM y estimar
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
