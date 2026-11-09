# Productos y apertura de oportunidades

## Crear oportunidad

El botón se detenía en un `req()` cuando el selector inicial no tenía un contacto válido, sin respuesta visible.
Ahora abre un selector modal si hace falta elegirlo; Continuar valida permisos y abre el formulario existente con
su namespace. Con contacto válido abre directamente el formulario. Sin contactos habilitados muestra un aviso.
No se crea ninguna oportunidad al abrir: se conserva la confirmación y persistencia del formulario vigente.

## Organización

`APP/misc/modules/productos/compartido/00_funciones.R`: transformaciones y motor de asociaciones.
`01_ejecucion.R`: Productos → Ejecución. `02_market_basket.R`: Productos → Market Basket.
Se retira PD_Administracion de sidebar, body, servidor y Resumen Ejecutivo; Catálogos de Administración se conserva.
El resumen muestra productos con movimiento y el número real de reglas del último análisis; antes de analizar no
inventa un número de reglas. Cambiar filtros invalida ese contador y la salida anterior del análisis.

Se usan el maestro actual, controles racafeForms, cajas racafeModulos, tablas Vancouver y estilos Compartido.
El botón Analizar está registrado con el preloader corporativo global. No hay preloader adicional por módulo.
No se modifica el contrato de exportación ni es necesario cambiar la BD para estas pantallas.

## Ejecución

Se leen hechos facturados cerrados/abiertos en el periodo y filtros seleccionados. Se excluyen proyecciones y nunca
se suman cantidades operativas repetidas del lote. El selector permite producto o categoría, manteniendo la línea
para no mezclar productos homónimos de negocios diferentes.

Indicadores: sacos netos de 70 kg, margen FCC, productos con movimiento, clientes con compra positiva, margen/kg,
pedidos, margen contable cerrado y margen abierto provisional. El margen unitario se calcula con totales, no
promediando ratios. Devoluciones y ajustes negativos permanecen en los netos. Un margen desconocido no se convierte
en cero. Los NIT, pedidos y lotes se tratan como identificadores de texto, sin separadores ni notación científica.

Gráficos: ranking por medida/top N, Pareto de contribución positiva en sacos, evolución mensual de los cinco
principales y resto, y dispersión de volumen/rentabilidad con tamaño por clientes. El Pareto no oculta los negativos
de la tabla. El detalle muestra hasta 200 movimientos recientes por fila; XLSX conserva todo el periodo, resumen,
serie y calidad. Las cantidades sin producto homologado se conservan en ejecución y se señalan en calidad.
No se distribuye un presupuesto de cliente entre productos sin una asignación disponible.

## Definición de cesta

- Pedido: NIT de facturación + código de pedido, con compras observadas dentro del periodo. No es factura individual.
- Cliente-mes: NIT principal + mes, como mezcla de compras del cliente.
- Cliente-periodo: NIT principal en todo el intervalo, como portafolio observado.

No se reconstruyen facturas a partir de `FacturasSYSCAFE`: algunas referencias del maestro están agregadas por
lote/mes y no permiten atribuir cada producto a una factura individual sin otra fuente transaccional.
Los pedidos desconocidos no se agrupan como si fueran uno solo. El nivel de ítem es línea/categoría/producto o
línea/categoría. La cantidad neta de cada ítem/cesta debe ser positiva; una devolución completa elimina ese ítem.
No se interpreta la cartera de compras observadas como inventario actual del cliente.

Los filtros de asesor, segmento, línea y exclusiones conservan el alcance autorizado. Producto/categoría del
controlbar seleccionan cestas que contengan esos ítems, pero conservan sus acompañantes. Las métricas son
condicionales a ese universo focalizado, no necesariamente a todo el CRM. La cobertura informa movimientos y
cestas elegibles, faltantes de producto/pedido/NIT, ajustes y cestas de varios ítems.

## Motor exacto de asociaciones

Apriori con índices verticales de cestas enumera todos los conjuntos frecuentes hasta la longitud elegida y genera
todas sus particiones dirigidas no vacías A→B, incluidas reglas con varios ítems en ambos lados. No se queda solo en
pares. El conteo mínimo efectivo es `max(mínimo absoluto, techo(soporte × N))`.
El denominador N conserva cestas de un solo ítem y cestas cuyos productos no resultan frecuentes. Duplicar líneas de
un ítem no aumenta su presencia. Por ello el soporte no se infla al eliminar productos raros del índice.

Para N cestas, nA/nB apariciones y nAB conjuntas:

- Soporte: nAB/N; confianza: nAB/nA; lift: confianza/(nB/N).
- Leverage: pAB−pA×pB; convicción: (1−pB)/(1−confianza).
- Jaccard: nAB/(nA+nB−nAB); coseno: nAB/raíz(nA×nB).
- Zhang: (pAB−pA×pB)/max(pAB×(1−pA), pA×(pB−pAB)).

Convicción infinita e indeterminada se distinguen. XLSX escribe «Infinita» para no perder esa condición.
La vista principal presenta productos y métricas clave; Métricas detalladas agrega las demás. Al seleccionar una
regla se muestran sus métricas y una vista previa de hasta 200 movimientos de cestas que la cumplen.
Los mapas de lift y soporte/confianza son descriptivos; no prueban causalidad ni garantizan demanda.

### Exploración estadística y temporal

Fisher unilateral de asociación positiva usa la tabla 2×2 de presencia/ausencia de A y B. Se calcula una vez por
partición sin duplicar A→B/B→A; BH se aplica a esa familia antes del filtro de confianza/lift.
La familia ya está seleccionada por frecuencia, y las cestas de un mismo cliente pueden depender entre sí:
los p/q son exploratorios y no constituyen garantía de control de errores ni validación causal de una campaña.

Con validación temporal se usan aproximadamente 70% de las fechas completas para descubrir reglas y las fechas
posteriores para medir soporte, confianza y lift de esas mismas reglas. No se divide una fecha o cesta entre ambos
tramos. El alcance registra las cantidades reales de cestas de entrenamiento/validación y su corte.
Una ausencia de antecedentes en validación conserva confianza vacía, distinta de confianza cero con antecedentes.
Es estabilidad observada, no una evaluación prospectiva independiente por cliente.

### Venta cruzada y límites

Las hipótesis seleccionan clientes que compraron el antecedente en el periodo y no tienen observación de parte del
consecuente. No estiman importes, margen incremental ni probabilidad causal. Se ordenan por lift, confianza y número
de cestas conjuntas. La lista interactiva se limita a 200 candidatos distintos cliente/ítems faltantes y se identifica
como un listado priorizado, no exhaustivo de toda la cartera. Incluye las métricas de validación si se solicitaron.

Los conjuntos admiten longitud 2–5 (inicial 3). Para mantener la app operativa, más de 50.000 candidatos de conjuntos
o 20.000 reglas candidatas detienen el cálculo con una indicación para ajustar parámetros. No se entrega una
familia parcialmente explorada como completa. El usuario controla soporte, mínimo absoluto, confianza, lift,
longitud, unidad de cesta, nivel, cierre financiero y validación/estadística.

La descarga incluye reglas, conjuntos, frecuencias, candidatos de venta cruzada, cestas, membresía cesta-ítem,
mapa de ítems, calidad, parámetros, filtros y alcance del último cálculo. Los controles nuevos no cambian resultados
hasta Analizar; cualquier cambio del alcance global bloquea la salida anterior y exige recalcular.

## Verificación

```sh
Rscript tests/test_productos_market_basket.R
Rscript tests/test_crear_oportunidad_apertura.R
```

Apriori se comparó contra enumeración exhaustiva en 12 muestras reproducibles hasta longitud cuatro. Se probaron
métricas con resultados conocidos, singleton/duplicados, convicción, límites, devoluciones, foco de cesta completa,
fechas y validación, recomendaciones e identificadores. El flujo de apertura se prueba con y sin selección,
contacto no autorizado y sin contactos; el transporte UI y el formulario persistente se sustituyen por espías.
También se renderizaron servidores con componentes reales, categorías/productos, todas las vistas de basket,
primer análisis, contador, validación temporal y bloqueo al cambiar filtros. No hubo consultas ni escrituras reales.
