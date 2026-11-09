# Navegación, CRM analítico y potencial comercial

## Apertura al primer clic y Cartera diferida

El preloader del CRM disparaba un clic en `crm_recalcular`. Ese clic sintético llegaba a los manejadores
exteriores de Bootstrap/bs4Dash y cerraba dropdowns o el controlbar recién abiertos. Ahora se detiene en
el botón: el listener de captura de racafeShiny sigue activando el preloader y el ciclo busy/idle lo cierra.
El clic real del controlbar tampoco alcanza el cierre exterior de bs4Dash situado en window.
No se reemplazan los eventos de selección del header ni su comportamiento al abrir modales.

`CargarTabUnaVez` registraba el servidor antes de insertar y enlazar la UI. Los primeros updatePickerInput
podían perderse: por eso un arnés que construía Cartera desde el inicio pasaba, pero la navegación real
podía dejar los selectores vacíos. La UI emite ahora una confirmación tras insertarse y el servidor se
registra una sola vez al recibirla. No se necesita pulsar Actualizar para inicializar las listas.
El arnés nuevo utiliza la función exacta extraída de APP/server.R y comienza en otra pestaña.

La corrección no genera cortes inexistentes: la fuente de Tesorería debe seguir cargada mediante el ETL
o el histórico autorizado. No se consultó ni escribió en producción durante estas pruebas.

## Fidelidad y Riesgo

Se conserva el tablero de señales observadas y se añaden RFM, BG/NBD, CLV y diagnóstico:

- Unidad de análisis: cliente principal/línea. Una compra es un día con cantidad neta positiva;
  varias líneas de factura del mismo día no se interpretan como recompra.
- RFM: días desde la última compra, días con compra y margen neto observado. Las puntuaciones 1–5
  son relativas a la población visible, conservan empates y no modifican la segmentación guardada en BD.
  Las devoluciones no crean recurrencia y sus importes sí reducen el margen y volumen netos.
- BG/NBD: máxima verosimilitud en escala logarítmica con tres inicializaciones. Usa frecuencia repetida,
  tiempo entre primera/última compra y antigüedad de observación, en semanas. Requiere al menos 20 UC
  y cinco con recompra. Informa falta de historia, falta de convergencia o parámetros en un límite.
- La probabilidad de actividad es la posterior del modelo no contractual, no la certeza de abandono.
  La esperanza de compras se calcula integrando la propensión de abandono, sin requerir a > 1.
- CLV es margen futuro esperado, descontado a una tasa anual configurable y horizonte en meses calendario.
  Usa el margen neto histórico por compra como supuesto de ticket constante. No es ingreso facturado,
  valoración contable ni un modelo Gamma-Gamma. Un margen desconocido produce CLV desconocido.
- El riesgo es una banda orientativa sobre actividad: alto <40%, medio 40–70% y bajo >=70%.
  La segmentación comercial debe revisarse con el contexto del cliente.
- Validación retrospectiva de 90 días: ajusta exclusivamente con compras anteriores al corte de entrenamiento
  y compara predicción con días de compra observados posteriores, incluyendo ceros. Muestra MAE, RMSE,
  totales y población. No incluye clientes nuevos del holdout en la población ya calibrada.
- El ajuste usa el historial anterior al corte seleccionado y los filtros dimensionales autorizados,
  sin recortar la antigüedad al rango de fechas del controlbar. El selector Estado filtra las señales originales.
  Cambiar datos, filtros o parámetros invalida la salida entrenada y su descarga hasta volver a Calcular.

La implementación utiliza la verosimilitud BG/NBD en forma estable y una integración numérica de su
expectativa condicional. Las pruebas contrastan la predicción con la expresión hipergeométrica publicada,
con parámetros conocidos, además de una integral independiente y propiedades de monotonía.
Referencia: Fader, Hardie y Lee (2005), *Counting Your Customers the Easy Way: An Alternative to the Pareto/NBD Model*;
[notas numéricas BG/NBD de Bruce Hardie](https://www.brucehardie.com/notes/027/).

## Prospección y potencial

El Kanban conserva arrastre y acciones, con formulario, filtros adaptables y panel corporativo.
Las cajas y pestañas de Contactos, Prospectos, Leads, Clientes, Descartados, Conversión y componentes
compartidos usan paneles y navegación corporativa; se conservan sus IDs, servidores y lógica de selección.
Los paneles colapsables utilizan details/summary accesible. Los adaptadores funcionan también con
racafeForms 0.7.3; cuando están disponibles, utilizan los componentes nuevos de 0.7.4.

Potencial de Mercado deja de ser un título vacío. Consulta CRMNALPOTENCIAL y CRMNALCONTACTO al abrir
o actualizar, restringe el directorio por el usuario/administrador y cruza ventas por CodContacto.
Compara el consumo mensual actual declarado con la facturación del mes en kg, muestra penetración,
brecha, margen COP, indicadores y gráfico interactivo. Conserva entidades sin ventas o sin potencial
para identificar cobertura y datos faltantes. No une clientes por razón social.

El potencial admite kg/mes, ton/mes y sacos/mes. Como el formulario antiguo no guarda peso del saco,
se ofrece un input de conversión, inicialmente 70 kg, que debe corresponder a la declaración comercial.
Los sacos facturados del maestro ya son equivalentes de 70 kg. El potencial no está expresado en COP;
no se inventa una facturación monetaria a partir del precio de pedido ni se confunde margen con ingreso.
El mes vigente puede estar incompleto y la ejecución respeta los filtros aplicados del controlbar.

## Cohortes, Productos y Market Basket

Se retira Dinámica de Clientes de sidebar, body, resumen ejecutivo y registro de servidores, junto con
sus dos placeholders. Los análisis longitudinales permanecen en Cohortes.

Estados y Transiciones recibe la fecha final aplicada del controlbar: prepara el año de ese corte y limita
ambos selectores entre enero y ese mes. El servidor acota también fechas enviadas fuera del rango.
El selector mensual muestra mes/año y devuelve una fecha, sin depender de los meses presentes en la tabla.

Ejecución de Productos utiliza cuatro gráficos Plotly, con hover de producto/categoría, periodo,
volumen, margen, participación o clientes según el gráfico. Se corrige además la división vectorial de
participaciones: un denominador escalar ya no repite la primera proporción en todos los productos.
Market Basket incorpora una guía inicialmente colapsada sobre cestas, soporte, confianza, lift,
validación temporal y uso de las hipótesis de venta cruzada.

## racafeForms 0.7.4

El PR complementario en Libreria-Racafe agrega:

- `racafeInputMonth` / `racafeUpdateMonth`: vista de año/mes, fechas normalizadas al primer día y límites mensuales.
- `racafeTabs`: pestañas Shiny con navegación corporativa, conservando valores e IDs.
- `racafeDisclosure`: sección colapsada por defecto con summary nativo, sin listeners globales.

El CRM conserva adaptadores para la versión anterior, por lo que actualizar la librería no es un
requisito para recuperar la apertura del controlbar o los datos de Cartera.

## Verificación reproducible

```sh
Rscript tests/test_crm_analitico.R
Rscript tests/test_productos_market_basket.R
Rscript tests/test_cohortes_estados.R
Rscript tests/test_estructura_modular.R
Rscript -e "shiny::runApp('tests/crm-analitico', host='127.0.0.1', port=3880)"
# Con Playwright/Chromium en otra terminal:
node tests/test-crm-analitico.cjs
```

La prueba de navegador incluye bs4Dash real, los componentes de header, el preloader y el controlbar:
verifica apertura al primer clic, inicialización de Cartera al navegar, los cuatro gráficos Plotly,
ajuste RFM/BG-NBD/CLV, potencial y guía de Basket. Todos los datos del arnés son ficticios.
