# Datos reales del Resumen Ejecutivo

Tres tarjetas reciben los valores de sus módulos:

- Cumplimiento total de sacos: suma de sacos facturados / presupuesto anual de sacos.
- Cumplimiento total de margen: suma del margen facturado / presupuesto anual de margen.
- Validación financiera: el mismo porcentaje de cuadre de sacos de su módulo.

Los cumplimientos usan la misma base mensual de la fila TOTAL del presupuesto,
con los filtros activos. Son proporciones (1 = 100%); no se limita el resultado
al 100%. Si el presupuesto es cero o el resultado no es finito se muestra «—».
Estas tres tarjetas no recurren a porcentajes de ejemplo.

Presupuesto y Validación se registran una sola vez al iniciar el servidor.
Sus interfaces continúan cargándose al abrir la pestaña. La lectura inicial del
snapshot financiero se aísla del contexto reactivo; las cifras del resumen se
consumen mediante reactivos de los módulos. Las dos tarjetas de presupuesto
navegan a Presupuesto y la de cuadre navega a Validación Financiera.

Indicadores de Mercado conserva las consultas y el cálculo de comparación.
El botón se alinea con los campos de fecha mediante flex y márgenes de formulario
controlados localmente. La tabla usa racafeTableVancouver, conserva importes,
porcentajes y colores de variación, y muestra las notas fuera de la tabla.

## Validación

Desde la raíz con las dependencias de la app instaladas:

```sh
Rscript tests/test_executive_live_values.R
Rscript tests/test_summary_wiring.R
Rscript tests/test_market_vancouver.R
```

Pruebas locales: totales anuales, ratios superiores a 100%, actualización reactiva,
presupuesto cero, registro único de fuentes y disponibilidad antes de visitar sus
pestañas. Se revisaron 30 llamadas públicas y el parseo de los archivos R.

Chromium comprobó alineación de los controles, tabla Vancouver sin card y ausencia
de desbordamiento móvil. El arnés usó fuentes de las librerías, gt 0.7.0 adaptando
dos opciones de padding, formateador de caja y registro de assets simulados, y
fuentes de mercado/controladores de módulos simulados. No se ejecutó la app
completa con servicios y datos de negocio.
