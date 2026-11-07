# Tabla de mercado con Vancouver y sin contenedor bs4Card ----
library(shiny)
library(dplyr)
sys.source("APP/misc/modules/generalidades/03_comparacion_indicadores.R", envir = environment())
.CargarHistoricoIndicadores <- function(fecha) data.frame(Fecha = as.Date(fecha))
.ConstruirComparacionIndicadores <- function(datos_anteriores, datos_actuales) data.frame(
  Nombre_Indicador = c("TRM", "Café"), Valor_Anterior = c(4000, 100), Valor_Actual = c(4200, 95),
  Diferencia_Absoluta = c(200, -5), Variacion_Porcentual = c(0.05, -0.05))
testServer(ComparacionIndicadores, args = list(id = "mercado"), {
  session$setInputs(fecha_anterior = as.Date("2026-01-01"), fecha_actual = as.Date("2026-01-02"), actualizar = 1)
  html <- output$tabla_comparacion$html
  stopifnot(grepl("gt_table", html, fixed = TRUE), grepl("TRM", html, fixed = TRUE),
    grepl("$4,200.00", html, fixed = TRUE), grepl("5.00%", html, fixed = TRUE),
    grepl("#495057", html, fixed = TRUE), !grepl('class="card', html, fixed = TRUE))
})
.html_mercado <- htmltools::renderTags(ComparacionIndicadoresUI("mercado"))$html
stopifnot(grepl('id="mercado-actualizar"', .html_mercado, fixed = TRUE),
  !grepl("margin-top:35px", .html_mercado, fixed = TRUE), !grepl('class="card', .html_mercado, fixed = TRUE))
cat("OK: mercado con Vancouver, importes, porcentajes y fechas sin caja exterior.\n")
