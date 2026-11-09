# Regresión: las diferencias cero no deben ocultar el cuadre mensual ----
library(shiny)
sys.source("APP/misc/compartido/00_presentacion.R", envir = environment())
.extraer_cuadre <- function(x, destino) {
  nombres <- c(".col_sacos_cuadre", ".col_diferencia_cuadre", ".col_pct_cuadre",
    ".registrar_tabla_cuadre", ".VF_CUADRE_MINIMO", ".VF_TOLERANCIA_SACOS")
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]]) &&
      as.character(x[[2]]) %in% nombres) eval(x, destino)
  if (is.call(x) || is.expression(x) || is.pairlist(x)) {
    partes <- if (is.call(x)) as.list(x)[-1L] else as.list(x)
    for (i in seq_along(partes)) {
      if (!identical(partes[[i]], quote(expr = ))) .extraer_cuadre(partes[[i]], destino)
    }
  }
}
.extraer_cuadre(parse("APP/misc/modules/generalidades/validacion_financiera/01_modulo.R"), environment())
servidor_cuadre <- function(input, output, session) {
  datos <- reactive(data.frame(FecMes = seq(as.Date("2026-01-01"), by = "month", length.out = 12),
    Mes = month.name, SacosFacturados = c(rep(100, 10), 0, 0), SacosPYG = c(rep(100, 10), 0, 0),
    Diferencia = rep(0, 12), PctCuadre = c(rep(1, 10), NA_real_, NA_real_)))
  tabla <- .registrar_tabla_cuadre("cuadre", datos, "Convencionales")
}
testServer(servidor_cuadre, {
  session$flushReact()
  html <- output$`cuadre-tabla`$html
  stopifnot(grepl("January", html, fixed = TRUE), grepl("December", html, fixed = TRUE),
    grepl("100.00%", html, fixed = TRUE), grepl("Convencionales", html, fixed = TRUE))
  session$setInputs(`cuadre-seleccionar` = 10)
  stopifnot(identical(tabla$seleccion()$id, as.Date("2026-10-01")))
})
cat("OK: doce meses, diferencias cero, porcentajes NA y selección del mes.\n")
