# Datos de la línea de meta: solo mes vigente y pendientes ----
library(dplyr)
library(tidyr)
.env_grafico <- new.env(parent = environment())
.extraer_grafico <- function(x) {
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]]) &&
      as.character(x[[2]]) %in% c("construir_grafico_serie", ".meta_mensual_requerida")) eval(x, .env_grafico)
  if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return()
  partes <- if (is.call(x)) as.list(x)[-1L] else as.list(x)
  for (i in seq_along(partes)) if (!identical(partes[[i]], quote(expr = ))) .extraer_grafico(partes[[i]])
}
.extraer_grafico(parse("APP/misc/modules/gestion_comercial/02_presupuesto.R"))
.env_grafico$.MESES <- month.name
.env_grafico$calendario_r <- function() list(mes_actual = 9L, mes_vigente = 11L, meses_rest = 2L)
# Capturar los datos entregados a Plotly sin requerir navegador ni conexión de negocio.
.env_grafico$plot_ly <- function() list(trazas = list())
.env_grafico$add_trace <- function(p, data, name, ...) { p$trazas[[name]] <- data; p }
.env_grafico$layout <- function(p, ...) p
.env_grafico$config <- function(p, ...) p
.datos_grafico <- data.frame(Mes_Num = 1:12, Fecha = month.name,
  Real = c(rep(10, 10), 20, 999), Ppto = rep(20, 12), Referencia = rep(1, 12))
for (.acumulada in c(TRUE, FALSE)) {
  .grafico <- .env_grafico$construir_grafico_serie(.datos_grafico, .acumulada,
    "Real", "Ppto", "Referencia", "Acumulado", "Mensual", "Sacos", "Sacos")
  .roja <- .grafico$trazas[[3]]
  .visibles <- .roja[!is.na(.roja$Valor), ]
  stopifnot(identical(as.character(.visibles$Fecha), c("November", "December")),
    identical(.visibles$Valor, if (.acumulada) c(170, 240) else c(70, 70)))
}
cat("OK: meta roja solo en noviembre y diciembre, sin usar facturas futuras como base.\n")
