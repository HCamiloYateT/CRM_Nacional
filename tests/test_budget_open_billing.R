# La fuente de facturación no debe recortarse por el cierre financiero ----
library(shiny)
library(dplyr)
.buscar_fuente <- function(x) {
  if (is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("datos_centrales_r"))) {
    return(x[[3]][[2]])
  }
  if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return(NULL)
  partes <- if (is.call(x)) as.list(x)[-1L] else as.list(x)
  for (i in seq_along(partes)) {
    if (identical(partes[[i]], quote(expr = ))) next
    encontrado <- .buscar_fuente(partes[[i]])
    if (!is.null(encontrado)) return(encontrado)
  }
  NULL
}
.cuerpo_fuente <- .buscar_fuente(parse("APP/misc/modules/gestion_comercial/presupuesto/01_modulo.R"))
stopifnot(!is.null(.cuerpo_fuente))
servidor_facturacion <- function(input, output, session) {
  dat <- data.frame(FecMes = as.Date(c("2026-09-01", "2026-10-01", "2026-11-01", "2026-12-01", NA)),
    FecFact = as.Date(c("2026-09-15", "2026-10-15", "2026-11-15", "2026-12-15", NA)),
    CliNitPpal = 1, LinNegCod = 10000, SacosFact = c(10, 20, 30, 40, 999), Margen = 1)
  .resolver_datos <- function(x) x
  filtros_r <- function() NULL
  calendario_r <- function() stop("El cierre no debe recortar la fuente de facturación.")
  clientes_presupuesto_r <- function() data.frame(CliNitPpal = 1, LinNegCod = 10000, CLCliNit = 1,
    SSPpto = 120, MNFCCPpto = 1200, Depto = "BOGOTÁ", Mpio = "BOGOTÁ", PerRazSoc = "Cliente")
  datos_centrales <- reactive(eval(.cuerpo_fuente, envir = environment()))
}
testServer(servidor_facturacion, {
  resultado <- datos_centrales()
  stopifnot(nrow(resultado) == 4L, sum(resultado$SacosFact) == 100,
    identical(resultado$FecMes, as.Date(c("2026-09-01", "2026-10-01", "2026-11-01", "2026-12-01"))))
})
cat("OK: se conservan facturas de octubre, noviembre y meses futuros, sin depender del cierre.\n")
