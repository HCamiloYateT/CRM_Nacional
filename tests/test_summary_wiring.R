# Fuentes disponibles sin visitar primero Presupuesto ni Validación ----
library(shiny)
.asignaciones_resumen <- list()
.llamadas_resumen <- c(Presupuesto = 0L, ValidacionFinanciera = 0L)
.recorrer_servidor <- function(x) {
  if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return()
  if (is.call(x)) {
    if (is.symbol(x[[1]]) && as.character(x[[1]]) %in% names(.llamadas_resumen)) {
      .llamadas_resumen[[as.character(x[[1]])]] <<- .llamadas_resumen[[as.character(x[[1]])]] + 1L
    }
    if (identical(x[[1]], as.name("<-")) && is.symbol(x[[2]]) &&
        as.character(x[[2]]) %in% c("presupuesto_mod", "validacion_mod", "indicadores_resumen")) {
      .asignaciones_resumen[[as.character(x[[2]])]] <<- x
    }
  }
  partes <- if (is.call(x)) as.list(x)[-1L] else as.list(x)
  for (i in seq_along(partes)) if (!identical(partes[[i]], quote(expr = ))) .recorrer_servidor(partes[[i]])
}
.recorrer_servidor(parse("APP/server.R"))
stopifnot(all(.llamadas_resumen == 1L), length(.asignaciones_resumen) == 3L)
servidor_cableado <- function(input, output, session) {
  data_c <- reactive(data.frame(Valor = 1))
  clientes_raw_cache <- filtros <- fact <- margenes_pyg <- nit_ppal <- clientes_snap <- n_cliente <- NULL
  Presupuesto <- function(id, ...) list(resumen_ejecutivo = reactive(
    list(sacos = input$sacos, margen = input$margen)))
  ValidacionFinanciera <- function(id, dat, ...) {
    dat() # El constructor real lee su snapshot: requiere isolate al iniciarlo fuera de un observer.
    list(cuadre_sacos = reactive(input$cuadre))
  }
  for (.asignacion in .asignaciones_resumen) eval(.asignacion, envir = environment())
}
testServer(servidor_cableado, {
  session$setInputs(sacos = 1.2, margen = 0.9, cuadre = 0.99)
  stopifnot(identical(indicadores_resumen(),
    list(gc_presupuesto_sacos = 1.2, gc_presupuesto_margen = 0.9, ge_validacion = 0.99)))
  session$setInputs(sacos = 1.4, cuadre = 0.98)
  stopifnot(indicadores_resumen()$gc_presupuesto_sacos == 1.4, indicadores_resumen()$ge_validacion == 0.98)
})
cat("OK: fuentes registradas una vez y cifras disponibles antes de abrir sus pestañas.\n")
