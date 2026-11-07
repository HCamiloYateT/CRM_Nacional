# Tablas Vancouver: filtros, orden, claves y reapertura de detalles ----
library(shiny)
sys.source("APP/misc/00_presentacion.R", envir = environment())
.datos_prueba <- data.frame(.clave = c("A", "B", "TOTAL"), Dimension = c("Alfa", "Beta", "TOTAL"),
  Monto = c(20, 10, 30), Porcentaje = c(0.2, 0.1, 0.3), check.names = FALSE)
.specs_prueba <- list(.clave = list(show = FALSE),
  Monto = list(name = "Importe", format = list(cell = list(prefix = "$", digits = 0, separators = TRUE))),
  Porcentaje = list(format = list(cell = list(percent = TRUE, digits = 1))))
.html_prueba <- htmltools::renderTags(.crm_tabla_datos(.datos_prueba, col_specs = .specs_prueba))$html
stopifnot(grepl("gt_table", .html_prueba, fixed = TRUE), !grepl(".clave", .html_prueba, fixed = TRUE),
  grepl("$20", .html_prueba, fixed = TRUE), grepl("20.0%", .html_prueba, fixed = TRUE),
  grepl("#495057", .html_prueba, fixed = TRUE), grepl("1.5px", .html_prueba, fixed = TRUE))
.eventos_tabla <- list()
servidor_tabla <- function(input, output, session) {
  session$sendModal <- function(type, message) .eventos_tabla[[length(.eventos_tabla) + 1L]] <<- message$html
  tabla <- .crm_tabla_servidor("consulta", data = reactive(.datos_prueba), col_specs = .specs_prueba,
    id_col = ".clave", modo_seleccion = "fila", page_size = 2,
    footer = c("Importes COP.", "Datos de prueba."), modal_titulo_fn = function(sel) paste("Detalle", sel$id),
    modal_contenido_fn = function(sel) p(sel$id))
}
testServer(servidor_tabla, {
  session$flushReact()
  stopifnot(grepl("Alfa", output$`consulta-tabla`$html, fixed = TRUE))
  session$setInputs(`consulta-ordenar` = "Monto")
  html <- output$`consulta-tabla`$html
  stopifnot(regexpr("Beta", html)[[1]] < regexpr("Alfa", html)[[1]])
  session$setInputs(`consulta-seleccionar` = 2)
  stopifnot(length(.eventos_tabla) == 1L, grepl("Detalle B", .eventos_tabla[[1]], fixed = TRUE))
  session$setInputs(`consulta-seleccionar` = 2)
  stopifnot(length(.eventos_tabla) == 2L)
  session$setInputs(`consulta-buscar` = "Alfa")
  html <- output$`consulta-tabla`$html
  stopifnot(grepl("Alfa", html, fixed = TRUE), !grepl("Beta", html, fixed = TRUE))
  session$setInputs(`consulta-seleccionar` = 2)
  stopifnot(length(.eventos_tabla) == 2L)
  stopifnot(grepl("Importes COP.", output$consulta_nota$html, fixed = TRUE))
  session$setInputs(`consulta-buscar` = "Sin coincidencias")
  stopifnot(grepl("Sin registros", output$`consulta-tabla`$html, fixed = TRUE))
})
cat("OK: tablas Vancouver, formatos, búsqueda, ordenamiento y reapertura.\n")


# Diferencia cero: 'inherit' no es un color válido para gt::cell_text. ----
.specs_cuadre <- list(Diferencia = list(style = function(x) list(color = "inherit")),
  PctCuadre = list(format = list(cell = list(percent = TRUE, digits = 2))))
.cuadre_html <- htmltools::renderTags(.crm_tabla_datos(data.frame(
  Mes = c("ENERO", "FEBRERO"), Diferencia = c(0, 0), PctCuadre = c(1, NA_real_)),
  col_specs = .specs_cuadre))$html
stopifnot(grepl("ENERO", .cuadre_html, fixed = TRUE), grepl("FEBRERO", .cuadre_html, fixed = TRUE),
  grepl("100.00%", .cuadre_html, fixed = TRUE))
cat("OK: cuadre con diferencias cero y meses sin movimiento.\n")
