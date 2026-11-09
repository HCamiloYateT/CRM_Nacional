# Regresión visual y de acciones, sin servicios de negocio ----
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1]] else ".")
library(shiny)
sys.source(file.path(root, "APP/misc/compartido/00_presentacion.R"), envir = environment())
sys.source(file.path(root, "APP/misc/compartido/04_interaccion.R"), envir = environment())
for (expresion in parse(file.path(root, "APP/misc/modules/encabezado/00_header.R"))) {
  if (is.call(expresion) && identical(expresion[[1]], as.name("<-")) && is.symbol(expresion[[2]]) &&
      as.character(expresion[[2]]) %in% c(".header_tarjeta", ".header_kpis_server")) eval(expresion)
}
.html <- function(x) htmltools::renderTags(x)$html
.checks_presentacion <- 0L
.verificar <- function(condicion) {
  stopifnot(isTRUE(condicion))
  .checks_presentacion <<- .checks_presentacion + 1L
}
html <- .html(.crm_caja_ui("accion", "Pedidos", "cart-shopping", footer = "Cantidad de pedidos.",
                           mostrar_boton = TRUE, tooltip = "Ver pedidos"))
boton <- regmatches(html, regexpr("(?s)<button\\b.*?</button>", html, perl = TRUE))
.verificar(length(boton) == 1L && nzchar(boton))
.verificar(!nzchar(trimws(gsub("<[^>]+>", "", boton))))
.verificar(grepl('title="Ver pedidos"', boton, fixed = TRUE))
.verificar(grepl('aria-label="Ver pedidos"', boton, fixed = TRUE))
.verificar(grepl("width:22px;height:22px", boton, fixed = TRUE))
.verificar(grepl("Cantidad de pedidos.", html, fixed = TRUE))
html <- .html(.header_tarjeta("Sacos asignados", "asignados", "weight-hanging", "Sacos asignados de 70 kg."))
.verificar(!grepl("<button", html, fixed = TRUE))
.verificar(grepl("Sacos asignados de 70 kg.", html, fixed = TRUE))

# Un clic abre un modal; actualizar textos no duplica observers ni genera aperturas ----
eventos <- list()
servidor <- function(input, output, session) {
  session$sendModal <- function(type, message) eventos[[length(eventos) + 1L]] <<- list(type = type, message = message)
  .crm_caja("informativa", reactive(10), texto = "Total", mostrar_boton = FALSE, footer = "Solo lectura.")
  .crm_caja("sin_accion", reactive(10), texto = "Información", mostrar_boton = TRUE)
  .crm_caja("accion", reactive(20), texto = "Detalle", titulo_modal = "Consulta",
              footer = reactive(input$nota), contenido_modal = function() p("Detalle disponible"))
}
testServer(servidor, {
  session$flushReact()
  .verificar(!grepl("<button", output$informativa_tarjeta$html, fixed = TRUE))
  .verificar(!grepl("<button", output$sin_accion_tarjeta$html, fixed = TRUE))
  .verificar(grepl("Solo lectura.", output$informativa_tarjeta$html, fixed = TRUE))
  .verificar(length(eventos) == 0L)
  session$setInputs(`accion-btn_abrir` = 1)
  .verificar(length(eventos) == 1L && eventos[[1]]$type == "show")
  .verificar(grepl("rf-modal-detail", eventos[[1]]$message$html, fixed = TRUE))
  session$setInputs(nota = "Consulta actualizada.")
  .verificar(length(eventos) == 1L)
  .verificar(grepl("Consulta actualizada.", output$accion_tarjeta$html, fixed = TRUE))
  session$setInputs(`accion-btn_abrir` = 2)
  .verificar(length(eventos) == 2L)
})

# Las métricas informativas del encabezado no tienen acción ----
selecciones <- character()
servidor_header <- function(input, output, session) {
  resumen <- reactive({
    x <- tagList()
    attr(x, "header_kpis") <- setNames(as.list(1:6),
      c("pedidos", "lotes", "produccion", "despachos", "asignados", "facturacion"))
    x
  })
  .header_kpis_server(resumen, on_detail = function(key) selecciones <<- c(selecciones, key))
}
testServer(servidor_header, {
  session$flushReact()
  session$setInputs(`header_cli_produccion-detalle` = 1, `header_cli_despachos-detalle` = 1,
                    `header_cli_asignados-detalle` = 1)
  .verificar(length(selecciones) == 0L)
  session$setInputs(`header_cli_pedidos-detalle` = 1)
  session$setInputs(`header_cli_lotes-detalle` = 1)
  session$setInputs(`header_cli_facturacion-detalle` = 1)
  .verificar(identical(selecciones, c("pedidos", "lotes", "facturacion")))
})
message("OK: ", .checks_presentacion, " comprobaciones de cajas, footer y acciones.")

