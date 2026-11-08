# Máquina de confirmación: transporte UI y persistencia simulados.
library(shiny)
library(dplyr)
e <- new.env(parent = globalenv())
e$`::` <- function(pkg, nombre) {
  p <- as.character(substitute(pkg)); n <- as.character(substitute(nombre))
  if (p %in% c("racafeForms", "racafeShiny")) return(function(...) NULL)
  getExportedValue(p, n)
}
e$FormatearNumero <- function(value, ...) as.character(value)
sys.source('APP/misc/modules/acciones/06_crear_oportunidad.R', e)
e$Choices <- function() list(linneg = c('A LA MEDIDA', 'CONVENCIONALES'))
e$CargarDatos <- function(tabla, ...) {
 if (tabla == 'CRMNALPRODS') return(data.frame(LinNeg = 'A LA MEDIDA', Categoria = 'Café',
   Producto = 'Molido', Excluir = 'NO'))
 data.frame(CodContacto = 'A', PerCod = 1, PerRazSoc = 'Cliente', Etapa = 'CLIENTE')
}
e$obtener_etapa_contacto <- function(...) 'CLIENTE'
e$peso_saco_linneg <- function(...) 70
e$determinar_linea_negocio <- function(...) list(cod = '21000')
e$Unicos <- unique
e$EsVacio <- function(x) !length(x) || all(is.na(x)) || all(as.character(x) == '')
e$Obligatorio <- identity
e$.rc_card <- function(...) shiny::tags$div('Resumen')
e$reset <- function(...) NULL
e$MostrarModalConfirmacion <- function(...) NULL
escrituras <- list()
e$registrar_oportunidad <- function(...) escrituras[[length(escrituras) + 1L]] <<- list(...)
e$.ManejarErrorAccion <- function(error, ...) stop(error)
shiny::testServer(e$CrearOportunidad, args = list(usuario = shiny::reactive('ANA'),
  codigo_contacto = shiny::reactive('A')), {
 session$setInputs(OP_ConfirmarCreacion = 1)
 stopifnot(length(escrituras) == 0L)
 session$setInputs(OP_LinNeg = 'A LA MEDIDA', OP_Categoria = 'Café', OP_Producto = 'Molido',
  OP_Fecha = Sys.Date() + 7, OP_Sacos = 10, OP_Frecuencia = 30, OP_Margen = 2)
 session$setInputs(OP_Crear = 1)
 stopifnot(length(escrituras) == 0L)
 session$setInputs(OP_Sacos = 999)
 session$setInputs(OP_ConfirmarCreacion = 2)
 stopifnot(length(escrituras) == 1L, escrituras[[1]]$sacos == 10, escrituras[[1]]$frecuencia == 30)
 session$setInputs(OP_ConfirmarCreacion = 3)
 stopifnot(length(escrituras) == 1L)
})
cat('OK: creación confirmada, snapshot de valores revisados y protección contra doble guardado.\n')
