# Apertura del flujo: sin consultas ni escrituras reales. El formulario de persistencia se sustituye por un espía.
library(shiny)
library(dplyr)
e <- new.env(parent = globalenv())
modales <- character()
avisos <- character()
formularios <- character()
e$`::` <- function(pkg, nombre) {
  p <- as.character(substitute(pkg)); n <- as.character(substitute(nombre))
  if (p == "shiny" && n == "showModal") return(function(modal, ...) modales <<- c(modales, as.character(modal)))
  if (p == "shiny" && n == "showNotification") return(function(ui, ...) avisos <<- c(avisos, as.character(ui)))
  if (p == "racafeForms") {
    if (n == "racafeUpdateSelect") return(function(...) NULL)
    if (n == "racafeModalDetail") return(function(title, ..., size = NULL, footer = NULL) {
      shiny::tags$div(shiny::tags$h4(title), ...)
    })
    if (n == "racafeForm") return(function(..., variant = NULL) shiny::tags$div(...))
    if (n == "racafeInputSelect") return(function(id, label, choices, ns, ...) {
      shiny::selectInput(ns(id), label, choices)
    })
    if (n == "racafeButton") return(function(id, label, ns, ...) shiny::actionButton(ns(id), label))
    if (n == "racafeModalFooter") return(function(...) shiny::tags$div())
  }
  getExportedValue(p, n)
}
sys.source("APP/misc/modules/gestion_comercial/03_oportunidades.R", e)
e$CrearOportunidad <- function(...) list(creaciones = shiny::reactive(0L))
e$CrearOportunidadUI <- function(id) { formularios <<- c(formularios, id); shiny::tags$div(id = id) }
e$DescartarOportunidad <- function(...) list(n = shiny::reactive(0L))
e$.gc_tarjetas_server <- function(...) NULL
e$.gc_descarga <- function(...) NULL
e$.crm_vancouver_server <- function(...) list(seleccion = shiny::reactive(NULL), limpiar = function() NULL)
contactos <- data.frame(CodContacto = c("A", "B"), PerCod = c(123, 456), PerRazSoc = c("Uno", "Dos"),
  UsuarioCrea = c("ANA", "LUIS"))
usuario <- shiny::reactiveVal("ANA")
shiny::testServer(e$OportunidadesGestion, args = list(usuario = usuario, es_admin = shiny::reactive(FALSE),
  hechos = shiny::reactive(data.frame()), cargar = function(tabla) {
    if (tabla == "CRMNALCONTACTO") contactos else data.frame()
  }), {
  session$setInputs(crear = 1)
  stopifnot(length(formularios) == 0L, grepl("Seleccionar contacto", tail(modales, 1), fixed = TRUE),
    grepl("contacto_creacion", tail(modales, 1), fixed = TRUE))
  session$setInputs(contacto_creacion = "B", continuar_creacion = 1)
  stopifnot(length(formularios) == 0L, length(avisos) == 1L)
  session$setInputs(contacto_creacion = "A", continuar_creacion = 2)
  stopifnot(length(formularios) == 1L, grepl("crear_op", formularios[[1]], fixed = TRUE),
    grepl("Crear oportunidad", tail(modales, 1), fixed = TRUE))
  session$setInputs(contacto = "A", crear = 2)
  stopifnot(length(formularios) == 2L)
  usuario("SIN CONTACTOS")
  session$setInputs(crear = 3)
  stopifnot(length(formularios) == 2L, length(avisos) == 2L)
})
cat("OK: botón sin selección abre el selector, respeta permisos y abre el formulario con namespace correcto.\n")
