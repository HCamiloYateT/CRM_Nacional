# Contrato de filtros: cero opciones no debe actuar como un comodín ----
library(shiny)
library(dplyr)
source("APP/misc/compartido/04_interaccion.R")
source("APP/misc/modules/encabezado/02_notificaciones_tareas.R")
source("APP/misc/modules/gestion_comercial/notas_tareas/01_modulo.R")
.TSK_PRIORIDADES <- c("URGENTE", "IMPORTANTE", "MEDIA", "BAJA")
x <- data.frame(cons = 1:4, titulo = c("Uno", "Dos", "Tres", "Cuatro"), Descripcion = "Texto",
  Actividades = "", FechaCumplimiento = Sys.Date(), Usuario = c("ANA", "ANA", "LUIS", "ANA"),
  Responsable = c("ANA", "ANA", "LUIS", "ANA"), Prioridad = c("URGENTE", "BAJA", "MEDIA", "MEDIA"),
  Etiquetas = c("Venta", "Cobro|Venta", "Otra", ""), Archivado = 0L, Cumplido = 0L)
tags <- c("Venta", "Cobro", "Otra", "__sin__")
f <- function(etiquetas = tags, prioridades = .TSK_PRIORIDADES) {
  .tareas_filtrar_tablero(x, "ANA", etiquetas = etiquetas, prioridades = prioridades)
}
stopifnot(nrow(f()) == 3L, identical(f("Venta", "BAJA")$cons, 2L), nrow(f(character())) == 0L,
  nrow(f(prioridades = character())) == 0L, nrow(f(NULL)) == 0L,
  nrow(.tareas_filtrar_tablero(x, "ADMIN", etiquetas = tags, prioridades = .TSK_PRIORIDADES,
    es_admin = TRUE)) == 4L)
d <- data.frame(Origen = c("Web", "Feria", NA, ""), Detalle = 1:4)
opciones <- .crm_opciones(d$Origen)
stopifnot(nrow(.crm_filtrar_opciones(d, "Origen", unname(opciones))) == 4L,
  nrow(.crm_filtrar_opciones(d, "Origen", "Web")) == 1L,
  nrow(.crm_filtrar_opciones(d, "Origen", character())) == 0L,
  nrow(.crm_filtrar_opciones(d, "Origen", "__crm_sin_dato__")) == 2L)
# La cascada conserva un vacío explícito y recupera todas tras una ausencia de opciones disponibles.
testServer(function(input, output, session) {
  seleccion <- reactive(.crm_seleccion_opciones(input, session, "filtro", input$disponibles))
}, {
  session$setInputs(disponibles = c("A", "B"))
  stopifnot(setequal(seleccion(), c("A", "B")))
  session$setInputs(filtro = character(), disponibles = c("A", "B", "C"))
  stopifnot(length(seleccion()) == 0L)
  session$setInputs(filtro = c("A", "B", "C"), disponibles = character())
  stopifnot(length(seleccion()) == 0L)
  session$setInputs(filtro = character(), disponibles = c("A", "B"))
  stopifnot(setequal(seleccion(), c("A", "B")))
})
cat("OK: filtros completos, parciales, vacíos, sin etiqueta, permisos y cascadas.\n")

# El presupuesto tampoco debe convertir NULL en selección completa.
env <- new.env(parent = globalenv())
extraer <- function(x) {
  if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return()
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]]) &&
      as.character(x[[2]]) %in% c(".filtro_completo", ".aplicar_seleccion")) eval(x, env)
  partes <- if (is.call(x)) as.list(x)[-1L] else as.list(x)
  for (i in seq_along(partes)) if (!identical(partes[[i]], quote(expr = ))) extraer(partes[[i]])
}
extraer(parse("APP/misc/modules/gestion_comercial/presupuesto/01_modulo.R"))
base <- data.frame(Segmento = c("GRANDE", "MEDIANO", "PEQUEÑO", "DETAL"))
stopifnot(!env$.filtro_completo(base, "Segmento", NULL),
  nrow(env$.aplicar_seleccion(base, "Segmento", NULL)) == 0L,
  nrow(env$.aplicar_seleccion(base, "Segmento", c("GRANDE", "MEDIANO", "PEQUEÑO"))) == 3L,
  nrow(env$.aplicar_seleccion(base, "Segmento", base$Segmento)) == 4L)
cat("OK: presupuesto con selección vacía o parcial, sin comodines implícitos.\n")
