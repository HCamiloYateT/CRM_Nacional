# Tablero y dropdown: pertenencia exacta, filtros y reapertura ----
library(shiny)
library(dplyr)
sys.source("APP/misc/modules/encabezado/02_notificaciones_tareas.R", envir = environment())
sys.source("APP/misc/modules/gestion_comercial/05_tareas.R", envir = environment())
.TSK_PRIORIDADES <- c("URGENTE", "IMPORTANTE", "MEDIA", "BAJA")
.notas_prueba <- data.frame(cons = 1:5, titulo = paste("Tarea", 1:5), Tipo = "list",
  Usuario = c("ANA", "LUIS", "ANABEL", "ANA", "ANA"), Responsable = c("ANA", "ANA|LUIS", "ANABEL", "ANA", "ANA"),
  FechaCumplimiento = as.Date("2026-12-01"), Prioridad = c("URGENTE", "BAJA", "MEDIA", "MEDIA", "MEDIA"),
  Archivado = c(0, 0, 0, 1, 0), Cumplido = c(0, 0, 0, 0, 1), Descripcion = "Revisar cliente",
  Actividades = "Llamar|Cotizar", Cumplidas = "1|0", Etiquetas = c("Comercial", "Clientes|Comercial", "Otro", "", ""),
  Respuesta = "", stringsAsFactors = FALSE)
stopifnot(identical(.tareas_visibles_usuario(.notas_prueba, "ANA")$cons, c(1L, 2L, 4L, 5L)),
  identical(.tareas_pendientes(.notas_prueba, "ANA")$cons, 1:2),
  identical(.tareas_filtrar_tablero(.notas_prueba, "ANA", etiquetas = "Clientes")$cons, 2L),
  nrow(.tareas_filtrar_tablero(.notas_prueba, "ANA", buscar = "[")) == 0L)
.cambios_prueba <- list()
.tareas_modificar_bd <- function(original, accion, cumplidas = NULL, respuesta = NULL, usuario = NULL, es_admin = FALSE) {
  .cambios_prueba[[length(.cambios_prueba) + 1L]] <<- list(id = original$cons, accion = accion)
}
servidor_tareas <- function(input, output, session) {
  notas <- reactiveVal(.notas_prueba)
  fuente <- NotificacionesTareas("fuente", usuario = reactive("ANA"), notas = notas)
  tablero <- TableroTareas("tablero", usuario = reactive("ANA"), datos = fuente$datos,
    abrir = fuente$abrir, refrescar = fuente$refrescar)
}
testServer(servidor_tareas, {
  session$flushReact()
  stopifnot(fuente$n() == 2L, nrow(fuente$datos()) == 4L,
    grepl("Tarea 1", output$`tablero-tablero`$html, fixed = TRUE),
    !grepl("Tarea 3", output$`tablero-tablero`$html, fixed = TRUE))
  session$setInputs(`tablero-abrir` = "1")
  stopifnot(length(.cambios_prueba) == 0L)
  session$setInputs(`fuente-cumplir` = 1)
  stopifnot(length(.cambios_prueba) == 0L)
  session$setInputs(`fuente-confirm_cumplir` = TRUE)
  stopifnot(length(.cambios_prueba) == 1L, .cambios_prueba[[1]]$accion == "cumplir")
  notas(.notas_prueba[.notas_prueba$cons != 1L, ])
  session$flushReact()
  stopifnot(fuente$n() == 1L, !grepl("Tarea 1", output$`tablero-tablero`$html, fixed = TRUE))
})
cat("OK: tareas propias/asignadas, filtros, sincronización y escritura solo después de confirmar.\n")

# El permiso administrativo se propaga a ambas vistas y reacciona a cambios de rol.
stopifnot(nrow(.tareas_visibles_usuario(.notas_prueba, "ADMIN", es_admin = TRUE)) == 5L,
  nrow(.tareas_visibles_usuario(.notas_prueba, "ADMIN", es_admin = FALSE)) == 0L,
  identical(.tareas_filtrar_tablero(.notas_prueba, "ADMIN", es_admin = TRUE)$cons, 1:3))
servidor_admin <- function(input, output, session) {
  admin <- reactiveVal(TRUE)
  fuente <- NotificacionesTareas("fuente", usuario = reactive("ANA"), notas = reactive(.notas_prueba),
    es_admin = admin)
  tablero <- TableroTareas("tablero", usuario = reactive("ANA"), datos = fuente$datos,
    abrir = fuente$abrir, refrescar = fuente$refrescar, es_admin = admin)
}
testServer(servidor_admin, {
  session$flushReact()
  stopifnot(fuente$n() == 3L, nrow(fuente$datos()) == 5L,
    grepl("Tarea 3", output$`tablero-tablero`$html, fixed = TRUE))
  admin(FALSE)
  session$flushReact()
  stopifnot(fuente$n() == 2L, nrow(fuente$datos()) == 4L,
    !grepl("Tarea 3", output$`tablero-tablero`$html, fixed = TRUE))
})
cat("OK: administrador ve todas; cambiar el rol restaura el filtro de pertenencia.\n")

# Tarjetas compactas: título/responsables en summary; metadatos dentro del detalle.
testServer(TableroTareas, args = list(usuario = reactive("ANA"), datos = reactive(.notas_prueba),
  abrir = function(id) NULL, refrescar = function() NULL), {
  session$flushReact()
  html <- output$tablero$html
  stopifnot(grepl('<details class="crm-tarea-detalle">', html, fixed = TRUE),
    grepl('Actividades: 1/2', html, fixed = TRUE),
    grepl('title="Responsable: ANA"', html, fixed = TRUE),
    grepl('title="Responsable: LUIS"', html, fixed = TRUE),
    !grepl('ANA|LUIS', html, fixed = TRUE), !grepl('<details open', html, fixed = TRUE))
})
