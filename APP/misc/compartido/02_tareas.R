# Reglas compartidas entre tablero, ficha y dropdown de tareas ----
.tareas_visibles_usuario <- function(datos, usuario = NULL, es_admin = FALSE) {
  if (isTRUE(es_admin) || is.null(usuario)) return(datos)
  if (length(usuario) != 1L || is.na(usuario) || !nzchar(trimws(usuario))) return(datos[0, , drop = FALSE])
  cuenta <- toupper(trimws(usuario))
  equivalencias <- if (exists("VENDEDORES_DATA", inherits = TRUE)) {
    toupper(VENDEDORES_DATA$Nombre[toupper(VENDEDORES_DATA$Asesor) == cuenta])
  } else character()
  responsable <- vapply(as.character(datos$Responsable), function(x) {
    if (is.na(x)) return(FALSE)
    any(toupper(trimws(strsplit(x, "|", fixed = TRUE)[[1]])) %in% c(cuenta, equivalencias))
  }, logical(1))
  datos[(!is.na(datos$Usuario) & toupper(as.character(datos$Usuario)) == cuenta) | responsable, , drop = FALSE]
}

.tareas_partir <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) {
    return(character(0))
  }
  trimws(strsplit(as.character(x[[1]]), "|", fixed = TRUE)[[1]])
}

.tareas_actividades <- function(registro) {
  textos <- .tareas_partir(registro$Actividades)
  estados <- .tareas_partir(registro$Cumplidas)
  if (length(estados) < length(textos)) {
    estados <- c(estados, rep("0", length(textos) - length(estados)))
  }
  if (length(estados) > length(textos)) estados <- estados[seq_along(textos)]
  list(textos = textos, cumplidas = estados == "1")
}

.tareas_prioridad <- function(x) {
  x <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    x == "URGENTE" ~ "danger",
    x == "IMPORTANTE" ~ "warning",
    TRUE ~ "neutral"
  )
}


# Historial compatible con respuestas de texto de versiones anteriores ----
.tareas_historial <- function(respuesta) {
  texto <- if (length(respuesta) && !is.na(respuesta[[1]])) as.character(respuesta[[1]]) else ""
  prefijo <- "CRM_NOTAS_V1:"
  if (startsWith(texto, prefijo)) {
    d <- tryCatch(jsonlite::fromJSON(substring(texto, nchar(prefijo) + 1L), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (is.list(d) && all(c("anterior", "comentarios", "actividades") %in% names(d))) return(d)
  }
  list(anterior = texto, comentarios = list(), actividades = list())
}

.tareas_codificar <- function(historial) {
  paste0("CRM_NOTAS_V1:", jsonlite::toJSON(historial, auto_unbox = TRUE, null = "null"))
}

.tareas_fecha_hora <- function(fecha) {
  format(as.POSIXct(as.numeric(fecha), origin = "1970-01-01", tz = "America/Bogota"),
         "%d/%m/%Y %I:%M %p", tz = "America/Bogota")
}

.tareas_comentarios_ui <- function(historial) {
  eventos <- historial$comentarios
  shiny::tagList(
    if (nzchar(historial$anterior)) shiny::tags$p(style = "white-space:pre-wrap;", historial$anterior),
    if (length(eventos)) {
      fechas <- vapply(eventos, function(x) x$fecha, numeric(1))
      autores <- vapply(eventos, function(x) x$autor, character(1))
      texto <- vapply(eventos, function(x) x$texto, character(1))
      hora <- as.POSIXct(fechas, origin = "1970-01-01", tz = "America/Bogota")
      racafeModulos::TimelineUI(data.frame(Fecha = as.Date(hora, tz = "America/Bogota"),
        Evento = autores, Descripcion = texto, Usuario = substring(.tareas_fecha_hora(fechas), 12L)),
        max_height = 180, agrupar = TRUE)
    }
  )
}

.tareas_metadatos_ui <- function(fila) {
  fecha <- if (length(fila$FechaHoraCrea)) {
    format(as.Date(fila$FechaHoraCrea, tz = "America/Bogota"), "%d/%m/%y")
  }
  racafeForms::racafeInfoGrid(
    racafeForms::racafeInfoItem("Creado por", fila$Usuario),
    racafeForms::racafeInfoItem("Fecha de creación", fecha, empty = "Fecha sin registrar"),
    racafeForms::racafeInfoItem("Asignado a", fila$Responsable),
    columns = 3
  )
}

.tareas_actividades_ui <- function(fila, historial) {
  a <- .tareas_actividades(fila)
  if (!length(a$textos)) return(NULL)
  shiny::tags$ul(class = "list-unstyled mb-0", lapply(seq_along(a$textos), function(i) {
    meta <- if (length(historial$actividades) >= i) historial$actividades[[i]]
    detalle <- if (length(meta$fecha)) paste("Cumplida el", .tareas_fecha_hora(meta$fecha), "·", meta$autor) else {
      "Fecha de cumplimiento sin registrar"
    }
    shiny::tags$li(shiny::icon(if (a$cumplidas[i]) "circle-check" else "circle"), " ", a$textos[i],
      if (a$cumplidas[i]) shiny::tags$small(class = "d-block text-muted", detalle),
      if (length(meta$nota) && nzchar(meta$nota)) shiny::tags$div(meta$nota))
  }))
}

.tareas_acciones <- function(fila, usuario, admin) {
  tarea <- identical(as.character(fila$Tipo[[1]]), "list")
  archivada <- isTRUE(as.integer(fila$Archivado[[1]]) == 1L)
  c(editar = "Editar",
    if (archivada) c(desarchivar = "Desarchivar") else c(archivar = "Archivar"),
    if (admin && tarea && !archivada && !isTRUE(as.integer(fila$Cumplido[[1]]) == 1L)) {
      c(cumplir = "Marcar cumplida")
    },
    if (!tarea && (admin || identical(as.character(fila$Usuario[[1]]), as.character(usuario)))) {
      c(eliminar = "Eliminar nota")
    })
}
