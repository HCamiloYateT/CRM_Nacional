# Población común del tablero y el dropdown ----
.tareas_visibles_usuario <- function(datos, usuario = NULL, es_admin = FALSE) {
  if (isTRUE(es_admin) || is.null(usuario)) return(datos)
  if (length(usuario) != 1L || is.na(usuario) || !nzchar(trimws(usuario))) return(datos[0, , drop = FALSE])
  responsable <- vapply(as.character(datos$Responsable), function(x) {
    if (is.na(x)) return(FALSE)
    usuario %in% trimws(strsplit(x, "|", fixed = TRUE)[[1]])
  }, logical(1))
  datos[(!is.na(datos$Usuario) & as.character(datos$Usuario) == usuario) | responsable, , drop = FALSE]
}

# PRESENTACION DE TAREAS DEL ENCABEZADO ----
.tareas_lista_item <- function(fila, input_id) {
  id <- as.character(fila$cons[[1]])
  titulo <- .header_nombre(fila$titulo, "Sin título")
  tipo <- if (identical(as.character(fila$Tipo[[1]]), "list")) "Tarea" else "Nota"
  prioridad <- .header_nombre(fila$Prioridad, "Sin prioridad")
  responsable <- .header_nombre(fila$Responsable, "Sin responsable")
  vencimiento <- suppressWarnings(as.Date(fila$FechaCumplimiento[[1]]))
  fecha <- if (is.na(vencimiento)) "Sin fecha" else format(vencimiento, "%d/%m/%Y")
  estado <- if (isTRUE(as.numeric(fila$Archivado[[1]]) == 1)) {
    "Archivada"
  } else if (isTRUE(as.numeric(fila$Cumplido[[1]]) == 1)) {
    "Cumplida"
  } else "Pendiente"
  actividades <- .tareas_actividades(fila)
  n_actividades <- length(actividades$textos)
  avance <- if (n_actividades) {
    paste0(sum(actividades$cumplidas), "/", n_actividades, " actividades")
  } else "Sin actividades"
  badges <- list(
    .header_lista_badge(tools::toTitleCase(tolower(prioridad)), .tareas_prioridad(prioridad)),
    .header_lista_badge(estado, if (estado == "Pendiente") "warning" else "neutral")
  )
  metadata <- c(Vencimiento = fecha)
  if (tipo == "Tarea") metadata <- c(metadata, Actividades = avance)
  .header_lista_item(
    titulo = titulo,
    detalle = paste(tipo, "·", gsub("|", " · ", responsable, fixed = TRUE)),
    metadata = metadata, badges = badges, id = input_id, value = id
  )
}

# Helpers ----
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

.tareas_pendientes <- function(datos, usuario = NULL) {
  requeridas <- c("cons", "titulo", "Tipo", "Usuario", "Responsable",
                  "FechaCumplimiento", "Prioridad", "Archivado", "Cumplido")
  faltantes <- setdiff(requeridas, names(datos))
  if (length(faltantes)) {
    stop(paste("CRMNALNOTAS: faltan columnas", paste(faltantes, collapse = ", ")),
         call. = FALSE)
  }
  
  datos <- .tareas_visibles_usuario(datos, usuario)
  datos <- datos %>% dplyr::filter(Archivado == 0, Cumplido == 0)

  datos %>%
    dplyr::mutate(
      prioridad_orden = dplyr::case_when(
        toupper(trimws(as.character(Prioridad))) == "URGENTE" ~ 1L,
        toupper(trimws(as.character(Prioridad))) == "IMPORTANTE" ~ 2L,
        toupper(trimws(as.character(Prioridad))) == "BAJA" ~ 4L,
        TRUE ~ 3L
      ),
      fecha_orden = suppressWarnings(as.Date(FechaCumplimiento))
    ) %>%
    dplyr::arrange(prioridad_orden, is.na(fecha_orden), fecha_orden, cons)
}

.tareas_modificar_bd <- function(original, accion, cumplidas = NULL, respuesta = NULL, usuario = NULL,
                                  es_admin = FALSE) {
  con <- racafeBD::ConectarBD()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWithTransaction(con, {
    sql <- DBI::sqlInterpolate(
      con, "SELECT cons, Actividades, Cumplidas, Archivado, Cumplido, Respuesta, Usuario, Responsable
            FROM CRMNALNOTAS WHERE cons = ?id FOR UPDATE",
      id = as.numeric(original$cons[[1]])
    )
    actual <- DBI::dbGetQuery(con, sql)
    if (nrow(actual) != 1L) {
      stop("El consecutivo no identifica un registro único. Corrija los duplicados.",
           call. = FALSE)
    }
    igual <- function(a, b) identical(as.character(a), as.character(b)) ||
      (length(a) == 1L && length(b) == 1L && is.na(a) && is.na(b))
    campos <- c("Actividades", "Cumplidas", "Archivado", "Cumplido", "Respuesta", "Usuario", "Responsable")
    if (!all(vapply(campos, function(campo) {
      igual(actual[[campo]][[1]], original[[campo]][[1]])
    }, logical(1)))) {
      stop("El registro fue modificado por otra sesión. Actualice y vuelva a abrirlo.",
           call. = FALSE)
    }
    if (!is.null(usuario) && nrow(.tareas_visibles_usuario(actual, usuario, es_admin)) != 1L) {
      stop("El registro ya no está asignado a este usuario.", call. = FALSE)
    }
    if (accion == "eliminar" && !identical(as.character(actual$Usuario[[1]]), as.character(usuario))) {
      stop("Solo el creador puede eliminar el registro.", call. = FALSE)
    }
    if (accion == "respuesta") {
      texto <- trimws(as.character(respuesta))
      if (length(texto) != 1L || is.na(texto) || !nzchar(texto)) stop("Escriba un comentario.", call. = FALSE)
      anterior <- as.character(actual$Respuesta[[1]])
      if (is.na(anterior)) anterior <- ""
      nuevo <- paste0(if (nzchar(anterior)) paste0(anterior, "\n"),
        format(Sys.time(), "%d/%m/%Y %H:%M"), " — ", usuario, ": ", texto)
      comando <- DBI::sqlInterpolate(con, "UPDATE CRMNALNOTAS SET Respuesta = ?valor WHERE cons = ?id",
        valor = nuevo, id = as.numeric(original$cons[[1]]))
    } else if (accion == "eliminar") {
      comando <- DBI::sqlInterpolate(con, "DELETE FROM CRMNALNOTAS WHERE cons = ?id",
        id = as.numeric(original$cons[[1]]))
    } else if (accion == "actividades") {
      estados <- .tareas_actividades(original)
      if (length(cumplidas) != length(estados$textos)) {
        stop("La cantidad de actividades cambió.", call. = FALSE)
      }
      valor <- paste(as.integer(cumplidas), collapse = "|")
      comando <- DBI::sqlInterpolate(
        con, "UPDATE CRMNALNOTAS SET Cumplidas = ?valor WHERE cons = ?id",
        valor = valor, id = as.numeric(original$cons[[1]])
      )
    } else if (accion == "cumplir") {
      comando <- DBI::sqlInterpolate(
        con, "UPDATE CRMNALNOTAS SET Cumplido = 1 WHERE cons = ?id",
        id = as.numeric(original$cons[[1]])
      )
    } else if (accion == "archivar") {
      comando <- DBI::sqlInterpolate(
        con, "UPDATE CRMNALNOTAS SET Archivado = 1 WHERE cons = ?id",
        id = as.numeric(original$cons[[1]])
      )
    } else stop("Acción no reconocida.", call. = FALSE)
    if (DBI::dbExecute(con, comando) != 1L) {
      stop("No fue posible actualizar el registro.", call. = FALSE)
    }
  })
  invisible(TRUE)
}

# Modulo ----
NotificacionesTareasUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    style = "min-width:310px;max-height:340px;overflow-y:auto;",
    shiny::uiOutput(ns("lista"))
  )
}

NotificacionesTareas <- function(id, usuario, notas = NULL, on_success = NULL,
                                 actualizaciones = NULL, es_admin = function() FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactivos ----
    revision_rv <- shiny::reactiveVal(0L)
    seleccionado_rv <- shiny::reactiveVal(NULL)
    guardando_rv <- shiny::reactiveVal(FALSE)
    registro_pendiente_rv <- shiny::reactiveVal(NULL)
    
    respuesta_pendiente_rv <- shiny::reactiveVal(NULL)
    resultado_r <- shiny::reactive({
      revision_rv()
      if (shiny::is.reactive(actualizaciones)) actualizaciones()
      tryCatch({
        datos <- if (is.null(notas)) racafeBD::CargarDatos("CRMNALNOTAS") else {
          if (shiny::is.reactive(notas)) notas() else notas
        }
        todos <- .tareas_visibles_usuario(datos, usuario(), es_admin())
        list(datos = .tareas_pendientes(todos), todos = todos, error = NULL)
      }, error = function(e) {
        list(datos = NULL, todos = NULL, error = conditionMessage(e))
      })
    })
    pendientes_r <- shiny::reactive(resultado_r()$datos)
    total_r <- shiny::reactive({
      resultado <- resultado_r()
      if (!is.null(resultado$error)) return(0L)
      nrow(resultado$datos)
    })
    
    # Funciones ----
    refrescar <- function() {
      revision_rv(shiny::isolate(revision_rv()) + 1L)
      if (is.function(on_success)) on_success()
    }
    abrir <- function(registro) {
      registro_pendiente_rv(NULL)
      respuesta_pendiente_rv(NULL)
      seleccionado_rv(registro)
      actividades <- .tareas_actividades(registro)
      es_tarea <- identical(as.character(registro$Tipo[[1]]), "list")
      contenido <- list(
        racafeForms::racafeInfoGrid(
          racafeForms::racafeInfoItem("TIPO", if (es_tarea) "Tarea" else "Nota"),
          racafeForms::racafeInfoItem("PRIORIDAD", registro$Prioridad[[1]]),
          racafeForms::racafeInfoItem("CREADOR", registro$Usuario[[1]]),
          racafeForms::racafeInfoItem("RESPONSABLE", registro$Responsable[[1]]),
          racafeForms::racafeInfoItem("VENCIMIENTO", racafeForms::racafeDateLabel(registro$FechaCumplimiento[[1]])),
          racafeForms::racafeInfoItem("ETIQUETAS", registro$Etiquetas[[1]]),
          columns = 2
        ),
        racafeForms::racafeInfoItem("DESCRIPCIÓN", registro$Descripcion[[1]]),
        racafeForms::racafeInfoItem("COMENTARIOS", registro$Respuesta[[1]]),
        racafeForms::racafeInputTextArea("respuesta", "Nuevo comentario", rows = 3, ns = ns)
      )
      if (es_tarea && length(actividades$textos)) {
        contenido <- c(contenido, list(
          racafeForms::racafeFormGroup(
            title = "Actividades", divider = FALSE,
            racafeForms::racafeChecklist("actividades", actividades$textos,
              checked = actividades$cumplidas, ns = ns)
          )
        ))
      }
      shiny::showModal(racafeForms::racafeModalDetail(
        title = stringr::str_to_title(stringr::str_to_lower(
          as.character(registro$titulo[[1]]))), size = "subventana3",
        do.call(shiny::tagList, contenido),
        footer = racafeForms::racafeModalFooter(
          racafeForms::racafeButtonCancel("cerrar", "Cerrar", ns = ns, size = "xs"),
          if (es_tarea && length(actividades$textos))
            racafeForms::racafeButton("guardar_actividades", "Guardar actividades",
                                      icon = "floppy-disk", variant = "secondary",
                                      ns = ns, size = "xs"),
          racafeForms::racafeButton("comentar", "Guardar comentario", icon = "comment", ns = ns, size = "xs"),
          if (identical(as.character(registro$Usuario[[1]]), as.character(usuario()))) {
            racafeForms::racafeButtonDelete("eliminar", label = "Eliminar", ns = ns, size = "xs")
          },
          racafeForms::racafeButton("archivar", "Archivar", icon = "archive",
                                    variant = "secondary", ns = ns, size = "xs"),
          racafeForms::racafeButton("cumplir", "Marcar cumplida",
                                    icon = "check", variant = "primary",
                                    ns = ns, size = "xs")
        )
      ))
    }
    guardar <- function(accion, estados = NULL, respuesta = NULL) {
      if (isTRUE(guardando_rv())) return(invisible(NULL))
      original <- shiny::isolate(seleccionado_rv())
      shiny::req(!is.null(original))
      guardando_rv(TRUE)
      on.exit(guardando_rv(FALSE), add = TRUE)
      tryCatch({
        .tareas_modificar_bd(original, accion, estados, respuesta = respuesta, usuario = usuario(),
          es_admin = es_admin())
        seleccionado_rv(NULL)
        registro_pendiente_rv(NULL)
        shiny::removeModal()
        refrescar()
        racafeForms::racafeNotify(
          session, "Registro actualizado", "Los cambios fueron guardados.",
          type = "success", duration = 4
        )
      }, error = function(e) {
        racafeForms::racafeNotify(
          session, "No fue posible actualizar", conditionMessage(e),
          type = "error", duration = 6
        )
      })
    }
    
    # Listado ----
    items_r <- shiny::reactive({
      resultado <- resultado_r()
      if (!is.null(resultado$error)) {
        return(shiny::tags$div(
          class = "dropdown-item", style = "white-space:normal;color:#b91c1c;",
          paste("No fue posible leer las tareas:", resultado$error)
        ))
      }
      datos <- resultado$datos
      if (!nrow(datos)) {
        return(racafeShiny::racafeListGroup(empty_message = "No hay tareas pendientes."))
      }
      do.call(racafeShiny::racafeListGroup, lapply(seq_len(nrow(datos)), function(i) {
        .tareas_lista_item(datos[i, , drop = FALSE], ns("abrir"))
      }))
    })
    
    shiny::observeEvent(input$abrir, {
      datos <- pendientes_r()
      fila <- datos[as.character(datos$cons) == as.character(input$abrir), , drop = FALSE]
      if (nrow(fila) != 1L) {
        racafeForms::racafeNotify(session, "Consecutivo duplicado",
                                  "El registro no tiene una clave única.", type = "error")
        return(invisible(NULL))
      }
      abrir(fila)
    })
    
    shiny::observeEvent(input$cerrar, {
      seleccionado_rv(NULL)
      registro_pendiente_rv(NULL)
      shiny::removeModal()
    })
    
    shiny::observeEvent(input$guardar_actividades, {
      registro <- shiny::isolate(seleccionado_rv())
      shiny::req(registro)
      actividades <- .tareas_actividades(registro)
      estados <- vapply(seq_along(actividades$textos), function(i) {
        as.character(i) %in% input$actividades
      }, logical(1))
      guardar("actividades", estados)
    })
    
    shiny::observeEvent(input$archivar, {
      shiny::req(seleccionado_rv())
      registro_pendiente_rv("archivar")
      racafeForms::racafeConfirm(
        session = session, id = "confirm_archivar", ns = ns,
        title = "Confirmar archivo", text = "El registro dejará de aparecer en pendientes.",
        type = "warning", confirm_label = "Archivar"
      )
    })
    
    shiny::observeEvent(input$confirm_archivar, {
      if (isTRUE(input$confirm_archivar) &&
          identical(shiny::isolate(registro_pendiente_rv()), "archivar")) {
        guardar("archivar")
      }
      registro_pendiente_rv(NULL)
    })
    
    shiny::observeEvent(input$cumplir, {
      registro <- shiny::isolate(seleccionado_rv())
      shiny::req(registro)
      actividades <- .tareas_actividades(registro)
      pendientes <- length(actividades$textos) - sum(actividades$cumplidas)
      registro_pendiente_rv("cumplir")
      racafeForms::racafeConfirm(
        session = session, id = "confirm_cumplir", ns = ns,
        title = "Confirmar cumplimiento",
        text = if (pendientes > 0L) {
          paste("Hay", pendientes, "actividades pendientes.",
                "¿Marcar toda la tarea como cumplida?")
        } else "¿Marcar este registro como cumplido?",
        type = "warning", confirm_label = "Marcar cumplida"
      )
    })

    shiny::observeEvent(input$confirm_cumplir, {
      if (isTRUE(input$confirm_cumplir) &&
          identical(shiny::isolate(registro_pendiente_rv()), "cumplir")) {
        guardar("cumplir")
      }
      registro_pendiente_rv(NULL)
    })
    
    shiny::observeEvent(input$comentar, {
      texto <- trimws(input$respuesta)
      shiny::req(nzchar(texto), seleccionado_rv())
      respuesta_pendiente_rv(texto)
      racafeForms::racafeConfirm(session, "confirm_comentar", title = "Guardar comentario",
        text = "¿Añadir este comentario al registro?", ns = ns, confirm_label = "Guardar")
    })
    shiny::observeEvent(input$confirm_comentar, {
      if (isTRUE(input$confirm_comentar) && !is.null(respuesta_pendiente_rv())) {
        guardar("respuesta", respuesta = respuesta_pendiente_rv())
      }
      respuesta_pendiente_rv(NULL)
    })
    shiny::observeEvent(input$eliminar, {
      registro <- seleccionado_rv()
      shiny::req(registro, identical(as.character(registro$Usuario[[1]]), as.character(usuario())))
      registro_pendiente_rv("eliminar")
      racafeForms::racafeConfirm(session, "confirm_eliminar", title = "Eliminar registro",
        text = "Esta acción elimina la nota o tarea. ¿Desea continuar?", ns = ns,
        type = "warning", confirm_label = "Eliminar")
    })
    shiny::observeEvent(input$confirm_eliminar, {
      if (isTRUE(input$confirm_eliminar) && identical(registro_pendiente_rv(), "eliminar")) guardar("eliminar")
      registro_pendiente_rv(NULL)
    })
    abrir_id <- function(id) {
      datos <- shiny::isolate(resultado_r()$todos)
      shiny::req(is.data.frame(datos))
      fila <- datos[as.character(datos$cons) == as.character(id), , drop = FALSE]
      if (nrow(fila) != 1L) stop("El registro no tiene una clave única o no está disponible.", call. = FALSE)
      abrir(fila)
    }

    # Retorno ----
    list(n = total_r, items = items_r, datos = shiny::reactive(resultado_r()$todos),
         abrir = abrir_id, refrescar = refrescar,
         badge_st = shiny::reactive(if (total_r()) "danger" else NULL),
         actualizaciones = shiny::reactive(revision_rv()))
  })
}


