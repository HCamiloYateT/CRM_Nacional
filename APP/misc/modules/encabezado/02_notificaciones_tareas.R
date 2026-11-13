# Población común del tablero y el dropdown ----


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
                                  es_admin = FALSE, prioridad = NULL, cambios = NULL) {
  con <- racafeBD::ConectarBD()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWithTransaction(con, {
    sql <- DBI::sqlInterpolate(
      con, "SELECT cons, Actividades, Cumplidas, Archivado, Cumplido, Respuesta, Usuario, Responsable,
                   titulo, Descripcion, Prioridad, Tipo
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
    campos <- c("Actividades", "Cumplidas", "Archivado", "Cumplido", "Respuesta", "Usuario", "Responsable",
      "titulo", "Descripcion", "Prioridad", "Tipo")
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
      if (!identical(as.character(actual$Tipo[[1]]), "list")) stop("Solo las tareas pueden marcarse como cumplidas.")
      comando <- DBI::sqlInterpolate(
        con, "UPDATE CRMNALNOTAS SET Cumplido = 1 WHERE cons = ?id",
        id = as.numeric(original$cons[[1]])
      )
    } else if (accion == "archivar") {
      comando <- DBI::sqlInterpolate(
        con, "UPDATE CRMNALNOTAS SET Archivado = 1 WHERE cons = ?id",
        id = as.numeric(original$cons[[1]])
      )
    } else if (accion == "desarchivar") {
      comando <- DBI::sqlInterpolate(con, "UPDATE CRMNALNOTAS SET Archivado = 0 WHERE cons = ?id",
        id = as.numeric(original$cons[[1]]))
    } else if (accion == "prioridad") {
      if (length(prioridad) != 1L || !prioridad %in% names(.COLORES_PRIORIDAD)) stop("La prioridad no es válida.")
      comando <- DBI::sqlInterpolate(con, "UPDATE CRMNALNOTAS SET Prioridad = ?valor WHERE cons = ?id",
        valor = prioridad, id = as.numeric(original$cons[[1]]))
    } else if (accion == "editar") {
      if (!isTRUE(es_admin) && !identical(as.character(actual$Usuario[[1]]), as.character(usuario))) {
        stop("Solo el creador o un administrador puede editar el registro.")
      }
      if (length(cambios$titulo) != 1L || is.na(cambios$titulo) || !nzchar(trimws(cambios$titulo)) ||
          nchar(cambios$titulo) > 250L || length(cambios$Descripcion) != 1L || is.na(cambios$Descripcion)) {
        stop("Ingrese un título de máximo 250 caracteres y revise la descripción.")
      }
      comando <- DBI::sqlInterpolate(con,
        "UPDATE CRMNALNOTAS SET titulo = ?titulo, Descripcion = ?descripcion WHERE cons = ?id",
        titulo = trimws(cambios$titulo), descripcion = cambios$Descripcion, id = as.numeric(original$cons[[1]]))
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
          if (es_tarea) racafeForms::racafeButton("cumplir", "Marcar cumplida",
                                    icon = "check", variant = "primary",
                                    ns = ns, size = "xs")
        )
      ))
    }
    guardar <- function(accion, estados = NULL, respuesta = NULL, prioridad = NULL, cambios = NULL) {
      if (isTRUE(guardando_rv())) return(invisible(NULL))
      original <- shiny::isolate(seleccionado_rv())
      shiny::req(!is.null(original))
      guardando_rv(TRUE)
      on.exit(guardando_rv(FALSE), add = TRUE)
      tryCatch({
        .tareas_modificar_bd(original, accion, estados, respuesta = respuesta, usuario = usuario(),
          es_admin = es_admin(), prioridad = prioridad, cambios = cambios)
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
      shiny::req(registro, identical(as.character(registro$Tipo[[1]]), "list"))
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

    # Las tarjetas y el header usan la misma selección, confirmación y escritura ----
    actuar <- function(id, accion, destino = NULL) {
      todos <- shiny::isolate(resultado_r()$todos)
      fila <- todos[as.character(todos$cons) %in% as.character(id), , drop = FALSE]
      shiny::req(nrow(fila) == 1L)
      registro_pendiente_rv(NULL)
      if (accion == "abrir") return(abrir(fila))
      if (accion == "eliminar" && !identical(as.character(fila$Usuario[[1]]), as.character(usuario()))) {
        stop("Solo el creador puede eliminar el registro.")
      }
      if (accion == "editar" && !isTRUE(es_admin()) &&
          !identical(as.character(fila$Usuario[[1]]), as.character(usuario()))) stop("No tiene permiso para editar.")
      shiny::req(accion %in% c("archivar", "desarchivar", "eliminar", "editar", "cumplir", "prioridad"))
      if (accion == "cumplir") shiny::req(identical(as.character(fila$Tipo[[1]]), "list"))
      seleccionado_rv(fila)
      if (accion == "editar") {
        shiny::showModal(racafeForms::racafeModal(title = "Editar nota o tarea", size = "subventana1",
          racafeForms::racafeInputText("editar_titulo", "Título", value = as.character(fila$titulo[[1]]), ns = ns),
          racafeForms::racafeInputTextArea("editar_descripcion", "Descripción", rows = 4,
            value = as.character(fila$Descripcion[[1]] %||% ""), ns = ns),
          racafeForms::racafeButton("solicitar_edicion", "Guardar", icon = "floppy-disk", ns = ns),
          footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
        return(invisible(NULL))
      }
      if (accion == "prioridad") shiny::req(destino %in% names(.COLORES_PRIORIDAD))
      registro_pendiente_rv(list(accion = accion, prioridad = destino))
      detalle <- if (accion == "cumplir") {
        a <- .tareas_actividades(fila)
        paste("Actividades pendientes:", sum(!a$cumplidas))
      } else if (accion == "prioridad") paste("Nueva prioridad:", destino) else as.character(fila$titulo[[1]])
      racafeForms::racafeConfirm(session, "confirm_accion_tablero", title = paste("Confirmar", accion),
        text = detalle, ns = ns, confirm_label = "Confirmar")
    }
    shiny::observeEvent(input$solicitar_edicion, {
      fila <- seleccionado_rv()
      shiny::req(fila)
      if (is.null(input$editar_titulo) || !nzchar(trimws(input$editar_titulo)) || nchar(input$editar_titulo) > 250L) {
        shiny::showNotification("Ingrese un título de máximo 250 caracteres.", type = "warning")
        return()
      }
      registro_pendiente_rv(list(accion = "editar", cambios = list(titulo = trimws(input$editar_titulo),
        Descripcion = input$editar_descripcion %||% "")))
      racafeForms::racafeConfirm(session, "confirm_accion_tablero", title = "Guardar cambios",
        text = input$editar_titulo, ns = ns, confirm_label = "Guardar")
    })
    shiny::observeEvent(input$confirm_accion_tablero, {
      pendiente <- registro_pendiente_rv()
      registro_pendiente_rv(NULL)
      if (!isTRUE(input$confirm_accion_tablero) || !is.list(pendiente)) return()
      guardar(pendiente$accion, prioridad = pendiente$prioridad, cambios = pendiente$cambios)
    })

    # Retorno ----
    list(n = total_r, items = items_r, datos = shiny::reactive(resultado_r()$todos),
         abrir = abrir_id, actuar = actuar, refrescar = refrescar,
         badge_st = shiny::reactive(if (total_r()) "danger" else NULL),
         actualizaciones = shiny::reactive(revision_rv()))
  })
}


