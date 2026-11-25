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

.tareas_modificar_bd <- function(original, accion, usuario, es_admin = FALSE, cambios = NULL) {
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
    if (accion == "eliminar" && (actual$Tipo[[1]] != "text" ||
        (!isTRUE(es_admin) && !identical(as.character(actual$Usuario[[1]]), as.character(usuario))))) {
      stop("Solo el creador o un administrador puede eliminar una nota.", call. = FALSE)
    }
    if (accion == "cumplir" && (!isTRUE(es_admin) || isTRUE(as.integer(actual$Archivado[[1]]) == 1L))) {
      stop("Solo un administrador puede marcar una tarea activa como cumplida.", call. = FALSE)
    }
    if (accion == "editar") {
      historial <- .tareas_historial(actual$Respuesta)
      if (nzchar(cambios$comentario)) historial$comentarios <- c(historial$comentarios, list(
        list(fecha = as.numeric(Sys.time()), autor = usuario, texto = cambios$comentario)))
      if (actual$Tipo[[1]] == "list") historial$actividades <- cambios$metadatos
      textos <- if (actual$Tipo[[1]] == "list") paste(cambios$textos, collapse = "|") else actual$Actividades[[1]]
      estados <- if (actual$Tipo[[1]] == "list") paste(as.integer(cambios$cumplidas), collapse = "|") else {
        actual$Cumplidas[[1]]
      }
      comando <- DBI::sqlInterpolate(con,
        "UPDATE CRMNALNOTAS SET Actividades = ?actividades, Cumplidas = ?cumplidas, Respuesta = ?respuesta
         WHERE cons = ?id", actividades = textos, cumplidas = estados, respuesta = .tareas_codificar(historial),
        id = as.numeric(original$cons[[1]]))
    } else if (accion == "eliminar") {
      comando <- DBI::sqlInterpolate(con, "DELETE FROM CRMNALNOTAS WHERE cons = ?id",
        id = as.numeric(original$cons[[1]]))
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
    } else stop("Acción no reconocida.", call. = FALSE)
    afectadas <- DBI::dbExecute(con, comando)
    if (!afectadas %in% if (accion == "editar") 0:1 else 1L) {
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
    
    revision <- shiny::reactiveVal(0L)
    pendiente <- shiny::reactiveVal(NULL)
    resultado <- shiny::reactive({
      revision()
      if (shiny::is.reactive(actualizaciones)) actualizaciones()
      tryCatch({
        d <- if (is.null(notas)) racafeBD::CargarDatos("CRMNALNOTAS") else if (is.function(notas)) notas() else notas
        d <- .tareas_visibles_usuario(d, usuario(), es_admin())
        list(datos = d, pendientes = .tareas_pendientes(d), error = NULL)
      }, error = function(e) list(datos = NULL, pendientes = NULL, error = conditionMessage(e)))
    })
    refrescar <- function() {
      revision(shiny::isolate(revision()) + 1L)
      if (is.function(on_success)) on_success()
    }
    guardar <- function(fila, accion, cambios = NULL) {
      tryCatch({
        .tareas_modificar_bd(fila, accion, usuario = usuario(), es_admin = es_admin(), cambios = cambios)
        shiny::removeModal()
        refrescar()
        racafeForms::racafeNotify(session, "Registro actualizado", type = "success")
        TRUE
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No fue posible actualizar", conditionMessage(e), type = "error")
        FALSE
      })
    }
    editar <- .EditarTareasNacional("edicion", usuario, guardar)
    obtener <- function(id) {
      d <- resultado()$datos
      fila <- d[as.character(d$cons) %in% as.character(id), , drop = FALSE]
      shiny::req(nrow(fila) == 1L)
      fila
    }
    abrir <- function(id) editar(obtener(id))
    actuar <- function(id, accion) {
      fila <- obtener(id)
      if (accion == "editar") return(editar(fila))
      shiny::req(accion %in% names(.tareas_acciones(fila, usuario(), es_admin())))
      pendiente(list(fila = fila, accion = accion))
      titulo <- switch(accion, archivar = "¿Archivar este registro?", desarchivar = "¿Desarchivar este registro?",
                       cumplir = "¿Marcar la tarea como cumplida?", eliminar = "¿Eliminar esta nota?")
      racafeForms::racafeConfirm(session, "confirmar", titulo, fila$titulo[[1]], ns = ns)
    }
    shiny::observeEvent(input$confirmar, {
      evento <- pendiente()
      pendiente(NULL)
      shiny::req(isTRUE(input$confirmar), !is.null(evento))
      guardar(evento$fila, evento$accion)
    })
    shiny::observeEvent(input$abrir, abrir(input$abrir))
    items <- shiny::reactive({
      d <- resultado()
      if (!is.null(d$error)) return(racafeForms::racafeFormMessage(d$error, type = "error"))
      items <- lapply(seq_len(nrow(d$pendientes)), function(i) {
        .tareas_lista_item(d$pendientes[i, , drop = FALSE], ns("abrir"))
      })
      do.call(racafeShiny::racafeListGroup, c(items, list(empty_message = "No hay notas ni tareas pendientes.")))
    })
    total <- shiny::reactive(NROW(resultado()$pendientes))
    list(n = total, items = items, datos = shiny::reactive(resultado()$datos), abrir = abrir, actuar = actuar,
         refrescar = refrescar, badge_st = shiny::reactive(if (total()) "danger" else NULL),
         actualizaciones = shiny::reactive(revision()))
  })
}
