# Edición de notas y tareas ----

.EditarTareasNacional <- function(id, usuario, guardar) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    registro <- shiny::reactiveVal(NULL)
    filas <- shiny::reactiveVal(list())
    estados <- shiny::reactiveValues()
    observadores <- list()
    consecutivo <- 0L
    leer <- function() {
      d <- filas()
      lapply(d, function(fila) {
        clave <- as.character(fila$id)
        fila$texto <- trimws(input[[paste0("texto_", clave)]] %||% fila$texto)
        fila$meta <- estados[[clave]]
        if (isTRUE(fila$meta$cumplida)) fila$meta$nota <- trimws(input[[paste0("nota_", clave)]] %||% fila$meta$nota)
        fila
      })
    }
    limpiar_fila <- function(clave) {
      for (o in observadores[[clave]]) o$destroy()
      observadores[[clave]] <<- NULL
      estados[[clave]] <- NULL
      output[[paste0("estado_", clave)]] <- NULL
    }
    nueva <- function(texto = "", cumplida = FALSE, meta = NULL) {
      consecutivo <<- consecutivo + 1L
      clave <- as.character(consecutivo)
      estados[[clave]] <- list(cumplida = cumplida, fecha = meta$fecha, autor = meta$autor, nota = meta$nota %||% "")
      boton <- paste0("cumplida_", clave)
      output[[paste0("estado_", clave)]] <- shiny::renderUI({
        estado <- estados[[clave]]
        shiny::tagList(
          racafeForms::racafeButton(boton, NULL, icon = if (estado$cumplida) "rotate-left" else "check",
                                    title = if (estado$cumplida) "Marcar pendiente" else "Marcar cumplida",
                                    size = "xxs", ns = ns),
          if (estado$cumplida) shiny::tags$small(class = "ml-2 text-muted text-nowrap",
            if (length(estado$fecha)) paste("Cumplida el", .tareas_fecha_hora(estado$fecha), "·", estado$autor)
            else "Fecha de cumplimiento sin registrar")
        )
      })
      observadores[[clave]] <<- list(
        shiny::observeEvent(input[[boton]], {
          completada <- !isTRUE(estados[[clave]]$cumplida)
          estados[[clave]] <- list(cumplida = completada,
            fecha = if (completada) as.numeric(Sys.time()), autor = if (completada) usuario(), nota = "")
          filas(leer())
        }),
        shiny::observeEvent(input[[paste0("quitar_", clave)]], {
          d <- leer()
          filas(Filter(function(x) as.character(x$id) != clave, d))
          limpiar_fila(clave)
        })
      )
      list(id = consecutivo, texto = texto, meta = estados[[clave]])
    }
    limpiar <- function() {
      for (fila in filas()) limpiar_fila(as.character(fila$id))
      filas(list())
      registro(NULL)
    }
    output$actividades <- shiny::renderUI({
      lapply(filas(), function(fila) {
        clave <- as.character(fila$id)
        shiny::tagList(
          shiny::div(class = "d-flex align-items-center",
            shiny::div(class = "flex-grow-1 mr-2",
              racafeForms::racafeInputText(paste0("texto_", clave), NULL, value = fila$texto,
                                           placeholder = "Actividad", ns = ns)),
            shiny::uiOutput(ns(paste0("estado_", clave)), inline = TRUE),
            racafeForms::racafeButton(paste0("quitar_", clave), NULL, icon = "trash",
                                      title = "Eliminar actividad", variant = "danger", size = "xxs", ns = ns)),
          if (isTRUE(fila$meta$cumplida)) racafeForms::racafeInputText(paste0("nota_", clave), "Nota de la actividad",
                                                                     value = fila$meta$nota, ns = ns)
        )
      })
    })
    shiny::observeEvent(input$agregar, filas(c(leer(), list(nueva()))))
    shiny::observeEvent(input$cancelar, { limpiar(); shiny::removeModal() })
    shiny::observeEvent(input$guardar, {
      fila <- registro()
      shiny::req(!is.null(fila))
      d <- leer()
      textos <- vapply(d, function(x) x$texto, character(1))
      if (any(!nzchar(textos) | grepl("|", textos, fixed = TRUE))) {
        return(racafeForms::racafeNotify(session, "Revise las actividades",
          "Complete o quite las filas vacías y evite el carácter |.", type = "warning"))
      }
      cambios <- list(comentario = trimws(input$comentario %||% ""), textos = textos,
                      cumplidas = vapply(d, function(x) isTRUE(x$meta$cumplida), logical(1)),
                      metadatos = lapply(d, function(x) x$meta))
      if (guardar(fila, "editar", cambios)) limpiar()
    })
    function(fila) {
      limpiar()
      registro(fila)
      historial <- .tareas_historial(fila$Respuesta)
      a <- .tareas_actividades(fila)
      tarea <- identical(as.character(fila$Tipo[[1]]), "list")
      if (tarea) filas(lapply(seq_along(a$textos), function(i) {
        meta <- if (length(historial$actividades) >= i) historial$actividades[[i]]
        nueva(a$textos[i], a$cumplidas[i], meta)
      }))
      shiny::showModal(racafeForms::racafeModalDetail(
        title = if (tarea) "Editar tarea" else "Editar nota", size = "subventana3",
        racafeForms::racafeForm(
          racafeForms::racafeInfoItem("Título", fila$titulo), .tareas_metadatos_ui(fila),
          racafeForms::racafeInfoItem("Descripción", fila$Descripcion),
          racafeForms::racafeInfoItem("Cronología de comentarios", .tareas_comentarios_ui(historial),
                                     empty = "Sin comentarios"),
          racafeForms::racafeInputTextArea("comentario", "Añadir comentario", rows = 3, ns = ns),
          if (tarea) shiny::div(class = "nota-actividades-contenedor",
            racafeForms::racafeButton("agregar", "Añadir", icon = "plus", size = "xxs", ns = ns),
            shiny::uiOutput(ns("actividades"))),
          variant = "plain"
        ),
        footer = racafeForms::racafeModalFooter(
          racafeForms::racafeButtonCancel("cancelar", ns = ns),
          racafeForms::racafeButton("guardar", "Guardar", icon = "check", ns = ns))
      ))
    }
  })
}
