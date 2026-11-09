# Tablero de tareas y notas del CRM ----
.tareas_filtrar_tablero <- function(datos, usuario, buscar = "", etiquetas = character(), prioridades = character(),
                                    archivadas = FALSE, cumplidas = FALSE, es_admin = FALSE) {
  datos <- .tareas_visibles_usuario(datos, usuario, es_admin)
  if (!is.data.frame(datos) || !nrow(datos)) return(datos)
  datos$Prioridad <- toupper(trimws(as.character(datos$Prioridad)))
  datos$Prioridad[is.na(datos$Prioridad) | !datos$Prioridad %in% .TSK_PRIORIDADES] <- "MEDIA"
  if (!archivadas) datos <- datos[is.na(datos$Archivado) | datos$Archivado == 0, , drop = FALSE]
  if (!cumplidas) datos <- datos[is.na(datos$Cumplido) | datos$Cumplido == 0, , drop = FALSE]
  if (!length(prioridades) || !length(etiquetas)) return(datos[0, , drop = FALSE])
  datos <- datos[datos$Prioridad %in% prioridades, , drop = FALSE]
  if (length(etiquetas)) {
    coinciden <- vapply(as.character(datos$Etiquetas), function(x) {
      tags <- if (is.na(x) || !nzchar(x)) "__sin__" else trimws(strsplit(x, "|", fixed = TRUE)[[1]])
      any(toupper(tags) %in% toupper(etiquetas))
    }, logical(1))
    datos <- datos[coinciden, , drop = FALSE]
  }
  if (length(buscar) && !is.na(buscar) && nzchar(trimws(buscar))) {
    texto <- paste(datos$titulo, datos$Descripcion, datos$Actividades)
    datos <- datos[grepl(tolower(trimws(buscar)), tolower(texto), fixed = TRUE), , drop = FALSE]
  }
  datos[order(suppressWarnings(as.Date(datos$FechaCumplimiento)), na.last = TRUE), , drop = FALSE]
}

TableroTareasUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(class = "crm-tareas-form", racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = shiny::tagList(
      shiny::tagAppendAttributes(racafeForms::racafeButton("crear", "Nueva tarea o nota", icon = "plus", ns = ns),
        `data-crm-crear-tarea` = "true"),
      racafeForms::racafeButton("actualizar", "Actualizar", icon = "arrows-rotate", variant = "secondary", ns = ns))),
    racafeForms::racafeFormRow(columns = 3, responsive = "auto",
      racafeForms::racafeInputText("buscar", "Buscar", placeholder = "Título, descripción o actividad", ns = ns),
      .crm_select("etiquetas", "Etiquetas", choices = c("Sin etiqueta" = "__sin__"),
        selected = "__sin__", multiple = TRUE, gender = "f", ns = ns),
      .crm_select("prioridades", "Prioridad", choices = .TSK_PRIORIDADES,
        selected = .TSK_PRIORIDADES, multiple = TRUE, gender = "f", ns = ns)),
    shiny::tags$div(class = "crm-tareas-switches",
      racafeForms::racafeInputSwitch("archivadas", "Incluir archivadas", value = FALSE, ns = ns),
      racafeForms::racafeInputSwitch("cumplidas", "Incluir cumplidas", value = FALSE, ns = ns)),
    shiny::uiOutput(ns("tablero"))))
}

TableroTareas <- function(id, usuario, datos, abrir, refrescar, es_admin = function() FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    etiquetas_iniciadas <- shiny::reactiveVal(FALSE)
    etiquetas_previas <- shiny::reactiveVal(character())
    fuente <- shiny::reactive(.tareas_visibles_usuario(datos(), usuario(), es_admin()))
    shiny::observeEvent(input$actualizar, refrescar())
    shiny::observeEvent(fuente(), {
      fuente <- fuente()
      shiny::req(is.data.frame(fuente))
      tags <- unique(unlist(lapply(as.character(fuente$Etiquetas), function(x) {
        if (is.na(x)) character() else trimws(strsplit(x, "|", fixed = TRUE)[[1]])
      })))
      tags <- sort(tags[nzchar(tags)])
      choices <- c(stats::setNames(tags, tags), "Sin etiqueta" = "__sin__")
      actual <- shiny::isolate(input$etiquetas)
      seleccionar_todas <- !etiquetas_iniciadas() || setequal(actual, etiquetas_previas())
      seleccion <- if (seleccionar_todas) unname(choices) else intersect(actual, unname(choices))
      .crm_update_select(session, "etiquetas", choices = choices, selected = seleccion)
      etiquetas_previas(unname(choices))
      etiquetas_iniciadas(TRUE)
    }, ignoreNULL = TRUE)
    seleccion <- shiny::reactive({
      fuente <- fuente()
      if (!is.data.frame(fuente)) return(NULL)
      .tareas_filtrar_tablero(fuente, usuario(), input$buscar, input$etiquetas, input$prioridades,
        isTRUE(input$archivadas), isTRUE(input$cumplidas), es_admin = es_admin())
    })
    output$tablero <- .crm_render_ui({
      fuente <- seleccion()
      if (is.null(fuente)) return(racafeForms::racafeFormMessage("No fue posible cargar las tareas.", type = "error"))
      if (!nrow(fuente)) return(racafeForms::racafeFormMessage(
        "No hay tareas ni notas para los filtros seleccionados.", type = "empty"))
      columnas <- lapply(.TSK_PRIORIDADES, function(prioridad) {
        filas <- fuente[fuente$Prioridad == prioridad, , drop = FALSE]
        tarjetas <- lapply(seq_len(nrow(filas)), function(i) {
          fila <- filas[i, , drop = FALSE]
          actividades <- .tareas_actividades(fila)
          texto <- trimws(as.character(fila$Descripcion[[1]]))
          if (is.na(texto) || !nzchar(texto)) texto <- "Sin descripción"
          if (nchar(texto) > 180) texto <- paste0(substr(texto, 1, 177), "…")
          boton <- racafeForms::racafeButton(paste0("abrir_", fila$cons[[1]]), "Ver detalle", icon = "arrow-right",
            variant = "secondary", size = "xs", ns = session$ns)
          # Un solo evento por tablero conserva la clave y evita observers por tarjeta.
          boton <- shiny::tagAppendAttributes(boton, onclick = paste0("Shiny.setInputValue(",
            jsonlite::toJSON(session$ns("abrir"), auto_unbox = TRUE), ",",
            jsonlite::toJSON(as.character(fila$cons[[1]]), auto_unbox = TRUE), ",{priority:'event'});"))
          responsables <- as.character(fila$Responsable[[1]])
          responsables <- if (is.na(responsables)) character() else {
            unique(trimws(strsplit(responsables, "|", fixed = TRUE)[[1]]))
          }
          responsables <- responsables[nzchar(responsables)]
          if (!length(responsables)) responsables <- "Sin responsable"
          badges <- lapply(responsables, function(nombre) {
            shiny::tagAppendAttributes(racafeForms::racafeStatusBadge(nombre, "neutral", size = "xs"),
              title = paste("Responsable:", nombre))
          })
          n_actividades <- length(actividades$textos)
          racafeForms::racafeCard(padding = "compact",
            shiny::tags$details(class = "crm-tarea-detalle",
              shiny::tags$summary(
                shiny::tags$strong(as.character(fila$titulo[[1]])),
                shiny::tags$span(class = "crm-tarea-badges", badges)),
              shiny::tags$div(class = "crm-tarea-expandido",
                shiny::tags$div(class = "crm-tarea-badges",
                  racafeForms::racafeStatusBadge(
                    if (identical(as.character(fila$Tipo[[1]]), "list")) "Tarea" else "Nota", "neutral", size = "xs"),
                  racafeForms::racafeStatusBadge(if (n_actividades) {
                    paste0("Actividades: ", sum(actividades$cumplidas), "/", n_actividades)
                  } else "Sin actividades", "neutral", size = "xs"),
                  if (isTRUE(as.integer(fila$Cumplido[[1]]) == 1L)) {
                    racafeForms::racafeStatusBadge("Cumplida", "success", size = "xs")
                  },
                  if (isTRUE(as.integer(fila$Archivado[[1]]) == 1L)) {
                    racafeForms::racafeStatusBadge("Archivada", "neutral", size = "xs")
                  }),
                shiny::tags$p(texto),
                racafeForms::racafeInfoItem("Vencimiento", racafeForms::racafeDateLabel(fila$FechaCumplimiento[[1]])),
                boton)))
        })
        shiny::tags$section(class = "crm-tareas-columna",
          shiny::tags$header(shiny::tags$strong(prioridad), racafeForms::racafeDataBadge("Registros", nrow(filas))),
          if (!length(tarjetas)) .crm_nota("Sin registros en esta prioridad.") else shiny::tagList(tarjetas))
      })
      shiny::tags$div(class = "crm-tareas-tablero", columnas)
    })
    shiny::observeEvent(input$abrir, {
      fuente <- seleccion()
      shiny::req(is.data.frame(fuente), as.character(input$abrir) %in% as.character(fuente$cons))
      abrir(input$abrir)
    })
    list(datos = seleccion)
  })
}


