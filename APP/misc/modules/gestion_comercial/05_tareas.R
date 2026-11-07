# Tablero de tareas y notas del CRM ----
.tareas_filtrar_tablero <- function(datos, usuario, buscar = "", etiquetas = character(), prioridades = character(),
                                    archivadas = FALSE, cumplidas = FALSE) {
  datos <- .tareas_visibles_usuario(datos, usuario)
  if (!is.data.frame(datos) || !nrow(datos)) return(datos)
  datos$Prioridad <- toupper(trimws(as.character(datos$Prioridad)))
  datos$Prioridad[is.na(datos$Prioridad) | !datos$Prioridad %in% .TSK_PRIORIDADES] <- "MEDIA"
  if (!archivadas) datos <- datos[is.na(datos$Archivado) | datos$Archivado == 0, , drop = FALSE]
  if (!cumplidas) datos <- datos[is.na(datos$Cumplido) | datos$Cumplido == 0, , drop = FALSE]
  if (length(prioridades)) datos <- datos[datos$Prioridad %in% prioridades, , drop = FALSE]
  if (length(etiquetas) && !"__todas__" %in% etiquetas) {
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
  shiny::tagList(
    racafeForms::racafeActionBar(
      shiny::tagAppendAttributes(racafeForms::racafeButton("crear", "Nueva tarea o nota", icon = "plus", ns = ns),
        `data-crm-crear-tarea` = "true"),
      racafeForms::racafeButton("actualizar", "Actualizar", icon = "arrows-rotate", variant = "secondary", ns = ns)),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputText("buscar", "Buscar", placeholder = "Título, descripción o actividad", ns = ns),
      racafeForms::racafeInputSelect("etiquetas", "Etiquetas", choices = c("Todas" = "__todas__"),
        selected = "__todas__", multiple = TRUE, ns = ns),
      racafeForms::racafeInputSelect("prioridades", "Prioridad", choices = .TSK_PRIORIDADES,
        selected = .TSK_PRIORIDADES, multiple = TRUE, ns = ns)),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputSwitch("archivadas", "Incluir archivadas", value = FALSE, ns = ns),
      racafeForms::racafeInputSwitch("cumplidas", "Incluir cumplidas", value = FALSE, ns = ns)),
    .crm_nota(c("Se muestran los registros creados por usted o asignados a su usuario.",
      "Abra una tarjeta para revisar actividades, comentar, archivar o marcar cumplida.")),
    shiny::uiOutput(ns("tablero")))
}

TableroTareas <- function(id, usuario, datos, abrir, refrescar) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$actualizar, refrescar())
    shiny::observeEvent(datos(), {
      fuente <- datos()
      shiny::req(is.data.frame(fuente))
      tags <- unique(unlist(lapply(as.character(fuente$Etiquetas), function(x) {
        if (is.na(x)) character() else trimws(strsplit(x, "|", fixed = TRUE)[[1]])
      })))
      tags <- sort(tags[nzchar(tags)])
      choices <- c("Todas" = "__todas__", stats::setNames(tags, tags), "Sin etiqueta" = "__sin__")
      actual <- shiny::isolate(input$etiquetas)
      actual <- intersect(actual, unname(choices))
      racafeForms::racafeUpdateSelect(session, "etiquetas", choices = choices,
        selected = if (length(actual)) actual else "__todas__")
    }, ignoreNULL = TRUE)
    seleccion <- shiny::reactive({
      fuente <- datos()
      if (!is.data.frame(fuente)) return(NULL)
      .tareas_filtrar_tablero(fuente, usuario(), input$buscar, input$etiquetas, input$prioridades,
        isTRUE(input$archivadas), isTRUE(input$cumplidas))
    })
    output$tablero <- shiny::renderUI({
      fuente <- seleccion()
      if (is.null(fuente)) return(racafeForms::racafeFormMessage("No fue posible cargar las tareas.", type = "error"))
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
          racafeForms::racafeCard(title = as.character(fila$titulo[[1]]),
            shiny::tags$p(texto),
            racafeForms::racafeInfoItem("Responsable", as.character(fila$Responsable[[1]])),
            racafeForms::racafeInfoItem("Vencimiento", racafeForms::racafeDateLabel(fila$FechaCumplimiento[[1]])),
            if (length(actividades$textos)) .crm_nota(paste(sum(actividades$cumplidas), "de",
              length(actividades$textos), "actividades cumplidas")),
            if (isTRUE(as.integer(fila$Cumplido[[1]]) == 1L)) racafeForms::racafeStatusBadge("Cumplida", "success"),
            if (isTRUE(as.integer(fila$Archivado[[1]]) == 1L)) racafeForms::racafeStatusBadge("Archivada", "neutral"),
            footer = boton)
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
