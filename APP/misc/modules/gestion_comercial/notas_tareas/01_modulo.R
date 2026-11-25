# Tablero de tareas y notas del CRM ----

.tareas_filtrar_tablero <- function(datos, usuario, buscar = "", etiquetas = character(), prioridades = character(),
                                    archivadas = FALSE, cumplidas = FALSE, es_admin = FALSE,
                                    tipo = "", vencidas = character()) {
  datos <- .tareas_visibles_usuario(datos, usuario, es_admin)
  if (!is.data.frame(datos) || !nrow(datos)) return(datos)
  datos$Prioridad <- toupper(trimws(as.character(datos$Prioridad)))
  datos$Prioridad[!datos$Prioridad %in% .TSK_PRIORIDADES] <- "MEDIA"
  fecha <- suppressWarnings(as.Date(datos$FechaCumplimiento))
  datos$Vencida <- datos$Tipo %in% "list" & !datos$Archivado %in% 1L & !datos$Cumplido %in% 1L &
    !is.na(fecha) & fecha < as.Date(Sys.time(), tz = "America/Bogota")
  if (!archivadas) datos <- datos[!datos$Archivado %in% 1L, , drop = FALSE]
  if (!cumplidas) datos <- datos[!datos$Cumplido %in% 1L, , drop = FALSE]
  if (length(prioridades)) datos <- datos[datos$Prioridad %in% prioridades, , drop = FALSE]
  if (nzchar(tipo)) datos <- datos[datos$Tipo %in% tipo, , drop = FALSE]
  datos <- datos[!datos$Prioridad %in% vencidas | datos$Vencida, , drop = FALSE]
  if (length(etiquetas)) {
    coinciden <- vapply(as.character(datos$Etiquetas), function(x) {
      tags <- .tareas_partir(x)
      any(toupper(if (length(tags)) tags else "__sin__") %in% toupper(etiquetas))
    }, logical(1))
    datos <- datos[coinciden, , drop = FALSE]
  }
  if (nzchar(trimws(buscar))) {
    texto <- paste(datos$titulo, datos$Descripcion, datos$Actividades)
    datos <- datos[grepl(tolower(trimws(buscar)), tolower(texto), fixed = TRUE), , drop = FALSE]
  }
  datos[order(suppressWarnings(as.Date(datos$FechaCumplimiento)), na.last = TRUE), , drop = FALSE]
}

TableroTareasUI <- function(id) {
  ns <- shiny::NS(id)
  columnas <- lapply(.TSK_PRIORIDADES, function(prioridad) {
    filtro <- shiny::uiOutput(ns(paste0("filtro_", prioridad)), inline = TRUE)
    colapsar <- shiny::tags$button(type = "button", class = "kb-collapse-toggle", `data-kb-toggle` = "true",
      title = paste("Colapsar", prioridad), `aria-expanded` = "true", shiny::icon("chevron-down"))
    fin <- shiny::tags$template(id = ns(paste0("fin_", prioridad)))
    contador <- shiny::textOutput(ns(paste0("cantidad_", prioridad)), inline = TRUE)
    vacia <- shiny::conditionalPanel(sprintf("output.cantidad_%s === '0'", prioridad),
                                      .crm_nota("Sin registros en esta prioridad."), ns = ns)
    columna <- racafeModulos::ColumnaKanbanUI(prioridad, shiny::tagList(fin, vacia), 0,
      color = unname(.crm_colores("prioridad", prioridad)), acciones = shiny::tagList(filtro, colapsar),
      header_filled = FALSE)
    columna <- htmltools::tagQuery(columna)$find(".kpb-col-count")$empty()$append(contador)$allTags()
    condicion <- sprintf("!input.prioridad || !input.prioridad.length || input.prioridad.includes('%s')", prioridad)
    shiny::conditionalPanel(condicion, columna, ns = ns, class = "kpb-slot", `data-kb-stage` = prioridad)
  })
  tablero <- racafeModulos::TableroKanbanUI(columnas, ns = ns)
  shiny::div(class = "crm-tareas-form",
    racafeForms::racafeForm(variant = "plain",
      racafeForms::racafeActionBar(left = racafeForms::racafeButton("crear", "Nueva tarea o nota", icon = "plus", ns = ns)),
      racafeForms::racafeFormRow(columns = 4, responsive = "fixed",
        racafeForms::racafeInputText("buscar", "Buscar", placeholder = "Título, descripción o actividad", ns = ns),
        shiny::uiOutput(ns("etiquetas_ui")),
        racafeForms::racafeInputSelect("prioridad", "Prioridad", .TSK_PRIORIDADES, multiple = TRUE,
                                       empty_label = "Todas", ns = ns),
        racafeForms::racafeInputSelect("tipo", "Tipo", c("Todos" = "", "Nota" = "text", "Tarea" = "list"), ns = ns)),
      shiny::div(class = "crm-tareas-switches",
        racafeForms::racafeInputSwitch("archivadas", "Incluir archivadas", ns = ns),
        racafeForms::racafeInputSwitch("cumplidas", "Incluir cumplidas", ns = ns)),
      shiny::tagAppendAttributes(tablero, class = "rm-task-board-shared")))
}

.tareas_tarjeta_ui <- function(fila, usuario, admin, ns) {
  actividades <- .tareas_actividades(fila)
  historial <- .tareas_historial(fila$Respuesta)
  tarea <- identical(as.character(fila$Tipo[[1]]), "list")
  etiquetas <- .tareas_partir(fila$Etiquetas)
  iconos <- c(editar = "pen", archivar = "archive", desarchivar = "box-open", cumplir = "check", eliminar = "trash")
  config <- lapply(iconos, function(icono) list(icono = icono, color = "#495057"))
  menu <- racafeModulos::MenuAccionesUI(as.character(fila$cons[[1]]), .tareas_acciones(fila, usuario, admin),
                                        config = config, ns = ns, id_evento = "AccionTarea")
  detalle <- shiny::tagList(
    .tareas_metadatos_ui(fila), racafeForms::racafeInfoItem("Descripción", fila$Descripcion),
    racafeForms::racafeInfoItem("Cronología de comentarios", .tareas_comentarios_ui(historial),
                               empty = "Sin comentarios"),
    racafeForms::racafeInfoItem("Etiquetas", if (length(etiquetas)) lapply(etiquetas, function(x) {
      shiny::tags$span(class = "badge font-weight-normal mr-1", style = "background:#e1ecf4;color:#284f6c;", x)
    }), empty = "Sin etiquetas"),
    if (tarea) racafeForms::racafeInfoItem("Actividades", .tareas_actividades_ui(fila, historial),
                                          empty = "Sin actividades"))
  tarjeta <- racafeModulos::TarjetaKanbanUI(fila$titulo[[1]], contexto = if (tarea) "TAREA" else "NOTA",
    responsables = racafeForms::racafeStatusBadge(.header_nombre(fila$Responsable, "Sin responsable"), size = "xs"),
    cuerpo = detalle, menu = menu, color = unname(.crm_colores("prioridad", fila$Prioridad)))
  total <- length(actividades$textos)
  avance <- sum(actividades$cumplidas)
  fecha <- suppressWarnings(as.Date(fila$FechaCumplimiento))
  resumen <- shiny::tagList(
    if (isTRUE(as.integer(fila$Archivado) == 1L)) shiny::tags$span(class = "text-muted mr-2",
      shiny::icon("archive"), " Archivada"),
    if (isTRUE(as.integer(fila$Cumplido) == 1L)) shiny::tags$span(class = "text-success",
      shiny::icon("circle-check"), " Cumplida"),
    if (length(fecha) && !is.na(fecha)) shiny::div(class = if (fila$Vencida) "text-danger" else "text-muted",
      shiny::icon("calendar"), " ", format(fecha, "%d/%m/%Y"), if (fila$Vencida) " · Vencida"),
    if (total) shiny::tagList(shiny::tags$small(sprintf("%d de %d actividades", avance, total)),
      shiny::div(class = "progress", shiny::div(class = "progress-bar bg-success", role = "progressbar",
        `aria-label` = "Actividades cumplidas", `aria-valuenow` = avance, `aria-valuemin` = 0, `aria-valuemax` = total,
        style = sprintf("width: %.2f%%;", 100 * avance / total)))))
  htmltools::tagAppendChild(tarjeta, resumen)
}

TableroTareas <- function(id, usuario, datos, abrir, refrescar, es_admin = function() FALSE, actuar = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    vencidas <- shiny::reactiveVal(character())
    tarjetas <- new.env(parent = emptyenv())
    secuencia <- 0L
    tags_disponibles <- shiny::reactive(sort(unique(unlist(lapply(datos()$Etiquetas, .tareas_partir)))))
    output$etiquetas_ui <- shiny::renderUI({
      opciones <- c("Sin etiqueta" = "__sin__", tags_disponibles())
      racafeForms::racafeInputSelect("etiquetas", "Etiquetas", opciones, multiple = TRUE, empty_label = "Todas",
                                     selected = intersect(shiny::isolate(input$etiquetas), opciones), ns = ns)
    })
    seleccion <- shiny::reactive(.tareas_filtrar_tablero(datos(), usuario(), input$buscar %||% "",
      input$etiquetas, input$prioridad, isTRUE(input$archivadas), isTRUE(input$cumplidas),
      es_admin(), input$tipo %||% "", vencidas()))
    for (p in .TSK_PRIORIDADES) local({
      columna <- p
      boton <- paste0("vencidas_", columna)
      output[[paste0("cantidad_", columna)]] <- shiny::renderText(sum(seleccion()$Prioridad == columna))
      output[[paste0("filtro_", columna)]] <- shiny::renderUI({
        activo <- columna %in% vencidas()
        shiny::tagAppendAttributes(racafeForms::racafeButton(boton, NULL, icon = "filter", size = "xxs",
          title = "Filtrar vencidas", variant = if (activo) "primary" else "secondary", ns = ns),
          `aria-pressed` = tolower(as.character(activo)))
      })
      shiny::observeEvent(input[[boton]], {
        actual <- vencidas()
        vencidas(if (columna %in% actual) setdiff(actual, columna) else c(actual, columna))
      })
    })
    montar <- function(prioridad, ancla) {
      secuencia <<- secuencia + 1L
      id <- paste0("tarjeta_", secuencia)
      contenido <- shiny::reactiveVal(NULL)
      output[[id]] <- shiny::renderUI({
        shiny::req(contenido())
        .tareas_tarjeta_ui(contenido(), usuario(), es_admin(), ns)
      })
      shiny::insertUI(ancla, "beforeBegin", shiny::uiOutput(ns(id)), immediate = TRUE)
      list(id = id, contenido = contenido, prioridad = prioridad)
    }
    shiny::observe({
      d <- seleccion()
      if (!is.data.frame(d)) d <- data.frame(cons = character(), Prioridad = character())
      for (clave in ls(tarjetas, all.names = TRUE)) {
        tarjeta <- tarjetas[[clave]]
        if (identical(d$Prioridad[match(clave, d$cons)], tarjeta$prioridad)) next
        shiny::removeUI(paste0("#", ns(tarjeta$id)), immediate = TRUE)
        output[[tarjeta$id]] <- NULL
        rm(list = clave, envir = tarjetas)
      }
      for (prioridad in .TSK_PRIORIDADES) {
        filas <- d[d$Prioridad == prioridad, , drop = FALSE]
        ancla <- paste0("#", ns(paste0("fin_", prioridad)))
        for (i in rev(seq_len(nrow(filas)))) {
          fila <- filas[i, , drop = FALSE]
          rownames(fila) <- NULL
          clave <- as.character(fila$cons)
          if (is.null(tarjetas[[clave]])) tarjetas[[clave]] <- montar(prioridad, ancla)
          tarjeta <- tarjetas[[clave]]
          tarjeta$contenido(fila)
          ancla <- paste0("#", ns(tarjeta$id))
        }
      }
    })
    shiny::observeEvent(input$AccionTarea, {
      x <- input$AccionTarea
      fila <- seleccion()[as.character(seleccion()$cons) %in% as.character(x$codigo), , drop = FALSE]
      shiny::req(nrow(fila) == 1L)
      if (is.function(actuar)) actuar(x$codigo, x$accion)
    })
    list(datos = seleccion)
  })
}
