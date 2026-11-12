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
        `data-rf-controlbar-tab` = "Crear tareas"),
NULL)),
    racafeForms::racafeFormRow(columns = 2, responsive = "auto",
      racafeForms::racafeInputText("buscar", "Buscar", placeholder = "Título, descripción o actividad", ns = ns),
      .crm_select("etiquetas", "Etiquetas", choices = c("Sin etiqueta" = "__sin__"),
        selected = "__sin__", multiple = TRUE, gender = "f", ns = ns)),
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
      .tareas_filtrar_tablero(fuente, usuario(), input$buscar, input$etiquetas, .TSK_PRIORIDADES,
        isTRUE(input$archivadas), isTRUE(input$cumplidas), es_admin = es_admin())
    })
    output$tablero <- .crm_render_ui({
      fuente <- seleccion()
      if (is.null(fuente)) return(racafeForms::racafeFormMessage("No fue posible cargar las tareas.", type = "error"))
      if (!nrow(fuente)) return(racafeForms::racafeFormMessage(
        "No hay tareas ni notas para los filtros seleccionados.", type = "empty"))
      racafeModulos::TableroTareasTarjetas(fuente, .TSK_PRIORIDADES, actividades_fn = .tareas_actividades,
        ns = session$ns, campos = c(id = "cons", titulo = "titulo", descripcion = "Descripcion",
          prioridad = "Prioridad", responsable = "Responsable", tipo = "Tipo", cumplido = "Cumplido",
          archivado = "Archivado", vencimiento = "FechaCumplimiento"), nota_fn = .crm_nota, colores = .COLORES_PRIORIDAD)
    })
    shiny::observeEvent(input$abrir, {
      fuente <- seleccion()
      shiny::req(is.data.frame(fuente), as.character(input$abrir) %in% as.character(fuente$cons))
      abrir(input$abrir)
    })
    list(datos = seleccion)
  })
}


