# Estados de salida: distinguir selección vacía, validación y fallo de cálculo ----
.crm_evaluar_salida <- function(calcular) {
  tryCatch(calcular(), error = function(error) {
    if (inherits(error, "validation") && nzchar(conditionMessage(error))) stop(error)
    if (inherits(error, "shiny.silent.error")) {
      shiny::validate(shiny::need(FALSE, "No hay datos para la selección actual. Revise los filtros y los campos."))
    }
    message("[CRM cálculo] ", conditionMessage(error))
    shiny::validate(shiny::need(FALSE, "No se pudo completar el cálculo. Revise los datos e intente actualizar."))
  })
}
.crm_render_ui <- function(expr, env = parent.frame(), quoted = FALSE) {
  funcion <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  shiny::renderUI(.crm_evaluar_salida(funcion))
}
.crm_render_plot <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
  funcion <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  shiny::renderPlot(.crm_evaluar_salida(funcion), ...)
}
.crm_render_plotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  funcion <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  plotly::renderPlotly({
    figura <- .crm_evaluar_salida(funcion)
    shiny::validate(shiny::need(!is.null(figura), "No hay datos para los filtros seleccionados."))
    figura <- plotly::plotly_build(figura)
    etiqueta <- function(x) {
      if (is.character(x)) return(gsub("convencionales", "Coproductos", x, ignore.case = TRUE))
      if (is.list(x)) return(lapply(x, etiqueta))
      x
    }
    figura$x$data <- etiqueta(figura$x$data)
    figura$x$layout <- etiqueta(figura$x$layout)
    tiene_datos <- any(vapply(figura$x$data, function(traza) {
      any(vapply(traza[intersect(c("x", "y", "z", "labels", "values", "value", "text", "lat", "lon"),
        names(traza))], length, integer(1)) > 0L) || length(traza$link$value) > 0L
    }, logical(1)))
    shiny::validate(shiny::need(tiene_datos || length(figura$x$layout$annotations),
      "No hay datos para los filtros seleccionados."))
    .crm_tipografia_plotly(figura)
  })
}

# Tipografía compartida: unidades en píxeles para Plotly y puntos para ggplot ----
.crm_tema_grafico <- function() {
  ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "bold"),
    axis.title = ggplot2::element_text(size = 12), axis.text = ggplot2::element_text(size = 11),
    legend.title = ggplot2::element_text(size = 11), legend.text = ggplot2::element_text(size = 11),
    strip.text = ggplot2::element_text(size = 12), plot.margin = ggplot2::margin(10, 12, 10, 8))
}
.crm_tipografia_plotly <- function(figura) {
  eje <- list(title = list(font = list(size = 12)), tickfont = list(size = 11), automargin = TRUE)
  estilo <- list(font = list(family = "Arial, sans-serif", size = 12, color = "#343A40"),
    title = list(font = list(size = 16)), xaxis = eje, yaxis = eje,
    legend = list(font = list(size = 11)), hoverlabel = list(font = list(size = 12)))
  # Los ejes de subgráficos reciben los mismos tamaños sin cambiar rangos ni unidades.
  for (nombre in grep("^[xy]axis[0-9]+$", names(figura$x$layout), value = TRUE)) estilo[[nombre]] <- eje
  for (nombre in c("title", grep("^[xy]axis[0-9]*$", names(figura$x$layout), value = TRUE))) {
    titulo <- if (nombre == "title") figura$x$layout$title else figura$x$layout[[nombre]]$title
    if (is.character(titulo)) {
      if (nombre == "title") figura$x$layout$title <- list(text = titulo) else {
        figura$x$layout[[nombre]]$title <- list(text = titulo)
      }
    }
  }
  for (i in seq_along(figura$x$data)) {
    tamano <- figura$x$data[[i]]$textfont$size
    if (is.numeric(tamano)) figura$x$data[[i]]$textfont$size <- pmax(11, tamano)
  }
  for (i in seq_along(figura$x$layout$annotations)) {
    tamano <- figura$x$layout$annotations[[i]]$font$size
    if (is.null(tamano) || is.numeric(tamano)) figura$x$layout$annotations[[i]]$font$size <- max(11, tamano)
  }
  figura$x$layout <- utils::modifyList(figura$x$layout, estilo)
  figura
}

# Filtros múltiples: vacío es cero opciones, nunca un comodín ----
.crm_opciones <- function(valores) {
  x <- trimws(as.character(valores))
  x[is.na(x) | !nzchar(x)] <- "__crm_sin_dato__"
  x <- sort(unique(x))
  stats::setNames(x, ifelse(x == "__crm_sin_dato__", "Sin dato", x))
}
.crm_filtrar_opciones <- function(datos, columna, seleccion) {
  x <- trimws(as.character(datos[[columna]]))
  x[is.na(x) | !nzchar(x)] <- "__crm_sin_dato__"
  datos[x %in% seleccion, , drop = FALSE]
}
.crm_seleccion_opciones <- function(input, session, id, choices) {
  clave <- session$ns(id)
  if (is.null(session$userData$crm_selecciones)) session$userData$crm_selecciones <- new.env(parent = emptyenv())
  estado <- session$userData$crm_selecciones
  anterior <- estado[[clave]]
  actual <- shiny::isolate(input[[id]])
  disponibles <- unname(choices)
  seleccion <- if (is.null(anterior) || setequal(actual, anterior)) disponibles else intersect(actual, disponibles)
  estado[[clave]] <- disponibles
  seleccion
}

# Un único preloader de racafeShiny, también para cambios de filtros y navegación ----
.crm_loader_ui <- function(show_logo = TRUE) {
  shiny::tagList(shiny::tags$button(id = "crm_recalcular", type = "button", hidden = "hidden",
    tabindex = "-1", `aria-hidden` = "true"),
    racafeShiny::racafeLoaderUI(startup = FALSE, busy = TRUE, show_logo = show_logo,
      triggers = c(calculando = "crm_recalcular", actualizar = "FT_Actualizar", actualizar = "ind_kpis-actualizar",
        consultar = "Tareas-actualizar", consultar = "Oportunidades-actualizar",
        calculando = "Calculadora-pesos-Calcular", calculando = "Calculadora-dolares-Calcular",
        calculando = "Calculadora-coproductos-Calcular", calculando = "MarketBasket-analizar")))
}

# Adaptar notas de componentes anteriores sin duplicar su lógica ni cambiar sus IDs ----
.crm_caja_modal_ui <- function(id) {
  shiny::tagList(racafeModulos::CajaModalUI(id), shiny::uiOutput(paste0(id, "_nota_forms")))
}
.crm_caja_modal <- function(id, ..., footer = NULL, footer_class = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  session$output[[paste0(id, "_nota_forms")]] <- .crm_render_ui({
    texto <- if (shiny::is.reactive(footer)) footer() else footer
    if (is.null(texto) || !length(texto)) return(NULL)
    .crm_nota(texto)
  })
  racafeModulos::CajaModal(id = id, ..., footer = NULL)
}
.crm_tabla_legacy_ui <- function(id, ...) {
  shiny::tagList(racafeModulos::TablaReactable2UI(id, ...), shiny::uiOutput(paste0(id, "_nota_forms")))
}
.crm_tabla_legacy <- function(id, data, ..., footer = NULL, footer_tipo = "info", mostrar_nota = TRUE,
                              sortable = TRUE, modo_seleccion = "ninguno", modal_footer = NULL,
                              modal_footer_tipo = "info", modal_contenido_fn = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  resolver <- function(x) if (shiny::is.reactive(x)) x() else x
  fuente <- shiny::reactive(.crm_evaluar_salida(function() resolver(data)))
  session$output[[paste0(id, "_nota_forms")]] <- .crm_render_ui({
    x <- fuente()
    if (is.data.frame(x) && !nrow(x)) return(racafeForms::racafeFormMessage(
      "No hay registros para los filtros seleccionados.", type = "empty"))
    notas <- c(if (isTRUE(resolver(mostrar_nota)) && modo_seleccion != "ninguno") {
      if (modo_seleccion == "fila") "Seleccione una fila para consultar su detalle." else {
        "Seleccione una celda para consultar su detalle."
      }
    }, resolver(footer))
    if (length(notas)) .crm_nota(notas)
  })
  contenido <- if (is.null(modal_contenido_fn)) NULL else function(seleccion) {
    shiny::tagList(modal_contenido_fn(seleccion), .crm_nota(resolver(modal_footer)))
  }
  racafeModulos::TablaReactable2(id = id, data = fuente, ..., footer = NULL, mostrar_nota = FALSE,
    sortable = sortable, modo_seleccion = modo_seleccion, modal_footer = NULL, modal_contenido_fn = contenido)
}

.crm_iniciar_loader <- function(input) {
  shiny::observeEvent(input$crm_recalcular, { invisible(NULL) }, ignoreInit = FALSE)
}
