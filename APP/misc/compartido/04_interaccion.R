# Estados de salida: distinguir selección vacía, validación y fallo de cálculo ----
.crm_evaluar_salida <- function(calcular) racafeShiny::racafeEvaluateOutput(calcular)
.crm_render_ui <- function(expr, env = parent.frame(), quoted = FALSE) {
  racafeShiny::racafeRenderUI(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
}
.crm_render_plot <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
  calcular <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  .crm_render_plotly({
    grafico <- calcular()
    plotly::ggplotly(grafico, height = 450, tooltip = "all")
  })
}
.crm_render_plotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  etiqueta <- function(x) {
    if (is.character(x)) return(gsub("convencionales", "Coproductos", x, ignore.case = TRUE))
    if (is.list(x)) return(lapply(x, etiqueta))
    x
  }
  racafeShiny::racafeRenderPlotly(if (quoted) expr else substitute(expr), env = env, quoted = TRUE,
    transform = function(figura) {
      altura <- suppressWarnings(as.numeric(figura$x$layout$height))
      figura$x$layout$height <- if (length(altura) == 1L && is.finite(altura)) max(450, altura) else 450
      ejes <- unique(c("xaxis", "yaxis", grep("^[xy]axis[0-9]*$", names(figura$x$layout), value = TRUE)))
      for (eje in ejes) {
        figura$x$layout[[eje]]$showgrid <- FALSE
        figura$x$layout[[eje]]$zeroline <- FALSE
      }
      figura$x$data <- etiqueta(figura$x$data)
      figura$x$layout <- etiqueta(figura$x$layout)
      corporativos <- c("#7A2028", "#C8A77D", "#495057", "#9B7160", "#DA291C", "#ADB5BD",
        "#6C5146", "#D8C5A8", "#343A40", "#B39578")
      originales <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2",
        "#7f7f7f", "#bcbd22", "#17becf")
      figura$x$layout$colorway <- corporativos
      normalizar_color <- function(v) {
        if (is.character(v)) {
          i <- match(tolower(v), originales)
          v[!is.na(i)] <- corporativos[i[!is.na(i)]]
        } else if (is.list(v)) v <- lapply(v, normalizar_color)
        v
      }
      figura$x$data <- lapply(figura$x$data, function(traza) {
        for (campo in intersect(c("marker", "line", "fillcolor"), names(traza))) {
          traza[[campo]] <- normalizar_color(traza[[campo]])
        }
        if (identical(traza$type, "heatmap")) traza$colorscale <- list(c(0, "#F5ECDD"), c(1, "#9B7160"))
        if (is.null(traza$hovertemplate) && is.null(traza$text) &&
            !any(traza$hoverinfo %in% c("skip", "none"))) {
          if (identical(traza$type, "pie")) {
            traza$hovertemplate <- "%{label}<br>VALOR: %{value:,.2f}<br>PARTICIPACIÓN: %{percent}<extra></extra>"
          } else if (!is.null(traza$x) && !is.null(traza$y)) {
            traza$hovertemplate <- "%{x}<br>VALOR: %{y:,.2f}<extra>%{fullData.name}</extra>"
          }
        }
        traza
      })
      plotly::config(figura, displayModeBar = FALSE, displaylogo = FALSE)
    })
}

# Tipografía compartida: unidades en píxeles para Plotly y puntos para ggplot ----
.crm_tema_grafico <- function() racafeGraph::tema_racafe_ggplot()
.crm_tipografia_plotly <- function(figura) racafeGraph::aplicar_tipografia_plotly(figura)

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
  racafeForms::racafeSelectSelection(input, session, id, choices)
}

# Un único preloader de racafeShiny, también para cambios de filtros y navegación ----
.crm_loader_ui <- function(show_logo = TRUE) {
  racafeShiny::racafeLoaderUI(startup = FALSE, busy = TRUE, show_logo = show_logo,
      triggers = c(calculando = "ConsultaIndividual-aplicar_precio", calculando = "ConsultaIndividualModal-aplicar_precio",
        calculando = "crm_recalcular", actualizar = "FT_Actualizar", actualizar = "ind_kpis-actualizar",
        consultar = "Tareas-actualizar", consultar = "Oportunidades-actualizar",
        calculando = "Calculadora-pesos-Calcular", calculando = "Calculadora-dolares-Calcular",
        calculando = "Calculadora-coproductos-Calcular", calculando = "MarketBasket-analizar"),
      input_scope = ".rf-form,.rf-field,.form-group,.tabbable,.main-sidebar,.control-sidebar",
      confirm_input = "crm_recalcular")
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
  shiny::tagList(racafeModulos::MenuAccionesDependencies(), racafeModulos::TablaReactable2UI(id, ...),
    shiny::uiOutput(paste0(id, "_nota_forms")))
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

# Altura legible incluso en los gráficos que antes usaban una imagen estática ----
.crm_plot_output <- function(outputId, width = "100%", height = "450px", ...) {
  pixeles <- suppressWarnings(as.numeric(sub("px$", "", height)))
  height <- if (length(pixeles) == 1L && is.finite(pixeles)) paste0(max(450, pixeles), "px") else "450px"
  plotly::plotlyOutput(outputId, width = width, height = height, ...)
}
