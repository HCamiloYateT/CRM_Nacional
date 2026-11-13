# Estados de salida: distinguir selección vacía, validación y fallo de cálculo ----
.crm_evaluar_salida <- function(calcular) racafeShiny::racafeEvaluateOutput(calcular)
.crm_render_ui <- function(expr, env = parent.frame(), quoted = FALSE) {
  racafeShiny::racafeRenderUI(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
}
.crm_render_plot <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
  calcular <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  .crm_render_plotly({
    grafico <- calcular()
    plotly::ggplotly(grafico, height = 500, tooltip = "all")
  })
}
# Formato compartido conservando intactas las expresiones de hover ----
.crm_estandarizar_plotly <- function(figura) {
  etiqueta <- function(x) {
    if (is.character(x)) return(gsub("convencionales", "Coproductos", x, ignore.case = TRUE))
    if (is.list(x)) return(lapply(x, etiqueta))
    x
  }
  altura <- suppressWarnings(as.numeric(figura$x$layout$height))
  compacto <- isTRUE(figura$x$layout$meta$crm_altura_compacta)
  minimo <- if (compacto) 160 else 500
  figura$x$layout$height <- if (length(altura) == 1L && is.finite(altura)) max(minimo, altura) else minimo
  ejes <- unique(c("xaxis", "yaxis", grep("^[xy]axis[0-9]*$", names(figura$x$layout), value = TRUE)))
  for (eje in ejes) {
    figura$x$layout[[eje]]$showgrid <- FALSE
    figura$x$layout[[eje]]$zeroline <- FALSE
  }
  for (eje in ejes) {
    titulo <- figura$x$layout[[eje]]$title
    if (is.list(titulo)) titulo <- titulo$text
    if (!length(titulo) || any(grepl("cumpl|%|porcentaje|probabili|participaci|variaci", titulo, ignore.case = TRUE)) ||
        !any(grepl("margen|precio|COP|\\$", titulo, ignore.case = TRUE))) next
    coordenada <- substr(eje, 1L, 1L)
    referencia <- sub("axis", "", eje, fixed = TRUE)
    trazas <- Filter(function(t) identical(t[[paste0(coordenada, "axis")]] %||% coordenada, referencia),
      figura$x$data)
    valores <- unlist(lapply(trazas, function(t) t[[coordenada]]), use.names = FALSE)
    if (!is.numeric(valores) || !any(is.finite(valores))) next
    marcas <- pretty(range(valores[is.finite(valores)]), n = 5)
    figura$x$layout[[eje]]$tickmode <- "array"
    figura$x$layout[[eje]]$tickvals <- marcas
    figura$x$layout[[eje]]$ticktext <- vapply(marcas, function(v) {
      if (any(grepl("millones", titulo, ignore.case = TRUE))) v <- v * 1e6
      divisor <- if (abs(v) >= 1e6) 1e6 else if (abs(v) >= 1e3) 1e3 else 1
      unidad <- if (divisor == 1e6) " MM" else if (divisor == 1e3) " M" else ""
      paste0("$", format(round(v / divisor, 1), trim = TRUE, scientific = FALSE), unidad)
    }, character(1))
  }
  meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE",
    "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
  for (eje in ejes) {
    coordenada <- substr(eje, 1L, 1L)
    referencia <- sub("axis", "", eje, fixed = TRUE)
    trazas <- Filter(function(t) identical(t[[paste0(coordenada, "axis")]] %||% coordenada, referencia), figura$x$data)
    fechas <- lapply(trazas, function(t) {
      v <- t[[coordenada]]
      if (inherits(v, c("Date", "POSIXt"))) return(as.Date(v))
      if (is.character(v) && length(v) && all(is.na(v) | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v))) {
        return(as.Date(substr(v, 1, 10)))
      }
      as.Date(character())
    })
    fechas <- as.Date(unlist(fechas), origin = "1970-01-01")
    fechas <- fechas[!is.na(fechas)]
    if (length(fechas) && as.numeric(max(fechas) - min(fechas)) > 45) {
      marcas <- seq(as.Date(format(min(fechas), "%Y-%m-01")), as.Date(format(max(fechas), "%Y-%m-01")), by = "month")
      marcas <- marcas[seq.int(1L, length(marcas), by = max(1L, ceiling(length(marcas) / 12L)))]
      figura$x$layout[[eje]]$tickmode <- "array"
      figura$x$layout[[eje]]$tickvals <- as.character(marcas)
      figura$x$layout[[eje]]$ticktext <- paste(meses[as.integer(format(marcas, "%m"))], format(marcas, "%Y"))
    }
    titulo <- figura$x$layout[[eje]]$title
    if (is.list(titulo)) titulo <- titulo$text
    if (length(titulo) && any(grepl("clientes|unidades|pedidos|facturas|registros|conteo", titulo,
      ignore.case = TRUE))) {
      v <- unlist(lapply(trazas, function(t) t[[coordenada]]), use.names = FALSE)
      if (is.numeric(v) && any(is.finite(v))) {
        marcas <- pretty(range(v[is.finite(v)]), n = 5)
        figura$x$layout[[eje]]$tickmode <- "array"; figura$x$layout[[eje]]$tickvals <- marcas
        figura$x$layout[[eje]]$ticktext <- vapply(marcas, function(n) {
          if (abs(n) >= 1000) paste0(format(round(n / 1000, 1), trim = TRUE), " K") else format(round(n), trim = TRUE)
        }, character(1))
      }
    }
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
        traza$hovertemplate <- paste0("<b>%{label}</b><br><b>VALOR:</b> %{value:,.2f}",
          "<br><b>PARTICIPACIÓN:</b> %{percent}<extra></extra>")
      } else if (!is.null(traza$x) && !is.null(traza$y)) {
        traza$hovertemplate <- paste0("<b>%{fullData.name}</b><br><b>PERIODO:</b> %{x}",
          "<br><b>VALOR:</b> %{y:,.2f}<extra></extra>")
      }
    }
    traza
  })
  dominios <- lapply(grep("^yaxis[0-9]*$", names(figura$x$layout), value = TRUE), function(eje)
    figura$x$layout[[eje]]$domain %||% c(0, 1))
  filas <- length(unique(vapply(dominios, function(d) paste(round(d, 3), collapse = "|"), character(1))))
  if (!compacto) figura$x$layout$height <- max(figura$x$layout$height, max(1L, filas) * 300)
  nombres <- vapply(figura$x$data, function(t) if (is.null(t$name)) "" else as.character(t$name)[1], character(1))
  renglones <- max(1L, ceiling(sum(pmin(nchar(nombres) + 5L, 90L)) / 100L))
  figura$x$layout$legend <- utils::modifyList(figura$x$layout$legend %||% list(),
    list(orientation = "h", x = 0, xanchor = "left", y = -0.23, yanchor = "top"))
  figura$x$layout$margin$b <- max(figura$x$layout$margin$b %||% 0, if (compacto) {
    if (identical(figura$x$layout$showlegend, FALSE)) 30 else 45 + 15 * renglones
  } else 110 + 22 * renglones)
  figura$x$layout$hoverlabel <- list(bgcolor = "#F1F3F5", font = list(color = "#343A40", size = 12))
  lineas <- vapply(figura$x$data, function(t) grepl("lines", t$mode %||% ""), logical(1))
  if (any(lineas)) figura$x$layout$hovermode <- "x unified"
  figura$x$data <- Filter(function(t) !identical(t$mode, "text"), figura$x$data)
  hover_estandar <- function(texto) {
    partes <- strsplit(texto, "<br[[:space:]]*/?>|\\n", perl = TRUE)[[1]]
    if (!length(partes)) return(texto)
    if (!grepl("^<b>", partes[1])) partes[1] <- paste0("<b>", partes[1], "</b>")
    if (length(partes) > 1L) partes[-1L] <- gsub(
      "^([[:alpha:]][[:alnum:] ()/%·_-]{0,90}):", "<b>\\1:</b>", partes[-1L], perl = TRUE)
    paste(partes, collapse = "<br>")
  }
  figura$x$data <- lapply(figura$x$data, function(t) {
    if (!identical(t$type, "treemap")) {
      if (!grepl("text", t$mode %||% "")) t$textfont <- NULL
      if (!is.null(t$mode)) t$mode <- gsub("(^text\\+|\\+text$|text)", "", t$mode)
      if (t$type %in% c("bar", "waterfall")) t$textposition <- "none"
      if (t$type %in% c("pie", "funnel", "sunburst")) t$textinfo <- "none"
    } else t$textinfo <- "label"
    if (!is.null(t$hovertemplate)) {
      t$hovertemplate <- gsub("<extra>.*?</extra>", "", t$hovertemplate, perl = TRUE)
      t$hovertemplate <- paste0(vapply(t$hovertemplate, hover_estandar, character(1)), "<extra></extra>")
    } else if (is.character(t$text) && !any(t$hoverinfo %in% c("skip", "none"))) {
      t$text <- vapply(t$text, hover_estandar, character(1))
      t$hovertemplate <- "%{text}<extra></extra>"
    }
    t
  })
  figura$x$layout$annotations <- Filter(function(a) {
    if (!identical(a$xref, "paper") || !identical(a$yref, "paper")) return(FALSE)
    if (!is.numeric(a$x) || !is.numeric(a$y)) return(FALSE)
    !any(vapply(dominios, function(d) a$x > 0 && a$x < 1 && a$y > d[1] && a$y < d[2], logical(1)))
  }, figura$x$layout$annotations)
  figura <- plotly::config(figura, displayModeBar = FALSE, displaylogo = FALSE, locale = "es")
  htmlwidgets::onRender(figura,
    "function(el,x){if(x.layout&&x.layout.height>0){el.style.height=x.layout.height+'px';}}")
}
.crm_render_plotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  calcular <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  sesion <- shiny::getDefaultReactiveDomain()
  estado <- shiny::reactiveVal(NULL)
  estado_registrado <- new.env(parent = emptyenv())
  estado_registrado$valor <- FALSE
  racafeShiny::racafeRenderPlotly({
    nombre <- shiny::getCurrentOutputInfo()$name
    if (!is.null(sesion) && !estado_registrado$valor) {
      local <- if (startsWith(nombre, sesion$ns(""))) substring(nombre, nchar(sesion$ns("")) + 1L) else nombre
      sesion$output[[paste0(local, "_estado")]] <- shiny::renderUI({
        mensaje <- estado()
        if (!is.null(mensaje)) shiny::tags$div(class = "crm-plot-message", .crm_nota(mensaje))
      })
      estado_registrado$valor <- TRUE
    }
    withCallingHandlers(.crm_medir(paste0("renderPlotly:", nombre), {
      figura <- calcular()
      estado(NULL)
      if (is.list(figura$x$attrs)) figura$x$attrs <- lapply(figura$x$attrs, function(a) {
        if (!grepl("text", a$mode %||% "")) a$textfont <- NULL
        a
      })
      figura
    }), error = function(e) {
      mensaje <- if (inherits(e, "validation") && nzchar(conditionMessage(e))) conditionMessage(e) else {
        if (inherits(e, "shiny.silent.error")) "No hay datos para la selección actual." else {
          "No se pudo completar el cálculo. Revise los datos e intente actualizar."
        }
      }
      estado(mensaje)
      if (inherits(e, c("validation", "shiny.silent.error"))) return(invisible(NULL))
      llamada <- conditionCall(e)
      funcion <- if (length(llamada)) paste(as.character(llamada[[1]]), collapse = "::") else "sin llamada"
      message("[Salida ", nombre, "] ", conditionMessage(e), " | función: ", funcion)
    })
  }, transform = .crm_estandarizar_plotly)
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
.crm_plot_output <- function(outputId, width = "100%", height = "500px", compact = FALSE, ...) {
  pixeles <- suppressWarnings(as.numeric(sub("px$", "", height)))
  height <- if (length(pixeles) == 1L && is.finite(pixeles)) {
    paste0(max(if (compact) 160 else 500, pixeles), "px")
  } else "500px"
  shiny::tags$div(class = paste("crm-plot-frame", if (compact) "crm-plot-frame-compact"),
    shiny::uiOutput(paste0(outputId, "_estado")),
    plotly::plotlyOutput(outputId, width = width, height = height, ...))
}
