# Modulos Auxiliares ----
## DetalleCurvaConversion ----
DetalleCurvaConversionClienteUI <- function(id, tipo) {
  ns <- NS(id)
  choices <- c("Ninguno (solo Total)" = "ninguno",
               "Origen" = "Origen",
               "Detalle Origen" = "DetOrigen",
               "Asesor" = "AsesorLead",
               "Línea de Negocio" = "LinNegocio",
               "Segmento" = "Segmento",
               "Cohorte" = "CohorteAnalisis"
               )
  tagList(
    fluidRow(
      style = "display:flex; gap:8px; flex-wrap:wrap; align-items:flex-end;",
      div(style = "width:220px;",
          ListaDesplegable(ns("agrupador"), label = h6("Desglosar por"), choices = choices,
                           selected = "ninguno", multiple = FALSE,size = 8)
          ),
      div(style = "width:220px;",
          ListaDesplegable(ns("unidad_tiempo"), label = h6("Unidad de Tiempo"), 
                           choices = c("Días" = "dias",
                                       "Semanas" = "semanas",
                                       "Meses" = "meses"
                                       ),
                           selected = "dias", multiple = FALSE, size = 5
                           )
          )
      ),
    br(),
    fluidRow(
      column(9,
             plotly::plotlyOutput(ns("curva_km"), height = "450px")
             ),
      column(3,
             CajaModalUI(ns("kpi_analizados")),
             CajaModalUI(ns("kpi_convertidos")),
             CajaModalUI(ns("kpi_censurados"))
             )
      )
    )
}
DetalleCurvaConversionCliente <- function(id, datos_conversion, tipo) {
  moduleServer(id, function(input, output, session) {
    # Datos ----
    datos_supervivencia_r <- reactive({
      dat <- datos_conversion()
      req(nrow(dat) > 0)
      dat %>%
        mutate(TiempoDias = ifelse(Evento, TiempoConversionAnalisis,
                                   as.numeric(difftime(Sys.time(), FechaInicioAnalisis, units = "days"))),
               EventoSurv = as.numeric(Evento)) %>%
        filter(is.finite(TiempoDias), TiempoDias >= 0)
    })
    # Curva ----
    .curva_km <- function(sub, unidad) {
      if (nrow(sub) < 2 || sum(sub$EventoSurv, na.rm = TRUE) == 0 ||
          !requireNamespace("survival", quietly = TRUE)) {
        return(NULL)
      }
      factor_t <- .factor_tiempo_cliente(unidad)
      max_tiempo <- max(sub$TiempoDias, na.rm = TRUE)
      if (!is.finite(max_tiempo)) return(NULL)
      max_periodo <- ceiling(max_tiempo / factor_t)
      periodos <- seq.int(0, max_periodo)
      tiempos <- periodos * factor_t
      fit <- survival::survfit(survival::Surv(TiempoDias, EventoSurv) ~ 1, data = sub)
      resumen <- summary(fit, times = tiempos, extend = TRUE)
      if (length(resumen$surv) != length(periodos)) return(NULL)
      data.frame(Periodo = periodos,
                 PeriodoTexto = paste0(.etiqueta_tiempo_cliente(unidad), " ", periodos),
                 TasaConversionAcum = round((1 - resumen$surv) * 100, 1),
                 RegistrosIniciales = nrow(sub),
                 Convertidos = vapply(tiempos, function(t) {
                   sum(sub$EventoSurv == 1 & sub$TiempoDias <= t, na.rm = TRUE)
                 }, numeric(1)))
    }
    fit_total_r <- reactive({
      dat <- datos_supervivencia_r()
      req(nrow(dat) > 0, sum(dat$EventoSurv, na.rm = TRUE) > 0)
      survival::survfit(survival::Surv(TiempoDias, EventoSurv) ~ 1, data = dat)
    })
    output$curva_km <- plotly::renderPlotly({
      req(input$agrupador, input$unidad_tiempo)
      dat <- datos_supervivencia_r()
      curva_total <- .curva_km(dat, input$unidad_tiempo)
      if (is.null(curva_total)) {
        return(plotly::config(plotly::plotly_empty(type = "scatter"), displayModeBar = FALSE))
      }
      p <- plotly::plot_ly()
      if (!identical(input$agrupador, "ninguno")) {
        col_grupo <- input$agrupador
        valores_grupo <- dat[[col_grupo]]
        valores_grupo[is.na(valores_grupo) | valores_grupo == ""] <- "SIN DATO"
        grupos <- unique(valores_grupo) %>% sort()
        if (identical(col_grupo, "CohorteAnalisis")) grupos <- tail(grupos, 12)
        for (g in grupos) {
          sub <- dat[valores_grupo == g, , drop = FALSE]
          curva_g <- .curva_km(sub, input$unidad_tiempo)
          if (is.null(curva_g)) next
          curva_g$Grupo <- as.character(g)
          p <- p %>%
            plotly::add_trace(data = curva_g, x = ~Periodo, y = ~TasaConversionAcum, name = as.character(g),
                              type = "scatter", mode = "lines",
                              line = list(shape = "hv", width = 2.5),
                              text = ~paste0("<b>", Grupo, "</b>", "<br>", PeriodoTexto,
                                             "<br>Registros iniciales: ", format(RegistrosIniciales, big.mark = ","),
                                             "<br>Convertidos: ", format(Convertidos, big.mark = ","),
                                             "<br>Conversión acumulada: ", TasaConversionAcum, "%"),
                              hovertemplate = "%{text}<extra></extra>")
        }
      }
      curva_total$Grupo <- "Total"
      p <- p %>%
        plotly::add_trace(data = curva_total, x = ~Periodo, y = ~TasaConversionAcum, name = "Total",
                          type = "scatter", mode = "lines",
                          line = list(shape = "hv", color = "black", width = 3, dash = "dot"),
                          text = ~paste0("<b>Total</b>", "<br>", PeriodoTexto,
                                         "<br>Registros iniciales: ", format(RegistrosIniciales, big.mark = ","),
                                         "<br>Convertidos: ", format(Convertidos, big.mark = ","),
                                         "<br>Conversión acumulada: ", TasaConversionAcum, "%"),
                          hovertemplate = "%{text}<extra></extra>")
      max_periodo <- max(curva_total$Periodo, na.rm = TRUE)
      dtick_x <- if (is.finite(max_periodo)) max(1, ceiling(max_periodo / 12)) else 1
      p %>%
        plotly::layout(
          margin = list(l = 40, r = 20, t = 10, b = 40),
          xaxis = list(title = .titulo_tiempo_cliente(input$unidad_tiempo, tipo),
                       tickmode = "linear", tick0 = 0, dtick = dtick_x, tickformat = "d"),
          yaxis = list(title = "% Convertidos a Cliente (acumulado)", range = c(0, 100)),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(orientation = "h", y = -0.2)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    # KPIs ----
    tabla_surv_r <- reactive({
      fit <- tryCatch(fit_total_r(), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      summary(fit)$table
    })
    CajaModal("kpi_analizados",
              valor = reactive({
                tabla <- tabla_surv_r()
                if (is.null(tabla)) 0 else html_valor(tabla[["records"]], formato = "coma", color = "#404040")
              }),
              texto = html_texto(if (identical(tipo, "contacto_cliente")) "Contactos analizados" else "Leads analizados",
                                 color = "#404040"),
              colores = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer = paste0("Incluye todo el universo analizado, ",
                              "sin restringir por etapa o estado vigente."))
    
    CajaModal("kpi_convertidos",
              valor = reactive({
                tabla <- tabla_surv_r()
                if (is.null(tabla)) 0 else html_valor(tabla[["events"]], formato = "coma", color = "#198754")
              }),
              texto = html_texto("Convertidos a Cliente", color = "#198754"),
              colores = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer = paste0("Contactos que alcanzaron históricamente la etapa Cliente, ",
                              "aunque actualmente estén en otra etapa o estado."))
    
    CajaModal("kpi_censurados",
              valor = reactive({
                tabla <- tabla_surv_r()
                if (is.null(tabla)) {
                  0
                } else {
                  html_valor(tabla[["records"]] - tabla[["events"]], formato = "coma", color = "#C11007")
                }
              }),
              texto = html_texto("Sin convertir aún", color = "#C11007"),
              colores = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer = paste0("Contactos sin conversión histórica registrada a Cliente ",
                              "al momento del análisis."))
    
    invisible(NULL)
  })
}

# DetalleConversionCliente ----
DetalleConversionClienteUI <- function(id, tipo) {
  ns <- NS(id)
  titulo_curva <- if (identical(tipo, "contacto_cliente")) "Conversión Contacto → Cliente" else "Conversión Lead → Cliente"
  titulo_cohorte <- if (identical(tipo, "contacto_cliente")) {
    "Conversión por Cohorte de Creación"
  } else {
    "Conversión por Cohorte de Promoción a Lead"
  }
  titulo_matriz <- if (identical(tipo, "contacto_cliente")) {
    "Matriz Mes de Creación × Meses hasta Conversión"
  } else {
    "Matriz Mes de Lead × Meses hasta Conversión"
  }
  tagList(
    fluidRow(
      column(6, box(title = "Tiempo hasta Conversión", width = 12, collapsible = FALSE,
                    plotly::plotlyOutput(ns("kpi_tiempo"), height = "260px"))),
      column(6, box(title = "Distribución del Tiempo de Conversión", width = 12, collapsible = FALSE,
                    plotly::plotlyOutput(ns("densidad_tiempo"), height = "220px"),
                    uiOutput(ns("nota_densidad"))))
    ),
    br(),
    box(title = titulo_curva, width = 12, collapsible = FALSE,
        DetalleCurvaConversionClienteUI(ns("mod_curva"), tipo = tipo)),
    br(),
    box(title = titulo_cohorte, width = 12, collapsible = TRUE, collapsed = FALSE,
        TablaReactable2UI(ns("tabla_cohorte")),
        tags$hr(),
        FormatearTexto(titulo_matriz, tamano_pct = 1, negrita = TRUE),
        div(style = "width:100%; overflow-x:auto;", TablaReactable2UI(ns("tabla_matriz"))),
        tags$hr(),
        shinyjs::hidden(
          tags$div(id = ns("BloqueDetalleCohorte"),
                   FormatearTexto("Registros de la celda seleccionada", tamano_pct = 0.9, color = "#64748B"),
                   TablaReactable2UI(ns("tabla_detalle")))
        ))
  )
}
DetalleConversionCliente <- function(id, datos_conversion, usuario, tipo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    codigo_seleccionado <- reactiveVal(NULL)
    accion_seleccionada <- reactiveVal(NULL)
    titulo_modal_actual <- reactiveVal(NULL)
    codigo_reactivar <- reactiveVal(NULL)
    trigger_reactivar <- reactiveVal(0)
    # Tiempo de conversion ----
    output$kpi_tiempo <- plotly::renderPlotly({
      dat <- datos_conversion() %>%
        filter(Evento) %>%
        mutate(RangoConversion = .rangos_antiguedad(TiempoConversionAnalisis)) %>%
        count(RangoConversion, .drop = FALSE, name = "n")
      .grafico_barras_horizontal(dat, "RangoConversion", "n",
                                 color = if (identical(tipo, "contacto_cliente")) "#1C398E" else "#C11007",
                                 titulo_x = "Clientes")
    })
    tiempos_conversion_r <- reactive({
      datos_conversion() %>%
        filter(Evento, is.finite(TiempoConversionAnalisis), TiempoConversionAnalisis >= 0) %>%
        pull(TiempoConversionAnalisis)
    })
    output$densidad_tiempo <- plotly::renderPlotly({
      x <- tiempos_conversion_r()
      if (length(x) < 3) {
        return(plotly::config(plotly::plotly_empty(type = "scatter"), displayModeBar = FALSE))
      }
      dens <- stats::density(x, from = 0)
      media <- mean(x, na.rm = TRUE)
      mediana <- stats::median(x, na.rm = TRUE)
      color <- if (identical(tipo, "contacto_cliente")) "#1C398E" else "#C11007"
      fillcolor <- if (identical(tipo, "contacto_cliente")) "rgba(28,57,142,0.15)" else "rgba(193,16,7,0.12)"
      plotly::plot_ly(x = dens$x, y = dens$y, type = "scatter", mode = "lines", fill = "tozeroy",
                      line = list(color = color), fillcolor = fillcolor,
                      hovertemplate = "Día %{x:.0f}<extra></extra>") %>%
        plotly::layout(
          margin = list(l = 30, r = 20, t = 10, b = 40),
          xaxis = list(title = if (identical(tipo, "contacto_cliente")) {
            "Días desde creación del Contacto"
          } else {
            "Días desde promoción a Lead"
          }),
          yaxis = list(title = "Densidad"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          shapes = list(
            list(type = "line", x0 = media, x1 = media, y0 = 0, y1 = max(dens$y),
                 line = list(color = "#C11007", dash = "dash", width = 1.5)),
            list(type = "line", x0 = mediana, x1 = mediana, y0 = 0, y1 = max(dens$y),
                 line = list(color = "#198754", dash = "dot", width = 1.5))
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    output$nota_densidad <- renderUI({
      x <- tiempos_conversion_r()
      if (length(x) < 3) return(NULL)
      tags$p(style = "font-size:0.75rem; color:#64748B; margin-top:4px;",
             tags$span(style = "color:#C11007;", "▬ "),
             paste0("Media: ", round(mean(x), 1), " días"),
             "   ",
             tags$span(style = "color:#198754;", "··· "),
             paste0("Mediana: ", round(median(x), 1), " días"))
    })
    # Curva de conversion ----
    DetalleCurvaConversionCliente("mod_curva", datos_conversion = datos_conversion, tipo = tipo)
    # Cohortes ----
    datos_cohorte_r <- reactive({
      dat <- datos_conversion() %>% distinct(CodContacto, .keep_all = TRUE)
      if (identical(tipo, "contacto_cliente")) {
        base <- dat %>% filter(!is.na(CohorteAnalisis))
        resumen <- base %>%
          group_by(Cohorte = CohorteAnalisis) %>%
          summarise(
            NumContactos = n_distinct(CodContacto),
            NumClientes = n_distinct(CodContacto[Evento]),
            TiempoConversionProm = round(mean(TiempoConversionAnalisis[Evento], na.rm = TRUE), 1),
            GestionesPreviasProm = round(mean(GestionesContactoCliente[Evento], na.rm = TRUE), 1),
            GestionesClienteProm = round(mean(GestionesCliente[Evento], na.rm = TRUE), 1),
            .groups = "drop"
          ) %>%
          mutate(TasaConversion = round(ifelse(NumContactos > 0, NumClientes / NumContactos * 100, 0), 1)) %>%
          arrange(desc(Cohorte))
        total_contactos <- n_distinct(base$CodContacto)
        total_clientes <- n_distinct(base$CodContacto[base$Evento])
        total <- tibble::tibble(
          Cohorte = "TOTAL",
          NumContactos = total_contactos,
          NumClientes = total_clientes,
          TiempoConversionProm = round(mean(base$TiempoConversionAnalisis[base$Evento], na.rm = TRUE), 1),
          GestionesPreviasProm = round(mean(base$GestionesContactoCliente[base$Evento], na.rm = TRUE), 1),
          GestionesClienteProm = round(mean(base$GestionesCliente[base$Evento], na.rm = TRUE), 1),
          TasaConversion = round(ifelse(total_contactos > 0, total_clientes / total_contactos * 100, 0), 1)
        )
      } else {
        base <- dat %>% filter(EsLead, !is.na(CohorteAnalisis))
        resumen <- base %>%
          group_by(Cohorte = CohorteAnalisis) %>%
          summarise(
            NumLeads = n_distinct(CodContacto),
            NumClientes = n_distinct(CodContacto[Evento]),
            TiempoConversionProm = round(mean(TiempoConversionAnalisis[Evento], na.rm = TRUE), 1),
            GestionesPreviasProm = round(mean(GestionesLeadCliente[Evento], na.rm = TRUE), 1),
            GestionesClienteProm = round(mean(GestionesCliente[Evento], na.rm = TRUE), 1),
            .groups = "drop"
          ) %>%
          mutate(TasaConversion = round(ifelse(NumLeads > 0, NumClientes / NumLeads * 100, 0), 1)) %>%
          arrange(desc(Cohorte))
        total_leads <- n_distinct(base$CodContacto)
        total_clientes <- n_distinct(base$CodContacto[base$Evento])
        total <- tibble::tibble(
          Cohorte = "TOTAL",
          NumLeads = total_leads,
          NumClientes = total_clientes,
          TiempoConversionProm = round(mean(base$TiempoConversionAnalisis[base$Evento], na.rm = TRUE), 1),
          GestionesPreviasProm = round(mean(base$GestionesLeadCliente[base$Evento], na.rm = TRUE), 1),
          GestionesClienteProm = round(mean(base$GestionesCliente[base$Evento], na.rm = TRUE), 1),
          TasaConversion = round(ifelse(total_leads > 0, total_clientes / total_leads * 100, 0), 1)
        )
      }
      bind_rows(resumen, total)
    })
    .estilo_cohorte <- .estilo_fila_total(datos_cohorte_r, "Cohorte")
    if (identical(tipo, "contacto_cliente")) {
      orden_cohorte <- c("Cohorte", "NumContactos", "NumClientes", "TasaConversion",
                         "TiempoConversionProm", "GestionesPreviasProm", "GestionesClienteProm")
      col_specs_cohorte <- list(
        Cohorte = reactable::colDef(name = "Cohorte de Creación", minWidth = 130, style = .estilo_cohorte),
        NumContactos = reactable::colDef(name = "Contactos", minWidth = 90, style = .estilo_cohorte),
        NumClientes = reactable::colDef(name = "Clientes", minWidth = 90, style = .estilo_cohorte),
        TasaConversion = reactable::colDef(name = "Tasa Contacto → Cliente", minWidth = 155,
                                           cell = function(v) paste0(v, "%"), style = .estilo_cohorte),
        TiempoConversionProm = reactable::colDef(name = "Tiempo Contacto → Cliente (días)", minWidth = 190,
                                                 style = .estilo_cohorte),
        GestionesPreviasProm = reactable::colDef(name = "Gestiones Contacto → Cliente", minWidth = 175,
                                                 style = .estilo_cohorte),
        GestionesClienteProm = reactable::colDef(name = "Gestiones como Cliente", minWidth = 150,
                                                 style = .estilo_cohorte)
      )
    } else {
      orden_cohorte <- c("Cohorte", "NumLeads", "NumClientes", "TasaConversion",
                         "TiempoConversionProm", "GestionesPreviasProm", "GestionesClienteProm")
      col_specs_cohorte <- list(
        Cohorte = reactable::colDef(name = "Cohorte de Ascenso a Lead", minWidth = 150, style = .estilo_cohorte),
        NumLeads = reactable::colDef(name = "Leads", minWidth = 90, style = .estilo_cohorte),
        NumClientes = reactable::colDef(name = "Clientes", minWidth = 90, style = .estilo_cohorte),
        TasaConversion = reactable::colDef(name = "Tasa Lead → Cliente", minWidth = 145,
                                           cell = function(v) paste0(v, "%"), style = .estilo_cohorte),
        TiempoConversionProm = reactable::colDef(name = "Tiempo Lead → Cliente (días)", minWidth = 170,
                                                 style = .estilo_cohorte),
        GestionesPreviasProm = reactable::colDef(name = "Gestiones Lead → Cliente", minWidth = 165,
                                                 style = .estilo_cohorte),
        GestionesClienteProm = reactable::colDef(name = "Gestiones como Cliente", minWidth = 150,
                                                 style = .estilo_cohorte)
      )
    }
    TablaReactable2(id = "tabla_cohorte", data = datos_cohorte_r, columnas = orden_cohorte,
                    col_specs = col_specs_cohorte, modo_seleccion = "ninguno", id_col = "Cohorte",
                    sortable = TRUE, searchable = FALSE, page_size = 12, compact = TRUE,
                    mostrar_badge = FALSE, mostrar_nota = FALSE)
    # Matriz de conversion ----
    meses_conversion <- isolate(
      datos_conversion() %>%
        filter(Evento) %>%
        mutate(MesConversionRel = .mes_conversion_rel(FechaInicioAnalisis, FechaEventoAnalisis)) %>%
        filter(!is.na(MesConversionRel), MesConversionRel >= 0) %>%
        pull(MesConversionRel) %>%
        unique() %>%
        sort()
    )
    cols_matriz <- paste0("Mes ", meses_conversion)
    matriz_cohorte_r <- reactive({
      base <- datos_conversion() %>%
        distinct(CodContacto, .keep_all = TRUE) %>%
        mutate(MesConversionRel = ifelse(Evento, .mes_conversion_rel(FechaInicioAnalisis, FechaEventoAnalisis),
                                         NA_integer_))
      if (identical(tipo, "contacto_cliente")) {
        totales <- base %>%
          filter(!is.na(CohorteAnalisis)) %>%
          group_by(CohorteAnalisis) %>%
          summarise(NumContactos = n_distinct(CodContacto), NumLeads = n_distinct(CodContacto[EsLead]),
                    NumClientes = n_distinct(CodContacto[Evento]), .groups = "drop")
      } else {
        totales <- base %>%
          filter(EsLead, !is.na(CohorteAnalisis)) %>%
          group_by(CohorteAnalisis) %>%
          summarise(NumLeads = n_distinct(CodContacto), NumClientes = n_distinct(CodContacto[Evento]),
                    .groups = "drop")
      }
      conversiones <- base %>%
        filter(Evento, !is.na(CohorteAnalisis), !is.na(MesConversionRel), MesConversionRel >= 0) %>%
        mutate(MesConversion = paste0("Mes ", MesConversionRel)) %>%
        group_by(CohorteAnalisis, MesConversion) %>%
        summarise(NumClientesMes = n_distinct(CodContacto), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = MesConversion, values_from = NumClientesMes, values_fill = 0)
      dat <- totales %>% left_join(conversiones, by = "CohorteAnalisis")
      for (col in cols_matriz) {
        if (!col %in% names(dat)) dat[[col]] <- 0L
        dat[[col]] <- coalesce(dat[[col]], 0L)
      }
      dat <- dat %>% rename(Cohorte = CohorteAnalisis)
      if (identical(tipo, "contacto_cliente")) {
        dat <- dat %>% select(Cohorte, NumContactos, NumLeads, NumClientes, all_of(cols_matriz))
      } else {
        dat <- dat %>% select(Cohorte, NumLeads, NumClientes, all_of(cols_matriz))
      }
      dat <- dat %>% arrange(desc(Cohorte))
      mes_actual <- lubridate::floor_date(Sys.Date(), "month")
      for (i in seq_len(nrow(dat))) {
        mes_cohorte <- as.Date(paste0(dat$Cohorte[[i]], "-01"))
        meses_disponibles <- .mes_conversion_rel(mes_cohorte, mes_actual)
        columnas_no_disponibles <- cols_matriz[as.integer(sub("^Mes ", "", cols_matriz)) > meses_disponibles]
        if (length(columnas_no_disponibles) > 0) dat[i, columnas_no_disponibles] <- 0
      }
      dat
    })
    .estilo_matriz <- function(columna) {
      force(columna)
      function(value, index) {
        dat <- matriz_cohorte_r()
        if (index > nrow(dat)) return(list())
        mes_rel <- as.integer(sub("^Mes ", "", columna))
        mes_cohorte <- as.Date(paste0(dat$Cohorte[[index]], "-01"))
        meses_disponibles <- .mes_conversion_rel(mes_cohorte, lubridate::floor_date(Sys.Date(), "month"))
        if (mes_rel > meses_disponibles) return(list(background = "#F3F4F6", color = "#CBD5E1"))
        valores <- as.numeric(dat[index, cols_matriz, drop = TRUE])
        valores <- valores[is.finite(valores)]
        valor <- suppressWarnings(as.numeric(value))
        if (!is.finite(valor) || length(valores) == 0) return(list())
        max_fila <- max(valores, na.rm = TRUE)
        if (!is.finite(max_fila) || max_fila <= 0) return(list())
        intensidad <- valor / max_fila
        list(
          background = paste0("rgba(28,57,142,", round(0.08 + intensidad * 0.72, 2), ")"),
          color = if (intensidad >= 0.6) "#FFFFFF" else "#404040",
          fontWeight = if (intensidad == 1) "bold" else "normal"
        )
      }
    }
    cols_fijas <- if (identical(tipo, "contacto_cliente")) {
      list(
        Cohorte = reactable::colDef(name = "Mes de Creación", minWidth = 120, sticky = "left"),
        NumContactos = reactable::colDef(name = "Contactos", minWidth = 90, sticky = "left"),
        NumLeads = reactable::colDef(name = "Leads", minWidth = 80),
        NumClientes = reactable::colDef(name = "Clientes", minWidth = 80)
      )
    } else {
      list(
        Cohorte = reactable::colDef(name = "Mes de Lead", minWidth = 120, sticky = "left"),
        NumLeads = reactable::colDef(name = "Leads", minWidth = 90, sticky = "left"),
        NumClientes = reactable::colDef(name = "Clientes", minWidth = 90)
      )
    }
    col_specs_matriz <- c(
      cols_fijas,
      stats::setNames(
        lapply(cols_matriz, function(columna) {
          local({
            col_actual <- columna
            reactable::colDef(
              name = col_actual, minWidth = 75, align = "center", html = TRUE,
              style = .estilo_matriz(col_actual),
              cell = function(value, index) {
                dat <- isolate(matriz_cohorte_r())
                if (index > nrow(dat)) return(value)
                cohorte <- dat$Cohorte[[index]]
                mes_rel <- as.integer(sub("^Mes ", "", col_actual))
                mes_cohorte <- as.Date(paste0(cohorte, "-01"))
                meses_disponibles <- .mes_conversion_rel(mes_cohorte, lubridate::floor_date(Sys.Date(), "month"))
                if (mes_rel > meses_disponibles) {
                  return(tags$div(style = "width:100%; text-align:center;", value))
                }
                js <- sprintf(
                  "Shiny.setInputValue('%s',{cohorte:%s,mes:%s},{priority:'event'});",
                  ns("celda_cohorte_click"),
                  jsonlite::toJSON(cohorte, auto_unbox = TRUE),
                  jsonlite::toJSON(col_actual, auto_unbox = TRUE)
                )
                tags$div(style = "width:100%;height:100%;cursor:pointer;text-align:center;", onclick = js, value)
              }
            )
          })
        }),
        cols_matriz
      )
    )
    TablaReactable2(id = "tabla_matriz", data = matriz_cohorte_r, columnas = NULL,
                    col_specs = col_specs_matriz, modo_seleccion = "ninguno", id_col = "Cohorte",
                    sortable = FALSE, searchable = FALSE, page_size = 24, compact = TRUE,
                    mostrar_badge = FALSE, mostrar_nota = FALSE)
    # Detalle de cohorte ----
    celda_cohorte <- reactive({
      req(input$celda_cohorte_click)
      mes_rel <- suppressWarnings(as.integer(sub("^Mes\\s+", "", input$celda_cohorte_click$mes)))
      req(!is.na(mes_rel))
      list(Cohorte = input$celda_cohorte_click$cohorte, MesRel = mes_rel)
    })
    detalle_cohorte_r <- reactive({
      sel <- celda_cohorte()
      codigos <- datos_conversion() %>%
        filter(Evento, CohorteAnalisis == sel$Cohorte) %>%
        mutate(MesConversionRel = .mes_conversion_rel(FechaInicioAnalisis, FechaEventoAnalisis)) %>%
        filter(MesConversionRel == sel$MesRel) %>%
        pull(CodContacto) %>%
        unique()
      req(length(codigos) > 0)
      etapa_actual <- derivar_etapa_actual() %>%
        filter(CodContacto %in% codigos) %>%
        select(CodContacto, EtapaActual = Etapa, EstadoActual = Estado) %>%
        distinct(CodContacto, .keep_all = TRUE)
      dat <- datos_conversion() %>%
        filter(Evento, CodContacto %in% codigos) %>%
        left_join(etapa_actual, by = "CodContacto") %>%
        distinct(CodContacto, .keep_all = TRUE) %>%
        mutate(Acciones = CodContacto)
      if (identical(tipo, "contacto_cliente")) {
        dat %>%
          select(Acciones, CodContacto, EtapaActual, EstadoActual, PerCod, PerRazSoc, AsesorLead, Origen,
                 DetOrigen, LinNegocio, Segmento, NitFacturacion, FechaConversionCliente, TiempoDiasContacto,
                 GestionesContactoCliente, GestionesCliente)
      } else {
        dat %>%
          select(Acciones, CodContacto, EtapaActual, EstadoActual, PerCod, PerRazSoc, AsesorLead, Origen,
                 DetOrigen, LinNegocio, Segmento, NitFacturacion, FechaConversionLead, FechaConversionCliente,
                 TiempoDiasLead, GestionesLeadCliente, GestionesCliente)
      }
    })
    observeEvent(detalle_cohorte_r(), {
      if (nrow(detalle_cohorte_r()) == 0) {
        shinyjs::hide(id = "BloqueDetalleCohorte")
      } else {
        shinyjs::show(id = "BloqueDetalleCohorte")
      }
    })
    columnas_detalle <- if (identical(tipo, "contacto_cliente")) {
      c("Acciones", "EtapaActual", "EstadoActual", "PerCod", "PerRazSoc", "AsesorLead", "Origen", "DetOrigen",
        "LinNegocio", "Segmento", "NitFacturacion", "FechaConversionCliente", "TiempoDiasContacto",
        "GestionesContactoCliente", "GestionesCliente")
    } else {
      c("Acciones", "EtapaActual", "EstadoActual", "PerCod", "PerRazSoc", "AsesorLead", "Origen", "DetOrigen",
        "LinNegocio", "Segmento", "NitFacturacion", "FechaConversionLead", "FechaConversionCliente",
        "TiempoDiasLead", "GestionesLeadCliente", "GestionesCliente")
    }
    col_specs_detalle <- list(
      Acciones = reactable::colDef(
        name = "", width = 45, minWidth = 45, maxWidth = 45, html = TRUE,
        sortable = FALSE, searchable = FALSE, align = "center",
        style = list(background = "#fff", border = "none", padding = "0"),
        headerStyle = list(background = "#fff", border = "none", padding = "0"),
        cell = function(value, index) {
          dat <- isolate(detalle_cohorte_r())
          if (index > nrow(dat)) return(NULL)
          etapa_actual <- dat$EtapaActual[[index]]
          acciones <- .acciones_por_etapa(etapa_actual)
          .celda_dropdown_acciones(value, acciones, ns)
        }
      ),
      EstadoActual = reactable::colDef(name = "Estado Actual", minWidth = 110),
      EtapaActual = reactable::colDef(name = "Etapa Actual", minWidth = 110, html = TRUE,
                                      cell = .badge_etapa_cliente),
      PerCod = reactable::colDef(name = "NIT", minWidth = 90),
      PerRazSoc = reactable::colDef(name = "Razón Social", minWidth = 190),
      AsesorLead = reactable::colDef(name = "Asesor", minWidth = 110),
      Origen = reactable::colDef(name = "Origen", minWidth = 110),
      DetOrigen = reactable::colDef(name = "Detalle Origen", minWidth = 130),
      LinNegocio = reactable::colDef(name = "Línea de Negocio", minWidth = 130),
      Segmento = reactable::colDef(name = "Segmento", minWidth = 110),
      NitFacturacion = reactable::colDef(name = "NIT Facturación", minWidth = 130),
      FechaConversionLead = reactable::colDef(name = "Fecha Lead", minWidth = 130),
      FechaConversionCliente = reactable::colDef(name = "Fecha Cliente", minWidth = 130),
      TiempoDiasContacto = reactable::colDef(name = "Contacto → Cliente (días)", minWidth = 165),
      TiempoDiasLead = reactable::colDef(name = "Lead → Cliente (días)", minWidth = 155),
      GestionesContactoCliente = reactable::colDef(name = "Gestiones Contacto → Cliente", minWidth = 175),
      GestionesLeadCliente = reactable::colDef(name = "Gestiones Lead → Cliente", minWidth = 165),
      GestionesCliente = reactable::colDef(name = "Gestiones como Cliente", minWidth = 150)
    )
    TablaReactable2(id = "tabla_detalle", data = detalle_cohorte_r, columnas = columnas_detalle,
                    col_specs = col_specs_detalle, modo_seleccion = "ninguno", id_col = "CodContacto",
                    sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
                    mostrar_badge = FALSE, mostrar_nota = FALSE,
                    cols_heatmap = if (identical(tipo, "contacto_cliente")) "TiempoDiasContacto" else "TiempoDiasLead")
    # Acciones ----
    .REGISTRO_MODULOS_DETALLE <- list(
      Editar = list(ui = function() EditarUI(ns("EditarDetalle"))),
      Relacionamiento = list(ui = function() RelacionamientoUI(ns("RelacionamientoDetalle"))),
      Promover = list(ui = function() PromoverUI(ns("PromoverDetalle"))),
      CrearOportunidad = list(ui = function() CrearOportunidadUI(ns("CrearOportunidadDetalle"))),
      Descartar = list(ui = function() DescartarUI(ns("DescartarDetalle")))
    )
    observeEvent(input$AccionSeleccionada, {
      seleccion <- input$AccionSeleccionada
      req(seleccion$codigo, seleccion$accion)
      etapa_actual <- detalle_cohorte_r() %>%
        filter(CodContacto == seleccion$codigo) %>%
        pull(EtapaActual)
      req(length(etapa_actual) == 1)
      acciones_permitidas <- .acciones_por_etapa(etapa_actual)
      req(seleccion$accion %in% names(acciones_permitidas))
      if (identical(seleccion$accion, "Reactivar")) {
        codigo_reactivar(seleccion$codigo)
        trigger_reactivar(isolate(trigger_reactivar()) + 1)
        return(invisible(NULL))
      }
      codigo_seleccionado(seleccion$codigo)
      accion_seleccionada(seleccion$accion)
      titulo_modal_actual(acciones_permitidas[[seleccion$accion]] %||% seleccion$accion)
      clase_modal <- .CONFIG_ACCIONES_EMBUDO[[seleccion$accion]]$modal %||% "subventana2"
      modal_construido <- modalDialog(
        title = titulo_modal_actual(),
        tagList(
          shinyjs::hidden(
            tags$div(id = ns("PreloaderModalDetalle"),
                     style = paste0("background:", preloader_actualizar$color, ";text-align:center;padding:30px;"),
                     preloader_actualizar$html)
          ),
          uiOutput(ns("ModalContenidoDetalle"))
        ),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      )
      showModal(htmltools::tagAppendAttributes(modal_construido, class = clase_modal))
    })
    output$ModalContenidoDetalle <- renderUI({
      req(accion_seleccionada())
      modulo <- .REGISTRO_MODULOS_DETALLE[[accion_seleccionada()]]
      req(modulo)
      modulo$ui()
    })
    outputOptions(output, "ModalContenidoDetalle", suspendWhenHidden = FALSE)
    observeEvent(input$AccionSeleccionada, {
      if (!identical(input$AccionSeleccionada$accion, "Reactivar"))
        shinyjs::show(id = "PreloaderModalDetalle", anim = FALSE)
    }, priority = 10)
    observe({
      req(accion_seleccionada())
      shinyjs::hide(id = "PreloaderModalDetalle", anim = FALSE)
    })
    Editar("EditarDetalle", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    Relacionamiento("RelacionamientoDetalle", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    Promover("PromoverDetalle", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    CrearOportunidad("CrearOportunidadDetalle", usuario = usuario,
                     codigo_contacto = reactive(codigo_seleccionado()))
    Descartar("DescartarDetalle", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    Reactivar("ReactivarDetalle", usuario = usuario, codigo_contacto = reactive(codigo_reactivar()),
              disparador = reactive(trigger_reactivar()))
    invisible(NULL)
  })
}

# DetalleAnalisisConversionCliente ----
DetalleAnalisisConversionClienteUI <- function(id, tipo) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "display:flex; gap:8px; flex-wrap:wrap;",
      div(style = "width:220px;", uiOutput(ns("Origen_ui"))),
      div(style = "width:220px;", uiOutput(ns("DetOrigen_ui")))
    ),
    br(),
    DetalleConversionClienteUI(ns("conversion"), tipo = tipo)
  )
}
DetalleAnalisisConversionCliente <- function(id, universo_contactos, usuario, tipo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Filtros ----
    output$Origen_ui <- renderUI({
      origenes <- universo_contactos()$Origen
      origenes <- origenes[!is.na(origenes)] %>% unique() %>% sort()
      ListaDesplegable(ns("Origen"), label = h6("Filtrar Origen"),
                       choices = c("Todos", origenes), selected = "Todos", multiple = FALSE, size = 8)
    })
    output$DetOrigen_ui <- renderUI({
      req(input$Origen)
      det <- if (identical(input$Origen, "Todos")) {
        universo_contactos()$DetOrigen
      } else {
        universo_contactos() %>% filter(Origen == input$Origen) %>% pull(DetOrigen)
      }
      det <- det[!is.na(det)] %>% unique() %>% sort()
      ListaDesplegable(ns("DetOrigen"), label = h6("Filtrar Detalle Origen"),
                       choices = c("Todos", det), selected = "Todos", multiple = FALSE, size = 8)
    })
    datos_filtrados_r <- reactive({
      dat <- universo_contactos()
      if (!is.null(input$Origen) && !identical(input$Origen, "Todos"))
        dat <- dat %>% filter(Origen == input$Origen)
      if (!is.null(input$DetOrigen) && !identical(input$DetOrigen, "Todos"))
        dat <- dat %>% filter(DetOrigen == input$DetOrigen)
      dat
    })
    datos_conversion_r <- reactive({
      dat <- datos_filtrados_r()
      if (identical(tipo, "contacto_cliente")) {
        dat %>%
          mutate(
            FechaInicioAnalisis = FechaHoraCrea,
            FechaEventoAnalisis = FechaConversionCliente,
            Evento = EsCliente,
            TiempoConversionAnalisis = TiempoDiasContacto,
            CohorteAnalisis = CohorteContacto
          )
      } else {
        dat %>%
          filter(EsLead) %>%
          mutate(
            FechaInicioAnalisis = FechaConversionLead,
            FechaEventoAnalisis = FechaConversionCliente,
            Evento = EsCliente,
            TiempoConversionAnalisis = TiempoDiasLead,
            CohorteAnalisis = CohorteLead
          )
      }
    })
    DetalleConversionCliente("conversion", datos_conversion = datos_conversion_r, usuario = usuario, tipo = tipo)
    invisible(NULL)
  })
}
# Modulo Principal ----
DetalleClientesUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(12,
             div(style = "display:flex;justify-content:flex-end;gap:8px;align-items:center;",
                 racafeShiny::BotonDescarga(
                   button_id = "descarga_clientes", icono = "file-excel", color_fondo = "#6c757d",
                   color_fuente = "#000", title = "Descargar", size = "xs", align = "right", ns = ns
                 )))
    ),
    br(),
    fluidRow(
      column(4,
             box(title = "Indicadores de Clientes", width = 12, collapsible = TRUE, collapsed = FALSE,
                 CajaModalUI(ns("kpi_total")),
                 br(),
                 CajaModalUI(ns("kpi_conversion_contacto")),
                 br(),
                 CajaModalUI(ns("kpi_conversion_lead")))),
      column(8,
             box(title = "Resumen por Dimensión", width = 12, collapsible = TRUE, collapsed = FALSE,
                 tabsetPanel(
                   tabPanel("Usuario", Saltos(), TablaReactable2UI(ns("tabla_usuario"))),
                   tabPanel("Origen", Saltos(), TablaReactable2UI(ns("tabla_origen")),
                            tags$hr(),
                            shinyjs::hidden(
                              tags$div(id = ns("BloqueDetOrigen"),
                                       FormatearTexto("Detalle Origen (según fila seleccionada arriba)",
                                                      tamano_pct = 0.8, color = "#64748B"),
                                       TablaReactable2UI(ns("tabla_det_origen")))
                            )),
                   tabPanel("Asesor", Saltos(), TablaReactable2UI(ns("tabla_asesor")),
                            FormatearTexto("Esta información solo está disponible desde la conversión a Lead.",
                                           tamano_pct = 0.75, color = "#64748B")),
                   tabPanel("Línea de Negocio", Saltos(), TablaReactable2UI(ns("tabla_linnegocio")),
                            FormatearTexto("Esta información solo está disponible desde la conversión a Lead.",
                                           tamano_pct = 0.75, color = "#64748B")),
                   tabPanel("Segmento", Saltos(), TablaReactable2UI(ns("tabla_segmento")),
                            FormatearTexto("Esta información solo está disponible desde la conversión a Lead.",
                                           tamano_pct = 0.75, color = "#64748B"))
                 ),
                 FormatearTexto(
                   paste0("Los valores corresponden al total de clientes cuya etapa vigente es CLIENTE ",
                          "y cuyo estado actual es ACTIVO."),
                   tamano_pct = 0.75, color = "#64748B"
                 )))
    ),
    box(title = "Detalle de Clientes", width = 12, collapsible = TRUE, collapsed = TRUE,
        PanelEtapaUI(ns("Listado")),
        FormatearTexto(
          "Se muestran únicamente los clientes cuya etapa vigente es CLIENTE y cuyo estado actual es ACTIVO.",
          tamano_pct = 0.75, color = "#64748B"
        ))
  )
}
DetalleClientes <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    # Datos ----
    codigos_clientes_activos_r <- reactive({
      derivar_etapa_actual() %>%
        filter(Etapa == "CLIENTE", Estado == "ACTIVO") %>%
        pull(CodContacto) %>%
        unique()
    })
    universo_contactos_r <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide(), add = TRUE)
      refresh_trigger()
      lead_data <- .obtener_conversion_lead()
      cliente_data <- .obtener_conversion_cliente()
      contactos <- CargarDatos("CRMNALCONTACTO") %>%
        mutate(FechaHoraCrea = as_datetime(FechaHoraCrea), Usuario = UsuarioCrea)
      rel <- tryCatch(
        CargarDatos("CRMNALRELACIONAMIENTO") %>%
          mutate(FechaHoraCrea = as_datetime(FechaHoraCrea)),
        error = function(e) {
          data.frame(CodContacto = character(), FechaHoraCrea = as.POSIXct(character()))
        }
      )
      base <- contactos %>%
        left_join(lead_data, by = "CodContacto") %>%
        left_join(cliente_data, by = "CodContacto") %>%
        mutate(
          FechaConversionLead = as_datetime(FechaConversionLead),
          FechaConversionCliente = as_datetime(FechaConversionCliente),
          EsLead = !is.na(FechaConversionLead),
          EsCliente = !is.na(FechaConversionCliente),
          EsClienteActivo = CodContacto %in% codigos_clientes_activos_r(),
          TiempoDiasContacto = round(
            as.numeric(difftime(FechaConversionCliente, FechaHoraCrea, units = "days")), 1
          ),
          TiempoDiasLead = round(
            as.numeric(difftime(FechaConversionCliente, FechaConversionLead, units = "days")), 1
          ),
          CohorteContacto = format(FechaHoraCrea, "%Y-%m"),
          CohorteLead = ifelse(EsLead, format(FechaConversionLead, "%Y-%m"), NA_character_)
        )
      gestiones <- rel %>%
        inner_join(
          base %>%
            filter(EsCliente) %>%
            select(CodContacto, FechaHoraCreaContacto = FechaHoraCrea, FechaConversionLead, FechaConversionCliente),
          by = "CodContacto"
        ) %>%
        group_by(CodContacto) %>%
        summarise(
          GestionesContactoCliente = sum(
            FechaHoraCrea >= FechaHoraCreaContacto & FechaHoraCrea < FechaConversionCliente, na.rm = TRUE
          ),
          GestionesLeadCliente = sum(
            !is.na(FechaConversionLead) & FechaHoraCrea >= FechaConversionLead &
              FechaHoraCrea < FechaConversionCliente,
            na.rm = TRUE
          ),
          GestionesCliente = sum(FechaHoraCrea >= FechaConversionCliente, na.rm = TRUE),
          .groups = "drop"
        )
      base %>%
        left_join(gestiones, by = "CodContacto") %>%
        mutate(
          GestionesContactoCliente = ifelse(EsCliente, coalesce(GestionesContactoCliente, 0L), NA_integer_),
          GestionesLeadCliente = ifelse(EsCliente & EsLead, coalesce(GestionesLeadCliente, 0L), NA_integer_),
          GestionesCliente = ifelse(EsCliente, coalesce(GestionesCliente, 0L), NA_integer_)
        )
    })
    clientes_activos_r <- reactive({
      universo_contactos_r() %>% filter(EsClienteActivo)
    })
    # KPIs ----
    CajaModal(
      "kpi_total",
      valor = reactive({
        html_valor(nrow(modulo_listado$datos()), formato = "coma", color = "#404040")
      }),
      texto = html_texto("Total de Clientes", color = "#404040"),
      icono = "user-group",
      colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = "Clientes cuya etapa vigente es CLIENTE y cuyo estado actual es ACTIVO."
    )
    tasa_conversion_contacto_r <- reactive({
      mean(universo_contactos_r()$EsCliente, na.rm = TRUE)
    })
    tasa_conversion_lead_r <- reactive({
      universo <- universo_contactos_r()
      total_leads <- sum(universo$EsLead, na.rm = TRUE)
      if (total_leads == 0) return(0)
      sum(universo$EsCliente & universo$EsLead, na.rm = TRUE) / total_leads
    })
    CajaModal(
      "kpi_conversion_contacto",
      valor = reactive({
        html_valor(tasa_conversion_contacto_r(), formato = "porcentaje", color = "#404040")
      }),
      texto = html_texto("Tasa de Conversión Contacto → Cliente", color = "#404040"),
      icono = "chart-line",
      colores = c(fondo = "white"),
      mostrar_boton = TRUE,
      titulo_modal = "Conversión Contacto → Cliente",
      icono_modal = "chart-line",
      contenido_modal = function() {
        DetalleAnalisisConversionClienteUI(ns("analisis_contacto_cliente"), tipo = "contacto_cliente")
      },
      footer = "Contactos convertidos históricamente a Cliente / total de Contactos creados."
    )
    CajaModal(
      "kpi_conversion_lead",
      valor = reactive({
        html_valor(tasa_conversion_lead_r(), formato = "porcentaje", color = "#404040")
      }),
      texto = html_texto("Tasa de Conversión Lead → Cliente", color = "#404040"),
      icono = "chart-line",
      colores = c(fondo = "white"),
      mostrar_boton = TRUE,
      titulo_modal = "Conversión Lead → Cliente",
      icono_modal = "chart-line",
      contenido_modal = function() {
        DetalleAnalisisConversionClienteUI(ns("analisis_lead_cliente"), tipo = "lead_cliente")
      },
      footer = "Leads convertidos históricamente a Cliente / total de contactos que alcanzaron la etapa Lead."
    )
    DetalleAnalisisConversionCliente("analisis_contacto_cliente", universo_contactos = universo_contactos_r,
                                     usuario = usuario, tipo = "contacto_cliente")
    DetalleAnalisisConversionCliente("analisis_lead_cliente", universo_contactos = universo_contactos_r,
                                     usuario = usuario, tipo = "lead_cliente")
    # Resumen por dimension ----
    datos_usuario_r <- reactive(.resumen_dimension_cliente(universo_contactos_r(), "Usuario"))
    .estilo_usuario <- .estilo_fila_total(datos_usuario_r, "Usuario")
    TablaReactable2(id = "tabla_usuario", data = datos_usuario_r, columnas = NULL,
                    col_specs = .col_specs_resumen_cliente("Usuario", "Usuario", .estilo_usuario),
                    modo_seleccion = "ninguno", id_col = "Usuario", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    datos_origen_r <- reactive(.resumen_dimension_cliente(universo_contactos_r(), "Origen"))
    .estilo_origen <- .estilo_fila_total(datos_origen_r, "Origen")
    modulo_tabla_origen <- TablaReactable2(
      id = "tabla_origen", data = datos_origen_r, columnas = NULL,
      col_specs = .col_specs_resumen_cliente("Origen", "Origen", .estilo_origen),
      modo_seleccion = "fila", id_col = "Origen", sortable = TRUE, searchable = TRUE,
      page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    origen_seleccionado <- reactive({
      sel <- modulo_tabla_origen$seleccion()
      if (is.null(sel)) return(NULL)
      sel$fila$Origen[[1]]
    })
    observeEvent(origen_seleccionado(), {
      if (is.null(origen_seleccionado()) || identical(origen_seleccionado(), "TOTAL")) {
        shinyjs::hide(id = "BloqueDetOrigen")
      } else {
        shinyjs::show(id = "BloqueDetOrigen")
      }
    }, ignoreNULL = FALSE)
    datos_det_origen_r <- reactive({
      req(origen_seleccionado())
      req(origen_seleccionado() != "TOTAL")
      base <- universo_contactos_r()
      if (identical(origen_seleccionado(), "SIN DATO")) {
        base <- base %>% filter(is.na(Origen) | Origen == "")
      } else {
        base <- base %>% filter(Origen == origen_seleccionado())
      }
      .resumen_dimension_cliente(base, "DetOrigen")
    })
    .estilo_det_origen <- .estilo_fila_total(datos_det_origen_r, "DetOrigen")
    TablaReactable2(id = "tabla_det_origen", data = datos_det_origen_r, columnas = NULL,
                    col_specs = .col_specs_resumen_cliente("DetOrigen", "Detalle Origen", .estilo_det_origen),
                    modo_seleccion = "ninguno", id_col = "DetOrigen", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    datos_asesor_r <- reactive(.resumen_dimension_cliente_convertido(universo_contactos_r(), "AsesorLead"))
    .estilo_asesor <- .estilo_fila_total(datos_asesor_r, "AsesorLead")
    TablaReactable2(id = "tabla_asesor", data = datos_asesor_r, columnas = NULL,
                    col_specs = .col_specs_resumen_cliente_convertido("AsesorLead", "Asesor", .estilo_asesor),
                    modo_seleccion = "ninguno", id_col = "AsesorLead", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    datos_linnegocio_r <- reactive(.resumen_dimension_cliente_convertido(universo_contactos_r(), "LinNegocio"))
    .estilo_linnegocio <- .estilo_fila_total(datos_linnegocio_r, "LinNegocio")
    TablaReactable2(
      id = "tabla_linnegocio", data = datos_linnegocio_r, columnas = NULL,
      col_specs = .col_specs_resumen_cliente_convertido("LinNegocio", "Línea de Negocio", .estilo_linnegocio),
      modo_seleccion = "ninguno", id_col = "LinNegocio", sortable = TRUE, searchable = TRUE,
      page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    datos_segmento_r <- reactive(.resumen_dimension_cliente_convertido(universo_contactos_r(), "Segmento"))
    .estilo_segmento <- .estilo_fila_total(datos_segmento_r, "Segmento")
    TablaReactable2(id = "tabla_segmento", data = datos_segmento_r, columnas = NULL,
                    col_specs = .col_specs_resumen_cliente_convertido("Segmento", "Segmento", .estilo_segmento),
                    modo_seleccion = "ninguno", id_col = "Segmento", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    # Listado ----
    modulo_listado <- PanelEtapa(id = "Listado", usuario = usuario, etapa = "CLIENTE", mostrar_titulo = FALSE)
    # Descarga ----
    output$descarga_clientes <- downloadHandler(
      filename = function() paste0("Clientes_CRM_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        openxlsx::write.xlsx(clientes_activos_r(), file, asTable = TRUE, overwrite = TRUE)
      }
    )
    invisible(NULL)
  })
}

# App de Prueba ----
ui <- bs4DashPage(
  title = "Prueba Detalle Clientes",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    use_waiter(),
    includeCSS(
      paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")
    ),
    box(title = "Detalle Clientes (prueba)", width = 12, DetalleClientesUI("AccionDetalleClientes"))
  )
)
server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  DetalleClientes("AccionDetalleClientes", usuario = usuario_sesion)
}
shinyApp(ui, server)