# Modulos Auxiliares ----
## DetalleCurvaConversion ----
DetalleCurvaConversionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "display:flex; gap:8px; flex-wrap:wrap; align-items:flex-end;",
      div(style = "width:220px;",
          ListaDesplegable(ns("agrupador"), label = h6("Desglosar por"),
                           choices = c("Ninguno (solo Total)" = "ninguno", "Origen" = "Origen",
                                       "Detalle Origen" = "DetOrigen", "Cohorte de Creación" = "Cohorte"),
                           selected = "ninguno", multiple = FALSE, size = 6)),
      div(style = "width:220px;",
          ListaDesplegable(ns("unidad_tiempo"), label = h6("Unidad de Tiempo"),
                           choices = c("Días" = "dias", "Semanas" = "semanas", "Meses" = "meses"),
                           selected = "dias", multiple = FALSE, size = 5))
    ),
    br(),
    fluidRow(
      column(9, plotly::plotlyOutput(ns("curva_km"), height = "450px")),
      column(3,
             CajaModalUI(ns("kpi_analizados")), br(),
             CajaModalUI(ns("kpi_convertidos")), br(),
             CajaModalUI(ns("kpi_censurados")))
    )
  )
}
DetalleCurvaConversion <- function(id, datos_filtrados) {
  moduleServer(id, function(input, output, session) {
    # Datos ----
    datos_supervivencia_r <- reactive({
      dat <- datos_filtrados()
      req(nrow(dat) > 0)
      dat %>%
        mutate(
          TiempoDias = ifelse(EsLead, TiempoConversionDias,
                              as.numeric(difftime(Sys.time(), FechaHoraCrea, units = "days"))),
          Evento = as.numeric(EsLead)
        ) %>%
        filter(TiempoDias >= 0)
    })
    # Curva ----
    .curva_km <- function(sub, unidad) {
      if (nrow(sub) < 2 || sum(sub$Evento) == 0 || !requireNamespace("survival", quietly = TRUE)) return(NULL)
      factor_t <- .factor_tiempo_lead(unidad)
      max_periodo <- ceiling(max(sub$TiempoDias, na.rm = TRUE) / factor_t)
      periodos <- seq.int(0, max_periodo)
      tiempos <- periodos * factor_t
      fit <- survival::survfit(survival::Surv(TiempoDias, Evento) ~ 1, data = sub)
      resumen <- summary(fit, times = tiempos, extend = TRUE)
      data.frame(
        Periodo = periodos,
        PeriodoTexto = paste0(.etiqueta_tiempo_lead(unidad), " ", periodos),
        TasaConversionAcum = round((1 - resumen$surv) * 100, 1),
        ContactosIniciales = nrow(sub),
        ContactosConvertidos = vapply(tiempos, function(t) {
          sum(sub$Evento == 1 & sub$TiempoDias <= t, na.rm = TRUE)
        }, numeric(1))
      )
    }
    fit_total_r <- reactive({
      dat <- datos_supervivencia_r()
      req(nrow(dat) > 0, sum(dat$Evento) > 0)
      survival::survfit(survival::Surv(TiempoDias, Evento) ~ 1, data = dat)
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
        if (identical(col_grupo, "Cohorte")) grupos <- tail(grupos, 12)
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
                                             "<br>Contactos iniciales: ", format(ContactosIniciales, big.mark = ","),
                                             "<br>Contactos convertidos: ",
                                             format(ContactosConvertidos, big.mark = ","),
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
                                         "<br>Contactos iniciales: ", format(ContactosIniciales, big.mark = ","),
                                         "<br>Contactos convertidos: ", format(ContactosConvertidos, big.mark = ","),
                                         "<br>Conversión acumulada: ", TasaConversionAcum, "%"),
                          hovertemplate = "%{text}<extra></extra>")
      max_periodo <- max(curva_total$Periodo, na.rm = TRUE)
      dtick_x <- switch(input$unidad_tiempo,
                        dias = max(1, ceiling(max_periodo / 12)),
                        semanas = max(1, ceiling(max_periodo / 12)),
                        meses = max(1, ceiling(max_periodo / 12)),
                        1)
      p %>%
        plotly::layout(
          margin = list(l = 40, r = 20, t = 10, b = 40),
          xaxis = list(title = .titulo_tiempo_lead(input$unidad_tiempo),
                       tickmode = "linear", tick0 = 0, dtick = dtick_x, tickformat = "d"),
          yaxis = list(title = "% Convertidos a Lead (acumulado)", range = c(0, 100)),
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
              texto = html_texto("Contactos analizados", color = "#404040"),
              colores = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer = paste0("Incluye todos los contactos del universo analizado, ",
                              "sin restringir por etapa o estado vigente."))
    CajaModal("kpi_convertidos",
              valor = reactive({
                tabla <- tabla_surv_r()
                if (is.null(tabla)) 0 else html_valor(tabla[["events"]], formato = "coma", color = "#198754")
              }),
              texto = html_texto("Convertidos", color = "#198754"),
              colores = c(fondo = "white"),
              mostrar_boton = FALSE,
              footer = paste0("Contactos que alcanzaron históricamente la etapa Lead, ",
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
              footer = paste0("Contactos sin conversión histórica registrada a Lead ",
                              "al momento del análisis."))
    invisible(NULL)
  })
}

## DetalleAnalisisConversion ----
DetalleAnalisisConversionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "display:flex; gap:8px; flex-wrap:wrap;",
      div(style = "width:220px;", uiOutput(ns("Origen_ui"))),
      div(style = "width:220px;", uiOutput(ns("DetOrigen_ui")))
    ),
    br(),
    fluidRow(
      column(6, box(title = "Tiempo hasta Conversión", width = 12, collapsible = FALSE,
                    plotly::plotlyOutput(ns("kpi_tiempo"), height = "260px"))),
      column(6, box(title = "Distribución del Tiempo de Conversión", width = 12, collapsible = FALSE,
                    plotly::plotlyOutput(ns("densidad_tiempo"), height = "220px"),
                    uiOutput(ns("nota_densidad"))))
    ),
    br(),
    box(title = "Conversión Contacto → Lead", width = 12, collapsible = FALSE,
        DetalleCurvaConversionUI(ns("mod_curva"))),
    br(),
    box(title = "Conversión por Cohorte de Creación", width = 12, collapsible = TRUE, collapsed = FALSE,
        TablaReactable2UI(ns("tabla_cohorte_conv")),
        FormatearTexto(
          paste0("La cohorte considera conversiones históricas a Lead. ",
                 "Los contactos convertidos pueden tener actualmente una etapa o estado diferente."),
          tamano_pct = 0.75, color = "#64748B"
        ),
        tags$hr(),
        FormatearTexto("Matriz Mes de Creación × Meses hasta Conversión", tamano_pct = 1, negrita = TRUE),
        div(style = "width:100%; overflow-x:auto;",
            TablaReactable2UI(ns("tabla_matriz_cohorte")),
            FormatearTexto(
              paste0("Cada celda representa contactos convertidos históricamente a Lead ",
                     "según su mes de creación y el mes relativo de conversión. ",
                     "El detalle muestra la etapa vigente de cada contacto."),
              tamano_pct = 0.75, color = "#64748B"
            )),
        tags$hr(),
        shinyjs::hidden(
          tags$div(id = ns("BloqueDetalleCohorte"),
                   FormatearTexto("Contactos de la celda seleccionada", tamano_pct = 0.9, color = "#64748B"),
                   TablaReactable2UI(ns("tabla_detalle_cohorte")))
        )),
    uiOutput(ns("ModalDetalle"))
  )
}
DetalleAnalisisConversion <- function(id, universo_contactos, usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    codigo_detalle_seleccionado <- reactiveVal(NULL)
    accion_detalle_seleccionada <- reactiveVal(NULL)
    titulo_modal_detalle <- reactiveVal(NULL)
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
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide(), add = TRUE)
      req(input$Origen, input$DetOrigen)
      dat <- universo_contactos()
      if (!identical(input$Origen, "Todos")) dat <- dat %>% filter(Origen == input$Origen)
      if (!identical(input$DetOrigen, "Todos")) dat <- dat %>% filter(DetOrigen == input$DetOrigen)
      dat
    })
    # Tiempo de conversion ----
    output$kpi_tiempo <- plotly::renderPlotly({
      dat <- datos_filtrados_r() %>%
        filter(EsLead) %>%
        mutate(RangoConversion = .rangos_antiguedad(TiempoConversionDias)) %>%
        count(RangoConversion, .drop = FALSE, name = "n")
      .grafico_barras_horizontal(dat, "RangoConversion", "n", color = "#1C398E", titulo_x = "Leads")
    })
    tiempos_conversion_r <- reactive({
      datos_filtrados_r() %>% filter(EsLead) %>% pull(TiempoConversionDias) %>% na.omit()
    })
    output$densidad_tiempo <- plotly::renderPlotly({
      x <- tiempos_conversion_r()
      if (length(x) < 3) {
        return(plotly::config(plotly::plotly_empty(type = "scatter"), displayModeBar = FALSE))
      }
      dens <- stats::density(x, from = 0)
      media <- mean(x, na.rm = TRUE)
      mediana <- stats::median(x, na.rm = TRUE)
      plotly::plot_ly(x = dens$x, y = dens$y, type = "scatter", mode = "lines", fill = "tozeroy",
                      line = list(color = "#1C398E"), fillcolor = "rgba(28,57,142,0.15)",
                      hovertemplate = "Día %{x:.0f}<extra></extra>") %>%
        plotly::layout(
          margin = list(l = 30, r = 20, t = 10, b = 40),
          xaxis = list(title = "Días hasta conversión"),
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
    DetalleCurvaConversion("mod_curva", datos_filtrados = datos_filtrados_r)
    # Cohortes ----
    datos_cohorte_conv_r <- reactive(.resumen_cohorte_lead(datos_filtrados_r()))
    .estilo_cohorte_conv <- .estilo_fila_total(datos_cohorte_conv_r, "Cohorte")
    TablaReactable2(
      id = "tabla_cohorte_conv", data = datos_cohorte_conv_r, columnas = NULL,
      col_specs = list(
        Cohorte = reactable::colDef(name = "Cohorte (Mes Creación)", minWidth = 130,
                                    style = .estilo_cohorte_conv),
        NumContactos = reactable::colDef(name = "Núm Contactos", minWidth = 100, style = .estilo_cohorte_conv),
        NumLeads = reactable::colDef(name = "Núm Leads", minWidth = 90, style = .estilo_cohorte_conv),
        PctLeads = reactable::colDef(name = "% Leads", minWidth = 80,
                                     cell = function(v) paste0(v, "%"), style = .estilo_cohorte_conv),
        TasaConversion = reactable::colDef(name = "Tasa de Conversión", minWidth = 120,
                                           cell = function(v) paste0(v, "%"), style = .estilo_cohorte_conv),
        TiempoConversionProm = reactable::colDef(name = "Conversión Contacto → Lead (días)", minWidth = 180,
                                                 style = .estilo_cohorte_conv),
        GestionesPreviasProm = reactable::colDef(name = "Gestiones Previas", minWidth = 120,
                                                 style = .estilo_cohorte_conv),
        GestionesLeadProm = reactable::colDef(name = "Gestiones como Lead", minWidth = 130,
                                              style = .estilo_cohorte_conv)
      ),
      modo_seleccion = "ninguno", id_col = "Cohorte", sortable = TRUE, searchable = FALSE,
      page_size = 12, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    # Matriz de conversion ----
    meses_conversion <- isolate(
      universo_contactos() %>%
        filter(EsLead) %>%
        mutate(MesConversionRel = .mes_conversion_rel(FechaHoraCrea, FechaConversion)) %>%
        pull(MesConversionRel) %>%
        unique() %>%
        sort()
    )
    cols_matriz <- paste0("Mes ", meses_conversion)
    matriz_cohorte_r <- reactive({
      base <- datos_filtrados_r() %>%
        mutate(MesConversionRel = ifelse(EsLead, .mes_conversion_rel(FechaHoraCrea, FechaConversion),
                                         NA_integer_))
      totales <- base %>%
        group_by(Cohorte) %>%
        summarise(NumContactos = n(), TotalConvertidos = sum(EsLead, na.rm = TRUE), .groups = "drop")
      conversiones <- base %>%
        filter(EsLead) %>%
        mutate(MesConversion = paste0("Mes ", MesConversionRel)) %>%
        count(Cohorte, MesConversion, name = "NumLeads") %>%
        tidyr::pivot_wider(names_from = MesConversion, values_from = NumLeads, values_fill = 0)
      dat <- totales %>% left_join(conversiones, by = "Cohorte")
      for (col in cols_matriz) {
        if (!col %in% names(dat)) dat[[col]] <- 0L
        dat[[col]] <- coalesce(dat[[col]], 0L)
      }
      dat <- dat %>%
        select(Cohorte, NumContactos, TotalConvertidos, all_of(cols_matriz)) %>%
        arrange(desc(Cohorte))
      mes_actual <- lubridate::floor_date(Sys.Date(), "month")
      for (i in seq_len(nrow(dat))) {
        mes_cohorte <- as.Date(paste0(dat$Cohorte[[i]], "-01"))
        meses_disponibles <- .mes_conversion_rel(mes_cohorte, mes_actual)
        columnas_no_disponibles <- cols_matriz[as.integer(sub("^Mes ", "", cols_matriz)) > meses_disponibles]
        if (length(columnas_no_disponibles) > 0) dat[i, columnas_no_disponibles] <- 0
      }
      dat
    })
    .estilo_matriz_cohorte <- function(columna) {
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
    col_specs_matriz <- c(
      list(
        Cohorte = reactable::colDef(name = "Mes de Creación", minWidth = 120, sticky = "left"),
        NumContactos = reactable::colDef(name = "Núm Contactos", minWidth = 105, sticky = "left"),
        TotalConvertidos = reactable::colDef(name = "Total Convertidos", minWidth = 120)
      ),
      stats::setNames(
        lapply(cols_matriz, function(columna) {
          local({
            col_actual <- columna
            reactable::colDef(
              name = col_actual, minWidth = 75, align = "center", html = TRUE,
              style = .estilo_matriz_cohorte(col_actual),
              cell = function(value, index) {
                dat <- isolate(matriz_cohorte_r())
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
    TablaReactable2(id = "tabla_matriz_cohorte", data = matriz_cohorte_r, columnas = NULL,
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
      codigos <- datos_filtrados_r() %>%
        filter(EsLead, Cohorte == sel$Cohorte) %>%
        mutate(MesConversionRel = .mes_conversion_rel(FechaHoraCrea, FechaConversion)) %>%
        filter(MesConversionRel == sel$MesRel) %>%
        pull(CodContacto)
      req(length(codigos) > 0)
      etapa_actual <- derivar_etapa_actual() %>%
        filter(CodContacto %in% codigos) %>%
        select(CodContacto, EtapaActual = Etapa) %>%
        distinct(CodContacto, .keep_all = TRUE)
      datos_filtrados_r() %>%
        filter(CodContacto %in% codigos) %>%
        left_join(etapa_actual, by = "CodContacto") %>%
        distinct(CodContacto, .keep_all = TRUE) %>%
        mutate(Acciones = CodContacto) %>%
        select(Acciones, CodContacto, EtapaActual, PerCod, PerRazSoc, Asesor, Origen, DetOrigen,
               LinNegocio, Segmento, FechaConversion, TiempoConversionDias, GestionesPrevias, GestionesLead)
    })
    observeEvent(detalle_cohorte_r(), {
      if (nrow(detalle_cohorte_r()) == 0) {
        shinyjs::hide(id = "BloqueDetalleCohorte")
      } else {
        shinyjs::show(id = "BloqueDetalleCohorte")
      }
    })
    .badge_etapa_actual <- function(value) {
      etapa_actual <- if (is.na(value) || value == "") "SIN ETAPA" else value
      colores <- c(CONTACTO = "#64748B", PROSPECTO = "#C8862A", LEAD = "#1C398E",
                   CLIENTE = "#198754", DESCARTADO = "#C11007")
      color <- colores[[etapa_actual]] %||% "#64748B"
      tags$span(
        style = paste0("display:inline-block;padding:2px 8px;border-radius:10px;font-size:0.72rem;",
                       "font-weight:600;white-space:nowrap;color:#FFFFFF;background:", color, ";"),
        etapa_actual
      )
    }
    TablaReactable2(
      id = "tabla_detalle_cohorte", data = detalle_cohorte_r,
      columnas = c("Acciones", "EtapaActual", "PerCod", "PerRazSoc", "Asesor", "Origen", "DetOrigen",
                   "LinNegocio", "Segmento", "FechaConversion", "TiempoConversionDias",
                   "GestionesPrevias", "GestionesLead"),
      col_specs = list(
        Acciones = reactable::colDef(
          name = "", minWidth = 60, html = TRUE, sortable = FALSE, searchable = FALSE,
          cell = function(value, index) {
            dat <- isolate(detalle_cohorte_r())
            if (index > nrow(dat)) return(NULL)
            etapa_actual <- dat$EtapaActual[[index]]
            acciones <- .acciones_por_etapa(etapa_actual)
            .celda_dropdown_acciones(value, acciones, ns)
          }
        ),
        EtapaActual = reactable::colDef(name = "Etapa Actual", minWidth = 110, html = TRUE,
                                        cell = .badge_etapa_actual),
        PerCod = reactable::colDef(name = "NIT", minWidth = 90),
        PerRazSoc = reactable::colDef(name = "Razón Social", minWidth = 190),
        Asesor = reactable::colDef(name = "Asesor", minWidth = 110),
        Origen = reactable::colDef(name = "Origen", minWidth = 110),
        DetOrigen = reactable::colDef(name = "Detalle Origen", minWidth = 130),
        LinNegocio = reactable::colDef(name = "Línea de Negocio", minWidth = 130),
        Segmento = reactable::colDef(name = "Segmento", minWidth = 110),
        FechaConversion = reactable::colDef(name = "Fecha Conversión", minWidth = 130),
        TiempoConversionDias = reactable::colDef(name = "Conversión Contacto → Lead (días)", minWidth = 180),
        GestionesPrevias = reactable::colDef(name = "Gestiones Previas", minWidth = 120),
        GestionesLead = reactable::colDef(name = "Gestiones como Lead", minWidth = 130)
      ),
      modo_seleccion = "ninguno", id_col = "CodContacto", sortable = TRUE, searchable = TRUE,
      page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE,
      cols_heatmap = "TiempoConversionDias"
    )
    # Acciones ----
    .REGISTRO_MODULOS_DETALLE <- list(
      Editar = list(ui = function() EditarUI(ns("EditarDetalleCohorte"))),
      Relacionamiento = list(ui = function() RelacionamientoUI(ns("RelacionamientoDetalleCohorte"))),
      Promover = list(ui = function() PromoverUI(ns("PromoverDetalleCohorte"))),
      CrearOportunidad = list(ui = function() CrearOportunidadUI(ns("CrearOportunidadDetalleCohorte"))),
      Descartar = list(ui = function() DescartarUI(ns("DescartarDetalleCohorte")))
    )
    codigo_reactivar_detalle <- reactiveVal(NULL)
    trigger_reactivar_detalle <- reactiveVal(0)
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
        codigo_reactivar_detalle(seleccion$codigo)
        trigger_reactivar_detalle(isolate(trigger_reactivar_detalle()) + 1)
        return(invisible(NULL))
      }
      codigo_detalle_seleccionado(seleccion$codigo)
      accion_detalle_seleccionada(seleccion$accion)
      titulo_modal_detalle(acciones_permitidas[[seleccion$accion]] %||% seleccion$accion)
      clase_modal <- .CONFIG_ACCIONES_EMBUDO[[seleccion$accion]]$modal %||% "subventana2"
      modal_construido <- modalDialog(
        title = titulo_modal_detalle(),
        tagList(
          shinyjs::hidden(
            tags$div(id = ns("PreloaderModalDetalle"),
                     style = paste0("background:", preloader_actualizar$color,
                                    ";text-align:center;padding:30px;"),
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
      req(accion_detalle_seleccionada())
      modulo <- .REGISTRO_MODULOS_DETALLE[[accion_detalle_seleccionada()]]
      req(modulo)
      modulo$ui()
    })
    outputOptions(output, "ModalContenidoDetalle", suspendWhenHidden = FALSE)
    observeEvent(input$AccionSeleccionada, {
      if (!identical(input$AccionSeleccionada$accion, "Reactivar"))
        shinyjs::show(id = "PreloaderModalDetalle", anim = FALSE)
    }, priority = 10)
    observe({
      req(accion_detalle_seleccionada())
      shinyjs::hide(id = "PreloaderModalDetalle", anim = FALSE)
    })
    modulo_editar_detalle <- Editar("EditarDetalleCohorte", usuario = usuario,
                                    codigo_contacto = reactive(codigo_detalle_seleccionado()))
    modulo_relacion_detalle <- Relacionamiento("RelacionamientoDetalleCohorte", usuario = usuario,
                                               codigo_contacto = reactive(codigo_detalle_seleccionado()))
    modulo_promover_detalle <- Promover("PromoverDetalleCohorte", usuario = usuario,
                                        codigo_contacto = reactive(codigo_detalle_seleccionado()))
    modulo_oportunidad_detalle <- CrearOportunidad("CrearOportunidadDetalleCohorte", usuario = usuario,
                                                   codigo_contacto = reactive(codigo_detalle_seleccionado()))
    modulo_descartar_detalle <- Descartar("DescartarDetalleCohorte", usuario = usuario,
                                          codigo_contacto = reactive(codigo_detalle_seleccionado()))
    modulo_reactivar_detalle <- Reactivar("ReactivarDetalleCohorte", usuario = usuario,
                                          codigo_contacto = reactive(codigo_reactivar_detalle()),
                                          disparador = reactive(trigger_reactivar_detalle()))
    invisible(NULL)
  })
}
# Modulo Principal ----
DetalleLeadsUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(12,
             div(style = "display:flex; justify-content:flex-end; gap:8px; align-items:center;",
                 racafeShiny::BotonDescarga(button_id = "descarga_leads", icono = "file-excel",
                                            color_fondo = "#6c757d", color_fuente = "#000",
                                            title = "Descargar", size = "xs", align = "right", ns = ns)))
    ),
    br(),
    fluidRow(
      column(4,
             box(title = "Indicadores de Leads", width = 12, collapsible = TRUE, collapsed = FALSE,
                 CajaModalUI(ns("kpi_total")), br(),
                 CajaModalUI(ns("kpi_conversion")))),
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
                   paste0("Las tablas resumen el historial de conversión Contacto → Lead. ",
                          "Los Leads corresponden a contactos que alcanzaron esta etapa, ",
                          "independientemente de su etapa o estado vigente."),
                   tamano_pct = 0.75, color = "#64748B"
                 )))
    ),
    box(title = "Detalle de Leads", width = 12, collapsible = TRUE, collapsed = TRUE,
        PanelEtapaUI(ns("Listado")),
        FormatearTexto(
          paste0("Se muestran únicamente los contactos cuya etapa vigente es LEAD ",
                 "y cuyo estado actual es ACTIVO."),
          tamano_pct = 0.75, color = "#64748B"
        ))
  )
}
DetalleLeads <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    # Datos ----
    universo_contactos_r <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide(), add = TRUE)
      refresh_trigger()
      lead_data <- CargarDatos("CONTACTOLEAD") %>%
        select(CodContacto, Segmento, LinNegocio, Asesor, FechaConversion)
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
        mutate(
          FechaConversion = as_datetime(FechaConversion),
          EsLead = !is.na(FechaConversion),
          TiempoConversionDias = round(
            as.numeric(difftime(FechaConversion, FechaHoraCrea, units = "days")), 1
          ),
          Cohorte = format(FechaHoraCrea, "%Y-%m")
        )
      gestiones <- rel %>%
        inner_join(
          base %>%
            filter(EsLead) %>%
            select(CodContacto, FechaHoraCreaContacto = FechaHoraCrea, FechaConversion),
          by = "CodContacto"
        ) %>%
        group_by(CodContacto) %>%
        summarise(
          GestionesPrevias = sum(
            FechaHoraCrea >= FechaHoraCreaContacto & FechaHoraCrea < FechaConversion, na.rm = TRUE
          ),
          GestionesLead = sum(FechaHoraCrea >= FechaConversion, na.rm = TRUE),
          .groups = "drop"
        )
      base %>%
        left_join(gestiones, by = "CodContacto") %>%
        mutate(
          GestionesPrevias = ifelse(EsLead, coalesce(GestionesPrevias, 0L), NA_integer_),
          GestionesLead = ifelse(EsLead, coalesce(GestionesLead, 0L), NA_integer_)
        )
    })
    leads_activos_r <- reactive({
      cods_activos <- derivar_etapa_actual() %>%
        filter(Etapa == "LEAD", Estado == "ACTIVO") %>%
        pull(CodContacto)
      universo_contactos_r() %>% filter(CodContacto %in% cods_activos)
    })
    # KPIs ----
    CajaModal(
      "kpi_total",
      valor = reactive(html_valor(nrow(leads_activos_r()), formato = "coma", color = "#404040")),
      texto = html_texto("Total de Leads", color = "#404040"),
      icono = "user-group",
      colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = paste0("Contactos cuya etapa vigente es LEAD y cuyo estado actual es ACTIVO. ",
                      "No incluye contactos que fueron Lead históricamente y hoy están en otra etapa.")
    )
    tasa_conversion_r <- reactive(mean(universo_contactos_r()$EsLead, na.rm = TRUE))
    CajaModal(
      "kpi_conversion",
      valor = reactive(html_valor(tasa_conversion_r(), formato = "porcentaje", color = "#404040")),
      texto = html_texto("Tasa de Conversión Contacto → Lead", color = "#404040"),
      icono = "chart-line",
      colores = c(fondo = "white"),
      mostrar_boton = TRUE,
      titulo_modal = "Análisis de Conversión Contacto → Lead",
      icono_modal = "chart-line",
      contenido_modal = function() DetalleAnalisisConversionUI(ns("mod_analisis_conversion")),
      footer = paste0("Contactos convertidos históricamente a Lead / total de contactos creados. ",
                      "Incluye registros que actualmente pueden estar en otra etapa o estado.")
    )
    DetalleAnalisisConversion("mod_analisis_conversion", universo_contactos = universo_contactos_r,
                              usuario = usuario)
    # Resumen por dimension ----
    datos_usuario_r <- reactive(.resumen_dimension_lead(universo_contactos_r(), "Usuario"))
    .estilo_total_usuario <- .estilo_fila_total(datos_usuario_r, "Usuario")
    TablaReactable2(id = "tabla_usuario", data = datos_usuario_r, columnas = NULL,
                    col_specs = .col_specs_resumen_lead("Usuario", "Usuario", .estilo_total_usuario),
                    modo_seleccion = "ninguno", id_col = "Usuario", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    datos_origen_r <- reactive(.resumen_dimension_lead(universo_contactos_r(), "Origen"))
    .estilo_total_origen <- .estilo_fila_total(datos_origen_r, "Origen")
    modulo_tabla_origen <- TablaReactable2(
      id = "tabla_origen", data = datos_origen_r, columnas = NULL,
      col_specs = .col_specs_resumen_lead("Origen", "Origen", .estilo_total_origen),
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
      .resumen_dimension_lead(base, "DetOrigen")
    })
    .estilo_total_det_origen <- .estilo_fila_total(datos_det_origen_r, "DetOrigen")
    TablaReactable2(id = "tabla_det_origen", data = datos_det_origen_r, columnas = NULL,
                    col_specs = .col_specs_resumen_lead("DetOrigen", "Detalle Origen", .estilo_total_det_origen),
                    modo_seleccion = "ninguno", id_col = "DetOrigen", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    datos_asesor_r <- reactive(.resumen_dimension_lead_convertido(universo_contactos_r(), "Asesor"))
    .estilo_total_asesor <- .estilo_fila_total(datos_asesor_r, "Asesor")
    TablaReactable2(id = "tabla_asesor", data = datos_asesor_r, columnas = NULL,
                    col_specs = .col_specs_resumen_lead_convertido("Asesor", "Asesor", .estilo_total_asesor),
                    modo_seleccion = "ninguno", id_col = "Asesor", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    datos_linnegocio_r <- reactive(.resumen_dimension_lead_convertido(universo_contactos_r(), "LinNegocio"))
    .estilo_total_linnegocio <- .estilo_fila_total(datos_linnegocio_r, "LinNegocio")
    TablaReactable2(
      id = "tabla_linnegocio", data = datos_linnegocio_r, columnas = NULL,
      col_specs = .col_specs_resumen_lead_convertido("LinNegocio", "Línea de Negocio", .estilo_total_linnegocio),
      modo_seleccion = "ninguno", id_col = "LinNegocio", sortable = TRUE, searchable = TRUE,
      page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    datos_segmento_r <- reactive(.resumen_dimension_lead_convertido(universo_contactos_r(), "Segmento"))
    .estilo_total_segmento <- .estilo_fila_total(datos_segmento_r, "Segmento")
    TablaReactable2(id = "tabla_segmento", data = datos_segmento_r, columnas = NULL,
                    col_specs = .col_specs_resumen_lead_convertido("Segmento", "Segmento", .estilo_total_segmento),
                    modo_seleccion = "ninguno", id_col = "Segmento", sortable = TRUE, searchable = TRUE,
                    page_size = 10, compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE)
    # Listado ----
    PanelEtapa(id = "Listado", usuario = usuario, etapa = "LEAD", mostrar_titulo = FALSE)
    # Descarga ----
    output$descarga_leads <- downloadHandler(
      filename = function() paste0("Leads_CRM_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        openxlsx::write.xlsx(leads_activos_r(), file, asTable = TRUE, overwrite = TRUE)
      }
    )
  })
}

# App de Prueba ----
ui <- bs4DashPage(
  title = "Prueba Detalle Leads",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    use_waiter(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                      "refs/heads/main/Styles/style.css")),
    box(title = "Detalle Leads (prueba)", width = 12, DetalleLeadsUI("AccionDetalleLeads"))
  )
)
server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  DetalleLeads("AccionDetalleLeads", usuario = usuario_sesion)
}
shinyApp(ui, server)