# EmbudoConversion
# Define las columnas comunes de las tablas historicas por dimension.
.col_specs_dimension_embudo <- function(estilo, nombre_dimension) {
  list(
    Dim = reactable::colDef(
      name = nombre_dimension, minWidth = 140, style = estilo
    ),
    Contactos = reactable::colDef(
      name = "Contactos", minWidth = 90, style = estilo
    ),
    Leads = reactable::colDef(
      name = "Leads", minWidth = 80, style = estilo
    ),
    Prospectos = reactable::colDef(
      name = "Prospectos", minWidth = 90, style = estilo
    ),
    Clientes = reactable::colDef(
      name = "Clientes", minWidth = 80, style = estilo
    ),
    Descartados = reactable::colDef(
      name = "Descartados", minWidth = 90, style = estilo
    ),
    TasaContactoCliente = reactable::colDef(
      name = "Contacto → Cliente", minWidth = 130,
      cell = function(v) paste0(round(v * 100, 1), "%"),
      style = estilo
    ),
    TasaContactoLead = reactable::colDef(
      name = "Contacto → Lead", minWidth = 120,
      cell = function(v) paste0(round(v * 100, 1), "%"),
      style = estilo
    ),
    TasaLeadCliente = reactable::colDef(
      name = "Lead → Cliente", minWidth = 110,
      cell = function(v) paste0(round(v * 100, 1), "%"),
      style = estilo
    )
  )
}

### Modulos Auxiliares ----
# Renderiza un grafico Plotly dentro del modal de detalle.
.GraficoDetalleEmbudoUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("grafico"), height = "420px")
}
.GraficoDetalleEmbudo <- function(id, plot_reactive) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- plotly::renderPlotly({
      plot_reactive()
    })
  })
}



### Modulo Principal ----
EmbudoConversionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = "Embudo de Conversión",
      width = 12, collapsible = FALSE,
      plotly::plotlyOutput(ns("grafico_embudo"), height = "340px")
    ),
    
    fluidRow(
      column(2, CajaModalUI(ns("kpi_total_contactos"))),
      column(2, CajaModalUI(ns("kpi_total_leads"))),
      column(2, CajaModalUI(ns("kpi_total_prospectos"))),
      column(3, CajaModalUI(ns("kpi_total_clientes"))),
      column(3, CajaModalUI(ns("kpi_total_descartados")))
    ),
    
    box(
      title = "Conversión Histórica",
      width = 12, collapsible = FALSE,
      fluidRow(
        column(3, CajaModalUI(ns("kpi_tasa_contacto_lead"))),
        column(3, CajaModalUI(ns("kpi_tasa_prospecto_a_lead"))),
        column(3, CajaModalUI(ns("kpi_tasa_lead_cliente"))),
        column(3, CajaModalUI(ns("kpi_mejor_canal")))
      ),
      tags$hr(),
      tabsetPanel(
        tabPanel(
          "Usuario",
          Saltos(),
          TablaReactable2UI(ns("tabla_usuario"))
        ),
        tabPanel(
          "Origen",
          Saltos(),
          TablaReactable2UI(ns("tabla_origen")),
          tags$hr(),
          shinyjs::hidden(
            tags$div(
              id = ns("BloqueDetOrigen"),
              FormatearTexto(
                "Detalle Origen",
                tamano_pct = 0.8,
                color = "#64748B"
              ),
              TablaReactable2UI(ns("tabla_det_origen"))
            )
          )
        ),
        tabPanel(
          "Línea de Negocio",
          Saltos(),
          TablaReactable2UI(ns("tabla_lin_negocio"))
        ),
        tabPanel(
          "Segmento",
          Saltos(),
          TablaReactable2UI(ns("tabla_segmento"))
        ),
        tabPanel(
          "Asesor",
          Saltos(),
          TablaReactable2UI(ns("tabla_asesor"))
        )
      ),
      tags$hr(),
      FormatearTexto(
        paste0(
          "Las cifras corresponden al histórico completo del embudo. ",
          "Un contacto se contabiliza como Lead, Prospecto, Cliente o ",
          "Descartado si alcanzó esa condición en algún momento."
        ),
        tamano_pct = 0.75,
        color = "#64748B"
      )
    ),
    
    box(
      title = "Velocidad del Ciclo",
      width = 12, collapsible = FALSE,
      fluidRow(
        column(4, CajaModalUI(ns("kpi_tiempo_contacto_lead"))),
        column(4, CajaModalUI(ns("kpi_tiempo_lead_cliente"))),
        column(4, CajaModalUI(ns("kpi_tiempo_ciclo_total")))
      )
    ),
    
    box(
      title = "Pérdida y Calidad",
      width = 12, collapsible = FALSE,
      fluidRow(
        column(3, CajaModalUI(ns("kpi_tasa_descarte_contacto"))),
        column(3, CajaModalUI(ns("kpi_tasa_descarte_prospecto"))),
        column(3, CajaModalUI(ns("kpi_tasa_descarte_lead"))),
        column(3, CajaModalUI(ns("kpi_motivo_top")))
      ),
      tags$hr(),
      plotly::plotlyOutput(ns("grafico_descartados"), height = "380px")
    ),
    
    box(
      title = "Actividad Reciente — 30 días",
      width = 12, collapsible = FALSE,
      fluidRow(
        column(4, CajaModalUI(ns("kpi_contactos_30d"))),
        column(4, CajaModalUI(ns("kpi_leads_30d"))),
        column(4, CajaModalUI(ns("kpi_clientes_30d")))
      )
    ),
    
    box(
      title = "Ubicación de Contactos por Departamento y Municipio",
      width = 12, collapsible = FALSE,
      fluidRow(
        column(
          5,
          reactable::reactableOutput(ns("tabla_geo"))
        ),
        column(
          7,
          leaflet::leafletOutput(ns("mapa_geo"), height = "420px")
        )
      ),
      tags$p(
        style = "font-size:11px; color:#888; margin-top:8px;",
        uiOutput(ns("nota_geo"))
      )
    )
  )
}
EmbudoConversion <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ### Datos ----
    # Calcula las metricas generales del embudo.
    metricas <- reactive({
      calcular_metricas_embudo()
    })
    
    # Calcula la georreferenciacion de los contactos.
    geo <- reactive({
      calcular_georeferenciacion()
    })
    
    # Construye el universo completo utilizado en las tablas historicas.
    universo_historico <- reactive({
      base_contactos <- CargarDatos("CRMNALCONTACTO")
      
      historial <- .cargar_embudo_seguro(
        "CRMNALHISTORIALETAPA",
        data.frame(
          CodContacto = character(),
          EtapaAnterior = character(),
          EtapaNueva = character(),
          FechaHora = as.POSIXct(character())
        )
      )
      
      leads <- .cargar_embudo_seguro(
        "CONTACTOLEAD",
        data.frame(
          CodContacto = character(),
          FechaConversion = as.POSIXct(character()),
          LinNegocio = character(),
          Segmento = character(),
          Asesor = character()
        )
      )
      
      .construir_universo_historico_embudo(
        base_contactos,
        historial,
        leads
      )
    })
    
    # Construye la estructura que alimenta el treemap de descartes.
    descartes_treemap_r <- reactive({
      contactos <- CargarDatos("CRMNALCONTACTO") %>%
        derivar_etapa_actual() %>%
        select(
          CodContacto,
          EtapaActual = Etapa,
          EtapaPreDescarte,
          Origen,
          DetOrigen
        )
      
      descartes <- CargarDatos("CRMNALDESCARTE") %>%
        mutate(
          FechaHoraModi = as_datetime(FechaHoraModi)
        ) %>%
        arrange(CodContacto, desc(FechaHoraModi)) %>%
        group_by(CodContacto) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        select(
          CodContacto,
          EtapaDescarte = Etapa,
          Razon1,
          FechaHoraModi
        )
      
      contactos %>%
        filter(EtapaActual == "DESCARTADO") %>%
        left_join(descartes, by = "CodContacto") %>%
        mutate(
          EtapaPreDescarte = coalesce(
            EtapaDescarte,
            EtapaPreDescarte,
            "CONTACTO"
          ),
          Origen = coalesce(Origen, "SIN DATO"),
          DetOrigen = coalesce(DetOrigen, "SIN DATO"),
          Razon1 = coalesce(Razon1, "SIN DATO")
        ) %>%
        count(
          EtapaPreDescarte,
          Origen,
          DetOrigen,
          Razon1,
          name = "Descartados"
        )
    })
    
    ### Graficos ----
    # Grafica el funnel principal de conversion.
    output$grafico_embudo <- plotly::renderPlotly({
      grafico_embudo(metricas())
    })
    
    # Mantiene el treemap probado de descartes por etapa y motivo.
    output$grafico_descartados <- plotly::renderPlotly({
      grafico_descartados(
        descartes_treemap_r()
      )
    })
    
    # Mantiene el detalle grafico de conversion por canal.
    .GraficoDetalleEmbudo(
      "grafico_canal",
      plot_reactive = reactive({
        treemap_canal(metricas()$conversion_por_canal)
      })
    )
    
    ### Tabla Usuario ----
    # Resume la conversion historica por usuario creador.
    datos_usuario_r <- reactive({
      .resumen_dimension_embudo(universo_historico(), "UsuarioCrea")
    })
    
    estilo_total_usuario <- .estilo_fila_total(
      datos_usuario_r, "Dim"
    )
    
    TablaReactable2(
      id = "tabla_usuario", data = datos_usuario_r, columnas = NULL,
      col_specs = .col_specs_dimension_embudo(
        estilo_total_usuario, "Usuario"
      ),
      modo_seleccion = "ninguno", id_col = "Dim",
      sortable = TRUE, searchable = TRUE, page_size = 10,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    ### Tabla Origen ----
    # Resume la conversion historica por origen.
    datos_origen_r <- reactive({
      .resumen_dimension_embudo(universo_historico(), "Origen")
    })
    
    estilo_total_origen <- .estilo_fila_total(
      datos_origen_r, "Dim"
    )
    
    modulo_tabla_origen <- TablaReactable2(
      id = "tabla_origen", data = datos_origen_r, columnas = NULL,
      col_specs = .col_specs_dimension_embudo(
        estilo_total_origen, "Origen"
      ),
      modo_seleccion = "fila", id_col = "Dim",
      sortable = TRUE, searchable = TRUE, page_size = 10,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    # Recupera el origen seleccionado.
    origen_seleccionado <- reactive({
      sel <- modulo_tabla_origen$seleccion()
      if (is.null(sel)) return(NULL)
      sel$fila$Dim[[1]]
    })
    
    # Muestra el detalle solo cuando existe un origen valido.
    observeEvent(origen_seleccionado(), {
      origen <- origen_seleccionado()
      if (is.null(origen) || identical(origen, "TOTAL")) {
        shinyjs::hide(id = "BloqueDetOrigen")
      } else {
        shinyjs::show(id = "BloqueDetOrigen")
      }
    }, ignoreNULL = FALSE)
    
    ### Tabla Detalle Origen ----
    # Resume el detalle del origen seleccionado.
    datos_det_origen_r <- reactive({
      req(origen_seleccionado())
      req(!identical(origen_seleccionado(), "TOTAL"))
      
      universo_historico() %>%
        filter(Origen == origen_seleccionado()) %>%
        .resumen_dimension_embudo("DetOrigen")
    })
    
    estilo_total_det_origen <- .estilo_fila_total(
      datos_det_origen_r, "Dim"
    )
    
    TablaReactable2(
      id = "tabla_det_origen", data = datos_det_origen_r,
      columnas = NULL,
      col_specs = .col_specs_dimension_embudo(
        estilo_total_det_origen, "Detalle Origen"
      ),
      modo_seleccion = "ninguno", id_col = "Dim",
      sortable = TRUE, searchable = TRUE, page_size = 10,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    ### Tabla Linea de Negocio ----
    # Resume la conversion historica por linea de negocio.
    datos_lin_negocio_r <- reactive({
      .resumen_dimension_embudo(universo_historico(), "LinNegocio")
    })
    
    estilo_total_lin_negocio <- .estilo_fila_total(
      datos_lin_negocio_r, "Dim"
    )
    
    TablaReactable2(
      id = "tabla_lin_negocio", data = datos_lin_negocio_r,
      columnas = NULL,
      col_specs = .col_specs_dimension_embudo(
        estilo_total_lin_negocio, "Línea de Negocio"
      ),
      modo_seleccion = "ninguno", id_col = "Dim",
      sortable = TRUE, searchable = TRUE, page_size = 10,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    ### Tabla Segmento ----
    # Resume la conversion historica por segmento.
    datos_segmento_r <- reactive({
      .resumen_dimension_embudo(universo_historico(), "Segmento")
    })
    
    estilo_total_segmento <- .estilo_fila_total(
      datos_segmento_r, "Dim"
    )
    
    TablaReactable2(
      id = "tabla_segmento", data = datos_segmento_r,
      columnas = NULL,
      col_specs = .col_specs_dimension_embudo(
        estilo_total_segmento, "Segmento"
      ),
      modo_seleccion = "ninguno", id_col = "Dim",
      sortable = TRUE, searchable = TRUE, page_size = 10,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    ### Tabla Asesor ----
    # Resume la conversion historica por asesor.
    datos_asesor_r <- reactive({
      .resumen_dimension_embudo(universo_historico(), "Asesor")
    })
    
    estilo_total_asesor <- .estilo_fila_total(
      datos_asesor_r, "Dim"
    )
    
    TablaReactable2(
      id = "tabla_asesor", data = datos_asesor_r,
      columnas = NULL,
      col_specs = .col_specs_dimension_embudo(
        estilo_total_asesor, "Asesor"
      ),
      modo_seleccion = "ninguno", id_col = "Dim",
      sortable = TRUE, searchable = TRUE, page_size = 10,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    ### Georreferenciacion ----
    # Construye la tabla de contactos por departamento y municipio.
    output$tabla_geo <- reactable::renderReactable({
      reactable::reactable(
        geo()$resumen_depto,
        sortable = TRUE,
        compact = TRUE,
        searchable = TRUE,
        columns = list(
          Depto = reactable::colDef(
            name = "Departamento"
          ),
          Mpio = reactable::colDef(
            name = "Municipio"
          ),
          Contactos = reactable::colDef(
            name = "Contactos"
          )
        )
      )
    })
    
    # Muestra en el mapa los contactos con coordenadas validas.
    output$mapa_geo <- leaflet::renderLeaflet({
      puntos <- geo()$puntos_mapa
      
      mapa <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron
        )
      
      if (nrow(puntos) == 0) {
        return(
          mapa %>%
            leaflet::setView(
              lng = -74.1,
              lat = 4.6,
              zoom = 5
            )
        )
      }
      
      mapa %>%
        leaflet::addMarkers(
          data = puntos,
          lng = ~lng,
          lat = ~lat,
          clusterOptions = leaflet::markerClusterOptions(),
          popup = ~paste0(
            "<b>",
            ifelse(is.na(PerRazSoc), PerCod, PerRazSoc),
            "</b><br>",
            Mpio, ", ", Depto, "<br>",
            ifelse(is.na(Direccion), "", Direccion)
          )
        ) %>%
        leaflet::fitBounds(
          lng1 = min(puntos$lng, na.rm = TRUE),
          lat1 = min(puntos$lat, na.rm = TRUE),
          lng2 = max(puntos$lng, na.rm = TRUE),
          lat2 = max(puntos$lat, na.rm = TRUE)
        )
    })
    
    # Informa la cobertura de coordenadas disponibles.
    output$nota_geo <- renderUI({
      paste0(
        geo()$total_con_coordenadas,
        " de ",
        geo()$total_con_ubicacion,
        " contactos con ubicación tienen coordenadas validadas (lat/lng)."
      )
    })
    
    ### Volumen ----
    # Muestra el numero vigente de contactos.
    CajaModal(
      "kpi_total_contactos",
      valor = reactive(metricas()$total_contactos),
      texto = "Contactos", icono = "address-book",
      mostrar_boton = FALSE,
      footer = "Contactos cuya etapa vigente es CONTACTO."
    )
    
    # Muestra el numero vigente de leads.
    CajaModal(
      "kpi_total_leads",
      valor = reactive(metricas()$total_leads),
      texto = "Leads", icono = "bullseye",
      mostrar_boton = FALSE,
      footer = "Contactos cuya etapa vigente es LEAD."
    )
    
    # Muestra el numero vigente de prospectos.
    CajaModal(
      "kpi_total_prospectos",
      valor = reactive(metricas()$total_prospectos),
      texto = "Prospectos", icono = "handshake",
      mostrar_boton = FALSE,
      footer = "Contactos cuya etapa vigente es PROSPECTO."
    )
    
    # Muestra el numero vigente de clientes.
    CajaModal(
      "kpi_total_clientes",
      valor = reactive(metricas()$total_clientes),
      texto = "Clientes", icono = "user-check",
      mostrar_boton = FALSE,
      footer = "Contactos cuya etapa vigente es CLIENTE."
    )
    
    # Muestra el numero vigente de descartados.
    CajaModal(
      "kpi_total_descartados",
      valor = reactive(metricas()$total_descartados),
      texto = "Descartados", icono = "ban",
      mostrar_boton = FALSE,
      footer = "Contactos cuya etapa vigente es DESCARTADO."
    )
    
    ### Conversion ----
    # Muestra la conversion historica de Contacto a Lead.
    CajaModal(
      "kpi_tasa_contacto_lead",
      valor = reactive(metricas()$tasa_contacto_lead),
      formato = "porcentaje",
      texto = "Contacto → Lead",
      icono = "arrow-right",
      mostrar_boton = FALSE,
      footer = "Conversión histórica de contactos que alcanzaron Lead."
    )
    
    # Muestra la conversion historica de Prospecto a Lead.
    CajaModal(
      "kpi_tasa_prospecto_a_lead",
      valor = reactive(metricas()$tasa_prospecto_a_lead),
      formato = "porcentaje",
      texto = "Prospecto → Lead",
      icono = "arrow-right",
      mostrar_boton = FALSE,
      footer = "Proporción histórica de Prospectos promovidos a Lead."
    )
    
    # Muestra la conversion historica de Lead a Cliente.
    CajaModal(
      "kpi_tasa_lead_cliente",
      valor = reactive(metricas()$tasa_lead_cliente),
      formato = "porcentaje",
      texto = "Lead → Cliente",
      icono = "arrow-right",
      mostrar_boton = FALSE,
      footer = "Conversión histórica de Leads que alcanzaron Cliente."
    )
    
    # Identifica el canal con mayor tasa de conversion.
    CajaModal(
      "kpi_mejor_canal",
      valor = reactive({
        dat <- metricas()$conversion_por_canal %>%
          group_by(Origen) %>%
          summarise(
            Contactos = sum(Contactos),
            Clientes = sum(Clientes),
            .groups = "drop"
          ) %>%
          mutate(
            TasaConversion = ifelse(
              Contactos > 0,
              Clientes / Contactos,
              0
            )
          ) %>%
          arrange(desc(TasaConversion), desc(Clientes))
        
        if (nrow(dat) == 0) "N/A" else dat$Origen[[1]]
      }),
      texto = "Mejor Canal (Origen)",
      icono = "trophy",
      mostrar_boton = TRUE,
      titulo_modal = "Conversión por Origen y Detalle de Origen",
      icono_modal = "trophy",
      tamano_modal = "l",
      contenido_modal = function() {
        .GraficoDetalleEmbudoUI(ns("grafico_canal"))
      },
      footer = paste0(
        "Origen con mayor conversión histórica Contacto → Cliente. ",
        "El detalle desagrega cada canal por Detalle Origen."
      )
    )
    
    ### Velocidad ----
    # Muestra los dias promedio desde la creacion hasta Lead.
    CajaModal(
      "kpi_tiempo_contacto_lead",
      valor = reactive(metricas()$tiempo_contacto_lead),
      formato = "numero",
      texto = "Días prom. hasta Lead",
      icono = "hourglass-half",
      mostrar_boton = FALSE,
      footer = "Promedio histórico de días entre creación y llegada a Lead."
    )
    
    # Muestra los dias promedio entre Lead y Cliente.
    CajaModal(
      "kpi_tiempo_lead_cliente",
      valor = reactive(metricas()$tiempo_lead_cliente),
      formato = "numero",
      texto = "Días prom. Lead → Cliente",
      icono = "hourglass-half",
      mostrar_boton = FALSE,
      footer = "Promedio histórico de días entre Lead y Cliente."
    )
    
    # Muestra la duracion promedio del ciclo completo.
    CajaModal(
      "kpi_tiempo_ciclo_total",
      valor = reactive(metricas()$tiempo_ciclo_total),
      formato = "numero",
      texto = "Días prom. Ciclo Completo",
      icono = "stopwatch",
      mostrar_boton = FALSE,
      footer = "Promedio histórico de días desde creación hasta Cliente."
    )
    
    ### Perdida y Calidad ----
    # Muestra la tasa historica de descarte desde Contacto.
    CajaModal(
      "kpi_tasa_descarte_contacto",
      valor = reactive(metricas()$tasa_descarte_contacto),
      formato = "porcentaje",
      texto = "Descarte en Contacto",
      icono = "circle-xmark",
      mostrar_boton = FALSE,
      footer = "Tasa histórica de descarte desde Contacto."
    )
    
    # Muestra la tasa historica de descarte desde Lead.
    CajaModal(
      "kpi_tasa_descarte_lead",
      valor = reactive(metricas()$tasa_descarte_lead),
      formato = "porcentaje",
      texto = "Descarte en Lead",
      icono = "circle-xmark",
      mostrar_boton = FALSE,
      footer = "Tasa histórica de descarte desde Lead."
    )
    
    # Muestra la tasa historica de descarte desde Prospecto.
    CajaModal(
      "kpi_tasa_descarte_prospecto",
      valor = reactive(metricas()$tasa_descarte_prospecto),
      formato = "porcentaje",
      texto = "Descarte en Prospecto",
      icono = "circle-xmark",
      mostrar_boton = FALSE,
      footer = "Tasa histórica de descarte desde Prospecto."
    )
    
    # Muestra el principal motivo de descarte sin abrir un detalle.
    CajaModal(
      "kpi_motivo_top",
      valor = reactive({
        dat <- metricas()$motivos_descarte
        if (nrow(dat) == 0) "N/A" else dat$CategoriaMotivo[[1]]
      }),
      texto = "Motivo Principal de Descarte",
      icono = "list-check",
      mostrar_boton = FALSE,
      footer = paste0(
        "Razón principal de descarte más frecuente. El análisis detallado ",
        "se consulta en el módulo DetalleDescartados."
      )
    )
    
    ### Actividad Reciente ----
    # Muestra los contactos creados durante los ultimos 30 dias.
    CajaModal(
      "kpi_contactos_30d",
      valor = reactive(metricas()$contactos_30d),
      texto = "Contactos (últimos 30d)",
      icono = "calendar-plus",
      mostrar_boton = FALSE,
      footer = "Contactos creados durante los últimos 30 días."
    )
    
    # Muestra las transiciones a Lead durante los ultimos 30 dias.
    CajaModal(
      "kpi_leads_30d",
      valor = reactive(metricas()$leads_30d),
      texto = "Leads (últimos 30d)",
      icono = "calendar-plus",
      mostrar_boton = FALSE,
      footer = "Transiciones a Lead registradas durante los últimos 30 días."
    )
    
    # Muestra las conversiones a Cliente durante los ultimos 30 dias.
    CajaModal(
      "kpi_clientes_30d",
      valor = reactive(metricas()$clientes_30d),
      texto = "Clientes (últimos 30d)",
      icono = "calendar-plus",
      mostrar_boton = FALSE,
      footer = "Conversiones a Cliente registradas durante los últimos 30 días."
    )
    
    invisible(NULL)
  })
}

### App de Prueba ----
# Ejecuta el modulo utilizando los datos reales del CRM.
ui <- bs4DashPage(
  title = "Prueba Embudo de Conversión",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(
      paste0(
        "https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
        "refs/heads/main/Styles/style.css"
      )
    ),
    EmbudoConversionUI("EmbudoConversion")
  )
)

server <- function(input, output, session) {
  EmbudoConversion("EmbudoConversion")
}

shinyApp(ui = ui, server = server)