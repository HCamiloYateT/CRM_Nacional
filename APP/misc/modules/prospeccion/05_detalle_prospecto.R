# Modulos Auxiliares ----
## DetalleAlianza ----
DetalleAlianzaUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen")),
    plotly::plotlyOutput(ns("serie"), height = "300px")
  )
}
DetalleAlianza <- function(id, nit_aliado, fechas_alianza) {
  moduleServer(id, function(input, output, session) {
    
    ventas_mensuales_r <- reactive({
      req(nit_aliado())
      data %>%
        filter(CliNitPpal == nit_aliado()) %>%
        mutate(Mes = lubridate::floor_date(FecFact, "month")) %>%
        group_by(Mes) %>%
        summarise(SacosPYG = sum(SacosPYG, na.rm = TRUE), .groups = "drop")
    })
    
    cosecha_r <- reactive({
      req(nit_aliado(), fechas_alianza())
      ventas <- ventas_mensuales_r()
      req(nrow(ventas) > 0)
      
      purrr::map_dfr(fechas_alianza(), function(t0) {
        t0_mes <- lubridate::floor_date(t0, "month")
        ventas %>%
          mutate(
            RelMes = (lubridate::year(Mes) - lubridate::year(t0_mes)) * 12 +
              (lubridate::month(Mes) - lubridate::month(t0_mes))
          )
      }) %>%
        group_by(RelMes) %>%
        summarise(SacosPYGProm = mean(SacosPYG, na.rm = TRUE),
                  NumEventos   = n(), .groups = "drop") %>%
        arrange(RelMes)
    })
    
    output$resumen <- renderUI({
      dat <- cosecha_r()
      if (nrow(dat) == 0) {
        return(tags$p(style = "color:#94A3B8;",
                      "Sin ventas registradas para este cliente aliado."))
      }
      
      antes     <- dat %>% filter(RelMes < 0)  %>% summarise(v = mean(SacosPYGProm)) %>% pull(v)
      despues   <- dat %>% filter(RelMes >= 0) %>% summarise(v = mean(SacosPYGProm)) %>% pull(v)
      variacion <- if (length(antes) > 0 && !is.na(antes) && antes != 0) {
        round((despues - antes) / antes * 100, 1)
      } else NA
      
      color <- if (!is.na(variacion) && variacion > 0) "#198754" else "#C11007"
      
      tags$div(
        tags$h5(paste0("Promedio mensual antes de la alianza (cosecha): ",
                       round(antes %||% 0, 0), " sacos")),
        tags$h5(paste0("Promedio mensual después de la alianza (cosecha): ",
                       round(despues %||% 0, 0), " sacos")),
        tags$h5(style = paste0("color:", color, "; font-weight:700;"),
                paste0("Variación: ",
                       ifelse(is.na(variacion), "N/A", paste0(variacion, "%"))))
      )
    })
    
    output$serie <- plotly::renderPlotly({
      dat <- cosecha_r()
      if (nrow(dat) == 0) {
        return(plotly::config(
          plotly::plotly_empty(type = "scatter"),
          displayModeBar = FALSE
        ))
      }
      
      plotly::plot_ly(
        dat, x = ~RelMes, y = ~SacosPYGProm,
        type = "scatter", mode = "lines+markers",
        line = list(color = "#1C398E", width = 2),
        marker = list(color = "#1C398E", size = 7),
        text = ~paste0("Mes relativo ", RelMes, " · ", NumEventos, " evento(s)"),
        hovertemplate = "%{text}<br>Sacos PYG: %{y:,.0f}<extra></extra>"
      ) %>%
        plotly::layout(
          margin = list(l = 40, r = 20, t = 10, b = 40),
          xaxis = list(
            title = "Meses relativos a la alianza (0 = mes de vinculación)",
            zeroline = TRUE, zerolinecolor = "#C11007", zerolinewidth = 2
          ),
          yaxis = list(title = "Sacos (PYG)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          hoverlabel = list(
            bgcolor = "#1A3C5E",
            font = list(color = "white", size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    invisible(NULL)
  })
}

# Modulo Principal ----
DetalleProspectosUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(12,
             div(style = "display:flex; justify-content:flex-end; gap:8px; align-items:center;",
                 racafeShiny::BotonDescarga(button_id = "descarga_prospectos", icono = "file-excel",
                                            color_fondo = "#6c757d", color_fuente = "#000",
                                            title = "Descargar", size = "xs",
                                            align = "right", ns = ns)
             )
      )
    ),
    br(),
    fluidRow(
      column(4,
             box(title = "Indicadores de Prospectos", width = 12,
                 collapsible = TRUE, collapsed = FALSE,
                 CajaModalUI(ns("kpi_total")), br(),
                 CajaModalUI(ns("kpi_alianzas"))
             )
      ),
      column(8,
             box(title = "Resumen por Dimensión", width = 12,
                 collapsible = TRUE, collapsed = FALSE,
                 tabsetPanel(
                   tabPanel("Usuario",
                            Saltos(),
                            TablaReactable2UI(ns("tabla_usuario"))
                   ),
                   tabPanel("Origen",
                            Saltos(),
                            TablaReactable2UI(ns("tabla_origen")),
                            tags$hr(),
                            shinyjs::hidden(
                              tags$div(id = ns("BloqueDetOrigen"),
                                       FormatearTexto(
                                         "Detalle Origen (según fila seleccionada arriba)",
                                         tamano_pct = 0.8, color = "#64748B"
                                       ),
                                       TablaReactable2UI(ns("tabla_det_origen"))
                              )
                            )
                   ),
                   tabPanel("Alianza",
                            Saltos(),
                            TablaReactable2UI(ns("tabla_alianza"))
                   )
                 )
             )
      )
    ),
    box(title = "Detalle de Prospectos", width = 12,
        collapsible = TRUE, collapsed = TRUE,
        PanelEtapaUI(ns("Listado"))
    )
  )
}
DetalleProspectos <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    # Datos ----
    prospectos_base <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide(), add = TRUE)
      
      refresh_trigger()
      
      contactos <- CargarDatos("CRMNALCONTACTO") %>%
        select(
          CodContacto,
          UsuarioCreaContacto   = UsuarioCrea,
          FechaHoraCreaContacto = FechaHoraCrea
        ) %>%
        mutate(FechaHoraCreaContacto = as_datetime(FechaHoraCreaContacto))
      
      derivar_etapa_actual() %>%
        filter(Etapa == "PROSPECTO", Estado == "ACTIVO") %>%
        derivar_fecha_entrada_etapa() %>%
        left_join(contactos, by = "CodContacto") %>%
        mutate(
          FechaEntradaEtapa = as_datetime(FechaEntradaEtapa),
          TiempoConversion  = as.numeric(difftime(FechaEntradaEtapa, FechaHoraCreaContacto, units = "days")),
          DiasEnEtapa       = as.numeric(difftime(Sys.time(), FechaEntradaEtapa, units = "days"))
          )
    })
    
    # KPIs ----
    CajaModal("kpi_total",
              valor = reactive(
                html_valor(nrow(prospectos_base()), formato = "coma", color = "#404040")
              ),
              texto = html_texto("Total de Prospectos", color = "#404040"),
              icono = "user-group", colores = c(fondo = "white"),
              mostrar_boton = FALSE
    )
    
    # Alianzas ----
    alianzas_r <- reactive({
      listar_todas_las_alianzas() %>%
        filter(CodContacto %in% prospectos_base()$CodContacto)
    })
    
    resumen_alianzas_r <- reactive({
      alianzas_r() %>%
        count(ClienteAliado, name = "NumProspectos", sort = TRUE)
    })
    
    CajaModal("kpi_alianzas",
              valor = reactive(
                html_valor(
                  dplyr::n_distinct(alianzas_r()$ClienteAliado),
                  formato = "coma", color = "#404040"
                )
              ),
              texto = html_texto("Número de Alianzas", color = "#404040"),
              icono = "handshake", colores = c(fondo = "white"),
              mostrar_boton = TRUE,
              titulo_modal = "Prospectos por Alianza — click en una fila para ver análisis de ventas",
              icono_modal = "handshake",
              contenido_modal = function() tagList(
                TablaReactable2UI(ns("tabla_kpi_alianzas")),
                tags$hr(),
                uiOutput(ns("bloque_analisis_alianza"))
              )
    )
    
    mod_tabla_kpi_alianzas <- TablaReactable2(
      id = "tabla_kpi_alianzas", data = resumen_alianzas_r,
      columnas = NULL,
      col_specs = list(
        ClienteAliado = reactable::colDef(name = "Cliente Aliado", minWidth = 180),
        NumProspectos = reactable::colDef(name = "N° Prospectos", minWidth = 100)
      ),
      modo_seleccion = "fila", id_col = "ClienteAliado",
      sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
      mostrar_badge = FALSE, mostrar_nota = TRUE
    )
    
    alianza_nit_rv    <- reactiveVal(NULL)
    alianza_nombre_rv <- reactiveVal(NULL)
    fechas_alianza_rv <- reactiveVal(NULL)
    
    observeEvent(mod_tabla_kpi_alianzas$seleccion(), {
      sel <- mod_tabla_kpi_alianzas$seleccion()
      req(sel)
      
      cliente <- sel$fila$ClienteAliado[[1]]
      
      registros <- alianzas_r() %>%
        filter(ClienteAliado == cliente)
      
      req(nrow(registros) > 0)
      
      nit_aliado <- CargarDatos("CRMNALCONTACTO") %>%
        filter(CodContacto == registros$CodClienteAliado[[1]]) %>%
        pull(PerCod) %>%
        dplyr::first()
      
      alianza_nit_rv(nit_aliado)
      alianza_nombre_rv(cliente)
      fechas_alianza_rv(as.Date(registros$FechaHoraCrea))
    })
    
    output$bloque_analisis_alianza <- renderUI({
      req(alianza_nombre_rv())
      
      tagList(
        FormatearTexto(
          paste0("Análisis de Ventas (Cosecha) — ", alianza_nombre_rv()),
          tamano_pct = 1.1
        ),
        FormatearTexto(
          paste0(length(fechas_alianza_rv()),
                 " evento(s) de alianza vinculados a este cliente"),
          tamano_pct = 0.8, color = "#64748B"
        ),
        DetalleAlianzaUI(ns("mod_detalle_alianza"))
      )
    })
    
    DetalleAlianza(
      "mod_detalle_alianza",
      nit_aliado = reactive(alianza_nit_rv()),
      fechas_alianza = reactive(fechas_alianza_rv())
    )
    
    # Resumen por dimension ----
    datos_usuario_r <- reactive(
      .resumen_dimension_prospecto(
        prospectos_base(),
        "UsuarioCreaContacto"
      )
    )
    .estilo_total_usuario <- .estilo_fila_total(
      datos_usuario_r,
      "UsuarioCreaContacto"
    )
    
    TablaReactable2(
      id = "tabla_usuario", data = datos_usuario_r, columnas = NULL,
      col_specs = .col_specs_resumen_prospecto(
        "UsuarioCreaContacto",
        "Usuario (Creación)",
        .estilo_total_usuario
      ),
      modo_seleccion = "ninguno", id_col = "UsuarioCreaContacto",
      sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
      mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    datos_origen_r <- reactive(
      .resumen_dimension_prospecto(
        prospectos_base(),
        "Origen"
      )
    )
    .estilo_total_origen <- .estilo_fila_total(
      datos_origen_r,
      "Origen"
    )
    
    modulo_tabla_origen <- TablaReactable2(
      id = "tabla_origen", data = datos_origen_r, columnas = NULL,
      col_specs = .col_specs_resumen_prospecto(
        "Origen",
        "Origen",
        .estilo_total_origen
      ),
      modo_seleccion = "fila", id_col = "Origen",
      sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
      mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    # Origen seleccionado en la tabla
    origen_seleccionado <- reactive({
      sel <- modulo_tabla_origen$seleccion()
      if (is.null(sel)) return(NULL)
      sel$fila$Origen[[1]]
    })
    
    # Detalle Origen ----
    observeEvent(origen_seleccionado(), {
      if (is.null(origen_seleccionado()) ||
          identical(origen_seleccionado(), "TOTAL")) {
        shinyjs::hide(id = "BloqueDetOrigen")
      } else {
        shinyjs::show(id = "BloqueDetOrigen")
      }
    }, ignoreNULL = FALSE)
    
    datos_det_origen_r <- reactive({
      req(origen_seleccionado())
      req(origen_seleccionado() != "TOTAL")
      
      base <- prospectos_base() %>%
        filter(
          if (identical(origen_seleccionado(), "SIN DATO")) {
            is.na(Origen)
          } else {
            Origen == origen_seleccionado()
          }
        )
      
      .resumen_dimension_prospecto(base, "DetOrigen")
    })
    .estilo_total_det_origen <- .estilo_fila_total(
      datos_det_origen_r,
      "DetOrigen"
    )
    
    TablaReactable2(
      id = "tabla_det_origen", data = datos_det_origen_r,
      columnas = NULL,
      col_specs = .col_specs_resumen_prospecto(
        "DetOrigen",
        "Detalle Origen",
        .estilo_total_det_origen
      ),
      modo_seleccion = "ninguno", id_col = "DetOrigen",
      sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
      mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    datos_alianza_r <- reactive({
      alianzas_r() %>%
        left_join(prospectos_base() %>%
                    select(CodContacto, TiempoConversion, DiasEnEtapa),
                  by = "CodContacto") %>%
        .resumen_dimension_prospecto("ClienteAliado")
      })
    .estilo_total_alianza <- .estilo_fila_total(datos_alianza_r, "ClienteAliado")
    
    TablaReactable2(id = "tabla_alianza", data = datos_alianza_r, columnas = NULL,
                    col_specs = .col_specs_resumen_prospecto("ClienteAliado", "Alianza", .estilo_total_alianza),
                    modo_seleccion = "ninguno", id_col = "ClienteAliado",
                    sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
                    mostrar_badge = FALSE, mostrar_nota = FALSE
                    )
    
    # Listado ----
    PanelEtapa(id = "Listado", usuario = usuario, etapa = "PROSPECTO", mostrar_titulo = FALSE)
    
    # Descarga ----
    output$descarga_prospectos <- downloadHandler(
      filename = function() {
        paste0(
          "Prospectos_CRM_",
          format(Sys.Date(), "%Y%m%d"),
          ".xlsx"
        )
      },
      content = function(file) {
        openxlsx::write.xlsx(
          prospectos_base(),
          file,
          asTable = TRUE,
          overwrite = TRUE
        )
      }
    )
  })
}

# App de Prueba ----
ui <- bs4DashPage(
  title = "Prueba Detalle Prospectos",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    use_waiter(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                      "refs/heads/main/Styles/style.css")),
    box(title = "Detalle Prospectos (prueba)", width = 12,
        DetalleProspectosUI("AccionDetalleProspectos"))
  )
)
server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  DetalleProspectos("AccionDetalleProspectos", usuario = usuario_sesion)
}
shinyApp(ui, server)