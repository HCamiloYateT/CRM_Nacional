# Detalle Contacto
# Modulos Auxiliares ----
## DetalleCreacionMensual ----
DetalleCreacionMensualUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "display:flex; justify-content:space-between; align-items:flex-end; flex-wrap:wrap; gap:8px;",
      div(style = "display:flex; gap:8px; flex-wrap:wrap;",
          div(style = "width:220px;", uiOutput(ns("Origen_ui"))),
          div(style = "width:220px;", uiOutput(ns("DetOrigen_ui")))
      ),
      div(
        racafeShiny::BotonesRadiales(inputId = ns("modo_tiempo"),
                                     choices = list("Por Mes" = "mes", "Por Año" = "anio"),
                                     selected = "mes", alineacion = "left",
                                     color_activo = "#C11007", color_inactivo = "#FFF")
      )
    ),
    plotly::plotlyOutput(ns("serie"), height = "280px"),
    br(),
    TablaReactable2UI(ns("tabla"))
  )
}
DetalleCreacionMensual <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$Origen_ui <- renderUI({
      origenes <- datos()$Origen %>% unique() %>% sort()
      ListaDesplegable(ns("Origen"), label = h6("Origen"),
                       choices = c("Todos", origenes), selected = "Todos",
                       multiple = FALSE, size = 8)
    })
    output$DetOrigen_ui <- renderUI({
      req(input$Origen)
      det <- if (identical(input$Origen, "Todos")) {
        datos()$DetOrigen
      } else {
        datos() %>% filter(Origen == input$Origen) %>% pull(DetOrigen)
      }
      det <- det[!is.na(det)] %>% unique() %>% sort()
      ListaDesplegable(ns("DetOrigen"), label = h6("Detalle Origen"),
                       choices = c("Todos", det), selected = "Todos",
                       multiple = FALSE, size = 8)
    })
    
    datos_filtrados <- reactive({
      req(input$Origen, input$DetOrigen)
      dat <- datos()
      if (!identical(input$Origen, "Todos"))    dat <- dat %>% filter(Origen == input$Origen)
      if (!identical(input$DetOrigen, "Todos")) dat <- dat %>% filter(DetOrigen == input$DetOrigen)
      dat
    })
    
    serie_periodo <- reactive({
      req(input$modo_tiempo)
      fmt <- if (identical(input$modo_tiempo, "anio")) "%Y" else "%Y-%m"
      
      datos_filtrados() %>%
        mutate(Periodo = format(FechaHoraCrea, fmt)) %>%
        count(Periodo, name = "NumContactos") %>%
        mutate(Pct = round(NumContactos / sum(NumContactos) * 100, 1)) %>%
        arrange(Periodo)
    })
    
    output$serie <- plotly::renderPlotly({
      dat <- serie_periodo()
      if (nrow(dat) == 0) return(plotly::config(plotly::plotly_empty(type = "scatter"), displayModeBar = FALSE))
      dat <- dat %>%
        mutate(NumContactosFmt = format(NumContactos, big.mark = ",", scientific = FALSE))
      
      plotly::plot_ly(
        dat, x = ~Periodo, y = ~NumContactos, type = "scatter", mode = "lines+markers",
        line = list(color = "#1C398E", width = 2), marker = list(color = "#1C398E", size = 7),
        text = ~paste0(NumContactosFmt, " contactos (", Pct, "%)"),
        hovertemplate = paste0("<b>%{x}</b><br>%{text}<extra></extra>")
      ) %>%
        plotly::layout(
          margin = list(l = 40, r = 20, t = 10, b = 40),
          xaxis  = list(title = ""), yaxis = list(title = "Contactos"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          hoverlabel = list(bgcolor = "#1A3C5E", font = list(color = "white", size = 12))
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    datos_tabla_r <- reactive({
      base <- serie_periodo() %>% arrange(desc(Periodo))
      total <- tibble::tibble(
        Periodo      = "TOTAL",
        NumContactos = sum(base$NumContactos),
        Pct          = 100
      )
      bind_rows(base, total)
    })
    
    .estilo_total_periodo <- function(value, index) {
      if (datos_tabla_r()$Periodo[[index]] == "TOTAL") {
        list(fontWeight = "bold", background = "#f8f9fa")
      } else list()
    }
    
    TablaReactable2(id = "tabla", data = datos_tabla_r, columnas = NULL, 
                    col_specs = list(Periodo      = reactable::colDef(name = "Periodo", minWidth = 100, style = .estilo_total_periodo),
                                     NumContactos = reactable::colDef(name = "N° Contactos", minWidth = 100, style = .estilo_total_periodo),
                                     Pct          = reactable::colDef(name = "% del Total", minWidth = 90,
                                                                      cell = function(v) paste0(v, "%"), style = .estilo_total_periodo)
                                     ), modo_seleccion = "ninguno", id_col = "Periodo",
                    sortable = TRUE, searchable = FALSE, page_size = 12, compact = TRUE,
                    mostrar_badge = FALSE, mostrar_nota = FALSE
                    )
    invisible(NULL)
  })
}

# Modulo Principal ----

DetalleContactoUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(12,
             div(style = "display:flex; justify-content:flex-end; gap:8px; align-items:center;",
                 racafeShiny::Boton(id = ns("btn_nuevo_contacto"), label = "Añadir",
                                    icono = "user-plus", align = "right", size = "xs",
                                    color_fondo = "#C11007", color_fuente = "#FFFFFF"),
                 racafeShiny::BotonDescarga(button_id = "etapa_contacto", icono = "file-excel",
                                            color_fondo = "#6c757d", color_fuente = "#000",
                                            title = "Descargar", size = "xs",
                                            align = "right", ns = ns)
             )
      )
    ),
    br(),
    fluidRow(
      column(4,
             box(title = "Indicadores de Contactos", width = 12,
                 collapsible = TRUE, collapsed = FALSE,
                 CajaModalUI(ns("kpi_total")), br(),
                 CajaModalUI(ns("kpi_creacion_prom")), br(),
                 CajaModalUI(ns("kpi_30"))
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
                                       FormatearTexto("Detalle Origen (según fila seleccionada arriba)",
                                                      tamano_pct = 0.8, color = "#64748B"),
                                       TablaReactable2UI(ns("tabla_det_origen"))
                              )
                            )
                   )
                 )
             )
      )
    ),
    box(title = "Detalle de Contactos", width = 12,
        collapsible = TRUE, collapsed = TRUE,
        PanelEtapaUI(ns("Listado"))
    )
  )
}
DetalleContacto <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    # Datos ----
    contactos_raw <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide(), add = TRUE)
      refresh_trigger()
      derivar_etapa_actual() %>%
        filter(Etapa == "CONTACTO", Estado == "ACTIVO") %>%
        mutate(
          FechaHoraCrea   = as_datetime(FechaHoraCrea),
          DiasSinGestion  = as.numeric(difftime(Sys.time(), FechaHoraCrea, units = "days")),
          RangoAntiguedad = .rangos_antiguedad(DiasSinGestion)
        )
    })
    contactos_enriquecidos <- reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide(), add = TRUE)
      
      base  <- contactos_raw()
      relac <- .dias_sin_relacionamiento_bulk(base$CodContacto, base$FechaHoraCrea)
      base %>% left_join(relac, by = "CodContacto")
    })
    
    # KPIs ----
    CajaModal("kpi_total",
              valor  = reactive(html_valor(nrow(contactos_enriquecidos()), formato = "coma", color = "#404040")),
              texto  = html_texto("Total de Contactos", color = "#404040"), icono = "user-group",
              colores = c(fondo = "white"), mostrar_boton = FALSE)
    
    # Creacion de Contactos.
    contactos_creacion_todos <- reactive({
      refresh_trigger()
      CargarDatos("CRMNALCONTACTO") %>%
        mutate(FechaHoraCrea = as_datetime(FechaHoraCrea))
    })
    creacion_mensual_todos <- reactive({
      contactos_creacion_todos() %>%
        mutate(MesCrea = format(FechaHoraCrea, "%Y-%m")) %>%
        count(MesCrea, name = "NumContactos")
    })
    CajaModal("kpi_creacion_prom",
              valor  = reactive({html_valor(
                round(mean(creacion_mensual_todos()$NumContactos, na.rm = TRUE), 1),
                formato = "coma", color = "#404040")}),
              texto  = html_texto("Creación promedio mensual", color = "#404040"),
              icono = "calendar-plus", colores = c(fondo = "white"), mostrar_boton = TRUE,
              titulo_modal = "Creación de contactos", icono_modal = "calendar-plus",
              contenido_modal = function() DetalleCreacionMensualUI(ns("modal_creacion_mensual"))
    )
    DetalleCreacionMensual("modal_creacion_mensual", datos = contactos_creacion_todos)
    
    # Contactos con 30+ días sin registro de relacionamiento
    CajaModal("kpi_30",
              valor  = reactive({html_valor(
                mean(contactos_enriquecidos()$DiasSinRelacionamiento >= 30, na.rm = TRUE),
                formato = "porcentaje", color = "#404040")}),
              texto  = html_texto("Contactos con 30+ días sin registro de relacionamiento", color = "#404040"),
              colores = c(fondo = "white"), mostrar_boton = TRUE, icono = "user-clock",
              titulo_modal = "Contactos con 30+ días sin registro de relacionamiento", icono_modal = "square",
              contenido_modal = function() PanelEtapaUI(ns("modal_kpi_30"))
    )
    PanelEtapa(id = "modal_kpi_30", usuario = usuario, etapa = "CONTACTO", mostrar_titulo = FALSE,
               filtro_extra = function(df) filter(df, DiasSinRelacionamiento >= 30)
               )
    
    # Resumen por dimension ----
    datos_usuario_r <- reactive(.resumen_dimension(contactos_enriquecidos(), "UsuarioCrea"))
    .estilo_total_usuario <- .estilo_fila_total(datos_usuario_r, "UsuarioCrea")
    
    TablaReactable2(id = "tabla_usuario",  data = datos_usuario_r,  columnas = NULL,
                    col_specs = .col_specs_resumen_contacto("UsuarioCrea", "Usuario (Creación)", .estilo_total_usuario),
                    modo_seleccion = "ninguno", id_col = "UsuarioCrea",
                    sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
                    mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    datos_origen_r <- reactive(.resumen_dimension(contactos_enriquecidos(), "Origen"))
    .estilo_total_origen <- .estilo_fila_total(datos_origen_r, "Origen")
    
    modulo_tabla_origen <- TablaReactable2(id = "tabla_origen", data = datos_origen_r, columnas = NULL,
                                           col_specs = .col_specs_resumen_contacto("Origen", "Origen", .estilo_total_origen),
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
    
    # Detalle Origen
    observeEvent(origen_seleccionado(), {
      if (is.null(origen_seleccionado())) {
        shinyjs::hide(id = "BloqueDetOrigen")
      } else {
        shinyjs::show(id = "BloqueDetOrigen")
      }
    }, ignoreNULL = FALSE)
    
    datos_det_origen_r <- reactive({
      req(origen_seleccionado())
      base <- contactos_enriquecidos() %>% filter(Origen == origen_seleccionado())
      .resumen_dimension(base, "DetOrigen")
    })
    .estilo_total_det_origen <- .estilo_fila_total(datos_det_origen_r, "DetOrigen")
    
    TablaReactable2(id = "tabla_det_origen", data = datos_det_origen_r, columnas = NULL,
                    col_specs = .col_specs_resumen_contacto("DetOrigen", "Detalle Origen", .estilo_total_det_origen),
                    modo_seleccion = "ninguno", id_col = "DetOrigen",
                    sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
                    mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    # Listado 
    PanelEtapa(id = "Listado", usuario = usuario, etapa = "CONTACTO", mostrar_titulo = FALSE)
    
    # Alta de contacto ----
    contacto_mod <- CrearContacto(id = "mod_nuevo_contacto", usuario = usuario)
    
    observeEvent(input$btn_nuevo_contacto, {
      showModal(modalDialog(title = "Nuevo Contacto", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                            CrearContactoUI(ns("mod_nuevo_contacto"))))
    })
    observeEvent(contacto_mod$n(), {
      req(contacto_mod$codigo())
      removeModal()
      refresh_trigger(isolate(refresh_trigger()) + 1)
    })
    
    # Descarga ----
    output$etapa_contacto <- downloadHandler(
      filename = function() {
        paste0("Contactos_CRM_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(contactos_enriquecidos(), file, asTable = TRUE, overwrite = TRUE)
      }
    )
  })
}

# App de Prueba ----
ui <- bs4DashPage(
  title = "Prueba Detalle Contacto",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    use_waiter(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    box(title = "Detalle Contacto (prueba)", width = 12, DetalleContactoUI("AccionDetalleContacto"))
  )
)
server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  DetalleContacto("AccionDetalleContacto", usuario = usuario_sesion)
}
shinyApp(ui, server)