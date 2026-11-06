# DetalleDescartados

### Modulo Principal ----
# Construye la interfaz principal del analisis de descartados.
DetalleDescartadosUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3, CajaModalUI(ns("kpi_contacto"))),
      column(3, CajaModalUI(ns("kpi_prospecto"))),
      column(3, CajaModalUI(ns("kpi_lead"))),
      column(3, CajaModalUI(ns("kpi_cliente")))
    ),
    div(style = "width:220px;",
        ListaDesplegable(ns("FiltroEtapa"),
                         label = h6("Filtrar por Etapa de Descarte"),
                         choices = c(
                           "Todas" = "TODAS",
                           setNames(
                             c("CONTACTO", "PROSPECTO", "LEAD", "CLIENTE"),
                             c(.ETIQUETA_DESCARTE_ETAPA[["CONTACTO"]],
                               .ETIQUETA_DESCARTE_ETAPA[["PROSPECTO"]],
                               .ETIQUETA_DESCARTE_ETAPA[["LEAD"]],
                               .ETIQUETA_DESCARTE_ETAPA[["CLIENTE"]])
                           )
                         ),
                         selected = "TODAS", multiple = FALSE)
    ),
    br(),
    fluidRow(
      column(
        8,
        box(title = "Motivos de Descarte", width = 12,
            collapsible = TRUE, collapsed = FALSE,
            TablaReactable2UI(ns("tabla_razon1")),
            shinyjs::hidden(
              tags$div(id = ns("BloqueOtrosRazon1"),
                       tags$hr(),
                       FormatearTexto("Categorías agrupadas en OTROS",
                                      tamano_pct = 0.8, color = "#64748B"),
                       TablaReactable2UI(ns("tabla_otros_razon1")))
            )
        )
      ),
      column(
        4,
        box(title = "Nube de Palabras", width = 12,
            collapsible = TRUE, collapsed = FALSE,
            plotly::plotlyOutput(ns("nube_palabras"), height = "400px"))
      )
    )
  )
}

DetalleDescartados <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    razon1_modal <- reactiveVal(NULL)
    
    ### Datos ----
    # Carga el historial de descartes registrado en CRMNALDESCARTE.
    descartes_r <- reactive({
      CargarDatos("CRMNALDESCARTE")
    })
    
    # Obtiene los contactos cuya etapa vigente es DESCARTADO.
    descartados_vigentes_r <- reactive({
      CargarDatos("CRMNALCONTACTO") %>%
        derivar_etapa_actual() %>%
        filter(Etapa == "DESCARTADO") %>%
        mutate(EtapaPreDescarte = coalesce(EtapaPreDescarte, "CONTACTO"))
    })
    
    # Aplica el filtro de etapa sobre el historial de descartes.
    descartes_filtrados_r <- reactive({
      etapa_filtro <- input$FiltroEtapa %||% "TODAS"
      dat <- descartes_r()
      if (!identical(etapa_filtro, "TODAS")) {
        dat <- dat %>% filter(Etapa == etapa_filtro)
      }
      dat
    })
    
    ### Indicadores ----
    # Calcula el total vigente descartado para usarlo como denominador.
    total_descartados_vigentes_r <- reactive({
      nrow(descartados_vigentes_r())
    })
    
    # Cuenta y calcula el porcentaje de descartados vigentes desde CONTACTO.
    CajaModal(
      "kpi_contacto",
      valor = reactive({
        n_etapa <- sum(
          descartados_vigentes_r()$EtapaPreDescarte == "CONTACTO",
          na.rm = TRUE
        )
        total <- total_descartados_vigentes_r()
        pct <- if (total > 0) n_etapa / total else 0
        
        tagList(
          html_valor(n_etapa, formato = "coma", color = "#404040"),
          FormatearTexto(
            paste0(" (", scales::percent(pct, accuracy = 0.1), ")"),
            tamano_pct = 0.8,
            color = "#404040"
          )
        )
      }),
      texto = html_texto("Descartados desde Contacto", color = "#404040"),
      icono = "user",
      colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = paste0(
        "Contactos cuya etapa vigente es DESCARTADO y cuya etapa previa ",
        "al descarte fue CONTACTO. El porcentaje se calcula sobre el total ",
        "de descartados vigentes."
      )
    )
    
    # Cuenta y calcula el porcentaje de descartados vigentes desde PROSPECTO.
    CajaModal(
      "kpi_prospecto",
      valor = reactive({
        n_etapa <- sum(
          descartados_vigentes_r()$EtapaPreDescarte == "PROSPECTO",
          na.rm = TRUE
        )
        total <- total_descartados_vigentes_r()
        pct <- if (total > 0) n_etapa / total else 0
        
        tagList(
          html_valor(n_etapa, formato = "coma", color = "#404040"),
          FormatearTexto(
            paste0(" (", scales::percent(pct, accuracy = 0.1), ")"),
            tamano_pct = 0.8,
            color = "#404040"
          )
        )
      }),
      texto = html_texto("Descartados desde Prospecto", color = "#404040"),
      icono = "user-clock",
      colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = paste0(
        "Contactos cuya etapa vigente es DESCARTADO y cuya etapa previa ",
        "al descarte fue PROSPECTO. El porcentaje se calcula sobre el total ",
        "de descartados vigentes."
      )
    )
    
    # Cuenta y calcula el porcentaje de descartados vigentes desde LEAD.
    CajaModal(
      "kpi_lead",
      valor = reactive({
        n_etapa <- sum(
          descartados_vigentes_r()$EtapaPreDescarte == "LEAD",
          na.rm = TRUE
        )
        total <- total_descartados_vigentes_r()
        pct <- if (total > 0) n_etapa / total else 0
        
        tagList(
          html_valor(n_etapa, formato = "coma", color = "#404040"),
          FormatearTexto(
            paste0(" (", scales::percent(pct, accuracy = 0.1), ")"),
            tamano_pct = 0.8,
            color = "#404040"
          )
        )
      }),
      texto = html_texto("Descartados desde Lead", color = "#404040"),
      icono = "bullseye",
      colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = paste0(
        "Contactos cuya etapa vigente es DESCARTADO y cuya etapa previa ",
        "al descarte fue LEAD. El porcentaje se calcula sobre el total ",
        "de descartados vigentes."
      )
    )
    
    # Cuenta y calcula el porcentaje de descartados vigentes desde CLIENTE.
    CajaModal(
      "kpi_cliente",
      valor = reactive({
        n_etapa <- sum(
          descartados_vigentes_r()$EtapaPreDescarte == "CLIENTE",
          na.rm = TRUE
        )
        total <- total_descartados_vigentes_r()
        pct <- if (total > 0) n_etapa / total else 0
        
        tagList(
          html_valor(n_etapa, formato = "coma", color = "#404040"),
          FormatearTexto(
            paste0(" (", scales::percent(pct, accuracy = 0.1), ")"),
            tamano_pct = 0.8,
            color = "#404040"
          )
        )
      }),
      texto = html_texto("Descartados desde Cliente", color = "#404040"),
      icono = "user-check",
      colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = paste0(
        "Contactos cuya etapa vigente es DESCARTADO y cuya etapa previa ",
        "al descarte fue CLIENTE. El porcentaje se calcula sobre el total ",
        "de descartados vigentes."
      )
    )
    ### Razon Principal ----
    # Construye el resumen principal de razones de descarte.
    resumen_razon1_r <- reactive({
      .resumen_top_n_otros_por_etapa(descartes_filtrados_r(), "Razon1")
    })
    
    # Extrae la tabla principal y define el estilo de la fila total.
    datos_razon1_r <- reactive(resumen_razon1_r()$tabla)
    estilo_razon1 <- .estilo_fila_total(datos_razon1_r, "Categoria")
    
    # Construye la tabla principal de razones.
    modulo_tabla_razon1 <- TablaReactable2(
      id = "tabla_razon1", data = datos_razon1_r, columnas = NULL,
      col_specs = .col_specs_resumen_etapa(estilo_razon1, "Razón"),
      modo_seleccion = "fila", id_col = "Categoria",
      sortable = TRUE, searchable = FALSE,
      page_size = .TOP_N_RAZONES_DESCARTE + 2,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    # Recupera la razon seleccionada en la tabla principal.
    razon1_seleccionada <- reactive({
      sel <- modulo_tabla_razon1$seleccion()
      if (is.null(sel)) NULL else sel$fila$Categoria[[1]]
    })
    
    # Limpia la seleccion anterior cuando cambia el filtro de etapa.
    observeEvent(input$FiltroEtapa, {
      razon1_modal(NULL)
      shinyjs::hide(id = "BloqueOtrosRazon1")
    }, ignoreInit = TRUE)
    
    # Abre el detalle o muestra las categorias agrupadas.
    observeEvent(razon1_seleccionada(), {
      sel <- razon1_seleccionada()
      if (is.null(sel) || identical(sel, "TOTAL")) {
        shinyjs::hide(id = "BloqueOtrosRazon1")
      } else if (startsWith(sel, "OTROS (")) {
        shinyjs::show(id = "BloqueOtrosRazon1")
      } else {
        shinyjs::hide(id = "BloqueOtrosRazon1")
        razon1_modal(sel)
        showModal(.modal_detalle_razon1(sel))
      }
    }, ignoreNULL = TRUE)
    
    ### Nube de Palabras ----
    # Descompone Razon1 y elimina las palabras definidas en .STOPWORDS_ES.
    datos_nube_r <- reactive({
      descartes_filtrados_r() %>%
        filter(!is.na(Razon1), trimws(Razon1) != "") %>%
        transmute(Palabra = str_to_lower(Razon1)) %>%
        mutate(Palabra = str_replace_all(
          Palabra,
          "[^a-záéíóúüñ ]",
          " "
        )) %>%
        tidyr::separate_rows(Palabra, sep = "\\s+") %>%
        filter(
          Palabra != "",
          nchar(Palabra) > 2,
          !Palabra %in% .STOPWORDS_ES
        ) %>%
        count(Palabra, sort = TRUE, name = "Frecuencia") %>%
        slice_head(n = 35) %>%
        mutate(
          Palabra = str_to_title(Palabra),
          Tamano = scales::rescale(Frecuencia, to = c(12, 34)),
          Posicion = row_number() - 1,
          Angulo = Posicion * 2.399963,
          Radio = sqrt(Posicion) * 0.42,
          X = Radio * cos(Angulo),
          Y = Radio * sin(Angulo),
          Tooltip = paste0(
            "<b>", Palabra, "</b><br>",
            "Frecuencia: ", format(Frecuencia, big.mark = ",")
          )
        )
    })
    
    # Genera la nube con el tamano de cada palabra segun su frecuencia.
    output$nube_palabras <- plotly::renderPlotly({
      dat <- datos_nube_r()
      
      if (nrow(dat) == 0) {
        return(
          plotly::config(
            plotly::plotly_empty(type = "scatter"),
            displayModeBar = FALSE
          )
        )
      }
      
      plotly::plot_ly(
        data = dat, x = ~X, y = ~Y, type = "scatter", mode = "text",
        text = ~Palabra, hovertext = ~Tooltip, hoverinfo = "text",
        textfont = list(size = dat$Tamano, color = "#1C398E")
      ) %>%
        plotly::layout(
          margin = list(l = 5, r = 5, t = 5, b = 5),
          xaxis = list(visible = FALSE, zeroline = FALSE),
          yaxis = list(visible = FALSE, zeroline = FALSE),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          hoverlabel = list(
            bgcolor = "#1A3C5E",
            font = list(color = "white", size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    ### Otros Motivos ----
    # Recupera las razones agrupadas en OTROS.
    detalle_otros_razon1_r <- reactive({
      req(resumen_razon1_r()$detalle_otros)
      resumen_razon1_r()$detalle_otros %>% arrange(desc(Descartados))
    })
    
    # Define el estilo del detalle de razones agrupadas.
    estilo_otros_razon1 <- .estilo_fila_total(
      detalle_otros_razon1_r, "Categoria"
    )
    
    # Construye la tabla de razones agrupadas.
    modulo_tabla_otros_razon1 <- TablaReactable2(
      id = "tabla_otros_razon1", data = detalle_otros_razon1_r,
      columnas = NULL,
      col_specs = .col_specs_resumen_etapa(
        estilo_otros_razon1, "Razón"
      ),
      modo_seleccion = "fila", id_col = "Categoria",
      sortable = TRUE, searchable = TRUE, page_size = 20,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    # Recupera la razon seleccionada dentro de OTROS.
    otros_razon1_seleccionada <- reactive({
      sel <- modulo_tabla_otros_razon1$seleccion()
      if (is.null(sel)) NULL else sel$fila$Categoria[[1]]
    })
    
    # Abre el modal desde el listado de categorias agrupadas.
    observeEvent(otros_razon1_seleccionada(), {
      sel <- otros_razon1_seleccionada()
      req(sel)
      razon1_modal(sel)
      showModal(.modal_detalle_razon1(sel))
    }, ignoreNULL = TRUE)
    
    ### Datos del Modal ----
    # Determina si la segunda razon debe analizarse como numero.
    es_numerico_modal_r <- reactive({
      req(razon1_modal())
      identical(.detalle_config_razon(razon1_modal())$tipo, "numero")
    })
    
    # Recupera los descartes correspondientes a la razon seleccionada.
    datos_razon1_modal_r <- reactive({
      req(razon1_modal())
      descartes_filtrados_r() %>% filter(Razon1 == razon1_modal())
    })
    
    ### Modal ----
    # Construye el contenido del modal segun el tipo de Razon2.
    .modal_detalle_razon1 <- function(razon1) {
      es_numerico <- identical(.detalle_config_razon(razon1)$tipo, "numero")
      
      contenido_razon2 <- if (es_numerico) {
        tagList(
          fluidRow(
            column(3, CajaModalUI(ns("kpi_precio_min"))),
            column(3, CajaModalUI(ns("kpi_precio_prom"))),
            column(3, CajaModalUI(ns("kpi_precio_mediana"))),
            column(3, CajaModalUI(ns("kpi_precio_max")))
          ),
          plotly::plotlyOutput(ns("histograma_precio"), height = "260px")
        )
      } else {
        tagList(
          TablaReactable2UI(ns("tabla_razon2_modal")),
          shinyjs::hidden(
            tags$div(
              id = ns("BloqueOtrosRazon2"), tags$hr(),
              FormatearTexto("Categorías agrupadas en OTROS",
                             tamano_pct = 0.8, color = "#64748B"),
              TablaReactable2UI(ns("tabla_otros_razon2"))
            )
          )
        )
      }
      
      modalDialog(
        title = paste("Detalle de la razón:", razon1),
        size = "xl", easyClose = TRUE, footer = modalButton("Cerrar"),
        contenido_razon2, tags$hr(),
        PanelEtapaUI(ns("panel_contactos_razon1"))
      )
    }
    
    ### Analisis Numerico ----
    # Convierte Razon2 a numero para las razones configuradas como numericas.
    precios_modal_r <- reactive({
      req(es_numerico_modal_r())
      precios <- suppressWarnings(
        as.numeric(gsub(",", "", datos_razon1_modal_r()$Razon2))
      )
      precios[is.finite(precios)]
    })
    
    # Muestra el precio minimo.
    CajaModal(
      "kpi_precio_min",
      valor = reactive({
        p <- precios_modal_r()
        if (length(p) == 0) 0 else min(p)
      }),
      texto = "Precio Mínimo", icono = "arrow-down",
      mostrar_boton = FALSE
    )
    
    # Muestra el precio promedio.
    CajaModal(
      "kpi_precio_prom",
      valor = reactive({
        p <- precios_modal_r()
        if (length(p) == 0) 0 else round(mean(p), 0)
      }),
      texto = "Precio Promedio", icono = "chart-line",
      mostrar_boton = FALSE
    )
    
    # Muestra la mediana del precio.
    CajaModal(
      "kpi_precio_mediana",
      valor = reactive({
        p <- precios_modal_r()
        if (length(p) == 0) 0 else round(median(p), 0)
      }),
      texto = "Precio Mediana", icono = "sort",
      mostrar_boton = FALSE
    )
    
    # Muestra el precio maximo.
    CajaModal(
      "kpi_precio_max",
      valor = reactive({
        p <- precios_modal_r()
        if (length(p) == 0) 0 else max(p)
      }),
      texto = "Precio Máximo", icono = "arrow-up",
      mostrar_boton = FALSE
    )
    
    # Grafica la distribucion de los valores numericos.
    output$histograma_precio <- plotly::renderPlotly({
      p <- precios_modal_r()
      
      if (length(p) == 0) {
        return(
          plotly::config(
            plotly::plotly_empty(type = "histogram"),
            displayModeBar = FALSE
          )
        )
      }
      
      plotly::plot_ly(
        x = p, type = "histogram",
        marker = list(color = "#1C398E")
      ) %>%
        plotly::layout(
          margin = list(l = 30, r = 20, t = 10, b = 40),
          xaxis = list(title = "Precio informado"),
          yaxis = list(title = "Descartes"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    ### Razon Secundaria ----
    # Resume Razon2 cuando corresponde a una variable categorica.
    resumen_razon2_modal_r <- reactive({
      req(razon1_modal())
      req(!es_numerico_modal_r())
      .resumen_top_n_otros_por_etapa(datos_razon1_modal_r(), "Razon2")
    })
    
    # Extrae la tabla de Razon2 y define el estilo de la fila total.
    datos_razon2_modal_r <- reactive(resumen_razon2_modal_r()$tabla)
    estilo_razon2_modal <- .estilo_fila_total(
      datos_razon2_modal_r, "Categoria"
    )
    
    # Construye la tabla de razones secundarias.
    TablaReactable2(
      id = "tabla_razon2_modal", data = datos_razon2_modal_r,
      columnas = NULL,
      col_specs = .col_specs_resumen_etapa(
        estilo_razon2_modal, "Detalle"
      ),
      modo_seleccion = "ninguno", id_col = "Categoria",
      sortable = TRUE, searchable = TRUE,
      page_size = .TOP_N_RAZONES_DESCARTE + 2,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    # Recupera las categorias secundarias agrupadas en OTROS.
    detalle_otros_razon2_r <- reactive({
      req(resumen_razon2_modal_r()$detalle_otros)
      resumen_razon2_modal_r()$detalle_otros %>% arrange(desc(Descartados))
    })
    
    # Define el estilo del detalle secundario.
    estilo_otros_razon2 <- .estilo_fila_total(
      detalle_otros_razon2_r, "Categoria"
    )
    
    # Construye la tabla secundaria de categorias agrupadas.
    TablaReactable2(
      id = "tabla_otros_razon2", data = detalle_otros_razon2_r,
      columnas = NULL,
      col_specs = .col_specs_resumen_etapa(
        estilo_otros_razon2, "Detalle"
      ),
      modo_seleccion = "ninguno", id_col = "Categoria",
      sortable = TRUE, searchable = TRUE, page_size = 20,
      compact = TRUE, mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    
    ### Panel de Contactos ----
    # Muestra como PanelEtapa los contactos asociados con la razon.
    PanelEtapa(
      id = "panel_contactos_razon1", usuario = usuario,
      etapa = "DESCARTADO", mostrar_titulo = FALSE,
      filtro_extra = function(df) {
        codigos <- datos_razon1_modal_r() %>%
          pull(CodContacto) %>%
          unique()
        df %>% filter(CodContacto %in% codigos)
      }
    )
    
    invisible(NULL)
  })
}

### App de Prueba ----
# Ejecuta el modulo utilizando los datos reales del CRM.
ui <- bs4DashPage(
  title = "Prueba Detalle Descartados",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    htmltools::includeCSS(
      paste0(
        "https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
        "refs/heads/main/Styles/style.css"
      )
    ),
    DetalleDescartadosUI("AccionDetalleDescartados")
  )
)

server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  DetalleDescartados(
    id = "AccionDetalleDescartados",
    usuario = usuario_sesion
  )
}

shinyApp(ui = ui, server = server)