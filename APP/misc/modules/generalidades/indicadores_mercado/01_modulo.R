# Comparacion Indicadores ----
ComparacionIndicadoresUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    .crm_estilos_ui(),
    shiny::tags$div(class = "mercado-filtros",
      style = "display:flex;gap:12px;flex-wrap:wrap;align-items:flex-end;margin-bottom:16px;",
      shiny::div(style = "width:220px;",
        racafeForms::racafeInputDate("fecha_anterior", label = "Fecha inicial",
          value = Sys.Date() - 1, width = "100%", ns = ns)),
      shiny::div(style = "width:220px;",
        racafeForms::racafeInputDate("fecha_actual", label = "Fecha final",
          value = Sys.Date(), width = "100%", ns = ns)),
      shiny::div(style = "height:38px;display:flex;align-items:center;",
        racafeForms::racafeButton("actualizar", label = NULL, size = "xs", variant = "primary",
          icon = "arrows-rotate", title = "Actualizar indicadores", ns = ns))),
    shiny::uiOutput(ns("tabla_comparacion")))
}

ComparacionIndicadores <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    datos_comparacion_r <- shiny::eventReactive(input$actualizar, ignoreNULL = FALSE, {

      
      shiny::req(input$fecha_anterior, input$fecha_actual)
      
      if (input$fecha_anterior > input$fecha_actual) {
        racafeForms::racafeNotify(type = "error", duration = 5,
                                title = "La fecha inicial no puede ser posterior a la fecha final",
                                session = shiny::getDefaultReactiveDomain())
        return(NULL)
      }
      
      es_hoy <- input$fecha_actual == Sys.Date()
      
      if (es_hoy) {
        datos_actuales_raw <- tryCatch(
          .ObtenerIndicadoresActuales(uid = uid, pwd = pwd),
          error = function(e) {
            racafeForms::racafeNotify(type = "error", duration = 5, title = "No fue posible cargar los datos vigentes",
                                    session = shiny::getDefaultReactiveDomain())
            warning("[Comparacion Indicadores] ", e$message)
            NULL
          }
        )
        
        if (is.null(datos_actuales_raw) || nrow(datos_actuales_raw) == 0L) {
          racafeForms::racafeNotify(type = "error", duration = 5,
                                  title = "No hay datos vigentes disponibles para comparar",
                                  session = shiny::getDefaultReactiveDomain())
          return(NULL)
        }
        
        datos_actuales <- .NormalizarIndicadoresActuales(datos_actuales_raw)
        fuente_actual <- "Datos vigentes"
      } else {
        datos_actuales <- .CargarHistoricoIndicadores(input$fecha_actual)
        
        if (nrow(datos_actuales) == 0L) {
          racafeForms::racafeNotify(type = "error", duration = 5,
                                  title = paste("No se encontraron datos para la fecha final:", input$fecha_actual),
                                  session = shiny::getDefaultReactiveDomain())
          return(NULL)
        }
        
        fuente_actual <- "Base de datos histórica"
      }
      
      datos_anteriores <- .CargarHistoricoIndicadores(input$fecha_anterior)
      
      if (nrow(datos_anteriores) == 0L) {
        racafeForms::racafeNotify(type = "error", duration = 5,
                                title = paste("No se encontraron datos para la fecha inicial:", input$fecha_anterior),
                                session = shiny::getDefaultReactiveDomain())
        return(NULL)
      }
      
      comparacion <- .ConstruirComparacionIndicadores(
        datos_anteriores = datos_anteriores,
        datos_actuales = datos_actuales
      ) %>%
        dplyr::filter(!is.na(Valor_Anterior) | !is.na(Valor_Actual))
      
      list(
        datos = comparacion,
        fuente_actual = fuente_actual,
        es_hoy = es_hoy
      )
    })
    
    output$tabla_comparacion <- .crm_render_ui({
      resultado <- datos_comparacion_r()
      shiny::req(!is.null(resultado))
      
      datos <- resultado$datos
      
      if (nrow(datos) == 0L) {
        return(
          shiny::tags$div(
            style = "padding:20px; text-align:center;",
            FormatearTexto("No hay datos disponibles para comparar",
                           negrita = FALSE, color = "#64748B", tamano_pct = 0.8,
                           alineacion = "center")
          )
        )
      }
      
      tabla <- .crm_tabla_vancouver(datos %>%
        dplyr::select(Nombre_Indicador, Valor_Anterior, Valor_Actual, Diferencia_Absoluta, Variacion_Porcentual),
        labels = c(Nombre_Indicador = "Indicador", Valor_Anterior = "Valor anterior",
          Valor_Actual = "Valor vigente", Diferencia_Absoluta = "Diferencia", Variacion_Porcentual = "Variación"),
        font_size = 11, row_padding = 3) %>%
        gt::fmt_currency(columns = c(Valor_Anterior, Valor_Actual, Diferencia_Absoluta),
          currency = "USD", decimals = 2, sep_mark = ",", dec_mark = ".") %>%
        gt::fmt_percent(columns = Variacion_Porcentual, decimals = 2) %>%
        gt::sub_missing(missing_text = "—")
      for (columna in c("Diferencia_Absoluta", "Variacion_Porcentual")) {
        for (signo in c(-1, 1)) {
          filas <- which(!is.na(datos[[columna]]) & sign(datos[[columna]]) == signo)
          if (length(filas)) tabla <- gt::tab_style(tabla,
            style = gt::cell_text(color = if (signo > 0) "#1F7A55" else "#9F0712"),
            locations = gt::cells_body(columns = dplyr::all_of(columna), rows = filas))
        }
      }
      shiny::tagList(tabla, .crm_nota(c(
        paste("Comparación entre", format(input$fecha_anterior, "%d/%m/%Y"),
          "y", format(input$fecha_actual, "%d/%m/%Y")),
        paste0("Fuente de datos en fecha final: ", resultado$fuente_actual, "."))))
    })

    invisible(NULL)
  })
}

# App de Prueba ----
DemoGeneralidadesComparacionIndicadores <- function() {
  ui <- bs4DashPage(
    title = "Prueba Comparacion Indicadores",
    header = bs4DashNavbar(),
    sidebar = bs4DashSidebar(),
    controlbar = bs4DashControlbar(),
    footer = bs4DashFooter(),
    body = bs4DashBody(
      useShinyjs(),
      use_waiter(),
      includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
      ComparacionIndicadoresUI("CompIndicadores")
    )
  )

  server <- function(input, output, session) {
    ComparacionIndicadores("CompIndicadores")
  }

  shinyApp(ui, server)
}

