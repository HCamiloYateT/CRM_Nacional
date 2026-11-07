# Comparacion Indicadores ----
ComparacionIndicadoresUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      style = "display:flex; gap:8px; flex-wrap:wrap; align-items:flex-start;",
      shiny::div(
        style = "width:220px;",
        racafeForms::racafeInputDate(id = ns("fecha_anterior"), label = shiny::h6("Fecha Inicial"),
                                value = Sys.Date() - 1, width = "100%")
      ),
      shiny::div(
        style = "width:220px;",
        racafeForms::racafeInputDate(id = ns("fecha_actual"), label = shiny::h6("Fecha Final"), value = Sys.Date(),
                                width = "100%")
      ),
      shiny::div(
        style = "width:100px; height:38px; margin-top:35px; display:flex; align-items:center;",
        racafeForms::racafeButton(id = ns("actualizar"), label = NULL, size = "xs", variant = "primary",
                                icon = "code-compare", title = "Comparar")
      )
    ),
    shiny::br(),
    bs4Dash::bs4Card(
      title = "Tabla Comparativa de Indicadores", width = 12,
      status = "white", solidHeader = TRUE, collapsible = TRUE,
      shiny::uiOutput(ns("tabla_comparacion"))
    )
  )
}

ComparacionIndicadores <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    formato <- racafeShiny::DefinirFormato("dolares")
    
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
    
    output$tabla_comparacion <- shiny::renderUI({
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
      
      filas <- lapply(seq_len(nrow(datos)), function(i) {
        local({
          fila <- i
          variacion <- datos$Variacion_Porcentual[[fila]]
          diferencia <- datos$Diferencia_Absoluta[[fila]]
          
          color <- dplyr::case_when(
            is.na(variacion) ~ "#374151",
            variacion > 0 ~ "#1F7A55",
            variacion < 0 ~ "#9F0712",
            TRUE ~ "#374151"
          )
          
          flecha <- dplyr::case_when(
            is.na(variacion) ~ "▶",
            variacion > 0 ~ "▲",
            variacion < 0 ~ "▼",
            TRUE ~ "▶"
          )
          
          variacion_txt <- if (is.na(variacion)) {
            "—"
          } else {
            paste0(flecha, " ", formatC(variacion * 100, format = "f", digits = 2), "%")
          }
          
          shiny::tags$tr(
            shiny::tags$td(
              style = paste0("padding:5px 8px; font-size:11px; font-weight:600;",
                             "color:#374151; border-bottom:1px solid #E5E7EB;"),
              datos$Nombre_Indicador[[fila]]
            ),
            shiny::tags$td(
              style = paste0("padding:5px 8px; font-size:11px; text-align:right;",
                             "border-bottom:1px solid #E5E7EB;"),
              formato(datos$Valor_Anterior[[fila]])
            ),
            shiny::tags$td(
              style = paste0("padding:5px 8px; font-size:11px; text-align:right;",
                             "border-bottom:1px solid #E5E7EB;"),
              formato(datos$Valor_Actual[[fila]])
            ),
            shiny::tags$td(
              style = paste0("padding:5px 8px; font-size:11px; text-align:right;",
                             "border-bottom:1px solid #E5E7EB; color:", color, ";"),
              formato(diferencia)
            ),
            shiny::tags$td(
              style = paste0("padding:5px 8px; font-size:11px; text-align:right;",
                             "font-weight:600; border-bottom:1px solid #E5E7EB;",
                             "color:", color, ";"),
              variacion_txt
            )
          )
        })
      })
      
      shiny::tagList(
        shiny::tags$div(
          style = "padding:2px 8px 8px 8px;",
          FormatearTexto(
            paste0(
              "Comparación entre ",
              format(input$fecha_anterior, "%d/%m/%Y"),
              " y ",
              format(input$fecha_actual, "%d/%m/%Y")
            ),
            negrita = FALSE, color = "#64748B", tamano_pct = 0.75
          )
        ),
        shiny::tags$div(
          style = "width:100%; overflow-x:auto;",
          shiny::tags$table(
            style = "width:100%; border-collapse:collapse;",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th(
                  style = paste0("text-align:left; padding:5px 8px; font-size:10px;",
                                 "font-weight:700; color:#64748B;",
                                 "border-bottom:1px solid #CBD5E1;"),
                  "Indicador"
                ),
                shiny::tags$th(
                  style = paste0("text-align:right; padding:5px 8px; font-size:10px;",
                                 "font-weight:700; color:#64748B;",
                                 "border-bottom:1px solid #CBD5E1;"),
                  "Valor Anterior"
                ),
                shiny::tags$th(
                  style = paste0("text-align:right; padding:5px 8px; font-size:10px;",
                                 "font-weight:700; color:#64748B;",
                                 "border-bottom:1px solid #CBD5E1;"),
                  "Valor Vigente"
                ),
                shiny::tags$th(
                  style = paste0("text-align:right; padding:5px 8px; font-size:10px;",
                                 "font-weight:700; color:#64748B;",
                                 "border-bottom:1px solid #CBD5E1;"),
                  "Diferencia"
                ),
                shiny::tags$th(
                  style = paste0("text-align:right; padding:5px 8px; font-size:10px;",
                                 "font-weight:700; color:#64748B;",
                                 "border-bottom:1px solid #CBD5E1;"),
                  "Variación"
                )
              )
            ),
            shiny::tags$tbody(filas)
          )
        ),
        shiny::tags$p(
          style = "padding:6px 8px 2px 8px; margin:0;",
          FormatearTexto(
            paste0("Fuente de datos en fecha final: ", resultado$fuente_actual, "."),
            negrita = FALSE, color = "#64748B", tamano_pct = 0.7
          )
        )
      )
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
