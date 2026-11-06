# Indicadores ----
TablaIndicadoresUI <- function(id) {
  ns <- shiny::NS(id)
  racafeShiny::racafeOverlayLoader("indicadores_loader", ns = ns,
    visible = TRUE, image = "indicadores_loader.gif", triggers = "actualizar",
    racafeForms::racafeForm(size = "compact", variant = "plain",
      shiny::tags$div(id = ns("indicadores_contenido"),
        racafeForms::racafeDetailSection("Indicadores de mercado",
          gt::gt_output(ns("tabla_indicadores")), shiny::uiOutput(ns("ultima_actualizacion"))),
        racafeForms::racafeDetailSection("Posición",
          gt::gt_output(ns("tabla_disponible")), shiny::uiOutput(ns("pie_disponible")))),
      racafeForms::racafeFormFooter(racafeForms::racafeButton("actualizar", "Actualizar", icon = "sync",
        variant = "primary", size = "xxs", ns = ns, title = "Actualizar indicadores"))))
}

TablaIndicadores <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    formato <- scales::label_dollar(prefix = "$ ", accuracy = 1,
                                    big.mark = SEP_MILES, decimal.mark = SEP_DECIMAL)
    
    # Reactivos ----
    datos_r <- shiny::reactiveVal(NULL)
    anterior_rv <- shiny::reactiveVal(NULL)
    disponible_rv <- shiny::reactiveVal(NULL)
    ultima_actualizacion_r <- shiny::reactiveVal(NULL)
    actualizando_rv <- shiny::reactiveVal(FALSE)
    
    # Funciones ----
    .pie_tabla <- function(texto) {
      shiny::tags$div(class = "text-muted text-right",
                      style = "font-size:10px;line-height:1.3;padding:3px 8px;margin:0;",
                      texto)
    }
    
    .estilo_tabla <- function(tabla) {
      racafeShiny::racafeCompactTable(tabla, backend = "gt", show_header = FALSE,
        font_size = 10, row_padding = 1, align = c("left", "right"))
    }
    
    .limpiar_html <- function(x) {
      x <- gsub("<[^>]*>", "", as.character(x))
      x <- gsub("&nbsp;", " ", x, fixed = TRUE)
      x <- gsub("&amp;", "&", x, fixed = TRUE)
      x <- gsub("&lt;", "<", x, fixed = TRUE)
      x <- gsub("&gt;", ">", x, fixed = TRUE)
      trimws(x)
    }
    
    .leer_indicadores <- function() {
      on.exit({
        actualizando_rv(FALSE)
        racafeShiny::racafeUpdateOverlayLoader(session, "indicadores_loader", FALSE)
      }, add = TRUE)
      
      tryCatch({
        actuales <- .ObtenerIndicadoresActuales(uid = uid, pwd = pwd)
        if (!is.data.frame(actuales) || !nrow(actuales)) {
          stop("La consulta no devolvió indicadores.", call. = FALSE)
        }
        datos_r(actuales)
        ultima_actualizacion_r(Sys.time())
        
        if (is.null(shiny::isolate(anterior_rv()))) {
          anterior_rv(.CargarAnteriorIndicadores())
        }
        if (is.null(shiny::isolate(disponible_rv()))) {
          disponible_rv(.CargarDisponibleIndicadores())
        }
      }, error = function(e) {
        racafeForms::racafeNotify(
          session, "Indicadores no disponibles", conditionMessage(e),
          type = "error", duration = 6
        )
        warning("[Indicadores] ", conditionMessage(e))
      })
      invisible(NULL)
    }
    
    # Actualización ----
    shiny::observeEvent(input$actualizar, {
      if (isTRUE(shiny::isolate(actualizando_rv()))) return(invisible(NULL))
      actualizando_rv(TRUE)
      session$onFlushed(function() .leer_indicadores(), once = TRUE)
    }, ignoreNULL = FALSE)
    
    # Outputs ----
    
    tabla_indicadores_r <- shiny::reactive({
      shiny::req(datos_r(), anterior_rv())
      .PrepararTendenciaIndicadores(
        datos_actuales = datos_r(), datos_anteriores = anterior_rv(),
        formato = formato
      )
    })
    
    tabla_disponible_r <- shiny::reactive({
      shiny::req(disponible_rv())
      .PrepararDisponibleIndicadores(disponible_rv())
    })
    
    output$tabla_indicadores <- gt::render_gt({
      tabla <- tabla_indicadores_r()
      shiny::validate(shiny::need(is.data.frame(tabla) &&
                                    all(c("Item", "Valor") %in% names(tabla)) && nrow(tabla) > 0L,
                                  "Sin indicadores disponibles"))
      valores <- as.character(tabla$Valor)
      texto <- .limpiar_html(valores)
      tabla <- data.frame(Item = as.character(tabla$Item), Valor = texto,
                          stringsAsFactors = FALSE)
      verdes <- which(grepl("▲|▴|↑|&#9650;", valores) |
                        grepl("#1F7A55|#198754|green", valores, ignore.case = TRUE))
      rojos <- which(grepl("▼|▾|↓|&#9660;", valores) |
                       grepl("#9F0712|#DC3545|red", valores, ignore.case = TRUE))
      resultado <- .estilo_tabla(tabla) |>
        gt::cols_width(Item ~ gt::pct(70), Valor ~ gt::pct(30))
      if (length(verdes)) resultado <- resultado |>
        gt::tab_style(gt::cell_text(color = "#1F7A55", weight = "bold"),
                      locations = gt::cells_body(columns = Valor, rows = verdes))
      if (length(rojos)) resultado <- resultado |>
        gt::tab_style(gt::cell_text(color = "#B4232E", weight = "bold"),
                      locations = gt::cells_body(columns = Valor, rows = rojos))
      resultado
    })
    
    output$ultima_actualizacion <- shiny::renderUI({
      shiny::req(ultima_actualizacion_r())
      .pie_tabla(paste0("Datos actualizados a: ",
                        format(ultima_actualizacion_r(), "%d/%m/%Y %H:%M")))
    })
    
    output$tabla_disponible <- gt::render_gt({
      tabla <- tabla_disponible_r()
      shiny::validate(shiny::need(is.data.frame(tabla) &&
                                    all(c("Concepto", "Valor") %in% names(tabla)) && nrow(tabla) > 0L,
                                  "Sin saldos disponibles"))
      tabla <- data.frame(Concepto = as.character(tabla$Concepto),
                          Valor = vapply(tabla$Valor, formato, character(1)),
                          stringsAsFactors = FALSE)
      .estilo_tabla(tabla) |>
        gt::cols_width(Concepto ~ gt::pct(69), Valor ~ gt::pct(31))
    })
    
    output$pie_disponible <- shiny::renderUI({
      shiny::req(disponible_rv())
      .pie_tabla(sprintf("Posición con cierre al %s",
                         .FechaDisponibleIndicadores(disponible_rv())))
    })
    
    # Retorno ----
    list(datos_r = shiny::reactive(datos_r()),
         ultima_actualizacion_r = shiny::reactive(ultima_actualizacion_r()))
  })
}
