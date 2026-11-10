# Gestión de clientes: señales observadas, no inferencia de altas comerciales ----
.fid_preparar <- function(hechos, clientes, corte, umbral = 90) {
  fact <- .com_hechos(hechos, corte, cerrados = FALSE)
  fact <- fact[!is.na(fact$SacosFact) & fact$SacosFact > 0, ]
  inicio <- as.Date(format(corte, "%Y-01-01"))
  historial <- fact %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::summarise(PrimeraObservada = .gc_fecha_min(FecFact), UltimaCompra = .gc_fecha_max(FecFact),
      SacosAnho = sum(SacosFact[FecFact >= inicio]),
      MargenAnho = .com_suma(Margen[FecFact >= inicio]), .groups = "drop")
  cli <- clientes %>% dplyr::select(CliNitPpal, LinNegCod, PerRazSoc, Asesor, Segmento, SSPpto, MNFCCPpto)
  x <- dplyr::full_join(cli, historial, by = c("CliNitPpal", "LinNegCod"))
  anteriores <- historial %>% dplyr::filter(PrimeraObservada < inicio) %>%
    dplyr::group_by(CliNitPpal) %>% dplyr::summarise(TeniaOtraLinea = TRUE, .groups = "drop")
  x <- dplyr::left_join(x, anteriores, by = "CliNitPpal")
  x$ClasificacionPresupuesto <- ifelse(is.na(x$MNFCCPpto), "No disponible",
    ifelse(x$MNFCCPpto > 0, "Con presupuesto", "Sin presupuesto"))
  x$DiasSinCompra <- as.integer(corte - x$UltimaCompra)
  x$Estado <- dplyr::case_when(is.na(x$UltimaCompra) ~ "Sin compra observada",
    x$DiasSinCompra > umbral ~ "Revisar inactividad",
    x$PrimeraObservada >= inicio & x$TeniaOtraLinea %in% TRUE ~ "Primera compra observada en nueva línea",
    x$PrimeraObservada >= inicio ~ "Primera compra observada del año",
    is.na(x$MNFCCPpto) & x$SacosAnho > 0 ~ "Presupuesto no disponible",
    x$MNFCCPpto == 0 & x$SacosAnho > 0 ~ "Compra sin presupuesto", TRUE ~ "Compra recurrente")
  x
}
.FID_ESTADOS <- c("Revisar inactividad", "Sin compra observada", "Primera compra observada del año",
  "Primera compra observada en nueva línea", "Compra sin presupuesto", "Presupuesto no disponible", "Compra recurrente")
.FID_CAJAS <- c(rfm_clientes = "Clientes/línea con compra", rfm_riesgo = "Leales en riesgo · RFM",
  btyd_riesgo = "Riesgo alto · BG/NBD", clv_margen = "CLV total de margen · COP")
FidelidadRiesgoUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputNumeric("umbral", "Umbral de revisión (días)", value = 90, min = 1,
        format = "number", ns = ns),
      .crm_select("estado", "Estado de las señales observadas", choices = .FID_ESTADOS,
        selected = .FID_ESTADOS, multiple = TRUE, ns = ns)),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputNumeric("horizonte", "Horizonte CLV (meses)", value = 12, min = 1, max = 36,
        decimals = 0, ns = ns),
      racafeForms::racafeInputNumeric("descuento", "Descuento anual (%)", value = 12, min = 0, max = 100, ns = ns)),
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("modelar", "Calcular RFM, BG/NBD y CLV",
      icon = "chart-line", ns = ns)),
    .crm_disclosure("Cómo interpretar fidelidad, riesgo y valor del cliente",
      .crm_nota(c("RFM responde: ¿compró hace poco?, ¿compra seguido?, ¿cuánto margen deja?",
        "Cada nota va de 1 a 5 frente al resto de clientes visibles; 5 es mejor.",
        "BG/NBD usa el ritmo de compras para estimar si seguirá comprando y cuántas veces volvería.",
        "Por ejemplo, 80% de actividad es una estimación, no un pedido. Un día con varias facturas cuenta una vez.",
        "CLV estima cuánto margen podría dejar en los próximos meses usando su compra habitual. No asegura ventas.",
        "El margen abierto puede ser provisional. Sin margen conocido, el CLV se mantiene desconocido.",
        "Riesgo orientativo: alto <40%, medio 40–70%, bajo >=70% de actividad estimada.",
        "Las primeras compras solo son las observadas en el historial disponible."))),
    .gc_tarjetas_ui(ns, .FID_CAJAS),
    .crm_tabs(
      tabPanel("RFM", .crm_tabla_ui(ns("rfm"))),
      tabPanel("BTYD y CLV", shiny::uiOutput(ns("estado_modelo")),
        shiny::conditionalPanel(sprintf("output['%s']", ns("modelo_disponible")),
          .crm_plot_output(ns("modelo_grafico"), height = "330px"), .crm_tabla_ui(ns("btyd")))),
      tabPanel("Diagnóstico del modelo",
        shiny::conditionalPanel(sprintf("output['%s']", ns("modelo_disponible")),
          .crm_tabla_ui(ns("modelo_parametros")), .crm_tabla_ui(ns("validacion"))),
        .crm_nota("Validación retrospectiva de 90 días sobre clientes con historia previa."))),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("resumen")), .crm_tabla_ui(ns("clientes")),
    .crm_nota(c("La primera compra es la primera observada en la exportación: no equivale a la creación del cliente.",
      "Un cliente sin presupuesto no se clasifica automáticamente como recuperado.",
      "La inactividad es una señal por días sin compra; el umbral es parametrizable y no cambia el estado del CRM.",
      "Los datos cerrados y abiertos alimentan la señal comercial; el margen abierto es provisional.")))
}
FidelidadRiesgo <- function(id, hechos, clientes, filtros) {
  shiny::moduleServer(id, function(input, output, session) {
    corte <- shiny::reactive({
      fechas <- as.Date(filtros()$fecha)
      shiny::req(length(fechas) == 2L, !anyNA(fechas))
      fechas[[2]]
    })
    firma <- shiny::reactive(list(corte = corte(), meses = input$horizonte, descuento = input$descuento,
      hechos = hechos()[intersect(c("CliNitPpal", "LinNegCod", "FecFact", "SacosFact", "Margen"), names(hechos()))]))
    modelo_ajustado <- shiny::eventReactive(input$modelar, {
      tryCatch({
        shiny::req(corte(), input$horizonte, is.finite(input$descuento))
        if (input$horizonte < 1 || input$horizonte > 36 || input$descuento < 0 || input$descuento > 100) {
          stop("Revise horizonte y descuento.")
        }
        m <- .fid_analizar_modelos(hechos(), as.Date(corte()), as.integer(input$horizonte), input$descuento / 100)
        m$validacion <- .fid_validar_btyd(hechos(), as.Date(corte())); m$firma <- firma(); m
      }, error = function(e) list(error = conditionMessage(e)))
    })
    modelo <- shiny::reactive({
      m <- modelo_ajustado()
      if (!is.null(m$error)) return(m)
      if (!identical(m$firma, firma())) return(list(error = "Los filtros o parámetros cambiaron. Vuelva a Calcular."))
      m
    })
    output$modelo_disponible <- shiny::reactive({
      !is.null(input$modelar) && input$modelar > 0 && is.null(modelo()$error)
    })
    shiny::outputOptions(output, "modelo_disponible", suspendWhenHidden = FALSE)
    .crm_vancouver_server("validacion", shiny::reactive({
      m <- modelo(); shiny::validate(shiny::need(is.null(m$error), m$error)); m$validacion
    }))
    rfm <- shiny::reactive(.fid_rfm(hechos(), as.Date(corte())))
    .gc_tarjetas_server(.FID_CAJAS, shiny::reactive({
      r <- rfm()
      m <- if (isTRUE(input$modelar > 0)) modelo() else NULL
      valido <- !is.null(m$clientes) && is.null(m$error)
      list(rfm_clientes = nrow(r), rfm_riesgo = sum(r$SegmentoRFM == "Leales en riesgo"),
        btyd_riesgo = if (valido) sum(m$clientes$RiesgoModelo == "Alto") else NA_real_,
        clv_margen = if (valido) .com_suma(m$clientes$CLVMargen) else NA_real_)
    }), list(clv_margen = "dinero"))
    .crm_vancouver_server("rfm", rfm,
      columnas = c("CliNitPpal", "Cliente", "LinNegCod", "RecenciaDias", "Compras", "Sacos", "Margen",
        "R", "F", "M", "SegmentoRFM"), col_labels = c(CliNitPpal = "NIT principal", LinNegCod = "Línea",
      RecenciaDias = "Días sin compra", Frecuencia = "Recompras observadas", SegmentoRFM = "Segmento RFM"))
    .crm_vancouver_server("btyd", shiny::reactive({
      m <- modelo(); shiny::validate(shiny::need(is.null(m$error), m$error)); m$clientes
    }), columnas = c("CliNitPpal", "Cliente", "LinNegCod", "ProbActivo", "RiesgoModelo", "ComprasEsperadas",
      "CLVMargen", "HorizonteMeses", "SegmentoRFM"),
      col_labels = c(CliNitPpal = "NIT principal", ProbActivo = "Probabilidad de actividad",
      CLVMargen = "CLV de margen COP", ComprasEsperadas = "Compras futuras esperadas"),
      col_specs = list(ProbActivo = list(format = list(percent = TRUE, digits = 1))))
    .crm_vancouver_server("modelo_parametros", shiny::reactive({
      m <- modelo(); shiny::validate(shiny::need(is.null(m$error), m$error)); m$parametros
    }))
    output$estado_modelo <- .crm_render_ui({
      if (is.null(input$modelar) || input$modelar < 1) {
        return(.crm_nota("Pulse Calcular para ajustar BG/NBD y estimar CLV."))
      }
      m <- modelo()
      if (!is.null(m$error)) return(.crm_nota(m$error))
      .crm_nota(c(paste("Modelo ajustado al", m$corte, "con", m$parametros$Clientes, "clientes/línea."),
        if (isTRUE(as.logical(m$parametros$EnLimite))) {
          "Parámetros en un límite: interprete las predicciones con cautela."
        }))
    })
    output$modelo_grafico <- .crm_render_plotly({
      m <- modelo(); shiny::validate(shiny::need(is.null(m$error), m$error)); x <- m$clientes
      plotly::plot_ly(x, x = ~ProbActivo, y = ~CLVMargen, type = "scatter", mode = "markers",
        text = ~paste(htmltools::htmlEscape(Cliente), "<br>NIT:", CliNitPpal, "<br>Línea:", LinNegCod,
          "<br>Compras esperadas:",
          round(ComprasEsperadas, 2)),
        hovertemplate = "%{text}<br>Actividad: %{x:.1%}<br>CLV: $%{y:,.0f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Probabilidad de actividad"), yaxis = list(title = "CLV de margen COP"))
    })
    datos <- shiny::reactive({
      shiny::req(corte(), is.finite(input$umbral), input$umbral > 0)
      x <- .fid_preparar(hechos(), .com_clientes(clientes(), as.Date(corte()), filtros()),
        as.Date(corte()), input$umbral)
      x <- x[x$Estado %in% input$estado, , drop = FALSE]
      x
    })
    resumen <- shiny::reactive(datos() %>% dplyr::group_by(Estado) %>%
      dplyr::summarise(ClientesLinea = dplyr::n(), SacosAnho = .com_suma(SacosAnho), .groups = "drop"))
    .crm_vancouver_server("resumen", data = resumen)
    .crm_vancouver_server("clientes", data = datos,
      col_labels = c(CliNitPpal = "NIT principal", PerRazSoc = "Cliente", LinNegCod = "Línea de negocio",
      SSPpto = "Presupuesto anual de sacos", MNFCCPpto = "Presupuesto anual de margen",
      PrimeraObservada = "Primera compra observada", UltimaCompra = "Última compra",
      DiasSinCompra = "Días sin compra", SacosAnho = "Sacos del año", MargenAnho = "Margen del año",
      ClasificacionPresupuesto = "Presupuesto", TeniaOtraLinea = "Con historia previa"))
    output$descargar <- shiny::downloadHandler(filename = function() "Fidelidad_riesgo.xlsx",
      content = function(file) {
        hojas <- list(Senales = .crm_nits_tabla(datos()), RFM = .crm_nits_tabla(rfm()))
        if (isTRUE(shiny::isolate(input$modelar) > 0) && is.null(modelo()$error)) {
          hojas$BTYD_CLV <- .crm_nits_tabla(modelo()$clientes)
          hojas$Parametros <- modelo()$parametros; hojas$Validacion <- modelo()$validacion
        }
        writexl::write_xlsx(hojas, file)
      })
  })
}
