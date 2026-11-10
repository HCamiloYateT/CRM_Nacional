# Presentación de cartera ----
.CAR_VISTAS <- c("Resumen", "Vencida", "Proyección de pagos", "Comportamiento de pago",
  "Rotación", "Recaudos", "Detalle")
.car_specs <- function() {
  porcentaje <- c("PctSaldo", "PctFacturas", "PctSacos", .CAR_TRAMOS)
  specs <- stats::setNames(lapply(porcentaje, function(n) list(format = list(percent = TRUE, digits = 1))), porcentaje)
  etiquetas <- c(SaldoCOP = "Saldo COP", PctSaldo = "% saldo", PctFacturas = "% facturas",
    PctSacos = "% sacos", SacosOrigen = "Sacos de origen", FormaPago = "Forma de pago",
    NumeroPedido = "Número de pedido", FechaFactura = "Fecha de factura", FechaVence = "Vencimiento",
    UltimaGestion = "Última gestión", Gestion = "Gestión", DiasVencido = "Días vencidos",
    PerRazSoc = "Cliente", FcnFec = "Fecha de factura", FecVto = "Vencimiento", FecPag = "Última fecha de pago",
    DiasPago = "Días hasta el pago", DiasAtraso = "Atraso al pagar (días)", DiasMoraActual = "Mora actual (días)",
    EstadoPago = "Estado de pago", DiasPagoPonderados = "Rotación (días)", DiasAnteriores = "Días año anterior",
    CambioDias = "Cambio (días)", Comparacion = "Comparación anual", ValorPagadoInferido = "Valor pagado COP",
    Anho = "Año de factura", Periodo = "Inicio del periodo", Acumulado = "Acumulado COP", PdcCod = "Pedido")
  for (n in names(etiquetas)) specs[[n]]$name <- etiquetas[[n]]
  for (n in c("Facturas", "Clientes", "Anho")) specs[[n]]$format <- list(digits = 0, separators = FALSE)
  for (n in c("SaldoCOP", "Valor", "Saldo", "Acumulado", "ValorPagadoInferido")) {
    specs[[n]]$format <- list(prefix = "$", digits = 0)
  }
  specs
}
.car_buscar <- function(x, texto) {
  if (is.null(texto) || !nzchar(trimws(texto)) || !nrow(x)) return(x)
  coincide <- vapply(seq_len(nrow(x)), function(i) any(vapply(x, function(v) {
    grepl(tolower(trimws(texto)), tolower(as.character(v[i])), fixed = TRUE)
  }, logical(1)), na.rm = TRUE), logical(1))
  x[coincide, , drop = FALSE]
}
CarteraComercialUI <- function(id) {
  ns <- shiny::NS(id)
  historico <- sprintf("input['%s'] == 'Comportamiento de pago' || input['%s'] == 'Rotación' || ",
    ns("vista"), ns("vista"))
  historico <- paste0(historico, sprintf("input['%s'] == 'Recaudos'", ns("vista")))
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("actualizar", "Actualizar",
      variant = "secondary", icon = "arrows-rotate", ns = ns)),
    racafeForms::racafeFormRow(columns = 3, responsive = "auto",
      .crm_select("corte", "Corte disponible de cartera", choices = character(), ns = ns),
      .crm_select("negocio", "Negocio del saldo al corte", choices = character(),
        multiple = TRUE, ns = ns),
      .crm_select("vista", "Consultar", choices = .CAR_VISTAS, empty = FALSE, ns = ns)),
    shiny::conditionalPanel(historico,
      racafeForms::racafeFormRow(columns = 3, responsive = "auto",
        racafeForms::racafeInputDateRange("facturas", "Fecha de factura (históricos)",
          start = Sys.Date() - 365, end = Sys.Date(), ns = ns),
        .crm_select("cliente", "Clientes (históricos)", choices = character(),
          multiple = TRUE, ns = ns),
        shiny::conditionalPanel(sprintf("input['%s'] == 'Comportamiento de pago'", ns("vista")),
          .crm_select("concepto", "Distribución por", choices = c("Valor", "Facturas", "Sacos"),
            empty = FALSE, ns = ns)))),
    shiny::conditionalPanel(sprintf("input['%s'] == 'Proyección de pagos'", ns("vista")),
      .crm_select("agrupacion", "Vencimientos por",
        choices = c("Semana" = "semana", "Mes" = "mes"), empty = FALSE, ns = ns)),
    shiny::uiOutput(ns("fuente")),
    shiny::conditionalPanel(sprintf("output['%s']", ns("saldo_disponible")),
    shiny::fluidRow(lapply(c("saldo", "documentos", "sacos"), function(k) {
      shiny::column(4, .crm_caja_output(ns(k)))
    })),
    .crm_nota("Los indicadores corresponden exclusivamente al corte y negocios seleccionados.")),
    shiny::conditionalPanel(sprintf("output['%s']", ns("detalle_disponible")),
    shiny::uiOutput(ns("nota_vista")), shiny::uiOutput(ns("grafico_ui")),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("tabla")),
    .crm_nota(c("Seleccione una fila o TOTAL para consultar su detalle.",
      "Importes en COP; sacos en la unidad de origen de Tesorería, sin convertir a 70 kg.",
      "TOTAL recalcula clientes y facturas distintas; sus participaciones pueden solaparse entre grupos."))))
}
CarteraComercial <- function(id, historico, clientes, nit_mapa, filtros, es_admin) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    detalle_clave <- shiny::reactiveVal(NULL)
    fechas_iniciadas <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$actualizar, revision(revision() + 1L))
    fuente <- shiny::reactive({
      revision()
      historico()
    })
    catalogo <- shiny::reactive(.com_clientes(clientes(), Sys.Date(), filtros()))
    saldos <- shiny::reactive({
      x <- fuente()$cartera
      if (!is.data.frame(x)) return(NULL)
      .car_scope(x, catalogo(), nit_mapa)
    })
    cortes_disponibles <- shiny::reactive({
      x <- fuente()$cartera
      if (!is.data.frame(x)) return(as.Date(character()))
      sort(unique(.car_fecha(x$TCEFch[!is.na(x$TCEFch)])), decreasing = TRUE, na.last = NA)
    })
    facturacion <- shiny::reactive({
      h <- fuente()
      shiny::validate(shiny::need(is.data.frame(h$facturacion), "La fuente no contiene facturación histórica."))
      .car_scope(h$facturacion, catalogo(), nit_mapa)
    })
    shiny::observe({
      x <- fuente()$cartera
      cortes <- cortes_disponibles()
      actual <- shiny::isolate(input$corte)
      .crm_update_select(session, "corte", choices = as.character(cortes),
        selected = if (length(actual) && actual %in% as.character(cortes)) actual else as.character(head(cortes, 1)))
      negocios <- sort(unique(as.character(x$Negocio[!is.na(x$Negocio)])))
      .crm_update_select(session, "negocio", choices = negocios,
        selected = .crm_seleccion_opciones(input, session, "negocio", negocios))
    })
    shiny::observe({
      x <- facturacion()
      directorio <- x %>% dplyr::group_by(NIT) %>%
        dplyr::summarise(Cliente = dplyr::first(PerRazSoc), .groups = "drop")
      opciones <- stats::setNames(directorio$NIT, paste(directorio$Cliente, directorio$NIT, sep = " | "))
      .crm_update_select(session, "cliente", choices = opciones,
        selected = .crm_seleccion_opciones(input, session, "cliente", opciones))
      if (!shiny::isolate(fechas_iniciadas()) && any(!is.na(x$FcnFec))) {
        fin <- .gc_fecha_max(as.Date(x$FcnFec))
        racafeForms::racafeUpdateDateRange(session, "facturas",
          start = max(.gc_fecha_min(as.Date(x$FcnFec)), fin - 365),
          end = fin, min = .gc_fecha_min(as.Date(x$FcnFec)), max = fin)
        fechas_iniciadas(TRUE)
      }
    })
    corte <- shiny::reactive({
      shiny::validate(shiny::need(length(input$corte) == 1L &&
        input$corte %in% as.character(cortes_disponibles()), "Seleccione un corte disponible de cartera."))
      as.Date(input$corte)
    })
    datos <- shiny::reactive({
      x <- .car_preparar(saldos(), corte())
      x[x$Negocio %in% input$negocio, , drop = FALSE]
    })
    pagos <- shiny::reactive({
      shiny::validate(shiny::need(length(input$facturas) == 2L && !anyNA(input$facturas),
        "Seleccione un rango válido de fecha de factura."))
      x <- .car_pagos(facturacion(), as.Date(input$facturas[1]), as.Date(input$facturas[2]))
      x[x$NIT %in% input$cliente & !x$FueraPeriodo, , drop = FALSE]
    })
    vista <- shiny::reactive(.com_elegir(input$vista, .CAR_VISTAS, "Resumen"))
    estado <- shiny::reactive({
      h <- fuente()
      if (!is.data.frame(h$cartera)) {
        return("No hay una fuente de cartera disponible. Actualice el data prep o cargue el histórico autorizado.")
      }
      if (!length(cortes_disponibles())) {
        return("La fuente de cartera no contiene cortes válidos. Actualice el data prep.")
      }
      if (vista() %in% c("Comportamiento de pago", "Rotación", "Recaudos")) {
        if (!is.data.frame(h$facturacion)) return("La fuente no contiene facturación histórica.")
        if (!length(input$cliente)) return("No hay clientes seleccionados para la consulta histórica.")
        if (length(input$facturas) != 2L || anyNA(input$facturas)) {
          return("Seleccione un rango de fecha de factura válido.")
        }
        if (!nrow(pagos())) return("No hay facturas para los clientes y fechas seleccionados.")
        return(NULL)
      }
      if (!length(input$corte) || !input$corte %in% as.character(cortes_disponibles())) {
        return("Seleccione un corte disponible de cartera.")
      }
      if (!nrow(saldos())) {
        return(paste("La fuente tiene cortes disponibles, pero no hay cartera que coincida con el alcance",
          "y los filtros actuales de asesor, segmento y línea. Revise los filtros aplicados."))
      }
      if (!length(input$negocio)) {
        return("No hay negocios seleccionados. Seleccione al menos uno para consultar saldos.")
      }
      if (!nrow(datos())) return("No hay cartera para el corte, negocios y filtros seleccionados.")
      NULL
    })
    output$detalle_disponible <- shiny::reactive(is.null(estado()))
    output$saldo_disponible <- shiny::reactive({
      is.data.frame(fuente()$cartera) && length(input$corte) == 1L &&
        input$corte %in% as.character(cortes_disponibles()) && length(input$negocio) > 0L && nrow(datos()) > 0L
    })
    shiny::outputOptions(output, "detalle_disponible", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "saldo_disponible", suspendWhenHidden = FALSE)
    output$fuente <- .crm_render_ui({
      h <- fuente()
      mensaje <- estado()
      shiny::tagList(
        if (is.data.frame(h$cartera)) .crm_nota(c(
          paste("Fuente:", if (is.null(h$origen)) "Archivo histórico de cartera" else h$origen),
          if (length(input$corte)) paste("Corte:", input$corte),
          if (!is.null(h$importado_en)) paste("Generado:", h$importado_en), h$aviso)),
        if (!is.null(mensaje)) racafeForms::racafeFormMessage(mensaje, type = "empty"))
    })
    .gc_tarjetas_server(c(saldo = "Saldo al corte · COP", documentos = "Facturas al corte", sacos = "Sacos al corte"),
      shiny::reactive({
        m <- .car_metricas(datos())
        list(saldo = m$SaldoCOP, documentos = m$Facturas, sacos = m$SacosOrigen)
      }), list(saldo = "dinero", sacos = "numero"))
    tabla <- shiny::reactive({
      x <- if (vista() %in% c("Resumen", "Vencida", "Proyección de pagos", "Detalle")) datos() else NULL
      switch(vista(),
        "Resumen" = .car_resumen(x, total = TRUE),
        "Vencida" = .car_resumen(x[x$Tipo == "Vencida", ], "Rango", total = TRUE),
        "Proyección de pagos" = .car_vencimientos(x, .com_elegir(input$agrupacion, c("mes", "semana"), "semana")),
        "Comportamiento de pago" = .car_comportamiento(pagos(),
          .com_elegir(input$concepto, c("Valor", "Facturas", "Sacos"), "Valor")),
        "Rotación" = .car_rotacion_tabla(pagos()),
        "Recaudos" = {
          p <- pagos()
          p[intersect(c("NIT", "PerRazSoc", "Factura", "PdcCod", "FcnFec", "FecVto", "FecPag",
            "EstadoPago", "DiasPago", "DiasAtraso", "DiasMoraActual", "Valor", "Saldo"), names(p))]
        },
        "Detalle" = .car_detalle(x))
    })
    output$nota_vista <- .crm_render_ui({
      texto <- switch(vista(),
        "Proyección de pagos" = c("Calendario contractual de cartera por vencer; no predice recaudos.",
          "Tabla y gráfico en COP, con el mismo acumulado cronológico. Semanas de lunes a domingo."),
        "Comportamiento de pago" = c("Se filtra por fecha de factura y cliente, independientemente del corte/negocio.",
          "Solo facturas con saldo cero y fecha de pago válida. Mora = pago − vencimiento, mínimo cero.",
          "Valor se expresa en millones de COP; los tramos son proporciones del total por NIT y forma de pago.",
          paste("Sin pago total acreditado:", sum(!pagos()$Pagada), "facturas visibles.")),
        "Rotación" = c("Se filtra por fecha de factura y cliente, independientemente del corte/negocio.",
          "Días factura → pago ponderados por valor positivo, solo facturas totalmente pagadas con fecha válida.",
          "Se compara exclusivamente con el año inmediatamente anterior del mismo NIT; sin comparable, estilo neutro."),
        "Recaudos" = c("Consulta de facturas por fecha de factura y cliente; incluye facturas sin pago acreditado.",
          "La fecha es la del último recibo disponible. No representa cada abono ni un flujo de caja diario.",
          "Días de pago: factura → pago. Días de atraso: vencimiento → pago.",
          "Mora actual: saldo pendiente al día de hoy."),
        "Vencida" = c("Mora calculada entre el corte y el vencimiento, de 1–7 a más de 30 días.",
          "Se omiten gestiones posteriores al corte; la fuente no permite reconstruir la gestión anterior."),
        "Resumen" = "Participaciones sobre saldo, facturas distintas y sacos de todo el universo visible al corte.",
        "Detalle" = "El valor y el saldo se conservan numéricos hasta su presentación y exportación.")
      .crm_nota(texto)
    })
    output$grafico_ui <- .crm_render_ui({
      if (vista() == "Proyección de pagos") .crm_plot_output(session$ns("grafico"), height = "330px")
    })
    output$grafico <- .crm_render_plotly({
      shiny::req(vista() == "Proyección de pagos")
      x <- tabla()
      shiny::validate(shiny::need(nrow(x) > 0, "No hay cartera por vencer para esta selección."))
      plotly::plot_ly(x, x = ~Periodo, y = ~SaldoCOP, type = "scatter", mode = "lines+markers",
        name = "Saldo COP", line = list(color = "#63666A")) %>%
        plotly::add_trace(y = ~Acumulado, name = "Acumulado COP", line = list(color = "#DA291C")) %>%
        plotly::layout(title = "Vencimientos contractuales", xaxis = list(title = "Inicio del periodo"),
          yaxis = list(title = "COP", tickprefix = "$"), legend = list(orientation = "h"))
    })
    seleccion <- .crm_vancouver_server("tabla", tabla, col_specs = .car_specs(), modo_seleccion = "fila")
    detalle <- shiny::reactive({
      clave <- detalle_clave()
      shiny::req(!is.null(clave))
      if (clave$vista %in% c("Resumen", "Vencida")) {
        x <- datos()
        if (clave$vista == "Vencida") x <- x[x$Tipo == "Vencida", ]
        if (clave$id != "TOTAL") x <- x[x[[clave$dimension]] == clave$id, ]
        x <- .car_detalle(x)
      } else {
        x <- tabla()
        if (vista() != clave$vista) return(x[0, , drop = FALSE])
        for (n in names(clave$llave)) {
          x <- x[!is.na(x[[n]]) & as.character(x[[n]]) == clave$llave[[n]], , drop = FALSE]
        }
      }
      .car_buscar(x, input$buscar_detalle)
    })
    shiny::observeEvent(seleccion$seleccion(), {
      fila <- seleccion$seleccion()$data
      dimension <- if (vista() == "Resumen") "Tipo" else if (vista() == "Vencida") "Rango" else NULL
      detalle_clave(list(vista = vista(), dimension = dimension,
        id = if (!is.null(dimension)) fila[[dimension]][1] else NULL,
        llave = lapply(fila[intersect(c("NIT", "Factura", "Anho", "FormaPago", "Periodo", "NumeroPedido"),
          names(fila))], as.character)))
      shiny::updateTextInput(session, "buscar_detalle", value = "")
      shiny::showModal(racafeForms::racafeModalDetail(title = "Detalle de cartera", size = "ventana",
        racafeForms::racafeInputText("buscar_detalle", "Buscar en el detalle", ns = session$ns),
        shiny::uiOutput(session$ns("totales_detalle")), .crm_descarga_ui("descargar_detalle", session$ns),
        .crm_tabla_ui(session$ns("detalle"), searchable = FALSE),
        .crm_nota("Totales y Excel responden a la búsqueda del detalle, incluyendo todas sus páginas."),
        footer = racafeForms::racafeModalFooter()))
      seleccion$limpiar()
    }, ignoreInit = TRUE)
    .crm_vancouver_server("detalle", detalle, col_specs = .car_specs(), searchable = FALSE)
    output$totales_detalle <- .crm_render_ui({
      x <- detalle()
      .crm_nota(c(paste("Filas visibles:", nrow(x)),
        if ("Valor" %in% names(x)) paste("Valor COP:", scales::dollar(.com_suma(x$Valor))),
        if ("Saldo" %in% names(x)) paste("Saldo COP:", scales::dollar(.com_suma(x$Saldo)))))
    })
    output$descargar_detalle <- shiny::downloadHandler(filename = function() "Detalle_cartera.xlsx",
      content = function(file) writexl::write_xlsx(.crm_nits_tabla(detalle()), file))
    .gc_descarga(output, tabla, "Cartera")
    list(datos = datos, pagos = pagos, tabla = tabla, detalle = detalle)
  })
}
