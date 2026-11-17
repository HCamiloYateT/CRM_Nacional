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
    Anho = "Año de factura", Periodo = "Inicio del periodo", Acumulado = "Acumulado COP", PdcCod = "Pedido", Factura = "Número de factura", Original = "Valor original (COP)", SaldoObservado = "Saldo al corte (COP)",
    ValorOriginal = "Valor original de la cosecha (COP)", SaldoPendiente = "Saldo pendiente (COP)",
    FacturasCohorte = "Facturas de la cosecha", FacturasObservadas = "Facturas con saldo observado",
    Emision = "Fecha de factura", Vencimiento = "Fecha de vencimiento", Edad = "Edad (meses calendario)",
    Vencida = "Vencida al corte")
  for (n in names(etiquetas)) specs[[n]]$name <- etiquetas[[n]]
  for (n in c("Facturas", "Clientes", "Anho", "Edad", "FacturasCohorte", "FacturasObservadas")) specs[[n]]$format <- list(digits = 0, separators = FALSE)
  for (n in c("SaldoCOP", "Valor", "Saldo", "Acumulado", "ValorPagadoInferido", "Original", "SaldoObservado", "ValorOriginal", "SaldoPendiente")) {
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
  shiny::tagList(shiny::uiOutput(ns("fuente")),
    .gc_tarjetas_ui(ns, c(saldo = "Saldo total · COP", vigente = "Saldo vigente · COP", vencido = "Saldo vencido · COP",
      porcentaje = "% de cartera vencida", mora = "Mayor mora · días", documentos = "Facturas con saldo")),
    bs4Dash::tabsetPanel(id = ns("cartera_tab"), selected = "Resumen de cartera",
    tabPanel("Resumen de cartera",
      .crm_select("dimension_saldo", "Ver cartera por", choices = c("Línea de negocio" = "Negocio",
        "Categoría" = "Categoria", "Producto" = "Producto"), selected = "Negocio", ns = ns),
      .crm_plot_output(ns("grafico_saldos")), .crm_tabla_ui(ns("resumen_saldos")),
      .crm_nota(c("Abierta: dinero que aún le deben. Vencida: la parte que ya pasó su fecha de pago.",
        "El porcentaje vencido permite identificar dónde conviene concentrar la gestión de cobro.",
        "Cuando no se puede identificar un único producto, se muestra SIN CLASIFICAR; el saldo no se reparte."))),
    tabPanel("Cosechas de crédito", racafeForms::racafeForm(variant = "plain",
      racafeForms::racafeFormRow(columns = 4,
        .crm_select("origen_cosecha", "Definir cosecha por", choices = c("Fecha de factura" = "Emision",
          "Fecha de vencimiento" = "Vencimiento"), selected = "Emision", ns = ns),
        racafeForms::racafeInputDateRange("periodo_cosecha", "Periodo de origen",
          start = as.Date(format(Sys.Date(), "%Y-01-01")), end = Sys.Date(), ns = ns),
        .crm_select("dimension_cosecha", "Comparar por", choices = c("Total" = "Total", "Línea" = "Linea",
          "Segmento" = "Segmento", "Asesor" = "Asesor", "Unidad comercial" = "UnidadComercial"),
          selected = "Total", ns = ns))),
      .crm_plot_output(ns("matriz_cosechas")), .crm_plot_output(ns("cosechas")),
      shiny::uiOutput(ns("limite_cosechas")),
      .crm_disclosure("Valores y cobertura de las celdas", .crm_tabla_ui(ns("cosechas_celdas"))),
      .crm_nota("Seleccione una celda o punto para consultar las facturas que explican ese valor."),
      .crm_tabla_ui(ns("cosechas_detalle"))),
    tabPanel("Antigüedad y exposición", .crm_plot_output(ns("antiguedad")),
      .crm_tabla_ui(ns("rangos")), .crm_plot_output(ns("concentracion")), .crm_tabla_ui(ns("clientes"))),
    tabPanel("Comportamiento por línea de negocio", .crm_plot_output(ns("lineas")),
      .crm_tabla_ui(ns("negocios"))),
    tabPanel("Cambios de mora entre los últimos cortes", .crm_plot_output(ns("deterioro")),
      .crm_tabla_ui(ns("deterioro_detalle"))),
    tabPanel("Pagos confirmados", .crm_tabla_ui(ns("pagos")),
      .crm_nota("Aquí solo se consideran facturas con pago total registrado. Sin fecha no se confirma un pago.")),
    tabPanel("Documentos al corte observado", .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("detalle")))),
    .crm_nota_metodologica(c("Las cifras corresponden al último reporte disponible hasta la fecha que eligió.",
      "El porcentaje vencido compara lo atrasado con todo lo que está por cobrar.",
      "Una reducción del saldo puede tener varias causas. Solo mostramos pagos cuando están confirmados.",
      "Use los filtros generales para consultar los clientes y líneas que tiene a cargo.")))
}
CarteraComercial <- function(id, historico, clientes, nit_mapa, filtros, es_admin, maestro = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    hasta <- shiny::reactive(as.Date(filtros()$fecha[[2]]))
    catalogo <- shiny::reactive(.com_clientes(clientes(), hasta(), filtros()))
    fuente <- shiny::reactive({
      h <- historico()
      shiny::validate(shiny::need(is.list(h) && is.data.frame(h$cartera),
        "No hay una fuente de cartera disponible. Actualice el ETL."))
      h
    })
    saldos <- shiny::reactive(.car_scope(fuente()$cartera, catalogo(), nit_mapa))
    cortes <- shiny::reactive({
      x <- .car_fecha(saldos()$TCEFch); sort(unique(x[!is.na(x) & x <= hasta()]))
    })
    corte <- shiny::reactive({ shiny::validate(shiny::need(length(cortes()) > 0L,
      "No hay cortes dentro del alcance comercial y hasta la fecha seleccionada.")); max(cortes()) })
    datos <- shiny::reactive({
      x <- .car_preparar(saldos(), corte())
      x$Categoria <- x$Producto <- rep("SIN CLASIFICAR", nrow(x))
      if (!is.null(maestro) && all(c("LinNegCod", "LinProCod") %in% names(x))) {
        m <- maestro()
        m$LinNegCod <- ifelse(m$LinNegCod == 20000, 21000, m$LinNegCod)
        meta <- m %>% dplyr::group_by(LinNegCod, LinProCod) %>% dplyr::summarise(
          Categoria = .gc_unico(Categoria), Producto = .gc_unico(Producto), .groups = "drop")
        claves <- function(z) paste(ifelse(z$LinNegCod == 20000, 21000, z$LinNegCod), z$LinProCod)
        i <- match(claves(x), claves(meta))
        for (campo in c("Categoria", "Producto")) {
          x[[campo]] <- dplyr::coalesce(as.character(meta[[campo]][i]), "SIN CLASIFICAR")
        }
      }
      x
    })
    resumen_saldos <- shiny::reactive({
      x <- datos()
      campo <- .com_elegir(input$dimension_saldo, c("Negocio", "Categoria", "Producto"), "Negocio")
      x$Grupo <- as.character(x[[campo]])
      x %>% dplyr::filter(is.finite(Saldo), Saldo > 0) %>% dplyr::group_by(Grupo) %>%
        dplyr::summarise(Abierta = sum(Saldo),
          Vencida = if (anyNA(DiasVencido)) NA_real_ else sum(Saldo[DiasVencido > 0]),
          Facturas = .car_distintos(Documento), .groups = "drop") %>%
        dplyr::mutate(PorcentajeVencido = Vencida / Abierta)
    })
    .crm_vancouver_server("resumen_saldos", resumen_saldos,
      col_labels = c(Grupo = "Grupo", Abierta = "Saldo por cobrar · COP", Vencida = "Saldo vencido · COP",
        PorcentajeVencido = "% vencido"),
      col_specs = list(PorcentajeVencido = list(format = list(percent = TRUE, digits = 1))))
    output$grafico_saldos <- .crm_render_plotly({
      x <- resumen_saldos()
      shiny::validate(shiny::need(nrow(x) > 0L, "No hay cartera por cobrar para estos filtros."))
      plotly::plot_ly(x, x = ~Grupo) %>%
        plotly::add_bars(y = ~Abierta, name = "Por cobrar", marker = list(color = "#0072B2"),
          hovertemplate = "%{x}<br>Por cobrar: $%{y:,.0f}<extra></extra>") %>%
        plotly::add_bars(y = ~Vencida, name = "Parte vencida", marker = list(color = "#D55E00"),
          hovertemplate = "%{x}<br>Vencido: $%{y:,.0f}<extra></extra>") %>%
        plotly::layout(barmode = "group", title = "Cuánto falta por cobrar y cuánto ya está vencido")
    })
    facturas <- shiny::reactive({
      f <- fuente()$facturacion
      shiny::validate(shiny::need(is.data.frame(f), "La fuente no contiene facturas históricas."))
      .car_facturas_autorizadas(f, fuente()$cartera, catalogo(), nit_mapa)
    })
    base_cosechas <- shiny::reactive({
      b <- .car_base_cosechas(saldos(), facturas(), hasta())
      c <- catalogo()
      meta <- c %>% dplyr::mutate(Principal = .car_id(CliNitPpal), LinNegCod = as.character(LinNegCod)) %>%
        dplyr::group_by(Principal, LinNegCod) %>% dplyr::summarise(Asesor = .gc_unico(Asesor),
          Segmento = .gc_unico(Segmento), .groups = "drop")
      b$documentos$Asesor <- NULL; b$documentos$Segmento <- NULL
      b$documentos <- dplyr::left_join(b$documentos, meta, by = c("Principal", "LinNegCod"),
        relationship = "many-to-one")
      b
    })
    shiny::observeEvent(hasta(), {
      shiny::updateDateRangeInput(session, "periodo_cosecha", start = as.Date(format(hasta(), "%Y-01-01")),
        end = hasta(), max = hasta())
    })
    cosechas <- shiny::reactive({
      shiny::req(input$cartera_tab == "Cosechas de crédito", length(input$periodo_cosecha) == 2L,
        input$origen_cosecha %in% c("Emision", "Vencimiento"),
        input$dimension_cosecha %in% c("Total", "Linea", "Segmento", "Asesor", "UnidadComercial"))
      .car_cosechas(corte = hasta(), hasta_origen = as.Date(input$periodo_cosecha[2]),
        origen = input$origen_cosecha %||% "Emision", dimension = input$dimension_cosecha %||% "Total",
        desde = as.Date(input$periodo_cosecha[1]), base = base_cosechas())
    })
    celda_cosecha <- shiny::reactiveVal(NULL)
    shiny::observeEvent(cosechas(), celda_cosecha(NULL), ignoreInit = TRUE)
    for (origen_evento in session$ns(c("car_cosechas", "car_matriz"))) local({
      origen <- origen_evento
      shiny::observeEvent(session$rootScope()$input[[paste0("plotly_click-", origen)]], {
        evento <- plotly::event_data("plotly_click", source = origen)
        clave <- unlist(evento$customdata, use.names = FALSE)[1]
        shiny::req(clave %in% cosechas()$curvas$Celda)
        celda_cosecha(clave)
      })
    })
    output$fuente <- .crm_render_ui(.crm_nota(paste("Corte observado:", corte(), "· Fuente:", fuente()$origen)))
    .gc_tarjetas_server(c(saldo = "Saldo total · COP", vigente = "Saldo vigente · COP", vencido = "Saldo vencido · COP",
      porcentaje = "% de cartera vencida", mora = "Mayor mora · días", documentos = "Facturas con saldo"),
      shiny::reactive({
        d <- datos(); positivos <- d[is.finite(d$Saldo) & d$Saldo > 0, ]
        conocido <- !is.na(positivos$DiasVencido)
        vencido <- if (all(conocido)) sum(positivos$Saldo[positivos$DiasVencido > 0]) else NA_real_
        list(saldo = .com_suma(d$Saldo), vigente = if (all(conocido)) sum(positivos$Saldo[positivos$DiasVencido <= 0]) else NA_real_,
          vencido = vencido, porcentaje = .gc_ratio(vencido, sum(positivos$Saldo)),
          mora = if (any(conocido)) max(pmax(positivos$DiasVencido[conocido], 0)) else NA_real_,
          documentos = .car_distintos(positivos$Documento))
      }), list(saldo = "dinero", vigente = "dinero", vencido = "dinero", porcentaje = "porcentaje"))
    output$cosechas <- .crm_render_plotly(.car_grafico_cosechas(cosechas(), session$ns("car_cosechas")))
    output$matriz_cosechas <- .crm_render_plotly(.car_matriz_cosechas(cosechas(), session$ns("car_matriz")))
    output$limite_cosechas <- .crm_render_ui(.crm_nota(c(cosechas()$limitacion, attr(facturas(), "limite_alcance"))))
    tabla_celdas <- .crm_vancouver_server("cosechas_celdas", shiny::reactive(cosechas()$curvas),
      id_col = "Celda", modo_seleccion = "fila", col_specs = .car_specs(),
      columnas = c("Cohorte", "Grupo", "Edad", "Corte", "ValorOriginal", "SaldoPendiente", "PctSaldo",
        "FacturasCohorte", "FacturasObservadas"))
    shiny::observeEvent(tabla_celdas$seleccion(), {
      seleccion <- tabla_celdas$seleccion()
      shiny::req(seleccion$fila$Celda %in% cosechas()$curvas$Celda)
      celda_cosecha(seleccion$fila$Celda)
    })
    .crm_vancouver_server("cosechas_detalle", shiny::reactive({
      shiny::req(celda_cosecha()); .car_detalle_cosecha(cosechas(), celda_cosecha())
    }), col_specs = .car_specs(), columnas = c("Factura", "Cohorte", "Grupo", "Edad", "Corte", "Emision",
      "Vencimiento", "PerRazSoc", "NIT", "UnidadComercial", "Linea", "Asesor", "Segmento", "Original",
      "SaldoObservado", "Vencida", "Nota"))
    rangos <- shiny::reactive(.car_resumen(datos(), "Rango"))
    .crm_vancouver_server("rangos", rangos, col_specs = .car_specs())
    output$antiguedad <- .crm_render_plotly(plotly::plot_ly(rangos(), x = ~Rango, y = ~SaldoCOP,
      type = "bar", marker = list(color = "#7A2028"), hovertemplate = "%{x}<br>Saldo: %{y:,.0f} COP<extra></extra>"))
    concentracion <- shiny::reactive({
      x <- datos() %>% dplyr::group_by(NIT, PerRazSoc) %>%
        dplyr::summarise(Saldo = .com_suma(Saldo), .groups = "drop") %>% dplyr::arrange(dplyr::desc(Saldo))
      x$Participacion <- .gc_ratio(x$Saldo, .com_suma(x$Saldo)); x
    })
    .crm_vancouver_server("clientes", concentracion)
    output$concentracion <- .crm_render_plotly(plotly::plot_ly(head(concentracion(), 15), x = ~Saldo,
      y = ~reorder(PerRazSoc, Saldo), type = "bar", orientation = "h", marker = list(color = "#495057"),
      hovertemplate = "%{y}<br>Saldo: %{x:,.0f} COP<extra></extra>"))
    negocios <- shiny::reactive(.car_resumen(datos(), "Negocio"))
    .crm_vancouver_server("negocios", negocios, col_specs = .car_specs())
    output$lineas <- .crm_render_plotly(plotly::plot_ly(negocios(), x = ~Negocio, y = ~SaldoCOP,
      type = "bar", marker = list(color = "#C8A77D"), hovertemplate = "%{x}<br>Saldo: %{y:,.0f} COP<extra></extra>"))
    deterioro <- shiny::reactive({
      fechas <- cortes()
      shiny::validate(shiny::need(length(fechas) >= 2L, "Se requieren dos cortes para comparar la mora."))
      resumen <- function(fecha) .car_preparar(saldos(), fecha) %>% dplyr::group_by(NIT, Documento) %>%
        dplyr::summarise(Mora = if (all(is.na(DiasVencido))) NA_real_ else max(pmax(DiasVencido, 0), na.rm = TRUE),
          Saldo = .com_suma(Saldo), .groups = "drop")
      dplyr::inner_join(resumen(tail(fechas, 1L)), resumen(tail(fechas, 2L)[[1]]), by = c("NIT", "Documento"),
        suffix = c("Actual", "Anterior")) %>% dplyr::mutate(CambioMora = MoraActual - MoraAnterior,
          ReduccionSaldoNoConciliada = SaldoAnterior - SaldoActual)
    })
    .crm_vancouver_server("deterioro_detalle", deterioro)
    output$deterioro <- .crm_render_plotly({
      x <- deterioro() %>% dplyr::group_by(NIT) %>% dplyr::summarise(Cambio = if (all(is.na(CambioMora))) NA_real_ else max(CambioMora, na.rm = TRUE), .groups = "drop")
      plotly::plot_ly(x, x = ~NIT, y = ~Cambio, type = "bar", marker = list(color = "#7A2028"),
        hovertemplate = "NIT: %{x}<br>Mayor cambio de mora de documentos comunes: %{y:.0f}<extra></extra>")
    })
    .crm_vancouver_server("pagos", shiny::reactive({
      x <- .car_pagos(facturas(), as.Date(filtros()$fecha[[1]]), hasta(), hoy = hasta())
      x[!x$FueraPeriodo, , drop = FALSE]
    }),
      columnas = c("NIT", "Factura", "FcnFec", "FecVto", "FecPag", "Pagada", "DiasAtraso", "Valor", "Saldo"))
    .crm_vancouver_server("detalle", datos, col_specs = .car_specs())
    .gc_descarga(output, datos, "Cartera_al_corte")
    list(datos = datos, cosechas = cosechas)
  })
}
