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
  shiny::tagList(shiny::uiOutput(ns("fuente")),
    .gc_tarjetas_ui(ns, c(saldo = "Saldo total · COP", vigente = "Saldo vigente · COP", vencido = "Saldo vencido · COP",
      porcentaje = "Exposición positiva vencida", mora = "Mayor mora · días", documentos = "Facturas con saldo")),
    bs4Dash::tabsetPanel(id = ns("cartera_tab"),
    tabPanel("Cosechas de crédito", .crm_plot_output(ns("cosechas")),
      shiny::uiOutput(ns("limite_cosechas")), .crm_tabla_ui(ns("cosechas_detalle"))),
    tabPanel("Antigüedad y exposición", .crm_plot_output(ns("antiguedad")),
      .crm_tabla_ui(ns("rangos")), .crm_plot_output(ns("concentracion")), .crm_tabla_ui(ns("clientes"))),
    tabPanel("Comportamiento por línea de negocio", .crm_plot_output(ns("lineas")),
      .crm_tabla_ui(ns("negocios"))),
    tabPanel("Cambios de mora entre los últimos cortes", .crm_plot_output(ns("deterioro")),
      .crm_tabla_ui(ns("deterioro_detalle"))),
    tabPanel("Pagos documentados y condiciones de crédito", .crm_tabla_ui(ns("pagos"))),
    tabPanel("Documentos al corte observado", .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("detalle")))),
    .crm_nota_metodologica(c("Corte: última fotografía disponible hasta la fecha final del controlbar.",
      "El porcentaje vencido usa saldos positivos; los saldos a favor permanecen en el saldo total.",
      "Las reducciones de saldo no se interpretan como pagos. La fuente de recibos aporta la última fecha, no cada abono.",
      "No se distribuyen saldos entre categorías o productos. El alcance comercial conserva cliente y línea.")))
}
CarteraComercial <- function(id, historico, clientes, nit_mapa, filtros, es_admin) {
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
    datos <- shiny::reactive(.car_preparar(saldos(), corte()))
    facturas <- shiny::reactive({
      f <- fuente()$facturacion
      shiny::validate(shiny::need(is.data.frame(f), "La fuente no contiene facturas históricas."))
      .car_facturas_autorizadas(f, fuente()$cartera, catalogo(), nit_mapa)
    })
    cosechas <- shiny::reactive(.car_cosechas(saldos(), facturas(), hasta()))
    output$fuente <- .crm_render_ui(.crm_nota(paste("Corte observado:", corte(), "· Fuente:", fuente()$origen)))
    .gc_tarjetas_server(c(saldo = "Saldo total · COP", vigente = "Saldo vigente · COP", vencido = "Saldo vencido · COP",
      porcentaje = "Exposición positiva vencida", mora = "Mayor mora · días", documentos = "Facturas con saldo"),
      shiny::reactive({
        d <- datos(); positivos <- d[is.finite(d$Saldo) & d$Saldo > 0, ]
        conocido <- !is.na(positivos$DiasVencido)
        vencido <- if (all(conocido)) sum(positivos$Saldo[positivos$DiasVencido > 0]) else NA_real_
        list(saldo = .com_suma(d$Saldo), vigente = if (all(conocido)) sum(positivos$Saldo[positivos$DiasVencido <= 0]) else NA_real_,
          vencido = vencido, porcentaje = .gc_ratio(vencido, sum(positivos$Saldo)),
          mora = if (any(conocido)) max(pmax(positivos$DiasVencido[conocido], 0)) else NA_real_,
          documentos = .car_distintos(positivos$Documento))
      }), list(saldo = "dinero", vigente = "dinero", vencido = "dinero", porcentaje = "porcentaje"))
    output$cosechas <- .crm_render_plotly(.car_grafico_cosechas(cosechas()))
    output$limite_cosechas <- .crm_render_ui(.crm_nota(c(cosechas()$limitacion, attr(facturas(), "limite_alcance"))))
    .crm_vancouver_server("cosechas_detalle", shiny::reactive(cosechas()$observaciones))
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
