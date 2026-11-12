# Vista transversal sobre el maestro ya autorizado y cargado; nunca consulta el histórico completo de nuevo ----
.TZ_CAJAS <- c(solicitados = "Sacos solicitados", asignados = "Sacos asignados", producidos = "Sacos producidos",
  despachados = "Sacos despachados", facturados = "Sacos facturados", cerrados = "Sacos con cierre financiero",
  margen = "Margen financiero · COP", pendiente = "Saldo pendiente · sacos 70 kg")
.tz_preparar <- function(x, pedido) {
  if (length(pedido) != 1L || is.na(pedido)) return(list(datos = x[0, ], indicadores = list(), eventos = data.frame(), conciliacion = data.frame(), facturas = data.frame()))
  d <- x[.crm_id_texto(x$CLPdcCod) %in% .crm_id_texto(pedido), , drop = FALSE]
  if (!nrow(d)) return(list(datos = d, indicadores = list(), eventos = data.frame(), conciliacion = data.frame(), facturas = data.frame()))
  sumar_unidad <- function(campos, valor) {
    if (!all(c(campos, valor) %in% names(d))) return(NA_real_)
    u <- d %>% dplyr::group_by(dplyr::across(dplyr::all_of(campos))) %>%
      dplyr::summarise(Valor = .gc_unico(.data[[valor]]), .groups = "drop")
    .com_suma(u$Valor)
  }
  lineas <- c("CLPdcCod", "CLPdcLin")
  lotes <- d[!is.na(d$CLLotCod) & as.character(d$CLLotCod) != "0", , drop = FALSE]
  por_lote <- function(campo) {
    if (!campo %in% names(lotes) || !nrow(lotes)) return(NA_real_)
    .com_suma((lotes %>% dplyr::group_by(CLLotCod) %>%
      dplyr::summarise(Valor = .gc_unico(.data[[campo]]), .groups = "drop"))$Valor)
  }
  reales <- d$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO")
  solicitados <- sumar_unidad(lineas, "SacosPedido70Linea")
  facturados <- if ("FacturadosConsolidados70" %in% names(d)) por_lote("FacturadosConsolidados70") else
    if ("FacturadosOperativos70" %in% names(d)) por_lote("FacturadosOperativos70") else
    .com_suma(d$SacosFact[reales])
  indicadores <- list(solicitados = solicitados, asignados = por_lote("SacosLote70"),
    producidos = por_lote("SacosProducidos70"), despachados = por_lote("SacosDespachados70"),
    facturados = facturados, cerrados = .com_suma(d$SacosFact[d$EstadoFinanciero == "CERRADO"]),
    margen = .com_suma(d$Margen[d$EstadoFinanciero == "CERRADO"]), pendiente = pmax(solicitados - facturados, 0))
  eventos <- dplyr::bind_rows(lapply(c(Creación = "FechaCreacionPedido", Asignación = "FechaAsignacionLote",
      Producción = "FechaProduccion", Despacho = "FechaDespachoReal", Facturación = "FecFact"), function(campo) {
    if (!campo %in% names(d)) return(NULL)
    evento <- if (campo == "FecFact") ifelse(d$EstadoFinanciero == "CERRADO",
      "Reconocimiento financiero", "Factura registrada") else rep(campo, nrow(d))
    data.frame(Fecha = as.Date(d[[campo]]), Evento = evento, Pedido = .crm_id_texto(d$CLPdcCod),
      Linea = .crm_id_texto(d$CLPdcLin), Lote = .crm_id_texto(d$CLLotCod))
  }))
  eventos$Evento <- dplyr::recode(eventos$Evento, FechaCreacionPedido = "Creación del pedido",
    FechaAsignacionLote = "Asignación del lote", FechaProduccion = "Producción", FechaDespachoReal = "Despacho",
    FecFact = "Factura / reconocimiento financiero")
  eventos <- unique(eventos[!is.na(eventos$Fecha), , drop = FALSE])
  conciliacion <- data.frame(Etapa = c("Pedido → asignación", "Asignación → producción", "Producción → despacho",
    "Despacho → facturación", "Facturación → cierre"), DiferenciaSacos = c(
      indicadores$solicitados - indicadores$asignados, indicadores$asignados - indicadores$producidos,
      indicadores$producidos - indicadores$despachados, indicadores$despachados - indicadores$facturados,
      indicadores$facturados - indicadores$cerrados))
  referencias <- attr(x, "facturas_detalle")
  if (is.data.frame(referencias)) referencias <- referencias[.crm_id_texto(referencias$CLLotCod) %in%
    .crm_id_texto(d$CLLotCod), , drop = FALSE] else referencias <- data.frame()
  if (nrow(referencias)) {
    fechas_factura <- data.frame(Fecha = as.Date(referencias$FecFact),
      Evento = paste("Factura", tolower(referencias$TipoFactura)), Pedido = .crm_id_texto(pedido),
      Linea = NA_character_, Lote = .crm_id_texto(referencias$CLLotCod))
    eventos <- unique(dplyr::bind_rows(eventos, fechas_factura))
  }
  list(datos = d, indicadores = indicadores, eventos = eventos, conciliacion = conciliacion, facturas = referencias)
}
TrazabilidadPedidoUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::uiOutput(ns("encabezado")), .gc_tarjetas_ui(ns, .TZ_CAJAS),
    .crm_plot_output(ns("cronologia")), .crm_disclosure("Conciliación entre etapas",
      .crm_tabla_ui(ns("conciliacion")), .crm_nota(c(
        "Una diferencia no demuestra un error: puede corresponder a ejecución parcial o a un desfase de actualización.",
        "Las cantidades desconocidas se conservan vacías. Los importes financieros solo incluyen meses cerrados."))),
    .crm_disclosure("Líneas y lotes observados", .crm_tabla_ui(ns("detalle"))),
    .crm_disclosure("Facturas nacionales y exteriores", .crm_tabla_ui(ns("facturas"))))
}
TrazabilidadPedido <- function(id, maestro, pedido) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive(.tz_preparar(maestro(), pedido()))
    output$encabezado <- .crm_render_ui({
      x <- datos()$datos
      shiny::validate(shiny::need(nrow(x) > 0L, "No hay un pedido identificado dentro del alcance autorizado."))
      racafeForms::racafeInfoGrid(racafeForms::racafeInfoItem("Pedido", .crm_id_texto(pedido())),
        racafeForms::racafeInfoItem("Cliente", paste(unique(x$PerRazSoc), collapse = " | ")),
        racafeForms::racafeInfoItem("Línea de negocio", paste(unique(x$CLLinNegNo), collapse = " | ")),
        racafeForms::racafeInfoItem("Creación", paste(unique(as.Date(x$FechaCreacionPedido)), collapse = " | ")),
        racafeForms::racafeInfoItem("Precio negociado · COP/kg", paste(unique(x$PrecioNegociadoKg), collapse = " | ")),
        racafeForms::racafeInfoItem("Estado", paste(unique(x$EstadoFinanciero), collapse = " | ")),
        racafeForms::racafeInfoItem("Alcance", "Líneas y productos dentro de los filtros autorizados"))
    })
    .gc_tarjetas_server(.TZ_CAJAS, shiny::reactive(datos()$indicadores))
    .crm_vancouver_server("detalle", shiny::reactive(datos()$datos), columnas = c("CLPdcCod", "CLPdcLin", "CLLotCod",
      "CLCliNit", "LinNegCod", "FechaCreacionPedido", "FechaAsignacionLote", "FechaProduccion", "FechaDespachoReal",
      "FecFact", "FacturasSYSCAFE", "EstadoFinanciero", "SacosFact", "Margen", "PrecioNegociadoOriginal",
      "UnidadPrecioOriginal", "MonedaPrecioOriginal", "PrecioNegociadoKg", "CantidadPedidoOrigen", "UnidadKgPedido",
      "SacosLoteOrigen", "SacosProducidosOrigen", "SacosDespachadosOrigen", "SacosLote70", "SacosProducidos70", "SacosDespachados70", "FacturadosOperativos70", "FacturadosConsolidados70"))
    .crm_vancouver_server("facturas", shiny::reactive(datos()$facturas),
      col_labels = c(CLLotCod = "Lote", FecFact = "Fecha de factura", TipoFactura = "Origen", NumeroFactura = "Factura"))
    .crm_vancouver_server("conciliacion", shiny::reactive(datos()$conciliacion))
    output$cronologia <- .crm_render_plotly({
      d <- datos()$eventos
      shiny::validate(shiny::need(nrow(d) > 0L, "No hay eventos con fecha documentada."))
      d$Detalle <- paste("Pedido", d$Pedido, "· línea", d$Linea, "· lote", d$Lote)
      plotly::plot_ly(d, x = ~Fecha, y = ~Evento, text = ~Detalle, type = "scatter", mode = "markers",
        marker = list(color = "#7A2028", size = 10),
        hovertemplate = "%{y}<br>%{x|%d/%m/%Y}<br>%{text}<extra></extra>")
    })
    list(datos = datos)
  })
}
