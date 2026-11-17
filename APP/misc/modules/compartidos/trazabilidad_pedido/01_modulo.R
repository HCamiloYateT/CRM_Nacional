# Vista transversal sobre el maestro ya autorizado y cargado; nunca consulta el histórico completo de nuevo ----
.TZ_CAJAS <- c(solicitados = "Sacos solicitados", asignados = "Sacos asignados", producidos = "Sacos producidos",
  despachados = "Sacos despachados", facturados = "Sacos facturados", cerrados = "Sacos con cierre financiero",
  margen = "Margen financiero · COP", margen_estimado = "Margen residual estimado · COP",
  margen_comercial = "Margen comercial consolidado · COP", pendiente = "Saldo pendiente · sacos 70 kg")
.tz_preparar <- function(x, pedido, detalle = TRUE) {
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
    if (any(reales)) .com_suma(d$SacosFact[reales]) else NA_real_
  capas <- if ("AplicarFechaEstimada" %in% names(d)) d else .crm_capa_ejecucion(d)
  indicadores <- list(solicitados = solicitados, asignados = por_lote("SacosLote70"),
    producidos = por_lote("SacosProducidos70"), despachados = por_lote("SacosDespachados70"),
    facturados = facturados,
    cerrados = if (any(d$EstadoFinanciero == "CERRADO"))
      .com_suma(d$SacosFact[d$EstadoFinanciero == "CERRADO"]) else NA_real_,
    margen = if (any(d$EstadoFinanciero == "CERRADO"))
      .com_suma(d$Margen[d$EstadoFinanciero == "CERRADO"]) else NA_real_,
    margen_estimado = .com_suma(capas$MetricaMargenProyectado),
    margen_comercial = .com_suma(capas$MetricaMargenEscenario), pendiente = pmax(solicitados - facturados, 0))
  if (!detalle) return(list(datos = d, indicadores = indicadores))
  eventos <- dplyr::bind_rows(lapply(c(Creación = "FechaCreacionPedido", Estimación = "FechaEstimadaPedido",
      Asignación = "FechaAsignacionLote",
      Producción = "FechaProduccion", Despacho = "FechaDespachoReal", Facturación = "FecFact"), function(campo) {
    if (!campo %in% names(d)) return(NULL)
    evento <- if (campo == "FecFact") rep("Factura registrada", nrow(d)) else rep(campo, nrow(d))
    data.frame(Fecha = as.Date(d[[campo]]), Evento = evento, Pedido = .crm_id_texto(d$CLPdcCod),
      Linea = .crm_id_texto(d$CLPdcLin), Lote = .crm_id_texto(d$CLLotCod))
  }))
  eventos$Evento <- dplyr::recode(eventos$Evento, FechaCreacionPedido = "Creación del pedido",
    FechaEstimadaPedido = "Cumplimiento estimado",
    FechaAsignacionLote = "Asignación del lote", FechaProduccion = "Producción", FechaDespachoReal = "Despacho",
    FecFact = "Factura / reconocimiento financiero")
  if ("FecMes" %in% names(d)) {
    cerrados <- d[d$EstadoFinanciero == "CERRADO" & !is.na(d$FecMes), , drop = FALSE]
    eventos <- dplyr::bind_rows(eventos, data.frame(Fecha = as.Date(cerrados$FecMes),
      Evento = rep("Periodo financiero (mes)", nrow(cerrados)), Pedido = .crm_id_texto(cerrados$CLPdcCod),
      Linea = .crm_id_texto(cerrados$CLPdcLin), Lote = .crm_id_texto(cerrados$CLLotCod)))
  }
  eventos <- unique(eventos[!is.na(eventos$Fecha), , drop = FALSE])
  conciliacion <- data.frame(Etapa = c("Pedido → asignación", "Asignación → producción", "Producción → despacho",
    "Despacho → facturación", "Facturación → cierre"), DiferenciaSacos = c(
      indicadores$solicitados - indicadores$asignados, indicadores$asignados - indicadores$producidos,
      indicadores$producidos - indicadores$despachados, indicadores$despachados - indicadores$facturados,
      indicadores$facturados - indicadores$cerrados))
  referencias <- attr(x, "facturas_detalle")
  if (is.data.frame(referencias)) {
    lote_valido <- .crm_id_texto(d$CLLotCod)
    lote_valido <- lote_valido[!is.na(lote_valido) & lote_valido != "0"]
    seleccion <- .crm_id_texto(referencias$CLLotCod) %in% lote_valido
    if (all(c("CLPdcCod", "CLPdcLin") %in% names(referencias))) {
      seleccion <- .crm_id_texto(referencias$CLPdcCod) %in% .crm_id_texto(pedido) &
        (paste(referencias$CLPdcCod, referencias$CLPdcLin) %in% paste(d$CLPdcCod, d$CLPdcLin) |
          seleccion & is.na(referencias$CLPdcLin))
    }
    referencias <- referencias[seleccion, , drop = FALSE]
  } else referencias <- data.frame()
  if (nrow(referencias)) {
    fechas_factura <- data.frame(Fecha = as.Date(referencias$FecFact),
      Evento = paste("Factura", tolower(referencias$TipoFactura)), Pedido = .crm_id_texto(pedido),
      Linea = NA_character_, Lote = .crm_id_texto(referencias$CLLotCod))
    eventos <- unique(dplyr::bind_rows(eventos, fechas_factura))
  }
  list(datos = d, indicadores = indicadores, eventos = eventos, conciliacion = conciliacion, facturas = referencias)
}
# Encabezado compacto compartiendo panel, badges y atributos con la unidad comercial ----
.tz_encabezado <- function(x, pedido,
                           indicadores = .tz_preparar(x, pedido, detalle = FALSE)$indicadores) {
  texto <- function(campos) {
    v <- .gc_columna(x, campos, NA_character_)
    if (is.numeric(v)) v <- .crm_id_texto(v)
    v <- unique(as.character(v[!is.na(v) & nzchar(as.character(v))]))
    if (length(v)) paste(v, collapse = " · ") else "No disponible"
  }
  fecha <- function(campo) {
    v <- as.Date(.gc_columna(x, campo, as.Date(NA)))
    v <- sort(unique(v[!is.na(v) & v > as.Date("1900-01-01")]))
    if (length(v)) paste(format(v, "%d/%m/%Y"), collapse = " · ") else "No disponible"
  }
  precio <- suppressWarnings(as.numeric(.gc_columna(x, "PrecioNegociadoKg", NA_real_)))
  precio <- sort(unique(precio[is.finite(precio) & precio > 0]))
  etapas <- c(facturados = "FACTURADO", despachados = "DESPACHADO", producidos = "PRODUCIDO",
    asignados = "ASIGNADO")
  evidencia <- names(etapas)[vapply(names(etapas), function(k) isTRUE(indicadores[[k]] > 0), logical(1))]
  operativo <- if (length(evidencia)) unname(etapas[evidencia[1]]) else {
    if (!any(!is.na(x$CLLotCod) & x$CLLotCod != 0)) "SIN LOTES REGISTRADOS" else "No disponible"
  }
  if (identical(operativo, "FACTURADO") && isTRUE(indicadores$facturados < indicadores$solicitados)) {
    operativo <- "FACTURACIÓN PARCIAL"
  }
  lineas <- unique(x[c("CLPdcCod", "CLPdcLin")])
  lotes <- unique(.crm_id_texto(x$CLLotCod))
  lotes <- lotes[!is.na(lotes) & lotes != "0"]
  cantidades <- if (all(c("CantidadPedidoOrigen", "UnidadKgPedido") %in% names(x))) {
    x %>% dplyr::group_by(CLPdcLin) %>% dplyr::summarise(
      Cantidad = .gc_unico(CantidadPedidoOrigen), KilosUnidad = .gc_unico(UnidadKgPedido), .groups = "drop")
  } else data.frame()
  cantidad_texto <- if (nrow(cantidades)) paste(vapply(seq_len(nrow(cantidades)), function(i) {
    f <- cantidades[i, ]
    paste0("Línea ", .crm_id_texto(f$CLPdcLin), ": ",
      if (is.na(f$Cantidad)) "NO DISPONIBLE" else format(f$Cantidad, trim = TRUE, scientific = FALSE),
      " unidades originales", if (!is.na(f$KilosUnidad)) paste0(" × ", f$KilosUnidad, " kg"))
  }, character(1)), collapse = " · ") else "No disponible"
  racafeForms::racafePanel(variant = "muted", title = paste("Pedido", .crm_id_texto(pedido)),
    shiny::tags$strong(texto("PerRazSoc")),
    shiny::tags$div(class = "crm-identidad-badges",
      racafeForms::racafeStatusBadge(paste("NIT", texto("CLCliNit")), size = "xs"),
      racafeForms::racafeStatusBadge(paste("LÍNEAS", nrow(lineas)), size = "xs"),
      racafeForms::racafeStatusBadge(paste("LOTES", length(lotes)), size = "xs"),
      lapply(unique(stats::na.omit(x$CLLinNegNo)), function(v) .crm_badge_categoria(v, "linneg")),
      racafeForms::racafeStatusBadge(paste("Estado del pedido:", texto("EstadoPedido")), size = "xs"),
      racafeForms::racafeStatusBadge(paste("Estado operativo:", operativo), size = "xs"),
      racafeForms::racafeStatusBadge(paste("Estado financiero:",
        gsub("_", " ", texto("EstadoFinanciero"))), size = "xs")),
    racafeForms::racafeInfoGrid(
      racafeForms::racafeInfoItem("Cuenta principal · NIT", texto("CliNitPpal")),
      racafeForms::racafeInfoItem("Asesor", texto("Asesor")),
      racafeForms::racafeInfoItem("Segmentos por línea",
        paste(unique(paste(x$CLLinNegNo, x$Segmento, sep = ": ")), collapse = " · ")),
      racafeForms::racafeInfoItem("Referencia del cliente", texto("PdcRefCli")),
      racafeForms::racafeInfoItem("Marcas solicitadas", texto("MarcaPedido")),
      racafeForms::racafeInfoItem("Nombres comerciales solicitados", texto("NombreComercialPedido")),
      racafeForms::racafeInfoItem("Categorías", texto("Categoria")),
      racafeForms::racafeInfoItem("Productos", texto("Producto")),
      racafeForms::racafeInfoItem("Cantidad del pedido · PdcCan", cantidad_texto),
      racafeForms::racafeInfoItem("Fecha de creación", fecha("FechaCreacionPedido")),
      racafeForms::racafeInfoItem("Fecha de cierre del pedido", fecha("FechaCierrePedido")),
      racafeForms::racafeInfoItem("Periodo financiero cerrado", fecha("FechaCortePyG")),
      racafeForms::racafeInfoItem("Fecha de producción", fecha("FechaProduccion")),
      racafeForms::racafeInfoItem("Fecha de despacho", fecha("FechaDespachoReal")),
      racafeForms::racafeInfoItem("Fecha de factura", fecha("FecFact")),
      racafeForms::racafeInfoItem("Fecha estimada", fecha(c("FechaEstimadaPedido", "PdcFecEst"))),
      racafeForms::racafeInfoItem("Precio negociado", if (!length(precio)) "No disponible" else
        paste(paste0("$", formatC(precio, format = "f", digits = 2, big.mark = ","), " / kg"), collapse = " · "))))
}

TrazabilidadPedidoUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::uiOutput(ns("encabezado")), bs4Dash::tabsetPanel(id = ns("pedido_tab"),
    tabPanel("Resumen", .gc_tarjetas_ui(ns, .TZ_CAJAS),
      .crm_nota("Una venta corresponde a un pedido; sus líneas, lotes y facturas no son ventas adicionales.")),
    tabPanel("Líneas", .crm_tabla_ui(ns("lineas"))),
    tabPanel("Mercado al crear el pedido", .crm_tabla_ui(ns("mercado_pedido")),
      .crm_nota(c("Estos eran los indicadores de mercado guardados el día en que se creó cada línea del pedido.",
        "Si no hay información para ese día, lo indicamos; no usamos el valor de otra fecha."))),
    tabPanel("Lotes", .crm_tabla_ui(ns("lotes"))),
    tabPanel("Facturación", .crm_tabla_ui(ns("facturas"))),
    tabPanel("Margen", .crm_tabla_ui(ns("margen")), .crm_nota(c(
      "Este margen es una estimación de lo que falta por vender del pedido.",
      "Se calcula con la información de hoy y puede cambiar cuando se cierre el mes."))),
    tabPanel("Conciliación", .crm_tabla_ui(ns("conciliacion")), .crm_nota(c(
      "Las diferencias pueden corresponder a ejecución parcial o a un desfase de actualización.",
      "Las cantidades desconocidas permanecen vacías; las cantidades se expresan en sacos equivalentes de 70 kg."))),
    tabPanel("Cronología", shiny::uiOutput(ns("cronologia")))))
}

TrazabilidadPedido <- function(id, maestro, pedido) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive(.tz_preparar(maestro(), pedido(), detalle = FALSE))
    contenido <- shiny::reactive({
      shiny::req(!is.null(input$pedido_tab), input$pedido_tab != "Resumen")
      .tz_preparar(maestro(), pedido())
    })
    output$encabezado <- .crm_render_ui({
      x <- datos()$datos
      shiny::validate(shiny::need(nrow(x) > 0L, "No hay un pedido identificado dentro del alcance autorizado."))
      .tz_encabezado(x, pedido(), datos()$indicadores)
    })
    .gc_tarjetas_server(.TZ_CAJAS, shiny::reactive(datos()$indicadores),
      c(stats::setNames(rep(list("numero"), 7L), c("solicitados", "asignados", "producidos", "despachados",
        "facturados", "cerrados", "pendiente")),
        list(margen = "dinero", margen_estimado = "dinero", margen_comercial = "dinero")))
    .crm_vancouver_server("mercado_pedido", shiny::reactive({
      shiny::req(input$pedido_tab == "Mercado al crear el pedido")
      x <- datos()$datos
      fechas <- sort(unique(as.Date(x$FechaCreacionPedido)))
      fechas <- fechas[!is.na(fechas)]
      if (!length(fechas)) return(data.frame())
      fuente <- .in_fuente(racafeBD::CargarDatos, "CRMINDICADORES")
      dplyr::bind_rows(lapply(fechas, function(fecha) {
        .in_mercado_fecha(fuente, as.Date(fecha, origin = "1970-01-01"))
      }))
    }), col_labels = c(FechaActualizacion = "Fecha de creación del pedido", Indicador = "Indicador de mercado",
      Valor = "Valor registrado ese día"))
    lineas <- shiny::reactive({
      shiny::req(input$pedido_tab == "Líneas")
      x <- contenido()$datos
      columnas <- intersect(c("CLCliNit", "MarcaPedidoCod", "MarcaPedido",
        "NombreComercialPedidoCod", "NombreComercialPedido",
        "CLLinNegNo", "Categoria", "Producto", "CantidadPedidoOrigen", "UnidadKgPedido",
        "FechaCreacionPedido", "FechaEstimadaPedido", "EstadoPedido",
        "SacosPedido70Linea", "PrecioNegociadoKg", "MonedaPrecioOriginal"), names(x))
      x %>% dplyr::group_by(CLPdcCod, CLPdcLin) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(columnas), .gc_unico), .groups = "drop")
    })
    lotes <- shiny::reactive({
      shiny::req(input$pedido_tab == "Lotes")
      x <- contenido()$datos
      columnas <- intersect(c("CLPdcCod", "CLPdcLin", "FechaAsignacionLote", "FechaProduccion", "FechaDespachoReal",
        "SacosLote70", "SacosProducidos70", "SacosDespachados70", "FacturadosConsolidados70"), names(x))
      x %>% dplyr::filter(!is.na(CLLotCod), as.character(CLLotCod) != "0") %>%
        dplyr::group_by(CLLotCod) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(columnas), .gc_unico), .groups = "drop")
    })
    .crm_vancouver_server("lineas", lineas, col_labels = c(CLPdcCod = "Código de pedido",
      CLPdcLin = "Línea del pedido", MarcaPedidoCod = "Código de marca", MarcaPedido = "Marca solicitada",
      NombreComercialPedido = "Nombre comercial solicitado", FechaEstimadaPedido = "Fecha estimada",
      CantidadPedidoOrigen = "Cantidad solicitada (unidad original)",
      SacosPedido70Linea = "Sacos solicitados (70 kg)"), enlaces_pedido = FALSE)
    .crm_vancouver_server("lotes", lotes, enlaces_pedido = FALSE,
      subtitulo = shiny::reactive(if (!nrow(lotes())) "Sin registros de lotes disponibles." else NULL))
    .crm_vancouver_server("margen", shiny::reactive({
      shiny::req(input$pedido_tab == "Margen")
      x <- .crm_capa_ejecucion(contenido()$datos)
      x %>% dplyr::group_by(EstadoFinanciero) %>% dplyr::summarise(
        SacosFacturados = .com_suma(MetricaSacosFacturados70),
        MargenFinanciero = .com_suma(MetricaMargenContable),
        MargenProvisional = .com_suma(MetricaMargenAbiertoEstimado),
        MargenEstimado = .com_suma(MetricaMargenProyectado), .groups = "drop")
    }), enlaces_pedido = FALSE)
    .crm_vancouver_server("facturas",
      shiny::reactive({
        shiny::req(input$pedido_tab == "Facturación")
        x <- contenido()$facturas
        shiny::validate(shiny::need(nrow(x) > 0L, "Sin información de facturación disponible."))
        shiny::validate(shiny::need(all(c("PrefijoFactura", "SacosFact70", "EsValida") %in% names(x)),
          "Actualice el ETL para consultar las facturas individuales con sus cantidades y claves documentales."))
        x
      }),
      col_labels = c(CLLotCod = "Código de lote", FecFact = "Fecha de factura", TipoFactura = "Tipo de factura",
        NumeroFactura = "Número de factura", PrefijoFactura = "Prefijo",
        CantidadOriginal = "Cantidad facturada original",
        SacosFact70 = "Sacos facturados (70 kg)", EsValida = "Factura válida", ClaveRepetida = "Clave por revisar",
        MargenDocumento = "Margen documental (COP)"), enlaces_pedido = FALSE)
    .crm_vancouver_server("conciliacion",
      shiny::reactive({ shiny::req(input$pedido_tab == "Conciliación"); contenido()$conciliacion }),
      enlaces_pedido = FALSE)
    output$cronologia <- .crm_render_ui({
      shiny::req(input$pedido_tab == "Cronología")
      d <- contenido()$eventos
      d$Referencia <- paste("Pedido", d$Pedido, "· línea", d$Linea, "· lote", d$Lote)
      racafeModulos::TimelineUI(d, agrupar = TRUE)
    })
    list(datos = datos)
  })
}
