# Cartera importada: saldos por corte, sin sumar snapshots de fechas distintas ----
.car_scope <- function(tabla, clientes, nit_mapa, admin = FALSE) {
  x <- tabla
  x$NIT <- as.character(x$PerCod)
  mapa <- nit_mapa %>% dplyr::transmute(NIT = as.character(PerCod), Principal = as.character(CliNitPpal)) %>%
    dplyr::distinct()
  if (anyDuplicated(mapa$NIT)) stop("La homologación NIT principal es ambigua.")
  x <- dplyr::left_join(x, mapa, by = "NIT", na_matches = "never")
  x$Principal <- dplyr::coalesce(x$Principal, x$NIT)
  permitidos <- as.character(clientes$CliNitPpal)
  x$Homologado <- x$Principal %in% permitidos
    if ("LinNegCod" %in% names(x)) {
    linea_cli <- ifelse(clientes$LinNegCod == 20000, 21000, clientes$LinNegCod)
    linea_car <- ifelse(x$LinNegCod == 20000, 21000, x$LinNegCod)
    permitidas <- paste(clientes$CliNitPpal, linea_cli)
    x$Homologado <- x$Homologado & paste(x$Principal, linea_car) %in% permitidas
  }
  if (!admin) x <- x[x$Homologado, , drop = FALSE]
  x
}
.car_preparar <- function(datos, fecha) {
  x <- datos[as.Date(datos$TCEFch) == as.Date(fecha), , drop = FALSE]
  x <- dplyr::distinct(x)
  x$NIT <- as.character(x$PerCod)
  x$FechaVence <- as.Date(x$TCEFchVto)
  x$FechaVence[x$FechaVence <= as.Date("1900-01-01")] <- as.Date(NA)
  x$DiasVencido <- as.integer(as.Date(fecha) - x$FechaVence)
  x$Saldo <- .gc_num(x$SaldoCOP)
  # El archivo ya convierte a COP; no volver a multiplicar por TRM ni dividir por mil.
  x$Tipo <- dplyr::case_when(x$Saldo < 0 ~ "Saldo a favor", is.na(x$FechaVence) ~ "Sin vencimiento",
    x$DiasVencido > 0 ~ "Vencida", TRUE ~ "Por vencer")
  x$Rango <- dplyr::case_when(is.na(x$DiasVencido) ~ "Sin fecha", x$DiasVencido <= 0 ~ "Por vencer",
    x$DiasVencido <= 30 ~ "1–30 días", x$DiasVencido <= 60 ~ "31–60 días",
    x$DiasVencido <= 90 ~ "61–90 días", TRUE ~ "Más de 90 días")
  x$Documento <- paste(x$NIT, x$Factura, .gc_columna(x, "TCETipCar"), sep = "|")
  x
}
.car_resumen <- function(datos, dimension = "Tipo") {
  datos %>% dplyr::group_by(.data[[dimension]]) %>%
    dplyr::summarise(SaldoCOP = .com_suma(Saldo), Documentos = dplyr::n_distinct(Documento),
      SacosOrigen = .gc_suma(.gc_num(TCESac)), .groups = "drop")
}
.car_vencimientos <- function(datos, periodo = "mes") {
  x <- datos[datos$Tipo == "Por vencer" & !is.na(datos$FechaVence) & datos$Saldo > 0, , drop = FALSE]
  x$Periodo <- if (periodo == "semana") {
    x$FechaVence - (as.integer(format(x$FechaVence, "%u")) - 1L)
  } else .gc_mes(x$FechaVence)
  x %>% dplyr::group_by(Periodo) %>% dplyr::summarise(SaldoCOP = .com_suma(Saldo), .groups = "drop") %>%
    dplyr::arrange(Periodo) %>% dplyr::mutate(Acumulado = cumsum(SaldoCOP))
}
.car_pagos <- function(datos, desde, hasta) {
  x <- dplyr::distinct(datos)
  x$NIT <- as.character(x$PerCod)
  x$FecPag <- as.Date(x$FecPag)
  x$FcnFec <- as.Date(x$FcnFec)
  x$FecPag[x$FecPag <= as.Date("1900-01-01")] <- as.Date(NA)
  x$DiasPago <- as.integer(x$FecPag - x$FcnFec)
  x$Valor <- .gc_num(x$FcnVal)
  x$Saldo <- .gc_num(x$FcnSal)
  # Se infiere pago total solo con fecha válida y saldo cero. No inventar abonos ni fechas de recaudo.
  x$Pagada <- !is.na(x$FecPag) & is.finite(x$Saldo) & abs(x$Saldo) < 0.01 & x$DiasPago >= 0 & x$FecPag <= Sys.Date()
  x$FueraPeriodo <- is.na(x$FecPag) | x$FecPag < desde | x$FecPag > hasta
  x
}
.car_rotacion <- function(pagos) {
  pagos %>% dplyr::filter(Pagada, !FueraPeriodo, is.finite(Valor), Valor > 0) %>%
    dplyr::mutate(Anho = as.integer(format(FcnFec, "%Y"))) %>% dplyr::group_by(NIT, PerRazSoc, Anho) %>%
    dplyr::summarise(DiasPagoPonderados = sum(DiasPago * Valor) / sum(Valor), Facturas = dplyr::n(),
      ValorPagadoInferido = sum(Valor), .groups = "drop")
}
CarteraComercialUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    shiny::uiOutput(ns("fuente")),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputSelect("corte", "Corte disponible de cartera", choices = character(), ns = ns),
      racafeForms::racafeInputSelect("negocio", "Negocio (saldos)", choices = character(), multiple = TRUE, ns = ns),
      racafeForms::racafeInputSelect("vista", "Vista", choices = c("Resumen", "Vencida", "Vencimientos",
        "Comportamiento de pago", "Rotación", "Recaudos", "Detalle"), ns = ns)),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputDateRange("pagos", "Periodo de pago para rotación y recaudos",
        start = as.Date(format(Sys.Date(), "%Y-01-01")), end = Sys.Date(), ns = ns),
      racafeForms::racafeInputSelect("agrupacion", "Vencimientos por", choices = c("Mes" = "mes", "Semana" = "semana"),
        ns = ns)),
    .gc_tarjetas_ui(ns, c(saldo = "Saldo COP visible al corte", vencida = "Cartera vencida COP",
      documentos = "Documentos", clientes = "Clientes")),
    shiny::uiOutput(ns("grafico_ui")), .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("tabla")),
    .crm_nota(c("Fuente: archivo importado, no consulta de cartera en tiempo real. Se muestra un único corte.",
      "Los registros sin homologación al universo de clientes visible no se suman al reporte.",
      "Los saldos ya están en COP; los sacos conservan la unidad de origen del archivo.",
      "Vencimientos es un calendario contractual, no una predicción ni un compromiso de recaudo.",
      "Rotación: días factura→pago, ponderados por el valor de facturas totalmente pagadas.",
      "Recaudos identifica pago total inferido; el archivo no permite reconstruir abonos parciales.",
      "Los pagos se filtran por cliente y periodo; Negocio filtra saldos, no pagos sin detalle de línea.",
      "Se aplican permisos por NIT principal y asesor; no se atribuye cartera a categorías o productos sin soporte.")))
}
CarteraComercial <- function(id, historico, clientes, nit_mapa, filtros, es_admin) {
  shiny::moduleServer(id, function(input, output, session) {
    fuente <- shiny::reactive({
      h <- historico()
      shiny::validate(shiny::need(!is.null(h$cartera),
        "No hay una fuente de cartera cargada. Solicite su actualización."))
      h
    })
    catalogo <- shiny::reactive(.com_clientes(clientes(), Sys.Date(), filtros()))
    saldos <- shiny::reactive(.car_scope(fuente()$cartera, catalogo(), nit_mapa,
      admin = FALSE))
    shiny::observe({
      x <- saldos()
      cortes <- sort(unique(as.Date(x$TCEFch)), decreasing = TRUE)
      negocios <- sort(unique(as.character(x$Negocio)))
      actual <- shiny::isolate(input$corte)
      racafeForms::racafeUpdateSelect(session, "corte", choices = as.character(cortes),
        selected = if (length(actual) && actual %in% as.character(cortes)) actual else as.character(head(cortes, 1)))
      seleccion <- shiny::isolate(input$negocio)
      racafeForms::racafeUpdateSelect(session, "negocio", choices = negocios,
        selected = if (is.null(seleccion)) negocios else intersect(seleccion, negocios))
    })
    corte <- shiny::reactive({ shiny::req(input$corte); as.Date(input$corte) })
    datos <- shiny::reactive({
      x <- .car_preparar(saldos(), corte())
      x[x$Negocio %in% input$negocio, , drop = FALSE]
    })
    output$fuente <- shiny::renderUI({
      h <- fuente()
      .crm_nota(c(paste("Archivo de cartera con cortes hasta", .gc_fecha_max(as.Date(h$cartera$TCEFch))),
        paste("Consulta al corte", corte())))
    })
    pagos <- shiny::reactive({
      h <- fuente()
      shiny::validate(shiny::need(!is.null(h$facturacion), "El archivo no contiene pagos/facturación."))
      shiny::req(length(input$pagos) == 2L)
      .car_pagos(.car_scope(h$facturacion, catalogo(), nit_mapa), as.Date(input$pagos[[1]]), as.Date(input$pagos[[2]]))
    })
    .gc_tarjetas_server(c(saldo = "Saldo COP visible al corte", vencida = "Cartera vencida COP",
      documentos = "Documentos", clientes = "Clientes"), shiny::reactive({
      x <- datos()
      list(saldo = .com_suma(x$Saldo), vencida = .com_suma(x$Saldo[x$Tipo == "Vencida"]),
        documentos = dplyr::n_distinct(x$Documento), clientes = dplyr::n_distinct(x$NIT))
    }), list(saldo = "dinero", vencida = "dinero"))
    tabla <- shiny::reactive({
      x <- datos()
      vista <- .com_elegir(input$vista, c("Resumen", "Vencida", "Vencimientos", "Comportamiento de pago",
        "Rotación", "Recaudos", "Detalle"), "Resumen")
      if (vista == "Resumen") return(.car_resumen(x))
      if (vista == "Vencida") return(.car_resumen(x[x$Tipo == "Vencida", ], "Rango"))
      if (vista == "Vencimientos") return(.car_vencimientos(x,
        .com_elegir(input$agrupacion, c("mes", "semana"), "mes")))
      if (vista == "Rotación") return(.car_rotacion(pagos()))
      if (vista == "Recaudos") {
        p <- pagos()
        return(p[p$Pagada & !p$FueraPeriodo, c("NIT", "PerRazSoc", "Factura", "FcnFec", "FecPag", "Valor", "DiasPago")])
      }
      if (vista == "Comportamiento de pago") {
        p <- pagos()
        p <- p[p$Pagada & !p$FueraPeriodo, ]
        p$DiasAtraso <- as.integer(p$FecPag - as.Date(.gc_columna(p, "FecVto")))
        p$EstadoPago <- ifelse(is.na(p$DiasAtraso), "Sin fecha de vencimiento",
          ifelse(p$DiasAtraso > 0, "Pagó después del vencimiento", "Pagó a tiempo"))
        return(p %>% dplyr::group_by(NIT, PerRazSoc, EstadoPago) %>%
          dplyr::summarise(Facturas = dplyr::n(), ValorPagadoInferido = .com_suma(Valor), .groups = "drop"))
      }
      x[c("NIT", "PerRazSoc", "Factura", "Negocio", "FechaVence", "DiasVencido", "Saldo", "Tipo", "Rango")]
    })
    output$grafico_ui <- shiny::renderUI({
      if (length(input$vista) == 1L && input$vista %in% c("Resumen", "Vencida", "Vencimientos")) {
        shiny::plotOutput(session$ns("grafico"), height = "260px")
      }
    })
    output$grafico <- shiny::renderPlot({
      shiny::req(input$vista %in% c("Resumen", "Vencida", "Vencimientos"))
      x <- tabla()
      shiny::validate(shiny::need(nrow(x) > 0, "Sin movimientos para este corte."))
      if (input$vista == "Vencimientos") {
        ggplot2::ggplot(x, ggplot2::aes(x = Periodo, y = SaldoCOP)) +
          ggplot2::geom_col(fill = "#DA291C") + ggplot2::labs(x = "Vencimiento contractual", y = "COP") +
          ggplot2::theme_minimal(base_size = 11)
      } else {
        etiqueta <- if (input$vista == "Resumen") "Tipo" else "Rango"
        ggplot2::ggplot(x, ggplot2::aes(x = .data[[etiqueta]], y = SaldoCOP)) +
          ggplot2::geom_col(fill = "#DA291C") + ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = "Saldo COP") + ggplot2::theme_minimal(base_size = 11)
      }
    })
    seleccion <- .crm_vancouver_server("tabla", data = tabla, modo_seleccion = "fila",
      col_labels = c(PerRazSoc = "Cliente", SaldoCOP = "Saldo COP", SacosOrigen = "Sacos de origen",
        DiasPagoPonderados = "Días de pago ponderados", ValorPagadoInferido = "Valor pagado inferido",
        FcnFec = "Fecha factura", FecPag = "Fecha de pago", DiasPago = "Días de pago",
        DiasVencido = "Días vencidos", FechaVence = "Vencimiento", Anho = "Año"))
    shiny::observeEvent(seleccion$seleccion(), {
      fila <- seleccion$seleccion()$data
      if ("Tipo" %in% names(fila)) {
        x <- datos()[datos()$Tipo == fila$Tipo[[1]], ]
      } else if ("Rango" %in% names(fila)) {
        x <- datos()[datos()$Rango == fila$Rango[[1]], ]
      } else if ("NIT" %in% names(fila) && input$vista %in% c("Rotación", "Recaudos", "Comportamiento de pago")) {
        x <- pagos(); x <- x[x$NIT == fila$NIT[[1]], ]
      } else x <- fila
      cols <- intersect(c("NIT", "PerRazSoc", "Factura", "Negocio", "FechaVence", "DiasVencido", "Saldo", "Tipo",
        "FcnFec", "FecPag", "DiasPago", "Valor", "Periodo", "SaldoCOP"), names(x))
      .gc_modal_tabla("Detalle de cartera", x[cols])
      seleccion$limpiar()
    })
    .gc_descarga(output, tabla, "Cartera")
    list(datos = datos)
  })
}
