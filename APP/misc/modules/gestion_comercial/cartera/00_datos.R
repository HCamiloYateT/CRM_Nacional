# Contrato y universos de cartera ----
.car_ratio <- function(a, b) {
  n <- max(length(a), length(b))
  if (!length(a) || !length(b)) return(numeric())
  b <- rep_len(b, n)
  ifelse(is.finite(b) & b > 0, rep_len(a, n) / b, NA_real_)
}
.CAR_TRAMOS <- c("Pago oportuno", "1–7 días", "8–15 días", "16–30 días", "Más de 30 días")
.car_fecha <- function(x) {
  y <- as.Date(x)
  y[!is.na(y) & y <= as.Date("1900-01-01")] <- as.Date(NA)
  y
}
.car_id <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE, digits = 22, big.mark = "")
  y <- trimws(as.character(x))
  y[is.na(y) | !nzchar(y)] <- NA_character_
  y
}
.car_llave <- function(x, columnas) {
  valores <- lapply(columnas, function(n) .car_id(x[[n]]))
  valida <- Reduce(`&`, lapply(valores, function(v) !is.na(v)))
  llave <- do.call(paste, c(valores, sep = "|"))
  llave[!valida] <- NA_character_
  llave
}
.car_distintos <- function(x) length(unique(x[!is.na(x) & nzchar(as.character(x))]))
.car_scope <- function(tabla, clientes, nit_mapa, admin = FALSE) {
  if (shiny::is.reactive(nit_mapa)) nit_mapa <- nit_mapa()
  x <- tabla
  x$NIT <- .car_id(x$PerCod)
  mapa <- nit_mapa %>% dplyr::transmute(NIT = .car_id(PerCod), Principal = .car_id(CliNitPpal)) %>%
    dplyr::filter(!is.na(NIT), !is.na(Principal)) %>% dplyr::distinct()
  if (anyDuplicated(mapa$NIT)) stop("La homologación NIT principal es ambigua.")
  x <- dplyr::left_join(x, mapa, by = "NIT", na_matches = "never")
  x$Principal <- dplyr::coalesce(x$Principal, x$NIT)
  x$Homologado <- x$Principal %in% .car_id(clientes$CliNitPpal)
  if ("LinNegCod" %in% names(x)) {
    linea_cli <- ifelse(clientes$LinNegCod == 20000, 21000, clientes$LinNegCod)
    linea_car <- ifelse(x$LinNegCod == 20000, 21000, x$LinNegCod)
    x$Homologado <- x$Homologado & paste(x$Principal, linea_car) %in% paste(.car_id(clientes$CliNitPpal), linea_cli)
  }
  # El rol nunca amplía este universo: clientes ya contiene el alcance autorizado y sus filtros.
  x[x$Homologado & !is.na(x$NIT), , drop = FALSE]
}
.car_preparar <- function(datos, fecha) {
  x <- dplyr::distinct(datos[!is.na(datos$TCEFch) & as.Date(datos$TCEFch) == as.Date(fecha), , drop = FALSE])
  x$NIT <- .car_id(x$PerCod)
  x$Factura <- .car_id(x$Factura)
  x$FechaVence <- .car_fecha(x$TCEFchVto)
  x$DiasVencido <- as.integer(as.Date(fecha) - x$FechaVence)
  if ("TGCFchGes" %in% names(x)) {
    x$TGCFchGes <- .car_fecha(x$TGCFchGes)
    posterior <- !is.na(x$TGCFchGes) & x$TGCFchGes > as.Date(fecha)
    x$TGCFchGes[posterior] <- as.Date(NA)
    if ("TGCObs" %in% names(x)) x$TGCObs[posterior] <- NA_character_
  }
  x$Saldo <- .gc_num(x$SaldoCOP)
  x$Valor <- .gc_num(.gc_columna(x, "ValorCOP"))
  x$Sacos <- .gc_num(.gc_columna(x, "TCESac"))
  x$Tipo <- dplyr::case_when(x$Saldo < 0 ~ "Saldo a favor", is.na(x$FechaVence) ~ "Sin vencimiento",
    x$DiasVencido > 0 ~ "Vencida", TRUE ~ "Por vencer")
  x$Rango <- dplyr::case_when(is.na(x$DiasVencido) ~ "Sin fecha", x$DiasVencido <= 0 ~ "Por vencer",
    x$DiasVencido <= 7 ~ "1–7 días", x$DiasVencido <= 15 ~ "8–15 días",
    x$DiasVencido <= 30 ~ "16–30 días", TRUE ~ "Más de 30 días")
  # Un documento puede tener varias líneas legítimas. Se suman sus importes y se cuenta una sola factura.
  x$Documento <- .car_llave(x, c("NIT", "Factura"))
  grano <- intersect(c("NIT", "Factura", "TCETipCar", "LinNegCod", "LinProCod", "TCEPdcRef", "TCEPdcLin"), names(x))
  if (anyDuplicated(x[grano])) stop("Hay líneas de cartera contradictorias para el mismo corte y documento.")
  x
}
.car_metricas <- function(x) {
  data.frame(SaldoCOP = .com_suma(x$Saldo), Facturas = .car_distintos(x$Documento),
    Clientes = .car_distintos(x$NIT), SacosOrigen = .com_suma(x$Sacos))
}
.car_resumen <- function(datos, dimension = "Tipo", total = FALSE) {
  if (!nrow(datos)) {
    x <- .car_metricas(datos)[0, ]; x[[dimension]] <- character()
    return(x[c(dimension, "SaldoCOP", "Facturas", "Clientes", "SacosOrigen")])
  }
  grupos <- split(seq_len(nrow(datos)), datos[[dimension]])
  x <- dplyr::bind_rows(lapply(names(grupos), function(k) {
    fila <- .car_metricas(datos[grupos[[k]], , drop = FALSE]); fila[[dimension]] <- k; fila
  }))
  universo <- .car_metricas(datos)
  x$PctSaldo <- .car_ratio(x$SaldoCOP, universo$SaldoCOP)
  x$PctFacturas <- .car_ratio(x$Facturas, universo$Facturas)
  x$PctSacos <- .car_ratio(x$SacosOrigen, universo$SacosOrigen)
  if (dimension == "Rango") x <- x[order(match(x$Rango, .CAR_TRAMOS[-1])), , drop = FALSE]
  if (total) {
    universo[[dimension]] <- "TOTAL"
    universo$PctSaldo <- .car_ratio(universo$SaldoCOP, universo$SaldoCOP)
    universo$PctFacturas <- .car_ratio(universo$Facturas, universo$Facturas)
    universo$PctSacos <- .car_ratio(universo$SacosOrigen, universo$SacosOrigen)
    x <- dplyr::bind_rows(x, universo)
  }
  x[c(dimension, "SaldoCOP", "PctSaldo", "Facturas", "PctFacturas", "Clientes", "SacosOrigen", "PctSacos")]
}
.car_vencimientos <- function(datos, periodo = "mes") {
  x <- datos[datos$Tipo == "Por vencer" & !is.na(datos$FechaVence) & !is.na(datos$Saldo) & datos$Saldo > 0, ]
  x$Periodo <- if (periodo == "semana") {
    x$FechaVence - (as.integer(format(x$FechaVence, "%u")) - 1L)
  } else .gc_mes(x$FechaVence)
  x %>% dplyr::group_by(Periodo) %>% dplyr::summarise(SaldoCOP = .com_suma(Saldo), .groups = "drop") %>%
    dplyr::arrange(Periodo) %>% dplyr::mutate(Acumulado = cumsum(SaldoCOP))
}

# Facturación y pagos: la fecha del filtro siempre es la de factura ----
.car_pagos <- function(datos, desde, hasta, hoy = Sys.Date()) {
  x <- dplyr::distinct(datos)
  x$NIT <- .car_id(x$PerCod)
  x$Factura <- .car_id(x$Factura)
  x$Documento <- .car_llave(x, c("NIT", intersect("FcnPrf", names(x)), "Factura"))
  if (anyDuplicated(x$Documento[!is.na(x$Documento)])) stop("Hay versiones contradictorias de una factura histórica.")
  x$FcnFec <- .car_fecha(x$FcnFec)
  x$FecPag <- .car_fecha(x$FecPag)
  x$FecPag[!is.na(x$FecPag) & x$FecPag > hoy] <- as.Date(NA)
  x$FecVto <- .car_fecha(.gc_columna(x, "FecVto"))
  x$DiasPago <- as.integer(x$FecPag - x$FcnFec)
  x$Valor <- .gc_num(x$FcnVal)
  x$Saldo <- .gc_num(x$FcnSal)
  x$Pagada <- !is.na(x$FecPag) & is.finite(x$Saldo) & abs(x$Saldo) < 0.01 &
    !is.na(x$DiasPago) & x$DiasPago >= 0
  x$DiasAtraso <- ifelse(x$Pagada & !is.na(x$FecVto), pmax(0, as.integer(x$FecPag - x$FecVto)), NA_real_)
  x$DiasMoraActual <- ifelse(!x$Pagada & x$Saldo > 0 & !is.na(x$FecVto),
    pmax(0, as.integer(as.Date(hoy) - x$FecVto)), NA_real_)
  x$EstadoPago <- ifelse(x$Pagada, "Pago total registrado", "Sin pago total acreditado")
  x$FueraPeriodo <- is.na(x$FcnFec) | x$FcnFec < desde | x$FcnFec > hasta
  x
}
.car_rotacion <- function(pagos) {
  pagos %>% dplyr::filter(Pagada, !FueraPeriodo, !is.na(Documento), is.finite(Valor), Valor > 0) %>%
    dplyr::mutate(Anho = as.integer(format(FcnFec, "%Y"))) %>% dplyr::group_by(NIT, Anho) %>%
    dplyr::summarise(PerRazSoc = dplyr::first(PerRazSoc), DiasPagoPonderados = sum(DiasPago * Valor) / sum(Valor),
      Facturas = .car_distintos(Documento), ValorPagadoInferido = sum(Valor), .groups = "drop")
}
.car_rotacion_tabla <- function(pagos) {
  x <- .car_rotacion(pagos)
  if (!nrow(x)) return(x)
  anterior <- x[c("NIT", "Anho", "DiasPagoPonderados")]
  anterior$Anho <- anterior$Anho + 1L
  names(anterior)[3] <- "DiasAnteriores"
  x <- dplyr::left_join(x, anterior, by = c("NIT", "Anho"))
  x$CambioDias <- x$DiasPagoPonderados - x$DiasAnteriores
  x$Comparacion <- ifelse(is.na(x$CambioDias), "Sin año comparable",
    ifelse(x$CambioDias > 0, "Más días", ifelse(x$CambioDias < 0, "Menos días", "Sin cambio")))
  x
}
.car_comportamiento <- function(pagos, concepto = "Valor") {
  x <- pagos[pagos$Pagada & !pagos$FueraPeriodo & !is.na(pagos$Documento) & !is.na(pagos$DiasAtraso), ]
  x$Tramo <- cut(x$DiasAtraso, breaks = c(-Inf, 0, 7, 15, 30, Inf), labels = .CAR_TRAMOS)
  x$FormaPago <- as.character(.gc_columna(x, "ForPagNom", "Sin dato"))
  x$Metrica <- if (concepto == "Valor") x$Valor / 1e6 else if (concepto == "Sacos") {
    .gc_num(.gc_columna(x, "Sacos"))
  } else rep(1, nrow(x))
  largo <- x %>% dplyr::group_by(NIT, Cliente = PerRazSoc, FormaPago, Tramo) %>%
    dplyr::summarise(Metrica = .com_suma(Metrica), .groups = "drop")
  if (!nrow(largo)) return(data.frame(NIT = character(), Cliente = character(), FormaPago = character()))
  largo$Tramo <- as.character(largo$Tramo)
  ancho <- tidyr::pivot_wider(largo, names_from = Tramo, values_from = Metrica, values_fill = 0)
  for (n in setdiff(.CAR_TRAMOS, names(ancho))) ancho[[n]] <- 0
  ancho$Total <- rowSums(as.data.frame(ancho[.CAR_TRAMOS]))
  for (n in .CAR_TRAMOS) ancho[[n]] <- .car_ratio(ancho[[n]], ancho$Total)
  ancho[c("NIT", "Cliente", "FormaPago", "Total", .CAR_TRAMOS)]
}
.car_detalle <- function(x) {
  mapa <- c(NIT = "NIT", Cliente = "PerRazSoc", Negocio = "Negocio", Factura = "Factura",
    NumeroPedido = "TCEPdcRef", Pedido = "TCEPdcCntC", FormaPago = "ForPagNom", FechaFactura = "FcnFec",
    FechaVence = "FechaVence", Valor = "Valor", Saldo = "Saldo", SacosOrigen = "Sacos", DiasVencido = "DiasVencido",
    UltimaGestion = "TGCFchGes", Gestion = "TGCObs", Observaciones = "TCEObsVen")
  salida <- x[intersect(unname(mapa), names(x))]
  names(salida) <- names(mapa)[match(names(salida), mapa)]
  for (n in intersect(c("NumeroPedido", "Pedido"), names(salida))) salida[[n]] <- .car_id(salida[[n]])
  for (n in intersect(c("FechaFactura", "UltimaGestion"), names(salida))) salida[[n]] <- .car_fecha(salida[[n]])
  salida
}

# Cosechas: universo fijo y puntos agregados únicamente a edades realmente observadas ----
.car_cosechas <- function(cartera, facturas, corte) {
  error <- function(texto) list(curvas = data.frame(), documentos = data.frame(), observaciones = data.frame(),
    limitacion = texto)
  requeridas <- c("PerCod", "Factura", "FcnFec", "FcnVal", "FecVto")
  if (!all(requeridas %in% names(facturas))) return(error("No hay emisión, valor original y vencimiento por factura."))
  f <- unique(facturas)
  f$NIT <- .car_id(f$PerCod); f$Factura <- .car_id(f$Factura)
  f$Documento <- .car_llave(f, c("NIT", "Factura"))
  f$Emision <- .car_fecha(f$FcnFec); f$Vencimiento <- .car_fecha(f$FecVto)
  f$Original <- .gc_num(f$FcnVal)
  f <- f[!is.na(f$Documento) & !is.na(f$Emision) & f$Emision <= corte, , drop = FALSE]
  if (!nrow(f)) return(error("No hay facturas emitidas dentro del alcance y hasta el corte seleccionado."))
  # TESHCARE no identifica prefijo: no unir números ambiguos ni elegir una versión de cabecera.
  if (anyDuplicated(f$Documento)) return(error("Hay números de factura ambiguos por NIT o prefijo; no se construyen cosechas."))
  incompletas <- !is.finite(f$Original) | f$Original <= 0 | is.na(f$Vencimiento)
  if (any(incompletas)) return(error("Hay facturas sin valor original positivo o vencimiento; el denominador de sus cohortes no es verificable."))
  f$Cohorte <- .gc_mes(f$Emision)
  base <- unique(cartera)
  base$Corte <- .car_fecha(base$TCEFch)
  base$Documento <- .car_llave(base, c("PerCod", "Factura"))
  base$SaldoObservado <- .gc_num(base$SaldoCOP)
  grano <- intersect(c("Corte", "PerCod", "Factura", "TCETipCar", "LinNegCod", "LinProCod",
    "TCEPdcRef", "TCEPdcLin"), names(base))
  if (anyDuplicated(base[grano])) stop("Hay líneas de cartera contradictorias para el mismo corte y documento.")
  observaciones <- base %>% dplyr::filter(!is.na(Corte), Corte <= corte, !is.na(Documento)) %>%
    dplyr::group_by(Corte, Documento) %>%
    dplyr::summarise(SaldoObservado = .com_suma(SaldoObservado), .groups = "drop")
  if (!nrow(observaciones)) return(error("No existen saldos históricos observados para reconstruir las cosechas."))
  d <- dplyr::inner_join(observaciones, f[c("Documento", "Cohorte", "Emision", "Vencimiento", "Original")],
    by = "Documento", relationship = "many-to-one")
  d$Dias <- as.integer(d$Corte - d$Emision)
  d <- d[d$Dias >= 0 & is.finite(d$SaldoObservado), , drop = FALSE]
  d$SaldoVencido <- ifelse(d$Corte > d$Vencimiento, pmax(d$SaldoObservado, 0), 0)
  universo <- f %>% dplyr::group_by(Cohorte) %>%
    dplyr::summarise(ValorOriginal = sum(Original), FacturasCohorte = dplyr::n(), .groups = "drop")
  curvas <- d %>% dplyr::group_by(Cohorte, Dias) %>%
    dplyr::summarise(SaldoVencido = sum(SaldoVencido), FacturasObservadas = dplyr::n_distinct(Documento),
      .groups = "drop") %>% dplyr::left_join(universo, by = "Cohorte") %>%
    dplyr::filter(FacturasObservadas == FacturasCohorte) %>%
    dplyr::mutate(PctMora = SaldoVencido / ValorOriginal)
  list(curvas = curvas, documentos = f, observaciones = d,
    limitacion = paste(attr(facturas, "limite_alcance"), "Fotografías periódicas: solo se grafican edades con todas las facturas de la cohorte observadas.",
      "La ausencia de una factura no equivale a saldo cero. No se interpolan saldos ni se infieren pagos.",
      if (!nrow(curvas)) "No hay edades comunes completas; consulte el detalle documental." else ""))
}
.car_grafico_cosechas <- function(x) {
  d <- x$curvas
  shiny::validate(shiny::need(nrow(d) > 0L, x$limitacion))
  d$NombreCohorte <- format(d$Cohorte, "%Y-%m")
  d$Lectura <- paste0("Cohorte: ", d$NombreCohorte, "<br>Valor original: ", format(d$ValorOriginal, big.mark = ","),
    " COP<br>Saldo vencido: ", format(d$SaldoVencido, big.mark = ","), " COP")
  plotly::plot_ly(d, x = ~Dias, y = ~PctMora, color = ~NombreCohorte, text = ~Lectura,
    type = "scatter", mode = "lines+markers", connectgaps = FALSE,
    colors = grDevices::colorRampPalette(c("#C8A77D", "#7A2028", "#495057"))(length(unique(d$Cohorte))),
    hovertemplate = "%{text}<br>Días desde emisión: %{x:.0f}<br>Saldo original en mora: %{y:.1%}<extra></extra>") %>%
    plotly::layout(xaxis = list(title = "Días desde emisión de las facturas"),
      yaxis = list(title = "% del valor original vencido y pendiente", tickformat = ".0%"))
}

.car_facturas_autorizadas <- function(facturas, cartera, clientes, nit_mapa) {
  if (!is.data.frame(facturas)) stop("La fuente no contiene facturas históricas.")
  f <- .car_scope(facturas, clientes, nit_mapa)
  if ("LinNegCod" %in% names(f)) return(f)
  if (!all(c("PerCod", "Factura", "LinNegCod") %in% names(cartera))) {
    stop("Las facturas no identifican su línea y no se pueden conciliar con cartera.")
  }
  total <- unique(.car_llave(cartera, c("PerCod", "Factura")))
  permitido <- .car_scope(cartera, clientes, nit_mapa)
  firma <- function(d) paste(.car_llave(d, c("PerCod", "Factura")), as.character(d$LinNegCod), sep = "|")
  lineas <- unique(data.frame(Documento = .car_llave(cartera, c("PerCod", "Factura")), Firma = firma(cartera)))
  firmas_permitidas <- unique(firma(permitido))
  aceptadas <- lineas %>% dplyr::group_by(Documento) %>%
    dplyr::summarise(Completa = all(Firma %in% firmas_permitidas), .groups = "drop")
  claves <- .car_llave(f, c("PerCod", "Factura"))
  f <- f[!is.na(claves) & claves %in% aceptadas$Documento[aceptadas$Completa] & claves %in% total, , drop = FALSE]
  attr(f, "limite_alcance") <- paste("Solo se incluyen facturas cuya línea se verificó en cartera y quedó íntegramente autorizada.",
    "Los documentos sin correspondencia o con líneas fuera del alcance no se prorratean.")
  f
}
