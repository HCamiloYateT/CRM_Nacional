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
# Base documental reutilizable sin atribuir pagos ni completar saldos ausentes ----
.car_base_cosechas <- function(cartera, facturas, corte) {
  if (!all(c("PerCod", "Factura", "FcnFec", "FcnVal", "FecVto") %in% names(facturas))) {
    stop("No hay emisión, valor original y vencimiento por factura.")
  }
  f <- unique(facturas)
  f$NIT <- .car_id(f$PerCod); f$Factura <- .car_id(f$Factura)
  f$Documento <- .car_llave(f, c("NIT", "Factura"))
  f$Emision <- .car_fecha(f$FcnFec); f$Vencimiento <- .car_fecha(f$FecVto)
  f$Original <- .gc_num(f$FcnVal)
  f <- f[!is.na(f$Documento) & !is.na(f$Emision) & f$Emision <= corte, , drop = FALSE]
  if (anyDuplicated(f$Documento)) stop("Hay números de factura ambiguos por NIT/prefijo; no se prorratean.")
  if (any(!is.finite(f$Original) | f$Original <= 0)) stop("Hay facturas sin valor original positivo verificable.")
  b <- unique(cartera)
  b$Corte <- .car_fecha(b$TCEFch)
  b$Documento <- .car_llave(b, c("PerCod", "Factura"))
  b$SaldoObservado <- .gc_num(b$SaldoCOP)
  grano <- intersect(c("Corte", "PerCod", "Factura", "TCETipCar", "LinNegCod", "LinProCod",
    "TCEPdcRef", "TCEPdcLin"), names(b))
  if (anyDuplicated(b[grano])) stop("Hay líneas de cartera contradictorias para el mismo corte y documento.")
  observaciones <- b %>% dplyr::filter(!is.na(Corte), Corte <= corte, !is.na(Documento)) %>%
    dplyr::group_by(Corte, Documento) %>%
    dplyr::summarise(SaldoObservado = .com_suma(SaldoObservado), .groups = "drop")
  b$LinNegCod <- as.character(.gc_columna(b, "LinNegCod"))
  b$LinNegCod[b$LinNegCod == "20000"] <- "21000"
  lineas <- b %>% dplyr::group_by(Documento) %>%
    dplyr::summarise(LineaObservada = .gc_unico(LinNegCod), .groups = "drop")
  f <- dplyr::left_join(f, lineas, by = "Documento", relationship = "one-to-one")
  f$LinNegCod <- dplyr::coalesce(as.character(.gc_columna(f, "LinNegCod")), as.character(f$LineaObservada))
  f$LinNegCod[f$LinNegCod == "20000"] <- "21000"
  f$Linea <- dplyr::recode(f$LinNegCod, `10000` = "COPRODUCTOS", `20000` = "A LA MEDIDA",
    `21000` = "A LA MEDIDA", .default = "LÍNEA NO UNÍVOCA")
  f$Principal <- .gc_columna(f, c("Principal", "CliNitPpal", "NIT"))
  f$UnidadComercial <- paste(f$Principal, f$Linea, sep = " · ")
  list(documentos = f, observaciones = observaciones)
}
.car_cosechas <- function(cartera = NULL, facturas = NULL, corte, origen = "Emision", dimension = "Total",
                          desde = NULL, base = NULL, hasta_origen = corte) {
  if (is.null(base)) base <- .car_base_cosechas(cartera, facturas, corte)
  f <- base$documentos
  if (!origen %in% c("Emision", "Vencimiento")) stop("Origen de cosecha no válido.")
  f$Cohorte <- as.Date(format(f[[origen]], "%Y-%m-01"))
  f <- f[!is.na(f$Cohorte) & f[[origen]] <= hasta_origen, , drop = FALSE]
  if (!is.null(desde)) f <- f[f[[origen]] >= as.Date(desde), , drop = FALSE]
  if (dimension != "Total" && !dimension %in% names(f)) stop("La dimensión de cartera no está disponible.")
  f$Grupo <- if (dimension == "Total") "TOTAL" else as.character(f[[dimension]])
  f$Grupo[is.na(f$Grupo) | !nzchar(f$Grupo)] <- "SIN DATO"
  o <- base$observaciones
  o <- o[!is.na(o$Corte) & o$Corte <= corte, , drop = FALSE]
  cortes <- o %>% dplyr::mutate(MesCorte = as.Date(format(Corte, "%Y-%m-01"))) %>%
    dplyr::group_by(MesCorte) %>% dplyr::summarise(Corte = max(Corte), .groups = "drop")
  documentos <- c("Documento", "Cohorte", "Grupo", "Emision", "Vencimiento", "Original", "NIT", "Factura",
    "Principal", "UnidadComercial", "Linea", "PerRazSoc", "Asesor", "Segmento")
  documentos <- intersect(documentos, names(f))
  detalle <- dplyr::inner_join(f[documentos], cortes, by = dplyr::join_by(Cohorte <= MesCorte),
    relationship = "many-to-many") %>%
    dplyr::left_join(o, by = c("Documento", "Corte"), relationship = "many-to-one")
  detalle$Edad <- (as.integer(format(detalle$MesCorte, "%Y")) -
    as.integer(format(detalle$Cohorte, "%Y"))) * 12L +
    as.integer(format(detalle$MesCorte, "%m")) - as.integer(format(detalle$Cohorte, "%m")) + 1L
  detalle$Vencida <- detalle$Corte > detalle$Vencimiento
  curvas <- detalle %>% dplyr::group_by(Cohorte, Grupo, Edad, Corte) %>%
    dplyr::summarise(ValorOriginal = sum(Original), FacturasCohorte = dplyr::n(),
      FacturasObservadas = sum(is.finite(SaldoObservado)), SaldoPendiente = .com_suma(SaldoObservado),
      .groups = "drop") %>% dplyr::mutate(PctSaldo = SaldoPendiente / ValorOriginal)
  inicial <- f %>% dplyr::group_by(Cohorte, Grupo) %>%
    dplyr::summarise(ValorOriginal = sum(Original), FacturasCohorte = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(Edad = 0L, Corte = as.Date(NA), FacturasObservadas = NA_integer_,
      SaldoPendiente = ValorOriginal, PctSaldo = 1)
  inicial$Referencia <- TRUE; curvas$Referencia <- FALSE
  curvas <- dplyr::bind_rows(inicial, curvas) %>% dplyr::arrange(Cohorte, Grupo, Edad)
  if (nrow(curvas)) curvas$Celda <- paste(curvas$Cohorte, curvas$Grupo, curvas$Edad, sep = "|")
  list(curvas = curvas, documentos = f, observaciones = detalle,
    limitacion = c("Edad 0: referencia del valor original (100%), no un saldo observado.",
      "Edad 1: último corte disponible del mes de origen; edad 2: mes siguiente, y así sucesivamente.",
      "Una factura ausente deja el porcentaje desconocido. No se interpolan saldos ni se infieren pagos.",
      "Las variaciones pueden incluir pagos, notas crédito o ajustes; esta fuente no permite separarlos.",
      "Asesor y segmento corresponden al catálogo actual, no a una asignación histórica reconstruida."))
}
.car_detalle_cosecha <- function(x, celda) {
  fila <- x$curvas[x$curvas$Celda %in% celda, , drop = FALSE]
  if (nrow(fila) != 1L) return(data.frame())
  if (fila$Edad == 0L) {
    d <- x$documentos[x$documentos$Cohorte == fila$Cohorte & x$documentos$Grupo == fila$Grupo, , drop = FALSE]
    d$SaldoObservado <- NA_real_
    d$Nota <- "Referencia original; no es un saldo histórico observado."
    return(d)
  }
  d <- x$observaciones
  d[d$Cohorte == fila$Cohorte & d$Grupo == fila$Grupo & d$Edad == fila$Edad & d$Corte == fila$Corte, , drop = FALSE]
}
.car_grafico_cosechas <- function(x, source = "car_cosechas") {
  d <- x$curvas
  shiny::validate(shiny::need(nrow(d) > 0L, "No hay cosechas para el periodo y alcance seleccionados."))
  d$Serie <- paste(format(d$Cohorte, "%Y-%m"), d$Grupo, sep = " · ")
  p <- plotly::plot_ly(source = source)
  for (serie in unique(d$Serie)) {
    f <- d[d$Serie == serie, , drop = FALSE]
    # Insertar edades sin observación como huecos, nunca unirlas como si hubiera saldos conocidos.
    f <- dplyr::left_join(data.frame(Edad = seq.int(0L, max(f$Edad))), f, by = "Edad")
    p <- plotly::add_trace(p, x = f$Edad, y = f$PctSaldo, name = serie, type = "scatter", mode = "lines+markers",
      connectgaps = FALSE, line = list(color = unname(.crm_colores("cosecha", serie))),
      customdata = lapply(seq_len(nrow(f)), function(i) list(f$Celda[i], f$ValorOriginal[i], f$SaldoPendiente[i])),
      hovertemplate = paste0("<b>", serie, "</b><br>Edad: %{x:.0f}<br>Saldo pendiente: %{y:.1%}",
        "<br>Original: $%{customdata[1]:,.0f}<br>Saldo: $%{customdata[2]:,.0f}<extra></extra>"))
  }
  rango <- range(c(0, 1, d$PctSaldo), na.rm = TRUE)
  plotly::event_register(plotly::layout(p, xaxis = list(title = "Edad de la cosecha (meses calendario)"),
    yaxis = list(title = "% del valor original pendiente", tickformat = ".0%", range = rango)), "plotly_click")
}
.car_matriz_cosechas <- function(x, source = "car_matriz") {
  d <- x$curvas
  shiny::validate(shiny::need(nrow(d) > 0L, "No hay cosechas para la selección."))
  filas <- unique(paste(format(d$Cohorte, "%Y-%m"), d$Grupo, sep = " · "))
  edades <- seq.int(0L, max(d$Edad))
  z <- matrix(NA_real_, nrow = length(filas), ncol = length(edades))
  claves <- matrix(NA_character_, nrow = length(filas), ncol = length(edades))
  i <- match(paste(format(d$Cohorte, "%Y-%m"), d$Grupo, sep = " · "), filas)
  z[cbind(i, d$Edad + 1L)] <- d$PctSaldo; claves[cbind(i, d$Edad + 1L)] <- d$Celda
  p <- plotly::plot_ly(x = edades, y = filas, z = z, customdata = lapply(seq_len(nrow(claves)), function(i) as.list(claves[i, ])), type = "heatmap",
    source = source, colorscale = list(c(0, "#F6F0E7"), c(1, "#6C5146")),
    hovertemplate = "%{y}<br>Edad: %{x:.0f}<br>Saldo pendiente: %{z:.1%}<extra></extra>", hoverongaps = FALSE)
  plotly::event_register(plotly::layout(p, yaxis = list(autorange = "reversed"),
    xaxis = list(title = "Edad 0 = referencia original; 1 = corte del mes de origen")), "plotly_click")
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
