# Extracción de cartera: reutilizar las conexiones administradas del CRM ----
.car_union_maestra <- function(x, y, by) {
  y <- dplyr::distinct(y)
  if (anyDuplicated(y[by])) stop(paste("Maestro de cartera ambiguo:", paste(by, collapse = ", ")))
  dplyr::left_join(x, y, by = by, na_matches = "never")
}
.car_preparar_fuentes <- function(cartera, facturas, personas, formas, lineas, productos,
                                  gestiones, pagos, pedidos, plazos, generado = Sys.time()) {
  cartera$TCESgl <- toupper(trimws(as.character(cartera$TCESgl)))
  # Conservar una única última gestión por NIT/factura antes de unir importes.
  gestiones <- gestiones %>% dplyr::group_by(PerCod, Factura) %>%
    dplyr::arrange(dplyr::desc(TGCSec), dplyr::desc(TGCFchGes), .by_group = TRUE) %>%
    dplyr::slice_head(n = 1L) %>% dplyr::ungroup() %>% dplyr::select(-TGCSec)
  x <- .car_union_maestra(cartera, personas[c("PerCod", "PerRazSoc")], "PerCod")
  x <- .car_union_maestra(x, formas, "ForPagCod")
  x <- .car_union_maestra(x, lineas, "LinNegCod")
  x <- .car_union_maestra(x, productos, "LinProCod")
  x <- .car_union_maestra(x, gestiones, c("PerCod", "Factura"))
  # TESHCARE no trae prefijo. Una fecha ambigua por prefijo queda sin homologar, sin replicar el saldo.
  fechas <- facturas %>% dplyr::group_by(PerCod, Factura) %>%
    dplyr::summarise(FcnFec = if (dplyr::n_distinct(as.Date(FcnFec), na.rm = TRUE) == 1L) {
      min(as.Date(FcnFec), na.rm = TRUE)
    } else as.Date(NA), .groups = "drop")
  x <- .car_union_maestra(x, fechas, c("PerCod", "Factura"))
  x <- x %>% dplyr::mutate(Negocio = dplyr::case_when(LinNegCod == 10000 ~ "COPRODUCTO",
    LinNegCod %in% c(20000, 21000) ~ "A LA MEDIDA", TRUE ~ "OTRO"),
    ValorCOP = dplyr::case_when(TCESgl == "USD" & TCETrm > 0 ~ TCETot * TCETrm,
      TCESgl == "COP" ~ TCETot, TRUE ~ NA_real_),
    SaldoCOP = dplyr::case_when(TCESgl == "USD" & TCETrm > 0 ~ TCESal * TCETrm,
      TCESgl == "COP" ~ TCESal, TRUE ~ NA_real_))
  # La cabecera de factura tiene un único valor, nunca se cruza con sus líneas de productos.
  f <- .car_union_maestra(facturas, personas, "PerCod")
  f <- .car_union_maestra(f, formas, "ForPagCod")
  f <- .car_union_maestra(f, pedidos, "PdcCod")
  f <- .car_union_maestra(f, pagos, "Factura")
  # El recibo no trae prefijo ni NIT: no atribuir su fecha a números de factura ambiguos.
  claves <- unique(f[c("Factura", "PerCod", "FcnPrf")])
  ambiguas <- claves$Factura[duplicated(claves$Factura) | duplicated(claves$Factura, fromLast = TRUE)]
  f$FecPag[f$Factura %in% ambiguas] <- as.Date(NA)
  plazo <- unique(as.numeric(plazos$FctPlazEdo))
  plazo <- plazo[is.finite(plazo) & plazo >= 0]
  defecto <- if (length(plazo) == 1L) plazo else NA_real_
  f$FecVto <- as.Date(f$FcnFec) + dplyr::coalesce(as.numeric(f$DiasPlazo), defecto)
  # TESRECPA aporta la última fecha de recibo, no el detalle de los abonos.
  f$FecPag <- as.Date(f$FecPag)
  f$FecPag[!is.na(f$FecPag) & f$FecPag <= as.Date("1900-01-01")] <- as.Date(NA)
  f$DiasPago <- as.integer(f$FecPag - as.Date(f$FcnFec))
  list(cartera = as.data.frame(x), facturacion = as.data.frame(f),
    clientes = dplyr::distinct(f[c("PerCod", "PerRazSoc")]), importado_en = generado,
    origen = "Tesorería nacional: TESHCARE / FCTFACNA", version = 1L)
}
.car_extraer_fuentes <- function(consultar = function(query) racafeBD::ConsultaSistema("syscafe", query = query)) {
  cartera <- consultar(paste("SELECT TCEFch, TCEFctNum AS Factura, TCETipCar, PerCod, ForPagCod,",
    "TCETot, TCESal, TCEFchVto, TCEDiaVen, TCEDiaXVen, TCEObsVen, TCEPdcCntC, TCEPdcRef, TCEPdcLin,",
    "LinNegCod, LinProCod, TCETrm, TCESac, TCEEst, TCECupMda, TCESgl",
    "FROM TESHCARE WHERE CiaCod = 10 AND TCETip = 'CN' AND LinNegCod IN (10000, 20000, 21000)"))
  facturas <- consultar(paste("SELECT FcnPdcRef AS PdcCod, FcnNum AS Factura, FcnPrf, FctFPagCod AS ForPagCod,",
    "FcnSac AS Sacos, FcnFec, FcnVal, FcnSal, FctNit AS PerCod, FcnEtd FROM FCTFACNA",
    "WHERE CiaCod = 10 AND FcnEtd = 'C' AND FcnFec >= '2018-01-01' AND FcnSac <> 0 AND FcnPdcRef <> 0"))
  personas <- consultar("SELECT PerCod, PerRazSoc, PerFecCre FROM NPERSONA")
  formas <- consultar("SELECT ForPagCod, ForPagNom, ForPagPlz AS DiasPlazo FROM NFORPAG")
  lineas <- consultar("SELECT LinNegCod, LinNegNom FROM NLINEANE WHERE CiaCod = 10")
  productos <- consultar("SELECT LinProCod, LinProNom FROM NTIPPROD WHERE CiaCod = 10")
  gestiones <- consultar(paste("SELECT TGCFctNum AS Factura, PerCod, TGCSec, TGCFchGes, TGCUsuGes, TGCObs",
    "FROM TESGESCA WHERE TGCTip = 'CN'"))
  pagos <- consultar(paste("SELECT PCCNumFac AS Factura, MAX(PCCFecRec) AS FecPag FROM TESRECPA",
    "WHERE CiaCod = 10 GROUP BY PCCNumFac"))
  pedidos <- consultar(paste("SELECT PdcCod, PdcFecCre, PdcCntCli AS Pedido FROM EXPPEDID",
    "WHERE CiaCod = 10 AND PdcCod <> 0 AND PdcAnoEmb >= 2022 AND CliNit <> 32"))
  plazos <- consultar("SELECT FctPlazEdo FROM FCTCTBPA WHERE CiaCod = 10 AND FctTipPro = 'C'")
  .car_preparar_fuentes(cartera, facturas, personas, formas, lineas, productos, gestiones, pagos, pedidos, plazos)
}
