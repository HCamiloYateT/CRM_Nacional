# Maestro de unidades comerciales y procedencia de atributos ----
.crm_unidades_comerciales <- function(hechos, clientes, contactos, estados, leads = data.frame(),
  nit_mapa = data.frame()) {
  ultimo <- function(d, claves, fecha, campos) {
    for (campo in setdiff(c(claves, campos), names(d))) d[[campo]] <- rep(NA_character_, nrow(d))
    d$FechaAtributo <- if (fecha %in% names(d)) as.Date(d[[fecha]]) else rep(as.Date(NA), nrow(d))
    d %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
      dplyr::filter(if (all(is.na(FechaAtributo))) TRUE else FechaAtributo %in% max(FechaAtributo, na.rm = TRUE)) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(campos), .gc_unico), .groups = "drop")
  }
  claves <- c("CliNitPpal", "LinNegCod")
  normalizar <- function(d) {
    for (c in claves) d[[c]] <- .crm_id_texto(.gc_columna(d, c))
    d$LinNegCod[d$LinNegCod == "20000"] <- "21000"
    d
  }
  h <- normalizar(hechos); c <- normalizar(clientes); e <- normalizar(estados)
  crm <- contactos
  crm$CliNitPpal <- .crm_id_texto(.gc_columna(crm, c("CliNitPpal", "PerCod")))
  crm$LinNegCod <- .crm_id_texto(.gc_columna(crm, "LinNegCod"))
  if (nrow(nit_mapa)) {
    nits <- .crm_id_texto(.gc_columna(nit_mapa, c("CLCliNit", "PerCod")))
    principales <- .crm_id_texto(nit_mapa$CliNitPpal)
    mapa <- unique(data.frame(NIT = nits, Principal = principales))
    if (anyDuplicated(mapa$NIT[!is.na(mapa$NIT)])) stop("El mapa de NIT tiene cuentas principales contradictorias.")
    crm$CliNitPpal <- dplyr::coalesce(mapa$Principal[match(crm$CliNitPpal, mapa$NIT)], crm$CliNitPpal)
  }
  conocidas <- dplyr::distinct(dplyr::bind_rows(h[claves], c[claves], e[claves])) %>%
    dplyr::filter(!is.na(CliNitPpal), !is.na(LinNegCod))
  sin_linea <- crm[is.na(crm$LinNegCod), , drop = FALSE]
  if (nrow(sin_linea)) {
    sin_linea$LinNegCod <- NULL
    expandidas <- dplyr::inner_join(sin_linea, conocidas, by = "CliNitPpal", relationship = "many-to-many")
    crm <- dplyr::bind_rows(crm, expandidas)
  }
  if (nrow(leads) && all(c("CodContacto", "LinNegCod") %in% names(leads))) {
    crm$CodContacto <- as.character(crm$CodContacto)
    l <- leads
    l$CodContacto <- as.character(l$CodContacto)
    l$CliNitPpal <- crm$CliNitPpal[match(as.character(l$CodContacto), as.character(crm$CodContacto))]
    l$LinNegCod <- .crm_id_texto(l$LinNegCod)
    crm <- dplyr::bind_rows(crm, dplyr::left_join(l[c("CodContacto", claves)],
      dplyr::select(crm, -dplyr::all_of(claves)), by = "CodContacto", relationship = "many-to-many"))
  }
  crm <- normalizar(crm)
  actual <- ultimo(c, claves, "FecProceso", c("PerRazSoc", "Asesor", "Segmento"))
  names(actual)[-(1:2)] <- paste0(names(actual)[-(1:2)], "CRM")
  transacciones <- ultimo(h, claves, "FecFact", c("PerRazSoc", "Asesor", "Segmento", "CLLinNegNo"))
  oficial <- ultimo(e, claves, "FecProceso", c("SegmentoRacafe", "FecProceso"))
  names(oficial)[names(oficial) == "FecProceso"] <- "FechaEstadoOficial"
  contacto <- ultimo(crm, claves, "FechaHoraCrea", c("CodContacto", "Origen", "UsuarioCrea", "Etapa"))
  universo <- dplyr::distinct(dplyr::bind_rows(h[claves], c[claves], e[claves], crm[claves])) %>%
    dplyr::filter(!is.na(CliNitPpal), !is.na(LinNegCod))
  x <- universo %>% dplyr::left_join(actual, by = claves) %>% dplyr::left_join(transacciones, by = claves) %>%
    dplyr::left_join(oficial, by = claves) %>% dplyr::left_join(contacto, by = claves)
  reales <- h[!is.na(.gc_columna(h, "FecFact")) &
    .gc_columna(h, "EstadoFinanciero") %in% c("CERRADO", "FACTURADO_ABIERTO"), claves, drop = FALSE]
  x$Identificador <- paste(x$CliNitPpal, x$LinNegCod, sep = "|")
  con_factura <- x$Identificador %in% paste(reales$CliNitPpal, reales$LinNegCod, sep = "|")
  con_estado <- x$Identificador %in% paste(e$CliNitPpal, e$LinNegCod, sep = "|")
  x$EtapaVigente <- ifelse(con_factura | con_estado, "CLIENTE", as.character(x$Etapa))
  x$EstadoCliente <- dplyr::case_when(grepl("NUEVO", x$SegmentoRacafe) ~ "NUEVO",
    grepl("INACTIVO|RECUPERAR", x$SegmentoRacafe) ~ "INACTIVO",
    x$SegmentoRacafe == "CLIENTE" ~ "ACTIVO", TRUE ~ NA_character_)
  x$PerRazSoc <- dplyr::coalesce(as.character(x$PerRazSocCRM), as.character(x$PerRazSoc))
  x$Asesor <- dplyr::coalesce(as.character(x$AsesorCRM), as.character(x$Asesor))
  x$Segmento <- dplyr::coalesce(as.character(x$SegmentoCRM), as.character(x$Segmento))
  x$CLLinNegNo <- dplyr::coalesce(as.character(x$CLLinNegNo),
    unname(c(`10000` = "COPRODUCTOS", `21000` = "A LA MEDIDA")[x$LinNegCod]))
  x$Responsable <- dplyr::coalesce(x$Asesor, as.character(x$UsuarioCrea))
  x$FuenteEtapa <- ifelse(con_factura, "FACTURACIÓN", ifelse(con_estado, "CRMNALSEGR", ifelse(!is.na(x$Etapa), "HISTORIAL CRM", NA_character_)))
  x$FuenteEstado <- ifelse(!is.na(x$EstadoCliente), "CRMNALSEGR", NA_character_)
  x$FuenteAsesor <- ifelse(!is.na(x$AsesorCRM), "CRMNALCLIENTE", ifelse(!is.na(x$Asesor), "MAESTRO", NA_character_))
  if (anyDuplicated(x$Identificador)) stop("El maestro contiene unidades comerciales duplicadas.")
  x
}
# Indicadores por unidad y periodo sin repetir cantidades operativas ----
.crm_metricas_unidades <- function(hechos, clientes, desde, hasta, cierre_presupuesto = hasta) {
  claves <- c("CliNitPpal", "LinNegCod")
  desde <- as.Date(desde); hasta <- as.Date(hasta); cierre_presupuesto <- as.Date(cierre_presupuesto)
  if (length(desde) != 1L || length(hasta) != 1L || is.na(desde) || is.na(hasta) || desde > hasta) {
    stop("El periodo de la unidad comercial no es válido.")
  }
  x <- hechos[hechos$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO") &
    !is.na(hechos$FecFact) & hechos$FecFact <= hasta, , drop = FALSE]
  for (k in claves) x[[k]] <- .crm_id_texto(x[[k]])
  x$LinNegCod[x$LinNegCod == "20000"] <- "21000"
  mes <- as.Date(format(hasta, "%Y-%m-01"))
  totales <- x %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(SacosMes = .com_suma(SacosFact[FecFact >= mes]),
      MargenMes = .com_suma(Margen[FecFact >= mes]),
      SacosPeriodo = .com_suma(SacosFact[FecFact >= desde]), MargenPeriodo = .com_suma(Margen[FecFact >= desde]),
      PrimeraFacturacion = .gc_fecha_min(FecFact), UltimaFacturacion = .gc_fecha_max(FecFact), .groups = "drop")
  metas <- lapply(seq.int(as.integer(format(desde, "%Y")), as.integer(format(hasta, "%Y"))), function(anho) {
    inicio <- max(desde, as.Date(sprintf("%04d-01-01", anho)))
    fin <- min(cierre_presupuesto, as.Date(sprintf("%04d-12-31", anho)))
    meta <- .com_clientes(clientes, fin, anual = TRUE)
    for (k in claves) meta[[k]] <- .crm_id_texto(meta[[k]])
    meta$LinNegCod[meta$LinNegCod == "20000"] <- "21000"
    meses <- if (inicio > fin) 0L else as.integer(format(fin, "%m")) - as.integer(format(inicio, "%m")) + 1L
    meta %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
      dplyr::summarise(PresupuestoSacos = .com_suma(SSPpto) * meses / 12,
        PresupuestoMargen = .com_suma(MNFCCPpto) * meses / 12, .groups = "drop")
  })
  universo <- dplyr::distinct(dplyr::bind_rows(totales[claves],
    dplyr::bind_rows(metas)[claves]))
  metas <- lapply(metas, function(m) dplyr::right_join(m, universo, by = claves, relationship = "one-to-one"))
  meta <- dplyr::bind_rows(metas) %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(PresupuestoSacos = .com_suma(PresupuestoSacos),
      PresupuestoMargen = .com_suma(PresupuestoMargen), .groups = "drop")
  z <- dplyr::full_join(totales, meta, by = claves, relationship = "one-to-one")
  z$CumplimientoSacos <- .gc_ratio(z$SacosPeriodo, z$PresupuestoSacos)
  z$CumplimientoMargen <- .gc_ratio(z$MargenPeriodo, z$PresupuestoMargen)
  z$MesReferencia <- rep(mes, nrow(z))
  z$PeriodoDesde <- rep(desde, nrow(z)); z$PeriodoHasta <- rep(hasta, nrow(z))
  z$CortePresupuesto <- rep(cierre_presupuesto, nrow(z))
  z
}

# Totales comerciales: sumar valores y recalcular porcentajes sobre los totales ----
.crm_agregar_indicadores <- function(unidades, grupos) {
  cantidades <- intersect(c("SacosMes", "MargenMes", "SacosPeriodo", "MargenPeriodo",
    "PresupuestoSacos", "PresupuestoMargen"), names(unidades))
  x <- unidades %>% dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) %>%
    dplyr::summarise(Unidades = dplyr::n_distinct(Identificador),
      dplyr::across(dplyr::all_of(cantidades), .com_suma),
      PrimeraFacturacion = .gc_fecha_min(PrimeraFacturacion),
      UltimaFacturacion = .gc_fecha_max(UltimaFacturacion), .groups = "drop")
  x$CumplimientoSacos <- .gc_ratio(x$SacosPeriodo, x$PresupuestoSacos)
  x$CumplimientoMargen <- .gc_ratio(x$MargenPeriodo, x$PresupuestoMargen)
  x
}
