# Panel aprobado: estados comerciales y ventas mensuales ----
.coha_preparar <- function(segmentos, clientes, hechos, hoy = Sys.Date()) {
  requeridas <- list(segmentos = c("FecProceso", "LinNegCod", "CliNitPpal", "SegmentoRacafe"),
    clientes = c("FecProceso", "LinNegCod", "CliNitPpal", "Asesor", "Segmento", "NumMesesRecuperar",
      "SSPpto", "MNFCCPpto", "Excluir"),
    hechos = c("LinNegCod", "CliNitPpal", "PerRazSoc", "FecFact", "SacosFact", "Margen", "EstadoFinanciero"))
  fuentes <- list(segmentos = segmentos, clientes = clientes, hechos = hechos)
  for (nombre in names(requeridas)) {
    if (!is.data.frame(fuentes[[nombre]]) || !all(requeridas[[nombre]] %in% names(fuentes[[nombre]]))) {
      stop(paste("Fuente de cohortes incompatible:", nombre), call. = FALSE)
    }
  }
  anio <- lubridate::year(hoy)
  claves <- c("LinNegCod", "CliNitPpal")
  segmentos$FecProceso <- as.Date(segmentos$FecProceso)
  t1 <- segmentos %>%
    dplyr::filter(lubridate::year(FecProceso) == anio, FecProceso <= hoy,
      !is.na(LinNegCod), !is.na(CliNitPpal)) %>%
    dplyr::transmute(FecProceso, LinNegCod, CliNitPpal,
      Estado = dplyr::recode(SegmentoRacafe, "CLIENTE" = "ACTIVO", "CLIENTE A RECUPERAR" = "INACTIVO")) %>%
    dplyr::distinct()
  if (any(!is.na(t1$Estado) & !t1$Estado %in% c("ACTIVO", "INACTIVO"))) {
    stop("CRMNALSEGR contiene estados no reconocidos para el panel de cohortes.", call. = FALSE)
  }
  if (anyDuplicated(t1[c("FecProceso", claves)])) {
    stop("CRMNALSEGR contiene estados contradictorios para una misma unidad comercial y fecha.", call. = FALSE)
  }
  # El presupuesto vigente es invariante por unidad comercial, como en el módulo aprobado.
  t2 <- clientes %>% dplyr::filter(!is.na(FecProceso), as.Date(FecProceso) <= hoy) %>%
    dplyr::group_by(LinNegCod, CliNitPpal) %>%
    dplyr::slice_max(FecProceso, n = 1L, with_ties = FALSE) %>% dplyr::ungroup() %>%
    dplyr::filter(lubridate::year(FecProceso) == anio, Excluir != "SI") %>%
    dplyr::transmute(LinNegCod, CliNitPpal, Asesor, Segmento, NumMesesRecuperar,
      PPtoSacos = SSPpto / 12, PPtoMargen = MNFCCPpto / 12,
      Presupuestado = dplyr::case_when(is.na(SSPpto) ~ "NO DISPONIBLE",
        SSPpto > 0 ~ "PRESUPUESTADO", TRUE ~ "NO PRESUPUESTADO"), Excluir)
  # SacosFact ya está expresado en 70 kg; no convertir otra vez ni incluir proyecciones.
  fact <- hechos %>% dplyr::filter(!is.na(FecFact), FecFact <= hoy,
    EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"))
  t3 <- fact %>% dplyr::filter(lubridate::year(FecFact) == anio) %>%
    dplyr::group_by(LinNegCod, CliNitPpal, FecProceso = lubridate::floor_date(FecFact, "month")) %>%
    dplyr::summarise(Sacos = sum(SacosFact, na.rm = TRUE),
      Margen = if (anyNA(Margen) || any(EstadoFinanciero != "CERRADO")) NA_real_ else sum(Margen),
      TieneVenta = TRUE, .groups = "drop")
  t4 <- hechos %>% dplyr::filter(!is.na(CliNitPpal), !is.na(LinNegCod)) %>%
    dplyr::arrange(dplyr::desc(FecFact)) %>% dplyr::group_by(LinNegCod, CliNitPpal) %>%
    dplyr::summarise(PerRazSoc = dplyr::first(PerRazSoc), .groups = "drop") %>%
    dplyr::left_join(fact %>% dplyr::group_by(LinNegCod, CliNitPpal) %>%
      dplyr::summarise(UltFecFact = if (dplyr::n() > 0L) max(as.Date(FecFact)) else as.Date(NA),
        .groups = "drop"), by = claves)
  # NUEVO se propaga a toda la trayectoria anual de la UC, sin reinterpretarlo como primera compra.
  panel <- dplyr::full_join(t1, t3, by = c("FecProceso", claves)) %>%
    dplyr::left_join(t4, by = claves) %>%
    dplyr::mutate(Estado = dplyr::coalesce(Estado, "NUEVO"), Sacos = dplyr::coalesce(Sacos, 0),
      Margen = dplyr::if_else(is.na(TieneVenta), 0, Margen)) %>%
    dplyr::group_by(LinNegCod, CliNitPpal) %>%
    dplyr::mutate(EstadoPanel = if (any(Estado == "NUEVO")) "NUEVO" else Estado,
      FecIngreso = if (any(Estado == "NUEVO")) min(FecProceso[Estado == "NUEVO"])
        else as.Date(paste0(anio, "-01-01")),
      EdadPanel = lubridate::interval(FecIngreso, FecProceso) %/% lubridate::period(1, units = "month") + 1) %>%
    dplyr::ungroup() %>% dplyr::inner_join(t2, by = claves) %>%
    dplyr::select(-TieneVenta) %>% dplyr::arrange(FecProceso, LinNegCod, CliNitPpal)
  panel
}

# Formatos y meses del módulo, sin dependencia de la librería antigua ----
.coha_mes <- function(fecha) {
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
    "septiembre", "octubre", "noviembre", "diciembre")
  paste(meses[as.integer(format(as.Date(fecha), "%m"))], format(as.Date(fecha), "%Y"))
}
.coha_formato <- function(tipo) {
  switch(tipo, numero = scales::label_number(accuracy = 0.01, big.mark = ",", decimal.mark = "."),
    coma = scales::label_number(accuracy = 0.01, big.mark = ",", decimal.mark = "."),
    dinero = scales::label_dollar(accuracy = 0.01, big.mark = ",", decimal.mark = "."),
    porcentaje = scales::label_percent(accuracy = 0.01, decimal.mark = "."))
}

# Acotar meses al año de la fecha final aplicada en el controlbar ----
.coha_acotar_mes <- function(fecha, limite) {
  fin <- as.Date(format(as.Date(limite), "%Y-%m-01"))
  inicio <- as.Date(format(fin, "%Y-01-01"))
  max(inicio, min(as.Date(format(as.Date(fecha), "%Y-%m-01")), fin))
}

# Una celda corresponde exclusivamente a una transición de estado ----
.coha_transicion_detalle <- function(transiciones, mensual, origen, destino) {
  if (!origen %in% c("ACTIVO", "INACTIVO") || !destino %in% c("ACTIVO", "INACTIVO")) {
    stop("Seleccione una transición entre estados válidos.")
  }
  x <- transiciones %>% dplyr::filter(as.character(EstadoInicial) == origen, as.character(EstadoFinal) == destino)
  claves <- c("CliNitPpal", "LinNegCod")
  if (anyDuplicated(x[claves])) stop("La transición contiene unidades comerciales duplicadas.")
  mensual$Linea <- .gc_columna(mensual, c("CLLinNegNo", "LinNeg", "LinNegocio"))
  meta <- mensual %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(Cliente = .gc_unico(PerRazSoc), Linea = .gc_unico(Linea),
      Sacos = .com_suma(Sacos), Margen = .com_suma(Margen), .groups = "drop")
  dplyr::left_join(x, meta, by = claves, relationship = "one-to-one")
}

# Proyección comercial del mes calendario sin modificar el cierre oficial ----
.coha_proyecciones_mes <- function(oficial, hechos, hoy = Sys.Date(), corte = hoy) {
  claves <- c("CliNitPpal", "LinNegCod")
  for (k in claves) {
    oficial[[k]] <- .crm_id_texto(oficial[[k]])
    hechos[[k]] <- .crm_id_texto(hechos[[k]])
  }
  if (anyDuplicated(oficial[claves])) stop("El cierre oficial tiene unidades duplicadas.")
  mes <- as.Date(format(hoy, "%Y-%m-01"))
  fin_mes <- seq(mes, by = "month", length.out = 2L)[[2]] - 1L
  h <- hechos[hechos$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO") &
    !is.na(hechos$FecFact) & hechos$FecFact <= min(as.Date(hoy), as.Date(corte)), , drop = FALSE]
  ultima <- h %>% dplyr::filter(!is.na(SacosFact), SacosFact > 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(UltimaDisponible = .gc_fecha_max(FecFact), .groups = "drop")
  movimiento <- h %>% dplyr::filter(FecFact >= mes) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(ComprasMes = sum(SacosFact > 0, na.rm = TRUE), MovimientosMes = dplyr::n(),
      SacosMesVigente = .com_suma(SacosFact), MargenMesVigente = .com_suma(Margen), .groups = "drop")
  x <- oficial %>% dplyr::left_join(ultima, by = claves, relationship = "one-to-one") %>%
    dplyr::left_join(movimiento, by = claves, relationship = "one-to-one")
  sin_movimiento <- is.na(x$MovimientosMes)
  x$SacosMesVigente[sin_movimiento] <- 0
  x$MargenMesVigente[sin_movimiento] <- 0
  x$UltimaDisponible <- pmax(as.Date(x$UltFecFact), x$UltimaDisponible, na.rm = TRUE)
  x$FechaLimite <- x$UltimaDisponible %m+% lubridate::period(month = as.numeric(x$NumMesesRecuperar))
  x$EstadoOficial <- as.character(x$EstadoPanel)
  x$EstadoProyectado <- dplyr::case_when(x$EstadoOficial == "INACTIVO" & x$ComprasMes > 0 ~ "ACTIVO",
    x$EstadoOficial == "ACTIVO" & !is.na(x$FechaLimite) & x$FechaLimite <= fin_mes ~ "INACTIVO",
    TRUE ~ x$EstadoOficial)
  x$MesProyeccion <- rep(mes, nrow(x))
  x
}
