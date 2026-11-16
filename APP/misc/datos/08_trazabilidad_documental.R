# Documentos originales; conservar prefijo y granularidad de factura, pedido, línea y lote ----
.tz_facturas_extraer <- function(fecha_desde, consultar = function(sql) {
  racafeBD::ConsultaSistema("syscafe", query = sql)
}) {
  fecha <- as.Date(fecha_desde)
  if (length(fecha) != 1L || is.na(fecha)) stop("Fecha inicial documental inválida.")
  nacional <- consultar(paste(
    "SELECT D.CiaCod, D.FcnPrf AS PrefijoFactura, D.FcnNum AS NumeroFactura,",
    "D.PdcCod AS CLPdcCod, D.PdcLin AS CLPdcLin, D.FcnLot AS CLLotCod,",
    "H.FcnFec AS FecFact, H.FcnEtd AS EstadoFactura, D.FcnLotAnu AS Anulado,",
    "D.FcnSacLot AS CantidadOriginal, D.FcnKilLot AS Kilos, D.FcnValLot AS ValorDocumento,",
    "P.UMeCod AS UnidadOriginal, U.UMeFac AS KgPorUnidad, 'NACIONAL' AS TipoFactura",
    "FROM FCTFACN1 D INNER JOIN FCTFACNA H ON D.CiaCod = H.CiaCod",
    "AND D.FcnPrf = H.FcnPrf AND D.FcnNum = H.FcnNum",
    "LEFT JOIN EXPPEDID P ON D.CiaCod = P.CiaCod AND D.PdcCod = P.PdcCod",
    "LEFT JOIN NUNIMEDI U ON P.UMeCod = U.UMeCod",
    sprintf("WHERE D.CiaCod = 10 AND H.FcnFec >= '%s'", format(fecha, "%Y-%m-%d"))))
  exterior <- consultar(paste(
    "SELECT D.CiaCod, H.FctPrf AS PrefijoFactura, D.FctNum AS NumeroFactura,",
    "H.PdcCod AS CLPdcCod, H.PdcLin AS CLPdcLin, D.FctLot AS CLLotCod,",
    "H.FctFec AS FecFact, H.FctEtd AS EstadoFactura, D.FctLotAnu AS Anulado,",
    "D.FctSacEmb AS CantidadOriginal, U.UMeFac AS KgPorUnidad, P.UMeCod AS UnidadOriginal,",
    "'EXTERIOR' AS TipoFactura",
    "FROM FCTFACC2 D INNER JOIN FCTFACCA H",
    "ON D.CiaCod = H.CiaCod AND D.FctNum = H.FctNum",
    "LEFT JOIN EXPPEDID P ON H.CiaCod = P.CiaCod AND H.PdcCod = P.PdcCod",
    "LEFT JOIN NUNIMEDI U ON P.UMeCod = U.UMeCod",
    sprintf("WHERE D.CiaCod = 10 AND H.FctFec >= '%s'", format(fecha, "%Y-%m-%d"))))
  if (nrow(exterior)) exterior$Kilos <- exterior$CantidadOriginal * exterior$KgPorUnidad
  x <- dplyr::bind_rows(nacional, exterior)
  x$FecFact <- as.Date(x$FecFact)
  x$FecMes <- as.Date(format(x$FecFact, "%Y-%m-01"))
  # Misma regla de cantidad que .cargar_fact; los kilos físicos se conservan para la auditoría.
  x$SacosDesdeKilos70 <- x$Kilos / 70
  x$SacosFact70 <- ifelse(x$KgPorUnidad %in% 62.5, x$CantidadOriginal * 62.5 / 70, x$CantidadOriginal)
  x$DiferenciaUnidad70 <- x$SacosFact70 - x$SacosDesdeKilos70
  x$MetodoCantidad <- ifelse(x$KgPorUnidad %in% 62.5, "CANTIDAD × 62,5 / 70", "CANTIDAD ORIGINAL")
  x$EsValida <- ifelse(x$TipoFactura == "NACIONAL", toupper(trimws(x$EstadoFactura)) == "C",
    !is.na(x$Anulado) & !toupper(trimws(x$Anulado)) %in% c("S", "SI"))
  x$ReferenciaFactura <- paste(x$TipoFactura, x$PrefijoFactura, x$NumeroFactura, sep = ":")
  # No sumar ni eliminar filas repetidas: documentar su multiplicidad para revisar la clave de origen.
  clave <- c("CiaCod", "TipoFactura", "PrefijoFactura", "NumeroFactura", "CLPdcCod", "CLPdcLin", "CLLotCod")
  x$ClaveRepetida <- duplicated(x[clave]) | duplicated(x[clave], fromLast = TRUE)
  x$MargenDocumento <- NA_real_
  x$NotaMargen <- "No se distribuye el margen agregado del lote entre facturas sin una relación verificable."
  x
}

# Captura opcional inmutable, únicamente desde el cálculo observado ----
.tz_snapshot_margen <- function(datos, carpeta = Sys.getenv("CRM_SNAPSHOTS_MARGEN", unset = "")) {
  if (!nzchar(carpeta)) return(invisible(NULL))
  campos <- c("CLPdcCod", "CLPdcLin", "CLLotCod", "FecMes", "EstadoFinanciero", "SacosProyectados",
    "MargenProyectado", "MargenAbiertoEstimado", "SacosFact", "MetodoMargenLinea", "FuenteFinanciera")
  faltan <- setdiff(c("CLPdcCod", "CLPdcLin", "EstadoFinanciero", "MargenProyectado"), names(datos))
  if (length(faltan)) stop("No se puede capturar el snapshot: faltan ", paste(faltan, collapse = ", "))
  x <- datos[datos$EstadoFinanciero %in% c("PROYECTADO", "FACTURADO_ABIERTO"),
    intersect(campos, names(datos)), drop = FALSE]
  if (!nrow(x)) return(invisible(NULL))
  abierto <- x$EstadoFinanciero == "FACTURADO_ABIERTO"
  x$MargenEstimadoCapturado <- x$MargenProyectado
  x$SacosEstimadosCapturados <- if ("SacosProyectados" %in% names(x)) x$SacosProyectados else NA_real_
  if (any(abierto)) {
    x$MargenEstimadoCapturado[abierto] <- if ("MargenAbiertoEstimado" %in% names(x)) {
      x$MargenAbiertoEstimado[abierto]
    } else NA_real_
    x$SacosEstimadosCapturados[abierto] <- if ("SacosFact" %in% names(x)) x$SacosFact[abierto] else NA_real_
  }
  x$CalculadoEn <- Sys.time()
  x$VersionMetodologia <- "CRM-ETL-RESIDUAL-1"
  if (!dir.exists(carpeta) && !dir.create(carpeta, recursive = TRUE)) stop("No se pudo crear la carpeta de snapshots.")
  destino <- tempfile("margen_observado_", tmpdir = carpeta, fileext = ".rds")
  temporal <- tempfile(tmpdir = carpeta)
  on.exit(unlink(temporal), add = TRUE)
  saveRDS(x, temporal)
  if (!file.rename(temporal, destino)) stop("No se pudo conservar el snapshot de margen.")
  invisible(destino)
}

# Evidencia documental del cambio de unidad, sin redondear ni deduplicar ventas ----
.tz_auditar_unidades <- function(x) {
  if (!nrow(x)) return(x)
  x$DiferenciaContraKilos <- ifelse(x$TipoFactura == "NACIONAL", x$DiferenciaUnidad70, 0)
  x$CausaConversion <- ifelse(x$TipoFactura == "EXTERIOR", "Regla exterior sin cambio",
    ifelse(is.na(x$DiferenciaContraKilos), "Cantidad o kilos no disponibles",
    ifelse(x$DiferenciaContraKilos != 0, "Cantidad convertida y kilos físicos distintos",
      "Cantidad y kilos coinciden")))
  x[intersect(c("CiaCod", "TipoFactura", "PrefijoFactura", "NumeroFactura",
    "CLPdcCod", "CLPdcLin",
    "CLLotCod", "FecFact", "FecMes", "CantidadOriginal", "KgPorUnidad", "Kilos",
    "SacosDesdeKilos70",
    "SacosFact70", "DiferenciaContraKilos", "MetodoCantidad", "CausaConversion",
    "EsValida", "ClaveRepetida"), names(x))]
}

# Comparar ambas medidas contra el PyG por mes y línea, conservando faltantes y claves ambiguas ----
.tz_cuadre_unidades <- function(documentos, pyg, lotes = data.frame()) {
  requeridos <- c("SacosDesdeKilos70", "SacosFact70", "EsValida", "FecMes", "CLLotCod", "TipoFactura")
  if (!all(requeridos %in% names(documentos))) stop("Actualice el ETL para auditar la conversión de facturas.")
  mapa <- pyg[c("CLLotCod", "LinNegCod")]
  if (all(c("CLLotCod", "LinNegCod") %in% names(lotes))) mapa <- dplyr::bind_rows(mapa,
    lotes[c("CLLotCod", "LinNegCod")])
  mapa$LinNegCod <- ifelse(mapa$LinNegCod == 20000, 21000, mapa$LinNegCod)
  mapa <- mapa %>% dplyr::filter(!is.na(CLLotCod)) %>% dplyr::group_by(CLLotCod) %>%
    dplyr::summarise(LinNegCod = {
      v <- unique(LinNegCod[!is.na(LinNegCod)])
      if (length(v) == 1L) v else NA_real_
    }, .groups = "drop")
  sumar <- function(v) if (anyNA(v)) NA_real_ else sum(v)
  fact <- documentos %>% dplyr::filter(EsValida %in% TRUE) %>%
    dplyr::left_join(mapa, by = "CLLotCod", relationship = "many-to-one") %>%
    dplyr::mutate(Antes = ifelse(TipoFactura == "NACIONAL", SacosDesdeKilos70, SacosFact70)) %>%
    dplyr::group_by(FecMes, LinNegCod) %>%
    dplyr::summarise(FacturacionAntes = sumar(Antes), FacturacionCorregida = sumar(SacosFact70),
      FilasDocumentales = dplyr::n(), TieneFactura = TRUE, .groups = "drop")
  financiero <- pyg %>% dplyr::mutate(LinNegCod = ifelse(LinNegCod == 20000, 21000, LinNegCod)) %>%
    dplyr::group_by(FecMes, LinNegCod) %>%
    dplyr::summarise(SacosPYG = sumar(SacosPYG), TienePYG = TRUE, .groups = "drop")
  dplyr::full_join(fact, financiero, by = c("FecMes", "LinNegCod"), relationship = "one-to-one") %>%
    dplyr::mutate(
      DiferenciaAntes = ifelse(is.na(TienePYG), 0, SacosPYG) -
        ifelse(is.na(TieneFactura), 0, FacturacionAntes),
      DiferenciaCorregida = ifelse(is.na(TienePYG), 0, SacosPYG) -
        ifelse(is.na(TieneFactura), 0, FacturacionCorregida),
      Revision = ifelse(is.na(LinNegCod), "Línea ausente o ambigua: revisar lote", "Comparación por mes y línea")) %>%
    dplyr::arrange(FecMes, LinNegCod)
}
