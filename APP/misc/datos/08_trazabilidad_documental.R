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
    "P.UMeCod AS UnidadOriginal, 'NACIONAL' AS TipoFactura",
    "FROM FCTFACN1 D INNER JOIN FCTFACNA H ON D.CiaCod = H.CiaCod",
    "AND D.FcnPrf = H.FcnPrf AND D.FcnNum = H.FcnNum",
    "LEFT JOIN EXPPEDID P ON D.CiaCod = P.CiaCod AND D.PdcCod = P.PdcCod",
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
  x$SacosFact70 <- x$Kilos / 70
  x$EsValida <- toupper(trimws(x$EstadoFactura)) == "C" & !toupper(trimws(x$Anulado)) %in% c("S", "SI")
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
