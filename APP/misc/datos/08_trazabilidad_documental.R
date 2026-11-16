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
    "EsValida", "ClaveRepetida", "CodigoLineaOriginal", "ProductoOriginal", "LinNegCodFinanciero",
    "ClaseAlcance", "MotivoAlcance"), names(x))]
}

# El alcance financiero no se deduce únicamente de un código de línea homologado ----
.tz_clasificar_alcance <- function(documentos, pyg, lotes = data.frame()) {
  numero <- function(x) suppressWarnings(as.numeric(as.character(x)))
  unico <- function(x) {
    v <- unique(x[!is.na(x)])
    if (length(v) == 1L) v else NA_real_
  }
  financiero <- pyg[c("CLLotCod", "LinNegCod")]
  financiero$CLLotCod <- numero(financiero$CLLotCod)
  financiero$LinNegCod <- numero(financiero$LinNegCod)
  financiero$LinNegCod[financiero$LinNegCod == 20000] <- 21000
  financiero <- financiero %>%
    dplyr::filter(!is.na(CLLotCod), CLLotCod != 0, LinNegCod %in% c(10000, 21000)) %>%
    dplyr::group_by(CLLotCod) %>% dplyr::summarise(
      LineaPYGAlcance = unico(LinNegCod), LineasPYG = dplyr::n_distinct(LinNegCod, na.rm = TRUE), .groups = "drop")
  original <- data.frame(CLLotCod = numeric(), CodigoLineaOriginal = numeric(), ProductoOriginal = numeric())
  if (is.data.frame(lotes) && "CLLotCod" %in% names(lotes)) {
    n <- nrow(lotes)
    campo <- function(candidatos) {
      k <- intersect(candidatos, names(lotes))
      if (length(k)) numero(lotes[[k[1]]]) else rep(NA_real_, n)
    }
    codigo <- campo(c("CLLinNegCo", "LinNegCodOrigen"))
    # La homologación solo perdió el origen de 20000/21000; 10000 sigue siendo inequívoco.
    if ("LinNegCod" %in% names(lotes)) {
      codigo[is.na(codigo) & numero(lotes$LinNegCod) %in% 10000] <- 10000
    }
    original <- data.frame(CLLotCod = numero(lotes$CLLotCod), CodigoLineaOriginal = codigo,
      ProductoOriginal = campo(c("CLLinProCo", "LinProCod"))) %>%
      dplyr::filter(!is.na(CLLotCod), CLLotCod != 0) %>% dplyr::group_by(CLLotCod) %>%
      dplyr::summarise(CodigoLineaOriginal = unico(CodigoLineaOriginal),
        ProductoOriginal = unico(ProductoOriginal), .groups = "drop")
  }
  mapa <- dplyr::full_join(financiero, original, by = "CLLotCod", relationship = "one-to-one")
  # Mismo catálogo de tipos de producto usado por el universo operativo anterior a la ampliación 20000.
  tipos_industria <- c(90030, 90040, 40050, 40100, 90025, 40150, 40200, 40010, 90035, 40250)
  mapa$LineaSistemaAlcance <- ifelse(mapa$CodigoLineaOriginal %in% 21000, 21000,
    ifelse(mapa$CodigoLineaOriginal %in% 10000 & mapa$ProductoOriginal %in% tipos_industria, 10000, NA_real_))
  mapa$LinNegCodFinanciero <- dplyr::coalesce(mapa$LineaPYGAlcance, mapa$LineaSistemaAlcance)
  mapa$LinNegCodFinanciero[which(mapa$LineasPYG > 1L)] <- NA_real_
  mapa$ClaseAlcance <- dplyr::case_when(
    mapa$LineasPYG > 1L ~ "POR REVISAR",
    mapa$LinNegCodFinanciero %in% c(10000, 21000) ~ "INCLUIDO",
    mapa$CodigoLineaOriginal == 20000 ~ "POR REVISAR",
    !is.na(mapa$CodigoLineaOriginal) ~ "FUERA DEL ALCANCE",
    TRUE ~ "POR REVISAR")
  mapa$MotivoAlcance <- dplyr::case_when(
    mapa$LineasPYG > 1L ~ "El lote tiene varias líneas financieras; no se distribuye su factura arbitrariamente",
    mapa$LineaPYGAlcance %in% c(10000, 21000) ~ "Lote documentado en el PyG de Industria Nacional",
    mapa$LineaSistemaAlcance %in% c(10000, 21000) ~ "Lote del universo operativo original de Industria Nacional",
    mapa$CodigoLineaOriginal == 20000 ~ "Código original 20000 sin evidencia de pertenencia al PyG comparado",
    !is.na(mapa$CodigoLineaOriginal) ~ "Línea o tipo de producto fuera del universo comparado",
    TRUE ~ "No hay una referencia original de lote que permita atribuir la factura")
  documentos <- documentos %>% dplyr::select(-dplyr::any_of(setdiff(names(mapa), "CLLotCod")))
  documentos$CLLotCod <- numero(documentos$CLLotCod)
  documentos <- dplyr::left_join(documentos, mapa, by = "CLLotCod", relationship = "many-to-one")
  documentos$ClaseAlcance[is.na(documentos$ClaseAlcance)] <- "POR REVISAR"
  documentos$MotivoAlcance[is.na(documentos$MotivoAlcance)] <-
    "No hay una referencia original de lote que permita atribuir la factura"
  documentos
}

# Comparar ambas medidas contra el PyG por mes y línea, conservando faltantes y claves ambiguas ----
.tz_cuadre_unidades <- function(documentos, pyg, lotes = data.frame()) {
  requeridos <- c("SacosDesdeKilos70", "SacosFact70", "EsValida", "FecMes", "CLLotCod", "TipoFactura")
  if (!all(requeridos %in% names(documentos))) stop("Actualice el ETL para auditar la conversión de facturas.")
  documentos <- .tz_clasificar_alcance(documentos, pyg, lotes)
  sumar <- function(v) if (anyNA(v)) NA_real_ else sum(v)
  fact <- documentos %>% dplyr::filter(EsValida %in% TRUE, ClaseAlcance == "INCLUIDO") %>%
    dplyr::mutate(LinNegCod = LinNegCodFinanciero) %>%
    dplyr::mutate(Antes = ifelse(TipoFactura == "NACIONAL", SacosDesdeKilos70, SacosFact70)) %>%
    dplyr::group_by(FecMes, LinNegCod) %>%
    dplyr::summarise(FacturacionAntes = sumar(Antes), FacturacionCorregida = sumar(SacosFact70),
      FilasDocumentales = dplyr::n(), TieneFactura = TRUE, .groups = "drop")
  financiero <- pyg %>% dplyr::mutate(LinNegCod = ifelse(LinNegCod == 20000, 21000, LinNegCod)) %>%
    dplyr::filter(LinNegCod %in% c(10000, 21000)) %>% dplyr::group_by(FecMes, LinNegCod) %>%
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

# Las diferencias mensuales conservan las claves de lote y las referencias de sus facturas ----
.tz_cuadre_lotes <- function(documentos, pyg, lotes = data.frame()) {
  documentos <- .tz_clasificar_alcance(documentos, pyg, lotes)
  sumar <- function(v) if (anyNA(v)) NA_real_ else sum(v)
  fact <- documentos %>% dplyr::filter(EsValida %in% TRUE, ClaseAlcance == "INCLUIDO") %>%
    dplyr::mutate(LinNegCod = LinNegCodFinanciero,
      Referencia = paste(TipoFactura, PrefijoFactura, NumeroFactura, sep = ":")) %>%
    dplyr::group_by(FecMes, LinNegCod, CLLotCod) %>% dplyr::summarise(
      SacosFactura = sumar(SacosFact70), Facturas = paste(unique(Referencia), collapse = " | "),
      TieneFactura = TRUE, .groups = "drop")
  financiero <- pyg %>% dplyr::mutate(LinNegCod = ifelse(LinNegCod == 20000, 21000, LinNegCod)) %>%
    dplyr::filter(LinNegCod %in% c(10000, 21000)) %>%
    dplyr::group_by(FecMes, LinNegCod, CLLotCod) %>%
    dplyr::summarise(SacosPYG = sumar(SacosPYG), TienePYG = TRUE, .groups = "drop")
  dplyr::full_join(fact, financiero, by = c("FecMes", "LinNegCod", "CLLotCod"), relationship = "one-to-one") %>%
    dplyr::mutate(DiferenciaSacos = ifelse(is.na(TienePYG), 0, SacosPYG) -
      ifelse(is.na(TieneFactura), 0, SacosFactura),
      Causa = dplyr::case_when(
        is.na(TieneFactura) ~ "PyG sin factura en este mes; revisar fechas y validez documental",
        is.na(TienePYG) ~ "Factura sin PyG en este mes; revisar reconocimiento financiero",
        is.na(DiferenciaSacos) ~ "Cantidad desconocida en alguna fuente",
        DiferenciaSacos != 0 ~ "Cantidad comercial distinta de los sacos del PyG",
        TRUE ~ "Conciliado")) %>% dplyr::arrange(FecMes, LinNegCod, CLLotCod)
}
