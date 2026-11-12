# Contratos analíticos: conservar procedencia y no sustituir importes desconocidos ----
.crm_id_texto <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE, digits = 22, big.mark = "")
  y <- trimws(as.character(x)); y[is.na(x) | y == ""] <- NA_character_; y
}
.crm_capa_ejecucion <- function(x) {
  n <- nrow(x)
  campo <- function(nombre, defecto = NA_real_) if (nombre %in% names(x)) x[[nombre]] else rep(defecto, n)
  estado <- campo("EstadoFinanciero", NA_character_)
  x$EstadoEjecucion <- unname(c(CERRADO = "Cerrado financiero", FACTURADO_ABIERTO = "Facturado sin cierre",
    PROYECTADO = "Estimado comercial")[estado])
  x$EsEstimado <- estado == "PROYECTADO"
  x$FechaAnalitica <- as.Date(campo("FecFact", as.Date(NA)))
  x$FechaAnalitica[x$EsEstimado %in% TRUE] <- as.Date(campo("PdcFecEst", as.Date(NA)))[x$EsEstimado %in% TRUE]
  x$SacosAnaliticos70 <- ifelse(x$EsEstimado, campo("SacosProyectados"), campo("SacosFact"))
  x$MargenAnalitico <- ifelse(estado == "CERRADO", campo("MargenContable"),
    ifelse(estado == "FACTURADO_ABIERTO", campo("MargenAbiertoEstimado"), campo("MargenProyectado")))
  if (all(c("CLLotCod", "SacosFact", "FacturadosOperativos70") %in% names(x))) {
    consolidado <- x %>% dplyr::filter(!is.na(CLLotCod), as.character(CLLotCod) != "0") %>%
      dplyr::group_by(CLLotCod) %>% dplyr::summarise(
        Financiero = {
          v <- SacosFact[EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO")]
          if (anyNA(v)) NA_real_ else sum(v)
        },
        Operativo = {
          v <- unique(FacturadosOperativos70[!is.na(FacturadosOperativos70)])
          if (length(v) == 1L) v[[1]] else NA_real_
        }, .groups = "drop")
    consolidado$FacturadosConsolidados70 <- pmax(consolidado$Financiero, consolidado$Operativo)
    x$FacturadosConsolidados70 <- consolidado$FacturadosConsolidados70[match(x$CLLotCod, consolidado$CLLotCod)]
  }
  x$OrigenDato <- campo("FuenteFinanciera", NA_character_)
  x$FechaCorteFuente <- as.Date(ifelse(estado == "CERRADO", as.numeric(as.Date(campo("FechaCortePyG", as.Date(NA)))),
    as.numeric(as.Date(campo("FechaCorteOperativo", as.Date(NA))))), origin = "1970-01-01")
  # Se conservan las métricas aditivas antiguas, pero un margen aplicable desconocido sigue siendo NA.
  for (par in list(c("CERRADO", "MetricaMargenContable", "MargenContable"),
      c("FACTURADO_ABIERTO", "MetricaMargenAbiertoEstimado", "MargenAbiertoEstimado"),
      c("PROYECTADO", "MetricaMargenProyectado", "MargenProyectado"))) {
    x[[par[[2]]]] <- ifelse(estado == par[[1]], campo(par[[3]]), 0)
  }
  for (par in list(c("CERRADO", "MetricaSacosCerrados70", "SacosFact"),
      c("FACTURADO_ABIERTO", "MetricaSacosAbiertos70", "SacosFact"),
      c("PROYECTADO", "MetricaSacosProyectados70", "SacosProyectados"))) {
    x[[par[[2]]]] <- ifelse(estado == par[[1]], campo(par[[3]]), 0)
  }
  x$MetricaSacosFacturados70 <- x$MetricaSacosCerrados70 + x$MetricaSacosAbiertos70
  x$MetricaSacosEscenario70 <- x$MetricaSacosFacturados70 + x$MetricaSacosProyectados70
  creacion <- as.Date(campo("FechaCreacionPedido", as.Date(NA)))
  mes_creacion <- as.Date(format(creacion, "%Y-%m-01"))
  x$RevisionFechaAnalitica <- dplyr::case_when(
    is.na(x$FechaAnalitica) & !is.na(x$EstadoEjecucion) ~ "Sin fecha para la evidencia disponible",
    estado == "CERRADO" & x$FechaAnalitica < mes_creacion ~ "Reconocimiento anterior al mes del pedido",
    estado != "CERRADO" & x$FechaAnalitica < creacion ~ "Fecha anterior a la creación del pedido",
    x$EsEstimado & x$FechaAnalitica < Sys.Date() ~ "Estimación vencida; se conserva la fecha original",
    TRUE ~ NA_character_)
  x$MetricaMargenEscenario <- x$MetricaMargenContable + x$MetricaMargenAbiertoEstimado + x$MetricaMargenProyectado
  x
}

# Diagnóstico de clasificación: jamás elimina ventas ni elige una correspondencia ambigua ----
.crm_diagnostico_productos <- function(x, productos) {
  claves <- c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
  llaves <- function(d) do.call(paste, c(lapply(d[claves], .crm_id_texto), sep = "|"))
  if ("FecProceso" %in% names(productos)) productos <- productos %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::filter(if (all(is.na(FecProceso))) TRUE else FecProceso %in% max(FecProceso, na.rm = TRUE)) %>%
    dplyr::ungroup()
  p <- unique(productos[c(claves, "Categoria", "Producto")])
  ambiguas <- unique(llaves(p)[duplicated(llaves(p)) | duplicated(llaves(p), fromLast = TRUE)])
  i <- match(llaves(x), llaves(p)); salida <- x[intersect(c("PdcCod", "PdcLin", claves), names(x))]
  causa <- rep("", nrow(x))
  agregar <- function(cond, texto) causa[which(cond)] <<- paste(causa[which(cond)], texto, sep = " · ")
  for (k in claves) agregar(is.na(x[[k]]) | as.character(x[[k]]) %in% c("", "0"), paste("Sin", k))
  agregar(llaves(x) %in% ambiguas, "Homologación múltiple")
  agregar(is.na(i), "Sin homologación CRM")
  agregar(!is.na(i) & (is.na(p$Categoria[i]) | p$Categoria[i] %in% c("", "SIN DATO")), "Sin categoría CRM")
  agregar(!is.na(i) & (is.na(p$Producto[i]) | p$Producto[i] %in% c("", "SIN DATO")), "Sin producto CRM")
  salida$Diagnostico <- sub("^ · ", "", causa)
  salida[nzchar(salida$Diagnostico), , drop = FALSE]
}

# Un único segmento actual por NIT y línea; informar empates contradictorios ----
.crm_segmento_vigente <- function(x) {
  campos <- c("CLIENTE", "LINNEG", "SEGMENTACION_CLIENTE")
  if (!all(campos %in% names(x))) stop("PyG no contiene la llave y el segmento esperados.")
  fecha <- intersect(c("FecProceso", "FECHA_ACTUALIZACION", "FECHA_CORTE_ARCHIVO", "FechaCorteArchivo", "FECFACTURA"), names(x))
  if (!length(fecha)) stop("PyG no tiene fecha efectiva para identificar el segmento vigente.")
  fecha_efectiva <- as.Date(rep(NA_character_, nrow(x)))
  for (campo in fecha) fecha_efectiva <- dplyr::coalesce(fecha_efectiva, as.Date(x[[campo]]))
  d <- data.frame(NITSegmento = .crm_id_texto(x$CLIENTE), LinNegCod = as.character(x$LINNEG),
    FechaSegmento = fecha_efectiva, SegmentoVigente = as.character(x$SEGMENTACION_CLIENTE))
  d$LinNegCod[d$LinNegCod == "20000"] <- "21000"
  d %>% dplyr::filter(!is.na(NITSegmento), !is.na(FechaSegmento)) %>%
    dplyr::group_by(NITSegmento, LinNegCod) %>% dplyr::filter(FechaSegmento == max(FechaSegmento)) %>%
    dplyr::summarise(FechaSegmento = max(FechaSegmento),
      SegmentoAmbiguo = dplyr::n_distinct(SegmentoVigente, na.rm = TRUE) > 1L,
      SegmentoVigente = if (dplyr::n_distinct(SegmentoVigente, na.rm = TRUE) == 1L)
        sort(unique(SegmentoVigente[!is.na(SegmentoVigente)]))[[1]] else NA_character_, .groups = "drop")
}

# Cobertura observada de CRMNALLOT, sin inventar un año de inicio ----
.crm_cobertura_lotes <- function(x) {
  fecha <- intersect(c("FecAsignLote", "CLLotFec", "FecMes", "FecProceso"), names(x))
  if (!length(fecha)) return(as.Date(NA))
  fechas <- as.Date(x[[fecha[[1]]]])
  fechas <- fechas[!is.na(fechas) & fechas > as.Date("1900-01-01")]
  if (length(fechas)) min(fechas) else as.Date(NA)
}

# Estados separados por mes: el residual comercial no se suma otra vez como facturación ----
.crm_capas_mensuales <- function(x, desde, hasta) {
  if (!all(c("EstadoEjecucion", "FechaAnalitica", "SacosAnaliticos70", "MargenAnalitico") %in% names(x))) {
    x <- .crm_capa_ejecucion(x)
  }
  x <- x[!is.na(x$FechaAnalitica) & x$FechaAnalitica >= desde &
    (x$FechaAnalitica <= hasta | x$EsEstimado %in% TRUE), , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  x$Mes <- as.Date(format(x$FechaAnalitica, "%Y-%m-01"))
  estricto <- function(v) if (anyNA(v)) NA_real_ else sum(v)
  x %>% dplyr::group_by(Mes, EstadoEjecucion) %>% dplyr::summarise(
    Sacos = estricto(SacosAnaliticos70), Margen = estricto(MargenAnalitico), Registros = dplyr::n(), .groups = "drop")
}
