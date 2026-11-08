# Importación de datos, sin ejecutar código de la app antigua ----
.com_leer_historico <- function(ruta) {
  if (!file.exists(ruta)) stop("No existe la fuente comercial indicada.")
  extension <- tolower(tools::file_ext(ruta))
  temporal <- NULL
  if (extension == "zip") {
    listado <- utils::unzip(ruta, list = TRUE)$Name
    candidato <- listado[grepl("(^|/)APP/data/datos[.]Rdata$", listado, ignore.case = TRUE)]
    if (length(candidato) != 1L || grepl("(^|/)\\.\\.(/|$)", candidato)) stop("ZIP sin datos comerciales únicos.")
    temporal <- tempfile("comercial_")
    dir.create(temporal)
    on.exit(unlink(temporal, recursive = TRUE), add = TRUE)
    utils::unzip(ruta, files = candidato, exdir = temporal)
    ruta <- file.path(temporal, candidato)
    extension <- "rdata"
  }
  if (extension == "rds") {
    fuente <- readRDS(ruta)
  } else if (extension %in% c("rdata", "rda")) {
    env <- new.env(parent = emptyenv())
    load(ruta, envir = env)
    fuente <- as.list(env)
  } else stop("Use ZIP, RData o RDS.")
  if (!is.list(fuente)) stop("La fuente debe contener tablas identificadas.")
  mapa <- c(comercial = "data_comercial", categorias = "data_categoria", cartera = "data_cartera",
    facturacion = "data_facturacion", compras = "Ultima_Compra", clientes = "clientesNacionales", tasas = "Tasas_Anual")
  resultado <- list()
  for (nombre in names(mapa)) {
    tabla <- fuente[[nombre]]
    if (is.null(tabla)) tabla <- fuente[[mapa[[nombre]]]]
    if (!is.null(tabla)) {
      if (!is.data.frame(tabla)) stop(paste("Fuente incompatible:", nombre))
      tabla <- as.data.frame(tabla)
      fechas <- intersect(c("Fecha", "FecProceso", "Fecha_Creacion", "Fec_Ultima_Compra", "TCEFch", "TCEFchVto",
        "FcnFec", "FecPag", "FecVto", "PerFecCre"), names(tabla))
      for (col in fechas) {
        tabla[[col]] <- as.Date(tabla[[col]])
        tabla[[col]][tabla[[col]] <= as.Date("1900-01-01")] <- as.Date(NA)
      }
      resultado[[nombre]] <- tabla
    }
  }
  if (!length(resultado)) stop("No se encontraron tablas comerciales reconocidas.")
  requeridas <- list(comercial = c("FctNit", "Fecha", "Volumen", "Margen", "PptoVolumen", "PptoMargen"),
    cartera = c("TCEFch", "Factura", "PerCod", "TCEFchVto", "SaldoCOP"),
    facturacion = c("Factura", "PerCod", "FcnFec", "FecPag", "FcnVal", "FcnSal"))
  for (nombre in intersect(names(requeridas), names(resultado))) {
    if (!all(requeridas[[nombre]] %in% names(resultado[[nombre]]))) stop(paste("Faltan columnas:", nombre))
  }
  resultado$importado_en <- if (!is.null(fuente$importado_en)) fuente$importado_en else Sys.time()
  resultado
}

.com_clientes <- function(datos, corte, filtros = NULL, anual = FALSE) {
  x <- datos
  requeridas <- c("CliNitPpal", "LinNegCod", "Asesor", "Segmento", "SSPpto", "MNFCCPpto")
  if (!all(requeridas %in% names(x))) stop("Faltan columnas del catálogo de clientes/presupuesto.")
  if ("FecProceso" %in% names(x)) {
    x$.OrdenProceso <- as.numeric(as.POSIXct(x$FecProceso, tz = "UTC"))
    x$FecProceso <- as.Date(x$FecProceso)
    x <- x[!is.na(x$FecProceso) & x$FecProceso <= corte, , drop = FALSE]
    if (anual) x <- x[format(x$FecProceso, "%Y") == format(corte, "%Y"), , drop = FALSE]
    x <- x %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
      dplyr::filter(.OrdenProceso ==
        if (any(!is.na(.OrdenProceso))) max(.OrdenProceso, na.rm = TRUE) else NA_real_) %>% dplyr::ungroup()
  } else if (anual) return(x[0, , drop = FALSE])
  x$CLLinNegNo <- as.character(.gc_columna(x, c("LinNegocio", "CLLinNegNo")))
  faltante <- is.na(x$CLLinNegNo) | !nzchar(x$CLLinNegNo)
  codigos <- c(`10000` = "CONVENCIONALES", `21000` = "A LA MEDIDA", `20000` = "DIFERENCIADOS")
  x$CLLinNegNo[faltante] <- unname(codigos[as.character(x$LinNegCod[faltante])])
  x$PerRazSoc <- as.character(.gc_columna(x, c("PerRazSoc", "Cliente")))
  x$Depto <- as.character(.gc_columna(x, c("Depto", "Departamento")))
  x$Mpio <- as.character(.gc_columna(x, c("Mpio", "Municipio")))
  x$Region <- as.character(.gc_columna(x, c("Region", "Región")))
  conservar <- unique(c(requeridas, "FecProceso", "CLLinNegNo", "PerRazSoc", "Depto", "Mpio", "Region"))
  x <- x %>% dplyr::select(dplyr::any_of(conservar)) %>% dplyr::distinct()
  if (anyDuplicated(x[c("CliNitPpal", "LinNegCod")])) stop("El presupuesto tiene snapshots contradictorios.")
  if (!is.null(filtros)) {
    for (par in list(c("Asesor", "asesor"), c("Segmento", "segmento"), c("CLLinNegNo", "linneg"))) {
      x <- x[x[[par[[1]]]] %in% filtros[[par[[2]]]], , drop = FALSE]
    }
  }
  x
}
.com_hechos <- function(datos, corte, cerrados = TRUE) {
  x <- datos %>% dplyr::filter(!is.na(FecFact), FecFact <= corte,
    EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"))
  if (cerrados) x <- x[x$EstadoFinanciero == "CERRADO", , drop = FALSE]
  x$Fecha <- .gc_mes(x$FecFact)
  x
}
.COM_DIMENSIONES <- c("Línea de negocio" = "CLLinNegNo", "Segmento" = "Segmento", "Asesor" = "Asesor",
  "Cliente principal" = "PerRazSoc", "Departamento" = "Depto", "Municipio" = "Mpio", "Región" = "Region",
  "Categoría" = "Categoria", "Producto" = "Producto", "Marca" = "Marca")
.com_resumen <- function(hechos, clientes, corte, dimension, acumulado = TRUE) {
  if (!dimension %in% unname(.COM_DIMENSIONES)) stop("Dimensión no permitida.")
  inicio <- if (acumulado) as.Date(format(corte, "%Y-01-01")) else .gc_mes(corte)
  x <- hechos[hechos$Fecha >= inicio & hechos$Fecha <= corte, , drop = FALSE]
  if (!dimension %in% names(x)) x[[dimension]] <- NA_character_
  x$Dimension <- as.character(x[[dimension]])
  x$Dimension[is.na(x$Dimension) | !nzchar(x$Dimension)] <- "Sin clasificar"
  ejecucion <- x %>% dplyr::group_by(Dimension) %>%
    dplyr::summarise(Sacos = sum(SacosFact, na.rm = TRUE), Margen = .com_suma(Margen),
      Clientes = dplyr::n_distinct(CliNitPpal), ConEjecucion = TRUE, .groups = "drop")
  asignable <- dimension %in% c("CLLinNegNo", "Segmento", "Asesor", "PerRazSoc", "Depto", "Mpio", "Region")
  meses <- if (acumulado) as.integer(format(corte, "%m")) else 1L
  if (asignable) {
    clientes$Dimension <- as.character(clientes[[dimension]])
    clientes$Dimension[is.na(clientes$Dimension) | !nzchar(clientes$Dimension)] <- "Sin clasificar"
    ppto <- clientes %>% dplyr::group_by(Dimension) %>%
      dplyr::summarise(PptoSacos = .gc_suma(SSPpto) * meses / 12,
        PptoMargen = .gc_suma(MNFCCPpto) * meses / 12, .groups = "drop")
    salida <- dplyr::full_join(ejecucion, ppto, by = "Dimension")
    salida$Sacos <- dplyr::coalesce(salida$Sacos, 0)
    salida$Margen[is.na(salida$ConEjecucion)] <- 0
    salida$Clientes <- dplyr::coalesce(salida$Clientes, 0L)
  } else {
    salida <- ejecucion
    salida$PptoSacos <- salida$PptoMargen <- rep(NA_real_, nrow(salida))
  }
  salida %>% dplyr::mutate(MargenKg = .gc_ratio(Margen, Sacos * 70),
    CumplSacos = .gc_ratio(Sacos, PptoSacos), CumplMargen = .gc_ratio(Margen, PptoMargen)) %>%
    dplyr::select(-dplyr::any_of("ConEjecucion"))
}
.com_serie <- function(hechos, corte) {
  hechos %>% dplyr::filter(format(Fecha, "%Y") %in% as.character(as.integer(format(corte, "%Y")) - 0:5)) %>%
    dplyr::group_by(Fecha) %>% dplyr::summarise(Sacos = sum(SacosFact, na.rm = TRUE),
      Margen = .com_suma(Margen), .groups = "drop") %>%
    dplyr::mutate(MargenKg = .gc_ratio(Margen, Sacos * 70)) %>% dplyr::arrange(Fecha)
}
.com_fin_mes <- function(x) seq(.gc_mes(x), by = "month", length.out = 2L)[[2]] - 1L
.com_suma <- function(x) if (anyNA(x)) NA_real_ else sum(x)

.com_elegir <- function(valor, validos, defecto) {
  if (length(valor) == 1L && !is.na(valor) && valor %in% validos) valor else defecto
}
.com_enriquecer_geografia <- function(hechos, clientes) {
  meta <- clientes[c("CliNitPpal", "LinNegCod", "Depto", "Mpio", "Region")]
  names(meta)[3:5] <- c("DeptoCRM", "MpioCRM", "RegionCRM")
  x <- dplyr::left_join(hechos, meta, by = c("CliNitPpal", "LinNegCod"), na_matches = "never")
  x$Depto <- dplyr::coalesce(as.character(.gc_columna(x, "Depto")), x$DeptoCRM)
  x$Mpio <- dplyr::coalesce(as.character(.gc_columna(x, "Mpio")), x$MpioCRM)
  x$Region <- dplyr::coalesce(as.character(.gc_columna(x, c("Region", "Región"))), x$RegionCRM)
  if (nrow(x) != nrow(hechos)) stop("La geografía multiplicó hechos comerciales.")
  x
}

.com_tasa_referencia <- function(tasas, anho) {
  if (is.null(tasas) || !all(c("Ano", "Tasa") %in% names(tasas))) return(NA_real_)
  x <- unique(.gc_num(tasas$Tasa[tasas$Ano == anho]))
  x <- x[is.finite(x) & x > 0]
  if (length(x) == 1L) x[[1]] else NA_real_
}
