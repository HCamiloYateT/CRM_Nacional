# Transiciones admitidas por los formularios comerciales ----
.kb_destinos <- function(etapa, previa = "CONTACTO") {
  switch(etapa, CONTACTO = c("LEAD", "PROSPECTO", "DESCARTADO"),
    PROSPECTO = c("LEAD", "DESCARTADO"), LEAD = c("CLIENTE", "DESCARTADO"),
    CLIENTE = "DESCARTADO", DESCARTADO = intersect(previa, c("CONTACTO", "LEAD", "PROSPECTO", "CLIENTE")),
    character())
}
.kb_buscar <- function(datos, texto) {
  if (is.null(texto) || !nzchar(trimws(texto))) return(datos)
  if (".Busqueda" %in% names(datos)) {
    return(datos[grepl(toupper(trimws(texto)), datos$.Busqueda, fixed = TRUE), , drop = FALSE])
  }
  campos <- intersect(c("PerRazSoc", "PerCod", "Asesor", "Segmento", "LinNegocio",
    "Origen", "Alianzas", "Razon1", "Razon2", "Razon3", "EtapaPreDescarte"), names(datos))
  encontrado <- rep(FALSE, nrow(datos))
  for (campo in campos) encontrado <- encontrado | (!is.na(datos[[campo]]) &
    grepl(toupper(trimws(texto)), toupper(as.character(datos[[campo]])), fixed = TRUE))
  datos[encontrado, , drop = FALSE]
}

# Clientes y atributos observados en la facturación del alcance seleccionado ----
.kb_facturacion <- function(pipeline, hechos, filtros, usuario, admin, presupuestos = data.frame()) {
  requeridas <- c("FecFact", "EstadoFinanciero", "CLCliNit", "CliNitPpal")
  if (!all(requeridas %in% names(hechos))) stop("Actualice la fuente de facturación del Kanban.")
  facturas <- .gc_filtrar_dimensiones(hechos, filtros)
  facturas <- facturas[facturas$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO") &
    !is.na(facturas$FecFact) & facturas$FecFact >= as.Date(paste0(format(as.Date(filtros$fecha[2]), "%Y"), "-01-01")) &
    facturas$FecFact <= as.Date(filtros$fecha[2]), , drop = FALSE]
  if (!isTRUE(admin)) facturas <- facturas[!is.na(facturas$Asesor) &
    toupper(trimws(facturas$Asesor)) == toupper(trimws(usuario)), , drop = FALSE]
  x <- as.data.frame(pipeline)
  x$CodContacto <- as.character(x$CodContacto)
  x$PerCod <- .crm_nit_historia(x$PerCod)
  x$CodigoCRM <- x$CodContacto
  x$EsSoloFacturacion <- rep(FALSE, nrow(x))
  x$UltimaFactura <- rep(as.Date(NA), nrow(x))
  x$SacosPeriodo <- rep(NA_real_, nrow(x))
  x$DetalleLineas <- rep(list(list()), nrow(x))
  principales <- .crm_nit_historia(facturas$CliNitPpal)
  grupos <- split(seq_len(nrow(facturas)), principales)
  corte <- as.Date(filtros$fecha[2])
  metas <- if (nrow(presupuestos)) .com_clientes(presupuestos, corte, filtros, anual = TRUE) else data.frame()
  metas_clave <- if (nrow(metas)) paste(.crm_nit_historia(metas$CliNitPpal), metas$LinNegCod) else character()
  fraccion <- as.integer(format(corte, "%m")) / 12
  importe <- function(v, moneda = FALSE) {
    if (length(v) != 1L || !is.finite(v)) return("SIN PRESUPUESTO VALIDADO")
    paste0(if (moneda) "$" else "", formatC(v, format = "f", digits = if (moneda) 0 else 1, big.mark = ","))
  }
  por_nit <- split(seq_len(nrow(x)), x$PerCod)
  por_vinculo <- split(seq_len(nrow(x)), .crm_nit_historia(x$NitVinculado))
  tarjetas <- list()
  usados <- character()
  juntar <- function(v) paste(sort(unique(v[!is.na(v) & nzchar(v)])), collapse = " | ")
  for (nit in names(grupos)) {
    datos <- facturas[grupos[[nit]], , drop = FALSE]
    indices <- unique(c(por_nit[[nit]], por_vinculo[[nit]]))
    if (length(indices) == 1L) {
      fila <- x[indices, , drop = FALSE]
      origen <- fila$Etapa
      fila$EsSoloFacturacion <- !origen %in% c("CLIENTE", "DESCARTADO")
      if (origen != "DESCARTADO") fila$Etapa <- "CLIENTE"
    } else {
      fila <- x[NA_integer_, , drop = FALSE]
      fila$CodContacto <- paste0("FACTURA:", nit)
      fila$CodigoCRM <- NA_character_
      fila$EsReconstruido <- TRUE
      fila$FuenteRegistro <- "FACTURACION"
      fila$IncluirConversion <- FALSE
      fila$Etapa <- "CLIENTE"
      fila$Estado <- "ACTIVO"
      fila$EsSoloFacturacion <- TRUE
      nombres <- as.character(.gc_columna(datos, c("PerRazSoc", "RazonSocial", "RazonSocialCliNit", "CLCliRazSo")))
      nombres <- nombres[!is.na(nombres) & nzchar(nombres)]
      fila$PerRazSoc <- if (length(nombres)) nombres[[1]] else paste("CLIENTE", nit)
    }
    usados <- c(usados, x$CodContacto[indices])
    fila$PerCod <- nit
    fila$Asesor <- juntar(as.character(datos$Asesor))
    fila$Segmento <- juntar(as.character(datos$Segmento))
    fila$LinNegocio <- juntar(as.character(.gc_columna(datos, c("CLLinNegNo", "LinNegocio"))))
    fila$UltimaFactura <- max(as.Date(datos$FecFact))
    fila$SacosPeriodo <- .gc_suma(.gc_num(.gc_columna(datos, "SacosFact")))
    lineas <- split(seq_len(nrow(datos)), as.character(datos$LinNegCod))
    fila$DetalleLineas <- list(lapply(names(lineas), function(codigo) {
      d <- datos[lineas[[codigo]], , drop = FALSE]
      m <- match(paste(nit, codigo), metas_clave)
      linea <- if (codigo == "10000") "COPRODUCTOS" else if (codigo == "21000") "A LA MEDIDA" else codigo
      list(Linea = linea, Segmento = juntar(as.character(d$Segmento)),
        UltimaCompra = max(as.Date(d$FecFact)), Sacos = .gc_suma(.gc_num(d$SacosFact)),
        Margen = importe(.gc_suma(.gc_num(d$Margen)), TRUE),
        PptoSacos = importe(if (!is.na(m)) metas$SSPpto[[m]] * fraccion else NA_real_),
        PptoMargen = importe(if (!is.na(m)) metas$MNFCCPpto[[m]] * fraccion else NA_real_, TRUE))
    }))
    tarjetas[[length(tarjetas) + 1L]] <- fila
  }
  # Los clientes provienen de los NIT facturados; no del subconjunto que ya tenía etapa CRM CLIENTE.
  otros <- x[!x$CodContacto %in% usados & x$Etapa != "CLIENTE", , drop = FALSE]
  hijas <- unique(.crm_nit_historia(hechos$CLCliNit)[
    !is.na(hechos$CliNitPpal) & .crm_nit_historia(hechos$CLCliNit) != .crm_nit_historia(hechos$CliNitPpal)])
  principales_conocidas <- unique(.crm_nit_historia(hechos$CliNitPpal))
  otros <- otros[!otros$PerCod %in% setdiff(hijas, principales_conocidas), , drop = FALSE]
  facturados <- dplyr::bind_rows(tarjetas)
  if (nrow(facturados)) {
    ambiguos <- duplicated(facturados$CodContacto) | duplicated(facturados$CodContacto, fromLast = TRUE)
    facturados$CodContacto[ambiguos] <- paste0("FACTURA:", facturados$PerCod[ambiguos])
    facturados$EsSoloFacturacion[ambiguos] <- TRUE
  }
  resultado <- dplyr::bind_rows(.kb_alcance(otros, filtros, usuario, admin), facturados)
  campos <- intersect(c("PerRazSoc", "PerCod", "Asesor", "Segmento", "LinNegocio", "Origen", "Alianzas",
    "Razon1", "Razon2", "Razon3", "EtapaPreDescarte"), names(resultado))
  resultado$.Busqueda <- toupper(do.call(paste, c(resultado[campos], sep = " | ")))
  resultado
}

# Enlaces de consulta con validación posterior del catálogo y permisos de la sesión ----
.kb_url_consulta <- function(codigo, nit) {
  url <- paste0("?crm_tab=IN_Consulta&crm_nit=", utils::URLencode(as.character(nit), reserved = TRUE))
  if (length(codigo) == 1L && !is.na(codigo) && nzchar(codigo)) {
    url <- paste0(url, "&crm_codigo=", utils::URLencode(codigo, reserved = TRUE))
  }
  url
}
.kb_parametros_consulta <- function(busqueda) {
  q <- shiny::parseQueryString(busqueda)
  if (!identical(q$crm_tab, "IN_Consulta")) return(NULL)
  codigo <- if (is.null(q$crm_codigo)) "" else trimws(as.character(q$crm_codigo))
  nit <- if (is.null(q$crm_nit)) "" else .crm_nit_historia(q$crm_nit)
  if (length(codigo) != 1L || nchar(codigo) > 200L || grepl("[[:cntrl:]]", codigo)) return(NULL)
  if (length(nit) != 1L || is.na(nit)) nit <- ""
  if (!nzchar(codigo) && !nzchar(nit)) return(NULL)
  list(codigo = codigo, nit = nit, solicitud = paste0("url:", busqueda))
}
