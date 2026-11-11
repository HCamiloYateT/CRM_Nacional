# Transiciones admitidas por los formularios comerciales ----
.kb_destinos <- function(etapa, previa = "CONTACTO") {
  switch(etapa, CONTACTO = c("LEAD", "PROSPECTO", "DESCARTADO"),
    PROSPECTO = c("LEAD", "DESCARTADO"), LEAD = c("CLIENTE", "DESCARTADO"),
    CLIENTE = "DESCARTADO", DESCARTADO = intersect(previa, c("CONTACTO", "LEAD", "PROSPECTO", "CLIENTE")),
    character())
}
.kb_buscar <- function(datos, texto) {
  if (is.null(texto) || !nzchar(trimws(texto))) return(datos)
  campos <- intersect(c("PerRazSoc", "PerCod", "Asesor", "Segmento", "LinNegocio",
    "Origen", "Alianzas", "Razon1", "Razon2", "Razon3", "EtapaPreDescarte"), names(datos))
  encontrado <- rep(FALSE, nrow(datos))
  for (campo in campos) encontrado <- encontrado | (!is.na(datos[[campo]]) &
    grepl(toupper(trimws(texto)), toupper(as.character(datos[[campo]])), fixed = TRUE))
  datos[encontrado, , drop = FALSE]
}

# Clientes y atributos observados en la facturación del alcance seleccionado ----
.kb_facturacion <- function(pipeline, hechos, filtros, usuario, admin) {
  requeridas <- c("FecFact", "EstadoFinanciero", "CLCliNit", "CliNitPpal")
  if (!all(requeridas %in% names(hechos))) stop("Actualice la fuente de facturación del Kanban.")
  facturas <- .gc_filtrar_dimensiones(hechos, filtros)
  facturas <- facturas[facturas$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO") &
    !is.na(facturas$FecFact) & facturas$FecFact >= as.Date(filtros$fecha[1]) &
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
  grupos <- split(seq_len(nrow(facturas)), .crm_nit_historia(facturas$CLCliNit))
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
      nombres <- as.character(.gc_columna(datos, c("RazonSocialCliNit", "CLCliRazSo", "PerRazSoc", "RazonSocial")))
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
    tarjetas[[length(tarjetas) + 1L]] <- fila
  }
  # Los clientes provienen de los NIT facturados; no del subconjunto que ya tenía etapa CRM CLIENTE.
  otros <- x[!x$CodContacto %in% usados & x$Etapa != "CLIENTE", , drop = FALSE]
  facturados <- dplyr::bind_rows(tarjetas)
  if (nrow(facturados)) {
    ambiguos <- duplicated(facturados$CodContacto) | duplicated(facturados$CodContacto, fromLast = TRUE)
    facturados$CodContacto[ambiguos] <- paste0("FACTURA:", facturados$PerCod[ambiguos])
    facturados$EsSoloFacturacion[ambiguos] <- TRUE
  }
  dplyr::bind_rows(.kb_alcance(otros, filtros, usuario, admin), facturados)
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
