# Transiciones admitidas por los formularios comerciales ----
.kb_destinos <- function(etapa, previa = "CONTACTO") {
  switch(etapa, CONTACTO = c("LEAD", "PROSPECTO", "DESCARTADO"),
    PROSPECTO = c("LEAD", "DESCARTADO"), LEAD = c("CLIENTE", "DESCARTADO"),
    CLIENTE = "DESCARTADO", DESCARTADO = intersect(previa, c("CONTACTO", "LEAD", "PROSPECTO", "CLIENTE")),
    character())
}
.kb_buscar <- function(datos, texto) {
  if (is.null(texto) || !nzchar(trimws(texto))) return(datos)
  campos <- intersect(c("PerRazSoc", "PerCod", "Asesor", "Segmento", "LinNegocio"), names(datos))
  encontrado <- rep(FALSE, nrow(datos))
  for (campo in campos) encontrado <- encontrado | (!is.na(datos[[campo]]) &
    grepl(toupper(trimws(texto)), toupper(as.character(datos[[campo]])), fixed = TRUE))
  datos[encontrado, , drop = FALSE]
}

# Clientes y atributos observados en la facturación del alcance seleccionado ----
.kb_facturacion <- function(pipeline, hechos, filtros, usuario, admin) {
  x <- pipeline
  cliente_facturado <- x$Etapa == "CLIENTE" | (x$Etapa == "DESCARTADO" & x$EtapaPreDescarte == "CLIENTE")
  clientes <- which(cliente_facturado)
  if (!length(clientes)) return(.kb_alcance(x, filtros, usuario, admin))
  requeridas <- c("FecFact", "EstadoFinanciero", "CLCliNit", "CliNitPpal")
  if (!all(requeridas %in% names(hechos))) stop("Actualice la fuente de facturación del Kanban.")
  facturas <- .gc_filtrar_dimensiones(hechos, filtros)
  facturas <- facturas[facturas$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO") &
    !is.na(facturas$FecFact) & facturas$FecFact >= as.Date(filtros$fecha[1]) &
    facturas$FecFact <= as.Date(filtros$fecha[2]), , drop = FALSE]
  if (!isTRUE(admin)) facturas <- facturas[!is.na(facturas$Asesor) &
    toupper(trimws(facturas$Asesor)) == toupper(trimws(usuario)), , drop = FALSE]
  indice <- split(seq_len(nrow(facturas)), .crm_nit_historia(facturas$CLCliNit))
  principales <- split(seq_len(nrow(facturas)), .crm_nit_historia(facturas$CliNitPpal))
  conservar <- rep(TRUE, nrow(x))
  for (i in clientes) {
    nits <- unique(na.omit(.crm_nit_historia(c(x$PerCod[i], x$NitVinculado[i]))))
    filas <- unique(unlist(c(indice[nits], principales[nits]), use.names = FALSE))
    if (!length(filas)) { conservar[i] <- FALSE; next }
    datos <- facturas[filas, , drop = FALSE]
    juntar <- function(v) paste(sort(unique(v[!is.na(v) & nzchar(v)])), collapse = " | ")
    x$Asesor[i] <- juntar(as.character(datos$Asesor))
    x$Segmento[i] <- juntar(as.character(datos$Segmento))
    x$LinNegocio[i] <- juntar(as.character(.gc_columna(datos, c("CLLinNegNo", "LinNegocio"))))
  }
  x$Linea <- as.character(x$LinNegocio)
  # Los clientes ya se filtraron conjuntamente por sus hechos; los demás conservan su alcance CRM.
  rbind(.kb_alcance(x[!cliente_facturado, , drop = FALSE], filtros, usuario, admin),
    x[conservar & cliente_facturado, , drop = FALSE])
}
