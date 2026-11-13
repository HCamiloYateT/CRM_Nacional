# Maestro de unidades comerciales y procedencia de atributos ----
.crm_unidades_comerciales <- function(hechos, clientes, contactos, estados, leads = data.frame()) {
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
  oficial <- ultimo(e, claves, "FecProceso", "SegmentoRacafe")
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
