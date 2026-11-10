# Historia comercial observada ----
.crm_historia_avisos <- new.env(parent = emptyenv())
.crm_marcas_historia <- function(datos) {
  if (!"EsReconstruido" %in% names(datos)) {
    if (all(c("CodContacto", "PerCod") %in% names(datos)) && !isTRUE(.crm_historia_avisos$legado)) {
      warning("Contactos sin marcador persistente: revise el esquema y la reparación histórica antes de auditar tasas.",
        call. = FALSE)
      .crm_historia_avisos$legado <- TRUE
    }
    datos$EsReconstruido <- rep(FALSE, nrow(datos))
  }
  if (!"FuenteRegistro" %in% names(datos)) datos$FuenteRegistro <- rep("CRM", nrow(datos))
  if (!"FechaImputada" %in% names(datos)) datos$FechaImputada <- rep(FALSE, nrow(datos))
  datos$EsReconstruido <- toupper(as.character(datos$EsReconstruido)) %in% c("TRUE", "1", "SI")
  datos$FechaImputada <- toupper(as.character(datos$FechaImputada)) %in% c("TRUE", "1", "SI")
  datos$IncluirConversion <- !datos$EsReconstruido & !datos$FechaImputada &
    !datos$FuenteRegistro %in% c("CLIENTE_HISTORICO", "MIGRACION")
  datos
}
es_historia_comercial_real <- function(datos) .crm_marcas_historia(datos)$IncluirConversion
filtrar_historia_comercial_real <- function(datos) {
  datos <- .crm_marcas_historia(datos)
  datos[datos$IncluirConversion, , drop = FALSE]
}
.crm_nit_historia <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE, digits = 22, big.mark = "")
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  x[is.na(x) | x %in% c("", "0") | !grepl("^[0-9]+$", x)] <- NA_character_
  x
}
obtener_fecha_creacion_cliente <- function(clientes) {
  fecha <- rep(as.POSIXct(NA, tz = "UTC"), nrow(clientes))
  for (columna in intersect(c("FecCreacionCliente", "FecCreacion", "MinFecFact"), names(clientes))) {
    candidata <- suppressWarnings(lubridate::as_datetime(clientes[[columna]], tz = "UTC"))
    valida <- !is.na(candidata) & candidata <= Sys.time() & candidata >= as.POSIXct("1900-01-01", tz = "UTC")
    usar <- is.na(fecha) & valida
    fecha[usar] <- candidata[usar]
  }
  fecha
}
obtener_clientes_sin_contacto <- function(clientes, contactos) {
  clientes$CliNitPpal <- .crm_nit_historia(clientes$CliNitPpal)
  clientes <- clientes[!is.na(clientes$CliNitPpal), , drop = FALSE]
  clientes <- clientes[!duplicated(clientes$CliNitPpal), , drop = FALSE]
  clientes[!clientes$CliNitPpal %in% .crm_nit_historia(contactos$PerCod), , drop = FALSE]
}

# Inspección y migración explícita de esquema ----
.crm_migrar_esquema_historia <- function(aplicar = FALSE, con = NULL) {
  propia <- is.null(con)
  if (propia) con <- racafeBD::ConectarBD()
  if (propia) on.exit(DBI::dbDisconnect(con), add = TRUE)
  tablas <- c("CRMNALCONTACTO", "CRMNALHISTORIALETAPA", "CONTACTOLEAD", "CRMNALLEADCLIENTE", "CRMNALVINCULONIT")
  esquema <- stats::setNames(lapply(tablas, function(tabla) {
    DBI::dbGetQuery(con, paste("SHOW COLUMNS FROM", DBI::dbQuoteIdentifier(con, tabla)))
  }), tablas)
  requeridas <- list(CRMNALCONTACTO = c("CodContacto", "PerCod", "FechaHoraCrea"),
    CRMNALHISTORIALETAPA = c("CodContacto", "EtapaNueva", "FechaHora"))
  for (tabla in names(requeridas)) {
    if (!all(requeridas[[tabla]] %in% esquema[[tabla]]$Field)) stop("Esquema no compatible: ", tabla)
  }
  esquema_sistema <- stats::setNames(lapply(c("NCLIENTE", "NPERSONA"), function(tabla) {
    racafeBD::ConsultaSistema("syscafe", paste0("SELECT * FROM ", tabla, " WHERE 1 = 0"))
  }), c("NCLIENTE", "NPERSONA"))
  cambios <- list(CRMNALCONTACTO = c(EsReconstruido = "BOOLEAN NOT NULL DEFAULT FALSE",
    FuenteRegistro = "VARCHAR(32) NOT NULL DEFAULT 'CRM'", FechaFuente = "DATETIME NULL",
    FechaImputada = "BOOLEAN NOT NULL DEFAULT FALSE"),
    CRMNALHISTORIALETAPA = c(EsReconstruido = "BOOLEAN NOT NULL DEFAULT FALSE"))
  sql <- unlist(lapply(names(cambios), function(tabla) {
    columnas <- setdiff(names(cambios[[tabla]]), esquema[[tabla]]$Field)
    vapply(columnas, function(columna) paste("ALTER TABLE", DBI::dbQuoteIdentifier(con, tabla),
      "ADD COLUMN", DBI::dbQuoteIdentifier(con, columna), cambios[[tabla]][[columna]]), character(1))
  }), use.names = FALSE)
  if (isTRUE(aplicar)) for (instruccion in sql) DBI::dbExecute(con, instruccion)
  structure(sql, esquema = esquema, esquema_sistema = esquema_sistema, aplicado = isTRUE(aplicar))
}
.crm_exigir_marcas_historia <- function(contactos) {
  if (!all(c("EsReconstruido", "FuenteRegistro", "FechaFuente", "FechaImputada") %in% names(contactos))) {
    stop("Falta el esquema de historia comercial. Revise .crm_migrar_esquema_historia() antes de ejecutar el ETL.")
  }
  invisible(TRUE)
}

# Contrato de fechas entre lecturas SQL y registros nuevos ----
.crm_normalizar_fechas_historia <- function(datos) {
  for (columna in intersect(c("FechaHoraCrea", "FechaHoraModi", "FechaFuente"), names(datos))) {
    valor <- datos[[columna]]
    if (is.factor(valor)) valor <- as.character(valor)
    if (is.character(valor)) valor[trimws(valor) %in% c("", "0000-00-00", "0000-00-00 00:00:00")] <- NA_character_
    fecha <- suppressWarnings(lubridate::as_datetime(valor, tz = "America/Bogota"))
    if (any(!is.na(valor) & is.na(fecha))) stop("Fecha inválida en ", columna, ". Revise la fuente SQL.")
    datos[[columna]] <- fecha
  }
  datos
}

# Fechas SQL sin sufijo de zona horaria ----
.crm_literal_historia <- function(con, x) {
  if (inherits(x, "POSIXt")) x <- format(x, format = "%Y-%m-%d %H:%M:%S", tz = "America/Bogota")
  as.character(DBI::dbQuoteLiteral(con, x))
}

# Registro transaccional de clientes históricos ----
.crm_registrar_cliente_historico <- function(cliente, usuario, generar_codigo) {
  con <- racafeBD::ConectarBD()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  bloqueo <- DBI::dbGetQuery(con, "SELECT GET_LOCK('CRMNAL_CONTACTOS_CLIENTES', 30) AS adquirido")
  if (!identical(as.integer(bloqueo$adquirido), 1L)) stop("No fue posible bloquear la integración de clientes.")
  on.exit(DBI::dbGetQuery(con, "SELECT RELEASE_LOCK('CRMNAL_CONTACTOS_CLIENTES')"), add = TRUE, after = FALSE)
  DBI::dbWithTransaction(con, {
    contactos <- DBI::dbReadTable(con, "CRMNALCONTACTO")
    .crm_exigir_marcas_historia(contactos)
    existente <- contactos[.crm_nit_historia(contactos$PerCod) %in% cliente$CliNitPpal, , drop = FALSE]
    if (nrow(existente)) {
      fila <- existente[1, , drop = FALSE]
      attr(fila, "Creado") <- FALSE
      fila
    } else {
      fecha <- obtener_fecha_creacion_cliente(cliente)
      campos <- DBI::dbGetQuery(con, "SHOW COLUMNS FROM CRMNALCONTACTO")
      obligatoria <- campos$Null[match("FechaHoraCrea", campos$Field)] == "NO"
      imputada <- is.na(fecha) && isTRUE(obligatoria)
      fila <- data.frame(CodContacto = generar_codigo(contactos), PerCod = cliente$CliNitPpal,
      PerRazSoc = cliente$PerRazSoc, Estado = "ACTIVO", EtapaPreDescarte = "CLIENTE",
      UsuarioCrea = usuario, FechaHoraCrea = if (imputada) Sys.time() else fecha,
      UsuarioMod = usuario, FechaHoraModi = Sys.time(), EsReconstruido = TRUE,
      FuenteRegistro = "CLIENTE_HISTORICO", FechaFuente = fecha, FechaImputada = imputada)
      columnas <- paste(DBI::dbQuoteIdentifier(con, names(fila)), collapse = ", ")
      valores <- vapply(fila, function(x) .crm_literal_historia(con, x[[1]]), character(1))
      DBI::dbExecute(con, paste0("INSERT INTO ", DBI::dbQuoteIdentifier(con, "CRMNALCONTACTO"),
        " (", columnas, ") VALUES (", paste(valores, collapse = ", "), ")"))
      attr(fila, "Creado") <- TRUE
      fila
    }
  })
}

# Reparación inicial revisable ----
.crm_plan_reparacion_clientes <- function(clientes, contactos, eventos, desde) {
  if (length(desde) != 1L || is.na(desde)) stop("Indique el inicio verificable de la carga anterior.")
  contactos <- .crm_marcas_historia(contactos)
  clientes$CliNitPpal <- .crm_nit_historia(clientes$CliNitPpal)
  clientes$FechaFuente <- obtener_fecha_creacion_cliente(clientes)
  reales <- unique(unlist(lapply(eventos, function(x) as.character(x$CodContacto)), use.names = FALSE))
  candidatos <- contactos[!contactos$EsReconstruido & contactos$UsuarioCrea %in% "SISTEMA" &
    !contactos$CodContacto %in% reales & .crm_nit_historia(contactos$PerCod) %in% clientes$CliNitPpal &
    !is.na(contactos$FechaHoraCrea) & as.POSIXct(contactos$FechaHoraCrea) >= as.POSIXct(desde), , drop = FALSE]
  candidatos$FechaFuente <- clientes$FechaFuente[match(.crm_nit_historia(candidatos$PerCod), clientes$CliNitPpal)]
  candidatos
}

# Reparación transaccional de candidatos aprobados ----
.crm_reparar_clientes_historicos <- function(clientes, codigos, desde, con = NULL) {
  if (!length(codigos)) return(invisible(0L))
  propia <- is.null(con)
  if (propia) con <- racafeBD::ConectarBD()
  if (propia) on.exit(DBI::dbDisconnect(con), add = TRUE)
  tablas <- c("CRMNALHISTORIALETAPA", "CONTACTOLEAD", "CRMNALLEADCLIENTE", "CRMNALVINCULONIT",
    "CRMNALRELACIONAMIENTO", "CRMNALCLOPT", "CRMNALPROSPECTOALIANZA", "CRMNALDESCARTE")
  DBI::dbWithTransaction(con, {
    contactos <- DBI::dbGetQuery(con, "SELECT * FROM CRMNALCONTACTO FOR UPDATE")
    .crm_exigir_marcas_historia(contactos)
    eventos <- lapply(tablas, function(tabla) DBI::dbReadTable(con, tabla))
    plan <- .crm_plan_reparacion_clientes(clientes, contactos, eventos, desde)
    codigos <- setdiff(unique(codigos), contactos$CodContacto[.crm_marcas_historia(contactos)$EsReconstruido])
    if (!all(codigos %in% plan$CodContacto)) stop("Hay candidatos con actividad o sin evidencia suficiente.")
    plan <- plan[plan$CodContacto %in% codigos, , drop = FALSE]
    campos <- DBI::dbGetQuery(con, "SHOW COLUMNS FROM CRMNALCONTACTO")
    obligatoria <- campos$Null[match("FechaHoraCrea", campos$Field)] == "NO"
    for (i in seq_len(nrow(plan))) {
      fecha <- plan$FechaFuente[i]
      imputada <- is.na(fecha) && isTRUE(obligatoria)
      literal <- function(x) .crm_literal_historia(con, x)
      sql <- paste0("UPDATE CRMNALCONTACTO SET EsReconstruido = TRUE, FuenteRegistro = 'CLIENTE_HISTORICO', ",
        "EtapaPreDescarte = 'CLIENTE', FechaFuente = ", literal(fecha),
        ", FechaImputada = ", literal(imputada), ", FechaHoraCrea = ", literal(if (imputada) Sys.time() else fecha),
        " WHERE CodContacto = ", literal(plan$CodContacto[i]))
      DBI::dbExecute(con, sql)
    }
    nrow(plan)
  })
}

# Firma del universo comercial previo a integrar clientes ----
.crm_firma_conversion <- function(contactos, historial) {
  contactos <- filtrar_historia_comercial_real(contactos)
  historial <- filtrar_historia_comercial_real(historial)
  columnas <- intersect(c("CodContacto", "FechaHoraCrea"), names(contactos))
  base <- as.data.frame(contactos[order(contactos$CodContacto), columnas, drop = FALSE])
  rownames(base) <- NULL
  eventos <- historial[historial$CodContacto %in% contactos$CodContacto, , drop = FALSE]
  columnas <- intersect(c("CodContacto", "EtapaAnterior", "EtapaNueva", "FechaHora"), names(eventos))
  eventos <- as.data.frame(eventos[order(eventos$CodContacto, eventos$FechaHora), columnas, drop = FALSE])
  rownames(eventos) <- NULL
  list(contactos = base, eventos = eventos)
}

# Diagnóstico de esquema CRM sin modificar datos ----
.crm_diagnostico_modelo_crm <- function(con = NULL, tablas = NULL) {
  propia <- is.null(con)
  if (propia) con <- racafeBD::ConectarBD()
  if (propia) on.exit(DBI::dbDisconnect(con), add = TRUE)
  disponibles <- DBI::dbListTables(con)
  if (is.null(tablas)) {
    patron <- paste0("^(CRMNALCONTACTO.*|CRMNALEAD.*|CRMNALLEAD.*|CONTACTOLEAD|",
      "CRMNALHISTORIALETAPA|CRMNALVINCULONIT|CRMNALPROSPECTOALIANZA|",
      "CRMNALRELACIONAMIENTO|CRMNALCLOPT|CRMNALDESCARTE|CRMNALDIRECTORIO.*)$")
    tablas <- disponibles[grepl(patron, toupper(disponibles))]
  }
  if (!length(tablas) || !all(tablas %in% disponibles)) stop("Revise los nombres reales de las tablas CRM.")
  estructura <- stats::setNames(lapply(tablas, function(tabla) {
    identificador <- as.character(DBI::dbQuoteIdentifier(con, tabla))
    campos <- DBI::dbGetQuery(con, paste("SHOW FULL COLUMNS FROM", identificador))
    medir <- intersect(c("CodContacto", "PerCod", "FechaHoraCrea", "FechaConversion"), campos$Field)
    resumen <- c("COUNT(*) AS Registros", vapply(medir, function(columna) {
      paste0("SUM(", DBI::dbQuoteIdentifier(con, columna), " IS NULL) AS ",
        DBI::dbQuoteIdentifier(con, paste0("Nulos_", columna)))
    }, character(1)))
    list(campos = campos, indices = DBI::dbGetQuery(con, paste("SHOW INDEX FROM", identificador)),
      definicion = DBI::dbGetQuery(con, paste("SHOW CREATE TABLE", identificador)),
      resumen = DBI::dbGetQuery(con, paste("SELECT", paste(resumen, collapse = ", "), "FROM", identificador)))
  }), tablas)
  nombres <- paste(DBI::dbQuoteLiteral(con, tablas), collapse = ", ")
  relaciones <- DBI::dbGetQuery(con, paste0(
    "SELECT TABLE_NAME, COLUMN_NAME, CONSTRAINT_NAME, REFERENCED_TABLE_NAME, REFERENCED_COLUMN_NAME ",
    "FROM information_schema.KEY_COLUMN_USAGE WHERE TABLE_SCHEMA = DATABASE() ",
    "AND TABLE_NAME IN (", nombres, ") ORDER BY TABLE_NAME, CONSTRAINT_NAME, ORDINAL_POSITION"))
  list(tablas = estructura, relaciones = relaciones)
}
