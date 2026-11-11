# Limpieza explícita de simulados, para ejecutar en RStudio ----
# source("scripts/limpiar_prospeccion_simulada.R") muestra la revisión sin borrar.
# Después de revisarla:
# limpiar_prospeccion_simulada(aplicar = TRUE,
#   codigos_simulados = revision_prospeccion$candidatos$CodContacto,
#   confirmar = "ELIMINAR SOLO SIMULADOS REVISADOS")
# Si los registros antiguos no tienen marca de simulación, pase sus códigos revisados en codigos_simulados.
# Nunca se decide por razón social, fecha, asesor o ausencia de ventas.
limpiar_prospeccion_simulada <- function(aplicar = FALSE, codigos_simulados = character(),
    confirmar = "", respaldo = file.path(getwd(), paste0("respaldo_prospeccion_",
      format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")), con = NULL) {
  propia <- is.null(con)
  if (propia) con <- racafeBD::ConectarBD()
  if (propia) on.exit(DBI::dbDisconnect(con), add = TRUE)
  tablas <- DBI::dbListTables(con)
  requeridas <- c("CRMNALCONTACTO", "CRMNALSEGR")
  if (!all(requeridas %in% tablas)) stop("Faltan tablas requeridas; no se ha eliminado nada.")
  contactos <- DBI::dbReadTable(con, "CRMNALCONTACTO")
  historia <- DBI::dbReadTable(con, "CRMNALSEGR")
  nit <- function(x) sub("[.]0+$", "", trimws(format(x, scientific = FALSE, trim = TRUE)))
  marcados <- rep(FALSE, nrow(contactos))
  if ("EsSimulado" %in% names(contactos)) {
    marcados <- toupper(as.character(contactos$EsSimulado)) %in% c("TRUE", "1", "SI")
  }
  if ("FuenteRegistro" %in% names(contactos)) {
    marcados <- marcados | toupper(trimws(contactos$FuenteRegistro)) %in%
      c("SIMULADO", "SIMULACION", "DEMO", "TEST")
  }
  marcados <- marcados | as.character(contactos$CodContacto) %in% as.character(codigos_simulados)
  protegido <- nit(contactos$PerCod) %in% nit(historia$CliNitPpal)
  if ("EsReconstruido" %in% names(contactos)) protegido <- protegido |
    toupper(as.character(contactos$EsReconstruido)) %in% c("TRUE", "1", "SI")
  # Proteger también cuentas hijas vinculadas a un principal con facturación histórica.
  if ("CRMNALVINCULONIT" %in% tablas) {
    vinculos <- DBI::dbReadTable(con, "CRMNALVINCULONIT")
    if (all(c("CodContacto", "NitVinculado") %in% names(vinculos))) {
      protegido <- protegido | as.character(contactos$CodContacto) %in%
        as.character(vinculos$CodContacto[nit(vinculos$NitVinculado) %in% nit(historia$CliNitPpal)])
    }
  }
  if ("CRMNALCLIENTE" %in% tablas) {
    clientes <- DBI::dbReadTable(con, "CRMNALCLIENTE")
    if (all(c("CLCliNit", "CliNitPpal") %in% names(clientes))) {
      hijas <- clientes$CLCliNit[nit(clientes$CliNitPpal) %in% nit(historia$CliNitPpal)]
      protegido <- protegido | nit(contactos$PerCod) %in% nit(hijas)
    }
  }
  candidatos <- contactos[marcados & !protegido, , drop = FALSE]
  protegidos <- contactos[marcados & protegido, , drop = FALSE]
  ids <- unique(as.character(candidatos$CodContacto))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  afectadas <- tablas[grepl("^(CRMNAL|CONTACTOLEAD$)", tablas)]
  afectadas <- afectadas[vapply(afectadas, function(tabla) {
    "CodContacto" %in% DBI::dbListFields(con, tabla)
  }, logical(1))]
  respaldos <- list()
  for (tabla in afectadas) {
    datos <- DBI::dbReadTable(con, tabla)
    respaldos[[tabla]] <- datos[as.character(datos$CodContacto) %in% ids, , drop = FALSE]
  }
  claves <- stats::setNames(rep("CodContacto", length(respaldos)), names(respaldos))
  personas <- respaldos$CRMNALDIRECTORIOPERSONA
  if (is.data.frame(personas) && nrow(personas) && "CRMNALDIRECTORIOPERSONARED" %in% tablas) {
    redes <- DBI::dbReadTable(con, "CRMNALDIRECTORIOPERSONARED")
    respaldos$CRMNALDIRECTORIOPERSONARED <- redes[redes$IdPersona %in% personas$IdPersona, , drop = FALSE]
    claves["CRMNALDIRECTORIOPERSONARED"] <- "IdPersona"
    afectadas <- union(afectadas, "CRMNALDIRECTORIOPERSONARED")
  }
  conteos <- data.frame(Tabla = names(respaldos), Filas = vapply(respaldos, nrow, integer(1)))
  revision <- list(candidatos = candidatos, protegidos = protegidos,
    pendientes_identificacion = contactos[!marcados & !protegido, , drop = FALSE],
    impacto = conteos, aplicado = FALSE)
  if (!isTRUE(aplicar)) {
    print(conteos, row.names = FALSE)
    message("Candidatos: ", nrow(candidatos), "; protegidos: ", nrow(protegidos),
      "; sin marca: ", nrow(revision$pendientes_identificacion),
      ". Vista previa: no se ha eliminado nada. Revise pendientes_identificacion sin asumir que sean simulados.")
    return(invisible(revision))
  }
  if (!identical(confirmar, "ELIMINAR SOLO SIMULADOS REVISADOS")) stop("Falta la confirmación explícita.")
  if (!length(ids)) stop("No hay candidatos identificados como simulados. Revise las marcas o los códigos.")
  if (!setequal(ids, as.character(codigos_simulados))) {
    stop("Pase exactamente los códigos de la vista previa revisada; no se ha eliminado nada.")
  }
  motores <- DBI::dbGetQuery(con,
    "SELECT TABLE_NAME, ENGINE FROM information_schema.TABLES WHERE TABLE_SCHEMA = DATABASE()")
  if (any(!toupper(motores$ENGINE[match(afectadas, motores$TABLE_NAME)]) %in% "INNODB")) {
    stop("Se requiere InnoDB en las tablas afectadas para garantizar rollback.")
  }
  if (file.exists(respaldo)) stop("El archivo de respaldo ya existe. Use otra ruta.")
  DBI::dbWithTransaction(con, {
    # Bloquear y volver a comprobar la selección, evitando borrar una fila modificada después de la revisión.
    literales <- paste(DBI::dbQuoteString(con, ids), collapse = ",")
    vigentes <- DBI::dbGetQuery(con, paste0("SELECT * FROM CRMNALCONTACTO WHERE CodContacto IN (",
      literales, ") FOR UPDATE"))
    if (!isTRUE(all.equal(vigentes[order(vigentes$CodContacto), ],
        candidatos[order(candidatos$CodContacto), ], check.attributes = FALSE))) {
      stop("Los contactos cambiaron desde la revisión; vuelva a generar la vista previa.")
    }
    saveRDS(list(fecha = Sys.time(), contactos = candidatos, tablas = respaldos), respaldo)
    orden <- c(intersect("CRMNALDIRECTORIOPERSONARED", afectadas),
      setdiff(afectadas, c("CRMNALDIRECTORIOPERSONARED", "CRMNALCONTACTO")), "CRMNALCONTACTO")
    for (tabla in orden) {
      clave <- claves[[tabla]]
      valores <- unique(respaldos[[tabla]][[clave]])
      if (!length(valores)) next
      literales_tabla <- paste(DBI::dbQuoteString(con, as.character(valores)), collapse = ",")
      DBI::dbExecute(con, paste0("DELETE FROM ", DBI::dbQuoteIdentifier(con, tabla),
        " WHERE ", DBI::dbQuoteIdentifier(con, clave), " IN (", literales_tabla, ")"))
    }
  })
  revision$aplicado <- TRUE
  revision$respaldo <- normalizePath(respaldo, mustWork = TRUE)
  message("Limpieza confirmada. Respaldo: ", revision$respaldo,
    ". Actualice el ETL y reinicie la app para descartar cachés.")
  invisible(revision)
}
if (interactive()) revision_prospeccion <- limpiar_prospeccion_simulada()
