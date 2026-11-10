# Historia anterior al año filtrado, registro ERP y población comparable para RFM/CLV y afinidad ----
directorio360 <- data.frame(PerCod = c(101, 102), PerRazSoc = c("CASA MATRIZ", "SEDE NORTE"),
  CliConCom = c("Contacto matriz", "Contacto sede"), CliTelCom = c("111111", "222222"),
  CliEmlCom = c("matriz@example.test", "sede@example.test"), CliDir = c("Calle 1", "Calle 2"),
  CiuExtNom = c("Bogotá", "Medellín"), Depto = c("Bogotá D.C.", "Antioquia"),
  FecCreacion = as.Date(c("2010-03-01", "2015-06-01")))
contactos_individual$FechaHoraCrea <- as.POSIXct(c("2020-01-01", "2018-06-01", "2020-01-01"))
historia360 <- do.call(rbind, lapply(c(101, 102, 500:529), function(nit) {
  productos <- if (nit == 101 || nit >= 520) "A" else if (nit == 102) "B" else c("B", "C")
  x <- expand.grid(Fecha = as.Date(c("2023-03-01", "2024-04-01", "2025-05-01")), Producto = productos)
  fila <- hechos_individual[rep(1, nrow(x)), ]
  fila$CliNitPpal <- if (nit %in% c(101, 102)) 101 else nit
  fila$CLCliNit <- nit
  fila$CodContacto <- if (nit == 102) "C2" else NA_character_
  fila$PerRazSoc <- paste("Cuenta", nit)
  fila$FecFact <- as.Date(x$Fecha) + (nit %% 17)
  fila$Producto <- as.character(x$Producto)
  fila$SacosFact <- 10; fila$Margen <- 1000
  fila$EstadoFinanciero <- "CERRADO"
  fila$CLPdcCod <- nit * 100 + seq_len(nrow(x))
  fila$CLLotCod <- fila$CLPdcCod
  fila
}))
hechos360 <- dplyr::bind_rows(hechos_individual, historia360)

# Precios reales de cabecera ficticios: conservar año anterior y referencias comerciales.
hechos360$PdcPrecioKilo <- ifelse(hechos360$CLCliNit == 102, 12000, 10000)
hechos360$PdcFecCre <- as.Date(hechos360$FecFact) - 2L
hechos360$Marca <- ifelse(hechos360$CLCliNit == 102, "Marca sede", "Marca matriz")
hechos360$MCNom <- ifelse(hechos360$CLCliNit == 102, "Referencia sede", "Referencia matriz")

# Estado comercial mensual conocido; no deducir NUEVO de ausencia de snapshots.
estados_ficha <- data.frame(CliNitPpal = c(101, 500, 501, 101), LinNegCod = 21000,
  FecProceso = as.Date(c("2026-11-01", "2026-11-01", "2026-11-01", "2026-10-01")),
  EstadoPanel = c("ACTIVO", "INACTIVO", "NUEVO", "INACTIVO"))
