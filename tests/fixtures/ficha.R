# Referencia visual solicitada: datos ficticios, sin consultar la ficha real de CREPES ----
.ficha_nit <- 860076919
.ficha_nits <- .ficha_nit + 0:5
.ficha_contacto <- contactos_individual[1, ]
.ficha_contacto$CodContacto <- "CREPES"; .ficha_contacto$PerCod <- .ficha_nit
.ficha_contacto$PerRazSoc <- "CREPES Y WAFFLES S.A"; .ficha_contacto$Asesor <- "ANA"
.ficha_contacto$FechaHoraCrea <- as.POSIXct("2018-02-14 10:35:22", tz = "America/Bogota")
.ficha_contacto$Origen <- "REFERIDO"; .ficha_contacto$DetOrigen <- NA_character_
contactos_individual <- dplyr::bind_rows(contactos_individual, .ficha_contacto)
.ficha_meta <- clientes_individual[1, ]; .ficha_meta$CliNitPpal <- .ficha_nit; .ficha_meta$CLCliNit <- .ficha_nit
.ficha_meta$PerRazSoc <- "CREPES Y WAFFLES S.A"; .ficha_meta$SSPpto <- 960; .ficha_meta$MNFCCPpto <- 235200000
clientes_individual <- dplyr::bind_rows(clientes_individual, .ficha_meta)
.ficha_hechos <- hechos360[rep(1, 7), ]
.ficha_hechos$CLCliNit <- c(.ficha_nit, .ficha_nits); .ficha_hechos$CliNitPpal <- .ficha_nit
.ficha_hechos$Asesor <- c(rep("ANA", 6), "LUIS")
.ficha_hechos$CodContacto <- c("CREPES", rep(NA_character_, 6))
.ficha_hechos$FecFact <- as.Date(c("2021-04-01", rep("2026-10-01", 6)))
.ficha_hechos$PerRazSoc <- "CREPES Y WAFFLES S.A"
.ficha_hechos$CLPdcCod <- 860000 + seq_len(nrow(.ficha_hechos))
.ficha_hechos$CLLotCod <- .ficha_hechos$CLPdcCod
hechos360 <- dplyr::bind_rows(hechos360, .ficha_hechos)
.ficha_directorio <- directorio360[rep(1, 6), ]
.ficha_directorio$PerCod <- .ficha_nits
.ficha_directorio$PerRazSoc <- c("CREPES Y WAFFLES S.A", paste("CREPES · Sede", 1:5))
.ficha_directorio$FecCreacion <- as.Date("2003-08-01")
.ficha_directorio$CliConCom <- "SIN DATO | Contacto comercial"
.ficha_directorio$CliTelCom <- "SIN DATO | 6011234567"
.ficha_directorio$CliEmlCom <- "ventas@example.test"
.ficha_directorio$CliDir <- "Calle 100"; .ficha_directorio$CiuExtNom <- "Bogotá"
directorio360 <- dplyr::bind_rows(directorio360, .ficha_directorio)
.ficha_fuentes <- list(
  CRMNALDIRECTORIOEMPRESARIAL = data.frame(CodContacto = "CREPES", CorreoGeneral = "hola@example.test"),
  CRMNALDIRECTORIOTELEFONO = data.frame(CodContacto = c("CREPES", "CREPES", "C3"),
    Telefono = c("6011234567", "3001234567", "NO VISIBLE")))
