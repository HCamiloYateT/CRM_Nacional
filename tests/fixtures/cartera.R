# Datos ficticios para verificar cartera sin conectarse a servicios de negocio ----
cartera_prueba <- data.frame(TCEFch = as.Date("2026-11-05"), PerCod = c(11, 11, 22, 33),
  Factura = c("F1", "F2", "F3", "F4"), LinNegCod = c(21000, 21000, 10000, 21000), LinProCod = 1:4,
  TCEPdcRef = 1:4, TCEPdcLin = 1, TCEPdcCntC = paste0("PED", 1:4),
  TCEFchVto = as.Date(c("2026-11-01", "2026-10-25", "2026-12-30", "2027-01-05")),
  SaldoCOP = c(100, 200, 300, 400), ValorCOP = c(110, 210, 310, 410), TCESac = c(1, 2, 3, 4),
  PerRazSoc = c("Cliente Alfa", "Cliente Alfa", "Cliente Beta", "Cliente Gamma"),
  Negocio = c("A LA MEDIDA", "A LA MEDIDA", "COPRODUCTO", "A LA MEDIDA"),
  ForPagNom = "Crédito", FcnFec = as.Date("2026-10-01"), TCEObsVen = "Revisión",
  TGCFchGes = as.Date("2026-11-02"), TGCObs = "Contacto registrado")
facturacion_prueba <- data.frame(PerCod = c(11, 11, 22, 33), Factura = paste0("F", 1:4), FcnPrf = "FE",
  PerRazSoc = c("Cliente Alfa", "Cliente Alfa", "Cliente Beta", "Cliente Gamma"),
  FcnFec = as.Date(c("2025-01-01", "2026-01-01", "2026-01-01", "2026-10-01")),
  FecPag = as.Date(c("2025-01-06", "2026-01-11", "2026-01-21", NA)),
  FecVto = as.Date(c("2025-01-10", "2026-01-15", "2026-01-15", "2026-11-05")),
  FcnVal = c(100, 100, 300, 500), FcnSal = c(0, 0, 0, 400), Sacos = c(1, 2, 3, 4),
  PdcCod = 1:4, ForPagNom = "Crédito")
clientes_prueba <- data.frame(CliNitPpal = c(11, 22, 33), CLCliNit = c(11, 22, 33),
  LinNegCod = c(21000, 10000, 21000), Asesor = c("ANA", "LUIS", "ANA"),
  Segmento = "MEDIANO", SSPpto = 100, MNFCCPpto = 1000, FecProceso = as.Date("2026-01-01"))
mapa_prueba <- data.frame(PerCod = c(11, 22, 33), CliNitPpal = c(11, 22, 33))
fuente_prueba <- list(cartera = cartera_prueba, facturacion = facturacion_prueba,
  origen = "Datos ficticios de prueba", importado_en = as.Date("2026-11-05"))
