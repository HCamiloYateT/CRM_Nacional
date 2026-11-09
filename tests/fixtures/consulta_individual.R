# Entidades sin ventas, lead vinculado, sucursales y un asesor ajeno ----
filtros_individual <- list(fecha = as.Date(c("2026-01-01", "2026-11-09")), asesor = "ANA", segmento = "MEDIANO",
  linneg = "A LA MEDIDA", categoria = "Café", producto = c("A", "B"))
contactos_individual <- data.frame(CodContacto = c("C1", "C2", "C3"),
  PerCod = c(900123456, 202, 303), PerRazSoc = c("Contacto sin ventas", "Lead con ventas", "Contacto sin ventas"),
  Asesor = c("ANA", "ANA", "LUIS"), UsuarioCrea = c("ANA", "ANA", "LUIS"), Estado = "ACTIVO")
etapas_individual <- data.frame(CodContacto = "C2", EtapaNueva = "LEAD", FechaHora = as.POSIXct("2026-01-01"))
vinculos_individual <- data.frame(CodContacto = "C2", NitVinculado = 102)
clientes_individual <- data.frame(CliNitPpal = c(101, 101, 303), CLCliNit = c(101, 102, 303),
  LinNegCod = 21000, Asesor = c("ANA", "ANA", "LUIS"), Segmento = "MEDIANO", SSPpto = c(10, 20, 30),
  MNFCCPpto = c(100, 200, 300), FecProceso = as.Date("2026-01-01"),
  PerRazSoc = c("Cuenta consolidada", "Cuenta consolidada", "Cuenta ajena"))
hechos_individual <- data.frame(CliNitPpal = c(101, 101, 101, 303), CLCliNit = c(101, 102, 101, 303),
  CodContacto = c(NA, "C2", NA, "C3"), PerRazSoc = "Mismo nombre",
  LinNegCod = 21000, CLLinNegNo = "A LA MEDIDA", Asesor = c("ANA", "ANA", "ANA", "LUIS"), Segmento = "MEDIANO",
  Categoria = "Café", Producto = c("A", "B", "A", "B"),
  FecFact = as.Date(c("2026-10-01", "2026-11-01", "2026-12-01", "2026-11-01")),
  EstadoFinanciero = c("CERRADO", "FACTURADO_ABIERTO", "FACTURADO_ABIERTO", "CERRADO"),
  SacosFact = c(2, 3, 4, 100), Margen = c(200, 300, 400, 10000), CLPdcCod = 1:4, CLPdcLin = 1,
  CLLotCod = 1:4, SacosPedido70Linea = c(2, 6, 10, 100), SacosLote70 = c(2, 6, 10, 100),
  SacosProducidos70 = c(2, 6, 10, 100), SacosDespachados70 = c(2, 3, 4, 100),
  FacturadosOperativos70 = c(2, 3, 4, 100), PdcFecEst = as.Date("2026-12-01"))
