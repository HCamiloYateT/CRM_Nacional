# Regresión del contrato ETL/app, sin consultas ni escrituras de negocio ----
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1]] else ".")
library(dplyr)
source(file.path(root, "APP/misc/data_contract.R"))

.checks <- 0L
.check <- function(condicion) {
  stopifnot(isTRUE(condicion))
  .checks <<- .checks + 1L
}
.error <- function(expresion, texto) {
  error <- tryCatch({ force(expresion); NULL }, error = identity)
  .check(inherits(error, "error") && grepl(texto, conditionMessage(error), fixed = TRUE))
}

# Hechos mensuales: dos facturas del mismo lote y un registro PyG sin marca ----
dat <- as.data.frame(setNames(rep(list(rep(NA_real_, 3)), length(.crm_columnas_data)), .crm_columnas_data))
for (columna in c("Segmento", "Asesor", "CLLinNegNo", "Excluir")) dat[[columna]] <- rep(NA_character_, 3)
dat$LinNegCod <- c(10000, 10000, 21000)
dat$LinProCod <- c(1, 1, 2)
dat$MCCod <- c(1, 1, NA)
dat$MrcCod <- c(1, 1, NA)
dat$CliNitPpal <- c(10, 10, 20)
dat$CLLotCod <- c(100, 100, 200)
dat$FecMes <- as.Date(c("2026-01-01", "2026-02-01", "2026-02-01"))
dat$FecFact <- as.Date(c("2026-01-15", "2026-02-15", "2026-02-20"))
dat$SacosFact <- c(2, 3, 5)
dat$Margen <- c(20, 30, 50)
dat$SacLote <- c(100, 100, NA)
dat$CLLinNegNo <- c("CONVENCIONALES", "CONVENCIONALES", "A LA MEDIDA")
productos <- data.frame(LinNegCod = c(10000, 21000), LinProCod = c(1, 2), MCCod = 1, MrcCod = 1,
                       LinProNom = c("CONVENCIONAL", "ESPECIAL"), MCNom = "EMPAQUE", Marca = "RACAFÉ",
                       Categoria = c("CAT A", "CAT B"), Producto = c("PROD A", "PROD B"), Excluir = "NO")
segmentos <- data.frame(CliNitPpal = c(10, 20), LinNegCod = c(10000, 21000), SegmentoRacafe = "CLIENTE")
ordenes <- data.frame(CLLotCod = c(100, 200), OrdenCompra = c("OC1, OC2", "OC3"))
resultado <- .crm_preparar_datos(dat, productos, segmentos, ordenes)
.check(nrow(resultado) == nrow(dat))
.check(identical(resultado$SacosFact, dat$SacosFact))
.check(identical(resultado$Margen, dat$Margen))
.check(identical(resultado$FecMes, dat$FecMes))
.check(identical(resultado$FecFact, dat$FecFact))
.check(resultado$Producto[[3]] == "PROD B")
.check(resultado$OrdenCompra[[1]] == "OC1, OC2")
.error(.crm_preparar_datos(dat, bind_rows(productos, productos[1, ]), segmentos, ordenes), "productos_snap")
.error(.crm_preparar_datos(dat, productos, bind_rows(segmentos, segmentos[1, ]), ordenes), "segmentaciones_actual")
.error(.crm_preparar_datos(dat, productos, segmentos, bind_rows(ordenes, ordenes[1, ])), "ord_cmp")
ambiguos <- bind_rows(productos, mutate(productos[2, ], MCCod = 2, Producto = "OTRO", Categoria = "OTRA"))
ambiguo <- .crm_preparar_datos(dat, ambiguos, segmentos, ordenes)
.check(ambiguo$Producto[[3]] == "SIN DATO")
.check(ambiguo$Categoria[[3]] == "SIN DATO")
.check(identical(ambiguo$SacosFact, dat$SacosFact))

# Contrato y carga atómica: rechazar exportaciones incompletas sin reemplazar datos vigentes ----
datos <- new.env(parent = emptyenv())
for (nombre in names(.crm_contrato_exportacion)) {
  datos[[nombre]] <- as.data.frame(setNames(rep(list(character()), length(.crm_contrato_exportacion[[nombre]])),
                                          .crm_contrato_exportacion[[nombre]]))
}
datos$ultimo_periodo_pyg <- as.Date("2026-02-01")
datos$data <- dat
datos$productos_snap <- productos
datos$segmentaciones_actual <- segmentos
datos$ord_cmp <- ordenes
.check(.crm_validar_exportacion(datos))
ruta <- tempfile(fileext = ".RData")
save(list = ls(datos), file = ruta, envir = datos)
destino <- new.env(parent = emptyenv())
.crm_cargar_exportacion(ruta, destino)
.check(identical(destino$data, dat))
rm("fact_cliente", envir = datos)
save(list = ls(datos), file = ruta, envir = datos)
.error(.crm_cargar_exportacion(ruta, destino), "fact_cliente")
.check(identical(destino$data, dat))
.error(.crm_cargar_exportacion(paste0(ruta, "-ausente")), "No existe")
datos$fact_cliente <- destino$fact_cliente
datos$data$FecMes <- as.character(datos$data$FecMes)
.error(.crm_validar_exportacion(datos), "debe ser Date")
unlink(ruta)

# Detalle operativo: una fila por lote, factura acumulada y pedido sin lote ----
operaciones <- data.frame(CLCliNit = c(10, 20, 30), LinNegCod = c(10000, 21000, 10000),
                         LinProCod = c(1, 2, 1), MCCod = c(1, 1, NA), MrcCod = c(1, 1, NA),
                         CLLinProNo = "TIPO", CLPdcCod = 1:3, CLPdcLin = 1,
                         CLLotCod = c(100, 200, NA), UMeFac = c(62.5, 70, 70),
                         SacLote = c(70, 20, NA), SacProd = c(56, 10, NA), SacDesp = c(28, 5, NA),
                         PdcFecCre = as.Date("2026-01-01"), FecProd = as.Date("2026-01-10"),
                         FecDesp = as.Date("1753-01-01"))
pedidos <- data.frame(PdcCod = 1:3, PdcLin = 1, PdcCan = c(70, 20, 30), UMeFac = c(62.5, 70, 70))
facturas <- data.frame(CLLotCod = c(100, 200), SacosFact = c(25, 5), FecFact = as.Date("2026-02-15"))
operativo <- .crm_preparar_operaciones(operaciones, pedidos, facturas, productos)
.check(nrow(operativo) == 3L)
.check(isTRUE(all.equal(operativo$SacLote[1], 62.5)))
.check(operativo$SacosFact[1] == 25)
.check(is.na(operativo$CLLotCod[3]) && operativo$SacosPedido[3] == 30)
.check(all(is.na(operativo$FecDesp)))
.error(.crm_preparar_operaciones(operaciones, pedidos, bind_rows(facturas, facturas[1, ]), productos), "fact_lote")

# La conversión utiliza primera factura por NIT aunque data solo conserve el mes PyG ----
env <- new.env(parent = globalenv())
sys.source(file.path(root, "APP/misc/modules/prospeccion/00_Funciones.R"), envir = env)
env$derivar_etapa_actual <- function() data.frame(CodContacto = "LEAD1", PerCod = "10", Etapa = "LEAD")
env$CargarDatos <- function(tabla) {
  if (tabla == "CRMNALLEADCLIENTE") return(data.frame(CodContacto = character()))
  data.frame(CodContacto = character(), NitVinculado = character())
}
escrituras <- list()
env$AgregarDatos <- function(fila, tabla) escrituras[[tabla]] <<- fila
env$registrar_transicion_etapa <- function(...) invisible(NULL)
primera <- data.frame(FctNit = c(10, 10), MinFecFact = as.Date(c("2025-01-05", "2026-02-01")))
.check(env$detectar_conversion_leads(facturas = primera) == 1L)
.check(escrituras$CRMNALLEADCLIENTE$FechaConversion[[1]] == as.Date("2025-01-05"))

# Exportación de dos objetos: mantener ejecución y proyección separadas ----
maestro <- dat
maestro$IdRegistroCRM <- 1:3
maestro$EstadoFinanciero <- c("CERRADO", "CERRADO", "FACTURADO_ABIERTO")
maestro$FechaCortePyG <- as.Date("2026-02-01")
maestro$CLPdcCod <- c(1, 1, 2)
maestro$CLPdcLin <- 1
maestro$CLCliNit <- c(10, 10, 20)
maestro$UMeFac <- c(62.5, 62.5, 70)
maestro$SacosLote70 <- c(62.5, 62.5, 20)
maestro$SacosProducidos70 <- c(50, 50, 10)
maestro$SacosDespachados70 <- c(25, 25, 5)
maestro$SacosPedido70Linea <- c(62.5, 62.5, 20)
maestro$FacturadosOperativos70 <- c(5, 5, 5)
maestro$FechaCreacionPedido <- as.Date("2026-01-01")
maestro$FechaProduccion <- as.Date("2026-01-10")
maestro$FechaInstruccionDespacho <- as.Date("2026-01-05")
maestro$FechaDespachoReal <- as.Date("2026-01-20")
maestro$CodigoMarcaLote <- 1
maestro$CodigoNombreComercialLote <- 1
maestro$SacosProyectados <- NA_real_
proyectado <- maestro[3, ]
proyectado$IdRegistroCRM <- 4L
proyectado$EstadoFinanciero <- "PROYECTADO"
proyectado$CLPdcCod <- 3
proyectado$CLLotCod <- NA_real_
proyectado$FecFact <- as.Date(NA)
proyectado$SacosFact <- NA_real_
proyectado$Margen <- NA_real_
proyectado$SacosProyectados <- 40
proyectado$SacosPedido70Linea <- 40
proyectado$SacosLote70 <- NA_real_
proyectado$SacosProducidos70 <- NA_real_
proyectado$SacosDespachados70 <- NA_real_
maestro <- bind_rows(maestro, proyectado)
referencia <- data.frame(CLLotCod = c(100, 100, 200), LinNegCod = c(10000, 10000, 21000),
                        FecMesFact = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")),
                        FecPrimerFact = as.Date(c("2026-01-05", "2026-02-05", "2026-03-05")),
                        FecFact = as.Date(c("2026-01-15", "2026-02-15", "2026-03-15")), SacosFact = c(2, 3, 5))
productos_actuales <- mutate(productos, FecProceso = as.Date("2026-02-01"))
productos_anteriores <- mutate(productos_actuales[1, ], FecProceso = as.Date("2025-01-01"), Producto = "ANTERIOR")
homologaciones <- list(clientes = destino$clientes_snap, clientes_historico = destino$clientes_snap,
                      nit_principal = data.frame(PerCod = c(10, 20), CliNitPpal = c(10, 20)),
                      segmentos = segmentos, productos = bind_rows(productos_anteriores, productos_actuales),
                      ordenes_compra = data.frame(Lote = c(100, 100, 200), OrdenCompra = c("OC1", "OC2", "OC3")),
                      clientes_sistema = destino$n_cliente, facturacion = referencia,
                      margenes_pyg = destino$margenes_pyg,
                      lotes = data.frame(CLLotCod = c(100, 200), CLCliNit = c(10, 20)))
.check(.crm_validar_maestro(maestro, homologaciones))
adaptados <- .crm_adaptar_maestro(maestro, homologaciones)
.check(nrow(adaptados$data) == 3L)
.check(identical(names(adaptados$data), .crm_columnas_data))
.check(identical(adaptados$data$SacosFact, dat$SacosFact))
.check(identical(adaptados$data$Margen, dat$Margen))
.check(identical(adaptados$crm_maestro, maestro))
.check(identical(adaptados$crm_maestro$FacturadosOperativos70, maestro$FacturadosOperativos70))
.check(identical(adaptados$fact, referencia))
.check(adaptados$fact_cliente$MinFecFact[adaptados$fact_cliente$FctNit == 10] == as.Date("2026-01-05"))
.check(adaptados$ord_cmp$OrdenCompra[adaptados$ord_cmp$CLLotCod == 100] == "OC1, OC2")
.check(!"ANTERIOR" %in% adaptados$productos_snap$Producto)
operativo_maestro <- adaptados$crm_detalle_operaciones
.check(nrow(operativo_maestro) == 3L)
.check(operativo_maestro$SacLote[operativo_maestro$CLLotCod == 100 & !is.na(operativo_maestro$CLLotCod)] == 62.5)
.check(all(operativo_maestro$FecDesp == as.Date("2026-01-20")))
.check(any(is.na(operativo_maestro$CLLotCod) & operativo_maestro$SacosPedido == 40))
mal <- maestro
mal$SacosFact[mal$EstadoFinanciero == "PROYECTADO"] <- 40
.error(.crm_validar_maestro(mal, homologaciones), "proyecciones")
mal <- maestro
mal$IdRegistroCRM[4] <- 1L
.error(.crm_validar_maestro(mal, homologaciones), "duplicadas")
homologaciones_incompletas <- homologaciones
homologaciones_incompletas$facturacion <- NULL
.error(.crm_validar_maestro(maestro, homologaciones_incompletas), "referencias necesarias")
ruta <- tempfile(fileext = ".RData")
# El artefacto contiene exactamente los dos objetos publicados por el nuevo ETL.
save(maestro, homologaciones, file = ruta)
ruta_correcta <- tempfile(fileext = ".RData")
crm_maestro <- maestro
save(crm_maestro, homologaciones, file = ruta_correcta)
.check(identical(sort(load(ruta_correcta, envir = new.env())), c("crm_maestro", "homologaciones")))
nueva_carga <- new.env(parent = emptyenv())
.crm_cargar_exportacion(ruta_correcta, nueva_carga)
.check(identical(nueva_carga$crm_maestro, maestro))
.check(identical(nueva_carga$data$SacosFact, dat$SacosFact))
.error(.crm_cargar_exportacion(ruta, nueva_carga), "crm_maestro")
.check(identical(nueva_carga$data$SacosFact, dat$SacosFact))
unlink(c(ruta, ruta_correcta))

# El ETL conserva exactamente dos objetos en el archivo de salida ----
guardados <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("save")),
                     as.list(parse(file.path(root, "DataPrep.R"))))
.check(length(guardados) == 1L)
.check(identical(as.character(guardados[[1]][[2]]), "crm_maestro") &&
         identical(as.character(guardados[[1]][[3]]), "homologaciones"))

# Los módulos no asignan ui/server globales ni lanzan demos durante el arranque ----
for (ruta in list.files(file.path(root, "APP/misc/modules"), "[.]R$", recursive = TRUE, full.names = TRUE)) {
  for (expresion in parse(ruta)) {
    if (!is.call(expresion)) next
    .check(!identical(expresion[[1]], as.name("shinyApp")))
    if (identical(expresion[[1]], as.name("<-"))) .check(!as.character(expresion[[2]])[[1]] %in% c("ui", "server"))
  }
}
message("OK: ", .checks, " verificaciones sin acceso a servicios de negocio.")
