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

# Los módulos no asignan ui/server globales ni lanzan demos durante el arranque ----
for (ruta in list.files(file.path(root, "APP/misc/modules"), "[.]R$", recursive = TRUE, full.names = TRUE)) {
  for (expresion in parse(ruta)) {
    if (!is.call(expresion)) next
    .check(!identical(expresion[[1]], as.name("shinyApp")))
    if (identical(expresion[[1]], as.name("<-"))) .check(!as.character(expresion[[2]])[[1]] %in% c("ui", "server"))
  }
}
message("OK: ", .checks, " verificaciones sin acceso a servicios de negocio.")
