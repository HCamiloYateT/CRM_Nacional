library(shiny)
library(dplyr)
sys.source("APP/misc/compartido/04_interaccion.R", envir = environment())
sys.source("APP/misc/modules/herramientas/calculadoras/01_modulo.R", envir = environment())
sys.source("tests/fixtures/formulas_calculadoras.R", envir = environment())
.ind_prueba <- data.frame(Item = c("TRM (Hoja de trabajo)", "Precio NYC (HT)", "Diferencial de Compra (HT)",
  "Precio Carga (Promedio de últimas entradas del día)", "Precio Consumo (Compras)",
  "Precio Molidos (Compras)", "Precio Soluble (Compras)"), Valor = c(4000, 200, 20, 1800000, 12000, 15000, 16000))
.datos_calculos <- list(
  CalculadoraCoproductos = list(COSTO_9D3 = 15000, COSTO_9D1 = 16000, COSTO_5D50 = 15500,
    COSTO_5D5 = 15000, COSTO_1D7 = 12000, CostosTrilladora = 400, GastosFijos = 200, Fletes = 0,
    MargenEsperado = 200, Pre9D3 = 20000, Pre9D1 = 20000, Pre5D50 = 20000, Pre5D5 = 20000, Pre1D7 = 20000,
    Kls9D3 = 1, Kls9D1 = 2, Kls5D50 = 3, Kls5D5 = 4, Kls1D7 = 5, Segmento = "MEDIANO"),
  CalculadoraDolaresExcelso = list(TRM = 4000, NYC = 200, PrimaUGQ = 20, PrimaMallas = 5,
    PrimaEspeciales = 10, Utilidad = 13.39, Empaque = 0, Grainpro = 0, Flete = 200),
  CalculadoraPesosExcelso = list(TRM = 4000, CostoConsumo = 12000, PrecioCarga = 1800000, PrimaEspecial = 2000,
    PrimaSupremo = 1000, MarBlend = 1000, MarUGQ = 1500, MarSup = 2200, MarEsp = 3000, Prima = 2500,
    FleteInt = 500, FleteExt = 300, CostosFijos = 200, Financieros = 1700, Precio = 40000))
for (.nombre in names(.datos_calculos)) {
  .entrada <- .datos_calculos[[.nombre]]
  .esperado <- get(paste0(".esperado_", .nombre))(.entrada)
  .args <- list(dat_ind = reactive(.ind_prueba), usr = reactive("ANALISTA"))
  if (.nombre == "CalculadoraCoproductos") .args$clientes <- data.frame(LinNegCod = 10000, CliNitPpal = 1,
    Segmento = "MEDIANO", FecProceso = Sys.Date(), MNFCCPpto = 14000, SSPpto = 1)
  if (.nombre == "CalculadoraPesosExcelso") .args$margenes <- data.frame(FechaHoraCrea = Sys.time(),
    MarBlend = 1000, MarUGQ = 1500, MarSup = 2200, MarEsp = 3000)
  testServer(get(.nombre), args = .args, {
    do.call(session$setInputs, c(.entrada, list(Calcular = 1)))
    stopifnot(isTRUE(all.equal(val_calculadora(), .esperado, check.attributes = FALSE)))
  })
}
cat("OK: las tres calculadoras conservan todas las fórmulas; costos cero admitidos.\n")

# Calcular no persiste; guardar requiere permiso y confirmación previa. ----
.guardados_prueba <- list()
.agregar_prueba <- function(datos) .guardados_prueba[[length(.guardados_prueba) + 1L]] <<- datos
testServer(CalculadoraPesosExcelso, args = list(dat_ind = reactive(.ind_prueba), usr = reactive("CMEDINA"),
  margenes = data.frame(FechaHoraCrea = Sys.time(), MarBlend = 1, MarUGQ = 2, MarSup = 3, MarEsp = 4),
  guardar_margenes = .agregar_prueba), {
  do.call(session$setInputs, c(.datos_calculos$CalculadoraPesosExcelso, list(Calcular = 1)))
  stopifnot(length(.guardados_prueba) == 0L)
  session$setInputs(ConfirmarMargenes = 1)
  stopifnot(length(.guardados_prueba) == 0L)
  session$setInputs(GuardarMargenes = 1)
  session$setInputs(MarBlend = 9999)
  session$setInputs(ConfirmarMargenes = 2)
  stopifnot(length(.guardados_prueba) == 1L, .guardados_prueba[[1]]$MarBlend == 1000)
  session$setInputs(ConfirmarMargenes = 3)
  stopifnot(length(.guardados_prueba) == 1L)
})
cat("OK: guardado autorizado, confirmado una vez y con los valores revisados.\n")


# Sin clientes, fechas ausentes y selección sin datos no deben advertir en max().
for (.fuente_vacia in list(
  data.frame(LinNegCod = numeric(), CliNitPpal = numeric(), Segmento = character(),
    FecProceso = as.Date(character()), MNFCCPpto = numeric(), SSPpto = numeric()),
  data.frame(LinNegCod = 10000, CliNitPpal = 1, Segmento = "MEDIANO",
    FecProceso = as.Date(NA), MNFCCPpto = 14000, SSPpto = 1))) {
  local({
    advertencias <- character()
    anterior <- options(warn = 2)
    on.exit(options(anterior))
    withCallingHandlers(testServer(CalculadoraCoproductos, args = list(dat_ind = reactive(.ind_prueba),
      usr = reactive("ANALISTA"), clientes = .fuente_vacia), {
      session$setInputs(Segmento = "MEDIANO")
      session$flushReact()
      session$setInputs(Segmento = "DETAL")
      session$flushReact()
    }), warning = function(w) advertencias <<- c(advertencias, conditionMessage(w)))
    stopifnot(!length(advertencias))
  })
}
cat("OK: coproductos sin advertencias con clientes vacíos o fechas ausentes.\n")
