library(dplyr)
source("APP/misc/01_analitica_comercial.R")
source("APP/misc/02_fuentes_comerciales.R")
source("APP/misc/modules/gestion_comercial/01_variables_bono.R")
source("APP/misc/modules/gestion_comercial/08_cartera.R")
source("APP/misc/modules/gestion_comercial/07_fidelidad_riesgo.R")

# Pesos, umbrales y bordes: cifras provisionales 2026 identificadas.
stopifnot(identical(vapply(c(0.69, 0.7, 0.849, 0.85, 0.99, 1), .bon_factor, numeric(1)),
  c(0, 0.33, 0.33, 0.67, 0.67, 1)), .bon_factor(0.8, "ochenta") == 1,
  .bon_factor(0.999, "cien") == 0, is.na(.bon_factor(NA_real_)), .bon_factor(0.85, "provisional") == 0.67)
reglas <- .bon_reglas("2025_carlos")
stopifnot(sum(reglas$Peso) == 1, identical(reglas$Peso, c(0.2, 0.6, 0.2)))
panel <- data.frame(Fecha = as.Date("2026-01-01"), Negocio = c("COPRODUCTO", "COPRODUCTO", "MEDIDA"),
  Segmento = c("DETAL", "MEDIANO", "DETAL"), Margen = c(100, 85, 100), PptoMargen = 100)
resultado <- .bon_calcular(panel, "2026_carlos", as.Date("2026-01-31"))
stopifnot(isTRUE(all.equal(sum(resultado$Aporte), 0.2 + 0.6 * 0.67 + 0.2)),
  all(grepl("provisionales", resultado$Politica)))
panel$PptoMargen <- 0
stopifnot(all(is.na(.bon_calcular(panel, "2026_carlos", as.Date("2026-01-31"))$Aporte)))

# Una fecha, no el histórico de saldos; conversión a COP ya hecha.
cartera <- data.frame(TCEFch = as.Date(c("2026-01-31", "2026-01-31", "2026-02-28")),
  Factura = c("F1", "F2", "F1"), PerCod = c(123, 456, 123), TCETipCar = "FAC", TCESac = 1,
  TCEFchVto = as.Date(c("2026-01-01", "2026-02-05", "2026-01-01")), SaldoCOP = c(1000, 2000, 500),
  PerRazSoc = c("A", "B", "A"), Negocio = "MEDIDA")
x <- .car_preparar(cartera, as.Date("2026-01-31"))
stopifnot(nrow(x) == 2L, sum(x$Saldo) == 3000, x$Tipo[[1]] == "Vencida", x$Tipo[[2]] == "Por vencer",
  sum(.car_vencimientos(x)$SaldoCOP) == 2000, sum(.car_resumen(x)$SaldoCOP) == 3000)
# No mostrar clientes fuera de permisos ni duplicar por la homologación.
cli <- data.frame(CliNitPpal = "999", LinNegCod = 21000)
mapa <- data.frame(PerCod = c(123, 456), CliNitPpal = c(999, 888))
stopifnot(nrow(.car_scope(cartera, cli, mapa)) == 2L)

# Rotación ponderada por importe; fecha inválida y abonos no son pagos totales inferidos.
pagos <- data.frame(Factura = c("A", "B", "C", "D"), PerCod = 123, PerRazSoc = "Cliente",
  FcnFec = as.Date("2026-01-01"), FecPag = as.Date(c("2026-01-11", "2026-01-21", "1753-01-01", "2026-01-10")),
  FcnVal = c(100, 300, 500, 100), FcnSal = c(0, 0, 0, 20))
p <- .car_pagos(pagos, as.Date("2026-01-01"), as.Date("2026-01-31"))
rotacion <- .car_rotacion(p)
stopifnot(sum(p$Pagada) == 2L, rotacion$DiasPagoPonderados == 17.5,
  nrow(.car_rotacion(p[0, ])) == 0L)

# Presupuesto una sola vez por cliente/línea; margen/kg calculado con totales.
cli <- data.frame(CliNitPpal = c(1, 1, 2), LinNegCod = 21000, Asesor = "ANA", Segmento = "DETAL",
  SSPpto = c(120, 120, 240), MNFCCPpto = c(1200, 1200, 2400),
  FecProceso = as.Date(c("2026-01-01", "2026-01-01", "2026-01-01")),
  PerRazSoc = c("Uno", "Uno", "Dos"))
cli <- .com_clientes(cli, as.Date("2026-01-31"), anual = TRUE)
stopifnot(nrow(cli) == 2L, sum(cli$MNFCCPpto) == 3600)
hechos <- data.frame(CliNitPpal = c(1, 2), LinNegCod = 21000, CLLinNegNo = "A LA MEDIDA", Asesor = "ANA",
  Segmento = "DETAL", Categoria = "Café", Producto = "Molido", Marca = "Racafé", PerRazSoc = c("Uno", "Dos"),
  FecFact = as.Date("2026-01-10"), EstadoFinanciero = "CERRADO", SacosFact = c(1, 9), Margen = c(70, 315),
  Depto = "D", Mpio = "M")
h <- .com_hechos(hechos, as.Date("2026-01-31"))
r <- .com_resumen(h, cli, as.Date("2026-01-31"), "CLLinNegNo")
stopifnot(nrow(r) == 1L, r$PptoMargen == 300, r$MargenKg == 0.55,
  all(is.na(.com_resumen(h, cli, as.Date("2026-01-31"), "Producto")$PptoMargen)))
cat("OK: bonos provisionales, saldos por corte, permisos, pagos ponderados y grano del presupuesto.\n")
# Un presupuesto desconocido no se transforma en cumplimiento cero ni en gasto ejecutado.
sin_anho <- .com_clientes(cli, as.Date("2025-12-31"), anual = TRUE)
stopifnot(nrow(sin_anho) == 0L)
parcial <- h
parcial$Margen[[1]] <- NA_real_
stopifnot(is.na(.com_resumen(parcial, cli, as.Date("2026-01-31"), "CLLinNegNo")$Margen))
# Margen negativo y presupuesto nulo se conservan; no repartir metas a productos.
negativo <- h
negativo$Margen <- c(-70, 0)
stopifnot(.com_resumen(negativo, cli, as.Date("2026-01-31"), "CLLinNegNo")$Margen == -70)
# Importación local mínima: fecha POSIX a Date y solo tablas reconocidas, sin ejecutar scripts.
local({
  ruta <- tempfile(fileext = ".RData")
  on.exit(unlink(ruta))
  data_comercial <- data.frame(FctNit = 1, Fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    Volumen = 1, Margen = 1, PptoVolumen = 1, PptoMargen = 1)
  codigo_no_ejecutar <- function() stop("Nunca debe ejecutarse")
  save(data_comercial, codigo_no_ejecutar, file = ruta)
  importado <- .com_leer_historico(ruta)
  stopifnot(inherits(importado$comercial$Fecha, "Date"), !"codigo_no_ejecutar" %in% names(importado))
})
# La discrepancia histórica no se oculta al migrar.
legado <- data.frame(Fecha = as.Date("2024-01-01"), Negocio = c("MEDIDA", "COPRODUCTO", "NUEVOS"),
  Segmento = "TODOS", Margen = 100, PptoMargen = 100)
legado <- .bon_calcular(legado, "2024_jonathan", as.Date("2024-01-31"))
stopifnot(legado$Aporte[legado$Componente == "MEDIDA"] == 0.75,
  legado$AporteIndicadorAntiguo[legado$Componente == "MEDIDA"] == 0.75 * 0.65,
  legado$Aporte[legado$Componente == "NUEVOS"] == 0.65 * 0.25)
stopifnot(.com_tasa_referencia(data.frame(Ano = 2025, Tasa = 4000), 2025) == 4000,
  is.na(.com_tasa_referencia(data.frame(Ano = c(2025, 2025), Tasa = c(4000, 4200)), 2025)),
  is.na(.com_tasa_referencia(NULL, 2026)))
# Revisiones intradía: tomar la última, no el máximo presupuesto ni un registro de auditoría arbitrario.
revisiones <- cli[rep(1, 2), ]
revisiones$FecProceso <- as.POSIXct(c("2026-01-01 08:00:00", "2026-01-01 10:00:00"), tz = "UTC")
revisiones$MNFCCPpto <- c(500, 400)
revisiones$RegistroAuditoria <- 1:2
stopifnot(.com_clientes(revisiones, as.Date("2026-01-31"), anual = TRUE)$MNFCCPpto == 400)


# Cada sucursal conserva su último corte antes de agregar por NIT principal.
sucursales <- data.frame(CLCliNit = c(11, 11, 12), CliNitPpal = 1, LinNegCod = 21000,
  FecProceso = as.POSIXct(c("2026-01-01", "2026-02-01", "2026-01-01"), tz = "UTC"),
  Asesor = "ANA", Segmento = "DETAL", SSPpto = c(100, 120, 240), MNFCCPpto = c(1000, 1200, 2400),
  PerRazSoc = c("Principal", "Principal", "Principal"), Mpio = c("A", "A", "B"))
consolidado <- .com_clientes(sucursales, as.Date("2026-02-28"), anual = TRUE)
stopifnot(nrow(consolidado) == 1L, consolidado$SSPpto == 360, consolidado$MNFCCPpto == 3600,
  is.na(consolidado$Mpio), !consolidado$PresupuestoEnRevision,
  nrow(attr(consolidado, "presupuesto_detalle")) == 2L)
# Distintas descripciones no duplican la meta; una dimensión ambigua permanece desconocida.
descripciones <- sucursales[rep(2, 2), ]
descripciones$PerRazSoc <- c("Nombre anterior", "Nombre actualizado")
unico <- .com_clientes(descripciones, as.Date("2026-02-28"), anual = TRUE)
stopifnot(nrow(unico) == 1L, unico$MNFCCPpto == 1200, is.na(unico$PerRazSoc))
# Una contradicción real no tumba todas las vistas ni escoge el importe mayor.
contradiccion <- descripciones
contradiccion$MNFCCPpto <- c(1200, 1500)
revision <- .com_clientes(contradiccion, as.Date("2026-02-28"), anual = TRUE)
stopifnot(nrow(revision) == 1L, revision$PresupuestoEnRevision, is.na(revision$MNFCCPpto), revision$SSPpto == 120)
# Un filtro de asesor no puede resolver artificialmente un empate de versiones contradictorias.
contradiccion$Asesor <- c("ANA", "LUIS")
filtrado <- .com_clientes(contradiccion, as.Date("2026-02-28"),
  filtros = list(asesor = "ANA", segmento = "DETAL", linneg = "A LA MEDIDA"), anual = TRUE)
stopifnot(nrow(filtrado) == 0L)
cat("OK: sucursales, empates de descripción, conflictos reales y filtros sin selección arbitraria.\n")
# No multiplicar ventas al conservar varios planes del mismo principal/segmento.
ventas <- hechos[1, ]
ventas$CliNitPpal <- 1
ventas$SacosFact <- 10
ventas$Margen <- 100
plan <- .bon_panel_crm(ventas, consolidado, as.Date("2026-02-28"), "ANA")
stopifnot(sum(plan$Margen) == 100, sum(plan$PptoMargen) == 600, nrow(plan) == 2L)
# El desglose municipal usa las metas de cada sucursal; no pierde la dimensión por agregar al principal.
municipios <- .com_resumen(.com_hechos(ventas, as.Date("2026-02-28")), consolidado,
  as.Date("2026-02-28"), "Mpio")
stopifnot(municipios$PptoMargen[municipios$Dimension == "A"] == 200,
  municipios$PptoMargen[municipios$Dimension == "B"] == 400)
# Asesores distintos conservan sus propios objetivos sin repetir el principal ni su ejecución.
por_asesor <- sucursales
por_asesor$Asesor[por_asesor$CLCliNit == 12] <- "LUIS"
planes <- .com_clientes(por_asesor, as.Date("2026-02-28"), anual = TRUE)
desglose <- .com_resumen(.com_hechos(ventas, as.Date("2026-02-28")), planes,
  as.Date("2026-02-28"), "Asesor")
stopifnot(desglose$PptoMargen[desglose$Dimension == "ANA"] == 200,
  desglose$PptoMargen[desglose$Dimension == "LUIS"] == 400)
plan_ana <- .bon_panel_crm(ventas, planes, as.Date("2026-02-28"), "ANA")
stopifnot(sum(plan_ana$PptoMargen) == 200, sum(plan_ana$Margen) == 100)
