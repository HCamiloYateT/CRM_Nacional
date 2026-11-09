# Regresiones con datos sintéticos, sin consultas de producción ----
library(dplyr)
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/datos/02_fuentes_comerciales.R")
source("APP/misc/datos/03_evolucion_historica.R")
source("APP/misc/modules/generalidades/evolucion_negocio/00_funciones.R")
clientes <- data.frame(CLCliNit = c("11", "22"), CliNitPpal = c("1", "2"), LinNegCod = 10000,
  Asesor = c("Ana", "Luis"), Segmento = "MEDIANO", Excluir = "NO", SSPpto = c(120, 240),
  MNFCCPpto = c(1200, 2400), FecProceso = as.POSIXct("2026-01-01", tz = "UTC"))
x <- data.frame(CliNitPpal = c("1", "1", "1", "2", "2"), LinNegCod = 10000,
  Asesor = c("Ana", "Ana", "Ana", "Luis", "Luis"), Segmento = "MEDIANO", Excluir = "NO",
  ProdExcluir = "NO", CLLinNegNo = "CONVENCIONALES", Categoria = "Consumo", Producto = "A",
  FecMes = as.Date(c("2025-01-01", "2025-12-01", "2026-01-01", "2026-01-01", "2026-02-01")),
  FecFact = as.Date(c("2025-01-10", "2025-12-10", "2026-01-10", "2026-01-10", "2026-02-10")),
  EstadoFinanciero = c(rep("CERRADO", 4), "FACTURADO_ABIERTO"),
  SacosFact = c(1, 9, 3, 7, 5), Margen = c(10, 90, 30, 70, 50))
f <- list(asesor = c("Ana", "Luis"), segmento = "MEDIANO", linneg = "CONVENCIONALES",
  categoria = "Consumo", producto = "A")
h <- data.frame(CLCliNit = c("11", "11", "33"), LinNegCod = 10000,
  FecMes = as.Date(c("2024-12-01", "2026-01-01", "2024-12-01")),
  SacosFact = c(2, 999, 1000), Margen = c(20, 9990, 10000), Fuente = "Excel")
corte <- as.Date("2026-01-31")
m <- .ev_mensual(x, h, clientes, f, corte)
stopifnot(sum(m$SacosFact) == 22, sum(m$Margen) == 220, attr(m, "historico_sin_homologacion") == 1)
stopifnot(!any(m$SacosFact %in% c(999, 1000)), .ev_completo(x, f))
f_ana <- f; f_ana$asesor <- "Ana"
ma <- .ev_mensual(x, h, clientes, f_ana, corte)
stopifnot(sum(ma$SacosFact) == 15, !.ev_completo(x, f_ana))
fp <- f; fp$producto <- "B"
stopifnot(nrow(.ev_mensual(x, h, clientes, fp, corte)) == 0)
a <- .ev_anual(m, clientes, f, corte, "Linea", comparable = TRUE)
stopifnot(a$Sacos[a$Anho == 2025] == 1, a$Sacos[a$Anho == 2026] == 10,
  a$PptoSacos[a$Anho == 2026] == 30, a$CumplSacos[a$Anho == 2026] == 1/3,
  a$VariacionSacos[a$Anho == 2026] == 9, a$ProySacos[a$Anho == 2026] == 120)
b <- .ev_anual(m, clientes, f, corte, "Linea", comparable = FALSE)
stopifnot(is.na(b$VariacionSacos[b$Anho == 2026]))
stopifnot(nrow(.ev_crecimiento(b, 2025, 2026)) == 0, nrow(.ev_crecimiento(a, 2025, 2026)) == 1)
provisional <- .ev_mensual(x, h, clientes, f, as.Date("2026-02-28"), abiertos = TRUE)
stopifnot(sum(provisional$SacosFact) == 27)
ref <- .ev_referencia("APP")$anual
r <- .ev_anual(m, clientes, f, corte, referencia = ref, permitir_referencia = TRUE, inicio_etl = 2025)
stopifnot(sum(r$Anho == 2017) == 1, r$Sacos[r$Anho == 2017] == 23864,
  !any(r$Anho > 2026), !any(grepl("Presentación", r$Fuente[r$Anho == 2026])),
  r$PptoSacos[r$Anho == 2025] == 56592,
  r$FuentePresupuesto[r$Anho == 2026] == "Snapshot de clientes del año")
r2 <- .ev_anual(m, clientes, f, corte, comparable = TRUE, referencia = ref, permitir_referencia = TRUE)
stopifnot(!any(grepl("Presentación", r2$Fuente)))
r3 <- .ev_anual(m, clientes, f, corte, presupuesto_disponible = FALSE)
stopifnot(all(is.na(r3$PptoSacos)))
# Un cliente con compras previas fuera del filtro no se presenta como nuevo.
n <- .ev_nuevos(ma, data.frame(CliNitPpal = "1", MinFecFact = as.Date("2015-01-01")), 2026, "Linea")
stopifnot(nrow(n) == 0)
p <- .ev_primera_compra(data.frame(FctNit = c(11, 12),
  MinFecFact = as.Date(c("2015-01-01", "2026-01-01"))),
  data.frame(PerCod = c(11, 12), CliNitPpal = 1))
stopifnot(nrow(p) == 1L, p$CliNitPpal == "1", p$MinFecFact == as.Date("2015-01-01"),
  nrow(.ev_nuevos(ma, p, 2026, "Linea")) == 0)
# Los empates contradictorios se excluyen antes de atribuir el histórico a un asesor.
cc <- rbind(clientes, transform(clientes[1, ], Asesor = "Otro"))
stopifnot(!"11" %in% .ev_catalogo(cc)$CLCliNit)
stopifnot(.ev_id(1128270846) == "1128270846")
cat("OK: prioridad ETL, permisos, filtros, periodos comparables, proyección separada, presupuesto y referencia.\n")
