# RFM, BG/NBD, potencial y límites de cohortes: pruebas sin conexiones externas ----
library(dplyr)
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/datos/02_fuentes_comerciales.R")
source("APP/misc/modules/gestion_comercial/fidelidad_riesgo/00_modelos.R")
source("APP/misc/modules/prospeccion/potencial_mercado/01_modulo.R")
source("APP/misc/modules/cohortes_clientes/estados/00_datos.R")
stopifnot(.coha_acotar_mes("2027-01-01", "2026-08-15") == as.Date("2026-08-01"),
  .coha_acotar_mes("2025-01-01", "2026-08-15") == as.Date("2026-01-01"))
f <- data.frame(CliNitPpal = rep(1:30, each = 4), LinNegCod = 21000,
  FecFact = as.Date("2025-01-01") + rep(c(0, 30, 90, 180), 30) + rep(1:30, each = 4),
  SacosFact = 1, Margen = 100, EstadoFinanciero = "CERRADO")
r <- .fid_rfm(f, as.Date("2026-01-01"))
stopifnot(nrow(r) == 30, all(r$Compras == 4), all(r$Frecuencia == 3))
# Dos líneas facturadas el mismo día siguen siendo una compra observada.
f2 <- bind_rows(f, f[1, ] %>% mutate(SacosFact = 2, Margen = 200))
r2 <- .fid_rfm(f2, as.Date("2026-01-01"))
stopifnot(r2$Compras[1] == 4, r2$Sacos[1] == 6)
m <- list(parametros = c(r = 0.8, alpha = 10, a = 1.5, b = 3))
p0 <- .fid_bg_predecir(r, m, 0); p1 <- .fid_bg_predecir(r, m, 10); p2 <- .fid_bg_predecir(r, m, 20)
stopifnot(all(p0$ComprasEsperadas == 0), all(p1$ComprasEsperadas >= 0),
  all(p2$ComprasEsperadas >= p1$ComprasEsperadas), all(p1$ProbActivo >= 0 & p1$ProbActivo <= 1))
# Para x=0, el modelo BG/NBD conserva actividad 1 y reduce la expectativa respecto a Poisson sin abandono.
z <- r[1, ]; z$Frecuencia <- 0; z$tx <- 0
p <- .fid_bg_predecir(z, m, 10)
stopifnot(p$ProbActivo == 1, p$ComprasEsperadas <= 0.8 * 10 / (10 + z$T))
# Contraste independiente de la integral respecto a la propensión de abandono.
p <- .fid_bg_predecir(r[1, ], m, 10)
num <- integrate(function(q) (1 - ((10 + r$T[1]) / (10 + r$T[1] + 10 * q))^(0.8 + 3)) /
  q * dbeta(q, 1.5, 6), 0, 1)$value * p$ProbActivo
stopifnot(abs(num - p$ComprasEsperadas) < 1e-5,
  inherits(try(.fid_bg_ajustar(r[1:5, ]), silent = TRUE), "try-error"))
ajuste <- .fid_bg_ajustar(r)
stopifnot(ajuste$Convergencia == 0, all(is.finite(ajuste$parametros)))
a <- .fid_analizar_modelos(f, as.Date("2026-01-01"), meses = 3, descuento = 0.12)
stopifnot(nrow(a$clientes) == 30, all(is.finite(a$clientes$CLVMargen)))
contactos <- data.frame(CodContacto = c("A", "B", "C"), PerCod = 1:3, PerRazSoc = c("Uno", "Dos", "Tres"))
potencial <- data.frame(CodContacto = c("A", "B"), ConsumoEsperado = c(1, 1), UnidadConsumo = c("TON/MES", "SACOS/MES"))
ventas <- data.frame(CodContacto = "A", FecFact = as.Date("2026-01-15"),
  SacosFact = 10, Margen = 100, EstadoFinanciero = "CERRADO")
x <- .pot_comparar(potencial, contactos, ventas, as.Date("2026-01-01"), kg_saco = 62.5)
stopifnot(x$PotencialKgMes[1] == 1000, x$KgFacturados[1] == 700, x$Penetracion[1] == 0.7,
  x$PotencialKgMes[2] == 62.5, is.na(x$PotencialKgMes[3]), x$KgFacturados[3] == 0)
cat("OK: RFM diario, BG/NBD, integral independiente, CLV, potencial, unidades y límites mensuales.\n")
# Contraste con la expresión hipergeométrica publicada para BG/NBD (serie convergente, a > 1).
h <- 10; fila <- r[1, ]; p <- .fid_bg_predecir(fila, m, h)
a <- 0.8 + fila$Frecuencia; b <- 3 + fila$Frecuencia
c <- 1.5 + 3 + fila$Frecuencia - 1; z <- h / (10 + fila$T + h)
termino <- 1; serie <- 1
for (n in 1:1000) {
  termino <- termino * (a + n - 1) * (b + n - 1) / ((c + n - 1) * n) * z
  serie <- serie + termino
  if (abs(termino) < 1e-13) break
}
referencia <- (1.5 + 3 + fila$Frecuencia - 1) / (1.5 - 1) *
  (1 - ((10 + fila$T) / (10 + fila$T + h))^a * serie) * p$ProbActivo
stopifnot(abs(referencia - p$ComprasEsperadas) < 1e-6)
cat("OK: predicción contrastada con expresión hipergeométrica BG/NBD.\n")

# Las devoluciones no crean recurrencia, pero sí reducen el margen y los sacos netos de RFM/CLV.
devolucion <- f[1, ] %>% mutate(FecFact = as.Date("2025-12-01"), SacosFact = -1, Margen = -50)
neto <- .fid_rfm(bind_rows(f, devolucion), as.Date("2026-01-01"))
stopifnot(neto$Compras[1] == 4, neto$Margen[1] == 350, neto$Sacos[1] == 3)

m2 <- list(parametros = c(r = 0.8, alpha = 10, a = 0.4, b = 0.3))
stopifnot(all(is.finite(.fid_bg_predecir(r, m2, 20)$ComprasEsperadas)))
