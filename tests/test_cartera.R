# Contratos de cartera y regresiones monetarias, sin servicios de negocio ----
library(dplyr)
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/datos/02_fuentes_comerciales.R")
source("APP/misc/datos/04_cartera.R")
source("APP/misc/modules/gestion_comercial/cartera/00_datos.R")
source("APP/misc/modules/gestion_comercial/cartera/01_modulo.R")
corte <- as.Date("2026-12-15")
x <- data.frame(TCEFch = corte, PerCod = c(11, 11, 22, 11, 33), Factura = c("A", "B", "A", "C", NA),
  LinNegCod = 21000, LinProCod = seq_len(5), TCEPdcRef = 1:5, TCEPdcLin = 1,
  TCEFchVto = corte + c(-2, -10, -20, 20, 0), SaldoCOP = c(100, 200, 300, 400, 0),
  ValorCOP = c(110, 210, 310, 410, 0), TCESac = 1, PerRazSoc = c("Igual", "Igual", "Igual", "Igual", "Sin factura"),
  Negocio = c("A LA MEDIDA", "A LA MEDIDA", "COPRODUCTO", "A LA MEDIDA", "A LA MEDIDA"),
  TGCFchGes = corte + 1, TGCObs = "Posterior")
z <- .car_preparar(bind_rows(x, x[1, ]), corte)
stopifnot(nrow(z) == 5, all(is.na(z$TGCFchGes)), all(is.na(z$TGCObs)))
r <- .car_resumen(z, total = TRUE)
stopifnot(tail(r$SaldoCOP, 1) == 1000, all(r$PctSaldo == c(0.4, 0.6, 1)),
  tail(r$Facturas, 1) == 4, tail(r$Clientes, 1) == 3)
v <- .car_resumen(z[z$Tipo == "Vencida", ], "Rango", total = TRUE)
stopifnot(identical(v$Rango, c("1–7 días", "8–15 días", "16–30 días", "TOTAL")),
  tail(v$Clientes, 1) == 2, sum(head(v$Clientes, -1)) == 3)
stopifnot(nrow(.car_resumen(z[0, ], total = TRUE)) == 0,
  nrow(.car_preparar(x, corte + 1)) == 0)
q <- x[1, ]; q$SaldoCOP <- 999
stopifnot(inherits(try(.car_preparar(bind_rows(x, q), corte), silent = TRUE), "try-error"))
z$Saldo <- 0
stopifnot(all(is.na(.car_resumen(z)$PctSaldo)))

# Un solo acumulado en COP, ordenado a través del cambio de año y semanas ISO.
z <- .car_preparar(x, corte)
z$FechaVence <- as.Date(c("2026-12-27", "2026-12-31", "2027-01-01", "2027-01-04", "2027-01-15"))
z$Tipo <- "Por vencer"
mes <- .car_vencimientos(z); sem <- .car_vencimientos(z, "semana")
stopifnot(nrow(mes) == 2, tail(mes$Acumulado, 1) == 1000, tail(sem$Acumulado, 1) == 1000,
  all(diff(sem$Periodo) >= 0), all(diff(mes$Periodo) > 0))

# NIT, no razón social; el rol no evita los filtros ni amplía el alcance recibido.
clientes <- data.frame(CliNitPpal = c("100", "200"), LinNegCod = c(21000, 10000))
mapa <- data.frame(PerCod = c(11, 22), CliNitPpal = c(100, 200))
stopifnot(nrow(.car_scope(x, clientes[1, ], mapa)) == 3,
  nrow(.car_scope(x, clientes[0, ], mapa, admin = TRUE)) == 0)

# Histórico por fecha de factura: incluir pendientes en consulta, excluirlos del pago realizado.
f <- data.frame(PerCod = c(11, 11, 22, 22, 22), Factura = LETTERS[1:5], FcnPrf = "FE",
  PerRazSoc = "Homónimo", FcnFec = as.Date(c("2025-12-01", rep("2026-01-01", 4))),
  FecPag = as.Date(c("2026-01-02", "2026-01-11", "2026-01-21", "1753-01-01", "2026-01-10")),
  FecVto = as.Date("2026-01-15"), FcnVal = c(100, 100, 300, 500, 100), FcnSal = c(0, 0, 0, 100, 20),
  Sacos = 1, ForPagNom = "Crédito")
p <- .car_pagos(f, as.Date("2026-01-01"), as.Date("2026-12-31"), hoy = as.Date("2026-12-15"))
stopifnot(p$FueraPeriodo[1], sum(p$Pagada & !p$FueraPeriodo) == 2,
  is.na(p$FecPag[4]), p$DiasMoraActual[4] > 0)
b <- .car_comportamiento(p, "Facturas")
stopifnot(nrow(b) == 2, all(.CAR_TRAMOS %in% names(b)), b$Total[1] == 1,
  all(abs(rowSums(b[.CAR_TRAMOS]) - 1) < 1e-10))
rot <- .car_rotacion_tabla(p)
stopifnot(all(rot$Comparacion == "Sin año comparable"), all(is.na(rot$CambioDias)))
q <- f[2, ]; q$FcnFec <- as.Date("2025-01-01"); q$FecPag <- as.Date("2025-01-06"); q$Factura <- "Z"
rot <- .car_rotacion_tabla(.car_pagos(bind_rows(f[-1, ], q), as.Date("2025-01-01"), as.Date("2026-12-31")))
stopifnot(rot$CambioDias[rot$NIT == "11" & rot$Anho == 2026] == 5)
stopifnot(nrow(.car_comportamiento(p[0, ])) == 0)
cat("OK: cortes, granularidad, distintos, permisos, COP, año/semana, pagos, homónimos y rotación.\n")

# ETL: no replicar líneas al unir maestros; convertir USD una sola vez y no inventar fechas de pago.
source("tests/fixtures/cartera.R")
raw <- cartera_prueba[c("TCEFch", "PerCod", "Factura", "LinNegCod", "LinProCod", "TCEPdcRef", "TCEPdcLin",
  "TCEPdcCntC", "TCEFchVto", "TCESac")]
raw$ForPagCod <- 1L; raw$TCESgl <- c("COP", "USD", "COP", "COP")
raw$TCETrm <- 4000; raw$TCETot <- 100; raw$TCESal <- 50
fac <- facturacion_prueba[setdiff(names(facturacion_prueba),
  c("PerRazSoc", "ForPagNom", "FecPag", "FecVto"))]
fac$ForPagCod <- 1L
personas <- distinct(facturacion_prueba[c("PerCod", "PerRazSoc")])
formas <- data.frame(ForPagCod = 1L, ForPagNom = "Crédito", DiasPlazo = 30)
lineas <- data.frame(LinNegCod = c(10000, 21000), LinNegNom = c("CONVENCIONALES", "A LA MEDIDA"))
productos <- data.frame(LinProCod = 1:4, LinProNom = paste0("Producto", 1:4))
gestion <- data.frame(PerCod = 11, Factura = "F1", TGCSec = 1:2,
  TGCFchGes = as.Date(c("2026-10-01", "2026-11-01")), TGCUsuGes = "ANA", TGCObs = c("Anterior", "Última"))
pagos <- facturacion_prueba[c("Factura", "FecPag")]
pedidos <- data.frame(PdcCod = 1:4, Pedido = paste0("P", 1:4))
preparar <- function() .car_preparar_fuentes(raw, fac, personas, formas, lineas, productos, gestion,
  pagos, pedidos, data.frame(FctPlazEdo = 30))
h <- preparar()
stopifnot(nrow(h$cartera) == 4, h$cartera$SaldoCOP[2] == 200000,
  h$cartera$TGCObs[1] == "Última", is.na(h$facturacion$FecPag[4]),
  h$facturacion$FecVto[1] == as.Date("2025-01-31"))
formas <- bind_rows(formas, transform(formas, DiasPlazo = 60))
stopifnot(inherits(try(preparar(), silent = TRUE), "try-error"))
cat("OK: ETL sin multiplicación de importes, última gestión, TRM y fechas no fabricadas.\n")
formas <- formas[1, ]
consultas <- character()
consultar <- function(query) {
  consultas <<- c(consultas, query)
  fuentes <- list(TESHCARE = raw, FCTFACNA = fac, NPERSONA = personas, NFORPAG = formas,
    NLINEANE = lineas, NTIPPROD = productos, TESGESCA = gestion, TESRECPA = pagos,
    EXPPEDID = pedidos, FCTCTBPA = data.frame(FctPlazEdo = 30))
  nombre <- names(fuentes)[vapply(names(fuentes), function(n) grepl(paste("FROM", n), query, fixed = TRUE),
    logical(1))]
  stopifnot(length(nombre) == 1L)
  fuentes[[nombre]]
}
h <- .car_extraer_fuentes(consultar)
stopifnot(length(consultas) == 10L, nrow(h$cartera) == nrow(raw), nrow(h$facturacion) == nrow(fac))
cat("OK: extracción completa con proveedor de consultas simulado y sin conexiones externas.\n")
