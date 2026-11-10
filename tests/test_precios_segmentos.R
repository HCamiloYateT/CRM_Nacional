source("tests/test_consulta360.R")
source("APP/misc/compartido/05_componentes_forms.R")
source("APP/misc/modules/consulta_individual/03_precios_pedidos.R")
source("APP/misc/modules/generalidades/validacion_financiera/00_segmentos.R")
source("APP/misc/modules/generalidades/indicadores_mercado/00_constantes.R")
x <- .in_facturacion_historica(.in_hechos(hechos360, hija), corte)
p <- .in_precios_pedidos(x)
stopifnot(all(p$NIT == "102"), sum(p$Sacos) == sum(x$SacosFact), all(p$Marca == "Marca sede"))
stopifnot(nrow(.in_precio_cercano(p, 12001)) == nrow(p), !nrow(.in_precio_cercano(p, 0)))
p$PrecioKg[[1]] <- NA_real_
stopifnot(nrow(.in_precio_cercano(p, 12001)) == nrow(p) - 1L)
indicadores <- list(datos = data.frame(FechaActualizacion = as.Date(c("2024-01-01", "2024-01-02")),
  TRM = c(4000, 4100)))
stopifnot(.in_mercado_fecha(indicadores, as.Date("2024-01-01"))$Valor == 4000)
stopifnot(!nrow(.in_mercado_fecha(indicadores, as.Date("2024-01-03"))))
snap <- data.frame(CliNitPpal = c(1, 1, 2, 1), LinNegCod = 10000, ANHO = c(2025, 2026, 2025, 2026),
  FecFactPYG = as.Date(c("2025-12-01", "2026-01-01", "2025-12-01", "2026-10-01")),
  FechaCorteArchivo = as.Date(c("2025-12-01", "2026-10-01", "2025-12-01", "2026-10-01")),
  SegmentoPYG = c("ANTIGUO", "PEQUEÑO", "ANTIGUO", NA_character_))
s <- .vf_segmentos_recientes(snap)
stopifnot(nrow(s) == 1L, is.na(s$SegmentoPYG), s$FechaSegmentoPYG == as.Date("2026-10-01"))
snap$FechaCorteArchivo <- as.Date(NA)
s <- .vf_segmentos_recientes(snap)
stopifnot(nrow(s) == 1L, is.na(s$SegmentoPYG))
ch <- .crm_choices(c("CONVENCIONALES", "A LA MEDIDA"))
stopifnot(identical(unname(ch), c("CONVENCIONALES", "A LA MEDIDA")), names(ch)[[1]] == "Coproductos")
cat("OK: precios sin inventar, NIT hija, indicadores del día exacto, segmentos recientes y labels.\n")

# Ejecutar exclusivamente el enriquecimiento puro del ETL con cabeceras repetidas/contradictorias.
expresiones <- parse("DataPrep.R")
pos <- which(vapply(expresiones, function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
  identical(x[[2]], as.name(".precios_pedido")), logical(1)))
etl <- new.env(parent = globalenv())
etl$ped <- data.frame(PdcCod = c(1, 1, 2, 2), CLCliNit = 102, PdcPrecioKilo = c(12000, 12000, 8000, 9000),
  PdcFecCre = as.Date("2024-01-01"))
etl$data_pyg <- data.frame(CLPdcCod = c(1, 1, 2), CLCliNit = 102, PdcPrecioKilo = NA_real_,
  PdcFecCre = as.Date(NA), SacosFact = c(10, 20, 30))
eval(expresiones[[pos]], etl); eval(expresiones[[pos + 1L]], etl)
stopifnot(nrow(etl$data_pyg) == 3L, sum(etl$data_pyg$SacosFact) == 60,
  all(etl$data_pyg$PdcPrecioKilo[1:2] == 12000), is.na(etl$data_pyg$PdcPrecioKilo[[3]]))
cat("OK: ETL conserva filas y cantidades; no elige precios contradictorios.\n")
