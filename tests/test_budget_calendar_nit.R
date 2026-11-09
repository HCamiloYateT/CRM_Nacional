# Calendario vigente, meta y NIT sin formato de importes ----
library(shiny)
sys.source("APP/misc/compartido/00_presentacion.R", envir = environment())
sys.source("APP/misc/compartido/04_interaccion.R", envir = environment())
.cal <- .crm_calendario_presupuesto(as.Date("2026-09-01"), 2026, as.Date("2026-11-07"))
stopifnot(.cal$mes_actual == 9L, .cal$mes_vigente == 11L, .cal$meses_rest == 2L,
  .crm_calendario_presupuesto(as.Date("2025-12-01"), 2025, as.Date("2026-11-07"))$meses_rest == 0L,
  .crm_calendario_presupuesto(as.Date("2026-09-01"), 2027, as.Date("2026-11-07"))$meses_rest == 12L,
  .crm_calendario_presupuesto(as.Date("2026-09-01"), 2026, as.Date("2026-12-01"))$meses_rest == 1L)
.datos_nit <- data.frame(CliNitPpal = c(700004331, 1128270846), PerCod = c(900123456, 901234567),
  Sacos = c(1200, 2400))
.normalizados <- .crm_nits_tabla(.datos_nit, c(PerCod = "NIT"))
stopifnot(identical(.normalizados$CliNitPpal, c("700004331", "1128270846")),
  identical(.normalizados$PerCod, c("900123456", "901234567")), identical(.normalizados$Sacos, .datos_nit$Sacos))
.html_nit <- htmltools::renderTags(.crm_tabla_datos(.datos_nit, col_labels = c(PerCod = "NIT")))$html
stopifnot(grepl(">700004331<", .html_nit, fixed = TRUE),
  grepl(">1128270846<", .html_nit, fixed = TRUE), grepl(">900123456<", .html_nit, fixed = TRUE),
  !grepl("700,004,331", .html_nit, fixed = TRUE))
cat("OK: noviembre y diciembre pendientes; NIT sin miles ni decimales y sacos numéricos.\n")
