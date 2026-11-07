# Cierre financiero y comparación comercial ----
library(shiny)
sys.source("APP/misc/00_presentacion.R", envir = environment())
.noviembre <- as.Date("2026-11-07")
for (.cierre in as.Date(c("2026-10-01", "2026-11-01", "2026-12-01"))) {
  .cal <- .crm_calendario_cierre(as.Date(.cierre, origin = "1970-01-01"), 2026, .noviembre)
  stopifnot(.cal$mes_actual == 10L, .cal$meses_rest == 2L)
}
stopifnot(.crm_calendario_cierre(as.Date("2026-09-01"), 2026, .noviembre)$meses_rest == 3L,
  .crm_calendario_cierre(as.Date("2025-12-01"), 2025, .noviembre)$meses_rest == 0L,
  .crm_calendario_cierre(as.Date("2026-10-01"), 2027, .noviembre)$meses_rest == 12L,
  .crm_calendario_cierre(as.Date(NA), 2026, .noviembre)$meses_rest == 12L)
stopifnot(identical(.crm_estado_segmento(c("GRANDE", "MEDIANO", NA, "GRANDE"),
  c("GRANDE", "GRANDE", "PEQUEÑO", NA)),
  c("Coincide", "Segmento diferente", "Sin segmento en PyG", "Sin segmento en CRM")))
stopifnot(!grepl("<i", as.character(.crm_titulo_modal("Cuadre mensual", "circle")), fixed = TRUE))
cat("OK: noviembre abierto, cierre financiero, segmentos PyG/CRM y títulos sin iconos.\n")
