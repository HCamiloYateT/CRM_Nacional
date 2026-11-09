# Tres cifras reales del resumen, sin porcentajes de ejemplo ----
library(shiny)
for (.archivo in c("APP/misc/modules/gestion_comercial/presupuesto/01_modulo.R",
  "APP/misc/modules/generalidades/resumen_ejecutivo/01_modulo.R")) {
  for (.expresion in parse(.archivo)) {
    if (is.call(.expresion) && identical(.expresion[[1]], as.name("<-"))) eval(.expresion)
  }
}
.totales <- .presupuesto_resumen_total(data.frame(Sacos_Real = c(100, 50, NA), Sacos_Ppto = c(50, 50, 50),
  Margen_Real = c(400, 200, NA), Margen_Ppto = c(100, 100, 100)))
stopifnot(.totales$sacos == 1, .totales$margen == 2,
  is.na(.presupuesto_resumen_total(data.frame(Sacos_Real = 10, Sacos_Ppto = 0,
    Margen_Real = 100, Margen_Ppto = 0))$sacos))
servidor_resumen <- function(input, output, session) {
  metricas <- reactiveVal(list(gc_presupuesto_sacos = 1, gc_presupuesto_margen = 2, ge_validacion = 0.99))
  ResumenEjecutivo("resumen", valores = metricas)
}
testServer(servidor_resumen, {
  session$flushReact()
  inicial <- vapply(c("resumen-gc_presupuesto_sacos-valor_display", "resumen-gc_presupuesto_margen-valor_display",
    "resumen-ge_validacion-valor_display"), function(id) output[[id]]$html, character(1))
  stopifnot(all(nzchar(inicial)), !any(grepl("—", inicial, fixed = TRUE)))
  metricas(list(gc_presupuesto_sacos = 1.25, gc_presupuesto_margen = 0.75, ge_validacion = 0.95))
  session$flushReact()
  actualizado <- vapply(names(inicial), function(id) output[[id]]$html, character(1))
  stopifnot(all(inicial != actualizado))
  metricas(list(gc_presupuesto_sacos = NA_real_, gc_presupuesto_margen = NA_real_, ge_validacion = NA_real_))
  session$flushReact()
  stopifnot(grepl("—", output$`resumen-ge_validacion-valor_display`$html, fixed = TRUE))
})
cat("OK: totales anuales, porcentajes superiores a 100%, actualización reactiva y datos ausentes.\n")
