# La estructura del sidebar debe tener puntos de entrada propios ----
stopifnot(!length(list.files("APP/misc", "[.]R$", full.names = TRUE)))
rutas <- c("generalidades/resumen_ejecutivo", "generalidades/validacion_financiera",
  "generalidades/evolucion_negocio", "generalidades/participacion_composicion", "generalidades/indicadores_mercado",
  "generalidades/inventarios", "gestion_comercial/presupuesto", "gestion_comercial/variables_bono",
  "gestion_comercial/oportunidades", "gestion_comercial/pendientes_gestion", "gestion_comercial/notas_tareas",
  "gestion_comercial/alertas_notificaciones", "gestion_comercial/fidelidad_riesgo", "gestion_comercial/cartera",
  "prospeccion/kanban", "prospeccion/contactos", "prospeccion/prospectos", "prospeccion/leads",
  "prospeccion/clientes", "prospeccion/descartados", "prospeccion/conversion", "prospeccion/potencial_mercado",
  "cohortes_clientes/estados", "cohortes_clientes/retencion",
  "cohortes_clientes/ventas", "cohortes_clientes/margen", "cohortes_clientes/frecuencia",
  "herramientas/calculadoras", "herramientas/tablas_conversion", "herramientas/informes",
  "productos/ejecucion", "productos/market_basket", "consulta_individual", "admon/catalogos")
stopifnot(all(vapply(file.path("APP/misc/modules", rutas), function(p) {
  dir.exists(p) && length(list.files(p, "[.]R$")) > 0L
}, logical(1))))
# Todas las fuentes literales de R apuntan a archivos existentes después de moverlas.
for (ruta in c(list.files("APP", "[.]R$", recursive = TRUE, full.names = TRUE),
  list.files("tests", "[.]R$", recursive = TRUE, full.names = TRUE), "DataPrep.R")) {
  recorrer <- function(x) {
    if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return()
    if (is.call(x) && as.character(x[[1]])[[1]] %in% c("source", "sys.source") &&
        length(x) > 1L && is.character(x[[2]]) && startsWith(x[[2]], "APP/")) stopifnot(file.exists(x[[2]]))
    partes <- if (is.call(x)) as.list(x)[-1L] else as.list(x)
    for (i in seq_along(partes)) if (!identical(partes[[i]], quote(expr = ))) recorrer(partes[[i]])
  }
  recorrer(parse(ruta))
}
cat("OK:", length(rutas), "opciones con carpeta propia, misc sin scripts sueltos y fuentes válidas.\n")
