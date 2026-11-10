library(dplyr)
source("APP/misc/compartido/00_presentacion.R")
source("APP/misc/compartido/05_componentes_forms.R")
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/datos/02_fuentes_comerciales.R")
source("APP/misc/modules/gestion_comercial/oportunidades/01_modulo.R")
source("APP/misc/modules/consulta_individual/00_datos.R")
source("APP/misc/modules/consulta_individual/02_perfil_analitico.R")
source("APP/misc/modules/consulta_individual/04_ficha_presentacion.R")
source("APP/misc/modules/consulta_individual/05_estado_comercial.R")
source("APP/misc/modules/cohortes_clientes/estados/00_datos.R")
source("tests/fixtures/consulta_individual.R")
source("tests/fixtures/consulta360.R")
corte <- as.Date("2026-11-10")
metas <- .com_clientes(clientes_individual, corte, filtros_individual, anual = TRUE)
cat_clientes <- .com_clientes(clientes_individual, corte, filtros_individual)
hechos <- .gc_filtrar_dimensiones(hechos360, filtros_individual)
catalogo <- .in_catalogo(contactos_individual, cat_clientes, etapas_individual, vinculos_individual,
  "ANA", FALSE, filtros_individual, hechos, directorio360)
hija <- .in_elegida(catalogo, "nit:102")
x <- .in_clasificacion_cliente(estados_ficha, .in_hechos(hechos, hija), metas, hija, corte)
stopifnot(x$datos$Estado == "ACTIVO", x$datos$CliNitPpal == "101", x$mes == as.Date("2026-11-01"))
for (nit in c(500, 501)) {
  entidad <- .in_elegida(catalogo, paste0("nit:", nit))
  r <- .in_clasificacion_cliente(estados_ficha, .in_hechos(hechos, entidad), metas, entidad, corte)
  stopifnot(r$datos$Estado == if (nit == 500) "INACTIVO" else "NUEVO")
}
# La ausencia del mes no implica NUEVO ni permite arrastrar el estado anterior.
antiguos <- estados_ficha[estados_ficha$FecProceso < as.Date("2026-11-01"), ]
sin_mes <- .in_clasificacion_cliente(antiguos, .in_hechos(hechos, hija), metas, hija, corte)
stopifnot(all(is.na(sin_mes$datos$Estado)))
contradictorios <- bind_rows(estados_ficha, transform(estados_ficha[1, ], EstadoPanel = "INACTIVO"))
revision <- .in_clasificacion_cliente(contradictorios, .in_hechos(hechos, hija), metas, hija, corte)
stopifnot(revision$datos$Estado == "REVISAR")
# Un estado futuro y otra línea/unidad no sustituyen el snapshot visible del mes.
futuros <- bind_rows(estados_ficha,
  transform(estados_ficha[1, ], FecProceso = as.Date("2026-11-30"), EstadoPanel = "INACTIVO"),
  transform(estados_ficha[1, ], LinNegCod = 10000, EstadoPanel = "INACTIVO"))
actual <- .in_clasificacion_cliente(futuros, .in_hechos(hechos, hija), metas, hija, corte)
stopifnot(nrow(actual$datos) == 1L, actual$datos$Estado == "ACTIVO")
stopifnot(is.null(.in_clasificacion_badges(NULL, hija)))
html <- as.character(.in_clasificacion_badges(x, hija))
stopifnot(grepl("Estado comercial", html), grepl("noviembre 2026", html), grepl("Cuenta principal 101", html))
cat("OK: estados del mes de Cohortes, hijas por unidad principal, ausencia, conflictos, futuro y alcance.\n")

# La historia de otra cuenta padre anterior no añade una segunda clasificación a la hija actual.
otro_padre <- bind_rows(.in_hechos(hechos, hija), transform(.in_hechos(hechos, hija), CliNitPpal = 999))
panel_otro <- bind_rows(estados_ficha, transform(estados_ficha[1, ], CliNitPpal = 999, EstadoPanel = "INACTIVO"))
actual <- .in_clasificacion_cliente(panel_otro, otro_padre, metas, hija, corte)
stopifnot(nrow(actual$datos) == 1L, actual$datos$CliNitPpal == "101", actual$datos$Estado == "ACTIVO")
