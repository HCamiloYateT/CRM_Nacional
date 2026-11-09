# Estados aprobados y adaptación al maestro actual, sin conexiones de negocio ----
library(dplyr)
library(shiny)
library(lubridate)
source("APP/misc/compartido/04_interaccion.R")
source("APP/misc/modules/cohortes_clientes/estados/00_datos.R")
source("APP/misc/modules/cohortes_clientes/estados/02_modulo.R")

# Cuatro trayectorias de transición y una UC nueva que conserva su clasificación todo el año.
meses <- as.Date(c("2026-01-01", "2026-02-01", "2026-03-01"))
seg <- tidyr::crossing(CliNitPpal = 1:4, FecProceso = meses) %>% mutate(LinNegCod = 21000L,
  SegmentoRacafe = case_when(CliNitPpal == 1L ~ "CLIENTE",
    CliNitPpal == 2L & FecProceso == meses[1] ~ "CLIENTE",
    CliNitPpal == 3L & FecProceso == meses[3] ~ "CLIENTE", TRUE ~ "CLIENTE A RECUPERAR"))
seg <- bind_rows(seg, tibble(CliNitPpal = 5L, FecProceso = meses[3], LinNegCod = 21000L,
  SegmentoRacafe = "CLIENTE"))
cli <- tibble(CliNitPpal = 1:6, LinNegCod = 21000L, FecProceso = as.Date("2026-01-01"),
  Asesor = "Ana", Segmento = "DETAL", NumMesesRecuperar = 4L, SSPpto = 120, MNFCCPpto = 12000,
  Excluir = c(rep("NO", 5), "SI"))
hechos <- tidyr::crossing(CliNitPpal = 1:6, FecFact = meses) %>% mutate(LinNegCod = 21000L,
  PerRazSoc = paste("Cliente", CliNitPpal), SacosFact = 10, Margen = 100, EstadoFinanciero = "CERRADO")
# La UC 5 vende sin estado en enero y se convierte en NUEVO para todo el panel.
hechos <- hechos %>% filter(!(CliNitPpal == 5 & FecFact == meses[2]))
panel <- .coha_preparar(seg, cli, hechos, hoy = as.Date("2026-03-20"))
stopifnot(nrow(panel) == 14L, !6L %in% panel$CliNitPpal,
  all(panel$EstadoPanel[panel$CliNitPpal == 5L] == "NUEVO"),
  all(panel$PPtoSacos == 10), all(panel$PPtoMargen == 1000),
  !anyDuplicated(panel[c("LinNegCod", "CliNitPpal", "FecProceso")]))

# El margen abierto permanece desconocido y las proyecciones no son facturación.
abierto <- hechos
abierto$EstadoFinanciero[abierto$FecFact == meses[3]] <- "FACTURADO_ABIERTO"
proyeccion <- hechos[1, ] %>% mutate(SacosFact = 999, Margen = 9999, EstadoFinanciero = "PROYECTADO")
p_abierto <- .coha_preparar(seg, cli, bind_rows(abierto, proyeccion), hoy = as.Date("2026-03-20"))
stopifnot(all(is.na(p_abierto$Margen[p_abierto$FecProceso == meses[3]])),
  sum(p_abierto$Sacos) == sum(panel$Sacos))
det <- .coha_preparar_metricas_cliente(panel, meses[3])
stopifnot(det$Ej_SacosVig[det$CliNitPpal == 1] == 10,
  det$Ej_SacosYTD[det$CliNitPpal == 1] == 30, det$Pt_SacosYTD[det$CliNitPpal == 1] == 30)
stopifnot(is.na(.coha_preparar_metricas_cliente(p_abierto, meses[3])$Ej_MargenVig[1]))

# Última factura por UC sin multiplicación por cambios de razón social.
hist <- hechos[1, ] %>% mutate(FecFact = as.Date("2025-12-15"), PerRazSoc = "Nombre anterior")
p_hist <- .coha_preparar(seg, cli, bind_rows(hechos, hist), hoy = as.Date("2026-03-20"))
stopifnot(nrow(p_hist) == nrow(panel), !any(p_hist$PerRazSoc == "Nombre anterior"),
  nrow(.coha_preparar(seg[0, ], cli[0, ], hechos[0, ], hoy = as.Date("2026-03-20"))) == 0)
conflicto <- bind_rows(seg, seg[1, ] %>% mutate(SegmentoRacafe = "CLIENTE A RECUPERAR"))
stopifnot(inherits(try(.coha_preparar(conflicto, cli, hechos, hoy = as.Date("2026-03-20")), silent = TRUE), "try-error"))

# Aislar solo la presentación para verificar los reactivos originales de todo el módulo.
local({
  env <- new.env(parent = globalenv())
  sys.source("APP/misc/modules/cohortes_clientes/estados/02_modulo.R", env)
  env$.coha_caja <- function(...) invisible(NULL)
  env$.coha_detalle <- function(...) invisible(NULL)
  env$.coha_tabla <- function(...) invisible(NULL)
  env$.crm_nota <- function(...) NULL
  panel_r <- reactiveVal(panel)
  testServer(env$CohortesEstados, args = list(bd = panel_r, data_t = reactive(hechos)), {
    session$setInputs(fecha_ini = "2026-01-01", fecha_fin = "2026-03-01", fil_ppto = c("con", "sin", "nd"),
      sel_tipo_matriz = "pct_fila")
    ind <- indicadores_trans()
    stopifnot(ind$aa == 1, ind$ai == 1, ind$ia == 1, ind$ii == 1,
      ind$retencion == 0.5, ind$churn == 0.5, ind$reactivacion == 0.5,
      nrow(trans_ucs()) == 4L, sum(resumen_mensual()$`marzo 2026`[1:3]) == 5)
    mensual <- indicadores_mensual()
    stopifnot(mensual$n_transiciones == 8L, mensual$aa == 2, mensual$ai == 1,
      mensual$ia == 1, mensual$ii == 4)
    session$setInputs(fil_ppto = "sin")
    stopifnot(nrow(bd_f()) == 0L, nrow(alertas_inactivar()) == 0L, n_en_riesgo() == 0L)
    session$setInputs(fil_ppto = character())
    stopifnot(nrow(bd_f()) == 0L)
    session$setInputs(fil_ppto = "con")
    stopifnot(nrow(bd_f()) == nrow(panel))

    # Sin activos, ifelse devolvía logical(0) y months() detenía las alertas.
    panel_r(panel %>% mutate(EstadoPanel = "INACTIVO"))
    stopifnot(nrow(alertas_inactivar()) == 0L, n_en_riesgo() == 0L)

    # El umbral faltante conserva los cuatro meses, incluso si la columna NA es lógica.
    panel_r(panel %>% mutate(EstadoPanel = "ACTIVO", NumMesesRecuperar = NA,
      UltFecFact = as.Date("2025-11-30")))
    stopifnot(n_en_riesgo() == 5L)

    # Un umbral explícito sigue sumando meses calendario y ajusta el fin de mes.
    panel_r(panel %>% mutate(EstadoPanel = "ACTIVO", NumMesesRecuperar = 1L,
      UltFecFact = as.Date("2026-01-31")))
    session$setInputs(fecha_fin = "2026-02-01")
    stopifnot(n_en_riesgo() == 4L)
  })
})
cat("OK: estados aprobados, presupuesto, transiciones, NUEVO anual, filtros y margen abierto.\n")

