# Vista aislada con datos ficticios; no consulta servicios del negocio ----
library(shiny)
library(dplyr)
library(lubridate)
source("APP/misc/compartido/00_presentacion.R")
source("APP/misc/modules/cohortes_clientes/estados/00_datos.R")
source("APP/misc/modules/cohortes_clientes/estados/01_componentes.R")
source("APP/misc/modules/cohortes_clientes/estados/02_modulo.R")

meses <- as.Date(paste0(format(Sys.Date(), "%Y"), c("-01-01", "-02-01", "-03-01")))
segmentos <- tidyr::crossing(CliNitPpal = 1:4, FecProceso = meses) %>% mutate(LinNegCod = 21000,
  SegmentoRacafe = case_when(CliNitPpal == 1 ~ "CLIENTE",
    CliNitPpal == 2 & FecProceso == meses[1] ~ "CLIENTE",
    CliNitPpal == 3 & FecProceso == meses[3] ~ "CLIENTE", TRUE ~ "CLIENTE A RECUPERAR"))
clientes <- tibble(CliNitPpal = 1:5, LinNegCod = 21000, FecProceso = meses[1], Asesor = "Ana",
  Segmento = "DETAL", NumMesesRecuperar = 4, SSPpto = c(120, 120, 0, 120, 0), MNFCCPpto = 12000, Excluir = "NO")
hechos <- tidyr::crossing(CliNitPpal = 1:5, FecFact = meses) %>% mutate(LinNegCod = 21000,
  PerRazSoc = paste("Cliente de prueba", CliNitPpal), SacosFact = 10, Margen = 100, EstadoFinanciero = "CERRADO")
panel <- .coha_preparar(segmentos, clientes, hechos, hoy = meses[3] + 20)

ui <- shiny::fluidPage(racafeForms::racafeFormsDependencies(),
  shiny::includeCSS("APP/www/crm-presentacion.css"),
  shiny::includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/",
    "Styles/style.css")), CohortesEstadosUI("cohortes"))
server <- function(input, output, session) {
  CohortesEstados("cohortes", bd = reactive(panel), data_t = reactive(hechos))
}
shinyApp(ui, server)
