# Recorrido de la consulta con fuentes ficticias y las mismas dependencias UI ----
library(shiny)
library(dplyr)
library(shinyjs)
root <- normalizePath("../..")
contexto <- getOption("crm.test.context", new.env(parent = globalenv()))
for (ruta in c("compartido/00_presentacion.R", "compartido/01_analitica_comercial.R", "compartido/04_interaccion.R",
  "compartido/05_componentes_forms.R", "datos/02_fuentes_comerciales.R",
  "modules/gestion_comercial/oportunidades/01_modulo.R", "modules/gestion_comercial/pendientes_gestion/01_modulo.R",
  "modules/gestion_comercial/fidelidad_riesgo/00_modelos.R", "modules/gestion_comercial/cartera/00_datos.R",
  "modules/encabezado/02_notificaciones_tareas.R", "modules/consulta_individual/00_datos.R",
  "modules/consulta_individual/01_modulo.R", "modules/consulta_individual/02_perfil_analitico.R",
  "modules/consulta_individual/03_precios_pedidos.R", "modules/consulta_individual/04_ficha_presentacion.R",
  "modules/generalidades/indicadores_mercado/00_constantes.R",
  "modules/generalidades/indicadores_mercado/00_funciones.R",
  "modules/productos/compartido/00_funciones.R",
  "modules/productos/market_basket/01_modulo.R", "modules/prospeccion/compartido/00_estilo.R",
  "modules/prospeccion/compartido/00_constantes.R", "modules/prospeccion/compartido/00_funciones.R",
  "datos/01_contrato.R", "modules/productos/sin_clasificar/00_datos.R",
  "modules/productos/sin_clasificar/01_modulo.R")) {
  sys.source(file.path(root, "APP/misc", ruta), contexto)
}
sys.source(file.path(root, "tests/fixtures/consulta_individual.R"), contexto)
sys.source(file.path(root, "tests/fixtures/consulta360.R"), contexto)
sys.source(file.path(root, "tests/fixtures/cartera.R"), contexto)
sys.source(file.path(root, "tests/fixtures/ficha.R"), contexto)
shiny::addResourcePath("individual-assets", file.path(root, "APP/www"))
ui <- with(contexto, bs4Dash::dashboardPage(
  header = bs4Dash::dashboardHeader(title = "Ficha · Prueba"),
  sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
  body = bs4Dash::dashboardBody(shiny::tagList(racafeForms::racafeFormsDependencies(),
    racafeForms::racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE),
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", href = "individual-assets/crm-presentacion.css"),
      tags$script(src = "individual-assets/crm-presentacion.js")), .crm_loader_ui(show_logo = FALSE),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputDate("fecha", "Corte", value = as.Date("2026-11-10")),
      .crm_select("asesor", "Asesor", c("ANA", "LUIS"), selected = "ANA", multiple = TRUE),
      .crm_select("linea", "Línea", c("A LA MEDIDA", "CONVENCIONALES"), selected = "A LA MEDIDA", multiple = TRUE)),
    ConsultaIndividualUI("consulta")))))
server <- function(input, output, session) {
  local <- new.env(parent = contexto)
  local$input <- input; local$output <- output; local$session <- session
  eval(quote({
    .crm_iniciar_loader(input)
    f <- reactive({
      x <- filtros_individual; x$asesor <- input$asesor; x$linneg <- input$linea
      x$fecha <- c(as.Date("2026-01-01"), as.Date(input$fecha)); x
    })
    cargar <- function(tabla) {
      if (tabla == "CRMNALCONTACTO") return(contactos_individual)
      if (tabla == "CRMNALHISTORIALETAPA") return(etapas_individual)
      if (tabla == "CRMNALVINCULONIT") return(vinculos_individual)
      if (tabla %in% names(.ficha_fuentes)) return(.ficha_fuentes[[tabla]])
      data.frame(CodContacto = character())
    }
    mapa <- data.frame(PerCod = c(101, 102, 303, .ficha_nits),
      CliNitPpal = c(101, 101, 303, rep(.ficha_nit, length(.ficha_nits))))
    ConsultaIndividual("consulta", reactive(hechos360), reactive(clientes_individual), f,
      reactive("ANA"), reactive(FALSE), mapa, cargar = cargar, directorio = reactive(directorio360))
  }), local)
}
shinyApp(ui, server)
