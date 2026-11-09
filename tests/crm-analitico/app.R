# Recorrido real con carga diferida y datos ficticios, sin conexiones del negocio ----
library(shiny)
library(dplyr)
library(lubridate)
library(bs4Dash)
root <- normalizePath("../..")
contexto <- getOption("crm.test.context")
if (is.null(contexto)) contexto <- new.env(parent = globalenv())
for (ruta in c("compartido/00_presentacion.R", "compartido/04_interaccion.R", "compartido/05_componentes_forms.R",
  "compartido/01_analitica_comercial.R", "datos/02_fuentes_comerciales.R",
  "modules/prospeccion/compartido/00_estilo.R", "modules/gestion_comercial/cartera/00_datos.R",
  "modules/gestion_comercial/cartera/01_modulo.R", "modules/productos/compartido/00_funciones.R",
  "modules/productos/ejecucion/01_modulo.R", "modules/productos/market_basket/01_modulo.R",
  "modules/prospeccion/potencial_mercado/01_modulo.R",
  "modules/gestion_comercial/oportunidades/01_modulo.R",
  "modules/gestion_comercial/fidelidad_riesgo/00_modelos.R",
  "modules/gestion_comercial/fidelidad_riesgo/01_modulo.R",
  "modules/cohortes_clientes/estados/00_datos.R", "modules/cohortes_clientes/estados/01_componentes.R",
  "modules/cohortes_clientes/estados/02_modulo.R")) sys.source(file.path(root, "APP/misc", ruta), contexto)
sys.source(file.path(root, "tests/fixtures/cartera.R"), contexto)
shiny::addResourcePath("crm-analitico-assets", file.path(root, "APP/www"))
contexto$root <- root
contexto$shared_css <- getOption("crm.test.shared_css",
  "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css")
contexto$shared_js <- getOption("crm.test.shared_js",
  "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Scripts/controlbar.js")
ui <- with(contexto, bs4Dash::dashboardPage(
  header = bs4Dash::dashboardHeader(title = "CRM · Prueba", rightUi = shiny::tagList(
    racafeModulos::MenuHeaderUI("MenuA", icon = shiny::icon("users"), title = "Clientes"),
    racafeModulos::MenuHeaderUI("MenuB", icon = shiny::icon("chart-line"), title = "Indicadores"))),
  sidebar = bs4Dash::dashboardSidebar(collapsed = FALSE, expandOnHover = FALSE, bs4Dash::sidebarMenu(id = "menu_principal",
    bs4Dash::menuItem("Inicio", tabName = "inicio", selected = TRUE),
    bs4Dash::menuItem("Cartera", tabName = "cartera"), bs4Dash::menuItem("Productos", tabName = "productos"),
    bs4Dash::menuItem("Fidelidad", tabName = "fidelidad"), bs4Dash::menuItem("Basket", tabName = "basket"),
    bs4Dash::menuItem("Potencial", tabName = "potencial"),
    bs4Dash::menuItem("Cohortes", tabName = "cohortes"))),
  controlbar = bs4Dash::dashboardControlbar(id = "controlbar", pinned = FALSE, overlay = FALSE,
    racafeForms::racafeInputSelect("filtro_prueba", "Filtro de prueba", c("Uno", "Dos")),
    racafeForms::racafeInputDate("fecha_tope", "Fecha final de prueba", value = as.Date("2026-08-15"))),
  body = bs4Dash::dashboardBody(racafeForms::racafeFormsDependencies(), shinyjs::useShinyjs(),
    tags$head(includeCSS(shared_css), includeScript(shared_js),
      tags$link(rel = "stylesheet", href = "crm-analitico-assets/crm-presentacion.css"),
      tags$script(src = "crm-analitico-assets/crm-presentacion.js")), .crm_loader_ui(show_logo = FALSE),
    bs4Dash::tabItems(bs4Dash::tabItem(tabName = "inicio", "Seleccione una sección."),
      bs4Dash::tabItem(tabName = "cartera", uiOutput("ui_cartera")),
      bs4Dash::tabItem(tabName = "productos", uiOutput("ui_productos")),
      bs4Dash::tabItem(tabName = "fidelidad", uiOutput("ui_fidelidad")),
      bs4Dash::tabItem(tabName = "basket", uiOutput("ui_basket")),
      bs4Dash::tabItem(tabName = "potencial", uiOutput("ui_potencial")),
      bs4Dash::tabItem(tabName = "cohortes", uiOutput("ui_cohortes"))))))
server <- function(input, output, session) {
  local <- new.env(parent = contexto)
  local$input <- input; local$output <- output; local$session <- session
  eval(quote({
    .crm_iniciar_loader(input)
    racafeModulos::MenuHeaderServer("MenuA", items_r = racafeForms::racafeInputSelect("campo_header", "Selector del header", c("Uno", "Dos")))
    racafeModulos::MenuHeaderServer("MenuB", items_r = tags$p("Contenido de indicadores"))
    cargados <- reactiveValues()
    # La función exacta del servidor de producción se usa en este arnés, sin copiar su implementación.
    ast <- parse(file.path(root, "APP/server.R"))[[1]][[3]][[3]]
    funcion <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("CargarTabUnaVez")), as.list(ast)[-1])[[1]][[3]]
    modulos_cargados <- cargados
    CargarTabUnaVez <- eval(funcion)
    f <- reactive(list(asesor = c("ANA", "LUIS"), segmento = "MEDIANO",
      linneg = c("A LA MEDIDA", "CONVENCIONALES")))
    hechos <- data.frame(CliNitPpal = rep(1:30, each = 5), LinNegCod = 21000, CLCliNit = rep(1:30, each = 5),
      CLLinNegNo = "A LA MEDIDA", Asesor = "ANA", Segmento = "MEDIANO", PerRazSoc = "Cliente de prueba",
      FecFact = as.Date("2026-01-01") + rep(c(0, 30, 90, 180, 260), 30) + rep(1:30, each = 5),
      SacosFact = 1, Margen = 100, EstadoFinanciero = "CERRADO", Categoria = "Café",
      Producto = rep(c("A", "B", "A", "B", "C"), 30), CLPdcCod = rep(1:30, each = 5), CLLotCod = 1:150)
    cli <- data.frame(CliNitPpal = 1:30, CLCliNit = 1:30, LinNegCod = 21000, Asesor = "ANA", Segmento = "MEDIANO",
      SSPpto = 100, MNFCCPpto = 1000, FecProceso = as.Date("2026-01-01"), PerRazSoc = "Cliente de prueba")
    CargarTabUnaVez("cartera", "ui_cartera", function() CarteraComercialUI("cartera"), function() {
      CarteraComercial("cartera", reactive(fuente_prueba), reactive(clientes_prueba), mapa_prueba,
        f, reactive(TRUE))
    })
    CargarTabUnaVez("productos", "ui_productos", function() ProductosEjecucionUI("productos"), function() {
      ProductosEjecucion("productos", reactive(hechos))
    })
    CargarTabUnaVez("fidelidad", "ui_fidelidad", function() FidelidadRiesgoUI("fidelidad"), function() {
      FidelidadRiesgo("fidelidad", reactive(hechos), reactive(cli), f)
    })
    CargarTabUnaVez("potencial", "ui_potencial", function() PotencialMercadoUI("potencial"), function() {
      PotencialMercado("potencial", reactive("ANA"), reactive(TRUE), reactive(transform(hechos,
        CodContacto = paste0("C", CliNitPpal))), cargar = function(tabla) {
          if (tabla == "CRMNALPOTENCIAL") data.frame(CodContacto = paste0("C", 1:30),
            ConsumoEsperado = 100, UnidadConsumo = "KG/MES") else {
            data.frame(CodContacto = paste0("C", 1:30), PerCod = 1:30, PerRazSoc = paste("Entidad", 1:30), Asesor = "ANA")
          }
        })
    })
    CargarTabUnaVez("cohortes", "ui_cohortes", function() CohortesEstadosUI("cohortes"), function() {
      segmentos <- tidyr::crossing(CliNitPpal = 1:30,
        FecProceso = seq(as.Date("2026-01-01"), as.Date("2026-08-01"), by = "month")) %>%
        mutate(LinNegCod = 21000, SegmentoRacafe = "CLIENTE")
      clientes_coh <- transform(cli, NumMesesRecuperar = 4, Excluir = "NO")
      panel <- reactive(.coha_preparar(segmentos, clientes_coh, hechos, hoy = as.Date(input$fecha_tope)))
      CohortesEstados("cohortes", panel, reactive(hechos), fecha_limite = reactive(as.Date(input$fecha_tope)))
    })
    CargarTabUnaVez("basket", "ui_basket", function() MarketBasketUI("basket"), function() {
      MarketBasket("basket", reactive(hechos), reactive(c(f(), list(categoria = "Café", producto = c("A", "B", "C")))))
    })
  }), local)
}
shinyApp(ui, server)
