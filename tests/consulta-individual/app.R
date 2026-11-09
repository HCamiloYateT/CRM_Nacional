# Recorrido de la consulta con fuentes ficticias y las mismas dependencias UI ----
library(shiny)
library(dplyr)
root <- normalizePath("../..")
contexto <- getOption("crm.test.context", new.env(parent = globalenv()))
for (ruta in c("compartido/00_presentacion.R", "compartido/01_analitica_comercial.R", "compartido/04_interaccion.R",
  "compartido/05_componentes_forms.R", "datos/02_fuentes_comerciales.R",
  "modules/gestion_comercial/oportunidades/01_modulo.R", "modules/gestion_comercial/pendientes_gestion/01_modulo.R",
  "modules/gestion_comercial/fidelidad_riesgo/00_modelos.R", "modules/gestion_comercial/cartera/00_datos.R",
  "modules/encabezado/02_notificaciones_tareas.R", "modules/consulta_individual/00_datos.R",
  "modules/consulta_individual/01_modulo.R", "modules/productos/compartido/00_funciones.R",
  "modules/productos/market_basket/01_modulo.R", "modules/prospeccion/compartido/00_estilo.R")) {
  sys.source(file.path(root, "APP/misc", ruta), contexto)
}
sys.source(file.path(root, "tests/fixtures/consulta_individual.R"), contexto)
sys.source(file.path(root, "tests/fixtures/cartera.R"), contexto)
shiny::addResourcePath("individual-assets", file.path(root, "APP/www"))
ui <- with(contexto, shiny::tagList(racafeForms::racafeFormsDependencies(),
  tags$head(tags$link(rel = "stylesheet", href = "individual-assets/crm-presentacion.css"),
    tags$script(src = "individual-assets/crm-presentacion.js")), .crm_loader_ui(show_logo = FALSE),
  racafeForms::racafeFormRow(columns = 3,
    racafeForms::racafeInputDate("fecha", "Fecha final del controlbar (prueba)", value = as.Date("2026-11-09")),
    racafeForms::racafeInputSelect("asesor", "Asesores", choices = c("ANA", "LUIS"),
      selected = "ANA", multiple = TRUE),
    racafeForms::racafeInputSwitch("fallar", "Simular fuente opcional caída", value = FALSE)),
  .crm_tabs(tabPanel("Consulta", ConsultaIndividualUI("consulta")),
    tabPanel("Basket", MarketBasketUI("basket")),
    tabPanel("Cajas de prospección", .crm_caja_output("pr_numero"), .crm_caja_output("pr_html"))),
  tags$pre(textOutput("alcance"))))
ui <- bs4Dash::dashboardPage(header = bs4Dash::dashboardHeader(title = "Consulta · Prueba"),
  sidebar = bs4Dash::dashboardSidebar(disable = TRUE), body = bs4Dash::dashboardBody(ui))
server <- function(input, output, session) {
  local <- new.env(parent = contexto)
  local$input <- input; local$output <- output; local$session <- session
  eval(quote({
    .crm_iniciar_loader(input)
    f <- reactive({
      x <- filtros_individual; x$asesor <- input$asesor
      x$fecha <- c(as.Date("2026-01-01"), as.Date(input$fecha)); x
    })
    cargar <- function(tabla) {
      if (tabla == "CRMNALCONTACTO") return(contactos_individual)
      if (tabla == "CRMNALHISTORIALETAPA") return(etapas_individual)
      if (tabla == "CRMNALVINCULONIT") return(vinculos_individual)
      if (tabla == "CRMNALPOTENCIAL") {
        if (isTRUE(input$fallar)) stop("Fuente temporalmente fuera de servicio")
        return(data.frame(CodContacto = "C2", ConsumoEsperado = 100, UnidadConsumo = "KG/MES"))
      }
      if (tabla == "CRMNALDIRECTORIOTELEFONO") return(data.frame(CodContacto = c("C1", "C3"),
        Telefono = c("111111", "999999")))
      if (tabla == "CRMNALCLOPT") return(data.frame(CodContacto = "C2", Producto = "B", SacosOP = 5))
      data.frame(CodContacto = character())
    }
    cartera <- fuente_prueba
    cartera$cartera$PerCod <- c(101, 102, 303, 303)
    mapa <- data.frame(PerCod = c(101, 102, 303), CliNitPpal = c(101, 101, 303))
    consulta <- ConsultaIndividual("consulta", reactive(hechos_individual), reactive(clientes_individual), f,
      reactive("ANA"), reactive(FALSE), mapa, reactive(cartera), cargar = cargar)
    output$alcance <- renderText({
      x <- consulta$ejecucion()
      paste(nrow(x), sum(x$SacosFact), sep = " | ")
    })
    MarketBasket("basket", reactive(hechos_individual), f)
    .pr_caja("pr_numero", reactive(0.75), formato = "porcentaje", texto = "Conversión", mostrar_boton = FALSE)
    .pr_caja("pr_html", reactive(tagList(tags$span("12"), tags$small(" (50%)"))),
      texto = "Entidades", mostrar_boton = FALSE, footer = "Nota fuera de la caja.")
  }), local)
}
shinyApp(ui, server)
