# Interfaz de cartera con datos ficticios; no habilita escrituras ni consultas externas ----
library(shiny)
library(dplyr)
root <- normalizePath("../..")
contexto <- getOption("crm.test.context")
if (is.null(contexto)) contexto <- new.env(parent = globalenv())
for (ruta in c("compartido/00_presentacion.R", "compartido/04_interaccion.R", "compartido/01_analitica_comercial.R",
  "datos/02_fuentes_comerciales.R", "modules/gestion_comercial/cartera/00_datos.R",
  "modules/gestion_comercial/cartera/01_modulo.R")) sys.source(file.path(root, "APP/misc", ruta), contexto)
sys.source(file.path(root, "tests/fixtures/cartera.R"), contexto)
shiny::addResourcePath("crm-cartera-assets", file.path(root, "APP/www"))
contexto$shared_css <- getOption("crm.test.shared_css",
  "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css")
ui <- with(contexto, fluidPage(theme = bslib::bs_theme(version = 4), racafeForms::racafeFormsDependencies(),
  tags$head(includeCSS(shared_css), tags$link(rel = "stylesheet", href = "crm-cartera-assets/crm-presentacion.css"),
    tags$script(src = "crm-cartera-assets/crm-presentacion.js")), .crm_loader_ui(show_logo = FALSE),
  racafeForms::racafeInputSelect("alcance", "Alcance de prueba",
    c("Administrador", "Propio", "Sin coincidencias"), empty = FALSE),
  racafeForms::racafeInputSelect("modo_fuente", "Fuente de prueba", c("Completa", "Vacía", "Ausente"),
    empty = FALSE), CarteraComercialUI("cartera")))
server <- function(input, output, session) {
  local <- new.env(parent = contexto)
  local$input <- input; local$output <- output; local$session <- session
  eval(quote({
    .crm_iniciar_loader(input)
    filtro <- reactive(list(asesor = if (input$alcance == "Propio") "ANA" else c("ANA", "LUIS"),
      segmento = if (input$alcance == "Sin coincidencias") "INEXISTENTE" else "MEDIANO",
      linneg = c("A LA MEDIDA", "CONVENCIONALES")))
    fuente <- reactive({
      if (input$modo_fuente == "Ausente") return(NULL)
      h <- fuente_prueba
      if (input$modo_fuente == "Vacía") h$cartera <- h$cartera[0, ]
      h
    })
    CarteraComercial("cartera", fuente, reactive(clientes_prueba), mapa_prueba,
      filtro, reactive(input$alcance == "Administrador"))
  }), local)
}
shinyApp(ui, server)
