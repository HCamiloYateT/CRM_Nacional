# DEMOSTRACION DE UI SIN SERVIDORES NI BASES DE DATOS DEL CRM ----
library(shiny)
library(bs4Dash)
root <- normalizePath("../..")
source(file.path(root, "tests/helpers.R"))
widgets <- .crm_ui_env(root)
sys.source(file.path(root, "APP/misc/ui/header.R"), widgets)
productos <- list(list(producto = "Café <script>texto</script>", presentacion = "Kg", empaque = "Bolsa",
  cantidad = 12.5, precio = 1234, total = 15425))
ui <- bs4DashPage(title = "Prueba UI CRM", header = widgets$header,
  sidebar = bs4DashSidebar(disable = TRUE),
  controlbar = bs4DashControlbar(id = "controlbar", skin = "light", pinned = FALSE, overlay = FALSE, width = "450px",
    controlbarMenu(id = "test_tabs", type = "tabs",
      controlbarItem("Filtros", widgets$FiltrosUI("Filtros")),
      controlbarItem("Cotizador", widgets$CotizacionUI("Cotizador")),
      controlbarItem("Notas", widgets$CrearNotasUI("CrearNota")),
      controlbarItem("Competencia", widgets$RegistrarCompetenciaUI("RegistrarCompetencia")))),
  body = bs4DashBody(
    racafeForms::racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE),
    h3("Prueba aislada: no se ejecutan operaciones del CRM"),
    racafeModulos::cajaValor_ui("valor", texto = "Ventas", mostrar_boton = TRUE),
    racafeShiny::cajaValor_ui("legacy", texto = "Alias compatible", mostrar_boton = TRUE),
    numericInput("cantidad", "Valor de prueba", 1234.56),
    widgets$.cot_tabla_resumen_ui(productos, 15425),
    verbatimTextOutput("state")))
ui <- shiny::tagList(htmltools::suppressDependencies("googlefonts", "ionicons"), ui)
server <- function(input, output, session) {
  output$user <- renderText("Prueba local")
  valor <- racafeModulos::cajaValor_server("valor", reactive(input$cantidad), "numero")
  legacy <- racafeShiny::cajaValor_server("legacy", reactive(input$cantidad), "numero")
  racafeModulos::MenuHeaderServer("MenuClientes", items_r = reactive(widgets$.header_items_clientes(
    data.frame(CLCliNit = "900123", CLLinNegNo = "Nacional", LinNegCod = "A", PerRazSoc = "Café & Cía."),
    "nuevo", "header_cliente_nuevo")), headerText = "Clientes")
  observeEvent(input$header_cliente_nuevo, showModal(racafeForms::racafeModalDetail("Cliente de prueba",
    racafeForms::racafeDataBadge("NIT", input$header_cliente_nuevo$nit),
    racafeForms::racafeChecklist("activities", c(review = "Revisar", confirm = "Confirmar"), checked = "confirm"))))
  output$state <- renderPrint(list(detalle = valor$click_detalle(), legacy = legacy$click_detalle(),
    titulo = input$`CrearNota-NOT_Titulo`, actividades = input$activities))
}
shinyApp(ui, server)
