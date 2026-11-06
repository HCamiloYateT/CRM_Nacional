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
  clientes <- data.frame(CLCliNit = as.character(900123 + seq_len(18) - 1L),
    CLLinNegNo = rep(c("A LA MEDIDA", "CONVENCIONALES"), 9), LinNegCod = "A",
    PerRazSoc = c("Café & Cía.", rep("COMPAÑÍA CAFETERA DE NOMBRE EXTENSO & ASOCIADOS", 17)))
  racafeModulos::MenuHeaderServer("MenuClientes", items_r = reactive(widgets$.header_items_clientes(
    clientes, "nuevo", "header_cliente_nuevo")), headerText = "Clientes por revisar", href = "#", footerText = "Ver clientes")
  clientes$UltActualizacion <- "01/01/2025"
  racafeModulos::MenuHeaderServer("MenuClientesAnt", items_r = reactive(widgets$.header_items_clientes(
    clientes[1:2, ], "antiguo", "header_cliente_antiguo")), headerText = "Clientes con información anterior")
  tareas <- data.frame(cons = seq_len(12), titulo = rep(c("Revisión de resultados comerciales de la zona cafetera",
    "Crear cotizaciones de clientes"), 6), Tipo = "list", Prioridad = rep(c("URGENTE", "IMPORTANTE"), 6),
    Responsable = "ANA|LUIS|EQUIPO COMERCIAL", FechaCumplimiento = as.Date("2026-11-06"),
    Archivado = 0, Cumplido = rep(c(0, 1), 6), Actividades = "Revisar|Confirmar|Notificar", Cumplidas = "1|0|0")
  racafeModulos::MenuHeaderServer("MenuTareas", items_r = reactive(do.call(racafeShiny::racafeListGroup,
    lapply(seq_len(nrow(tareas)), function(i) widgets$.tareas_lista_item(tareas[i, ], "task_selected")))),
    headerText = "Tareas y notas", href = "#", footerText = "Ver todas las tareas")
  racafeModulos::MenuHeaderServer("MenuIndicadores", items_r = widgets$TablaIndicadoresUI("ind_kpis"))
  shiny::moduleServer("ind_kpis", function(input, output, session) {
    output$tabla_indicadores <- gt::render_gt(racafeShiny::racafeCompactTable(data.frame(
      Indicador = c("TRM (Hoja de trabajo)", "Precio Carga (Promedio de últimas entradas del día)",
        rep("Precio consumo y posición de mercado", 12)), Valor = c("$ 3,228", "$ 2,112,214", rep("$ 22,202", 12))),
      font_size = 12, row_padding = 4, show_header = FALSE, align = c("left", "right")))
    output$tabla_disponible <- gt::render_gt(racafeShiny::racafeCompactTable(
      data.frame(Concepto = c("Sacos", "Precio / Carga"), Valor = c("1,244", "$ 2,141,600")),
      font_size = 12, row_padding = 4, show_header = FALSE, align = c("left", "right")))
    session$onFlushed(function() racafeShiny::racafeUpdateOverlayLoader(session, "indicadores_loader", FALSE), once = TRUE)
  })
  observeEvent(input$header_cliente_nuevo, showModal(racafeForms::racafeModalDetail("Cliente de prueba",
    racafeForms::racafeDataBadge("NIT", input$header_cliente_nuevo$nit),
    racafeForms::racafeChecklist("activities", c(review = "Revisar", confirm = "Confirmar"), checked = "confirm"))))
  output$state <- renderPrint(list(detalle = valor$click_detalle(), legacy = legacy$click_detalle(),
    titulo = input$`CrearNota-NOT_Titulo`, actividades = input$activities, tarea = input$task_selected))
}
shinyApp(ui, server)
