# CLIENTES Y TABLAS CON DATOS FICTICIOS ----
library(shiny)
library(bs4Dash)
library(dplyr)
root <- normalizePath("../..")
source(file.path(root, "tests/helpers.R"))
widgets <- .crm_ui_env(root)
clients <- data.frame(CLCliNit = c("900123", "900124"), CliNitPpal = c("900123", "900124"), LinNegCod = "A",
  PerRazSoc = c("CLIENTE CAFETERO A", "CLIENTE CAFETERO B"), CLLinNegNo = "A LA MEDIDA")
operations <- data.frame(CLCliNit = c(rep("900123", 2), rep("900124", 5)), LinNegCod = "A",
  CLPdcCod = 46000 + seq_len(7), CLPdcLin = 1, CLLotCod = 2700000 + seq_len(7),
  ProductoComercial = "CUNDINAMARCA EXCELSO CAFÉ & PRODUCTO DE NOMBRE EXTENSO", LinProCod = "P1",
  SacLote = 1, SacProd = 1, SacDesp = 1, SacosFact = 1, SacosPedido = 1,
  PdcFecCre = as.Date("2026-10-20"), FecProd = as.Date("2026-10-22"), FecDesp = as.Date("2026-10-23"),
  FecFact = as.Date("2026-10-24"), FuenteFactura = "FACTURACIÓN")
history <- data.frame(CLCliNit = "900123", LinNegCod = "A", FecProceso = as.Date(c("2024-01-01", "2025-01-01")),
  CliNitPpal = "900123", Asesor = "ANA", Segmento = "PEQUEÑO", Excluir = "NO", NumMesesRecuperar = 3)
table_data <- data.frame(Producto = c("Café", "Cacao", "Total"), Cantidad = c(2, 3, 5),
  Total = c(1234.56, 200, 1434.56))
ui <- bs4DashPage(title = "Detalle y Vancouver", header = bs4DashNavbar(title = "Prueba de clientes",
  rightUi = tagList(racafeModulos::MenuHeaderUI("MenuClientes", icon("users"), min_width = "390px",
    keep_open_on_select = TRUE), racafeModulos::MenuHeaderUI("MenuClientesAnt", icon("user-clock"),
    min_width = "390px", keep_open_on_select = TRUE))), sidebar = bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    racafeForms::racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE),
    h3("Tablas nuevas y existentes, sin operaciones de negocio"),
    gt::gt_output("default_table"), gt::gt_output("custom_table"),
    gt::gt_output("existing_table"), gt::gt_output("plain_gt")))
ui <- tagList(htmltools::suppressDependencies("googlefonts", "ionicons"), ui)
server <- function(input, output, session) {
  identity <- reactiveVal(NULL)
  summary <- reactive(widgets$.header_resumen_cliente(identity(), clients, history,
    por_clinit = TRUE, operaciones = operations))
  widgets$.header_kpis_server(summary)
  output$resumen_cli_modal <- renderUI(summary())
  output$resumen_cli_ant_modal <- renderUI(widgets$.header_historial_comercial(identity(), history))
  racafeModulos::MenuHeaderServer("MenuClientes", items_r = reactive(widgets$.header_items_clientes(
    clients, "nuevo", "header_cliente_nuevo")))
  old <- clients[1, ]; old$UltActualizacion <- "01/01/2025"
  racafeModulos::MenuHeaderServer("MenuClientesAnt", items_r = reactive(widgets$.header_items_clientes(
    old, "antiguo", "header_cliente_antiguo")))
  .open <- function(selection, old = FALSE) {
    fila <- clients[clients$CLCliNit == selection$nit, ]
    identity(list(nit = selection$nit, linneg_cod = selection$linneg))
    showModal(widgets$.header_cliente_modal_ui(fila$PerRazSoc, fila, identity(), old))
  }
  observeEvent(input$header_cliente_nuevo, .open(input$header_cliente_nuevo))
  observeEvent(input$header_cliente_antiguo, .open(input$header_cliente_antiguo, TRUE))
  observeEvent(input$header_cliente_cerrar, removeModal())
  output$default_table <- gt::render_gt(racafeShiny::racafeTableVancouver(table_data))
  output$custom_table <- gt::render_gt(racafeShiny::racafeTableVancouver(table_data,
    header_color = "#245A81", header_width = 3, total_background = "#EEF0F2", row_padding = 9,
    align = c(Cantidad = "center"), labels = c(Producto = "Artículo")))
  output$existing_table <- gt::render_gt(racafeShiny::racafeCompactTable(table_data))
  output$plain_gt <- gt::render_gt(gt::gt(table_data))
}
shinyApp(ui, server)
