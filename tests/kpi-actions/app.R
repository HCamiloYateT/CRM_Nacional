# UI REALES Y SERVIDORES SIMULADOS SIN OPERACIONES BD ----
library(shiny)
library(bs4Dash)
root <- normalizePath("../..")
source(file.path(root, "tests/helpers.R"))
widgets <- .crm_ui_env(root)
for (file in c("01_resumen_ejecutivo.R", "02_validacion_financiera.R")) {
  for (expression in parse(file.path(root, "APP/misc/modules/generalidades", file))) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) && length(expression) == 3L &&
        ((is.call(expression[[3]]) && identical(expression[[3]][[1]], as.name("function"))) ||
          identical(expression[[2]], as.name(".RESUMEN_EJECUTIVO_SECCIONES")))) eval(expression, widgets)
  }
}
sections <- widgets$.RESUMEN_EJECUTIVO_SECCIONES
cards <- unlist(lapply(sections, `[[`, "cajas"), recursive = FALSE)
tabs <- unique(vapply(cards, `[[`, character(1), "tab"))
financial_tab <- cards[[1]]$tab
financial_ids <- c("kpi_cuadre", "kpi_lotes", "kpi_fact_sin_pyg", "kpi_pyg_sin_fact", "kpi_pyg_sin_lote",
  "kpi_linea", "kpi_segmentos", "kpi_calidad")
ui <- bs4DashPage(title = "Corrección KPI", header = bs4DashNavbar(title = "KPI y acciones"),
  sidebar = bs4DashSidebar(do.call(bs4SidebarMenu, c(list(id = "menu_principal"),
    list(bs4SidebarMenuItem("Resumen", tabName = "resumen_demo", icon = icon("chart-line"))),
    lapply(tabs, function(tab) bs4SidebarMenuItem(tab, tabName = tab, icon = icon("circle")))))),
  body = bs4DashBody(
    racafeForms::racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE),
    do.call(bs4TabItems, c(list(bs4TabItem(tabName = "resumen_demo", widgets$ResumenEjecutivoUI("re"))),
      lapply(tabs, function(tab) bs4TabItem(tabName = tab,
        if (tab == financial_tab) widgets$ValidacionFinancieraUI("fin") else h3(paste("Destino", tab))))))))
ui <- tagList(htmltools::suppressDependencies("googlefonts", "ionicons"), ui)
server <- function(input, output, session) {
  widgets$ResumenEjecutivo("re")
  shiny::moduleServer("fin", function(input, output, session) {
    output$tabla_extraccion_pyg <- gt::render_gt(gt::gt(data.frame(Mes = "ENERO", Sacos = 100)))
    output$auditoria_base <- renderUI(p("Datos ficticios de prueba, sin consultas ni guardados"))
    for (key in financial_ids) local({
      current <- key
      widgets$.crm_caja(current, valor = shiny::reactive(if (current == "kpi_cuadre") 1 else 0),
        texto = current, formato = if (current == "kpi_cuadre") "porcentaje" else "entero",
        contenido_modal = function() p(paste("Detalle financiero simulado", current)),
        titulo_modal = paste("Detalle", current), icono_modal = "circle-info")
    })
  })
}
shinyApp(ui, server)
