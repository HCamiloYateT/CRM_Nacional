# Interfaz real de módulos con fuentes sintéticas; nunca consulta ni escribe en la base de producción.
library(shiny)
library(dplyr)
root <- normalizePath("../..")
contexto <- getOption("crm.test.context")
if (is.null(contexto)) {
  library(racafeForms); library(racafeShiny); library(racafeModulos)
  contexto <- new.env(parent = globalenv())
}
for (ruta in c("compartido/00_presentacion.R", "compartido/04_interaccion.R", "compartido/01_analitica_comercial.R",
  "datos/02_fuentes_comerciales.R", "modules/encabezado/02_notificaciones_tareas.R",
  "modules/controlbar/03_crear_nota.R", "modules/gestion_comercial/notas_tareas/01_modulo.R",
  "modules/gestion_comercial/oportunidades/01_modulo.R",
  "modules/gestion_comercial/pendientes_gestion/01_modulo.R", "modules/herramientas/calculadoras/01_modulo.R")) {
  sys.source(file.path(root, "APP/misc", ruta), contexto)
}
contexto$CrearOportunidad <- function(...) list(creaciones = reactive(0))
contexto$DescartarOportunidad <- function(...) list(n = reactive(0))
contexto$CrearOportunidadUI <- function(id) racafeForms::racafeFormMessage(
  "Esta prueba no habilita la persistencia de oportunidades.", type = "empty")
contexto$DescartarOportunidadUI <- contexto$CrearOportunidadUI
contexto$.TSK_PRIORIDADES <- c("URGENTE", "IMPORTANTE", "MEDIA", "BAJA")
shiny::addResourcePath("crm-test-assets", file.path(root, "APP/www"))
contexto$shared_css <- getOption("crm.test.shared_css",
  "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css")
contexto$root <- root
ui <- with(contexto, fluidPage(theme = bslib::bs_theme(version = 4), shinyjs::useShinyjs(),
  racafeForms::racafeFormsDependencies(),
  tags$head(includeCSS(shared_css),
    tags$link(rel = "stylesheet", href = "crm-test-assets/crm-presentacion.css"),
    tags$script(src = "crm-test-assets/crm-presentacion.js")),
  .crm_loader_ui(show_logo = FALSE),
  tabsetPanel(id = "menu_principal",
    tabPanel("Tareas", value = "GC_Tareas", TableroTareasUI("Tareas")),
    tabPanel("Oportunidades", value = "GC_Oportunidades", OportunidadesGestionUI("Oportunidades")),
    tabPanel("Calculadoras", value = "HE_Calculadoras", CalculadoraUI("Calculadora")),
    tabPanel("Pendientes", value = "GC_Pendientes", PendientesGestionUI("Pendientes")),
    tabPanel("Prueba de cálculo", value = "prueba", racafeForms::racafeInputSelect("caso", "Caso",
      c("Válido", "Sin datos", "Error"), empty = FALSE), plotOutput("grafico"),
      plotly::plotlyOutput("interactivo"), uiOutput("estado")))))
server <- function(input, output, session) {
  local <- new.env(parent = contexto)
  local$input <- input; local$output <- output; local$session <- session
  eval(quote({
    .crm_iniciar_loader(input)
    notas <- reactiveVal(data.frame(cons = 1:3, titulo = c("Tarea Alfa", "Tarea Beta", "Nota Gamma"),
      Descripcion = "Descripción", Actividades = "", Cumplidas = "", Tipo = c("list", "list", "note"),
      Responsable = "Ana", Usuario = "Ana", Asesor = "Ana", Etiquetas = c("Venta", "Cobro", ""),
      Prioridad = c("URGENTE", "MEDIA", "BAJA"), Archivado = 0, Cumplido = 0,
      FechaCumplimiento = as.character(Sys.Date() + 2)))
    refrescar <- function() { Sys.sleep(0.6); notas(isolate(notas())) }
    TableroTareas("Tareas", reactive("Ana"), notas, abrir = function(id) NULL,
      refrescar = refrescar, es_admin = reactive(TRUE))
    contactos <- data.frame(CodContacto = "A", PerCod = "123456789", PerRazSoc = "Cliente de prueba", Asesor = "Ana")
    op <- data.frame(IdOportunidad = c("OP1", "OP2"), CodContacto = "A", LinNegCod = 10000,
      Categoria = "Consumo", Producto = "Café", FechaHoraCrea = as.character(Sys.Date()),
      FechaCumpOP = as.character(Sys.Date() + 30), Sacos = 10, Margen = 200,
      Descartado = c("NO", "SI"), UsuarioCrea = "Ana", FrecuenciaDias = 30)
    hechos <- data.frame(CodContacto = character(), LinNegCod = numeric(), FecFact = as.Date(character()),
      EstadoFinanciero = character(), Producto = character(), Categoria = character(), SacosFact = numeric(),
      Margen = numeric())
    OportunidadesGestion("Oportunidades", reactive("Ana"), reactive(TRUE), reactive(hechos), cargar = function(tabla) {
      Sys.sleep(0.3)
      if (tabla == "CRMNALCLOPT") op else contactos
    })
    maestro <- data.frame(CLPdcCod = c(1, 1, 2), CLPdcLin = 1, CLLotCod = c(10, 10, NA),
      EstadoFinanciero = c("CERRADO", "CERRADO", "PENDIENTE"), SacosPedido70Linea = c(100, 100, 60),
      SacosLote70 = c(40, 40, NA), FacturadosOperativos70 = c(5, 5, 0),
      SacosProducidos70 = c(20, 20, NA), SacosDespachados70 = c(10, 10, NA),
      SacosFact = c(2, 3, NA), Margen = c(20, 30, NA),
      FecFact = as.Date(c("2026-01-02", "2026-01-03", NA)), CliNitPpal = c(100, 100, 200),
      PerRazSoc = "Cliente de prueba", PdcFecEst = Sys.Date() + c(-10, -10, 30),
      FechaCreacionPedido = Sys.Date() - 15, EstadoPedido = "A")
    PendientesGestion("Pendientes", reactive(maestro), reactive("Ana"))
    indicadores <- reactive(data.frame(Item = c("TRM (Hoja de trabajo)", "Precio NYC (HT)",
      "Diferencial de Compra (HT)", "Precio Carga (Promedio de últimas entradas del día)",
      "Precio Consumo (Compras)", "Precio Molidos (Compras)", "Precio Soluble (Compras)"),
      Valor = c(4000, 200, 20, 1800000, 12000, 15000, 16000)))
    clientes <- data.frame(LinNegCod = 10000, CliNitPpal = 1, Segmento = "MEDIANO", FecProceso = Sys.Date(),
      MNFCCPpto = 14000, SSPpto = 1)
    CalculadoraCoproductos("Calculadora-coproductos", indicadores, reactive("Ana"), clientes)
    CalculadoraDolaresExcelso("Calculadora-dolares", indicadores, reactive("Ana"))
    CalculadoraPesosExcelso("Calculadora-pesos", indicadores, reactive("Ana"), margenes = data.frame())
    output$grafico <- .crm_render_plot({
      Sys.sleep(0.7)
      if (identical(input$caso, "Error")) stop("Error sintético verificable")
      if (identical(input$caso, "Sin datos")) req(FALSE)
      plot(1:5, main = "Cálculo válido")
    })
    output$interactivo <- .crm_render_plotly({
      if (identical(input$caso, "Error")) stop("Error sintético Plotly")
      if (identical(input$caso, "Sin datos")) return(plotly::plotly_empty())
      plotly::plot_ly(x = 1:4, y = c(2, 4, 1, 5), type = "bar") %>%
        plotly::layout(title = "Gráfico interactivo de prueba", xaxis = list(title = "Periodo"))
    })
    output$estado <- renderUI(tags$span(input$caso))
  }), local)
}
shinyApp(ui, server)
