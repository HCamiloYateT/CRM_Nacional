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
  "modules/consulta_individual/03_precios_pedidos.R",
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
shiny::addResourcePath("individual-assets", file.path(root, "APP/www"))
ui <- with(contexto, shiny::tagList(racafeForms::racafeFormsDependencies(), shinyjs::useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", href = "individual-assets/crm-presentacion.css"),
    tags$script(src = "individual-assets/crm-presentacion.js")), .crm_loader_ui(show_logo = FALSE),
  racafeForms::racafeFormRow(columns = 3,
    racafeForms::racafeInputDate("fecha", "Fecha final del controlbar (prueba)", value = as.Date("2026-11-09")),
    racafeForms::racafeInputSelect("asesor", "Asesores", choices = c("ANA", "LUIS"),
      selected = "ANA", multiple = TRUE),
    racafeForms::racafeInputSwitch("fallar", "Simular fuente opcional caída", value = FALSE)),
  .crm_select("obligatorio", "Ejemplo de campo obligatorio", c("Sí" = "SI", "No" = "NO"), required = TRUE),
  .crm_select("opcional", "Ejemplo de selección pendiente", c("NINGUNO", "A", "B"), selected = ""),
  .crm_tabs(tabPanel("Consulta", ConsultaIndividualUI("consulta")),
    tabPanel("Basket", MarketBasketUI("basket")),
    tabPanel("Clasificación", ProductosSinClasificarUI("clasificacion")),
    tabPanel("Cajas de prospección", .crm_caja_output("pr_numero"), .crm_caja_output("pr_html"))),
  .crm_disclosure("Prueba del menú de detalle",
    tags$div(style = "transform:translateZ(0);position:relative;z-index:1;",
      HTML(.celda_dropdown_acciones("C1", c(Editar = "Editar entidad"), NS("detalle")))),
    tags$p(style = "position:relative;z-index:2;", "Texto que antes cubría los botones del detalle")),
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
      if (tabla == "CRMINDICADORES") return(data.frame(FechaActualizacion = as.Date("2024-03-31"), TRM = 4100))
      if (tabla == "CRMNALCONTACTO") return(contactos_individual)
      if (tabla == "CRMNALHISTORIALETAPA") return(data.frame(CodContacto = "C2",
        FechaHora = as.POSIXct(c("2018-06-01", "2024-01-01", "2025-01-01"), tz = "America/Bogota"),
        EtapaAnterior = c("INICIO", "CONTACTO", "LEAD"),
        EtapaNueva = c("CONTACTO", "LEAD", "CLIENTE"), Usuario = "ANA"))
      if (tabla == "CONTACTOLEAD") return(data.frame(CodContacto = "C2", FechaConversion = as.Date("2024-01-01")))
      if (tabla == "CRMNALLEADCLIENTE") return(data.frame(CodContacto = "C2", FechaConversion = as.Date("2025-01-01")))
      if (tabla == "CRMNALVINCULONIT") return(vinculos_individual)
      if (tabla == "CRMNALPOTENCIAL") {
        if (isTRUE(input$fallar)) stop("Fuente temporalmente fuera de servicio")
        return(data.frame(CodContacto = "C2", ConsumoEsperado = 100, UnidadConsumo = "KG/MES"))
      }
      if (tabla == "CRMNALDIRECTORIOTELEFONO") return(data.frame(CodContacto = c("C1", "C3"),
        Telefono = c("111111", "999999")))
      if (tabla == "CRMNALCLOPT") return(data.frame(CodContacto = "C2", Producto = "C", Categoria = "Café",
        LinNegCod = 21000, IdOportunidad = "OP-2024", FechaHoraCrea = as.Date("2024-06-01"),
        FechaCumpOP = as.Date("2025-12-01"), SacosOP = 5, MargenOP = 100, Descartado = "NO"))
      data.frame(CodContacto = character())
    }
    cartera <- fuente_prueba
    cartera$cartera$PerCod <- c(101, 102, 303, 303)
    mapa <- data.frame(PerCod = c(101, 102, 303), CliNitPpal = c(101, 101, 303))
    consulta <- ConsultaIndividual("consulta", reactive(hechos360), reactive(clientes_individual), f,
      reactive("ANA"), reactive(FALSE), mapa, reactive(cartera), cargar = cargar, directorio = reactive(directorio360))
    output$alcance <- renderText({
      x <- consulta$ejecucion()
      paste(nrow(x), sum(x$SacosFact), sep = " | ")
    })
    productos <- reactiveVal(data.frame(LinNegCod = 21000, LinProCod = 1:2, MCCod = 4, MrcCod = 5,
      Categoria = c(NA, "REGIONAL"), Producto = c(NA, "HUILA"), Marca = "RACAFÉ",
      MCNom = c("CAFÉ NUEVO", "CAFÉ CLASIFICADO"), LinProNom = "SACO", Excluir = "NO"))
    ProductosSinClasificar("clasificacion", reactive("ANA"), function(x) productos(x),
      cargar = function(...) productos(), guardar = function(fila, tabla, llaves, original) {
        x <- productos()
        i <- .clas_llave(x) %in% .clas_llave(fila)
        x[i, ] <- fila
        productos(x)
      })
    MarketBasket("basket", reactive(hechos_individual), f)
    .pr_caja("pr_numero", reactive(0.75), formato = "porcentaje", texto = "Conversión", mostrar_boton = FALSE)
    .pr_caja("pr_html", reactive(tagList(tags$span("12"), tags$small(" (50%)"))),
      texto = "Entidades", mostrar_boton = FALSE, footer = "Nota fuera de la caja.")
  }), local)
}
shinyApp(ui, server)
