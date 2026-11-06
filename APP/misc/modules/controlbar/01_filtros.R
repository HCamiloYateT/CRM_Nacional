# Helpers ----
.filtros_choices <- function(x, incluir_sin_dato = TRUE) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (!incluir_sin_dato) x <- setdiff(x, "SIN DATO")
  unique(x)
}

.filtros_seleccion <- function(actual, disponibles) {
  if (is.null(actual)) return(disponibles)
  intersect(as.character(actual), disponibles)
}

# Modulo ----
FiltrosUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafePanel(title = NULL, padding = "compact", bordered = FALSE,
                           racafeForms::racafeForm(size = "compact", variant = "plain",
                                                   racafeForms::racafeFormGroup(
                                                     racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                                                racafeForms::racafeInputSelect("FT_Periodo", "Valores a Usar", c("DINÁMICO", "ESTÁTICO"), selected = "DINÁMICO",
                                                                                                               multiple = FALSE, empty = FALSE, search = FALSE, ns = ns),
                                                                                racafeForms::racafeInputDateRange("FT_Fecha", "Fecha de Factura", start = as.Date(format(Sys.Date(), "%Y-01-01")),
                                                                                                                  end = Sys.Date(), required = TRUE, ns = ns)),
                                                     racafeForms::racafeInputSwitch("FT_Fechas", "Incluir lotes sin facturar", value = TRUE, variant = "primary", ns = ns)),
                                                   racafeForms::racafeFormGroup(racafeForms::racafeInputSelect("FT_Asesor", "Asesor", choices = character(0), multiple = TRUE, size = 10, ns = ns)),
                                                   racafeForms::racafeFormGroup(divider = FALSE,
                                                                                racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                                                                           racafeForms::racafeInputSelect("FT_LinNeg", "Línea de Negocio", choices = character(0), multiple = TRUE, size = 10, ns = ns),
                                                                                                           racafeForms::racafeInputSelect("FT_Segmento", "Segmento", choices = character(0), multiple = TRUE, size = 10, ns = ns)),
                                                                                racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                                                                           racafeForms::racafeInputSelect("FT_Categoria", "Categoría", choices = character(0), multiple = TRUE, size = 10, ns = ns),
                                                                                                           racafeForms::racafeInputSelect("FT_Producto", "Producto", choices = character(0), multiple = TRUE, size = 10, ns = ns))),
                                                   racafeForms::racafeFormFooter(align = "right",
                                                                                 racafeForms::racafeButton("FT_Aplicar", "Aplicar Filtros", icon = "check", variant = "primary", size = "xs", ns = ns)))
  )
}

Filtros <- function(id, usuario, es_admin, dat) {
  moduleServer(id, function(input, output, session) {
    # Constantes ----
    filtros_aplicados_rv <- reactiveVal(NULL)
    inicializado <- reactiveVal(FALSE)
    asesores_permitidos <- reactiveVal(character(0))
    catalogos_rv <- reactiveVal({
      catalogos <- Choices()
      if (is.null(attr(catalogos, "relaciones"))) {
        .admon_choices_migrar_relaciones()
        catalogos <- Choices()
      }
      catalogos
    })
    
    # Funciones ----
    actualizar <- function(id, choices, selected = choices) {
      choices <- .filtros_choices(choices)
      selected <- intersect(as.character(selected), choices)
      racafeForms::racafeUpdateSelect(session, id,
                                      choices = stats::setNames(choices, toupper(choices)),
                                      selected = selected)
    }
    capturar <- function() {
      categorias <- input$FT_Categoria %||% character(0)
      if ("FUERA DE NORMA" %in% categorias) categorias <- unique(c(categorias, "BLEND"))
      list(periodo = input$FT_Periodo, fecha = as.Date(input$FT_Fecha), sin_factura = isTRUE(input$FT_Fechas),
           asesor = intersect(input$FT_Asesor, asesores_permitidos()), linneg = input$FT_LinNeg,
           segmento = input$FT_Segmento, categoria = categorias, producto = input$FT_Producto)
    }
    
    # Reactivos ----
    disponibles <- reactive({
      x <- dat()
      req(is.data.frame(x))
      validate(need(all(c("Asesor", "Segmento", "CLLinNegNo", "Categoria", "Producto", "FecFact") %in% names(x)),
                    "Faltan columnas requeridas para los filtros"))
      x %>% mutate(across(c(Asesor, Segmento, CLLinNegNo, Categoria, Producto),
                          ~ tidyr::replace_na(trimws(as.character(.x)), "SIN DATO")))
    })
    
    observeEvent(list(disponibles(), usuario(), es_admin()), {
      x <- disponibles()
      catalogos <- catalogos_rv()
      asesores <- intersect(.filtros_choices(catalogos$personas, FALSE), .filtros_choices(x$Asesor, FALSE))
      autorizado <- isTRUE(es_admin())
      permitidos <- if (autorizado) asesores else intersect(asesores, toupper(trimws(as.character(usuario()))))
      asesores_permitidos(permitidos)
      actualizar("FT_Asesor", permitidos, .filtros_seleccion(isolate(input$FT_Asesor), permitidos))
      racafeForms::racafeUpdateEnabled(session, "FT_Asesor", enabled = autorizado)
      segmentos <- .filtros_choices(catalogos$segmento)
      lineas <- .filtros_choices(catalogos$linneg, incluir_sin_dato = FALSE)
      actualizar("FT_Segmento", segmentos, .filtros_seleccion(isolate(input$FT_Segmento), segmentos))
      actualizar("FT_LinNeg", lineas, .filtros_seleccion(isolate(input$FT_LinNeg), lineas))
      fechas <- as.Date(x$FecFact)
      fechas <- fechas[!is.na(fechas)]
      if (length(fechas)) {
        minimo <- min(fechas)
        maximo <- max(Sys.Date(), max(fechas))
        actual <- isolate(input$FT_Fecha)
        inicio <- if (length(actual) == 2L && !anyNA(actual)) as.Date(actual[[1]]) else as.Date(format(Sys.Date(), "%Y-01-01"))
        fin <- if (length(actual) == 2L && !anyNA(actual)) as.Date(actual[[2]]) else maximo
        racafeForms::racafeUpdateDateRange(session, "FT_Fecha", min = minimo, max = maximo,
                                           start = max(minimo, min(inicio, maximo)), end = max(minimo, min(fin, maximo)))
      }
    }, ignoreNULL = FALSE)
    
    observeEvent(list(input$FT_LinNeg, catalogos_rv()), {
      catalogos <- catalogos_rv()
      lineas <- intersect(catalogos$linneg, input$FT_LinNeg %||% character(0))
      categorias <- unique(unlist(lapply(lineas, function(linea) {
        .admon_choices_valores(catalogos, "categoria", linea)
      }), use.names = FALSE))
      actualizar("FT_Categoria", categorias, .filtros_seleccion(isolate(input$FT_Categoria), categorias))
    }, ignoreNULL = FALSE)
    
    observeEvent(list(input$FT_LinNeg, input$FT_Categoria, catalogos_rv()), {
      catalogos <- catalogos_rv()
      lineas <- intersect(catalogos$linneg, input$FT_LinNeg %||% character(0))
      categorias <- input$FT_Categoria %||% character(0)
      productos <- unique(unlist(lapply(lineas, function(linea) {
        validas <- intersect(.admon_choices_valores(catalogos, "categoria", linea), categorias)
        unlist(lapply(validas, function(categoria) {
          .admon_choices_valores(catalogos, "producto", linea, categoria)
        }), use.names = FALSE)
      }), use.names = FALSE))
      actualizar("FT_Producto", productos, .filtros_seleccion(isolate(input$FT_Producto), productos))
    }, ignoreNULL = FALSE)
    
    observeEvent(list(input$FT_Producto, input$FT_Asesor, input$FT_Fecha), {
      if (!inicializado() && length(input$FT_Producto) && length(input$FT_Asesor) && length(input$FT_Fecha) == 2L) {
        filtros_aplicados_rv(capturar())
        inicializado(TRUE)
      }
    }, ignoreNULL = FALSE)
    
    observeEvent(input$FT_Aplicar, {
      if (length(input$FT_Fecha) != 2L || anyNA(input$FT_Fecha)) {
        racafeForms::racafeShowValidation(session, racafeForms::racafeValidateForm(
          input, list(FT_Fecha = list(label = "Fecha de Factura", required = TRUE, kind = "range")),
          ns = session$ns), mode = "inline")
        return(invisible(NULL))
      }
      filtros_aplicados_rv(capturar())
    })
    
    # Retorno ----
    filtros_aplicados_rv
  })
}
