# Consulta integral de una entidad, sin modificar registros ----
.IN_CAJAS <- c(sacos = "Sacos facturados · 70 kg", margen = "Margen FCC del periodo",
  pedidos = "Pedidos facturados", productos = "Productos comprados")
ConsultaIndividualUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("actualizar", "Actualizar",
      icon = "arrows-rotate", variant = "secondary", ns = ns)),
    .crm_select("entidad", "Buscar por nombre o NIT de facturación",
      choices = character(), ns = ns),
    shiny::uiOutput(ns("identidad")), shiny::uiOutput(ns("contenido")))
}
ConsultaIndividual <- function(id, hechos, clientes, filtros, usuario, es_admin, nit_mapa,
                               cartera = function() NULL, tareas = function() data.frame(),
                               cargar = racafeBD::CargarDatos, directorio = function() data.frame()) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    modelo_guardado <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$actualizar, revision(revision() + 1L))
    contactos <- shiny::reactive({
      revision()
      fuente <- .in_fuente(cargar, "CRMNALCONTACTO")
      shiny::validate(shiny::need(is.null(fuente$error), fuente$error))
      fuente$datos
    })
    fuentes <- shiny::reactive({
      revision()
      lapply(.IN_FUENTES, function(x) .in_fuente(cargar, x$tabla))
    })
    vinculos <- shiny::reactive({ revision(); .in_fuente(cargar, "CRMNALVINCULONIT") })
    periodo <- shiny::reactive({
      x <- as.Date(filtros()$fecha)
      shiny::validate(shiny::need(length(x) == 2L && !anyNA(x) && x[[1]] <= x[[2]],
        "Seleccione un periodo válido en el controlbar."))
      x
    })
    catalogo_clientes <- shiny::reactive(.com_clientes(clientes(), periodo()[[2]], filtros()))
    catalogo <- shiny::reactive(.in_catalogo(contactos(), catalogo_clientes(), fuentes()$etapas$datos,
      vinculos()$datos, usuario(), es_admin(), filtros(), .gc_filtrar_dimensiones(hechos(), filtros()), directorio()))
    shiny::observeEvent(catalogo(), {
      x <- catalogo()
      actual <- shiny::isolate(input$entidad)
      opciones <- stats::setNames(x$Clave, x$Etiqueta)
      seleccion <- if (length(actual) == 1L && actual %in% x$Clave) actual else head(x$Clave, 1L)
      .crm_update_select(session, "entidad", choices = opciones, selected = seleccion)
    })
    entidad <- shiny::reactive({
      x <- .in_elegida(catalogo(), input$entidad)
      shiny::validate(shiny::need(nrow(x) == 1L, "Seleccione una entidad disponible para los filtros del controlbar."))
      x
    })
    historia <- shiny::reactive(.in_hechos(.gc_filtrar_dimensiones(hechos(), filtros()), entidad()))
    ejecucion <- shiny::reactive(.in_periodo(historia(), periodo()))
    valores <- shiny::reactive(.in_resumen(ejecucion()))
    output$identidad <- .crm_render_ui({
      x <- entidad()
      badge <- function(texto, clase = "") shiny::tags$span(class = paste("badge", clase), texto)
      shiny::tagList(shiny::h4(x$Nombre), shiny::tags$div(class = "crm-identidad-badges",
        badge(paste("Tipo:", x$Tipo), "badge-tipo"), badge(paste("Vista:", x$Alcance)),
        badge(paste("NIT", dplyr::coalesce(x$NIT, "sin registrar"))),
        lapply(strsplit(dplyr::coalesce(x$Asesor, "Sin asesor"), "|", fixed = TRUE)[[1]],
          function(asesor) badge(paste("Asesor:", trimws(asesor))))),
        if (x$VinculoEnRevision) .crm_nota("Hay varios NIT vinculados. Revise el vínculo de facturación."))
    })
    output$contenido <- .crm_render_ui({
      x <- entidad()
      comercial <- x$Tipo == "CLIENTE" || nrow(historia()) > 0L
      crm <- !is.na(x$Codigo)
      potencial <- crm && (x$Tipo %in% c("PROSPECTO", "LEAD", "CLIENTE") ||
        .in_tiene_detalle(fuentes()$potencial, x$Codigo) || .in_tiene_detalle(fuentes()$oportunidades, x$Codigo))
      opciones <- c("Ficha", if (crm) "Gestión y seguimiento",
        if (potencial) "Potencial y oportunidades",
        if (comercial) c("Ejecución y productos", "Pendientes", "Cartera"),
        if (comercial && isTRUE(x$EsPrincipal)) "Fidelidad y valor")
      seleccion <- shiny::isolate(input$detalle_tabs)
      if (length(seleccion) != 1L || !seleccion %in% opciones) seleccion <- "Ficha"
      .crm_tabs(
        tabPanel("Ficha", .crm_tabla_ui(session$ns("ficha"), searchable = FALSE),
          if (crm) shiny::tagList(
            .crm_tabla_ui(session$ns("directorio"), "Directorio empresarial"),
            .crm_tabla_ui(session$ns("telefonos"), "Teléfonos"),
            .crm_tabla_ui(session$ns("personas"), "Personas de contacto"),
            .crm_tabla_ui(session$ns("redes"), "Redes sociales"),
            .crm_tabla_ui(session$ns("ubicaciones"), "Ubicaciones"),
            .crm_tabla_ui(session$ns("sucursales"), "Sucursales")) else {
              .crm_nota("Para consultar directorio y gestiones propias, seleccione una entidad del CRM.")
            }),
        if (crm) tabPanel("Gestión y seguimiento",
          .crm_tabla_ui(session$ns("gestiones"), "Gestiones realizadas"),
          .crm_tabla_ui(session$ns("recordatorios"), "Recordatorios"),
          .crm_tabla_ui(session$ns("tareas"), "Tareas y notas visibles para usted"),
          .crm_disclosure("Recorrido de la entidad", .crm_tabla_ui(session$ns("etapas")))),
        if (potencial) tabPanel("Potencial y oportunidades",
          .crm_tabla_ui(session$ns("potencial"), "Potencial registrado"),
          .crm_tabla_ui(session$ns("oportunidades"), "Oportunidades de la entidad")),
        if (comercial) tabPanel("Ejecución y productos", .gc_tarjetas_ui(session$ns, .IN_CAJAS),
          plotly::plotlyOutput(session$ns("tendencia"), height = "310px"),
          .crm_descarga_ui("descargar", session$ns),
          .crm_tabla_ui(session$ns("productos"), "Compras por categoría y producto"),
          .crm_tabla_ui(session$ns("movimientos"), "Facturación del periodo"),
          .crm_disclosure("Metas y clasificación comercial", .crm_tabla_ui(session$ns("presupuesto")),
            .crm_nota(c("Las metas son referencias mensuales del último registro disponible al corte.",
              "No se asigna presupuesto de cliente a cada producto. Un valor vacío requiere revisión."))),
          .crm_nota(c("El margen de meses abiertos es provisional; un importe desconocido no se convierte en cero.",
            if (crm) "Para consultar fidelidad y valor consolidado, seleccione la cuenta principal en el buscador."))),
        if (comercial) tabPanel("Pendientes", .crm_tabla_ui(session$ns("pendientes")),
          .crm_nota(c("Incluye pedidos sin asignar y futuros, producción, despacho, facturación y revisión de margen.",
            "Usa el flujo disponible y la fecha final del controlbar como referencia de vencimiento.",
            "Las etapas pueden solaparse: sus cantidades no deben sumarse."))),
        if (comercial) tabPanel("Cartera", shiny::uiOutput(session$ns("corte_cartera")),
          .crm_tabla_ui(session$ns("cartera"))),
        if (comercial && isTRUE(x$EsPrincipal)) tabPanel("Fidelidad y valor",
          .crm_disclosure("¿Qué me dicen estos indicadores?",
            .crm_nota(c("RFM resume tres preguntas: ¿compró hace poco?, ¿compra seguido?, ¿cuánto margen deja?",
              "Las notas van de 1 a 5 frente a los otros clientes visibles; 5 es el valor más favorable.",
              "BG/NBD usa el ritmo de compras para estimar si el cliente seguirá comprando y cuántas veces volvería.",
              "CLV traduce esas compras esperadas a margen futuro estimado. No es una venta asegurada.",
              "Por ejemplo, una actividad de 80% es una estimación del modelo; no confirma que exista un pedido.",
              "Se analiza la historia hasta la fecha final del controlbar, con sus filtros comerciales."))),
          .crm_tabla_ui(session$ns("rfm"), "Hábitos de compra de la cuenta principal"),
          racafeForms::racafeButton("modelar", "Estimar compras y valor a 12 meses", ns = session$ns),
          shiny::uiOutput(session$ns("modelo"))), id = session$ns("detalle_tabs"), selected = seleccion)
    })
    ficha <- shiny::reactive({
      x <- entidad()
      if (is.na(x$Codigo)) return(data.frame(Campo = c("Cuenta principal", "NIT", "Asesor"),
        Valor = c(x$Nombre, x$NIT, x$Asesor)))
      c <- contactos()[.in_id(contactos()$CodContacto) %in% .in_codigos(x), , drop = FALSE]
      campos <- intersect(c("CodContacto", "PerRazSoc", "PerCod", "Asesor", "Origen", "DetOrigen", "Estado",
        "FechaHoraCrea", "Observaciones"), names(c))
      etiquetas <- c(CodContacto = "Código de entidad", PerRazSoc = "Nombre", PerCod = "Documento registrado en CRM",
        DetOrigen = "Detalle de origen",
        FechaHoraCrea = "Creación", Observaciones = "Observaciones", Asesor = "Asesor",
        Origen = "Origen", Estado = "Estado")
      c <- .crm_nits_tabla(c)
      if ("PerCod" %in% names(c)) c$PerCod <- .in_id(c$PerCod)
      data.frame(Campo = unname(etiquetas[campos]), Valor = vapply(c[campos], function(v) {
        paste(unique(stats::na.omit(as.character(v))), collapse = " | ")
      }, character(1)))
    })
    .crm_vancouver_server("ficha", ficha, searchable = FALSE)
    for (nombre in names(.IN_FUENTES)) local({
      clave <- nombre
      .crm_vancouver_server(clave, shiny::reactive(.in_detalle_crm(fuentes()[[clave]], entidad(),
        .IN_FUENTES[[clave]]$campos)))
    })
    .crm_vancouver_server("tareas", shiny::reactive({
      x <- .tareas_visibles_usuario(tareas(), usuario(), es_admin())
      shiny::validate(shiny::need(!nrow(x) || "CodContacto" %in% names(x),
        "Las tareas existentes no tienen un vínculo de entidad. Consúltelas en Notas y Tareas."))
      .in_detalle_crm(list(datos = x), entidad(), c("titulo", "Descripcion", "Responsable", "Prioridad",
        "FechaCumplimiento", "Cumplido", "Archivado"))
    }))
    .gc_tarjetas_server(.IN_CAJAS, valores, list(sacos = "numero", margen = "dinero"))
    .crm_vancouver_server("presupuesto", shiny::reactive(.in_presupuesto(catalogo_clientes(), entidad())))
    productos <- shiny::reactive({
      ejecucion() %>% dplyr::group_by(Categoria, Producto) %>%
        dplyr::summarise(Sacos70kg = .com_suma(SacosFact), MargenFCC = .com_suma(Margen), .groups = "drop")
    })
    .crm_vancouver_server("productos", productos,
      col_labels = c(Categoria = "Categoría", Producto = "Producto", Sacos70kg = "Sacos facturados (70 kg)",
        MargenFCC = "Margen total FCC (COP)"))
    .crm_vancouver_server("movimientos", shiny::reactive({
      x <- ejecucion()
      .crm_nits_tabla(x[intersect(c("CLCliNit", "CLPdcCod", "CLLotCod", "FecFact", "CLLinNegNo", "Categoria",
        "Producto", "SacosFact", "Margen", "EstadoFinanciero"), names(x))])
    }), col_labels = c(CLCliNit = "NIT", CLPdcCod = "Pedido", CLLotCod = "Lote", FecFact = "Fecha de factura",
      CLLinNegNo = "Línea", SacosFact = "Sacos 70 kg", EstadoFinanciero = "Estado financiero"))
    .crm_vancouver_server("pendientes", shiny::reactive(.pen_preparar(historia(), periodo()[[2]])$detalle),
      columnas = c("Etapa", "Nivel", "Pedido", "Linea", "Lote", "Fecha", "Cantidad", "Motivo"))
    clientes_cartera <- shiny::reactive({
      catalogo <- catalogo_clientes()[c("CliNitPpal", "LinNegCod")]
      movimientos <- poblacion()[c("CliNitPpal", "LinNegCod")]
      catalogo$CliNitPpal <- .in_id(catalogo$CliNitPpal)
      movimientos$CliNitPpal <- .in_id(movimientos$CliNitPpal)
      dplyr::distinct(dplyr::bind_rows(catalogo, movimientos))
    })
    cartera_entidad <- shiny::reactive({
      h <- cartera()
      shiny::validate(shiny::need(is.list(h) && is.data.frame(h$cartera), "No hay fuente de cartera disponible."))
      x <- .car_scope(h$cartera, clientes_cartera(), nit_mapa)
      cortes <- sort(unique(as.Date(x$TCEFch)), decreasing = TRUE, na.last = NA)
      cortes <- cortes[cortes <= periodo()[[2]]]
      shiny::validate(shiny::need(length(cortes) > 0, "No hay corte de cartera hasta la fecha seleccionada."))
      corte <- cortes[[1]]
      e <- entidad()
      x <- x[as.Date(x$TCEFch) == corte, , drop = FALSE]
      x <- if (e$Alcance == "Cuenta principal") x[x$Principal %in% e$Principal, , drop = FALSE] else {
        x[x$NIT %in% stats::na.omit(e$NitFacturacion), , drop = FALSE]
      }
      list(datos = .car_preparar(x, corte), corte = corte, origen = h$origen)
    })
    output$corte_cartera <- .crm_render_ui({
      x <- cartera_entidad()
      .crm_nota(c(paste("Corte de cartera:", x$corte, "· Fuente:", x$origen),
        "La cartera corresponde a cliente y línea; no se distribuye por categoría ni producto."))
    })
    .crm_vancouver_server("cartera", shiny::reactive(cartera_entidad()$datos),
      columnas = c("NIT", "Factura", "FechaVence", "DiasVencido", "Saldo", "Sacos", "Rango"))
    poblacion <- shiny::reactive(.gc_filtrar_dimensiones(hechos(), filtros()))
    .crm_vancouver_server("rfm", shiny::reactive({
      x <- .fid_rfm(poblacion(), periodo()[[2]])
      x[.in_id(x$CliNitPpal) %in% entidad()$Principal, , drop = FALSE]
    }), columnas = c("LinNegCod", "Ultima", "RecenciaDias", "Compras", "Margen", "R", "F", "M", "SegmentoRFM"),
      col_labels = c(LinNegCod = "Línea", Ultima = "Última compra", RecenciaDias = "Días sin compra",
        Compras = "Días con compra", SegmentoRFM = "Hábitos de compra"))
    firma <- shiny::reactive(list(filtros(), revision(), hechos()))
    shiny::observeEvent(firma(), modelo_guardado(NULL), ignoreInit = TRUE)
    shiny::observeEvent(input$modelar, {
      resultado <- tryCatch(.fid_analizar_modelos(poblacion(), periodo()[[2]], 12L, 0.12),
        error = function(e) list(error = conditionMessage(e)))
      modelo_guardado(list(firma = firma(), resultado = resultado))
    })
    output$modelo <- .crm_render_ui({
      m <- modelo_guardado()
      if (is.null(m) || !identical(m$firma, firma())) return(.crm_nota("Pulse Estimar para calcular el modelo."))
      if (!is.null(m$resultado$error)) return(racafeForms::racafeFormMessage(m$resultado$error, type = "error"))
      x <- m$resultado$clientes
      x <- x[.in_id(x$CliNitPpal) %in% entidad()$Principal, , drop = FALSE]
      if (!nrow(x)) return(.crm_nota("Esta cuenta no tiene historia suficiente para estimar compras futuras."))
      tabla <- data.frame(Linea = x$LinNegCod, ActividadPct = 100 * x$ProbActivo,
        Compras12Meses = x$ComprasEsperadas, Margen12Meses = x$CLVMargen)
      names(tabla) <- c("Línea", "Posibilidad de seguir comprando (%)", "Compras estimadas · 12 meses",
        "Margen estimado · COP")
      shiny::tagList(.crm_tabla_vancouver(tabla, font_size = 11, row_padding = 2),
        .crm_nota("Estimación a 12 meses con descuento anual de 12%. No equivale a pedidos confirmados."))
    })
    output$tendencia <- .crm_render_plotly({
      x <- ejecucion()
      shiny::validate(shiny::need(nrow(x) > 0, "Sin facturación para esta entidad y el periodo seleccionado."))
      x <- x %>% dplyr::mutate(Mes = .gc_mes(FecFact)) %>% dplyr::group_by(Mes) %>%
        dplyr::summarise(Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen), .groups = "drop")
      plotly::plot_ly(x, x = ~Mes, y = ~Sacos, type = "scatter", mode = "lines+markers",
        line = list(color = "#7A2028"), text = ~paste("Margen FCC:", format(Margen, big.mark = ",")),
        hovertemplate = "%{x|%Y-%m}<br>Sacos 70 kg: %{y:,.2f}<br>%{text}<extra></extra>") %>%
        plotly::layout(title = list(text = "Compras mensuales", font = list(size = 16)),
          xaxis = list(title = "Mes"), yaxis = list(title = "Sacos 70 kg"), font = list(size = 12))
    })
    .gc_descarga(output, ejecucion, "consulta_individual")
    invisible(list(catalogo = catalogo, entidad = entidad, ejecucion = ejecucion, historia = historia))
  })
}
