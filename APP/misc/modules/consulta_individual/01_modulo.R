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
    revision_modelos <- shiny::reactiveVal(0L)
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
    contexto_crm <- shiny::reactive(.in_contexto_crm(entidad(), catalogo()))
    familia <- shiny::reactive(.in_familia(entidad(), catalogo(), nit_mapa, directorio()))
    metas_anuales <- shiny::reactive(.com_clientes(clientes(), periodo()[[2]], filtros(), anual = TRUE))
    historia <- shiny::reactive(.in_hechos(.gc_filtrar_dimensiones(hechos(), filtros()), entidad()))
    ejecucion <- shiny::reactive(.in_periodo(historia(), periodo()))
    valores <- shiny::reactive(.in_resumen(ejecucion()))
    historico <- shiny::reactive(.in_facturacion_historica(historia(), periodo()[[2]]))
    datos_cliente <- shiny::reactive(.in_datos_cliente(entidad(), familia(), directorio(), historia()))
    output$identidad <- .crm_render_ui({
      x <- entidad()
      badge <- function(texto, clase = "") shiny::tags$span(class = paste("badge", clase), texto)
      shiny::tagList(shiny::h4(x$Nombre), shiny::tags$div(class = "crm-identidad-badges",
        badge(paste("Tipo:", x$Tipo), "badge-tipo"), badge(paste("Vista:", x$Alcance)),
        badge(paste("NIT", dplyr::coalesce(x$NIT, "sin registrar"))),
        lapply(strsplit(dplyr::coalesce(x$Asesor, "Sin asesor"), "|", fixed = TRUE)[[1]],
          function(asesor) badge(paste("Asesor:", trimws(asesor)))),
        badge(.in_estado_presupuesto(metas_anuales(), entidad(), periodo()[[2]])),
        badge(familia()$relacion)),
        if (x$VinculoEnRevision) .crm_nota("Hay varios NIT vinculados. Revise el vínculo de facturación."))
    })
    output$contenido <- .crm_render_ui({
      x <- entidad()
      comercial <- x$Tipo == "CLIENTE" || nrow(historia()) > 0L
      crm <- !is.na(contexto_crm()$Codigo)
      potencial <- crm && (x$Tipo %in% c("PROSPECTO", "LEAD", "CLIENTE") ||
        .in_tiene_detalle(fuentes()$potencial, x$Codigo) || .in_tiene_detalle(fuentes()$oportunidades, x$Codigo))
      opciones <- c("Ficha", if (crm) "Gestión y seguimiento",
        if (potencial) "Potencial y oportunidades",
        if (comercial) c("Ejecución y productos", "Histórico completo", "Preferencias de compra",
          "Pendientes", "Cartera", "Fidelidad y valor", "Venta cruzada"))
      seleccion <- shiny::isolate(input$detalle_tabs)
      if (length(seleccion) != 1L || !seleccion %in% opciones) seleccion <- "Ficha"
      .crm_tabs(
        tabPanel("Ficha", shiny::uiOutput(session$ns("alcance_crm")),
          .crm_tabla_ui(session$ns("ficha"), "Registro CRM asociado", searchable = FALSE),
          .crm_tabla_ui(session$ns("antiguedad"), "Antigüedad y relación comercial", searchable = FALSE),
          .crm_tabla_ui(session$ns("contacto_sistema"), "Datos de contacto del cliente"),
          .crm_tabla_ui(session$ns("geografia"), "Ubicación del cliente"),
          .crm_tabla_ui(session$ns("familia"), "Cuenta padre y cuentas relacionadas visibles"),
          .crm_tabla_ui(session$ns("presupuesto"), "Presupuesto y metas de la cuenta"),
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
          shiny::uiOutput(session$ns("alcance_seguimiento")),
          .crm_tabla_ui(session$ns("gestiones"), "Gestiones realizadas"),
          .crm_tabla_ui(session$ns("recordatorios"), "Recordatorios"),
          .crm_tabla_ui(session$ns("tareas"), "Tareas y notas visibles para usted"),
          .crm_tabla_ui(session$ns("cronologia"), "Recorrido comercial completo"),
          shiny::uiOutput(session$ns("estado_cronologia")),
          .crm_disclosure("Conversiones y alianzas registradas",
            .crm_tabla_ui(session$ns("conversion_lead"), "Conversión a lead"),
            .crm_tabla_ui(session$ns("conversion_cliente"), "Conversión a cliente"),
            .crm_tabla_ui(session$ns("alianzas"), "Alianzas comerciales"))),
        if (potencial) tabPanel("Potencial y oportunidades",
          .crm_nota("Las oportunidades muestran el estado actual guardado para la entidad comercial asociada."),
          .crm_tabla_ui(session$ns("potencial"), "Potencial registrado"),
          .crm_tabla_ui(session$ns("oportunidades"), "Oportunidades de la entidad")),
        if (comercial) tabPanel("Ejecución y productos", .gc_tarjetas_ui(session$ns, .IN_CAJAS),
          plotly::plotlyOutput(session$ns("tendencia"), height = "310px"),
          .crm_descarga_ui("descargar", session$ns),
          .crm_tabla_ui(session$ns("productos"), "Compras por categoría y producto"),
          .crm_tabla_ui(session$ns("movimientos"), "Facturación del periodo"),
          .crm_nota(c("El margen de meses abiertos es provisional; un importe desconocido no se convierte en cero.",
            "El periodo corresponde a las fechas seleccionadas en el controlbar."))),
        if (comercial) tabPanel("Histórico completo",
          .crm_nota("Incluye todos los años disponibles hasta el corte del controlbar, con sus filtros comerciales."),
          .gc_tarjetas_ui(session$ns, c(hist_sacos = "Sacos en todo el histórico (70 kg)",
            hist_margen = "Margen histórico FCC (COP)", hist_pedidos = "Pedidos históricos",
            hist_anhos = "Años con información")),
          plotly::plotlyOutput(session$ns("historico_grafico"), height = "330px"),
          .crm_tabla_ui(session$ns("historico_anual"), "Compras y margen por año"),
          .crm_descarga_ui("descargar_historico", session$ns, "Descargar histórico completo")),
        if (comercial) tabPanel("Preferencias de compra",
          plotly::plotlyOutput(session$ns("preferencias_grafico"), height = "330px"),
          .crm_tabla_ui(session$ns("preferencias"), "Qué productos compra, con qué frecuencia y desde cuándo"),
          .crm_nota(c("Ranking basado en las compras históricas de esta cuenta hasta el corte seleccionado.",
            "Las preferencias declaradas en Potencial muestran el último registro disponible."))),
        if (comercial) tabPanel("Pendientes", .crm_tabla_ui(session$ns("pendientes")),
          .crm_nota(c("Incluye pedidos sin asignar y futuros, producción, despacho, facturación y revisión de margen.",
            "Usa el flujo disponible y la fecha final del controlbar como referencia de vencimiento.",
            "Las etapas pueden solaparse: sus cantidades no deben sumarse."))),
        if (comercial) tabPanel("Cartera", shiny::uiOutput(session$ns("corte_cartera")),
          .crm_tabla_ui(session$ns("cartera"))),
        if (comercial) tabPanel("Fidelidad y valor",
          .crm_disclosure("¿Qué me dicen estos indicadores?",
            .crm_nota(c("RFM resume tres preguntas: ¿compró hace poco?, ¿compra seguido?, ¿cuánto margen deja?",
              "Las notas van de 1 a 5 frente a los otros clientes visibles; 5 es el valor más favorable.",
              "BG/NBD usa el ritmo de compras para estimar si el cliente seguirá comprando y cuántas veces volvería.",
              "CLV traduce esas compras esperadas a margen futuro estimado. No es una venta asegurada.",
              "Por ejemplo, una actividad de 80% es una estimación del modelo; no confirma que exista un pedido.",
              "La cuenta hija se analiza por su NIT; la vista consolidada agrupa las cuentas relacionadas visibles.",
              "Se usa toda la historia hasta el corte. Las estimaciones no son pedidos confirmados."))),
          .crm_tabla_ui(session$ns("rfm"), "Hábitos de compra de la cuenta consultada"),
          racafeForms::racafeButton("modelar", "Recalcular estimación a 12 meses", ns = session$ns),
          shiny::uiOutput(session$ns("modelo"))),
        if (comercial) tabPanel("Venta cruzada",
          .crm_disclosure("Cómo usar estas propuestas",
            .crm_nota(c("Son productos que otras cuentas comparables compran junto con los de esta cuenta.",
              "Solo se proponen productos que esta cuenta aún no registra en su historia observada.",
              "Se requieren al menos dos cuentas que respalden la combinación y afinidad mayor que 1.",
              "Las compras pueden ser de pedidos y años distintos. Confirme interés, calidad y presentación."))),
          shiny::uiOutput(session$ns("venta_cruzada"))),
        id = session$ns("detalle_tabs"), selected = seleccion)
    })
    nota_crm <- shiny::reactive({
      x <- contexto_crm()
      if (x$OrigenCRM == "Cuenta padre" && !is.na(x$Codigo)) {
        .crm_nota(paste("Esta cuenta usa el seguimiento comercial de su cuenta padre.",
          "Sus compras, presupuesto y modelos se mantienen separados por su NIT de facturación."))
      }
    })
    output$alcance_crm <- .crm_render_ui(nota_crm())
    output$alcance_seguimiento <- .crm_render_ui(nota_crm())
    ficha <- shiny::reactive({
      x <- contexto_crm()
      if (is.na(x$Codigo)) return(data.frame(Campo = c("Cuenta consultada", "NIT de facturación", "Asesor"),
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
      if (clave == "oportunidades") {
        .crm_vancouver_server(clave, shiny::reactive(.in_oportunidades(fuentes()[[clave]], contexto_crm())),
          col_labels = c(Creacion = "Fecha de creación", FechaObjetivo = "Fecha objetivo", Linea = "Línea de negocio",
            SacosObjetivo = "Sacos objetivo (unidad declarada)", MargenObjetivoKg = "Margen objetivo por kg (COP)",
            EstadoActual = "Estado actual del registro"))
      } else .crm_vancouver_server(clave, shiny::reactive(.in_detalle_crm(fuentes()[[clave]], contexto_crm(),
        .IN_FUENTES[[clave]]$campos)))
    })
    .crm_vancouver_server("tareas", shiny::reactive({
      x <- .tareas_visibles_usuario(tareas(), usuario(), es_admin())
      shiny::validate(shiny::need(!nrow(x) || "CodContacto" %in% names(x),
        "Las tareas existentes no tienen un vínculo de entidad. Consúltelas en Notas y Tareas."))
      .in_detalle_crm(list(datos = x), contexto_crm(), c("titulo", "Descripcion", "Responsable", "Prioridad",
        "FechaCumplimiento", "Cumplido", "Archivado"))
    }))
    .gc_tarjetas_server(.IN_CAJAS, valores, list(sacos = "numero", margen = "dinero"))
    .crm_vancouver_server("presupuesto", shiny::reactive(.in_presupuesto(metas_anuales(), entidad())),
      col_labels = c(PresupuestoEnRevision = "Requiere revisión del presupuesto"),
      col_specs = list(PresupuestoEnRevision = list(cell = function(x) {
        if (is.na(x)) "Sin información" else if (x) "Sí" else "No"
      })))
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
    nivel <- shiny::reactive(if (entidad()$Alcance == "Cuenta principal") "principal" else "nit")
    poblacion_nit <- shiny::reactive(.in_poblacion_modelo(poblacion(), "nit"))
    poblacion_principal <- shiny::reactive(.in_poblacion_modelo(poblacion(), "principal"))
    rfm_nit <- shiny::reactive(.fid_rfm(poblacion_nit(), periodo()[[2]]))
    rfm_principal <- shiny::reactive(.fid_rfm(poblacion_principal(), periodo()[[2]]))
    .crm_vancouver_server("rfm", shiny::reactive({
      x <- if (nivel() == "nit") rfm_nit() else rfm_principal()
      x$Linea <- .in_linea(x$LinNegCod)
      x[.in_id(x$CliNitPpal) %in% .in_id_modelo(entidad()), , drop = FALSE]
    }), columnas = c("Linea", "Primera", "Ultima", "RecenciaDias", "Compras", "Margen",
      "R", "F", "M", "SegmentoRFM"),
      col_labels = c(Linea = "Línea de negocio", Primera = "Primera compra observada", Ultima = "Última compra",
        RecenciaDias = "Días sin compra", Compras = "Días con compra", SegmentoRFM = "Perfil de compra",
        R = "Recencia (1 a 5)", F = "Frecuencia (1 a 5)", M = "Margen aportado (1 a 5)"))
    calcular_modelo <- function(x) tryCatch(.fid_analizar_modelos(x, periodo()[[2]], 12L, 0.12),
      error = function(e) list(error = conditionMessage(e)))
    modelo_nit <- shiny::reactive({ revision_modelos(); calcular_modelo(poblacion_nit()) })
    modelo_principal <- shiny::reactive({ revision_modelos(); calcular_modelo(poblacion_principal()) })
    shiny::observeEvent(input$modelar, revision_modelos(revision_modelos() + 1L))
    output$modelo <- .crm_render_ui({
      m <- if (nivel() == "nit") modelo_nit() else modelo_principal()
      if (!is.null(m$error)) {
        message("[Consulta individual: CLV] ", m$error)
        return(.crm_nota(paste("Las compras disponibles no permiten estimar el valor futuro con fiabilidad.",
          "El perfil RFM sigue disponible; el modelo requiere al menos 20 cuentas y cinco con recompra.")))
      }
      x <- m$clientes
      x <- x[.in_id(x$CliNitPpal) %in% .in_id_modelo(entidad()), , drop = FALSE]
      if (!nrow(x)) return(.crm_nota("Esta cuenta no tiene historia suficiente para estimar compras futuras."))
      tabla <- data.frame(Linea = .in_linea(x$LinNegCod), ActividadPct = 100 * x$ProbActivo,
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
    .crm_vancouver_server("familia", shiny::reactive(familia()$cuentas),
      col_labels = c(Relacion = "Relación con la cuenta padre"))
    .crm_vancouver_server("antiguedad", shiny::reactive(.in_antiguedad(contexto_crm(), datos_cliente(),
      contactos(), historico(), periodo()[[2]])), searchable = FALSE)
    .crm_vancouver_server("contacto_sistema", datos_cliente,
      columnas = c("NIT", "Cuenta", "Contacto", "Telefono", "Correo"))
    .crm_vancouver_server("geografia", datos_cliente,
      columnas = c("NIT", "Cuenta", "Direccion", "Ciudad", "Departamento", "Region", "Pais"),
      col_labels = c(Region = "Región", Pais = "País"))
    .crm_vancouver_server("cronologia", shiny::reactive(.in_cronologia(contexto_crm(), contactos(), fuentes(),
      periodo()[[2]])), columnas = c("Momento", "EntidadCRM", "Evento", "Detalle", "Responsable", "Origen"),
      col_labels = c(Momento = "Fecha y hora registrada", EntidadCRM = "Entidad comercial",
        Origen = "Registro de origen"))
    output$estado_cronologia <- .crm_render_ui({
      fallidas <- vapply(fuentes(), function(x) !is.null(x$error), logical(1))
      if (any(fallidas)) .crm_nota("Parte del seguimiento no pudo cargarse. Actualice para completar el recorrido.")
    })
    anual <- shiny::reactive(.in_historico_anual(historico()))
    .crm_vancouver_server("historico_anual", anual,
      col_labels = c(Anho = "Año", Sacos = "Sacos facturados (70 kg)", Margen = "Margen total FCC (COP)",
        DiasConCompra = "Días con compra", Pedidos = "Pedidos facturados"))
    preferencias <- shiny::reactive(.in_preferencias(historico()))
    .crm_vancouver_server("preferencias", preferencias,
      col_labels = c(Sacos = "Sacos netos (70 kg)", Margen = "Margen FCC (COP)", DiasConCompra = "Días con compra",
        PrimeraCompra = "Primera compra del producto", UltimaCompra = "Última compra observada"))
    .gc_tarjetas_server(c(hist_sacos = "Sacos en todo el histórico (70 kg)", hist_margen = "Margen histórico FCC (COP)",
      hist_pedidos = "Pedidos históricos", hist_anhos = "Años con información"), shiny::reactive({
        x <- .in_resumen(historico())
        list(hist_sacos = x$sacos, hist_margen = x$margen, hist_pedidos = x$pedidos, hist_anhos = nrow(anual()))
      }), list(hist_sacos = "numero", hist_margen = "dinero"))
    output$historico_grafico <- .crm_render_plotly({
      x <- anual()
      shiny::validate(shiny::need(nrow(x) > 0, "No hay compras históricas para esta cuenta."))
      plotly::plot_ly(x, x = ~Anho, y = ~Sacos, type = "bar", name = "Sacos de 70 kg",
        marker = list(color = "#7A2028"),
        hovertemplate = "Año %{x}<br>Sacos: %{y:,.2f}<extra></extra>") %>%
        plotly::add_trace(y = ~Margen, type = "scatter", mode = "lines+markers", yaxis = "y2", name = "Margen FCC",
          line = list(color = "#495057"), hovertemplate = "Año %{x}<br>Margen FCC: $%{y:,.0f}<extra></extra>") %>%
        plotly::layout(title = "Cómo han evolucionado las compras de la cuenta",
          xaxis = list(title = "Año", dtick = 1, tickformat = "d"),
          yaxis = list(title = "Sacos de 70 kg"),
          yaxis2 = list(title = "Margen FCC (COP)", overlaying = "y", side = "right"))
    })
    output$preferencias_grafico <- .crm_render_plotly({
      x <- preferencias() %>% dplyr::group_by(CLLinNegNo, Categoria, Producto) %>%
        dplyr::summarise(Sacos = .com_suma(Sacos), .groups = "drop") %>% dplyr::arrange(dplyr::desc(Sacos))
      x <- head(x[is.finite(x$Sacos) & x$Sacos > 0, , drop = FALSE], 10L)
      shiny::validate(shiny::need(nrow(x) > 0, "No hay productos con compras positivas en la historia disponible."))
      x$Etiqueta <- paste(x$CLLinNegNo, x$Categoria, x$Producto, sep = " · ")
      plotly::plot_ly(x, x = ~Sacos, y = ~reorder(Etiqueta, Sacos), type = "bar", orientation = "h",
        marker = list(color = "#7A2028"), hovertemplate = "%{y}<br>Sacos: %{x:,.2f}<extra></extra>") %>%
        plotly::layout(title = "Los productos que más compra esta cuenta", xaxis = list(title = "Sacos de 70 kg"),
          yaxis = list(title = ""), margin = list(l = 20))
    })
    venta_nit <- shiny::reactive(.in_venta_base(hechos(), filtros(), periodo()[[2]], "nit"))
    venta_principal <- shiny::reactive(.in_venta_base(hechos(), filtros(), periodo()[[2]], "principal"))
    output$venta_cruzada <- .crm_render_ui({
      base <- if (nivel() == "nit") venta_nit() else venta_principal()
      x <- .in_propuestas(base, entidad())
      if (!nrow(x)) return(.crm_nota(paste("No hay propuestas con evidencia suficiente para esta cuenta.",
        "Revise sus preferencias declaradas y sus oportunidades registradas.")))
      x$Oportunidad <- .in_cruce_oportunidades(x, base, fuentes()$oportunidades, contexto_crm())
      columnas <- c("Tiene", "Falta", "CestasEvidencia", "Confianza", "Lift", "Oportunidad")
      tabla <- .crm_tabla_vancouver(x[columnas], font_size = 11, row_padding = 3,
        labels = c(Tiene = "Ya compra", Falta = "Podría ofrecerle también",
          CestasEvidencia = "Cuentas que compran ambos",
          Confianza = "Proporción que compra ambos", Lift = "Afinidad de la combinación",
          Oportunidad = "Seguimiento comercial ya registrado"))
      gt::fmt_percent(tabla, columns = "Confianza", decimals = 1)
    })
    output$descargar_historico <- shiny::downloadHandler(
      filename = function() paste0("historico_cuenta_", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(list(Movimientos = .crm_nits_tabla(historico()),
        PorAnho = .crm_nits_tabla(anual()), Preferencias = preferencias()), file))
    .gc_descarga(output, ejecucion, "consulta_individual")
    invisible(list(catalogo = catalogo, entidad = entidad, ejecucion = ejecucion, historia = historia))
  })
}
