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
    shiny::uiOutput(ns("identidad")), shiny::uiOutput(ns("visibilidad_tabs")), shiny::uiOutput(ns("contenido")))
}
ConsultaIndividual <- function(id, hechos, clientes, filtros, usuario, es_admin, nit_mapa,
                               cartera = function() NULL, tareas = function() data.frame(),
                               cargar = racafeBD::CargarDatos, directorio = function() data.frame(),
                               estados = function() data.frame(), solicitud = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    revision_modelos <- shiny::reactiveVal(0L)
    solicitud_atendida <- shiny::reactiveVal(NULL)
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
    catalogo <- shiny::reactive({
      crm <- contactos()
      leads <- fuentes()$conversion_lead$datos
      if (all(c("CodContacto", "Asesor", "FechaConversion") %in% names(leads))) {
        leads <- leads[order(leads$FechaConversion, decreasing = TRUE), , drop = FALSE]
        indice <- match(as.character(crm$CodContacto), as.character(leads$CodContacto))
        crm$Asesor <- dplyr::coalesce(as.character(.gc_columna(crm, "Asesor")), as.character(leads$Asesor[indice]))
      }
      .in_catalogo(crm, catalogo_clientes(), fuentes()$etapas$datos, vinculos()$datos, usuario(), es_admin(),
        filtros(), .gc_filtrar_dimensiones(hechos(), filtros()), directorio())
    })
    shiny::observeEvent(list(catalogo(), solicitud()), {
      x <- catalogo()
      actual <- shiny::isolate(input$entidad)
      opciones <- stats::setNames(x$Clave, x$Etiqueta)
      seleccion <- if (length(actual) == 1L && actual %in% x$Clave) actual else head(x$Clave, 1L)
      destino <- solicitud()
      if (!is.null(destino) && !identical(destino, shiny::isolate(solicitud_atendida()))) {
        codigos <- strsplit(ifelse(is.na(x$Codigo), "", as.character(x$Codigo)), "|", fixed = TRUE)
        coincidencias <- integer()
        if (length(destino$codigo) == 1L && !is.na(destino$codigo) && nzchar(destino$codigo)) {
          coincidencias <- which(vapply(codigos, function(ids) destino$codigo %in% ids, logical(1)))
        }
        por_nit <- which(!is.na(x$NIT) & as.character(x$NIT) %in% destino$nit)
        if (length(por_nit)) coincidencias <- por_nit
        if (length(coincidencias)) seleccion <- x$Clave[coincidencias[1]] else {
          shiny::showNotification("La entidad no está disponible para el alcance actual.", type = "warning")
        }
        solicitud_atendida(destino)
      }
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
    presupuesto_ficha <- shiny::reactive(.in_presupuesto(metas_anuales(), entidad()))
    antiguedad_ficha <- shiny::reactive(.in_antiguedad(contexto_crm(), datos_cliente(),
      contactos(), historico(), periodo()[[2]]))
    estado_comercial <- shiny::reactive({
      revision()
      if (entidad()$Tipo != "CLIENTE" && !nrow(historia())) return(NULL)
      tryCatch(.in_clasificacion_cliente(estados(), historia(), metas_anuales(), entidad(), periodo()[[2]],
        principal = familia()$principal), error = function(e) {
          message("[Consulta individual · estados] ", conditionMessage(e))
          list(datos = data.frame(), mes = .gc_mes(periodo()[[2]]), error = TRUE)
        })
    })
    output$identidad <- .crm_render_ui({
      x <- entidad()
      shiny::tagList(.in_ficha_identidad(x, contexto_crm(), contactos(), familia(),
        .in_estado_presupuesto(metas_anuales(), x, periodo()[[2]]),
        .in_clasificacion_badges(estado_comercial(), x)),
        if (x$VinculoEnRevision) .crm_nota("Hay varios NIT vinculados. Revise el vínculo de facturación."))
    })
    tabs_disponibles <- shiny::reactive({
      x <- tryCatch(entidad(), error = function(e) NULL)
      if (is.null(x)) return(character())
      comercial <- x$Tipo == "CLIENTE" || nrow(historia()) > 0L
      crm <- !is.na(contexto_crm()$Codigo)
      potencial <- crm && (x$Tipo %in% c("PROSPECTO", "LEAD", "CLIENTE") ||
        .in_tiene_detalle(fuentes()$potencial, x$Codigo) || .in_tiene_detalle(fuentes()$oportunidades, x$Codigo))
      opciones <- c("Ficha", if (crm) "Gestión y seguimiento",
        if (potencial) "Potencial y oportunidades",
        if (comercial) c("Ejecución y productos", "Histórico completo", "Preferencias de compra",
          "Pendientes", "Cartera", "Fidelidad y valor", "Venta cruzada"))
      opciones
    })
    output$visibilidad_tabs <- shiny::renderUI({
      disponibles <- tabs_disponibles()
      prefijo <- paste0("#", session$ns("contenido"))
      todas <- c("Ficha", "Gestión y seguimiento", "Potencial y oportunidades", "Ejecución y productos",
        "Histórico completo", "Preferencias de compra", "Pendientes", "Cartera", "Fidelidad y valor", "Venta cruzada")
      reglas <- if (!length(disponibles)) paste0(prefijo, "{display:none;}") else {
        vapply(setdiff(todas, disponibles), function(tab) {
          valor <- jsonlite::toJSON(tab, auto_unbox = TRUE)
          paste0(prefijo, " li:has(>a[data-value=", valor, "]),", prefijo,
            " .tab-pane[data-value=", valor, "]{display:none!important;}")
        }, character(1))
      }
      shiny::tags$style(shiny::HTML(paste(reglas, collapse = "\n")))
    })
    shiny::observeEvent(list(tabs_disponibles(), input$detalle_tabs), {
      if (!is.null(input$detalle_tabs) && !input$detalle_tabs %in% tabs_disponibles()) {
        shiny::updateTabsetPanel(session, "detalle_tabs", selected = "Ficha")
      }
    })
    # Los contenedores se montan una sola vez; cambian sus outputs y la disponibilidad de navegación.
    output$contenido <- .crm_render_ui({
      .crm_tabs(
        tabPanel("Ficha", .in_ficha_ui(session$ns)),
        tabPanel("Gestión y seguimiento",
          shiny::uiOutput(session$ns("alcance_seguimiento")),
          .in_panel_ui("gestiones", "Gestiones realizadas", session$ns),
          .in_panel_ui("recordatorios", "Recordatorios", session$ns),
          .in_panel_ui("tareas", "Tareas y notas visibles para usted", session$ns),
          .in_panel_ui("cronologia", "Recorrido comercial completo", session$ns),
          shiny::uiOutput(session$ns("estado_cronologia")),
          .crm_disclosure("Conversiones y alianzas registradas",
            .in_panel_ui("conversion_lead", "Conversión a lead", session$ns),
            .in_panel_ui("conversion_cliente", "Conversión a cliente", session$ns),
            .in_panel_ui("alianzas", "Alianzas comerciales", session$ns))),
        tabPanel("Potencial y oportunidades",
          .crm_nota("Las oportunidades muestran el estado actual guardado para la entidad comercial asociada."),
          .in_panel_ui("potencial", "Potencial registrado", session$ns),
          .in_panel_ui("oportunidades", "Oportunidades de la entidad", session$ns)),
        tabPanel("Ejecución y productos", .gc_tarjetas_ui(session$ns, .IN_CAJAS),
          .crm_plot_output(session$ns("tendencia"), height = "310px"),
          .crm_descarga_ui("descargar", session$ns),
          .crm_tabla_ui(session$ns("productos"), "Marcas y nombres comerciales comprados"),
          .crm_disclosure("Pedidos, precios históricos e indicadores de mercado",
            .crm_nota(c("Sacos y margen corresponden a facturas del histórico hasta el corte del controlbar.",
              "El precio por kilo es el registrado en la cabecera del pedido, no margen por kilo.",
              "Las cabeceras sin precio o fecha quedan vacías; actualice DataPrep para completar la información.")),
            shiny::uiOutput(session$ns("rango_precios")),
            racafeForms::racafeInputNumeric("precio_comparar", "Precio por kilo a comparar (COP)",
              value = 0, min = 0, decimals = 2, ns = session$ns),
            .crm_nota("Escriba un precio positivo. Se buscan los pedidos con precio más cercano de esta cuenta."),
            .crm_tabla_ui(session$ns("pedidos_precios"), "Pedidos y referencias comerciales del histórico"),
            .crm_tabla_ui(session$ns("precio_cercano"), "Pedidos con precio más cercano"),
            shiny::uiOutput(session$ns("fecha_mercado")),
            .crm_tabla_ui(session$ns("mercado_precio"), "Indicadores del día del pedido más cercano")),
          .crm_tabla_ui(session$ns("movimientos"), "Facturación del periodo"),
          .crm_nota(c("El margen de meses abiertos es provisional; un importe desconocido no se convierte en cero.",
            "El periodo corresponde a las fechas seleccionadas en el controlbar."))),
        tabPanel("Histórico completo",
          .crm_nota("Incluye todos los años disponibles hasta el corte del controlbar, con sus filtros comerciales."),
          .gc_tarjetas_ui(session$ns, c(hist_sacos = "Sacos en todo el histórico (70 kg)",
            hist_margen = "Margen histórico FCC (COP)", hist_pedidos = "Pedidos históricos",
            hist_anhos = "Años con información")),
          .crm_plot_output(session$ns("historico_grafico"), height = "330px"),
          .crm_tabla_ui(session$ns("historico_anual"), "Compras y margen por año"),
          .crm_descarga_ui("descargar_historico", session$ns, "Descargar histórico completo")),
        tabPanel("Preferencias de compra",
          .crm_plot_output(session$ns("preferencias_grafico"), height = "330px"),
          .crm_tabla_ui(session$ns("preferencias"), "Qué productos compra, con qué frecuencia y desde cuándo"),
          .crm_nota(c("Ranking basado en las compras históricas de esta cuenta hasta el corte seleccionado.",
            "Las preferencias declaradas en Potencial muestran el último registro disponible."))),
        tabPanel("Pendientes", .crm_tabla_ui(session$ns("pendientes")),
          .crm_nota(c("Incluye pedidos sin asignar y futuros, producción, despacho, facturación y revisión de margen.",
            "Usa el flujo disponible y la fecha final del controlbar como referencia de vencimiento.",
            "Las etapas pueden solaparse: sus cantidades no deben sumarse."))),
        tabPanel("Cartera", shiny::uiOutput(session$ns("corte_cartera")),
          .crm_tabla_ui(session$ns("cartera"))),
        tabPanel("Fidelidad y valor",
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
        tabPanel("Venta cruzada",
          .crm_disclosure("Cómo usar estas propuestas",
            .crm_nota(c("Son productos que otras cuentas comparables compran junto con los de esta cuenta.",
              "Solo se proponen productos que esta cuenta aún no registra en su historia observada.",
              "Se requieren al menos dos cuentas que respalden la combinación y afinidad mayor que 1.",
              "Las compras pueden ser de pedidos y años distintos. Confirme interés, calidad y presentación."))),
          shiny::uiOutput(session$ns("venta_cruzada"))),
        id = session$ns("detalle_tabs"), selected = "Ficha")
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
    output[["ficha-tabla"]] <- .crm_render_ui(.in_ficha_crm_ui(
      .in_ficha_crm_registros(contexto_crm(), contactos())))
    output$presupuesto_ficha <- .crm_render_ui(.in_ficha_presupuesto_ui(presupuesto_ficha(),
      .in_estado_presupuesto(metas_anuales(), entidad(), periodo()[[2]]),
      .crm_tabla_ui(session$ns("presupuesto"), searchable = FALSE), ns = session$ns))
    output[["antiguedad-tabla"]] <- .crm_render_ui(.in_ficha_antiguedad_ui(antiguedad_ficha(), ns = session$ns))
    output[["contacto_sistema-tabla"]] <- .crm_render_ui(shiny::tags$div(id = session$ns("geografia-tabla"),
      .in_ficha_contacto_ui(datos_cliente())))
    output[["familia-tabla"]] <- .crm_render_ui(.in_ficha_familia_ui(familia()))
    output$familia_contador <- .crm_render_ui(racafeForms::racafeStatusBadge(
      paste(length(familia()$hijos), if (length(familia()$hijos) == 1L) "hija visible" else {
        "hijas visibles"
      }), variant = "neutral"))
    complementos_ficha <- shiny::reactive(.in_ficha_complementos(fuentes(), contexto_crm()))
    output$complementarios_contador <- .crm_render_ui(.in_ficha_complementos_badge(complementos_ficha()))
    output$complementarios <- .crm_render_ui(.in_ficha_complementos_ui(complementos_ficha(), session$ns))
    for (nombre in names(.IN_FUENTES)) local({
      clave <- nombre
      .in_conteo(output, clave, shiny::reactive(.in_detalle_crm(fuentes()[[clave]], contexto_crm(),
        .IN_FUENTES[[clave]]$campos)))
      if (clave == "oportunidades") {
        .crm_vancouver_server(clave, shiny::reactive(.in_oportunidades(fuentes()[[clave]], contexto_crm())),
          col_labels = c(Creacion = "Fecha de creación", FechaObjetivo = "Fecha objetivo", Linea = "Línea de negocio",
            SacosObjetivo = "Sacos objetivo (unidad declarada)", MargenObjetivoKg = "Margen objetivo por kg (COP)",
            EstadoActual = "Estado actual del registro"))
      } else .crm_vancouver_server(clave, shiny::reactive(.in_detalle_crm(fuentes()[[clave]], contexto_crm(),
        .IN_FUENTES[[clave]]$campos)))
    })
    tareas_entidad <- shiny::reactive({
      x <- .tareas_visibles_usuario(tareas(), usuario(), es_admin())
      shiny::validate(shiny::need(!nrow(x) || "CodContacto" %in% names(x),
        "Las tareas existentes no tienen un vínculo de entidad. Consúltelas en Notas y Tareas."))
      .in_detalle_crm(list(datos = x), contexto_crm(), c("titulo", "Descripcion", "Responsable", "Prioridad",
        "FechaCumplimiento", "Cumplido", "Archivado"))
    })
    .crm_vancouver_server("tareas", tareas_entidad)
    .in_conteo(output, "tareas", tareas_entidad)
    .gc_tarjetas_server(.IN_CAJAS, valores, list(sacos = "numero", margen = "dinero"))
    .crm_vancouver_server("presupuesto", presupuesto_ficha,
      col_labels = c(MetaMensualSacos = "Meta mensual · sacos 70 kg", MetaMensualMargen = "Meta mensual · margen COP",
        FecProceso = "Actualización", PresupuestoEnRevision = "Revisión del presupuesto"),
      col_specs = list(MetaMensualSacos = list(cell = .in_ficha_numero),
        MetaMensualMargen = list(cell = function(x) .in_ficha_numero(x, moneda = TRUE)),
        PresupuestoEnRevision = list(cell = function(x) {
        if (is.na(x)) "Sin información" else if (x) "Sí" else "No"
      })))
    productos <- shiny::reactive({
      ejecucion() %>%
        dplyr::mutate(Marca = as.character(.gc_columna(ejecucion(), "Marca")),
          NombreComercial = as.character(.gc_columna(ejecucion(), "MCNom"))) %>%
        dplyr::group_by(Marca, NombreComercial) %>%
        dplyr::summarise(Sacos70kg = .com_suma(SacosFact), MargenFCC = .com_suma(Margen), .groups = "drop")
    })
    .crm_vancouver_server("productos", productos,
      col_labels = c(Marca = "Marca", NombreComercial = "Nombre comercial", Sacos70kg = "Sacos facturados (70 kg)",
        MargenFCC = "Margen total FCC (COP)"))
    .crm_vancouver_server("movimientos", shiny::reactive({
      x <- ejecucion()
      .crm_nits_tabla(x[intersect(c("CLCliNit", "CLPdcCod", "CLLotCod", "FecFact", "CLLinNegNo", "Categoria",
        "Producto", "SacosFact", "Margen", "EstadoFinanciero"), names(x))])
    }), col_labels = c(CLCliNit = "NIT", CLPdcCod = "Pedido", CLLotCod = "Lote", FecFact = "Fecha de factura",
      CLLinNegNo = "Línea", SacosFact = "Sacos 70 kg", EstadoFinanciero = "Estado financiero"))
    pedidos_precios <- shiny::reactive(.in_precios_pedidos(historico()))
    cercano <- shiny::reactive(.in_precio_cercano(pedidos_precios(), input$precio_comparar))
    .crm_vancouver_server("pedidos_precios", pedidos_precios,
      col_labels = c(PrecioKg = "Precio del pedido por kg (COP)", Sacos = "Sacos facturados (70 kg)",
        Margen = "Margen total FCC (COP)", FechaPedido = "Creación del pedido", UltimaFactura = "Última factura"))
    .crm_vancouver_server("precio_cercano", cercano,
      col_labels = c(PrecioKg = "Precio histórico por kg (COP)", DiferenciaCOP = "Diferencia por kg (COP)",
        Sacos = "Sacos facturados (70 kg)", FechaPedido = "Creación del pedido"))
    output$rango_precios <- .crm_render_ui({
      precios <- pedidos_precios()$PrecioKg
      precios <- precios[is.finite(precios) & precios > 0]
      if (!length(precios)) return(.crm_nota("No hay precios de pedido registrados para este histórico."))
      shiny::tags$div(class = "crm-identidad-badges",
        racafeForms::racafeDataBadge("Precio mínimo / kg", paste("COP", format(min(precios), big.mark = ","))),
        racafeForms::racafeDataBadge("Precio máximo / kg", paste("COP", format(max(precios), big.mark = ","))))
    })
    indicadores_historicos <- shiny::reactive({ revision(); .in_fuente(cargar, "CRMINDICADORES") })
    mercado_pedido <- shiny::reactive({
      x <- cercano()
      if (!nrow(x)) return(data.frame())
      .in_mercado_fecha(indicadores_historicos(), x$FechaPedido[[1]])
    })
    .crm_vancouver_server("mercado_precio", mercado_pedido,
      col_labels = c(FechaActualizacion = "Fecha del indicador", Valor = "Valor registrado"))
    output$fecha_mercado <- .crm_render_ui({
      x <- cercano()
      if (!nrow(x)) return(.crm_nota("No hay pedidos con precio válido para comparar; escriba un precio positivo."))
      if (is.na(x$FechaPedido[[1]])) return(.crm_nota("El pedido más cercano no tiene fecha de creación registrada."))
      .crm_nota(c(paste("Referencia: pedido", x$Pedido[[1]], "del", x$FechaPedido[[1]],
        "· Si hay empate de precios se usa el pedido más reciente con fecha registrada."),
        "Los indicadores corresponden exactamente a ese día; no se sustituyen por cotizaciones de otra fecha.",
        if (!nrow(mercado_pedido())) "No hay indicadores guardados para ese día."))
    })
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
    cronologia <- shiny::reactive(.in_cronologia(contexto_crm(), contactos(), fuentes(), periodo()[[2]]))
    .in_conteo(output, "cronologia", cronologia)
    .crm_vancouver_server("cronologia", cronologia,
      columnas = c("Momento", "EntidadCRM", "Evento", "Detalle", "Responsable", "Origen"),
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
