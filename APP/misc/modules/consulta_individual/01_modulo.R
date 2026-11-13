# Consulta integral de una entidad, sin modificar registros ----
.IN_CAJAS <- c(pedidos = "Pedidos únicos", lineas = "Líneas de pedido únicas", lotes = "Lotes únicos",
  facturas = "Facturas identificadas", productos = "Productos diferentes", sacos = "Sacos facturados · 70 kg",
  margen = "Margen FCC · COP", precio = "Precio medio de pedido · COP/kg")
ConsultaIndividualUI <- function(id) {
  ns <- shiny::NS(id)
  .accion_formulario_ui(id, shiny::tags$div(class = "in-analitica",
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = NULL),
    .crm_select("entidad", "Buscar por nombre o NIT de facturación",
      choices = character(), ns = ns),
    shiny::uiOutput(ns("identidad")), shiny::uiOutput(ns("visibilidad_tabs")), shiny::uiOutput(ns("contenido")))))
}
ConsultaIndividual <- function(id, hechos, clientes, filtros, usuario, es_admin, nit_mapa,
                               cartera = function() NULL, tareas = function() data.frame(),
                               cargar = racafeBD::CargarDatos, directorio = function() data.frame(),
                               estados = function() data.frame(), solicitud = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    mapa_nit <- shiny::reactive(if (shiny::is.reactive(nit_mapa)) nit_mapa() else nit_mapa)
    revision <- shiny::reactiveVal(0L)
    revision_modelos <- shiny::reactiveVal(0L)
    shiny::observeEvent(.pr_fuentes()$revision_total(), revision(shiny::isolate(revision()) + 1L), ignoreInit = TRUE)
    solicitud_atendida <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$actualizar, revision(revision() + 1L))
    contactos <- shiny::reactive({
      revision()
      fuente <- .in_fuente(cargar, "CRMNALCONTACTO")
      shiny::validate(shiny::need(is.null(fuente$error), fuente$error))
      fuente$datos
    })
    .fuentes_cache <- lapply(.IN_FUENTES, function(fuente) {
      force(fuente)
      shiny::reactive({ revision(); .in_fuente(cargar, fuente$tabla) })
    })
    fuentes <- shiny::reactive(lapply(.fuentes_cache, function(fuente) fuente()))
    vinculos <- shiny::reactive({ revision(); .in_fuente(cargar, "CRMNALVINCULONIT") })
    periodo <- shiny::reactive({
      x <- as.Date(filtros()$fecha)
      shiny::validate(shiny::need(length(x) == 2L && !anyNA(x) && x[[1]] <= x[[2]],
        "Seleccione fechas válidas para el Período de análisis."))
      x
    })
    catalogo_clientes <- shiny::reactive(.com_clientes(clientes(), periodo()[[2]], filtros()))
    catalogo <- shiny::reactive({
      crm <- contactos()
      .in_catalogo(crm, catalogo_clientes(), data.frame(), vinculos()$datos, usuario(), es_admin(),
        filtros(), .gc_filtrar_dimensiones(hechos(), filtros()), directorio(),
        asignaciones = .com_clientes(clientes(), periodo()[[2]]))
    })
    shiny::observeEvent(list(catalogo(), solicitud(), input$FormularioListo), {
      shiny::req(input$FormularioListo)
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
          seleccion <- character()
          shiny::showNotification("La entidad no está disponible para el alcance actual.", type = "warning")
        }
        solicitud_atendida(destino)
      }
      .crm_update_select(session, "entidad", choices = opciones, selected = seleccion)
    })
    entidad <- shiny::reactive({
      x <- .in_elegida(catalogo(), input$entidad)
      shiny::validate(shiny::need(nrow(x) == 1L, "Seleccione una entidad disponible para su alcance comercial."))
      x
    })
    contexto_crm <- shiny::reactive(.in_contexto_crm(entidad(), catalogo()))
    familia <- shiny::reactive(.in_familia(entidad(), catalogo(), mapa_nit(), directorio()))
    metas_anuales <- shiny::reactive(.com_clientes(clientes(), periodo()[[2]], filtros(), anual = TRUE))
    historia <- shiny::reactive(.in_hechos(.gc_filtrar_dimensiones(hechos(), filtros()), entidad()))
    historia_completa <- shiny::reactive({
      x <- historia()
      fechas <- x$FecFact[!is.na(x$FecFact) & x$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO")]
      if (!length(fechas)) return(x[0, , drop = FALSE])
      .in_periodo(x, range(fechas))
    })
    periodo_ejecucion <- shiny::reactive({
      if (identical(input$alcance_periodo, "historico") && nrow(historia_completa())) {
        range(historia_completa()$FecFact)
      } else periodo()
    })
    ejecucion <- shiny::reactive(.in_periodo(historia(), periodo_ejecucion()))
    valores <- shiny::reactive(.in_kpi_ejecucion(ejecucion()))
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
        if (nrow(historia()) || (x$Tipo == "CLIENTE" && nrow(presupuesto_ficha()))) .in_estado_presupuesto(metas_anuales(), x, periodo()[[2]]) else NULL,
        .in_clasificacion_badges(estado_comercial(), x)),
        if (x$VinculoEnRevision) .crm_nota("Hay varios NIT vinculados. Revise el vínculo de facturación."))
    })
    tabs_disponibles <- shiny::reactive({
      x <- tryCatch(entidad(), error = function(e) NULL)
      if (is.null(x)) return(character())
      comercial <- nrow(historia()) > 0L
      crm <- !is.na(contexto_crm()$Codigo)
      potencial <- crm
      opciones <- c("Ficha", if (crm) "Gestión y seguimiento",
        if (potencial) "Potencial y oportunidades",
        if (comercial) c("Ejecución y productos", "Preferencias de compra",
          "Pendientes", "Cartera", "Fidelidad y valor", "Venta cruzada"))
      opciones
    })
    output$visibilidad_tabs <- shiny::renderUI({
      disponibles <- tabs_disponibles()
      prefijo <- paste0("#", session$ns("contenido"))
      todas <- c("Ficha", "Gestión y seguimiento", "Potencial y oportunidades", "Ejecución y productos",
        "Preferencias de compra", "Pendientes", "Cartera", "Fidelidad y valor", "Venta cruzada")
      reglas <- if (!length(disponibles)) paste0(prefijo, "{display:none;}") else {
        vapply(setdiff(todas, disponibles), function(tab) {
          valor <- jsonlite::toJSON(tab, auto_unbox = TRUE)
          paste0(prefijo, " li:has(>a[data-value=", valor, "]),", prefijo,
            " .tab-pane[data-value=", valor, "]{display:none!important;}")
        }, character(1))
      }
      x <- tryCatch(entidad(), error = function(e) NULL)
      if (!is.null(x) && !nrow(historia()) && x$Tipo != "CLIENTE") {
        reglas <- c(reglas, paste0("#", session$ns("ficha_box_presupuesto"), "{display:none!important;}"))
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
          .gc_tarjetas_ui(session$ns, c(seg_gestiones = "Gestiones registradas", seg_recordatorios = "Recordatorios",
            seg_tareas = "Tareas y notas", seg_eventos = "Eventos en el recorrido")),
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
          .gc_tarjetas_ui(session$ns, c(op_total = "Oportunidades registradas", op_activas = "Sin descartar",
            op_descartadas = "Descartadas", op_vencidas = "Fecha objetivo superada")),
          .crm_plot_output(session$ns("oportunidades_fechas")),
          .crm_nota("Las oportunidades muestran el estado actual guardado para la entidad comercial asociada."),
          .in_panel_ui("potencial", "Potencial registrado", session$ns),
          .in_panel_ui("oportunidades", "Oportunidades de la entidad", session$ns)),
        tabPanel("Ejecución y productos",
          racafeForms::racafeInputRadio("alcance_periodo", "Cobertura de ejecución",
            choices = c("Período de análisis" = "analisis", "Histórico completo" = "historico"),
            selected = "analisis", ns = session$ns),
          shiny::uiOutput(session$ns("cobertura_ejecucion")),
          shiny::fluidRow(lapply(names(.IN_CAJAS), function(clave) shiny::column(3,
            .crm_caja_output(session$ns(clave))))),
          .crm_select("variable_tendencia", "Variable de la gráfica",
            choices = c("SACOS" = "Sacos", "MARGEN" = "Margen", "PRECIO POR KG DEL PEDIDO" = "PrecioKg"),
            selected = "Sacos", ns = session$ns, empty = FALSE),
          .crm_plot_output(session$ns("tendencia"), height = "450px"),
          .crm_select("dimension_ejecucion", "Agrupar ejecución por", choices = c(
            "Categoría y producto" = "producto", "Categoría" = "categoria", "Línea de negocio" = "linea"),
            selected = "producto", ns = session$ns),
          .crm_plot_output(session$ns("productos_treemap")),
          .crm_tabla_ui(session$ns("resumen_productos"), "Ejecución por dimensión"),
          .crm_disclosure("Cómo se calculan los precios y las participaciones",
            .crm_nota(c("Cada línea negociada con precio y unidad verificados tiene el mismo peso; los lotes y facturas no lo multiplican.",
              "Un pedido puede aparecer en varios grupos: no sume sus conteos entre productos.",
              "Las participaciones usan los totales del período y de la entidad. Margen total cero: participación no disponible.",
              "El treemap omite tamaños negativos o inválidos; las tablas conservan todos los importes."))),
          .crm_disclosure("Comparador histórico por precio de pedido",
            .crm_nota(c("Se compara toda la historia disponible de la entidad dentro de su alcance comercial.",
              "El precio negociado proviene de la línea del pedido cuando su moneda y unidad están verificadas. No es margen ni precio neto de factura.")),
            shiny::uiOutput(session$ns("rango_precios")),
            racafeForms::racafeInputNumeric("precio_comparar", "Precio objetivo · COP/kg", value = 0, min = 0,
              decimals = 2, ns = session$ns),
            racafeForms::racafeButton("aplicar_precio", "Aplicar", icon = "calculator", ns = session$ns),
            shiny::uiOutput(session$ns("estado_precio")),
            .crm_tabla_ui(session$ns("precio_cercano"), "Transacciones con precios más cercanos"),
            shiny::uiOutput(session$ns("fecha_mercado")),
            .crm_tabla_ui(session$ns("mercado_precio"), "Indicadores de mercado en el día de la factura"))),
        tabPanel("Preferencias de compra",
          .crm_plot_output(session$ns("preferencias_grafico"), height = "450px"),
          .crm_tabla_ui(session$ns("preferencias"), "Qué productos compra, con qué frecuencia y desde cuándo"),
          .crm_nota(c("Ranking basado en las compras históricas de esta cuenta hasta el corte seleccionado.",
            "Las preferencias declaradas en Potencial muestran el último registro disponible."))),
        tabPanel("Pendientes", lapply(.PEN_TIPOS, function(tipo) {
          mid <- paste0("pendiente_", match(tipo, .PEN_TIPOS))
          .crm_disclosure(tipo, .crm_tabla_ui(session$ns(paste0(mid, "resumen"))),
            .crm_tabla_ui(session$ns(mid)))
        }), .crm_disclosure("Cómo leer los pendientes", .crm_nota(c(
          "Las categorías pueden solaparse: no sume sus saldos como un total único.",
          "Sacos no disponibles cuando se mezclan niveles operativos; consulte cada registro.",
          "Los pedidos fuera de cobertura verificable se separan como información incompleta.")))),
        tabPanel("Cartera", shiny::uiOutput(session$ns("corte_cartera")),
          .crm_plot_output(session$ns("cartera_cosechas")),
          shiny::uiOutput(session$ns("limite_cosechas")),
          .crm_disclosure("Observaciones que sustentan las cosechas", .crm_tabla_ui(session$ns("cosechas_detalle"))),
          .gc_tarjetas_ui(session$ns, c(car_saldo = "Saldo documentado · COP", car_vencido = "Saldo vencido · COP",
            car_mora = "Mora máxima · días", car_puntualidad = "Pagos puntuales acreditados")),




          .crm_tabla_ui(session$ns("cartera_concentracion"), "Concentración del saldo por documento"),
          .crm_tabla_ui(session$ns("pagos_detalle"), "Facturas y pagos totales acreditados"),
          .crm_tabla_ui(session$ns("cartera"), "Cartera al último corte disponible"),
          .crm_nota(c("Cada corte es una fotografía: no se suman saldos de fechas diferentes.",
            "Una reducción del saldo no prueba un pago. Solo se acredita pago con fecha y saldo conciliado.",
            "Los cambios entre cortes pueden deberse a movimientos y cobertura, no solo a deterioro o mejoría."))),
        tabPanel("Fidelidad y valor",
          .crm_disclosure("¿Qué me dicen estos indicadores?",
            .crm_nota(c("RFM resume tres preguntas: ¿compró hace poco?, ¿compra seguido?, ¿cuánto margen deja?",
              "Las notas van de 1 a 5 frente a los otros clientes visibles; 5 es el valor más favorable.",
              "BG/NBD usa el ritmo de compras para estimar si el cliente seguirá comprando y cuántas veces volvería.",
              "CLV traduce esas compras esperadas a margen futuro estimado. No es una venta asegurada.",
              "Por ejemplo, una actividad de 80% es una estimación del modelo; no confirma que exista un pedido.",
              "La cuenta hija se analiza por su NIT; la vista consolidada agrupa las cuentas relacionadas visibles.",
              "Se usa toda la historia hasta el corte. Las estimaciones no son pedidos confirmados."))),
          .crm_plot_output(session$ns("rfm_radar")),
          .crm_tabla_ui(session$ns("rfm_perfil"), "RFM: valores, puntajes y rangos observados"),
          .crm_tabla_ui(session$ns("rfm"), "Hábitos de compra de la cuenta consultada"),
          .crm_plot_output(session$ns("rfm_evolucion")),
          .crm_tabla_ui(session$ns("rfm_historia"), "Cortes históricos RFM de margen"),
          .crm_nota("Los cortes históricos del ETL usan ventanas de 12 meses y su población de referencia; el perfil actual usa toda la historia visible."),
          racafeForms::racafeButton("modelar", "Recalcular estimación a 12 meses", ns = session$ns),
          shiny::uiOutput(session$ns("modelo")),
          .crm_plot_output(session$ns("modelo_distribucion")),
          .crm_plot_output(session$ns("modelo_compras")),
          .crm_disclosure("Detalle del ajuste BG/NBD",
            .crm_tabla_ui(session$ns("modelo_parametros"), "Parámetros estimados de la población"),
            .crm_nota(c("r y alpha describen diferencias en el ritmo de compra; a y b describen la propensión a dejar de comprar.",
              "Estos parámetros calibran el modelo de la población. No son metas asignadas a este cliente."))),
          .crm_tabla_ui(session$ns("modelo_intervalos"), "Incertidumbre predictiva de compras"),
          .crm_nota(c("La distribución predictiva usa 4.000 simulaciones condicionales BG/NBD a 12 meses.",
            "El rango predictivo incluye variación de compras, no incertidumbre de parámetros ni garantías comerciales.",
            "El radar compara percentiles de la población visible: más alto siempre es más favorable.",
            "Los límites RFM son los valores observados en cada puntaje, no umbrales universales."))),
        tabPanel("Venta cruzada",
          .crm_disclosure("Cómo usar estas propuestas",
            .crm_nota(c("Son productos que otras cuentas comparables compran junto con los de esta cuenta.",
              "Solo se proponen productos que esta cuenta aún no registra en su historia observada.",
              "Se requieren al menos dos pedidos que respalden la combinación y afinidad mayor que 1.",
              "Cada cesta es un pedido de un NIT. Una asociación no demuestra causalidad ni asegura una venta futura.",
              "Soporte: de cada 100 pedidos, cuántos incluyen ambos productos.",
              "Confianza: de cada 100 pedidos con A, cuántos también incluyen B.",
              "Lift mayor que 1: aparecen juntos más de lo habitual; no demuestra que ofrecer B cause una compra."))),
          .gc_tarjetas_ui(session$ns, c(mba_transacciones = "Pedidos analizados de la entidad",
            mba_productos = "Productos diferentes comprados", mba_reglas = "Reglas con evidencia",
            mba_propuestas = "Productos complementarios propuestos")),
          .crm_plot_output(session$ns("mba_frecuencias")),
          .crm_tabla_ui(session$ns("mba_combinaciones"), "Combinaciones frecuentes en la población comparable"),
          .crm_plot_output(session$ns("mba_reglas_grafico")),
          .crm_tabla_ui(session$ns("mba_reglas"), "Reglas: soporte, confianza, lift y pedidos que las respaldan"),
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
    output[["antiguedad-tabla"]] <- .crm_render_ui({
      if (!nrow(historia())) {
        x <- .in_ficha_crm_registros(contexto_crm(), contactos())
        return(.in_ficha_info(list("Antigüedad desde el registro en CRM" =
          .in_edad(.gc_fecha_min(.in_fecha(.gc_columna(x, "FechaHoraCrea"))), periodo()[[2]]))))
      }
      .in_ficha_antiguedad_ui(antiguedad_ficha(), ns = session$ns)
    })
    output[["contacto_sistema-tabla"]] <- .crm_render_ui({
      if (!nrow(historia()) && entidad()$Tipo != "CLIENTE" && contexto_crm()$OrigenCRM == "Propio") {
        obtener <- function(clave) .in_detalle_crm(.fuentes_cache[[clave]](), contexto_crm(), .IN_FUENTES[[clave]]$campos)
        empresa <- obtener("directorio"); telefonos <- obtener("telefonos"); ubicaciones <- obtener("ubicaciones")
        return(.in_ficha_info(list("Correos registrados" = .gc_columna(empresa, "CorreoGeneral"),
          "Teléfonos registrados" = .gc_columna(telefonos, c("Telefono", "Numero")),
          "Direcciones" = .gc_columna(ubicaciones, "Direccion"),
          "Ciudad" = .gc_columna(ubicaciones, c("Municipio", "Mpio")),
          "Departamento" = .gc_columna(ubicaciones, c("Departamento", "Depto"))), ocultar_vacios = TRUE))
      }
      .in_ficha_contacto_ui(datos_cliente())
    })
    output[["familia-tabla"]] <- .crm_render_ui(.in_ficha_familia_ui(familia()))
    output$familia_contador <- .crm_render_ui(racafeForms::racafeStatusBadge(
      paste(length(familia()$hijos), if (length(familia()$hijos) == 1L) "hija visible" else {
        "hijas visibles"
      }), variant = "neutral"))
    complementos_ficha <- shiny::reactive(.in_ficha_complementos(lapply(.fuentes_cache[names(.IN_COMPLEMENTOS)], function(f) f()), contexto_crm()))
    output$complementarios_contador <- .crm_render_ui(.in_ficha_complementos_badge(complementos_ficha()))
    output$complementarios <- .crm_render_ui(.in_ficha_complementos_ui(complementos_ficha(), session$ns))
    for (nombre in names(.IN_FUENTES)) local({
      clave <- nombre
      .in_conteo(output, clave, shiny::reactive(.in_detalle_crm(.fuentes_cache[[clave]](), contexto_crm(),
        .IN_FUENTES[[clave]]$campos)))
      if (clave == "oportunidades") {
        .crm_vancouver_server(clave, shiny::reactive(.in_oportunidades(.fuentes_cache[[clave]](), contexto_crm())),
          col_labels = c(Creacion = "Fecha de creación", FechaObjetivo = "Fecha objetivo", Linea = "Línea de negocio",
            SacosObjetivo = "Sacos objetivo (unidad declarada)", MargenObjetivoKg = "Margen objetivo por kg (COP)",
            EstadoActual = "Estado actual del registro"))
      } else .crm_vancouver_server(clave, shiny::reactive(.in_detalle_crm(.fuentes_cache[[clave]](), contexto_crm(),
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
    for (id in names(.IN_CAJAS)) local({
      clave <- id
      .crm_caja(clave, shiny::reactive(valores()[[clave]]), texto = .IN_CAJAS[[clave]],
        icono = "chart-simple", formato = if (clave %in% c("margen", "precio")) "dinero" else "numero",
        mostrar_boton = TRUE, titulo_modal = .IN_CAJAS[[clave]], contenido_modal = function() {
          if (!is.finite(valores()[[clave]])) return(.crm_nota(
            "No hay identificadores o valores suficientes en la fuente para calcular este indicador."))
          .crm_tabla_ui(session$ns(paste0("detalle_kpi_", clave)))
        })
      .crm_vancouver_server(paste0("detalle_kpi_", clave), shiny::reactive({
        .in_detalle_indicador(ejecucion(), clave)
      }))
    })
    output$cobertura_ejecucion <- .crm_render_ui(.crm_nota(c(paste("Período de análisis:",
      paste(periodo_ejecucion(), collapse = " a ")),
      if (is.na(valores()$facturas)) "No se recibieron identificadores de factura: ese conteo no está disponible.",
      "Los productos se cuentan por código comercial y línea cuando ese identificador está disponible.")))
    .crm_vancouver_server("presupuesto", presupuesto_ficha,
      col_labels = c(MetaMensualSacos = "Meta mensual · sacos 70 kg", MetaMensualMargen = "Meta mensual · margen COP",
        FecProceso = "Actualización", PresupuestoEnRevision = "Revisión del presupuesto"),
      col_specs = list(MetaMensualSacos = list(cell = .in_ficha_numero),
        MetaMensualMargen = list(cell = function(x) .in_ficha_numero(x, moneda = TRUE)),
        PresupuestoEnRevision = list(cell = function(x) {
        if (is.na(x)) "Sin información" else if (x) "Sí" else "No"
      })))
    dimensiones_ejecucion <- shiny::reactive(switch(input$dimension_ejecucion %||% "producto",
      categoria = c("LinNegCod", "Categoria"), linea = "LinNegCod", c("LinNegCod", "Categoria", "Producto")))
    .crm_vancouver_server("resumen_productos", shiny::reactive(.crm_resumen_dimension(ejecucion(), dimensiones_ejecucion())),
      col_specs = list(ParticipacionSacos = list(format = list(percent = TRUE, digits = 1)),
        ParticipacionMargen = list(format = list(percent = TRUE, digits = 1))))
    output$productos_treemap <- .crm_render_plotly({
      .crm_treemap_productos(.crm_resumen_dimension(ejecucion(), c("Categoria", "Producto")))
    })
    pedidos_precios <- shiny::reactive(.in_precios_pedidos(historia_completa()))
    transacciones_precio <- shiny::reactive(.in_transacciones_precio(historia_completa()))
    captura_precio <- shiny::reactiveVal(NULL)
    validacion_precio <- .accion_validacion(input, session, shiny::reactive(entidad()$Clave), "precio_comparar",
      function() c("Ingrese un precio positivo" = !isTRUE(length(input$precio_comparar) == 1L &&
        is.finite(input$precio_comparar) && input$precio_comparar > 0)))
    shiny::observeEvent(input$aplicar_precio, {
      if (!validacion_precio$validate()) return(invisible(NULL))
      captura_precio(list(precio = input$precio_comparar, entidad = entidad()$Clave, revision = revision(), instante = Sys.time()))
    })
    resultado_precio <- shiny::eventReactive(captura_precio(), {
      x <- captura_precio(); .in_buscar_precio(transacciones_precio(), x$precio)
    })
    cercano <- shiny::reactive({
      x <- captura_precio()
      shiny::validate(shiny::need(!is.null(x), "Ingrese un precio y pulse Aplicar."),
        shiny::need(identical(x$entidad, entidad()$Clave) && identical(x$revision, revision()),
          "La entidad o la fuente cambió. Pulse Aplicar nuevamente."))
      resultado_precio()
    })
    output$estado_precio <- .crm_render_ui({
      x <- captura_precio()
      .crm_nota(if (is.null(x) || !identical(x$entidad, entidad()$Clave)) "No hay comparación aplicada a esta entidad." else
        if (!identical(x$precio, input$precio_comparar)) "Precio modificado: el resultado anterior se mantiene hasta pulsar Aplicar." else
          paste("Precio aplicado:", x$precio, "COP/kg"))
    })
    .crm_vancouver_server("precio_cercano", cercano,
      col_specs = list(DiferenciaPct = list(format = list(percent = TRUE, digits = 2))),
      col_labels = c(PrecioKg = "Precio histórico por kg (COP)", DiferenciaCOP = "Diferencia por kg (COP)",
        Sacos = "Sacos facturados (70 kg)", FecFact = "Fecha de factura", DiferenciaPct = "Diferencia / precio objetivo"))
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
      .in_mercado_fecha(indicadores_historicos(), x$FecFact[[1]])
    })
    .crm_vancouver_server("mercado_precio", mercado_pedido,
      col_labels = c(FechaActualizacion = "Fecha del indicador", Valor = "Valor registrado"))
    output$fecha_mercado <- .crm_render_ui({
      x <- cercano()
      if (!nrow(x)) return(.crm_nota("No hay pedidos con precio válido para comparar; escriba un precio positivo."))
      if (is.na(x$FecFact[[1]])) return(.crm_nota("La transacción más cercana no tiene fecha de factura."))
      .crm_nota(c(paste("Referencia: pedido", x$Pedido[[1]], "del", x$FecFact[[1]],
        "· Desempate: factura reciente, NIT, pedido, lote y producto."),
        "Los indicadores corresponden exactamente a ese día; no se sustituyen por cotizaciones de otra fecha.",
        if (!nrow(mercado_pedido())) "No hay indicadores guardados para ese día."))
    })
    pendientes_entidad <- shiny::reactive(.pen_tipificar(.pen_preparar(historia(), periodo()[[2]])$detalle, historia()))
    for (tipo in .PEN_TIPOS) local({
      grupo <- tipo; mid <- paste0("pendiente_", match(grupo, .PEN_TIPOS))
      d <- shiny::reactive({ x <- pendientes_entidad(); x[x$TipoPendiente == grupo, , drop = FALSE] })
      .crm_vancouver_server(paste0(mid, "resumen"), shiny::reactive(.pen_resumen_tipo(d())))
      seleccion_tabla <- .crm_tabla_servidor(mid, data = d, columnas = c("Pedido", "Linea", "Lote", "Fecha", "Nivel", "Cantidad", "Motivo"),
        modo_seleccion = "fila", id_col = "Registro", modal_titulo_fn = function(sel) "Trazabilidad del pedido",
        modal_contenido_fn = function(sel) TrazabilidadPedidoUI(session$ns(paste0(mid, "traza"))))
      TrazabilidadPedido(paste0(mid, "traza"), historia, shiny::reactive({
        seleccion <- seleccion_tabla$seleccion()
        if (is.null(seleccion)) NULL else seleccion$fila$Pedido
      }))
    })
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
      x <- .car_scope(h$cartera, clientes_cartera(), mapa_nit())
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
    modelo_nit <- shiny::reactive({ shiny::req(input$detalle_tabs == "Fidelidad y valor"); revision_modelos(); calcular_modelo(poblacion_nit()) })
    modelo_principal <- shiny::reactive({ shiny::req(input$detalle_tabs == "Fidelidad y valor"); revision_modelos(); calcular_modelo(poblacion_principal()) })
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
      shiny::validate(shiny::need(nrow(x) > 0, "SIN FACTURACIÓN PARA EL PERÍODO DE ANÁLISIS."))
      variable <- if (isTRUE(input$variable_tendencia %in% c("Margen", "PrecioKg"))) input$variable_tendencia else "Sacos"
      if (variable == "PrecioKg") {
        x <- .in_precios_pedidos(x) %>% dplyr::group_by(NIT, Pedido) %>%
          dplyr::summarise(Fecha = .gc_fecha_min(UltimaFactura), PrecioKg = .gc_unico(PrecioKg), .groups = "drop")
        x <- x %>% dplyr::filter(is.finite(PrecioKg), PrecioKg > 0, !is.na(Fecha)) %>%
          dplyr::mutate(Mes = .gc_mes(Fecha)) %>% dplyr::group_by(Mes) %>%
          dplyr::summarise(Valor = mean(PrecioKg), .groups = "drop")
        unidad <- "Precio medio de pedido · COP/kg"
      } else {
        x <- x %>% dplyr::mutate(Mes = .gc_mes(FecFact)) %>% dplyr::group_by(Mes) %>%
          dplyr::summarise(Valor = .com_suma(if (variable == "Margen") Margen else SacosFact), .groups = "drop")
        unidad <- if (variable == "Margen") "Margen FCC · COP" else "Sacos facturados · 70 kg"
      }
      shiny::validate(shiny::need(nrow(x) > 0L, "NO HAY VALORES OBSERVADOS PARA ESTA VARIABLE."))
      x$Lectura <- paste(unidad, format(x$Valor, big.mark = ","))
      plotly::plot_ly(x, x = ~Mes, y = ~Valor, type = "scatter", mode = "lines+markers",
        line = list(color = "#7A2028"), text = ~Lectura,
        hovertemplate = "%{x|%Y-%m}<br>%{text}<extra></extra>") %>%
        plotly::layout(title = list(text = "Evolución mensual · Período de análisis", font = list(size = 16)),
          xaxis = list(title = "Mes"), yaxis = list(title = unidad), font = list(size = 12))
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
    preferencias <- shiny::reactive(.in_preferencias(historico()))
    .crm_vancouver_server("preferencias", preferencias,
      col_labels = c(Sacos = "Sacos netos (70 kg)", Margen = "Margen FCC (COP)", DiasConCompra = "Días con compra",
        PrimeraCompra = "Primera compra del producto", UltimaCompra = "Última compra observada"))
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
    venta_nit <- shiny::reactive({ shiny::req(input$detalle_tabs == "Venta cruzada")
      .in_venta_base(hechos(), filtros(), periodo()[[2]], "nit") })
    venta_principal <- shiny::reactive({ shiny::req(input$detalle_tabs == "Venta cruzada")
      .in_venta_base(hechos(), filtros(), periodo()[[2]], "principal") })
    output$venta_cruzada <- .crm_render_ui({
      base <- if (nivel() == "nit") venta_nit() else venta_principal()
      x <- .in_propuestas(base, entidad())
      if (!nrow(x)) return(.crm_nota(paste("No hay propuestas con evidencia suficiente para esta cuenta.",
        "Revise sus preferencias declaradas y sus oportunidades registradas.")))
      x$Oportunidad <- .in_cruce_oportunidades(x, base, .fuentes_cache$oportunidades(), contexto_crm())
      columnas <- c("Tiene", "Falta", "CestasEvidencia", "Confianza", "Lift", "Oportunidad")
      tabla <- .crm_tabla_vancouver(x[columnas], font_size = 11, row_padding = 3,
        labels = c(Tiene = "Ya compra", Falta = "Podría ofrecerle también",
          CestasEvidencia = "Pedidos que contienen ambos",
          Confianza = "Pedidos con B entre los que incluyen A", Lift = "Afinidad de la combinación",
          Oportunidad = "Seguimiento comercial ya registrado"))
      gt::fmt_percent(tabla, columns = "Confianza", decimals = 1)
    })
    .gc_tarjetas_server(c(seg_gestiones = "Gestiones", seg_recordatorios = "Recordatorios", seg_tareas = "Tareas",
      seg_eventos = "Eventos"), shiny::reactive(list(
        seg_gestiones = nrow(.in_detalle_crm(.fuentes_cache$gestiones(), contexto_crm(), .IN_FUENTES$gestiones$campos)),
        seg_recordatorios = nrow(.in_detalle_crm(.fuentes_cache$recordatorios(), contexto_crm(),
          .IN_FUENTES$recordatorios$campos)), seg_tareas = nrow(tareas_entidad()), seg_eventos = nrow(cronologia()))))
    oportunidades_actuales <- shiny::reactive(.in_oportunidades(.fuentes_cache$oportunidades(), contexto_crm()))
    .gc_tarjetas_server(c(op_total = "Oportunidades", op_activas = "Sin descartar", op_descartadas = "Descartadas",
      op_vencidas = "Fecha objetivo superada"), shiny::reactive({
        x <- oportunidades_actuales(); conocidas <- x$EstadoActual != "Sin estado registrado"
        list(op_total = nrow(x), op_activas = if (any(conocidas)) sum(x$EstadoActual == "Sin descartar") else NA_real_,
          op_descartadas = if (any(conocidas)) sum(x$EstadoActual == "Descartada") else NA_real_,
          op_vencidas = if (any(!is.na(x$FechaObjetivo))) {
            sum(x$FechaObjetivo < periodo()[[2]] & x$EstadoActual == "Sin descartar", na.rm = TRUE)
          } else NA_real_)
      }))
    output$oportunidades_fechas <- .crm_render_plotly({
      x <- oportunidades_actuales() %>% dplyr::filter(!is.na(FechaObjetivo)) %>%
        dplyr::mutate(Mes = .gc_mes(FechaObjetivo)) %>% dplyr::count(Mes, EstadoActual)
      shiny::validate(shiny::need(nrow(x) > 0L, "NO HAY OPORTUNIDADES CON FECHA OBJETIVO REGISTRADA."))
      plotly::plot_ly(x, x = ~Mes, y = ~n, color = ~EstadoActual, type = "bar",
        colors = c("#7A2028", "#495057", "#C8A77D"),
        hovertemplate = "%{x|%Y-%m}<br>Oportunidades: %{y:.0f}<extra>%{fullData.name}</extra>") %>%
        plotly::layout(title = "Distribución de fechas objetivo y estado de las oportunidades")
    })
    # Cartera histórica, con separación entre saldo observado y pago acreditado ----
    cartera_analitica <- shiny::reactive({
      shiny::req(input$detalle_tabs == "Cartera")
      .in_cartera_historia(cartera(), clientes_cartera(), mapa_nit(), entidad(), periodo()[[2]])
    })
    .gc_tarjetas_server(c(car_saldo = "Saldo documentado · COP", car_vencido = "Saldo vencido · COP",
      car_mora = "Mora máxima · días", car_puntualidad = "Pagos puntuales acreditados"), shiny::reactive({
        x <- cartera_analitica(); ultimo <- tail(x$evolucion, 1L)
        p <- x$pagos; buenos <- if (nrow(p)) p$Pagada & is.finite(p$DiasAtraso) else logical()
        list(car_saldo = ultimo$Saldo, car_vencido = ultimo$Vencido, car_mora = ultimo$DiasMoraMaximo,
          car_puntualidad = if (any(buenos)) mean(p$DiasAtraso[buenos] == 0) else NA_real_)
      }), list(car_saldo = "dinero", car_vencido = "dinero", car_mora = "numero", car_puntualidad = "porcentaje"))
    cosechas <- shiny::reactive({
      shiny::req(input$detalle_tabs == "Cartera")
      fuente <- cartera(); e <- entidad()
      shiny::validate(shiny::need(is.list(fuente) && is.data.frame(fuente$cartera) && is.data.frame(fuente$facturacion),
        "No hay saldos históricos y facturas disponibles para reconstruir cosechas."))
      elegir <- function(x) {
        x <- .car_scope(x, clientes_cartera(), mapa_nit())
        if (e$Alcance == "Cuenta principal") x[x$Principal %in% e$Principal, , drop = FALSE] else
          x[x$NIT %in% e$NitFacturacion, , drop = FALSE]
      }
      facturas <- .car_facturas_autorizadas(fuente$facturacion, fuente$cartera, clientes_cartera(), mapa_nit())
      # El alcance ya se aplicó; no repetir el join de NIT principal.
      f <- if (e$Alcance == "Cuenta principal") facturas[facturas$Principal %in% e$Principal, , drop = FALSE] else
        facturas[facturas$NIT %in% e$NitFacturacion, , drop = FALSE]
      .car_cosechas(elegir(fuente$cartera), f, periodo()[[2]])
    })
    output$cartera_cosechas <- .crm_render_plotly(.car_grafico_cosechas(cosechas()))
    output$limite_cosechas <- .crm_render_ui(.crm_disclosure("Alcance y limitaciones de las cosechas",
      .crm_nota(cosechas()$limitacion)))
    .crm_vancouver_server("cosechas_detalle", shiny::reactive(cosechas()$observaciones))
    .crm_vancouver_server("pagos_detalle", shiny::reactive(cartera_analitica()$pagos),
      columnas = c("NIT", "Factura", "FcnFec", "FecVto", "FecPag", "Pagada", "DiasPago", "DiasAtraso", "Valor", "Saldo"),
      col_labels = c(FcnFec = "Emisión", FecVto = "Vencimiento", FecPag = "Pago total registrado",
        Pagada = "Pago total acreditado", DiasPago = "Días hasta pago", DiasAtraso = "Mora al pagar · días"))
    .crm_vancouver_server("cartera_concentracion", shiny::reactive({
      x <- cartera_analitica()$actual %>% dplyr::group_by(NIT, Factura) %>%
        dplyr::summarise(Saldo = .com_suma(Saldo), .groups = "drop") %>%
        dplyr::filter(is.finite(Saldo), Saldo > 0) %>% dplyr::arrange(dplyr::desc(Saldo))
      x$Participacion <- x$Saldo / sum(x$Saldo)
      x
    }), col_specs = list(Participacion = list(format = list(percent = TRUE, digits = 1))))

    # Fidelidad: comparación relativa y predicción condicional BG/NBD ----
    rfm_actual <- shiny::reactive(if (nivel() == "nit") rfm_nit() else rfm_principal())
    perfil_rfm <- shiny::reactive(.in_rfm_comparar(rfm_actual(), .in_id_modelo(entidad())))
    .crm_vancouver_server("rfm_perfil", perfil_rfm,
      col_labels = c(LimiteObservadoInferior = "Mínimo observado para ese puntaje",
        LimiteObservadoSuperior = "Máximo observado para ese puntaje", PercentilFavorable = "Percentil favorable · 0 a 100"))
    output$rfm_radar <- .crm_render_plotly({
      x <- perfil_rfm()
      shiny::validate(shiny::need(nrow(x) > 0, "NO HAY HISTORIA POSITIVA PARA CALCULAR RFM."))
      p <- plotly::plot_ly(type = "scatterpolar", mode = "lines+markers")
      for (linea in unique(x$Linea)) {
        color <- c("#7A2028", "#C8A77D", "#9B7160")[[1L + (match(linea, unique(x$Linea)) - 1L) %% 3L]]
        d <- x[x$Linea == linea, , drop = FALSE]
        p <- p %>% plotly::add_trace(r = c(d$PercentilFavorable, d$PercentilFavorable[[1]]),
          theta = c(d$Dimension, d$Dimension[[1]]), name = linea, line = list(color = color), marker = list(color = color),
          hovertemplate = "%{theta}<br>Percentil favorable: %{r:.1f}<extra>%{fullData.name}</extra>")
      }
      p %>% plotly::add_trace(r = rep(50, 4L), theta = c(x$Dimension[1:3], x$Dimension[[1]]),
        name = "REFERENTE CENTRAL DE LA POBLACIÓN", line = list(color = "#495057", dash = "dash"),
        hovertemplate = "%{theta}<br>Referencia: percentil 50<extra></extra>") %>%
        plotly::layout(title = "Perfil relativo de fidelidad · más alto es más favorable",
          polar = list(radialaxis = list(range = c(0, 100))))
    })
    rfm_historico <- shiny::reactive({
      revision()
      f <- .in_fuente(cargar, "CRMNALRFMM")
      shiny::validate(shiny::need(is.null(f$error), "NO ESTÁ DISPONIBLE EL HISTÓRICO RFM."))
      x <- f$datos
      if (!"CliNitPpal" %in% names(x) && "customer_id" %in% names(x)) x$CliNitPpal <- x$customer_id
      shiny::validate(shiny::need(all(c("CliNitPpal", "FecProceso") %in% names(x)), "LA FUENTE NO IDENTIFICA CORTES RFM."))
      e <- entidad()
      shiny::validate(shiny::need(e$Alcance == "Cuenta principal" || e$NitFacturacion %in% e$Principal,
        "EL HISTÓRICO RFM ES DE CUENTA PADRE; NO SE COPIA EL PERFIL A UNA CUENTA HIJA."))
      x <- x[.in_id(x$CliNitPpal) %in% .in_id_modelo(e) & !is.na(.in_fecha(x$FecProceso)) &
        .in_fecha(x$FecProceso) <= periodo()[[2]], , drop = FALSE]
      lineas <- unique(historia_completa()$LinNegCod)
      if ("LinNegCod" %in% names(x)) x <- x[as.character(x$LinNegCod) %in% as.character(lineas), , drop = FALSE]
      shiny::validate(shiny::need(nrow(x) > 0L, "No existen cortes RFM para esta cuenta, líneas y fecha. No se sustituye el perfil por ceros."))
      x
    })
    .crm_vancouver_server("rfm_historia", rfm_historico,
      columnas = c("FecProceso", "LinNegCod", "SegmentoAnalitica", "recency_days", "transaction_count", "amount",
        "recency_score", "frequency_score", "monetary_score", "rfm_score"),
      col_labels = c(FecProceso = "Corte", LinNegCod = "Código de línea", SegmentoAnalitica = "Perfil histórico",
        recency_days = "Días desde compra", transaction_count = "Compras observadas", amount = "Margen · COP",
        recency_score = "Recencia · 1 a 5", frequency_score = "Frecuencia · 1 a 5", monetary_score = "Margen · 1 a 5"))
    output$rfm_evolucion <- .crm_render_plotly({
      x <- rfm_historico()
      campos <- intersect(c("recency_score", "frequency_score", "monetary_score"), names(x))
      shiny::validate(shiny::need(nrow(x) > 0 && length(campos) > 0,
        "NO HAY CORTES CON PUNTAJES RFM PARA GRAFICAR SU EVOLUCIÓN."))
      x$Corte <- .in_fecha(x$FecProceso)
      x$Linea <- .in_linea(.gc_columna(x, "LinNegCod"))
      x <- tidyr::pivot_longer(x, cols = dplyr::all_of(campos), names_to = "Dimension", values_to = "Puntaje")
      x$Dimension <- dplyr::recode(x$Dimension, recency_score = "RECENCIA", frequency_score = "FRECUENCIA",
        monetary_score = "MARGEN")
      x$Serie <- paste(x$Linea, x$Dimension, sep = " · ")
      plotly::plot_ly(x, x = ~Corte, y = ~Puntaje, color = ~Serie, type = "scatter", mode = "lines+markers", line = list(shape = "hv"),
        colors = c("#7A2028", "#C8A77D", "#495057", "#9B7160", "#343a40", "#B39578"),
        hovertemplate = "%{x|%d/%m/%Y}<br>Puntaje: %{y:.0f}<extra>%{fullData.name}</extra>") %>%
        plotly::layout(title = "Evolución de los hábitos de compra en los cortes disponibles",
          yaxis = list(title = "Puntaje histórico · 1 a 5", range = c(0.5, 5.5), dtick = 1))
    })
    modelo_actual <- shiny::reactive(if (nivel() == "nit") modelo_nit() else modelo_principal())
    predictivo <- shiny::reactive(.in_btyd_predictivo(modelo_actual(), .in_id_modelo(entidad()), periodo()[[2]]))
    datos_predictivos <- shiny::reactive({
      x <- predictivo()
      shiny::validate(shiny::need(is.null(x$error), x$error))
      x$datos
    })
    output$modelo_distribucion <- .crm_render_plotly({
      x <- datos_predictivos()
      plotly::plot_ly(x, x = ~Compras, y = ~Probabilidad, color = ~Linea, type = "scatter", mode = "lines+markers",
        colors = c("#7A2028", "#C8A77D"),
        hovertemplate = "%{x:.0f} días con compra<br>Probabilidad: %{y:.1%}<extra>%{fullData.name}</extra>") %>%
        plotly::layout(title = "Probabilidades discretas de días con compra · próximos 12 meses",
          yaxis = list(title = "Probabilidad", tickformat = ".0%"), xaxis = list(title = "Días con compra"),
          shapes = unlist(lapply(split(x, x$Linea), function(d) list(
            list(type = "line", x0 = d$MediaSimulada[[1]], x1 = d$MediaSimulada[[1]], y0 = 0, y1 = 1,
              yref = "paper", line = list(color = "#7A2028", dash = "dash")),
            list(type = "line", x0 = d$MedianaSimulada[[1]], x1 = d$MedianaSimulada[[1]], y0 = 0, y1 = 1,
              yref = "paper", line = list(color = "#495057", dash = "dot")))), recursive = FALSE),
          annotations = unlist(lapply(split(x, x$Linea), function(d) list(
            list(x = d$MediaSimulada[[1]], y = 1, yref = "paper", text = paste(d$Linea[[1]], "Media"), showarrow = FALSE),
            list(x = d$MedianaSimulada[[1]], y = .9, yref = "paper", text = paste(d$Linea[[1]], "Mediana"), showarrow = FALSE))),
            recursive = FALSE))
    })
    output$modelo_compras <- .crm_render_plotly({
      x <- datos_predictivos() %>% dplyr::distinct(Linea, Esperadas)
      h <- .in_hechos(if (nivel() == "nit") poblacion_nit() else poblacion_principal(), entidad())
      h <- .com_hechos(h, periodo()[[2]], cerrados = FALSE)
      h <- h[h$FecFact > lubridate::add_with_rollback(periodo()[[2]], lubridate::years(-1)) & h$SacosFact > 0, ]
      observadas <- h %>% dplyr::group_by(LinNegCod) %>%
        dplyr::summarise(Observadas = dplyr::n_distinct(FecFact), .groups = "drop")
      observadas$Linea <- .in_linea(observadas$LinNegCod)
      x <- dplyr::left_join(x, observadas, by = "Linea")
      x$Observadas[is.na(x$Observadas)] <- 0
      plotly::plot_ly(x, x = ~Linea, y = ~Observadas, type = "bar", name = "OBSERVADAS · ÚLTIMOS 12 MESES",
        marker = list(color = "#495057"), hovertemplate = "%{x}<br>Días con compra observados: %{y:.0f}<extra></extra>") %>%
        plotly::add_trace(y = ~Esperadas, name = "ESTIMADAS · PRÓXIMOS 12 MESES", marker = list(color = "#7A2028"),
          hovertemplate = "%{x}<br>Compras estimadas: %{y:.1f}<extra></extra>") %>%
        plotly::layout(title = "Compras observadas y compras futuras estimadas", barmode = "group")
    })
    .crm_vancouver_server("modelo_parametros", shiny::reactive({
      x <- predictivo(); shiny::validate(shiny::need(is.null(x$error), x$error)); x$parametros
    }))
    .crm_vancouver_server("modelo_intervalos", shiny::reactive(datos_predictivos() %>%
      dplyr::distinct(Linea, ProbActivo, Esperadas, Inferior95, Superior95, MediaSimulada)),
      col_labels = c(ProbActivo = "Probabilidad estimada de seguir comprando", Esperadas = "Compras esperadas · 12 meses",
        Inferior95 = "Límite predictivo inferior · 95%", Superior95 = "Límite predictivo superior · 95%"),
      col_specs = list(ProbActivo = list(format = list(percent = TRUE, digits = 1))))

    # Evidencia de venta cruzada: compras, combinaciones, reglas y propuesta ----
    venta_actual <- shiny::reactive(if (nivel() == "nit") venta_nit() else venta_principal())
    compras_mba <- shiny::reactive({
      x <- venta_actual()$base$movimientos
      x[x$NIT %in% .in_id_modelo(entidad()), , drop = FALSE]
    })
    frecuencias_mba <- shiny::reactive({
      base <- venta_actual()$base
      x <- compras_mba() %>% dplyr::distinct(Cesta, Item, Etiqueta) %>% dplyr::count(Item, Etiqueta, name = "Pedidos")
      x$ParticipacionCliente <- x$Pedidos / dplyr::n_distinct(compras_mba()$Cesta)
      general <- base$membresia %>% dplyr::count(Item, name = "PedidosPoblacion")
      x <- dplyr::left_join(x, general, by = "Item")
      x$ParticipacionPoblacion <- x$PedidosPoblacion / nrow(base$cestas)
      x
    })
    .gc_tarjetas_server(c(mba_transacciones = "Pedidos analizados", mba_productos = "Productos comprados",
      mba_reglas = "Reglas con evidencia", mba_propuestas = "Propuestas"), shiny::reactive({
        x <- compras_mba(); base <- venta_actual()
        list(mba_transacciones = dplyr::n_distinct(x$Cesta), mba_productos = dplyr::n_distinct(x$Item),
          mba_reglas = nrow(base$reglas), mba_propuestas = nrow(.in_propuestas(base, entidad())))
      }))
    output$mba_frecuencias <- .crm_render_plotly({
      x <- frecuencias_mba() %>% dplyr::arrange(dplyr::desc(Pedidos)) %>% head(12L)
      shiny::validate(shiny::need(nrow(x) > 0L, "NO HAY PRODUCTOS IDENTIFICADOS EN LOS PEDIDOS DE ESTA ENTIDAD."))
      plotly::plot_ly(x, x = ~Etiqueta, y = ~ParticipacionCliente, type = "bar", name = "ENTIDAD",
        marker = list(color = "#7A2028"), hovertemplate = "%{x}<br>Presente en %{y:.1%} de sus pedidos<extra></extra>") %>%
        plotly::add_trace(y = ~ParticipacionPoblacion, name = "POBLACIÓN COMPARABLE", marker = list(color = "#C8A77D"),
          hovertemplate = "%{x}<br>Presente en %{y:.1%} de los pedidos de la población<extra></extra>") %>%
        plotly::layout(title = "Productos más frecuentes y participación en las compras", barmode = "group",
          yaxis = list(title = "Proporción de pedidos", tickformat = ".0%"))
    })
    reglas_mba <- shiny::reactive({
      b <- venta_actual(); x <- b$reglas
      if (!nrow(x)) {
        x$CompraActual <- x$Complemento <- x$Evidencia <- character()
        return(x)
      }
      x$CompraActual <- vapply(x$Antecedente, .mba_etiquetas, character(1), mapa = b$base$mapa)
      x$Complemento <- vapply(x$Consecuente, .mba_etiquetas, character(1), mapa = b$base$mapa)
      x$Evidencia <- ifelse(x$CestasAmbos < 5, "ESCASA · MENOS DE 5 PEDIDOS", "REVISAR CONTEXTO COMERCIAL")
      x
    })
    .crm_vancouver_server("mba_reglas", reglas_mba,
      columnas = c("CompraActual", "Complemento", "Soporte", "Confianza", "Lift", "CestasAmbos", "Evidencia"),
      col_specs = list(Soporte = list(format = list(percent = TRUE, digits = 1)),
        Confianza = list(format = list(percent = TRUE, digits = 1))))
    .crm_vancouver_server("mba_combinaciones", shiny::reactive({
      x <- reglas_mba()
      x$Combinacion <- vapply(seq_len(nrow(x)), function(i) paste(sort(c(x$CompraActual[[i]], x$Complemento[[i]])),
        collapse = " + "), character(1))
      x %>% dplyr::arrange(dplyr::desc(CestasAmbos)) %>% dplyr::distinct(Combinacion, CestasAmbos)
    }),
      col_labels = c(CestasAmbos = "Pedidos que contienen ambos productos"))
    output$mba_reglas_grafico <- .crm_render_plotly({
      x <- reglas_mba()
      shiny::validate(shiny::need(nrow(x) > 0L, "NO HAY REGLAS CON EVIDENCIA SUFICIENTE."))
      x$Lectura <- paste(x$CompraActual, "→", x$Complemento, "· pedidos:", x$CestasAmbos)
      plotly::plot_ly(x, x = ~Confianza, y = ~Lift, size = ~CestasAmbos, text = ~Lectura,
        type = "scatter", mode = "markers", marker = list(color = "#7A2028"),
        hovertemplate = "%{text}<br>Confianza: %{x:.1%}<br>Lift: %{y:.2f}<extra></extra>") %>%
        plotly::layout(title = "Asociaciones: confianza, afinidad y respaldo", xaxis = list(tickformat = ".0%"))
    })
    .gc_descarga(output, ejecucion, "consulta_individual")
    invisible(list(catalogo = catalogo, entidad = entidad, ejecucion = ejecucion, historia = historia))
  })
}
