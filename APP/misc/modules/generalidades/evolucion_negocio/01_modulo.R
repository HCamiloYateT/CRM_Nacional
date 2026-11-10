# Evolución del Negocio: lectura financiera y plan de crecimiento ----
.EV_CAJAS <- c(sacos = "Sacos de 70 kg", margen = "Margen neto FCC",
  cumpl_sacos = "Cumplimiento en sacos", cumpl_margen = "Cumplimiento en margen")
.EV_PCT <- stats::setNames(rep(list(list(format = list(percent = TRUE, digits = 1))), 6),
  c("CumplSacos", "CumplMargen", "VariacionSacos", "VariacionMargen", "CAGR_Sacos", "CAGR_Margen"))
.EV_PIPELINE_REFERENCIA <- data.frame(
  Alcance = c("Oportunidades", "Potencial de negocio mensual", "Potencial planta tostadora mensual"),
  Clientes = c(27L, 181L, 73L), Sacos = c(726, 20290, 591), MargenEsperado = c(68, 1030, 149) * 1e6,
  Diapositiva = c(13L, 14L, 15L), Fuente = "Presentación: escenario estático de 2026, no ejecución")

EvolucionNegocioUI <- function(id, cierre = Sys.Date()) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeFormRow(columns = 3,
      .crm_select("vista", "Lectura del negocio", choices = c(
        "Industria Nacional y líneas" = "Linea", "Evolución por segmento" = "Segmento",
        "Evolución por asesor" = "Asesor", "Clientes nuevos observados" = "Nuevos",
        "Plan de crecimiento 2029" = "Plan", "Referencias de la presentación" = "Referencia"),
        selected = "Linea", empty = FALSE, ns = ns),
      racafeForms::racafeInputDate("corte", "Consultar hasta", value = .com_fin_mes(cierre), ns = ns),
      racafeForms::racafeInputNumeric("desde", "Desde el año", value = 2017, min = 2016, thousand = "",
        max = as.integer(format(Sys.Date(), "%Y")), format = "integer", ns = ns)),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputSwitch("comparable", "Comparar los mismos meses de cada año", value = FALSE, ns = ns),
      racafeForms::racafeInputSwitch("abiertos", "Incluir facturación provisional", value = FALSE, ns = ns),
      racafeForms::racafeInputSwitch("proyectar", "Mostrar proyección simple de cierre", value = FALSE, ns = ns)),
    shiny::uiOutput(ns("alcance")),
    shiny::conditionalPanel("!['Plan','Referencia','Nuevos'].includes(input.vista)", ns = ns,
      .gc_tarjetas_ui(ns, .EV_CAJAS), .crm_nota(c(
        "Indicadores del año consultado. El cumplimiento compara los mismos meses con el presupuesto anual / 12.",
        "El mes efectivo y el origen de cada cifra se indican debajo y en la tabla."))),
    shiny::conditionalPanel("['Linea','Segmento','Asesor'].includes(input.vista)", ns = ns,
      racafeForms::racafeFormRow(columns = 2,
        .crm_select("negocio", "Línea del desglose",
          choices = c("A la medida", "Coproductos"),
          selected = c("A la medida", "Coproductos"), multiple = TRUE, gender = "f", ns = ns),
        .crm_select("base", "Año base para crecimiento",
          choices = as.character(2016:max(2016L, as.integer(format(Sys.Date(), "%Y")) - 1L)),
          selected = "2023", empty = FALSE, ns = ns)),
      .crm_plot_output(ns("total"), height = "260px"),
      .crm_plot_output(ns("margen"), height = "440px"),
      .crm_plot_output(ns("sacos"), height = "440px"),
      .crm_descarga_ui("descargar", ns),
      .crm_tabla_ui(ns("anual"), titulo = "Ejecución y presupuesto por año"),
      .crm_nota(c("Margen en COP; las gráficas lo expresan en millones. Volumen en sacos equivalentes de 70 kg.",
        "La proyección es acumulado × 12 / meses consultados, no una meta ni ejecución realizada.",
        "La variación interanual queda vacía si los periodos no tienen el mismo número de meses.")),
      .crm_tabla_ui(ns("crecimiento"), titulo = "Crecimiento entre periodos comparables"),
      .crm_nota(c("CAGR = (valor final / valor inicial)^(1 / años) − 1, solo con bases positivas.",
        "Para comparar un año parcial con 2023 active la comparación de los mismos meses.",
        "El histórico anual de la presentación no permite esa comparación mensual.",
        "Presupuestos de años completos sin snapshot: referencia original del documento, con fuente indicada."))),
    shiny::conditionalPanel("input.vista == 'Nuevos'", ns = ns,
      .gc_tarjetas_ui(ns, c(nuevos_clientes = "Clientes únicos con primera compra observada",
        nuevos_sacos = "Sacos de clientes nuevos observados", nuevos_margen = "Margen de clientes nuevos observados",
        nuevos_ticket = "Sacos por cliente nuevo observado")),
      .crm_plot_output(ns("nuevos_grafico"), height = "320px"),
      .crm_tabla_ui(ns("nuevos"), titulo = "Primera compra observada en el año consultado"),
      .crm_nota(c("Primera compra positiva observada por NIT principal, contrastada con la fecha del sistema.",
        "La historia disponible puede comenzar después del nacimiento del cliente; no implica alta real en CRM.",
        "Un cliente puede aparecer en varias líneas; los grupos no se suman como clientes únicos."))),
    shiny::conditionalPanel("input.vista == 'Plan'", ns = ns,
      shiny::uiOutput(ns("plan_ui")),
      .crm_plot_output(ns("plan_margen"), height = "330px"),
      .crm_plot_output(ns("plan_sacos"), height = "330px"),
      .crm_tabla_ui(ns("plan"), titulo = "Escenario estratégico de la presentación"),
      .crm_nota(c("2026: cierre proyectado del documento. 2027–2029: metas estratégicas, no presupuestos vigentes.",
        "Meta 2029: $8.000 millones de margen y 85.000 sacos; $5.000 millones a la medida y $3.000 en coproductos.",
        "Escenario corporativo fijo de solo lectura para administradores; no responde a filtros comerciales."))),
    shiny::conditionalPanel("input.vista == 'Referencia'", ns = ns,
      .crm_tabla_ui(ns("referencia"), titulo = "Valores originales de los gráficos de la presentación"),
      .crm_nota(c("Los valores 2026 del documento son proyecciones y no se incorporan a la ejecución del ETL.",
        "2027: presupuestos del documento, separados del catálogo vigente.",
        "Las láminas usan universos distintos: no se suman entre sí los totales por línea, segmento o asesor.")),
      .crm_tabla_ui(ns("pipeline"), titulo = "Pipeline y potencial: fotografía de la presentación"),
      .crm_nota(c("Las oportunidades son objetivos por negocio; los potenciales son estimaciones mensuales.",
        "Estas tres poblaciones pueden solaparse: no se suman ni se presentan como ventas realizadas.",
        "Potencial general: corte a agosto de 2026. Las demás láminas no precisan el mismo corte.")),
      racafeForms::racafeActionBar(left = shiny::tagList(
        racafeForms::racafeButton("ir_oportunidades", "Consultar oportunidades vigentes",
          icon = "arrow-right", ns = ns),
        racafeForms::racafeButton("ir_prospeccion", "Consultar prospección vigente", icon = "arrow-right", ns = ns)))))
}

EvolucionNegocio <- function(id, hechos, clientes, filtros, cierre, es_admin,
                            historico = function() NULL, primera_compra = function() NULL,
                            ruta_app = .ruta_app, navegar = function(tab) NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    referencia <- .ev_referencia(ruta_app)
    vista <- shiny::reactive(.com_elegir(input$vista,
      c("Linea", "Segmento", "Asesor", "Nuevos", "Plan", "Referencia"), "Linea"))
    corte <- shiny::reactive({
      shiny::req(input$corte)
      fecha <- .com_fin_mes(as.Date(input$corte))
      if (!isTRUE(input$abiertos)) fecha <- min(fecha, .com_fin_mes(cierre))
      shiny::validate(shiny::need(fecha >= as.Date("2016-01-01"), "Seleccione un corte desde 2016."))
      fecha
    })
    desde <- shiny::reactive({
      x <- suppressWarnings(as.integer(input$desde))
      if (length(x) != 1L || is.na(x)) return(2017L)
      max(2016L, min(as.integer(format(corte(), "%Y")), x))
    })
    mensual <- shiny::reactive(.ev_mensual(hechos(), historico(), clientes(), filtros(), corte(),
      abiertos = isTRUE(input$abiertos)))
    dimension <- shiny::reactive(if (vista() %in% c("Segmento", "Asesor")) vista() else "Linea")
    anual <- shiny::reactive({
      fuente <- hechos()
      fechas <- fuente$FecMes[fuente$EstadoFinanciero == "CERRADO" & !is.na(fuente$FecMes)]
      inicio <- if (length(fechas)) min(as.integer(format(fechas, "%Y"))) else 2016L
      x <- mensual()
      x <- x[x$Linea %in% input$negocio, , drop = FALSE]
      f <- filtros()
      codigos <- c("A la medida" = "A LA MEDIDA", Coproductos = "CONVENCIONALES")
      f$linneg <- intersect(f$linneg, unname(codigos[input$negocio]))
      e <- .ev_anual(x, clientes(), f, corte(), dimension(), isTRUE(input$comparable), referencia$anual,
        permitir_referencia = isTRUE(es_admin()) && .ev_completo(fuente, filtros()), inicio_etl = inicio,
        presupuesto_disponible = .ev_completo(fuente, filtros(), c("categoria", "producto")))
      e[e$Anho >= desde() & e$Anho <= as.integer(format(corte(), "%Y")), ]
    })
    indicador <- shiny::reactive({
      x <- anual(); x <- x[x$Anho == as.integer(format(corte(), "%Y")), ]
      list(sacos = if (nrow(x)) .com_suma(x$Sacos) else NA_real_,
        margen = if (nrow(x)) .com_suma(x$Margen) else NA_real_,
        cumpl_sacos = .gc_ratio(.com_suma(x$Sacos), .com_suma(x$PptoSacos)),
        cumpl_margen = .gc_ratio(.com_suma(x$Margen), .com_suma(x$PptoMargen)))
    })
    .gc_tarjetas_server(.EV_CAJAS, indicador, list(sacos = "numero", margen = "dinero",
      cumpl_sacos = "porcentaje", cumpl_margen = "porcentaje"))
    output$alcance <- .crm_render_ui(.crm_nota(c(
      paste("Corte efectivo:", format(corte(), "%Y-%m"), "·", if (isTRUE(input$abiertos)) {
        "Incluye facturación provisional; su margen puede ser estimado."
      } else "Solo meses cerrados financieramente."),
      if (is.null(historico())) "Excel histórico aún no instalado; consulte la referencia anual disponible." else {
        "Excel histórico instalado; se excluyen clientes sin homologación comercial válida."
      })))
    .crm_vancouver_server("anual", data = anual, col_specs = .EV_PCT,
      col_labels = c(Anho = "Año", PptoSacos = "Ppto. comparable sacos", PptoMargen = "Ppto. comparable margen",
        CumplSacos = "Cumpl. sacos", CumplMargen = "Cumpl. margen", MargenKg = "Margen/kg",
        MargenProvisional = "Margen provisional", ProySacos = "Proyección simple sacos",
        ProyMargen = "Proyección simple margen", MesesComparados = "Meses comparados"))
    crecimiento <- shiny::reactive({
      base <- suppressWarnings(as.integer(input$base))
      if (length(base) != 1L || is.na(base)) base <- 2023L
      .ev_crecimiento(anual(), base, as.integer(format(corte(), "%Y")))
    })
    .crm_vancouver_server("crecimiento", data = crecimiento, col_specs = .EV_PCT)
    output$margen <- .crm_render_plot({
      x <- anual(); shiny::validate(shiny::need(nrow(x), "No hay historia para el alcance seleccionado."))
      .ev_grafico(x, "Margen", isTRUE(input$proyectar))
    })
    output$sacos <- .crm_render_plot({
      x <- anual(); shiny::validate(shiny::need(nrow(x), "No hay historia para el alcance seleccionado."))
      .ev_grafico(x, "Sacos", isTRUE(input$proyectar))
    })
    output$total <- .crm_render_plot({
      x <- anual() %>% dplyr::group_by(Anho) %>% dplyr::summarise(Margen = .com_suma(Margen),
        Sacos = .com_suma(Sacos), .groups = "drop")
      shiny::validate(shiny::need(nrow(x), "No hay historia para el alcance seleccionado."))
      z <- rbind(data.frame(Anho = x$Anho, Metrica = "Margen · millones COP", Valor = x$Margen / 1e6),
        data.frame(Anho = x$Anho, Metrica = "Sacos de 70 kg", Valor = x$Sacos))
      ggplot2::ggplot(z, ggplot2::aes(x = factor(Anho), y = Valor)) +
        ggplot2::geom_col(fill = "#495057", na.rm = TRUE) + ggplot2::facet_wrap(~Metrica, scales = "free_y") +
        ggplot2::labs(x = NULL, y = NULL, title = "Industria Nacional · alcance consultado") +
        .crm_tema_grafico()
    })
    nuevos <- shiny::reactive(.ev_nuevos(mensual(), primera_compra(), as.integer(format(corte(), "%Y")), "Linea"))
    .gc_tarjetas_server(c(nuevos_clientes = "Clientes únicos con primera compra observada",
      nuevos_sacos = "Sacos de clientes nuevos observados", nuevos_margen = "Margen de clientes nuevos observados",
      nuevos_ticket = "Sacos por cliente nuevo observado"), shiny::reactive({
      x <- nuevos(); n <- attr(x, "clientes_unicos")
      list(nuevos_clientes = n, nuevos_sacos = .com_suma(x$Sacos), nuevos_margen = .com_suma(x$Margen),
        nuevos_ticket = .gc_ratio(.com_suma(x$Sacos), n))
    }), list(nuevos_sacos = "numero", nuevos_margen = "dinero", nuevos_ticket = "numero"))
    .crm_vancouver_server("nuevos", data = nuevos)
    output$nuevos_grafico <- .crm_render_plot({
      x <- nuevos(); shiny::validate(shiny::need(nrow(x), "Sin primeras compras observadas en este año."))
      ggplot2::ggplot(x, ggplot2::aes(x = Dimension, y = Clientes)) + ggplot2::geom_col(fill = "#DA291C") +
        ggplot2::labs(x = NULL, y = "Clientes") + .crm_tema_grafico()
    })
    plan <- shiny::reactive({ shiny::req(isTRUE(es_admin())); referencia$plan })
    output$plan_ui <- .crm_render_ui({
      if (!isTRUE(es_admin())) return(.crm_nota("El escenario corporativo está disponible para administradores."))
      .crm_nota("Plan original: escenario de la presentación, separado de la ejecución y del presupuesto vigente.")
    })
    .crm_vancouver_server("plan", data = plan)
    .crm_vancouver_server("referencia", data = shiny::reactive({
      shiny::req(isTRUE(es_admin())); referencia$anual
    }))
    .crm_vancouver_server("pipeline", data = shiny::reactive({
      shiny::req(isTRUE(es_admin())); .EV_PIPELINE_REFERENCIA
    }))
    for (medida in c("Margen", "Sacos")) local({
      m <- medida
      output[[paste0("plan_", tolower(m))]] <- .crm_render_plot({
        x <- plan(); x$Valor <- x[[paste0("Meta", m)]] / if (m == "Margen") 1e6 else 1
        ggplot2::ggplot(x, ggplot2::aes(x = factor(Anho), y = Valor, fill = Linea)) +
          ggplot2::geom_col() + ggplot2::scale_fill_manual(values = c("#DA291C", "#495057")) +
          ggplot2::labs(x = NULL, y = if (m == "Margen") "Meta margen · millones COP" else "Meta sacos 70 kg",
            fill = NULL) + .crm_tema_grafico() + ggplot2::theme(legend.position = "bottom")
      })
    })
    calidad <- shiny::reactive({
      x <- mensual()
      resumen <- x %>% dplyr::group_by(Fuente) %>% dplyr::summarise(Movimientos = dplyr::n(),
        Desde = .gc_fecha_min(FecMes), Hasta = .gc_fecha_max(FecMes), SinMargen = sum(is.na(Margen)),
        SinSacos = sum(is.na(SacosFact)), .groups = "drop")
      omitidos <- attr(x, "historico_sin_homologacion")
      if (isTRUE(es_admin()) && !is.null(omitidos) && omitidos > 0L) resumen <- dplyr::bind_rows(resumen,
        data.frame(Fuente = "Excel sin homologación comercial: excluido", Movimientos = omitidos,
          Desde = as.Date(NA), Hasta = as.Date(NA), SinMargen = NA_integer_, SinSacos = NA_integer_))
      resumen
    })
    .crm_vancouver_server("calidad", data = calidad)
    output$descargar <- shiny::downloadHandler(filename = function() paste0("Evolucion_", Sys.Date(), ".xlsx"),
      content = function(file) {
        hojas <- list(Ejecucion = anual(), Crecimiento = crecimiento(), PrimerasCompras = nuevos(),
          Cobertura = calidad(), Consulta = data.frame(Corte = corte(), Desde = desde(),
            Comparable = isTRUE(input$comparable), Provisional = isTRUE(input$abiertos)))
        if (isTRUE(es_admin())) hojas <- c(hojas, list(Referencia = referencia$anual, Plan = referencia$plan))
        writexl::write_xlsx(hojas, file)
      })
    shiny::observeEvent(input$ir_oportunidades, navegar("GC_Oportunidades"))
    shiny::observeEvent(input$ir_prospeccion, navegar("PR_Conversion"))
    list(datos = anual)
  })
}
