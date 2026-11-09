# Evolución y composición: vistas del mismo hecho financiero ----
AnalisisComercialUI <- function(id, tipo = "evolucion", fecha = Sys.Date()) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputDate("corte", "Mes de consulta", value = fecha, ns = ns),
      .crm_select("dimension", "Dimensión", choices = .COM_DIMENSIONES, ns = ns),
      .crm_select("medida", "Medida", choices = c("Sacos 70 kg" = "Sacos",
        "Margen FCC" = "Margen", "Margen por kilo" = "MargenKg", "Margen US¢/lb" = "MargenUSClb"),
        selected = "Margen", ns = ns)),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputSwitch("acumulado", "Acumulado del año", value = TRUE, ns = ns),
      racafeForms::racafeInputSwitch("abiertos", "Incluir facturación de meses abiertos", value = FALSE, ns = ns)),
    .gc_tarjetas_ui(ns, c(sacos = "Sacos facturados de 70 kg", margen = "Margen FCC",
      kilos = "Margen por kilo", cumplimiento = "Cumplimiento del presupuesto de margen")),
    shiny::plotOutput(ns("grafico"), height = "300px"),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("dimensiones")),
    if (tipo == "evolucion") .crm_tabla_ui(ns("serie"), titulo = "Comportamiento mensual e interanual"),
    .crm_nota(c("Se aplican las dimensiones del controlbar; el periodo se selecciona en este informe.",
      "Margen por kilo = margen total / (sacos de 70 kg × 70), sin promediar razones por lote.",
      "Margen US¢/lb usa la tasa anual de referencia importada; sin tasa válida queda vacío, no usa TRM actual.",
      "El presupuesto anual se toma una sola vez por cliente/línea y se distribuye en doce meses.",
      "Categoría, producto y marca muestran ejecución: no se reparte entre ellos un presupuesto de cliente.",
      "Los abiertos son provisionales; al incluirlos el margen mostrado incorpora estimaciones.",
      "La variación compara los mismos meses del año anterior; una base cero se muestra vacía.")))
}
AnalisisComercial <- function(id, hechos, clientes, filtros, tipo = "evolucion", historico = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    corte <- shiny::reactive({
      shiny::req(input$corte)
      .com_fin_mes(as.Date(input$corte))
    })
    fuente <- shiny::reactive(.com_hechos(hechos(), corte(), cerrados = !isTRUE(input$abiertos)))
    catalogo <- shiny::reactive(.com_clientes(clientes(), corte(), filtros(), anual = TRUE))
    dimension <- shiny::reactive(.com_elegir(input$dimension, unname(.COM_DIMENSIONES), "CLLinNegNo"))
    tabla <- shiny::reactive({
      actual <- .com_resumen(fuente(), catalogo(), corte(), dimension(), isTRUE(input$acumulado))
      anterior <- .com_fin_mes(as.Date(paste0(as.integer(format(corte(), "%Y")) - 1L,
        format(corte(), "-%m-01"))))
      comparativo <- .com_resumen(fuente(), .com_clientes(clientes(), anterior, filtros(), anual = TRUE),
        anterior, dimension(), isTRUE(input$acumulado))
      comparativo <- comparativo[c("Dimension", "Sacos", "Margen")]
      names(comparativo) <- c("Dimension", "SacosAnterior", "MargenAnterior")
      tasa <- .com_tasa_referencia(historico()$tasas, as.integer(format(corte(), "%Y")))
      actual$MargenUSClb <- .gc_ratio(actual$MargenKg * 0.453592 * 100, tasa)
      dplyr::left_join(actual, comparativo, by = "Dimension") %>%
        dplyr::mutate(VariacionSacos = .gc_ratio(Sacos - SacosAnterior, abs(SacosAnterior)),
          VariacionMargen = .gc_ratio(Margen - MargenAnterior, abs(MargenAnterior)))
    })
    serie <- shiny::reactive({
      x <- .com_serie(fuente(), corte())
      x$Anho <- as.integer(format(x$Fecha, "%Y"))
      x$Mes <- as.integer(format(x$Fecha, "%m"))
      if (isTRUE(input$acumulado)) x <- x %>% dplyr::group_by(Anho) %>% dplyr::arrange(Mes, .by_group = TRUE) %>%
        dplyr::mutate(Sacos = cumsum(Sacos), Margen = cumsum(Margen), MargenKg = .gc_ratio(Margen, Sacos * 70)) %>%
        dplyr::ungroup()
      tasas <- historico()$tasas
      x$MargenUSClb <- x$MargenKg * 0.453592 * 100 /
        vapply(x$Anho, function(a) .com_tasa_referencia(tasas, a), numeric(1))
      x
    })
    .gc_tarjetas_server(c(sacos = "Sacos facturados de 70 kg", margen = "Margen FCC",
      kilos = "Margen por kilo", cumplimiento = "Cumplimiento del presupuesto de margen"), shiny::reactive({
      x <- tabla()
      margen <- .com_suma(x$Margen)
      sacos <- .com_suma(x$Sacos)
      list(sacos = sacos, margen = margen, kilos = .gc_ratio(margen, sacos * 70),
        cumplimiento = .gc_ratio(margen, .com_suma(x$PptoMargen)))
    }), list(sacos = "numero", margen = "dinero", kilos = "dinero", cumplimiento = "porcentaje"))
    .crm_tabla_servidor("dimensiones", data = tabla, modo_seleccion = "fila", id_col = "Dimension",
      col_labels = c(Dimension = "Dimensión", Sacos = "Sacos 70 kg", PptoSacos = "Presupuesto sacos",
        PptoMargen = "Presupuesto margen", MargenKg = "Margen/kg", MargenUSClb = "Margen US¢/lb",
        CumplSacos = "Cumpl. sacos", CumplMargen = "Cumpl. margen", SacosAnterior = "Sacos año anterior",
        MargenAnterior = "Margen año anterior", VariacionSacos = "Variación sacos",
        VariacionMargen = "Variación margen"),
      col_specs = list(CumplSacos = list(format = list(percent = TRUE, digits = 1)),
        CumplMargen = list(format = list(percent = TRUE, digits = 1)),
        VariacionSacos = list(format = list(percent = TRUE, digits = 1)),
        VariacionMargen = list(format = list(percent = TRUE, digits = 1))),
      modal_titulo_fn = function(sel) "Detalle de la dimensión",
      modal_contenido_fn = function(sel) {
        x <- fuente()
        dim <- dimension()
        etiqueta <- as.character(x[[dim]])
        etiqueta[is.na(etiqueta) | !nzchar(etiqueta)] <- "Sin clasificar"
        x <- x[etiqueta == sel$id & format(x$Fecha, "%Y") == format(corte(), "%Y"), , drop = FALSE]
        if (!isTRUE(input$acumulado)) x <- x[x$Fecha == .gc_mes(corte()), ]
        cols <- intersect(c("CliNitPpal", "PerRazSoc", "CLLinNegNo", "Asesor", "Categoria", "Producto",
          "FecFact", "CLLotCod", "SacosFact", "Margen", "EstadoFinanciero"), names(x))
        .crm_tabla_vancouver(.crm_nits_tabla(x[cols]), font_size = 11, row_padding = 2)
      })
    if (tipo == "evolucion") .crm_vancouver_server("serie", data = serie)
    output$grafico <- .crm_render_plot({
      medida <- .com_elegir(input$medida, c("Sacos", "Margen", "MargenKg", "MargenUSClb"), "Margen")
      x <- if (tipo == "evolucion") serie() else tabla()
      shiny::validate(shiny::need(nrow(x) > 0, "No hay información para este periodo."))
      if (tipo == "evolucion") {
        ggplot2::ggplot(x, ggplot2::aes(x = Mes, y = .data[[medida]], group = Anho, colour = as.character(Anho))) +
          ggplot2::geom_line(data = x %>% dplyr::group_by(Anho) %>% dplyr::filter(dplyr::n() > 1L),
            linewidth = 0.7, na.rm = TRUE) + ggplot2::geom_point(size = 1.5, na.rm = TRUE) +
          ggplot2::scale_colour_manual(values = grDevices::colorRampPalette(
            c("#6C757D", "#212529", "#DA291C"))(dplyr::n_distinct(x$Anho))) +
          ggplot2::scale_x_continuous(breaks = 1:12) + ggplot2::labs(x = "Mes", y = medida, colour = "Año") +
          .crm_tema_grafico()
      } else {
        x <- x[order(abs(x[[medida]]), decreasing = TRUE), , drop = FALSE]
        x <- head(x, 15)
        ggplot2::ggplot(x, ggplot2::aes(x = stats::reorder(Dimension, .data[[medida]]), y = .data[[medida]])) +
          ggplot2::geom_col(fill = "#DA291C", na.rm = TRUE) + ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = medida,
            caption = "Las 15 mayores contribuciones absolutas; tabla completa abajo.") +
          .crm_tema_grafico()
      }
    })
    .gc_descarga(output, tabla, paste0("Analisis_", tipo))
    list(datos = tabla)
  })
}
