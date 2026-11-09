# Tablero dinámico de ejecución por categoría y producto ----
.PROD_CAJAS <- c(sacos = "Sacos facturados (70 kg)", margen = "Margen FCC total",
  productos = "Productos con movimiento",
  clientes = "Clientes con compra", margen_kg = "Margen por kilo", pedidos = "Pedidos facturados",
  cerrado = "Margen contable cerrado", abierto = "Margen abierto provisional")
ProductosEjecucionUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputDateRange("periodo", "Periodo de facturación",
        start = as.Date(format(Sys.Date(), "%Y-01-01")), end = Sys.Date(), ns = ns),
      racafeForms::racafeInputSelect("nivel", "Agrupar por",
        choices = c("Producto" = "producto", "Categoría" = "categoria"),
        ns = ns),
      racafeForms::racafeInputSelect("medida", "Medida de los gráficos", choices = c("Sacos 70 kg" = "Sacos",
        "Margen FCC" = "Margen", "Margen por kilo" = "MargenKg", "Clientes" = "Clientes"), ns = ns)),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputNumeric("top", "Productos/categorías del ranking", value = 15, min = 3, max = 40,
        format = "number", decimals = 0, ns = ns),
      racafeForms::racafeInputSwitch("cerrados", "Solo meses cerrados financieramente", value = FALSE, ns = ns)),
    .gc_tarjetas_ui(ns, .PROD_CAJAS),
    shiny::fluidRow(shiny::column(6, plotly::plotlyOutput(ns("ranking"), height = "340px")),
      shiny::column(6, plotly::plotlyOutput(ns("pareto"), height = "340px"))),
    shiny::fluidRow(shiny::column(6, plotly::plotlyOutput(ns("tendencia"), height = "280px")),
      shiny::column(6, plotly::plotlyOutput(ns("rentabilidad"), height = "280px"))),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("productos"), titulo = "Ejecución por producto o categoría"),
    .crm_tabla_ui(ns("calidad"), titulo = "Cobertura y calidad del periodo"),
    .crm_nota(c("Se aplican los filtros del controlbar y el periodo de facturación indicado aquí.",
      "Las cantidades operativas de lote no se suman: solo hechos facturados, sin proyecciones de pedidos.",
      "El margen total conserva ajustes y devoluciones. El abierto es provisional y se presenta también por separado.",
      "Margen/kg = margen neto / (sacos netos × 70); no se promedian márgenes unitarios.",
      "Pareto usa sacos positivos; la tabla conserva negativos y productos sin homologar.",
      "Pulse una fila para consultar sus clientes y movimientos. No se asignan presupuestos de cliente a productos.")))
}
ProductosEjecucion <- function(id, hechos) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive({
      shiny::req(length(input$periodo) == 2L)
      .prod_preparar(hechos(), as.Date(input$periodo), isTRUE(input$cerrados))
    })
    nivel <- shiny::reactive(.com_elegir(input$nivel, c("producto", "categoria"), "producto"))
    tabla <- shiny::reactive(.prod_resumen(datos(), nivel()))
    medida <- shiny::reactive(.com_elegir(input$medida, c("Sacos", "Margen", "MargenKg", "Clientes"), "Sacos"))
    top <- shiny::reactive({
      if (length(input$top) != 1L || !is.finite(input$top)) return(15L)
      as.integer(min(40, max(3, input$top)))
    })
    .gc_tarjetas_server(.PROD_CAJAS, shiny::reactive(.prod_indicadores(datos())),
      list(sacos = "numero", margen = "dinero", margen_kg = "dinero", cerrado = "dinero", abierto = "dinero"))
    calidad <- shiny::reactive({
      x <- datos()
      data.frame(Movimientos = nrow(x), SinProductoHomologado = sum(!x$ProductoValido),
        SinPedido = sum(is.na(x$Pedido)), SinSacos = sum(is.na(x$SacosFact)), SinMargen = sum(is.na(x$Margen)),
        AjustesNegativos = sum(x$SacosFact < 0 | x$Margen < 0, na.rm = TRUE),
        MovimientosAbiertos = sum(x$EstadoFinanciero == "FACTURADO_ABIERTO"))
    })
    .crm_vancouver_server("calidad", data = calidad)
    .crm_tabla_servidor("productos", data = tabla, id_col = "Grupo", modo_seleccion = "fila",
      columnas = c("Linea", "Categoria", "Producto", "Sacos", "Margen", "MargenKg",
        "Clientes", "Pedidos", "Lotes", "MargenCerrado", "MargenAbierto", "Participacion"),
      col_labels = c(Linea = "Línea", Categoria = "Categoría", Sacos = "Sacos 70 kg", MargenKg = "Margen/kg",
        MargenCerrado = "Margen cerrado", MargenAbierto = "Margen provisional", Participacion = "% sacos positivos"),
      col_specs = list(Participacion = list(format = list(percent = TRUE, digits = 1))),
      modal_titulo_fn = function(sel) "Detalle de ejecución del producto",
      modal_contenido_fn = function(sel) {
        fila <- tabla()[tabla()$Grupo == sel$id, ]
        x <- datos()
        x <- x[x$Linea == fila$Linea & x$Categoria == fila$Categoria, ]
        if (nivel() == "producto") x <- x[x$Producto == fila$Producto, ]
        cols <- intersect(c("NIT", "Cliente", "Linea", "Categoria", "Producto", "Pedido", "Lote", "FecFact",
          "SacosFact", "Margen", "EstadoFinanciero"), names(x))
        x <- x[order(x$FecFact, decreasing = TRUE), ]
        shiny::tagList(.crm_tabla_vancouver(head(x[cols], 200), font_size = 11, row_padding = 2),
          .crm_nota(paste("Vista previa de", min(200, nrow(x)), "de", nrow(x),
            "movimientos. La descarga contiene el detalle completo del periodo.")))
      })
    ranking <- shiny::reactive({
      x <- tabla()
      x <- x[order(abs(x[[medida()]]), decreasing = TRUE, na.last = TRUE), ]
      head(x, top())
    })
    output$ranking <- .crm_render_plotly({
      x <- ranking()
      shiny::validate(shiny::need(nrow(x) > 0, "No hay ejecución para este periodo."))
      x$Hover <- paste(htmltools::htmlEscape(x$Etiqueta), "<br>Medida:", .prod_medida_label(medida()),
        "<br>Valor:", format(x[[medida()]], big.mark = ","), "<br>Clientes:", x$Clientes)
      p <- ggplot2::ggplot(x, ggplot2::aes(x = stats::reorder(Etiqueta, .data[[medida()]]),
        y = .data[[medida()]], text = Hover)) +
        ggplot2::geom_col(fill = "#DA291C", na.rm = TRUE) + ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = .prod_medida_label(medida()), title = "Ranking de contribución neta") +
        .crm_tema_grafico()
      plotly::ggplotly(p, tooltip = "text")
    })
    output$pareto <- .crm_render_plotly({
      x <- tabla(); x <- x[order(x$Participacion, decreasing = TRUE), ]
      x$Acumulado <- cumsum(x$Participacion)
      x$Posicion <- seq_len(nrow(x))
      shiny::validate(shiny::need(nrow(x) && any(is.finite(x$Acumulado)), "Sin contribución positiva de sacos."))
      p <- ggplot2::ggplot(x, ggplot2::aes(x = Posicion, y = Acumulado,
        text = paste(htmltools::htmlEscape(Etiqueta), "<br>Acumulado:", scales::percent(Acumulado),
          "<br>Sacos netos:", scales::comma(Sacos)))) +
        ggplot2::geom_line(data = if (nrow(x) > 1L) x else x[0, ],
          colour = "#DA291C", linewidth = 0.7, na.rm = TRUE) +
        ggplot2::geom_point(colour = "#DA291C", size = 1.5, na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", colour = "#6C757D") +
        ggplot2::scale_y_continuous(labels = scales::label_percent()) +
        ggplot2::labs(x = "Productos/categorías ordenados", y = "Participación acumulada",
          title = "Concentración 80/20") +
        .crm_tema_grafico()
      plotly::ggplotly(p, tooltip = "text")
    })
    serie <- shiny::reactive({
      x <- datos()
      etiquetas <- ranking()$Etiqueta
      x$Serie <- if (nivel() == "categoria") paste(x$Linea, x$Categoria, sep = " › ") else x$Etiqueta
      principales <- head(etiquetas, 5)
      x$Serie[!x$Serie %in% principales] <- "Otros"
      x %>% dplyr::group_by(Mes, Serie) %>% dplyr::summarise(Sacos = .prod_suma(SacosFact),
        Margen = .prod_suma(Margen), Clientes = dplyr::n_distinct(NIT[!is.na(NIT)]), .groups = "drop") %>%
        dplyr::mutate(MargenKg = .prod_div(Margen, Sacos * 70))
    })
    output$tendencia <- .crm_render_plotly({
      x <- serie()
      shiny::validate(shiny::need(nrow(x) > 0, "Sin serie temporal."))
      p <- ggplot2::ggplot(x, ggplot2::aes(x = Mes, y = .data[[medida()]], colour = Serie, group = Serie,
        text = paste(htmltools::htmlEscape(Serie), "<br>Mes:", format(Mes, "%Y-%m"),
          "<br>Sacos:", scales::comma(Sacos), "<br>Margen COP:", scales::comma(Margen),
          "<br>Margen/kg COP:", scales::comma(MargenKg), "<br>Clientes:", Clientes))) +
        ggplot2::geom_line(data = x %>% dplyr::group_by(Serie) %>% dplyr::filter(dplyr::n() > 1L),
          linewidth = 0.6, na.rm = TRUE) + ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = grDevices::colorRampPalette(
          c("#DA291C", "#6C757D", "#212529"))(dplyr::n_distinct(x$Serie))) +
        ggplot2::labs(x = NULL, y = .prod_medida_label(medida()),
          title = "Evolución de los cinco principales y resto") +
        .crm_tema_grafico() + ggplot2::theme(legend.position = "bottom")
      plotly::ggplotly(p, tooltip = "text")
    })
    output$rentabilidad <- .crm_render_plotly({
      x <- tabla()
      shiny::validate(shiny::need(nrow(x) > 0, "Sin productos en el periodo."))
      p <- ggplot2::ggplot(x, ggplot2::aes(x = Sacos, y = MargenKg, size = Clientes,
        text = paste(htmltools::htmlEscape(Etiqueta), "<br>Sacos:", scales::comma(Sacos),
          "<br>Margen/kg COP:", scales::comma(MargenKg), "<br>Clientes:", Clientes))) +
        ggplot2::geom_point(colour = "#DA291C", alpha = 0.65, na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = 0, colour = "#6C757D", linetype = "dashed") +
        ggplot2::labs(x = "Sacos netos de 70 kg", y = "Margen neto/kg", title = "Volumen, rentabilidad y alcance") +
        .crm_tema_grafico()
      plotly::ggplotly(p, tooltip = "text")
    })
    output$descargar <- shiny::downloadHandler(filename = function() paste0("Productos_", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(list(Resumen = tabla(), Serie = serie(),
        Movimientos = .crm_nits_tabla(datos()), Calidad = calidad()), file))
    list(datos = datos, resumen = tabla)
  })
}
