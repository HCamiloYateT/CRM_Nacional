# Tablero dinámico de ejecución por categoría y producto ----
.PROD_CAJAS <- c(sacos = "Sacos facturados (70 kg)", margen = "Margen FCC total",
  productos = "Productos con movimiento",
  clientes = "Clientes con compra", unidades = "Unidades comerciales", margen_kg = "Margen por kilo",
    pedidos = "Pedidos facturados",
  cerrado = "Margen contable cerrado", abierto = "Margen abierto provisional")
ProductosEjecucionUI <- function(id, periodo_local = FALSE) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    do.call(racafeForms::racafeFormRow, c(list(columns = 4), Filter(Negate(is.null), list(
      if (periodo_local) racafeForms::racafeInputDateRange("periodo", "Periodo de facturación",
        start = as.Date(format(Sys.Date(), "%Y-01-01")), end = Sys.Date(), ns = ns),
      .crm_select("nivel", "Agrupar por",
        choices = c("Producto" = "producto", "Categoría" = "categoria"),
        ns = ns),
      .crm_select("medida", "Medida de los gráficos", choices = c("Sacos 70 kg" = "Sacos",
        "Margen FCC" = "Margen", "Margen por kilo" = "MargenKg", "Clientes" = "Clientes",
          "Unidades comerciales" = "Unidades"), ns = ns))))),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputNumeric("top", "Productos/categorías del ranking", value = 15, min = 3, max = 40,
        format = "number", decimals = 0, ns = ns),
      racafeForms::racafeInputSwitch("cerrados", "Solo meses cerrados financieramente", value = FALSE, ns = ns)),
    .gc_tarjetas_ui(ns, .PROD_CAJAS),
    .crm_select("tamano_arbol", "Tamaño de categoría y producto", choices = c("Unidades comerciales" = "Unidades",
      "Sacos" = "Sacos", "Margen" = "Margen"), ns = ns),
    .crm_plot_output(ns("arbol_productos")),
    .crm_nota("El tamaño usa valores positivos. Un cliente puede comprar varios productos; sus conteos no se suman."),
    shiny::fluidRow(shiny::column(6, .crm_plot_output(ns("ranking"), height = "340px")),
      shiny::column(6, .crm_plot_output(ns("pareto"), height = "340px"))),
    shiny::fluidRow(shiny::column(6, .crm_plot_output(ns("tendencia"), height = "280px")),
      shiny::column(6, .crm_plot_output(ns("rentabilidad"), height = "280px"))),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("productos"), titulo = "Ejecución por producto o categoría"),
    .crm_tabla_ui(ns("calidad"), titulo = "Cobertura y calidad del periodo"),
    .crm_nota(c("Se aplican los filtros del controlbar y su periodo de facturación.",
      "Las cantidades operativas de lote no se suman: solo hechos facturados, sin proyecciones de pedidos.",
      "El margen total conserva ajustes y devoluciones. El abierto es provisional y se presenta también por separado.",
      "Margen/kg = margen neto / (sacos netos × 70); no se promedian márgenes unitarios.",
      "Pareto usa sacos positivos; la tabla conserva negativos y productos sin homologar.",
      "Pulse una fila para consultar sus clientes y movimientos. No se asignan presupuestos de cliente a productos.")))
}
ProductosEjecucion <- function(id, hechos, filtros = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive({
      periodo <- if (is.null(filtros)) input$periodo else filtros()$fecha
      shiny::req(length(periodo) == 2L)
      .prod_preparar(hechos(), as.Date(periodo), isTRUE(input$cerrados))
    })
    nivel <- shiny::reactive(.com_elegir(input$nivel, c("producto", "categoria"), "producto"))
    tabla <- shiny::reactive(.prod_resumen(datos(), nivel()))
    medida <- shiny::reactive(.com_elegir(input$medida, c("Sacos", "Margen", "MargenKg", "Clientes",
      "Unidades"), "Sacos"))
    top <- shiny::reactive({
      if (length(input$top) != 1L || !is.finite(input$top)) return(15L)
      as.integer(min(40, max(3, input$top)))
    })
    .gc_tarjetas_server(.PROD_CAJAS, shiny::reactive(.prod_indicadores(datos())),
      list(sacos = "numero", margen = "dinero", margen_kg = "dinero", cerrado = "dinero", abierto = "dinero"))
    output$arbol_productos <- .crm_render_plotly({
      x <- datos()
      mes_referencia <- as.Date(format(if (is.null(filtros)) tail(input$periodo,
        1L) else filtros()$fecha[[2]], "%Y-%m-01"))
      resumir <- function(z, claves) z %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
        dplyr::summarise(Clientes = dplyr::n_distinct(NIT[!is.na(NIT) & SacosFact > 0]),
          Unidades = dplyr::n_distinct(paste(NIT[!is.na(NIT)], Linea[!is.na(NIT)], sep = "|")),
          Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen),
          SacosMes = .com_suma(SacosFact[FecFact >= mes_referencia]),
          MargenMes = .com_suma(Margen[FecFact >= mes_referencia]),
          Primera = .gc_fecha_min(FecFact), Ultima = .gc_fecha_max(FecFact), .groups = "drop")
      hojas <- resumir(x, c("Linea", "Categoria", "Producto"))
      categorias <- resumir(x, c("Linea", "Categoria"))
      medida_arbol <- .com_elegir(input$tamano_arbol, c("Unidades", "Sacos", "Margen"), "Unidades")
      hojas$Id <- paste("P", hojas$Linea, hojas$Categoria, hojas$Producto, sep = "|")
      hojas$Padre <- paste("C", hojas$Linea, hojas$Categoria, sep = "|")
      hojas$Etiqueta <- hojas$Producto
      hojas$Tamano <- pmax(0, hojas[[medida_arbol]])
      categorias$Id <- paste("C", categorias$Linea, categorias$Categoria, sep = "|")
      categorias$Padre <- ""; categorias$Etiqueta <- paste(categorias$Linea, categorias$Categoria, sep = " · ")
      categorias$Tamano <- 0
      categorias$Padre <- paste("L", categorias$Linea, sep = "|")
      lineas <- resumir(x, "Linea")
      lineas$Id <- paste("L", lineas$Linea, sep = "|")
      lineas$Padre <- ""; lineas$Etiqueta <- lineas$Linea; lineas$Tamano <- 0
      arbol <- dplyr::bind_rows(lineas, categorias, hojas)
      arbol$Color <- c(unname(.crm_colores("linneg", lineas$Linea)),
        unname(.crm_colores("categoria", categorias$Categoria, categorias$Linea)),
        unname(.crm_colores("producto", hojas$Producto, hojas$Linea, hojas$Categoria)))
      shiny::validate(shiny::need(any(hojas$Tamano > 0, na.rm = TRUE), "No hay valores positivos para esta medida."))
      arbol$Tamano[!is.finite(arbol$Tamano)] <- 0
      plotly::plot_ly(arbol, type = "treemap", ids = ~Id, labels = ~Etiqueta, parents = ~Padre, values = ~Tamano,
        branchvalues = "remainder", customdata = .crm_customdata(arbol, c("Clientes", "Unidades", "Sacos",
          "Margen", "SacosMes", "MargenMes", "Primera", "Ultima")),
        marker = list(colors = arbol$Color),
        hovertemplate = paste0("<b>%{label}</b><br><b>CLIENTES:</b> %{customdata[0]:.0f}",
          "<br><b>UNIDADES COMERCIALES:</b> %{customdata[1]:.0f}<br><b>SACOS:</b> %{customdata[2]:,.2f}",
          "<br><b>MARGEN DEL PERIODO COP:</b> $%{customdata[3]:,.0f}",
          "<br><b>SACOS DEL MES DE REFERENCIA:</b> %{customdata[4]:,.2f}",
          "<br><b>MARGEN DEL MES COP:</b> $%{customdata[5]:,.0f}",
          "<br><b>PRIMERA FACTURA:</b> %{customdata[6]}<br><b>ÚLTIMA FACTURA:</b> %{customdata[7]}<extra></extra>"))
    })
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
        "Clientes", "Unidades", "Pedidos", "Lotes", "MargenCerrado", "MargenAbierto", "Participacion"),
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
      colores <- unname(.crm_colores(if (nivel() == "categoria") "categoria" else "producto",
        if (nivel() == "categoria") x$Categoria else x$Producto, x$Linea,
        if (nivel() == "categoria") "" else x$Categoria))
      x$Valor <- x[[medida()]]
      moneda <- medida() %in% c("Margen", "MargenKg")
      plotly::plot_ly(x, x = ~Valor, y = ~Etiqueta, type = "bar", orientation = "h", marker = list(color = colores),
        customdata = .crm_customdata(x, c("Clientes", "Unidades", "Sacos", "Margen")),
        hovertemplate = paste0("<b>%{y}</b><br><b>", .prod_medida_label(medida()), ":</b> ",
          if (moneda) "$" else "", "%{x:,.2f}<br><b>CLIENTES:</b> %{customdata[0]:.0f}",
          "<br><b>UNIDADES:</b> %{customdata[1]:.0f}<br><b>SACOS:</b> %{customdata[2]:,.2f}",
          "<br><b>MARGEN COP:</b> $%{customdata[3]:,.0f}<extra></extra>")) %>%
        plotly::layout(title = "Ranking de contribución neta", xaxis = list(title = .prod_medida_label(medida())),
          yaxis = list(title = "", categoryorder = "array", categoryarray = rev(x$Etiqueta)))

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
        Margen = .prod_suma(Margen), Clientes = dplyr::n_distinct(NIT[!is.na(NIT)]),
        Unidades = dplyr::n_distinct(paste(NIT[!is.na(NIT)], Linea[!is.na(NIT)], sep = "|")), .groups = "drop") %>%
        dplyr::mutate(MargenKg = .prod_div(Margen, Sacos * 70))
    })
    output$tendencia <- .crm_render_plotly({
      x <- serie()
      shiny::validate(shiny::need(nrow(x) > 0, "Sin serie temporal."))
      mapa <- datos() %>% dplyr::distinct(Etiqueta, Linea, Categoria, Producto)
      color <- .crm_colores(if (nivel() == "categoria") "categoria" else "producto",
        if (nivel() == "categoria") mapa$Categoria else mapa$Producto, mapa$Linea,
        if (nivel() == "categoria") "" else mapa$Categoria)
      if (nivel() == "categoria") mapa$Etiqueta <- paste(mapa$Linea, mapa$Categoria, sep = " › ")
      names(color) <- mapa$Etiqueta
      color <- c(color[!duplicated(names(color))], Otros = "#ADB5BD")
      p <- ggplot2::ggplot(x, ggplot2::aes(x = Mes, y = .data[[medida()]], colour = Serie, group = Serie,
        text = paste(htmltools::htmlEscape(Serie), "<br>Mes:", format(Mes, "%Y-%m"),
          "<br>Sacos:", scales::comma(Sacos), "<br>Margen COP:", scales::comma(Margen),
          "<br>Margen/kg COP:", scales::comma(MargenKg), "<br>Clientes:", Clientes))) +
        ggplot2::geom_line(data = x %>% dplyr::group_by(Serie) %>% dplyr::filter(dplyr::n() > 1L),
          linewidth = 0.6, na.rm = TRUE) + ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = color) +
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
