# Participación: mismo intervalo y alcance comercial del controlbar ----
AnalisisComercialUI <- function(id, tipo = "participacion", fecha = Sys.Date()) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeFormRow(columns = 3,
      .crm_select("dimension", "Dimensión", choices = .COM_DIMENSIONES, selected = "CLLinNegNo", ns = ns),
      .crm_select("medida", "Medida", choices = c("Sacos de 70 kg" = "Sacos", "Margen FCC · COP" = "Margen"),
        selected = "Sacos", ns = ns),
      .crm_select("representacion", "Representación", choices = c("Valores absolutos" = "absolutos",
        "Participación · categoría y producto" = "participacion"), selected = "absolutos", ns = ns)),
    racafeForms::racafeInputSwitch("abiertos", "Incluir facturación sin cierre financiero", value = FALSE, ns = ns),
    .gc_tarjetas_ui(ns, c(sacos = "Sacos de 70 kg", margen = "Margen FCC · COP", kilos = "Margen por kg · COP")),
    .crm_plot_output(ns("grafico")), .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("dimensiones")),
    .crm_nota_metodologica(c("El periodo y el alcance comercial provienen del controlbar.",
      "Los sacos y el margen se suman desde el mismo universo; el porcentaje usa el total completo seleccionado.",
      "El treemap representa sacos positivos por categoría y producto; el color muestra margen, que puede ser negativo.",
      "El margen por kilogramo divide el margen total entre sacos × 70. No es el precio de venta.",
      "La facturación sin cierre conserva su condición provisional. Un margen desconocido permanece vacío.")))
}
AnalisisComercial <- function(id, hechos, clientes, filtros, tipo = "participacion", historico = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    fuente <- shiny::reactive({
      f <- filtros(); shiny::req(length(f$fecha) == 2L)
      x <- .gc_filtrar_dimensiones(hechos(), f)
      x <- .com_hechos(x, as.Date(f$fecha[[2]]), cerrados = !isTRUE(input$abiertos))
      x[!is.na(x$FecFact) & x$FecFact >= as.Date(f$fecha[[1]]) & x$FecFact <= as.Date(f$fecha[[2]]), , drop = FALSE]
    })
    dimension <- shiny::reactive(.com_elegir(input$dimension, unname(.COM_DIMENSIONES), "CLLinNegNo"))
    tabla <- shiny::reactive({
      x <- fuente(); d <- dimension()
      if (d %in% c("Categoria", "Producto")) {
        resumen <- .crm_resumen_dimension(x, c("Categoria", "Producto"))
      } else {
        resumen <- .crm_resumen_dimension(x, d)
      }
      if (nrow(resumen)) resumen$MargenKg <- .gc_ratio(resumen$Margen, resumen$Sacos * 70)
      resumen
    })
    .gc_tarjetas_server(c(sacos = "Sacos de 70 kg", margen = "Margen FCC · COP", kilos = "Margen por kg · COP"),
      shiny::reactive({ x <- fuente(); s <- .com_suma(x$SacosFact); m <- .com_suma(x$Margen)
        list(sacos = s, margen = m, kilos = .gc_ratio(m, s * 70)) }),
      list(sacos = "numero", margen = "dinero", kilos = "dinero"))
    .crm_vancouver_server("dimensiones", tabla,
      col_labels = c(CLLinNegNo = "Línea de negocio", Categoria = "Categoría", Producto = "Producto",
        Sacos = "Sacos de 70 kg", Margen = "Margen FCC · COP", ParticipacionSacos = "Participación en sacos",
        ParticipacionMargen = "Participación en margen", MargenKg = "Margen por kg · COP"),
      col_specs = list(ParticipacionSacos = list(format = list(percent = TRUE, digits = 1)),
        ParticipacionMargen = list(format = list(percent = TRUE, digits = 1))))
    output$grafico <- .crm_render_plotly({
      if (identical(input$representacion, "participacion")) {
        .crm_treemap_productos(.crm_resumen_dimension(fuente(), c("Categoria", "Producto")))
      } else {
        x <- tabla(); shiny::validate(shiny::need(nrow(x) > 0L, "No hay datos para este intervalo y selección."))
        medida <- .com_elegir(input$medida, c("Sacos", "Margen"), "Sacos")
        jerarquia <- all(c("Categoria", "Producto") %in% names(x))
        eje <- if (jerarquia) "Categoria" else dimension()
        x$Valor <- x[[medida]]; x$Etiqueta <- as.character(x[[eje]])
        x$Detalle <- paste0(x$Etiqueta, if (jerarquia) paste0(" · ", x$Producto) else "",
          "<br>Sacos: ", format(round(x$Sacos, 2), big.mark = ","),
          "<br>Margen: ", format(round(x$Margen), big.mark = ","), " COP",
          "<br>Participación en sacos: ", round(100 * x$ParticipacionSacos, 1), "%")
        p <- plotly::plot_ly(x, x = ~Etiqueta, y = ~Valor, text = ~Detalle, type = "bar",
          hovertemplate = "%{text}<extra></extra>")
        if (jerarquia) {
          p <- plotly::plot_ly(x, x = ~Etiqueta, y = ~Valor, color = ~Producto, type = "bar", text = ~Detalle,
            colors = grDevices::colorRampPalette(c("#E9DDC8", "#9B7160", "#7A2028"))(max(1L, dplyr::n_distinct(x$Producto))),
            hovertemplate = "%{text}<extra></extra>")
        } else p <- plotly::style(p, marker = list(color = "#7A2028"))
        plotly::layout(p, barmode = "relative", yaxis = list(title = if (medida == "Sacos") "Sacos de 70 kg" else "Margen · COP"),
          xaxis = list(title = ""))
      }
    })
    .gc_descarga(output, tabla, "Participacion")
    list(datos = tabla)
  })
}
