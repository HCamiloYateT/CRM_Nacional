# Orquestación de análisis, validación temporal y salida reproducible ----
.mba_analizar <- function(cestas, soporte, minimo, longitud, confianza, lift, validacion = FALSE, estadistica = TRUE) {
  ids <- cestas$cestas$Cesta
  validos <- integer()
  corte <- as.Date(NA)
  if (validacion) {
    fechas <- sort(unique(cestas$cestas$Fecha))
    if (length(fechas) < 2L) stop("La validación temporal requiere cestas en al menos dos fechas distintas.")
    corte <- fechas[[min(length(fechas) - 1L, max(1L, floor(length(fechas) * 0.7)))]]
    ids <- cestas$cestas$Cesta[cestas$cestas$Fecha <= corte]
    validos <- cestas$cestas$Cesta[cestas$cestas$Fecha > corte]
  }
  modelo <- .mba_itemsets(cestas$membresia, ids, soporte, minimo, longitud)
  reglas <- .mba_reglas(modelo, confianza, lift, estadistica)
  if (validacion) reglas <- .mba_evaluar(reglas, cestas$membresia, validos)
  conjuntos <- lapply(modelo$conjuntos, function(x) data.frame(
    Conjunto = .mba_etiquetas(.mba_clave(x$items), cestas$mapa),
    Longitud = length(x$items), Cestas = x$cuenta, Soporte = x$cuenta / modelo$n))
  conjuntos <- if (length(conjuntos)) dplyr::bind_rows(conjuntos) else {
    data.frame(Conjunto = character(), Longitud = integer(), Cestas = integer(), Soporte = numeric())
  }
  frecuencias <- cestas$membresia %>% dplyr::filter(Cesta %in% ids) %>% dplyr::count(Item, name = "Cestas") %>%
    dplyr::left_join(cestas$mapa, by = "Item") %>%
    dplyr::mutate(Soporte = .prod_div(Cestas, length(ids))) %>% dplyr::arrange(dplyr::desc(Cestas))
  recomendaciones <- .mba_recomendaciones(reglas, cestas$movimientos, cestas$mapa)
  reglas$SiCompra <- .mba_etiquetas(reglas$Antecedente, cestas$mapa)
  reglas$TambienCompra <- .mba_etiquetas(reglas$Consecuente, cestas$mapa)
  list(reglas = reglas, conjuntos = conjuntos, frecuencias = frecuencias, recomendaciones = recomendaciones,
    base = cestas, diagnostico = data.frame(CestasObservadas = nrow(cestas$cestas), CestasModelo = length(ids),
      CestasValidacion = length(validos), CorteModelo = corte, MinimoEfectivo = modelo$minimo,
      CandidatosProbados = modelo$candidatos, ReglasRetenidas = nrow(reglas), ItemsObservados = nrow(cestas$mapa)))
}
.MBA_CAJAS <- c(cestas = "Cestas observadas", multiples = "Cestas con varios ítems", items = "Ítems observados",
  reglas = "Reglas retenidas")
MarketBasketUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    shiny::tags$div(class = "crm-basket-guia",
      .crm_disclosure("Qué es Market Basket y cómo usar este tablero",
        racafeForms::racafeNoticeList(list(
          shiny::tagList(shiny::tags$strong("Qué muestra. "),
            "Productos que un mismo cliente suele comprar. Sirve para explorar qué más ofrecerle."),
          shiny::tagList(shiny::tags$strong("Cómo empezar. "),
            "Elija clientes y fechas en el controlbar. Después pulse Analizar."),
          shiny::tagList(shiny::tags$strong("Cómo leer A → B. "),
            "Quien compra A también aparece comprando B. Revise cuántas compras respaldan la relación."),
          shiny::tagList(shiny::tags$strong("Soporte. "),
            "De cada 100 compras del tablero, cuántas incluyen ambos productos."),
          shiny::tagList(shiny::tags$strong("Confianza. "),
            "De cada 100 compras que incluyen A, cuántas también incluyen B."),
          shiny::tagList(shiny::tags$strong("Afinidad (lift). "),
            "Mayor que 1 indica que se compran juntos más de lo habitual; menor que 1, menos."),
          shiny::tagList(shiny::tags$strong("Antes de ofrecer. "),
            "El inicio permite relaciones vistas una sola vez. Confirme si tienen sentido para el cliente.")),
          font_size = 13, text_color = "#495057", accent_color = "#7A2028"), open = FALSE)),
    racafeForms::racafeFormRow(columns = 3,
      .crm_select("cesta", .crm_label_ayuda("Unidad de cesta", "Pedido agrupa productos de una misma compra; cliente agrupa su historial del periodo."), choices = c("Pedido facturado" = "pedido",
        "Cliente principal y mes" = "cliente_mes", "Cliente principal en el periodo" = "cliente"),
        selected = "cliente", ns = ns),
      .crm_select("nivel", .crm_label_ayuda("Ítem", "Elija productos o categorías para buscar combinaciones."),
        choices = c("Producto" = "producto", "Categoría" = "categoria"), ns = ns)),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputNumeric("soporte", .crm_label_ayuda("Soporte mínimo (%)", "De 0 a 100: porcentaje de cestas con la combinación. Un valor menor deja pasar combinaciones raras."), value = 0, min = 0, max = 100, ns = ns),
      racafeForms::racafeInputNumeric("confianza", .crm_label_ayuda("Confianza mínima (%)", "De 0 a 100: entre quienes compran A, cuántos compran B. Un valor menor muestra más reglas."), value = 0, min = 0, max = 100, ns = ns),
      racafeForms::racafeInputNumeric("lift", .crm_label_ayuda("Lift mínimo", "0 no restringe. 1 es lo esperado sin asociación; mayor que 1 indica que se compran juntos más de lo esperado."), value = 0, min = 0, ns = ns)),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputNumeric("minimo", .crm_label_ayuda("Mínimo de cestas conjuntas", "Compras que deben respaldar una combinación. Un valor mayor exige más evidencia."), value = 1, min = 1,
        decimals = 0, ns = ns),
      racafeForms::racafeInputNumeric("longitud", .crm_label_ayuda("Máximo de ítems por conjunto", "De 2 a 5 productos por combinación. Más ítems requieren más cálculo y más datos."), value = 2, min = 2, max = 5,
        decimals = 0, ns = ns),
      .crm_select("vista", .crm_label_ayuda("Tabla", "Elija el detalle que quiere revisar de los mismos resultados."), choices = c("Reglas", "Métricas detalladas", "Ítems frecuentes",
        "Conjuntos frecuentes",
        "Venta cruzada", "Tamaño de cestas", "Calidad y alcance"), ns = ns)),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputSwitch("cerrados", .crm_label_ayuda("Solo meses cerrados", "Excluye facturas de meses abiertos si está activado."), value = FALSE, ns = ns),
      racafeForms::racafeInputSwitch("validacion", .crm_label_ayuda("Validación temporal 70/30 por fechas", "Busca las reglas en el primer 70% del tiempo y comprueba si se mantienen después."), value = FALSE, ns = ns),
      racafeForms::racafeInputSwitch("estadistica", .crm_label_ayuda("Comprobación estadística avanzada (Fisher/BH)", "Revisa si la asociación podría deberse al azar. Puede ocultar reglas con pocas compras."),
        value = FALSE, ns = ns)),
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("analizar", "Analizar",
      icon = "magnifying-glass", ns = ns)),
    shiny::uiOutput(ns("estado")), .gc_tarjetas_ui(ns, .MBA_CAJAS),
    shiny::fluidRow(shiny::column(6, .crm_plot_output(ns("fuerza"), height = "300px")),
      shiny::column(6, .crm_plot_output(ns("pares"), height = "300px"))),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("tabla")),
    .crm_tabla_ui(ns("parametros"), titulo = "Alcance y parámetros del último análisis"),
    .crm_disclosure("Detalle técnico del análisis",
      .crm_nota(c("Pedido significa pedido observado facturado en el periodo, no factura individual.",
      "Cliente-mes y cliente-periodo miden mezcla de compras, no simultaneidad ni causalidad.",
      "La cantidad neta por cesta/ítem debe ser positiva; las compras anuladas por devoluciones quedan fuera.",
      "Se requiere categoría/producto válido. Las compras observadas no equivalen al inventario actual del cliente.",
      "Producto/categoría del controlbar seleccionan cestas que los contengan y conservan sus acompañantes.",
      "El denominador incluye cestas de un ítem y cestas sin productos frecuentes.",
      "Apriori enumera conjuntos y reglas dentro de soporte/longitud elegidos; un límite de recursos detiene el cálculo.",
      "Fisher/BH se aplica a las particiones frecuentes probadas, antes del filtro de confianza/lift. Es exploratorio.",
      "Las cestas de un mismo cliente pueden depender entre sí; p/q no garantizan validez comercial ni efecto causal.",
      "Convicción infinita indica confianza 100% con consecuente no universal; un caso indeterminado queda vacío.",
      "Venta cruzada muestra hasta 200 hipótesis priorizadas por lift/confianza/evidencia, no demanda garantizada.",
      "Cambiar filtros requiere volver a Analizar. La descarga conserva el último resultado y sus parámetros."))))
}
MarketBasket <- function(id, hechos, filtros, on_result = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    calculo <- shiny::eventReactive(input$analizar, {
      tryCatch({
        periodo <- as.Date(filtros()$fecha)
        if (length(periodo) != 2L || anyNA(periodo)) stop("Seleccione un periodo válido en el controlbar.")
        parametros <- c(input$soporte, input$confianza, input$lift, input$minimo, input$longitud)
        if (length(parametros) != 5L || any(!is.finite(parametros)) || input$confianza < 0 ||
            input$confianza > 100 || input$lift < 0) stop("Revise los parámetros del análisis.")
        f <- filtros()
        tipo <- .com_elegir(input$cesta, c("pedido", "cliente_mes", "cliente"), "pedido")
        nivel <- .com_elegir(input$nivel, c("producto", "categoria"), "producto")
        fuente <- hechos()
        alcance <- f
        alcance$categoria <- unique(fuente$Categoria)
        alcance$producto <- unique(fuente$Producto)
        fuente <- .gc_filtrar_dimensiones(fuente, alcance)
        fuente <- .prod_preparar(fuente, periodo, isTRUE(input$cerrados))
        cestas <- .mba_cestas(fuente, tipo, nivel, f)
        resultado <- .mba_analizar(cestas, input$soporte / 100, input$minimo, as.integer(input$longitud),
          input$confianza / 100, input$lift, isTRUE(input$validacion), isTRUE(input$estadistica))
        resultado$parametros <- data.frame(Desde = periodo[[1]], Hasta = periodo[[2]],
          Cesta = tipo, Item = nivel, SoporteMinimo = input$soporte / 100, ConfianzaMinima = input$confianza / 100,
          LiftMinimo = input$lift, MinimoCestas = input$minimo, LongitudMaxima = as.integer(input$longitud),
          SoloCerrados = isTRUE(input$cerrados), ValidacionTemporal = isTRUE(input$validacion),
          PruebasEstadisticas = isTRUE(input$estadistica), FechaCalculo = Sys.time())
        resultado$alcance_filtros <- f
        resultado$fuente_firma <- hechos()
        resultado$filtros <- data.frame(Filtro = names(f),
          Valor = vapply(f, function(x) paste(x, collapse = " | "), character(1)))
        list(resultado = resultado, error = NULL)
      }, error = function(e) list(resultado = NULL, error = conditionMessage(e)))
    }, ignoreInit = TRUE)
    shiny::observeEvent(calculo(), {
      x <- calculo()
      if (is.function(on_result)) on_result(if (is.null(x$error)) nrow(x$resultado$reglas) else NA_real_)
    })
    resultado <- shiny::reactive({
      x <- calculo()
      shiny::validate(shiny::need(is.null(x$error), x$error))
      vigente <- identical(filtros(), x$resultado$alcance_filtros) && identical(hechos(), x$resultado$fuente_firma)
      shiny::validate(shiny::need(vigente,
        "Los filtros o la clasificación cambiaron. Pulse Analizar para actualizar los resultados."))
      x$resultado
    })
    output$estado <- .crm_render_ui({
      if (is.null(input$analizar) || input$analizar == 0) return(.crm_nota("Configure la cesta y pulse Analizar."))
      x <- calculo()
      if (!is.null(x$error)) return(racafeForms::racafeFormMessage(x$error, type = "error"))
      if (!(identical(filtros(), x$resultado$alcance_filtros) && identical(hechos(), x$resultado$fuente_firma))) {
        return(.crm_nota("Los filtros o la clasificación cambiaron. Pulse Analizar para actualizar los resultados."))
      }
      if (!nrow(x$resultado$reglas)) return(.crm_nota(c("No hay reglas que superen estos umbrales.",
        "Revise la cobertura; puede reducir soporte/mínimo o cambiar la definición de cesta.")))
      .crm_nota("Resultados del último cálculo. Asociación observada no implica causalidad.")
    })
    .gc_tarjetas_server(.MBA_CAJAS, shiny::reactive({
      r <- resultado()
      list(cestas = nrow(r$base$cestas), multiples = sum(r$base$cestas$Items > 1L),
        items = nrow(r$base$mapa), reglas = nrow(r$reglas))
    }))
    tabla <- shiny::reactive({
      r <- resultado()
      vista <- .com_elegir(input$vista, c("Reglas", "Métricas detalladas", "Ítems frecuentes",
        "Conjuntos frecuentes", "Venta cruzada",
        "Tamaño de cestas", "Calidad y alcance"), "Reglas")
      if (vista %in% c("Reglas", "Métricas detalladas")) {
        principales <- c("Regla", "SiCompra", "TambienCompra", "CestasAmbos", "Soporte", "Confianza", "Lift", "Qvalor")
        extra <- if (vista == "Métricas detalladas") c("Leverage", "Conviccion", "Jaccard", "Coseno", "Zhang",
          "Pvalor", "CestasAntecedente", "CestasConsecuente", "AntecedentesValidacion", "AmbosValidacion")
        return(dplyr::select(r$reglas, dplyr::any_of(c(principales, extra,
          "SoporteValidacion", "ConfianzaValidacion", "LiftValidacion"))))
      }
      if (vista == "Ítems frecuentes") return(r$frecuencias)
      if (vista == "Conjuntos frecuentes") return(r$conjuntos)
      if (vista == "Venta cruzada") return(r$recomendaciones)
      if (vista == "Tamaño de cestas") return(r$base$cestas %>% dplyr::count(Items, name = "Cestas"))
      r$base$cobertura
    })
    especificaciones <- stats::setNames(lapply(c("Soporte", "Confianza", "SoporteValidacion", "ConfianzaValidacion"),
      function(x) list(format = list(percent = TRUE, digits = 2))),
      c("Soporte", "Confianza", "SoporteValidacion", "ConfianzaValidacion"))
    for (p in c("Pvalor", "Qvalor")) especificaciones[[p]] <- list(cell = function(x) {
      if (is.na(x)) "—" else format.pval(x, digits = 3, eps = 0.001)
    })
    seleccion <- .crm_vancouver_server("tabla", data = tabla, modo_seleccion = "fila", col_specs = especificaciones,
      col_labels = c(SiCompra = "Si compra", TambienCompra = "También compra", CestasAmbos = "Cestas conjuntas",
        SoporteValidacion = "Soporte validación", ConfianzaValidacion = "Confianza validación",
        LiftValidacion = "Lift validación"))
    shiny::observeEvent(seleccion$seleccion(), {
      fila <- seleccion$seleccion()$data
      r <- resultado()
      if (!"Regla" %in% names(fila)) return(invisible(NULL))
      regla <- r$reglas[r$reglas$Regla == fila$Regla[[1]], ]
      items <- as.integer(strsplit(paste(regla$Antecedente, regla$Consecuente, sep = ","), ",", fixed = TRUE)[[1]])
      miembros <- r$base$membresia
      cestas <- Reduce(intersect, lapply(items, function(item) miembros$Cesta[miembros$Item == item]))
      evidencia <- r$base$movimientos[r$base$movimientos$Cesta %in% cestas, ]
      evidencia <- evidencia %>% dplyr::distinct(Cesta, NIT, Cliente, Pedido, FecFact, Categoria, Producto)
      metricas <- regla[setdiff(names(regla), c("Antecedente", "Consecuente"))]
      metricas <- data.frame(Metrica = names(metricas), Valor = vapply(metricas,
        function(x) if (is.na(x[[1]])) "—" else as.character(x[[1]]), character(1)))
      shiny::showModal(racafeForms::racafeModalDetail(title = "Evidencia observada de la regla", size = "subventana2",
        .crm_tabla_vancouver(metricas, font_size = 11, row_padding = 2), shiny::tags$h5("Cestas observadas"),
        .crm_tabla_vancouver(head(evidencia, 200), font_size = 11, row_padding = 2),
        .crm_nota(c("Cestas con todos los ítems de la regla; asociación no implica causalidad.",
          paste("Vista previa de", min(200, nrow(evidencia)), "de", nrow(evidencia),
            "movimientos. La descarga incluye membresías y mapa completo de ítems."))),
        footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
      seleccion$limpiar()
    })
    .crm_vancouver_server("parametros", data = shiny::reactive(dplyr::bind_cols(resultado()$parametros,
      resultado()$diagnostico)))
    output$fuerza <- .crm_render_plot({
      x <- resultado()$reglas
      shiny::validate(shiny::need(nrow(x) > 0, "Sin reglas para graficar."))
      x$Detalle <- paste0("SI COMPRA: ", x$SiCompra, "<br>TAMBIÉN COMPRA: ", x$TambienCompra,
        "<br>CESTAS EN COMÚN: ", x$CestasAmbos, "<br>SOPORTE: ", round(100 * x$Soporte, 2),
        "%<br>CONFIANZA: ", round(100 * x$Confianza, 2), "%<br>LIFT: ", round(x$Lift, 2))
      ggplot2::ggplot(x, ggplot2::aes(text = Detalle, x = Soporte, y = Confianza, size = CestasAmbos, colour = Lift)) +
        ggplot2::geom_point(alpha = 0.7) + ggplot2::scale_colour_gradient(low = "#6C757D", high = "#DA291C") +
        ggplot2::scale_x_continuous(labels = scales::label_percent()) +
        ggplot2::scale_y_continuous(labels = scales::label_percent()) +
        ggplot2::labs(title = "Soporte, confianza y lift", size = "Cestas") + .crm_tema_grafico()
    })
    output$pares <- .crm_render_plot({
      r <- resultado()
      x <- r$reglas[!grepl(",", r$reglas$Antecedente) & !grepl(",", r$reglas$Consecuente), ]
      principales <- head(as.character(r$frecuencias$Item), 12)
      x <- x[x$Antecedente %in% principales & x$Consecuente %in% principales, ]
      shiny::validate(shiny::need(nrow(x) > 0, "Sin reglas de pares para el mapa de lift."))
      ggplot2::ggplot(x, ggplot2::aes(x = SiCompra, y = TambienCompra, fill = Lift)) + ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "#6C757D", mid = "#F8F9FA", high = "#DA291C", midpoint = 1) +
        ggplot2::labs(x = "Antecedente", y = "Consecuente", title = "Lift entre los doce ítems más frecuentes") +
        .crm_tema_grafico() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })
    output$descargar <- shiny::downloadHandler(filename = function() paste0("MarketBasket_", Sys.Date(), ".xlsx"),
      content = function(file) {
        r <- resultado()
        reglas <- r$reglas
        if ("Conviccion" %in% names(reglas)) reglas$Conviccion <- ifelse(is.infinite(reglas$Conviccion),
          "Infinita", as.character(reglas$Conviccion))
        writexl::write_xlsx(list(Reglas = reglas, Conjuntos = r$conjuntos, Frecuencias = r$frecuencias,
          VentaCruzada = r$recomendaciones, Cestas = .crm_nits_tabla(r$base$cestas),
          Membresia = r$base$membresia, Items = r$base$mapa,
          Calidad = r$base$cobertura,
          Parametros = r$parametros, Filtros = r$filtros, Alcance = r$diagnostico), file)
      })
    list(resultado = resultado)
  })
}
