# Potencial mensual declarado frente a compras facturadas de la entidad comercial ----
.pot_comparar <- function(potencial, contactos, hechos, mes, kg_saco = 70) {
  if (!is.finite(kg_saco) || kg_saco <= 0) stop("Indique el peso válido del saco de potencial.")
  if (!all(c("CodContacto", "ConsumoEsperado", "UnidadConsumo") %in% names(potencial))) {
    stop("La fuente de potencial no contiene consumo y unidad por contacto.")
  }
  potencial$CodContacto <- as.character(potencial$CodContacto)
  contactos$CodContacto <- as.character(contactos$CodContacto)
  hechos$CodContacto <- as.character(hechos$CodContacto)
  if (anyDuplicated(contactos$CodContacto)) stop("Hay códigos de contacto duplicados en el directorio.")
  potencial <- dplyr::distinct(potencial)
  if (anyDuplicated(potencial$CodContacto)) stop("Hay potenciales contradictorios para una misma entidad comercial.")
  x <- dplyr::left_join(contactos[c("CodContacto", "PerCod", "PerRazSoc")], potencial, by = "CodContacto")
  x$NIT <- as.character(x$PerCod)
  valor <- .gc_num(x$ConsumoEsperado); unidad <- toupper(trimws(as.character(x$UnidadConsumo)))
  x$PotencialKgMes <- dplyr::case_when(valor < 0 ~ NA_real_, unidad == "SACOS/MES" ~ valor * kg_saco,
    unidad == "KG/MES" ~ valor, unidad == "TON/MES" ~ valor * 1000, TRUE ~ NA_real_)
  f <- hechos %>% dplyr::filter(!is.na(FecFact), .gc_mes(FecFact) == .gc_mes(mes),
    EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"), !is.na(CodContacto)) %>%
    dplyr::group_by(CodContacto) %>% dplyr::summarise(SacosFacturados70 = sum(SacosFact, na.rm = TRUE),
      MargenCOP = .com_suma(Margen), .groups = "drop")
  x <- dplyr::left_join(x, f, by = "CodContacto")
  sin_factura <- is.na(x$SacosFacturados70)
  x$SacosFacturados70[sin_factura] <- 0; x$MargenCOP[sin_factura] <- 0
  x$KgFacturados <- x$SacosFacturados70 * 70
  x$BrechaKg <- x$PotencialKgMes - x$KgFacturados
  x$Penetracion <- ifelse(is.finite(x$PotencialKgMes) & x$PotencialKgMes > 0,
    x$KgFacturados / x$PotencialKgMes, NA_real_)
  x$Revision <- dplyr::case_when(is.na(x$PotencialKgMes) ~ "Sin potencial comparable",
    x$PotencialKgMes == 0 ~ "Potencial declarado cero", x$Penetracion > 1 ~ "Facturación supera potencial",
    x$KgFacturados <= 0 ~ "Sin compra neta en el mes", TRUE ~ "Potencial por desarrollar")
  x[c("CodContacto", "NIT", "PerRazSoc", "ConsumoEsperado", "UnidadConsumo", "PotencialKgMes",
    "SacosFacturados70", "KgFacturados", "MargenCOP", "Penetracion", "BrechaKg", "Revision")]
}
PotencialMercadoUI <- function(id, periodo_local = FALSE) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("actualizar", "Actualizar", ns = ns,
      variant = "secondary", icon = "arrows-rotate")),
    racafeForms::racafeFormRow(columns = 2,
      if (periodo_local) .crm_input_mes("mes", "Año y mes de facturación", value = Sys.Date(), ns = ns),
      racafeForms::racafeInputNumeric("kg_saco", "Kg por saco declarado en el potencial", value = 70, min = 1,
        ns = ns)),
    .gc_tarjetas_ui(ns, c(entidades = "Entidades comerciales", comparable = "Con potencial comparable",
      facturados = "Kg facturados en el mes", brecha = "Brecha positiva de potencial · kg")),
    .crm_plot_output(ns("grafico"), height = "330px"), .crm_descarga_ui("descargar", ns),
    .crm_tabla_ui(ns("tabla")),
    .crm_nota(c("El potencial es consumo mensual actualmente declarado; no es presupuesto ni compromiso de compra.",
      "Facturación se compara en kg: los sacos de ejecución del CRM equivalen a 70 kg.",
      "El peso del saco declarado es configurable porque el formulario de potencial no conserva ese dato.",
      "La penetración compara volumen mensual, no COP. Margen COP se presenta por separado.",
      "Se mantienen entidades sin ventas y sin potencial para identificar oportunidades y datos faltantes.",
      "Se compara el último mes del periodo del controlbar. La ejecución puede cubrir solo parte de ese mes.")))
}
PotencialMercado <- function(id, usuario, es_admin, hechos, cargar = racafeBD::CargarDatos, filtros = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    fuentes <- shiny::eventReactive(input$actualizar, {
      list(potencial = cargar("CRMNALPOTENCIAL"),
        contactos = cargar("CRMNALCONTACTO"))
    }, ignoreNULL = FALSE)
    datos <- shiny::reactive({
      mes <- if (is.null(filtros)) input$mes else tail(as.Date(filtros()$fecha), 1L)
      shiny::req(mes, input$kg_saco)
      h <- fuentes()
      contactos <- .op_contactos_visibles(h$contactos, usuario(), es_admin())
      ejecucion <- hechos()
      if (!is.null(filtros)) {
        f <- filtros()
        contactos <- contactos[contactos$Asesor %in% f$asesor, , drop = FALSE]
        fechas <- as.Date(f$fecha)
        shiny::req(length(fechas) == 2L, !anyNA(fechas))
        ejecucion <- ejecucion[!is.na(ejecucion$FecFact) & ejecucion$FecFact >= fechas[[1]] &
          ejecucion$FecFact <= fechas[[2]], , drop = FALSE]
      }
      .pot_comparar(h$potencial, contactos, ejecucion, as.Date(mes), input$kg_saco)
    })
    .gc_tarjetas_server(c(entidades = "Entidades comerciales", comparable = "Con potencial comparable",
      facturados = "Kg facturados en el mes", brecha = "Brecha positiva de potencial · kg"), shiny::reactive({
      x <- datos()
      list(entidades = nrow(x), comparable = sum(!is.na(x$PotencialKgMes)),
        facturados = sum(x$KgFacturados), brecha = sum(pmax(0, x$BrechaKg), na.rm = TRUE))
    }))
    .crm_vancouver_server("tabla", datos, col_labels = c(PerRazSoc = "Entidad comercial",
      PotencialKgMes = "Potencial kg/mes", Penetracion = "Penetración del potencial", BrechaKg = "Brecha kg"),
      col_specs = list(Penetracion = list(format = list(percent = TRUE, digits = 1))))
    output$grafico <- .crm_render_plotly({
      x <- datos(); x <- x[is.finite(x$PotencialKgMes), ]
      shiny::validate(shiny::need(nrow(x) > 0, "No hay potencial comparable para las entidades visibles."))
      plotly::plot_ly(x, x = ~PotencialKgMes, y = ~KgFacturados, type = "scatter", mode = "markers",
        text = ~htmltools::htmlEscape(PerRazSoc),
        hovertemplate = "%{text}<br>Potencial: %{x:,.0f} kg/mes<br>Facturado: %{y:,.0f} kg<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Potencial mensual declarado · kg"),
          yaxis = list(title = "Kg facturados en el mes"))
    })
    .gc_descarga(output, datos, "Potencial_mercado")
  })
}
