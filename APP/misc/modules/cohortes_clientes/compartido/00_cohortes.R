# Cohortes de primera compra observada por cliente y línea de negocio ----
.coh_panel <- function(hechos, inicio, corte) {
  inicio <- .gc_mes(inicio)
  corte <- .gc_mes(corte)
  base <- hechos %>% dplyr::filter(!is.na(CliNitPpal), !is.na(LinNegCod), !is.na(FecFact),
    EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"), FecFact <= Sys.Date(), .gc_mes(FecFact) <= corte) %>%
    dplyr::mutate(Mes = .gc_mes(FecFact))
  base$Cliente <- as.character(.gc_columna(base, "PerRazSoc"))
  base$Asesor <- as.character(.gc_columna(base, "Asesor"))
  metadata <- base %>% dplyr::arrange(dplyr::desc(FecFact)) %>%
    dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::summarise(Cliente = dplyr::first(Cliente), Asesor = dplyr::first(Asesor), .groups = "drop")
  compras <- base %>% dplyr::group_by(CliNitPpal, LinNegCod, Mes) %>%
    dplyr::summarise(Sacos = sum(SacosFact, na.rm = TRUE),
      Margen = if (any(EstadoFinanciero != "CERRADO") || anyNA(Margen)) NA_real_ else sum(Margen),
      .groups = "drop")
  cohortes <- compras %>% dplyr::filter(Sacos > 0) %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::summarise(Cohorte = .gc_fecha_min(Mes), .groups = "drop") %>% dplyr::filter(Cohorte >= inicio)
  if (!nrow(cohortes) || inicio > corte) return(tibble::tibble())
  meses <- seq(inicio, corte, by = "month")
  panel <- tidyr::crossing(cohortes, Mes = meses) %>% dplyr::filter(Mes >= Cohorte) %>%
    dplyr::left_join(compras, by = c("CliNitPpal", "LinNegCod", "Mes")) %>%
    dplyr::mutate(TieneMovimiento = !is.na(Sacos), Activo = !is.na(Sacos) & Sacos > 0,
      Sacos = dplyr::coalesce(Sacos, 0),
      Edad = (as.integer(format(Mes, "%Y")) - as.integer(format(Cohorte, "%Y"))) * 12L +
        as.integer(format(Mes, "%m")) - as.integer(format(Cohorte, "%m")))
  # Solo meses cerrados sin compra llevan margen cero; el abierto conserva NA.
  cierre <- .gc_fecha_max(as.Date(.gc_columna(hechos, "FechaCortePyG")))
  if (is.na(cierre)) cierre <- .gc_fecha_max(base$Mes[base$EstadoFinanciero == "CERRADO"])
  panel %>% dplyr::mutate(Margen = ifelse(!TieneMovimiento & !is.na(cierre) & Mes <= cierre, 0, Margen)) %>%
    dplyr::left_join(metadata, by = c("CliNitPpal", "LinNegCod")) %>%
    dplyr::group_by(CliNitPpal, LinNegCod) %>% dplyr::arrange(Mes, .by_group = TRUE) %>%
    dplyr::mutate(MesesConCompra = cumsum(Activo), MesesObservados = dplyr::row_number(),
      EstadoMes = dplyr::case_when(
      Activo & Edad == 0L ~ "Primera compra observada",
      Activo & dplyr::lag(Activo, default = FALSE) ~ "Compra recurrente",
      Activo ~ "Compra reactivada", TRUE ~ "Sin compra")) %>% dplyr::ungroup()
}
.coh_resumen <- function(panel, metrica) {
  if (!nrow(panel)) return(tibble::tibble())
  panel %>% dplyr::group_by(Cohorte, Mes, Edad) %>%
    dplyr::summarise(Base = dplyr::n(), Activos = sum(Activo), Retencion = sum(Activo) / dplyr::n(),
      Sacos = sum(Sacos), Margen = if (anyNA(Margen)) NA_real_ else sum(Margen),
      Frecuencia = sum(MesesConCompra) / sum(MesesObservados), .groups = "drop") %>%
    dplyr::mutate(Valor = switch(metrica, retencion = Retencion, ventas = Sacos, margen = Margen,
      frecuencia = Frecuencia))
}
.coh_matriz <- function(resumen) {
  if (!nrow(resumen)) return(data.frame(Cohorte = character(), Base = integer()))
  resumen %>% dplyr::mutate(Periodo = paste0("M", Edad), Cohorte = as.character(Cohorte)) %>%
    dplyr::select(Cohorte, Base, Periodo, Valor) %>%
    tidyr::pivot_wider(names_from = Periodo, values_from = Valor) %>% dplyr::arrange(Cohorte)
}
.COH_TITULOS <- c(retencion = "Retención de clientes", ventas = "Ventas por cohorte", margen = "Margen por cohorte",
                  frecuencia = "Frecuencia mensual de compra")
CohorteClientesUI <- function(id, metrica) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeInputDateRange("periodo", "Cohortes desde / seguimiento hasta",
      start = as.Date(format(Sys.Date(), "%Y-01-01")), end = Sys.Date(), ns = ns),
    .gc_tarjetas_ui(ns, c(base = "Clientes-línea de las cohortes", activos = "Con compra en último mes")),
    shiny::plotOutput(ns("evolucion"), height = "260px"),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("matriz"), titulo = .COH_TITULOS[[metrica]]),
    .crm_nota(c("Unidad de análisis: NIT principal y línea de negocio; M0 es el mes de primera compra observada.",
      "La primera compra se busca en toda la historia disponible, antes de recortar las fechas del informe.",
      "La retención mide clientes con compra positiva en cada mes / base original de la cohorte.",
      "Cero indica un mes observado sin compra; vacío indica un mes futuro o un margen aún no cerrado.",
      "Frecuencia: meses con compra / meses observados desde M0, sin inferir fechas de facturas individuales.",
      "El último mes puede estar en curso; la retención de compra mensual no es el estado comercial del CRM.",
      "Las cohortes anteriores al inicio de la exportación no pueden identificarse como altas reales.")),
    .crm_tabla_ui(ns("clientes"), titulo = "Seguimiento por cliente"))
}
CohorteClientes <- function(id, hechos, metrica) {
  shiny::moduleServer(id, function(input, output, session) {
    panel <- shiny::reactive({
      shiny::req(length(input$periodo) == 2L)
      .coh_panel(hechos(), input$periodo[[1]], min(as.Date(input$periodo[[2]]), Sys.Date()))
    })
    resumen <- shiny::reactive(.coh_resumen(panel(), metrica))
    matriz <- shiny::reactive(.coh_matriz(resumen()))
    .gc_tarjetas_server(c(base = "Clientes-línea de las cohortes", activos = "Con compra en último mes"),
      shiny::reactive({
        x <- panel()
        if (!nrow(x)) return(list(base = 0, activos = 0))
        x <- x[x$Mes == max(x$Mes), ]
        list(base = nrow(x), activos = sum(x$Activo))
      }))
    .crm_vancouver_server("matriz", data = matriz, page_size = 12,
      col_specs = stats::setNames(lapply(0:600, function(i) list(format = list(
        percent = metrica %in% c("retencion", "frecuencia"),
        digits = if (metrica %in% c("retencion", "frecuencia")) 1 else 2))), paste0("M", 0:600)))
    .crm_vancouver_server("clientes", data = panel, page_size = 15,
      col_labels = c(CliNitPpal = "NIT principal", LinNegCod = "Línea"))
    output$evolucion <- shiny::renderPlot({
      x <- resumen()
      shiny::validate(shiny::need(nrow(x) > 0, "No hay cohortes para este periodo y estos filtros."))
      ggplot2::ggplot(x, ggplot2::aes(x = Edad, y = Valor, group = Cohorte, colour = as.character(Cohorte))) +
        ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) + ggplot2::geom_point(size = 1.5, na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = grDevices::colorRampPalette(
          c("#DA291C", "#6C757D", "#212529"))(dplyr::n_distinct(x$Cohorte))) +
        ggplot2::labs(x = "Meses desde la primera compra observada", y = .COH_TITULOS[[metrica]], colour = "Cohorte") +
        ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom")
    })
    .gc_descarga(output, panel, paste0("Cohortes_", metrica))
    list(datos = panel)
  })
}
