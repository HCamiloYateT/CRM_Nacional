# Gestión de clientes: señales observadas, no inferencia de altas comerciales ----
.fid_preparar <- function(hechos, clientes, corte, umbral = 90) {
  fact <- .com_hechos(hechos, corte, cerrados = FALSE)
  fact <- fact[!is.na(fact$SacosFact) & fact$SacosFact > 0, ]
  inicio <- as.Date(format(corte, "%Y-01-01"))
  historial <- fact %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::summarise(PrimeraObservada = .gc_fecha_min(FecFact), UltimaCompra = .gc_fecha_max(FecFact),
      SacosAnho = sum(SacosFact[FecFact >= inicio]),
      MargenAnho = .com_suma(Margen[FecFact >= inicio]), .groups = "drop")
  cli <- clientes %>% dplyr::select(CliNitPpal, LinNegCod, PerRazSoc, Asesor, Segmento, SSPpto, MNFCCPpto)
  x <- dplyr::full_join(cli, historial, by = c("CliNitPpal", "LinNegCod"))
  anteriores <- historial %>% dplyr::filter(PrimeraObservada < inicio) %>%
    dplyr::group_by(CliNitPpal) %>% dplyr::summarise(TeniaOtraLinea = TRUE, .groups = "drop")
  x <- dplyr::left_join(x, anteriores, by = "CliNitPpal")
  x$ClasificacionPresupuesto <- ifelse(is.na(x$MNFCCPpto), "No disponible",
    ifelse(x$MNFCCPpto > 0, "Con presupuesto", "Sin presupuesto"))
  x$DiasSinCompra <- as.integer(corte - x$UltimaCompra)
  x$Estado <- dplyr::case_when(is.na(x$UltimaCompra) ~ "Sin compra observada",
    x$DiasSinCompra > umbral ~ "Revisar inactividad",
    x$PrimeraObservada >= inicio & x$TeniaOtraLinea %in% TRUE ~ "Primera compra observada en nueva línea",
    x$PrimeraObservada >= inicio ~ "Primera compra observada del año",
    is.na(x$MNFCCPpto) & x$SacosAnho > 0 ~ "Presupuesto no disponible",
    x$MNFCCPpto == 0 & x$SacosAnho > 0 ~ "Compra sin presupuesto", TRUE ~ "Compra recurrente")
  x
}
FidelidadRiesgoUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputDate("corte", "Fecha de análisis", value = Sys.Date(), ns = ns),
      racafeForms::racafeInputNumeric("umbral", "Umbral de revisión (días)", value = 90, min = 1,
        format = "number", ns = ns),
      racafeForms::racafeInputSelect("estado", "Estado", choices = c("Todos", "Revisar inactividad",
        "Sin compra observada", "Primera compra observada del año", "Primera compra observada en nueva línea",
        "Compra sin presupuesto", "Presupuesto no disponible", "Compra recurrente"), ns = ns)),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("resumen")), .crm_tabla_ui(ns("clientes")),
    .crm_nota(c("La primera compra es la primera observada en la exportación: no equivale a la creación del cliente.",
      "Un cliente sin presupuesto no se clasifica automáticamente como recuperado.",
      "La inactividad es una señal por días sin compra; el umbral es parametrizable y no cambia el estado del CRM.",
      "Los datos cerrados y abiertos alimentan la señal comercial; el margen abierto es provisional.")))
}
FidelidadRiesgo <- function(id, hechos, clientes, filtros) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive({
      shiny::req(input$corte, is.finite(input$umbral), input$umbral > 0)
      x <- .fid_preparar(hechos(), .com_clientes(clientes(), as.Date(input$corte), filtros()),
        as.Date(input$corte), input$umbral)
      if (!is.null(input$estado) && input$estado != "Todos") x <- x[x$Estado == input$estado, ]
      x
    })
    resumen <- shiny::reactive(datos() %>% dplyr::group_by(Estado) %>%
      dplyr::summarise(ClientesLinea = dplyr::n(), SacosAnho = .com_suma(SacosAnho), .groups = "drop"))
    .crm_vancouver_server("resumen", data = resumen)
    .crm_vancouver_server("clientes", data = datos,
      col_labels = c(CliNitPpal = "NIT principal", PerRazSoc = "Cliente", LinNegCod = "Línea de negocio",
      SSPpto = "Presupuesto anual de sacos", MNFCCPpto = "Presupuesto anual de margen",
      PrimeraObservada = "Primera compra observada", UltimaCompra = "Última compra",
      DiasSinCompra = "Días sin compra", SacosAnho = "Sacos del año", MargenAnho = "Margen del año",
      ClasificacionPresupuesto = "Presupuesto", TeniaOtraLinea = "Con historia previa"))
    .gc_descarga(output, datos, "Fidelidad_riesgo")
  })
}
