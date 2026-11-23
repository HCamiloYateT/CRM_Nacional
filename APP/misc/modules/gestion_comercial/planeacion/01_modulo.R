# Planeación: saldo por facturar, una sola vez por línea del pedido ----
.plan_calcular <- function(maestro, corte, anteriores = TRUE, futuros = TRUE) {
  corte <- as.Date(corte)
  if (length(corte) != 1L || is.na(corte)) stop("Selecciona una fecha de corte válida.")
  mes <- as.Date(format(corte, "%Y-%m-01"))
  meses <- seq(mes, by = "month", length.out = 8L)
  titulos <- paste(c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")[
    as.integer(format(meses[1:7], "%m"))], format(meses[1:7], "%Y"))
  etiquetas <- c(Anteriores = "MESES ANTERIORES", stats::setNames(titulos, paste0("M", 0:6)),
    Futuros = "MESES FUTUROS", SinFecha = "SIN FECHA ESTIMADA")
  base <- maestro %>% dplyr::mutate(NIT = .crm_id_texto(CliNitPpal),
    Pedido = .crm_id_texto(CLPdcCod), Linea = .crm_id_texto(CLPdcLin))
  unidades <- base %>% dplyr::filter(!is.na(NIT), !is.na(LinNegCod)) %>%
    dplyr::group_by(NIT, LinNegCod) %>%
    dplyr::summarise(Cliente = paste(sort(unique(stats::na.omit(PerRazSoc))), collapse = " | "),
      .groups = "drop")
  metadatos <- base %>% dplyr::group_by(Pedido, Linea) %>%
    dplyr::summarise(LinNegCod = .gc_unico(LinNegCod), .groups = "drop")
  pedidos <- .pen_preparar(maestro, corte, detalle = FALSE)$pedidos %>%
    dplyr::left_join(metadatos, by = c("Pedido", "Linea"))
  sin_cantidad <- sum(pedidos$Activo & !is.finite(pedidos$Pendientes))
  pedidos <- pedidos %>% dplyr::filter(Activo, is.finite(Pendientes), Pendientes > 0.1)
  sin_unidad <- sum(is.na(pedidos$NIT) | is.na(pedidos$LinNegCod))
  pedidos <- pedidos %>% dplyr::filter(!is.na(NIT), !is.na(LinNegCod)) %>%
    dplyr::mutate(Columna = dplyr::case_when(is.na(Fecha) ~ "SinFecha", Fecha < mes ~ "Anteriores",
      Fecha >= meses[8] ~ "Futuros",
      TRUE ~ paste0("M", match(as.Date(format(Fecha, "%Y-%m-01")), meses) - 1L)))
  pedidos <- pedidos[(isTRUE(anteriores) & pedidos$Columna %in% c("Anteriores", "SinFecha")) |
    (isTRUE(futuros) & !pedidos$Columna %in% c("Anteriores", "SinFecha")), , drop = FALSE]
  tabla <- unidades
  for (columna in names(etiquetas)) {
    saldo <- pedidos[pedidos$Columna == columna, ] %>% dplyr::group_by(NIT, LinNegCod) %>%
      dplyr::summarise(Valor = sum(Pendientes), .groups = "drop")
    indice <- match(paste(tabla$NIT, tabla$LinNegCod), paste(saldo$NIT, saldo$LinNegCod))
    tabla[[columna]] <- dplyr::coalesce(saldo$Valor[indice], 0)
  }
  tabla$LineaNegocio <- .crm_label_comercial(.in_linea(tabla$LinNegCod))
  tabla <- tabla[c("NIT", "Cliente", "LineaNegocio", names(etiquetas))]
  tabla <- tabla[order(tabla$Cliente, tabla$LineaNegocio), , drop = FALSE]
  if (!nrow(pedidos)) tabla <- tabla[0, , drop = FALSE]
  list(tabla = tabla, etiquetas = etiquetas, sin_cantidad = sin_cantidad, sin_unidad = sin_unidad,
    detalle = pedidos)
}

PlaneacionComercialUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputSwitch("anteriores", "Incluir pendientes anteriores o sin fecha", value = TRUE, ns = ns),
      racafeForms::racafeInputSwitch("futuros", "Incluir el mes vigente y los pedidos futuros", value = TRUE, ns = ns)),
    .crm_tabla_ui(ns("planeacion"), "Planeación de compras · sacos pendientes de facturar (70 kg)"),
    shiny::uiOutput(ns("nota")))
}
PlaneacionComercial <- function(id, data_t, filtros) {
  shiny::moduleServer(id, function(input, output, session) {
    resultado <- shiny::reactive({
      .plan_calcular(data_t(), as.Date(filtros()$fecha[[2]]), input$anteriores, input$futuros)
    })
    especificaciones <- shiny::reactive({
      columnas <- names(resultado()$etiquetas)
      c(list(NIT = list(minWidth = 120), Cliente = list(minWidth = 240), LineaNegocio = list(minWidth = 150)),
        stats::setNames(lapply(columnas, function(x) list(minWidth = 125, format = list(digits = 1))), columnas))
    })
    .crm_vancouver_server("planeacion", shiny::reactive(resultado()$tabla), col_specs = especificaciones,
      col_labels = shiny::reactive(c(NIT = "NIT PRINCIPAL", Cliente = "CLIENTE", LineaNegocio = "LÍNEA DE NEGOCIO",
        resultado()$etiquetas)), page_size = 20)
    output$nota <- .crm_render_ui({
      x <- resultado()
      .crm_nota(c(paste("El mes vigente corresponde al mes de corte del panel derecho.",
        "Los demás filtros comerciales se mantienen."),
        "Cada celda muestra lo solicitado menos lo ya facturado, según la fecha estimada del pedido.",
        "Un pedido con varias líneas o lotes no se suma de nuevo por aparecer en varios meses financieros.",
        "Meses futuros agrupa todo lo posterior a los seis meses siguientes al vigente. Sin fecha queda separado.",
        "Las filas en cero permiten identificar unidades sin compras programadas dentro de la selección.",
        paste("Se muestran pedidos abiertos. Son una planeación, no ventas confirmadas.",
          "La información se renueva desde Actualizar."),
        if (x$sin_cantidad) paste(x$sin_cantidad, "líneas no tienen un saldo calculable y requieren revisión."),
        if (x$sin_unidad) paste(x$sin_unidad, "líneas pendientes no tienen cliente o línea de negocio identificable.")))
    })
  })
}
