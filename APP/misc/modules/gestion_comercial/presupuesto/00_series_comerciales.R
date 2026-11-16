# Ejecución comercial integrada y trayectoria requerida independiente del pronóstico ----
.ppto_series_comerciales <- function(maestro, presupuesto, corte, medida = c("Sacos", "Margen"), acumulada = FALSE) {
  medida <- match.arg(medida)
  corte <- as.Date(corte)
  if (length(corte) != 1L || is.na(corte)) stop("El corte del presupuesto no es válido.")
  if (!all(c("Mes", "PptoSacos", "PptoMargen") %in% names(presupuesto))) stop("Falta el presupuesto mensual.")
  if (anyDuplicated(presupuesto$Mes)) stop("El presupuesto mensual contiene meses duplicados.")
  anho <- as.integer(format(corte, "%Y"))
  meses <- seq(as.Date(sprintf("%d-01-01", anho)), by = "month", length.out = 12L)
  p <- presupuesto[match(meses, as.Date(presupuesto$Mes)), , drop = FALSE]
  plan <- p[[if (medida == "Sacos") "PptoSacos" else "PptoMargen"]]
  if (any(!is.finite(plan)) || any(plan < 0)) stop("El ritmo requiere doce presupuestos mensuales válidos.")
  x <- .crm_capa_ejecucion(maestro)
  if ("IdRegistroCRM" %in% names(x) && anyDuplicated(x$IdRegistroCRM[!is.na(x$IdRegistroCRM)])) {
    stop("El maestro contiene hechos duplicados; no se construye una ejecución duplicada.")
  }
  x <- x[!is.na(x$FechaAnalitica) & format(x$FechaAnalitica, "%Y") == as.character(anho) &
    x$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO", "PROYECTADO"), , drop = FALSE]
  x$Mes <- as.Date(format(x$FechaAnalitica, "%Y-%m-01"))
  valor <- if (medida == "Sacos") x$SacosAnaliticos70 else x$MargenAnalitico
  sumar <- function(v) if (anyNA(v)) NA_real_ else sum(v)
  ejecucion <- vapply(meses, function(m) sumar(valor[x$Mes == m]), numeric(1))
  componentes <- vapply(meses, function(m) {
    filas <- which(x$Mes == m)
    if (!length(filas)) return("Sin operaciones observadas")
    paste(vapply(split(filas, x$EstadoEjecucion[filas]), function(i) {
      paste0(x$EstadoEjecucion[i[1]], ": ", if (is.na(sumar(valor[i]))) "No disponible" else {
        paste0(if (medida == "Margen") "$" else "", format(round(sumar(valor[i]), 2), big.mark = ","))
      })
    }, character(1)), collapse = "<br>")
  }, character(1))
  observado <- sumar(valor[x$FechaAnalitica <= corte & x$EstadoFinanciero != "PROYECTADO"])
  fin_mes <- c(meses[-1L], as.Date(sprintf("%d-01-01", anho + 1L))) - 1L
  dias_mes <- as.numeric(fin_mes - meses + 1L)
  fraccion <- pmin(1, pmax(0, as.numeric(fin_mes - corte) / dias_mes))
  pesos <- plan * fraccion
  futuro <- meses >= as.Date(format(corte, "%Y-%m-01"))
  ritmo <- rep(NA_real_, 12L)
  minimo_aplicado <- FALSE
  if (any(futuro) && is.finite(observado)) {
    pesos_validos <- if (sum(pesos) > 0) pesos else fraccion
    if (sum(pesos_validos) == 0) pesos_validos <- as.numeric(futuro)
    reparto <- (sum(plan) - observado) * pesos_validos / sum(pesos_validos)
    ritmo[futuro] <- pmax(reparto[futuro], pesos[futuro])
    minimo_aplicado <- any(reparto[futuro] < pesos[futuro])
  }
  requerido <- if (acumulada) observado + cumsum(ifelse(futuro, ritmo, 0)) else ritmo
  requerido[!futuro] <- NA_real_
  salida <- data.frame(Mes = fin_mes, Presupuesto = if (acumulada) cumsum(plan) else plan,
    Ejecucion = if (acumulada) cumsum(ejecucion) else ejecucion, Ritmo = requerido,
    Composicion = componentes, BaseCorte = observado)
  attr(salida, "saldo_requerido") <- sum(plan) - observado
  attr(salida, "minimo_aplicado") <- minimo_aplicado
  attr(salida, "corte") <- corte
  attr(salida, "acumulada") <- acumulada
  salida
}
.ppto_grafico_comercial <- function(x, medida) {
  dinero <- identical(medida, "Margen")
  formato <- if (dinero) "$%{y:,.0f}" else "%{y:,.2f}"
  color <- function(nombre) unname(.crm_colores("auxiliar", nombre))
  g <- plotly::plot_ly(height = 500)
  for (serie in c("Presupuesto", "Ejecución", "Ritmo requerido")) {
    campo <- c(Presupuesto = "Presupuesto", Ejecución = "Ejecucion", `Ritmo requerido` = "Ritmo")[[serie]]
    d <- x
    if (serie == "Ritmo requerido") d <- d[is.finite(d$Ritmo), , drop = FALSE]
    g <- plotly::add_trace(g, x = d$Mes, y = d[[campo]], name = serie, type = "scatter", mode = "lines+markers",
      connectgaps = FALSE, marker = list(color = color(serie), size = 6), line = list(color = color(serie),
        dash = c(Presupuesto = "dash", Ejecución = "solid", `Ritmo requerido` = "dot")[[serie]]),
      hovertemplate = paste0(serie, ": ", formato, "<extra></extra>"))
  }
  plotly::layout(g, hovermode = "x unified", xaxis = list(hoverformat = "%B %Y", tickmode = "array",
    tickvals = x$Mes, ticktext = c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC"),
    tickangle = -45),
    yaxis = list(title = if (dinero) "Margen · COP" else "Sacos de 70 kg"),
    meta = list(crm_leyenda = TRUE))
}

# Conciliación por hecho financiero: agregar antes del cruce evita productos cartesianos ----
.ppto_conciliar_registros <- function(pyg, exportacion, modulo, filtros = NULL) {
  claves <- c("FecMes", "LinNegCod", "CLCliNit", "CLPdcCod", "CLLotCod")
  campos <- c(Sacos = "SacosFact", Margen = "Margen")
  sumar <- function(v) if (anyNA(v)) NA_real_ else sum(v)
  normalizar <- function(d) {
    d$FecMes <- as.Date(d$FecMes)
    d$LinNegCod <- ifelse(as.numeric(d$LinNegCod) == 20000, 21000, as.numeric(d$LinNegCod))
    for (k in setdiff(claves, c("FecMes", "LinNegCod"))) {
      if (!k %in% names(d)) d[[k]] <- NA_character_
      d[[k]] <- .crm_id_texto(d[[k]])
    }
    d
  }
  resumir <- function(d, sufijo) {
    z <- normalizar(d) %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
      dplyr::summarise(Sacos = sumar(SacosFact), Margen = sumar(Margen), Registros = dplyr::n(), .groups = "drop")
    names(z)[names(z) %in% c("Sacos", "Margen", "Registros")] <-
      paste0(c("Sacos", "Margen", "Registros"), sufijo)
    z
  }
  base <- normalizar(exportacion)
  resultado <- dplyr::full_join(resumir(pyg, "PYG"), resumir(modulo, "Modulo"), by = claves,
    relationship = "one-to-one")
  causas <- base
  causas$CausaFiltro <- rep("", nrow(base))
  dimensiones <- c(linneg = "CLLinNegNo", asesor = "Asesor", segmento = "Segmento",
    categoria = "Categoria", producto = "Producto")
  for (nombre in names(dimensiones)) {
    campo <- dimensiones[[nombre]]; seleccion <- filtros[[nombre]]
    if (is.null(seleccion) || !campo %in% names(base)) next
    fuera <- which(!is.na(base[[campo]]) & !as.character(base[[campo]]) %in% seleccion)
    causas$CausaFiltro[fuera] <- paste(causas$CausaFiltro[fuera],
      paste0("Filtro ", campo, ": ", base[[campo]][fuera]), sep = " · ")
  }
  for (campo in intersect(c("Excluir", "ProdExcluir"), names(base))) {
    fuera <- which(base[[campo]] %in% "SI")
    causas$CausaFiltro[fuera] <- paste(causas$CausaFiltro[fuera], paste0(campo, " = SI"), sep = " · ")
  }
  if (length(filtros$fecha) == 2L) {
    intervalo <- as.Date(format(as.Date(filtros$fecha), "%Y-%m-01"))
    fuera <- which(base$FecMes < intervalo[1] | base$FecMes > intervalo[2])
    causas$CausaFiltro[fuera] <- paste(causas$CausaFiltro[fuera], "Fuera del periodo", sep = " · ")
  }
  causas$CausaFiltro <- sub("^ · ", "", causas$CausaFiltro)
  causas <- causas %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(CausaFiltro = paste(unique(CausaFiltro[nzchar(CausaFiltro)]), collapse = " · "),
      EnExportacion = TRUE, .groups = "drop")
  resultado <- dplyr::left_join(resultado, causas, by = claves, relationship = "one-to-one")
  resultado$Presencia <- ifelse(is.na(resultado$RegistrosPYG), "SOLO MÓDULO",
    ifelse(is.na(resultado$RegistrosModulo), "SOLO PYG", "AMBAS FUENTES"))
  for (m in names(campos)) {
    a <- resultado[[paste0(m, "PYG")]]; b <- resultado[[paste0(m, "Modulo")]]
    a[is.na(resultado$RegistrosPYG)] <- 0
    b[is.na(resultado$RegistrosModulo)] <- 0
    resultado[[paste0("Diferencia", m)]] <- b - a
  }
  resultado$Causa <- dplyr::case_when(
    resultado$Presencia == "SOLO PYG" & !is.na(resultado$CausaFiltro) & nzchar(resultado$CausaFiltro) ~
      resultado$CausaFiltro,
    resultado$Presencia == "SOLO PYG" & is.na(resultado$EnExportacion) ~
      "Ausente en exportación comercial; revisar homologación y claves de origen",
    resultado$Presencia == "SOLO PYG" ~ "Excluido en preparación del módulo; revisar dimensiones disponibles",
    resultado$Presencia == "SOLO MÓDULO" ~ "Sin coincidencia PyG en mes, línea, NIT, pedido y lote",
    is.na(resultado$DiferenciaSacos) | is.na(resultado$DiferenciaMargen) ~ "Importe desconocido en alguna fuente",
    resultado$DiferenciaSacos != 0 | resultado$DiferenciaMargen != 0 ~ "Importes distintos para la misma clave",
    TRUE ~ "Conciliado")
  resultado %>% dplyr::select(-CausaFiltro, -EnExportacion) %>% dplyr::arrange(FecMes, LinNegCod, CLPdcCod, CLLotCod)
}
