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
  observado <- sumar(valor[x$FechaAnalitica <= corte])
  fin_mes <- c(meses[-1L], as.Date(sprintf("%d-01-01", anho + 1L))) - 1L
  dias_mes <- as.numeric(fin_mes - meses + 1L)
  fraccion <- pmin(1, pmax(0, as.numeric(fin_mes - corte) / dias_mes))
  pesos <- plan * fraccion
  futuro <- fraccion > 0
  ritmo <- rep(NA_real_, 12L)
  minimo_aplicado <- FALSE
  if (any(futuro) && is.finite(observado)) {
    pesos_validos <- if (sum(pesos) > 0) pesos else fraccion
    reparto <- (sum(plan) - observado) * pesos_validos / sum(pesos_validos)
    ritmo[futuro] <- pmax(reparto[futuro], pesos[futuro])
    minimo_aplicado <- any(reparto[futuro] < pesos[futuro])
  }
  requerido <- if (acumulada) observado + cumsum(ifelse(futuro, ritmo, 0)) else ritmo
  requerido[!futuro] <- NA_real_
  salida <- data.frame(Mes = fin_mes, Presupuesto = if (acumulada) cumsum(plan) else plan,
    Ejecucion = if (acumulada) cumsum(ejecucion) else ejecucion, Ritmo = requerido,
    Composicion = componentes, BaseCorte = observado)
  if (acumulada) salida$Composicion <- vapply(seq_len(12L), function(i) {
    paste(componentes[seq_len(i)], collapse = "<br>")
  }, character(1))
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
    if (serie == "Ritmo requerido" && isTRUE(attr(x, "acumulada"))) {
      d <- d[d$Mes > attr(x, "corte") & is.finite(d$Ritmo), , drop = FALSE]
      if (nrow(d)) d <- rbind(transform(d[1, ], Mes = attr(x, "corte"), Ritmo = x$BaseCorte[1]), d)
    }
    g <- plotly::add_trace(g, x = d$Mes, y = d[[campo]], name = serie, type = "scatter", mode = "lines+markers",
      connectgaps = FALSE, line = list(color = color(serie),
        dash = c(Presupuesto = "dash", Ejecución = "solid", `Ritmo requerido` = "dot")[[serie]]),
      customdata = lapply(d$Composicion, function(v) list(v)),
      hovertemplate = paste0("<b>", serie, "</b><br>", formato,
        if (serie == "Ejecución") "<br>%{customdata[0]}" else "", "<extra></extra>"))
  }
  plotly::layout(g, hovermode = "x unified", yaxis = list(title = if (dinero) "Margen · COP" else "Sacos de 70 kg"),
    meta = list(crm_leyenda = TRUE))
}
