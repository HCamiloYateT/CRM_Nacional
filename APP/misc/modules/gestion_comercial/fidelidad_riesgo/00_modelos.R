# RFM y BG/NBD: compras observadas por cliente/línea/día, sin confundir líneas de factura con recurrencia ----
.fid_rfm <- function(hechos, corte) {
  x <- .com_hechos(hechos, corte, cerrados = FALSE)
  monetario <- x %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::summarise(MargenNeto = .com_suma(Margen), SacosNetos = sum(SacosFact, na.rm = TRUE), .groups = "drop")
  x$Cliente <- as.character(.gc_columna(x, "PerRazSoc", "Sin nombre"))
  x <- x %>% dplyr::filter(!is.na(CliNitPpal), !is.na(LinNegCod)) %>%
    dplyr::group_by(CliNitPpal, LinNegCod, FecFact) %>%
    dplyr::summarise(Cliente = dplyr::first(Cliente), Sacos = sum(SacosFact, na.rm = TRUE),
      Margen = .com_suma(Margen), .groups = "drop") %>%
    dplyr::filter(Sacos > 0)
  x <- x %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::summarise(Cliente = dplyr::first(Cliente), Primera = min(FecFact), Ultima = max(FecFact),
      Compras = dplyr::n(),
      Sacos = sum(Sacos), Margen = .com_suma(Margen),
      MargenPorCompra = .com_suma(Margen) / dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(RecenciaDias = as.integer(corte - Ultima), Frecuencia = Compras - 1L,
      tx = as.numeric(Ultima - Primera) / 7, T = as.numeric(corte - Primera) / 7)
  x <- dplyr::left_join(x, monetario, by = c("CliNitPpal", "LinNegCod"))
  x$Margen <- x$MargenNeto; x$Sacos <- x$SacosNetos
  x$MargenPorCompra <- x$Margen / x$Compras
  x$MargenNeto <- x$SacosNetos <- NULL
  puntaje <- function(v, inverso = FALSE) {
    buenos <- is.finite(v)
    s <- rep(NA_integer_, length(v))
    if (any(buenos)) s[buenos] <- pmax(1L, pmin(5L,
      ceiling(rank(if (inverso) -v[buenos] else v[buenos], ties.method = "average") / sum(buenos) * 5)))
    s
  }
  x$R <- puntaje(x$RecenciaDias, TRUE); x$F <- puntaje(x$Compras); x$M <- puntaje(x$Margen)
  x$SegmentoRFM <- dplyr::case_when(x$R >= 4 & x$F >= 4 ~ "Leales recientes",
    x$R <= 2 & x$F >= 4 ~ "Leales en riesgo", x$Compras == 1 ~ "Compra única observada",
    x$R <= 2 ~ "Inactivos relativos", TRUE ~ "En desarrollo")
  x
}
.fid_logsum <- function(a, b) { m <- pmax(a, b); m + log(exp(a - m) + exp(b - m)) }
.fid_bg_ll <- function(parametros, x) {
  p <- exp(parametros); r <- p[1]; alpha <- p[2]; a <- p[3]; b <- p[4]
  f <- x$Frecuencia
  A1 <- lgamma(r + f) - lgamma(r) + r * log(alpha)
  A2 <- lbeta(a, b + f) - lbeta(a, b)
  vivo <- -(r + f) * log(alpha + x$T)
  muerto <- rep(-Inf, length(f)); i <- f > 0
  muerto[i] <- log(a) - log(b + f[i] - 1) - (r + f[i]) * log(alpha + x$tx[i])
  -sum(A1 + A2 + .fid_logsum(vivo, muerto))
}
.fid_bg_ajustar <- function(x) {
  if (nrow(x) < 20L || sum(x$Frecuencia > 0) < 5L) {
    stop("BG/NBD requiere al menos 20 clientes/línea y cinco con recompra para ajustar la población.")
  }
  inicios <- list(c(1, 10, 1, 3), c(0.5, 30, 2, 5), c(2, 50, 0.5, 2))
  ajustes <- lapply(inicios, function(p) tryCatch(stats::optim(log(p), .fid_bg_ll, x = x,
    method = "L-BFGS-B", lower = rep(log(1e-4), 4), upper = rep(log(1e5), 4)), error = function(e) NULL))
  ajustes <- Filter(function(a) !is.null(a) && a$convergence == 0L && is.finite(a$value), ajustes)
  if (!length(ajustes)) stop("BG/NBD no convergió. Amplíe el historial o revise los datos de compras.")
  mejor <- ajustes[[which.min(vapply(ajustes, function(a) a$value, numeric(1)))]]
  list(parametros = stats::setNames(exp(mejor$par), c("r", "alpha", "a", "b")),
    LogVerosimilitud = -mejor$value, Clientes = nrow(x), Convergencia = mejor$convergence,
    EnLimite = any(abs(mejor$par - log(1e-4)) < 0.01 | abs(mejor$par - log(1e5)) < 0.01))
}
.fid_bg_predecir <- function(x, modelo, semanas) {
  p <- modelo$parametros; r <- p[["r"]]; alpha <- p[["alpha"]]; a <- p[["a"]]; b <- p[["b"]]
  log_odds <- rep(-Inf, nrow(x)); i <- x$Frecuencia > 0
  log_odds[i] <- log(a) - log(b + x$Frecuencia[i] - 1) +
    (r + x$Frecuencia[i]) * log((alpha + x$T[i]) / (alpha + x$tx[i]))
  x$ProbActivo <- stats::plogis(-log_odds)
  # Expectativa condicional integrada sobre la propensión de abandono; semanas es la unidad del ajuste.
  x$ComprasEsperadas <- vapply(seq_len(nrow(x)), function(i) {
    shape <- r + x$Frecuencia[i]; rate <- alpha + x$T[i]
    integral <- stats::integrate(function(q) {
      abandono <- stats::qbeta(q, a, b + x$Frecuencia[i])
      valor <- -expm1(-shape * log1p(semanas * abandono / rate)) / abandono
      valor[abandono == 0] <- shape * semanas / rate
      valor
    }, 0, 1, rel.tol = 1e-5, subdivisions = 200L)$value
    x$ProbActivo[i] * integral
  }, numeric(1))
  x
}
.fid_analizar_modelos <- function(hechos, corte, meses = 12L, descuento = 0.12) {
  rfm <- .fid_rfm(hechos, corte)
  modelo <- .fid_bg_ajustar(rfm)
  # El horizonte suma meses calendario; no presupone que todos tengan treinta días.
  limites <- as.Date(lubridate::add_with_rollback(corte, lubridate::period(seq_len(meses), units = "month")))
  anteriores <- rep(0, nrow(rfm)); clv <- rep(0, nrow(rfm))
  pred <- NULL
  for (j in seq_along(limites)) {
    pred <- .fid_bg_predecir(rfm, modelo, as.numeric(limites[j] - corte) / 7)
    clv <- clv + (pred$ComprasEsperadas - anteriores) * rfm$MargenPorCompra / (1 + descuento)^(j / 12)
    anteriores <- pred$ComprasEsperadas
  }
  pred$CLVMargen <- clv
  pred$RiesgoModelo <- ifelse(pred$ProbActivo < 0.4, "Alto", ifelse(pred$ProbActivo < 0.7, "Medio", "Bajo"))
  pred$HorizonteMeses <- meses
  list(clientes = pred, parametros = as.data.frame(as.list(c(modelo$parametros,
    LogVerosimilitud = modelo$LogVerosimilitud, Clientes = modelo$Clientes,
    EnLimite = modelo$EnLimite))), corte = corte)
}

# Validación retrospectiva: predecir noventa días sin usar sus compras para ajustar ----
.fid_validar_btyd <- function(hechos, corte, dias = 90L) {
  tryCatch({
    entrenamiento <- corte - dias
    base <- .fid_rfm(hechos, entrenamiento)
    ajuste <- .fid_bg_ajustar(base)
    esperado <- .fid_bg_predecir(base, ajuste, dias / 7)
    compras <- .com_hechos(hechos, corte, cerrados = FALSE) %>%
      dplyr::filter(FecFact > entrenamiento) %>% dplyr::group_by(CliNitPpal, LinNegCod, FecFact) %>%
      dplyr::summarise(Sacos = sum(SacosFact, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(Sacos > 0) %>% dplyr::count(CliNitPpal, LinNegCod, name = "Observadas")
    z <- dplyr::left_join(esperado, compras, by = c("CliNitPpal", "LinNegCod"))
    z$Observadas <- dplyr::coalesce(z$Observadas, 0L)
    data.frame(InicioValidacion = entrenamiento + 1L, FinValidacion = corte, Clientes = nrow(z),
      ComprasEsperadas = sum(z$ComprasEsperadas), ComprasObservadas = sum(z$Observadas),
      MAE = mean(abs(z$ComprasEsperadas - z$Observadas)),
      RMSE = sqrt(mean((z$ComprasEsperadas - z$Observadas)^2)), Estado = "Validación retrospectiva de 90 días")
  }, error = function(e) data.frame(Estado = paste("Validación no disponible:", conditionMessage(e))))
}
