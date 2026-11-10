# RFM y BG/NBD: compras observadas por cliente/línea/día, sin confundir líneas de factura con recurrencia ----
.fid_rfm_vacio <- function() {
  data.frame(CliNitPpal = character(), LinNegCod = numeric(), Cliente = character(),
    Primera = as.Date(character()), Ultima = as.Date(character()), Compras = integer(),
    Sacos = numeric(), Margen = numeric(), MargenPorCompra = numeric(), RecenciaDias = integer(),
    Frecuencia = integer(), tx = numeric(), T = numeric(), R = integer(), F = integer(), M = integer(),
    SegmentoRFM = character())
}
.fid_rfm <- function(hechos, corte) {
  x <- .com_hechos(hechos, corte, cerrados = FALSE)
  if (!nrow(x)) return(.fid_rfm_vacio())
  monetario <- x %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::summarise(MargenNeto = .com_suma(Margen), SacosNetos = sum(SacosFact, na.rm = TRUE), .groups = "drop")
  x$Cliente <- as.character(.gc_columna(x, "PerRazSoc", "Sin nombre"))
  x <- x %>% dplyr::filter(!is.na(CliNitPpal), !is.na(LinNegCod)) %>%
    dplyr::group_by(CliNitPpal, LinNegCod, FecFact) %>%
    dplyr::summarise(Cliente = dplyr::first(Cliente), Sacos = sum(SacosFact, na.rm = TRUE),
      Margen = .com_suma(Margen), .groups = "drop") %>%
    dplyr::filter(Sacos > 0)
  if (!nrow(x)) return(.fid_rfm_vacio())
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


.fid_bg_ajustar <- function(x) racafeForecast::AjustarBGNBD(x)
.fid_bg_predecir <- function(x, modelo, semanas) racafeForecast::PredecirBGNBD(x, modelo, semanas)
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
