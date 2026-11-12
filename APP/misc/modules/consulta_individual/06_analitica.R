# Unidades observadas y estadística de ejecución ----
.in_clave_observada <- function(x, campos) {
  if (!length(campos) || !all(campos %in% names(x))) return(rep(NA_character_, nrow(x)))
  valores <- lapply(x[campos], .in_id)
  completa <- Reduce(`&`, lapply(valores, function(v) !is.na(v)))
  clave <- do.call(paste, c(valores, sep = " | "))
  clave[!completa] <- NA_character_
  clave
}
.in_unidades <- function(x) {
  nit <- .in_id(.gc_columna(x, "CLCliNit"))
  x$Pedido <- .in_clave_observada(x, c("CLCliNit", "CLPdcCod"))
  x$LineaPedido <- .in_clave_observada(x, c("CLCliNit", "CLPdcCod", "CLPdcLin"))
  x$Lote <- .in_clave_observada(x, c("CLCliNit", "LinNegCod", "CLLotCod"))
  campo_factura <- intersect(c("Factura", "NumeroFactura", "NroFactura", "FctCod"), names(x))
  prefijo <- head(intersect(c("PrefijoFactura", "FcnPrf", "Prefijo"), names(x)), 1L)
  x$Factura <- if (length(campo_factura)) .in_clave_observada(x, c("CLCliNit", prefijo, campo_factura[[1]])) else
    rep(NA_character_, nrow(x))
  x$ProductoDiferente <- .in_clave_observada(x, c("LinNegCod", "Categoria", "Producto"))
  sin_producto <- toupper(as.character(.gc_columna(x, "Producto"))) %in% c("", "SIN DATO", "SIN DATOS")
  sin_producto <- sin_producto | is.na(x$ProductoDiferente)
  if ("LinProCod" %in% names(x)) {
    referencia <- .in_clave_observada(x, c("LinNegCod", "LinProCod"))
    x$ProductoDiferente[sin_producto] <- referencia[sin_producto]
  } else x$ProductoDiferente[sin_producto] <- NA_character_
  x
}
.in_kpi_ejecucion <- function(x) {
  u <- .in_unidades(x)
  contar <- function(col) if (!nrow(u) || all(is.na(u[[col]]))) NA_real_ else dplyr::n_distinct(u[[col]], na.rm = TRUE)
  precios <- .in_precios_pedidos(x) %>% dplyr::group_by(NIT, Pedido, LineaPedidoPrecio) %>%
    dplyr::summarise(PrecioKg = .gc_unico(PrecioKg), .groups = "drop")
  precios <- precios$PrecioKg
  precios <- precios[is.finite(precios) & precios > 0]
  list(pedidos = contar("Pedido"), lineas = contar("LineaPedido"), lotes = contar("Lote"),
    facturas = contar("Factura"), productos = contar("ProductoDiferente"),
    sacos = if (nrow(x)) .com_suma(x$SacosFact) else NA_real_,
    margen = if (nrow(x)) .com_suma(x$Margen) else NA_real_,
    precio = if (length(precios)) mean(precios) else NA_real_)
}
.in_transacciones_precio <- function(x, precio = NULL) {
  if (!is.null(precio) && (length(precio) != 1L || !is.finite(precio) || precio <= 0)) return(data.frame())
  x$PrecioKg <- .gc_num(.gc_columna(x, if ("PrecioNegociadoOriginal" %in% names(x)) "PrecioNegociadoKg" else c("PdcPrecioKilo", "PrecioKiloPedido")))
  x <- x[is.finite(x$PrecioKg) & x$PrecioKg > 0 & is.finite(x$SacosFact) & x$SacosFact > 0, , drop = FALSE]
  if (!nrow(x)) return(x)
  x$Pedido <- .in_id(.gc_columna(x, "CLPdcCod")); x$Lote <- .in_id(.gc_columna(x, "CLLotCod"))
  x$Cliente <- as.character(.gc_columna(x, c("RazonSocialCliNit", "PerRazSoc")))
  x <- x %>% dplyr::group_by(CLCliNit, Cliente, Pedido, Lote, LinNegCod, FecFact, Producto, PrecioKg, EstadoFinanciero) %>%
    dplyr::summarise(Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen), .groups = "drop")
  x$CalidadMargen <- ifelse(x$EstadoFinanciero == "CERRADO", "OBSERVADO · MES CERRADO", "PROVISIONAL · MES ABIERTO")
  if (is.null(precio)) return(x)
  .in_buscar_precio(x, precio)
}
.in_buscar_precio <- function(x, precio) {
  if (!nrow(x) || length(precio) != 1L || !is.finite(precio) || precio <= 0) return(x[0, , drop = FALSE])
  x$DiferenciaCOP <- abs(x$PrecioKg - precio)
  x$DiferenciaPct <- x$DiferenciaCOP / precio
  # Desempate estable: distancia, factura más reciente, NIT, pedido, lote y producto.
  x %>% dplyr::arrange(DiferenciaCOP, dplyr::desc(FecFact), CLCliNit, Pedido, Lote, Producto, EstadoFinanciero) %>% head(10L)
}

# RFM: percentiles favorables y rangos realmente observados por puntaje ----
.in_rfm_comparar <- function(poblacion, ids) {
  propios <- poblacion[.in_id(poblacion$CliNitPpal) %in% ids, , drop = FALSE]
  dplyr::bind_rows(lapply(seq_len(nrow(propios)), function(i) {
    fila <- propios[i, , drop = FALSE]
    dplyr::bind_rows(lapply(list(c("RECENCIA · DÍAS", "RecenciaDias", "R"),
      c("FRECUENCIA · DÍAS CON COMPRA", "Compras", "F"), c("VALOR · MARGEN COP", "Margen", "M")), function(par) {
        v <- poblacion[[par[[2]]]]; valor <- fila[[par[[2]]]]; puntaje <- fila[[par[[3]]]]
        rango <- v[poblacion[[par[[3]]]] %in% puntaje & is.finite(v)]
        bueno <- if (par[[3]] == "R") -v else v
        referencia <- if (par[[3]] == "R") -valor else valor
        percentil <- if (is.finite(referencia) && any(is.finite(bueno))) {
          100 * (sum(bueno < referencia, na.rm = TRUE) + 0.5 * sum(bueno == referencia, na.rm = TRUE)) /
            sum(is.finite(bueno))
        } else NA_real_
        data.frame(Linea = .in_linea(fila$LinNegCod), Dimension = par[[1]], Valor = valor, Puntaje = puntaje,
          LimiteObservadoInferior = if (length(rango)) min(rango) else NA_real_,
          LimiteObservadoSuperior = if (length(rango)) max(rango) else NA_real_, PercentilFavorable = percentil,
          Perfil = fila$SegmentoRFM)
      }))
  }))
}
.in_btyd_predictivo <- function(modelo, ids, corte, simulaciones = 4000L) {
  if (!is.null(modelo$error)) return(list(error = modelo$error))
  x <- modelo$clientes[.in_id(modelo$clientes$CliNitPpal) %in% ids, , drop = FALSE]
  if (!nrow(x)) return(list(error = "LA CUENTA NO TIENE HISTORIA SUFICIENTE PARA BG/NBD."))
  p <- as.numeric(unlist(modelo$parametros[1, c("r", "alpha", "a", "b")], use.names = FALSE))
  if (any(!is.finite(p) | p <= 0)) return(list(error = "PARÁMETROS BG/NBD NO VÁLIDOS."))
  semilla <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(if (is.null(semilla)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", semilla, envir = .GlobalEnv), add = TRUE)
  horizonte <- as.numeric(lubridate::add_with_rollback(as.Date(corte), lubridate::years(1)) - as.Date(corte)) / 7
  resultados <- lapply(seq_len(nrow(x)), function(i) {
    fila <- x[i, , drop = FALSE]
    codigo <- paste(fila$CliNitPpal, fila$LinNegCod, corte)
    set.seed(as.integer(sum(utf8ToInt(codigo) * seq_along(utf8ToInt(codigo))) %% 2147483646) + 1L)
    if (any(!is.finite(c(fila$ProbActivo, fila$Frecuencia, fila$T))) || fila$ProbActivo < 0 || fila$ProbActivo > 1) return(NULL)
    activo <- stats::rbinom(simulaciones, 1, fila$ProbActivo)
    tasa <- stats::rgamma(simulaciones, shape = p[[1]] + fila$Frecuencia, rate = p[[2]] + fila$T)
    abandono <- stats::rbeta(simulaciones, p[[3]], p[[4]] + fila$Frecuencia)
    compras <- activo * pmin(stats::rpois(simulaciones, tasa * horizonte), stats::rgeom(simulaciones, abandono) + 1L)
    limite <- as.numeric(stats::quantile(compras, c(0.025, 0.975), names = FALSE))
    if (anyNA(compras)) return(NULL)
    distribucion <- data.frame(compras = 0:max(compras), Freq = tabulate(compras + 1L, nbins = max(compras) + 1L))
    data.frame(Linea = .in_linea(fila$LinNegCod), Compras = as.numeric(distribucion$compras),
      Probabilidad = distribucion$Freq / simulaciones, Inferior95 = limite[[1]], Superior95 = limite[[2]],
      Esperadas = fila$ComprasEsperadas, Observadas = fila$Compras,
      MediaSimulada = mean(compras), MedianaSimulada = stats::median(compras), ProbActivo = fila$ProbActivo)
  })
  resultado <- dplyr::bind_rows(resultados)
  if (!nrow(resultado)) return(list(error = "La cuenta no tiene parámetros individuales válidos para simular compras."))
  list(datos = resultado, parametros = modelo$parametros)
}

# Cartera: saldos por corte y pagos acreditados, sin inferir recaudo por diferencia de saldos ----
.in_cartera_historia <- function(fuente, clientes, mapa, entidad, corte) {
  if (!is.list(fuente) || !is.data.frame(fuente$cartera)) stop("NO HAY FUENTE DE CARTERA DISPONIBLE.")
  elegir <- function(x) {
    if (entidad$Alcance == "Cuenta principal") x[x$Principal %in% entidad$Principal, , drop = FALSE] else
      x[x$NIT %in% entidad$NitFacturacion, , drop = FALSE]
  }
  x <- elegir(.car_scope(fuente$cartera, clientes, mapa))
  cortes <- sort(unique(.car_fecha(x$TCEFch))); cortes <- cortes[!is.na(cortes) & cortes <= corte]
  if (!length(cortes)) stop("NO HAY CORTES DE CARTERA PARA ESTA ENTIDAD.")
  evolucion <- dplyr::bind_rows(lapply(cortes, function(fecha) {
    d <- .car_preparar(x, as.Date(fecha, origin = "1970-01-01"))
    validos <- d[is.finite(d$Saldo) & d$Saldo > 0, , drop = FALSE]
    por_documento <- validos %>% dplyr::filter(!is.na(Documento)) %>% dplyr::group_by(Documento) %>%
      dplyr::summarise(Mora = if (all(is.na(DiasVencido))) NA_real_ else max(DiasVencido, na.rm = TRUE),
        .groups = "drop")
    mora <- por_documento$Mora[is.finite(por_documento$Mora)]
    data.frame(Corte = as.Date(fecha, origin = "1970-01-01"), Saldo = .com_suma(d$Saldo),
      Vencido = if (anyNA(validos$DiasVencido)) NA_real_ else sum(validos$Saldo[validos$DiasVencido > 0]),
      DiasMoraPromedio = if (length(mora)) mean(pmax(0, mora)) else NA_real_,
      DiasMoraMaximo = if (length(mora)) max(pmax(0, mora)) else NA_real_,
      Documentos = .car_distintos(d$Documento))
  }))
  pagos <- data.frame()
  f <- fuente$facturacion
  if (is.data.frame(f) && all(c("PerCod", "Factura", "FcnFec", "FecPag", "FcnVal", "FcnSal") %in% names(f))) {
    f <- elegir(.car_facturas_autorizadas(f, fuente$cartera, clientes, mapa))
    pagos <- .car_pagos(f, as.Date("1900-01-02"), corte, hoy = corte)
    pagos <- pagos[!pagos$FueraPeriodo, , drop = FALSE]
  }
  list(evolucion = evolucion, pagos = pagos,
    actual = .car_preparar(x, max(cortes)), cortes = length(cortes))
}

# Resúmenes distintos por unidad; el precio cuenta cada pedido una vez dentro del grupo ----
.in_detalle_indicador <- function(x, clave) {
  d <- switch(clave, pedidos = c("CLCliNit", "CLPdcCod"),
    lineas = c("CLCliNit", "CLPdcCod", "CLPdcLin"), lotes = c("CLCliNit", "LinNegCod", "CLLotCod"),
    facturas = c("CLCliNit", "Factura"), precio = c("CLCliNit", "CLPdcCod", "CLPdcLin"), productos = c("LinNegCod", "Categoria", "Producto"),
    c("LinNegCod", "Categoria", "Producto"))
  if (clave == "facturas") x <- .in_unidades(x)
  .crm_resumen_dimension(x, d)
}
