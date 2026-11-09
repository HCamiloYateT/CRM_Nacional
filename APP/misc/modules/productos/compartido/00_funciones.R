# Hechos de producto: movimientos facturados, sin proyecciones ni multiplicación por lotes ----
.prod_suma <- function(x) if (anyNA(x)) NA_real_ else sum(x)
.prod_div <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, NA_real_)
.prod_identificador <- function(x) {
  valor <- if (is.numeric(x)) {
    format(x, scientific = FALSE, trim = TRUE, digits = 22, big.mark = "")
  } else as.character(x)
  valor[is.na(x)] <- NA_character_
  valor
}
.prod_preparar <- function(datos, fechas, cerrados = FALSE) {
  requeridas <- c("EstadoFinanciero", "FecFact", "SacosFact", "Margen", "Categoria", "Producto", "CliNitPpal")
  if (!all(requeridas %in% names(datos))) stop("Faltan campos del maestro de productos; actualice DataPrep.")
  if (length(fechas) != 2L || anyNA(fechas) || fechas[[1]] > fechas[[2]]) stop("Seleccione un periodo válido.")
  x <- datos %>% dplyr::filter(EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"),
    !is.na(FecFact), FecFact >= fechas[[1]], FecFact <= fechas[[2]])
  if (cerrados) x <- x[x$EstadoFinanciero == "CERRADO", ]
  x$Categoria <- trimws(as.character(x$Categoria))
  x$Producto <- trimws(as.character(x$Producto))
  x$ProductoValido <- !is.na(x$Categoria) & nzchar(x$Categoria) & !is.na(x$Producto) & nzchar(x$Producto) &
    !toupper(x$Producto) %in% c("SIN DATO", "SIN PRODUCTO", "SIN CLASIFICAR") &
    !toupper(x$Categoria) %in% c("SIN DATO", "SIN CATEGORÍA", "SIN CLASIFICAR")
  x$Categoria[is.na(x$Categoria) | !nzchar(x$Categoria)] <- "Sin categoría"
  x$Producto[is.na(x$Producto) | !nzchar(x$Producto)] <- "Sin producto"
  x$Linea <- as.character(.gc_columna(x, c("CLLinNegNo", "LinNegCod")))
  x$Linea[is.na(x$Linea)] <- "Sin línea"
  x$NIT <- .prod_identificador(x$CliNitPpal)
  x$NITFactura <- .prod_identificador(.gc_columna(x, c("CLCliNit", "CliNitPpal")))
  x$Pedido <- .prod_identificador(.gc_columna(x, "CLPdcCod"))
  x$Pedido[is.na(x$Pedido) | !nzchar(x$Pedido) | x$Pedido == "0"] <- NA_character_
  x$NIT[x$NIT %in% c("", "0")] <- NA_character_
  x$NITFactura[x$NITFactura %in% c("", "0")] <- NA_character_
  x$Cliente <- as.character(.gc_columna(x, "PerRazSoc"))
  x$Lote <- .prod_identificador(.gc_columna(x, "CLLotCod"))
  x$Lote[x$Lote %in% c("", "0")] <- NA_character_
  x$Mes <- .gc_mes(x$FecFact)
  mapa <- x %>% dplyr::distinct(Linea, Categoria, Producto) %>% dplyr::arrange(Linea, Categoria, Producto) %>%
    dplyr::mutate(Item = seq_len(dplyr::n()), Etiqueta = paste(Linea, Categoria, Producto, sep = " › "))
  x <- dplyr::left_join(x, mapa, by = c("Linea", "Categoria", "Producto"))
  x
}
.prod_resumen <- function(datos, nivel = "producto") {
  claves <- if (nivel == "categoria") c("Linea", "Categoria") else c("Item", "Linea", "Categoria", "Producto")
  x <- datos %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::summarise(Sacos = .prod_suma(SacosFact), Margen = .prod_suma(Margen),
      MargenCerrado = .prod_suma(Margen[EstadoFinanciero == "CERRADO"]),
      MargenAbierto = .prod_suma(Margen[EstadoFinanciero == "FACTURADO_ABIERTO"]),
      Clientes = dplyr::n_distinct(NIT[!is.na(NIT) & SacosFact > 0 & !is.na(SacosFact)]),
      Pedidos = dplyr::n_distinct(paste(NITFactura[!is.na(Pedido)], Pedido[!is.na(Pedido)], sep = "|")),
      Lotes = dplyr::n_distinct(Lote[!is.na(Lote)]), .groups = "drop") %>%
    dplyr::mutate(MargenKg = .prod_div(Margen, Sacos * 70),
      Participacion = .prod_div(pmax(Sacos, 0), sum(pmax(Sacos, 0), na.rm = TRUE))) %>%
    dplyr::arrange(dplyr::desc(Sacos))
  x$Grupo <- seq_len(nrow(x))
  x$Etiqueta <- if (nivel == "categoria") paste(x$Linea, x$Categoria, sep = " › ") else {
    paste(x$Linea, x$Categoria, x$Producto, sep = " › ")
  }
  x
}
.prod_indicadores <- function(datos) {
  listado <- datos %>% dplyr::filter(ProductoValido) %>% dplyr::distinct(Item)
  list(sacos = .prod_suma(datos$SacosFact), margen = .prod_suma(datos$Margen), productos = nrow(listado),
    clientes = dplyr::n_distinct(datos$NIT[!is.na(datos$NIT) & datos$SacosFact > 0 & !is.na(datos$SacosFact)]),
    margen_kg = .prod_div(.prod_suma(datos$Margen), .prod_suma(datos$SacosFact) * 70),
    pedidos = dplyr::n_distinct(paste(datos$NITFactura[!is.na(datos$Pedido)],
      datos$Pedido[!is.na(datos$Pedido)], sep = "|")),
    cerrado = .prod_suma(datos$Margen[datos$EstadoFinanciero == "CERRADO"]),
    abierto = .prod_suma(datos$Margen[datos$EstadoFinanciero == "FACTURADO_ABIERTO"]))
}

# Cestas observadas: no interpretar listas mensuales de facturas como facturas individuales ----
.mba_cestas <- function(datos, tipo = "pedido", nivel = "producto", filtros = NULL) {
  x <- datos
  total_movimientos <- nrow(x)
  x <- x[!is.na(x$SacosFact) & x$ProductoValido & !is.na(x$NITFactura), ]
  if (tipo == "pedido") {
    x <- x[!is.na(x$Pedido) & nzchar(x$Pedido) & x$Pedido != "0", ]
    claves <- c("NITFactura", "Pedido")
  } else if (tipo == "cliente_mes") {
    x <- x[!is.na(x$NIT), ]; claves <- c("NIT", "Mes")
  } else if (tipo == "cliente") {
    x <- x[!is.na(x$NIT), ]; claves <- "NIT"
  } else stop("Tipo de cesta no permitido.")
  x <- x %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::mutate(Cesta = dplyr::cur_group_id()) %>% dplyr::ungroup()
  if (nivel == "categoria") {
    mapa <- x %>% dplyr::distinct(Linea, Categoria) %>% dplyr::arrange(Linea, Categoria) %>%
      dplyr::mutate(Item = seq_len(dplyr::n()), Etiqueta = paste(Linea, Categoria, sep = " › "))
    x$Item <- x$Etiqueta <- NULL
    x <- dplyr::left_join(x, mapa, by = c("Linea", "Categoria"))
  } else if (nivel != "producto") stop("Nivel de ítem no permitido.")
  netos <- x %>% dplyr::group_by(Cesta, Item) %>%
    dplyr::summarise(SacosNetos = sum(SacosFact), .groups = "drop") %>% dplyr::filter(SacosNetos > 0)
  x <- dplyr::semi_join(x, netos, by = c("Cesta", "Item"))
  # El filtro global de producto/categoría selecciona cestas completas, no borra sus acompañantes.
  if (!is.null(filtros)) {
    elegibles <- x$Categoria %in% filtros$categoria & x$Producto %in% filtros$producto
    x <- x[x$Cesta %in% unique(x$Cesta[elegibles]), ]
  }
  mapa <- x %>% dplyr::distinct(Item, Etiqueta) %>% dplyr::arrange(Item)
  membresia <- x %>% dplyr::distinct(Cesta, Item)
  cestas <- x %>% dplyr::group_by(Cesta) %>%
    dplyr::summarise(Fecha = .gc_fecha_max(FecFact), NIT = .gc_unico(NIT), .groups = "drop") %>%
    dplyr::left_join(membresia %>% dplyr::count(Cesta, name = "Items"), by = "Cesta")
  list(movimientos = x, membresia = membresia, cestas = cestas, mapa = mapa,
    cobertura = data.frame(MovimientosPeriodo = total_movimientos, MovimientosElegibles = nrow(x),
      Cestas = nrow(cestas), CestasMultiproducto = sum(cestas$Items > 1L),
      MovimientosSinProducto = sum(!datos$ProductoValido),
      MovimientosNoPositivos = sum(is.na(datos$SacosFact) | datos$SacosFact <= 0),
      MovimientosSinPedido = sum(is.na(datos$Pedido)), MovimientosSinNIT = sum(is.na(datos$NITFactura))))
}
.mba_clave <- function(items) paste(sort(items), collapse = ",")
.mba_itemsets <- function(membresia, cestas, soporte = 0.03, minimo = 3L, longitud = 3L, limite = 50000L) {
  n <- length(cestas)
  if (!is.finite(soporte) || soporte <= 0 || soporte > 1 || !is.finite(minimo) || minimo < 1 ||
      !longitud %in% 2:5) stop("Revise soporte, mínimo de cestas y longitud (2 a 5).")
  if (!n) return(list(conjuntos = list(), n = 0L, minimo = minimo, candidatos = 0L))
  umbral <- max(ceiling(minimo), ceiling(soporte * n - 1e-10))
  vertical <- split(membresia$Cesta, membresia$Item)
  vertical <- lapply(vertical, function(ids) sort(unique(ids[ids %in% cestas])))
  vertical <- vertical[lengths(vertical) >= umbral]
  conjuntos <- lapply(names(vertical), function(id) as.integer(id))
  nombres <- vapply(conjuntos, .mba_clave, character(1))
  todos <- stats::setNames(lapply(seq_along(conjuntos), function(i) {
    list(items = conjuntos[[i]], tids = vertical[[i]], cuenta = length(vertical[[i]]))
  }), nombres)
  previos <- todos
  candidatos_totales <- 0L
  for (k in 2:longitud) {
    if (length(previos) < 2L) break
    grupos <- split(seq_along(previos), vapply(previos, function(a) .mba_clave(head(a$items, k - 2L)), character(1)))
    generados <- list()
    for (indices in grupos) {
      if (length(indices) < 2L) next
      posibles <- length(indices) * (length(indices) - 1) / 2
      if (candidatos_totales + posibles > limite) {
        stop(paste("Se alcanzó el límite de candidatos. Aumente el soporte o reduzca la longitud;",
          "no se muestran reglas parciales."))
      }
      pares <- utils::combn(indices, 2L)
      for (j in seq_len(ncol(pares))) {
        a <- previos[[pares[1, j]]]; b <- previos[[pares[2, j]]]
        union <- sort(unique(c(a$items, b$items)))
        if (length(union) != k) next
        candidatos_totales <- candidatos_totales + 1L
        subconjuntos <- utils::combn(union, k - 1L, simplify = FALSE)
        if (!all(vapply(subconjuntos, .mba_clave, character(1)) %in% names(previos))) next
        tids <- intersect(a$tids, b$tids)
        if (length(tids) >= umbral) generados[[.mba_clave(union)]] <- list(items = union,
          tids = tids, cuenta = length(tids))
      }
    }
    if (!length(generados)) break
    todos <- c(todos, generados)
    previos <- generados
  }
  list(conjuntos = todos, n = n, minimo = umbral, candidatos = candidatos_totales)
}
.mba_metricas <- function(n, a, b, ab) {
  pa <- a / n; pb <- b / n; pab <- ab / n
  confianza <- .prod_div(ab, a)
  denominador <- max(pab * (1 - pa), pa * (pb - pab))
  conviccion <- if (is.na(confianza)) NA_real_ else if (confianza == 1) {
    if (pb < 1) Inf else NA_real_
  } else (1 - pb) / (1 - confianza)
  c(Soporte = pab, Confianza = confianza, Lift = .prod_div(confianza, pb),
    Leverage = pab - pa * pb, Conviccion = conviccion, Jaccard = .prod_div(ab, a + b - ab),
    Coseno = .prod_div(ab, sqrt(a * b)), Zhang = .prod_div(pab - pa * pb, denominador))
}
.mba_reglas <- function(modelo, confianza = 0.5, lift = 1, estadistica = TRUE, limite = 20000L) {
  conjuntos <- modelo$conjuntos
  vacio <- data.frame(Regla = integer(), Antecedente = character(), Consecuente = character(),
    CestasAmbos = integer(), Soporte = numeric(), Confianza = numeric(), Lift = numeric())
  if (!length(conjuntos)) return(vacio)
  familias <- Filter(function(x) length(x$items) > 1L, conjuntos)
  total <- sum(vapply(familias, function(x) 2^length(x$items) - 2L, numeric(1)))
  if (total > limite) stop("Hay demasiadas reglas candidatas. Suba el soporte o reduzca la longitud.")
  filas <- list()
  for (conjunto in familias) {
    for (tam in seq_len(length(conjunto$items) - 1L)) {
      antecedentes <- utils::combn(conjunto$items, tam, simplify = FALSE)
      for (a in antecedentes) {
        b <- setdiff(conjunto$items, a)
        na <- conjuntos[[.mba_clave(a)]]$cuenta
        nb <- conjuntos[[.mba_clave(b)]]$cuenta
        fila <- data.frame(Antecedente = .mba_clave(a), Consecuente = .mba_clave(b),
          CestasAntecedente = na, CestasConsecuente = nb, CestasAmbos = conjunto$cuenta)
        met <- .mba_metricas(modelo$n, na, nb, conjunto$cuenta)
        for (nombre in names(met)) fila[[nombre]] <- met[[nombre]]
        filas[[length(filas) + 1L]] <- fila
      }
    }
  }
  if (!length(filas)) return(vacio)
  reglas <- dplyr::bind_rows(filas)
  reglas$Pvalor <- reglas$Qvalor <- rep(NA_real_, nrow(reglas))
  if (estadistica) {
    particiones <- vapply(seq_len(nrow(reglas)), function(i) {
      paste(sort(c(reglas$Antecedente[[i]], reglas$Consecuente[[i]])), collapse = "|")
    }, character(1))
    unicas <- which(!duplicated(particiones))
    valores <- vapply(unicas, function(i) {
      a <- reglas$CestasAntecedente[[i]]; b <- reglas$CestasConsecuente[[i]]; ab <- reglas$CestasAmbos[[i]]
      tabla <- matrix(c(ab, a - ab, b - ab, modelo$n - a - b + ab), 2L)
      if (any(rowSums(tabla) == 0) || any(colSums(tabla) == 0)) return(NA_real_)
      stats::fisher.test(tabla, alternative = "greater")$p.value
    }, numeric(1))
    reglas$Pvalor <- valores[match(particiones, particiones[unicas])]
    reglas$Qvalor <- stats::p.adjust(valores, method = "BH")[match(particiones, particiones[unicas])]
  }
  # BH se calcula sobre toda la familia antes de filtrar por confianza/lift.
  reglas <- reglas[is.finite(reglas$Confianza) & reglas$Confianza >= confianza &
    is.finite(reglas$Lift) & reglas$Lift >= lift, , drop = FALSE]
  reglas <- reglas[order(-reglas$Lift, -reglas$Confianza, -reglas$CestasAmbos), ]
  reglas$Regla <- seq_len(nrow(reglas))
  reglas
}
.mba_evaluar <- function(reglas, membresia, cestas) {
  n <- length(cestas)
  vertical <- split(membresia$Cesta, membresia$Item)
  contar <- function(clave) {
    items <- strsplit(clave, ",", fixed = TRUE)[[1]]
    listas <- lapply(items, function(item) intersect(vertical[[item]], cestas))
    if (!length(listas)) integer() else Reduce(intersect, listas)
  }
  salida <- reglas
  salida$SoporteValidacion <- salida$ConfianzaValidacion <- salida$LiftValidacion <- rep(NA_real_, nrow(salida))
  salida$AntecedentesValidacion <- salida$ConsecuentesValidacion <- salida$AmbosValidacion <- rep(0L, nrow(salida))
  if (!n) return(salida)
  for (i in seq_len(nrow(salida))) {
    a <- contar(salida$Antecedente[[i]]); b <- contar(salida$Consecuente[[i]])
    ab <- length(intersect(a, b))
    met <- .mba_metricas(n, length(a), length(b), ab)
    salida$AntecedentesValidacion[[i]] <- length(a)
    salida$ConsecuentesValidacion[[i]] <- length(b)
    salida$AmbosValidacion[[i]] <- ab
    salida$SoporteValidacion[[i]] <- met[["Soporte"]]
    salida$ConfianzaValidacion[[i]] <- met[["Confianza"]]
    salida$LiftValidacion[[i]] <- met[["Lift"]]
  }
  salida
}
.mba_etiquetas <- function(clave, mapa) {
  vapply(clave, function(x) paste(mapa$Etiqueta[match(as.integer(strsplit(x, ",", fixed = TRUE)[[1]]), mapa$Item)],
    collapse = " + "), character(1))
}
.mba_recomendaciones <- function(reglas, movimientos, mapa, maximo = 200L) {
  vacio <- data.frame(NIT = character(), Cliente = character(), Tiene = character(), Falta = character(),
    Confianza = numeric(), Lift = numeric(), CestasEvidencia = integer(), Regla = integer(),
    ConfianzaValidacion = numeric(), LiftValidacion = numeric())
  if (!nrow(reglas) || !nrow(movimientos)) return(vacio)
  cartera <- split(movimientos$Item, movimientos$NIT)
  cartera <- lapply(cartera, unique)
  nombres <- movimientos %>% dplyr::filter(!is.na(NIT)) %>% dplyr::group_by(NIT) %>%
    dplyr::summarise(Cliente = .gc_unico(Cliente), .groups = "drop")
  filas <- list(); vistos <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(nrow(reglas))) {
    a <- as.integer(strsplit(reglas$Antecedente[[i]], ",", fixed = TRUE)[[1]])
    b <- as.integer(strsplit(reglas$Consecuente[[i]], ",", fixed = TRUE)[[1]])
    for (nit in names(cartera)) {
      items <- cartera[[nit]]
      faltan <- setdiff(b, items)
      if (!all(a %in% items) || !length(faltan)) next
      clave <- paste(nit, .mba_clave(faltan), sep = "|")
      if (exists(clave, vistos, inherits = FALSE)) next
      assign(clave, TRUE, vistos)
      filas[[length(filas) + 1L]] <- data.frame(NIT = nit, Cliente = nombres$Cliente[match(nit, nombres$NIT)],
        Tiene = .mba_etiquetas(.mba_clave(a), mapa), Falta = .mba_etiquetas(.mba_clave(faltan), mapa),
        Confianza = reglas$Confianza[[i]], Lift = reglas$Lift[[i]], CestasEvidencia = reglas$CestasAmbos[[i]],
        Regla = reglas$Regla[[i]],
        ConfianzaValidacion = if ("ConfianzaValidacion" %in% names(reglas)) {
          reglas$ConfianzaValidacion[[i]]
        } else NA_real_,
        LiftValidacion = if ("LiftValidacion" %in% names(reglas)) reglas$LiftValidacion[[i]] else NA_real_)
      if (length(filas) >= maximo) return(dplyr::bind_rows(filas))
    }
  }
  if (length(filas)) dplyr::bind_rows(filas) else vacio
}
