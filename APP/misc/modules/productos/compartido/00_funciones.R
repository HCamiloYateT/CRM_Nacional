# Hechos de producto: movimientos facturados, sin proyecciones ni multiplicación por lotes ----
.prod_suma <- function(x) racafeCore::SumaEstricta(x)
.prod_div <- function(a, b) {
  if (!length(a) || !length(b)) return(numeric())
  b <- rep_len(b, max(length(a), length(b)))
  ifelse(is.finite(b) & b > 0, a / b, NA_real_)
}
.prod_identificador <- function(x) racafeCore::IdentificadorTexto(x)
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
  racafeForecast::ConjuntosFrecuentes(membresia, cestas, soporte, minimo, longitud, limite)
}
.mba_metricas <- function(n, a, b, ab) {
  racafeForecast::MetricasAsociacion(n, a, b, ab)
}
.mba_reglas <- function(modelo, confianza = 0.5, lift = 1, estadistica = TRUE, limite = 20000L) {
  racafeForecast::ReglasAsociacion(modelo, confianza, lift, estadistica, limite)
}
.mba_evaluar <- function(reglas, membresia, cestas) {
  racafeForecast::ValidarReglasAsociacion(reglas, membresia, cestas)
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

.prod_medida_label <- function(x) {
  unname(c(Sacos = "Sacos netos de 70 kg", Margen = "Margen FCC · COP",
    MargenKg = "Margen por kilo · COP", Clientes = "Clientes con compra")[[x]])
}
