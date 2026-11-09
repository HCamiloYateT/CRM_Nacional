# Fuentes y alcance de la evolución ----
.ev_linea <- function(codigo) {
  ifelse(as.character(codigo) == "10000", "Coproductos",
    ifelse(as.character(codigo) %in% c("20000", "21000"), "A la medida", "Otra línea"))
}
.ev_id <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  y <- format(x, scientific = FALSE, trim = TRUE, big.mark = "", digits = 22)
  y[is.na(x)] <- NA_character_
  y
}
.ev_referencia <- function(ruta_app) {
  ruta <- file.path(ruta_app, "misc/modules/generalidades/evolucion_negocio/data")
  list(anual = utils::read.csv(file.path(ruta, "referencia_presentacion.csv"), stringsAsFactors = FALSE),
    plan = utils::read.csv(file.path(ruta, "plan_crecimiento.csv"), stringsAsFactors = FALSE))
}
.ev_completo <- function(hechos, filtros, campos = c("asesor", "segmento", "linneg", "categoria", "producto")) {
  mapa <- c(asesor = "Asesor", segmento = "Segmento", linneg = "CLLinNegNo",
    categoria = "Categoria", producto = "Producto")
  x <- hechos
  for (col in intersect(c("Excluir", "ProdExcluir"), names(x))) {
    x <- x[is.na(x[[col]]) | x[[col]] != "SI", , drop = FALSE]
  }
  all(vapply(campos, function(campo) {
    valores <- unique(as.character(x[[mapa[[campo]]]]))
    valores <- valores[!is.na(valores)]
    length(valores) > 0L && all(valores %in% filtros[[campo]])
  }, logical(1)))
}
.ev_catalogo <- function(clientes) {
  x <- clientes
  if (!"CLCliNit" %in% names(x)) x$CLCliNit <- x$CliNitPpal
  x$CLCliNit <- .ev_id(x$CLCliNit)
  x$CliNitPpal <- .ev_id(x$CliNitPpal)
  if ("FecProceso" %in% names(x)) {
    x$.Corte <- as.numeric(as.POSIXct(x$FecProceso, tz = "UTC"))
    x <- x %>% dplyr::filter(!is.na(.Corte)) %>% dplyr::group_by(CLCliNit, LinNegCod) %>%
      dplyr::filter(.Corte == max(.Corte)) %>% dplyr::ungroup()
  }
  columnas <- c("CLCliNit", "LinNegCod", "CliNitPpal", "Asesor", "Segmento", "Excluir")
  for (col in setdiff(columnas, names(x))) x[[col]] <- NA_character_
  x <- unique(as.data.frame(x[columnas]))
  # Los empates contradictorios no autorizan una atribución histórica arbitraria.
  ambiguo <- duplicated(x[1:2]) | duplicated(x[1:2], fromLast = TRUE)
  x <- x[!ambiguo & !is.na(x$Asesor) & nzchar(trimws(x$Asesor)), ]
  x$CLLinNegNo <- ifelse(x$LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA")
  x
}
.ev_mensual <- function(hechos, historico, clientes, filtros, corte, abiertos = FALSE) {
  x <- hechos
  x$FecMes <- .gc_mes(x$FecMes)
  # El límite se determina antes de los filtros: el Excel no repone ventas excluidas por el usuario.
  limites <- x %>% dplyr::filter(EstadoFinanciero == "CERRADO", !is.na(FecMes)) %>%
    dplyr::group_by(LinNegCod) %>% dplyr::summarise(InicioETL = min(FecMes), .groups = "drop")
  x <- .gc_filtrar_dimensiones(x, filtros)
  x <- x[x$EstadoFinanciero %in% c("CERRADO", if (abiertos) "FACTURADO_ABIERTO") &
    !is.na(x$FecFact) & !is.na(x$FecMes) & x$FecMes <= .gc_mes(corte), , drop = FALSE]
  x$CliNitPpal <- .ev_id(x$CliNitPpal)
  x$Linea <- .ev_linea(x$LinNegCod)
  x$Fuente <- ifelse(x$EstadoFinanciero == "CERRADO", "ETL / PyG", "ETL / facturación provisional")
  columnas <- c("CliNitPpal", "LinNegCod", "Linea", "Asesor", "Segmento", "FecMes", "SacosFact", "Margen", "Fuente")
  x <- x[columnas]
  omitidos <- 0L
  if (!is.null(historico) && .ev_completo(hechos, filtros, c("categoria", "producto"))) {
    h <- .ev_validar_historico(historico)
    h <- dplyr::left_join(h, limites, by = "LinNegCod")
    h <- h[is.na(h$InicioETL) | h$FecMes < h$InicioETL, ]
    h <- dplyr::left_join(h, .ev_catalogo(clientes), by = c("CLCliNit", "LinNegCod"), na_matches = "never")
    omitidos <- sum(is.na(h$CliNitPpal))
    h <- h[!is.na(h$CliNitPpal) & h$FecMes <= .gc_mes(corte), ]
    h <- .gc_filtrar_dimensiones(h, filtros)
    h$Linea <- .ev_linea(h$LinNegCod)
    x <- dplyr::bind_rows(x, h[columnas])
  }
  attr(x, "historico_sin_homologacion") <- omitidos
  x
}

# Comparativos anuales sin mezclar ejecutado con escenarios ----
.ev_dimension <- function(x, dimension) {
  if (dimension == "Total") return(rep("Industria Nacional", nrow(x)))
  if (dimension == "Linea") return(as.character(x$Linea))
  y <- as.character(x[[dimension]])
  y[is.na(y) | !nzchar(trimws(y))] <- "Sin clasificar"
  y
}
.ev_anual <- function(mensual, clientes, filtros, corte, dimension = "Linea", comparable = FALSE,
                      referencia = NULL, permitir_referencia = FALSE, inicio_etl = 2020L,
                      presupuesto_disponible = TRUE) {
  anho <- as.integer(format(corte, "%Y"))
  mes <- as.integer(format(corte, "%m"))
  x <- mensual
  x$Anho <- as.integer(format(x$FecMes, "%Y"))
  x$Mes <- as.integer(format(x$FecMes, "%m"))
  if (comparable) x <- x[x$Mes <= mes, ]
  x$Dimension <- .ev_dimension(x, dimension)
  e <- x %>% dplyr::group_by(Anho, Dimension) %>%
    dplyr::summarise(Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen),
      MargenProvisional = .com_suma(Margen[Fuente == "ETL / facturación provisional"]),
      Clientes = dplyr::n_distinct(CliNitPpal[SacosFact > 0 & !is.na(SacosFact)], na.rm = TRUE),
      Fuente = paste(sort(unique(Fuente)), collapse = " | "), .groups = "drop")
  # Presupuesto desde los snapshots de cada año, una vez por unidad comercial.
  anos <- sort(unique(c(e$Anho, anho)))
  presupuestos <- lapply(anos, function(a) {
    fecha <- if (a == anho) corte else as.Date(paste0(a, "-12-31"))
    c <- .com_clientes(clientes, fecha, filtros, anual = TRUE)
    detalle <- attr(c, "presupuesto_detalle")
    if (!is.null(detalle)) c <- detalle
    c$Linea <- .ev_linea(c$LinNegCod)
    c$Dimension <- .ev_dimension(c, dimension)
    c %>% dplyr::group_by(Dimension) %>% dplyr::summarise(Anho = a,
      PptoSacosAnual = .com_suma(SSPpto), PptoMargenAnual = .com_suma(MNFCCPpto),
      FuentePresupuesto = "Snapshot de clientes del año", .groups = "drop")
  })
  p <- dplyr::bind_rows(presupuestos)
  e <- dplyr::full_join(e, p, by = c("Anho", "Dimension"))
  # La referencia anual no tiene detalle mensual ni permite atribuir ventas a un asesor o producto.
  if (permitir_referencia && !comparable && dimension %in% c("Total", "Linea") && !is.null(referencia)) {
    r <- referencia[referencia$Ambito == "Linea" & referencia$Anho <= min(2025L, anho) &
      referencia$Linea %in% c("A la medida", "Coproductos"), ]
    codigos <- c("A la medida" = "A LA MEDIDA", Coproductos = "CONVENCIONALES")
    r <- r[unname(codigos[r$Linea]) %in% filtros$linneg, ]
    r$Dimension <- .ev_dimension(r, dimension)
    r <- r %>% dplyr::group_by(Anho, Dimension) %>%
      dplyr::summarise(Sacos = .com_suma(Sacos), Margen = .com_suma(Margen),
        PptoSacosAnual = .com_suma(PptoSacos), PptoMargenAnual = .com_suma(PptoMargen),
        Fuente = "Presentación / referencia anual redondeada",
        FuentePresupuesto = "Presentación / presupuesto anual original", .groups = "drop")
    # Una meta histórica documentada completa años completos sin snapshot, nunca un empate contradictorio.
    clave <- paste(e$Anho, e$Dimension)
    pos <- match(clave, paste(r$Anho, r$Dimension))
    completar <- is.na(e$FuentePresupuesto) & !is.na(pos) & (e$Anho < anho | mes == 12L)
    e$PptoSacosAnual[completar] <- r$PptoSacosAnual[pos[completar]]
    e$PptoMargenAnual[completar] <- r$PptoMargenAnual[pos[completar]]
    e$FuentePresupuesto[completar] <- r$FuentePresupuesto[pos[completar]]
    r <- r[r$Anho < min(inicio_etl, anho + as.integer(mes == 12L)), ]
    r <- dplyr::anti_join(r, e, by = c("Anho", "Dimension"))
    e <- dplyr::bind_rows(e, r)
  }
  if (!presupuesto_disponible) {
    e$PptoSacosAnual <- e$PptoMargenAnual <- NA_real_
    e$FuentePresupuesto <- "No asignable al filtro de producto/categoría"
  }
  e$MesesComparados <- ifelse(e$Anho == anho | comparable, mes, 12L)
  e$PptoSacos <- e$PptoSacosAnual * e$MesesComparados / 12
  e$PptoMargen <- e$PptoMargenAnual * e$MesesComparados / 12
  e$CumplSacos <- .gc_ratio(e$Sacos, e$PptoSacos)
  e$CumplMargen <- .gc_ratio(e$Margen, e$PptoMargen)
  e$MargenKg <- .gc_ratio(e$Margen, e$Sacos * 70)
  e$ProySacos <- ifelse(e$Anho == anho & mes < 12, e$Sacos * 12 / mes, NA_real_)
  e$ProyMargen <- ifelse(e$Anho == anho & mes < 12, e$Margen * 12 / mes, NA_real_)
  e$Estado <- ifelse(e$Anho == anho & mes < 12, paste("Acumulado a mes", mes), "Año completo")
  e$Fuente[is.na(e$Fuente)] <- "Solo presupuesto disponible"
  e <- e %>% dplyr::group_by(Dimension) %>% dplyr::arrange(Anho, .by_group = TRUE) %>%
    dplyr::mutate(VariacionSacos = ifelse(Anho - dplyr::lag(Anho) == 1L &
      MesesComparados == dplyr::lag(MesesComparados), .gc_ratio(Sacos - dplyr::lag(Sacos),
      abs(dplyr::lag(Sacos))), NA_real_),
      VariacionMargen = ifelse(Anho - dplyr::lag(Anho) == 1L & MesesComparados == dplyr::lag(MesesComparados),
        .gc_ratio(Margen - dplyr::lag(Margen), abs(dplyr::lag(Margen))), NA_real_)) %>% dplyr::ungroup()
  as.data.frame(e)
}
.ev_crecimiento <- function(anual, base, fin) {
  x <- anual[anual$Anho %in% c(base, fin), ]
  b <- x[x$Anho == base, ]; f <- x[x$Anho == fin, ]
  x <- dplyr::inner_join(b, f, by = "Dimension", suffix = c("Base", "Fin"))
  x <- x[x$MesesComparadosBase == x$MesesComparadosFin, ]
  data.frame(Dimension = x$Dimension, Desde = rep(base, nrow(x)), Hasta = rep(fin, nrow(x)),
    VariacionSacos = .gc_ratio(x$SacosFin - x$SacosBase, abs(x$SacosBase)),
    VariacionMargen = .gc_ratio(x$MargenFin - x$MargenBase, abs(x$MargenBase)),
    CAGR_Sacos = ifelse(x$SacosBase > 0 & x$SacosFin > 0 & fin > base,
      (x$SacosFin / x$SacosBase)^(1 / max(1, fin - base)) - 1, NA_real_),
    CAGR_Margen = ifelse(x$MargenBase > 0 & x$MargenFin > 0 & fin > base,
      (x$MargenFin / x$MargenBase)^(1 / max(1, fin - base)) - 1, NA_real_))
}
# El contrato exporta fact_cliente por NIT de facturación; homologar antes de clasificar clientes nuevos.
.ev_primera_compra <- function(facturas, nit_principal) {
  mapa <- nit_principal %>% dplyr::transmute(FctNit = .ev_id(PerCod), CliNitPpal = .ev_id(CliNitPpal)) %>%
    dplyr::group_by(FctNit) %>% dplyr::summarise(CliNitPpal = .gc_unico(CliNitPpal), .groups = "drop")
  x <- facturas %>% dplyr::transmute(FctNit = .ev_id(FctNit), MinFecFact = as.Date(MinFecFact))
  dplyr::left_join(x, mapa, by = "FctNit", na_matches = "never") %>%
    dplyr::filter(!is.na(CliNitPpal)) %>% dplyr::group_by(CliNitPpal) %>%
    dplyr::summarise(MinFecFact = .gc_fecha_min(MinFecFact), .groups = "drop")
}
.ev_nuevos <- function(mensual, primera_compra, anho, dimension) {
  x <- mensual
  primero <- x %>% dplyr::filter(SacosFact > 0, !is.na(SacosFact)) %>% dplyr::group_by(CliNitPpal) %>%
    dplyr::summarise(PrimeraObservada = min(FecMes), .groups = "drop")
  if (!is.null(primera_compra) && all(c("CliNitPpal", "MinFecFact") %in% names(primera_compra))) {
    p <- primera_compra %>% dplyr::transmute(CliNitPpal = .ev_id(CliNitPpal), Fecha = as.Date(MinFecFact)) %>%
      dplyr::group_by(CliNitPpal) %>% dplyr::summarise(PrimeraSistema = .gc_fecha_min(Fecha), .groups = "drop")
    primero <- dplyr::left_join(primero, p, by = "CliNitPpal")
    primero$PrimeraObservada <- pmin(primero$PrimeraObservada, primero$PrimeraSistema, na.rm = TRUE)
  }
  x <- dplyr::left_join(x, primero, by = "CliNitPpal")
  x <- x[format(x$PrimeraObservada, "%Y") == as.character(anho) &
    format(x$FecMes, "%Y") == as.character(anho), ]
  x$Dimension <- .ev_dimension(x, dimension)
  n_clientes <- dplyr::n_distinct(x$CliNitPpal)
  resultado <- x %>% dplyr::group_by(Dimension) %>% dplyr::summarise(Clientes = dplyr::n_distinct(CliNitPpal),
    Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen), .groups = "drop")
  attr(resultado, "clientes_unicos") <- n_clientes
  resultado
}

# Gráficas: dos métricas con sus unidades, sin doble eje ----
.ev_grafico <- function(anual, medida, proyectar = FALSE) {
  ppto <- if (medida == "Margen") "PptoMargen" else "PptoSacos"
  proy <- if (medida == "Margen") "ProyMargen" else "ProySacos"
  divisor <- if (medida == "Margen") 1e6 else 1
  e <- data.frame(Anho = anual$Anho, Dimension = anual$Dimension,
    Serie = "Ejecutado", Valor = anual[[medida]] / divisor)
  p <- data.frame(Anho = anual$Anho, Dimension = anual$Dimension,
    Serie = "Presupuesto comparable", Valor = anual[[ppto]] / divisor)
  x <- rbind(p, e)
  x$Serie <- factor(x$Serie, levels = c("Presupuesto comparable", "Ejecutado"))
  g <- ggplot2::ggplot(x, ggplot2::aes(x = factor(Anho), y = Valor, fill = Serie)) +
    ggplot2::geom_col(position = "dodge", na.rm = TRUE, width = 0.75) +
    ggplot2::facet_wrap(~Dimension, scales = "free_y", ncol = 2) +
    ggplot2::scale_fill_manual(values = c("#D9D9D9", "#DA291C")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(x = NULL, y = if (medida == "Margen") "Margen FCC · millones COP" else "Sacos de 70 kg",
      fill = NULL, title = if (medida == "Margen") "Evolución del margen" else "Evolución del volumen") +
    .crm_tema_grafico() +
    ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if (proyectar) {
    z <- anual[is.finite(anual[[proy]]), ]; z$Valor <- z[[proy]] / divisor
    g <- g + ggplot2::geom_point(data = z, ggplot2::aes(x = factor(Anho), y = Valor,
      shape = "Proyección simple de cierre"), inherit.aes = FALSE, colour = "#495057", size = 3) +
      ggplot2::scale_shape_manual(values = 5, name = NULL)
  }
  g
}
