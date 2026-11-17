# Escalas comerciales estables entre filtros y sesiones ----
.crm_colores_catalogo <- function() {
  if (!exists(".admon_choices_actualizacion", mode = "function")) return(data.frame())
  if (!exists(".ADMON_CHOICES_RDS") || !file.exists(.ADMON_CHOICES_RDS)) return(data.frame())
  sesion <- shiny::getDefaultReactiveDomain()
  revision <- if (is.null(sesion)) shiny::isolate(.admon_choices_actualizacion()) else .admon_choices_actualizacion()
  cache <- if (!is.null(sesion)) sesion$userData$colores_catalogo else NULL
  if (!is.null(cache) && identical(cache$revision, revision)) return(cache$datos)
  datos <- .admon_choices_colores(.admon_choices_leer())
  if (!is.null(sesion)) sesion$userData$colores_catalogo <- list(revision = revision, datos = datos)
  datos
}
.CRM_PALETA_ANALITICA <- c("#DA291C", "#842029", "#A56F5B", "#B98458", "#C39A61", "#D8BE98",
  "#788368", "#91A18B", "#476C75", "#607C88", "#737F88", "#414A52")
.crm_tono_producto <- function(color, clave) {
  h <- 0
  for (v in utf8ToInt(clave)) h <- (h * 131 + v) %% 2147483647
  base <- as.numeric(grDevices::col2rgb(color)) / 255
  mezcla <- 0.15 + ((h %/% 2) %% 1000) / 2500
  destino <- if (h %% 2L) rep(1, 3) else rep(0, 3)
  grDevices::rgb(matrix(base * (1 - mezcla) + destino * mezcla, nrow = 1))
}
.crm_colores <- function(dimension, valores, padre = "", padre2 = "") {
  valores <- toupper(trimws(as.character(valores)))
  valores[is.na(valores) | !nzchar(valores)] <- "SIN DATO"
  padre <- rep_len(toupper(as.character(padre)), length(valores))
  padre2 <- rep_len(toupper(as.character(padre2)), length(valores))
  padre[padre == "COPRODUCTOS"] <- "CONVENCIONALES"
  padre2[padre2 == "COPRODUCTOS"] <- "CONVENCIONALES"
  padre[padre == "DIFERENCIADOS"] <- "A LA MEDIDA"
  if (identical(dimension, "producto")) {
    padres <- .crm_colores("categoria", padre2, padre)
    derivados <- vapply(seq_along(valores), function(i) {
      if (valores[i] %in% c("SIN DATO", "SIN DATOS", "SIN PRODUCTO", "SIN CLASIFICAR", "SIN HOMOLOGAR")) return("#ADB5BD")
      .crm_tono_producto(padres[i], paste(padre[i], padre2[i], valores[i], sep = "|"))
    }, character(1))
    return(stats::setNames(derivados, valores))
  }
  paletas <- list(prioridad = .COLORES_PRIORIDAD,
    auxiliar = c(PRESUPUESTO = "#495057", EJECUCIÓN = "#DA291C", `RITMO REQUERIDO` = "#A38A33",
      `SIN CLASIFICAR` = "#ADB5BD"),
    linneg = c(COPRODUCTOS = "#0072B2", CONVENCIONALES = "#0072B2", `10000` = "#0072B2",
      `A LA MEDIDA` = "#E69F00", DIFERENCIADOS = "#E69F00", `21000` = "#E69F00"),
    etapa = c(CONTACTO = "#6C757D", LEAD = "#0072B2", PROSPECTO = "#E69F00", CLIENTE = "#009E73",
      DESCARTADO = "#CC79A7"), estado = c(ACTIVO = "#009E73", INACTIVO = "#6C757D", NUEVO = "#CC79A7"),
    rfm = c(`LEALES RECIENTES` = "#7A2028", `LEALES EN RIESGO` = "#495057", `NUEVOS OBSERVADOS` = "#C8A77D",
      `RECIENTES OCASIONALES` = "#9B7160", `OCASIONALES RECIENTES` = "#9B7160", `POR REACTIVAR` = "#ADB5BD",
        `COMPRA ÚNICA OBSERVADA` = "#D8C5A8",
      `INACTIVOS RELATIVOS` = "#6C757D", `EN DESARROLLO` = "#9B7160"))
  padre[padre == "COPRODUCTOS"] <- "CONVENCIONALES"
  padre2[padre2 == "COPRODUCTOS"] <- "CONVENCIONALES"
  padre[padre == "DIFERENCIADOS"] <- "A LA MEDIDA"
  hash <- function(x) {
    h <- 0
    for (v in utf8ToInt(x)) h <- (h * 131 + v) %% 2147483647
    h
  }
  colores <- vapply(seq_along(valores), function(i) {
    if (valores[i] %in% c("SIN DATO", "SIN DATOS", "SIN CATEGORÍA", "SIN CATEGORIA",
        "SIN PRODUCTO", "SIN CLASIFICAR", "SIN HOMOLOGAR")) return("#ADB5BD")
    fijo <- unname(paletas[[dimension]][valores[i]])
    if (length(fijo) && !is.na(fijo)) return(fijo)
    h <- hash(if (dimension == "categoria") paste(dimension, valores[i], sep = "|") else
      paste(dimension, padre[i], padre2[i], valores[i], sep = "|"))
    if (dimension %in% c("categoria", "producto")) {
      return(grDevices::hcl(h = h %% 360, c = 55, l = 55))
    }
    grDevices::hcl(h = h %% 360, c = 55, l = 50 + (h %/% 37) %% 15)
  }, character(1))
  configurados <- .crm_colores_catalogo()
  if (nrow(configurados)) {
    firma <- function(d, p, p2, v) paste(d, p, p2, v, sep = "\r")
    equivalencia <- function(x) {
      x[x == "COPRODUCTOS"] <- "CONVENCIONALES"
      x[x == "DIFERENCIADOS"] <- "A LA MEDIDA"
      x
    }
    indice <- match(firma(dimension, equivalencia(padre), equivalencia(padre2), equivalencia(valores)),
      firma(configurados$catalogo, equivalencia(configurados$padre), equivalencia(configurados$padre2),
        equivalencia(configurados$opcion)))
    if (dimension == "categoria") {
      globales <- which(configurados$catalogo == "categoria" & configurados$padre == "" & configurados$padre2 == "")
      global <- globales[match(valores, configurados$opcion[globales])]
      indice[!is.na(global)] <- global[!is.na(global)]
    }
    colores[!is.na(indice)] <- configurados$color[indice[!is.na(indice)]]
  }
  stats::setNames(colores, valores)
}
# Variables adicionales por punto sin perder matrices durante plotly_build ----
.crm_customdata <- function(datos, columnas) {
  lapply(seq_len(nrow(datos)), function(i) unname(as.list(datos[i, columnas, drop = FALSE])))
}

# Evitar asignaciones nuevas prácticamente indistinguibles dentro de una dimensión ----
.crm_color_distinguible <- function(color, otros) {
  if (!length(otros)) return(TRUE)
  rgb <- grDevices::col2rgb(otros)
  objetivo <- as.numeric(grDevices::col2rgb(color))
  all(sqrt(colSums((rgb - objetivo)^2)) >= 25)
}
