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
.crm_colores <- function(dimension, valores, padre = "", padre2 = "") {
  valores <- toupper(trimws(as.character(valores)))
  valores[is.na(valores) | !nzchar(valores)] <- "SIN DATO"
  padre <- rep_len(toupper(as.character(padre)), length(valores))
  padre2 <- rep_len(toupper(as.character(padre2)), length(valores))
  paletas <- list(linneg = c(COPRODUCTOS = "#7A2028", CONVENCIONALES = "#7A2028", `10000` = "#7A2028",
      `A LA MEDIDA` = "#9B7160", DIFERENCIADOS = "#9B7160", `21000` = "#9B7160"),
    etapa = c(CONTACTO = "#C8C8C8", LEAD = "#0073A8", PROSPECTO = "#C9A66B", CLIENTE = "#B3001B",
      DESCARTADO = "#1A1A1A"), estado = c(ACTIVO = "#6C5146", INACTIVO = "#ADB5BD", NUEVO = "#C8A77D"),
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
    h <- hash(paste(dimension, padre[i], padre2[i], valores[i], sep = "|"))
    if (dimension %in% c("categoria", "producto")) {
      base <- if (padre[i] %in% c("A LA MEDIDA", "21000")) 48 else 12
      categoria <- if (dimension == "categoria") valores[i] else padre2[i]
      tono <- base + (hash(categoria) %% 29) - 14
      luz <- if (dimension == "categoria") 45 + hash(categoria) %% 15 else 38 + h %% 35
      return(grDevices::hcl(h = tono, c = 22 + hash(categoria) %% 15, l = luz))
    }
    grDevices::hcl(h = 12 + h %% 35, c = 18 + h %% 23, l = 35 + (h %/% 37) %% 35)
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
    colores[!is.na(indice)] <- configurados$color[indice[!is.na(indice)]]
  }
  stats::setNames(colores, valores)
}
# Variables adicionales por punto sin perder matrices durante plotly_build ----
.crm_customdata <- function(datos, columnas) {
  lapply(seq_len(nrow(datos)), function(i) unname(as.list(datos[i, columnas, drop = FALSE])))
}
