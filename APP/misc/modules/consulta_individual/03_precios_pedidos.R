# Pedidos históricos: referencias comerciales y precio registrado, sin inferirlo del margen ----
.in_panel_ui <- function(id, title, ns) {
  .crm_disclosure(shiny::tagList(title, " ", shiny::uiOutput(ns(paste0(id, "_conteo")), inline = TRUE)),
    .crm_tabla_ui(ns(id)))
}
.in_conteo <- function(output, id, datos) {
  output[[paste0(id, "_conteo")]] <- shiny::renderUI({
    resultado <- tryCatch(list(n = nrow(datos())), error = function(e) list(n = NA_integer_))
    texto <- if (is.na(resultado$n)) "No disponible" else paste(resultado$n, "registros")
    racafeForms::racafeStatusBadge(texto, variant = "neutral", size = "sm")
  })
}
.in_precios_pedidos <- function(x) {
  x$Pedido <- .in_id(.gc_columna(x, "CLPdcCod"))
  x$NIT <- .in_id(.gc_columna(x, "CLCliNit"))
  x$FechaPedido <- .in_fecha(.gc_columna(x, "PdcFecCre"))
  x$PrecioKg <- .gc_num(.gc_columna(x, c("PdcPrecioKilo", "PrecioKiloPedido")))
  x$Marca <- as.character(.gc_columna(x, "Marca"))
  x$NombreComercial <- as.character(.gc_columna(x, c("MCNom", "NombreComercial")))
  x$Linea <- as.character(.gc_columna(x, c("CLLinNegNo", "LinNegCod")))
  x <- x[!is.na(x$Pedido), , drop = FALSE]
  x %>% dplyr::group_by(NIT, Pedido, Linea, FechaPedido, Marca, NombreComercial, PrecioKg) %>%
    dplyr::summarise(Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen),
      UltimaFactura = if (any(!is.na(FecFact))) max(FecFact, na.rm = TRUE) else as.Date(NA), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(FechaPedido), Pedido)
}
.in_precio_cercano <- function(x, precio) {
  if (length(precio) != 1L || !is.finite(precio) || precio <= 0) return(x[0, , drop = FALSE])
  x <- x[is.finite(x$PrecioKg) & x$PrecioKg > 0 & is.finite(x$Sacos) & x$Sacos > 0, , drop = FALSE]
  if (!nrow(x)) return(x)
  x$DiferenciaCOP <- abs(x$PrecioKg - precio)
  x[x$DiferenciaCOP == min(x$DiferenciaCOP), , drop = FALSE] %>% dplyr::arrange(dplyr::desc(FechaPedido))
}
.in_mercado_fecha <- function(datos, fecha) {
  if (!is.null(datos$error)) stop("No fue posible consultar los indicadores históricos.")
  x <- datos$datos
  if (!nrow(x) || length(fecha) != 1L || is.na(fecha)) return(data.frame())
  if (!"FechaActualizacion" %in% names(x)) stop("Los indicadores no tienen fecha de actualización.")
  x <- x[.in_fecha(x$FechaActualizacion) == fecha & !is.na(.in_fecha(x$FechaActualizacion)), , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  claves <- intersect(.IND_CLAVES_CANON, names(x))
  if (!length(claves)) return(data.frame())
  # Conservar ambigüedades y versiones: no elegir arbitrariamente una cotización del día.
  x <- unique(x[c("FechaActualizacion", claves)])
  tidyr::pivot_longer(x, cols = dplyr::all_of(claves), names_to = "Indicador", values_to = "Valor") %>%
    dplyr::mutate(Indicador = dplyr::recode(Indicador, !!!.IND_NOMBRES_COMPARACION))
}
