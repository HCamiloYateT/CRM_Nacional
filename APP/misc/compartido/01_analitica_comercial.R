# Utilidades compartidas de análisis comercial ----
.gc_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
.gc_unico <- function(x) {
  valores <- unique(x[!is.na(x)])
  if (length(valores) == 1L) valores[[1]] else x[NA_integer_][[1]]
}
.gc_suma <- function(x) if (!length(x) || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
.gc_mes <- function(x) racafeCore::PrimerDia(as.Date(x))
.gc_fecha_min <- function(x) if (any(!is.na(x))) min(x, na.rm = TRUE) else as.Date(NA)
.gc_fecha_max <- function(x) if (any(!is.na(x))) max(x, na.rm = TRUE) else as.Date(NA)
.gc_ratio <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, NA_real_)
.gc_columna <- function(dat, nombres, defecto = NA_character_) {
  nombre <- intersect(nombres, names(dat))
  if (!length(nombre)) return(rep(defecto, nrow(dat)))
  valor <- dat[[nombre[[1]]]]
  for (col in nombre[-1L]) {
    faltantes <- is.na(valor) | as.character(valor) == ""
    valor[faltantes] <- dat[[col]][faltantes]
  }
  valor
}
.gc_filtrar_dimensiones <- function(datos, filtros) {
  mapa <- c(Asesor = "asesor", Segmento = "segmento", CLLinNegNo = "linneg",
            Categoria = "categoria", Producto = "producto")
  for (col in names(mapa)) {
    if (col %in% names(datos)) datos <- datos[datos[[col]] %in% filtros[[mapa[[col]]]], , drop = FALSE]
  }
  for (col in intersect(c("Excluir", "ProdExcluir"), names(datos))) {
    datos <- datos[is.na(datos[[col]]) | datos[[col]] != "SI", , drop = FALSE]
  }
  datos
}
.gc_descarga <- function(output, datos, nombre) {
  output$descargar <- shiny::downloadHandler(
    filename = function() paste0(nombre, "_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(.crm_nits_tabla(datos()), file))
}
.gc_tarjetas_ui <- function(ns, cajas) {
  shiny::fluidRow(lapply(names(cajas), function(id) shiny::column(3,
    .crm_caja_ui(ns(id), texto = cajas[[id]], icono = "chart-simple"))))
}
.gc_tarjetas_server <- function(cajas, valores, formatos = NULL) {
  for (id in names(cajas)) local({
    clave <- id
    .crm_caja(clave, shiny::reactive(valores()[[clave]]), texto = cajas[[clave]], icono = "chart-simple",
      formato = if (is.null(formatos[[clave]])) "entero" else formatos[[clave]])
  })
}
.gc_modal_tabla <- function(titulo, datos, notas = NULL) {
  shiny::showModal(racafeForms::racafeModalDetail(title = titulo, size = "subventana2",
    shiny::tags$div(style = "overflow-x:auto;", .crm_tabla_vancouver(.crm_nits_tabla(datos),
      font_size = 11, row_padding = 2)), if (!is.null(notas)) .crm_nota(notas),
    footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
}

# Treemap con totales de origen: los tamaños inválidos solo se excluyen de la figura ----
.crm_treemap_productos <- function(x) {
  shiny::validate(shiny::need(nrow(x) && all(c("Categoria", "Producto", "Sacos") %in% names(x)),
    "No hay categorías y productos disponibles para el treemap."))
  x <- x[is.finite(x$Sacos) & x$Sacos > 0 & !is.na(x$Categoria) & !is.na(x$Producto), , drop = FALSE]
  shiny::validate(shiny::need(nrow(x) > 0L, "No hay sacos positivos con categoría y producto identificados."))
  con_linea <- "CLLinNegNo" %in% names(x)
  if (!con_linea) x$CLLinNegNo <- ""
  categorias <- x %>% dplyr::group_by(CLLinNegNo, Categoria) %>%
    dplyr::summarise(Sacos = sum(Sacos), Margen = .com_suma(Margen), .groups = "drop")
  lineas <- x %>% dplyr::group_by(CLLinNegNo) %>%
    dplyr::summarise(Sacos = sum(Sacos), Margen = .com_suma(Margen), .groups = "drop")
  categoria_id <- paste("C", categorias$CLLinNegNo, categorias$Categoria, sep = "|")
  producto_id <- paste("P", x$CLLinNegNo, x$Categoria, x$Producto, sep = "|")
  nodos <- data.frame(Id = "TOTAL", Padre = "", Etiqueta = "TOTAL", Sacos = sum(x$Sacos),
    Margen = .com_suma(x$Margen), Color = "#495057")
  if (con_linea) nodos <- dplyr::bind_rows(nodos, data.frame(
    Id = paste0("L|", lineas$CLLinNegNo), Padre = "TOTAL", Etiqueta = lineas$CLLinNegNo,
    Sacos = lineas$Sacos, Margen = lineas$Margen, Color = unname(.crm_colores("linneg", lineas$CLLinNegNo))))
  nodos <- dplyr::bind_rows(nodos, data.frame(Id = categoria_id,
    Padre = if (con_linea) paste0("L|", categorias$CLLinNegNo) else "TOTAL", Etiqueta = categorias$Categoria,
    Sacos = categorias$Sacos, Margen = categorias$Margen,
    Color = unname(.crm_colores("categoria", categorias$Categoria, categorias$CLLinNegNo))),
    data.frame(Id = producto_id, Padre = paste("C", x$CLLinNegNo, x$Categoria, sep = "|"),
      Etiqueta = x$Producto, Sacos = x$Sacos, Margen = x$Margen,
      Color = unname(.crm_colores("producto", x$Producto, x$CLLinNegNo, x$Categoria))))
  nodos$Participacion <- nodos$Sacos / sum(x$Sacos)
  plotly::plot_ly(nodos, type = "treemap", ids = ~Id, labels = ~Etiqueta, parents = ~Padre,
    values = ~Sacos, branchvalues = "total", textinfo = "label", marker = list(colors = nodos$Color),
    customdata = .crm_customdata(nodos, c("Participacion", "Margen")),
    hovertemplate = paste0("<b>%{label}</b><br>Sacos: %{value:,.2f}<br>Participación: %{customdata[0]:.1%}",
      "<br>Margen: $%{customdata[1]:,.0f}<extra></extra>"))
}

.crm_clave_precio <- function(x, campos) {
  if (!all(campos %in% names(x))) return(rep(NA_character_, nrow(x)))
  valores <- lapply(x[campos], .crm_id_texto)
  clave <- do.call(paste, c(valores, sep = "|"))
  clave[!Reduce(`&`, lapply(valores, function(v) !is.na(v)))] <- NA_character_
  clave
}
.crm_resumen_dimension <- function(x, dimensiones) {
  dimensiones <- intersect(dimensiones, names(x))
  if (!length(dimensiones) || !nrow(x)) return(data.frame())
  x$PedidoPrecio <-  .crm_clave_precio(x, c("CLCliNit", "CLPdcCod",
    if ("PrecioNegociadoOriginal" %in% names(x)) "CLPdcLin"))
  x$PrecioObservado <- .gc_num(.gc_columna(x, c("PrecioNegociadoKg", "PdcPrecioKilo", "PrecioKiloPedido")))
  # Si el precio negociado existe pero su unidad no fue validada, no sustituirlo por el de cabecera.
  if ("PrecioNegociadoOriginal" %in% names(x)) x$PrecioObservado <- .gc_num(.gc_columna(x, "PrecioNegociadoKg"))
  precios <- x %>% dplyr::filter(!is.na(PedidoPrecio)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(dimensiones, "PedidoPrecio")))) %>%
    dplyr::summarise(Precio = .gc_unico(PrecioObservado[is.finite(PrecioObservado) & PrecioObservado > 0]),
      .groups = "drop") %>% dplyr::group_by(dplyr::across(dplyr::all_of(dimensiones))) %>%
    dplyr::summarise(PrecioMinimoKg = if (any(is.finite(Precio))) min(Precio, na.rm = TRUE) else NA_real_,
      PrecioMaximoKg = if (any(is.finite(Precio))) max(Precio, na.rm = TRUE) else NA_real_,
      PrecioPromedioKg = if (any(is.finite(Precio))) mean(Precio, na.rm = TRUE) else NA_real_,
      PedidosConPrecio = sum(is.finite(Precio)), .groups = "drop")
  resumen <- x %>% dplyr::group_by(dplyr::across(dplyr::all_of(dimensiones))) %>%
    dplyr::summarise(Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen), .groups = "drop")
  resumen$ParticipacionSacos <- .gc_ratio(resumen$Sacos, .com_suma(x$SacosFact))
  total_margen <- .com_suma(x$Margen)
  resumen$ParticipacionMargen <- if (is.finite(total_margen) && total_margen != 0) resumen$Margen / total_margen else NA_real_
  dplyr::left_join(resumen, precios, by = dimensiones, na_matches = "na") %>% dplyr::arrange(dplyr::desc(Sacos))
}
