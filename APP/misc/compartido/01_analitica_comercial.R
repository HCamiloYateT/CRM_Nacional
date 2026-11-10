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
