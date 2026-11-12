# Compatibilidad de componentes nuevos mientras se actualiza racafeForms ----
.crm_forms_funcion <- function(nombre) {
  tryCatch(getExportedValue("racafeForms", nombre), error = function(e) NULL)
}
.crm_input_mes <- function(...) racafeForms::racafeInputMonth(...)
.crm_disclosure <- local({
  contador <- 0L
  function(title, ..., open = FALSE) {
    contador <<- contador + 1L
    badge <- NULL
    if (inherits(title, "shiny.tag.list") && is.character(title[[1L]])) {
      badge <- shiny::tagList(title[-1L]); title <- title[[1L]]
    } else if (!is.character(title)) {
      badge <- title; title <- "Detalle"
    }
    if (!length(title)) title <- "Detalle"
    sesion <- shiny::getDefaultReactiveDomain()
    id <- paste0("crm_seccion_", contador)
    if (!is.null(sesion)) id <- sesion$ns(id)
    racafeShiny::racafeCollapsible(id, title, open = open, ..., color = "#6C757D", header_badge = badge, class = "crm-seccion")
  }
})
.crm_nota_metodologica <- function(texto, title = "Cómo leer esta sección", ...) {
  .crm_disclosure(title, .crm_nota(texto, ...))
}
.crm_seccion_presupuesto <- function(..., title = NULL, width = 12, collapsible = TRUE, collapsed = FALSE,
                                     status = NULL, solidHeader = NULL, footer = NULL, elevation = NULL) {
  shiny::column(width, if (collapsible) .crm_disclosure(title, ..., footer, open = !collapsed) else
    racafeForms::racafePanel(title = title, ..., footer = footer))
}

# bs4Dash construye la navegación compatible con Bootstrap 4.
.crm_tabs <- function(..., id = NULL, selected = NULL, type = "tabs") {
  racafeForms::racafeTabs(..., id = id, selected = selected, type = type, engine = "bs4dash", styled = FALSE)
}

# Un selector muestra opciones reales; la ausencia de selección es un estado del control.
.crm_choices <- function(x) {
  if (!length(x)) return(x)
  if (is.list(x)) return(lapply(x, .crm_choices))
  nombres <- names(x)
  quitar <- is.na(x) | as.character(x) == ""
  etiquetas <- if (is.null(nombres)) as.character(x) else nombres
  quitar <- quitar | toupper(trimws(etiquetas)) %in% c("NINGUNO", "NINGUNA")
  x <- x[!quitar]
  if (!length(x)) return(x)
  names(x) <- .crm_label_comercial(if (is.null(names(x))) as.character(x) else names(x))
  x
}
.crm_select <- function(id, label = NULL, choices = NULL, ..., empty = FALSE, empty_label = "Seleccione...") {
  argumentos <- list(...)
  sin_seleccion <- is.null(argumentos$selected) || !length(argumentos$selected) ||
    all(is.na(argumentos$selected) | argumentos$selected == "")
  obligatorio <- !isTRUE(argumentos$multiple) && sin_seleccion
  if (obligatorio) argumentos$selected <- character()
  control <- do.call(racafeForms::racafeInputSelect,
    c(list(id = id, label = label, choices = .crm_choices(choices), empty = FALSE,
      empty_label = "Seleccione...", placeholder = if (obligatorio) "Seleccione..."), argumentos))
  control
}
.crm_update_select <- function(session, id, ..., choices = NULL) {
  argumentos <- list(...)
  if ("selected" %in% names(argumentos) &&
      (is.null(argumentos$selected) || all(is.na(argumentos$selected) | argumentos$selected == ""))) {
    argumentos$selected <- character()
  }
  # Bootstrap-select recrea el picker al actualizar: el título de placeholder no debe borrar un valor real.
  if ("selected" %in% names(argumentos)) {
    opciones <- argumentos$options %||% list()
    opciones$title <- if (!length(argumentos$selected)) "Seleccione..." else ""
    argumentos$options <- opciones
  }
  do.call(racafeForms::racafeUpdateSelect,
    c(list(session = session, id = id, choices = .crm_choices(choices)), argumentos))
}

# Los nombres comerciales se presentan sin cambiar valores ni códigos del ETL ----
.crm_label_comercial <- function(x) {
  if (!is.character(x)) return(x)
  toupper(gsub("convencionales", "Coproductos", x, ignore.case = TRUE))
}
