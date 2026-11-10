# Compatibilidad de componentes nuevos mientras se actualiza racafeForms ----
.crm_forms_funcion <- function(nombre) {
  tryCatch(getExportedValue("racafeForms", nombre), error = function(e) NULL)
}
.crm_input_mes <- function(...) {
  funcion <- .crm_forms_funcion("racafeInputMonth")
  if (!is.null(funcion)) funcion(...) else racafeForms::racafeInputDate(..., view = "month")
}
.crm_disclosure <- function(title, ..., open = FALSE) {
  funcion <- .crm_forms_funcion("racafeDisclosure")
  if (!is.null(funcion)) return(funcion(title, ..., open = open))
  racafeForms::racafePanel(title = NULL, shiny::tags$details(open = if (open) NA else NULL,
    shiny::tags$summary(title), shiny::tags$div(style = "padding-top:12px;", ...)))
}
# bs4Dash construye la navegación compatible con Bootstrap 4.
.crm_tabs <- function(..., id = NULL, selected = NULL, type = "tabs") {
  paneles <- Filter(Negate(is.null), list(...))
  do.call(bs4Dash::tabsetPanel, c(paneles, list(id = id, selected = selected, type = type)))
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
  obligatorio <- !isTRUE(argumentos$multiple) && sin_seleccion &&
    (isTRUE(argumentos$required) || "selected" %in% names(argumentos))
  if (obligatorio) argumentos$selected <- character()
  control <- do.call(racafeForms::racafeInputSelect,
    c(list(id = id, label = label, choices = .crm_choices(choices), empty = FALSE,
      empty_label = "Seleccione..."), argumentos))
  if (obligatorio) {
    control <- htmltools::tagQuery(control)$find("select")$addAttrs(title = "Seleccione...")$allTags()
  }
  control
}
.crm_update_select <- function(session, id, ..., choices = NULL) {
  racafeForms::racafeUpdateSelect(session, id, choices = .crm_choices(choices), ...)
}

# Los nombres comerciales se presentan sin cambiar valores ni códigos del ETL ----
.crm_label_comercial <- function(x) {
  if (!is.character(x)) return(x)
  gsub("convencionales", "Coproductos", x, ignore.case = TRUE)
}
