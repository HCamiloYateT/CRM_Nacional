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
.crm_tabs <- function(..., id = NULL, selected = NULL, type = "tabs") {
  funcion <- .crm_forms_funcion("racafeTabs")
  if (!is.null(funcion)) return(funcion(..., id = id, selected = selected, type = type))
  shiny::tags$div(class = "rf-tabs", shiny::tabsetPanel(..., id = id, selected = selected, type = type))
}
