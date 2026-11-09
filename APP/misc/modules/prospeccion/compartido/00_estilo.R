# Panel corporativo que conserva el ancho y los IDs de los módulos de Prospección ----
.pr_panel <- function(..., title = NULL, width = 12, collapsible = FALSE, collapsed = FALSE,
                       status = NULL, solidHeader = FALSE, height = NULL, background = NULL,
                       footer = NULL, id = NULL, closable = FALSE, maximizable = FALSE) {
  contenido <- if (collapsible && !is.null(title)) .crm_disclosure(title, ..., open = !collapsed) else {
    racafeForms::racafePanel(title = title, ..., footer = footer)
  }
  shiny::column(width, shiny::tags$div(class = "crm-prospeccion", id = id, contenido))
}

# Las cajas de Prospección comparten el mismo componente de los otros capítulos ----
.pr_caja <- function(id, valor, formato = "numero", ...) {
  # Los módulos anteriores entregan algunos valores ya formateados por html_valor().
  racafeShiny::DefinirFormato("crm_prospeccion", fn = function(x) as.character(x))
  contenido <- shiny::reactive({
    x <- if (shiny::is.reactive(valor)) valor() else valor
    if (!length(x) || (length(x) == 1L && is.na(x))) return(NA_character_)
    if (is.numeric(x)) return(as.character(racafeShiny::FormatearNumero(x, formato)))
    if (inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) return(as.character(x))
    htmltools::htmlEscape(as.character(x))
  })
  .crm_caja(id, valor = contenido, formato = "crm_prospeccion", ...)
}
