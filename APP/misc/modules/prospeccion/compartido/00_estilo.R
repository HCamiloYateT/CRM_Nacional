# Panel corporativo que conserva el ancho y los IDs de los módulos de Prospección ----
.pr_panel <- function(..., title = NULL, width = 12, collapsible = FALSE, collapsed = FALSE,
                       status = NULL, solidHeader = FALSE, height = NULL, background = NULL,
                       footer = NULL, id = NULL, closable = FALSE, maximizable = FALSE) {
  contenido <- if (collapsible && !is.null(title)) .crm_disclosure(title, ..., open = !collapsed) else {
    racafeForms::racafePanel(title = title, ..., footer = footer)
  }
  shiny::column(width, shiny::tags$div(class = "crm-prospeccion", id = id, contenido))
}
