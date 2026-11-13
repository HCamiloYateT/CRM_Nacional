# Presentación compartida de la unidad comercial ----
.crm_badge_categoria <- function(valor, tipo) {
  if (length(valor) != 1L || is.na(valor) || !nzchar(valor)) return(NULL)
  color <- if (tipo == "etapa") unname(.COLORES_ETAPA[valor]) else "#6C5146"
  if (is.na(color)) color <- "#495057"
  shiny::tagAppendAttributes(racafeForms::racafeStatusBadge(valor, size = "xs"),
    style = paste0("border:1px solid ", color, ";color:", color, ";background:#fff;"))
}

.crm_unidad_ficha <- function(datos) {
  if (!nrow(datos)) return(.crm_nota("No hay una unidad comercial con línea identificada para esta entidad."))
  shiny::tagList(lapply(seq_len(nrow(datos)), function(i) {
    x <- datos[i, , drop = FALSE]
    racafeForms::racafePanel(variant = "muted", title = x$PerRazSoc,
      shiny::tags$div(class = "crm-identidad-badges",
        .crm_badge_categoria(x$EtapaVigente, "etapa"), .crm_badge_categoria(x$EstadoCliente, "estado"),
        .crm_badge_categoria(x$Origen, "origen")),
      racafeForms::racafeInfoGrid(racafeForms::racafeInfoItem("NIT", x$CliNitPpal),
        racafeForms::racafeInfoItem("Línea de negocio", .crm_label_comercial(x$CLLinNegNo)),
        racafeForms::racafeInfoItem("Asesor", x$Asesor), racafeForms::racafeInfoItem("Responsable", x$Responsable),
        racafeForms::racafeInfoItem("Segmento", x$Segmento)))
  }))
}
.crm_unidades_sesion <- function() {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session) || is.null(session$userData$unidades_comerciales)) return(data.frame())
  session$userData$unidades_comerciales()
}
UnidadComercialUI <- function(id) shiny::uiOutput(shiny::NS(id)("contenido"))
UnidadComercial <- function(id, datos, seleccion) {
  shiny::moduleServer(id, function(input, output, session) {
    output$contenido <- .crm_render_ui(.crm_unidad_ficha(datos()[datos()$Identificador %in% seleccion(), , drop = FALSE]))
  })
}
