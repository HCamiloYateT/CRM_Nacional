# Componentes corporativos de presentación ----
.crm_caja_ui <- function(id, texto, icono, footer = NULL, mostrar_boton = FALSE, id_boton = "detalle",
                         tooltip = "Ver detalle", color_fondo = NULL) {
  caja <- racafeModulos::cajaValor_ui(id, texto = texto, icono = icono, footer = footer, compact = TRUE,
                                     mostrar_boton = mostrar_boton, label_boton = NULL, id_boton = id_boton)
  if (!is.null(color_fondo)) caja <- shiny::tagAppendAttributes(caja, style = paste0("background:", color_fondo, ";"))
  if (mostrar_boton) {
    caja <- htmltools::tagQuery(caja)$find(".rmcv-button")$removeAttrs("title")$removeAttrs("aria-label")$
      addAttrs(title = tooltip, `aria-label` = tooltip,
               style = "width:22px;height:22px;min-width:22px;padding:2px;font-size:10px;line-height:1;")$allTags()
  }
  caja
}

.crm_caja_output <- function(id) shiny::uiOutput(paste0(id, "_tarjeta"))

.crm_tamano_modal <- function(tamano) {
  equivalencias <- c(xl = "ventana", l = "subventana1", m = "subventana3", s = "aviso")
  if (tamano %in% names(equivalencias)) unname(equivalencias[[tamano]]) else tamano
}

.crm_caja <- function(id, valor, formato = "numero", texto = "", icono = "info-circle", colores = NULL,
                      color_fondo_hex = NULL, mostrar_boton = TRUE, contenido_modal = NULL, titulo_modal = "",
                      icono_modal = NULL, tamano_modal = "xl", footer = NULL, footer_class = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  output <- session$output
  .valor <- function(x) if (shiny::is.reactive(x)) x() else x
  accion <- isTRUE(mostrar_boton) && !is.null(contenido_modal)
  output[[paste0(id, "_tarjeta")]] <- shiny::renderUI({
    .crm_caja_ui(session$ns(id), texto = .valor(texto), icono = .valor(icono), footer = .valor(footer),
                  mostrar_boton = accion, id_boton = "btn_abrir", tooltip = .valor(titulo_modal),
                  color_fondo = .valor(color_fondo_hex))
  })
  modulo <- racafeModulos::cajaValor_server(id, valor_r = valor, formato = formato,
                                          empty = "—", id_boton = "btn_abrir")
  if (accion) {
    shiny::observeEvent(modulo$click_detalle(), {
      contenido <- if (is.function(contenido_modal)) contenido_modal() else contenido_modal
      shiny::showModal(racafeForms::racafeModalDetail(title = shiny::tagList(
                          if (!is.null(icono_modal)) shiny::icon(icono_modal), .valor(titulo_modal)), contenido,
                        size = .crm_tamano_modal(tamano_modal), easy_close = TRUE,
                        footer = racafeForms::racafeModalFooter()))
    }, ignoreInit = TRUE)
  }
  invisible(modulo)
}

.crm_tabla_vancouver <- function(datos, ...) {
  racafeShiny::racafeTableVancouver(datos, header_color = "#495057", header_width = 1.5, decimals = NULL, ...)
}

.crm_tabla_ui <- function(id, titulo = NULL, subtitulo = NULL, footer = NULL, footer_tipo = "info",
                          sortable = TRUE, mostrar_nota = TRUE, estilo = "minimal") {
  selector <- paste0("#", shiny::NS(id)("tabla"))
  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(selector, " .rt-thead.-header .rt-th {",
                                         "border-top:0!important;border-bottom:1.5px solid #495057!important;}"))),
    if (!is.null(titulo)) shiny::tags$div(class = "rt-titulo", titulo),
    if (!is.null(subtitulo)) shiny::tags$div(class = "rt-subtitulo", subtitulo),
    racafeModulos::TablaReactable2UI(id, estilo = estilo),
    if (!is.null(footer)) racafeForms::racafeFormMessage(footer, type = "info", size = "compact") else {
      shiny::uiOutput(paste0(id, "_nota"))
    }
  )
}

.crm_tabla_servidor <- function(..., footer = NULL, footer_tipo = "info", modal_titulo_fn = NULL, modal_contenido_fn = NULL,
                                modal_pre_fn = NULL,
                                modal_size = "xl", modal_icon = NULL, modal_footer = NULL,
                                modal_footer_tipo = "info") {
  argumentos <- list(...)
  id <- if (!is.null(argumentos$id)) argumentos$id else argumentos[[1]]
  session <- shiny::getDefaultReactiveDomain()
  output <- session$output
  output[[paste0(id, "_nota")]] <- shiny::renderUI({
    nota <- if (shiny::is.reactive(footer)) footer() else footer
    if (is.null(nota)) return(NULL)
    tipo <- if (footer_tipo == "danger") "error" else if (footer_tipo == "warning") "warning" else "info"
    racafeForms::racafeFormMessage(nota, type = tipo, size = "compact")
  })
  tabla <- racafeModulos::TablaReactable2(..., footer = NULL)
  if (!is.null(modal_contenido_fn)) {
    shiny::observeEvent(tabla$seleccion(), {
      seleccion <- tabla$seleccion()
      shiny::req(seleccion)
      if (!is.null(modal_pre_fn)) modal_pre_fn(seleccion)
      titulo <- if (is.function(modal_titulo_fn)) modal_titulo_fn(seleccion) else modal_titulo_fn
      shiny::showModal(racafeForms::racafeModalDetail(title = shiny::tagList(
                          if (!is.null(modal_icon)) shiny::icon(modal_icon), titulo), modal_contenido_fn(seleccion),
                        size = .crm_tamano_modal(modal_size), easy_close = TRUE,
                        footer = racafeForms::racafeModalFooter(
                          left = if (!is.null(modal_footer)) {
                            racafeForms::racafeFormMessage(modal_footer, type = "info", size = "compact")
                          }, right = shiny::modalButton("Cerrar"))))
      tabla$limpiar()
    }, ignoreInit = TRUE)
  }
  tabla
}
