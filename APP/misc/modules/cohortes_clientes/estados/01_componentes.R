# Tarjetas actuales con los valores y semáforos del módulo aprobado ----
.coha_valor <- function(v, formato = "numero", color = "#212529") {
  shiny::tags$span(style = paste0("color:", color, ";"),
    if (is.na(v) || !is.finite(v)) "—" else .coha_formato(formato)(v))
}
.coha_caja <- function(id, valor, formato = "numero", texto = "", icono = "info-circle", colores = NULL,
                       mostrar_boton = TRUE, contenido_modal = NULL, titulo_modal = "", icono_modal = NULL,
                       tamano_modal = "xl", footer = NULL, footer_class = NULL) {
  .crm_caja(id, valor, formato, texto, icono, mostrar_boton = mostrar_boton, contenido_modal = contenido_modal,
    titulo_modal = titulo_modal, tamano_modal = tamano_modal, footer = footer,
    formatear = function(v) if (is.numeric(v)) .coha_formato(formato)(v) else v)
}

# Tablas Vancouver: fila en transiciones y celda en el resumen mensual ----
.coha_tabla <- function(id, data, titulo = NULL, subtitulo = NULL, footer = NULL, footer_tipo = "info",
                        col_specs = NULL, id_col = NULL, modo_seleccion = "ninguno", searchable = TRUE,
                        sortable = TRUE, page_size = 15L, filas_bloqueadas = NULL, modal_titulo_fn = NULL,
                        modal_pre_fn = NULL, modal_contenido_fn = NULL, modal_size = "xl", columnas_permitidas = NULL) {
  .permitida <- function(sel) !is.null(sel) && !identical(as.character(sel$id), "TOTAL")
  if (modo_seleccion != "celda") {
    if (!is.null(modal_contenido_fn)) {
      # Registrar el observador una vez y validar la selección antes de abrir el modal.
      tabla <- .crm_vancouver_server(id, data = data, col_specs = col_specs, id_col = id_col,
        modo_seleccion = modo_seleccion, searchable = searchable, sortable = sortable,
        page_size = page_size, titulo = titulo, subtitulo = subtitulo)
      shiny::observeEvent(tabla$seleccion(), {
        sel <- tabla$seleccion()
        shiny::req(.permitida(sel))
        if (!is.null(modal_pre_fn)) modal_pre_fn(sel)
        shiny::showModal(racafeForms::racafeModalDetail(title = modal_titulo_fn(sel),
          modal_contenido_fn(sel), size = .crm_tamano_modal(modal_size), easy_close = TRUE,
          footer = racafeForms::racafeModalFooter()))
        tabla$limpiar()
      }, ignoreInit = TRUE)
      session <- shiny::getDefaultReactiveDomain()
      session$output[[paste0(id, "_nota")]] <- .crm_render_ui({
        .crm_nota(if (shiny::is.reactive(footer)) footer() else footer)
      })
      return(invisible(tabla))
    }
    return(.crm_tabla_servidor(id, data = data, titulo = titulo, subtitulo = subtitulo, footer = footer,
      footer_tipo = footer_tipo, col_specs = col_specs, id_col = id_col, modo_seleccion = modo_seleccion,
      searchable = searchable, sortable = sortable, page_size = page_size))
  }
  racafeModulos::TablaVancouverCeldas(id, data, titulo = titulo, subtitulo = subtitulo, footer = footer,
    col_specs = col_specs, id_col = id_col, modal_titulo_fn = modal_titulo_fn,
    modal_pre_fn = modal_pre_fn, modal_contenido_fn = modal_contenido_fn,
    modal_size = .crm_tamano_modal(modal_size), datos_fn = .crm_tabla_datos,
    nota_fn = .crm_nota, seleccion_permitida = function(sel) .permitida(sel) &&
      (is.null(columnas_permitidas) || sel$col %in% columnas_permitidas))
}

