# Tarjetas actuales con los valores y semáforos del módulo aprobado ----
.coha_valor <- function(v, formato = "numero", color = "#212529") {
  shiny::tags$span(style = paste0("color:", color, ";"),
    if (is.na(v) || !is.finite(v)) "—" else .coha_formato(formato)(v))
}
.coha_caja <- function(id, valor, formato = "numero", texto = "", icono = "info-circle", colores = NULL,
                       mostrar_boton = TRUE, contenido_modal = NULL, titulo_modal = "", icono_modal = NULL,
                       tamano_modal = "xl", footer = NULL, footer_class = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  .val <- function(x) if (shiny::is.reactive(x)) x() else x
  accion <- isTRUE(mostrar_boton) && !is.null(contenido_modal)
  session$output[[paste0(id, "_tarjeta")]] <- .crm_render_ui({
    .crm_caja_ui(session$ns(id), texto = .val(texto), icono = .val(icono), footer = .val(footer),
      mostrar_boton = accion, id_boton = "btn_abrir", tooltip = .val(titulo_modal))
  })
  shiny::moduleServer(id, function(input, output, session) {
    output$valor_display <- .crm_render_ui({
      v <- .val(valor)
      if (is.numeric(v)) .coha_formato(formato)(v) else v
    })
    shiny::observeEvent(input$btn_abrir, {
      shiny::req(accion)
      shiny::showModal(racafeForms::racafeModalDetail(title = .val(titulo_modal), contenido_modal(),
        size = .crm_tamano_modal(tamano_modal), easy_close = TRUE, footer = racafeForms::racafeModalFooter()))
    }, ignoreInit = TRUE)
  })
}

# Tablas Vancouver: fila en transiciones y celda en el resumen mensual ----
.coha_tabla <- function(id, data, titulo = NULL, subtitulo = NULL, footer = NULL, footer_tipo = "info",
                        col_specs = NULL, id_col = NULL, modo_seleccion = "ninguno", searchable = TRUE,
                        sortable = TRUE, page_size = 15L, filas_bloqueadas = NULL, modal_titulo_fn = NULL,
                        modal_pre_fn = NULL, modal_contenido_fn = NULL, modal_size = "xl") {
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
  shiny::moduleServer(id, function(input, output, session) {
    .val <- function(x) if (shiny::is.reactive(x)) x() else x
    output$tabla <- .crm_render_ui({
      datos <- .val(data)
      if (!nrow(datos)) return(.crm_nota("Sin registros para los filtros seleccionados."))
      tabla <- .crm_tabla_datos(datos, col_specs = col_specs)
      for (col in setdiff(names(datos), id_col)) {
        botones <- vapply(seq_len(nrow(datos)), function(i) {
          valor <- as.character(datos[[col]][i])
          if (identical(as.character(datos[[id_col]][i]), "TOTAL")) return(valor)
          evento <- paste0("Shiny.setInputValue(", jsonlite::toJSON(session$ns("celda"), auto_unbox = TRUE),
            ",", jsonlite::toJSON(list(id = as.character(datos[[id_col]][i]), col = col), auto_unbox = TRUE),
            ",{priority:'event'});")
          as.character(shiny::tags$button(type = "button", class = "btn btn-link crm-tabla-enlace",
            style = "padding:0;font:inherit;color:inherit;", onclick = evento,
            `aria-label` = paste(datos[[id_col]][i], col, valor), valor))
        }, character(1))
        tabla <- gt::text_transform(tabla, locations = gt::cells_body(columns = tidyselect::all_of(col)),
          fn = local({ texto <- botones; function(x) texto }))
      }
      shiny::tagList(if (!is.null(titulo)) shiny::tags$strong(.val(titulo)),
        if (!is.null(subtitulo)) shiny::tags$div(.val(subtitulo)), tabla, .crm_nota(.val(footer)))
    })
    shiny::observeEvent(input$celda, {
      datos <- .val(data)
      sel <- input$celda
      shiny::req(is.list(sel), is.character(sel$id), length(sel$id) == 1L,
        is.character(sel$col), length(sel$col) == 1L, .permitida(sel),
        sel$id %in% as.character(datos[[id_col]]), sel$col %in% setdiff(names(datos), id_col))
      if (!is.null(modal_pre_fn)) modal_pre_fn(sel)
      shiny::showModal(racafeForms::racafeModalDetail(title = modal_titulo_fn(sel), modal_contenido_fn(sel),
        size = .crm_tamano_modal(modal_size), easy_close = TRUE, footer = racafeForms::racafeModalFooter()))
    }, ignoreInit = TRUE)
  })
}

