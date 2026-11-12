# Completar la categoría y el producto comercial de una referencia ----
ProductosSinClasificarUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = NULL),
    .crm_nota("Seleccione una referencia y complete su categoría y producto para incluirla en los análisis."),
    .crm_tabla_ui(ns("referencias"), titulo = "Referencias pendientes de clasificación"),
    shiny::uiOutput(ns("edicion")))
}
ProductosSinClasificar <- function(id, usuario, al_actualizar,
                                  cargar = racafeBD::CargarDatos, guardar = .clas_actualizar_bd, operaciones = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    pendiente <- shiny::reactiveVal(NULL)
    fuente <- shiny::reactive({ .pr_fuentes()$revision(); revision(); .clas_validar(cargar("CRMNALPRODS")) })
    filas <- shiny::reactive(.clas_pendientes(fuente(), if (is.null(operaciones)) list() else operaciones()))
    tabla <- .crm_vancouver_server("referencias", filas, id_col = "Referencia", modo_seleccion = "fila",
      columnas = c("NumeroLotes", "Marca", "NombreComercial", "Presentacion", "Linea", "Categoria", "Producto"),
      col_labels = c(NumeroLotes = "Lotes distintos", NombreComercial = "Nombre comercial", Presentacion = "Presentación / referencia del sistema",
        Linea = "Línea de negocio", Categoria = "Categoría actual", Producto = "Producto actual"))
    elegida <- shiny::reactive({
      seleccion <- tabla$seleccion()
      if (is.null(seleccion)) return(filas()[0, , drop = FALSE])
      filas()[filas()$Referencia %in% seleccion$id, , drop = FALSE]
    })
    output$edicion <- .crm_render_ui({
      x <- elegida()
      if (nrow(x) != 1L) return(NULL)
      racafeForms::racafePanel(title = paste(x$Marca, x$NombreComercial, sep = " · "),
        .crm_select("categoria", "Categoría", choices = .clas_opciones(fuente(), x), ns = session$ns),
        shiny::uiOutput(session$ns("producto_ui")),
        racafeForms::racafeButton("guardar", "Guardar clasificación y actualizar tableros",
          icon = "floppy-disk", ns = session$ns))
    })
    output$producto_ui <- .crm_render_ui({
      .crm_select("producto", "Producto comercial", choices = .clas_opciones(fuente(), elegida(), input$categoria),
        ns = session$ns)
    })
    shiny::observeEvent(input$actualizar, {
      tryCatch({
        nuevo <- .crm_snapshot_productos(.clas_validar(cargar("CRMNALPRODS")))
        al_actualizar(nuevo)
        revision(revision() + 1L)
        tabla$limpiar()
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })
    shiny::observeEvent(input$guardar, {
      x <- elegida()
      if (nrow(x) != 1L || length(input$categoria) != 1L || length(input$producto) != 1L) {
        shiny::showNotification("Seleccione la referencia, su categoría y su producto.", type = "warning")
        return()
      }
      pendiente(list(fila = x, categoria = input$categoria, producto = input$producto))
      racafeShiny::MostrarModalConfirmacion(ns = session$ns, titulo = "Guardar clasificación del producto",
        texto = paste(x$Marca, x$NombreComercial, "→", input$categoria, "/", input$producto),
        id_cancelar = "cancelar", id_confirmar = "confirmar", label_confirmar = "Guardar",
        icono_confirmar = "floppy-disk", color_confirmar = "#7A2028")
    })
    shiny::observeEvent(input$cancelar, { pendiente(NULL); shiny::removeModal() })
    shiny::observeEvent(input$confirmar, {
      x <- pendiente()
      shiny::req(x)
      pendiente(NULL)
      shiny::removeModal()
      tryCatch({
        nuevo <- .clas_guardar(x$fila, x$categoria, x$producto, usuario(), cargar, guardar)
        al_actualizar(nuevo)
        revision(revision() + 1L)
        tabla$limpiar()
        shiny::showNotification("Clasificación guardada. Los tableros se actualizaron con sus filtros actuales.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8))
    })
  })
}
