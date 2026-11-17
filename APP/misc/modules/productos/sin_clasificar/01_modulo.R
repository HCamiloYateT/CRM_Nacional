# Completar la categoría y el producto comercial de una referencia ----
ProductosSinClasificarUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = .crm_descarga_ui("plantilla", ns, label = "Descargar plantilla Excel")),
    .crm_disclosure("Clasificar varias referencias con Excel",
      .crm_nota(c("Descargue la plantilla y complete únicamente las columnas Categoria y Producto.",
        "Deje en blanco las referencias que no quiere cambiar. No modifique los códigos.",
        "Suba el archivo, revise los cambios y confirme. Si una fila no es válida, no se guarda ninguna.")),
      racafeForms::racafeInputFile("excel", "Subir plantilla completada", accept = ".xlsx", ns = ns),
      shiny::uiOutput(ns("estado_carga")), .crm_tabla_ui(ns("carga_masiva"))),
    .crm_nota("Use la acción de cada referencia para completar su categoría y producto."),
    .crm_tabla_ui(ns("referencias"), titulo = "Referencias pendientes de clasificación"))
}
ProductosSinClasificar <- function(id, usuario, al_actualizar,
                                  cargar = racafeBD::CargarDatos, guardar = .clas_actualizar_bd, operaciones = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    pendiente <- shiny::reactiveVal(NULL)
    masivo <- shiny::reactiveVal(NULL)
    masivo_confirmado <- shiny::reactiveVal(NULL)
    referencia_elegida <- shiny::reactiveVal(NULL)
    fuente <- shiny::reactive({ revision(); .clas_validar(.pr_cargar("CRMNALPRODS", cargar)) })
    filas <- shiny::reactive({
      x <- .clas_pendientes(fuente(), if (is.null(operaciones)) list() else operaciones())
      x$Accion <- x$Referencia
      x
    })
    output$plantilla <- shiny::downloadHandler(
      filename = function() paste0("productos_sin_clasificar_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- filas()
        for (n in .CLAS_CLAVES) x[[n]] <- .crm_id_texto(x[[n]])
        x$Categoria <- x$Producto <- rep("", nrow(x))
        columnas <- c("Referencia", .CLAS_CLAVES, "Marca", "NombreComercial", "Categoria", "Producto")
        opciones <- dplyr::bind_rows(lapply(unique(x$LinNegCod), function(linea) {
          fila <- x[x$LinNegCod == linea, , drop = FALSE][1, ]
          dplyr::bind_rows(lapply(.clas_opciones(fuente(), fila), function(categoria) {
            productos <- .clas_opciones(fuente(), fila, categoria)
            data.frame(LinNegCod = rep(linea, length(productos)),
              Categoria = rep(categoria, length(productos)), Producto = productos)
          }))
        }))
        if (!ncol(opciones)) opciones <- data.frame(Categoria = character(), Producto = character())
        writexl::write_xlsx(list(Referencias = x[columnas],
          Opciones = unique(opciones[c("Categoria", "Producto")])), file)
      })
    shiny::observeEvent(input$excel, {
      masivo(NULL)
      tryCatch({
        if (tolower(tools::file_ext(input$excel$name)) != "xlsx") stop("Suba un archivo Excel con extensión .xlsx.")
        archivo <- as.data.frame(readxl::read_excel(input$excel$datapath, col_types = "text",
          .name_repair = "check_unique", n_max = 5001L))
        masivo(.clas_validar_excel(archivo, filas(), fuente()))
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })
    output$estado_carga <- .crm_render_ui({
      x <- masivo()
      if (is.null(x)) return(.crm_nota("Suba una plantilla para revisar los cambios antes de guardar."))
      shiny::tagList(.crm_nota(paste(nrow(x), "referencias listas para clasificar.")),
        racafeForms::racafeButtonSave("aplicar_masivo", "Guardar clasificación del Excel", ns = session$ns))
    })
    .crm_vancouver_server("carga_masiva", shiny::reactive(masivo() %||% data.frame()),
      columnas = c("Marca", "NombreComercial", "CategoriaNueva", "ProductoNuevo"),
      col_labels = c(CategoriaNueva = "Nueva categoría", ProductoNuevo = "Nuevo producto"))
    shiny::observeEvent(input$aplicar_masivo, {
      shiny::req(masivo())
      masivo_confirmado(masivo())
      racafeShiny::MostrarModalConfirmacion(ns = session$ns, titulo = "Confirmar clasificación masiva",
        texto = paste("Se actualizarán", nrow(masivo()), "referencias. ¿Desea guardar estos cambios?"),
        id_cancelar = "cancelar_masivo", id_confirmar = "confirmar_masivo", label_confirmar = "Guardar")
    })
    shiny::observeEvent(input$cancelar_masivo, { masivo_confirmado(NULL); shiny::removeModal() })
    shiny::observeEvent(input$confirmar_masivo, {
      x <- masivo_confirmado()
      shiny::req(x)
      masivo_confirmado(NULL); shiny::removeModal()
      tryCatch({
        nuevo <- .clas_guardar_masivo(x, usuario(), cargar)
        al_actualizar(nuevo); .pr_invalidar("CRMNALPRODS")
        revision(revision() + 1L); masivo(NULL); referencia_elegida(NULL); tabla$limpiar()
        shiny::showNotification("Clasificaciones guardadas. Los tableros se actualizaron.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 10))
    })
    tabla <- .crm_vancouver_server("referencias", filas, id_col = "Referencia", modo_seleccion = "fila",
      col_specs = list(Accion = list(name = "Clasificar", html = TRUE, minWidth = 70, cell = function(v, i) {
        accion <- paste0("Shiny.setInputValue(", jsonlite::toJSON(session$ns("clasificar"), auto_unbox = TRUE),
          ",", jsonlite::toJSON(as.character(v), auto_unbox = TRUE), ",{priority: 'event'})")
        shiny::tagAppendAttributes(racafeForms::racafeButton(paste0("clasificar_", i), label = NULL, icon = "pen",
          variant = "secondary", size = "xs", title = "Clasificar referencia", ns = session$ns), onclick = accion)
      })),
      columnas = c("Accion", "NumeroLotes", "MrcCod", "Marca", "NombreComercial", "Presentacion", "Linea",
        "Categoria", "Producto",
        "PrimerPedido", "UltimoPedido", "SacosVendidos", "PrimeraFactura", "UltimaFactura"),
      col_labels = c(MrcCod = "Código de marca", SacosVendidos = "Sacos facturados · 70 kg",
        PrimerPedido = "Primer pedido", UltimoPedido = "Último pedido", PrimeraFactura = "Primera factura",
        UltimaFactura = "Última factura", NumeroLotes = "Lotes distintos",
          NombreComercial = "Nombre comercial", Presentacion = "Presentación / referencia del sistema",
        Linea = "Línea de negocio", Categoria = "Categoría actual", Producto = "Producto actual"))
    shiny::observeEvent(tabla$seleccion(), {
      seleccion <- tabla$seleccion()
      shiny::req(seleccion$id %in% filas()$Referencia)
      referencia_elegida(seleccion$id)
      shiny::showModal(racafeForms::racafeModal(title = "Pedidos de esta referencia", size = "ventana",
        .crm_tabla_ui(session$ns("pedidos")), footer = shiny::modalButton("Cerrar")))
    }, ignoreNULL = TRUE)
    elegida <- shiny::reactive({
      filas()[filas()$Referencia %in% referencia_elegida(), , drop = FALSE]
    })
    shiny::observeEvent(input$clasificar, {
      shiny::req(input$clasificar %in% filas()$Referencia)
      referencia_elegida(input$clasificar)
      pendiente(NULL)
      shiny::showModal(racafeForms::racafeModal(title = "Clasificar referencia", size = "subventana2",
        shiny::uiOutput(session$ns("edicion")), footer = shiny::modalButton("Cerrar")))
    })
    output$edicion <- .crm_render_ui({
      x <- elegida()
      if (nrow(x) != 1L) return(NULL)
      racafeForms::racafePanel(title = paste(x$Marca, x$NombreComercial, sep = " · "),
        .crm_select("categoria", "Categoría", choices = .clas_opciones(fuente(), x), ns = session$ns),
        shiny::uiOutput(session$ns("producto_ui")),
        .crm_disclosure("Pedidos asociados a esta referencia", .crm_tabla_ui(session$ns("pedidos"))),
        racafeForms::racafeButton("guardar", "Guardar clasificación y actualizar tableros",
          icon = "floppy-disk", ns = session$ns))
    })
    output$producto_ui <- .crm_render_ui({
      .crm_select("producto", "Producto comercial", choices = .clas_opciones(fuente(), elegida(), input$categoria),
        ns = session$ns)
    })
    .crm_vancouver_server("pedidos", shiny::reactive({
      shiny::req(nrow(elegida()) == 1L)
      .clas_pedidos_relacionados(if (is.null(operaciones)) list() else operaciones(), elegida()$Referencia)
    }), col_labels = c(CLPdcCod = "Código de pedido", CLPdcLin = "Línea del pedido",
      CantidadSolicitada = "Cantidad solicitada (unidad original)", FechaPedido = "Fecha de creación del pedido",
      SacosFacturados70 = "Sacos facturados (70 kg)"))
    shiny::observeEvent(input$actualizar, {
      tryCatch({
        nuevo <- .crm_snapshot_productos(.clas_validar(cargar("CRMNALPRODS")))
        al_actualizar(nuevo)
        .pr_invalidar("CRMNALPRODS")
        revision(revision() + 1L)
        referencia_elegida(NULL)
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
        .pr_invalidar("CRMNALPRODS")
        revision(revision() + 1L)
        referencia_elegida(NULL)
        tabla$limpiar()
        shiny::showNotification("Clasificación guardada. Los tableros se actualizaron con sus filtros actuales.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8))
    })
  })
}
