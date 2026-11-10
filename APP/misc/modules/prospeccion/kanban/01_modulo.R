# Alcance del controlbar; los filtros locales solo pueden reducirlo ----
.kb_alcance <- function(datos, filtros, usuario, admin = FALSE) {
  x <- datos
  x$Asesor <- as.character(.gc_columna(x, c("Asesor", "Asesor.y", "Asesor.x")))
  x$Linea <- as.character(.gc_columna(x, c("LinNegocio", "CLLinNegNo", "Linea")))
  x$Segmento <- as.character(.gc_columna(x, "Segmento"))
  x <- .op_contactos_visibles(x, usuario, admin)
  for (par in list(c("Asesor", "asesor"), c("Linea", "linneg"), c("Segmento", "segmento"))) {
    valor <- x[[par[[1]]]]
    permitido <- filtros[[par[[2]]]]
    x <- x[length(permitido) > 0L & (is.na(valor) | !nzchar(valor) | valor %in% permitido), , drop = FALSE]
  }
  x
}

# Tablero con salidas independientes por etapa ----
KanbanEmbudoUI <- function(id) {
  ns <- NS(id)
  columnas <- lapply(ETAPAS_EMBUDO, function(etapa) {
    contenido <- tagList(
      racafeForms::racafeInputText(paste0("buscar_", etapa), paste("Buscar en", etapa), ns = ns),
      uiOutput(ns(paste0("col_", etapa))))
    div(class = "kpb-slot", `data-kb-stage` = etapa,
      if (etapa == "DESCARTADO") .crm_disclosure("DESCARTADOS", contenido) else contenido)
  })
  racafeForms::racafeForm(variant = "plain", racafeModulos::MenuAccionesDependencies(), useShinyjs(),
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("actualizar", "Actualizar",
      icon = "arrows-rotate", variant = "secondary", ns = ns)),
    racafeModulos::TableroKanbanUI(id = "kpb_board", ns = ns, columnas))
}
KanbanEmbudo <- function(id, usuario, filtros, es_admin = function() FALSE,
                         hechos = function() data.frame(), al_consultar = function(codigo, nit) NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0L)
    codigo_seleccionado <- reactiveVal(NULL)
    accion_seleccionada <- reactiveVal(NULL)
    titulo_modal_actual <- reactiveVal(NULL)
    destino_arrastre <- reactiveVal(NULL)
    pipeline_raw <- reactive({
      refresh_trigger()
      cargar_pipeline_embudo()
    })
    # Filtrar o buscar no vuelve a consultar tablas ni ejecutar conversiones.
    pipeline_filtrado <- reactive(.kb_facturacion(pipeline_raw(), hechos(), filtros(), usuario(), es_admin()))
    observeEvent(input$actualizar, {
      tryCatch({
        detectar_conversion_leads(usuario(), codigos = isolate(pipeline_filtrado()$CodContacto))
        refresh_trigger(isolate(refresh_trigger()) + 1L)
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    for (etapa in ETAPAS_EMBUDO) local({
      clave <- etapa
      limite <- reactiveVal(50L)
      alerta <- reactiveVal(FALSE)
      busqueda <- debounce(reactive(input[[paste0("buscar_", clave)]]), 250)
      filas_base <- reactive({ x <- pipeline_filtrado(); x[x$Etapa == clave, , drop = FALSE] })
      filas <- reactive({
        x <- .kb_buscar(filas_base(), busqueda())
        if (isTRUE(alerta())) x <- x[x$EstadoGestion %in% "critical", , drop = FALSE]
        x
      })
      observeEvent(list(busqueda(), filas_base()), limite(50L), ignoreInit = TRUE)
      observeEvent(input[[paste0("mas_", clave)]], limite(limite() + 50L))
      observeEvent(input[[paste0("alerta_", clave)]], { alerta(!alerta()); limite(50L) })
      output[[paste0("col_", clave)]] <- .crm_render_ui({
        x <- filas()
        visibles <- head(x, limite())
        cuerpo <- if (!nrow(x)) .crm_nota("Sin registros para esta columna y los filtros actuales.") else {
          tagList(lapply(seq_len(nrow(visibles)), function(i) .render_tarjeta_embudo(visibles[i, ], ns)),
            if (nrow(x) > nrow(visibles)) racafeForms::racafeButton(paste0("mas_", clave),
              paste("Mostrar 50 más ·", nrow(visibles), "de", nrow(x)), variant = "secondary", size = "xs", ns = ns))
        }
        badge <- if (clave %in% c("CONTACTO", "LEAD")) actionLink(ns(paste0("alerta_", clave)),
          paste(if (alerta()) "Ver todas" else "Sin gestión:", sum(filas_base()$EstadoGestion %in% "critical")))
        racafeModulos::ColumnaKanbanUI(clave, cuerpo, cantidad = nrow(x), color = COLOR_ETAPA_EMBUDO[[clave]],
          badges = badge, acciones = if (clave == "CONTACTO") racafeForms::racafeButton("btn_nuevo_contacto",
            "Añadir", icon = "plus", size = "xs", ns = ns))
      })
    })

    # Despacho de acciones — mismo patron que PanelEtapa.R: una unica
    # tabla de registro + una unica instancia de cada modulo generico,
    # en vez de 10 reactiveVal y una cadena if/else por accion.
    # Reactivar queda FUERA de esta tabla: su UI es vacia (ReactivarUI) y
    # el modulo abre su propio modal de confirmacion al cambiar
    # `disparador()` — envolverlo en el modal generico duplicaria dialogo
    .REGISTRO_MODULOS_KANBAN <- list(
      Editar           = list(ui = function() EditarUI(ns("Editar"))),
      Relacionamiento  = list(ui = function() RelacionamientoUI(ns("Relacionamiento"))),
      Promover         = list(ui = function() PromoverUI(ns("Promover"))),
      Descartar        = list(ui = function() DescartarUI(ns("Descartar"))),
      CrearOportunidad = list(ui = function() CrearOportunidadUI(ns("CrearOportunidad")))
    )
    
    trigger_refresco_kanban <- function() refresh_trigger(isolate(refresh_trigger()) + 1)
    
    # codigo/disparador de Reactivar, deliberadamente AISLADOS de
    # codigo_seleccionado(): este ultimo cambia con cualquier accion
    # (Editar, Promover, etc.), y Reactivar dispara su modal apenas
    # `disparador()` cambia — compartir la misma reactiveVal abriria el
    # modal de reactivacion al hacer click en cualquier boton de la tarjeta
    codigo_reactivar   <- reactiveVal(NULL)
    trigger_reactivar  <- reactiveVal(0)
    
    abrir_accion <- function(seleccion) {
      req(seleccion$codigo, seleccion$accion)
      x <- isolate(pipeline_filtrado())
      x <- x[as.character(x$CodContacto) %in% as.character(seleccion$codigo), , drop = FALSE]
      if (nrow(x) != 1L || !seleccion$accion %in% .acciones_por_etapa_kanban(x$Etapa)) {
        showNotification("La acción no está permitida para esta entidad o su alcance.", type = "warning")
        return(invisible(NULL))
      }
      if (seleccion$accion == "Consultar") return(al_consultar(as.character(x$CodContacto), as.character(x$PerCod)))
      vigente <- obtener_etapa_contacto(as.character(x$CodContacto))
      if (!identical(vigente, as.character(x$Etapa))) {
        trigger_refresco_kanban()
        showNotification("La etapa cambió. El tablero se actualizó; revise la entidad.", type = "warning")
        return(invisible(NULL))
      }
      if (!is.null(seleccion$destino) && !seleccion$destino %in% .kb_destinos(vigente, x$EtapaPreDescarte)) {
        showNotification("Esa transición no está permitida.", type = "warning")
        return(invisible(NULL))
      }
      if (seleccion$accion == "Reactivar") {
        codigo_reactivar(seleccion$codigo)
        trigger_reactivar(isolate(trigger_reactivar()) + 1L)
        return(invisible(NULL))
      }
      destino_arrastre(seleccion$destino)
      codigo_seleccionado(seleccion$codigo)
      accion_seleccionada(seleccion$accion)
      titulo_modal_actual(.CONFIG_ACCIONES_EMBUDO[[seleccion$accion]]$etiqueta)
      showModal(racafeForms::racafeModal(
        size = .CONFIG_ACCIONES_EMBUDO[[seleccion$accion]]$modal %||% "subventana2",
        title = titulo_modal_actual(), uiOutput(ns("ModalContenido")), easy_close = TRUE,
        footer = modalButton("Cerrar")))
    }
    observeEvent(input$AccionSeleccionada, abrir_accion(input$AccionSeleccionada))
    observeEvent(input$MoverTarjeta, {
      movimiento <- input$MoverTarjeta
      req(movimiento$codigo, movimiento$desde, movimiento$hacia)
      x <- isolate(pipeline_filtrado())
      x <- x[as.character(x$CodContacto) %in% as.character(movimiento$codigo), , drop = FALSE]
      if (nrow(x) != 1L || x$Etapa != movimiento$desde ||
          !movimiento$hacia %in% .kb_destinos(x$Etapa, x$EtapaPreDescarte)) {
        showNotification("Movimiento no permitido o tarjeta desactualizada.", type = "warning")
        return(invisible(NULL))
      }
      accion <- if (movimiento$hacia == "DESCARTADO") "Descartar" else {
        if (movimiento$desde == "DESCARTADO") "Reactivar" else "Promover"
      }
      abrir_accion(list(codigo = movimiento$codigo, accion = accion, destino = movimiento$hacia))
    })
    output$ModalContenido <- .crm_render_ui({
      req(accion_seleccionada())
      modulo <- .REGISTRO_MODULOS_KANBAN[[accion_seleccionada()]]
      req(modulo)
      modulo$ui()
    })
    outputOptions(output, "ModalContenido", suspendWhenHidden = FALSE)
    
    # Instanciacion unica de cada submodulo genérico
    modulo_editar      <- Editar("Editar", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    modulo_relacion    <- Relacionamiento("Relacionamiento", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    modulo_promover    <- Promover("Promover", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()),
      destino_inicial = reactive(destino_arrastre()))
    modulo_descartar   <- Descartar("Descartar", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    modulo_oportunidad <- CrearOportunidad("CrearOportunidad", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    # Reactivar recibe su propio par codigo/disparador — nunca el
    # codigo_seleccionado compartido (ver nota arriba)
    modulo_reactivar   <- Reactivar("Reactivar", usuario = usuario, codigo_contacto = reactive(codigo_reactivar()),
                                    disparador = reactive(trigger_reactivar()))
    
    observeEvent(modulo_promover$actualizaciones(), {
      tryCatch({
        detectar_conversion_leads(usuario(), codigos = isolate(pipeline_filtrado()$CodContacto))
        removeModal()
        trigger_refresco_kanban()
      },
        error = function(e) showNotification(conditionMessage(e), type = "error"))
    }, ignoreInit = TRUE)
    contadores_retorno <- list(
      modulo_editar$actualizaciones, modulo_descartar$actualizaciones,
      modulo_oportunidad$creaciones, modulo_relacion$gestiones, modulo_reactivar$n
    )
    lapply(contadores_retorno, function(contador) {
      if (is.function(contador)) {
        observeEvent(contador(), { removeModal(); trigger_refresco_kanban() }, ignoreInit = TRUE)
      }
    })
    
    # Alta de contacto ----
    contacto_mod <- CrearContacto(id = "mod_nuevo_contacto", usuario = usuario)
    observeEvent(input$btn_nuevo_contacto, {
      showModal(racafeForms::racafeModal(title = "Nuevo Contacto",
        size = "subventana1",
        easy_close = TRUE,
        footer = modalButton("Cerrar"),
        CrearContactoUI(ns("mod_nuevo_contacto"))))
    })
    observeEvent(contacto_mod$n(), { removeModal(); trigger_refresco_kanban() }, ignoreInit = TRUE)
    
    invisible(NULL)
  })
}
