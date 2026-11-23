# Acciones de la entidad consultada: misma configuración y formularios que el Kanban.
.in_acciones_disponibles <- function(etapa) {
  acciones <- .acciones_por_etapa(etapa)
  acciones[names(acciones) != "Consultar"]
}
.in_registros_accion <- function(contexto, pipeline) {
  codigos <- .in_codigos(contexto)
  if (!length(codigos)) return(data.frame(CodContacto = character(), PerRazSoc = character(),
    PerCod = character(), Etapa = character()))
  pipeline[as.character(pipeline$CodContacto) %in% codigos, , drop = FALSE]
}
ConsultaAccionesUI <- function(id) {
  shiny::uiOutput(shiny::NS(id)("menu"))
}
ConsultaAcciones <- function(id, contexto, usuario, al_actualizar = function(tablas) NULL,
                             al_volver = function() NULL, al_abrir = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    activo <- shiny::reactiveVal(FALSE)
    codigo <- shiny::reactiveVal(NULL)
    accion <- shiny::reactiveVal(NULL)
    codigo_reactivar <- shiny::reactiveVal(NULL)
    disparo_reactivar <- shiny::reactiveVal(0L)
    registros <- shiny::reactive(.in_registros_accion(contexto(), cargar_pipeline_embudo()))
    elegido <- shiny::reactive({
      x <- registros()
      if (!nrow(x)) return(x)
      seleccion <- input$registro
      if (length(seleccion) != 1L || !seleccion %in% x$CodContacto) seleccion <- x$CodContacto[[1]]
      x[as.character(x$CodContacto) %in% as.character(seleccion), , drop = FALSE]
    })
    cerrar <- function() {
      if (!shiny::isolate(activo())) return(invisible(NULL))
      activo(FALSE)
      shiny::removeModal()
      al_volver()
    }
    shiny::observeEvent(list(contexto()$Clave, input$registro), {
      activo(FALSE)
      codigo_reactivar(NULL)
    }, ignoreInit = TRUE, priority = 30)
    output$menu <- .crm_render_ui({
      x <- registros(); e <- elegido()
      if (!nrow(e)) return(.crm_nota("Esta entidad no tiene un registro CRM asociado para ejecutar acciones."))
      opciones <- .in_acciones_disponibles(as.character(e$Etapa[[1]]))
      if (!length(opciones)) return(NULL)
      menu <- racafeModulos::MenuAccionesUI(as.character(e$CodContacto[[1]]), opciones,
        config = .CONFIG_ACCIONES_EMBUDO, ns = session$ns)
      menu <- htmltools::tagQuery(menu)$find(".rm-action-trigger")$addClass("pr-action-trigger")$allTags()
      menu <- htmltools::tagQuery(menu)$find(".rm-action-panel")$addClass("pr-action-panel")$allTags()
      racafeForms::racafeActionBar(left = if (nrow(x) > 1L) {
        .crm_select("registro", "Registro CRM sobre el que se realizará la acción",
          choices = stats::setNames(as.character(x$CodContacto),
            paste(x$PerRazSoc, x$PerCod, x$Etapa, sep = " · ")),
          selected = as.character(e$CodContacto[[1]]), ns = session$ns)
      }, right = menu)
    })
    registro_ui <- list(Editar = EditarUI, Relacionamiento = RelacionamientoUI,
      Promover = PromoverUI, Descartar = DescartarUI, CrearOportunidad = CrearOportunidadUI)
    shiny::observeEvent(input$AccionSeleccionada, {
      sel <- input$AccionSeleccionada
      e <- elegido()
      if (length(sel$codigo) != 1L || length(sel$accion) != 1L || is.na(sel$accion) ||
          nrow(e) != 1L || !identical(as.character(sel$codigo), as.character(e$CodContacto[[1]])) ||
          !sel$accion %in% names(.in_acciones_disponibles(as.character(e$Etapa[[1]])))) {
        shiny::showNotification("La acción no está disponible para la entidad seleccionada.", type = "warning")
        return()
      }
      vigente <- tryCatch(obtener_etapa_contacto(as.character(sel$codigo)), error = function(e) {
        shiny::showNotification(paste("No se pudo verificar la etapa:", conditionMessage(e)), type = "error")
        NULL
      })
      if (is.null(vigente)) return()
      if (!identical(vigente, as.character(e$Etapa[[1]]))) {
        al_actualizar(c("CRMNALCONTACTO", "CRMNALHISTORIALETAPA"))
        shiny::showNotification("La etapa cambió. La información se actualizó; debe revisarse la acción.",
          type = "warning")
        return()
      }
      al_abrir()
      codigo(as.character(sel$codigo)); accion(sel$accion); activo(TRUE)
      if (identical(sel$accion, "Reactivar")) {
        codigo_reactivar(as.character(sel$codigo))
        disparo_reactivar(shiny::isolate(disparo_reactivar()) + 1L)
        return()
      }
      cfg <- .CONFIG_ACCIONES_EMBUDO[[sel$accion]]
      shiny::showModal(racafeForms::racafeModal(title = cfg$etiqueta, size = cfg$modal %||% "subventana2",
        shiny::tagAppendAttributes(shiny::uiOutput(session$ns("formulario")),
          `data-crm-action-close` = session$ns("CerrarAccion")),
        easy_close = TRUE, footer = shiny::modalButton("Cerrar")))
    })
    shiny::observeEvent(input$CerrarAccion, cerrar(), ignoreInit = TRUE)
    shiny::observeEvent(input[["Reactivar-REA_Confirmar"]], {
      if (identical(accion(), "Reactivar") && isFALSE(input[["Reactivar-REA_Confirmar"]])) cerrar()
    }, ignoreInit = TRUE)
    output$formulario <- .crm_render_ui({
      shiny::req(activo(), accion() %in% names(registro_ui))
      registro_ui[[accion()]](session$ns(accion()))
    })
    codigo_formulario <- function(tipo) shiny::reactive({
      shiny::req(activo(), identical(accion(), tipo), codigo() %in% registros()$CodContacto)
      codigo()
    })
    editar <- Editar("Editar", usuario, codigo_formulario("Editar"))
    gestion <- Relacionamiento("Relacionamiento", usuario, codigo_formulario("Relacionamiento"))
    promover <- Promover("Promover", usuario, codigo_formulario("Promover"))
    descartar <- Descartar("Descartar", usuario, codigo_formulario("Descartar"))
    oportunidad <- CrearOportunidad("CrearOportunidad", usuario, codigo_formulario("CrearOportunidad"))
    reactivar <- Reactivar("Reactivar", usuario, shiny::reactive(codigo_reactivar()),
      disparador = shiny::reactive(disparo_reactivar()))
    shiny::observeEvent(promover$actualizaciones(), {
      tryCatch({
        detectar_conversion_leads(usuario(), codigos = shiny::isolate(codigo()))
        al_actualizar(c("CRMNALCONTACTO", "CRMNALHISTORIALETAPA", "CONTACTOLEAD", "CRMNALLEADCLIENTE",
          "CRMNALPROSPECTOALIANZA", "CRMNALVINCULONIT"))
        cerrar()
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    }, ignoreInit = TRUE)
    cambios <- list(
      list(contador = editar$actualizaciones, tablas = "CRMNALCONTACTO"),
      list(contador = gestion$gestiones, tablas = "CRMNALRELACIONAMIENTO"),
      list(contador = descartar$actualizaciones,
        tablas = c("CRMNALCONTACTO", "CRMNALHISTORIALETAPA", "CRMNALDESCARTE")),
      list(contador = oportunidad$creaciones, tablas = "CRMNALCLOPT"),
      list(contador = reactivar$n, tablas = c("CRMNALCONTACTO", "CRMNALHISTORIALETAPA", "CRMNALDESCARTE")))
    lapply(cambios, function(cambio) {
      force(cambio)
      if (is.function(cambio$contador)) shiny::observeEvent(cambio$contador(), {
        al_actualizar(cambio$tablas)
        cerrar()
      }, ignoreInit = TRUE)
    })
  })
}
