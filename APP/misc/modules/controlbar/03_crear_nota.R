CrearNotasUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafePanel(
    title = NULL, padding = "compact", bordered = FALSE,
    racafeForms::racafeForm(size = "compact", variant = "plain",
                            racafeForms::racafeFormGroup(
                              racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                .crm_select("NOT_Tipo", label = "Tipo", required = TRUE,
                                                               choices = Choices()$tipo_nota,
                                                               selected = intersect("text", unname(Choices()$tipo_nota)), multiple = FALSE, empty = FALSE,
                                                               search = FALSE, size = 2, ns = ns),
                                racafeForms::racafeInputText("NOT_Titulo", label = "Título", required = TRUE, ns = ns)
                              )
                            ),
                            racafeForms::racafeFormGroup(
                              racafeForms::racafeInputTextArea("NOT_Descripcion", label = "Descripción", value = "",
                                                               placeholder = "Ingrese el detalle de la nota o tarea", rows = 4, ns = ns)
                            ),
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'list'", ns("NOT_Tipo")),
                              racafeForms::racafeFormGroup(
                                racafeForms::racafeInputDate("NOT_FechaFin", label = "Fecha de Cumplimiento", required = TRUE,
                                                             value = Sys.Date() + 1, min = Sys.Date(), ns = ns),
                                racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                  .crm_select("NOT_Prioridad", label = "Prioridad", required = TRUE,
                                                                 choices = stats::setNames(.TSK_PRIORIDADES, toupper(.TSK_PRIORIDADES)), selected = "MEDIA", multiple = FALSE,
                                                                 empty = FALSE, size = 4, ns = ns),
                                  .crm_select("NOT_Responsable", label = "Responsable", required = TRUE,
                                                                 choices = stats::setNames(Choices()$personas, toupper(Choices()$personas)), selected = NULL, multiple = FALSE,
                                                                 empty = TRUE, size = 10, ns = ns)
                                )
                              )
                            ),
                            racafeForms::racafeFormGroup(
                              shiny::tags$div(class = "nota-etiquetas",
                                              racafeModulos::ListaDesplegableAgregableUI(ns("NOT_Tags")))
                            ),
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'list'", ns("NOT_Tipo")),
                              racafeForms::racafeFormGroup(
                                racafeForms::racafeInputSwitch("NOT_CheckList", "Incluir lista de actividades", value = FALSE,
                                                               variant = "primary", ns = ns),
                                conditionalPanel(
                                  condition = sprintf("input['%s']", ns("NOT_CheckList")),
                                  tags$div(class = "nota-actividades-contenedor",
                                           tags$div(class = "nota-actividades-header",
                                                    racafeForms::racafeButton("NOT_Agregar", "Añadir", icon = "plus",
                                                                              variant = "primary", size = "xxs", ns = ns)),
                                           tags$div(id = ns("NOT_Actividades"), .nota_actividad_ui(ns = ns, numero = 1L)))
                                )
                              )
                            ),
                            racafeForms::racafeFormFooter(
                              racafeForms::racafeButton("NOT_Crear", "Crear Nota", icon = "note-sticky",
                                                        variant = "primary", size = "xs", ns = ns), align = "right"
                            )
    )
  )
}
CrearNotas <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactivos ----
    catalogos_iniciados_rv <- reactiveVal(FALSE)
    contador_actividades_rv <- reactiveVal(1L)
    actualizaciones_rv <- reactiveVal(0L)
    actividades_cumplidas_rv <- reactiveValues()
    actividades_activas_rv <- reactiveVal(1L)
    guardando_rv <- reactiveVal(FALSE)
    confirmacion_rv <- reactiveVal(FALSE)
    
    catalogos_r <- ChoicesReactivos()
    observeEvent(catalogos_r(), {
      opciones <- catalogos_r()$tipo_nota
      actual <- shiny::isolate(input$NOT_Tipo)
      if (!catalogos_iniciados_rv() && is.null(actual)) actual <- "text"
      .crm_update_select(session, "NOT_Tipo", choices = opciones, selected = intersect(actual, unname(opciones)))
      catalogos_iniciados_rv(TRUE)
    })

    etiquetas_modulo <- racafeModulos::ListaDesplegableAgregable(
      id = "NOT_Tags", label = "Etiquetas", choices = .nota_obtener_etiquetas(), selected = character(0),
      multiple = TRUE, fem = TRUE
    )
    
    # Funciones ----
    .estado_actividad <- function(indice) {
      isTRUE(actividades_cumplidas_rv[[as.character(indice)]])
    }
    
    .render_boton_cumplir <- function(indice) {
      output_id <- paste0("NOT_BotonCumplir_", indice)
      
      output[[output_id]] <- renderUI({
        cumplida <- .estado_actividad(indice)
        
        racafeForms::racafeButton(
          paste0("NOT_Cumplir_", indice), label = NULL,
          icon = if (cumplida) "rotate-left" else "check", variant = "primary", size = "xxs",
          ns = ns, title = if (cumplida) "Marcar como pendiente" else "Marcar como cumplida"
        )
      })
      
      outputOptions(output, output_id, suspendWhenHidden = FALSE)
    }
    
    .actualizar_estado_visual <- function(indice) {
      id_actividad <- ns(paste0("NOT_Actividad_", indice))
      
      if (.estado_actividad(indice)) {
        shinyjs::addClass(id = id_actividad, class = "nota-actividad-cumplida")
      } else {
        shinyjs::removeClass(id = id_actividad, class = "nota-actividad-cumplida")
      }
      
      .render_boton_cumplir(indice)
    }
    
    .alternar_actividad <- function(indice) {
      clave <- as.character(indice)
      
      actividades_cumplidas_rv[[clave]] <- !isTRUE(actividades_cumplidas_rv[[clave]])
      
      .actualizar_estado_visual(indice)
    }
    
    .limpiar_actividad <- function(indice) {
      updateTextInput(session = session, inputId = paste0("NOT_Texto_", indice), value = "")
      
      actividades_cumplidas_rv[[as.character(indice)]] <- FALSE
      
      .actualizar_estado_visual(indice)
    }
    
    .registrar_observers_actividad <- function(indice) {
      force(indice)
      
      observeEvent(input[[paste0("NOT_Cumplir_", indice)]], {
        .alternar_actividad(indice)
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("NOT_Eliminar_", indice)]], {
        if (indice == 1L) {
          .limpiar_actividad(indice)
          
          return(invisible(NULL))
        }
        
        removeUI(selector = paste0("#", ns(paste0("NOT_Actividad_", indice))), immediate = TRUE)
        
        actividades_cumplidas_rv[[as.character(indice)]] <- NULL
        actividades_activas_rv(setdiff(isolate(actividades_activas_rv()), indice))
      }, ignoreInit = TRUE)
    }
    
    .obtener_actividades <- function() {
      if (!identical(input$NOT_Tipo, "list") || !isTRUE(input$NOT_CheckList)) {
        return(list(actividades = character(0), cumplidas = character(0)))
      }
      
      actividades <- character(0)
      cumplidas <- character(0)
      
      for (i in isolate(actividades_activas_rv())) {
        texto <- input[[paste0("NOT_Texto_", i)]]
        
        if (!is.null(texto) && nzchar(trimws(texto))) {
          actividades <- c(actividades, trimws(texto))
          
          cumplidas <- c(cumplidas, ifelse(.estado_actividad(i), "1", "0"))
        }
      }
      
      list(actividades = actividades, cumplidas = cumplidas)
    }
    
    .validar_formulario <- function() {
      errores <- character(0)
      
      if (length(input$NOT_Tipo) != 1L || !input$NOT_Tipo %in% unname(catalogos_r()$tipo_nota)) {
        errores <- c(errores, "Debe seleccionar el tipo de nota.")
      }
      
      if (is.null(input$NOT_Titulo) || !nzchar(trimws(input$NOT_Titulo))) {
        errores <- c(errores, "El título es obligatorio.")
      }
      
      if (identical(input$NOT_Tipo, "list")) {
        if (is.null(input$NOT_FechaFin)) {
          errores <- c(errores, "La fecha de cumplimiento es obligatoria.")
        }
        
        if (!is.null(input$NOT_FechaFin) && as.Date(input$NOT_FechaFin) < Sys.Date()) {
          errores <- c(errores, "La fecha de cumplimiento no puede ser anterior a hoy.")
        }
        
        if (is.null(input$NOT_Prioridad) || !nzchar(input$NOT_Prioridad)) {
          errores <- c(errores, "La prioridad es obligatoria.")
        }
        
        if (is.null(input$NOT_Responsable) || !nzchar(input$NOT_Responsable)) {
          errores <- c(errores, "El responsable es obligatorio para una tarea.")
        }
      }
      
      errores
    }
    
    .limpiar_actividades <- function() {
      numero_actividades <- isolate(contador_actividades_rv())
      
      for (i in seq_len(numero_actividades)) {
        if (i == 1L) {
          .limpiar_actividad(i)
        } else {
          removeUI(selector = paste0("#", ns(paste0("NOT_Actividad_", i))), immediate = TRUE)
          
          actividades_cumplidas_rv[[as.character(i)]] <- NULL
        }
      }
      
      contador_actividades_rv(1L)
      actividades_activas_rv(1L)
    }
    
    .limpiar_formulario <- function() {
      .crm_update_select(session, "NOT_Tipo", selected = intersect("text", unname(catalogos_r()$tipo_nota)))
      
      updateTextInput(session = session, inputId = "NOT_Titulo", value = "")
      
      updateTextAreaInput(session = session, inputId = "NOT_Descripcion", value = "")
      
      racafeForms::racafeUpdateDate(session = session, id = "NOT_FechaFin", value = Sys.Date() + 1)
      
      .crm_update_select(session, "NOT_Prioridad", selected = "MEDIA")
      
      .crm_update_select(session, "NOT_Responsable", selected = usuario())
      
      racafeForms::racafeUpdateSwitch(session, "NOT_CheckList", value = FALSE)
      
      .limpiar_actividades()
    }
    
    # Formulario ----
    actividades_cumplidas_rv[["1"]] <- FALSE
    
    .render_boton_cumplir(1L)
    
    .registrar_observers_actividad(1L)
    
    observeEvent(usuario(), {
      req(usuario())
      
      .crm_update_select(session, "NOT_Responsable", selected = usuario())
    }, ignoreNULL = FALSE)
    
    observeEvent(input$NOT_Agregar, {
      numero <- isolate(contador_actividades_rv()) + 1L
      
      contador_actividades_rv(numero)
      actividades_activas_rv(c(isolate(actividades_activas_rv()), numero))
      
      actividades_cumplidas_rv[[as.character(numero)]] <- FALSE
      
      insertUI(selector = paste0("#", ns("NOT_Actividades")), where = "beforeEnd",
               ui = .nota_actividad_ui(ns = ns, numero = numero))
      
      .render_boton_cumplir(numero)
      
      .registrar_observers_actividad(numero)
    })
    
    .resumen_confirmacion_ui <- function() {
      es_tarea <- identical(input$NOT_Tipo, "list")
      descripcion <- trimws(input$NOT_Descripcion %||% "")
      etiquetas <- etiquetas_modulo$seleccion()
      etiquetas <- etiquetas[!is.na(etiquetas) & nzchar(etiquetas)]
      actividades <- .obtener_actividades()
      
      detalle <- list(
        racafeForms::racafeInfoItem("TIPO", if (es_tarea) "TAREA" else "NOTA DE REUNIÓN"),
        racafeForms::racafeInfoItem("TÍTULO", trimws(input$NOT_Titulo)),
        racafeForms::racafeInfoItem("DESCRIPCIÓN", if (nzchar(descripcion)) descripcion else "Sin descripción"),
        racafeForms::racafeInfoItem("ETIQUETAS", if (length(etiquetas)) paste(etiquetas, collapse = ", ") else "Sin etiquetas")
      )
      
      if (es_tarea) {
        detalle <- c(detalle, list(
          racafeForms::racafeInfoItem("FECHA DE CUMPLIMIENTO", format(as.Date(input$NOT_FechaFin), "%d/%m/%Y")),
          racafeForms::racafeInfoItem("PRIORIDAD", toupper(input$NOT_Prioridad)),
          racafeForms::racafeInfoItem("RESPONSABLE", toupper(input$NOT_Responsable))
        ))
      }
      
      lista_actividades <- if (es_tarea && isTRUE(input$NOT_CheckList) &&
                               length(actividades$actividades)) {
        shiny::tags$div(
          shiny::tags$strong("ACTIVIDADES"),
          shiny::tags$ul(lapply(seq_along(actividades$actividades), function(i) {
            estado <- if (identical(actividades$cumplidas[[i]], "1")) "Completada" else "Pendiente"
            shiny::tags$li(paste0(actividades$actividades[[i]], " (", estado, ")"))
          }))
        )
      } else NULL
      
      shiny::tagList(
        shiny::tags$p("Revise la información antes de crear el registro."),
        do.call(racafeForms::racafeInfoGrid, c(detalle, list(columns = 2))),
        lista_actividades
      )
    }
    
    # Crear Nota ----
    observeEvent(input$NOT_Crear, {
      if (isTRUE(guardando_rv())) return(invisible(NULL))
      confirmacion_rv(FALSE)
      formulario <- list(
        NOT_Tipo = list(label = "Tipo", required = TRUE, kind = "select"),
        NOT_Titulo = list(label = "Título", required = TRUE, kind = "text"),
        NOT_FechaFin = list(label = "Fecha de Cumplimiento", required = FALSE, kind = "date"),
        NOT_Responsable = list(label = "Responsable", required = FALSE, kind = "select"),
        NOT_Prioridad = list(label = "Prioridad", required = FALSE, kind = "select")
      )
      validacion <- racafeForms::racafeValidateForm(
        input, formulario, ns = session$ns,
        required = list(
          NOT_FechaFin = function(values) identical(values$NOT_Tipo, "list"),
          NOT_Responsable = function(values) identical(values$NOT_Tipo, "list"),
          NOT_Prioridad = function(values) identical(values$NOT_Tipo, "list")
        ),
        rules = list(NOT_FechaFin = function(value, values) {
          if (as.Date(value) >= Sys.Date()) TRUE else "La fecha no puede ser anterior a hoy."
        })
      )
      errores <- .validar_formulario()
      racafeForms::racafeShowValidation(session, validacion, mode = "inline")
      if (!validacion$valid || length(errores) > 0L) return(invisible(NULL))
      
      es_tarea <- identical(input$NOT_Tipo, "list")
      
      confirmacion_rv(TRUE)
      showModal(racafeForms::racafeModalDetail(
        title = if (es_tarea) "Confirmar tarea" else "Confirmar nota",
        size = "subventana3",
        racafeShiny::racafeOverlayLoader("guardar", .resumen_confirmacion_ui(),
          triggers = "NOT_Confirmar", ns = ns),
        footer = racafeForms::racafeModalFooter(
          racafeForms::racafeButtonCancel("NOT_Cancelar", "Cancelar",
                                          ns = ns, size = "xs"),
          racafeForms::racafeButton("NOT_Confirmar", "Crear",
                                    icon = "note-sticky", variant = "primary",
                                    size = "xs", ns = ns)
        )
      ))
    })
    
    observeEvent(input$NOT_Cancelar, {
      confirmacion_rv(FALSE)
      removeModal()
    })
    
    observeEvent(input$NOT_Confirmar, {
      on.exit(racafeShiny::racafeUpdateOverlayLoader(session, "guardar", FALSE), add = TRUE)
      if (!isTRUE(confirmacion_rv()) || isTRUE(guardando_rv())) return(invisible(NULL))
      errores <- .validar_formulario()
      if (length(errores)) {
        confirmacion_rv(FALSE)
        removeModal()
        racafeForms::racafeNotify(session, "Campos pendientes",
                                  paste(errores, collapse = " "),
                                  type = "error", duration = 5)
        return(invisible(NULL))
      }
      guardando_rv(TRUE)
      confirmacion_rv(FALSE)
      on.exit(guardando_rv(FALSE), add = TRUE)
      racafeShiny::racafeUpdateOverlayLoader(session, "guardar", TRUE)
      
      tryCatch({
        req(usuario())
        
        actividades <- .obtener_actividades()
        
        es_tarea <- identical(input$NOT_Tipo, "list")
        
        registro <- .nota_construir_registro(
          titulo = input$NOT_Titulo, tipo = input$NOT_Tipo, descripcion = input$NOT_Descripcion,
          fecha_cumplimiento = if (es_tarea) as.Date(input$NOT_FechaFin) else as.Date(NA),
          prioridad = if (es_tarea) stringr::str_to_upper(trimws(input$NOT_Prioridad)) else NA_character_,
          responsable = if (es_tarea) input$NOT_Responsable else NA_character_,
          etiquetas = etiquetas_modulo$seleccion(), actividades = actividades$actividades,
          cumplidas = actividades$cumplidas, usuario = usuario()
        )
        
        racafeBD::AgregarDatos(registro, "CRMNALNOTAS")
        
        removeModal()
        
        actualizaciones_rv(isolate(actualizaciones_rv()) + 1L)
        
        .limpiar_formulario()
        
        racafeForms::racafeNotify(
          session, if (es_tarea) "Tarea creada" else "Nota creada",
          "El registro se guardó correctamente.", type = "success", duration = 4
        )
      }, error = function(e) {
        removeModal()
        
        .ManejarErrorAccion(error = e, operacion = "crear la nota", usuario = usuario())
      })
    })
    
    # Retorno ----
    list(actualizaciones = reactive(actualizaciones_rv()))
  })
}
