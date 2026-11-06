RegistrarCompetenciaUI <- function(id) {
  ns <- NS(id)

  racafeForms::racafePanel(
    title = NULL, padding = "compact", bordered = FALSE,
    racafeForms::racafeForm(size = "compact", variant = "plain",
      racafeModulos::ListaDesplegableAgregableUI(ns("COMP_Nombre")),
      shiny::uiOutput(ns("COMP_Error")),
      racafeForms::racafeInputTextArea("COMP_Observaciones", label = "Observaciones", value = "",
                                      placeholder = "Información adicional sobre la competencia...",
                                      rows = 3, ns = ns),
      racafeForms::racafeFormFooter(
        racafeForms::racafeButton("COMP_Guardar", "Registrar Competencia", icon = "floppy-disk",
                                  variant = "primary", size = "xs", ns = ns, title = "Registrar competencia"),
        align = "right"
      )
    )
  )
}

RegistrarCompetencia <- function(id, usr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactivos ----
    actualizaciones_rv <- reactiveVal(0L)
    error_competencia_rv <- reactiveVal(NULL)
    registro_pendiente_rv <- reactiveVal(NULL)
    guardando_rv <- reactiveVal(FALSE)

    datos_competencia_rv <- reactiveVal(.registrar_competencia_cargar_datos())

    competencia_modulo <- racafeModulos::ListaDesplegableAgregable(
      id = "COMP_Nombre",
      label = Obligatorio("Nombre de la Competencia"),
      choices = .registrar_competencia_choices(isolate(datos_competencia_rv())),
      selected = character(0), multiple = FALSE, fem = TRUE
    )

    competencia_seleccionada_r <- reactive({
      seleccion <- competencia_modulo$seleccion()
      if (is.null(seleccion) || !length(seleccion)) return("")
      trimws(as.character(seleccion[[1]]))
    })

    competencia_existe_r <- reactive({
      competencia <- competencia_seleccionada_r()
      if (!nzchar(competencia)) return(FALSE)
      datos <- datos_competencia_rv()
      if (!nrow(datos) || !"Competencia" %in% names(datos)) return(FALSE)
      any(toupper(trimws(as.character(datos$Competencia))) == toupper(competencia), na.rm = TRUE)
    })

    output$COMP_Error <- renderUI({
      error <- error_competencia_rv()
      if (is.null(error)) return(NULL)
      racafeForms::racafeFormMessage(error, type = "error", size = "compact")
    })

    observeEvent(list(competencia_seleccionada_r(), input$COMP_Observaciones), {
      error_competencia_rv(NULL)
    }, ignoreInit = TRUE)

    # Funciones ----
    .registrar_o_comentar <- function(pendiente) {
      conexion <- racafeBD::ConectarBD()
      on.exit(DBI::dbDisconnect(conexion), add = TRUE)

      DBI::dbWithTransaction(conexion, {
        sql_busqueda <- DBI::sqlInterpolate(
          conexion,
          "SELECT Competencia FROM CRMNALCOMPETENCIA WHERE UPPER(TRIM(Competencia)) = UPPER(TRIM(?nombre)) FOR UPDATE",
          nombre = pendiente$competencia
        )
        encontrados <- DBI::dbGetQuery(conexion, sql_busqueda)

        if (nrow(encontrados) > 1L) {
          stop("Existen competencias duplicadas en la base de datos. Requiere depuración.", call. = FALSE)
        }

        if (nrow(encontrados) == 1L) {
          if (!nzchar(pendiente$observaciones)) {
            stop("Ingrese un comentario para la competencia existente.", call. = FALSE)
          }
          comentario <- sprintf("[%s | %s] %s", format(Sys.time(), "%d/%m/%Y %H:%M"),
                                pendiente$usuario, pendiente$observaciones)
          sql_actualizacion <- DBI::sqlInterpolate(
            conexion,
            paste0("UPDATE CRMNALCOMPETENCIA ",
                   "SET Observaciones = CONCAT_WS(CHAR(10), NULLIF(Observaciones, ''), ?comentario) ",
                   "WHERE UPPER(TRIM(Competencia)) = UPPER(TRIM(?nombre))"),
            comentario = comentario, nombre = pendiente$competencia
          )
          filas <- DBI::dbExecute(conexion, sql_actualizacion)
          if (filas != 1L) stop("No se pudo actualizar la competencia existente.", call. = FALSE)
          "comentario"
        } else {
          registro <- data.frame(
            UsuarioCrea = pendiente$usuario,
            FechaHoraCrea = Sys.time(),
            Competencia = pendiente$competencia,
            PrecioUGQ = .registrar_competencia_ugq_actual(),
            Observaciones = pendiente$observaciones,
            stringsAsFactors = FALSE
          )
          DBI::dbWriteTable(conexion, "CRMNALCOMPETENCIA", registro,
                            append = TRUE, row.names = FALSE)
          "competencia"
        }
      })
    }

    # Registrar Competencia ----
    observeEvent(input$COMP_Guardar, {
      if (isTRUE(guardando_rv())) return(invisible(NULL))
      competencia <- competencia_seleccionada_r()
      observaciones <- trimws(input$COMP_Observaciones %||% "")

      if (!nzchar(competencia)) {
        error_competencia_rv("Seleccione o ingrese el nombre de la competencia.")
        return(invisible(NULL))
      }
      if (competencia_existe_r() && !nzchar(observaciones)) {
        racafeForms::racafeNotify(session, "Observación requerida",
                                  "Ingrese un comentario para la competencia registrada.",
                                  type = "warning", duration = 4)
        return(invisible(NULL))
      }

      pendiente <- list(
        competencia = competencia,
        observaciones = observaciones,
        usuario = req(usr()),
        existe = isTRUE(competencia_existe_r())
      )
      registro_pendiente_rv(pendiente)
      accion <- if (pendiente$existe) "Agregar comentario" else "Registrar competencia"

      showModal(racafeForms::racafeModal(
        title = "Confirmar competencia", size = "subventana5",
        racafeForms::racafeInfoGrid(
          racafeForms::racafeInfoItem("ACCIÓN", accion),
          racafeForms::racafeInfoItem("COMPETENCIA", pendiente$competencia),
          columns = 2
        ),
        racafeForms::racafeInfoItem(
          "OBSERVACIONES",
          if (nzchar(pendiente$observaciones)) pendiente$observaciones else "Sin observaciones"
        ),
        footer = racafeForms::racafeModalFooter(
          racafeForms::racafeButtonCancel("COMP_Cancelar", "Cancelar", ns = ns, size = "xs"),
          racafeForms::racafeButton("COMP_Confirmar", "Confirmar", icon = "floppy-disk",
                                    variant = "primary", size = "xs", ns = ns)
        )
      ))
    })

    observeEvent(input$COMP_Cancelar, {
      registro_pendiente_rv(NULL)
      removeModal()
    })

    observeEvent(input$COMP_Confirmar, {
      pendiente <- isolate(registro_pendiente_rv())
      if (is.null(pendiente) || isTRUE(guardando_rv())) return(invisible(NULL))
      registro_pendiente_rv(NULL)
      guardando_rv(TRUE)
      on.exit(guardando_rv(FALSE), add = TRUE)

      waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
      on.exit(waiter_hide(), add = TRUE)

      tryCatch({
        accion <- .registrar_o_comentar(pendiente)
        removeModal()
        datos_competencia_rv(.registrar_competencia_cargar_datos())
        actualizaciones_rv(isolate(actualizaciones_rv()) + 1L)
        updateTextAreaInput(session, "COMP_Observaciones", value = "")
        racafeForms::racafeNotify(
          session,
          if (identical(accion, "comentario")) "Comentario agregado" else "Competencia registrada",
          if (identical(accion, "comentario")) "Se actualizó la competencia existente." else
            "La nueva competencia fue registrada.",
          type = "success", duration = 4
        )
      }, error = function(e) {
        removeModal()
        .ManejarErrorAccion(error = e, operacion = "registrar la competencia", usuario = usr())
      })
    })

    # Retorno ----
    list(actualizaciones = reactive(actualizaciones_rv()))
  })
}
