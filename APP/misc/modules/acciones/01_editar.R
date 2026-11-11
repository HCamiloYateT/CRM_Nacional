# Editar
# Modulos Auxiliares ----

## Identificacion ----
IdentificacionUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    uiOutput(ns("Titulo")),
    box(title = "Identificación", width = 12, collapsible = FALSE,
        .crm_select(id = ns("CampoAutorizaDatos"),
                                label = Obligatorio("Autoriza Tratamiento de Datos"), choices = c("SI", "NO"),
                                selected = "SI", multiple = FALSE, empty = FALSE),
        fluidRow(
          column(6, racafeForms::racafeInputText(id = ns("CampoNit"), label = h6("NIT"), width = "100%")),
          column(6, racafeForms::racafeInputText(id = ns("CampoRazonSocial"), label = h6("Razón Social"),
                                                 width = "100%"))
        ),
        fluidRow(
          column(12, FormatearTexto("* Debe diligenciar Razón Social, NIT o ambos",
                                    tamano_pct = 0.75, color = "grey"))
        ),
        fluidRow(
          column(6, uiOutput(ns("ValidacionNit"))),
          column(6, uiOutput(ns("ValidacionRazonSocial")))
        ),
        uiOutput(ns("OrigenTexto")),
        racafeForms::racafeButton(id = ns("BotonGuardar"), label = "Guardar Cambios", size = "xs", variant = "primary",
                                icon = "save")
    )
  )
}
Identificacion <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    contador_actualizaciones <- reactiveVal(0)
    
    # Funciones ----
    # Normalizacion de texto (squish + upper), igual que CrearContacto
    .NormalizarRazonSocial <- function(texto) {
      if (EsVacio(texto)) return(NA_character_)
      str_squish(texto) %>% str_to_upper()
    }
    
    # Datos ----
    datos_contacto <- reactive({
      req(codigo_contacto())
      CargarDatos("CRMNALCONTACTO") %>%
        filter(CodContacto == codigo_contacto())
    })
    
    # Fuente unica para validacion de unicidad: se consultaba dos veces
    # (una por cada renderUI en vivo -.ValidarNit-, otra al confirmar
    # guardado en .ValidarCamposFormulario). Ahora ambos consumidores
    # derivan del mismo reactive, cacheado por el ciclo reactivo
    contactos_otros <- reactive({
      req(codigo_contacto())
      CargarDatos("CRMNALCONTACTO") %>%
        filter(CodContacto != codigo_contacto())
    })
    
    nits_marlot <- reactive({
      CargarDatos("CRMNALMARLOT")$CLIENTE
    })
    
    # Validaciones ----
    # Validacion de campos obligatorios y unicidad de NIT antes de guardar
    .ValidarCamposFormulario <- function() {
      nit_ingresado <- input$CampoNit %||% ""
      nit_original <- datos_contacto()$PerCod
      nit_cambio <- !EsVacio(nit_ingresado) && nit_ingresado != nit_original
      
      c("Debe diligenciar Razón Social, NIT o ambos" =
          EsVacio(input$CampoRazonSocial) && EsVacio(nit_ingresado),
        "El NIT debe ser un valor numérico válido" =
          !EsVacio(nit_ingresado) && !EsEnteroPositivo(nit_ingresado),
        "El NIT ya existe como contacto activo" =
          nit_cambio && nit_ingresado %in% contactos_otros()$PerCod,
        "El NIT ya existe en CRMNALMARLOT" =
          nit_cambio && nit_ingresado %in% nits_marlot()
      )
    }
    
    # Outputs Textos ----
    output$Titulo <- renderUI({
      req(nrow(datos_contacto()) > 0)
      h4(.titulo_identificacion(datos_contacto()$PerCod, datos_contacto()$PerRazSoc))
    })
    
    # Validaciones en vivo, defensivas ante fallos de consulta (CargarDatos)
    output$ValidacionRazonSocial <- renderUI({
      tryCatch({
        .ValidarRazonSocial(razon_social_ingresada = input$CampoRazonSocial %||% "",
                            codigo_contacto_actual = codigo_contacto())
      }, error = function(error) {
        FormatearTexto(paste0("* Error de validación: ", conditionMessage(error)),
                       negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.75)
      })
    })
    
    output$ValidacionNit <- renderUI({
      tryCatch({
        .ValidarNit(nit_ingresado = input$CampoNit %||% "", codigo_contacto_actual = codigo_contacto())
      }, error = function(error) {
        FormatearTexto(paste0("* Error de validación: ", conditionMessage(error)),
                       negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.75)
      })
    })
    
    output$OrigenTexto <- renderUI({
      req(nrow(datos_contacto()) > 0)
      detalle_origen <- datos_contacto()$DetOrigen[[1]]
      texto_origen <- paste0(
        "Origen: ", datos_contacto()$Origen[[1]],
        if (!EsVacio(detalle_origen)) paste0(" - ", detalle_origen) else ""
      )
      FormatearTexto(texto_origen, negrita = TRUE, tamano_pct = 0.85, color = "#374151")
    })
    
    # Observadores de Inputs ----
    # Precarga del formulario, incluye AutorizaTD
    observeEvent(list(datos_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      if (nrow(datos_contacto()) > 0) {
        updateTextInput(session = session, inputId = "CampoRazonSocial",
                        value = datos_contacto()$PerRazSoc)
        updateTextInput(session = session, inputId = "CampoNit",
                        value = datos_contacto()$PerCod)
        .crm_update_select(session = session, selected = datos_contacto()$AutorizaTD,
                                id = "CampoAutorizaDatos")
      }
    })
    
    # Observadores -----
    observeEvent(input$BotonGuardar, {
      condiciones_validacion <- .ValidarCamposFormulario()
      if (any(condiciones_validacion)) {
        for (nombre_condicion in names(condiciones_validacion)) {
          if (condiciones_validacion[[nombre_condicion]]) {
            racafeForms::racafeNotify(duration = 4, type = "error", title = nombre_condicion,
                                    session = shiny::getDefaultReactiveDomain())
          }
        }
        return(invisible(NULL))
      }
      
      coincidencia_exacta <- .BuscarContactoPorRazonSocial(
        razon_social_ingresada = input$CampoRazonSocial,
        codigo_contacto_actual = codigo_contacto()
      )
      
      texto_confirmacion <- if (!is.null(coincidencia_exacta)) {
        paste0("Ya existe un contacto con esta razón social (NIT: ",
               coincidencia_exacta$nit %||% "sin NIT",
               "). ¿Desea continuar y guardar de todas formas?")
      } else {
        "¿Desea guardar los cambios de este contacto?"
      }
      
      # Reutiliza el modal centralizado de racafeShiny, igual que CrearContacto,
      # en vez de la variante local .ConstruirModalConfirmacionContacto / MostrarModalConClase
      .accion_modal_confirmacion(
        ns = ns, titulo = "Confirmar guardado", texto = texto_confirmacion,
        id_cancelar = "BotonCancelarGuardado", id_confirmar = "BotonConfirmarGuardado",
        label_confirmar = "Guardar Cambios", icono_confirmar = "floppy-disk"
      )
    })
    
    observeEvent(input$BotonCancelarGuardado, {
      removeModal()
    })
    
    observeEvent(input$BotonConfirmarGuardado, {
      if (!.accion_validar(.ValidarCamposFormulario())) return(invisible(NULL))
      # Fila con los datos actualizados; AutorizaTD es editable
      fila_actualizada <- data.frame(
        CodContacto   = codigo_contacto(),
        UsuarioCrea   = datos_contacto()$UsuarioCrea,
        FechaHoraCrea = datos_contacto()$FechaHoraCrea,
        UsuarioMod    = usuario(), FechaHoraModi = Sys.time(),
        AutorizaTD    = input$CampoAutorizaDatos,
        PerRazSoc     = .NormalizarRazonSocial(input$CampoRazonSocial), PerCod = input$CampoNit,
        Origen        = datos_contacto()$Origen, DetOrigen = datos_contacto()$DetOrigen,
        Estado        = datos_contacto()$Estado,
        stringsAsFactors = FALSE
      ) %>%
        mutate(across(where(is.character) & !c("PerRazSoc"),
                      ~ str_to_upper(ifelse(. == "", NA_character_, .))))
      
      tryCatch({
        ReemplazarDatos(fila_actualizada, "CRMNALCONTACTO", llaves = list(CodContacto = codigo_contacto()))
        removeModal()
        racafeForms::racafeNotify(duration = 4, type = "success", title = "Contacto modificado exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        contador_actualizaciones(contador_actualizaciones() + 1)
      }, error = function(error) {
        removeModal()
        .ManejarErrorAccion(error = error, operacion = "guardar el contacto",
                            usuario = usuario(), codigo_contacto = codigo_contacto())
      })
    })
    
    # Retorno ----
    list(actualizaciones = reactive(contador_actualizaciones()))
  })
}

## Directorio ----
DirectorioUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    # Datos Empresariales ----
    box(title = "Datos Empresariales", width = 12, collapsible = FALSE,
        racafeForms::racafeInputText(id = ns("CampoCorreoEmpresarial"), label = h6("Correo"), width = "100%"),
        tags$hr(style = "border-color: grey;"),
        h6("Teléfonos"),
        fluidRow(
          column(10, racafeForms::racafeInputText(id = ns("CampoTelefonoNuevo"), label = NULL,
                                                  placeholder = "Número de teléfono",
                                  width = "100%")),
          column(1, racafeForms::racafeButton(id = ns("BotonAgregarTelefono"), label = "Agregar", size = "xs",
                                  variant = "primary", icon = "plus"))
        ),
        uiOutput(ns("ListaTelefonosEmpresariales")),
        tags$hr(style = "border-color: grey;"),
        h6("Redes Sociales"),
        fluidRow(
          column(5, .crm_select(id = ns("CampoTipoRed"), label = NULL, choices = .accion_opciones("directorio_red_social", .TIPOS_RED_SOCIAL),
                                  selected = NULL, multiple = FALSE, empty = FALSE)),
          column(6, racafeForms::racafeInputText(id = ns("CampoUsuarioRed"), label = NULL,
                                  placeholder = "Enlace o usuario de la cuenta", width = "100%")),
          column(1, racafeForms::racafeButton(id = ns("BotonAgregarRed"), label = "Agregar", size = "xs",
                                              variant = "primary",
                                  icon = "plus"))
        ),
        racafeModulos::TablaReactableUI(ns("TablaRedesEmpresariales"), titulo = NULL),
        racafeForms::racafeButton(id = ns("BotonGuardarDatosEmpresariales"), label = "Guardar", size = "xs",
                                variant = "primary", icon = "floppy-disk")
    ),
    # Datos Personales ----
    box(title = "Datos Personales", width = 12, collapsible = FALSE,
        div(style = "text-align: right; margin-bottom: 10px;",
            racafeForms::racafeButton(id = ns("BotonAnadirPersona"), label = "Añadir Persona", size = "xs",
                                    variant = "success", icon = "user-plus")),
        shinyjs::hidden(
          div(id = ns("PanelNuevaPersona"),
              style = "border:1px solid #E2E8F0; border-radius:6px; padding:12px; margin-bottom:14px;",
              fluidRow(
                column(6, racafeForms::racafeInputText(id = ns("CampoNombre"), label = Obligatorio("Nombre"),
                                                       width = "100%")),
                column(6, racafeForms::racafeInputText(id = ns("CampoCargo"), label = h6("Cargo"), width = "100%"))
              ),
              fluidRow(
                column(6, racafeForms::racafeInputText(id = ns("CampoTelefonoPersona"), label = h6("Teléfono"),
                                                       width = "100%")),
                column(6, racafeForms::racafeInputText(id = ns("CampoCorreoPersona"), label = h6("Correo"),
                                                       width = "100%"))
              ),
              tags$hr(style = "border-color: grey;"),
              h6("Redes Sociales"),
              fluidRow(
                column(5, .crm_select(id = ns("CampoTipoRedPersona"), label = NULL,
                                        choices = .accion_opciones("directorio_red_social", .TIPOS_RED_SOCIAL), selected = NULL, multiple = FALSE, empty = FALSE)),
                column(5, racafeForms::racafeInputText(id = ns("CampoUsuarioRedPersona"), label = NULL,
                                        placeholder = "Usuario o enlace", width = "100%")),
                column(2, racafeForms::racafeButton(id = ns("BotonAgregarRedPersona"), label = NULL, size = "xs",
                                        variant = "primary", icon = "plus"))
              ),
              uiOutput(ns("RedesTemporales")),
              div(style = "text-align: right; margin-top: 10px;",
                  racafeForms::racafeButton(id = ns("BotonCancelarPersona"), label = "Cancelar", size = "xs",
                                          variant = "ghost", icon = "xmark"),
                  racafeForms::racafeButton(id = ns("BotonGuardarPersona"), label = "Guardar Persona", size = "xs",
                                          variant = "primary", icon = "floppy-disk"))
          )
        ),
        racafeModulos::TablaReactableUI(ns("TablaPersonas"), titulo = "Personas de Contacto")
    )
  )
}
Directorio <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    .confirmar_guardado <- .accion_guardado(input, session, codigo_contacto, usuario)
    ns <- session$ns
    
    # Contadores de actualizzacion de las bases -----
    contador_actualizacion_correo_empresa    <- reactiveVal(0)
    contador_actualizacion_telefonos_empresa <- reactiveVal(0)
    contador_actualizacion_redes_empresa     <- reactiveVal(0)
    contador_actualizacion_personas          <- reactiveVal(0)
    redes_temporales_persona_nueva           <- reactiveVal(list())
    
    # Empresarial ----
    
    ## Correo ----
    ### Datos ----
    datos_correo_empresa <- reactive({
      contador_actualizacion_correo_empresa()
      codigo_actual <- codigo_contacto()
      req(codigo_actual)
      cargar_info_empresarial_directorio(codigo_actual)
    })
    ### Observers ----
    observeEvent(list(codigo_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      datos <- datos_correo_empresa()
      correo_actual <- if (nrow(datos) > 0) datos$CorreoGeneral[[1]] else ""
      updateTextInput(session, "CampoCorreoEmpresarial", value = correo_actual)
    }, ignoreInit = FALSE)
    observeEvent(input$BotonGuardarDatosEmpresariales, {
      req(codigo_contacto())
      if (!.accion_validar(c("El correo no es válido" =
          .accion_correo_invalido(input$CampoCorreoEmpresarial)))) return(invisible(NULL))
      .confirmar_guardado({
        tryCatch({
          guardar_info_empresarial_directorio(codigo_contacto(), input$CampoCorreoEmpresarial, usuario())
          contador_actualizacion_correo_empresa(isolate(contador_actualizacion_correo_empresa()) + 1)
          racafeForms::racafeNotify(duration = 3, type = "success", title = "Datos empresariales guardados exitosamente",
                                  session = shiny::getDefaultReactiveDomain())
        }, error = function(error) {
          .ManejarErrorAccion(error = error, operacion = "guardar los datos empresariales",
                              usuario = usuario(), codigo_contacto = codigo_contacto())
        })
      }, titulo = "Guardar datos empresariales")
    })

    observadores_telefonos <- new.env(parent = emptyenv())
    session$onSessionEnded(function() lapply(as.list(observadores_telefonos), function(obs) obs$destroy()))
    
    ## Telefono ----
    ### Datos ----
    datos_telefonos <- reactive({
      contador_actualizacion_telefonos_empresa()
      req(codigo_contacto())
      listar_telefonos_directorio(codigo_contacto())
    })
    ### Outputs ----
    output$ListaTelefonosEmpresariales <- renderUI({
      telefonos <- datos_telefonos()
      if (nrow(telefonos) == 0) {
        return(tags$p("Sin teléfonos registrados.", style = "color:#94A3B8; font-size:12px;"))
      }
      tagList(lapply(seq_len(nrow(telefonos)), function(indice_fila) {
        tags$span(
          style = paste0("display:inline-block; margin:3px 4px; padding:3px 10px; ",
                         "border-radius:10px; background:#EEF2FF; font-size:12px;"),
          icon("phone"), " ", telefonos$Telefono[indice_fila], " ",
          racafeForms::racafeButton(paste0("BotonEliminarTelefono_", telefonos$IdTelefono[indice_fila]),
            label = NULL, icon = "xmark", title = "Eliminar teléfono", ns = ns, size = "xs", variant = "ghost")
        )
      }))
    })
    ### Observers ----
    observeEvent(input$BotonAgregarTelefono, {
      req(codigo_contacto())
      if (!.accion_validar(c("Ingrese un teléfono válido de 7 a 15 dígitos" =
          EsVacio(input$CampoTelefonoNuevo) || .accion_telefono_invalido(input$CampoTelefonoNuevo)))) {
        return(invisible(NULL))
      }
      if (trimws(input$CampoTelefonoNuevo %||% "") == "") {
        racafeForms::racafeNotify(type = "warning", duration = 3, title = "Ingrese un número de teléfono",
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      if (nrow(datos_telefonos()) >= .MAX_TELEFONOS_GENERAL) {
        racafeForms::racafeNotify(type = "warning", duration = 4,
                                title = paste0("Máximo ", .MAX_TELEFONOS_GENERAL, " teléfonos"),
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      .confirmar_guardado({
        registrar_telefono_directorio(codigo_contacto(), input$CampoTelefonoNuevo, usuario())
        updateTextInput(session, "CampoTelefonoNuevo", value = "")
        contador_actualizacion_telefonos_empresa(isolate(contador_actualizacion_telefonos_empresa()) + 1)
      }, titulo = "Añadir teléfono")
    })
    # Enlazar dinamicamente los botones de eliminar telefono 
    observe({
      telefonos <- datos_telefonos()
      vigentes <- as.character(telefonos$IdTelefono)
      for (clave in setdiff(ls(observadores_telefonos), vigentes)) {
        observadores_telefonos[[clave]]$destroy()
        rm(list = clave, envir = observadores_telefonos)
      }
      lapply(telefonos$IdTelefono, function(id_tel) {
        clave <- as.character(id_tel)
        if (exists(clave, envir = observadores_telefonos, inherits = FALSE)) return(invisible(NULL))
        observadores_telefonos[[clave]] <- observeEvent(input[[paste0("BotonEliminarTelefono_", id_tel)]], {
          .confirmar_guardado({
            eliminar_telefono_directorio(id_tel)
            contador_actualizacion_telefonos_empresa(isolate(contador_actualizacion_telefonos_empresa()) + 1)
          }, titulo = "Eliminar teléfono")
        }, ignoreInit = TRUE)
      })
    })
    
    ## Redes Sociales ----
    ### Datos ----
    datos_redes <- reactive({
      contador_actualizacion_redes_empresa()
      req(codigo_contacto())
      redes <- listar_redes_directorio(codigo_contacto())
      if (nrow(redes) == 0) return(redes %>% mutate(RedHTML = character(), Eliminar = character()))
      redes %>%
        mutate(RedHTML = mapply(.linea_red_social, TipoRedSocial, UsuarioRed), Eliminar = IdRed)
    })
    ### Outputs ----
    modulo_tabla_redes_empresa <- racafeModulos::TablaReactable(
      id = "TablaRedesEmpresariales", data = datos_redes, columnas = NULL,
      col_specs = list(
        Eliminar      = .coldef_accion("Eliminar", "trash", COLOR_ERROR),
        RedHTML       = reactable::colDef(name = "Red Social", html = TRUE, minWidth = 240),
        TipoRedSocial = reactable::colDef(show = FALSE),
        UsuarioRed    = reactable::colDef(show = FALSE),
        IdRed         = reactable::colDef(show = FALSE),
        # Columna tecnica de BD que no debe verse en la tabla (item 1)
        CodContacto   = reactable::colDef(show = FALSE)
      ),
      modo_seleccion = "celda", id_col = "IdRed", cols_activos = "Eliminar",
      sortable = FALSE, searchable = FALSE, page_size = 10, compact = TRUE,
      mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    ### Observers ----
    observeEvent(modulo_tabla_redes_empresa$seleccion(), {
      seleccion_actual <- modulo_tabla_redes_empresa$seleccion()
      req(seleccion_actual, seleccion_actual$col == "Eliminar")
      .confirmar_guardado({
        eliminar_red_social_directorio(seleccion_actual$fila$IdRed[[1]])
        contador_actualizacion_redes_empresa(isolate(contador_actualizacion_redes_empresa()) + 1)
        racafeForms::racafeNotify(duration = 3, type = "success", title = "Red social eliminada",
                                session = shiny::getDefaultReactiveDomain())
      }, titulo = "Eliminar red social")
    })
    observeEvent(input$BotonAgregarRed, {
      req(codigo_contacto())
      if (input$CampoTipoRed == "" || trimws(input$CampoUsuarioRed %||% "") == "") {
        racafeForms::racafeNotify(type = "warning", duration = 3,
                                title = "Seleccione el tipo de red y el usuario/enlace",
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      .confirmar_guardado({
        registrar_red_social_directorio(codigo_contacto(), input$CampoTipoRed, input$CampoUsuarioRed)
        updateTextInput(session, "CampoUsuarioRed", value = "")
        contador_actualizacion_redes_empresa(isolate(contador_actualizacion_redes_empresa()) + 1)
      }, titulo = "Añadir red social")
    })
    
    # Personal ----
    
    ## Reactivos ----
    ### Datos ----
    datos_personas <- reactive({
      contador_actualizacion_personas()
      req(codigo_contacto())
      listar_personas_directorio(codigo_contacto()) %>%
        mutate(Eliminar = IdPersona)
    })
    ### Outputs ----
    modulo_tabla_personas <- racafeModulos::TablaReactable(
      id = "TablaPersonas", data = datos_personas, columnas = NULL,
      col_specs = list(
        Eliminar    = .coldef_accion("Eliminar", "trash", COLOR_ERROR),
        Nombre      = reactable::colDef(name = "Nombre", minWidth = 120),
        Cargo       = reactable::colDef(name = "Cargo", minWidth = 100),
        Telefono    = reactable::colDef(name = "Teléfono", minWidth = 100),
        Correo      = reactable::colDef(name = "Correo", minWidth = 140),
        RedesHTML   = reactable::colDef(name = "Redes Sociales", html = TRUE, minWidth = 220),
        IdPersona   = reactable::colDef(show = FALSE),
        CodContacto = reactable::colDef(show = FALSE)
      ),
      modo_seleccion = "celda", id_col = "IdPersona", cols_activos = "Eliminar",
      sortable = TRUE, searchable = TRUE, page_size = 10, compact = TRUE,
      mostrar_badge = FALSE, mostrar_nota = FALSE
    )
    ## Observers ----
    observeEvent(modulo_tabla_personas$seleccion(), {
      seleccion_actual <- modulo_tabla_personas$seleccion()
      req(seleccion_actual, seleccion_actual$col == "Eliminar")
      .confirmar_guardado({
        eliminar_persona_directorio(seleccion_actual$fila$IdPersona[[1]])
        contador_actualizacion_personas(isolate(contador_actualizacion_personas()) + 1)
        racafeForms::racafeNotify(duration = 3, type = "success", title = "Persona eliminada",
                                session = shiny::getDefaultReactiveDomain())
      }, titulo = "Eliminar persona")
    })
    
    # Abrir / cerrar panel de nueva persona
    observeEvent(input$BotonAnadirPersona, {
      if (nrow(datos_personas()) >= .MAX_PERSONAS_CONTACTO) {
        racafeForms::racafeNotify(type = "warning", duration = 4,
                                title = paste0("Máximo ", .MAX_PERSONAS_CONTACTO, " personas de contacto"),
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      redes_temporales_persona_nueva(list())
      shinyjs::show("PanelNuevaPersona")
    })
    observeEvent(input$BotonCancelarPersona, { shinyjs::hide("PanelNuevaPersona") })
    
    # Agregar red temporal a la lista en memoria (aun no se guarda en BD)
    observeEvent(input$BotonAgregarRedPersona, {
      if (input$CampoTipoRedPersona == "" || trimws(input$CampoUsuarioRedPersona %||% "") == "") {
        racafeForms::racafeNotify(type = "warning", duration = 3,
                                title = "Seleccione el tipo de red y el usuario/enlace",
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      redes_actuales <- redes_temporales_persona_nueva()
      redes_actuales[[length(redes_actuales) + 1]] <- list(
        tipo = input$CampoTipoRedPersona, usuario = input$CampoUsuarioRedPersona
      )
      redes_temporales_persona_nueva(redes_actuales)
      updateTextInput(session, "CampoUsuarioRedPersona", value = "")
    })
    output$RedesTemporales <- renderUI({
      redes_actuales <- redes_temporales_persona_nueva()
      if (length(redes_actuales) == 0) return(NULL)
      tagList(lapply(seq_along(redes_actuales), function(indice_red) {
        tags$span(
          style = paste0("display:inline-block; margin:3px 4px; padding:2px 8px; border-radius:10px; ",
                         "background:#EEF2FF; font-size:11px;"),
          paste0(redes_actuales[[indice_red]]$tipo, ": ", redes_actuales[[indice_red]]$usuario), " ",
          actionLink(ns(paste0("BotonQuitarRed_", indice_red)), label = icon("xmark"))
        )
      }))
    })
    # Enlazar dinamicamente los botones de quitar red temporal 
    observe({
      n <- length(redes_temporales_persona_nueva())
      req(n > 0)
      lapply(seq_len(n), function(indice_red) {
        observeEvent(input[[paste0("BotonQuitarRed_", indice_red)]], {
          redes_actuales <- redes_temporales_persona_nueva()
          redes_actuales[[indice_red]] <- NULL
          redes_temporales_persona_nueva(redes_actuales)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    # Guardar persona nueva junto con sus redes temporales -----
    observeEvent(input$BotonGuardarPersona, {
      if (!.accion_validar(c("El correo de la persona no es válido" = .accion_correo_invalido(input$CampoCorreoPersona),
          "El teléfono de la persona no es válido" = .accion_telefono_invalido(input$CampoTelefonoPersona)))) return(invisible(NULL))
      if (trimws(input$CampoNombre %||% "") == "") {
        racafeForms::racafeNotify(type = "error", duration = 4, title = "El nombre es obligatorio",
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      .confirmar_guardado({
        identificador_persona_nueva <- registrar_persona_directorio(
          codigo_contacto(), input$CampoNombre, input$CampoCargo,
          input$CampoTelefonoPersona, input$CampoCorreoPersona, usuario()
        )
        for (red_temporal in redes_temporales_persona_nueva()) {
          registrar_red_social_persona_directorio(identificador_persona_nueva, red_temporal$tipo,
                                                  red_temporal$usuario)
        }
      
        shinyjs::hide("PanelNuevaPersona")
        updateTextInput(session, "CampoNombre", value = "")
        updateTextInput(session, "CampoCargo", value = "")
        updateTextInput(session, "CampoTelefonoPersona", value = "")
        updateTextInput(session, "CampoCorreoPersona", value = "")
        redes_temporales_persona_nueva(list())
        contador_actualizacion_personas(isolate(contador_actualizacion_personas()) + 1)
        racafeForms::racafeNotify(duration = 3, type = "success", title = "Persona de contacto agregada",
                                session = shiny::getDefaultReactiveDomain())
      }, titulo = "Añadir persona")
    })
  })
}
## Geografia ----
BuscadorDireccionUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    tags$head(
      # selectize.js renderiza su control con una altura distinta a la de
      # un input de texto normal; se iguala aqui para que ambos campos
      # queden a la misma altura y alineados en la fila
      tags$style(HTML(
        ".selectize-input { min-height: 38px; display: flex; align-items: center; }"
      ))
    ),
    tags$div(style = "position: relative;",
             fluidRow(class = "align-items-end",
                      column(4,
                             .crm_select("Pais", label = h6("País"), choices = .PAISES_BUSCADOR,
                                                           selected = "CO", empty = FALSE, ns = ns)
                      ),
                      column(8,
                             racafeForms::racafeInputText(id = ns("Direccion"), label = h6("Dirección"), value = "",
                                                     placeholder = "Empiece a escribir la dirección...", width = "100%")
                      )
             ),
             fluidRow(
               column(6, racafeForms::racafeInputText("Departamento", "Departamento / Estado", ns = ns)),
               column(6, racafeForms::racafeInputText("Municipio", "Municipio / Ciudad", ns = ns))),
             uiOutput(ns("ListaSugerencias"))
    ),
    uiOutput(ns("Resultado")),
    uiOutput(ns("Diagnostico"))
  )
}
BuscadorDireccion <- function(id, pais_iso2 = "CO", valores_iniciales = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    resultado_lugar <- reactiveVal(NULL)
    estado_autocompletado <- reactiveVal(list(status = NULL, sugerencias = NULL))
    sugerencias_visibles <- reactiveVal(FALSE)
    ultima_seleccion <- reactiveVal(NULL)
    
    # Captura del texto -----
    texto_debounced <- reactive(input$Direccion) %>% debounce(1000)
    observeEvent(texto_debounced(), {
      texto <- texto_debounced()
      req(nzchar(trimws(texto %||% "")))
      
      if (identical(texto, isolate(ultima_seleccion()))) {
        return(invisible(NULL))
      }
      
      resp <- google_autocompletar_direccion(texto, pais_iso2 = input$Pais)
      estado_autocompletado(resp)
      sugerencias_visibles(TRUE)
    }, ignoreInit = TRUE)
    
    # Sugerencias -----
    output$ListaSugerencias <- renderUI({
      req(sugerencias_visibles())
      resp <- estado_autocompletado()
      if (is.null(resp$sugerencias) || nrow(resp$sugerencias) == 0) {
        return(NULL)
      }
      tags$div(style = paste("position:absolute; z-index:1000; background:#fff; width:83%;",
                             "border:1px solid #dee2e6; border-radius:4px; margin-top:-14px;",
                             "box-shadow:0 2px 6px rgba(0,0,0,0.15);"),
               lapply(seq_len(nrow(resp$sugerencias)), function(i) {
                 fila <- resp$sugerencias[i, ]
                 tags$div(fila$texto,
                          style = "padding:8px 12px; cursor:pointer; border-bottom:1px solid #f1f1f1;",
                          onmouseover = "this.style.background='#f8f9fa'",
                          onmouseout = "this.style.background='#fff'",
                          onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})",
                                            ns("SugerenciaClick"),
                                            jsonlite::toJSON(
                                              list(texto = fila$texto, place_id = fila$place_id),
                                              auto_unbox = TRUE
                                            )
                          )
                 )
               })
      )
    })
    
    # Precarga (ej. al editar un contacto existente): usa directamente los
    # valores ya guardados en BD, sin volver a consultar Google. La
    # 'ultima_seleccion' se fija ANTES del updateTextInput para que el
    # debounce del cambio programatico no reabra la lista de sugerencias
    observeEvent(list(valores_iniciales(), input$FormularioListo), {
      req(input$FormularioListo)
      valores <- valores_iniciales()
      req(!is.null(valores))
      
      updateTextInput(session, "Departamento", value = valores$Depto %||% "")
      updateTextInput(session, "Municipio", value = valores$Mpio %||% "")
      ultima_seleccion(valores$Direccion %||% "")
      updateTextInput(session, "Direccion", value = valores$Direccion %||% "")
      sugerencias_visibles(FALSE)
      
      if (!EsVacio(valores$Direccion)) {
        resultado_lugar(list(
          status = "OK",
          pais = valores$Pais %||% NA_character_,
          depto = valores$Depto %||% NA_character_,
          mpio = valores$Mpio %||% NA_character_,
          sublocalidad = NA_character_,
          barrio = NA_character_,
          direccion_formateada = valores$Direccion,
          lat = suppressWarnings(as.numeric(valores$lat)),
          lng = suppressWarnings(as.numeric(valores$lng))
        ))
      }
    })
    
    # Resultados ----
    observeEvent(input$SugerenciaClick, {
      seleccion <- input$SugerenciaClick
      ultima_seleccion(seleccion$texto)
      updateTextInput(session, "Direccion", value = seleccion$texto)
      sugerencias_visibles(FALSE)
      
      resp <- google_obtener_detalle_lugar(seleccion$place_id)
      estado_autocompletado(list(status = resp$status, sugerencias = NULL))
      if (resp$status == "OK") {
        resultado_lugar(resp)
      } else {
        resultado_lugar(NULL)
      }
    })
    output$Resultado <- renderUI({
      r <- resultado_lugar()
      if (is.null(r)) {
        return(NULL)
      }
      tags$div(
        style = "margin-top:6px;",
        FormatearTexto(paste0("OK ", r$direccion_formateada %||% "Coordenadas obtenidas"),
                       tamano_pct = 0.75, color = "#198754"),
        tags$br(),
        tags$span(
          style = "font-size:70%; color:#64748B;",
          tags$b("País: "), r$pais %||% "-", " | ",
          tags$b("Depto: "), r$depto %||% "-", " | ",
          tags$b("Municipio: "), r$mpio %||% "-", " | ",
          tags$b("Sublocalidad: "), r$sublocalidad %||% "-", " | ",
          tags$b("Barrio: "), r$barrio %||% "-", " | ",
          tags$b("Lat: "), round(r$lat %||% 0, 5), " | ",
          tags$b("Lng: "), round(r$lng %||% 0, 5)
        )
      )
    })
    output$Diagnostico <- renderUI({
      resp <- estado_autocompletado()
      req(!is.null(resp$status), resp$status != "OK", resp$status != "TEXTO_CORTO")
      
      mensaje <- switch(
        resp$status,
        "ZERO_RESULTS" = "No se encontraron direcciones que coincidan con lo escrito.",
        "ERROR_CONEXION" = "No fue posible conectarse con Google. Verifica tu conexión a internet.",
        "REQUEST_DENIED" = "El servicio de búsqueda de direcciones no está disponible en este momento.",
        "INVALID_REQUEST" = "La búsqueda no se pudo procesar. Intenta escribir la dirección nuevamente.",
        "OVER_QUERY_LIMIT" = "Se alcanzó el límite de búsquedas disponibles. Intenta más tarde.",
        "RESOURCE_EXHAUSTED" = "Se alcanzó el límite de búsquedas disponibles. Intenta más tarde.",
        "SIN_PLACE_ID" = "No se pudo obtener el detalle de la dirección seleccionada.",
        # Cualquier otro status (HTTP xxx, etc.) usa un mensaje generico
        "Ocurrió un problema al buscar la dirección. Intenta nuevamente."
      )
      
      tags$div(
        style = "margin-top:4px;",
        FormatearTexto(mensaje, tamano_pct = 0.7, color = "#C11007")
      )
    })
    
    observeEvent(resultado_lugar(), {
      r <- resultado_lugar()
      updateTextInput(session, "Departamento", value = r$depto %||% "")
      updateTextInput(session, "Municipio", value = r$mpio %||% "")
    }, ignoreNULL = TRUE)

    # Retorno del modulo — misma firma que el Direccion anterior (Nominatim),
    # asi que Geografia no necesita cambios en como consume este resultado
    reactive({
      r <- resultado_lugar()
      list(Pais = r$pais %||% input$Pais,
           Depto = input$Departamento,
           Mpio = input$Municipio,
           Direccion = input$Direccion,
           lat = r$lat %||% NA_real_,
           lng = r$lng %||% NA_real_)
    })
  })
}

GeografiaUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(7,
           box(title = "Dirección Principal", width = 12, collapsible = FALSE,
               BuscadorDireccionUI(ns("Direccion")),
               racafeForms::racafeButton(id = ns("BotonGuardarUbicacion"), label = "Guardar", size = "xs",
                                       variant = "primary", icon = "floppy-disk")
           ),
           box(title = "Sedes del Contacto", width = 12, collapsible = FALSE,
               fluidRow(
                 column(8,
                        FormatearTexto("Sedes propias de la entidad (puntos de venta, bodegas, oficinas, etc.)", 
                                       tamano_pct = 0.8, color = "#64748B")
                 ),
                 column(4,
                        racafeForms::racafeButton(id = ns("BotonAnadirSucursal"), label = "Añadir", size = "xs",
                                                variant = "success", icon = "plus")
                 )
               ),
               shinyjs::hidden(
                 div(id = ns("PanelNuevaSucursal"),
                     style = "border:1px solid #E2E8F0; border-radius:6px; padding:12px; margin-bottom:14px; margin-top:10px;",
                     fluidRow(
                       column(7,
                              racafeForms::racafeInputText(id = ns("CampoNombreSucursal"),
                                                      label = Obligatorio("Nombre"), width = "100%")
                       ),
                       column(5,
                              .crm_select(id = ns("CampoTipoSede"), label = Obligatorio("Tipo"),
                                                      choices = .TIPOS_SEDE, selected = NULL, multiple = FALSE,
                                                      empty = FALSE)
                       )
                     ),
                     BuscadorDireccionUI(ns("DireccionSucursal")),
                     racafeForms::racafeInputSwitch(label = "Marcar como sede principal/administrativa del contacto",
                                             value = FALSE, id = ns("CampoEsPrincipal"), variant = "danger"),
                     fluidRow(
                       column(6),
                       column(3,
                              racafeForms::racafeButton(id = ns("BotonCancelarSucursal"), label = "Cancelar",
                                                      size = "xs", variant = "secondary", icon = "xmark")
                       ),
                       column(3,
                              racafeForms::racafeButton(id = ns("BotonGuardarSucursal"), label = "Guardar", size = "xs",
                                                      variant = "primary", icon = "floppy-disk")
                       ),
                     ))
               ),
               racafeModulos::TablaReactableUI(ns("TablaSucursales"), titulo = "Listado de Sedes")
           )
    ),
    column(5,
           box(title = "Mapa", width = 12, collapsible = FALSE,
               leaflet::leafletOutput(ns("MapaGeneral"), height = "400px"),
               tags$div(
                 style = "margin-top:10px; font-size:12px; display:flex;flex-wrap:wrap; gap:8px 14px;",
                 tags$span(
                   tags$span(style = paste0(
                     "display:inline-block; width:10px; height:10px; border-radius:50%; ",
                     "background:", .COLOR_SEDE_PRINCIPAL, "; margin-right:4px;"
                   )),
                   "Sede Principal"
                 ),
                 lapply(names(.COLORES_TIPO_SEDE), function(tipo_sede) {
                   tags$span(
                     tags$span(style = paste0(
                       "display:inline-block; width:10px; height:10px; border-radius:50%; ",
                       "background:", .COLORES_TIPO_SEDE[[tipo_sede]], "; margin-right:4px;"
                     )),
                     tipo_sede
                   )
                 })
               )
           )
    )
  )
}
Geografia <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    .confirmar_guardado <- .accion_guardado(input, session, codigo_contacto, usuario)
    
    # Datos ----
    
    # Contadores de refresco: uno por sub-seccion, cada uno dispara solo
    # los reactives que realmente dependen de esa seccion
    contador_actualizacion_sucursales <- reactiveVal(0)
    contador_actualizacion_ubicacion <- reactiveVal(0)
    
    # Fuente unica de la dirección principal: antes se consultaba dos
    # veces (una para precargar el formulario, otra dentro de
    # renderLeaflet). Ahora ambos consumidores derivan de este reactive
    ubicacion_principal_raw <- reactive({
      contador_actualizacion_ubicacion()
      req(codigo_contacto())
      cargar_ubicacion_contacto(codigo_contacto())
    })
    
    ubicacion_existente <- reactive({
      ubicacion <- ubicacion_principal_raw()
      if (nrow(ubicacion) == 0) {
        return(NULL)
      }
      as.list(ubicacion[1, ])
    })
    
    modulo_direccion_principal <- BuscadorDireccion(
      id = "Direccion", pais_iso2 = "CO", valores_iniciales = ubicacion_existente
    )
    
    # Fuente unica de las sedes del contacto: antes se consultaba dos
    # veces (una para la tabla, otra dentro de renderLeaflet, cada una
    # perdiendo columnas distintas). Ahora la tabla y el mapa derivan
    # del mismo reactive con todas las columnas disponibles.
    # Defensivo: si la columna Tipo aun no existe en
    # CRMNALCONTACTOSUCURSAL (ALTER TABLE pendiente, ver nota al final
    # del modulo), se crea vacía en vez de fallar
    sucursales_contacto_raw <- reactive({
      contador_actualizacion_sucursales()
      req(codigo_contacto())
      sucursales <- listar_sucursales_contacto(codigo_contacto())
      if (!"Tipo" %in% names(sucursales)) {
        sucursales$Tipo <- NA_character_
      }
      sucursales
    })
    
    # Tipo es una columna nueva en CRMNALCONTACTOSUCURSAL (ver nota al
    # final del modulo); en registros creados antes de este cambio, o
    # desde EmbudoContactos.R mientras no se actualice allá también,
    # llega NA y se muestra como "Sin tipo"
    datos_sucursales_tabla <- reactive({
      sucursales_contacto_raw() %>%
        mutate(
          Eliminar = IdSucursal,
          Tipo = ifelse(EsVacio(Tipo), "Sin tipo", Tipo),
          Principal = ifelse(EsPrincipal == 1, "Si", "No")
        ) %>%
        select(Eliminar, Nombre, Tipo, Depto, Mpio, Direccion, Principal, IdSucursal)
    })
    
    modulo_direccion_sucursal <- BuscadorDireccion(id = "DireccionSucursal", pais_iso2 = "CO")
    
    # Observadores de Inputs ----
    
    observeEvent(input$BotonGuardarUbicacion, {
      req(codigo_contacto())
      direccion_actual <- modulo_direccion_principal()
      if (!.accion_validar(c("El país es obligatorio" = EsVacio(direccion_actual$Pais),
          "La dirección es obligatoria" = EsVacio(direccion_actual$Direccion)))) return(invisible(NULL))
      .confirmar_guardado({
        tryCatch({
          guardar_ubicacion_contacto(
            codigo_contacto(), direccion_actual$Pais, direccion_actual$Depto,
            direccion_actual$Mpio, direccion_actual$Direccion,
            direccion_actual$lat, direccion_actual$lng, usuario()
          )
          racafeForms::racafeNotify(duration = 3, type = "success", title = "Ubicación guardada exitosamente",
                                  session = shiny::getDefaultReactiveDomain())
          contador_actualizacion_ubicacion(isolate(contador_actualizacion_ubicacion()) + 1)
        }, error = function(error) {
          .ManejarErrorAccion(
            error = error, operacion = "guardar la ubicación",
            usuario = usuario(), codigo_contacto = codigo_contacto()
          )
        })
      }, titulo = "Guardar ubicación")
    })
    
    modulo_tabla_sucursales <- racafeModulos::TablaReactable(
      id = "TablaSucursales",
      data = datos_sucursales_tabla,
      columnas = NULL,
      col_specs = list(
        Eliminar = .coldef_accion("Eliminar", "trash", COLOR_ERROR),
        Nombre = reactable::colDef(name = "Nombre", minWidth = 130),
        Tipo = reactable::colDef(name = "Tipo de Sede", minWidth = 110),
        Depto = reactable::colDef(name = "Departamento", minWidth = 110),
        Mpio = reactable::colDef(name = "Municipio", minWidth = 110),
        Direccion = reactable::colDef(name = "Dirección", minWidth = 180),
        Principal = reactable::colDef(name = "Sede Principal/Admin.", minWidth = 130),
        IdSucursal = reactable::colDef(show = FALSE)
      ),
      modo_seleccion = "celda",
      id_col = "IdSucursal",
      cols_activos = "Eliminar",
      sortable = TRUE,
      searchable = TRUE,
      page_size = 10,
      compact = TRUE,
      mostrar_badge = FALSE,
      mostrar_nota = FALSE
    )
    
    observeEvent(modulo_tabla_sucursales$seleccion(), {
      seleccion_actual <- modulo_tabla_sucursales$seleccion()
      req(seleccion_actual, seleccion_actual$col == "Eliminar")
      .confirmar_guardado({
        tryCatch({
          eliminar_sucursal_contacto(seleccion_actual$fila$IdSucursal[[1]])
          contador_actualizacion_sucursales(isolate(contador_actualizacion_sucursales()) + 1)
          racafeForms::racafeNotify(duration = 3, type = "success", title = "Sede eliminada",
                                  session = shiny::getDefaultReactiveDomain())
        }, error = function(error) {
          .ManejarErrorAccion(
            error = error, operacion = "eliminar la sede del contacto",
            usuario = usuario(), codigo_contacto = codigo_contacto()
          )
        })
      }, titulo = "Eliminar sede")
    })
    
    observeEvent(input$BotonAnadirSucursal, {
      shinyjs::show("PanelNuevaSucursal")
    })
    
    observeEvent(input$BotonCancelarSucursal, {
      shinyjs::hide("PanelNuevaSucursal")
    })
    
    observeEvent(input$BotonGuardarSucursal, {
      req(codigo_contacto())
      if (EsVacio(input$CampoNombreSucursal)) {
        racafeForms::racafeNotify(type = "error", duration = 4, title = "El nombre de la sede es obligatorio",
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      if (EsVacio(input$CampoTipoSede)) {
        racafeForms::racafeNotify(type = "error", duration = 4, title = "El tipo de sede es obligatorio",
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      direccion_sucursal_actual <- modulo_direccion_sucursal()
      if (!.accion_validar(c("El país de la sede es obligatorio" = EsVacio(direccion_sucursal_actual$Pais),
          "La dirección de la sede es obligatoria" = EsVacio(direccion_sucursal_actual$Direccion)))) {
        return(invisible(NULL))
      }
      .confirmar_guardado({
        tryCatch({
          registrar_sucursal_contacto(
            codigo_contacto(), input$CampoNombreSucursal, direccion_sucursal_actual$Pais,
            direccion_sucursal_actual$Depto, direccion_sucursal_actual$Mpio,
            direccion_sucursal_actual$Direccion, direccion_sucursal_actual$lat,
            direccion_sucursal_actual$lng, input$CampoEsPrincipal, usuario(),
            tipo = input$CampoTipoSede
          )
          shinyjs::hide("PanelNuevaSucursal")
          updateTextInput(session, "CampoNombreSucursal", value = "")
          .crm_update_select(session = session, selected = character(0), id = "CampoTipoSede")
          contador_actualizacion_sucursales(isolate(contador_actualizacion_sucursales()) + 1)
          racafeForms::racafeNotify(duration = 3, type = "success", title = "Sede agregada exitosamente",
                                  session = shiny::getDefaultReactiveDomain())
        }, error = function(error) {
          .ManejarErrorAccion(
            error = error, operacion = "guardar la sede del contacto",
            usuario = usuario(), codigo_contacto = codigo_contacto()
          )
        })
      }, titulo = "Añadir sede")
    })
    
    # Mapa: ya no vuelve a consultar CargarDatos(); reutiliza los dos
    # reactives de arriba, que ya están cacheados por el ciclo reactivo
    output$MapaGeneral <- leaflet::renderLeaflet({
      ubicacion_principal <- ubicacion_principal_raw()
      sucursales <- sucursales_contacto_raw()
      
      puntos_principal <- if (nrow(ubicacion_principal) > 0) {
        ubicacion_principal %>%
          filter(!is.na(lat), !is.na(lng)) %>%
          mutate(Etiqueta = paste0("Sede Principal: ", Direccion %||% ""))
      } else {
        data.frame(lat = double(), lng = double(), Etiqueta = character())
      }
      
      # Color del marcador segun el tipo de sede; las sedes sin tipo
      # (registradas antes de este cambio) toman el color de "Otro"
      puntos_sucursales <- if (nrow(sucursales) > 0) {
        sucursales %>%
          filter(!is.na(lat), !is.na(lng)) %>%
          mutate(
            TipoMostrado = ifelse(EsVacio(Tipo), "Otro", Tipo),
            ColorMarcador = dplyr::coalesce(
              .COLORES_TIPO_SEDE[TipoMostrado], .COLORES_TIPO_SEDE[["Otro"]]
            ),
            Etiqueta = paste0(TipoMostrado, " ", Nombre, ": ", Direccion %||% "")
          )
      } else {
        data.frame(
          lat = double(), lng = double(), Etiqueta = character(), ColorMarcador = character()
        )
      }
      
      mapa <- leaflet::leaflet() %>%
        leaflet::addTiles()
      
      if (nrow(puntos_principal) == 0 && nrow(puntos_sucursales) == 0) {
        return(mapa %>% leaflet::setView(lng = -74.1, lat = 4.6, zoom = 5))
      }
      
      if (nrow(puntos_principal) > 0) {
        mapa <- mapa %>%
          leaflet::addCircleMarkers(
            data = puntos_principal, lng = ~lng, lat = ~lat, radius = 8,
            color = .COLOR_SEDE_PRINCIPAL, fillOpacity = 0.85, stroke = FALSE, popup = ~Etiqueta
          )
      }
      if (nrow(puntos_sucursales) > 0) {
        mapa <- mapa %>%
          leaflet::addCircleMarkers(
            data = puntos_sucursales, lng = ~lng, lat = ~lat, radius = 6,
            color = ~ColorMarcador, fillOpacity = 0.75, stroke = FALSE, popup = ~Etiqueta
          )
      }
      
      todas_latitudes <- c(puntos_principal$lat, puntos_sucursales$lat)
      todas_longitudes <- c(puntos_principal$lng, puntos_sucursales$lng)
      if (length(unique(todas_latitudes)) == 1 && length(unique(todas_longitudes)) == 1) {
        mapa %>% leaflet::setView(lng = todas_longitudes[1], lat = todas_latitudes[1], zoom = 14)
      } else {
        mapa %>%
          leaflet::fitBounds(
            lng1 = min(todas_longitudes), lat1 = min(todas_latitudes),
            lng2 = max(todas_longitudes), lat2 = max(todas_latitudes)
          )
      }
    })
  })
}
## Potencial ----
PotencialUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    box(title = "Potencial de Negocio", width = 12, collapsible = FALSE,
        fluidRow(
          column(4,
                 .crm_select(id = ns("CampoTostador"), label = h6("¿Es tostador?"),
                                         choices = .choices_potencial$si_no, selected = "NO", multiple = FALSE,
                                         empty = FALSE)),
          column(4,
                 .crm_select(id = ns("CampoAlianza"), label = h6("¿Tiene alianza con tostadora?"),
                                         choices = .choices_potencial$si_no, selected = "NO", multiple = FALSE,
                                         empty = FALSE),
                 conditionalPanel(condition = paste0("input['", ns("CampoAlianza"), "'] == 'SI'"),
                                  racafeForms::racafeInputText(id = ns("CampoDetalleAlianza"), label = h6("¿Con cuál?"),
                                                          width = "100%")
                 )
          ),
          column(4,
                 .crm_select(id = ns("CampoMaquila"), label = h6("¿Maquila?"),
                                         choices = .choices_potencial$si_no, selected = "NO", multiple = FALSE,
                                         empty = FALSE),
                 conditionalPanel(condition = paste0("input['", ns("CampoMaquila"), "'] == 'SI'"),
                                  racafeForms::racafeInputText(id = ns("CampoDetalleMaquila"),
                                                          label = h6("Detalle de maquila"), width = "100%")
                 )
          )
        ),
        tags$hr(style = "border-color: grey;"),
        fluidRow(
          column(4,
                 racafeForms::racafeInputNumeric(id = ns("CampoAniosOperacion"), label = h6("Años de Operación"),
                                         value = NA, min = 0, width = "100%")
          ),
          column(4,
                 .crm_select(id = ns("CampoTipoCompra"), label = h6("Tipo de Compra"),
                                         choices = .accion_opciones("pot_tipo_compra", .choices_potencial$tipo_compra), selected = NULL, multiple = FALSE,
                                         empty = FALSE)
          ),
          column(4,
                 .crm_select(id = ns("CampoFrecuenciaCompra"), label = h6("Frecuencia de Compra"),
                                         choices = .accion_opciones("pot_frecuencia_compra", .choices_potencial$frecuencia_compra), selected = NULL,
                                         multiple = FALSE, empty = FALSE)
          )
        ),
        fluidRow(
          column(6,
                 h6("Orígenes / Calidades Preferidos"),
                 tags$div(`data-crm-form-ready` = ns("CalidadesListo"),
                   racafeModulos::ListaDesplegableAgregableUI(ns("CampoCalidades")))
          ),
          column(3,
                 racafeForms::racafeInputNumeric(id = ns("CampoConsumoEsperado"), label = h6("Consumo Esperado"),
                                         value = NA, min = 0, width = "100%")),
          column(3,
                 .crm_select(id = ns("CampoUnidadConsumo"), label = h6("Unidad"),
                                         choices = .accion_opciones("pot_unidad_consumo", .choices_potencial$unidad_consumo), selected = "SACOS/MES",
                                         multiple = FALSE, empty = FALSE))
        ),
        h6("Certificaciones de Interés"),
        .crm_select("CampoCertificaciones", label = NULL, multiple = TRUE,
                                       choices = .accion_opciones("pot_certificaciones", .choices_potencial$certificaciones), ns = ns,
                                       empty_label = "Seleccione una o varias"),
        racafeForms::racafeInputTextArea(id = ns("CampoObservaciones"), label = h6("Observaciones"), value = "",
                                width = "100%", rows = 3),
        racafeForms::racafeButton(id = ns("BotonGuardarPotencial"), label = "Guardar", size = "xs", variant = "primary",
                                icon = "floppy-disk")
    )
  )
}
Potencial <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    .confirmar_guardado <- .accion_guardado(input, session, codigo_contacto, usuario)
    opciones_productos <- unname(unlist(Choices()$producto, use.names = FALSE))
    anteriores <- CargarDatos("CRMNALPOTENCIAL")$CalidadesPreferidas
    opciones_productos <- sort(unique(toupper(c(opciones_productos,
      unlist(strsplit(anteriores[!is.na(anteriores)], "|", fixed = TRUE))))))
    opciones_productos <- opciones_productos[!is.na(opciones_productos) & nzchar(opciones_productos) &
      opciones_productos != "AGREGAR..."]
    calidades <- racafeModulos::ListaDesplegableAgregable("CampoCalidades", label = NULL,
      choices = opciones_productos, selected = character(), multiple = TRUE, uppercase = TRUE)
    observeEvent(list(codigo_contacto(), input$FormularioListo, input$CalidadesListo), {
      req(input$FormularioListo)
      req(codigo_contacto())
      datos_potencial <- cargar_potencial_contacto(codigo_contacto())
      if (nrow(datos_potencial) == 0) {
        return(invisible(NULL))
      }
      .crm_update_select(session = session,
                              selected = ifelse(datos_potencial$EsTostador[[1]] == 1, "SI", "NO"), id = "CampoTostador")
      .crm_update_select(session = session,
                              selected = ifelse(datos_potencial$AlianzaTostadora[[1]] == 1, "SI", "NO"),
                              id = "CampoAlianza")
      updateTextInput(session, "CampoDetalleAlianza", value = datos_potencial$DetalleAlianza[[1]] %||% "")
      .crm_update_select(session = session,
                              selected = ifelse(datos_potencial$Maquila[[1]] == 1, "SI", "NO"), id = "CampoMaquila")
      updateTextInput(session, "CampoDetalleMaquila", value = datos_potencial$DetalleMaquila[[1]] %||% "")
      racafeForms::racafeUpdateNumeric(session = session,
                              value = suppressWarnings(as.numeric(datos_potencial$AniosOperacion[[1]])),
                              id = "CampoAniosOperacion")
      .crm_update_select(session = session, selected = datos_potencial$TipoCompra[[1]] %||% "",
                              id = "CampoTipoCompra")
      .crm_update_select(session = session, selected = datos_potencial$FrecuenciaCompra[[1]] %||% "",
                              id = "CampoFrecuenciaCompra")
      
      calidades_previas <- if (!EsVacio(datos_potencial$CalidadesPreferidas[[1]])) {
        strsplit(datos_potencial$CalidadesPreferidas[[1]], "|", fixed = TRUE)[[1]]
      } else {
        NULL
      }
      if (!is.null(input$CalidadesListo)) racafeForms::racafeUpdateSelect(session, "CampoCalidades-sel",
        selected = toupper(calidades_previas))
      racafeForms::racafeUpdateNumeric(session = session,
                              value = suppressWarnings(as.numeric(datos_potencial$ConsumoEsperado[[1]])),
                              id = "CampoConsumoEsperado")
      .crm_update_select(session = session, selected = datos_potencial$UnidadConsumo[[1]] %||% "SACOS/MES",
                              id = "CampoUnidadConsumo")
      
      certificaciones_previas <- if (!EsVacio(datos_potencial$CertificacionesInteres[[1]])) {
        strsplit(datos_potencial$CertificacionesInteres[[1]], "|", fixed = TRUE)[[1]]
      } else {
        NULL
      }
      .crm_update_select(session, "CampoCertificaciones", selected = certificaciones_previas)
      updateTextAreaInput(session, "CampoObservaciones", value = datos_potencial$Observaciones[[1]] %||% "")
    })
    
    observeEvent(input$BotonGuardarPotencial, {
      req(codigo_contacto())
      calidades_actuales <- calidades$seleccion()
      consumo <- input$CampoConsumoEsperado
      anios <- input$CampoAniosOperacion
      consumo_invalido <- length(consumo) > 1L ||
        (length(consumo) == 1L && !is.na(consumo) && (!is.finite(consumo) || consumo < 0))
      anios_invalidos <- length(anios) > 1L ||
        (length(anios) == 1L && !is.na(anios) && (!is.finite(anios) || anios < 0))
      if (!.accion_validar(c("El consumo no puede ser negativo" = consumo_invalido,
          "Seleccione la unidad del consumo" = isTRUE(consumo > 0) && EsVacio(input$CampoUnidadConsumo),
          "Los años de operación no pueden ser negativos" = anios_invalidos))) return(invisible(NULL))
      .confirmar_guardado({
        tryCatch({
          guardar_potencial_contacto(
            cod_contacto = codigo_contacto(),
            es_tostador = as.numeric(identical(input$CampoTostador, "SI")),
            alianza = as.numeric(identical(input$CampoAlianza, "SI")),
            detalle_alianza = input$CampoDetalleAlianza,
            maquila = as.numeric(identical(input$CampoMaquila, "SI")),
            detalle_maquila = input$CampoDetalleMaquila,
            anios_operacion = input$CampoAniosOperacion,
            tipo_compra = input$CampoTipoCompra,
            calidades_preferidas = calidades_actuales,
            consumo_esperado = input$CampoConsumoEsperado,
            unidad_consumo = input$CampoUnidadConsumo,
            frecuencia_compra = input$CampoFrecuenciaCompra,
            certificaciones_interes = input$CampoCertificaciones,
            observaciones = input$CampoObservaciones,
            usr = usuario()
          )
          racafeForms::racafeNotify(duration = 3, type = "success", title = "Potencial de negocio guardado",
                                  session = shiny::getDefaultReactiveDomain())
        }, error = function(error) {
          .ManejarErrorAccion(
            error = error,
            operacion = "guardar el potencial de negocio",
            usuario = usuario(),
            codigo_contacto = codigo_contacto()
          )
        })
      }, titulo = "Guardar potencial")
    })
  })
}

# Modulo Principal ----

EditarUI <- function(id) {
  ns <- NS(id)
  bs4Dash::tabsetPanel(
    tabPanel("Identificación", Saltos(), IdentificacionUI(ns("Identificacion"))),
    tabPanel("Directorio", Saltos(), DirectorioUI(ns("Directorio"))),
    tabPanel("Geografía", Saltos(), GeografiaUI(ns("Geografia"))),
    tabPanel("Potencial", Saltos(), PotencialUI(ns("Potencial")))
  )
}
Editar <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    modulo_identificacion <- Identificacion(id = "Identificacion", usuario = usuario, codigo_contacto = codigo_contacto)
    Directorio(id = "Directorio", usuario = usuario, codigo_contacto = codigo_contacto)
    Geografia(id = "Geografia", usuario = usuario, codigo_contacto = codigo_contacto)
    Potencial(id = "Potencial", usuario = usuario, codigo_contacto = codigo_contacto)
    
    list(actualizaciones = modulo_identificacion$actualizaciones)
  })
}

# App de prueba ----
DemoAccionesEditar <- function() {
  ui <- bs4DashPage(
    title = "Prueba Accion Editar",
    header = bs4DashNavbar(),
    sidebar = bs4DashSidebar(),
    controlbar = bs4DashControlbar(),
    footer = bs4DashFooter(),
    body = bs4DashBody(
      useShinyjs(),
      includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                        "refs/heads/main/Styles/style.css")),
      box(
        title = "Editar Contacto (prueba)",
        width = 12,
        racafeForms::racafeInputText(id = "CodigoContactoPrueba", label = "Codigo de Contacto a editar", value = ""),
        EditarUI("AccionEditar")
      )
    )
  )

  server <- function(input, output, session) {
    usuario_sesion <- reactive("CMEDINA")
    codigo_contacto_prueba <- reactive(input$CodigoContactoPrueba)
    Editar("AccionEditar", usuario = usuario_sesion, codigo_contacto = codigo_contacto_prueba)
  }

  shinyApp(ui, server)
}
