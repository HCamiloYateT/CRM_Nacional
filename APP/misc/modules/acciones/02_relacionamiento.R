# Relacionamiento.
# Modulo Principal ----
RelacionamientoUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    uiOutput(ns("Encabezado")),
    # Relacionamiento ----
    box(title = "Relacionamiento", width = 12, collapsible = FALSE,
        fluidRow(
          column(6,
                 h5("Nuevo Relacionamiento"),
                 .crm_select(id = ns("GES_Tipo"), label = Obligatorio("Tipo de Gestión"),
                                         choices = .TIPOS_RELACIONAMIENTO, selected = NULL, multiple = FALSE,
                                         empty = FALSE),
                 racafeForms::racafeInputTextArea(id = ns("GES_Comentario"), label = Obligatorio("Comentario"),
                                         value = "", placeholder = "Describa la gestión realizada", width = "100%",
                                         rows = 4),
                 racafeForms::racafeButton(id = ns("GES_Guardar"), label = "Guardar", size = "xs", variant = "primary",
                                         icon = "save")
          ),
          column(6,
                 h5("Historial de Gestiones"),
                 tags$div(class = "rc-scroll-box", style = "height:320px;overflow-y:auto;padding-right:4px;",
                          uiOutput(ns("GES_Timeline"))
                 )
          )
        )
    ),
    # Recordatorios ----
    box(title = "Recordatorios", width = 12, collapsible = FALSE,
        fluidRow(
          column(6,
                 h5("Programar Recordatorio"),
                 .crm_select(id = ns("REC_Asesor"), label = Obligatorio("Asesor"),
                                         choices = Choices()$personas, selected = NULL, multiple = FALSE,
                                           empty = FALSE),
                 .crm_select(id = ns("REC_Canal"), label = Obligatorio("Canal"),
                                         choices = .CANALES_RECORDATORIO, selected = NULL, multiple = FALSE,
                                         empty = FALSE),
                 racafeForms::racafeInputDateTime("REC_Fecha", label = Obligatorio("Fecha y Hora"),
                                                       min = Sys.time(), ns = ns),
                 racafeForms::racafeInputTextArea(id = ns("REC_Mensaje"), label = Obligatorio("Mensaje"), value = "",
                                         placeholder = "Ingrese el mensaje del recordatorio", width = "100%", rows = 3),
                 racafeForms::racafeButton(id = ns("REC_Programar"), label = "Programar", size = "xs",
                                         variant = "primary", icon = "calendar")
          ),
          column(6,
                 h5("Recordatorios Programados"),
                 tags$div(class = "rc-scroll-box", style = "height:320px;overflow-y:auto;padding-right:4px;",
                          uiOutput(ns("REC_Lista")))
          )
        )
    )
  )
}
Relacionamiento <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, codigo_contacto, usuario)
    .accion_encabezado(output, codigo_contacto)
    ns <- session$ns
    
    gestion_trigger      <- reactiveVal(0)
    recordatorio_trigger <- reactiveVal(0)
    rv_obs <- reactiveValues(cumplir = list(), confirma = list())
    trigger_refresco <- reactiveVal(0)
    
    observeEvent(list(codigo_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      for (campo in c("GES_Tipo", "REC_Asesor", "REC_Canal")) .crm_update_select(session, campo, selected = character())
      for (campo in c("GES_Comentario", "REC_Mensaje")) updateTextAreaInput(session, campo, value = "")
    })
    # Datos base del contacto — filtrado en SQL, no full scan de CRMNALCONTACTO ----
    data_contacto <- reactive({
      req(codigo_contacto())
      cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(codigo_contacto()))
      CargarDatos("CRMNALCONTACTO", condicion = cond)
    })
    etapa_contacto <- reactive({
      trigger_refresco()
      req(codigo_contacto())
      obtener_etapa_contacto(codigo_contacto())
    })
    
    identificador <- reactive({
      req(nrow(data_contacto()) > 0)
      data_contacto()$PerRazSoc %||% data_contacto()$PerCod
    })
    
    output$Titulo <- renderUI({
      req(nrow(data_contacto()) > 0)
      h4(.titulo_identificacion(data_contacto()$PerCod, data_contacto()$PerRazSoc))
    })
    output$EstadoActual <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa))
      tags$div(
        style = "margin-bottom:14px;",
        tags$span(style = "font-size:13px; color:#374151; margin-right:6px;", "Etapa vigente:"),
        .badge_etapa(etapa)
      )
    })
    
    # Cargas centralizadas — un solo reactive por entidad, invalidado por su
    # trigger, reutilizado por el render y por los observers dinamicos. Evita
    # las 2-4 llamadas independientes a CargarDatos() por ciclo que existian
    # cuando cada bloque volvia a consultar por su cuenta.
    gestiones_data <- reactive({
      req(codigo_contacto())
      gestion_trigger()
      listar_relacionamiento(codigo_contacto())
    })
    recordatorios_data <- reactive({
      req(codigo_contacto())
      recordatorio_trigger()
      listar_recordatorio(codigo_contacto())
    })
    
    # Gestión comercial ----
    output$GES_Timeline <- renderUI({
      dat <- gestiones_data()
      if (nrow(dat) == 0) return(tags$p("Sin gestiones registradas aún.", style = "color:#94A3B8; font-size:12px;"))
      tagList(lapply(seq_len(nrow(dat)), function(i) {
        .rc_card(
          borde_color = "#dc3545",
          header = tagList(.badge_tipo_gestion(dat$TipoGestion[i]), " — ",
                           tags$strong(dat$UsuarioCrea[i]), " — ",
                           format(dat$FechaHoraCrea[i], "%d/%m/%Y %H:%M")),
          cuerpo = dat$Comentario[i]
        )
      }))
    })
    
    validacion_gestion <- .accion_validacion(input, session, codigo_contacto,
      c("GES_Tipo", "GES_Comentario"), function() c(
        "Seleccione el tipo de gestión" = EsVacio(input$GES_Tipo),
        "El comentario es obligatorio" = EsVacio(input$GES_Comentario)))
    observeEvent(input$GES_Guardar, {
      if (!validacion_gestion$validate()) return(invisible(NULL))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar gestión",
        texto = paste0("¿Desea registrar esta gestión de tipo '", input$GES_Tipo, "'?"),
        id_cancelar = "GES_CancelarGuardado", id_confirmar = "GES_ConfirmarGuardado",
        label_confirmar = "Registrar", icono_confirmar = "floppy-disk"
      )
    })
    observeEvent(input$GES_CancelarGuardado, { .confirmacion$cancelar("GES_ConfirmarGuardado"); removeModal() })
    observeEvent(input$GES_ConfirmarGuardado, {
      if (!.confirmacion$aceptar("GES_ConfirmarGuardado", validacion_gestion$validate)) return(invisible(NULL))
      tryCatch({
        registrar_relacionamiento(codigo_contacto(), input$GES_Tipo, input$GES_Comentario, usuario())
        removeModal()
        updateTextAreaInput(session, "GES_Comentario", value = "")
        gestion_trigger(gestion_trigger() + 1)
        racafeForms::racafeNotify(duration = 3, type = "success", title = "Gestión registrada exitosamente",
                                session = shiny::getDefaultReactiveDomain())
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste("Error al registrar la gestión:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    # Recordatorios ----
    output$REC_Lista <- renderUI({
      dat <- recordatorios_data()
      if (nrow(dat) == 0) return(tags$p("Sin recordatorios programados.", style = "color:#94A3B8; font-size:12px;"))
      tagList(lapply(seq_len(nrow(dat)), function(i) {
        cumplido <- isTRUE(dat$Enviado[i] == 1)
        vencido  <- !cumplido && dat$FechaRecordatorio[i] < Sys.time()
        
        cuerpo_ui <- tagList(
          tags$div(dat$Mensaje[i]),
          if (cumplido && !is.na(dat$FechaHoraEnvio[i]))
            tags$div(style = "margin-top:4px; font-size:0.75em; color:#1a7a3f;",
                     icon("check-double"), " Cumplido el ",
                     format(dat$FechaHoraEnvio[i], "%d/%m/%Y %H:%M"))
        )
        
        .rc_card(
          borde_color = "#dc3545",
          bg          = if (vencido) "#FDECEA" else "#FFFFFF",
          header = tagList(.badge_canal(dat$Canal[i]), " ",
                           tags$strong(dat$Asesor[i]), " — ",
                           format(dat$FechaRecordatorio[i], "%d/%m/%Y %H:%M")),
          extra_header = racafeForms::racafeButton(id = ns(paste0("REC_Cumplir_", dat$IdRecordatorio[i])),
                                                   label = if (cumplido) NULL else "Marcar cumplido",
                                                   icon = if (cumplido) "check-double" else "check", size = "xxs",
                                                   variant = if (cumplido) "success" else "primary"),
          cuerpo = cuerpo_ui
        )
      }))
    })
    
    # Escritura real de "cumplido" — extraida para no duplicarla entre el
    # flujo normal y cualquier reintento futuro.
    .rc_marcar_cumplido <- function(id_rec) {
      fila_actual <- listar_recordatorio(codigo_contacto()) %>%
        filter(IdRecordatorio == id_rec)
      if (nrow(fila_actual) == 0) return(invisible(NULL))
      if (isTRUE(fila_actual$Enviado[1] == 1)) return(invisible(NULL))  # ya cumplido, evita doble-escritura
      
      fila_actual$Enviado <- 1
      fila_actual$FechaHoraEnvio <- Sys.time()
      tryCatch({
        racafeBD::ReemplazarDatos(fila_actual, "CRMNALRECORDATORIO", list(IdRecordatorio = id_rec))
        recordatorio_trigger(recordatorio_trigger() + 1)
        racafeForms::racafeNotify(duration = 3, type = "success", title = "Recordatorio marcado como cumplido",
                                session = shiny::getDefaultReactiveDomain())
      }, error = function(e) racafeForms::racafeNotify(duration = 5, type = "error",
                              title = paste("Error al marcar el recordatorio como cumplido:", conditionMessage(e)),
                              session = shiny::getDefaultReactiveDomain()))
    }
    
    # Registro dinamico de observers para "Marcar cumplido" + su confirmacion.
    # Reutiliza recordatorios_data() en vez de volver a consultar la BD.
    observe({
      dat <- recordatorios_data()
      rv_obs$cumplir  <- .rc_limpiar_observers(rv_obs$cumplir, dat$IdRecordatorio)
      rv_obs$confirma <- .rc_limpiar_observers(rv_obs$confirma, dat$IdRecordatorio)
      
      lapply(dat$IdRecordatorio, function(id_rec) {
        btn_id <- paste0("REC_Cumplir_", id_rec)
        
        # Click en "Marcar cumplido" -> abre modal, no escribe directo
        if (is.null(rv_obs$cumplir[[btn_id]])) {
          rv_obs$cumplir[[btn_id]] <- observeEvent(input[[btn_id]], {
            # Relectura puntual filtrada por llave — ya es barata gracias al
            # filtro SQL de listar_recordatorio(); necesaria porque el estado
            # pudo cambiar entre el render y el click.
            fila_actual <- listar_recordatorio(codigo_contacto()) %>% filter(IdRecordatorio == id_rec)
            if (nrow(fila_actual) == 0 || isTRUE(fila_actual$Enviado[1] == 1)) return(invisible(NULL))
            
            .confirmacion$abrir(
              ns = ns, titulo = "Confirmar cumplimiento",
              texto = paste0("¿Confirma que el recordatorio para ", fila_actual$Asesor[1], " fue atendido?"),
              id_cancelar = paste0("REC_CumplirCancelar_", id_rec),
              id_confirmar = paste0("REC_CumplirConfirmar_", id_rec),
              label_confirmar = "Marcar cumplido", icono_confirmar = "check"
            )
          }, ignoreInit = TRUE)
        }
        
        # Cancelar / Confirmar del modal — un wrapper con destroy() propio para
        # que .rc_limpiar_observers pueda tratarlos como una sola unidad
        confirma_id <- paste0("REC_confirma_", id_rec)
        if (is.null(rv_obs$confirma[[confirma_id]])) {
          cancelar_id  <- paste0("REC_CumplirCancelar_", id_rec)
          confirmar_id <- paste0("REC_CumplirConfirmar_", id_rec)
          
          obs_cancelar  <- observeEvent(input[[cancelar_id]], { removeModal() }, ignoreInit = TRUE)
          obs_confirmar <- observeEvent(input[[confirmar_id]], {
            if (!.confirmacion$aceptar(confirmar_id)) return(invisible(NULL))
            .rc_marcar_cumplido(id_rec)
            removeModal()
          }, ignoreInit = TRUE)
          
          rv_obs$confirma[[confirma_id]] <- list(
            destroy = function() { obs_cancelar$destroy(); obs_confirmar$destroy() }
          )
        }
      })
    })
    
    validacion_recordatorio <- .accion_validacion(input, session, codigo_contacto,
      c("REC_Asesor", "REC_Canal", "REC_Fecha", "REC_Fecha", "REC_Mensaje"), function() c(
        "Seleccione el asesor" = EsVacio(input$REC_Asesor),
        "Seleccione el canal" = EsVacio(input$REC_Canal),
        "Seleccione una fecha y hora futura" = !isTRUE(input$REC_Fecha > Sys.time()),
        "El horario permitido es de 07:00 a 17:59" = !isTRUE(lubridate::hour(input$REC_Fecha) %in% 7:17),
        "El mensaje es obligatorio" = EsVacio(input$REC_Mensaje)))
    observeEvent(input$REC_Programar, {
      if (!validacion_recordatorio$validate()) return(invisible(NULL))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar recordatorio",
        texto = paste0("¿Desea programar este recordatorio para ", input$REC_Asesor,
                       " el ", format(input$REC_Fecha, "%d/%m/%Y %H:%M"), "?"),
        id_cancelar = "REC_CancelarProgramar", id_confirmar = "REC_ConfirmarProgramar",
        label_confirmar = "Programar", icono_confirmar = "calendar"
      )
    })
    observeEvent(input$REC_CancelarProgramar, { .confirmacion$cancelar("REC_ConfirmarProgramar"); removeModal() })
    observeEvent(input$REC_ConfirmarProgramar, {
      if (!.confirmacion$aceptar("REC_ConfirmarProgramar", validacion_recordatorio$validate)) return(invisible(NULL))
      tryCatch({
        registrar_recordatorio(codigo_contacto(), input$REC_Asesor, input$REC_Fecha,
                               input$REC_Canal, input$REC_Mensaje, usuario())
        removeModal()
        updateTextAreaInput(session, "REC_Mensaje", value = "")
        recordatorio_trigger(recordatorio_trigger() + 1)
        racafeForms::racafeNotify(duration = 3, type = "success", title = "Recordatorio programado exitosamente",
                                session = shiny::getDefaultReactiveDomain())
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste("Error al programar el recordatorio:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    # Retorno ----
    list(gestiones = reactive(gestion_trigger()))
  })
}

# App de prueba ----
DemoAccionesRelacionamiento <- function() {
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
        RelacionamientoUI("AccionEditar")
      )
    )
  )

  server <- function(input, output, session) {
    usuario_sesion <- reactive("CMEDINA")
    codigo_contacto_prueba <- reactive(input$CodigoContactoPrueba)
    Relacionamiento("AccionEditar", usuario_sesion, codigo_contacto_prueba)
  }

  shinyApp(ui, server)
}
