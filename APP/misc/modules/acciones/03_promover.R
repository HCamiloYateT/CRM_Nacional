# Promover.
# Modulos Auxiliares ----

## FormularioAscenderProspecto ----
FormularioAscenderProspectoUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    box(title = "Ascender a Prospecto", width = 12, collapsible = FALSE,
        FormatearTexto("Vincula este contacto como alianza de uno o varios clientes existentes.",
                       tamano_pct = 0.8, color = "#64748B"),
        .crm_select("PRO_Alianzas", label = Obligatorio("Cliente(s) Aliado(s)"),
                                       choices = .choices_cliente_ppal(), multiple = TRUE, ns = ns,
                                       empty_label = "Seleccione uno o varios clientes"),
        racafeForms::racafeInputTextArea(id = ns("PRO_Observacion"), label = h6("Observación"), value = "",
                                width = "100%", rows = 2),
        racafeForms::racafeButton(id = ns("PRO_Solicitar"), label = "Ascender", size = "xs", variant = "primary",
                                icon = "arrow-up")
    )
  )
}
FormularioAscenderProspecto <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, cod_contacto, usuario)
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    observeEvent(list(cod_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      .crm_update_select(session, "PRO_Alianzas", selected = character())
      updateTextAreaInput(session, "PRO_Observacion", value = "")
    })
    validacion <- .accion_validacion(input, session, cod_contacto,
      c("PRO_Alianzas"), function() c("Seleccione al menos un cliente aliado" = EsVacio(input$PRO_Alianzas)))
    observeEvent(input$PRO_Solicitar, {
      if (!validacion$validate()) return(invisible(NULL))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar ascenso a Prospecto",
        texto = paste0("¿Deseas ascender este contacto a Prospecto con ", length(input$PRO_Alianzas),
                       " alianza(s) registrada(s)?"),
        id_cancelar = "PRO_Cancelar", id_confirmar = "PRO_Confirmar",
        label_confirmar = "Ascender a Prospecto", icono_confirmar = "arrow-up"
      )
    })
    observeEvent(input$PRO_Cancelar, { .confirmacion$cancelar("PRO_Confirmar"); removeModal() })
    observeEvent(input$PRO_Confirmar, {
      if (!.confirmacion$aceptar("PRO_Confirmar", validacion$validate)) return(invisible(NULL))
      tryCatch({
        convertir_contacto_a_prospecto(cod_contacto(), input$PRO_Alianzas, usuario(), input$PRO_Observacion)
        removeModal()
        racafeForms::racafeNotify(duration = 4, type = "success", title = "Contacto ascendido a Prospecto exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste("Error al ascender el contacto:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    list(n = reactive(ret()))
  })
}

## FormularioAscenderLead ----
FormularioAscenderLeadUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    box(title = "Ascender a Lead", width = 12, collapsible = FALSE,
        .crm_select(id = ns("ASC_LinNeg"), label = Obligatorio("Línea de Negocio"),
                                choices = Choices()$linneg, selected = NULL, multiple = FALSE, empty = FALSE),
        .crm_select(id = ns("ASC_Segmento"), label = Obligatorio("Segmento"),
                                choices = Choices()$segmento, selected = NULL, multiple = FALSE, empty = FALSE),
        .crm_select(id = ns("ASC_Asesor"), label = Obligatorio("Asesor"),
                                choices = Choices()$personas, selected = NULL, multiple = FALSE, empty = FALSE),
        racafeForms::racafeButton(id = ns("ASC_Solicitar"), label = "Ascender", size = "xs", variant = "primary",
                                icon = "arrow-up")
    )
  )
}
FormularioAscenderLead <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, cod_contacto, usuario)
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    .data_contacto <- if (!is.null(data_contacto)) {
      data_contacto
    } else {
      reactive({
        req(cod_contacto())
        cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto()))
        CargarDatos("CRMNALCONTACTO", condicion = cond)
      })
    }
    
    validar_campos_ascenso <- function() {
      c(
        "El campo Asesor es obligatorio" = EsVacio(input$ASC_Asesor),
        "El campo Segmento es obligatorio" = EsVacio(input$ASC_Segmento),
        "El campo Línea de Negocio es obligatorio" = EsVacio(input$ASC_LinNeg)
      )
    }
    observeEvent(list(cod_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      .crm_update_select(session, "ASC_Asesor", selected = character())
      .crm_update_select(session, "ASC_Segmento", selected = character())
      .crm_update_select(session, "ASC_LinNeg", selected = character())
    })
    validacion <- .accion_validacion(input, session, cod_contacto,
      c("ASC_Asesor", "ASC_Segmento", "ASC_LinNeg"), validar_campos_ascenso)
    observeEvent(input$ASC_Solicitar, {
      if (!validacion$validate()) return(invisible(NULL))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar ascenso a Lead",
        texto = paste0("¿Deseas ascender este contacto a Lead con asesor ", input$ASC_Asesor,
                       ", segmento ", input$ASC_Segmento, " y línea de negocio ", input$ASC_LinNeg, "?"),
        id_cancelar = "ASC_Cancelar", id_confirmar = "ASC_Confirmar",
        label_confirmar = "Ascender a Lead", icono_confirmar = "arrow-up"
      )
    })
    observeEvent(input$ASC_Cancelar, { .confirmacion$cancelar("ASC_Confirmar"); removeModal() })
    observeEvent(input$ASC_Confirmar, {
      if (!.confirmacion$aceptar("ASC_Confirmar", validacion$validate)) return(invisible(NULL))
      tryCatch({
        convertir_contacto_a_lead(cod_contacto(), input$ASC_Asesor, input$ASC_Segmento, input$ASC_LinNeg, usuario())
        removeModal()
        racafeForms::racafeNotify(duration = 4, type = "success", title = "Contacto ascendido a Lead exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste("Error al ascender el contacto:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    list(n = reactive(ret()))
  })
}

## FormularioReclasificarLead ----
FormularioReclasificarLeadUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    box(title = "Reclasificar a Lead", width = 12, collapsible = FALSE,
        FormatearTexto("El volumen de este Prospecto ahora sí interesa para venta directa. Las alianzas ya registradas quedan como historial.",
                       tamano_pct = 0.8, color = "#64748B"),
        .crm_select(id = ns("RLE_Asesor"), label = Obligatorio("Asesor"),
                                choices = Choices()$personas, selected = NULL, multiple = FALSE, empty = FALSE),
        .crm_select(id = ns("RLE_Segmento"), label = Obligatorio("Segmento"),
                                choices = Choices()$segmento, selected = NULL, multiple = FALSE, empty = FALSE),
        .crm_select(id = ns("RLE_LinNeg"), label = Obligatorio("Línea de Negocio"),
                                choices = Choices()$linneg, selected = NULL, multiple = FALSE, empty = FALSE),
        racafeForms::racafeButton(id = ns("RLE_Solicitar"), label = "Reclasificar a Lead", size = "xs",
                                variant = "primary", icon = "arrow-up")
    )
  )
}
FormularioReclasificarLead <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, cod_contacto, usuario)
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    .data_contacto <- if (!is.null(data_contacto)) {
      data_contacto
    } else {
      reactive({
        req(cod_contacto())
        cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto()))
        CargarDatos("CRMNALCONTACTO", condicion = cond)
      })
    }
    validar_campos_reclasificacion <- function() {
      c(
        "El campo Asesor es obligatorio" = EsVacio(input$RLE_Asesor),
        "El campo Segmento es obligatorio" = EsVacio(input$RLE_Segmento),
        "El campo Línea de Negocio es obligatorio" = EsVacio(input$RLE_LinNeg)
      )
    }
    observeEvent(list(cod_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      .crm_update_select(session, "RLE_Asesor", selected = character())
      .crm_update_select(session, "RLE_Segmento", selected = character())
      .crm_update_select(session, "RLE_LinNeg", selected = character())
    })
    validacion <- .accion_validacion(input, session, cod_contacto,
      c("RLE_Asesor", "RLE_Segmento", "RLE_LinNeg"), validar_campos_reclasificacion)
    observeEvent(input$RLE_Solicitar, {
      if (!validacion$validate()) return(invisible(NULL))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar reclasificación a Lead",
        texto = paste0("¿Deseas reclasificar este Prospecto a Lead con asesor ", input$RLE_Asesor,
                       ", segmento ", input$RLE_Segmento, " y línea de negocio ", input$RLE_LinNeg, "?"),
        id_cancelar = "RLE_Cancelar", id_confirmar = "RLE_Confirmar",
        label_confirmar = "Reclasificar a Lead", icono_confirmar = "arrow-up"
      )
    })
    observeEvent(input$RLE_Cancelar, { .confirmacion$cancelar("RLE_Confirmar"); removeModal() })
    observeEvent(input$RLE_Confirmar, {
      if (!.confirmacion$aceptar("RLE_Confirmar", validacion$validate)) return(invisible(NULL))
      tryCatch({
        convertir_prospecto_a_lead(cod_contacto(), input$RLE_Asesor, input$RLE_Segmento, input$RLE_LinNeg, usuario())
        removeModal()
        racafeForms::racafeNotify(duration = 4, type = "success", title = "Prospecto reclasificado a Lead exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste("Error al reclasificar:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    list(n = reactive(ret()))
  })
}

## FormularioVincularNit ----
FormularioVincularNitUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    box(title = "Vincular NIT de Facturación", width = 12, collapsible = FALSE,
        FormatearTexto("Úsalo cuando el lead entró con un NIT distinto al que finalmente factura.",
                       tamano_pct = 0.8, color = "#64748B"),
        racafeForms::racafeInputText(id = ns("VIN_Nit"), label = Obligatorio("NIT que Factura"), width = "100%"),
        racafeForms::racafeInputTextArea(id = ns("VIN_Observacion"), label = h6("Observación"), value = "",
                                width = "100%", rows = 2),
        racafeForms::racafeButton(id = ns("VIN_Guardar"), label = "Vincular", size = "xs", variant = "primary",
                                icon = "link")
    )
  )
}
FormularioVincularNit <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, cod_contacto, usuario)
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    .data_contacto <- if (!is.null(data_contacto)) {
      data_contacto
    } else {
      reactive({
        req(cod_contacto())
        cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto()))
        CargarDatos("CRMNALCONTACTO", condicion = cond)
      })
    }

    observeEvent(list(cod_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      v <- cargar_vinculo_nit(cod_contacto())
      updateTextInput(session, "VIN_Nit", value = if (nrow(v)) v$NitVinculado[[1]] else "")
      updateTextAreaInput(session, "VIN_Observacion", value = "")
    })
    validacion <- .accion_validacion(input, session, cod_contacto,
      c("VIN_Nit"), function() c("Ingrese un NIT válido" = EsVacio(input$VIN_Nit) || !EsEnteroPositivo(input$VIN_Nit)))
    observeEvent(input$VIN_Guardar, {
      if (!validacion$validate()) return(invisible(NULL))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar vinculación de NIT",
        texto = paste0("¿Deseas vincular el NIT ", input$VIN_Nit, " como NIT de facturación de este lead?"),
        id_cancelar = "VIN_Cancelar", id_confirmar = "VIN_Confirmar",
        label_confirmar = "Vincular", icono_confirmar = "link"
      )
    })
    
    observeEvent(input$VIN_Cancelar, { .confirmacion$cancelar("VIN_Confirmar"); removeModal() })
    
    observeEvent(input$VIN_Confirmar, {
      if (!.confirmacion$aceptar("VIN_Confirmar", validacion$validate)) return(invisible(NULL))
      tryCatch({
        registrar_vinculo_nit(cod_contacto(), input$VIN_Nit, usuario(), input$VIN_Observacion)
        removeModal()
        racafeForms::racafeNotify(duration = 3, type = "success", title = "NIT vinculado exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 5, type = "error",
                                title = paste("Error al vincular:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    list(n = reactive(ret()))
  })
}

# Modulo Principal ----
PromoverUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Encabezado")),
    uiOutput(ns("SelectorDestino")),
    uiOutput(ns("Contenido"))
  )
}
Promover <- function(id, usuario, codigo_contacto, destino_inicial = function() NULL) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, codigo_contacto, usuario)
    ns <- session$ns
    
    trigger_refresco <- reactiveVal(0)
    .accion_encabezado(output, codigo_contacto, function() { trigger_refresco(); .pr_fuentes()$revision() })
    # Destino elegido cuando la etapa vigente es CONTACTO (unica etapa con
    # mas de un formulario posible); se resetea cada vez que cambia el
    # contacto para no arrastrar la eleccion de un contacto al siguiente
    destino_contacto <- reactiveVal(NULL)
    
    # Fuente unica de datos del contacto para todo el modulo, incluidos los
    # 3 formularios hijos que antes la reconsultaban por su cuenta (punto 1)
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
    
    observeEvent(list(codigo_contacto(), destino_inicial()), {
      destino <- destino_inicial()
      destino_contacto(if (length(destino) == 1L && destino %in% c("LEAD", "PROSPECTO")) destino else NULL)
    })
    
    output$Titulo <- renderUI({
      req(nrow(data_contacto()) > 0)
      h4(.titulo_identificacion(data_contacto()$PerCod, data_contacto()$PerRazSoc))
    })
    
    # Bloque de contexto: NIT, Razon Social, Etapa vigente y mensaje de
    # destino de la promocion — previo al selector/formulario
    output$EstadoActual <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa))
      
      tags$div(
        style = "margin-bottom:14px;",
        tags$span(style = "font-size:13px; color:#374151; margin-right:6px;", "Etapa vigente:"),
        .badge_etapa(etapa),
        if (identical(etapa, "CONTACTO")) {
          tags$div(style = "margin-top:6px;",
                   FormatearTexto("Este contacto tiene dos caminos posibles: promoverlo a Lead o a Prospecto.",
                                  tamano_pct = 0.8, color = "#1C398E", negrita = TRUE))
        }
      )
    })
    
    # Selector de destino, solo visible cuando la etapa vigente es CONTACTO
    # (unica etapa con dos rutas posibles: Lead o Prospecto)
    output$SelectorDestino <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa), etapa == "CONTACTO")
      
      .crm_select(id = ns("CampoDestino"), label = Obligatorio("Promover a"),
                              choices = c("Seleccione una opción" = "", "Lead" = "LEAD", "Prospecto" = "PROSPECTO"),
                              selected = destino_contacto() %||% "", multiple = FALSE, empty = FALSE)
    })
    
    observeEvent(input$CampoDestino, {
      destino_contacto(if (identical(input$CampoDestino, "")) NULL else input$CampoDestino)
    }, ignoreNULL = FALSE)
    
    # Sub-modulos instanciados una sola vez; el renderUI solo decide cual
    # UI mostrar, no recrea servers en cada cambio de etapa/destino.
    # data_contacto se comparte con los 3 formularios que muestran titulo,
    # eliminando sus consultas independientes a CRMNALCONTACTO
    modulo_ascender_lead      <- FormularioAscenderLead("Ascender", usuario, codigo_contacto, data_contacto)
    modulo_ascender_prospecto <- FormularioAscenderProspecto("AscenderProspecto", usuario, codigo_contacto)
    modulo_reclasificar       <- FormularioReclasificarLead("Reclasificar", usuario, codigo_contacto, data_contacto)
    modulo_vincular           <- FormularioVincularNit("Vincular", usuario, codigo_contacto, data_contacto)
    
    observeEvent(modulo_ascender_lead$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_ascender_prospecto$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_reclasificar$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_vincular$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    
    output$Contenido <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa))
      
      if (etapa == "CONTACTO") {
        # Sin eleccion aun en "Promover a" -> no se pinta ningun formulario
        if (is.null(destino_contacto())) return(NULL)
        return(if (identical(destino_contacto(), "PROSPECTO")) {
          FormularioAscenderProspectoUI(ns("AscenderProspecto"))
        } else {
          FormularioAscenderLeadUI(ns("Ascender"))
        })
      }
      
      switch(etapa,
             "PROSPECTO"  = FormularioReclasificarLeadUI(ns("Reclasificar")),
             "LEAD"       = FormularioVincularNitUI(ns("Vincular")),
             "CLIENTE"    = FormatearTexto("Este contacto ya es Cliente.", negrita = TRUE,
                                           color = "#198754", tamano_pct = 0.9),
             "DESCARTADO" = FormatearTexto("Este contacto está descartado; reactívelo antes de promoverlo.",
                                           negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.9),
             FormatearTexto("Etapa no reconocida para este contacto.", negrita = TRUE,
                            color = COLOR_ERROR, tamano_pct = 0.9)
      )
    })
    
    list(etapa = etapa_contacto, actualizaciones = reactive(trigger_refresco()))
  })
}

# App de prueba ----
DemoAccionesPromover <- function() {
  ui <- bs4DashPage(
    title = "Prueba Accion Promover",
    header = bs4DashNavbar(),
    sidebar = bs4DashSidebar(),
    controlbar = bs4DashControlbar(),
    footer = bs4DashFooter(),
    body = bs4DashBody(
      useShinyjs(),
      includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                        "refs/heads/main/Styles/style.css")),
      box(
        title = "Promover Contacto (prueba)",
        width = 12,
        racafeForms::racafeInputText(id = "CodigoContactoPrueba", label = "Codigo de Contacto a promover", value = ""),
        PromoverUI("AccionPromover")
      )
    )
  )

  server <- function(input, output, session) {
    usuario_sesion <- reactive("CMEDINA")
    codigo_contacto_prueba <- reactive(input$CodigoContactoPrueba)
    Promover("AccionPromover", usuario = usuario_sesion, codigo_contacto = codigo_contacto_prueba)
  }

  shinyApp(ui, server)
}
