# Descartar.
# Modulos Auxiliares ----
FormularioDescartarUI <- function(id, etapa) {
  ns       <- NS(id)
  etiqueta <- .ETIQUETA_DESCARTE_ETAPA[[etapa]] %||% "Registro"
  tagList(
    box(title = paste("Descartar", etiqueta), width = 12, collapsible = FALSE,
        .crm_select(id = ns("DES_Razon"), label = Obligatorio("Razón de Descarte"),
                                choices = .RAZONES_DESCARTE[[etapa]], selected = NULL, multiple = FALSE, empty = FALSE),
        uiOutput(ns("DetalleRazon")),
        uiOutput(ns("DetalleOtro")),
        racafeForms::racafeButton(id = ns("DES_Solicitar"), label = paste("Descartar", etiqueta), size = "xs",
                                variant = "primary", icon = "ban")
    )
  )
}
FormularioDescartar <- function(id, usuario, cod_contacto, etapa, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    ns       <- session$ns
    ret      <- reactiveVal(0)
    etiqueta <- .ETIQUETA_DESCARTE_ETAPA[[etapa]] %||% "registro"
    
    .data_contacto <- if (!is.null(data_contacto)) {
      data_contacto
    } else {
      reactive({
        req(cod_contacto())
        cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto()))
        CargarDatos("CRMNALCONTACTO", condicion = cond)
      })
    }
    
    detalle_config <- reactive({
      req(input$DES_Razon)
      if (identical(input$DES_Razon, "OTRAS")) {
        list(tipo = "texto", label = "Especifique la razón",
             placeholder = "Describa el motivo del descarte")
      } else {
        .detalle_config_razon(input$DES_Razon)
      }
    })
    
    # Choices resueltas de la lista vigente (evita recalcular Choices()/BD
    # dos veces — una para pintar el select y otra para detectar "Otro...")
    choices_lista <- reactive({
      cfg <- detalle_config()
      req(identical(cfg$tipo, "lista"))
      if (is.function(cfg$choices)) cfg$choices() else cfg$choices
    })
    
    # Detecta la opcion "Otro.../Otra..." al final de un catalogo, si
    # existe. Generico: no requiere marcar cada entrada de
    # .DETALLE_RAZON_DESCARTE manualmente
    .opcion_otro <- function(choices) {
      ultimo <- utils::tail(choices, 1)
      if (length(ultimo) == 1 && grepl("^Otr[ao]", ultimo, ignore.case = TRUE)) ultimo else NULL
    }
    
    # TRUE cuando la opcion "Otro..." de la lista vigente esta seleccionada
    muestra_detalle_otro <- reactive({
      cfg <- detalle_config()
      if (!identical(cfg$tipo, "lista")) return(FALSE)
      otro <- .opcion_otro(choices_lista())
      !is.null(otro) && any(input$DES_Detalle %in% otro)
    })
    
    output$DetalleRazon <- renderUI({
      cfg <- detalle_config()
      if (identical(cfg$tipo, "numero")) {
        racafeForms::racafeInputNumeric(id = "DES_Detalle", label = Obligatorio(cfg$label), value = NA, min = 0,
                                ns = ns, decimals = 0, format = "number")
      } else if (identical(cfg$tipo, "lista")) {
        .crm_select(id = ns("DES_Detalle"), label = Obligatorio(cfg$label),
                                choices = choices_lista(), selected = NULL, multiple = isTRUE(cfg$multiple),
                                empty = FALSE)
      } else {
        racafeForms::racafeInputTextArea(id = ns("DES_Detalle"), label = Obligatorio(cfg$label), value = "",
                                placeholder = cfg$placeholder, width = "100%", rows = 3)
      }
    })
    
    # Campo abierto SOLO para la opcion "Otro..." dentro de una lista —
    # unico lugar de todo el formulario (junto a la razon OTRAS) con texto
    # libre, por diseño
    output$DetalleOtro <- renderUI({
      req(muestra_detalle_otro())
      otro <- .opcion_otro(choices_lista())
      racafeForms::racafeInputTextArea(id = ns("DES_DetalleOtro"), label = Obligatorio(paste("Especifique:", otro)),
                              value = "", placeholder = "Describa el detalle", width = "100%", rows = 2)
    })
    
    # Detalle estructurado "crudo" (sin sustituir la opcion "Otro...") — es
    # el nivel 2 de la jerarquia Razon1/Razon2/Razon3, y lo reutiliza
    # motivo_final() para no duplicar el switch por cfg$tipo
    detalle_valor <- reactive({
      cfg <- detalle_config()
      if (identical(cfg$tipo, "numero")) {
        format(input$DES_Detalle %||% NA, big.mark = ",", scientific = FALSE)
      } else if (identical(cfg$tipo, "lista")) {
        paste(input$DES_Detalle %||% character(0), collapse = "; ")
      } else {
        trimws(input$DES_Detalle %||% "")
      }
    })
    
    # Nivel 3 de la jerarquia: el texto libre de "Otro...", solo cuando esa
    # opcion esta seleccionada dentro de la lista
    razon3_valor <- reactive({
      if (!muestra_detalle_otro()) return(NA_character_)
      trimws(input$DES_DetalleOtro %||% "")
    })
    
    motivo_final <- reactive({
      cfg   <- detalle_config()
      otro  <- .opcion_otro(choices_lista())
      items <- input$DES_Detalle %||% character(0)
      # Sustituye la etiqueta generica "Otro X" por "Otro X: <detalle escrito>"
      detalle <- if (identical(cfg$tipo, "lista") && !is.null(otro) && otro %in% items) {
        items[items == otro] <- paste0(otro, ": ", razon3_valor())
        paste(items, collapse = "; ")
      } else {
        detalle_valor()
      }
      if (identical(input$DES_Razon, "OTRAS")) detalle else paste0(input$DES_Razon, " | ", cfg$label, ": ", detalle)
    })
    
    validar_campos_descarte <- function() {
      cfg <- detalle_config()
      detalle_vacio <- if (identical(cfg$tipo, "numero")) {
        is.null(input$DES_Detalle) || is.na(input$DES_Detalle)
      } else if (identical(cfg$tipo, "lista")) {
        length(input$DES_Detalle) == 0
      } else {
        EsVacio(input$DES_Detalle)
      }
      otro_vacio <- muestra_detalle_otro() && EsVacio(input$DES_DetalleOtro)
      c(
        "El campo Razón de Descarte es obligatorio" = EsVacio(input$DES_Razon),
        setNames(detalle_vacio, paste0("El campo '", cfg$label, "' es obligatorio")),
        "Debe especificar el detalle de la opción 'Otro'" = otro_vacio
      )
    }
    
    observeEvent(input$DES_Solicitar, {
      cond <- validar_campos_descarte()
      if (any(cond)) {
        sapply(names(cond), function(x) {
          if (cond[[x]]) racafeForms::racafeNotify(session, title = x, type = "error", duration = 4)
        })
        return(invisible(NULL))
      }
      racafeShiny::MostrarModalConfirmacion(
        ns = ns, titulo = paste("Confirmar descarte de", etiqueta),
        texto = paste0("¿Deseas descartar este ", tolower(etiqueta), "? Motivo: ", motivo_final()),
        id_cancelar = "DES_Cancelar", id_confirmar = "DES_Confirmar",
        label_confirmar = paste("Descartar", etiqueta), icono_confirmar = "ban"
      )
    })
    
    observeEvent(input$DES_Cancelar, { removeModal() })
    
    observeEvent(input$DES_Confirmar, {
      tryCatch({
        # Razon1/Razon2/Razon3 se pasan explicitas (en vez de dejar que
        # descartar_generico() infiera una unica razon del texto de
        # motivo_final()) — evita ambiguedad cuando el detalle escrito por
        # el usuario coincide por casualidad con otra razon del catalogo, y
        # persiste la jerarquia completa en CRMNALDESCARTE.Razon1/2/3
        descartar_generico(cod_contacto(), etapa, motivo_final(), usuario(), razon1 = input$DES_Razon,
                           razon2 = detalle_valor(), razon3 = razon3_valor())
        removeModal()
        racafeForms::racafeNotify(duration = 4, type = "success", title = paste(etiqueta, "descartado exitosamente"),
                                session = shiny::getDefaultReactiveDomain())
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste0("Error al descartar el ", tolower(etiqueta), ": ", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    list(n = reactive(ret()))
  })
}

# Modulo Principal ----
DescartarUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    uiOutput(ns("EstadoActual")),
    uiOutput(ns("Contenido"))
  )
}
Descartar <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    trigger_refresco <- reactiveVal(0)
    
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
    
    modulo_descartar_contacto  <- FormularioDescartar("DescartarContacto", usuario, codigo_contacto,
                                                      etapa = "CONTACTO", data_contacto = data_contacto)
    modulo_descartar_prospecto <- FormularioDescartar("DescartarProspecto", usuario, codigo_contacto,
                                                      etapa = "PROSPECTO", data_contacto = data_contacto)
    modulo_descartar_lead      <- FormularioDescartar("DescartarLead", usuario, codigo_contacto,
                                                      etapa = "LEAD", data_contacto = data_contacto)
    modulo_descartar_cliente   <- FormularioDescartar("DescartarCliente", usuario, codigo_contacto,
                                                      etapa = "CLIENTE", data_contacto = data_contacto)
    
    observeEvent(modulo_descartar_contacto$n(),  { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_descartar_prospecto$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_descartar_lead$n(),      { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_descartar_cliente$n(),   { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    
    output$Contenido <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa))
      
      switch(etapa,
             "CONTACTO"   = FormularioDescartarUI(ns("DescartarContacto"), etapa = "CONTACTO"),
             "PROSPECTO"  = FormularioDescartarUI(ns("DescartarProspecto"), etapa = "PROSPECTO"),
             "LEAD"       = FormularioDescartarUI(ns("DescartarLead"), etapa = "LEAD"),
             "CLIENTE"    = FormularioDescartarUI(ns("DescartarCliente"), etapa = "CLIENTE"),
             "DESCARTADO" = FormatearTexto("Este registro ya está descartado. Usa el módulo de Reactivación para restaurarlo.",
                                           negrita = TRUE, color = "#6c757d", tamano_pct = 0.9),
             FormatearTexto("Etapa no reconocida para este contacto.", negrita = TRUE,
                            color = COLOR_ERROR, tamano_pct = 0.9)
      )
    })
    
    list(etapa = etapa_contacto, actualizaciones = reactive(trigger_refresco()))
  })
}

# App de prueba ----
DemoAccionesDescartar <- function() {
  ui <- bs4DashPage(
    title = "Prueba Accion Descartar",
    header = bs4DashNavbar(),
    sidebar = bs4DashSidebar(),
    controlbar = bs4DashControlbar(),
    footer = bs4DashFooter(),
    body = bs4DashBody(
      useShinyjs(),
      includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                        "refs/heads/main/Styles/style.css")),
      box(
        title = "Descartar Contacto (prueba)",
        width = 12,
        racafeForms::racafeInputText(id = "CodigoContactoPrueba", label = "Codigo de Contacto a descartar", value = ""),
        DescartarUI("AccionDescartar")
      )
    )
  )

  server <- function(input, output, session) {
    usuario_sesion         <- reactive("CMEDINA")
    codigo_contacto_prueba <- reactive(input$CodigoContactoPrueba)
    Descartar("AccionDescartar", usuario = usuario_sesion, codigo_contacto = codigo_contacto_prueba)
  }

  shinyApp(ui, server)
}
