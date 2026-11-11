# Descarte confirmado desde contacto o tablero ----
DescartarOportunidadUI <- function(id) {
  ns <- shiny::NS(id)
  .accion_formulario_ui(id, racafeForms::racafeForm(variant = "plain", shiny::uiOutput(ns("ResumenOportunidad")),
    .crm_select("DES_Razon", "Razón de descarte", choices = .RAZONES_DESCARTE_OPORTUNIDAD,
      selected = NULL, ns = ns),
    racafeForms::racafeInputTextArea("DES_Comentario", "Comentario adicional", rows = 3, ns = ns),
    racafeForms::racafeButton("DES_Solicitar", "Descartar oportunidad", icon = "ban", ns = ns)))
}
DescartarOportunidad <- function(id, usuario, id_oportunidad) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, id_oportunidad, usuario)
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    oportunidad_actual <- reactive({
      req(id_oportunidad())
      cond <- sprintf("IdOportunidad = '%s'", .rc_escapar_sql(id_oportunidad()))
      CargarDatos("CRMNALCLOPT", condicion = cond)
    })
    
    output$ResumenOportunidad <- renderUI({
      req(nrow(oportunidad_actual()) > 0)
      op <- oportunidad_actual()
      .rc_card(
        borde_color = "#94A3B8", header = paste0(op$Categoria[[1]], " — ", op$Producto[[1]]),
        cuerpo = paste0("Sacos: ", op$Sacos[[1]], " | Margen: ", op$Margen[[1]],
                        " | Cumplimiento: ", format(op$FechaCumpOP[[1]], "%d/%m/%Y"))
      )
    })
    
    motivo_final <- reactive({
      comentario <- trimws(input$DES_Comentario %||% "")
      if (EsVacio(comentario)) input$DES_Razon else paste0(input$DES_Razon, " | ", comentario)
    })
    
    observeEvent(list(id_oportunidad(), input$FormularioListo), {
      req(input$FormularioListo)
      .crm_update_select(session, "DES_Razon", selected = character())
      updateTextAreaInput(session, "DES_Comentario", value = "")
    })
    validacion <- .accion_validacion(input, session, id_oportunidad, "DES_Razon",
      function() c("Seleccione la razón de descarte" = EsVacio(input$DES_Razon)))
    observeEvent(input$DES_Solicitar, {
      if (!validacion$validate()) return(invisible(NULL))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar descarte de oportunidad",
        texto = paste0("¿Deseas descartar esta oportunidad? Motivo: ", motivo_final()),
        id_cancelar = "DES_Cancelar", id_confirmar = "DES_Confirmar",
        label_confirmar = "Descartar", icono_confirmar = "ban"
      )
    })
    
    observeEvent(input$DES_Cancelar, { .confirmacion$cancelar("DES_Confirmar"); removeModal() })
    
    observeEvent(input$DES_Confirmar, {
      if (!.confirmacion$aceptar("DES_Confirmar", validacion$validate)) return(invisible(NULL))
      tryCatch({
        descartar_oportunidad(id_oportunidad(), motivo_final(), usuario())
        removeModal()
        racafeForms::racafeNotify(duration = 4, type = "success", title = "Oportunidad descartada exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        updateTextAreaInput(session, "DES_Comentario", value = "")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste0("Error al descartar la oportunidad: ", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    list(n = reactive(ret()))
  })
}

# App de prueba ----
DemoAccionesDescartarOportunidad <- function() {
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
        racafeForms::racafeInputText(id = "id_oportunidad", label = "Codigo de Contacto a editar", value = ""),
        DescartarOportunidadUI("AccionEditar")
      )
    )
  )

  server <- function(input, output, session) {
    usuario_sesion <- reactive("CMEDINA")
    id_oportunidad <- reactive(input$id_oportunidad)
    DescartarOportunidad("AccionEditar", usuario = usuario_sesion, id_oportunidad = id_oportunidad)
  }

  shinyApp(ui, server)
}
