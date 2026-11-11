# Crear oportunidades desde el contacto o desde el tablero comercial ----
CrearOportunidadUI <- function(id) {
  ns <- shiny::NS(id)
  .accion_formulario_ui(id, racafeForms::racafeForm(variant = "plain",
    shiny::uiOutput(ns("Encabezado")),
    racafeForms::racafeCard(title = "Producto",
      .crm_select("OP_LinNeg", "Línea de negocio", choices = c("", Choices()$linneg), ns = ns),
      racafeForms::racafeFormRow(columns = 2,
        .crm_select("OP_Categoria", "Categoría", choices = "", ns = ns),
        .crm_select("OP_Producto", "Producto", choices = "", ns = ns))),
    racafeForms::racafeCard(title = "Objetivo de la oportunidad",
      racafeForms::racafeFormRow(columns = 2,
        racafeForms::racafeInputDate("OP_Fecha", "Fecha de cumplimiento", value = Sys.Date() + 7,
          min = Sys.Date(), ns = ns), shiny::uiOutput(ns("OP_Sacos_UI"))),
      racafeForms::racafeFormRow(columns = 2,
        racafeForms::racafeInputNumeric("OP_Frecuencia", "Frecuencia (días)", value = NA, format = "number", ns = ns),
        racafeForms::racafeInputNumeric("OP_Margen", "Margen por kilo", value = NA, format = "currency", ns = ns)),
      shiny::uiOutput(ns("OP_Resumen"))),
    racafeForms::racafeFormFooter(racafeForms::racafeButton("OP_Crear", "Crear oportunidad",
      icon = "plus", ns = ns))))
}
CrearOportunidad <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, codigo_contacto, usuario)
    
    .accion_encabezado(output, codigo_contacto)
    ns <- session$ns
    creacion_trigger <- reactiveVal(0)
    creacion_pendiente <- reactiveVal(NULL)
    
    observeEvent(list(codigo_contacto(), input$FormularioListo), {
      req(input$FormularioListo)
      creacion_pendiente(NULL)
      .crm_update_select(session, "OP_LinNeg", selected = character())
      for (campo in c("OP_Sacos", "OP_Frecuencia", "OP_Margen")) {
        racafeForms::racafeUpdateNumeric(session, campo, value = NA_real_)
      }
    })
    # Datos ----
    
    # Catalogo completo de productos; no depende del contacto, se carga una
    # sola vez por sesion del modulo y se reutiliza en las dos cascadas
    prods <- reactive(CargarDatos("CRMNALPRODS"))
    
    # Datos del contacto filtrados en SQL — no full scan de CRMNALCONTACTO,
    # mismo patron que data_contacto() en Relacionamiento.R
    datos_contacto <- reactive({
      req(codigo_contacto())
      cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(codigo_contacto()))
      CargarDatos("CRMNALCONTACTO", condicion = cond)
    })
    
    # Etapa vigente del contacto: misma fuente de verdad que usa Promover
    # (obtener_etapa_contacto, Generales.R) — ninguna regla de derivacion
    # se duplica aqui; solo se lee el resultado ya centralizado
    etapa_contacto <- reactive({
      req(codigo_contacto())
      obtener_etapa_contacto(codigo_contacto())
    })
    
    # Encabezado: razon social + badge de etapa (reutiliza .badge_etapa,
    # ya usado en Promover — sin CSS ni logica de color nueva)
    output$Titulo <- renderUI({
      req(nrow(datos_contacto()) > 0)
      h4(.titulo_identificacion(datos_contacto()$PerCod, datos_contacto()$PerRazSoc))
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
    
    # Actualiza categorias segun linea de negocio ----
    # CRMNALPRODS usa la columna LinNeg (texto), no CLLinNegNo; ademas se
    # excluyen combinaciones marcadas Excluir = "SI" (el modulo original
    # no aplicaba este filtro y mostraba categorias/productos invalidos)
    observeEvent(input$OP_LinNeg, {
      req(input$OP_LinNeg)
      cho_cat <- prods() %>%
        dplyr::filter(LinNeg == input$OP_LinNeg, is.na(Excluir) | Excluir != "SI") %>%
        dplyr::pull(Categoria) %>%
        Unicos()
      
      # Diagnostico defensivo: si el filtro no encuentra nada, el problema
      # es de datos (LinNeg no coincide entre Choices() y CRMNALPRODS), no
      # de actualizacion del picker — se hace visible en vez de fallar
      # en silencio
      if (length(cho_cat) == 0) {
        racafeForms::racafeNotify(duration = 5, type = "warning",
                                title = paste0("No hay categorías registradas en CRMNALPRODS para '", input$OP_LinNeg,
                                               "'"),
                                session = shiny::getDefaultReactiveDomain())
      }
      
      # selected = "" evita que el picker retenga una categoria de la
      # linea de negocio anterior que ya no existe en las nuevas choices
      .crm_update_select(session = session, choices = c("", cho_cat), selected = "", id = "OP_Categoria")
      .crm_update_select(session = session, choices = "", selected = "", id = "OP_Producto")
    })
    
    # Actualiza productos segun categoria ----
    observeEvent(input$OP_Categoria, {
      req(input$OP_Categoria)
      cho_prod <- prods() %>%
        dplyr::filter(LinNeg == input$OP_LinNeg, Categoria == input$OP_Categoria, Excluir != "SI") %>%
        dplyr::pull(Producto) %>%
        Unicos()
      
      .crm_update_select(session = session, choices = c("", cho_prod), selected = "", id = "OP_Producto")
    })
    
    # Label dinamico de sacos segun linea de negocio ----
    output$OP_Sacos_UI <- renderUI({
      label_saco <- if (!is.null(input$OP_LinNeg) && input$OP_LinNeg != "") {
        peso <- peso_saco_linneg(input$OP_LinNeg)
        if (!is.na(peso)) paste0("Sacos (", peso, " kgs)") else "Sacos"
      } else {
        "Sacos"
      }
      racafeForms::racafeInputNumeric(id = ns("OP_Sacos"), label = Obligatorio(label_saco), value = NA, decimals = 2,
                              format = "number")
    })
    
    # Resumen calculado de la oportunidad ----
    output$OP_Resumen <- renderUI({
      req(input$OP_Sacos, input$OP_LinNeg, input$OP_Frecuencia, input$OP_Margen)
      if (is.na(input$OP_Sacos) || is.na(input$OP_Frecuencia) || is.na(input$OP_Margen) ||
          input$OP_Sacos == "" || input$OP_Frecuencia == "" || input$OP_Margen == "") {
        return(.rc_card(
          borde_color = "#94A3B8", header = "Resumen",
          cuerpo = "Complete los campos para ver el resumen."
        ))
      }
      
      peso_saco <- peso_saco_linneg(input$OP_LinNeg)
      if (is.na(peso_saco)) {
        return(.rc_card(
          borde_color = "#C11007", header = "Resumen",
          cuerpo = "Línea de negocio sin peso de saco configurado."
        ))
      }
      
      sacos        <- as.numeric(input$OP_Sacos)
      frecuencia   <- as.numeric(input$OP_Frecuencia)
      margen_kg    <- as.numeric(input$OP_Margen)
      sacos_mes    <- sacos / frecuencia * 30
      margen_saco  <- margen_kg * peso_saco
      margen_total <- margen_saco * sacos
      margen_mes   <- margen_total / frecuencia * 30
      
      resumen_text <- paste0(
        "Oportunidad por ",
        FormatearNumero(sacos, formato = "coma", negrita = TRUE),
        " sacos de ",
        FormatearNumero(peso_saco, formato = "numero", negrita = TRUE),
        " kgs cada ",
        FormatearNumero(frecuencia, formato = "coma", negrita = TRUE),
        " días, es decir, ",
        FormatearNumero(sacos_mes, formato = "numero", negrita = TRUE),
        " sacos mensuales, dejando un margen por kilo de ",
        FormatearNumero(margen_kg, formato = "dinero", negrita = TRUE),
        " lo que equivale a ",
        FormatearNumero(margen_saco, formato = "dinero", negrita = TRUE),
        " por saco y un margen total de oportunidad de ",
        FormatearNumero(margen_total, formato = "dinero", negrita = TRUE),
        " y un estimado mensual de ",
        FormatearNumero(margen_mes, formato = "dinero", negrita = TRUE)
      ) %>% HTML
      
      .rc_card(borde_color = "#C11007", header = "Resumen", cuerpo = resumen_text)
    })
    
    # Crea oportunidad en BD ----
    .campos_validos <- function() {
      c(
        "El campo Línea de Negocio es obligatorio" = EsVacio(input$OP_LinNeg),
        "El campo Categoría es obligatorio"        = EsVacio(input$OP_Categoria),
        "El campo Producto es obligatorio"         = EsVacio(input$OP_Producto),
        "El campo Sacos es obligatorio"            = EsVacio(input$OP_Sacos),
        "El campo Frecuencia es obligatorio"       = EsVacio(input$OP_Frecuencia),
        "El campo Margen por kilo es obligatorio"  = EsVacio(input$OP_Margen),
        "Los sacos deben ser positivos" = !isTRUE(is.finite(input$OP_Sacos) && input$OP_Sacos > 0),
        "La frecuencia debe ser positiva" = !isTRUE(is.finite(input$OP_Frecuencia) && input$OP_Frecuencia > 0),
        "La fecha debe ser vigente" = !isTRUE(as.Date(input$OP_Fecha) >= Sys.Date())
      )
      
    }
    validacion <- .accion_validacion(input, session, codigo_contacto,
      c("OP_LinNeg", "OP_Categoria", "OP_Producto", "OP_Sacos", "OP_Frecuencia", "OP_Margen",
        "OP_Sacos", "OP_Frecuencia", "OP_Fecha"), .campos_validos)
    observeEvent(input$OP_Crear, {
      if (!validacion$validate()) return(invisible(NULL))

      # Codigo y nombre de linea de negocio — reutiliza determinar_linea_negocio()
      # (Generales.R, compartida con Promover) en vez de duplicar el mapeo.
      # linea$cod llega como texto ("21000"); se convierte a numerico porque
      # LinNegCod es numerico tanto en CRMNALPRODS como en CRMNALCLOPT
      linea     <- determinar_linea_negocio(input$OP_LinNeg)
      linnegcod <- as.numeric(linea$cod)
      
      if (is.na(linnegcod)) {
        racafeForms::racafeNotify(duration = 6, type = "error",
                                title = paste0("La línea de negocio '", input$OP_LinNeg, "' no está mapeada en ",
                 "determinar_linea_negocio(). Contacte a Sistemas antes de continuar."),
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      
      creacion_pendiente(list(contacto = codigo_contacto(), linea = linnegcod,
        categoria = input$OP_Categoria, producto = input$OP_Producto, fecha = input$OP_Fecha,
        sacos = input$OP_Sacos, margen = input$OP_Margen, frecuencia = input$OP_Frecuencia))
      .confirmacion$abrir(
        ns = ns, titulo = "Confirmar creación",
        texto = paste0("¿Desea registrar esta oportunidad de '", input$OP_Producto, "'?"),
        id_cancelar = "OP_CancelarCreacion", id_confirmar = "OP_ConfirmarCreacion",
        label_confirmar = "Crear", icono_confirmar = "floppy-disk"
      )
    })
    
    observeEvent(input$OP_CancelarCreacion, { .confirmacion$cancelar("OP_ConfirmarCreacion"); creacion_pendiente(NULL); removeModal() })
    observeEvent(input$OP_ConfirmarCreacion, {
      if (!.confirmacion$aceptar("OP_ConfirmarCreacion", validacion$validate)) return(invisible(NULL))
      pendiente <- creacion_pendiente()
      req(pendiente, identical(pendiente$contacto, codigo_contacto()))
      creacion_pendiente(NULL)
      # Etapa capturada al confirmar — fotografia historica; no debe
      # recalcularse despues aunque el contacto avance de etapa mas adelante
      etapa_al_crear <- etapa_contacto()
      
      tryCatch({
        registrar_oportunidad(
          cod_contacto        = pendiente$contacto,
          linnegcod            = pendiente$linea,
          categoria            = pendiente$categoria,
          producto             = pendiente$producto,
          fecha_cumplimiento   = pendiente$fecha,
          sacos                = pendiente$sacos,
          margen               = pendiente$margen,
          frecuencia          = pendiente$frecuencia,
          etapa                = etapa_al_crear,
          usr                  = usuario()
        )
        removeModal()
        racafeForms::racafeNotify(duration = 3, type = "success", title = "Oportunidad registrada exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        
        inputs_reset <- c("OP_LinNeg", "OP_Categoria", "OP_Producto",
                          "OP_Fecha", "OP_Sacos", "OP_Frecuencia", "OP_Margen")
        lapply(inputs_reset, reset)
        creacion_trigger(creacion_trigger() + 1)
      }, error = function(error) {
        removeModal()
        .ManejarErrorAccion(error = error, operacion = "crear la oportunidad",
                            usuario = usuario(), codigo_contacto = codigo_contacto())
      })
    })
    
    # Retorno — contador de creaciones, para que un modulo padre (ej. el
    # timeline de oportunidades del contacto) pueda invalidar su cache sin
    # que CrearOportunidad conozca su implementacion
    list(creaciones = reactive(creacion_trigger()))
  })
}

# App de prueba ----
DemoAccionesCrearOportunidad <- function() {
  ui <- bs4DashPage(
    title = "Prueba Acción CrearOportunidad",
    header = bs4DashNavbar(),
    sidebar = bs4DashSidebar(),
    controlbar = bs4DashControlbar(),
    footer = bs4DashFooter(),
    body = bs4DashBody(
      useShinyjs(),
      includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                        "refs/heads/main/Styles/style.css")),
      box(
        title = "Crear Oportunidad (prueba)", width = 12,
        racafeForms::racafeInputText(id = "CodigoContactoPrueba", label = "Código de Contacto", value = ""),
        CrearOportunidadUI("AccionCrearOportunidad")
      )
    )
  )

  server <- function(input, output, session) {
    usuario_sesion <- reactive("CMEDINA")
    codigo_contacto_prueba <- reactive(input$CodigoContactoPrueba)

    CrearOportunidad("AccionCrearOportunidad", usuario = usuario_sesion,
                     codigo_contacto = codigo_contacto_prueba)
  }

  shinyApp(ui, server)
}
