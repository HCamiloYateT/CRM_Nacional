# Kanban 
KanbanEmbudoUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$script(HTML(JS_KANBAN_EMBUDO))),
    useShinyjs(),
    # Filtros — Origen y Detalle Origen anidados (Detalle depende del
    # Origen elegido), mismo patron que DetalleCreacionMensual/
    # DetalleAnalisisConversion (Generales.R). Los 3 campos son
    # ListaDesplegable (pickerInput) por homogeneidad; en fluidRow/column
    # en vez de flex con anchos fijos, para que no se solapen al reducir
    # el viewport
    div(
      class = "kpb-filtros",
      style = "display:flex; align-items:flex-end; gap:10px; flex-wrap:wrap;",
      
      div(style = "width:250px;", uiOutput(ns("Origen_ui"))),
      div(style = "width:250px;", uiOutput(ns("DetOrigen_ui"))),
      div(style = "width:250px;", uiOutput(ns("Asesor_ui")))
    ),
    uiOutput(ns("kanban_board")),
    tags$script(HTML(
      "document.addEventListener('click', function(e){
    document.querySelectorAll('.rc-panel-acciones').forEach(function(p){
    if(!p.contains(e.target) && !e.target.closest('.rc-boton-acciones')){
    p.style.display='none';
    }
    });
   });"
    ))
  )
}
KanbanEmbudo <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    refresh_trigger      <- reactiveVal(0)
    codigo_seleccionado  <- reactiveVal(NULL)
    accion_seleccionada  <- reactiveVal(NULL)
    titulo_modal_actual  <- reactiveVal(NULL)
    
    # Estado del filtro "solo alertas", uno por columna (punto 1) — ya no
    # es un switch global; cada columna CONTACTO/LEAD tiene su propio
    # boton-badge que alterna entre "todas" y "solo alertas"
    filtro_alertas_col <- reactiveValues(CONTACTO = FALSE, LEAD = FALSE)
    observeEvent(input$ToggleAlertas_CONTACTO, { filtro_alertas_col$CONTACTO <- !filtro_alertas_col$CONTACTO })
    observeEvent(input$ToggleAlertas_LEAD,     { filtro_alertas_col$LEAD     <- !filtro_alertas_col$LEAD })
    
    # detectar_conversion_leads() es una dependencia externa (definida junto
    # a EmbudoLeads.R, no en este archivo)
    observeEvent(refresh_trigger(), { detectar_conversion_leads(usuario()) }, ignoreNULL = FALSE)
    
    # Datos ----
    pipeline_raw <- reactive({
      refresh_trigger()
      cargar_pipeline_embudo()
    })
    
    # Origen / Detalle Origen / Asesor — los 3 como ListaDesplegable
    # (pickerInput), mismo patron que DetalleCreacionMensual (Generales.R):
    # render directo para pintar las choices correctas desde el primer
    # paint, en vez de selectInput + updateSelectInput (que ademas
    # generaba el desalineado visual entre Asesor y Origen/DetOrigen)
    output$Origen_ui <- renderUI({
      origenes <- pipeline_raw()$Origen %>% unique() %>% sort()
      ListaDesplegable(ns("filtro_origen"), label = h6("Origen"),
                       choices = c("Todos", origenes), selected = "Todos", multiple = FALSE, size = 8)
    })
    output$DetOrigen_ui <- renderUI({
      req(input$filtro_origen)
      det <- if (identical(input$filtro_origen, "Todos")) {
        pipeline_raw()$DetOrigen
      } else {
        pipeline_raw() %>% filter(Origen == input$filtro_origen) %>% pull(DetOrigen)
      }
      det <- det[!is.na(det)] %>% unique() %>% sort()
      ListaDesplegable(ns("filtro_detorigen"), label = h6("Detalle Origen"),
                       choices = c("Todos", det), selected = "Todos", multiple = FALSE, size = 8)
    })
    output$Asesor_ui <- renderUI({
      asesores <- pipeline_raw()$Asesor %>% unique() %>% sort()
      ListaDesplegable(ns("filtro_asesor"), label = h6("Asesor"),
                       choices = c("Todos", asesores), selected = "Todos", multiple = FALSE, size = 8)
    })
    
    pipeline_filtrado <- reactive({
      dat <- pipeline_raw()
      if (!is.null(input$filtro_origen) && input$filtro_origen != "Todos") dat <- dat %>% filter(Origen == input$filtro_origen)
      if (!is.null(input$filtro_detorigen) && input$filtro_detorigen != "Todos") dat <- dat %>% filter(DetOrigen == input$filtro_detorigen)
      if (!is.null(input$filtro_asesor) && input$filtro_asesor != "Todos") dat <- dat %>% filter(Asesor == input$filtro_asesor)
      dat
    })
    
    # Metricas de conversion para el header de la columna CLIENTE (punto 5):
    # % Lead->Cliente y % Contacto->Cliente. Se calculan sobre el universo
    # SIN filtrar (pipeline_raw), igual que hacia el antiguo kpi_conversion,
    # para que no varien con los filtros de Origen/Asesor de la vista
    metricas_conversion_r <- reactive({
      total_contactos_historico <- nrow(pipeline_raw())
      total_leads_historico     <- tryCatch(nrow(CargarDatos("CONTACTOLEAD")), error = function(e) 0)
      total_clientes            <- pipeline_raw() %>% filter(Etapa == "CLIENTE") %>% nrow()
      list(
        pct_lead_cliente     = if (total_leads_historico > 0) round(total_clientes / total_leads_historico * 100) else 0,
        pct_contacto_cliente = if (total_contactos_historico > 0) round(total_clientes / total_contactos_historico * 100) else 0
      )
    })
    
    # Board ----
    output$kanban_board <- renderUI({
      dat <- pipeline_filtrado()
      if (nrow(dat) == 0) {
        return(div(class = "kpb-board kpb-board-vacio", tags$i(class = "fas fa-inbox fa-2x"),
                   tags$p("Sin registros con los filtros activos.")))
      }
      
      construir_columna <- function(etapa_col, boton_extra = NULL, badges_extra = NULL) {
        filas_todas <- dat %>% filter(Etapa == etapa_col)
        n_alertas <- if (etapa_col %in% c("CONTACTO", "LEAD")) sum(filas_todas$EstadoGestion == "critical") else 0
        
        # El boton-toggle de alertas filtra SOLO su propia columna, no el
        # board completo (punto 1) — cada columna mantiene su estado
        # independiente en filtro_alertas_col
        filas <- if (etapa_col %in% c("CONTACTO", "LEAD") && isTRUE(filtro_alertas_col[[etapa_col]])) {
          filas_todas %>% filter(EstadoGestion == "critical")
        } else {
          filas_todas
        }
        n_col <- nrow(filas)
        
        cuerpo <- if (n_col == 0) {
          div(class = "kpb-col-vacio", tags$i(class = "fas fa-inbox"), tags$span("Sin registros"))
        } else {
          do.call(tagList, lapply(seq_len(n_col), function(i) .render_tarjeta_embudo(filas[i, ])))
        }
        
        toggle_alertas <- if (etapa_col %in% c("CONTACTO", "LEAD")) {
          actionLink(ns(paste0("ToggleAlertas_", etapa_col)),
                     label = tagList(tags$i(class = "fas fa-exclamation-triangle"), " ", n_alertas),
                     class = paste0("kpb-col-alert-toggle", if (isTRUE(filtro_alertas_col[[etapa_col]])) " activo" else ""),
                     title = if (isTRUE(filtro_alertas_col[[etapa_col]])) "Mostrando solo alertas — clic para ver todas"
                     else "Mostrando todas — clic para ver solo alertas")
        } else NULL
        
        div(class = "kpb-col",
            div(class = "kpb-col-header", style = paste0("background:", COLOR_ETAPA_EMBUDO[[etapa_col]], ";"),
                div(class = "kpb-col-header-inner",
                    tags$span(class = "kpb-col-titulo", etapa_col),
                    div(class = "kpb-col-badges",
                        tags$span(class = "kpb-col-count", as.character(n_col)),
                        toggle_alertas,
                        badges_extra,
                        boton_extra))),
            div(class = "kpb-col-body", cuerpo))
      }
      
      boton_nuevo_contacto <- actionButton(ns("btn_nuevo_contacto"), label = tagList(icon("plus"), "Añadir"), class = "kpb-col-add")
      
      # Badges de conversion en la columna CLIENTE (punto 5): % Lead->Cliente
      # y % Contacto->Cliente, en vez del KPI global que se retiro
      metricas <- metricas_conversion_r()
      badges_cliente <- tagList(
        tags$span(class = "kpb-col-metric-badge", title = "Conversión Lead → Cliente",
                  paste0(metricas$pct_lead_cliente, "% Lead")),
        tags$span(class = "kpb-col-metric-badge", title = "Conversión Contacto → Cliente",
                  paste0(metricas$pct_contacto_cliente, "% Contacto"))
      )
      
      div(id = ns("kpb_board"), `data-kanban-ns` = ns(""), class = "kpb-board",
          construir_columna("CONTACTO", boton_extra = boton_nuevo_contacto),
          construir_columna("LEAD"),
          construir_columna("PROSPECTO"),
          construir_columna("CLIENTE", badges_extra = badges_cliente),
          construir_columna("DESCARTADO"))
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
    
    observeEvent(input$AccionSeleccionada, {
      seleccion <- input$AccionSeleccionada
      req(seleccion$codigo, seleccion$accion)
      
      if (identical(seleccion$accion, "Reactivar")) {
        # Nonce en trigger_reactivar para que el dedup interno de Reactivar
        # (clave_disparo = codigo + disparador) no bloquee un segundo click
        # sobre el mismo contacto tras cancelar el primer intento
        codigo_reactivar(seleccion$codigo)
        trigger_reactivar(isolate(trigger_reactivar()) + 1)
        return(invisible(NULL))
      }
      
      codigo_seleccionado(seleccion$codigo)
      accion_seleccionada(seleccion$accion)
      titulo_modal_actual(.CONFIG_ACCIONES_EMBUDO[[seleccion$accion]]$etiqueta %||% seleccion$accion)
      
      clase_modal <- .CONFIG_ACCIONES_EMBUDO[[seleccion$accion]]$modal %||% "subventana2"
      
      modal_construido <- modalDialog(title = titulo_modal_actual(), uiOutput(ns("ModalContenido")),
                                      easyClose = TRUE, footer = modalButton("Cerrar"))
      showModal(htmltools::tagAppendAttributes(modal_construido, class = clase_modal))
    })
    output$ModalContenido <- renderUI({
      req(accion_seleccionada())
      modulo <- .REGISTRO_MODULOS_KANBAN[[accion_seleccionada()]]
      req(modulo)
      modulo$ui()
    })
    outputOptions(output, "ModalContenido", suspendWhenHidden = FALSE)
    
    # Instanciacion unica de cada submodulo genérico
    modulo_editar      <- Editar("Editar", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    modulo_relacion    <- Relacionamiento("Relacionamiento", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    modulo_promover    <- Promover("Promover", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    modulo_descartar   <- Descartar("Descartar", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    modulo_oportunidad <- CrearOportunidad("CrearOportunidad", usuario = usuario, codigo_contacto = reactive(codigo_seleccionado()))
    # Reactivar recibe su propio par codigo/disparador — nunca el
    # codigo_seleccionado compartido (ver nota arriba)
    modulo_reactivar   <- Reactivar("Reactivar", usuario = usuario, codigo_contacto = reactive(codigo_reactivar()),
                                    disparador = reactive(trigger_reactivar()))
    
    contadores_retorno <- list(
      modulo_editar$actualizaciones, modulo_promover$etapa, modulo_descartar$etapa,
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
      showModal(modalDialog(title = "Nuevo Contacto", size = "l", easyClose = TRUE, footer = modalButton("Cerrar"),
                            CrearContactoUI(ns("mod_nuevo_contacto"))))
    })
    observeEvent(contacto_mod$n(), { removeModal(); trigger_refresco_kanban() })
    
    invisible(NULL)
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title = "Prueba Kanban Embudo",
  header = bs4DashNavbar(), sidebar = bs4DashSidebar(), controlbar = bs4DashControlbar(), footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    KanbanEmbudoUI("EmbudoKanban")
  )
)
server <- function(input, output, session) {
  KanbanEmbudo("EmbudoKanban", usuario = reactive("CMEDINA"))
}
shinyApp(ui, server)