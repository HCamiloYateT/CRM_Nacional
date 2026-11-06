server <- function(input, output, session) {
  
  # Datos reactivos ----
  usuario <- reactive({
    if (is.null(session$user)) "HCYATE" else str_to_upper(session$user)
  })
  grupo <- reactive({
    if (is.null(session$groups)) "ANALÍTICA" else stringr::str_to_upper(session$groups)
  })
  
  # Administración ----
  es_admin <- reactive({
    usuario_actual <- stringr::str_to_upper(stringr::str_trim(usuario()))
    grupos_actuales <- stringr::str_to_upper(stringr::str_trim(grupo()))
    (!identical(usuario_actual, "SIN IDENTIFICAR") &&
       (identical(usuario_actual, "JONATHAN CAÑON") ||
      any(grupos_actuales %in% c("ANALÍTICA", "ANALITICA"))))
  })
  
  output$ui_menu_admin <- bs4Dash::renderMenu({
    req(isTRUE(es_admin()))
    
    bs4Dash::bs4SidebarMenuItem(
      "Administrar", icon = icon("user-shield"),
      bs4Dash::bs4SidebarMenuSubItem(
        "Catálogos", tabName = "AD_Catalogos",
        icon = icon("list")
      )
    )
  })

  observeEvent(input$menu_principal, {
    if (identical(input$menu_principal, "AD_Catalogos") && !isTRUE(es_admin())) {
      bs4Dash::updateTabItems(session, "menu_principal", selected = "GE_Resumen")
    }
  }, ignoreInit = FALSE)

  filtros <- Filtros("Filtros", usuario, es_admin, dat = data_c)
  
  datos_rv <- reactiveVal(data)
  
  data_c <- reactive({
    req(datos_rv())
    
    productos_r <- productos_snap %>%
      select(LinNegCod, LinProCod, MCCod, MrcCod, LinProNom, MCNom, Marca, Categoria, Producto,
             ProdExcluir = Excluir)
    
    segmentacion_r <- segmentaciones_actual %>% select(-any_of("FecMes"))
    
    datos_rv() %>%
      left_join(productos_r, by = c("LinNegCod", "LinProCod", "MCCod", "MrcCod")) %>%
      left_join(segmentacion_r, by = c("CliNitPpal", "LinNegCod")) %>%
      left_join(ord_cmp, by = "CLLotCod") %>%
      mutate(
        SegmentoAsignadoSistema = Segmento %in% c(NA_character_, "", "SIN DATO"),
        Excluir = ifelse(Excluir %in% c(NA_character_, "", "SIN DATO"), "NO", Excluir),
        ProdExcluir = ifelse(ProdExcluir %in% c(NA_character_, "", "SIN DATO"), "NO", ProdExcluir),
        Segmento = case_when(
          !SegmentoAsignadoSistema ~ Segmento,
          LinNegCod == 10000 & SacLote <= 240 ~ "DETAL",
          LinNegCod == 10000 & SacLote > 240 ~ "MEDIANO",
          LinNegCod == 21000 & SacLote < 50 ~ "DETAL",
          LinNegCod == 21000 & SacLote >= 50 ~ "MEDIANO",
          TRUE ~ "SIN DATO"
        ),
        SegmentoRacafe = ifelse(
          SegmentoRacafe %in% c(NA_character_, "", "SIN DATO") & !is.na(FecFact), "CLIENTE", SegmentoRacafe
        ),
        SegmentoRacafe = ifelse(SegmentoRacafe %in% c(NA_character_, ""), "SIN DATO", SegmentoRacafe),
        Asesor = ifelse(Asesor %in% c(NA_character_, ""), "SIN DATO", Asesor),
        CLLinNegNo = ifelse(CLLinNegNo %in% c(NA_character_, ""), "SIN DATO", CLLinNegNo),
        Categoria = ifelse(Categoria %in% c(NA_character_, ""), "SIN DATO", Categoria),
        Producto = ifelse(Producto %in% c(NA_character_, ""), "SIN DATO", Producto)
      )
  })
  
  data_t <- reactive({
    f <- filtros()
    req(data_c(), f$asesor, f$segmento, f$linneg, f$categoria, f$producto)
    
    data_c() %>%
      filter(Excluir != "SI", ProdExcluir != "SI", Asesor %in% f$asesor, Segmento %in% f$segmento,
             CLLinNegNo %in% f$linneg, Categoria %in% f$categoria, Producto %in% f$producto)
  })
  
  data_f <- reactive({
    f <- filtros()
    req(data_t(), f$fecha)
    
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
    on.exit(waiter_hide(), add = TRUE)
    
    fecha_ini <- as.Date(f$fecha[[1]])
    fecha_fin <- as.Date(f$fecha[[2]])
    
    if (isTRUE(f$sin_factura)) {
      data_t() %>% filter(is.na(FecFact) | dplyr::between(FecFact, fecha_ini, fecha_fin))
    } else {
      data_t() %>% filter(!is.na(FecFact), dplyr::between(FecFact, fecha_ini, fecha_fin))
    }
  })
  
  # Carga diferida ----
  modulos_cargados <- reactiveValues()
  CargarTabUnaVez <- function(tab_name, ui_id, ui_fun, server_fun = NULL) {
    observeEvent(input$menu_principal, {
      req(identical(input$menu_principal, tab_name))
      if (isTRUE(modulos_cargados[[tab_name]])) return()
      
      output[[ui_id]] <- renderUI(ui_fun())
      if (!is.null(server_fun)) server_fun()
      
      modulos_cargados[[tab_name]] <- TRUE
    }, ignoreInit = FALSE)
  }
  
  # Outputs ----
  ## Contolbar ----
  Cotizacion(id = "Cotizador", usuario = usuario, dat = data_c)
  notas_creadas <- CrearNotas(id = "CrearNota", usuario = usuario)
  CrearContacto("AccionCrear", usuario)
  RegistrarCompetencia(id = "RegistrarCompetencia", usr = usuario)
  
  ## Header ----
  output$user <- renderUI({
    FormatearTexto(paste(usuario()) %>% HTML, negrita = TRUE, tamano_pct = 0.75, alineacion = "center", color = "#999")
  })
  
  ### Indicadores ----
  TablaIndicadores("ind_kpis")
  res_ind <- MenuHeaderServer(id = "MenuIndicadores", items_r = TablaIndicadoresUI("ind_kpis"), headerText = NULL,
                              href = "#", footerText = "Comparación de indicadores")
  
  observeEvent(res_ind$footer_click(), {
    bs4Dash::updateTabItems(session, "menu_principal", selected = "GE_Comparacion")
  })
  ### Tareas ----
  HeaderTareasServer(session = session, usuario = usuario,
                    actualizaciones = notas_creadas$actualizaciones)
  ### Clientes ----
  clientes_raw_cache <- reactiveVal(CargarDatos("CRMNALCLIENTE"))
  
  observeEvent(input$FT_Actualizar, {
    clientes_raw_cache(CargarDatos("CRMNALCLIENTE"))
  })
  
  # Descarga de clientes ----
  output$FT_DescargarClientes <- downloadHandler(
    filename = function() {
      paste0("InformacionClientes_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      clientes_snap <- clientes_raw_cache() %>%
        dplyr::mutate(
          CLCliNit = as.character(CLCliNit),
          LinNegCod = as.character(LinNegCod),
          FecProceso = as.POSIXct(FecProceso)
        ) %>%
        dplyr::arrange(dplyr::desc(FecProceso)) %>%
        dplyr::distinct(CLCliNit, LinNegCod, .keep_all = TRUE) %>%
        dplyr::left_join(
          n_cliente %>%
            dplyr::transmute(
              CLCliNit = as.character(PerCod),
              RazonSocial = as.character(PerRazSoc)
            ) %>%
            dplyr::distinct(CLCliNit, .keep_all = TRUE),
          by = "CLCliNit"
        ) %>%
        dplyr::relocate(RazonSocial, .after = CLCliNit) %>%
        dplyr::arrange(RazonSocial, LinNegCod)
      
      writexl::write_xlsx(clientes_snap, path = file)
    }
  )
  
  
  HeaderClientesServer(
    data_c = data_c, datos_rv = datos_rv, clientes_raw = clientes_raw_cache,
    usuario = usuario, ncliente = reactive(n_cliente), output = output,
    es_admin = es_admin
  )
  
  ### Migración de Cartera ----
  HeaderMigrarCarteraServer(
    usuario = usuario, clientes_raw = clientes_raw_cache,
    es_admin = es_admin
  )
  
  ## Body ----
  ### Generalidades ----
  CargarTabUnaVez("GE_Resumen", "ui_GE_Resumen", function() ResumenEjecutivoUI("ResumenEjecutivo"),
                  function() ResumenEjecutivo("ResumenEjecutivo"))
  
  CargarTabUnaVez("GE_Validacion", "ui_GE_Validacion", function() ValidacionFinancieraUI("ValidacionFinanciera"),
                  function() {
                    ValidacionFinanciera(id = "ValidacionFinanciera", filtros = filtros,
                                         dat = data_c, fact = fact, margenes_pyg = margenes_pyg,
                                         nit_ppal = nit_ppal, clientes_snap = clientes_snap, 
                                         n_cliente = n_cliente)
                  })
  
  CargarTabUnaVez("GE_Evolucion", "ui_GE_Evolucion", function() h4("Evolución del Negocio"))
  
  CargarTabUnaVez("GE_Participacion", "ui_GE_Participacion", function() h4("Participación y Composición"))
  
  CargarTabUnaVez("GE_Comparacion", "ui_GE_Comparacion", function() ComparacionIndicadoresUI("CompIndicadores"),
                  function() ComparacionIndicadores("CompIndicadores"))
  
  CargarTabUnaVez("GE_Inventarios", "ui_GE_Inventarios", function() h4("Inventarios"))
  
  ### Gestión Comercial ----
  CargarTabUnaVez("GC_Resumen", "ui_GC_Resumen", function() h4("Resumen"))
  #### Presupuesto ----
  Presupuesto("GC_Presupuesto_mod", dat = data_c, clientes_raw = clientes_raw_cache, filtros = filtros)
  
  CargarTabUnaVez("GC_Presupuesto", "ui_GC_Presupuesto", function() {
    PresupuestoUI("GC_Presupuesto_mod")
  })
  
  CargarTabUnaVez("GC_Variables", "ui_GC_Variables", function() h4("Variables para Bonificación"))
  CargarTabUnaVez("GC_Oportunidades", "ui_GC_Oportunidades", function() h4("Oportunidades"))
  CargarTabUnaVez("GC_Pendientes", "ui_GC_Pendientes", function() h4("Pendientes de Gestión"))
  CargarTabUnaVez("GC_Tareas", "ui_GC_Tareas", function() h4("Notas y Tareas"))
  CargarTabUnaVez("GC_Alertas", "ui_GC_Alertas", function() h4("Alertas y Notificaciones"))
  CargarTabUnaVez("GC_Fidelidad", "ui_GC_Fidelidad", function() h4("Fidelidad y Riesgo"))
  CargarTabUnaVez("GC_Cartera", "ui_GC_Cartera", function() h4("Cartera"))
  
  ### Prospección ----
  CargarTabUnaVez("PR_Kanban", "ui_PR_Kanban", function() KanbanEmbudoUI("Kanban"),
                  function() KanbanEmbudo("Kanban", usuario))
  
  CargarTabUnaVez("PR_Contactos", "ui_PR_Contactos", function() DetalleContactoUI("DetalleContacto"),
                  function() DetalleContacto("DetalleContacto", usuario))
  
  CargarTabUnaVez("PR_Prospectos", "ui_PR_Prospectos", function() DetalleProspectosUI("DetalleProspectos"),
                  function() DetalleProspectos("DetalleProspectos", usuario))
  
  CargarTabUnaVez("PR_Leads", "ui_PR_Leads", function() DetalleLeadsUI("DetalleLeads"),
                  function() DetalleLeads("DetalleLeads", usuario))
  
  CargarTabUnaVez("PR_Clientes", "ui_PR_Clientes", function() DetalleClientesUI("DetalleClientes"),
                  function() DetalleClientes("DetalleClientes", usuario))
  
  CargarTabUnaVez("PR_Descartados", "ui_PR_Descartados", function() DetalleDescartadosUI("DetalleDescartados"),
                  function() DetalleDescartados("DetalleDescartados"))
  
  CargarTabUnaVez("PR_Conversion", "ui_PR_Conversion", function() EmbudoConversionUI("EmbudoConversion"),
                  function() EmbudoConversion("EmbudoConversion"))
  
  CargarTabUnaVez("PR_Potencial", "ui_PR_Potencial", function() h4("Potencial de Mercado"))
  
  ### Dinámica de Clientes ----
  CargarTabUnaVez("DC_Nuevos", "ui_DC_Nuevos", function() h4("Clientes Nuevos"))
  
  CargarTabUnaVez("DC_Perdidos", "ui_DC_Perdidos", function() h4("Clientes Perdidos"))
  
  ### Cohortes de Clientes ----
  CargarTabUnaVez("CO_Retencion", "ui_CO_Retencion", function() h4("Retención"))
  
  CargarTabUnaVez("CO_Ventas", "ui_CO_Ventas", function() h4("Ventas"))
  
  CargarTabUnaVez("CO_Margen", "ui_CO_Margen", function() h4("Margen"))
  
  CargarTabUnaVez("CO_Frecuencia", "ui_CO_Frecuencia", function() h4("Frecuencia"))
  
  ### Herramientas ----
  CargarTabUnaVez("HE_Calculadoras", "ui_HE_Calculadoras", function() h4("Calculadoras"))
  
  CargarTabUnaVez("HE_Conversion", "ui_HE_Conversion", function() h4("Tablas de Conversión"))
  
  CargarTabUnaVez("HE_Informes", "ui_HE_Informes", function() h4("Informes"))
  
  ### Productos ----
  CargarTabUnaVez("PD_Ejecucion", "ui_PD_Ejecucion", function() h4("Ejecución"))
  
  CargarTabUnaVez("PD_MarketBasket", "ui_PD_MarketBasket", function() h4("Market Basket"))
  
  CargarTabUnaVez("PD_Administracion", "ui_PD_Administracion", function() h4("Administración"))
  
  ### Administración ----
  CargarTabUnaVez("AD_Catalogos", "ui_AD_Administrar", function() {
    req(isTRUE(es_admin()))
    AdministrarCatalogosUI("Administrar")
  }, function() {
    req(isTRUE(es_admin()))
    AdministrarCatalogos("Administrar", usuario, grupo, es_admin)
  })

  ### Consulta Individual ----
  CargarTabUnaVez("IN_Consulta", "ui_IN_Consulta", function() h4("Consulta Individual"))
  
}