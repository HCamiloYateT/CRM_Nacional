server <- function(input, output, session) {
  .crm_iniciar_loader(input)
  
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
    
    .crm_preparar_datos(datos_rv(), productos_snap, segmentaciones_actual, ord_cmp)
  })
  
  data_t <- reactive({
    f <- filtros()
    req(data_c(), !is.null(f))
    
    data_c() %>%
      filter(Excluir != "SI", ProdExcluir != "SI", Asesor %in% f$asesor, Segmento %in% f$segmento,
             CLLinNegNo %in% f$linneg, Categoria %in% f$categoria, Producto %in% f$producto)
  })
  
  data_f <- reactive({
    f <- filtros()
    req(data_t(), f$fecha)
    
    
    fecha_ini <- as.Date(f$fecha[[1]])
    fecha_fin <- as.Date(f$fecha[[2]])
    
    if (isTRUE(f$sin_factura)) {
      data_t() %>% filter(is.na(FecFact) | dplyr::between(FecFact, fecha_ini, fecha_fin))
    } else {
      data_t() %>% filter(!is.na(FecFact), dplyr::between(FecFact, fecha_ini, fecha_fin))
    }
  })
  
  # Maestro completo: no aplicar el filtro de fechas de facturación a saldos pendientes.
  maestro_base_comercial <- reactive(.crm_preparar_datos(crm_maestro, productos_snap,
    segmentaciones_actual, ord_cmp))
  maestro_comercial <- reactive(.gc_filtrar_dimensiones(maestro_base_comercial(), filtros()))

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
  indicadores_mod <- TablaIndicadores("ind_kpis")
  res_ind <- racafeModulos::MenuHeaderServer(id = "MenuIndicadores", items_r = TablaIndicadoresUI("ind_kpis"),
                                            headerText = NULL, href = "#", footerText = "Comparación de indicadores")
  
  observeEvent(res_ind$footer_click(), {
    bs4Dash::updateTabItems(session, "menu_principal", selected = "GE_Comparacion")
  })
  ### Tareas ----
  notas_revision <- reactiveVal(0L)
  notas_datos <- reactive({
    notas_revision()
    notas_creadas$actualizaciones()
    racafeBD::CargarDatos("CRMNALNOTAS")
  })
  menu_tareas <- HeaderTareasServer(session = session, usuario = usuario, notas = notas_datos, es_admin = es_admin,
    actualizaciones = notas_creadas$actualizaciones,
    on_success = function() notas_revision(isolate(notas_revision()) + 1L))
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
    es_admin = es_admin, operaciones = crm_detalle_operaciones
  )
  
  ### Migración de Cartera ----
  HeaderMigrarCarteraServer(
    usuario = usuario, clientes_raw = clientes_raw_cache,
    es_admin = es_admin
  )
  
  # Archivo histórico opcional: datos locales, nunca scripts de la app anterior.
  comercial_historico_r <- reactive({
    incorporado <- homologaciones$comercial_historico
    if (!is.null(incorporado)) return(incorporado)
    ruta <- Sys.getenv("CRM_DATOS_COMERCIALES", unset = file.path(.ruta_app, "data", "comercial_historico.rds"))
    if (file.exists(ruta)) .com_leer_historico(ruta) else NULL
  })
  hechos_analisis_comercial <- reactive(.com_enriquecer_geografia(maestro_comercial(),
    .com_clientes(clientes_raw_cache(), Sys.Date(), filtros())))

  ## Body ----
  # Fuentes del resumen disponibles antes de abrir sus pestañas ----
  presupuesto_mod <- Presupuesto("GC_Presupuesto_mod", dat = data_c,
    clientes_raw = clientes_raw_cache, filtros = filtros)
  validacion_mod <- isolate(ValidacionFinanciera(id = "ValidacionFinanciera", filtros = filtros,
    dat = data_c, fact = fact, margenes_pyg = margenes_pyg, nit_ppal = nit_ppal,
    clientes_snap = clientes_snap, n_cliente = n_cliente))
  reglas_productos_rv <- reactiveVal(NA_real_)
  observeEvent(filtros(), { reglas_productos_rv(NA_real_) }, ignoreInit = TRUE)
  indicadores_resumen <- reactive({
    presupuesto <- presupuesto_mod$resumen_ejecutivo()
    list(gc_presupuesto_sacos = presupuesto$sacos, gc_presupuesto_margen = presupuesto$margen,
      ge_validacion = validacion_mod$cuadre_sacos(), gc_tareas = menu_tareas$tareas$n(),
      pd_ejecucion = if (length(filtros()$fecha) == 2L) {
        .prod_indicadores(.prod_preparar(maestro_comercial(), as.Date(filtros()$fecha)))$productos
      } else NA_real_, pd_marketbasket = reglas_productos_rv())
  })

  ### Generalidades ----
  CargarTabUnaVez("GE_Resumen", "ui_GE_Resumen", function() ResumenEjecutivoUI("ResumenEjecutivo"),
    function() ResumenEjecutivo("ResumenEjecutivo", valores = indicadores_resumen))
  CargarTabUnaVez("GE_Validacion", "ui_GE_Validacion", function() ValidacionFinancieraUI("ValidacionFinanciera"))

  CargarTabUnaVez("GE_Evolucion", "ui_GE_Evolucion",
    function() EvolucionNegocioUI("EvolucionNegocio", ultimo_periodo_pyg),
    function() EvolucionNegocio("EvolucionNegocio", maestro_base_comercial, clientes_raw_cache, filtros,
      ultimo_periodo_pyg, es_admin, historico = function() {
        if (!is.null(homologaciones$evolucion_historica)) return(homologaciones$evolucion_historica)
        ruta <- Sys.getenv("CRM_EVOLUCION_HISTORICA", unset = file.path(.ruta_app, "data", "evolucion_historica.rds"))
        if (file.exists(ruta)) .ev_leer_historico(ruta) else NULL
      }, primera_compra = function() .ev_primera_compra(fact_cliente, nit_ppal),
      navegar = function(tab) bs4Dash::updateTabItems(session, "menu_principal", selected = tab)))
  
  CargarTabUnaVez("GE_Participacion", "ui_GE_Participacion",
    function() AnalisisComercialUI("AnalisisParticipacion", "participacion", ultimo_periodo_pyg),
    function() AnalisisComercial("AnalisisParticipacion", hechos_analisis_comercial, clientes_raw_cache, filtros, "participacion",
      historico = comercial_historico_r))
  
  CargarTabUnaVez("GE_Comparacion", "ui_GE_Comparacion", function() ComparacionIndicadoresUI("CompIndicadores"),
                  function() ComparacionIndicadores("CompIndicadores"))
  
  CargarTabUnaVez("GE_Inventarios", "ui_GE_Inventarios", function() InventariosUI("Inventarios"))
  
  ### Gestión Comercial ----
  #### Presupuesto ----
  
  CargarTabUnaVez("GC_Presupuesto", "ui_GC_Presupuesto", function() {
    PresupuestoUI("GC_Presupuesto_mod")
  })
  
  CargarTabUnaVez("GC_Variables", "ui_GC_Variables", function() VariablesBonoUI("VariablesBono", ultimo_periodo_pyg),
    function() VariablesBono("VariablesBono", hechos_analisis_comercial, clientes_raw_cache, filtros,
      comercial_historico_r, ultimo_periodo_pyg))
  CargarTabUnaVez("GC_Oportunidades", "ui_GC_Oportunidades", function() OportunidadesGestionUI("Oportunidades"),
    function() OportunidadesGestion("Oportunidades", usuario, es_admin, maestro_base_comercial))
  CargarTabUnaVez("GC_Pendientes", "ui_GC_Pendientes", function() PendientesGestionUI("Pendientes"),
    function() PendientesGestion("Pendientes", maestro_comercial, usuario))
  CargarTabUnaVez("GC_Tareas", "ui_GC_Tareas", function() TableroTareasUI("Tareas"), function() {
    TableroTareas("Tareas", usuario = usuario, datos = menu_tareas$tareas$datos, es_admin = es_admin,
      abrir = menu_tareas$tareas$abrir, refrescar = menu_tareas$tareas$refrescar)
  })
  CargarTabUnaVez("GC_Alertas", "ui_GC_Alertas", function() AlertasNotificacionesUI("AlertasNotificaciones"))
  CargarTabUnaVez("GC_Fidelidad", "ui_GC_Fidelidad", function() FidelidadRiesgoUI("FidelidadRiesgo"),
    function() FidelidadRiesgo("FidelidadRiesgo", hechos_analisis_comercial, clientes_raw_cache, filtros))
  CargarTabUnaVez("GC_Cartera", "ui_GC_Cartera", function() CarteraComercialUI("CarteraComercial"),
    function() CarteraComercial("CarteraComercial", comercial_historico_r, clientes_raw_cache, nit_ppal,
      filtros, es_admin))
  
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
  
  CargarTabUnaVez("PR_Potencial", "ui_PR_Potencial", function() PotencialMercadoUI("PotencialMercado"))
  
  ### Dinámica de Clientes ----
  CargarTabUnaVez("DC_Nuevos", "ui_DC_Nuevos", function() ClientesNuevosUI("ClientesNuevos"))
  
  CargarTabUnaVez("DC_Perdidos", "ui_DC_Perdidos", function() ClientesPerdidosUI("ClientesPerdidos"))
  
  ### Cohortes de Clientes ----
  segmentos_cohortes <- eventReactive(input$FT_Actualizar, {
    racafeBD::CargarDatos("CRMNALSEGR")
  }, ignoreNULL = FALSE)
  data_cohortes <- reactive(.coha_preparar(segmentos_cohortes(), clientes_raw_cache(), maestro_base_comercial()))
  CargarTabUnaVez("CO_Estados", "ui_CO_Estados", function() CohortesEstadosUI("CohortesEstados"),
    function() CohortesEstados("CohortesEstados", bd = data_cohortes, data_t = maestro_comercial))

  CargarTabUnaVez("CO_Retencion", "ui_CO_Retencion", function() CohorteRetencionUI("CohorteRetencion"),
    function() CohorteRetencion("CohorteRetencion", maestro_comercial))
  
  CargarTabUnaVez("CO_Ventas", "ui_CO_Ventas", function() CohorteVentasUI("CohorteVentas"),
    function() CohorteVentas("CohorteVentas", maestro_comercial))
  
  CargarTabUnaVez("CO_Margen", "ui_CO_Margen", function() CohorteMargenUI("CohorteMargen"),
    function() CohorteMargen("CohorteMargen", maestro_comercial))
  
  CargarTabUnaVez("CO_Frecuencia", "ui_CO_Frecuencia", function() CohorteFrecuenciaUI("CohorteFrecuencia"),
    function() CohorteFrecuencia("CohorteFrecuencia", maestro_comercial))
  
  ### Herramientas ----
  CargarTabUnaVez("HE_Calculadoras", "ui_HE_Calculadoras", function() CalculadoraUI("Calculadora"), function() {
    Calculadora("Calculadora", dat_ind = indicadores_mod$datos_r, usr = usuario, clientes = clientes_raw_cache)
  })
  
  CargarTabUnaVez("HE_Conversion", "ui_HE_Conversion", function() TablasConversionUI("TablasConversion"))
  
  CargarTabUnaVez("HE_Informes", "ui_HE_Informes", function() InformesUI("Informes"))
  
  ### Productos ----
  CargarTabUnaVez("PD_Ejecucion", "ui_PD_Ejecucion", function() ProductosEjecucionUI("ProductosEjecucion"),
    function() ProductosEjecucion("ProductosEjecucion", maestro_comercial))
  
  CargarTabUnaVez("PD_MarketBasket", "ui_PD_MarketBasket", function() MarketBasketUI("MarketBasket"),
    function() MarketBasket("MarketBasket", maestro_base_comercial, filtros,
      on_result = function(n) reglas_productos_rv(n)))
  
  
  ### Administración ----
  CargarTabUnaVez("AD_Catalogos", "ui_AD_Administrar", function() {
    req(isTRUE(es_admin()))
    AdministrarCatalogosUI("Administrar")
  }, function() {
    req(isTRUE(es_admin()))
    AdministrarCatalogos("Administrar", usuario, grupo, es_admin)
  })

  ### Consulta Individual ----
  CargarTabUnaVez("IN_Consulta", "ui_IN_Consulta", function() ConsultaIndividualUI("ConsultaIndividual"))
  
}





