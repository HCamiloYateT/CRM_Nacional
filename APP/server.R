server <- function(input, output, session) {
  .crm_iniciar_loader(input)
  consulta_solicitada <- reactiveVal(NULL)
  observeEvent(session$clientData$url_search, {
    destino <- .kb_parametros_consulta(session$clientData$url_search)
    if (!is.null(destino)) {
      consulta_solicitada(destino)
      bs4Dash::updateTabItems(session, "menu_principal", selected = "IN_Consulta")
    }
  }, once = TRUE, ignoreInit = FALSE)
  
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
        "Catálogos", tabName = "AD_Catalogos", icon = icon("list")),
      bs4Dash::bs4SidebarMenuSubItem("Escalas y etapas", tabName = "AD_Escalas", icon = icon("palette")),
      bs4Dash::bs4SidebarMenuSubItem("Módulos transversales", tabName = "AD_Transversales", icon = icon("cubes"))
    )
  })

  observeEvent(input$menu_principal, {
    if (isTRUE(input$menu_principal %in% c("AD_Catalogos", "AD_Escalas", "AD_Transversales")) &&
        !isTRUE(es_admin())) {
      bs4Dash::updateTabItems(session, "menu_principal", selected = "GE_Resumen")
    }
  }, ignoreInit = FALSE)

  filtros <- Filtros("Filtros", usuario, es_admin, dat = data_c)
  
  # Una exportación compartida por proceso alimenta las sesiones sin releer SQL.
  ruta_exportacion <- file.path(.ruta_app, "data", "data.RData")
  inicial <- new.env(parent = emptyenv())
  .crm_cargar_exportacion(ruta_exportacion, inicial)
  exportacion_rv <- reactiveVal(as.list(inicial))
  datos_rv <- reactiveVal(inicial$data)
  productos_cache <- reactiveVal(inicial$productos_snap)
  clientes_raw_cache <- reactiveVal(inicial$homologaciones$fuentes_consulta$CRMNALCLIENTE %||%
    inicial$clientes_raw)
  firma_en_uso <- reactiveVal(.crm_firma_exportacion(ruta_exportacion))
  session$userData$fuentes_etl <- reactive(exportacion_rv()$homologaciones$fuentes_consulta)

  # Base completa, selección comercial y selección temporal: una sola transformación por capa.
  data_c <- reactive({
    h <- exportacion_rv()
    x <- .crm_medir("transformacion:base_comercial", .crm_base_compartida(h$crm_maestro, productos_cache(),
      h$segmentaciones_actual, h$ord_cmp))
    attr(x, "facturas_detalle") <- h$homologaciones$facturas_detalle
    x
  })
  maestro_base_comercial <- data_c
  data_facturada <- reactive(data_c() %>% dplyr::filter(EstadoFinanciero != "PROYECTADO"))

  # Cambiar fechas no invalida la selección de asesor, segmento, línea y productos.
  filtros_dimensiones <- reactiveVal(NULL)
  observeEvent(filtros(), {
    f <- filtros()
    req(!is.null(f))
    filtros_dimensiones(f[c("asesor", "segmento", "linneg", "categoria", "producto")])
  }, priority = 100)
  data_t <- reactive({
    req(!is.null(filtros_dimensiones()))
    .crm_medir("filtros:base_comercial", .gc_filtrar_dimensiones(data_c(), filtros_dimensiones()))
  })
  maestro_comercial <- data_t
  session$userData$data_c <- data_c
  session$userData$data_t <- data_t
  
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
  
  session$userData$data_f <- data_f

  unidades_comerciales <- reactive(.crm_medir("construccion:unidades_comerciales", {
    contactos <- derivar_etapa_actual(.pr_cargar("CRMNALCONTACTO"), .pr_cargar("CRMNALHISTORIALETAPA"),
      .pr_cargar("CRMNALSEGR"))
    .crm_unidades_comerciales(maestro_base_comercial(), clientes_raw_cache(), contactos,
      .pr_cargar("CRMNALSEGR"), .pr_cargar("CONTACTOLEAD"),
      nit_mapa = exportacion_rv()$homologaciones$nit_principal)
  }))
  ventas_comerciales <- reactive(.crm_medir("agregacion:ventas_comerciales",
    .crm_ventas_comerciales(maestro_comercial())))
  session$userData$ventas_comerciales <- ventas_comerciales

  indicadores_unidades <- reactive(.crm_medir("agregacion:indicadores_unidades", {
    f <- filtros()
    .crm_metricas_unidades(maestro_comercial(), clientes_raw_cache(), f$fecha[[1]], f$fecha[[2]])
  }))
  rfm_resultado <- reactiveVal(NULL)
  revision_datos_comerciales <- reactiveVal(0L)
  observeEvent(list(exportacion_rv(), productos_cache()), {
    revision_datos_comerciales(isolate(revision_datos_comerciales()) + 1L)
  }, priority = 100)
  rfm_comercial <- reactive(.crm_medir("agregacion:rfm", {
    x <- .fid_rfm(maestro_comercial(), as.Date(filtros()$fecha[[2]]))
    rfm_resultado(list(filtros = filtros(), revision = revision_datos_comerciales(), datos = x))
    x
  }))
  session$userData$rfm <- rfm_comercial
  datos_unidades <- reactive({
    x <- unidades_comerciales()
    x <- dplyr::left_join(x, indicadores_unidades(), by = c("CliNitPpal", "LinNegCod"), relationship = "one-to-one")
    modelo <- rfm_resultado()
    x$SegmentoRFM <- rep(NA_character_, nrow(x))
    if (!is.null(modelo) && identical(modelo$filtros, filtros()) &&
        identical(modelo$revision, revision_datos_comerciales())) {
      clave <- function(d) paste(.crm_id_texto(d$CliNitPpal), .crm_id_texto(d$LinNegCod), sep = "|")
      x$SegmentoRFM <- modelo$datos$SegmentoRFM[match(clave(x), clave(modelo$datos))]
    }
    x
  })
  session$userData$unidades_comerciales <- datos_unidades
  output$leyenda_dimensiones <- .crm_render_ui({
    req(isTRUE(input$escalas_color > 0L))
    u <- datos_unidades()
    campos <- c(linneg = "CLLinNegNo", segmento = "Segmento", estado = "EstadoCliente", rfm = "SegmentoRFM", origen = "Origen")
    titulos <- c(linneg = "LÍNEA DE NEGOCIO", segmento = "SEGMENTO COMERCIAL", estado = "ESTADO OFICIAL",
      rfm = "SEGMENTO RFM", origen = "ORIGEN")
    generales <- tagList(lapply(names(campos), function(dimension) {
      valores <- sort(unique(stats::na.omit(u[[campos[[dimension]]]])))
      valores <- head(valores[nzchar(valores)], 50L)
      if (!length(valores)) return(NULL)
      tagList(tags$strong(titulos[[dimension]]), lapply(valores, function(v) div(
        tags$span(class = "kb-scale-swatch", style = paste0("background:", .crm_colores(dimension, v)[[1]], ";")),
        .crm_label_comercial(v))), tags$hr())
    }))
    m <- maestro_comercial()
    jerarquia <- unique(m[intersect(c("CLLinNegNo", "Categoria", "Producto"), names(m))])
    categorias <- if (all(c("CLLinNegNo", "Categoria") %in% names(jerarquia))) {
      unique(jerarquia[c("CLLinNegNo", "Categoria")])
    } else data.frame()
    tagList(generales, if (nrow(categorias)) tags$section(class = "crm-leyenda-grupo",
      tags$strong("CATEGORÍAS"), lapply(seq_len(nrow(categorias)), function(i) {
        f <- categorias[i, ]
        tags$div(tags$span(class = "kb-scale-swatch", style = paste0("background:",
          .crm_colores("categoria", f$Categoria, f$CLLinNegNo), ";")),
          paste(.crm_label_comercial(f$CLLinNegNo), f$Categoria, sep = " · "))
      })), tags$section(class = "crm-leyenda-grupo", tags$strong("CONVENCIONES"),
        lapply(c("Presupuesto", "Ejecución", "Ritmo requerido"), function(v) {
          tags$div(tags$span(class = "kb-scale-swatch", style = paste0("background:",
            .crm_colores("auxiliar", v), ";")), v)
        })))
  })


  # Pedido accesible desde las tablas sin consultas adicionales al histórico ----
  pedido_consulta <- reactiveVal(NULL)
  session$userData$trazabilidad_disponible <- TRUE
  pedido_modal <- reactiveVal(NULL)
  pedido_desde_clasificacion <- reactiveVal(FALSE)
  fuente_pedido_modal <- reactive(.tz_fuente_modal(maestro_base_comercial(), filtros(),
    pedido_desde_clasificacion()))
  TrazabilidadPedido("TrazabilidadGlobal", maestro_comercial, pedido_consulta)
  TrazabilidadPedido("TrazabilidadModal", fuente_pedido_modal, pedido_modal)
  observe({
    req(identical(input$menu_principal, "TR_Pedido"))
    codigos <- sort(unique(.crm_id_texto(maestro_comercial()$CLPdcCod)))
    codigos <- codigos[!is.na(codigos) & codigos != "0"]
    elegido <- isolate(pedido_consulta())
    .crm_update_select(session, "tr_pedido", choices = codigos,
      selected = if (length(elegido) && elegido %in% codigos) elegido else character())
  })
  observeEvent(input$tr_pedido, {
    codigo <- .crm_id_texto(input$tr_pedido)
    req(length(codigo) == 1L, codigo %in% .crm_id_texto(maestro_comercial()$CLPdcCod))
    pedido_consulta(codigo)
  })
  observeEvent(input$crm_ver_pedido, {
    codigo <- .crm_id_texto(input$crm_ver_pedido$pedido)
    req(length(codigo) == 1L, !is.na(codigo), codigo != "0")
    clasificacion <- identical(input$menu_principal, "PD_SinClasificar")
    fuente <- .tz_fuente_modal(maestro_base_comercial(), filtros(), clasificacion)
    if (!codigo %in% .crm_id_texto(fuente$CLPdcCod)) {
      showNotification("El pedido no está disponible con los filtros comerciales actuales.", type = "warning")
      return(invisible(NULL))
    }
    pedido_desde_clasificacion(clasificacion)
    pedido_modal(codigo)
    showModal(racafeForms::racafeModal(title = paste("Pedido", codigo), size = "ventana", easy_close = FALSE,
      TrazabilidadPedidoUI("TrazabilidadModal"), footer = racafeForms::racafeModalFooter(
        right = racafeForms::racafeButton("cerrar_trazabilidad", "Cerrar", variant = "secondary"))))
  })

  observeEvent(input$cerrar_trazabilidad, { pedido_modal(NULL); removeModal() })

  # Carga diferida ----
  modulos_cargados <- reactiveValues()
  CargarTabUnaVez <- function(tab_name, ui_id, ui_fun, server_fun = NULL) {
    observeEvent(input$menu_principal, {
      req(identical(input$menu_principal, tab_name))
      if (isTRUE(modulos_cargados[[tab_name]])) return()
      
      output[[ui_id]] <- renderUI(shiny::tagList(ui_fun(), if (!is.null(server_fun)) {
        shiny::tags$script(shiny::HTML(paste0("setTimeout(function(){Shiny.setInputValue(",
          jsonlite::toJSON(paste0(ui_id, "_ready"), auto_unbox = TRUE),
          ",Date.now(),{priority:'event'});},0);")))
      }))
      if (!is.null(server_fun)) {
        observeEvent(input[[paste0(ui_id, "_ready")]], server_fun(), once = TRUE, ignoreInit = FALSE)
      }
      
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
  recargar_exportacion <- function() {
    firma <- .crm_firma_exportacion(ruta_exportacion)
    if (identical(firma, isolate(firma_en_uso()))) return(invisible(FALSE))
    nueva <- new.env(parent = emptyenv())
    .crm_cargar_exportacion(ruta_exportacion, nueva)
    # Resolver primero las excepciones por guardado; un fallo conserva la carga anterior completa.
    vigentes <- .pr_fuentes()$vigentes
    nuevos_productos <- if (exists("CRMNALPRODS", vigentes, inherits = FALSE)) {
      .crm_snapshot_productos(CargarDatos("CRMNALPRODS"))
    } else nueva$productos_snap
    nuevos_clientes <- if (exists("CRMNALCLIENTE", vigentes, inherits = FALSE)) {
      CargarDatos("CRMNALCLIENTE")
    } else nueva$homologaciones$fuentes_consulta$CRMNALCLIENTE %||% nueva$clientes_raw
    exportacion_rv(as.list(nueva))
    datos_rv(nueva$data)
    productos_cache(nuevos_productos)
    clientes_raw_cache(nuevos_clientes)
    firma_en_uso(firma)
    .pr_invalidar()
    invisible(TRUE)
  }
  revisar_exportacion <- function() tryCatch(recargar_exportacion(), error = function(e) {
    racafeForms::racafeNotify(session, title = "Se conserva la última carga válida",
      text = conditionMessage(e), type = "error")
    invisible(FALSE)
  })
  observeEvent(input$FT_Actualizar, {
    revisar_exportacion()
    notas_revision(isolate(notas_revision()) + 1L)
    .pr_invalidar()
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
          exportacion_rv()$n_cliente %>%
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
    usuario = usuario, ncliente = reactive(exportacion_rv()$n_cliente), output = output,
    es_admin = es_admin, operaciones = reactive(exportacion_rv()$crm_detalle_operaciones)
  )
  
  ### Migración de Cartera ----
  HeaderMigrarCarteraServer(
    usuario = usuario, clientes_raw = clientes_raw_cache,
    es_admin = es_admin
  )
  
  # Archivo histórico opcional: datos locales, nunca scripts de la app anterior.
  comercial_historico_r <- reactive({
    incorporado <- exportacion_rv()$homologaciones$comercial_historico
    if (!is.null(incorporado)) return(incorporado)
    ruta <- Sys.getenv("CRM_DATOS_COMERCIALES", unset = file.path(.ruta_app, "data", "comercial_historico.rds"))
    if (file.exists(ruta)) .com_leer_historico(ruta) else NULL
  })
  cartera_financiera_r <- reactive({
    vigente <- exportacion_rv()$homologaciones$cartera_financiera
    if (!is.null(vigente$cartera)) return(vigente)
    anterior <- comercial_historico_r()
    if (!is.null(anterior$cartera)) {
      anterior$origen <- "Archivo histórico de cartera"
      anterior$aviso <- if (!is.null(vigente$error)) vigente$error else "Fuente pendiente de actualizar mediante ETL."
      return(anterior)
    }
    vigente
  })
  hechos_analisis_comercial <- reactive(.com_enriquecer_geografia(maestro_comercial(),
    .com_clientes(clientes_raw_cache(), Sys.Date(), filtros())))

  ## Body ----
  # Fuentes del resumen disponibles antes de abrir sus pestañas ----
  presupuesto_mod <- Presupuesto("GC_Presupuesto_mod", dat = data_facturada, maestro = maestro_base_comercial,
    clientes_raw = clientes_raw_cache, filtros = filtros,
    margenes_pyg = reactive(exportacion_rv()$homologaciones$margenes_pyg))
  validacion_mod <- isolate(ValidacionFinanciera(id = "ValidacionFinanciera", filtros = filtros,
    dat = data_facturada, fact = reactive(exportacion_rv()$homologaciones$facturacion),
    margenes_pyg = reactive(exportacion_rv()$homologaciones$margenes_pyg),
    nit_ppal = reactive(exportacion_rv()$homologaciones$nit_principal),
    clientes_snap = reactive(exportacion_rv()$homologaciones$clientes),
    n_cliente = reactive(exportacion_rv()$n_cliente),
    facturas_documentales = reactive(exportacion_rv()$homologaciones$facturas_detalle),
    lotes_operativos = reactive(exportacion_rv()$homologaciones$lotes)))
  reglas_productos_rv <- reactiveVal(NA_real_)
  observeEvent(filtros(), { reglas_productos_rv(NA_real_) }, ignoreInit = TRUE)
  resumen_pipeline <- reactive(.kb_facturacion(cargar_pipeline_embudo(), maestro_base_comercial(),
    filtros(), usuario(), es_admin(), clientes_raw_cache()))
  resumen_pendientes <- reactive({
    x <- .pen_tipificar(.pen_preparar(maestro_comercial(), as.Date(filtros()$fecha[[2]]))$detalle,
      maestro_comercial())
    x <- x[x$TipoPendiente %in% .PEN_TIPOS, , drop = FALSE]
    nrow(unique(x[c("Nivel", "Pedido", "Linea", "Lote")]))
  })
  resumen_cartera <- reactive({
    h <- cartera_financiera_r()
    c <- .com_clientes(clientes_raw_cache(), as.Date(filtros()$fecha[[2]]), filtros())
    x <- .car_scope(h$cartera, c, exportacion_rv()$homologaciones$nit_principal)
    fechas <- as.Date(x$TCEFch)
    fechas <- fechas[!is.na(fechas) & fechas <= as.Date(filtros()$fecha[[2]])]
    if (!length(fechas)) return(NA_real_)
    vigente <- .car_preparar(x, max(fechas))
    .com_suma(vigente$Saldo[vigente$Tipo == "Por vencer" & is.finite(vigente$Saldo) & vigente$Saldo > 0])
  })
  indicadores_resumen <- reactive({
    seguro <- function(expr) tryCatch(expr, error = function(e) {
      message("[Resumen ejecutivo] ", conditionMessage(e)); NA_real_
    })
    contar_etapa <- function(etapa) seguro(sum(resumen_pipeline()$Etapa %in% etapa))
    list(gc_presupuesto_sacos = function() seguro(presupuesto_mod$resumen_ejecutivo()$sacos),
      gc_presupuesto_margen = function() seguro(presupuesto_mod$resumen_ejecutivo()$margen),
      ge_validacion = function() seguro(validacion_mod$cuadre_sacos()),
      gc_tareas = function() seguro(menu_tareas$tareas$n()),
      gc_cartera = function() seguro(resumen_cartera()),
      pr_contactos = function() contar_etapa("CONTACTO"), pr_leads = function() contar_etapa("LEAD"),
      pr_clientes = function() contar_etapa("CLIENTE"), pr_descartados = function() contar_etapa("DESCARTADO"),
      co_estados = function() seguro(.resumen_unidades_cohorte(data_cohortes(), maestro_comercial(),
        as.Date(filtros()$fecha[[2]]))),
      gc_oportunidades = function() seguro({
        contactos <- .op_contactos_visibles(.pr_cargar("CRMNALCONTACTO"), usuario(), es_admin())
        x <- .op_preparar(.pr_cargar("CRMNALCLOPT"), contactos, maestro_comercial(),
          usuario(), es_admin(), as.Date(filtros()$fecha[[2]]))
        f <- filtros()
        x <- x[.in_linea(x$Linea) %in% f$linneg & x$Categoria %in% f$categoria &
          x$Producto %in% f$producto & (is.na(x$Asesor) | x$Asesor %in% f$asesor), , drop = FALSE]
        sum(!x$Descartada & x$Estado != "Cumplida", na.rm = TRUE)
      }))
  })

  ### Generalidades ----
  CargarTabUnaVez("IN_Inicio", "ui_IN_Inicio", function() InicioCRMUI("InicioCRM"))
  CargarTabUnaVez("GE_Resumen", "ui_GE_Resumen", function() ResumenEjecutivoUI("ResumenEjecutivo"),
    function() ResumenEjecutivo("ResumenEjecutivo", valores = indicadores_resumen))
  CargarTabUnaVez("GE_Validacion", "ui_GE_Validacion", function() ValidacionFinancieraUI("ValidacionFinanciera"))

  CargarTabUnaVez("GE_Evolucion", "ui_GE_Evolucion",
    function() EvolucionNegocioUI("EvolucionNegocio", exportacion_rv()$ultimo_periodo_pyg),
    function() EvolucionNegocio("EvolucionNegocio", maestro_base_comercial, clientes_raw_cache, filtros,
      exportacion_rv()$ultimo_periodo_pyg, es_admin, historico = function() {
        if (!is.null(exportacion_rv()$homologaciones$evolucion_historica)) return(exportacion_rv()$homologaciones$evolucion_historica)
        ruta <- Sys.getenv("CRM_EVOLUCION_HISTORICA", unset = file.path(.ruta_app, "data", "evolucion_historica.rds"))
        if (file.exists(ruta)) .ev_leer_historico(ruta) else NULL
      }, primera_compra = function() .ev_primera_compra(exportacion_rv()$fact_cliente,
        exportacion_rv()$nit_ppal),
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
  
  CargarTabUnaVez("GC_Variables", "ui_GC_Variables", function() VariablesBonoUI("VariablesBono", exportacion_rv()$ultimo_periodo_pyg),
    function() VariablesBono("VariablesBono", hechos_analisis_comercial, clientes_raw_cache, filtros,
      comercial_historico_r, ultimo_periodo_pyg))
  CargarTabUnaVez("GC_Oportunidades", "ui_GC_Oportunidades", function() OportunidadesGestionUI("Oportunidades"),
    function() OportunidadesGestion("Oportunidades", usuario, es_admin, maestro_base_comercial, filtros = filtros))
  CargarTabUnaVez("GC_Planeacion", "ui_GC_Planeacion", function() PlaneacionComercialUI("Planeacion"),
    function() PlaneacionComercial("Planeacion", data_t, filtros))
  CargarTabUnaVez("GC_Pendientes", "ui_GC_Pendientes", function() PendientesGestionUI("Pendientes"),
    function() PendientesGestion("Pendientes", maestro_comercial, usuario,
      corte = shiny::reactive(as.Date(filtros()$fecha[[2]]))))
  CargarTabUnaVez("GC_Tareas", "ui_GC_Tareas", function() TableroTareasUI("Tareas"), function() {
    TableroTareas("Tareas", usuario = usuario, datos = menu_tareas$tareas$datos, es_admin = es_admin,
      actuar = menu_tareas$tareas$actuar,
      abrir = menu_tareas$tareas$abrir, refrescar = menu_tareas$tareas$refrescar)
  })
  CargarTabUnaVez("GC_Alertas", "ui_GC_Alertas", function() AlertasNotificacionesUI("AlertasNotificaciones"))
  CargarTabUnaVez("GC_Fidelidad", "ui_GC_Fidelidad", function() FidelidadRiesgoUI("FidelidadRiesgo"),
    function() FidelidadRiesgo("FidelidadRiesgo",
      reactive(.gc_filtrar_dimensiones(maestro_base_comercial(), filtros())), clientes_raw_cache, filtros))
  CargarTabUnaVez("GC_Cartera", "ui_GC_Cartera", function() CarteraComercialUI("CarteraComercial"),
    function() CarteraComercial("CarteraComercial", cartera_financiera_r, clientes_raw_cache,
      reactive(exportacion_rv()$homologaciones$nit_principal),
      filtros, es_admin, maestro = maestro_comercial))
  
  consulta_modal_solicitada <- reactiveVal(NULL)
  actualizar_clientes_individual <- function() {
    tryCatch(clientes_raw_cache(CargarDatos("CRMNALCLIENTE")), error = function(e) {
      racafeForms::racafeNotify(session, title = "Los cambios se guardaron; falta actualizar la ficha",
        text = conditionMessage(e), type = "warning")
    })
  }
  consulta_modal_iniciada <- FALSE
  abrir_consulta_modal <- function(codigo, nit = "") {
    if (length(codigo) != 1L || is.na(codigo) || nchar(codigo) > 200L) codigo <- ""
    nit <- .crm_nit_historia(nit)
    if (!nzchar(codigo) && (length(nit) != 1L || is.na(nit))) return(invisible(NULL))
    if (!consulta_modal_iniciada) {
      ConsultaIndividual("ConsultaIndividualModal", maestro_base_comercial, clientes_raw_cache, filtros,
        usuario, es_admin, reactive(exportacion_rv()$homologaciones$nit_principal), cartera_financiera_r, notas_datos,
        directorio = reactive(exportacion_rv()$n_cliente), estados = data_cohortes,
        solicitud = consulta_modal_solicitada, data_t = data_t,
        al_actualizar_clientes = actualizar_clientes_individual)
      consulta_modal_iniciada <<- TRUE
    }
    consulta_modal_solicitada(list(codigo = codigo, nit = nit, solicitud = Sys.time()))
    showModal(racafeForms::racafeModal(title = "Consulta individual", size = "ventana", easy_close = TRUE,
      ConsultaIndividualUI("ConsultaIndividualModal")))
  }
  observeEvent(input$PR_Consultar, abrir_consulta_modal(input$PR_Consultar$codigo, input$PR_Consultar$nit))

  ### Prospección ----
  CargarTabUnaVez("PR_Kanban", "ui_PR_Kanban", function() KanbanEmbudoUI("Kanban"),
                  function() KanbanEmbudo("Kanban", usuario, filtros, es_admin, hechos = maestro_base_comercial,
                    presupuestos = clientes_raw_cache, actualizacion = reactive(input$FT_Actualizar),
                    al_consultar = abrir_consulta_modal))
  
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
  
  CargarTabUnaVez("PR_Potencial", "ui_PR_Potencial", function() PotencialMercadoUI("PotencialMercado"),
    function() PotencialMercado("PotencialMercado", usuario, es_admin, maestro_comercial, filtros = filtros))
  
  ### Cohortes de Clientes ----
  segmentos_cohortes <- reactive(.pr_cargar("CRMNALSEGR"))
  data_cohortes <- reactive({
    req(length(filtros()$fecha) == 2L)
    .coha_preparar(segmentos_cohortes(), clientes_raw_cache(), maestro_base_comercial(),
      hoy = as.Date(filtros()$fecha[[2]]))
  })
  CargarTabUnaVez("CO_Estados", "ui_CO_Estados", function() CohortesEstadosUI("CohortesEstados"),
    function() CohortesEstados("CohortesEstados", bd = data_cohortes, data_t = maestro_comercial,
      cierre_oficial = reactive(.gc_fecha_max(segmentos_cohortes()$FecProceso)),
      fecha_limite = reactive(as.Date(filtros()$fecha[[2]]))))

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
  CargarTabUnaVez("PD_SinClasificar", "ui_PD_SinClasificar", function() ProductosSinClasificarUI("SinClasificar"),
    function() ProductosSinClasificar("SinClasificar", usuario, function(x) {
      productos_cache(x)
      .pr_invalidar("CRMNALPRODS")
    },
      operaciones = shiny::reactive(list(exportacion_rv()$homologaciones$lotes, exportacion_rv()$crm_maestro))))
  CargarTabUnaVez("PD_Ejecucion", "ui_PD_Ejecucion", function() ProductosEjecucionUI("ProductosEjecucion"),
    function() ProductosEjecucion("ProductosEjecucion", maestro_comercial, filtros))
  
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

  CargarTabUnaVez("AD_Transversales", "ui_AD_Transversales", function() {
    req(isTRUE(es_admin())); ModulosTransversalesUI("Transversales")
  }, function() {
    req(isTRUE(es_admin()))
    ModulosTransversales("Transversales", usuario, es_admin, maestro_comercial, datos_unidades)
  })
  CargarTabUnaVez("AD_Escalas", "ui_AD_Escalas", function() {
    req(isTRUE(es_admin())); EscalasComercialesUI("Escalas")
  }, function() {
    req(isTRUE(es_admin())); EscalasComerciales("Escalas", es_admin)
  })

  ### Consulta Individual ----
  CargarTabUnaVez("IN_Consulta", "ui_IN_Consulta", function() ConsultaIndividualUI("ConsultaIndividual"),
    function() ConsultaIndividual("ConsultaIndividual", maestro_base_comercial, clientes_raw_cache, filtros,
      usuario, es_admin, reactive(exportacion_rv()$homologaciones$nit_principal), cartera_financiera_r, notas_datos,
      directorio = reactive(exportacion_rv()$n_cliente), estados = data_cohortes,
      solicitud = consulta_solicitada, data_t = data_t,
      al_actualizar_clientes = actualizar_clientes_individual))
  
}





