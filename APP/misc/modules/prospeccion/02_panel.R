# PanelEtapa
PanelEtapaUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    TablaReactableUI(ns("TablaEtapa"), titulo = NULL),
    tags$script(
      HTML(
        "document.addEventListener('click', function(e){
          document.querySelectorAll('.rc-panel-acciones').forEach(function(p){
            if(!p.contains(e.target) && !e.target.closest('.rc-boton-acciones')){
              p.style.display='none';
            }
          });
        });"
      )
    )
  )
}
PanelEtapa <- function(id, usuario, etapa, mostrar_titulo = TRUE, filtro_extra = NULL) {
  stopifnot(etapa %in% ETAPAS_EMBUDO)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    acciones <- .acciones_por_etapa(etapa)
    
    trigger_refresco    <- reactiveVal(0)
    codigo_seleccionado <- reactiveVal(NULL)
    titulo_modal_actual <- reactiveVal(NULL)
    
    # Datos ----
    contactos_base <- reactive({
      trigger_refresco()
      
      CargarDatos("CRMNALCONTACTO") %>%
        derivar_etapa_actual() %>%
        filter(Etapa == etapa) %>%
        select(
          all_of(
            union(
              .COLUMNAS_BASE_PANEL,
              if (etapa %in% c("CONTACTO", "PROSPECTO", "LEAD", "CLIENTE")) {
                "DetOrigen"
              } else if (identical(etapa, "DESCARTADO")) {
                c(
                  "EtapaPreDescarte",
                  "FechaHoraModi",
                  "FechaHoraCrea"
                )
              } else {
                NULL
              }
            )
          )
        )
    })
    
    datos_etapa <- reactive({
      base <- contactos_base()
      
      if (nrow(base) == 0) {
        for (col in .columnas_extra_etapa(etapa))
          base[[col]] <- character()
        
        base$Acciones <- character()
        
        return(
          base %>%
            relocate(Acciones, .before = 1)
        )
      }
      
      if (identical(etapa, "PROSPECTO")) {
        alianzas <- tryCatch({
          dat <- CargarDatos("CRMNALPROSPECTOALIANZA") %>%
            filter(CodContacto %in% base$CodContacto)
          
          dat <- dat %>%
            bind_cols(
              .separar_nit_razon_social(dat$CodClienteAliado)
            )
          
          dat %>%
            group_by(CodContacto) %>%
            summarise(
              NumAlianzas = n(),
              AliadosTexto = if (n() == 1) {
                RazonSocial[1]
              } else {
                paste0(n(), " aliados")
              },
              .groups = "drop"
            )
        }, error = function(e) {
          data.frame(
            CodContacto = character(),
            NumAlianzas = integer(),
            AliadosTexto = character()
          )
        })
        
        base <- base %>%
          left_join(alianzas, by = "CodContacto") %>%
          mutate(
            NumAlianzas = coalesce(NumAlianzas, 0L),
            AliadosTexto = coalesce(AliadosTexto, "Sin alianzas")
          )
        
      } else if (identical(etapa, "LEAD")) {
        leads <- tryCatch({
          CargarDatos("CONTACTOLEAD") %>%
            filter(CodContacto %in% base$CodContacto) %>%
            select(
              CodContacto,
              Segmento,
              LinNegocio,
              AsesorLead = Asesor,
              FechaConversion
            )
        }, error = function(e) {
          data.frame(
            CodContacto = character(),
            Segmento = character(),
            LinNegocio = character(),
            AsesorLead = character(),
            FechaConversion = as.POSIXct(character())
          )
        })
        
        crea_contacto <- CargarDatos("CRMNALCONTACTO") %>%
          filter(CodContacto %in% base$CodContacto) %>%
          select(
            CodContacto,
            FechaHoraCrea
          )
        
        rel <- tryCatch(
          CargarDatos("CRMNALRELACIONAMIENTO") %>%
            filter(CodContacto %in% base$CodContacto) %>%
            mutate(
              FechaHoraCrea = as_datetime(FechaHoraCrea)
            ),
          error = function(e) {
            data.frame(
              CodContacto = character(),
              FechaHoraCrea = as.POSIXct(character())
            )
          }
        )
        
        base <- base %>%
          left_join(leads, by = "CodContacto") %>%
          left_join(crea_contacto, by = "CodContacto") %>%
          mutate(
            FechaConversion = as_datetime(FechaConversion),
            FechaHoraCrea = as_datetime(FechaHoraCrea),
            TiempoConversionDias = round(
              as.numeric(
                difftime(
                  FechaConversion,
                  FechaHoraCrea,
                  units = "days"
                )
              ),
              1
            )
          )
        
        gestiones <- rel %>%
          inner_join(
            base %>%
              select(
                CodContacto,
                FechaHoraCreaContacto = FechaHoraCrea,
                FechaConversion
              ),
            by = "CodContacto"
          ) %>%
          group_by(CodContacto) %>%
          summarise(
            GestionesPrevias = sum(
              FechaHoraCrea >= FechaHoraCreaContacto &
                FechaHoraCrea < FechaConversion,
              na.rm = TRUE
            ),
            GestionesLead = sum(
              FechaHoraCrea >= FechaConversion,
              na.rm = TRUE
            ),
            .groups = "drop"
          )
        
        base <- base %>%
          left_join(gestiones, by = "CodContacto") %>%
          mutate(
            GestionesPrevias = coalesce(
              GestionesPrevias,
              0L
            ),
            GestionesLead = coalesce(
              GestionesLead,
              0L
            )
          )
        
      } else if (identical(etapa, "CLIENTE")) {
        codigos <- base$CodContacto
        
        clientes <- .obtener_conversion_cliente(codigos)
        leads <- .obtener_conversion_lead(codigos)
        
        fechas_contacto <- CargarDatos("CRMNALCONTACTO") %>%
          filter(CodContacto %in% codigos) %>%
          transmute(CodContacto, FechaHoraCrea = as_datetime(FechaHoraCrea))
        
        rel <- tryCatch(
          CargarDatos("CRMNALRELACIONAMIENTO") %>%
            filter(CodContacto %in% codigos) %>%
            mutate(FechaHoraCrea = as_datetime(FechaHoraCrea)),
          error = function(e) data.frame(CodContacto = character(), FechaHoraCrea = as.POSIXct(character()))
        )
        
        base <- base %>%
          left_join(fechas_contacto, by = "CodContacto") %>%
          left_join(leads, by = "CodContacto") %>%
          left_join(clientes, by = "CodContacto") %>%
          mutate(
            TiempoContactoCliente = round(
              as.numeric(difftime(FechaConversionCliente, FechaHoraCrea, units = "days")), 1
            ),
            TiempoLeadCliente = round(
              as.numeric(difftime(FechaConversionCliente, FechaConversionLead, units = "days")), 1
            )
          )
        
        gestiones <- rel %>%
          inner_join(
            base %>%
              select(CodContacto, FechaHoraCreaContacto = FechaHoraCrea,
                     FechaConversionLead, FechaConversionCliente),
            by = "CodContacto"
          ) %>%
          group_by(CodContacto) %>%
          summarise(
            GestionesContactoCliente = sum(
              FechaHoraCrea >= FechaHoraCreaContacto & FechaHoraCrea < FechaConversionCliente, na.rm = TRUE
            ),
            GestionesLeadCliente = sum(
              !is.na(FechaConversionLead) & FechaHoraCrea >= FechaConversionLead &
                FechaHoraCrea < FechaConversionCliente, na.rm = TRUE
            ),
            GestionesCliente = sum(FechaHoraCrea >= FechaConversionCliente, na.rm = TRUE),
            .groups = "drop"
          )
        
        base <- base %>%
          left_join(gestiones, by = "CodContacto") %>%
          mutate(
            GestionesContactoCliente = coalesce(GestionesContactoCliente, 0L),
            GestionesLeadCliente = coalesce(GestionesLeadCliente, 0L),
            GestionesCliente = coalesce(GestionesCliente, 0L)
          )
        
      } else if (identical(etapa, "CONTACTO")) {
        crea_info <- tryCatch({
          CargarDatos("CRMNALCONTACTO") %>%
            filter(CodContacto %in% base$CodContacto) %>%
            select(
              CodContacto,
              UsuarioCrea,
              FechaHoraCrea
            )
        }, error = function(e) {
          data.frame(
            CodContacto = character(),
            UsuarioCrea = character(),
            FechaHoraCrea = as.POSIXct(character())
          )
        })
        
        base <- base %>%
          left_join(
            crea_info,
            by = "CodContacto"
          ) %>%
          mutate(
            FechaHoraCrea = as_datetime(FechaHoraCrea),
            DiasSinGestion = as.numeric(
              difftime(
                Sys.time(),
                FechaHoraCrea,
                units = "days"
              )
            )
          )
        
        relac <- .dias_sin_relacionamiento_bulk(
          base$CodContacto,
          base$FechaHoraCrea
        )
        
        base <- base %>%
          left_join(
            relac,
            by = "CodContacto"
          )
        
      } else if (identical(etapa, "DESCARTADO")) {
        base <- base %>%
          mutate(
            FechaHoraModi = as_datetime(FechaHoraModi),
            FechaHoraCrea = as_datetime(FechaHoraCrea),
            EtapaPreDescarte = ifelse(
              is.na(EtapaPreDescarte),
              "CONTACTO",
              EtapaPreDescarte
            ),
            DiasDesdeDescarte = as.numeric(
              difftime(
                Sys.time(),
                FechaHoraModi,
                units = "days"
              )
            ),
            DiasHastaDescarte = as.numeric(
              difftime(
                FechaHoraModi,
                FechaHoraCrea,
                units = "days"
              )
            )
          ) %>%
          left_join(
            obtener_ultimo_motivo_descarte(),
            by = "CodContacto"
          ) %>%
          mutate(
            CategoriaMotivo = .categoria_motivo_por_etapa(
              Motivo,
              EtapaPreDescarte
            )
          ) %>%
          select(
            -FechaHoraModi,
            -FechaHoraCrea
          )
      }
      
      if (!is.null(filtro_extra))
        base <- filtro_extra(base)
      
      base %>%
        mutate(
          Acciones = CodContacto
        ) %>%
        relocate(
          Acciones,
          .before = 1
        )
    })
    
    # Outputs ----
    output$Titulo <- renderUI({
      req(mostrar_titulo)
      
      h4(
        paste0(
          "Gestión — Etapa ",
          etapa,
          " (",
          nrow(datos_etapa()),
          ")"
        )
      )
    })
    
    columnas_visibles_panel <- function(etapa) {
      c(
        "Acciones",
        "PerCod",
        "PerRazSoc",
        "Origen",
        if (etapa %in% c("CONTACTO", "PROSPECTO", "LEAD")) {
          "DetOrigen"
        } else {
          NULL
        },
        .columnas_extra_etapa(etapa)
      )
    }
    
    modulo_tabla <- TablaReactable(
      id = "TablaEtapa",
      data = datos_etapa,
      columnas = columnas_visibles_panel(etapa),
      col_specs = c(
        list(
          Acciones = .coldef_dropdown_acciones(
            acciones,
            ns
          ),
          PerCod = reactable::colDef(
            name = "NIT",
            minWidth = 90
          ),
          PerRazSoc = reactable::colDef(
            name = "Razón Social",
            minWidth = 200
          ),
          Origen = reactable::colDef(
            name = "Origen",
            minWidth = 110
          ),
          DetOrigen = reactable::colDef(
            name = "Detalle Origen",
            minWidth = 140
          )
        ),
        .coldefs_extra_etapa(etapa)
      ),
      modo_seleccion = "ninguno",
      id_col = "CodContacto",
      cols_activos = character(0),
      sortable = TRUE,
      searchable = TRUE,
      page_size = 15,
      compact = TRUE,
      mostrar_badge = FALSE,
      mostrar_nota = FALSE,
      cols_heatmap = if (identical(etapa, "CONTACTO")) {
        "DiasSinRelacionamiento"
      } else if (identical(etapa, "LEAD")) {
        "TiempoConversionDias"
      } else {
        NULL
      }
    )
    
    # Observers ----
    .REGISTRO_MODULOS_PANEL <- list(
      Editar = list(
        ui = function() {
          EditarUI(ns("Editar"))
        }
      ),
      Relacionamiento = list(
        ui = function() {
          RelacionamientoUI(
            ns("Relacionamiento")
          )
        }
      ),
      Promover = list(
        ui = function() {
          PromoverUI(ns("Promover"))
        }
      ),
      Descartar = list(
        ui = function() {
          DescartarUI(ns("Descartar"))
        }
      ),
      CrearOportunidad = list(
        ui = function() {
          CrearOportunidadUI(
            ns("CrearOportunidad")
          )
        }
      )
    )
    
    accion_seleccionada <- reactiveVal(NULL)
    codigo_reactivar    <- reactiveVal(NULL)
    trigger_reactivar   <- reactiveVal(0)
    
    observeEvent(input$AccionSeleccionada, {
      seleccion <- input$AccionSeleccionada
      req(
        seleccion$codigo,
        seleccion$accion
      )
      
      if (identical(
        seleccion$accion,
        "Reactivar"
      )) {
        codigo_reactivar(
          seleccion$codigo
        )
        trigger_reactivar(
          isolate(
            trigger_reactivar()
          ) + 1
        )
        return(
          invisible(NULL)
        )
      }
      
      codigo_seleccionado(
        seleccion$codigo
      )
      accion_seleccionada(
        seleccion$accion
      )
      titulo_modal_actual(
        acciones[[seleccion$accion]] %||%
          seleccion$accion
      )
      
      clase_modal <- .CONFIG_ACCIONES_EMBUDO[[seleccion$accion]]$modal %||% "subventana2"
      
      modal_construido <- modalDialog(
        title = titulo_modal_actual(),
        tagList(
          shinyjs::hidden(
            tags$div(
              id = ns("PreloaderModal"),
              style = paste0(
                "background:",
                preloader_actualizar$color,
                "; text-align:center; padding:30px;"
              ),
              preloader_actualizar$html
            )
          ),
          uiOutput(
            ns("ModalContenido")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      )
      
      showModal(
        htmltools::tagAppendAttributes(
          modal_construido,
          class = clase_modal
        )
      )
    })
    
    output$ModalContenido <- renderUI({
      req(
        accion_seleccionada()
      )
      
      modulo <- .REGISTRO_MODULOS_PANEL[[accion_seleccionada()]]
      
      req(modulo)
      
      modulo$ui()
    })
    
    observeEvent(
      input$AccionSeleccionada,
      {
        shinyjs::show(
          id = "PreloaderModal",
          anim = FALSE
        )
      },
      priority = 10
    )
    
    outputOptions(
      output,
      "ModalContenido",
      suspendWhenHidden = FALSE
    )
    
    observe({
      req(
        accion_seleccionada()
      )
      
      shinyjs::hide(
        id = "PreloaderModal",
        anim = FALSE
      )
    })
    
    # Instanciacion unica de cada submodulo
    modulo_editar <- Editar(
      "Editar",
      usuario = usuario,
      codigo_contacto = reactive(
        codigo_seleccionado()
      )
    )
    
    modulo_relacion <- Relacionamiento(
      "Relacionamiento",
      usuario = usuario,
      codigo_contacto = reactive(
        codigo_seleccionado()
      )
    )
    
    modulo_promover <- Promover(
      "Promover",
      usuario = usuario,
      codigo_contacto = reactive(
        codigo_seleccionado()
      )
    )
    
    modulo_descartar <- Descartar(
      "Descartar",
      usuario = usuario,
      codigo_contacto = reactive(
        codigo_seleccionado()
      )
    )
    
    modulo_oportunidad <- CrearOportunidad(
      "CrearOportunidad",
      usuario = usuario,
      codigo_contacto = reactive(
        codigo_seleccionado()
      )
    )
    
    modulo_reactivar <- Reactivar(
      "Reactivar",
      usuario = usuario,
      codigo_contacto = reactive(
        codigo_reactivar()
      ),
      disparador = reactive(
        trigger_reactivar()
      )
    )
    
    contadores_retorno <- list(
      modulo_editar$actualizaciones,
      modulo_promover$etapa,
      modulo_descartar$etapa,
      modulo_oportunidad$creaciones,
      modulo_relacion$gestiones,
      modulo_reactivar$n
    )
    
    lapply(
      contadores_retorno,
      function(contador) {
        if (is.function(contador)) {
          observeEvent(
            contador(),
            {
              trigger_refresco(
                trigger_refresco() + 1
              )
            },
            ignoreInit = TRUE
          )
        }
      }
    )
    
    list(
      datos = datos_etapa
    )
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title = "Prueba Panel Etapa",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    includeCSS(
      paste0(
        "https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
        "refs/heads/main/Styles/style.css"
      )
    ),
    useShinyjs(),
    box(
      title = "Etapa CONTACTO — sin filtro",
      width = 12,
      PanelEtapaUI("PanelContacto")
    ),
    box(
      title = "Etapa CONTACTO — solo 30+ dias (filtro_extra)",
      width = 12,
      PanelEtapaUI("PanelContacto30")
    ),
    box(
      title = "Etapa LEAD (sin cambios)",
      width = 12,
      PanelEtapaUI("PanelLead")
    ),
    box(
      title = "Etapa DESCARTADO (nueva)",
      width = 12,
      PanelEtapaUI("PanelDescartado")
    )
  )
)

server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  
  PanelEtapa(
    "PanelContacto",
    usuario = usuario_sesion,
    etapa = "CONTACTO",
    acciones = c(
      Editar = "Editar Contacto",
      Relacionamiento = "Gestión Comercial",
      Promover = "Promover Etapa",
      Descartar = "Descartar Registro",
      CrearOportunidad = "Nueva Oportunidad"
    )
  )
  
  PanelEtapa(
    "PanelContacto30",
    usuario = usuario_sesion,
    etapa = "CONTACTO",
    filtro_extra = function(df) {
      filter(
        df,
        DiasSinGestion >= 30
      )
    },
    acciones = c(
      Editar = "Editar Contacto",
      Relacionamiento = "Gestión Comercial",
      Promover = "Promover Etapa",
      Descartar = "Descartar Registro",
      CrearOportunidad = "Nueva Oportunidad"
    )
  )
  
  PanelEtapa(
    "PanelLead",
    usuario = usuario_sesion,
    etapa = "LEAD",
    acciones = c(
      Editar = "Editar Contacto",
      Relacionamiento = "Gestión Comercial",
      Promover = "Promover Etapa",
      Descartar = "Descartar Registro",
      CrearOportunidad = "Nueva Oportunidad"
    )
  )
  
  PanelEtapa(
    "PanelDescartado",
    usuario = usuario_sesion,
    etapa = "DESCARTADO",
    acciones = c(
      Editar = "Editar Contacto",
      Relacionamiento = "Comentar",
      Reactivar = "Reactivar"
    )
  )
}

shinyApp(ui, server)
