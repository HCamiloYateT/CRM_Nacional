# Header ----
header <- bs4DashNavbar(status = "white", border = FALSE, sidebarIcon = icon("bars"),
                        title = dashboardBrand(title = "CRM Nacional",
                                               href = "https://analitica.racafe.com/PortalAnalitica/",
                                               image = paste0(.url_compartido, "/img/logo2.png")
                                               ),
                        controlbarIcon = icon("gears"),
                        leftUi = tagList(tags$li(class = "dropdown",
                                                 style = "display:flex;align-items:center;gap:8px;padding:8px 12px;cursor:default;",
                                                 tags$span(uiOutput("user")),
                                                 racafeShiny::Boton("FT_Actualizar", label = NULL, icono = "sync", size = "xxs",
                                                                    title = "Actualizar", color_fondo = "#6c757d",
                                                                    color_hover = "#DA291C"),
                                                 shiny::tagAppendAttributes(
                                                   racafeForms::racafeButtonDownload("FT_DescargarClientes",
                                                     label = NULL, icon = "users", size = "xxs", variant = "secondary"),
                                                   title = "Descargar clientes"),
                                                 div(class = "dropdown",
                                                   tagAppendAttributes(racafeForms::racafeButton("escalas_color", NULL,
                                                     icon = "palette", title = "Escalas de color", variant = "secondary",
                                                     size = "xs"), `data-toggle` = "dropdown", `aria-haspopup` = "true"),
                                                   div(class = "dropdown-menu kb-scale-menu",
                                                     uiOutput("leyenda_dimensiones"),
                                                     tags$strong("ETAPA DE LA ENTIDAD"),
                                                     lapply(ETAPAS_EMBUDO, function(etapa) div(
                                                       tags$span(class = "kb-scale-swatch", style = paste0("background:",
                                                         unname(.crm_colores("etapa", etapa)))), etapa)),
                                                     tags$hr(), tags$strong("PRIORIDAD DE TAREAS"),
                                                     lapply(names(.COLORES_PRIORIDAD), function(prioridad) div(
                                                       tags$span(class = "kb-scale-swatch", style = paste0("background:",
                                                         .COLORES_PRIORIDAD[[prioridad]])), prioridad))))
                                                 )
                                         ),
                        rightUi = tagList(
                          racafeModulos::MenuHeaderUI("MenuIndicadores", icon = icon("arrow-trend-up"),
                            min_width = "400px", title = "Indicadores"),
                          racafeModulos::MenuHeaderUI("MenuTareas", icon = icon("bars-progress"),
                            min_width = "390px", title = "Tareas", keep_open_on_select = TRUE),
                          racafeModulos::MenuHeaderUI("MenuClientes", icon = icon("users"),
                            min_width = "390px", title = "Clientes sin información", keep_open_on_select = TRUE),
                          racafeModulos::MenuHeaderUI("MenuClientesAnt", icon = icon("user-clock"),
                            min_width = "390px", title = "Clientes con información antigüa",
                            keep_open_on_select = TRUE),
                          racafeModulos::MenuHeaderUI("MenuMigrarCartera", icon = icon("people-arrows"),
                            min_width = "360px", title = "Migrar cartera", direct = TRUE)
                        ))

