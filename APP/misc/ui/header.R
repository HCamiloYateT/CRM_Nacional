# Header ----
header <- bs4DashNavbar(status = "white", border = FALSE, sidebarIcon = icon("bars"),
                        title = dashboardBrand(title = "CRM Nacional",
                                               href = "https://analitica.racafe.com/PortalAnalitica/",
                                               image = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png"
                                               ),
                        controlbarIcon = icon("gears"),
                        leftUi = tagList(tags$li(class = "dropdown",
                                                 tags$style(HTML("

                            .crm-header-table, .crm-header-table tr, .crm-header-table td,
                            .crm-header-table th {background:transparent!important;box-shadow:none!important;}
                            .crm-header-table td, .crm-header-table th {border-top:0!important;border-bottom:0!important;}
                            .crm-header-item:hover {background:#f8fafc!important;}
                            .crm-header-item:focus {background:#f8fafc!important;}
                            .rf-metric-grid .rf-metric {min-width:0;}
                            .crm-header-item { padding:8px 12px!important; white-space:normal!important; }
                            .crm-header-item + .crm-header-item { border-top:1px solid #f1f2f4; }
                            .crm-header-item-titulo { font-size:12px; font-weight:600; line-height:1.3;
                              overflow-wrap:anywhere; color:#344054; }
                            .crm-header-item-detalle { font-size:10px; color:#667085; margin-top:2px; }
                            .crm-header-item-badges { display:flex; flex-wrap:wrap; gap:4px; margin-top:4px; }
                            .crm-header-vacio { font-size:11px; color:#667085; }
                            #li_menu_tareas .dropdown-menu, #li_menu_clientes .dropdown-menu,
                            #li_menu_clientes_ant .dropdown-menu { width:390px; max-width:calc(100vw - 20px);
                              max-height:520px; overflow-y:auto; }
                            #li_menu_tareas .dropdown-menu { left:auto!important; right:0!important; }
                            #li_menu_indicadores, #li_menu_tareas, #li_menu_clientes, #li_menu_clientes_ant,
                            #li_menu_migrar_cartera { flex:0 0 auto!important; width:auto!important; min-width:0!important; }
                            #li_menu_indicadores > a.nav-link, #li_menu_tareas > a.nav-link,
                            #li_menu_clientes > a.nav-link,
                            #li_menu_clientes_ant > a.nav-link,
                            #li_menu_migrar_cartera > a.nav-link { width:38px!important; height:38px!important;
                              min-width:38px!important; padding:8px!important; display:inline-flex!important;
                              align-items:center;justify-content:center; }
                                                 ")),
                                                 style = "display:flex;align-items:center;gap:8px;padding:8px 12px;cursor:default;",
                                                 tags$script(HTML("
                                                   if (!window.crmHeaderRetenerMenu) {
                                                     window.crmHeaderRetenerMenu = true;
                                                     window.crmMenuSeleccionado = null;
                                                     document.addEventListener('click', function(e) {
                                                       var fila = e.target.closest(
                                                         '#li_menu_tareas .crm-header-item,' +
                                                         '#li_menu_clientes .crm-header-item,' +
                                                         '#li_menu_clientes_ant .crm-header-item');
                                                       if (!fila) return;
                                                       var li = fila.closest('li.nav-item.dropdown');
                                                       if (li) window.crmMenuSeleccionado = li.id;
                                                     }, true);
                                                     $(document).on('shown.bs.modal hidden.bs.modal', '.modal', function() {
                                                       var id = window.crmMenuSeleccionado;
                                                       if (!id) return;
                                                       setTimeout(function() {
                                                         var menu = $('#' + id);
                                                         menu.addClass('show');
                                                         menu.children('.dropdown-menu').addClass('show');
                                                         menu.children('a.nav-link').attr('aria-expanded', 'true');
                                                       }, 0);
                                                     });
                                                     $(document).on('click', '.crm-header-item', function(e) {
                                                       e.stopPropagation();
                                                     });
                                                   }
                                                 ")),
                                                 tags$span(uiOutput("user")),
                                                 racafeShiny::Boton("FT_Actualizar", label = NULL, icono = "sync", size = "xxs",
                                                                    title = "Actualizar", color_fondo = "#6c757d",
                                                                    color_hover = "#DA291C"),
                                                 racafeShiny::BotonDescarga("FT_DescargarClientes", icono = "users", size = "xxs",
                                                                            title = "Descargar Clientes", color_fondo = "#6c757d",
                                                                            color_hover = "#DA291C")
                                                 )
                                         ),
                        rightUi = tagList(
                          htmltools::tagAppendAttributes(
                            racafeModulos::MenuHeaderUI("MenuIndicadores", icon = icon("arrow-trend-up"),
                                                       min_width = "400px", title = "Indicadores"),
                            id = "li_menu_indicadores"),
                          htmltools::tagAppendAttributes(
                            racafeModulos::MenuHeaderUI("MenuTareas", icon = icon("bars-progress"),
                                                       min_width = "360px", title = "Tareas"),
                            id = "li_menu_tareas"),
                          htmltools::tagAppendAttributes(racafeModulos::MenuHeaderUI("MenuClientes", icon = icon("users"),
                                                                                     min_width = "360px", title = "Clientes sin información"),
                                                         id = "li_menu_clientes"),
                          htmltools::tagAppendAttributes(racafeModulos::MenuHeaderUI("MenuClientesAnt", icon = icon("user-clock"),
                                                                                     min_width = "360px", title = "Clientes con información antigüa"
                                                                                     ),
                                                         id = "li_menu_clientes_ant"),
                          htmltools::tagAppendAttributes(racafeModulos::MenuHeaderUI("MenuMigrarCartera", icon = icon("people-arrows"),
                                                                                     min_width = "360px", title = "Migrar cartera"),
                                                         id = "li_menu_migrar_cartera")
                          )
                        )
