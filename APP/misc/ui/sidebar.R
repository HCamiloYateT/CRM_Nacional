sidebar <- bs4DashSidebar(
  status = "danger",
  expandOnHover = FALSE,
  bs4SidebarMenu(
    id = "menu_principal",
    # Generalidades ----
    bs4SidebarMenuItem("Generalidades", icon = icon("clipboard-list"), selected = FALSE,
                       bs4SidebarMenuSubItem("Resumen ejecutivo", tabName = "GE_Resumen", icon = icon("gauge-high")),
                       bs4SidebarMenuSubItem("Validación financiera", tabName = "GE_Validacion", icon = icon("code-compare")),
                       bs4SidebarMenuSubItem("Evolución del negocio", tabName = "GE_Evolucion", icon = icon("chart-line")),
                       bs4SidebarMenuSubItem("Participación y composición", tabName = "GE_Participacion", icon = icon("chart-pie")),
                       bs4SidebarMenuSubItem("Indicadores de mercado", tabName = "GE_Comparacion", icon = icon("chart-bar")),
                       bs4SidebarMenuSubItem("Inventarios", tabName = "GE_Inventarios", icon = icon("boxes-stacked"))
    ),
    # Gestión Comercial ----
    bs4SidebarMenuItem("Gestión comercial", icon = icon("briefcase"), selected = FALSE,
                       bs4SidebarMenuSubItem("Presupuesto", tabName = "GC_Presupuesto", icon = icon("money-bill-trend-up")),
                       bs4SidebarMenuSubItem("Variables (Bono)", tabName = "GC_Variables", icon = icon("medal")),
                       bs4SidebarMenuSubItem("Oportunidades", tabName = "GC_Oportunidades", icon = icon("bullseye")),
                       bs4SidebarMenuSubItem("Pendientes de gestión", tabName = "GC_Pendientes", icon = icon("list-check")),
                       bs4SidebarMenuSubItem("Trazabilidad de pedidos", tabName = "TR_Pedido", icon = icon("timeline")),
                       bs4SidebarMenuSubItem("Notas y tareas", tabName = "GC_Tareas", icon = icon("tasks")),
                       bs4SidebarMenuSubItem("Fidelidad y riesgo", tabName = "GC_Fidelidad", icon = icon("shield-halved")),
                       bs4SidebarMenuSubItem("Cartera", tabName = "GC_Cartera", icon = icon("file-invoice-dollar"))
    ),
    # Prospección ----
    bs4SidebarMenuItem("Prospección", icon = icon("filter"), selected = FALSE,
                       bs4SidebarMenuSubItem("Kanban", tabName = "PR_Kanban", icon = icon("columns")),
                       bs4SidebarMenuSubItem("Contactos", tabName = "PR_Contactos", icon = icon("address-book")),
                       bs4SidebarMenuSubItem("Prospectos", tabName = "PR_Prospectos", icon = icon("handshake")),
                       bs4SidebarMenuSubItem("Leads", tabName = "PR_Leads", icon = icon("bullseye")),
                       bs4SidebarMenuSubItem("Clientes", tabName = "PR_Clientes", icon = icon("users")),
                       bs4SidebarMenuSubItem("Descartados", tabName = "PR_Descartados", icon = icon("ban")),
                       bs4SidebarMenuSubItem("Conversión", tabName = "PR_Conversion", icon = icon("funnel-dollar")),
                       bs4SidebarMenuSubItem("Potencial de mercado", tabName = "PR_Potencial", icon = icon("map-location-dot"))
    ),
    # Cohortes de Clientes ----
    bs4SidebarMenuItem("Cohortes de clientes", icon = icon("chart-gantt"), selected = FALSE,
                       bs4SidebarMenuSubItem("Estados y Transiciones", tabName = "CO_Estados",
                                             icon = icon("people-arrows"))
    ),
    # Herramientas ----
    bs4SidebarMenuItem("Herramientas", icon = icon("screwdriver-wrench"), selected = FALSE,
                       bs4SidebarMenuSubItem("Calculadoras", tabName = "HE_Calculadoras", icon = icon("calculator"))
    ),
    # Productos ----
    bs4SidebarMenuItem("Productos", icon = icon("boxes-stacked"), selected = FALSE,
                       bs4SidebarMenuSubItem("Ejecución", tabName = "PD_Ejecucion", icon = icon("chart-column")),
                       bs4SidebarMenuSubItem("Market Basket", tabName = "PD_MarketBasket", icon = icon("basket-shopping")),
                       bs4SidebarMenuSubItem("Productos sin clasificar", tabName = "PD_SinClasificar",
                         icon = icon("tags"))
    ),
    # Consulta Individual ----
    bs4SidebarMenuItem("Consulta individual", icon = icon("user"), tabName = "IN_Consulta"),
    # Administración ----
    menuItemOutput("ui_menu_admin")
  )
)

