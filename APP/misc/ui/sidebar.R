sidebar <- bs4DashSidebar(
  status = "danger",
  expandOnHover = FALSE,
  bs4SidebarMenu(
    id = "menu_principal",
    # Generalidades ----
    bs4SidebarMenuItem("Generalidades", icon = icon("clipboard-list"), selected = FALSE,
                       bs4SidebarMenuSubItem("Resumen Ejecutivo", tabName = "GE_Resumen", icon = icon("gauge-high")),
                       bs4SidebarMenuSubItem("Validación Financiera", tabName = "GE_Validacion", icon = icon("code-compare")),
                       bs4SidebarMenuSubItem("Evolución del Negocio", tabName = "GE_Evolucion", icon = icon("chart-line")),
                       bs4SidebarMenuSubItem("Participación y Composición", tabName = "GE_Participacion", icon = icon("chart-pie")),
                       bs4SidebarMenuSubItem("Indicadores de Mercado", tabName = "GE_Comparacion", icon = icon("chart-bar")),
                       bs4SidebarMenuSubItem("Inventarios", tabName = "GE_Inventarios", icon = icon("boxes-stacked"))
    ),
    # Gestión Comercial ----
    bs4SidebarMenuItem("Gestión Comercial", icon = icon("briefcase"), selected = FALSE,
                       bs4SidebarMenuSubItem("Presupuesto", tabName = "GC_Presupuesto", icon = icon("money-bill-trend-up")),
                       bs4SidebarMenuSubItem("Variables (Bono)", tabName = "GC_Variables", icon = icon("medal")),
                       bs4SidebarMenuSubItem("Oportunidades", tabName = "GC_Oportunidades", icon = icon("bullseye")),
                       bs4SidebarMenuSubItem("Pendientes de Gestión", tabName = "GC_Pendientes", icon = icon("list-check")),
                       bs4SidebarMenuSubItem("Notas y Tareas", tabName = "GC_Tareas", icon = icon("tasks")),
                       bs4SidebarMenuSubItem("Alertas y Notificaciones", tabName = "GC_Alertas", icon = icon("bell")),
                       bs4SidebarMenuSubItem("Fidelidad y Riesgo", tabName = "GC_Fidelidad", icon = icon("shield-halved")),
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
                       bs4SidebarMenuSubItem("Potencial de Mercado", tabName = "PR_Potencial", icon = icon("map-location-dot"))
    ),
    # Dinámica de Clientes ----
    bs4SidebarMenuItem("Dinámica de Clientes", icon = icon("people-arrows"), selected = FALSE,
                       bs4SidebarMenuSubItem("Clientes Nuevos", tabName = "DC_Nuevos", icon = icon("user-plus")),
                       bs4SidebarMenuSubItem("Clientes Perdidos", tabName = "DC_Perdidos", icon = icon("user-minus"))
    ),
    # Cohortes de Clientes ----
    bs4SidebarMenuItem("Cohortes de Clientes", icon = icon("chart-gantt"), selected = FALSE,
                       bs4SidebarMenuSubItem("Retención", tabName = "CO_Retencion", icon = icon("people-group")),
                       bs4SidebarMenuSubItem("Ventas", tabName = "CO_Ventas", icon = icon("chart-line")),
                       bs4SidebarMenuSubItem("Margen", tabName = "CO_Margen", icon = icon("dollar-sign")),
                       bs4SidebarMenuSubItem("Frecuencia", tabName = "CO_Frecuencia", icon = icon("clock"))
    ),
    # Herramientas ----
    bs4SidebarMenuItem("Herramientas", icon = icon("screwdriver-wrench"), selected = FALSE,
                       bs4SidebarMenuSubItem("Calculadoras", tabName = "HE_Calculadoras", icon = icon("calculator")),
                       bs4SidebarMenuSubItem("Tablas de Conversión", tabName = "HE_Conversion", icon = icon("table")),
                       bs4SidebarMenuSubItem("Informes", tabName = "HE_Informes", icon = icon("file-export"))
    ),
    # Productos ----
    bs4SidebarMenuItem("Productos", icon = icon("boxes-stacked"), selected = FALSE,
                       bs4SidebarMenuSubItem("Ejecución", tabName = "PD_Ejecucion", icon = icon("chart-column")),
                       bs4SidebarMenuSubItem("Market Basket", tabName = "PD_MarketBasket", icon = icon("basket-shopping")),
                       bs4SidebarMenuSubItem("Administración", tabName = "PD_Administracion", icon = icon("gears"))
    ),
    # Consulta Individual ----
    bs4SidebarMenuItem("Consulta Individual", icon = icon("user"), tabName = "IN_Consulta"),
    # Administración ----
    menuItemOutput("ui_menu_admin")
  )
)

