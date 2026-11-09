body <- bs4DashBody(
  useShinyjs(),
  .recursos_compartidos,
  .crm_loader_ui(),
  bs4TabItems(
    # Generalidades ----
    bs4TabItem(tabName = "GE_Resumen", uiOutput("ui_GE_Resumen")),
    bs4TabItem(tabName = "GE_Validacion", uiOutput("ui_GE_Validacion")),
    bs4TabItem(tabName = "GE_Evolucion", uiOutput("ui_GE_Evolucion")),
    bs4TabItem(tabName = "GE_Participacion", uiOutput("ui_GE_Participacion")),
    bs4TabItem(tabName = "GE_Comparacion", uiOutput("ui_GE_Comparacion")),
    bs4TabItem(tabName = "GE_Inventarios", uiOutput("ui_GE_Inventarios")),
    
    # Gestión Comercial ----
    bs4TabItem(tabName = "GC_Presupuesto", uiOutput("ui_GC_Presupuesto")),
    bs4TabItem(tabName = "GC_Variables", uiOutput("ui_GC_Variables")),
    bs4TabItem(tabName = "GC_Oportunidades", uiOutput("ui_GC_Oportunidades")),
    bs4TabItem(tabName = "GC_Pendientes", uiOutput("ui_GC_Pendientes")),
    bs4TabItem(tabName = "GC_Tareas", uiOutput("ui_GC_Tareas")),
    bs4TabItem(tabName = "GC_Alertas", uiOutput("ui_GC_Alertas")),
    bs4TabItem(tabName = "GC_Fidelidad", uiOutput("ui_GC_Fidelidad")),
    bs4TabItem(tabName = "GC_Cartera", uiOutput("ui_GC_Cartera")),
    
    # Prospección ----
    bs4TabItem(tabName = "PR_Kanban", uiOutput("ui_PR_Kanban")),
    bs4TabItem(tabName = "PR_Contactos", uiOutput("ui_PR_Contactos")),
    bs4TabItem(tabName = "PR_Prospectos", uiOutput("ui_PR_Prospectos")),
    bs4TabItem(tabName = "PR_Leads", uiOutput("ui_PR_Leads")),
    bs4TabItem(tabName = "PR_Clientes", uiOutput("ui_PR_Clientes")),
    bs4TabItem(tabName = "PR_Descartados", uiOutput("ui_PR_Descartados")),
    bs4TabItem(tabName = "PR_Conversion", uiOutput("ui_PR_Conversion")),
    bs4TabItem(tabName = "PR_Potencial", uiOutput("ui_PR_Potencial")),
    
    # Cohortes de Clientes ----
    bs4TabItem(tabName = "CO_Estados", uiOutput("ui_CO_Estados")),
    bs4TabItem(tabName = "CO_Retencion", uiOutput("ui_CO_Retencion")),
    bs4TabItem(tabName = "CO_Ventas", uiOutput("ui_CO_Ventas")),
    bs4TabItem(tabName = "CO_Margen", uiOutput("ui_CO_Margen")),
    bs4TabItem(tabName = "CO_Frecuencia", uiOutput("ui_CO_Frecuencia")),
    
    # Herramientas ----
    bs4TabItem(tabName = "HE_Calculadoras", uiOutput("ui_HE_Calculadoras")),
    bs4TabItem(tabName = "HE_Conversion", uiOutput("ui_HE_Conversion")),
    bs4TabItem(tabName = "HE_Informes", uiOutput("ui_HE_Informes")),
    
    # Productos ----
    bs4TabItem(tabName = "PD_Ejecucion", uiOutput("ui_PD_Ejecucion")),
    bs4TabItem(tabName = "PD_MarketBasket", uiOutput("ui_PD_MarketBasket")),
    bs4TabItem(tabName = "PD_SinClasificar", uiOutput("ui_PD_SinClasificar")),
    
    # Administración ----
    bs4TabItem(tabName = "AD_Catalogos", uiOutput("ui_AD_Administrar")),

    # Consulta Individual ----
    bs4TabItem(tabName = "IN_Consulta", uiOutput("ui_IN_Consulta"))
  )
)


