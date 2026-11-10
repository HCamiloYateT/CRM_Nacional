# Constantes ----
.RESUMEN_EJECUTIVO_SECCIONES <- list(
  list(seccion = "Generalidades", icono = "clipboard-list", cajas = list(
    list(id = "ge_validacion", texto = "Validación Financiera", icono = "file-circle-check",
         formato = "porcentaje", valor = NA_real_, footer = "Cuadre total de sacos en meses cerrados.",
         tab = "GE_Validacion"),
    list(id = "ge_evolucion", texto = "Evolución del Negocio", icono = "chart-line",
         formato = "porcentaje", valor = 0.92, tab = "GE_Evolucion"),
    list(id = "ge_participacion", texto = "Participación y Composición", icono = "chart-pie",
         formato = "porcentaje", valor = 0.68, tab = "GE_Participacion"),
    list(id = "ge_comparacion", texto = "Indicadores de Mercado", icono = "chart-bar",
         formato = "numero", valor = 184.5, tab = "GE_Comparacion"),
    list(id = "ge_inventarios", texto = "Inventarios", icono = "boxes-stacked",
         formato = "entero", valor = 420L, tab = "GE_Inventarios")
  )),
  list(seccion = "Gestión Comercial", icono = "briefcase", cajas = list(
    list(id = "gc_presupuesto_sacos", texto = "Cumplimiento total de sacos", icono = "weight-hanging",
         formato = "porcentaje", valor = NA_real_, footer = "Sacos facturados / presupuesto anual de sacos.",
         tab = "GC_Presupuesto"),
    list(id = "gc_presupuesto_margen", texto = "Cumplimiento total de margen", icono = "money-bill-trend-up",
         formato = "porcentaje", valor = NA_real_, footer = "Margen facturado / presupuesto anual de margen.",
         tab = "GC_Presupuesto"),
    list(id = "gc_variables", texto = "Variables (Bono)", icono = "medal",
         formato = "porcentaje", valor = 0.76, tab = "GC_Variables"),
    list(id = "gc_oportunidades", texto = "Oportunidades", icono = "bullseye",
         formato = "entero", valor = 37L, tab = "GC_Oportunidades"),
    list(id = "gc_pendientes", texto = "Pendientes de Gestión", icono = "list-check",
         formato = "entero", valor = 18L, tab = "GC_Pendientes"),
    list(id = "gc_tareas", texto = "Tareas pendientes", icono = "tasks",
         formato = "entero", valor = NA_real_, tab = "GC_Tareas"),
    list(id = "gc_alertas", texto = "Alertas y Notificaciones", icono = "bell",
         formato = "entero", valor = 6L, tab = "GC_Alertas"),
    list(id = "gc_fidelidad", texto = "Fidelidad y Riesgo", icono = "shield-halved",
         formato = "porcentaje", valor = 0.91, tab = "GC_Fidelidad"),
    list(id = "gc_cartera", texto = "Cartera", icono = "file-invoice-dollar",
         formato = "dinero", valor = 125000000, tab = "GC_Cartera")
  )),
  list(seccion = "Prospección", icono = "filter", cajas = list(
    list(id = "pr_kanban", texto = "Kanban", icono = "columns",
         formato = "entero", valor = 64L, tab = "PR_Kanban"),
    list(id = "pr_contactos", texto = "Contactos", icono = "address-book",
         formato = "entero", valor = 84L, tab = "PR_Contactos"),
    list(id = "pr_prospectos", texto = "Prospectos", icono = "handshake",
         formato = "entero", valor = 43L, tab = "PR_Prospectos"),
    list(id = "pr_leads", texto = "Leads", icono = "bullseye",
         formato = "entero", valor = 28L, tab = "PR_Leads"),
    list(id = "pr_clientes", texto = "Clientes", icono = "users",
         formato = "entero", valor = 126L, tab = "PR_Clientes"),
    list(id = "pr_descartados", texto = "Descartados", icono = "ban",
         formato = "entero", valor = 31L, tab = "PR_Descartados"),
    list(id = "pr_conversion", texto = "Conversión", icono = "funnel-dollar",
         formato = "porcentaje", valor = 0.32, tab = "PR_Conversion"),
    list(id = "pr_potencial", texto = "Potencial de Mercado", icono = "map-location-dot",
         formato = "entero", valor = 215L, tab = "PR_Potencial")
  )),
  list(seccion = "Cohortes de Clientes", icono = "chart-gantt", cajas = list(
    list(id = "co_retencion", texto = "Retención", icono = "people-group",
         formato = "porcentaje", valor = 0.86, tab = "CO_Retencion"),
    list(id = "co_ventas", texto = "Ventas", icono = "chart-line",
         formato = "dinero", valor = 850000000, tab = "CO_Ventas"),
    list(id = "co_margen", texto = "Margen", icono = "dollar-sign",
         formato = "dinero", valor = 126000000, tab = "CO_Margen"),
    list(id = "co_frecuencia", texto = "Frecuencia", icono = "clock",
         formato = "numero", valor = 2.8, tab = "CO_Frecuencia")
  )),
  list(seccion = "Herramientas", icono = "screwdriver-wrench", cajas = list(
    list(id = "he_calculadoras", texto = "Calculadoras", icono = "calculator",
         formato = "entero", valor = 4L, tab = "HE_Calculadoras"),
    list(id = "he_conversion", texto = "Tablas de Conversión", icono = "table",
         formato = "entero", valor = 6L, tab = "HE_Conversion"),
    list(id = "he_informes", texto = "Informes", icono = "file-export",
         formato = "entero", valor = 12L, tab = "HE_Informes")
  )),
  list(seccion = "Productos", icono = "boxes-stacked", cajas = list(
    list(id = "pd_ejecucion", texto = "Productos facturados", icono = "chart-column",
         formato = "entero", valor = NA_real_, footer = "Productos con compra en el periodo del controlbar.",
         tab = "PD_Ejecucion"),
    list(id = "pd_marketbasket", texto = "Market Basket", icono = "basket-shopping",
         formato = "entero", valor = NA_real_, footer = "Reglas retenidas en el último análisis de compras conjuntas.",
         tab = "PD_MarketBasket")
  ))
)

# Funciones ----


.resumen_ejecutivo_seccion_ui <- function(titulo, icono) {
  shiny::tags$div(class = "re-seccion", shiny::icon(icono), shiny::tags$span(titulo))
}

.resumen_ejecutivo_caja_ui <- function(ns, caja) {
  .crm_caja_ui(ns(caja$id), icono = caja$icono, texto = caja$texto,
                mostrar_boton = TRUE, tooltip = paste("Abrir", caja$texto), footer = caja$footer)
}

# Modulo Principal ----
ResumenEjecutivoUI <- function(id) {
  ns <- shiny::NS(id)
  bloques <- lapply(.RESUMEN_EJECUTIVO_SECCIONES, function(seccion) {
    shiny::tagList(
      .resumen_ejecutivo_seccion_ui(seccion$seccion, seccion$icono),
      shiny::fluidRow(lapply(seccion$cajas, function(caja) {
        shiny::column(3, shiny::tags$div(
          class = "re-columna", .resumen_ejecutivo_caja_ui(ns, caja)
        ))
      }))
    )
  })
  shiny::tags$div(
    class = "resumen-ejecutivo",
    .crm_estilos_ui(),
    do.call(shiny::tagList, bloques)
  )
}

ResumenEjecutivo <- function(id, valores = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    for (seccion in .RESUMEN_EJECUTIVO_SECCIONES) {
      for (caja in seccion$cajas) {
        local({
          actual <- caja
          valor_r <- shiny::reactive({
            indicadores <- if (shiny::is.reactive(valores)) valores() else valores
            if (!is.null(indicadores) && actual$id %in% names(indicadores)) {
              valor <- indicadores[[actual$id]]
              if (is.function(valor)) valor <- valor()
              if (length(valor) == 1L && is.numeric(valor) && is.finite(valor)) {
                return(valor)
              }
            }
            actual$valor
          })
          modulo <- racafeModulos::cajaValor_server(actual$id, valor_r = valor_r,
                                        formato = actual$formato, empty = "—")
          shiny::observeEvent(modulo$click_detalle(), {
            bs4Dash::updateTabItems(session = session$rootScope(),
                                    inputId = "menu_principal", selected = actual$tab)
          }, ignoreInit = TRUE)
        })
      }
    }
  })
}



