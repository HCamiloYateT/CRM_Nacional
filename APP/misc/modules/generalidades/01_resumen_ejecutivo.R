# Constantes ----
.RESUMEN_EJECUTIVO_SECCIONES <- list(
  list(seccion = "Generalidades", icono = "clipboard-list", cajas = list(
    list(id = "ge_validacion", texto = "Validación Financiera", icono = "file-circle-check",
         formato = "porcentaje", valor = 0.85, tab = "GE_Validacion"),
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
    list(id = "gc_resumen", texto = "Resumen", icono = "gauge-high",
         formato = "entero", valor = 82L, tab = "GC_Resumen"),
    list(id = "gc_presupuesto", texto = "Presupuesto", icono = "money-bill-trend-up",
         formato = "porcentaje", valor = 0.87, tab = "GC_Presupuesto"),
    list(id = "gc_variables", texto = "Variables (Bono)", icono = "medal",
         formato = "porcentaje", valor = 0.76, tab = "GC_Variables"),
    list(id = "gc_oportunidades", texto = "Oportunidades", icono = "bullseye",
         formato = "entero", valor = 37L, tab = "GC_Oportunidades"),
    list(id = "gc_pendientes", texto = "Pendientes de Gestión", icono = "list-check",
         formato = "entero", valor = 18L, tab = "GC_Pendientes"),
    list(id = "gc_tareas", texto = "Notas y Tareas", icono = "tasks",
         formato = "entero", valor = 9L, tab = "GC_Tareas"),
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
  list(seccion = "Dinámica de Clientes", icono = "people-arrows", cajas = list(
    list(id = "dc_nuevos", texto = "Clientes Nuevos", icono = "user-plus",
         formato = "entero", valor = 14L, tab = "DC_Nuevos"),
    list(id = "dc_perdidos", texto = "Clientes Perdidos", icono = "user-minus",
         formato = "entero", valor = 5L, tab = "DC_Perdidos")
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
    list(id = "pd_ejecucion", texto = "Ejecución", icono = "chart-column",
         formato = "porcentaje", valor = 0.94, tab = "PD_Ejecucion"),
    list(id = "pd_marketbasket", texto = "Market Basket", icono = "basket-shopping",
         formato = "entero", valor = 24L, tab = "PD_MarketBasket"),
    list(id = "pd_administracion", texto = "Administración", icono = "gears",
         formato = "entero", valor = 18L, tab = "PD_Administracion")
  ))
)

# Funciones ----
.resumen_ejecutivo_estilos <- function() {
  shiny::tags$style(shiny::HTML("\n    .resumen-ejecutivo {position:relative;min-height:260px;}
    .resumen-ejecutivo .re-loader {position:absolute;inset:0;z-index:5;
      display:flex;flex-direction:column;align-items:center;justify-content:center;
      gap:12px;background:rgba(255,255,255,.93);border-radius:6px;}
    .resumen-ejecutivo .re-loader-spinner {width:48px;height:48px;
      border:4px solid #e5e7eb;border-top-color:#DA291C;
      border-radius:50%;animation:re-girar .8s linear infinite;}
    .resumen-ejecutivo .re-loader-texto {font-size:12px;color:#64748b;}
    @keyframes re-girar {to {transform:rotate(360deg);}}
    .resumen-ejecutivo .re-seccion {display:flex;align-items:center;gap:8px;\n      margin:15px 0 8px;padding-bottom:5px;border-bottom:1px solid #e2e8f0;}\n    .resumen-ejecutivo .re-seccion i {color:#DA291C;font-size:12px;}\n    .resumen-ejecutivo .re-seccion span {font-size:12px;font-weight:700;\n      color:#374151;text-transform:uppercase;letter-spacing:.02em;}\n    .resumen-ejecutivo .re-columna {margin-bottom:10px;}\n    .resumen-ejecutivo .re-navegar {cursor:pointer;outline:none;}\n    .resumen-ejecutivo .racafe-caja-valor:not(.rmcv-card) {min-height:92px;padding:10px 12px;\n      gap:7px;border:1px solid #e5e7eb;box-shadow:0 1px 2px rgba(0,0,0,.04);\n      transition:border-color .12s,box-shadow .12s;}\n    .resumen-ejecutivo .re-navegar:hover .racafe-caja-valor:not(.rmcv-card),\n    .resumen-ejecutivo .re-navegar:focus .racafe-caja-valor:not(.rmcv-card) {\n      border-color:#DA291C;box-shadow:0 2px 7px rgba(0,0,0,.08);}\n    .resumen-ejecutivo .racafe-caja-valor:not(.rmcv-card) > div:first-child > div:first-child {\n      min-width:0;align-items:center!important;gap:8px!important;}\n    .resumen-ejecutivo .racafe-caja-valor:not(.rmcv-card) > div:first-child > div:first-child > div:first-child {\n      font-size:1.2em!important;margin-right:0!important;flex-shrink:0;
      color:#DA291C!important;}\n    .resumen-ejecutivo .racafe-caja-valor:not(.rmcv-card) > div:first-child > div:first-child > div:last-child {\n      font-size:11px!important;line-height:1.3;overflow-wrap:anywhere;}\n    .resumen-ejecutivo .racafe-caja-valor:not(.rmcv-card) > div:last-child {font-size:1.3em!important;\n      line-height:1.15;}\n  "))
}

.resumen_ejecutivo_seccion_ui <- function(titulo, icono) {
  shiny::tags$div(class = "re-seccion", shiny::icon(icono), shiny::tags$span(titulo))
}

.resumen_ejecutivo_caja_ui <- function(ns, caja) {
  .crm_caja_ui(ns(caja$id), icono = caja$icono, texto = caja$texto,
                mostrar_boton = TRUE, tooltip = paste("Abrir", caja$texto))
}

# Preloader ----
.resumen_ejecutivo_loader_js <- function(ns) {
  cajas <- unlist(lapply(.RESUMEN_EJECUTIVO_SECCIONES, function(seccion) {
    vapply(seccion$cajas, function(caja) {
      ns(paste0(caja$id, "-valor_display"))
    }, character(1))
  }), use.names = FALSE)
  ids <- jsonlite::toJSON(cajas, auto_unbox = FALSE)
  loader <- jsonlite::toJSON(ns("loader"), auto_unbox = TRUE)
  shiny::tags$script(shiny::HTML(sprintf("
    (function() {
      var pendientes = new Set(%s);
      var loaderId = %s;
      function verificar() {
        if (pendientes.size) return;
        var loader = document.getElementById(loaderId);
        if (loader) {
          loader.style.display = 'none';
          loader.setAttribute('aria-hidden', 'true');
        }
        $(document).off('shiny:value.resumen_loader shiny:error.resumen_loader',
                         registrar);
      }
      function registrar(event) {
        var id = event.target && event.target.id;
        if (!pendientes.has(id)) return;
        pendientes.delete(id);
        if (!pendientes.size) requestAnimationFrame(verificar);
      }
      $(document).on('shiny:value.resumen_loader shiny:error.resumen_loader',
                      registrar);
      if (!pendientes.size) verificar();
    })();
  ", ids, loader)))
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
    .resumen_ejecutivo_estilos(),
    .resumen_ejecutivo_loader_js(ns),
    do.call(shiny::tagList, bloques),
    shiny::tags$div(
      id = ns("loader"), class = "re-loader", role = "status",
      `aria-live` = "polite", `aria-label` = "Cargando tarjetas",
      shiny::tags$div(class = "re-loader-spinner", `aria-hidden` = "true"),
      shiny::tags$span(class = "re-loader-texto", "Cargando indicadores...")
    )
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
              if (length(valor) == 1L && is.numeric(valor) && !is.na(valor)) {
                return(valor)
              }
            }
            actual$valor
          })
          modulo <- racafeModulos::cajaValor_server(actual$id, valor_r = valor_r,
                                        formato = actual$formato)
          shiny::observeEvent(modulo$click_detalle(), {
            bs4Dash::updateTabItems(session = session$rootScope(),
                                    inputId = "menu_principal", selected = actual$tab)
          }, ignoreInit = TRUE)
        })
      }
    }
  })
}
