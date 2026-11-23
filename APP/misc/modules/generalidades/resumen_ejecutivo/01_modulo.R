# Constantes ----
.RESUMEN_EJECUTIVO_SECCIONES <- list(
  list(seccion = "Generalidades", icono = "clipboard-list", cajas = list(
    list(id = "ge_validacion", texto = "Validación financiera", icono = "file-circle-check",
      formato = "porcentaje", valor = NA_real_, footer = "Sacos que cuadran con el cierre financiero.",
      tab = "GE_Validacion")
  )),
  list(seccion = "Gestión comercial", icono = "briefcase", cajas = list(
    list(id = "gc_presupuesto_sacos", texto = "Cumplimiento de sacos", icono = "weight-hanging",
      formato = "porcentaje", valor = NA_real_, footer = "Sacos ejecutados frente al presupuesto anual.",
      tab = "GC_Presupuesto"),
    list(id = "gc_presupuesto_margen", texto = "Cumplimiento de margen", icono = "money-bill-trend-up",
      formato = "porcentaje", valor = NA_real_, footer = "Margen ejecutado frente al presupuesto anual.",
      tab = "GC_Presupuesto"),
    list(id = "gc_oportunidades", texto = "Oportunidades activas", icono = "bullseye",
      formato = "entero", valor = NA_real_, footer = "Oportunidades sin descartar y todavía sin cumplir.",
      tab = "GC_Oportunidades"),
    list(id = "gc_tareas", texto = "Tareas pendientes", icono = "tasks",
      formato = "entero", valor = NA_real_, footer = "Tareas pendientes que puedes consultar según tu usuario.",
      tab = "GC_Tareas"),
    list(id = "gc_cartera", texto = "Cartera vigente", icono = "file-invoice-dollar",
      formato = "dinero", valor = NA_real_, footer = "Saldo abierto sin vencer en el último corte disponible.",
      tab = "GC_Cartera")
  )),
  list(seccion = "Prospección", icono = "filter", cajas = list(
    list(id = "pr_contactos", texto = "Contactos", icono = "address-book",
      formato = "entero", valor = NA_real_, footer = "Entidades en etapa CONTACTO dentro de tu alcance.",
      tab = "PR_Contactos"),
    list(id = "pr_leads", texto = "Leads", icono = "bullseye",
      formato = "entero", valor = NA_real_, footer = "Entidades en etapa LEAD dentro de tu alcance.",
      tab = "PR_Leads"),
    list(id = "pr_clientes", texto = "Clientes", icono = "users",
      formato = "entero", valor = NA_real_, footer = "Cuentas principales en etapa CLIENTE dentro de tu alcance.",
      tab = "PR_Clientes"),
    list(id = "pr_descartados", texto = "Descartados", icono = "ban",
      formato = "entero", valor = NA_real_, footer = "Entidades descartadas dentro de tu alcance.",
      tab = "PR_Descartados")
  )),
  list(seccion = "Cohortes de clientes", icono = "chart-gantt", cajas = list(
    list(id = "co_estados", texto = "Unidades de la cohorte al mes de corte", icono = "people-arrows",
      formato = "entero", valor = NA_real_, footer = "Cliente y línea de negocio: cada combinación cuenta una vez.",
      tab = "CO_Estados")
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
            NA_real_
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




# Población acumulada de la cohorte hasta el mes del corte, sin repetir cliente y línea.
.resumen_unidades_cohorte <- function(panel, hechos, corte) {
  if (!is.data.frame(panel) || !nrow(panel)) return(NA_real_)
  x <- panel[!is.na(panel$FecProceso) & as.Date(panel$FecProceso) <= corte, , drop = FALSE]
  if ("FecIngreso" %in% names(x)) x <- x[!is.na(x$FecIngreso) & x$FecIngreso <= corte, , drop = FALSE]
  claves <- c("CliNitPpal", "LinNegCod")
  x <- dplyr::semi_join(x, unique(hechos[claves]), by = claves)
  nrow(unique(x[claves]))
}
