# Definiciones y Funciones ----

# Formatos globales del módulo
.coha_fmt_sacos  <- .coha_formato("coma")
.coha_fmt_margen <- .coha_formato("dinero")
.coha_fmt_pct    <- .coha_formato("porcentaje")


# Semáforo de cumplimiento: >= 100% verde, >= 85% naranja, rojo
.coha_style_cumpl <- function(v) {
  if (is.null(v) || is.na(v) || is.infinite(v)) return(NULL)
  list(
    color      = if (v >= 1) "#1E8449" else if (v >= 0.85) "#D4780A" else "#C0392B",
    fontWeight = "600",
    whiteSpace = "nowrap"
  )
}

# Celda de cumplimiento: porcentaje con 2 decimales o guión si ausente
.coha_cell_cumpl <- function(v) {
  if (is.null(v) || is.na(v) || is.infinite(v)) return("\u2014")
  .coha_fmt_pct(v)
}

# Etiqueta de presupuesto para tablas Vancouver ----
.coha_badge_presupuestado <- function(v) {
  if (is.null(v) || is.na(v)) return("—")
  switch(v, "PRESUPUESTADO" = "Con Ppto", "NO PRESUPUESTADO" = "Sin Ppto", v)
}

# Semáforo de antigüedad para la última fecha de factura
.coha_style_fec_fact <- function(v) {
  if (is.null(v) || is.na(v)) return(list(color = "#9CA3AF", whiteSpace = "nowrap"))
  dias <- as.integer(Sys.Date() - as.Date(v))
  list(
    whiteSpace = "nowrap",
    fontWeight = "600",
    color = if (dias <= 30) "#065F46" else if (dias <= 60) "#92400E" else "#991B1B"
  )
}

# Transformación pura: agrega métricas Mes Vigente + Año Corrido por cliente.
# FIX v2: PPtoSacos/PPtoMargen son invariantes por mes (un único valor por UC-año).
# Se extraen con first() sobre registros no-NA antes del summarise de ejecución,
# evitando que el ifelse devuelva 0 cuando la fila del mes_v no tiene ppto
# (caso del mes inicial donde t2 no fue propagado a ese periodo).
# FIX v2: Pt_SacosYTD y Pt_MargenYTD se calculan como ppto_mensual × n_meses_ytd,
# donde n_meses_ytd es el número de meses presentes en dat para esa UC,
# asegurando coherencia cuando dat llega filtrado a un subconjunto del año.
.coha_preparar_metricas_cliente <- function(dat, mes_v) {
  
  # Paso 1: presupuesto mensual único por UC — tomar el primer valor no-NA y > 0
  ppto_uc <- dat %>%
    dplyr::group_by(LinNegCod, CliNitPpal) %>%
    dplyr::summarise(
      Pt_SacosVig = {
        vals <- PPtoSacos[!is.na(PPtoSacos) & PPtoSacos > 0]
        if (length(vals) > 0) vals[[1L]] else dplyr::first(PPtoSacos, default = 0)
      },
      Pt_MargenVig = {
        vals <- PPtoMargen[!is.na(PPtoMargen) & PPtoMargen > 0]
        if (length(vals) > 0) vals[[1L]] else dplyr::first(PPtoMargen, default = 0)
      },
      .groups = "drop"
    )
  
  # Paso 2: ejecución mes vigente, YTD y número de meses del panel para esta UC
  ejec <- dat %>%
    dplyr::group_by(LinNegCod, CliNitPpal, PerRazSoc, Asesor, Segmento,
                    Presupuestado, UltFecFact) %>%
    dplyr::summarise(
      Ej_SacosVig  = sum(dplyr::if_else(FecProceso == mes_v, Sacos,  0), na.rm = TRUE),
      Ej_MargenVig = sum(dplyr::if_else(FecProceso == mes_v, Margen, 0)),
      Ej_SacosYTD  = sum(Sacos,  na.rm = TRUE),
      Ej_MargenYTD = sum(Margen),
      # Número de meses del panel para esta UC — base del ppto acumulado
      n_meses_ytd  = dplyr::n_distinct(FecProceso),
      .groups = "drop"
    )
  
  # Paso 3: join ppto + métricas derivadas
  ejec %>%
    dplyr::left_join(ppto_uc, by = c("LinNegCod", "CliNitPpal")) %>%
    dplyr::mutate(
      # Presupuesto YTD = ppto mensual × meses acumulados en el panel
      Pt_SacosYTD    = Pt_SacosVig  * n_meses_ytd,
      Pt_MargenYTD   = Pt_MargenVig * n_meses_ytd,
      Cump_SacosVig  = dplyr::if_else(Pt_SacosVig  > 0, Ej_SacosVig  / Pt_SacosVig,  NA_real_),
      Cump_MargenVig = dplyr::if_else(Pt_MargenVig > 0, Ej_MargenVig / Pt_MargenVig,  NA_real_),
      Cump_SacosYTD  = dplyr::if_else(Pt_SacosYTD  > 0, Ej_SacosYTD  / Pt_SacosYTD,  NA_real_),
      Cump_MargenYTD = dplyr::if_else(Pt_MargenYTD > 0, Ej_MargenYTD / Pt_MargenYTD,  NA_real_)
    ) %>%
    dplyr::select(
      LinNegCod, CliNitPpal, PerRazSoc, Segmento, Asesor, Presupuestado, UltFecFact,
      Ej_SacosVig,  Pt_SacosVig,  Cump_SacosVig,
      Ej_MargenVig, Pt_MargenVig, Cump_MargenVig,
      Ej_SacosYTD,  Pt_SacosYTD,  Cump_SacosYTD,
      Ej_MargenYTD, Pt_MargenYTD, Cump_MargenYTD
    )
}

# Definición centralizada de columnas reactable para el detalle de cliente.
# v2: badges Presupuestado, semáforo UltFecFact, whiteSpace controlado,
# minWidth ajustados para que ningún texto quede cortado, fontWeight en YTD.
# Parámetro incluir_nombres_id: TRUE muestra LinNeg + NIT; FALSE los oculta.
.coha_coldefs_cliente <- function(incluir_nombres_id = TRUE) {
  
  comunes <- list(
    
    PerRazSoc = list(
      name     = "Cliente",
      minWidth = 240,
      sticky   = "left",
      # Permite wrap en nombres largos para que no queden cortados
      style    = list(whiteSpace = "normal", lineHeight = "1.35", fontWeight = "500")
    ),
    
    Segmento = list(
      name     = "Segmento",
      minWidth = 120,
      style    = list(whiteSpace = "nowrap")
    ),
    
    Asesor = list(
      name     = "Asesor",
      minWidth = 140,
      style    = list(whiteSpace = "nowrap")
    ),
    
    Presupuestado = list(
      name     = "Ppto",
      minWidth = 110,
      cell     = function(v) .coha_badge_presupuestado(v)
    ),
    
    UltFecFact = list(
      name     = "\u00daltima Factura",
      minWidth = 130,
      style    = function(v) .coha_style_fec_fact(v),
      cell     = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d/%m/%Y")
    ),
    
    # --- Mes vigente — sacos ---
    Ej_SacosVig = list(
      name     = "Sacos Mes",
      minWidth = 110,
      style    = list(whiteSpace = "nowrap"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_sacos(v)
    ),
    Pt_SacosVig = list(
      name     = "Ppto Sacos Mes",
      minWidth = 130,
      style    = list(whiteSpace = "nowrap", color = "#6B7280"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_sacos(v)
    ),
    Cump_SacosVig = list(
      name     = "% Cumpl Sacos Mes",
      minWidth = 140,
      cell     = function(v) .coha_cell_cumpl(v),
      style    = function(v) .coha_style_cumpl(v)
    ),
    
    # --- Mes vigente — margen ---
    Ej_MargenVig = list(
      name     = "Margen Mes",
      minWidth = 140,
      style    = list(whiteSpace = "nowrap"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_margen(v)
    ),
    Pt_MargenVig = list(
      name     = "Ppto Margen Mes",
      minWidth = 155,
      style    = list(whiteSpace = "nowrap", color = "#6B7280"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_margen(v)
    ),
    Cump_MargenVig = list(
      name     = "% Cumpl Margen Mes",
      minWidth = 155,
      cell     = function(v) .coha_cell_cumpl(v),
      style    = function(v) .coha_style_cumpl(v)
    ),
    
    # --- Año corrido — sacos ---
    Ej_SacosYTD = list(
      name     = "Sacos YTD",
      minWidth = 110,
      style    = list(whiteSpace = "nowrap", fontWeight = "600"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_sacos(v)
    ),
    Pt_SacosYTD = list(
      name     = "Ppto Sacos YTD",
      minWidth = 135,
      style    = list(whiteSpace = "nowrap", color = "#6B7280"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_sacos(v)
    ),
    Cump_SacosYTD = list(
      name     = "% Cumpl Sacos YTD",
      minWidth = 145,
      cell     = function(v) .coha_cell_cumpl(v),
      style    = function(v) .coha_style_cumpl(v)
    ),
    
    # --- Año corrido — margen ---
    Ej_MargenYTD = list(
      name     = "Margen YTD",
      minWidth = 140,
      style    = list(whiteSpace = "nowrap", fontWeight = "600"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_margen(v)
    ),
    Pt_MargenYTD = list(
      name     = "Ppto Margen YTD",
      minWidth = 155,
      style    = list(whiteSpace = "nowrap", color = "#6B7280"),
      cell     = function(v) if (is.na(v) || v == 0) "\u2014" else .coha_fmt_margen(v)
    ),
    Cump_MargenYTD = list(
      name     = "% Cumpl Margen YTD",
      minWidth = 160,
      cell     = function(v) .coha_cell_cumpl(v),
      style    = function(v) .coha_style_cumpl(v)
    )
  )
  
  cols_id <- if (incluir_nombres_id) {
    list(
      LinNegCod  = list(
        name     = "Lin. Neg.",
        minWidth = 110,
        sticky   = "left",
        cell     = function(v) as.character(v),
        style    = list(whiteSpace = "nowrap")
      ),
      CliNitPpal = list(
        name       = "NIT Ppal",
        minWidth   = 110,
        sticky     = "left",
        style      = list(whiteSpace = "nowrap", fontFamily = "monospace")
      )
    )
  } else {
    # En contextos de cohortes el agrupamiento ya está implícito; se ocultan
    list(
      LinNegCod  = list(show = FALSE),
      CliNitPpal = list(show = FALSE)
    )
  }
  
  c(cols_id, comunes)
}

.na0 <- function(x) if (is.null(x) || is.na(x)) 0 else x


# Modulo Tabla Detalle ----

.coha_detalle_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("sin_datos")),
    .crm_tabla_ui(ns("tbl")),
    shiny::uiOutput(ns("ui_descarga"))
  )
}

.coha_detalle <- function(id, data, mes_vig, titulo, subtitulo = NULL,
                                 footer = NULL, footer_tipo = "info") {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Columnas reactable — fuente única de verdad, con IDs visibles
    .coldefs <- .coha_coldefs_cliente(incluir_nombres_id = TRUE)
    
    # Reactive de datos transformados — recalcula solo si data o mes_vig cambian.
    # FIX: .coha_preparar_metricas_cliente v2 resuelve ppto en mes inicial y YTD coherente.
    data_prep <- shiny::reactive({
      shiny::req(data(), mes_vig())
      dat <- data()
      if (nrow(dat) == 0) return(NULL)
      .coha_preparar_metricas_cliente(dat, mes_vig())
    })
    
    # Mensaje de estado vacío — solo visible cuando no hay datos
    output$sin_datos <- .crm_render_ui({
      if (!is.null(data_prep())) return(NULL)
      shiny::tags$div(
        style = paste0(
          "display:flex; align-items:center; gap:8px;",
          "padding:16px 12px; color:#6B7280;",
          "font-size:0.85rem; font-style:italic;"
        ),
        shiny::icon("circle-info"),
        "No hay información disponible."
      )
    })
    
    .coha_tabla("tbl", data = shiny::reactive({
      datos <- data_prep()
      if (is.null(datos)) data.frame() else datos
    }), titulo = titulo, subtitulo = subtitulo, footer = footer, footer_tipo = footer_tipo,
      col_specs = .coldefs, id_col = "CliNitPpal", modo_seleccion = "ninguno", page_size = 20L)

    # UI del botón de descarga — solo visible cuando hay datos
    output$ui_descarga <- .crm_render_ui({
      shiny::req(!is.null(data_prep()))
      .crm_descarga_ui("btn_descarga", session$ns, label = "Descargar")
    })
    
    # Handler de descarga — exporta data_prep como libro Excel (.xlsx)
    output$btn_descarga <- shiny::downloadHandler(
      filename = function() {
        tit       <- if (shiny::is.reactive(titulo)) titulo() else titulo
        tit_limpio <- gsub("[^A-Za-z0-9_\\-]", "_", tit)
        paste0(tit_limpio, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(.crm_nits_tabla(data_prep()), file)
      }
    )
    
  })
}


# Modulo Cohortes ----

CohortesEstadosUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Filtros globales del módulo ----
    racafeForms::racafeForm(variant = "plain",
      racafeForms::racafeFormRow(columns = 2,
        shiny::uiOutput(ns("ui_fechas")),
        racafeForms::racafeInputSelect("fil_ppto", "Presupuesto",
          choices = c("Con Ppto" = "con", "Sin Ppto" = "sin", "Sin dato" = "nd"),
          selected = c("con", "sin", "nd"), multiple = TRUE, ns = ns))),
    shiny::uiOutput(ns("sin_datos")),

    # Bloque KPIs de periodo: inicio vs mes vigente ----
    racafeForms::racafePanel(
      title       = "Resumen de Periodo",
      # Fila — Mes Inicial
      uiOutput(ns("lbl_mes_inicial")),
      fluidRow(
        column(3, .crm_caja_output(ns("kpi_ini_total"))),
        column(3, .crm_caja_output(ns("kpi_ini_activo"))),
        column(3, .crm_caja_output(ns("kpi_ini_inactivo"))),
        column(3, .crm_caja_output(ns("kpi_ini_nuevo")))
      ),
      tags$hr(style = "margin: 8px 0;"),
      # Fila — Mes Vigente
      uiOutput(ns("lbl_mes_vigente")),
      fluidRow(
        column(3, .crm_caja_output(ns("kpi_vig_total"))),
        column(3, .crm_caja_output(ns("kpi_vig_activo"))),
        column(3, .crm_caja_output(ns("kpi_vig_inactivo"))),
        column(3, .crm_caja_output(ns("kpi_vig_nuevo")))
      ),
      tags$hr(style = "margin:10px 0;"),
      tags$p(
        tags$strong("Alertas del Mes Vigente"),
        style = "font-weight:700; color:#374151; margin-bottom:4px;"
      ),
      fluidRow(
        column(6, .crm_caja_output(ns("kpi_vig_en_riesgo"))),
        column(6, .crm_caja_output(ns("kpi_vig_por_recuperar")))
      )
    ),
    
    # Bloque Indicadores de Transición ----
    racafeForms::racafePanel(
      title       = "Indicadores de Transici\u00f3n",
      fluidRow(
        # Columna izquierda — matrices de transición
        column(4,
               .crm_tabla_ui(ns("tbl_matriz_trans_abs"), searchable = FALSE),
               racafeForms::racafeInputSelect("sel_tipo_matriz", NULL,
                 choices = c("% fila (inicio)" = "pct_fila", "% columna (vigente)" = "pct_col",
                   "% total general" = "pct_total"), selected = "pct_fila", empty = FALSE, ns = ns),
               .crm_tabla_ui(ns("tbl_matriz_trans"), searchable = FALSE)
        ),
        # Columna derecha — KPIs por grupo
        column(8,
               tags$p(
                 tags$strong("Tasas de Transici\u00f3n"),
                 style = paste0(
                   "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;",
                   "text-transform:uppercase; letter-spacing:0.04em;"
                 )
               ),
               fluidRow(
                 column(4, .crm_caja_output(ns("kpi_retencion"))),
                 column(4, .crm_caja_output(ns("kpi_churn"))),
                 column(4, .crm_caja_output(ns("kpi_reactivacion")))
               ),
               tags$hr(style = "margin:10px 0;"),
               tags$p(
                 tags$strong("Volumen y Movimiento"),
                 style = paste0(
                   "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;",
                   "text-transform:uppercase; letter-spacing:0.04em;"
                 )
               ),
               fluidRow(
                 column(6, .crm_caja_output(ns("kpi_cambio_neto"))),
                 column(6, .crm_caja_output(ns("kpi_estabilidad")))
               )
        )
      )
    ),
    
    # Resumen mensual por estado ----
    racafeForms::racafePanel(
      "Resumen Mensual por Estado",
      # Grupo 3 — duración y dinámica (tasas mes a mes)
      tags$p(
        tags$strong("Duraci\u00f3n y Din\u00e1mica"),
        style = paste0(
          "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;",
          "text-transform:uppercase; letter-spacing:0.04em;"
        )
      ),
      fluidRow(
        column(4, .crm_caja_output(ns("kpi_vida_activo"))),
        column(4, .crm_caja_output(ns("kpi_t_reactiv"))),
        column(4, .crm_caja_output(ns("kpi_balance_neto")))
      ),
      tags$hr(style = "margin:10px 0;"),
      # Grupo 4 — largo plazo y valor en riesgo
      tags$p(
        tags$strong("Largo Plazo y Valor en Riesgo"),
        style = paste0(
          "margin:0 0 4px 0; color:#6B7280; font-size:0.8rem;",
          "text-transform:uppercase; letter-spacing:0.04em;"
        )
      ),
      fluidRow(
        column(3, .crm_caja_output(ns("kpi_pi_activo"))),
        column(3, .crm_caja_output(ns("kpi_pi_inactivo"))),
        column(3, .crm_caja_output(ns("kpi_sacos_riesgo"))),
        column(3, .crm_caja_output(ns("kpi_rev_riesgo")))
      ),
      tags$hr(style = "margin:10px 0;"),
      .crm_tabla_ui(ns("tbl_resumen_mensual"), searchable = FALSE),
      plotly::plotlyOutput(ns("plot_crecimiento"), height = "400px")
    )
  )
}

CohortesEstados <- function(id, bd, data_t) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sel_matriz_estado <- reactiveVal(NULL)
    sel_resumen <- reactiveValues(estado = NULL, mes = NULL)
    
    # Constantes ----
    .niveles_trans   <- c("ACTIVO", "INACTIVO")
    .niveles_resumen <- c("ACTIVO", "INACTIVO", "NUEVO")
    
    
    # Helpers internos ----
    
    # Sustituye NULL o NA por un valor por defecto (a diferencia de %||%, que solo cubre NULL)
    .na0 <- function(x) if (is.null(x) || is.na(x)) 0 else x
    
    # Semáforo de color para KPIs de tasa
    .col_kpi <- function(v, tipo) {
      if (is.null(v) || is.na(v)) return("#999999")
      switch(tipo,
             retencion   = if (v >= 0.80) "#27AE60" else if (v >= 0.60) "#F4A820" else "#E74C3C",
             churn       = if (v <= 0.10) "#27AE60" else if (v <= 0.20) "#F4A820" else "#E74C3C",
             reactivacion = if (v >= 0.30) "#27AE60" else if (v >= 0.15) "#F4A820" else "#E74C3C",
             "#999999"
      )
    }
    
    # Conteo de UCs únicas (LinNegCod + CliNitPpal) en un estado dado
    .n_estado <- function(dat_corte, estado) {
      en_estado <- dat_corte$EstadoPanel == estado
      dplyr::n_distinct(dat_corte$LinNegCod[en_estado], dat_corte$CliNitPpal[en_estado])
    }
    
    # Promedio por UC para una métrica en un mes dado (solo UCs con valor > 0)
    .promedio_uc_mes <- function(dat, mes, var) {
      if (var == "Margen" && anyNA(dat[[var]][dat$FecProceso == mes])) return(NA_real_)
      prom <- dat %>%
        dplyr::filter(FecProceso == mes, .data[[var]] > 0) %>%
        dplyr::group_by(LinNegCod, CliNitPpal) %>%
        dplyr::summarise(valor_uc = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(prom = mean(valor_uc, na.rm = TRUE)) %>%
        dplyr::pull(prom)
      if (length(prom) == 0 || is.na(prom)) 0 else prom
    }
    
    
    # Datos reactivos ----
    
    # Selectores de meses disponibles; conservar selección al refrescar el panel ----
    output$ui_fechas <- .crm_render_ui({
      fechas <- sort(unique(bd()$FecProceso))
      if (!length(fechas)) return(NULL)
      opciones <- stats::setNames(as.character(fechas), .coha_mes(fechas))
      anterior <- shiny::isolate(list(input$fecha_ini, input$fecha_fin))
      inicial <- if (length(anterior[[1]]) == 1L && anterior[[1]] %in% opciones) anterior[[1]] else opciones[[1]]
      final <- if (length(anterior[[2]]) == 1L && anterior[[2]] %in% opciones) anterior[[2]] else tail(opciones, 1)
      racafeForms::racafeFormRow(columns = 2,
        racafeForms::racafeInputSelect("fecha_ini", "Mes inicial", choices = opciones,
          selected = inicial, empty = FALSE, ns = ns),
        racafeForms::racafeInputSelect("fecha_fin", "Mes final", choices = opciones,
          selected = final, empty = FALSE, ns = ns))
    })
    output$sin_datos <- .crm_render_ui({
      if (!nrow(bd())) return(.crm_nota("No hay estados ni ventas disponibles para el año en curso."))
      req(input$fecha_ini, input$fecha_fin)
      if (!nrow(bd_f())) .crm_nota("No hay unidades comerciales para los filtros seleccionados.")
    })
    observeEvent(list(input$fecha_ini, input$fecha_fin), {
      req(input$fecha_ini, input$fecha_fin)
      if (as.Date(input$fecha_ini) > as.Date(input$fecha_fin)) {
        shinyWidgets::updatePickerInput(session, "fecha_fin", selected = input$fecha_ini)
      }
    })

    # Mes vigente / mes inicial del panel — gobernados por el filtro de usuario
    mes_vigente <- reactive({
      req(input$fecha_fin)
      lubridate::floor_date(as.Date(input$fecha_fin), "month")
    })
    mes_inicio <- reactive({
      req(input$fecha_ini)
      lubridate::floor_date(as.Date(input$fecha_ini), "month")
    })
    
    # Universo de UCs activas según filtros dimensionales de data_t
    ucs_activas <- reactive({
      req(data_t())
      data_t() %>%
        select(LinNegCod, CliNitPpal) %>%
        distinct()
    })
    
    # bd filtrado al universo dimensional, rango de meses y filtro global de presupuesto
    bd_f <- reactive({
      req(bd(), ucs_activas(), mes_inicio(), mes_vigente())
      dat <- bd() %>%
        semi_join(ucs_activas(), by = join_by(LinNegCod, CliNitPpal)) %>%
        filter(FecProceso >= mes_inicio(), FecProceso <= mes_vigente())
      estados <- c(con = "PRESUPUESTADO", sin = "NO PRESUPUESTADO", nd = "NO DISPONIBLE")
      dplyr::filter(dat, Presupuestado %in% unname(estados[input$fil_ppto]))
    })
    
    # Corte vigente: snapshot del último mes de bd_f
    bd_vig <- reactive({
      req(bd_f(), mes_vigente())
      bd_f() %>% filter(FecProceso == mes_vigente())
    })
    
    # Corte inicial: snapshot del primer mes de bd_f
    bd_ini <- reactive({
      req(bd_f(), mes_inicio())
      bd_f() %>% filter(FecProceso == mes_inicio())
    })
    
    # Alertas — activos que llegan al límite de inactivación este mes
    alertas_inactivar <- reactive({
      req(bd_vig(), mes_vigente())
      ucs <- bd_vig() %>%
        filter(EstadoPanel == "ACTIVO") %>%
        mutate(
          meses_rec = dplyr::coalesce(as.numeric(NumMesesRecuperar), 4),
          fec_lim   = UltFecFact %m+% lubridate::period(month = meses_rec)
        ) %>%
        filter(!is.na(fec_lim), month(fec_lim) == month(mes_vigente())) %>%
        select(LinNegCod, CliNitPpal)
      bd_f() %>%
        semi_join(ucs, by = join_by(LinNegCod, CliNitPpal))
    })
    
    # Alertas — inactivos que facturaron en el mes vigente (próximos a reactivarse)
    alertas_recuperados <- reactive({
      req(bd_vig())
      ucs <- bd_vig() %>%
        filter(EstadoPanel == "INACTIVO", Sacos != 0) %>%
        select(LinNegCod, CliNitPpal)
      bd_f() %>%
        semi_join(ucs, by = join_by(LinNegCod, CliNitPpal))
    })
    
    # Estado inicial/final por UC (excluye NUEVO para la matriz de transición)
    trans_ucs <- reactive({
      req(bd_f(), mes_inicio(), mes_vigente())
      bd_f() %>%
        group_by(LinNegCod, CliNitPpal) %>%
        filter(
          FecProceso %in% c(mes_inicio(), mes_vigente()),
          EstadoPanel != "NUEVO"
        ) %>%
        arrange(FecProceso) %>%
        summarise(
          EstadoInicial = dplyr::first(EstadoPanel),
          EstadoFinal   = dplyr::last(EstadoPanel),
          .groups = "drop"
        ) %>%
        mutate(
          EstadoInicial = factor(EstadoInicial, levels = .niveles_trans),
          EstadoFinal   = factor(EstadoFinal,   levels = .niveles_trans)
        )
    })
    
    # Matriz de transición absoluta: cruce estado inicial vs final por UC
    matriz_trans <- reactive({
      req(trans_ucs())
      trans_ucs() %>%
        count(EstadoInicial, EstadoFinal, name = "Clientes", .drop = FALSE) %>%
        tidyr::pivot_wider(
          names_from  = EstadoFinal,
          values_from = Clientes,
          values_fill = 0
        ) %>%
        mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
        bind_rows(
          (.) %>% summarise(EstadoInicial = "TOTAL", across(where(is.numeric), sum))
        )
    })
    
    # ID de fila seleccionada en la matriz absoluta (para modal de detalle)
    
    # Indicadores derivados de la matriz inicio-fin
    indicadores_trans <- reactive({
      req(trans_ucs())
      tt <- trans_ucs()
      aa <- sum(tt$EstadoInicial == "ACTIVO"   & tt$EstadoFinal == "ACTIVO",   na.rm = TRUE)
      ai <- sum(tt$EstadoInicial == "ACTIVO"   & tt$EstadoFinal == "INACTIVO", na.rm = TRUE)
      ia <- sum(tt$EstadoInicial == "INACTIVO" & tt$EstadoFinal == "ACTIVO",   na.rm = TRUE)
      ii <- sum(tt$EstadoInicial == "INACTIVO" & tt$EstadoFinal == "INACTIVO", na.rm = TRUE)
      list(
        aa           = aa,
        ai           = ai,
        ia           = ia,
        ii           = ii,
        retencion    = dplyr::if_else((aa + ai) > 0, aa / (aa + ai), NA_real_),
        churn        = dplyr::if_else((aa + ai) > 0, ai / (aa + ai), NA_real_),
        reactivacion = dplyr::if_else((ia + ii) > 0, ia / (ia + ii), NA_real_),
        activos_ini  = .n_estado(bd_ini(), "ACTIVO"),
        activos_fin  = .n_estado(bd_vig(), "ACTIVO"),
        cambio_neto  = .n_estado(bd_vig(), "ACTIVO") - .n_estado(bd_ini(), "ACTIVO")
      )
    })
    
    # Indicadores mes a mes sobre todo el panel — base de Duración/Dinámica y Largo Plazo.
    # Construye pares de meses consecutivos y acumula conteos de transición.
    indicadores_mensual <- reactive({
      req(bd_f())
      
      # Subconjunto invariante: solo ACTIVO/INACTIVO, columnas mínimas
      panel_ai <- bd_f() %>%
        dplyr::filter(EstadoPanel %in% c("ACTIVO", "INACTIVO")) %>%
        dplyr::distinct(LinNegCod, CliNitPpal, FecProceso, EstadoPanel)
      
      fechas <- sort(unique(panel_ai$FecProceso))
      
      if (length(fechas) < 2) {
        return(list(aa = 0L, ai = 0L, ia = 0L, ii = 0L,
                    retencion = NA_real_, churn = NA_real_,
                    reactivacion = NA_real_, n_transiciones = 0L))
      }
      
      # Pares de meses consecutivos t → t+1
      pares <- purrr::map_dfr(seq_len(length(fechas) - 1), function(i) {
        t0 <- fechas[i]
        t1 <- fechas[i + 1]
        snap_t0 <- panel_ai %>%
          dplyr::filter(FecProceso == t0) %>%
          dplyr::select(LinNegCod, CliNitPpal, Estado_t0 = EstadoPanel)
        snap_t1 <- panel_ai %>%
          dplyr::filter(FecProceso == t1) %>%
          dplyr::select(LinNegCod, CliNitPpal, Estado_t1 = EstadoPanel)
        dplyr::inner_join(snap_t0, snap_t1, by = c("LinNegCod", "CliNitPpal"))
      })
      
      if (nrow(pares) == 0) {
        return(list(aa = 0L, ai = 0L, ia = 0L, ii = 0L,
                    retencion = NA_real_, churn = NA_real_,
                    reactivacion = NA_real_, n_transiciones = 0L))
      }
      
      # Conteos acumulados de cada tipo de transición
      conteos <- pares %>%
        dplyr::summarise(
          aa = sum(Estado_t0 == "ACTIVO"   & Estado_t1 == "ACTIVO",   na.rm = TRUE),
          ai = sum(Estado_t0 == "ACTIVO"   & Estado_t1 == "INACTIVO", na.rm = TRUE),
          ia = sum(Estado_t0 == "INACTIVO" & Estado_t1 == "ACTIVO",   na.rm = TRUE),
          ii = sum(Estado_t0 == "INACTIVO" & Estado_t1 == "INACTIVO", na.rm = TRUE)
        )
      
      aa <- as.integer(conteos$aa)
      ai <- as.integer(conteos$ai)
      ia <- as.integer(conteos$ia)
      ii <- as.integer(conteos$ii)
      
      list(
        aa             = aa,
        ai             = ai,
        ia             = ia,
        ii             = ii,
        retencion      = dplyr::if_else((aa + ai) > 0, aa / (aa + ai), NA_real_),
        churn          = dplyr::if_else((aa + ai) > 0, ai / (aa + ai), NA_real_),
        reactivacion   = dplyr::if_else((ia + ii) > 0, ia / (ia + ii), NA_real_),
        n_transiciones = nrow(pares)
      )
    })
    
    # Indicadores de duración derivados de las tasas mensuales
    indicadores_duracion <- reactive({
      req(indicadores_mensual())
      ind <- indicadores_mensual()
      list(
        t_vida_activo = if (!is.na(ind$churn)        && ind$churn        > 0)
          round(1 / ind$churn, 1)        else NA_real_,
        t_reactiv     = if (!is.na(ind$reactivacion) && ind$reactivacion > 0)
          round(1 / ind$reactivacion, 1) else NA_real_,
        v_deterioro   = if (!is.na(ind$retencion) && ind$retencion > 0 && ind$retencion < 1)
          round(log(0.5) / log(ind$retencion), 1) else NA_real_
      )
    })
    
    # Estado estacionario de la cadena de Markov (Markov equilibrio)
    estado_estacionario <- reactive({
      req(indicadores_mensual())
      ind   <- indicadores_mensual()
      denom <- .na0(ind$churn) + .na0(ind$reactivacion)
      pi_a  <- if (!is.na(denom) && denom > 0) .na0(ind$reactivacion) / denom else NA_real_
      pi_i  <- if (!is.na(denom) && denom > 0) .na0(ind$churn)        / denom else NA_real_
      
      n_activos_act   <- .n_estado(bd_vig(), "ACTIVO")
      n_inactivos_act <- .n_estado(bd_vig(), "INACTIVO")
      total_actual    <- n_activos_act + n_inactivos_act
      pct_actual_a    <- if (total_actual > 0) n_activos_act / total_actual else NA_real_
      distancia       <- if (!is.na(pi_a) && !is.na(pct_actual_a))
        abs(pct_actual_a - pi_a) else NA_real_
      
      list(pi_a = pi_a, pi_i = pi_i, distancia = distancia)
    })
    
    # Indicadores de valor en riesgo basados en tasas mensuales
    indicadores_valor <- reactive({
      req(bd_f(), indicadores_mensual())
      ind <- indicadores_mensual()
      n_activos_act <- .n_estado(bd_vig(), "ACTIVO")
      n_inactivos   <- .n_estado(bd_vig(), "INACTIVO")
      ticket_prom   <- .promedio_uc_mes(bd_f(), mes_vigente(), "Margen")
      sacos_prom    <- .promedio_uc_mes(bd_f(), mes_vigente(), "Sacos")
      list(
        ticket_prom  = ticket_prom,
        sacos_prom   = sacos_prom,
        rev_riesgo   = if (!is.na(ind$churn)) n_activos_act * ind$churn * ticket_prom else NA_real_,
        sacos_riesgo = if (!is.na(ind$churn)) n_activos_act * ind$churn * sacos_prom  else NA_real_,
        balance_neto = .na0(ind$reactivacion) * n_inactivos - .na0(ind$churn) * n_activos_act,
        n_inactivos  = n_inactivos
      )
    })
    
    # Resumen mensual por estado para la tabla de tendencia
    resumen_mensual <- reactive({
      req(bd_f())
      bd_f() %>%
        mutate(EstadoPanel = factor(EstadoPanel, levels = .niveles_resumen)) %>%
        group_by(FecProceso, EstadoPanel, .drop = FALSE) %>%
        summarise(
          Clientes = dplyr::n_distinct(LinNegCod, CliNitPpal),
          .groups  = "drop"
        ) %>%
        mutate(Mes = .coha_mes(FecProceso)) %>%
        select(-FecProceso) %>%
        tidyr::pivot_wider(
          names_from  = Mes,
          values_from = Clientes,
          values_fill = 0L
        ) %>%
        rename(Estado = EstadoPanel) %>%
        mutate(Estado = as.character(Estado)) %>%
        bind_rows(summarise(., Estado = "TOTAL", across(where(is.numeric), sum)))
    })
    
    # Serie temporal para el gráfico de crecimiento
    serie_crecimiento <- reactive({
      req(bd_f())
      bd_f() %>%
        dplyr::filter(EstadoPanel %in% c("ACTIVO", "INACTIVO", "NUEVO")) %>%
        dplyr::distinct(FecProceso, LinNegCod, CliNitPpal, EstadoPanel) %>%
        dplyr::group_by(FecProceso) %>%
        dplyr::summarise(
          ACTIVO   = sum(EstadoPanel == "ACTIVO"),
          INACTIVO = sum(EstadoPanel == "INACTIVO"),
          NUEVO    = sum(EstadoPanel == "NUEVO"),
          Total    = dplyr::n(),
          .groups  = "drop"
        ) %>%
        dplyr::arrange(FecProceso) %>%
        dplyr::mutate(Mes = .coha_mes(FecProceso))
    })
    
    # Conteos de UC para alertas (reactivos independientes para evitar recálculo)
    n_en_riesgo  <- reactive(
      dplyr::n_distinct(alertas_inactivar()$LinNegCod,  alertas_inactivar()$CliNitPpal)
    )
    n_recuperando <- reactive(
      dplyr::n_distinct(alertas_recuperados()$LinNegCod, alertas_recuperados()$CliNitPpal)
    )
    
    # Mapa mes-etiqueta → fecha real (para modal del resumen mensual)
    mapa_mes_fecha <- reactive({
      req(bd_f())
      fechas <- sort(unique(bd_f()$FecProceso))
      stats::setNames(fechas, .coha_mes(fechas))
    })
    
    # Estado reactivo de la celda seleccionada en la tabla de resumen mensual
    
    
    # Outputs ----
    
    ## Resumen de Periodo ----
    
    ### Helper .kpi_periodo — integra CajaModal + .coha_detalle ----
    # Encapsula la lógica repetida de los 8 KPIs de periodo (ini/vig × 4 tipos).
    # FIX YTD: data recibe el acumulado hasta el mes del corte, no solo el snapshot.
    # Así Ej_SacosYTD refleja todos los meses hasta ese corte, no solo uno.
    .kpi_periodo <- function(corte, tipo) {
      
      # Acumulado YTD hasta el mes del corte, filtrado por estado si aplica
      datos_estado <- reactive({
        dat <- bd_f() %>% dplyr::filter(FecProceso <= corte$mes())
        if (!is.null(tipo$estado)) {
          ucs_en_corte <- corte$data() %>%
            dplyr::filter(EstadoPanel == tipo$estado) %>%
            dplyr::select(LinNegCod, CliNitPpal)
          dat <- dplyr::semi_join(dat, ucs_en_corte, by = c("LinNegCod", "CliNitPpal"))
        }
        dat
      })
      n_uc <- reactive(dplyr::n_distinct(
        corte$data() %>%
          { if (!is.null(tipo$estado)) dplyr::filter(., EstadoPanel == tipo$estado) else . } %>%
          dplyr::pull(LinNegCod),
        corte$data() %>%
          { if (!is.null(tipo$estado)) dplyr::filter(., EstadoPanel == tipo$estado) else . } %>%
          dplyr::pull(CliNitPpal)
      ))
      
      id_kpi <- paste0("kpi_", corte$id, "_", tipo$id)
      id_tbl <- paste0("tbl_mod_", corte$id, "_", tipo$id)
      
      titulo_modal <- reactive(paste0(
        tipo$etiqueta_titulo, " \u2014 ", corte$etiqueta_modal,
        " \u2014 ", .coha_mes(corte$mes())
      ))
      titulo_tabla <- reactive(paste0(
        tipo$etiqueta_titulo, " \u2014 ", corte$etiqueta_tabla,
        " \u2014 ", .coha_mes(corte$mes())
      ))
      footer_tabla <- reactive(paste0(
        n_uc(), " ", tipo$footer_sufijo, " en el ", corte$etiqueta_footer
      ))
      
      .coha_caja(
        id_kpi,
        valor          = n_uc,
        formato        = "coma",
        texto          = tipo$texto,
        icono          = tipo$icono,
        colores        = c(fondo = "white"),
        mostrar_boton  = TRUE,
        titulo_modal   = titulo_modal,
        icono_modal    = NULL,
        tamano_modal   = "xl",
        contenido_modal = function() .coha_detalle_ui(ns(id_tbl)),
        footer         = if (tipo$id == "total")
          reactive(paste("Corte", .coha_mes(corte$mes()))) else NULL,
        footer_class   = "caja-modal-footer"
      )
      
      .coha_detalle(
        id          = id_tbl,
        data        = datos_estado,
        mes_vig     = corte$mes,
        titulo      = titulo_tabla,
        subtitulo   = if (tipo$id == "total") {
          paste0("Todos los estados en el ", corte$etiqueta_footer)
        } else NULL,
        footer      = footer_tabla,
        footer_tipo = tipo$footer_tipo
      )
    }
    
    # Configuración de cortes (inicio / vigente)
    .cortes_periodo <- list(
      ini = list(
        id              = "ini",
        data            = bd_ini,
        mes             = mes_inicio,
        etiqueta_modal  = "Inicio",
        etiqueta_tabla  = "Inicio de Periodo",
        etiqueta_footer = "corte inicial"
      ),
      vig = list(
        id              = "vig",
        data            = bd_vig,
        mes             = mes_vigente,
        etiqueta_modal  = "Mes Vigente",
        etiqueta_tabla  = "Mes Vigente",
        etiqueta_footer = "mes vigente"
      )
    )
    
    # Configuración de tipos de KPI (total / activo / inactivo / nuevo)
    .tipos_kpi_periodo <- list(
      total = list(
        id              = "total",
        estado          = NULL,
        texto           = "Total UCs",
        icono           = "users",
        etiqueta_titulo = "Detalle",
        footer_sufijo   = "unidades comerciales",
        footer_tipo     = "info"
      ),
      activo = list(
        id              = "activo",
        estado          = "ACTIVO",
        texto           = "Activos",
        icono           = "check-circle",
        etiqueta_titulo = "Activos",
        footer_sufijo   = "UC(s) activas",
        footer_tipo     = "info"
      ),
      inactivo = list(
        id              = "inactivo",
        estado          = "INACTIVO",
        texto           = "A Recuperar",
        icono           = "exclamation",
        etiqueta_titulo = "A Recuperar",
        footer_sufijo   = "UC(s) inactivas",
        footer_tipo     = "warning"
      ),
      nuevo = list(
        id              = "nuevo",
        estado          = "NUEVO",
        texto           = "Nuevos",
        icono           = "star",
        etiqueta_titulo = "Nuevos",
        footer_sufijo   = "UC(s) nuevas",
        footer_tipo     = "info"
      )
    )
    
    ### Labels de mes ----
    output$lbl_mes_inicial <- .crm_render_ui({
      tags$h6(
        paste0("Mes Inicial \u2014 ", .coha_mes(mes_inicio())),
        style = "font-weight:700; color:#374151;"
      )
    })
    output$lbl_mes_vigente <- .crm_render_ui({
      tags$h6(
        paste0("Mes Vigente \u2014 ", .coha_mes(mes_vigente())),
        style = "font-weight:700; color:#374151;"
      )
    })
    
    ### Registrar los 8 KPIs de periodo (2 cortes × 4 tipos) ----
    for (corte in .cortes_periodo) {
      for (tipo in .tipos_kpi_periodo) .kpi_periodo(corte, tipo)
    }
    
    ### KPI — activos en riesgo de inactivarse este mes ----
    .coha_caja(
      "kpi_vig_en_riesgo",
      valor           = n_en_riesgo,
      formato         = "coma",
      texto           = "En Riesgo",
      icono           = "exclamation-triangle",
      colores         = c(fondo = "white"),
      mostrar_boton   = TRUE,
      titulo_modal    = reactive(paste0(
        "En Riesgo de Inactivarse \u2014 ", .coha_mes(mes_vigente())
      )),
      icono_modal     = NULL,
      tamano_modal    = "xl",
      contenido_modal = function() .coha_detalle_ui(ns("tbl_mod_vig_en_riesgo")),
      footer          = reactive(paste0(
        n_en_riesgo(), " activos llegan al l\u00edmite de inactivaci\u00f3n este mes"
      )),
      footer_class    = "caja-modal-footer"
    )
    
    .coha_detalle(
      id        = "tbl_mod_vig_en_riesgo",
      data      = alertas_inactivar,
      mes_vig   = mes_vigente,
      titulo    = reactive(paste0(
        "En Riesgo de Inactivarse \u2014 ", .coha_mes(mes_vigente())
      )),
      subtitulo = "Activos cuya \u00faltima factura alcanza el umbral de inactivaci\u00f3n este mes",
      footer    = reactive(paste0(
        n_en_riesgo(), " UC(s) en riesgo de pasar a 'A Recuperar' este mes"
      )),
      footer_tipo = "warning"
    )
    
    ### KPI — inactivos que facturaron este mes ----
    .coha_caja(
      "kpi_vig_por_recuperar",
      valor           = n_recuperando,
      formato         = "coma",
      texto           = "Recuper\u00e1ndose",
      icono           = "undo",
      colores         = c(fondo = "white"),
      mostrar_boton   = TRUE,
      titulo_modal    = reactive(paste0(
        "Inactivos que Facturaron \u2014 ", .coha_mes(mes_vigente())
      )),
      icono_modal     = NULL,
      tamano_modal    = "xl",
      contenido_modal = function() .coha_detalle_ui(ns("tbl_mod_vig_por_recuperar")),
      footer          = reactive(paste0(
        n_recuperando(), " inactivos con factura en el mes \u2014 pr\u00f3ximos a reactivarse"
      )),
      footer_class    = "caja-modal-footer"
    )
    
    .coha_detalle(
      id        = "tbl_mod_vig_por_recuperar",
      data      = alertas_recuperados,
      mes_vig   = mes_vigente,
      titulo    = reactive(paste0(
        "Inactivos que Facturaron \u2014 ", .coha_mes(mes_vigente())
      )),
      subtitulo = "Inactivos con compra en el mes vigente \u2014 candidatos a reactivaci\u00f3n",
      footer    = reactive(paste0(
        n_recuperando(), " UC(s) inactivas con facturaci\u00f3n en el mes vigente"
      )),
      footer_tipo = "info"
    )
    
    
    ## Indicadores de Transición ----
    
    ### Modal de detalle de la matriz absoluta ----
    # FIX YTD: data pasa el acumulado completo del panel para las UCs seleccionadas,
    # y mes_vig apunta al mes vigente global (cierre del período de transición).
    .coha_detalle(
      id        = "tbl_mod_matriz_total",
      data      = reactive({
        req(sel_matriz_estado(), trans_ucs())
        ucs_sel <- trans_ucs() %>%
          dplyr::filter(EstadoInicial == sel_matriz_estado()) %>%
          dplyr::select(LinNegCod, CliNitPpal)
        bd_f() %>%
          dplyr::semi_join(ucs_sel, by = dplyr::join_by(LinNegCod, CliNitPpal))
      }),
      mes_vig   = mes_vigente,
      titulo    = reactive(paste0(
        "Detalle \u2014 Estado Inicial: ", sel_matriz_estado() %||% "", " \u2014 ",
        .coha_mes(mes_inicio()), " \u2192 ", .coha_mes(mes_vigente())
      )),
      subtitulo = "Clientes cuyo estado inicial corresponde a la fila seleccionada",
      footer    = reactive({
        req(sel_matriz_estado(), trans_ucs())
        n <- trans_ucs() %>%
          dplyr::filter(EstadoInicial == sel_matriz_estado()) %>%
          nrow()
        paste0(n, " unidades comerciales con estado inicial '", sel_matriz_estado(), "'")
      }),
      footer_tipo = "info"
    )
    
    ### Matriz absoluta — modo fila para habilitar modal ----
    .coha_tabla(
      id   = "tbl_matriz_trans_abs",
      data = reactive({
        req(matriz_trans())
        matriz_trans() %>% dplyr::rename(Estado = EstadoInicial)
      }),
      titulo    = "Transiciones",
      subtitulo = reactive(paste0(
        .coha_mes(mes_inicio()), " \u2192 ", .coha_mes(mes_vigente())
      )),
      footer = reactive({
        ind <- indicadores_trans()
        paste0(
          "Retuvieron: ",       ind$aa, "<br/>",
          "Se perdieron: ",     ind$ai, "<br/>",
          "Se recuperaron: ",   ind$ia, "<br/>",
          "Siguen inactivos: ", ind$ii
        ) %>% HTML
      }),
      id_col     = "Estado",
      col_specs  = list(
        Estado   = list(
          name     = "Estado Inicial",
          minWidth = 130,
          sticky   = "left",
          style    = list(whiteSpace = "nowrap", fontWeight = "600")
        ),
        ACTIVO   = list(name = "Activo",   minWidth = 85,
                                     style = list(whiteSpace = "nowrap")),
        INACTIVO = list(name = "Inactivo", minWidth = 90,
                                     style = list(whiteSpace = "nowrap")),
        TOTAL    = list(name = "Total",    minWidth = 80,
                                     style = list(fontWeight = "bold", whiteSpace = "nowrap"))
      ),
      modo_seleccion   = "fila",
      filas_bloqueadas = as.character(length(.niveles_trans)),
      searchable       = FALSE,
      sortable         = FALSE,
      page_size        = 10L,
      modal_titulo_fn  = function(sel) paste0("Detalle \u2014 ", sel$id),
      modal_pre_fn     = function(sel) sel_matriz_estado(as.character(sel$id)),
      modal_contenido_fn = function(sel) .coha_detalle_ui(session$ns("tbl_mod_matriz_total")),
      modal_size       = "xl"
    )
    
    ### Matriz porcentual ----
    .coha_tabla(
      id   = "tbl_matriz_trans",
      data = reactive({
        req(matriz_trans(), input$sel_tipo_matriz)
        m     <- matriz_trans()
        datos <- m %>% dplyr::filter(EstadoInicial != "TOTAL")
        tot_g <- sum(datos$ACTIVO, na.rm = TRUE) + sum(datos$INACTIVO, na.rm = TRUE)
        tot_a <- sum(datos$ACTIVO,   na.rm = TRUE)
        tot_i <- sum(datos$INACTIVO, na.rm = TRUE)
        
        result <- switch(input$sel_tipo_matriz,
                         pct_fila = m %>%
                           dplyr::mutate(
                             ACTIVO   = ifelse(TOTAL > 0, ACTIVO   / TOTAL, NA_real_),
                             INACTIVO = ifelse(TOTAL > 0, INACTIVO / TOTAL, NA_real_),
                             TOTAL    = ifelse(TOTAL > 0, 1,                NA_real_)
                           ),
                         pct_col = m %>%
                           dplyr::mutate(
                             ACTIVO   = if (tot_a > 0) ACTIVO   / tot_a else NA_real_,
                             INACTIVO = if (tot_i > 0) INACTIVO / tot_i else NA_real_,
                             TOTAL    = if (tot_g > 0) TOTAL    / tot_g else NA_real_
                           ),
                         pct_total = m %>%
                           dplyr::mutate(
                             ACTIVO   = if (tot_g > 0) ACTIVO   / tot_g else NA_real_,
                             INACTIVO = if (tot_g > 0) INACTIVO / tot_g else NA_real_,
                             TOTAL    = if (tot_g > 0) TOTAL    / tot_g else NA_real_
                           )
        )
        result %>% dplyr::rename(Estado = EstadoInicial)
      }),
      titulo = reactive({
        switch(input$sel_tipo_matriz,
               pct_fila  = "Transiciones \u2014 % Fila (origen)",
               pct_col   = "Transiciones \u2014 % Columna (destino)",
               pct_total = "Transiciones \u2014 % Total general"
        )
      }),
      subtitulo = reactive(paste0(
        .coha_mes(mes_inicio()), " \u2192 ", .coha_mes(mes_vigente())
      )),
      footer = reactive({
        ind <- indicadores_trans()
        switch(input$sel_tipo_matriz,
               pct_fila = paste0(
                 "De cada 100 activos al inicio: ",
                 round(.na0(ind$retencion) * 100, 1), " siguen activos, ",
                 round(.na0(ind$churn)     * 100, 1), " se perdieron"
               ) %>% HTML,
               pct_col = paste0(
                 "De cada 100 activos al cierre: ",
                 round(.na0(ind$retencion) * 100, 1), " ya eran activos antes"
               ) %>% HTML,
               pct_total = {
                 total <- ind$aa + ind$ai + ind$ia + ind$ii
                 v     <- if (total > 0) (ind$aa + ind$ii) / total else NA_real_
                 paste0(
                   round(.na0(v) * 100, 1),
                   "% del panel no cambi\u00f3 de estado en el periodo"
                 ) %>% HTML
               }
        )
      }),
      id_col    = "Estado",
      col_specs = list(
        Estado   = list(
          name     = "Estado Inicial",
          minWidth = 130,
          sticky   = "left",
          style    = list(whiteSpace = "nowrap", fontWeight = "600")
        ),
        ACTIVO   = list(
          name     = "Activo",
          minWidth = 85,
          cell     = function(v) if (is.na(v)) "\u2014" else .coha_fmt_pct(v),
          style    = list(whiteSpace = "nowrap")
        ),
        INACTIVO = list(
          name     = "Inactivo",
          minWidth = 90,
          cell     = function(v) if (is.na(v)) "\u2014" else .coha_fmt_pct(v),
          style    = list(whiteSpace = "nowrap")
        ),
        TOTAL    = list(
          name     = "Total",
          minWidth = 80,
          cell     = function(v) if (is.na(v)) "\u2014" else .coha_fmt_pct(v),
          style    = list(fontWeight = "bold", whiteSpace = "nowrap")
        )
      ),
      modo_seleccion = "ninguno",
      searchable     = FALSE,
      sortable       = FALSE,
      page_size      = 10L
    )
    
    ### KPIs de tasa ----
    .coha_caja(
      "kpi_retencion",
      valor = reactive({
        v <- indicadores_trans()$retencion
        .coha_valor(v, formato = "porcentaje",
                                  color = .col_kpi(v, "retencion"))
      }),
      texto         = "Retenci\u00f3n Activos",
      icono         = "check-double",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        ind <- indicadores_trans()
        paste0(
          ind$aa, " clientes activos se mantuvieron<br/>",
          "Activos al inicio: ", ind$activos_ini,
          " \u2014 al cierre: ", ind$activos_fin
        ) %>% HTML
      }),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_churn",
      valor = reactive({
        v <- indicadores_trans()$churn
        .coha_valor(v, formato = "porcentaje",
                                  color = .col_kpi(v, "churn"))
      }),
      texto         = "P\u00e9rdida (Churn)",
      icono         = "arrow-down",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        ind <- indicadores_trans()
        if (is.na(ind$cambio_neto)) {
          "Sin datos suficientes para calcular el cambio neto."
        } else {
          neto_txt <- if (ind$cambio_neto >= 0) paste0("+", ind$cambio_neto)
          else as.character(ind$cambio_neto)
          paste0(
            ind$ai, " clientes dejaron de comprar<br/>",
            "Cambio neto de la base: ", neto_txt, " clientes"
          ) %>% HTML
        }
      }),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_reactivacion",
      valor = reactive({
        v <- indicadores_trans()$reactivacion
        .coha_valor(v, formato = "porcentaje",
                                  color = .col_kpi(v, "reactivacion"))
      }),
      texto         = "Reactivaci\u00f3n",
      icono         = "sync-alt",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        ind <- indicadores_trans()
        paste0(
          ind$ia, " inactivos volvieron a comprar<br/>",
          ind$ii, " permanecen sin facturar"
        ) %>% HTML
      }),
      footer_class  = "caja-modal-footer"
    )
    
    ### KPIs de volumen ----
    .coha_caja(
      "kpi_cambio_neto",
      valor = reactive({
        v     <- indicadores_trans()$cambio_neto
        color <- if (is.na(v)) "#999999" else if (v >= 0) "#27AE60" else "#E74C3C"
        .coha_valor(v, formato = "coma", color = color)
      }),
      texto         = "Cambio Neto",
      icono         = "exchange-alt",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        ind <- indicadores_trans()
        if (is.na(ind$cambio_neto)) {
          "Sin datos suficientes para calcular el cambio neto."
        } else {
          tendencia <- if (ind$cambio_neto > 0) {
            paste0("La base de activos creci\u00f3 en ", ind$cambio_neto, " clientes")
          } else if (ind$cambio_neto < 0) {
            paste0("La base de activos se redujo en ", abs(ind$cambio_neto), " clientes")
          } else {
            "La base no cambi\u00f3 de tama\u00f1o"
          }
          paste0(
            ind$ia, " recuperados vs ", ind$ai, " perdidos<br/>",
            tendencia
          ) %>% HTML
        }
      }),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_estabilidad",
      valor = reactive({
        ind   <- indicadores_trans()
        total <- ind$aa + ind$ai + ind$ia + ind$ii
        v     <- dplyr::if_else(total > 0, (ind$aa + ind$ii) / total, NA_real_)
        .coha_valor(
          v, formato = "porcentaje",
          color = if (is.na(v))       "#999999"
          else if (v >= 0.75) "#27AE60"
          else if (v >= 0.55) "#F4A820"
          else                "#E74C3C"
        )
      }),
      texto         = "Estabilidad",
      icono         = "anchor",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        ind   <- indicadores_trans()
        total <- ind$aa + ind$ai + ind$ia + ind$ii
        paste0(
          ind$aa + ind$ii, " de ", total, " clientes no cambiaron de estado"
        ) %>% HTML
      }),
      footer_class  = "caja-modal-footer"
    )
    
    
    ## Resumen mensual por estado ----
    
    ### KPIs de duración y dinámica (tasas mes a mes) ----
    .coha_caja(
      "kpi_vida_activo",
      valor = reactive({
        v <- indicadores_duracion()$t_vida_activo
        .coha_valor(
          v, formato = "coma",
          color = if (is.na(v))     "#999999"
          else if (v >= 6)  "#27AE60"
          else if (v >= 3)  "#F4A820"
          else              "#E74C3C"
        )
      }),
      texto         = "Vida Media Activo (meses)",
      icono         = "hourglass-half",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        v <- indicadores_duracion()$t_vida_activo
        if (is.na(v)) "Sin suficientes transiciones para calcular."
        else paste0(
          "Meses promedio que un cliente permanece activo antes de inactivarse"
        ) %>% HTML
      }),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_t_reactiv",
      valor = reactive({
        v <- indicadores_duracion()$t_reactiv
        .coha_valor(
          v, formato = "coma",
          color = if (is.na(v))     "#999999"
          else if (v <= 3)  "#27AE60"
          else if (v <= 6)  "#F4A820"
          else              "#E74C3C"
        )
      }),
      texto         = "Tiempo de Reactivaci\u00f3n (meses)",
      icono         = "clock",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        v <- indicadores_duracion()$t_reactiv
        if (is.na(v)) "Sin suficientes transiciones para calcular."
        else paste0(
          "Meses promedio que tarda un inactivo en volver a comprar.<br/>",
          "<strong>Nota:</strong> los que nunca regresan no se cuentan,",
          " lo que reduce el promedio observado."
        ) %>% HTML
      }),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_balance_neto",
      valor = reactive({
        v     <- indicadores_valor()$balance_neto
        color <- if (is.na(v))   "#999999"
        else if (v > 0)  "#27AE60"
        else if (v == 0) "#F4A820"
        else             "#E74C3C"
        .coha_valor(round(v, 1), formato = "coma", color = color)
      }),
      texto         = "Balance Neto de Base",
      icono         = "balance-scale",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        val <- indicadores_valor()
        if (is.na(val$balance_neto)) {
          "Sin suficientes transiciones para calcular el balance neto."
        } else {
          tendencia <- if (val$balance_neto > 0) "<strong>La base est\u00e1 creciendo.</strong>"
          else if (val$balance_neto < 0) "<strong>La base est\u00e1 decreciendo.</strong>"
          else "<strong>La base est\u00e1 estable.</strong>"
          paste0(
            tendencia, "<br/>",
            "Recuperaciones esperadas vs p\u00e9rdidas esperadas por mes.<br/>",
            "<strong>Nota:</strong> asume que las tasas mensuales",
            " se mantienen constantes el pr\u00f3ximo mes."
          ) %>% HTML
        }
      }),
      footer_class  = "caja-modal-footer"
    )
    
    ### KPIs de largo plazo y valor en riesgo ----
    .coha_caja(
      "kpi_pi_activo",
      valor = reactive({
        v <- estado_estacionario()$pi_a
        .coha_valor(
          v, formato = "porcentaje",
          color = if (is.na(v))       "#999999"
          else if (v >= 0.60) "#27AE60"
          else if (v >= 0.40) "#F4A820"
          else                "#E74C3C"
        )
      }),
      texto         = "Activos en Largo Plazo",
      icono         = "infinity",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        ee <- estado_estacionario()
        if (is.na(ee$pi_a)) "Sin suficientes transiciones para proyectar."
        else paste0(
          "Proporci\u00f3n a la que converge la base activa",
          " si las tasas no cambian.<br/>",
          "Distancia al equilibrio hoy: ", .coha_fmt_pct(.na0(ee$distancia)), "<br/>",
          "<strong>Nota:</strong> asume tasas constantes."
        ) %>% HTML
      }),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_pi_inactivo",
      valor = reactive({
        v <- estado_estacionario()$pi_i
        .coha_valor(
          v, formato = "porcentaje",
          color = if (is.na(v))       "#999999"
          else if (v <= 0.30) "#27AE60"
          else if (v <= 0.50) "#F4A820"
          else                "#E74C3C"
        )
      }),
      texto         = "Inactivos en Largo Plazo",
      icono         = "moon",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive({
        ee <- estado_estacionario()$pi_i
        if (is.na(ee)) "Sin suficientes transiciones para proyectar."
        else paste0(
          "Proporci\u00f3n estructural de inactivos si las tasas no cambian"
        ) %>% HTML
      }),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_sacos_riesgo",
      valor = reactive({
        v <- indicadores_valor()$sacos_riesgo
        .coha_valor(
          v, formato = "coma",
          color = if (is.na(v)) "#999999" else "#E74C3C"
        )
      }),
      texto         = "Sacos en Riesgo",
      icono         = "exclamation-triangle",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive(
        "Sacos mensuales en riesgo si el churn se mantiene" %>% HTML
      ),
      footer_class  = "caja-modal-footer"
    )
    
    .coha_caja(
      "kpi_rev_riesgo",
      valor = reactive({
        v <- indicadores_valor()$rev_riesgo
        .coha_valor(
          v, formato = "dinero",
          color = if (is.na(v)) "#999999" else "#E74C3C"
        )
      }),
      texto         = "Margen en Riesgo",
      icono         = "dollar-sign",
      colores       = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer        = reactive(
        "Margen mensual en riesgo si el churn se mantiene" %>% HTML
      ),
      footer_class  = "caja-modal-footer"
    )
    
    ### Tabla resumen mensual con selección por celda ----
    .coha_tabla(
      id        = "tbl_resumen_mensual",
      data      = resumen_mensual,
      titulo    = "Clientes por Estado y Mes",
      subtitulo = paste0(
        "Conteo de unidades comerciales \u00fanicas",
        " \u2014 clic en una celda para ver el detalle"
      ),
      id_col    = "Estado",
      col_specs = list(
        Estado = list(
          name     = "Estado",
          minWidth = 140,
          sticky   = "left",
          style    = list(whiteSpace = "nowrap", fontWeight = "600")
        )
      ),
      modo_seleccion   = "celda",
      filas_bloqueadas = as.character(length(.niveles_resumen)),
      searchable       = FALSE,
      sortable         = FALSE,
      page_size        = 10L,
      modal_titulo_fn  = function(sel) paste0(sel$id, " \u2014 ", sel$col),
      modal_contenido_fn = function(sel) {
        sel_resumen$estado <- sel$id
        sel_resumen$mes    <- sel$col
        .coha_detalle_ui(ns("tbl_mod_resumen_celda"))
      },
      modal_size       = "xl"
    )
    
    # Detalle de celda del resumen mensual.
    # FIX YTD: data recibe el acumulado hasta la fecha seleccionada para las UCs
    # en ese estado en ese mes, no solo el snapshot puntual del mes.
    # Así Ej_SacosYTD ≠ Ej_SacosVig cuando la celda no es el primer mes.
    .coha_detalle(
      id   = "tbl_mod_resumen_celda",
      data = reactive({
        req(sel_resumen$estado, sel_resumen$mes, mapa_mes_fecha())
        fecha_sel <- mapa_mes_fecha()[[sel_resumen$mes]]
        req(!is.null(fecha_sel))
        # UCs en el estado seleccionado en el mes exacto
        ucs_en_mes <- bd_f() %>%
          dplyr::filter(FecProceso == fecha_sel, EstadoPanel == sel_resumen$estado) %>%
          dplyr::select(LinNegCod, CliNitPpal)
        # Acumulado YTD hasta fecha_sel para esas UCs
        bd_f() %>%
          dplyr::semi_join(ucs_en_mes, by = c("LinNegCod", "CliNitPpal")) %>%
          dplyr::filter(FecProceso <= fecha_sel)
      }),
      mes_vig = reactive({
        req(sel_resumen$mes, mapa_mes_fecha())
        mapa_mes_fecha()[[sel_resumen$mes]]
      }),
      titulo    = reactive({
        req(sel_resumen$estado, sel_resumen$mes)
        paste0(sel_resumen$estado, " \u2014 ", sel_resumen$mes)
      }),
      subtitulo = "Detalle de unidades comerciales para la celda seleccionada",
      footer    = reactive({
        req(sel_resumen$estado, sel_resumen$mes, mapa_mes_fecha())
        fecha_sel <- mapa_mes_fecha()[[sel_resumen$mes]]
        n <- bd_f() %>%
          dplyr::filter(FecProceso == fecha_sel, EstadoPanel == sel_resumen$estado) %>%
          dplyr::summarise(n = dplyr::n_distinct(LinNegCod, CliNitPpal)) %>%
          dplyr::pull(n)
        paste0(n, " UC(s) en estado ", sel_resumen$estado, " \u2014 ", sel_resumen$mes)
      }),
      footer_tipo = "info"
    )
    
    ### Gráfico de crecimiento ----
    output$plot_crecimiento <- .crm_render_plotly({
      req(serie_crecimiento())
      df <- serie_crecimiento()
      
      plotly::plot_ly(df, x = ~FecProceso) %>%
        plotly::add_trace(
          y             = ~Total,
          name          = "Total",
          type          = "scatter",
          mode          = "lines+markers",
          line          = list(color = "#08519c", width = 2.5),
          marker        = list(color = "#08519c", size = 7),
          hovertemplate = "<b>Total</b><br>Mes: %{x|%b-%y}<br>Clientes: %{y:,}<extra></extra>"
        ) %>%
        plotly::add_trace(
          y             = ~ACTIVO,
          name          = "Activos",
          type          = "scatter",
          mode          = "lines+markers",
          line          = list(color = "#2ca25f", width = 2, dash = "dot"),
          marker        = list(color = "#2ca25f", size = 6),
          hovertemplate = "<b>Activos</b><br>Mes: %{x|%b-%y}<br>Clientes: %{y:,}<extra></extra>"
        ) %>%
        plotly::add_trace(
          y             = ~INACTIVO,
          name          = "Inactivos",
          type          = "scatter",
          mode          = "lines+markers",
          line          = list(color = "#cb181d", width = 2, dash = "dot"),
          marker        = list(color = "#cb181d", size = 6),
          hovertemplate = "<b>Inactivos</b><br>Mes: %{x|%b-%y}<br>Clientes: %{y:,}<extra></extra>"
        ) %>%
        plotly::add_trace(
          y             = ~NUEVO,
          name          = "Nuevos",
          type          = "scatter",
          mode          = "lines+markers",
          line          = list(color = "#f16913", width = 2, dash = "dash"),
          marker        = list(color = "#f16913", size = 6),
          hovertemplate = "<b>Nuevos</b><br>Mes: %{x|%b-%y}<br>Clientes: %{y:,}<extra></extra>"
        ) %>%
        plotly::layout(
          title         = list(
            text = "Crecimiento poblacional",
            font = list(size = 14),
            x    = 0.02
          ),
          xaxis         = list(
            title       = "",
            tickangle   = -45,
            tickformat  = "%b-%y",
            fixedrange  = FALSE
          ),
          yaxis         = list(title = "Clientes \u00fanicos", tickformat = ","),
          legend        = list(orientation = "h", x = 0, y = -0.25),
          hovermode     = "x unified",
          margin        = list(l = 50, r = 20, t = 50, b = 80),
          plot_bgcolor  = "white",
          paper_bgcolor = "white"
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
  })
}

