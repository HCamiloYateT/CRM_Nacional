# TablaDetalleDat ----
TablaDetalleDatUI <- function(id) {
  ns <- NS(id)
  div(
    class = "rt-contenedor reactable-wrap rt-sortable",
    div(class = "rt-titulo",    "Detalle de Transacciones"),
    div(class = "rt-subtitulo", "Todas las columnas del dataset activo"),
    reactable::reactableOutput(ns("tabla"))
  )
}
TablaDetalleDat <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    
    # Helpers: inferencia de formato por nombre y tipo de columna
    .es_fecha  <- function(nm) grepl("Fec|Date|fecha", nm, ignore.case = TRUE)
    .es_dinero <- function(nm) grepl("Margen|Precio|Valor|Monto|MNFCC", nm, ignore.case = TRUE)
    .es_num    <- function(nm) grepl("Sac|Kilo|Cant|Num", nm, ignore.case = TRUE)
    
    # Construccion de colDef evaluada con el df disponible en el momento del render
    .build_coldefs <- function(df) {
      nms  <- names(df)
      cols <- lapply(nms, function(nm) {
        col <- df[[nm]]
        if (.es_fecha(nm) && inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
          reactable::colDef(
            name = nm, minWidth = 110,
            cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %Y")
          )
        } else if (.es_dinero(nm) && is.numeric(col)) {
          reactable::colDef(
            name   = nm, minWidth = 110,
            format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
          )
        } else if (.es_num(nm) && is.numeric(col)) {
          reactable::colDef(
            name   = nm, minWidth = 90,
            format = reactable::colFormat(separators = TRUE, digits = 1)
          )
        } else {
          reactable::colDef(name = nm, minWidth = 100)
        }
      })
      names(cols) <- nms
      cols
    }
    
    # Render principal: colDef construidos dentro del renderReactable con df disponible
    output$tabla <- reactable::renderReactable({
      req(dat())
      df <- dat()
      reactable::reactable(
        data            = df,
        columns         = .build_coldefs(df),
        sortable        = TRUE,
        searchable      = TRUE,
        defaultPageSize = 20,
        compact         = TRUE,
        bordered        = TRUE,
        striped         = FALSE,
        highlight       = TRUE,
        language = reactable::reactableLang(
          searchPlaceholder = "Buscar...",
          noData            = "Sin resultados",
          pageInfo          = "{rowStart}\u2013{rowEnd} de {rows} registros",
          pagePrevious      = "Anterior",
          pageNext          = "Siguiente"
        ),
        theme = reactable::reactableTheme(
          headerStyle = list(fontWeight = "600", fontSize = "12px", cursor = "pointer"),
          cellStyle   = list(fontSize = "12px", padding = "3px 6px")
        )
      )
    })
  })
}

# Presupuesto ----

PresupuestoUI <- function(id) {
  ns <- NS(id)
  tagList(
    # KPIs principales
    fluidRow(
      column(3, racafeModulos::CajaModalUI(ns("kpi_cumpl_sacos_ytd"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_cumpl_margen_ytd"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_ritmo_req_sacos"))),
      column(3, racafeModulos::CajaModalUI(ns("kpi_ritmo_req_margen")))
    ),
    # Tabla GT resumen mensual/acumulado — con scroll horizontal
    fluidRow(
      bs4Dash::bs4Card(
        title = "Presupuesto", status = "white", solidHeader = TRUE,
        width = 12, collapsible = FALSE,
        div(
          style = "overflow-x: auto; width: 100%;",
          gt_output(ns("Presupuesto"))
        )
      )
    ),
    # Switch vista acumulada / mensual para graficos de serie
    fluidRow(
      column(4,
             div(
               style = "margin-left: 5px;",
               shinyWidgets::materialSwitch(
                 inputId = ns("vista_acumulada"), label = "Vista Acumulada",
                 value = TRUE, status = "danger", inline = TRUE, width = "100%"
               )
             )
      )
    ),
    # Graficos de serie temporal sacos y margen
    fluidRow(
      column(6,
             bs4Dash::bs4Card(
               title = "Presupuesto de Sacos (70 Kgs)", status = "white",
               solidHeader = TRUE, width = 12, collapsible = TRUE,
               plotlyOutput(ns("GraficoPresupuestoSacos"))
             )
      ),
      column(6,
             bs4Dash::bs4Card(
               title = "Presupuesto de $MNFCC", status = "white",
               solidHeader = TRUE, width = 12, collapsible = TRUE,
               plotlyOutput(ns("GraficoPresupuestoMargen"))
             )
      )
    ),
    # Tablas de cumplimiento por dimension + graficos — colapsado por defecto
    fluidRow(
      bs4Dash::bs4Card(
        title = "Cumplimiento y proyección por dimensión", status = "white",
        solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        fluidRow(
          column(10,
                 racafe::BotonesRadiales(
                   inputId = "dim_breakdown",
                   choices = c(
                     "Línea de Negocio" = "linneg", "Segmento" = "segmento",
                     "Asesor" = "asesor", "Departamento" = "departamento",
                     "Municipio" = "municipio", "Unidad Comercial" = "unidadcomercial"
                   ),
                   selected = "linneg", ns = ns
                 )
          )
        ),
        # Tabla de Sacos
        uiOutput(ns("tabla_sacos_ui")),
        div(style = "margin-top: 16px;"),
        # Tabla de Margen
        uiOutput(ns("tabla_margen_ui")),
        uiOutput(ns("footer_metodologia")),
        fluidRow(
          column(6,
                 bs4Dash::bs4Card(
                   title = uiOutput(ns("titulo_proyeccion")),
                   status = "white", solidHeader = FALSE, width = 12,
                   collapsible = TRUE, elevation = 1,
                   fluidRow(
                     column(12,
                            div(
                              style = "display:flex; align-items:center; gap:12px; margin-bottom:4px;",
                              racafe::BotonesRadiales(
                                inputId = "medida_proyeccion",
                                choices = c("Sacos" = "sacos", "Margen" = "margen"),
                                selected = "sacos", ns = ns
                              )
                            )
                     )
                   ),
                   plotlyOutput(ns("grafico_proyeccion"))
                 )
          ),
          column(6,
                 bs4Dash::bs4Card(
                   title = "Velocidad de ejecución frente a la requerida",
                   status = "white", solidHeader = FALSE, width = 12,
                   collapsible = TRUE, elevation = 1,
                   fluidRow(
                     column(12,
                            div(
                              style = "display:flex; align-items:center; gap:12px; margin-bottom:4px;",
                              racafe::BotonesRadiales(
                                inputId = "medida_velocidad",
                                choices = c("Sacos" = "sacos", "Margen" = "margen"),
                                selected = "sacos", ns = ns
                              )
                            )
                     )
                   ),
                   plotlyOutput(ns("grafico_velocidad"))
                 )
          )
        )
      )
    )
  )
}
Presupuesto <- function(id, dat, clientes_raw, filtros = NULL, margenes_pyg = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Constantes ----
    .MESES <- c(
      "ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
      "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"
    )
    
    # Escala semaforica unificada
    .SEMAFORO <- list(
      umbral_exceso  = 1.00, umbral_alto    = 0.80, umbral_bajo    = 0.50,
      col_azul       = "#1D4ED8", col_verde    = "#15803D",
      col_amarillo   = "#B45309", col_rojo     = "#B91C1C",
      fondo_azul     = "#EFF6FF", fondo_verde    = "#F0FDF4",
      fondo_amarillo = "#FFFBEB", fondo_rojo     = "#FEF2F2",
      punto_azul     = "#2563EB", punto_verde    = "#16A34A",
      punto_amarillo = "#D97706", punto_rojo     = "#DC2626"
    )
    
    # Colores de brecha
    .CV <- "#15803D"; .CA <- "#B45309"; .CR <- "#B91C1C"
    
    # Helpers ----
    
    # Normalizacion de kilos por linea de negocio
    .normalizar_kilos <- function(df) {
      df %>% mutate(
        SacosFact = suppressWarnings(as.numeric(SacosFact)),
        Margen = suppressWarnings(as.numeric(Margen)),
        Margen = ifelse(is.finite(Margen), Margen, NA_real_)
      )
    }
    
    # Mapeo dimension a columna y etiqueta
    .dim_col <- function(dim) {
      switch(dim,
             linneg          = "LinNeg",
             segmento        = "Segmento",
             asesor          = "Asesor",
             departamento    = "Departamento",
             municipio       = "Municipio",
             unidadcomercial = "UnidadComercial"
      )
    }
    .dim_lbl <- function(dim) {
      switch(dim,
             linneg          = "Línea de Negocio",
             segmento        = "Segmento",
             asesor          = "Asesor",
             departamento    = "Departamento",
             municipio       = "Municipio",
             unidadcomercial = "Unidad Comercial"
      )
    }
    
    # Semaforo: retorna color o emoji segun tipo y valor
    .semaforo <- function(v, tipo = "fondo_na") {
      s <- .SEMAFORO
      switch(tipo,
             tramo = dplyr::case_when(
               is.na(v)             ~ "na",
               v >= s$umbral_exceso ~ "azul",
               v >= s$umbral_alto   ~ "verde",
               v >= s$umbral_bajo   ~ "amarillo",
               TRUE                 ~ "rojo"
             ),
             fondo = dplyr::case_when(
               v >= s$umbral_exceso ~ s$fondo_azul,
               v >= s$umbral_alto   ~ s$fondo_verde,
               v >= s$umbral_bajo   ~ s$fondo_amarillo,
               TRUE                 ~ s$fondo_rojo
             ),
             fondo_na = dplyr::case_when(
               is.na(v)             ~ "white",
               v >= s$umbral_exceso ~ s$fondo_azul,
               v >= s$umbral_alto   ~ s$fondo_verde,
               v >= s$umbral_bajo   ~ s$fondo_amarillo,
               TRUE                 ~ s$fondo_rojo
             ),
             texto_na = dplyr::case_when(
               is.na(v)             ~ "#374151",
               v >= s$umbral_exceso ~ s$col_azul,
               v >= s$umbral_alto   ~ s$col_verde,
               v >= s$umbral_bajo   ~ s$col_amarillo,
               TRUE                 ~ s$col_rojo
             ),
             emoji = dplyr::case_when(
               is.na(v)             ~ "",
               v >= s$umbral_exceso ~ "\U1F535",
               v >= s$umbral_alto   ~ "\U1F7E2",
               v >= s$umbral_bajo   ~ "\U1F7E1",
               TRUE                 ~ "\U1F534"
             )
      )
    }
    
    # Color de punto scatter segun proyeccion de cierre
    .color_punto_cumpl <- function(cx, cy) {
      s <- .SEMAFORO
      dplyr::case_when(
        cy >= s$umbral_exceso                     ~ s$punto_azul,
        cy >= s$umbral_alto & cx >= s$umbral_alto ~ s$punto_verde,
        cy >= s$umbral_bajo & cx >= s$umbral_bajo ~ s$punto_amarillo,
        TRUE                                      ~ s$punto_rojo
      )
    }
    
    # Estilos semaforo para columnas de porcentaje en tabla gt
    gt_pct_style_semaforo <- function(gt_table, ...) {
      cols <- rlang::ensyms(...)
      s    <- .SEMAFORO
      for (col in cols) {
        gt_table <- gt_table %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_rojo, weight = "bold"),
            locations = gt::cells_body(columns = !!col, rows = !!col < s$umbral_bajo)
          ) %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_amarillo, weight = "bold"),
            locations = gt::cells_body(
              columns = !!col,
              rows    = !!col >= s$umbral_bajo & !!col < s$umbral_alto
            )
          ) %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_verde, weight = "bold"),
            locations = gt::cells_body(
              columns = !!col,
              rows    = !!col >= s$umbral_alto & !!col < s$umbral_exceso
            )
          ) %>%
          gt::tab_style(
            style     = gt::cell_text(color = s$col_azul, weight = "bold"),
            locations = gt::cells_body(columns = !!col, rows = !!col >= s$umbral_exceso)
          )
      }
      gt_table
    }
    
    # Estilo variaciones ----
    gt_var_style_local <- function(gt_table, columna) {
      valores <- gt_table[["_data"]][[columna]]
      if (is.null(valores) || !is.numeric(valores)) {
        return(gt_table)
      }
      
      for (estilo in list(
        list(filas = which(!is.na(valores) & valores > 0), color = .CV),
        list(filas = which(!is.na(valores) & valores < 0), color = .CR)
      )) {
        if (length(estilo$filas) > 0L) {
          gt_table <- gt::tab_style(
            gt_table,
            style = gt::cell_text(color = estilo$color, weight = "bold"),
            locations = gt::cells_body(
              columns = dplyr::all_of(columna),
              rows = estilo$filas
            )
          )
        }
      }
      gt_table
    }
    
    # ETL ----
    
    # ETL base: agrupa ventas por cliente/linea/periodo/mes
    procesar_datos_base <- function(data, periodo_desde) {
      data %>%
        filter(!is.na(FecMes), year(FecMes) >= periodo_desde,
               !is.na(SacosFact) | !is.na(Margen)) %>%
        .normalizar_kilos() %>%
        group_by(
          CliNitPpal, LinNegCod,
          Periodo = year(FecMes),
          Fecha = .MESES[month(FecMes)]
        ) %>%
        summarise(
          Sacos70   = sum(SacosFact, na.rm = TRUE),
          MargenFCC = sum(Margen, na.rm = TRUE),
          .groups   = "drop"
        )
    }
    
    # ETL con dimensiones extendidas para breakdowns
    procesar_datos_dim <- function(data, periodo) {
      data %>%
        filter(!is.na(FecMes), year(FecMes) == periodo,
               !is.na(SacosFact) | !is.na(Margen)) %>%
        .normalizar_kilos() %>%
        group_by(
          CliNitPpal, LinNegCod, LinNeg = CLLinNegNo, Segmento,
          Asesor, Departamento = Depto, Municipio = Mpio, RazSoc = PerRazSoc,
          Fecha = .MESES[month(FecMes)]
        ) %>%
        summarise(
          Sacos70   = sum(SacosFact, na.rm = TRUE),
          MargenFCC = sum(Margen, na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        mutate(
          Mes_Num         = match(Fecha, .MESES),
          UnidadComercial = paste0(RazSoc, " — ", LinNeg)
        )
    }
    
    # ETL lotes: detalle de facturas para modal de breakdown
    procesar_lotes_dim <- function(data, periodo) {
      data %>%
        filter(!is.na(FecMes), year(FecMes) == periodo,
               !is.na(SacosFact) | !is.na(Margen)) %>%
        .normalizar_kilos() %>%
        mutate(
          Sacos70         = SacosFact,
          UnidadComercial = paste0(PerRazSoc, " — ", CLLinNegNo)
        ) %>%
        select(
          FecFact, CliNitPpal, PerRazSoc, LinNegCod, CLLinNegNo,
          Segmento, Asesor, Departamento = Depto, Municipio = Mpio,
          UnidadComercial, CLLotCod, Sacos70, Margen
        )
    }
    
    # Presupuesto mensual: ultimo proceso del anio por cliente/linea
    obtener_presupuesto <- function(data, clientes, periodo) {
      clientes %>%
        transmute(
          CliNitPpal, LinNegCod, Periodo = periodo,
          PptoSS = dplyr::coalesce(SSPpto, 0) / 12,
          PptoMa = dplyr::coalesce(MNFCCPpto, 0) / 12
        ) %>%
        tidyr::expand_grid(Fecha = .MESES)
    }
    
    # Snapshot de presupuesto con dimensiones extendidas
    obtener_presupuesto_dim <- function(data, clientes, periodo) {
      clientes %>%
        transmute(
          CliNitPpal, LinNegCod, LinNeg = LinNegocio, Segmento,
          Asesor, Departamento = Depto, Municipio = Mpio,
          RazSoc = PerRazSoc,
          PptoSS_anual = dplyr::coalesce(SSPpto, 0),
          PptoMa_anual = dplyr::coalesce(MNFCCPpto, 0)
        )
    }
    
    # Serie temporal por periodo/mes para graficos de linea
    preparar_datos_grafico <- function(datos_base, ppto, periodo) {
      db <- datos_base %>%
        filter(Periodo == periodo) %>%
        group_by(Periodo, Fecha) %>%
        summarise(
          Sacos70   = sum(Sacos70,   na.rm = TRUE),
          MargenFCC = sum(MargenFCC, na.rm = TRUE),
          .groups   = "drop"
        )
      ppto_ag <- ppto %>%
        filter(Periodo == periodo) %>%
        group_by(Periodo, Fecha) %>%
        summarise(
          PptoSS = sum(PptoSS, na.rm = TRUE),
          PptoMa = sum(PptoMa, na.rm = TRUE),
          .groups = "drop"
        )
      tidyr::crossing(Periodo = periodo, Fecha = .MESES) %>%
        left_join(db, by = c("Periodo", "Fecha")) %>%
        left_join(ppto_ag, by = c("Periodo", "Fecha")) %>%
        mutate(
          Mes_Num     = match(Fecha, .MESES),
          Sacos_Real  = Sacos70,
          Margen_Real = MargenFCC,
          Sacos_Ppto  = coalesce(PptoSS,   0),
          Margen_Ppto = coalesce(PptoMa,   0)
        ) %>%
        arrange(Mes_Num)
    }
    
    # Ritmo ponderado de los ultimos N meses con mayor peso al mes mas reciente
    # Degrada a promedio simple si hay menos meses que la ventana
    .ritmo_ponderado_dim <- function(datos_dim, col_valor, mes_actual,
                                     dim_cols, ventana = 3L) {
      datos_dim %>%
        filter(Mes_Num <= mes_actual, !is.na(.data[[col_valor]])) %>%
        group_by(across(all_of(dim_cols)), Mes_Num) %>%
        summarise(v = sum(.data[[col_valor]], na.rm = TRUE), .groups = "drop") %>%
        group_by(across(all_of(dim_cols))) %>%
        group_modify(~ {
          ultimos <- .x %>% arrange(Mes_Num) %>% tail(ventana)
          n_ult   <- nrow(ultimos)
          if (n_ult == 0L) return(tibble(ritmo = 0))
          pesos <- seq_len(n_ult)
          tibble(ritmo = sum(ultimos$v * pesos) / sum(pesos))
        }) %>%
        ungroup()
    }
    
    # Metricas de cumplimiento y proyeccion agregadas por dimension
    agregar_por_dimension <- function(datos_dim, ppto_dim, dim_cols) {
      cal        <- calendario_r()
      mes_actual <- cal$mes_actual
      meses_rest <- cal$meses_rest
      es_multi   <- length(dim_cols) > 1L
      claves     <- c("CliNitPpal", "LinNegCod")
      grupos     <- if (es_multi) c(claves, "RazSoc", "LinNeg") else "dim"
      
      # Los atributos del presupuesto identifican también a los clientes sin ventas.
      mapa <- dplyr::full_join(
        ppto_dim %>%
          group_by(across(all_of(claves))) %>%
          arrange(desc(abs(PptoSS_anual) + abs(PptoMa_anual)), .by_group = TRUE) %>%
          slice_head(n = 1L) %>%
          ungroup() %>%
          select(all_of(claves), LinNeg, Segmento, Asesor, Departamento, Municipio, RazSoc),
        datos_dim %>% select(all_of(claves), LinNeg, Segmento, Asesor,
                             Departamento, Municipio, RazSoc) %>%
          distinct(CliNitPpal, LinNegCod, .keep_all = TRUE),
        by = claves, suffix = c("", ".dat")
      )
      
      for (columna in c("LinNeg", "Segmento", "Asesor", "Departamento", "Municipio", "RazSoc")) {
        mapa[[columna]] <- dplyr::coalesce(
          dplyr::na_if(as.character(mapa[[columna]]), "SIN DATO"),
          as.character(mapa[[paste0(columna, ".dat")]])
        )
      }
      mapa <- mapa %>%
        select(all_of(c(claves, "LinNeg", "Segmento", "Asesor", "Departamento", "Municipio", "RazSoc"))) %>%
        distinct(CliNitPpal, LinNegCod, .keep_all = TRUE) %>%
        mutate(across(c(LinNeg, Segmento, Asesor, Departamento, Municipio, RazSoc),
                      ~ coalesce(.x, "SIN DATO")))
      
      ejec <- datos_dim %>%
        select(all_of(claves), Mes_Num, Sacos70, MargenFCC) %>%
        left_join(mapa, by = claves)
      ppto <- ppto_dim %>%
        left_join(mapa, by = claves, suffix = c("", ".map"))
      for (columna in c("LinNeg", "Segmento", "Asesor", "Departamento", "Municipio", "RazSoc")) {
        ppto[[columna]] <- dplyr::coalesce(
          dplyr::na_if(as.character(ppto[[columna]]), "SIN DATO"),
          as.character(ppto[[paste0(columna, ".map")]]), "SIN DATO"
        )
      }
      ppto <- ppto %>%
        select(all_of(claves), LinNeg, Segmento, Asesor, Departamento, Municipio, RazSoc,
               PptoSS_anual, PptoMa_anual)
      
      if (!es_multi) {
        ejec <- ejec %>% mutate(dim = .data[[dim_cols]])
        ppto <- ppto %>% mutate(dim = .data[[dim_cols]])
      }
      
      ejec_ytd <- ejec %>%
        filter(Mes_Num <= mes_actual) %>%
        group_by(across(all_of(grupos))) %>%
        summarise(Sacos_YTD = sum(Sacos70, na.rm = TRUE),
                  Margen_YTD = sum(MargenFCC, na.rm = TRUE), .groups = "drop")
      ejec_mes <- ejec %>%
        filter(Mes_Num == mes_actual) %>%
        group_by(across(all_of(grupos))) %>%
        summarise(Sacos_Mes = sum(Sacos70, na.rm = TRUE),
                  Margen_Mes = sum(MargenFCC, na.rm = TRUE), .groups = "drop")
      # Presupuesto agregado con metricas mensuales y YTD derivadas
      ppto_ag <- ppto %>%
        group_by(across(all_of(grupos))) %>%
        summarise(Ppto_Sacos_Anual = sum(PptoSS_anual, na.rm = TRUE),
                  Ppto_Margen_Anual = sum(PptoMa_anual, na.rm = TRUE), .groups = "drop")
      # Ritmos ponderados de los ultimos 3 meses para proyeccion de cierre
      ritmo_sacos <- .ritmo_ponderado_dim(ejec, "Sacos70", mes_actual, grupos) %>%
        rename(Ritmo_Pond_Sacos = ritmo)
      ritmo_margen <- .ritmo_ponderado_dim(ejec, "MargenFCC", mes_actual, grupos) %>%
        rename(Ritmo_Pond_Margen = ritmo)
      
      # Calculo de metricas de cumplimiento, ritmo y proyeccion
      # Proyeccion usa ritmo ponderado reciente; cae a promedio YTD si no existe
      dplyr::full_join(ppto_ag, ejec_ytd, by = grupos) %>%
        dplyr::full_join(ejec_mes, by = grupos) %>%
        left_join(ritmo_sacos, by = grupos) %>%
        left_join(ritmo_margen, by = grupos) %>%
        mutate(
          across(c(Sacos_YTD, Margen_YTD, Sacos_Mes, Margen_Mes,
                   Ppto_Sacos_Anual, Ppto_Margen_Anual), ~ coalesce(.x, 0)),
          Ppto_Sacos_Mes = Ppto_Sacos_Anual / 12,
          Ppto_Margen_Mes = Ppto_Margen_Anual / 12,
          Ppto_Sacos_YTD = Ppto_Sacos_Anual * mes_actual / 12,
          Ppto_Margen_YTD = Ppto_Margen_Anual * mes_actual / 12,
          Cumpl_Sacos_YTD = .division_segura(Sacos_YTD / Ppto_Sacos_YTD),
          Cumpl_Margen_YTD = .division_segura(Margen_YTD / Ppto_Margen_YTD),
          Cumpl_Sacos_Mes = .division_segura(Sacos_Mes / Ppto_Sacos_Mes),
          Cumpl_Margen_Mes = .division_segura(Margen_Mes / Ppto_Margen_Mes),
          Brecha_Sacos = Sacos_YTD - Ppto_Sacos_YTD,
          Brecha_Margen = Margen_YTD - Ppto_Margen_YTD,
          Ritmo_Sacos_Actual = .division_segura(Sacos_YTD / pmax(mes_actual, 1L)),
          Ritmo_Margen_Actual = .division_segura(Margen_YTD / pmax(mes_actual, 1L)),
          Ritmo_Sacos_Req = .division_segura((Ppto_Sacos_Anual - Sacos_YTD) / pmax(meses_rest, 1L)),
          Ritmo_Margen_Req = .division_segura((Ppto_Margen_Anual - Margen_YTD) / pmax(meses_rest, 1L)),
          Meses_Restantes = meses_rest,
          Meta_Mensual_Sacos = Ritmo_Sacos_Req,
          Meta_Mensual_Margen = Ritmo_Margen_Req,
          Ritmo_Proj_Sacos = coalesce(Ritmo_Pond_Sacos, Ritmo_Sacos_Actual),
          Ritmo_Proj_Margen = coalesce(Ritmo_Pond_Margen, Ritmo_Margen_Actual),
          Proyeccion_Sacos = Sacos_YTD + Ritmo_Proj_Sacos * meses_rest,
          Proyeccion_Margen = Margen_YTD + Ritmo_Proj_Margen * meses_rest,
          Prob_Sacos = .division_segura(Proyeccion_Sacos / Ppto_Sacos_Anual),
          Prob_Margen = .division_segura(Proyeccion_Margen / Ppto_Margen_Anual)
        ) %>%
        arrange(desc(Sacos_YTD))
    }
    
    # Grafico de serie temporal ejecucion vs presupuesto con meta futura
    construir_grafico_serie <- function(datos_grafico, vista_acumulada,
                                        col_real, col_ppto, col_ref_na,
                                        titulo_acum, titulo_mensual,
                                        label_y_acum, label_y_mensual,
                                        formato_y = "numero") {
      mes_actual <- calendario_r()$mes_actual
      
      datos_grafico <- datos_grafico %>%
        arrange(Mes_Num) %>%
        mutate(
          acum_real = cumsum(ifelse(is.na(.data[[col_real]]), 0, .data[[col_real]])),
          acum_ppto = cumsum(.data[[col_ppto]])
        )
      
      ultimo_mes <- suppressWarnings(
        max(datos_grafico$Mes_Num[!is.na(datos_grafico[[col_ref_na]])], na.rm = TRUE)
      )
      if (is.infinite(ultimo_mes)) ultimo_mes <- 0
      
      datos_grafico <- datos_grafico %>%
        mutate(acum_real = ifelse(Mes_Num <= ultimo_mes, acum_real, NA_real_))
      
      ejec_acum <- datos_grafico %>%
        filter(Mes_Num <= ultimo_mes, !is.na(.data[[col_real]])) %>%
        summarise(s = sum(.data[[col_real]], na.rm = TRUE)) %>%
        pull(s)
      
      ppto_total <- sum(datos_grafico[[col_ppto]], na.rm = TRUE)
      meses_rest <- 12L - mes_actual
      meta_mens  <- .division_segura((ppto_total - ejec_acum) / pmax(meses_rest, 1))
      
      datos_grafico <- datos_grafico %>%
        mutate(
          meta_mensual = ifelse(Mes_Num > mes_actual, meta_mens, NA_real_),
          meta_acum    = ifelse(
            Mes_Num > mes_actual,
            ejec_acum + (Mes_Num - mes_actual) * meta_mens, NA_real_
          ),
          Fecha = factor(Fecha, levels = .MESES, ordered = TRUE)
        )
      
      if (vista_acumulada) {
        datos_plot <- datos_grafico %>%
          select(Fecha, acum_ppto, acum_real, meta_acum) %>%
          pivot_longer(c(acum_ppto, acum_real, meta_acum),
                       names_to = "Tipo", values_to = "Valor") %>%
          mutate(
            Tipo = dplyr::recode(Tipo,
                                 acum_ppto = "Presupuesto Acumulado",
                                 acum_real = "Ejecutado Acumulado",
                                 meta_acum = "Meta Acumulada para Ppto"
            ),
            Tipo = factor(Tipo, levels = c(
              "Presupuesto Acumulado", "Ejecutado Acumulado", "Meta Acumulada para Ppto"
            ))
          )
        titulo_g <- titulo_acum; titulo_y <- label_y_acum
      } else {
        datos_plot <- datos_grafico %>%
          select(
            Fecha,
            ppto     = all_of(col_ppto),
            real     = all_of(col_real),
            meta_men = meta_mensual
          ) %>%
          pivot_longer(c(ppto, real, meta_men), names_to = "Tipo", values_to = "Valor") %>%
          mutate(
            Tipo = dplyr::recode(Tipo,
                                 ppto     = "Presupuesto Mensual",
                                 real     = "Ejecutado Mensual",
                                 meta_men = "Meta mensual para presupuesto"
            ),
            Tipo = factor(Tipo, levels = c(
              "Presupuesto Mensual", "Ejecutado Mensual", "Meta mensual para presupuesto"
            ))
          )
        titulo_g <- titulo_mensual; titulo_y <- label_y_mensual
      }
      
      trazas <- list(
        list(
          tipo  = if (vista_acumulada) "Presupuesto Acumulado" else "Presupuesto Mensual",
          color = "#52525C", dash = "dash"
        ),
        list(
          tipo  = if (vista_acumulada) "Ejecutado Acumulado" else "Ejecutado Mensual",
          color = "#1C398E", dash = "solid"
        ),
        list(
          tipo  = if (vista_acumulada) "Meta Acumulada para Ppto" else "Meta mensual para presupuesto",
          color = "#82181A", dash = "dot"
        )
      )
      
      p <- plot_ly()
      for (tr in trazas) {
        p <- p %>% add_trace(
          data = filter(datos_plot, Tipo == tr$tipo),
          x = ~Fecha, y = ~Valor,
          name = tr$tipo, type = "scatter", mode = "lines+markers",
          line   = list(color = tr$color, dash = tr$dash, width = 2),
          marker = list(color = tr$color),
          hovertemplate = paste0(
            "<b>", tr$tipo, "</b><br>Mes: %{x}<br>Valor: %{y:,.0f}<extra></extra>"
          )
        )
      }
      
      yax <- list(title = titulo_y, tickformat = ",", rangemode = "tozero")
      if (formato_y == "dinero") yax$tickprefix <- "$"
      
      p %>%
        layout(
          title     = titulo_g,
          xaxis     = list(title = "Mes"),
          yaxis     = yax,
          legend    = list(
            orientation = "h", x = 0.5, y = -0.2,
            xanchor = "center", yanchor = "top"
          ),
          hovermode = "x unified"
        ) %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)
    }
    
    # División segura ----
    .division_segura <- function(numerador, denominador = 1) {
      valor <- suppressWarnings(numerador / denominador)
      replace(valor, !is.finite(valor), 0)
    }
    
    # Fuente financiera ----
    .resolver_datos <- function(x) {
      if (shiny::is.reactive(x)) x() else x
    }
    
    pyg_referencia_r <- reactive({
      fuente <- .resolver_datos(margenes_pyg)
      if (is.null(fuente)) return(NULL)
      req(is.data.frame(fuente))
      validate(need(all(c("FecMes", "LINNEG", "SACOS70", "MARGEN") %in% names(fuente)),
                    "La fuente PyG no contiene FecMes, LINNEG, SACOS70 y MARGEN."))
      fuente %>%
        transmute(FecMes = as.Date(FecMes),
                  LinNegCod = ifelse(as.numeric(LINNEG) == 20000, 21000, as.numeric(LINNEG)),
                  Sacos70 = if (exists(".convertir_numero_margen", mode = "function")) {
                    .convertir_numero_margen(SACOS70)
                  } else suppressWarnings(as.numeric(SACOS70)),
                  MargenFCC = if (exists(".convertir_numero_margen", mode = "function")) {
                    .convertir_numero_margen(MARGEN)
                  } else suppressWarnings(as.numeric(MARGEN))) %>%
        filter(!is.na(FecMes), LinNegCod %in% c(10000, 21000))
    })
    
    cierre_pyg_r <- reactive({
      fuente <- pyg_referencia_r()
      if (!is.null(fuente) && nrow(fuente) > 0L) {
        return(max(fuente$FecMes, na.rm = TRUE))
      }
      if (exists("ultimo_periodo_pyg", envir = .GlobalEnv, inherits = FALSE)) {
        return(as.Date(get("ultimo_periodo_pyg", envir = .GlobalEnv)))
      }
      as.Date(NA)
    })
    
    # Reactivos ----
    
    # Filtros aplicados ----
    filtros_r <- reactive({
      if (is.null(filtros)) return(NULL)
      if (shiny::is.reactive(filtros)) filtros() else filtros
    })
    
    .filtro_completo <- function(datos, columna, seleccion) {
      if (is.null(seleccion) || "TODOS" %in% toupper(as.character(seleccion))) return(TRUE)
      seleccion <- setdiff(as.character(seleccion), "")
      if (columna == "Segmento" && setequal(seleccion, c("GRANDE", "MEDIANO", "PEQUEÑO"))) return(TRUE)
      if (length(seleccion) == 0L) return(FALSE)
      if (columna %in% c("Categoria", "Producto") &&
          exists("productos_snap", envir = .GlobalEnv, inherits = FALSE)) {
        catalogo <- get("productos_snap", envir = .GlobalEnv)
        f <- filtros_r()
        if (is.data.frame(catalogo) &&
            all(c("LinNeg", "Categoria", "Producto") %in% names(catalogo)) &&
            !is.null(f) && length(f$linneg) > 0L) {
          catalogo <- catalogo %>% filter(LinNeg %in% f$linneg)
          if (columna == "Producto" && length(f$categoria) > 0L) {
            catalogo <- catalogo %>% filter(Categoria %in% f$categoria)
          }
          disponibles <- unique(as.character(catalogo[[columna]]))
          disponibles <- disponibles[!is.na(disponibles) & nzchar(disponibles)]
          if (columna == "Categoria") disponibles <- unique(c(disponibles, "SIN DATO"))
          if (length(disponibles) > 0L && all(disponibles %in% seleccion)) return(TRUE)
        }
      }
      disponibles <- unique(as.character(datos[[columna]]))
      disponibles <- disponibles[!is.na(disponibles) & nzchar(disponibles)]
      all(disponibles %in% seleccion)
    }
    
    .aplicar_seleccion <- function(datos, columna, seleccion) {
      if (.filtro_completo(datos, columna, seleccion)) return(datos)
      seleccion <- setdiff(as.character(seleccion), "")
      if (columna %in% c("CLLinNegNo", "LinNegocio") && "DIFERENCIADOS" %in% seleccion) {
        seleccion <- unique(c(seleccion, "A LA MEDIDA"))
      }
      if (length(seleccion) == 0L) return(datos[0, , drop = FALSE])
      datos %>% filter(as.character(.data[[columna]]) %in% seleccion)
    }
    
    llaves_producto_r <- reactive({
      f <- filtros_r()
      if (is.null(f)) return(NULL)
      datos <- if (shiny::is.reactive(dat)) dat() else dat
      req(is.data.frame(datos))
      for (columna in c("Categoria", "Producto", "CLLinNegNo")) {
        if (!columna %in% names(datos)) datos[[columna]] <- "SIN DATO"
      }
      datos <- datos %>%
        mutate(LinNegCod = ifelse(as.numeric(LinNegCod) == 20000, 21000, as.numeric(LinNegCod)))
      universo <- datos %>% .aplicar_seleccion("CLLinNegNo", f$linneg)
      categorias <- unique(as.character(universo$Categoria))
      productos <- unique(as.character(
        .aplicar_seleccion(universo, "Categoria", f$categoria)$Producto
      ))
      hay_restriccion <- (!is.null(f$categoria) && !setequal(f$categoria, categorias)) ||
        (!is.null(f$producto) && !setequal(f$producto, productos))
      if (!hay_restriccion) return(NULL)
      universo %>%
        .aplicar_seleccion("Categoria", f$categoria) %>%
        .aplicar_seleccion("Producto", f$producto) %>%
        distinct(CliNitPpal, LinNegCod)
    })
    
    # Población comercial del presupuesto ----
    poblacion_presupuesto_r <- reactive({
      datos <- .resolver_datos(dat)
      req(is.data.frame(datos))
      f <- filtros_r()
      for (columna in c("CLLinNegNo", "Segmento", "Asesor", "Categoria", "Producto")) {
        if (!columna %in% names(datos)) datos[[columna]] <- "SIN DATO"
      }
      datos <- datos %>%
        mutate(LinNegCod = ifelse(LinNegCod == 20000, 21000, LinNegCod))
      if (is.null(f)) return(NULL)
      datos <- .aplicar_seleccion(datos, "CLLinNegNo", f$linneg)
      restricciones <- any(c(
        !.filtro_completo(datos, "Categoria", f$categoria),
        !.filtro_completo(datos, "Producto", f$producto)
      ))
      if (!restricciones) return(NULL)
      datos %>%
        .aplicar_seleccion("Categoria", f$categoria) %>%
        .aplicar_seleccion("Producto", f$producto) %>%
        distinct(CliNitPpal, LinNegCod)
    })
    
    # Presupuesto CRM ----
    clientes_presupuesto_r <- reactive({
      clientes <- if (shiny::is.reactive(clientes_raw)) clientes_raw() else clientes_raw
      req(is.data.frame(clientes))
      requeridas <- c("FecProceso", "CLCliNit", "CliNitPpal", "LinNegCod", "SSPpto", "MNFCCPpto",
                      "Segmento", "Asesor")
      validate(need(all(requeridas %in% names(clientes)),
                    paste("Faltan columnas de presupuesto:",
                          paste(setdiff(requeridas, names(clientes)), collapse = ", "))))
      for (columna in c("Depto", "Mpio", "PerRazSoc")) {
        if (!columna %in% names(clientes)) clientes[[columna]] <- NA_character_
      }
      clientes <- clientes %>%
        mutate(
          FecProceso = as.Date(FecProceso),
          CLCliNit = suppressWarnings(as.numeric(CLCliNit)),
          CliNitPpal = suppressWarnings(as.numeric(CliNitPpal)),
          LinNegCod = ifelse(as.numeric(LinNegCod) == 20000, 21000, as.numeric(LinNegCod)),
          SSPpto = suppressWarnings(as.numeric(SSPpto)),
          MNFCCPpto = suppressWarnings(as.numeric(MNFCCPpto)),
          LinNegocio = ifelse(LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA")
        ) %>%
        filter(!is.na(CLCliNit), !is.na(CliNitPpal), !is.na(LinNegCod),
               !is.na(FecProceso), lubridate::year(FecProceso) == lubridate::year(Sys.Date()),
               !is.na(Asesor), Asesor != "TOMÁS RESTREPO") %>%
        group_by(CLCliNit, LinNegCod) %>%
        filter(FecProceso == max(FecProceso, na.rm = TRUE)) %>%
        summarise(
          SSPpto = if (all(is.na(SSPpto))) 0 else max(SSPpto, na.rm = TRUE),
          MNFCCPpto = if (all(is.na(MNFCCPpto))) 0 else max(MNFCCPpto, na.rm = TRUE),
          ValoresSacos = n_distinct(SSPpto, na.rm = TRUE),
          ValoresMargen = n_distinct(MNFCCPpto, na.rm = TRUE),
          CliNitPpal = dplyr::first(CliNitPpal),
          Segmento = dplyr::first(Segmento),
          Asesor = dplyr::first(Asesor),
          Depto = dplyr::first(Depto),
          Mpio = dplyr::first(Mpio),
          PerRazSoc = dplyr::first(PerRazSoc),
          LinNegocio = dplyr::first(LinNegocio),
          .groups = "drop"
        )
      if (any(clientes$ValoresSacos > 1L | clientes$ValoresMargen > 1L)) {
        stop("Existen valores distintos en la última fecha por CLCliNit y LinNegCod.")
      }
      clientes <- clientes %>%
        select(-ValoresSacos, -ValoresMargen)
      f <- filtros_r()
      if (!is.null(f)) {
        clientes <- .aplicar_seleccion(clientes, "LinNegocio", f$linneg)
        clientes <- .aplicar_seleccion(clientes, "Segmento", f$segmento)
        clientes <- .aplicar_seleccion(clientes, "Asesor", f$asesor)
      }
      llaves <- poblacion_presupuesto_r()
      if (!is.null(llaves)) {
        clientes <- clientes %>% semi_join(llaves, by = c("CliNitPpal", "LinNegCod"))
      }
      clientes
    })
    
    datos_centrales_r <- reactive({
      datos <- .resolver_datos(dat)
      req(is.data.frame(datos))
      requeridas <- c("FecMes", "FecFact", "CliNitPpal", "LinNegCod", "SacosFact", "Margen")
      validate(need(all(requeridas %in% names(datos)),
                    paste("Faltan columnas de datos:",
                          paste(setdiff(requeridas, names(datos)), collapse = ", "))))
      for (columna in c("CLLinNegNo", "Segmento", "Asesor", "PerRazSoc", "Depto", "Mpio",
                        "CLLotCod", "Categoria", "Producto", "Excluir", "ProdExcluir")) {
        if (!columna %in% names(datos)) datos[[columna]] <- NA
      }
      clientes <- clientes_presupuesto_r() %>%
        group_by(CliNitPpal, LinNegCod) %>%
        arrange(desc(abs(SSPpto) + abs(MNFCCPpto)), CLCliNit, .by_group = TRUE) %>%
        slice_head(n = 1L) %>% ungroup() %>%
        select(CliNitPpal, LinNegCod, DeptoCRM = Depto, MpioCRM = Mpio,
               RazonCRM = PerRazSoc)
      datos <- datos %>%
        mutate(FecMes = as.Date(FecMes), FecFact = as.Date(FecFact),
               CliNitPpal = suppressWarnings(as.numeric(CliNitPpal)),
               LinNegCod = ifelse(as.numeric(LinNegCod) == 20000, 21000, as.numeric(LinNegCod)),
               SacosFact = suppressWarnings(as.numeric(SacosFact)),
               Margen = suppressWarnings(as.numeric(Margen)),
               CLLinNegNo = ifelse(LinNegCod == 10000, "CONVENCIONALES", "A LA MEDIDA")) %>%
        left_join(clientes, by = c("CliNitPpal", "LinNegCod")) %>%
        mutate(Depto = coalesce(na_if(as.character(Depto), "SIN DATO"), as.character(DeptoCRM)),
               Mpio = coalesce(na_if(as.character(Mpio), "SIN DATO"), as.character(MpioCRM)),
               PerRazSoc = coalesce(na_if(as.character(PerRazSoc), "SIN DATO"), as.character(RazonCRM)),
               across(c(Depto, Mpio, PerRazSoc, Segmento, Asesor),
                      ~ coalesce(na_if(as.character(.x), ""), "SIN DATO"))) %>%
        select(-DeptoCRM, -MpioCRM, -RazonCRM)
      f <- filtros_r()
      if (!is.null(f)) {
        datos <- .aplicar_seleccion(datos, "CLLinNegNo", f$linneg)
        datos <- .aplicar_seleccion(datos, "Asesor", f$asesor)
        datos <- .aplicar_seleccion(datos, "Segmento", f$segmento)
        datos <- .aplicar_seleccion(datos, "Categoria", f$categoria)
        datos <- .aplicar_seleccion(datos, "Producto", f$producto)
        if (!is.null(f$fecha) && length(f$fecha) == 2L) {
          inicio <- as.Date(f$fecha[[1L]])
          fin <- as.Date(f$fecha[[2L]])
          inicio_mes <- lubridate::floor_date(inicio, "month")
          fin_mes <- lubridate::floor_date(fin, "month")
          datos <- datos %>% filter(dplyr::between(FecMes, inicio_mes, fin_mes) |
                                      lubridate::year(FecMes) == lubridate::year(inicio_mes) - 1L)
        }
      }
      datos %>% filter(!is.na(FecMes))
    })
    
    periodo_r <- reactive({
      f <- filtros_r()
      if (!is.null(f$fecha) && length(f$fecha) == 2L) {
        return(lubridate::year(as.Date(f$fecha[[2L]])))
      }
      lubridate::year(Sys.Date())
    })
    calendario_r <- reactive({
      datos <- datos_centrales_r() %>%
        filter(lubridate::year(FecMes) == periodo_r(),
               !is.na(SacosFact) | !is.na(Margen))
      mes <- if (nrow(datos) > 0L) max(lubridate::month(datos$FecMes)) else 0L
      list(mes_actual = mes, meses_rest = max(0L, 12L - mes))
    })
    
    # Reactivos de datos base
    datos_base_r    <- reactive({ procesar_datos_base(datos_centrales_r(), periodo_r() - 1L) })
    datos_act_r     <- reactive({ procesar_datos_base(datos_centrales_r(), periodo_r()) })
    ppto_r          <- reactive({ obtener_presupuesto(datos_centrales_r(), clientes_presupuesto_r(), periodo_r()) })
    datos_dim_r     <- reactive({ procesar_datos_dim(datos_centrales_r(), periodo_r()) })
    ppto_dim_r      <- reactive({ obtener_presupuesto_dim(datos_centrales_r(), clientes_presupuesto_r(), periodo_r()) })
    lotes_dim_r     <- reactive({ procesar_lotes_dim(datos_centrales_r(), periodo_r()) })
    datos_grafico_r <- reactive({
      preparar_datos_grafico(datos_act_r(), ppto_r(), periodo_r())
    })
    
    # Nombres de columnas dinamicos segun periodo activo
    col_names_r <- reactive({
      p  <- periodo_r()
      pa <- p - 1L
      list(
        sacos_ant       = paste0("Sacos70_", pa),
        margen_ant      = paste0("MargenFCC_", pa),
        sacos_act       = paste0("Sacos70_", p),
        margen_act      = paste0("MargenFCC_", p),
        ppto_sacos      = paste0("PptoSS_", p),
        ppto_margen     = paste0("PptoMa_", p),
        cumpl_sacos     = paste0("CumplSS", p),
        cumpl_margen    = paste0("CumplMgn", p),
        acum_sacos_ant  = paste0("Acum_Sacos70_", pa),
        acum_margen_ant = paste0("Acum_MargenFCC_", pa),
        acum_ppto_ss    = paste0("Acum_PptoSS_", p),
        acum_sacos_act  = paste0("Acum_Sacos70_", p),
        acum_ppto_ma    = paste0("Acum_PptoMa_", p),
        acum_margen_act = paste0("Acum_MargenFCC_", p),
        cumpl_ss_acum   = paste0("CumplSS_Acum_", p),
        cumpl_mgn_acum  = paste0("CumplMgn_Acum_", p),
        varss_acum      = paste0("VarSS_Acum_", p),
        varmgn_acum     = paste0("VarMgn_Acum_", p)
      )
    })
    
    # Breakdown por dimension seleccionada
    breakdown_r <- reactive({
      req(datos_dim_r(), ppto_dim_r(), !is.null(input$dim_breakdown))
      dim_cols <- if (isTRUE(input$dim_breakdown == "unidadcomercial")) {
        c("RazSoc", "LinNeg")
      } else {
        .dim_col(input$dim_breakdown)
      }
      agregar_por_dimension(datos_dim_r(), ppto_dim_r(), dim_cols)
    })
    
    # Fila total para breakdown — calcula cumplimientos sobre la suma, no promedio
    .fila_total_breakdown <- function(df, cols_suma) {
      df %>%
        summarise(across(all_of(cols_suma), ~ sum(.x, na.rm = TRUE))) %>%
        mutate(
          Cumpl_Sacos_YTD  = .division_segura(Sacos_YTD  / Ppto_Sacos_YTD),
          Cumpl_Margen_YTD = .division_segura(Margen_YTD / Ppto_Margen_YTD),
          Cumpl_Sacos_Mes  = .division_segura(Sacos_Mes  / Ppto_Sacos_Mes),
          Cumpl_Margen_Mes = .division_segura(Margen_Mes / Ppto_Margen_Mes),
          Prob_Sacos       = .division_segura(Proyeccion_Sacos  / Ppto_Sacos_Anual),
          Prob_Margen      = .division_segura(Proyeccion_Margen / Ppto_Margen_Anual),
          Sem_Sacos        = purrr::map_chr(Prob_Sacos,  ~ .semaforo(.x, "emoji")),
          Sem_Margen       = purrr::map_chr(Prob_Margen, ~ .semaforo(.x, "emoji")),
          .dim_key         = "TOTAL"
        )
    }
    
    # Columnas numericas que se suman en la fila total
    .COLS_SUMA <- c(
      "Sacos_Mes", "Ppto_Sacos_Mes", "Sacos_YTD", "Ppto_Sacos_YTD",
      "Brecha_Sacos", "Proyeccion_Sacos", "Meta_Mensual_Sacos",
      "Margen_Mes", "Ppto_Margen_Mes", "Margen_YTD", "Ppto_Margen_YTD",
      "Brecha_Margen", "Proyeccion_Margen", "Meta_Mensual_Margen",
      "Ppto_Sacos_Anual", "Ppto_Margen_Anual"
    )
    
    # Tabla de breakdown completa (sacos + margen) con fila total
    breakdown_tabla_r <- reactive({
      req(breakdown_r(), !is.null(input$dim_breakdown))
      df    <- breakdown_r()
      es_uc <- isTRUE(input$dim_breakdown == "unidadcomercial")
      
      fila_total <- .fila_total_breakdown(df, .COLS_SUMA)
      
      df <- df %>%
        mutate(
          Sem_Sacos  = purrr::map_chr(Prob_Sacos,  ~ .semaforo(.x, "emoji")),
          Sem_Margen = purrr::map_chr(Prob_Margen, ~ .semaforo(.x, "emoji"))
        )
      
      if (es_uc) {
        df <- df %>% mutate(.dim_key = paste0(RazSoc, "||", LinNeg))
        fila_total <- fila_total %>% mutate(RazSoc = "TOTAL", LinNeg = "")
        bind_rows(df, fila_total)
      } else {
        df <- df %>% mutate(.dim_key = dim)
        fila_total <- fila_total %>% mutate(.dim_key = "TOTAL")
        bind_rows(df, fila_total)
      }
    })
    
    # Reactivo de tabla de sacos — subset de columnas para tabla superior
    breakdown_tabla_sacos_r <- reactive({
      req(breakdown_tabla_r())
      df    <- breakdown_tabla_r()
      es_uc <- isTRUE(input$dim_breakdown == "unidadcomercial")
      cols_dim <- if (es_uc) c(".dim_key", "RazSoc", "LinNeg") else ".dim_key"
      df %>%
        select(
          all_of(cols_dim),
          Sacos_Mes, Ppto_Sacos_Mes, Cumpl_Sacos_Mes,
          Sacos_YTD, Ppto_Sacos_YTD, Cumpl_Sacos_YTD, Brecha_Sacos,
          Proyeccion_Sacos, Prob_Sacos, Sem_Sacos, Meta_Mensual_Sacos
        ) %>%
        mutate(.row_type = ifelse(.dim_key == "TOTAL", "total", ""))
    })
    
    # Reactivo de tabla de margen — subset de columnas para tabla inferior
    breakdown_tabla_margen_r <- reactive({
      req(breakdown_tabla_r())
      df    <- breakdown_tabla_r()
      es_uc <- isTRUE(input$dim_breakdown == "unidadcomercial")
      cols_dim <- if (es_uc) c(".dim_key", "RazSoc", "LinNeg") else ".dim_key"
      df %>%
        select(
          all_of(cols_dim),
          Margen_Mes, Ppto_Margen_Mes, Cumpl_Margen_Mes,
          Margen_YTD, Ppto_Margen_YTD, Cumpl_Margen_YTD, Brecha_Margen,
          Proyeccion_Margen, Prob_Margen, Sem_Margen, Meta_Mensual_Margen
        ) %>%
        mutate(.row_type = ifelse(.dim_key == "TOTAL", "total", ""))
    })
    
    # KPIs derivados de datos_grafico_r
    kpi_sacos_r <- reactive({
      cal  <- calendario_r()
      dg   <- datos_grafico_r() %>% filter(Mes_Num <= cal$mes_actual)
      ejec <- sum(dg$Sacos_Real,  na.rm = TRUE)
      ppto <- sum(dg$Sacos_Ppto,  na.rm = TRUE)
      list(
        ejec = ejec, ppto = ppto,
        cumpl = .division_segura(ejec / ppto),
        periodo = periodo_r(), mes_actual = cal$mes_actual
      )
    })
    kpi_margen_r <- reactive({
      cal  <- calendario_r()
      dg   <- datos_grafico_r() %>% filter(Mes_Num <= cal$mes_actual)
      ejec <- sum(dg$Margen_Real, na.rm = TRUE)
      ppto <- sum(dg$Margen_Ppto, na.rm = TRUE)
      list(
        ejec = ejec, ppto = ppto,
        cumpl = .division_segura(ejec / ppto),
        periodo = periodo_r(), mes_actual = cal$mes_actual
      )
    })
    kpi_ritmo_sacos_r <- reactive({
      cal        <- calendario_r()
      dg         <- datos_grafico_r()
      ejec_ytd   <- dg %>%
        filter(Mes_Num <= cal$mes_actual) %>%
        summarise(s = sum(Sacos_Real, na.rm = TRUE)) %>%
        pull(s)
      ppto_anual <- dg %>%
        summarise(p = sum(Sacos_Ppto, na.rm = TRUE)) %>%
        pull(p)
      faltante   <- ppto_anual - ejec_ytd
      list(
        ritmo      = .division_segura(faltante / pmax(cal$meses_rest, 1)),
        faltante   = faltante,
        meses_rest = cal$meses_rest
      )
    })
    kpi_ritmo_margen_r <- reactive({
      cal        <- calendario_r()
      dg         <- datos_grafico_r()
      ejec_ytd   <- dg %>%
        filter(Mes_Num <= cal$mes_actual) %>%
        summarise(m = sum(Margen_Real, na.rm = TRUE)) %>%
        pull(m)
      ppto_anual <- dg %>%
        summarise(p = sum(Margen_Ppto, na.rm = TRUE)) %>%
        pull(p)
      faltante   <- ppto_anual - ejec_ytd
      list(
        ritmo      = .division_segura(faltante / pmax(cal$meses_rest, 1)),
        faltante   = faltante,
        meses_rest = cal$meses_rest
      )
    })
    
    # Helpers de colDef para las tablas breakdown ----
    
    # ColDef de columna de dimension segun dim activo
    .col_dim_def <- function(dim) {
      es_uc <- isTRUE(dim == "unidadcomercial")
      if (es_uc) list(
        .dim_key = reactable::colDef(show = FALSE),
        RazSoc   = reactable::colDef(
          name = "Razón Social", minWidth = 200, sticky = "left"
        ),
        LinNeg   = reactable::colDef(
          name = "Línea de Negocio", minWidth = 150, sticky = "left"
        )
      ) else list(
        .dim_key = reactable::colDef(
          name = .dim_lbl(dim), minWidth = 180, sticky = "left"
        )
      )
    }
    
    # Columnas de sacos — sin margen
    .cols_sacos <- function() {
      list(
        Sacos_Mes = reactable::colDef(
          name   = "Eje. Mes", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Ppto_Sacos_Mes = reactable::colDef(
          name   = "Ppto Mes", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Cumpl_Sacos_Mes = reactable::colDef(
          name   = "% Mes", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "600")
        ),
        Sacos_YTD = reactable::colDef(
          name   = "Eje. Acum", minWidth = 100,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Ppto_Sacos_YTD = reactable::colDef(
          name   = "Ppto Acum", minWidth = 100,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Cumpl_Sacos_YTD = reactable::colDef(
          name   = "% Acum", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "700")
        ),
        Brecha_Sacos = reactable::colDef(
          name   = "Brecha", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0),
          style  = function(v) list(
            color = if (!is.na(v) && v < 0) .CR else .CV, fontWeight = "600"
          )
        ),
        Proyeccion_Sacos = reactable::colDef(
          name   = "Proy. Cierre", minWidth = 100,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Prob_Sacos = reactable::colDef(
          name   = "Verosimilitud", minWidth = 90,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(color = .semaforo(v, "texto_na"), fontWeight = "700")
        ),
        Sem_Sacos          = reactable::colDef(name = "", minWidth = 35),
        Meta_Mensual_Sacos = reactable::colDef(
          name   = "Meta Mens.", minWidth = 90,
          format = reactable::colFormat(separators = TRUE, digits = 0)
        )
      )
    }
    
    # Columnas de margen — sin sacos
    .cols_margen <- function() {
      list(
        Margen_Mes = reactable::colDef(
          name   = "Eje. Mes", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Ppto_Margen_Mes = reactable::colDef(
          name   = "Ppto Mes", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Cumpl_Margen_Mes = reactable::colDef(
          name   = "% Mes", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "600")
        ),
        Margen_YTD = reactable::colDef(
          name   = "Eje. Acum", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Ppto_Margen_YTD = reactable::colDef(
          name   = "Ppto Acum", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Cumpl_Margen_YTD = reactable::colDef(
          name   = "% Acum", minWidth = 70,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(background = .semaforo(v, "fondo_na"), fontWeight = "700")
        ),
        Brecha_Margen = reactable::colDef(
          name   = "Brecha", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0),
          style  = function(v) list(
            color = if (!is.na(v) && v < 0) .CR else .CV, fontWeight = "600"
          )
        ),
        Proyeccion_Margen = reactable::colDef(
          name   = "Proy. Cierre", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        ),
        Prob_Margen = reactable::colDef(
          name   = "Verosimilitud", minWidth = 90,
          format = reactable::colFormat(percent = TRUE, digits = 1),
          style  = function(v) list(color = .semaforo(v, "texto_na"), fontWeight = "700")
        ),
        Sem_Margen          = reactable::colDef(name = "", minWidth = 35),
        Meta_Mensual_Margen = reactable::colDef(
          name   = "Meta Mens.", minWidth = 110,
          format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
        )
      )
    }
    
    # Contadores de version — uno por tabla para evitar race conditions al cambiar dimension
    .breakdown_ver_sacos  <- reactiveVal(1L)
    .breakdown_ver_margen <- reactiveVal(1L)
    .ns_local             <- session$ns
    
    # Incrementa ambas versiones al cambiar dimension
    observeEvent(input$dim_breakdown, {
      .breakdown_ver_sacos(.breakdown_ver_sacos()   + 1L)
      .breakdown_ver_margen(.breakdown_ver_margen() + 1L)
    }, ignoreInit = FALSE)
    
    # Auditoría presupuesto ----
    observeEvent(list(clientes_presupuesto_r(), filtros_r()), {
      clientes <- if (shiny::is.reactive(clientes_raw)) clientes_raw() else clientes_raw
      req(is.data.frame(clientes))
      
      crm_base <- clientes %>%
        mutate(FecProceso = as.Date(FecProceso),
               CLCliNit = suppressWarnings(as.numeric(CLCliNit)),
               LinNegCod = ifelse(as.numeric(LinNegCod) == 20000, 21000, as.numeric(LinNegCod)),
               SSPpto = suppressWarnings(as.numeric(SSPpto)),
               MNFCCPpto = suppressWarnings(as.numeric(MNFCCPpto))) %>%
        filter(!is.na(FecProceso), lubridate::year(FecProceso) == lubridate::year(Sys.Date()),
               LinNegCod %in% c(10000, 21000), !is.na(CLCliNit)) %>%
        group_by(CLCliNit, LinNegCod) %>%
        filter(FecProceso == max(FecProceso, na.rm = TRUE)) %>%
        summarise(Asesor = dplyr::first(Asesor),
                  SSPpto = if (all(is.na(SSPpto))) 0 else max(SSPpto, na.rm = TRUE),
                  MNFCCPpto = if (all(is.na(MNFCCPpto))) 0 else max(MNFCCPpto, na.rm = TRUE),
                  ValoresSacos = dplyr::n_distinct(SSPpto, na.rm = TRUE),
                  ValoresMargen = dplyr::n_distinct(MNFCCPpto, na.rm = TRUE),
                  .groups = "drop")
      
      crm_industria <- crm_base %>% filter(!is.na(Asesor), Asesor != "TOMÁS RESTREPO")
      crm_modulo <- clientes_presupuesto_r()
      
      .resumen_auditoria <- function(df) {
        df %>% group_by(LinNegCod) %>%
          summarise(Cuentas = dplyr::n_distinct(CLCliNit),
                    Sacos = sum(SSPpto, na.rm = TRUE),
                    Margen = sum(MNFCCPpto, na.rm = TRUE), .groups = "drop")
      }
      
      excluidos <- crm_industria %>%
        anti_join(crm_modulo %>% distinct(CLCliNit, LinNegCod),
                  by = c("CLCliNit", "LinNegCod")) %>%
        filter(SSPpto != 0 | MNFCCPpto != 0) %>%
        arrange(LinNegCod, desc(SSPpto))
      
      referencia <- tibble::tibble(LinNegCod = c(10000, 21000),
                                   SacosExcel = c(69324.285714, 9996),
                                   MargenExcel = c(2048576000, 2219532000))
      conciliacion <- .resumen_auditoria(crm_modulo) %>%
        right_join(referencia, by = "LinNegCod") %>%
        mutate(DifSacos = Sacos - SacosExcel, DifMargen = Margen - MargenExcel)
      
      cat("\n========== AUDITORÍA PRESUPUESTO CRM ==========")
      cat("\nFecha: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sep = "")
      cat("\nFiltros aplicados:\n")
      print(filtros_r())
      cat("\nCRM completo (incluye otras unidades):\n")
      print(.resumen_auditoria(crm_base), n = Inf)
      cat("\nCRM Industria Nacional, sin filtros comerciales:\n")
      print(.resumen_auditoria(crm_industria), n = Inf)
      cat("\nCRM utilizado por Presupuesto.R:\n")
      print(.resumen_auditoria(crm_modulo), n = Inf)
      cat("\nComparación contra Excel (sin filtros restrictivos):\n")
      print(conciliacion, n = Inf)
      cat("\nCuentas con presupuesto descartadas por cruces o filtros:\n")
      print(excluidos, n = Inf, width = Inf)
      cat("\nContradicciones por NIT y línea en última fecha:\n")
      print(crm_base %>% filter(ValoresSacos > 1L | ValoresMargen > 1L), n = Inf)
      cat("\n================================================\n")
    }, ignoreInit = FALSE)
    
    # Auditoría ejecución financiera ----
    observeEvent(list(datos_centrales_r(), filtros_r()), {
      fuente <- pyg_referencia_r()
      cierre <- cierre_pyg_r()
      if (is.null(fuente) || is.na(cierre)) return(invisible(NULL))
      f <- filtros_r()
      base <- datos_centrales_r() %>%
        filter(lubridate::year(FecMes) == periodo_r(), FecMes <= cierre) %>%
        group_by(FecMes, LinNegCod) %>%
        summarise(SacosModulo = sum(SacosFact, na.rm = TRUE),
                  MargenModulo = sum(Margen, na.rm = TRUE), .groups = "drop")
      referencia <- fuente %>%
        filter(lubridate::year(FecMes) == periodo_r(), FecMes <= cierre)
      if (!is.null(f) && length(f$linneg) > 0L) {
        codigos <- ifelse("CONVENCIONALES" %in% f$linneg, 10000, numeric(0))
        if (any(f$linneg %in% c("A LA MEDIDA", "DIFERENCIADOS"))) codigos <- c(codigos, 21000)
        referencia <- referencia %>% filter(LinNegCod %in% codigos)
      }
      referencia <- referencia %>%
        group_by(FecMes, LinNegCod) %>%
        summarise(SacosPYG = sum(Sacos70, na.rm = TRUE),
                  MargenPYG = sum(MargenFCC, na.rm = TRUE), .groups = "drop")
      control <- full_join(referencia, base, by = c("FecMes", "LinNegCod")) %>%
        mutate(across(c(SacosPYG, MargenPYG, SacosModulo, MargenModulo), ~ coalesce(.x, 0)),
               DifSacos = SacosModulo - SacosPYG,
               DifMargen = MargenModulo - MargenPYG) %>%
        arrange(LinNegCod, FecMes)
      cat("\n========== CONCILIACIÓN MENSUAL DE EJECUCIÓN Y PyG ==========\n")
      print(control, n = Inf, width = Inf)
      cat("\nTotales por línea:\n")
      print(control %>% group_by(LinNegCod) %>%
              summarise(across(c(SacosPYG, MargenPYG, SacosModulo, MargenModulo,
                                 DifSacos, DifMargen), sum), .groups = "drop"), n = Inf)
      if (any(abs(control$DifSacos) > 0.1 | abs(control$DifMargen) > 1)) {
        cat("IMPORTANTE: Hay diferencias por filtros de población o dimensiones.\n")
      }
      cat("============================================================\n")
    }, ignoreInit = FALSE)
    
    # Auditoría de meses disponibles ----
    observeEvent(list(datos_centrales_r(), filtros_r()), {
      datos_originales <- .resolver_datos(dat) %>%
        mutate(FecMes = as.Date(FecMes),
               LinNegCod = ifelse(as.numeric(LinNegCod) == 20000, 21000, as.numeric(LinNegCod))) %>%
        filter(lubridate::year(FecMes) == periodo_r(),
               !is.na(SacosFact) | !is.na(Margen))
      f <- filtros_r()
      if (!is.null(f) && length(f$linneg) > 0L) {
        lineas <- numeric(0)
        if ("CONVENCIONALES" %in% f$linneg) lineas <- c(lineas, 10000)
        if (any(c("A LA MEDIDA", "DIFERENCIADOS") %in% f$linneg)) lineas <- c(lineas, 21000)
        datos_originales <- datos_originales %>% filter(LinNegCod %in% lineas)
      }
      sin_filtros <- datos_originales %>% group_by(FecMes, LinNegCod) %>%
        summarise(SacosOrigen = sum(SacosFact, na.rm = TRUE),
                  MargenOrigen = sum(Margen, na.rm = TRUE), .groups = "drop")
      con_filtros <- datos_centrales_r() %>%
        filter(lubridate::year(FecMes) == periodo_r(),
               !is.na(SacosFact) | !is.na(Margen)) %>%
        group_by(FecMes, LinNegCod) %>%
        summarise(SacosModulo = sum(SacosFact, na.rm = TRUE),
                  MargenModulo = sum(Margen, na.rm = TRUE), .groups = "drop")
      control <- full_join(sin_filtros, con_filtros, by = c("FecMes", "LinNegCod")) %>%
        mutate(across(c(SacosOrigen, MargenOrigen, SacosModulo, MargenModulo), ~ coalesce(.x, 0)),
               DifSacos = SacosModulo - SacosOrigen,
               DifMargen = MargenModulo - MargenOrigen) %>%
        arrange(FecMes, LinNegCod)
      cat("\n========== EJECUCIÓN DISPONIBLE: ORIGEN VS FILTROS ==========\n")
      print(control, n = Inf, width = Inf)
      cat("==============================================================\n")
    }, ignoreInit = FALSE)
    
    # Validación de reactivos ----
    .reactivos_requeridos <- c("periodo_r", "calendario_r", "datos_grafico_r", "ppto_r",
                               "kpi_sacos_r", "kpi_margen_r", "kpi_ritmo_sacos_r", "kpi_ritmo_margen_r")
    .reactivos_faltantes <- .reactivos_requeridos[
      !vapply(.reactivos_requeridos, exists, logical(1), envir = environment(), inherits = FALSE)
    ]
    if (length(.reactivos_faltantes)) {
      stop("Presupuesto.R - reactivos no registrados: ", paste(.reactivos_faltantes, collapse = ", "))
    }
    
    # Outputs ----
    
    ## KPIs CajaModal ----
    racafeModulos::CajaModal(
      id = "kpi_cumpl_sacos_ytd", valor = reactive(kpi_sacos_r()$cumpl),
      formato = "porcentaje",
      texto   = reactive(paste0("Cumpl. Sacos \u2014 Acum. ", kpi_sacos_r()$periodo)),
      icono   = "check-double", colores = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.semaforo(kpi_sacos_r()$cumpl, "fondo")),
      mostrar_boton   = FALSE,
      footer = reactive(
        paste0(
          FormatearNumero(kpi_sacos_r()$ejec, "coma"), " sacos facturados de ",
          FormatearNumero(kpi_sacos_r()$ppto, "coma"), " presupuestados"
        ) %>% HTML
      )
    )
    racafeModulos::CajaModal(
      id = "kpi_cumpl_margen_ytd", valor = reactive(kpi_margen_r()$cumpl),
      formato = "porcentaje",
      texto   = reactive(paste0("Cumpl. Margen \u2014 Acum. ", kpi_margen_r()$periodo)),
      icono   = "dollar-sign", colores = reactive(c(fondo = "white")),
      color_fondo_hex = reactive(.semaforo(kpi_margen_r()$cumpl, "fondo")),
      mostrar_boton   = FALSE,
      footer = reactive(
        paste0(
          FormatearNumero(kpi_margen_r()$ejec, "dinero"), " facturado de ",
          FormatearNumero(kpi_margen_r()$ppto, "dinero"), " presupuestado"
        ) %>% HTML
      )
    )
    racafeModulos::CajaModal(
      id = "kpi_ritmo_req_sacos", valor = reactive(kpi_ritmo_sacos_r()$ritmo),
      formato = "numero", texto = "Sacos / mes requeridos", icono = "arrow-trend-up",
      colores = reactive(c(fondo = "white")), color_fondo_hex = "#F8FAFC",
      mostrar_boton = FALSE,
      footer = reactive(
        paste0(
          "Faltan ", FormatearNumero(kpi_ritmo_sacos_r()$faltante, "coma"),
          " sacos en ", kpi_ritmo_sacos_r()$meses_rest,
          " meses para cerrar el presupuesto"
        ) %>% HTML
      )
    )
    racafeModulos::CajaModal(
      id = "kpi_ritmo_req_margen", valor = reactive(kpi_ritmo_margen_r()$ritmo),
      formato = "dinero", texto = "Margen / mes requerido", icono = "money-bill-trend-up",
      colores = reactive(c(fondo = "white")), color_fondo_hex = "#F8FAFC",
      mostrar_boton = FALSE,
      footer = reactive(
        paste0(
          "Faltan ", FormatearNumero(kpi_ritmo_margen_r()$faltante, "dinero"),
          " en ", kpi_ritmo_margen_r()$meses_rest,
          " meses para cerrar el presupuesto de margen"
        ) %>% HTML
      )
    )
    
    ## Tabla GT mensual/acumulada ----
    output$Presupuesto <- render_gt({
      waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
      periodo     <- periodo_r()
      periodo_ant <- periodo - 1L
      cn          <- col_names_r()
      
      datos_ant <- datos_base_r() %>% filter(Periodo == periodo_ant)
      datos_act <- datos_act_r()  %>% filter(Periodo == periodo)
      ppto      <- ppto_r()
      
      ventas_mes <- bind_rows(datos_ant, datos_act) %>%
        group_by(Periodo, Fecha) %>%
        summarise(Sacos70 = sum(Sacos70, na.rm = TRUE),
                  MargenFCC = sum(MargenFCC, na.rm = TRUE), .groups = "drop")
      ppto_mes <- ppto %>%
        group_by(Periodo, Fecha) %>%
        summarise(PptoSS = sum(PptoSS, na.rm = TRUE),
                  PptoMa = sum(PptoMa, na.rm = TRUE), .groups = "drop")
      t1 <- tidyr::crossing(
        Periodo = c(periodo_ant, periodo), Fecha = .MESES
      ) %>%
        left_join(ventas_mes, by = c("Periodo", "Fecha")) %>%
        left_join(ppto_mes, by = c("Periodo", "Fecha")) %>%
        pivot_wider(names_from = Periodo, values_from = c(Sacos70, MargenFCC, PptoSS, PptoMa)) %>%
        mutate(
          Fecha = factor(Fecha, levels = .MESES, ordered = TRUE),
          across(starts_with(c("Sacos70_", "MargenFCC_")), ~ .x),
          !!cn$cumpl_sacos  := !!sym(cn$sacos_act)  / !!sym(cn$ppto_sacos),
          VarSS              = Variacion(!!sym(cn$sacos_ant),  !!sym(cn$sacos_act)),
          !!cn$cumpl_margen := !!sym(cn$margen_act) / !!sym(cn$ppto_margen),
          VarMgn             = Variacion(!!sym(cn$margen_ant), !!sym(cn$margen_act))
        ) %>%
        arrange(Fecha) %>%
        mutate(
          !!cn$acum_sacos_ant  := ifelse(
            !is.na(!!sym(cn$sacos_ant)),
            cumsum(coalesce(!!sym(cn$sacos_ant),  0)), NA
          ),
          !!cn$acum_margen_ant := ifelse(
            !is.na(!!sym(cn$margen_ant)),
            cumsum(coalesce(!!sym(cn$margen_ant), 0)), NA
          ),
          !!cn$acum_ppto_ss    := ifelse(
            !is.na(!!sym(cn$ppto_sacos)),
            cumsum(coalesce(!!sym(cn$ppto_sacos), 0)), NA
          ),
          !!cn$acum_ppto_ma    := ifelse(
            !is.na(!!sym(cn$ppto_margen)),
            cumsum(coalesce(!!sym(cn$ppto_margen), 0)), NA
          ),
          !!cn$acum_sacos_act  := ifelse(
            !is.na(!!sym(cn$sacos_act)),
            cumsum(coalesce(!!sym(cn$sacos_act),  0)), NA
          ),
          !!cn$acum_margen_act := ifelse(
            !is.na(!!sym(cn$margen_act)),
            cumsum(coalesce(!!sym(cn$margen_act), 0)), NA
          ),
          !!cn$cumpl_ss_acum   := ifelse(
            !is.na(!!sym(cn$acum_sacos_act)) &
              !is.na(!!sym(cn$acum_ppto_ss)) & !!sym(cn$acum_ppto_ss) != 0,
            !!sym(cn$acum_sacos_act) / !!sym(cn$acum_ppto_ss), NA
          ),
          !!cn$cumpl_mgn_acum  := ifelse(
            !is.na(!!sym(cn$acum_margen_act)) &
              !is.na(!!sym(cn$acum_ppto_ma)) & !!sym(cn$acum_ppto_ma) != 0,
            !!sym(cn$acum_margen_act) / !!sym(cn$acum_ppto_ma), NA
          ),
          !!cn$varss_acum      := ifelse(
            !is.na(!!sym(cn$acum_sacos_ant)) & !is.na(!!sym(cn$acum_sacos_act)),
            Variacion(!!sym(cn$acum_sacos_ant), !!sym(cn$acum_sacos_act)), NA
          ),
          !!cn$varmgn_acum     := ifelse(
            !is.na(!!sym(cn$acum_margen_ant)) & !is.na(!!sym(cn$acum_margen_act)),
            Variacion(!!sym(cn$acum_margen_ant), !!sym(cn$acum_margen_act)), NA
          )
        ) %>%
        select(
          Fecha,
          !!sym(cn$sacos_ant),  !!sym(cn$acum_sacos_ant),
          !!sym(cn$margen_ant), !!sym(cn$acum_margen_ant),
          !!sym(cn$ppto_sacos), !!sym(cn$sacos_act),
          !!sym(cn$cumpl_sacos), VarSS,
          !!sym(cn$acum_ppto_ss),  !!sym(cn$acum_sacos_act),
          !!sym(cn$cumpl_ss_acum), !!sym(cn$varss_acum),
          !!sym(cn$ppto_margen), !!sym(cn$margen_act),
          !!sym(cn$cumpl_margen), VarMgn,
          !!sym(cn$acum_ppto_ma),   !!sym(cn$acum_margen_act),
          !!sym(cn$cumpl_mgn_acum), !!sym(cn$varmgn_acum)
        )
      
      fila_total <- t1 %>%
        summarise(
          !!sym(cn$sacos_ant)   := sum(!!sym(cn$sacos_ant),   na.rm = TRUE),
          !!sym(cn$margen_ant)  := sum(!!sym(cn$margen_ant),  na.rm = TRUE),
          !!sym(cn$ppto_sacos)  := sum(!!sym(cn$ppto_sacos),  na.rm = TRUE),
          !!sym(cn$sacos_act)   := sum(!!sym(cn$sacos_act),   na.rm = TRUE),
          !!sym(cn$ppto_margen) := sum(!!sym(cn$ppto_margen), na.rm = TRUE),
          !!sym(cn$margen_act)  := sum(!!sym(cn$margen_act),  na.rm = TRUE),
          across(c(
            !!sym(cn$acum_sacos_ant), !!sym(cn$acum_margen_ant),
            !!sym(cn$acum_ppto_ss),   !!sym(cn$acum_sacos_act),
            !!sym(cn$acum_ppto_ma),   !!sym(cn$acum_margen_act),
            !!sym(cn$cumpl_ss_acum),  !!sym(cn$varss_acum),
            !!sym(cn$cumpl_mgn_acum), !!sym(cn$varmgn_acum)
          ), ~ NA_real_)
        ) %>%
        mutate(
          Fecha = "TOTAL",
          !!cn$cumpl_sacos  := !!sym(cn$sacos_act)  / !!sym(cn$ppto_sacos),
          VarSS              = Variacion(!!sym(cn$sacos_ant),  !!sym(cn$sacos_act)),
          !!cn$cumpl_margen := !!sym(cn$margen_act) / !!sym(cn$ppto_margen),
          VarMgn             = Variacion(!!sym(cn$margen_ant), !!sym(cn$margen_act))
        )
      
      gt1 <- bind_rows(t1, fila_total) %>%
        gt() %>%
        tab_header(
          title    = md(paste0("**Seguimiento de Presupuesto ", periodo, "**")),
          subtitle = md(paste0(
            "Comparativo mensual y acumulado de sacos y margen FCC frente al presupuesto",
            " — ejecución disponible hasta: ", .MESES[calendario_r()$mes_actual],
            " — período anterior: ", periodo_ant
          ))
        ) %>%
        tab_spanner(label = as.character(periodo_ant), columns = 2:5) %>%
        tab_spanner(label = as.character(periodo),     columns = 6:21) %>%
        tab_spanner(label = "Sacos Mensual",    columns = 6:9) %>%
        tab_spanner(label = "Sacos Acumulado",  columns = 10:13) %>%
        tab_spanner(label = "$MNFCC Mensual",   columns = 14:17) %>%
        tab_spanner(label = "$MNFCC Acumulado", columns = 18:21) %>%
        cols_label(
          Fecha = "",
          .list = setNames(
            c(
              "", "Sacos", "Sacos Acum.", "Margen", "Margen Acum.",
              rep(c("Presupuesto", "Ejecutado", "Cumplimiento", "Comp. Per. Ant."), 4)
            ),
            c(
              "Fecha",
              cn$sacos_ant, cn$acum_sacos_ant, cn$margen_ant, cn$acum_margen_ant,
              cn$ppto_sacos, cn$sacos_act, cn$cumpl_sacos, "VarSS",
              cn$acum_ppto_ss, cn$acum_sacos_act, cn$cumpl_ss_acum, cn$varss_acum,
              cn$ppto_margen, cn$margen_act, cn$cumpl_margen, "VarMgn",
              cn$acum_ppto_ma, cn$acum_margen_act, cn$cumpl_mgn_acum, cn$varmgn_acum
            )
          )
        ) %>%
        fmt_number(columns   = c(2, 3, 6, 7, 10, 11), decimals = 0) %>%
        fmt_currency(columns = c(4, 5, 14, 15, 18, 19), currency = "COP", decimals = 0) %>%
        fmt_percent(columns  = c(8, 9, 12, 13, 16, 17, 20, 21), decimals = 1) %>%
        gt_minimal_style() %>%
        gt::tab_options(
          table.width = gt::pct(100), container.width = gt::pct(100),
          table.font.size = gt::px(11), column_labels.font.size = gt::px(10),
          data_row.padding = gt::px(5)
        ) %>%
        gt::tab_style(
          style = gt::cell_text(color = "#1F2937"),
          locations = gt::cells_body(columns = dplyr::everything())
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#F0FDF4"),
          locations = gt::cells_body(columns = dplyr::all_of(c(
            cn$sacos_ant, cn$acum_sacos_ant, cn$margen_ant, cn$acum_margen_ant
          )))
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#F8FAFC"),
          locations = gt::cells_body(columns = dplyr::all_of(c(
            cn$ppto_sacos, cn$sacos_act, cn$cumpl_sacos, "VarSS",
            cn$ppto_margen, cn$margen_act, cn$cumpl_margen, "VarMgn"
          )))
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#EFF6FF"),
          locations = gt::cells_body(columns = dplyr::all_of(c(
            cn$acum_ppto_ss, cn$acum_sacos_act, cn$cumpl_ss_acum, cn$varss_acum,
            cn$acum_ppto_ma, cn$acum_margen_act, cn$cumpl_mgn_acum, cn$varmgn_acum
          )))
        ) %>%
        tab_source_note(
          source_note = htmltools::tags$div(
            style = "margin-top:8px; font-size:12px; text-align:left;",
            tags$span(style = paste0(
              "display:inline-block;width:15px;height:15px;",
              "background-color:#F0FDF4;border:1px solid #ccc;",
              "margin-right:6px;vertical-align:middle;"
            )),
            tags$span(
              paste0("Año Anterior (", periodo_ant, ")"),
              style = "font-weight:bold;font-size:12px;"
            ),
            tags$span(style = paste0(
              "display:inline-block;width:15px;height:15px;",
              "background-color:#F8FAFC;border:1px solid #ccc;",
              "margin-left:18px;margin-right:6px;vertical-align:middle;"
            )),
            tags$span(
              paste0("Mensual (", periodo, ")"),
              style = "font-weight:bold;font-size:12px;"
            ),
            tags$span(style = paste0(
              "display:inline-block;width:15px;height:15px;",
              "background-color:#EFF6FF;border:1px solid #ccc;",
              "margin-left:18px;margin-right:6px;vertical-align:middle;"
            )),
            tags$span(
              paste0("Acumulado (", periodo, ")"),
              style = "font-weight:bold;font-size:12px;"
            )
          )
        ) %>%
        tab_source_note(
          source_note = htmltools::tags$div(
            style = paste0(
              "margin-top:10px;padding:12px 14px;background:#F8FAFC;",
              "border-left:3px solid #94A3B8;border-radius:4px;font-size:11px;",
              "color:#374151;line-height:1.8;"
            ),
            tags$span(HTML(paste0(
              "<b>Cumplimiento:</b> ",
              "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;",
              "border-radius:3px;'>Ejecutado \u00f7 Presupuesto</code>"
            ))),
            tags$br(),
            tags$span(HTML(paste0(
              "<b>Comparación con año anterior:</b> ",
              "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;",
              "border-radius:3px;'>(Actual \u2212 Anterior) \u00f7 |Anterior|</code>"
            )))
          )
        ) %>%
        sub_missing(columns = everything(), rows = everything(), missing_text = "") %>%
        tab_style(
          style     = cell_text(weight = "bold"),
          locations = cells_body(rows = Fecha == "TOTAL")
        ) %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_sacos))    %>%
        gt_var_style_local("VarSS") %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_ss_acum))  %>%
        gt_var_style_local(cn$varss_acum) %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_margen))   %>%
        gt_var_style_local("VarMgn") %>%
        gt_pct_style_semaforo(!!sym(cn$cumpl_mgn_acum)) %>%
        gt_var_style_local(cn$varmgn_acum)
      
      waiter_hide()
      return(gt1)
    })
    
    ## Titulo dinamico sub-card proyeccion ----
    output$titulo_proyeccion <- renderUI({
      paste0("Proyección de cierre por dimensión \u2014 ", periodo_r())
    })
    
    ## Graficos de serie temporal ----
    output$GraficoPresupuestoSacos <- renderPlotly({
      construir_grafico_serie(
        datos_grafico_r(), input$vista_acumulada,
        "Sacos_Real", "Sacos_Ppto", "Sacos70",
        "Ejecución vs Presupuesto \u2014 Sacos (Acumulado)",
        "Ejecución vs Presupuesto \u2014 Sacos (Mensual)",
        "Sacos Acumulados", "Sacos Mensuales", "numero"
      )
    })
    output$GraficoPresupuestoMargen <- renderPlotly({
      construir_grafico_serie(
        datos_grafico_r(), input$vista_acumulada,
        "Margen_Real", "Margen_Ppto", "MargenFCC",
        "Ejecución vs Presupuesto \u2014 Margen (Acumulado)",
        "Ejecución vs Presupuesto \u2014 Margen (Mensual)",
        "Margen Acumulado", "Margen Mensual", "dinero"
      )
    })
    
    ## UI dinamica: dos tablas breakdown (Sacos arriba, Margen abajo) ----
    output$tabla_sacos_ui <- renderUI({
      ver <- .breakdown_ver_sacos()
      racafeModulos::TablaReactableUI(
        .ns_local(paste0("tabla_ss_", ver)),
        estilo    = "minimal2",
        titulo    = "Sacos (70 Kgs)",
        subtitulo = NULL,
        sortable  = TRUE
      )
    })
    
    output$tabla_margen_ui <- renderUI({
      ver <- .breakdown_ver_margen()
      racafeModulos::TablaReactableUI(
        .ns_local(paste0("tabla_ma_", ver)),
        estilo    = "minimal2",
        titulo    = "Margen ($MNFCC)",
        subtitulo = NULL,
        sortable  = TRUE
      )
    })
    
    # Server tabla de Sacos — sin modal (navegacion visual solamente)
    observeEvent(.breakdown_ver_sacos(), {
      ver <- .breakdown_ver_sacos()
      dim <- isolate(input$dim_breakdown)
      req(!is.null(dim), nzchar(dim))
      mid        <- paste0("tabla_ss_", ver)
      cols_todas <- c(.col_dim_def(dim), .cols_sacos())
      
      racafeModulos::TablaReactable(
        id             = mid,
        data           = breakdown_tabla_sacos_r,
        id_col         = ".dim_key",
        modo_seleccion = "ninguno",
        sortable       = TRUE,
        searchable     = TRUE,
        page_size      = 20,
        compact        = TRUE,
        mostrar_badge  = FALSE,
        mostrar_nota   = FALSE,
        col_specs      = cols_todas,
        columnas       = names(cols_todas)
      )
    }, ignoreInit = FALSE)
    
    # Server tabla de Margen — mantiene modal con detalle de lotes
    observeEvent(.breakdown_ver_margen(), {
      ver <- .breakdown_ver_margen()
      dim <- isolate(input$dim_breakdown)
      req(!is.null(dim), nzchar(dim))
      mid        <- paste0("tabla_ma_", ver)
      cols_todas <- c(.col_dim_def(dim), .cols_margen())
      
      racafeModulos::TablaReactable(
        id             = mid,
        data           = breakdown_tabla_margen_r,
        id_col         = ".dim_key",
        modo_seleccion = "fila",
        sortable       = TRUE,
        searchable     = TRUE,
        page_size      = 20,
        compact        = TRUE,
        mostrar_badge  = FALSE,
        mostrar_nota   = FALSE,
        col_specs      = cols_todas,
        columnas       = names(cols_todas),
        modal_titulo_fn = function(sel) {
          titulo_dim <- gsub("\\|\\|", " \u2014 ", sel$id, fixed = FALSE)
          paste0(
            "Lotes facturados \u2014 ", titulo_dim,
            " \u00b7 ", isolate(periodo_r())
          )
        },
        modal_contenido_fn = function(sel) {
          dim_val <- sel$id
          dim_sel <- isolate(input$dim_breakdown)
          lotes   <- isolate(lotes_dim_r())
          es_uc   <- isTRUE(dim_sel == "unidadcomercial")
          
          # Filtrado de lotes segun dimension y valor seleccionado
          lotes_fil <- if (dim_val == "TOTAL") {
            lotes
          } else if (es_uc) {
            uc_val <- gsub("\\|\\|", " \u2014 ", dim_val, fixed = FALSE)
            lotes %>% filter(UnidadComercial == uc_val)
          } else {
            col_dim <- switch(dim_sel,
                              linneg       = "CLLinNegNo",
                              segmento     = "Segmento",
                              asesor       = "Asesor",
                              departamento = "Departamento",
                              municipio    = "Municipio"
            )
            lotes %>% filter(.data[[col_dim]] == dim_val)
          }
          
          # Agrupacion de lotes para tabla del modal
          df_modal <- lotes_fil %>%
            group_by(FecFact, PerRazSoc, CLLinNegNo, CLLotCod, Asesor, Segmento) %>%
            summarise(
              Sacos70 = sum(Sacos70, na.rm = TRUE),
              Margen  = sum(Margen,  na.rm = TRUE),
              .groups = "drop"
            ) %>%
            arrange(desc(FecFact))
          
          if (nrow(df_modal) == 0) {
            return(p(
              "Sin lotes registrados para esta dimensión en el periodo.",
              style = "color:#888; margin-top:12px;"
            ))
          }
          
          # Tabla reactable de detalle de lotes en modal
          reactable::reactable(
            data            = df_modal,
            sortable        = TRUE,
            searchable      = TRUE,
            defaultPageSize = 15,
            compact         = TRUE,
            bordered        = TRUE,
            highlight       = TRUE,
            language = reactable::reactableLang(
              searchPlaceholder = "Buscar...",
              noData            = "Sin resultados",
              pageInfo          = "{rowStart}\u2013{rowEnd} de {rows} registros",
              pagePrevious      = "Anterior",
              pageNext          = "Siguiente"
            ),
            theme = reactable::reactableTheme(
              headerStyle = list(fontWeight = "600", fontSize = "12px"),
              cellStyle   = list(fontSize = "12px", padding = "3px 6px")
            ),
            columns = list(
              FecFact    = reactable::colDef(
                name = "Fecha Factura", minWidth = 110,
                cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %Y")
              ),
              PerRazSoc  = reactable::colDef(name = "Razón Social",  minWidth = 180),
              CLLinNegNo = reactable::colDef(name = "Línea de Negocio", minWidth = 130),
              CLLotCod   = reactable::colDef(name = "Lote",          minWidth = 80),
              Asesor     = reactable::colDef(name = "Asesor",        minWidth = 120),
              Segmento   = reactable::colDef(name = "Segmento",      minWidth = 100),
              Sacos70    = reactable::colDef(
                name   = "Sacos (70Kg)", minWidth = 100,
                format = reactable::colFormat(separators = TRUE, digits = 1)
              ),
              Margen     = reactable::colDef(
                name   = "Margen ($)", minWidth = 110,
                format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 0)
              )
            )
          )
        },
        modal_size = "xl",
        modal_icon = "box-archive"
      )
    }, ignoreInit = FALSE)
    
    ## Footer metodologia ----
    output$footer_metodologia <- renderUI({
      htmltools::tags$div(
        style = paste0(
          "margin-top:4px;margin-bottom:16px;padding:12px 14px;background:#F8FAFC;",
          "border-left:3px solid #94A3B8;border-radius:4px;font-size:11px;",
          "color:#374151;line-height:1.8;"
        ),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Cumplimiento:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Ejecutado \u00f7 Presupuesto</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Brecha:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Ejecutado \u2212 Presupuesto acumulado al mes</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Proyección de cierre:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Ejecutado acumulado + (ritmo ponderado de los 3 últimos meses \u00d7 meses restantes)</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Verosimilitud:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "Proyección \u00f7 Presupuesto anual</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>Meta mensual:</b> ",
          "<code style='background:#f3f4f6;color:#374151;padding:1px 5px;border-radius:3px;'>",
          "(Presupuesto anual \u2212 Ejecutado acumulado) \u00f7 Meses restantes</code>"
        ))),
        htmltools::tags$br(),
        htmltools::tags$span(htmltools::HTML(paste0(
          "<b>TOTAL:</b> suma de todas las dimensiones del periodo. ",
          "Los indicadores se calculan sobre la fila y no como promedio de la columna. ",
          "Ejecución: PyG de meses cerrados; el presupuesto corresponde a Industria Nacional."
        )))
      )
    })
    
    ## Grafico cuadrantes: proyeccion de cierre vs cumplimiento acumulado ----
    output$grafico_proyeccion <- renderPlotly({
      req(breakdown_r())
      df      <- breakdown_r()
      lbl     <- .dim_lbl(input$dim_breakdown)
      medida  <- input$medida_proyeccion
      periodo <- periodo_r()
      s       <- .SEMAFORO
      
      if (medida == "sacos") {
        col_cumpl  <- "Cumpl_Sacos_YTD";  col_prob   <- "Prob_Sacos"
        col_ytd    <- "Sacos_YTD";        col_brecha <- "Brecha_Sacos"
        lbl_medida <- "Sacos"
        fmt_val    <- function(x) scales::comma(x, accuracy = 1)
      } else {
        col_cumpl  <- "Cumpl_Margen_YTD"; col_prob   <- "Prob_Margen"
        col_ytd    <- "Margen_YTD";       col_brecha <- "Brecha_Margen"
        lbl_medida <- "Margen"
        fmt_val    <- function(x) scales::dollar(x, prefix = "$", accuracy = 1)
      }
      
      x_max <- max(df[[col_cumpl]] * 1.15, 1.1, na.rm = TRUE)
      y_max <- max(df[[col_prob]]  * 1.15, 1.1, na.rm = TRUE)
      
      df <- df %>%
        mutate(
          dim         = if ("dim" %in% names(df)) dim
          else paste0(RazSoc, " \u2014 ", LinNeg),
          .cx         = !!sym(col_cumpl), .cy     = !!sym(col_prob),
          .ytd        = !!sym(col_ytd),   .brecha = !!sym(col_brecha),
          color_punto = .color_punto_cumpl(.cx, .cy),
          tooltip     = paste0(
            "<b>", dim, "</b><br>",
            "Cumpl. Acum. ", periodo, ": ", scales::percent(.cx, accuracy = 0.1), "<br>",
            "Proyección de cierre: ",          scales::percent(.cy, accuracy = 0.1), "<br>",
            lbl_medida, " Acum.: ", fmt_val(.ytd), "<br>",
            "Brecha: ", fmt_val(.brecha)
          )
        )
      
      plot_ly(
        df, x = ~.cx, y = ~.cy,
        text       = ~gsub("\n", "<br>", stringr::str_wrap(dim, width = 18)),
        customdata = ~tooltip,
        type = "scatter", mode = "markers+text",
        marker = list(
          color = ~color_punto, size = 14, opacity = 0.90,
          line  = list(color = "white", width = 1.5)
        ),
        textfont      = list(size = 8, color = "#374151"),
        textposition  = "top center",
        hovertemplate = "%{customdata}<extra></extra>"
      ) %>%
        add_segments(
          x = s$umbral_alto, xend = s$umbral_alto, y = 0, yend = y_max,
          inherit = FALSE,
          line = list(color = "#94A3B8", dash = "dot", width = 1),
          showlegend = FALSE
        ) %>%
        add_segments(
          x = 0, xend = x_max, y = s$umbral_exceso, yend = s$umbral_exceso,
          inherit = FALSE,
          line = list(color = "#94A3B8", dash = "dot", width = 1),
          showlegend = FALSE
        ) %>%
        layout(
          title = paste0(
            "Proyección de Cierre \u2014 ", periodo,
            " \u00b7 ", lbl, " (", lbl_medida, ")"
          ),
          xaxis = list(
            title      = paste0("Cumpl. Acum. ", periodo, " (%)"),
            tickformat = ".0%", range = c(0, x_max)
          ),
          yaxis = list(
            title      = "Proyección de Cierre (%)",
            tickformat = ".0%", range = c(0, y_max)
          ),
          shapes = list(
            list(
              type = "rect", x0 = 0, y0 = 0,
              x1 = s$umbral_alto, y1 = s$umbral_exceso,
              fillcolor = "#FEE2E2", opacity = 0.50, line = list(width = 0)
            ),
            list(
              type = "rect", x0 = s$umbral_alto, y0 = s$umbral_exceso,
              x1 = x_max, y1 = y_max,
              fillcolor = "#DCFCE7", opacity = 0.50, line = list(width = 0)
            ),
            list(
              type = "rect", x0 = 0, y0 = s$umbral_exceso,
              x1 = s$umbral_alto, y1 = y_max,
              fillcolor = "#FEF9C3", opacity = 0.55, line = list(width = 0)
            ),
            list(
              type = "rect", x0 = s$umbral_alto, y0 = 0,
              x1 = x_max, y1 = s$umbral_exceso,
              fillcolor = "#FEF9C3", opacity = 0.55, line = list(width = 0)
            )
          )
        ) %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    
    ## Diagrama bullet: ritmo mensual actual vs requerido ----
    output$grafico_velocidad <- renderPlotly({
      req(breakdown_r())
      df      <- breakdown_r()
      lbl     <- .dim_lbl(input$dim_breakdown)
      medida  <- input$medida_velocidad
      periodo <- periodo_r()
      
      if (medida == "sacos") {
        col_actual <- "Ritmo_Sacos_Actual"; col_req    <- "Ritmo_Sacos_Req"
        lbl_medida <- "Sacos";              lbl_unidad <- "sacos/mes"
        fmt_val    <- function(x) scales::comma(x, accuracy = 1)
      } else {
        col_actual <- "Ritmo_Margen_Actual"; col_req   <- "Ritmo_Margen_Req"
        lbl_medida <- "Margen";              lbl_unidad <- "$/mes"
        fmt_val    <- function(x) scales::dollar(x, prefix = "$", accuracy = 1)
      }
      
      df <- df %>%
        arrange(!!sym(col_actual)) %>%
        mutate(
          dim         = if ("dim" %in% names(df)) dim
          else paste0(RazSoc, " \u2014 ", LinNeg),
          dim         = factor(dim, levels = dim),
          .act        = !!sym(col_actual), .req = !!sym(col_req),
          .b_rojo     = .req * 0.70,
          .b_amarillo = .req * 0.90,
          .b_verde    = .req * 1.20,
          .ratio      = .division_segura(.act / pmax(.req, 1e-9)),
          tooltip     = paste0(
            "<b>", dim, "</b><br>",
            "Ritmo actual: ",    fmt_val(.act), " ", lbl_unidad, "<br>",
            "Ritmo requerido: ", fmt_val(.req), " ", lbl_unidad, "<br>",
            "Ratio: ", scales::percent(.ratio, accuracy = 0.1)
          )
        )
      
      bar_w  <- 0.25
      band_w <- 0.55
      
      p <- plot_ly() %>%
        add_bars(
          data = df, x = ~.b_rojo, y = ~dim, base = 0, orientation = "h",
          width = band_w, name = "Riesgo (< 70%)",
          marker    = list(color = "#FEE2E2", line = list(color = "#FECACA", width = 0.5)),
          hoverinfo = "skip", showlegend = TRUE
        ) %>%
        add_bars(
          data = df, x = ~(.b_amarillo - .b_rojo), y = ~dim, base = ~.b_rojo,
          orientation = "h", width = band_w, name = "Atención (70-90%)",
          marker    = list(color = "#FEF9C3", line = list(color = "#FDE68A", width = 0.5)),
          hoverinfo = "skip", showlegend = TRUE
        ) %>%
        add_bars(
          data = df, x = ~(.b_verde - .b_amarillo), y = ~dim, base = ~.b_amarillo,
          orientation = "h", width = band_w, name = "En ruta (\u2265 90%)",
          marker    = list(color = "#DCFCE7", line = list(color = "#BBF7D0", width = 0.5)),
          hoverinfo = "skip", showlegend = TRUE
        ) %>%
        add_bars(
          data = df, x = ~.act, y = ~dim, base = 0, orientation = "h",
          width = bar_w, name = paste0("Ritmo actual (", lbl_unidad, ")"),
          marker        = list(color = "#334155", opacity = 0.90, line = list(width = 0)),
          customdata    = ~tooltip,
          hovertemplate = "%{customdata}<extra></extra>", showlegend = TRUE
        ) %>%
        add_markers(
          data = df, x = ~.req, y = ~dim, name = "Ritmo requerido",
          marker    = list(
            color  = "#0F172A", symbol = "line-ns-open", size = 16,
            line   = list(color = "#0F172A", width = 4)
          ),
          hoverinfo = "skip", showlegend = TRUE
        )
      
      x_max <- max(df$.b_verde * 1.05, df$.act * 1.05, na.rm = TRUE)
      
      p %>%
        layout(
          title   = paste0(
            "Velocidad de ejecución frente a la requerida \u2014 ", lbl, " (", lbl_medida, ")"
          ),
          barmode = "overlay",
          xaxis   = list(
            title = paste0(lbl_medida, " por mes"), tickformat = ",", range = c(0, x_max)
          ),
          yaxis  = list(title = ""),
          legend = list(
            orientation = "v", x = 1.02, y = 1,
            xanchor = "left", yanchor = "top",
            font = list(size = 10), traceorder = "normal"
          ),
          hovermode = "y unified"
        ) %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    
  })
}
