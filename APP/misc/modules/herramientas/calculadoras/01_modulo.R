.calc_requerir <- function(...) {
  valores <- list(...)
  valido <- vapply(valores, function(x) is.numeric(x) && length(x) == 1L && is.finite(x), logical(1))
  shiny::validate(shiny::need(all(valido), "Complete los campos con valores numéricos válidos."))
}

# Indicadores con valor escalar válido ----
.calc_indicador <- function(datos, item) {
  valor <- suppressWarnings(as.numeric(datos$Valor[as.character(datos$Item) == item]))
  valor <- valor[is.finite(valor)]
  if (length(valor)) valor[[1]] else 0
}

# Helpers UI internos ----

# Campos numéricos con etiqueta y formato corporativos ----
.calc_input_fila <- function(label, input_id, value = 0, symbol = "$") {
  racafeForms::racafeInputNumeric(input_id, label = label, value = value,
    format = if (symbol == "$") "currency" else "number", width = "100%")
}

# Fila estándar: label izquierda, uiOutput alineado a la derecha
.calc_output_fila <- function(label, output_id) {
  fluidRow(
    column(6, racafeShiny::FormatearTexto(label)),
    column(6, class = "calc-output-right", uiOutput(output_id))
  )
}

# Encabezado de columna para tablas internas de calculadora
.calc_header_col <- function(label, width, left = FALSE) {
  cls <- if (left) "calc-header-col calc-header-col--left" else "calc-header-col"
  column(width, racafeShiny::FormatearTexto(label), class = cls)
}

# Botón calcular centrado, idéntico en las tres calculadoras
.calc_boton_calcular <- function(ns) {
  racafeForms::racafeFormFooter(racafeForms::racafeButton("Calcular", "Calcular", icon = "calculator",
    variant = "primary", size = "sm", ns = ns))
}

# Card bs4 con solidHeader blanco sin colapso (shorthand)
.calc_card <- function(title, ...) racafeForms::racafeCard(title = title, ...)

# Helpers server internos ----

# Reemplaza ifelse(is.null(input$x), 0, input$x)
.calc_inp0 <- function(x) if (is.null(x) || is.na(x)) 0 else x

# Patrón de inicialización única
.calc_init_once <- function(fn_init) {
  done <- reactiveVal(FALSE)
  observe({
    if (!done()) { fn_init(); done(TRUE) }
  })
}

# Renderiza un output de texto formateado como dinero o porcentaje
.calc_render_val <- function(output, id, reactive_fn, col, tipo = "dollar") {
  output[[id]] <- .crm_render_ui({
    val <- reactive_fn() %>% pull(!!col)
    shiny::validate(shiny::need(length(val) == 1L && is.finite(val),
      "No hay un resultado numérico válido. Revise los campos y pulse Calcular."))
    val_fmt <- switch(tipo,
                      "dollar"  = scales::dollar(val, accuracy = 0.01),
                      "percent" = scales::percent(val, accuracy = 0.01),
                      scales::dollar(val, accuracy = 0.01)
    )
    shiny::tags$span(class = "calc-resultado", val_fmt)
  })
}

# Registra múltiples outputs sobre un named list (nombre -> tipo)
.calc_bind_outputs <- function(output, config, reactive_fn) {
  lapply(names(config), function(id) {
    .calc_render_val(output, id, reactive_fn, id, config[[id]])
  })
}


# Coproductos ----
CalculadoraCoproductosUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafeForm(variant = "plain",
    # Selector de segmento
    fluidRow(
      column(12,
             div(class = "calc-segmento-wrap",
                 .crm_select("Segmento", label = "Segmento",
                   choices = c("MEDIANO", "DETAL"), selected = "MEDIANO", ns = ns)
             )
      )
    ),
    fluidRow(
      # Columna izquierda - Inputs
      column(6,
             .calc_card("Costos por Producto",
                        .calc_input_fila("Costo 1D7",  ns("COSTO_1D7")),
                        .calc_input_fila("Costo 5D5",  ns("COSTO_5D5")),
                        .calc_input_fila("Costo 5D50", ns("COSTO_5D50")),
                        .calc_input_fila("Costo 9D1",  ns("COSTO_9D1")),
                        .calc_input_fila("Costo 9D3",  ns("COSTO_9D3"))
             ),
             .calc_card("Otros Costos",
                        .calc_input_fila("Cargos de Trilladora", ns("CostosTrilladora"), value = 400),
                        .calc_input_fila("Gastos Fijos",          ns("GastosFijos"),      value = 200),
                        .calc_input_fila("Fletes",                ns("Fletes"),           value = 200),
                        .calc_input_fila("Margen Esperado",        ns("MargenEsperado"),  value = 200)
             )
      ),
      # Columna derecha - Outputs
      column(6,
             .calc_card("Precios Calculados",
                        .calc_output_fila("Precio 1D7",  ns("Precio_1d7")),
                        .calc_output_fila("Precio 5D5",  ns("Precio_5d5")),
                        .calc_output_fila("Precio 5D50", ns("Precio_5d50")),
                        .calc_output_fila("Precio 9D1",  ns("Precio_9d1")),
                        .calc_output_fila("Precio 9D3",  ns("Precio_9d3"))
             ),
             .calc_card("Viabilidad de Negocios",
                        fluidRow(
                          .calc_header_col("Producto", 4, left = TRUE),
                          .calc_header_col("Precio",   2),
                          .calc_header_col("Kilos",    2),
                          .calc_header_col("Margen",   2),
                          .calc_header_col("Utilidad", 2)
                        ),
                        br(),
                        fluidRow(class = "calc-producto-row",
                                 column(4, racafeShiny::FormatearTexto("1D7")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Pre1D7"), NULL, 0,
                                     format = "currency", width = "100%")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Kls1D7"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar1D7"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl1D7")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, racafeShiny::FormatearTexto("5D5")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Pre5D5"), NULL, 0,
                                     format = "currency", width = "100%")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Kls5D5"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar5D5"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl5D5")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, racafeShiny::FormatearTexto("5D50")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Pre5D50"), NULL, 0,
                                     format = "currency", width = "100%")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Kls5D50"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar5D50"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl5D50")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, racafeShiny::FormatearTexto("9D1")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Pre9D1"), NULL, 0,
                                     format = "currency", width = "100%")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Kls9D1"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar9D1"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl9D1")))
                        ),
                        fluidRow(class = "calc-producto-row",
                                 column(4, racafeShiny::FormatearTexto("9D3")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Pre9D3"), NULL, 0,
                                     format = "currency", width = "100%")),
                                 column(2, racafeForms::racafeInputNumeric(ns("Kls9D3"), NULL, 0, width = "100%")),
                                 column(2, class = "calc-output-center", uiOutput(ns("Mar9D3"))),
                                 column(2, class = "calc-output-center", uiOutput(ns("Utl9D3")))
                        ),
                        br()
             ),
             .calc_boton_calcular(ns)
      )
    )
  )
}

CalculadoraCoproductos <- function(id, dat_ind, usr, clientes = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar defaults al cambiar segmento
    observeEvent(list(input$Segmento, dat_ind()), ignoreNULL = FALSE, {
      aux1 <- dat_ind()
      req(is.data.frame(aux1), nrow(aux1) > 0L)
      
      get_val <- function(item) .calc_indicador(aux1, item)

      val_9d1  <- get_val("Precio Soluble (Compras)")
      val_5d5  <- get_val("Precio Molidos (Compras)")
      val_1d7  <- get_val("Precio Consumo (Compras)")
      val_9d3  <- if (!is.na(val_9d1)) val_9d1 - 1000 else NA
      val_5d50 <- if (!any(is.na(c(val_9d1, val_5d5)))) mean(c(val_9d1, val_5d5)) else NA
      
      margen_esperado <- tryCatch({
        fuente_clientes <- if (is.null(clientes)) racafeBD::CargarDatos("CRMNALCLIENTE") else {
          if (shiny::is.reactive(clientes)) clientes() else clientes
        }
        fuente_clientes %>%
          filter(LinNegCod == 10000, Segmento == input$Segmento) %>%
          group_by(LinNegCod, CliNitPpal) %>%
          filter(if (any(!is.na(FecProceso))) FecProceso == max(FecProceso, na.rm = TRUE) else FALSE) %>%
          group_by(Segmento) %>%
          summarise(
            MargenKilo = sum(MNFCCPpto, na.rm = TRUE) / (sum(SSPpto, na.rm = TRUE) * 70),
            .groups = "drop"
          ) %>%
          pull(MargenKilo)
      }, error = function(e) 200)
      
      if (length(margen_esperado) != 1L || !is.finite(margen_esperado)) margen_esperado <- 200
      gast_fijos <- ifelse(input$Segmento == "DETAL", 450, 200)
      
      racafeForms::racafeUpdateNumeric(session, "COSTO_9D3",      value = val_9d3)
      racafeForms::racafeUpdateNumeric(session, "COSTO_9D1",      value = val_9d1)
      racafeForms::racafeUpdateNumeric(session, "COSTO_5D50",     value = val_5d50)
      racafeForms::racafeUpdateNumeric(session, "COSTO_5D5",      value = val_5d5)
      racafeForms::racafeUpdateNumeric(session, "COSTO_1D7",      value = val_1d7)
      racafeForms::racafeUpdateNumeric(session, "MargenEsperado", value = margen_esperado)
      racafeForms::racafeUpdateNumeric(session, "GastosFijos",    value = gast_fijos)
    })
    
    # Cálculos disparados por botón
    val_calculadora <- eventReactive(input$Calcular, ignoreNULL = FALSE, {
      .calc_requerir(input$COSTO_9D3, input$COSTO_9D1, input$COSTO_5D50,
          input$COSTO_5D5, input$COSTO_1D7, input$CostosTrilladora,
          input$GastosFijos, input$Fletes, input$MargenEsperado)
      
      data.frame(
        val_9d3  = input$COSTO_9D3,  val_9d1  = input$COSTO_9D1,
        val_5d50 = input$COSTO_5D50, val_5d5  = input$COSTO_5D5,
        val_1d7  = input$COSTO_1D7,
        Trilladora     = input$CostosTrilladora,
        GastosFijos    = input$GastosFijos,
        Fletes         = input$Fletes,
        MargenEsperado = input$MargenEsperado,
        Pre9D3  = .calc_inp0(input$Pre9D3),  Pre9D1  = .calc_inp0(input$Pre9D1),
        Pre5D50 = .calc_inp0(input$Pre5D50), Pre5D5  = .calc_inp0(input$Pre5D5),
        Pre1D7  = .calc_inp0(input$Pre1D7),
        Kls9D3  = .calc_inp0(input$Kls9D3),  Kls9D1  = .calc_inp0(input$Kls9D1),
        Kls5D50 = .calc_inp0(input$Kls5D50), Kls5D5  = .calc_inp0(input$Kls5D5),
        Kls1D7  = .calc_inp0(input$Kls1D7)
      ) %>%
        mutate(
          Precio_9d3  = val_9d3  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_9d1  = val_9d1  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_5d50 = val_5d50 + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_5d5  = val_5d5  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Precio_1d7  = val_1d7  + Trilladora + GastosFijos + Fletes + MargenEsperado,
          Mar9D3  = Pre9D3  - Precio_9d3,  Mar9D1  = Pre9D1  - Precio_9d1,
          Mar5D50 = Pre5D50 - Precio_5d50, Mar5D5  = Pre5D5  - Precio_5d5,
          Mar1D7  = Pre1D7  - Precio_1d7,
          Utl9D3  = Mar9D3  * Kls9D3,     Utl9D1  = Mar9D1  * Kls9D1,
          Utl5D50 = Mar5D50 * Kls5D50,    Utl5D5  = Mar5D5  * Kls5D5,
          Utl1D7  = Mar1D7  * Kls1D7
        )
    })
    
    .calc_bind_outputs(output,
                  list(
                    Precio_9d3 = "dollar", Precio_9d1 = "dollar", Precio_5d50 = "dollar",
                    Precio_5d5 = "dollar", Precio_1d7 = "dollar",
                    Mar9D3  = "dollar", Mar9D1  = "dollar", Mar5D50 = "dollar",
                    Mar5D5  = "dollar", Mar1D7  = "dollar",
                    Utl9D3  = "dollar", Utl9D1  = "dollar", Utl5D50 = "dollar",
                    Utl5D5  = "dollar", Utl1D7  = "dollar"
                  ),
                  val_calculadora
    )
  })
}


# Dólares Excelso ----
CalculadoraDolaresExcelsoUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafeForm(variant = "plain", fluidRow(
    column(6,
           .calc_card("Parámetros",
                      .calc_input_fila("TRM",                  ns("TRM")),
                      .calc_input_fila("Precio NYC (HT)",       ns("NYC")),
                      .calc_input_fila("Diferencial de Compra", ns("PrimaUGQ")),
                      .calc_input_fila("Prima Mallas",           ns("PrimaMallas")),
                      .calc_input_fila("Prima Especiales",       ns("PrimaEspeciales")),
                      .calc_input_fila("Utilidad",               ns("Utilidad")),
                      .calc_input_fila("Empaque",                ns("Empaque")),
                      .calc_input_fila("Grainpro",               ns("Grainpro")),
                      .calc_input_fila("Flete",                  ns("Flete"))
           )
    ),
    column(6,
           .calc_card("Resultados",
                      .calc_output_fila("Total",            ns("Total")),
                      .calc_output_fila("Precio (USD)",     ns("PrecioUSD")),
                      .calc_output_fila("Precio (COP)",     ns("PrecioCOP")),
                      .calc_output_fila("Precio con Flete", ns("PrecioFlete"))
           ),
           .calc_boton_calcular(ns)
    )
  ))

}

CalculadoraDolaresExcelso <- function(id, dat_ind, usr) {
  moduleServer(id, function(input, output, session) {
    
    # Inicialización única de defaults desde indicadores y sistema
    # Los indicadores se comparten con el menú de indicadores del encabezado.
    .calc_init_once(function() {
      req(is.data.frame(dat_ind()), nrow(dat_ind()) > 0L)
      defaults <- list(
        TRM            = dat_ind() %>% filter(Item == "TRM (Hoja de trabajo)") %>% pull(Valor),
        NYC            = dat_ind() %>% filter(Item == "Precio NYC (HT)") %>% pull(Valor),
        PrimaUGQ       = .calc_indicador(dat_ind(), "Diferencial de Compra (HT)"),
        PrimaMallas    = 0, PrimaEspeciales = 0,
        Utilidad       = 13.39,
        Empaque        = 0, Grainpro = 0, Flete = 200
      )
      for (id_input in names(defaults)) {
        racafeForms::racafeUpdateNumeric(session, id = id_input, value = defaults[[id_input]])
      }
    })
    
    # Cálculos disparados por botón
    val_calculadora <- eventReactive(input$Calcular, ignoreNULL = FALSE, {
      .calc_requerir(input$TRM, input$NYC, input$PrimaUGQ, input$PrimaMallas, input$PrimaEspeciales,
        input$Utilidad, input$Empaque, input$Grainpro, input$Flete)
      validate(need(input$TRM > 0, "La TRM debe ser mayor que cero."))
      data.frame(
        TRM             = input$TRM,
        NYC             = input$NYC,
        PrimaUGQ        = input$PrimaUGQ,
        PrimaMallas     = input$PrimaMallas,
        PrimaEspeciales = input$PrimaEspeciales,
        Utilidad        = input$Utilidad,
        Empaque         = input$Empaque,
        Grainpro        = input$Grainpro,
        Flete           = input$Flete
      ) %>%
        mutate(
          Total       = NYC + PrimaUGQ + PrimaMallas + PrimaEspeciales + Utilidad + Empaque + Grainpro,
          PrecioUSD   = Total * 2.20462 / 100,
          PrecioCOP   = PrecioUSD * TRM,
          PrecioFlete = PrecioCOP + Flete
        )
    })
    
    .calc_bind_outputs(output,
                  list(Total = "dollar", PrecioUSD = "dollar", PrecioCOP = "dollar", PrecioFlete = "dollar"),
                  val_calculadora
    )
  })
}


# Pesos Excelso ----
CalculadoraPesosExcelsoUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafeForm(variant = "plain", fluidRow(
    column(6,
           .calc_card("Parámetros Generales",
                      .calc_input_fila("TRM",                ns("TRM"),           value = 4403),
                      .calc_input_fila("Costo Consumo",      ns("CostoConsumo"),  value = 28000),
                      .calc_input_fila("Precio Carga",       ns("PrecioCarga"),   value = 3107000),
                      .calc_input_fila("Prima Especial +83", ns("PrimaEspecial"), value = 2000),
                      .calc_input_fila("Prima Supremo",      ns("PrimaSupremo"),  value = 1000)
           ),
           .calc_card("Resultados Generales",
                      .calc_output_fila("Precio por Kilo", ns("PrecioKilo")),
                      .calc_output_fila("Rendimiento",     ns("Rendimiento")),
                      .calc_output_fila("Costo por Kilo",  ns("CostoKilo"))
           ),
           .calc_card("Parámetros de Producto",
                      fluidRow(
                        .calc_header_col("Producto", 6, left = TRUE),
                        .calc_header_col("Costo",    2),
                        .calc_header_col("Margen",   2),
                        .calc_header_col("Precio",   2)
                      ),
                      shiny::br(),
                      fluidRow(class = "calc-producto-row",
                               column(6, racafeShiny::FormatearTexto("Blend")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosBlend"))),
                               column(2, racafeForms::racafeInputNumeric(ns("MarBlend"), NULL, 0,
                                   format = "currency", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreBlend")))
                      ),
                      fluidRow(class = "calc-producto-row",
                               column(6, racafeShiny::FormatearTexto("UGQ")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosUGQ"))),
                               column(2, racafeForms::racafeInputNumeric(ns("MarUGQ"), NULL, 0,
                                   format = "currency", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreUGQ")))
                      ),
                      fluidRow(class = "calc-producto-row",
                               column(6, racafeShiny::FormatearTexto("Supremo")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosSup"))),
                               column(2, racafeForms::racafeInputNumeric(ns("MarSup"), NULL, 0,
                                   format = "currency", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreSup")))
                      ),
                      fluidRow(class = "calc-producto-row",
                               column(6, racafeShiny::FormatearTexto("Especial 83+")),
                               column(2, class = "calc-output-center", uiOutput(ns("CosEsp"))),
                               column(2, racafeForms::racafeInputNumeric(ns("MarEsp"), NULL, 0,
                                   format = "currency", width = "100%")),
                               column(2, class = "calc-output-center", uiOutput(ns("PreEsp")))
                      )
           )
    ),
    column(6,
           .calc_card("Costos y Utilidades",
                      .calc_input_fila("Prima",           ns("Prima"),       value = 2000),
                      .calc_input_fila("Flete Interno",   ns("FleteInt"),    value = 1000),
                      .calc_input_fila("Flete a Cliente", ns("FleteExt"),    value = 300),
                      .calc_input_fila("Costos Fijos",    ns("CostosFijos"), value = 500),
                      .calc_input_fila("Financieros",     ns("Financieros"), value = 1700),
                      br(),
                      .calc_output_fila("Costos",             ns("Costos2")),
                      .calc_input_fila("Precio",              ns("Precio"),    value = 39420),
                      .calc_output_fila("Utilidad",           ns("Utilidad2")),
                      .calc_output_fila("Utilidad (cts/lb)",  ns("UtilidadCtvs2"))
           ),
           .calc_boton_calcular(ns), shiny::uiOutput(ns("guardar_margenes_ui"))
    )
  ))

}

CalculadoraPesosExcelso <- function(id, dat_ind, usr, margenes = NULL, guardar_margenes = NULL) {
  moduleServer(id, function(input, output, session) {
    
    margenes_pendientes_rv <- reactiveVal(NULL)
    output$guardar_margenes_ui <- .crm_render_ui({
      if (!usr() %in% c("CMEDINA", "JGCANON")) return(NULL)
      racafeForms::racafeButtonSave("GuardarMargenes", label = "Guardar márgenes", ns = session$ns)
    })
    # Control de acceso a márgenes según usuario
    observe({
      permitido <- usr() %in% c("CMEDINA", "JGCANON")
      lapply(c("MarBlend", "MarUGQ", "MarSup", "MarEsp"), function(id) {
        racafeForms::racafeUpdateEnabled(session, id, enabled = permitido)
      })
    })
    
    # Inicialización única de defaults desde indicadores y BD de márgenes
    .calc_init_once(function() {
      req(is.data.frame(dat_ind()), nrow(dat_ind()) > 0L)
      defaults <- list(
        TRM           = dat_ind() %>%
          filter(Item == "TRM (Hoja de trabajo)") %>% pull(Valor),
        CostoConsumo  = dat_ind() %>%
          filter(Item == "Precio Consumo (Compras)") %>% pull(Valor),
        PrecioCarga   = dat_ind() %>%
          filter(Item == "Precio Carga (Promedio de últimas entradas del día)") %>% pull(Valor),
        PrimaEspecial = 2000, Prima = 2500,
        FleteInt = 500, FleteExt = 500, CostosFijos = 500, Financieros = 1700, Precio = 40000
      )
      for (id_input in names(defaults)) {
        racafeForms::racafeUpdateNumeric(session, id = id_input, value = defaults[[id_input]])
      }
      
      # Márgenes persistidos en BD
      aux_mar <- tryCatch({
        if (is.null(margenes)) racafeBD::CargarDatos("CRMNALCPMAR") else {
          if (shiny::is.reactive(margenes)) margenes() else margenes
        }
      }, error = function(e) NULL)
      if (is.data.frame(aux_mar) && nrow(aux_mar)) {
        aux_mar <- aux_mar %>% arrange(desc(FechaHoraCrea)) %>% slice_head(n = 1L)
      }
      lapply(c("MarBlend", "MarUGQ", "MarSup", "MarEsp"), function(inp) {
        valor <- if (is.data.frame(aux_mar) && nrow(aux_mar)) as.numeric(aux_mar[[inp]][[1]]) else 0
        racafeForms::racafeUpdateNumeric(session, id = inp, value = valor)
      })
    })
    
    # Cálculos disparados por botón
    val_calculadora <- eventReactive(input$Calcular, ignoreNULL = FALSE, {
      .calc_requerir(input$TRM, input$CostoConsumo, input$PrecioCarga,
          input$PrimaEspecial, input$Prima, input$FleteInt,
          input$FleteExt, input$CostosFijos, input$Financieros, input$Precio)
      
      validate(need(input$TRM > 0, "La TRM debe ser mayor que cero."))
      data.frame(
        TRM           = input$TRM,
        CostoConsumo  = input$CostoConsumo,
        PrecioCarga   = input$PrecioCarga,
        PrimaEspecial = input$PrimaEspecial,
        PrimaSupremo  = .calc_inp0(input$PrimaSupremo),
        MarBlend      = .calc_inp0(input$MarBlend),
        MarUGQ        = .calc_inp0(input$MarUGQ),
        MarSup        = .calc_inp0(input$MarSup),
        MarEsp        = .calc_inp0(input$MarEsp),
        Prima         = input$Prima,
        FleteInt      = input$FleteInt,
        FleteExt      = input$FleteExt,
        CostosFijos   = input$CostosFijos,
        Financieros   = input$Financieros,
        Precio        = input$Precio
      ) %>%
        mutate(
          PrecioKilo    = PrecioCarga / 125,
          Rendimiento   = 70 / 96.89,
          CostoKilo     = PrecioKilo / Rendimiento,
          Costos        = CostoKilo + PrimaEspecial + FleteInt + Financieros,
          CosUGQ        = CostoKilo,
          PrecioUGQ     = CosUGQ + MarUGQ,
          UtilidadKilo  = PrecioUGQ - Costos,
          UtilidadCtvs  = ((UtilidadKilo * 70) / 1.54322) / TRM,
          CosBlend      = (CostoConsumo * 0.4) + (CosUGQ * 0.6),
          PreBlend      = CosBlend + MarBlend,
          UtBlend       = (((PreBlend - CosBlend) * 70) / 1.54322) / TRM,
          PreUGQ        = CosUGQ + MarUGQ,
          UtUGQ         = (((PreUGQ - CosUGQ) * 70) / 1.54322) / TRM,
          CosSup        = CosUGQ + PrimaSupremo,
          PreSup        = CosSup + MarSup,
          UtSup         = (((PreSup - CosSup) * 70) / 1.54322) / TRM,
          CosEsp        = CosUGQ + PrimaEspecial,
          PreEsp        = CosEsp + MarEsp,
          UtEsp         = (((PreEsp - CosEsp) * 70) / 1.54322) / TRM,
          Costos2       = CostoKilo + Prima + FleteInt + FleteExt + CostosFijos + Financieros,
          Utilidad2     = Precio - Costos2,
          UtilidadCtvs2 = ((Utilidad2 * 70) / 1.54322) / TRM
        )
    })
    
    .calc_bind_outputs(output,
                  list(
                    PrecioKilo    = "dollar",  Rendimiento   = "percent", CostoKilo    = "dollar",
                    Costos        = "dollar",  PrecioUGQ     = "dollar",  UtilidadKilo = "dollar",
                    UtilidadCtvs  = "dollar",  CosBlend      = "dollar",  PreBlend     = "dollar",
                    UtBlend       = "dollar",  CosUGQ        = "dollar",  PreUGQ       = "dollar",
                    UtUGQ         = "dollar",  CosSup        = "dollar",  PreSup       = "dollar",
                    UtSup         = "dollar",  CosEsp        = "dollar",  PreEsp       = "dollar",
                    UtEsp         = "dollar",  Costos2       = "dollar",  Utilidad2    = "dollar",
                    UtilidadCtvs2 = "dollar"
                  ),
                  val_calculadora
    )
    
    # Guardar márgenes es una acción explícita, separada del cálculo ----
    observeEvent(input$GuardarMargenes, {
      req(usr() %in% c("CMEDINA", "JGCANON"))
      valores <- c(input$MarBlend, input$MarUGQ, input$MarSup, input$MarEsp)
      validate(need(length(valores) == 4L && all(is.finite(valores)), "Revise los cuatro márgenes."))
      margenes_pendientes_rv(valores)
      racafeShiny::MostrarModalConfirmacion(ns = session$ns, titulo = "Guardar márgenes",
        texto = "¿Guardar estos márgenes como referencia para las calculadoras?",
        id_cancelar = "CancelarMargenes", id_confirmar = "ConfirmarMargenes",
        label_confirmar = "Guardar", icono_confirmar = "floppy-disk", color_confirmar = "#DA291C")
    })
    observeEvent(input$CancelarMargenes, {
      margenes_pendientes_rv(NULL)
      shiny::removeModal()
    })
    observeEvent(input$ConfirmarMargenes, {
      req(usr() %in% c("CMEDINA", "JGCANON"), !is.null(margenes_pendientes_rv()))
      valores <- margenes_pendientes_rv()
      margenes_pendientes_rv(NULL)
      tryCatch({
        registro <- data.frame(UsuarioCrea = usr(), FechaHoraCrea = Sys.time(),
          MarBlend = valores[[1]], MarUGQ = valores[[2]], MarSup = valores[[3]], MarEsp = valores[[4]])
        if (is.function(guardar_margenes)) guardar_margenes(registro) else {
          racafeBD::AgregarDatos(registro, "CRMNALCPMAR")
        }
        shiny::removeModal()
        racafeForms::racafeNotify(session, "Márgenes guardados", type = "success")
      }, error = function(e) racafeForms::racafeNotify(session, "No fue posible guardar", conditionMessage(e),
        type = "error"))
    })

  })
}


# Módulo integrador ----
CalculadoraUI <- function(id) {
  ns <- NS(id)
  bs4Dash::tabsetPanel(
    tabPanel("Pesos - Excelso",   shiny::br(), CalculadoraPesosExcelsoUI(ns("pesos"))),
    tabPanel("Dólares - Excelso", shiny::br(), CalculadoraDolaresExcelsoUI(ns("dolares"))),
    tabPanel("Coproductos",       shiny::br(), CalculadoraCoproductosUI(ns("coproductos")))
  )
}

Calculadora <- function(id, dat_ind, usr, clientes = NULL) {
  moduleServer(id, function(input, output, session) {
    CalculadoraPesosExcelso("pesos",      dat_ind, usr)
    CalculadoraDolaresExcelso("dolares",  dat_ind, usr)
    CalculadoraCoproductos("coproductos", dat_ind, usr, clientes = clientes)
  })
}


