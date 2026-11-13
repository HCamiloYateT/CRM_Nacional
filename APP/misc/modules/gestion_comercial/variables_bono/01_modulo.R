# Reglas del adjunto: simulación identificada por versión, sin liquidaciones automáticas ----
# Valores quemados autorizados para construir la vista; reemplazar por la política real.
.BON_PROVISIONAL_2026 <- list(umbrales = c(0.70, 0.85, 1.00), factores = c(0, 0.33, 0.67, 1.00))
.BON_ESQUEMAS <- c("2026 provisional: perfil Carlos" = "2026_carlos",
  "2026 provisional: perfil Jonathan" = "2026_jonathan", "2026 provisional: cuatrimestres" = "2026_periodos",
  "2025: Carlos (20/60/20)" = "2025_carlos", "2025: Jonathan (47/53)" = "2025_jonathan",
  "2025: Gustavo/Luis por cuatrimestre" = "2025_periodos", "2024: Carlos" = "2024_carlos",
  "2024: Jonathan, comparación de fórmulas" = "2024_jonathan", "2024: Gustavo/Luis por cuatrimestre" = "2024_periodos")
.bon_factor <- function(cumplimiento, caso = "escalonado") {
  if (!is.finite(cumplimiento)) return(NA_real_)
  if (caso == "provisional") {
    indice <- findInterval(cumplimiento, .BON_PROVISIONAL_2026$umbrales) + 1L
    return(.BON_PROVISIONAL_2026$factores[[indice]])
  }
  switch(caso, ochenta = as.numeric(cumplimiento >= 0.8), cien = as.numeric(cumplimiento >= 1),
    escalonado = if (cumplimiento < 0.7) 0 else if (cumplimiento < 0.85) 0.33 else if (cumplimiento < 1) 0.67 else 1)
}
.bon_reglas <- function(esquema) {
  if (startsWith(esquema, "2026_")) {
    reglas <- .bon_reglas(sub("^2026", "2025", esquema))
    reglas$Caso <- "provisional"
    return(reglas)
  }
  if (esquema == "2025_carlos") return(data.frame(
    Componente = c("COPRODUCTO DETAL", "COPRODUCTO MEDIANO", "MEDIDA DETAL"),
    Negocio = c("COPRODUCTO", "COPRODUCTO", "MEDIDA"), Segmento = c("DETAL", "MEDIANO", "DETAL"),
    Peso = c(0.2, 0.6, 0.2), Caso = c("cien", "escalonado", "cien")))
  if (esquema == "2025_jonathan") return(data.frame(Componente = c("COPRODUCTO", "MEDIDA"),
    Negocio = c("COPRODUCTO", "MEDIDA"), Segmento = "TODOS", Peso = c(0.47, 0.53), Caso = "escalonado"))
  if (esquema == "2024_carlos") return(data.frame(Componente = c("MEDIANO", "DETAL"), Negocio = "COPRODUCTO",
    Segmento = c("MEDIANO", "DETAL"), Peso = c(0.7, 0.3), Caso = "escalonado"))
  if (esquema == "2024_jonathan") return(data.frame(Componente = c("MEDIDA", "COPRODUCTO", "NUEVOS"),
    Negocio = c("MEDIDA", "COPRODUCTO", "NUEVOS"), Segmento = "TODOS", Peso = c(0.75, 0.35, 0.65),
    Caso = c("escalonado", "escalonado", "cien")))
  if (esquema %in% c("2025_periodos", "2024_periodos")) return(data.frame(
    Componente = paste("Cuatrimestre", 1:3), Negocio = "TODOS", Segmento = "TODOS", Peso = 1, Caso = "ochenta"))
  stop("Esquema no permitido.")
}
.bon_calcular <- function(panel, esquema, corte) {
  reglas <- .bon_reglas(esquema)
  for (col in c("Ejecutado", "Presupuesto", "Cumplimiento", "Factor", "Aporte", "AporteIndicadorAntiguo")) {
    reglas[[col]] <- rep(NA_real_, nrow(reglas))
  }
  for (i in seq_len(nrow(reglas))) {
    x <- panel[format(panel$Fecha, "%Y") == format(corte, "%Y") & panel$Fecha <= corte, , drop = FALSE]
    if (grepl("periodos$", esquema)) x <- x[ceiling(as.integer(format(x$Fecha, "%m")) / 4) == i, ]
    if (reglas$Negocio[[i]] != "TODOS") x <- x[x$Negocio == reglas$Negocio[[i]], , drop = FALSE]
    if (reglas$Segmento[[i]] != "TODOS") x <- x[x$Segmento == reglas$Segmento[[i]], , drop = FALSE]
    # Un periodo todavía no iniciado permanece vacío, no es incumplimiento cero.
    if (!nrow(x)) next
    reglas$Ejecutado[[i]] <- .com_suma(x$Margen)
    reglas$Presupuesto[[i]] <- .com_suma(x$PptoMargen)
    reglas$Cumplimiento[[i]] <- .gc_ratio(reglas$Ejecutado[[i]], reglas$Presupuesto[[i]])
    reglas$Factor[[i]] <- .bon_factor(reglas$Cumplimiento[[i]], reglas$Caso[[i]])
    reglas$Aporte[[i]] <- reglas$Peso[[i]] * reglas$Factor[[i]]
    if (esquema == "2024_jonathan" && reglas$Componente[[i]] == "NUEVOS") {
      reglas$Aporte[[i]] <- reglas$Aporte[[i]] * 0.25
    }
    reglas$AporteIndicadorAntiguo[[i]] <- reglas$Aporte[[i]]
    if (esquema == "2024_jonathan" && reglas$Componente[[i]] == "MEDIDA") {
      reglas$AporteIndicadorAntiguo[[i]] <- reglas$Aporte[[i]] * 0.65
    }
  }
  reglas$Politica <- if (startsWith(esquema, "2026_")) {
    "SIMULACIÓN 2026: parámetros provisionales"
  } else if (substr(esquema, 1, 4) != format(corte, "%Y")) {
    "SIMULACIÓN de esquema histórico en otro año"
  } else "Histórica"
  etiquetas <- c(ochenta = "Desde 80%", cien = "Desde 100%", escalonado = "Tramos 70/85/100%",
    provisional = "Tramos provisionales 70/85/100%")
  reglas$Regla <- unname(etiquetas[reglas$Caso])
  reglas$Caso <- NULL
  reglas
}
.bon_panel_crm <- function(hechos, clientes, corte, asesor) {
  detalle_presupuesto <- attr(clientes, "presupuesto_detalle", exact = TRUE)
  if (!is.null(detalle_presupuesto)) clientes <- detalle_presupuesto
  x <- .com_hechos(hechos, corte, cerrados = TRUE)
  x <- x[format(x$Fecha, "%Y") == format(corte, "%Y"), ]
  if (!is.null(asesor)) x <- x[x$Asesor == asesor, ]
  ejec <- x %>% dplyr::group_by(CliNitPpal, LinNegCod, Segmento, Fecha) %>%
    dplyr::summarise(Margen = .com_suma(Margen), ConEjecucion = TRUE, .groups = "drop")
  cli <- if (is.null(asesor)) clientes else clientes[clientes$Asesor == asesor, ]
  meses <- seq(as.Date(format(corte, "%Y-01-01")), .gc_mes(corte), by = "month")
  cli <- cli %>% dplyr::group_by(CliNitPpal, LinNegCod, Segmento) %>%
    dplyr::summarise(MNFCCPpto = .com_suma(MNFCCPpto), .groups = "drop")
  plan <- tidyr::crossing(cli, Fecha = meses) %>% dplyr::mutate(PptoMargen = MNFCCPpto / 12)
  dplyr::full_join(plan, ejec, by = c("CliNitPpal", "LinNegCod", "Segmento", "Fecha")) %>%
    dplyr::mutate(Margen = ifelse(is.na(ConEjecucion), 0, Margen),
      Negocio = dplyr::case_when(LinNegCod == 10000 ~ "COPRODUCTO", LinNegCod == 21000 ~ "MEDIDA", TRUE ~ "OTRO"))
}
.bon_legacy_ui <- function(id, fecha = Sys.Date()) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    .crm_nota(c("Información en confirmación.", "Módulo en construcción.")),
    racafeForms::racafeFormRow(columns = 3,
      racafeForms::racafeInputDate("corte", "Mes de consulta", value = fecha, ns = ns),
      .crm_select("asesor", "Asesor", choices = character(), ns = ns),
      .crm_select("esquema", "Esquema de referencia", choices = .BON_ESQUEMAS, ns = ns)),
    .crm_nota(c("Consulta de variables y simulación; no genera pagos ni establece la política vigente.",
      "2026 usa valores provisionales: 70/85/100% de cumplimiento → 33/67/100% del factor; debajo de 70% es cero.",
      "Pesos provisionales de referencia: los de 2025. Deben reemplazarse junto con los umbrales reales.",
      "El legado de Jonathan 2024 tiene dos fórmulas distintas: se muestran ambas sin ocultar la diferencia.",
      "Un periodo sin presupuesto o aún no iniciado no produce una bonificación calculable.",
      "Carlos/Jonathan son perfiles de negocio; Gustavo/Luis evalúan al asesor seleccionado.",
      "Los filtros y permisos pueden limitar la base: el resultado sigue siendo una simulación, no una liquidación.")),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("componentes"), titulo = "Componentes del incentivo"),
    .crm_tabla_ui(ns("mensual"), titulo = "Trazabilidad mensual de ejecución y presupuesto"))
}
.bon_legacy_server <- function(id, hechos, clientes, filtros, historico, cierre) {
  shiny::moduleServer(id, function(input, output, session) {
    corte <- shiny::reactive({ shiny::req(input$corte); .com_fin_mes(as.Date(input$corte)) })
    shiny::observe({
      asesores <- sort(unique(clientes()$Asesor[clientes()$Asesor %in% filtros()$asesor]))
      if (grepl("^2024", .com_elegir(input$esquema, unname(.BON_ESQUEMAS), "2026_carlos"))) {
        h <- historico()
        if (!is.null(h$comercial)) {
          permitidos <- as.character(.com_clientes(clientes(), Sys.Date(), filtros())$CliNitPpal)
          asesores <- sort(unique(h$comercial$Asesor[as.character(h$comercial$CliNitPpal) %in% permitidos]))
        }
      }
      asesores <- asesores[!is.na(asesores)]
      .crm_update_select(session, "asesor", choices = asesores,
        selected = shiny::isolate(input$asesor))
    })
    esquema <- shiny::reactive({ shiny::req(input$esquema %in% unname(.BON_ESQUEMAS)); input$esquema })
    panel <- shiny::reactive({
      shiny::req(input$asesor)
      if (grepl("^2024", esquema())) {
        h <- historico()
        shiny::validate(shiny::need(!is.null(h$comercial), "No hay datos históricos disponibles para este esquema."))
        permitidos <- as.character(.com_clientes(clientes(), Sys.Date(), filtros())$CliNitPpal)
        return(.bon_historico(h, corte(), permitidos, if (grepl("periodos$", esquema())) input$asesor else NULL))
      }
      shiny::validate(shiny::need(.gc_mes(corte()) <= .gc_mes(cierre), "El mes no está cerrado financieramente."))
      cli <- .com_clientes(clientes(), corte(), filtros(), anual = TRUE)
      .bon_panel_crm(hechos(), cli, corte(), if (grepl("periodos$", esquema())) input$asesor else NULL)
    })
    resultado <- shiny::reactive(.bon_calcular(panel(), esquema(), corte()))
    .crm_vancouver_server("componentes", data = resultado,
      col_specs = stats::setNames(lapply(c("Peso", "Cumplimiento", "Factor", "Aporte", "AporteIndicadorAntiguo"),
        function(x) list(format = list(percent = TRUE, digits = 2))),
        c("Peso", "Cumplimiento", "Factor", "Aporte", "AporteIndicadorAntiguo")))
    .crm_vancouver_server("mensual", data = panel)
    .gc_descarga(output, resultado, "Variables_bono")
    list(datos = resultado)
  })
}

.bon_historico <- function(historico, corte, permitidos, asesor = NULL) {
  x <- historico$comercial
  if (is.null(x)) stop("No hay fuente comercial histórica importada.")
  x <- x[as.character(x$CliNitPpal) %in% permitidos, ]
  if (!is.null(asesor)) x <- x[x$Asesor == asesor & !is.na(x$Asesor), ]
  if (!is.null(asesor)) return(x)
  inicio <- as.Date(format(corte, "%Y-01-01"))
  nuevos <- x[!is.na(x$Fecha_Creacion) & as.Date(x$Fecha_Creacion) >= inicio & x$Volumen > 0 & !is.na(x$Volumen), ]
  compras <- historico$compras
  recuperados <- x[0, ]
  if (!is.null(compras)) {
    umbral <- if (as.integer(format(corte, "%Y")) < 2025) 730 else 0
    if (!"CliNitPpal" %in% names(compras)) {
      mapa <- historico$comercial %>% dplyr::distinct(FctNit, CliNitPpal) %>%
        dplyr::group_by(FctNit) %>% dplyr::filter(dplyr::n() == 1L) %>% dplyr::ungroup()
      compras$CliNitPpal <- mapa$CliNitPpal[match(compras$FctNit, mapa$FctNit)]
    }
    compras <- compras[as.character(compras$CliNitPpal) %in% permitidos, , drop = FALSE]
    recuperados <- compras[!is.na(compras$Fecha_Creacion) & as.Date(compras$Fecha_Creacion) < inicio &
      !is.na(compras$Diferencia) & compras$Diferencia >= umbral & compras$PptoMargen == 0, , drop = FALSE]
  }
  objetivos <- historico$comercial %>% dplyr::filter(FctNit %in% c(1, 2)) %>% dplyr::group_by(Fecha) %>%
    dplyr::summarise(PptoMargen = .com_suma(PptoMargen), .groups = "drop")
  ejecucion <- dplyr::bind_rows(nuevos, recuperados) %>% dplyr::group_by(Fecha) %>%
    dplyr::summarise(Margen = .com_suma(Margen), .groups = "drop")
  panel_nuevos <- dplyr::full_join(objetivos, ejecucion, by = "Fecha") %>%
    dplyr::mutate(Negocio = "NUEVOS", Segmento = "TODOS")
  dplyr::bind_rows(x, panel_nuevos)
}

# Vista activa mientras se confirma la política ----
VariablesBonoUI <- function(id, fecha = Sys.Date()) {
  .crm_nota(c("Información en confirmación.", "Módulo en construcción."))
}
VariablesBono <- function(id, hechos, clientes, filtros, historico, cierre) invisible(NULL)
