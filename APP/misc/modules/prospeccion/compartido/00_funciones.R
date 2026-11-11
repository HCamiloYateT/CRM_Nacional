# Lecturas compartidas por sesión; las acciones de escritura conservan su consulta vigente ----
.pr_fuentes <- function() {
  sesion <- shiny::getDefaultReactiveDomain()
  if (is.null(sesion)) return(NULL)
  if (is.null(sesion$userData$prospeccion_fuentes)) {
    sesion$userData$prospeccion_fuentes <- list(revision = shiny::reactiveVal(0L),
      tablas = new.env(parent = emptyenv()))
  }
  sesion$userData$prospeccion_fuentes
}
.pr_cargar <- function(tabla) {
  fuente <- .pr_fuentes()
  if (is.null(fuente)) return(CargarDatos(tabla))
  fuente$revision()
  if (!exists(tabla, envir = fuente$tablas, inherits = FALSE)) fuente$tablas[[tabla]] <- CargarDatos(tabla)
  fuente$tablas[[tabla]]
}
.pr_invalidar <- function(tablas = NULL) {
  fuente <- .pr_fuentes()
  if (is.null(fuente)) return(invisible(NULL))
  quitar <- ls(fuente$tablas, all.names = TRUE)
  if (!is.null(tablas)) quitar <- intersect(quitar, tablas)
  rm(list = quitar, envir = fuente$tablas)
  fuente$revision(shiny::isolate(fuente$revision()) + 1L)
}

# Funciones Embudo ----

## Generales ----
derivar_etapa_actual <- function(contactos = NULL, historial = NULL, clientes_historicos = NULL) {
  if (is.null(contactos)) contactos <- CargarDatos("CRMNALCONTACTO")
  if (!"EtapaPreDescarte" %in% names(contactos)) contactos$EtapaPreDescarte <- NA_character_
  contactos <- .crm_marcas_historia(contactos)
  if (is.null(clientes_historicos)) clientes_historicos <- .pr_historico_clientes()
  nits_clientes <- .crm_nit_historia(clientes_historicos$CliNitPpal)
  if (is.null(historial)) historial <- tryCatch({
    CargarDatos("CRMNALHISTORIALETAPA") %>% mutate(FechaHora = as_datetime(FechaHora))
  }, error = function(e) data.frame(CodContacto = character(), EtapaNueva = character(),
                                    FechaHora = as.POSIXct(character())))
  ultima_transicion <- historial %>%
    group_by(CodContacto) %>%
    filter(!is.na(FechaHora)) %>%
    arrange(desc(FechaHora)) %>%
    slice(1) %>%
    ungroup() %>%
    select(CodContacto, EtapaUltimaTransicion = EtapaNueva)
  contactos %>%
    left_join(ultima_transicion, by = "CodContacto") %>%
    mutate(
      EtapaPreDescarte = ifelse(is.na(EtapaPreDescarte), "CONTACTO", EtapaPreDescarte),
      Etapa = case_when(
        .crm_nit_historia(PerCod) %in% nits_clientes ~ "CLIENTE",
        EsReconstruido ~ "SIN ETAPA OBSERVADA",
        Estado == "DESCARTADO" ~ "DESCARTADO",
        is.na(EtapaUltimaTransicion) ~ "CONTACTO",
        TRUE ~ EtapaUltimaTransicion
      )
    ) %>%
    select(-EtapaUltimaTransicion)
}
# Fecha de entrada a la etapa actual
derivar_fecha_entrada_etapa <- function(contactos_con_etapa, historial = NULL) {
  if (is.null(historial)) historial <- tryCatch({
    CargarDatos("CRMNALHISTORIALETAPA") %>% mutate(FechaHora = as_datetime(FechaHora))
  }, error = function(e) data.frame(CodContacto = character(), EtapaNueva = character(),
                                    FechaHora = as.POSIXct(character())))
  ultima_transicion <- historial %>%
    group_by(CodContacto) %>%
    filter(!is.na(FechaHora)) %>%
    arrange(desc(FechaHora)) %>%
    slice(1) %>%
    ungroup() %>%
    select(CodContacto, FechaUltimaTransicion = FechaHora)
  contactos_con_etapa %>%
    left_join(ultima_transicion, by = "CodContacto") %>%
    mutate(FechaEntradaEtapa = coalesce(FechaUltimaTransicion, as_datetime(FechaHoraCrea))) %>%
    select(-FechaUltimaTransicion)
}
# Clasifica un numero de dias en rangos de antiguedad, para agrupar en tablas/graficos
.rangos_antiguedad <- function(dias) {
  cut(dias, breaks = c(-Inf, 7, 15, 30, 60, Inf),
      labels = c("0-7 días", "8-15 días", "16-30 días", "31-60 días", "+60 días"), right = TRUE)
}
# Grafico de barras horizontales generico (plotly)
.grafico_barras_horizontal <- function(dat, col_label, col_valor, color = "#DA291C", titulo_x = "Conteo") {
  if (nrow(dat) == 0) return(plotly::config(plotly::plotly_empty(type = "bar"), displayModeBar = FALSE))
  dat <- dat[order(dat[[col_valor]]), ]
  p <- plotly::plot_ly(
    dat, x = dat[[col_valor]], y = factor(dat[[col_label]], levels = dat[[col_label]]),
    type = "bar", orientation = "h", marker = list(color = color),
    hovertemplate = paste0("<b>%{y}</b><br>", titulo_x, ": %{x}<extra></extra>")
  ) %>%
    plotly::layout(
      margin = list(l = 10, r = 20, t = 10, b = 30), xaxis = list(title = titulo_x),
      yaxis = list(title = ""), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = "#495057", font = list(color = "white", size = 12))
    )
  plotly::config(p, displayModeBar = FALSE)
}
# Formatea las filas marcadas como "TOTAL" en tablas reactable (negrita + fondo)
.estilo_fila_total <- function(datos_r, col_id) {
  function(value, index) {
    es_total <- identical(as.character(datos_r()[[col_id]][[index]]), "TOTAL")
    if (es_total) list(fontWeight = "bold", background = "#f8f9fa") else list()
  }
}
# Dias desde la ultima gestion
.dias_sin_relacionamiento_bulk <- function(cods, fechas_fallback) {
  if (length(cods) == 0) {
    return(data.frame(CodContacto = character(), DiasSinRelacionamiento = numeric()))
  }
  ultima_gestion <- tryCatch({
    .pr_cargar("CRMNALRELACIONAMIENTO") %>%
      filter(CodContacto %in% cods) %>%
      mutate(FechaHoraCrea = as_datetime(FechaHoraCrea)) %>%
      group_by(CodContacto) %>%
      summarise(UltimaGestion = max(FechaHoraCrea), .groups = "drop")
  }, error = function(e) data.frame(CodContacto = character(), UltimaGestion = as.POSIXct(character())))
  data.frame(CodContacto = cods, FechaFallback = fechas_fallback, stringsAsFactors = FALSE) %>%
    left_join(ultima_gestion, by = "CodContacto") %>%
    mutate(
      UltimaGestion = dplyr::coalesce(UltimaGestion, as_datetime(FechaFallback)),
      DiasSinRelacionamiento = as.numeric(difftime(Sys.time(), UltimaGestion, units = "days"))
    ) %>%
    select(CodContacto, DiasSinRelacionamiento)
}
# Trae todas las alianzas activas
listar_todas_las_alianzas <- function() {
  alianzas <- tryCatch(.pr_cargar("CRMNALPROSPECTOALIANZA"),
                       error = function(e) data.frame(CodContacto = character(), CodClienteAliado = character()))
  if (nrow(alianzas) == 0) return(alianzas %>% mutate(ClienteAliado = character()))
  clientes <- .pr_cargar("CRMNALCONTACTO") %>% select(CodContacto, PerRazSoc, PerCod)
  alianzas %>%
    left_join(clientes, by = c("CodClienteAliado" = "CodContacto")) %>%
    mutate(ClienteAliado = coalesce(PerRazSoc, PerCod, CodClienteAliado)) %>%
    select(-PerRazSoc, -PerCod)
}
# Asigna la razon de descarte
.categoria_motivo_por_etapa <- function(motivo, etapa) {
  unlist(Map(function(m, e) {
    catalogo <- .RAZONES_DESCARTE[[e]] %||% character(0)
    if (is.na(m) || !(m %in% catalogo)) "OTRAS" else m
  }, motivo, etapa), use.names = FALSE)
}
# Ultimo motivo de descarte registrado en el historial
obtener_ultimo_motivo_descarte <- function() {
  tryCatch({
    CargarDatos("CRMNALHISTORIALETAPA") %>%
      filter(EtapaNueva == "DESCARTADO") %>%
      mutate(FechaHora = as_datetime(FechaHora)) %>%
      group_by(CodContacto) %>%
      filter(!is.na(FechaHora)) %>%
    arrange(desc(FechaHora)) %>%
      slice(1) %>%
      ungroup() %>%
      select(CodContacto, Motivo)
  }, error = function(e) data.frame(CodContacto = character(), Motivo = character()))
}
# Acciones por etapa
.acciones_por_etapa <- function(etapa) {
  claves <- unique(c(.ACCIONES_ETAPA_EMBUDO[[etapa]] %||% character(0), "Consultar"))
  setNames(vapply(claves, function(clave) .CONFIG_ACCIONES_EMBUDO[[clave]]$etiqueta, character(1)), claves)
}

## CrearContacto ----
# Genera el consecutivo diario CT-AAAAMMDD-### para un nuevo contacto
generar_codigo_contacto <- function() {
  hoy <- format(Sys.Date(), "%Y%m%d")
  prefijo <- paste0("CT-", hoy, "-")
  existentes <- tryCatch({
    CargarDatos("CRMNALCONTACTO") %>%
      filter(str_starts(CodContacto, prefijo)) %>%
      pull(CodContacto)
  }, error = function(e) character(0))
  if (length(existentes) == 0) {
    consecutivo <- 1
  } else {
    consecutivo <- existentes %>%
      str_remove(prefijo) %>%
      as.integer() %>%
      max(na.rm = TRUE) %>%
      {. + 1}
  }
  paste0(prefijo, formatC(consecutivo, width = 3, flag = "0"))
}

## Panel ----
# Separa "NIT - Razón Social" en sus dos componentes
.separar_nit_razon_social <- function(texto_combinado) {
  nit <- sub("^(\\d+)\\s-\\s.*$", "\\1", texto_combinado)
  nit[!grepl("^\\d+$", nit)] <- NA_character_
  razon_social <- sub("^\\d+\\s-\\s", "", texto_combinado)
  razon_social[is.na(nit)] <- NA_character_
  data.frame(NitPrincipal = nit, RazonSocial = razon_social, stringsAsFactors = FALSE)
}
# Columnas extra a mostrar en la tabla del panel
.columnas_extra_etapa <- function(etapa) {
  switch(
    etapa,
    "PROSPECTO" = c("NumAlianzas", "AliadosTexto"),
    "LEAD" = c("Segmento", "LinNegocio", "AsesorLead", "TiempoConversionDias", "GestionesPrevias",
               "GestionesLead"),
    "CLIENTE" = c("DetOrigen", "NitFacturacion", "TiempoContactoCliente", "GestionesContactoCliente",
                  "TiempoLeadCliente", "GestionesLeadCliente", "GestionesCliente"),
    "CONTACTO" = c("UsuarioCrea", "FechaHoraCrea", "DiasSinGestion", "DiasSinRelacionamiento", "DetOrigen"),
    character(0)
  )
}
# Definiciones reactable de las columnas extra por etapa
.coldefs_extra_etapa <- function(etapa) {
  switch(
    etapa,
    "PROSPECTO" = list(
      NumAlianzas = reactable::colDef(name = "Alianzas", minWidth = 80),
      AliadosTexto = reactable::colDef(name = "Aliado(s)", minWidth = 160)
    ),
    "LEAD" = list(
      Segmento = reactable::colDef(name = "Segmento", minWidth = 110),
      LinNegocio = reactable::colDef(name = "Línea de Negocio", minWidth = 130),
      AsesorLead = reactable::colDef(name = "Asesor", minWidth = 110),
      TiempoConversionDias = reactable::colDef(name = "Conversión Contacto → Lead (días)", minWidth = 180),
      GestionesPrevias = reactable::colDef(name = "Gestiones Previas", minWidth = 120),
      GestionesLead = reactable::colDef(name = "Gestiones como Lead", minWidth = 130)
    ),
    "CLIENTE" = list(
      DetOrigen = reactable::colDef(name = "Detalle Origen", minWidth = 130),
      NitFacturacion = reactable::colDef(name = "NIT Facturación", minWidth = 130),
      TiempoContactoCliente = reactable::colDef(name = "Contacto → Cliente (días)", minWidth = 165),
      GestionesContactoCliente = reactable::colDef(name = "Gestiones Contacto → Cliente", minWidth = 175),
      TiempoLeadCliente = reactable::colDef(name = "Lead → Cliente (días)", minWidth = 155),
      GestionesLeadCliente = reactable::colDef(name = "Gestiones Lead → Cliente", minWidth = 165),
      GestionesCliente = reactable::colDef(name = "Gestiones como Cliente", minWidth = 150)
    ),
    "CONTACTO" = list(
      UsuarioCrea = reactable::colDef(name = "Creado por", minWidth = 100),
      FechaHoraCrea = reactable::colDef(name = "Fecha Creación", minWidth = 130,
                                        format = reactable::colFormat(datetime = TRUE)),
      DiasSinGestion = reactable::colDef(name = "Antigüedad (días)", minWidth = 110,
                                         cell = function(v) round(v, 0)),
      DiasSinRelacionamiento = reactable::colDef(name = "Días sin Relac.", minWidth = 110,
                                                 cell = function(v) round(v, 0)),
      DetOrigen = reactable::colDef(name = "Detalle Origen", minWidth = 130,
                                    cell = function(v) if (is.na(v)) "" else v)
    ),
    list()
  )
}
# Boton circular por accion dentro del panel desplegable de acciones

# Celda de acciones (dropdown flotante) del panel
.celda_dropdown_acciones <- function(codigo, acciones, ns) {
  menu <- racafeModulos::MenuAccionesUI(codigo, acciones, config = .CONFIG_ACCIONES_EMBUDO, ns = ns)
  menu$children[[1]] <- tagAppendAttributes(menu$children[[1]], class = "pr-action-trigger")
  menu$children[[2]] <- tagAppendAttributes(menu$children[[2]], class = "pr-action-panel")
  as.character(menu)
}
# Definicion reactable de la columna de acciones (dropdown)
.coldef_dropdown_acciones <- function(acciones, ns) {
  reactable::colDef(name = "", width = 45, minWidth = 45, maxWidth = 45, html = TRUE,
                    sortable = FALSE, searchable = FALSE, align = "center",
                    style = list(background = "#fff", border = "none", padding = "0"),
                    headerStyle = list(background = "#fff", border = "none", padding = "0"),
                    cell = function(value) .celda_dropdown_acciones(value, acciones, ns))
}

## DetalleContacto ----
# Especificaciones de columna tablas
.col_specs_resumen_contacto <- function(dimension, nombre, estilo) {
  specs <- list(
    reactable::colDef(name = nombre, minWidth = 120, style = estilo),
    reactable::colDef(name = "Núm Contactos", minWidth = 100, style = estilo),
    reactable::colDef(name = "% de Contactos", minWidth = 90,
                      cell = function(v) paste0(v, "%"), style = estilo),
    reactable::colDef(name = "Antiguedad (días)", minWidth = 140, style = estilo),
    reactable::colDef(name = "Dias sin relacionamiento", minWidth = 140, style = estilo)
  )
  names(specs) <- c(dimension, "NumContactos", "Pct", "AntiguedadPromDias", "DiasSinRelacionamientoProm")
  specs
}
# Resumen (conteo, antiguedad prom., dias sin relacionamiento prom.) por dimension, con fila TOTAL
.resumen_dimension <- function(datos, columna_dim, etiqueta_na = "SIN DATO") {
  resumen <- datos %>%
    mutate(Dim = ifelse(is.na(.data[[columna_dim]]) | .data[[columna_dim]] == "",
                        etiqueta_na, .data[[columna_dim]])) %>%
    group_by(Dim) %>%
    summarise(
      NumContactos = n(),
      AntiguedadPromDias = round(mean(DiasSinGestion, na.rm = TRUE), 1),
      DiasSinRelacionamientoProm = round(mean(DiasSinRelacionamiento, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(Pct = round(NumContactos / sum(NumContactos) * 100, 1)) %>%
    relocate(Pct, .after = NumContactos) %>%
    arrange(desc(NumContactos))
  total <- tibble::tibble(
    Dim = "TOTAL",
    NumContactos = nrow(datos),
    AntiguedadPromDias = round(mean(datos$DiasSinGestion, na.rm = TRUE), 1),
    DiasSinRelacionamientoProm = round(mean(datos$DiasSinRelacionamiento, na.rm = TRUE), 1),
    Pct = 100
  )
  bind_rows(resumen, total) %>% rename(!!columna_dim := Dim)
}

## DetalleProspecto ----
# Especificaciones de columna tablas
.col_specs_resumen_prospecto <- function(dimension, nombre, estilo) {
  specs <- list(
    reactable::colDef(name = nombre, minWidth = 120, style = estilo),
    reactable::colDef(name = "Núm Prospectos", minWidth = 100, style = estilo),
    reactable::colDef(name = "% Prospectos", minWidth = 90,
                      cell = function(v) paste0(v, "%"), style = estilo),
    reactable::colDef(name = "Conversión Contacto → Prospecto (días)", minWidth = 180, style = estilo),
    reactable::colDef(name = "Antigüedad como Prospecto (días)", minWidth = 180, style = estilo)
  )
  names(specs) <- c(dimension, "NumProspectos", "Pct", "TiempoConversionProm", "DiasEnEtapaProm")
  specs
}
# Resumen (conteo, dias en etapa prom.) por dimension, con fila TOTAL
.resumen_dimension_prospecto <- function(datos, columna_dim, etiqueta_na = "SIN DATO") {
  resumen <- datos %>%
    mutate(Dim = ifelse(is.na(.data[[columna_dim]]) | .data[[columna_dim]] == "",
                        etiqueta_na, .data[[columna_dim]])) %>%
    group_by(Dim) %>%
    summarise(
      NumProspectos = n(),
      Pct = round(NumProspectos / nrow(datos) * 100, 1),
      TiempoConversionProm = round(mean(TiempoConversion, na.rm = TRUE), 1),
      DiasEnEtapaProm = round(mean(DiasEnEtapa, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(NumProspectos))
  total <- tibble::tibble(
    Dim = "TOTAL",
    NumProspectos = nrow(datos),
    Pct = 100,
    TiempoConversionProm = round(mean(datos$TiempoConversion, na.rm = TRUE), 1),
    DiasEnEtapaProm = round(mean(datos$DiasEnEtapa, na.rm = TRUE), 1)
  )
  bind_rows(resumen, total) %>% rename(!!columna_dim := Dim)
}

## DetalleLeads ----
.resumen_dimension_lead <- function(datos, columna_dim, etiqueta_na = "SIN DATO") {
  total_leads <- sum(datos$EsLead, na.rm = TRUE)
  resumen <- datos %>%
    mutate(Dim = ifelse(is.na(.data[[columna_dim]]) | .data[[columna_dim]] == "",
                        etiqueta_na, .data[[columna_dim]])) %>%
    group_by(Dim) %>%
    summarise(
      NumContactos = n(),
      NumLeads = sum(EsLead, na.rm = TRUE),
      TiempoConversionProm = round(mean(TiempoConversionDias[EsLead], na.rm = TRUE), 1),
      GestionesPreviasProm = round(mean(GestionesPrevias[EsLead], na.rm = TRUE), 1),
      GestionesLeadProm = round(mean(GestionesLead[EsLead], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(
      PctLeads = round(NumLeads / total_leads * 100, 1),
      TasaConversion = round(NumLeads / NumContactos * 100, 1)
    ) %>%
    arrange(desc(NumLeads))
  total <- tibble::tibble(
    Dim = "TOTAL",
    NumContactos = nrow(datos),
    NumLeads = total_leads,
    TiempoConversionProm = round(mean(datos$TiempoConversionDias[datos$EsLead], na.rm = TRUE), 1),
    GestionesPreviasProm = round(mean(datos$GestionesPrevias[datos$EsLead], na.rm = TRUE), 1),
    GestionesLeadProm = round(mean(datos$GestionesLead[datos$EsLead], na.rm = TRUE), 1),
    PctLeads = 100,
    TasaConversion = round(total_leads / nrow(datos) * 100, 1)
  )
  bind_rows(resumen, total) %>% rename(!!columna_dim := Dim)
}
.resumen_dimension_lead_convertido <- function(datos, columna_dim, etiqueta_na = "SIN DATO") {
  datos <- datos %>% filter(EsLead)
  total_leads <- nrow(datos)
  resumen <- datos %>%
    mutate(Dim = ifelse(is.na(.data[[columna_dim]]) | .data[[columna_dim]] == "",
                        etiqueta_na, .data[[columna_dim]])) %>%
    group_by(Dim) %>%
    summarise(
      NumLeads = n(),
      TiempoConversionProm = round(mean(TiempoConversionDias, na.rm = TRUE), 1),
      GestionesPreviasProm = round(mean(GestionesPrevias, na.rm = TRUE), 1),
      GestionesLeadProm = round(mean(GestionesLead, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(PctLeads = round(NumLeads / total_leads * 100, 1)) %>%
    arrange(desc(NumLeads))
  total <- tibble::tibble(
    Dim = "TOTAL",
    NumLeads = total_leads,
    TiempoConversionProm = round(mean(datos$TiempoConversionDias, na.rm = TRUE), 1),
    GestionesPreviasProm = round(mean(datos$GestionesPrevias, na.rm = TRUE), 1),
    GestionesLeadProm = round(mean(datos$GestionesLead, na.rm = TRUE), 1),
    PctLeads = 100
  )
  bind_rows(resumen, total) %>% rename(!!columna_dim := Dim)
}
.col_specs_resumen_lead <- function(dimension, nombre, estilo) {
  specs <- list(
    reactable::colDef(name = nombre, minWidth = 120, style = estilo),
    reactable::colDef(name = "Núm Contactos", minWidth = 100, style = estilo),
    reactable::colDef(name = "Núm Leads", minWidth = 90, style = estilo),
    reactable::colDef(name = "% Leads", minWidth = 80,
                      cell = function(v) paste0(v, "%"), style = estilo),
    reactable::colDef(name = "Tasa de Conversión", minWidth = 120,
                      cell = function(v) paste0(v, "%"), style = estilo),
    reactable::colDef(name = "Conversión Contacto → Lead (días)", minWidth = 180, style = estilo),
    reactable::colDef(name = "Gestiones Previas", minWidth = 120, style = estilo),
    reactable::colDef(name = "Gestiones como Lead", minWidth = 130, style = estilo)
  )
  names(specs) <- c(dimension, "NumContactos", "NumLeads", "PctLeads", "TasaConversion",
                    "TiempoConversionProm", "GestionesPreviasProm", "GestionesLeadProm")
  specs
}
.col_specs_resumen_lead_convertido <- function(dimension, nombre, estilo) {
  specs <- list(
    reactable::colDef(name = nombre, minWidth = 120, style = estilo),
    reactable::colDef(name = "Núm Leads", minWidth = 90, style = estilo),
    reactable::colDef(name = "% Leads", minWidth = 80,
                      cell = function(v) paste0(v, "%"), style = estilo),
    reactable::colDef(name = "Conversión Contacto → Lead (días)", minWidth = 180, style = estilo),
    reactable::colDef(name = "Gestiones Previas", minWidth = 120, style = estilo),
    reactable::colDef(name = "Gestiones como Lead", minWidth = 130, style = estilo)
  )
  names(specs) <- c(dimension, "NumLeads", "PctLeads", "TiempoConversionProm",
                    "GestionesPreviasProm", "GestionesLeadProm")
  specs
}
.resumen_cohorte_lead <- function(datos) {
  total_leads <- sum(datos$EsLead, na.rm = TRUE)
  resumen <- datos %>%
    group_by(Cohorte) %>%
    summarise(
      NumContactos = n(),
      NumLeads = sum(EsLead, na.rm = TRUE),
      TiempoConversionProm = round(mean(TiempoConversionDias[EsLead], na.rm = TRUE), 1),
      GestionesPreviasProm = round(mean(GestionesPrevias[EsLead], na.rm = TRUE), 1),
      GestionesLeadProm = round(mean(GestionesLead[EsLead], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(
      PctLeads = round(NumLeads / total_leads * 100, 1),
      TasaConversion = round(NumLeads / NumContactos * 100, 1)
    ) %>%
    arrange(desc(Cohorte))
  total <- tibble::tibble(
    Cohorte = "TOTAL",
    NumContactos = nrow(datos),
    NumLeads = total_leads,
    TiempoConversionProm = round(mean(datos$TiempoConversionDias[datos$EsLead], na.rm = TRUE), 1),
    GestionesPreviasProm = round(mean(datos$GestionesPrevias[datos$EsLead], na.rm = TRUE), 1),
    GestionesLeadProm = round(mean(datos$GestionesLead[datos$EsLead], na.rm = TRUE), 1),
    PctLeads = 100,
    TasaConversion = round(total_leads / nrow(datos) * 100, 1)
  )
  bind_rows(resumen, total)
}
.mes_conversion_rel <- function(fecha_creacion, fecha_conversion) {
  pmax(0L,
       (lubridate::year(fecha_conversion) - lubridate::year(fecha_creacion)) * 12L +
         lubridate::month(fecha_conversion) - lubridate::month(fecha_creacion))
}
.factor_tiempo_embudo <- function(unidad) {
  switch(unidad, dias = 1, semanas = 7, meses = 30.4375, 1)
}

.etiqueta_tiempo_embudo <- function(unidad) {
  switch(unidad, dias = "Día", semanas = "Semana", meses = "Mes")
}

.factor_tiempo_lead <- function(unidad) .factor_tiempo_embudo(unidad)
.titulo_tiempo_lead <- function(unidad) {
  switch(unidad,
         dias = "Días desde creación del Contacto",
         semanas = "Semanas desde creación del Contacto",
         meses = "Meses desde creación del Contacto")
}
.etiqueta_tiempo_lead <- function(unidad) .etiqueta_tiempo_embudo(unidad)

## Detalle Descartados ----
.pct_vs_total <- function(descartados, total) {
  if (total > 0) round(descartados / total * 100, 1) else rep(0, length(descartados))
}
.resumen_top_n_otros_por_etapa <- function(dat, col_categoria, top_n = .TOP_N_RAZONES_DESCARTE) {
  total <- nrow(dat)
  dat$.Categoria <- ifelse(is.na(dat[[col_categoria]]) | trimws(dat[[col_categoria]]) == "",
                           "SIN DATO", dat[[col_categoria]])
  
  conteo <- dat %>%
    count(.Categoria, Etapa, name = "n") %>%
    tidyr::pivot_wider(names_from = Etapa, values_from = n, values_fill = 0)
  
  # Asegura que las 4 columnas de etapa existan siempre, aunque esta
  # categoria no tenga descartes registrados en alguna de ellas
  for (et in c("CONTACTO", "PROSPECTO", "LEAD", "CLIENTE")) {
    if (!et %in% names(conteo)) conteo[[et]] <- 0L
  }
  
  conteo <- conteo %>%
    rename(Categoria = .Categoria, DescContacto = CONTACTO, DescProspecto = PROSPECTO,
           DescLead = LEAD, DescCliente = CLIENTE) %>%
    mutate(Descartados = DescContacto + DescProspecto + DescLead + DescCliente) %>%
    arrange(desc(Descartados))
  
  if (nrow(conteo) > top_n) {
    top   <- conteo %>% slice_head(n = top_n)
    resto <- conteo %>% slice_tail(n = -top_n)
    otros <- tibble::tibble(
      Categoria = paste0("OTROS (", nrow(resto), ")"), DescContacto = sum(resto$DescContacto),
      DescProspecto = sum(resto$DescProspecto), DescLead = sum(resto$DescLead),
      DescCliente = sum(resto$DescCliente), Descartados = sum(resto$Descartados)
    )
    tabla_visible <- bind_rows(top, otros)
    detalle_otros <- resto %>% mutate(Pct = .pct_vs_total(Descartados, total))
  } else {
    tabla_visible <- conteo
    detalle_otros <- NULL
  }
  
  # Pct siempre relativo al total real (incluye lo agrupado en OTROS),
  # nunca al subconjunto visible - asi la suma de porcentajes cuadra
  tabla_visible <- tabla_visible %>% mutate(Pct = .pct_vs_total(Descartados, total))
  fila_total <- tibble::tibble(
    Categoria = "TOTAL", DescContacto = sum(conteo$DescContacto), DescProspecto = sum(conteo$DescProspecto),
    DescLead = sum(conteo$DescLead), DescCliente = sum(conteo$DescCliente), Descartados = total,
    Pct = ifelse(total > 0, 100, 0)
  )
  
  list(tabla = bind_rows(tabla_visible, fila_total), detalle_otros = detalle_otros)
}
.col_specs_resumen_etapa <- function(estilo, nombre_categoria) {
  list(
    Categoria     = reactable::colDef(name = nombre_categoria, minWidth = 220, style = estilo),
    Descartados   = reactable::colDef(name = "Total", minWidth = 80, style = estilo),
    Pct           = reactable::colDef(name = "%", minWidth = 60, cell = function(v) paste0(v, "%"), style = estilo),
    DescContacto  = reactable::colDef(name = "Contacto", minWidth = 90, style = estilo),
    DescProspecto = reactable::colDef(name = "Prospecto", minWidth = 90, style = estilo),
    DescLead      = reactable::colDef(name = "Lead", minWidth = 80, style = estilo),
    DescCliente   = reactable::colDef(name = "Cliente", minWidth = 80, style = estilo)
  )
}
## Kanban ----
# Semaforo de gestion segun dias sin gestion (ok / warning / critical)
.semaforo_gestion <- function(dias) {
  dplyr::case_when(dias >= 30 ~ "critical", dias >= 15 ~ "warning", TRUE ~ "ok")
}
# Badge HTML con icono y dias, coloreado segun .semaforo_gestion()
.html_badge_gestion <- function(dias) {
  estado <- .semaforo_gestion(dias)
  icono <- switch(estado, critical = "fas fa-exclamation-triangle",
                  warning = "fas fa-clock", "fas fa-check-circle")
  as.character(tags$span(
    class = paste0("kpb-sla kpb-sla-", estado), title = paste0(dias, " días sin gestión"),
    tags$i(class = icono), paste0(" ", dias, "d")
  ))
}
# Tarjeta de KPI del embudo (valor + label) (sin uso detectado hoy; revisar)
.kpi_embudo <- function(valor, label, clase_extra = "") {
  div(class = trimws(paste0("kpb-kpi ", clase_extra)),
      div(class = "kpb-kpi-valor", as.character(valor)),
      div(class = "kpb-kpi-label", label))
}
# Boton de accion sobre una tarjeta del kanban
.btn_kanban <- function(cod_contacto, accion) {
  cfg <- .CONFIG_ACCIONES_EMBUDO[[accion]] %||% list(etiqueta = accion, icono = "ellipsis", color = "#C11007")
  tags$button(
    class = "kpb-btn-accion", title = cfg$etiqueta,
    `data-cod-contacto` = cod_contacto, `data-kanban-action` = accion,
    tags$i(class = paste0("fas fa-", cfg$icono), style = paste0("color:", cfg$color, "; font-size:.65rem;"))
  )
}
# Carga el pipeline completo del embudo (todas las etapas) para el kanban
cargar_pipeline_embudo <- function(cargar = .pr_cargar) {
  contactos <- .crm_normalizar_fechas_historia(cargar("CRMNALCONTACTO"))
  historial <- cargar("CRMNALHISTORIALETAPA") %>% mutate(FechaHora = as_datetime(FechaHora, tz = "America/Bogota"))
  contactos <- derivar_etapa_actual(contactos, historial) %>% derivar_fecha_entrada_etapa(historial)
  leads <- cargar("CONTACTOLEAD") %>% mutate(FechaConversion = as_datetime(FechaConversion, tz = "America/Bogota")) %>%
    arrange(desc(FechaConversion)) %>%
    distinct(CodContacto, .keep_all = TRUE) %>%
    select(CodContacto, Asesor, Segmento, LinNegocio, FechaConversionLead = FechaConversion)
  alianzas <- cargar("CRMNALPROSPECTOALIANZA")
  nombres <- setNames(as.character(contactos$PerRazSoc), as.character(contactos$CodContacto))
  por_nit <- setNames(as.character(contactos$PerRazSoc), .crm_nit_historia(contactos$PerCod))
  aliados <- as.character(alianzas$CodClienteAliado)
  nits <- setNames(.crm_nit_historia(contactos$PerCod), as.character(contactos$CodContacto))
  alianzas$NitAliado <- unname(nits[aliados])
  faltante <- is.na(alianzas$NitAliado)
  alianzas$NitAliado[faltante] <- .crm_nit_historia(sub("[[:space:]]*[-|].*$", "", aliados[faltante]))
  nombre_nit <- unname(por_nit[alianzas$NitAliado])
  nombre_libre <- sub("^[0-9]+[[:space:]]*[-|][[:space:]]*", "", aliados)
  nombre_libre[!is.na(.crm_nit_historia(nombre_libre))] <- "ALIADO SIN NOMBRE REGISTRADO"
  alianzas <- alianzas %>% mutate(ClienteAliado = coalesce(unname(nombres[as.character(CodClienteAliado)]),
    nombre_nit, nombre_libre)) %>% group_by(CodContacto) %>%
    summarise(NumAlianzas = n(), Alianzas = paste(unique(ClienteAliado), collapse = " | "),
      NitsAliados = paste(unique(stats::na.omit(NitAliado)), collapse = " | "), .groups = "drop")
  vinculos <- cargar("CRMNALVINCULONIT") %>% group_by(CodContacto) %>%
    summarise(NitVinculado = if (n_distinct(NitVinculado, na.rm = TRUE) == 1L) {
      first(na.omit(as.character(NitVinculado)))
    } else NA_character_,
      .groups = "drop")
  descartes <- cargar("CRMNALDESCARTE") %>%
    mutate(FechaHoraModi = as_datetime(FechaHoraModi, tz = "America/Bogota")) %>%
    arrange(desc(FechaHoraModi)) %>% distinct(CodContacto, .keep_all = TRUE) %>%
    select(CodContacto, Razon1, Razon2, Razon3, FechaDescarte = FechaHoraModi)
  resultado <- contactos %>% left_join(leads, by = "CodContacto") %>% left_join(alianzas, by = "CodContacto") %>%
    left_join(vinculos, by = "CodContacto") %>% left_join(descartes, by = "CodContacto") %>%
    mutate(NumAlianzas = coalesce(NumAlianzas, 0L),
      DiasEnEtapa = as.numeric(difftime(Sys.time(), FechaEntradaEtapa, units = "days")),
      EstadoGestion = .semaforo_gestion(DiasEnEtapa))
  if ("Motivo" %in% names(historial)) {
    motivos <- historial %>% filter(EtapaNueva == "DESCARTADO") %>% arrange(desc(FechaHora)) %>%
      distinct(CodContacto, .keep_all = TRUE)
    faltantes <- is.na(resultado$Razon1) | !nzchar(trimws(as.character(resultado$Razon1)))
    indices <- match(resultado$CodContacto[faltantes], motivos$CodContacto)
    resultado$Razon1[faltantes] <- as.character(motivos$Motivo[indices])
  }
  resultado$FechaDescarte <- dplyr::coalesce(resultado$FechaDescarte, resultado$FechaEntradaEtapa)
  for (campo in c("Asesor", "Segmento", "LinNegocio")) {
    resultado[[campo]] <- .gc_columna(resultado, c(campo, paste0(campo, ".y"), paste0(campo, ".x")))
  }
  resultado
}
# Acciones disponibles en el kanban, segun etapa del contacto
.acciones_por_etapa_kanban <- function(etapa) {
  unique(c(names(.acciones_por_etapa(etapa)), "Consultar"))
}
# Renderiza la tarjeta HTML de un contacto en el kanban, segun su etapa
.render_tarjeta_embudo <- function(fila, ns = identity) {
  texto <- function(campo, defecto = "SIN DATO") {
    x <- if (campo == "PerCod") .crm_nit_historia(fila[[campo]]) else as.character(fila[[campo]])
    if (!length(x) || is.na(x) || !nzchar(x)) defecto else .crm_label_comercial(x)
  }
  fecha <- function(campo) {
    x <- fila[[campo]]
    if (!length(x) || is.na(x)) return("SIN FECHA")
    if (inherits(x, "POSIXt")) format(x, "%d/%m/%Y", tz = "America/Bogota") else format(as.Date(x), "%d/%m/%Y")
  }
  solo_consulta <- isTRUE(fila$EsSoloFacturacion)
  acciones <- if (solo_consulta) character() else .acciones_por_etapa(fila$Etapa)
  acciones["Consultar"] <- "Consulta individual"
  config <- c(.CONFIG_ACCIONES_EMBUDO, list(Consultar = list(icono = "address-card", color = "#495057")))
  menu <- racafeModulos::MenuAccionesUI(as.character(fila$CodContacto), acciones, config = config, ns = ns)
  menu$children[[1]] <- shiny::tagAppendAttributes(menu$children[[1]], `aria-label` = "Acciones", class = "pr-action-trigger")
  menu$children[[2]] <- shiny::tagAppendAttributes(menu$children[[2]], class = "kpb-actions-panel pr-action-panel")
  codigo <- if ("CodigoCRM" %in% names(fila)) as.character(fila$CodigoCRM) else as.character(fila$CodContacto)
  nit <- texto("PerCod", "")
  menu <- htmltools::tagQuery(menu)$find(".rm-action-option")$each(function(boton, i) {
    if (!identical(boton$attribs$`data-rm-action`, "Consultar")) return(boton)
    boton$attribs$`data-crm-nit` <- nit
    boton$attribs$`data-rm-code` <- if (length(codigo) && !is.na(codigo)) codigo else ""
    boton
  })$allTags()
  lineas <- if ("DetalleLineas" %in% names(fila)) fila$DetalleLineas[[1]] else list()
  tabla_cliente <- if (length(lineas)) tags$table(class = "kb-cliente-tabla",
    tags$thead(tags$tr(tags$th("INDICADOR"), lapply(lineas, function(x) tags$th(x$Linea)))),
    tags$tbody(lapply(list(
      c("SEGMENTO", "Segmento"), c("ÚLTIMA COMPRA", "UltimaCompra"),
      c("SACOS ACUM.", "Sacos"), c("PPTO. SACOS ACUM.", "PptoSacos"),
      c("MARGEN ACUM. · COP", "Margen"), c("PPTO. MARGEN ACUM. · COP", "PptoMargen")), function(campo) {
        tags$tr(tags$th(scope = "row", campo[[1]]), lapply(lineas, function(x) {
          valor <- x[[campo[[2]]]]
          if (inherits(valor, "Date")) valor <- format(valor, "%d/%m/%Y")
          if (is.numeric(valor)) valor <- formatC(valor, format = "f", digits = 1, big.mark = ",")
          tags$td(valor)
        }))
      }))) else .crm_nota("SIN COMPRAS PARA EL ALCANCE SELECCIONADO.")
  campos <- switch(fila$Etapa,
    CONTACTO = racafeForms::racafeInfoItem("Creación", fecha("FechaHoraCrea")),
    PROSPECTO = tagList(racafeForms::racafeInfoItem("Aliado", texto("Alianzas", "SIN ALIANZAS")),
      racafeForms::racafeInfoItem("Origen", texto("Origen"))),
    LEAD = tagList(racafeForms::racafeInfoItem("Segmento", texto("Segmento")),
      racafeForms::racafeInfoItem("Ingreso a LEAD", fecha("FechaConversionLead"))),
    CLIENTE = tabla_cliente,
    DESCARTADO = tagList(
      if (texto("Razon2", "") != "") racafeForms::racafeInfoItem("Detalle", texto("Razon2")),
      if (texto("Razon3", "") != "") racafeForms::racafeInfoItem("Observación", texto("Razon3")),
      racafeForms::racafeInfoItem("Descarte", fecha("FechaDescarte"))))
  contexto <- switch(fila$Etapa,
    CONTACTO = paste("ORIGEN:", texto("Origen")),
    PROSPECTO = paste("NIT ALIADO:", texto("NitsAliados", "SIN NIT REGISTRADO")),
    LEAD = texto("LinNegocio"), CLIENTE = paste("NIT:", nit),
    DESCARTADO = paste("DE", texto("EtapaPreDescarte"), "·", texto("Razon1", "SIN MOTIVO REGISTRADO")))
  creador <- fila$Etapa == "CONTACTO" || texto("Asesor", "") == ""
  responsables <- if (creador) texto("UsuarioCrea", "SIN REGISTRO") else texto("Asesor", "SIN ASESOR")
  rol <- if (creador) "CREADOR: " else "ASESOR: "
  destinos <- if (solo_consulta) character() else .kb_destinos(fila$Etapa, texto("EtapaPreDescarte", "CONTACTO"))
  div(class = "kpb-card kpb-card-compact", draggable = if (solo_consulta) "false" else "true",
    `data-cod-contacto` = fila$CodContacto, `data-kb-source` = fila$Etapa,
    `data-kb-targets` = paste(destinos, collapse = " "),
    `data-kb-search` = toupper(paste(unlist(fila[intersect(c("PerRazSoc", "PerCod", "Asesor", "Segmento",
      "LinNegocio", "Origen", "Alianzas", "Razon1", "Razon2", "Razon3"), names(fila))]), collapse = " ")),
    style = paste0("border-left:3px solid ", COLOR_ETAPA_EMBUDO[[fila$Etapa]], ";"),
    tags$details(tags$summary(tags$strong(texto("PerRazSoc", "SIN RAZÓN SOCIAL")),
      tags$span(class = "kpb-responsables", lapply(strsplit(responsables, " | ", fixed = TRUE)[[1]],
        function(x) racafeForms::racafeStatusBadge(paste0(rol, x), "neutral", size = "xs"))),
      tags$span(class = "kb-contexto", title = contexto, contexto)),
      div(class = "kpb-card-body", if (fila$Etapa != "CLIENTE") racafeForms::racafeInfoItem("NIT", nit), campos)),
    div(class = "kpb-card-menu", menu))
}
# Detecta Leads con primera factura y los convierte a Cliente, registrando la transicion
detectar_conversion_leads <- function(usr = "SISTEMA", facturas = fact_cliente, codigos = NULL) {
  leads_activos <- derivar_etapa_actual() %>% filter(Etapa == "LEAD") %>% select(CodContacto, PerCod)
  if (!is.null(codigos)) leads_activos <- leads_activos %>% filter(CodContacto %in% as.character(codigos))
  if (!nrow(leads_activos)) return(invisible(0L))
  ya_convertidos <- tryCatch(CargarDatos("CRMNALLEADCLIENTE")$CodContacto, error = function(e) character(0))
  leads_activos <- leads_activos %>% filter(!CodContacto %in% ya_convertidos)
  if (nrow(leads_activos) == 0) return(invisible(0))
  vinculos <- tryCatch(CargarDatos("CRMNALVINCULONIT"),
                       error = function(e) data.frame(CodContacto = character(), NitVinculado = character()))
  leads_activos <- leads_activos %>%
    left_join(vinculos %>% select(CodContacto, NitVinculado), by = "CodContacto") %>%
    mutate(NitEfectivo = suppressWarnings(as.numeric(ifelse(!is.na(NitVinculado), NitVinculado, PerCod)))) %>%
    filter(!is.na(NitEfectivo))
  # La facturación por NIT conserva la primera compra, independientemente del cierre PyG.
  primera_factura_por_nit <- facturas %>%
    transmute(CLCliNit = suppressWarnings(as.numeric(FctNit)), FechaConversion = as.Date(MinFecFact)) %>%
    filter(!is.na(CLCliNit), !is.na(FechaConversion), is.finite(as.numeric(FechaConversion))) %>%
    group_by(CLCliNit) %>%
    summarise(FechaConversion = min(FechaConversion), .groups = "drop")
  convertidos <- leads_activos %>% inner_join(primera_factura_por_nit, by = c("NitEfectivo" = "CLCliNit"))
  for (i in seq_len(nrow(convertidos))) {
    fila <- data.frame(CodContacto = convertidos$CodContacto[i], NitFacturacion = convertidos$NitEfectivo[i],
                       FechaConversion = convertidos$FechaConversion[i], stringsAsFactors = FALSE)
    AgregarDatos(fila, "CRMNALLEADCLIENTE")
    registrar_transicion_etapa(convertidos$CodContacto[i], "LEAD", "CLIENTE", usr,
                               motivo = paste("Factura detectada NIT", convertidos$NitEfectivo[i]))
  }
  invisible(nrow(convertidos))
}

## DetalleCliente ----
.obtener_conversion_cliente <- function(codigos = NULL) {
  dat <- .pr_cargar("CRMNALLEADCLIENTE") %>% mutate(FechaConversion = as_datetime(FechaConversion))
  if (!is.null(codigos)) dat <- dat %>% filter(CodContacto %in% codigos)
  dat %>%
    filter(!is.na(FechaConversion)) %>%
    group_by(CodContacto) %>%
    summarise(
      FechaConversionCliente = min(FechaConversion, na.rm = TRUE),
      NitFacturacion = paste(sort(unique(na.omit(as.character(NitFacturacion)))), collapse = ", "),
      .groups = "drop"
    )
}
.obtener_conversion_lead <- function(codigos = NULL) {
  dat <- .pr_cargar("CONTACTOLEAD") %>% mutate(FechaConversion = as_datetime(FechaConversion))
  if (!is.null(codigos)) dat <- dat %>% filter(CodContacto %in% codigos)
  dat %>%
    filter(!is.na(FechaConversion)) %>%
    arrange(FechaConversion) %>%
    distinct(CodContacto, .keep_all = TRUE) %>%
    transmute(CodContacto, Segmento, LinNegocio, AsesorLead = Asesor, FechaConversionLead = FechaConversion)
}
.badge_etapa_cliente <- function(value) {
  etapa_actual <- if (is.na(value) || value == "") "SIN ETAPA" else value
  colores <- c(CONTACTO = "#64748B", PROSPECTO = "#C8862A", LEAD = "#DA291C",
               CLIENTE = "#198754", DESCARTADO = "#C11007")
  color <- colores[[etapa_actual]] %||% "#64748B"
  tags$span(
    style = paste0("display:inline-block;padding:2px 8px;border-radius:10px;",
                   "font-size:0.72rem;font-weight:600;white-space:nowrap;",
                   "color:#FFFFFF;background:", color, ";"),
    etapa_actual
  )
}
.factor_tiempo_cliente <- function(unidad) .factor_tiempo_embudo(unidad)
.etiqueta_tiempo_cliente <- function(unidad) .etiqueta_tiempo_embudo(unidad)
.titulo_tiempo_cliente <- function(unidad, tipo) {
  if (identical(tipo, "contacto_cliente")) {
    switch(unidad,
           dias = "Días desde creación del Contacto",
           semanas = "Semanas desde creación del Contacto",
           meses = "Meses desde creación del Contacto")
  } else {
    switch(unidad,
           dias = "Días desde promoción a Lead",
           semanas = "Semanas desde promoción a Lead",
           meses = "Meses desde promoción a Lead")
  }
}
.resumen_dimension_cliente <- function(datos, columna_dim, etiqueta_na = "SIN DATO") {
  base <- datos %>%
    mutate(Dim = ifelse(is.na(.data[[columna_dim]]) | .data[[columna_dim]] == "",
                        etiqueta_na, as.character(.data[[columna_dim]])))
  resumen <- base %>%
    group_by(Dim) %>%
    summarise(
      NumContactos = n(),
      NumLeads = sum(EsLead, na.rm = TRUE),
      NumClientes = sum(EsCliente, na.rm = TRUE),
      TiempoContactoClienteProm = round(mean(TiempoDiasContacto[EsCliente], na.rm = TRUE), 1),
      GestionesContactoClienteProm = round(mean(GestionesContactoCliente[EsCliente], na.rm = TRUE), 1),
      TiempoLeadClienteProm = round(mean(TiempoDiasLead[EsCliente & EsLead], na.rm = TRUE), 1),
      GestionesLeadClienteProm = round(mean(GestionesLeadCliente[EsCliente & EsLead], na.rm = TRUE), 1),
      GestionesClienteProm = round(mean(GestionesCliente[EsCliente], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(
      ConversionContactoCliente = round(ifelse(NumContactos > 0, NumClientes / NumContactos * 100, 0), 1),
      ConversionLeadCliente = round(ifelse(NumLeads > 0, NumClientes / NumLeads * 100, 0), 1)
    ) %>%
    arrange(desc(NumClientes))
  total <- tibble::tibble(
    Dim = "TOTAL",
    NumContactos = nrow(base),
    NumLeads = sum(base$EsLead, na.rm = TRUE),
    NumClientes = sum(base$EsCliente, na.rm = TRUE),
    TiempoContactoClienteProm = round(mean(base$TiempoDiasContacto[base$EsCliente], na.rm = TRUE), 1),
    GestionesContactoClienteProm = round(mean(base$GestionesContactoCliente[base$EsCliente], na.rm = TRUE), 1),
    TiempoLeadClienteProm = round(mean(base$TiempoDiasLead[base$EsCliente & base$EsLead], na.rm = TRUE), 1),
    GestionesLeadClienteProm = round(
      mean(base$GestionesLeadCliente[base$EsCliente & base$EsLead], na.rm = TRUE), 1
    ),
    GestionesClienteProm = round(mean(base$GestionesCliente[base$EsCliente], na.rm = TRUE), 1)
  ) %>%
    mutate(
      ConversionContactoCliente = round(ifelse(NumContactos > 0, NumClientes / NumContactos * 100, 0), 1),
      ConversionLeadCliente = round(ifelse(NumLeads > 0, NumClientes / NumLeads * 100, 0), 1)
    )
  bind_rows(resumen, total) %>% rename(!!columna_dim := Dim)
}
.resumen_dimension_cliente_convertido <- function(datos, columna_dim, etiqueta_na = "SIN DATO") {
  base <- datos %>%
    distinct(CodContacto, .keep_all = TRUE) %>%
    mutate(Dim = ifelse(is.na(.data[[columna_dim]]) | .data[[columna_dim]] == "",
                        etiqueta_na, as.character(.data[[columna_dim]])))
  resumen <- base %>%
    group_by(Dim) %>%
    summarise(
      NumLeads = n_distinct(CodContacto[EsLead]),
      NumClientes = n_distinct(CodContacto[EsCliente]),
      TiempoLeadClienteProm = round(mean(TiempoDiasLead[EsCliente & EsLead], na.rm = TRUE), 1),
      GestionesLeadClienteProm = round(mean(GestionesLeadCliente[EsCliente & EsLead], na.rm = TRUE), 1),
      GestionesClienteProm = round(mean(GestionesCliente[EsCliente], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(NumClientes))
  total_clientes <- sum(resumen$NumClientes, na.rm = TRUE)
  total_leads <- sum(resumen$NumLeads, na.rm = TRUE)
  resumen <- resumen %>%
    mutate(
      PctClientes = ifelse(total_clientes > 0, NumClientes / total_clientes, 0),
      ConversionLeadCliente = ifelse(NumLeads > 0, NumClientes / NumLeads, NA_real_)
    )
  total <- tibble::tibble(
    Dim = "TOTAL",
    NumLeads = total_leads,
    NumClientes = total_clientes,
    PctClientes = ifelse(total_clientes > 0, 1, 0),
    ConversionLeadCliente = ifelse(total_leads > 0, total_clientes / total_leads, NA_real_),
    TiempoLeadClienteProm = round(mean(base$TiempoDiasLead[base$EsCliente & base$EsLead], na.rm = TRUE), 1),
    GestionesLeadClienteProm = round(
      mean(base$GestionesLeadCliente[base$EsCliente & base$EsLead], na.rm = TRUE), 1
    ),
    GestionesClienteProm = round(mean(base$GestionesCliente[base$EsCliente], na.rm = TRUE), 1)
  )
  bind_rows(resumen, total) %>% rename(!!columna_dim := Dim)
}
.col_specs_resumen_cliente <- function(dimension, nombre, estilo) {
  specs <- list(
    reactable::colDef(name = nombre, minWidth = 120, style = estilo),
    reactable::colDef(name = "Núm Contactos", minWidth = 100, style = estilo),
    reactable::colDef(name = "Núm Leads", minWidth = 90, style = estilo),
    reactable::colDef(name = "Núm Clientes", minWidth = 100, style = estilo),
    reactable::colDef(name = "Conv. Contacto → Cliente", minWidth = 150,
                      cell = function(v) ifelse(is.na(v), "", paste0(v, "%")), style = estilo),
    reactable::colDef(name = "Contacto → Cliente (días)", minWidth = 160, style = estilo),
    reactable::colDef(name = "Gestiones Contacto → Cliente", minWidth = 175, style = estilo),
    reactable::colDef(name = "Conv. Lead → Cliente", minWidth = 140,
                      cell = function(v) ifelse(is.na(v), "", paste0(v, "%")), style = estilo),
    reactable::colDef(name = "Lead → Cliente (días)", minWidth = 150, style = estilo),
    reactable::colDef(name = "Gestiones Lead → Cliente", minWidth = 165, style = estilo),
    reactable::colDef(name = "Gestiones como Cliente", minWidth = 150, style = estilo)
  )
  names(specs) <- c(dimension, "NumContactos", "NumLeads", "NumClientes", "ConversionContactoCliente",
                    "TiempoContactoClienteProm", "GestionesContactoClienteProm", "ConversionLeadCliente",
                    "TiempoLeadClienteProm", "GestionesLeadClienteProm", "GestionesClienteProm")
  specs
}
.col_specs_resumen_cliente_convertido <- function(dimension, nombre, estilo) {
  specs <- list(
    reactable::colDef(name = nombre, minWidth = 120, style = estilo),
    reactable::colDef(name = "Núm Leads", minWidth = 90, style = estilo),
    reactable::colDef(name = "Núm Clientes", minWidth = 100, style = estilo),
    reactable::colDef(name = "% de Clientes", minWidth = 100,
                      cell = function(v) scales::percent(v, accuracy = 0.1, decimal.mark = SEP_DECIMAL), style = estilo),
    reactable::colDef(name = "Conversión Lead → Cliente", minWidth = 155,
                      cell = function(v) scales::percent(v, accuracy = 0.1, decimal.mark = SEP_DECIMAL), style = estilo),
    reactable::colDef(name = "Tiempo Lead → Cliente (días)", minWidth = 175, style = estilo),
    reactable::colDef(name = "Gestiones Lead → Cliente", minWidth = 165, style = estilo),
    reactable::colDef(name = "Gestiones como Cliente", minWidth = 150, style = estilo)
  )
  names(specs) <- c(dimension, "NumLeads", "NumClientes", "PctClientes", "ConversionLeadCliente",
                    "TiempoLeadClienteProm", "GestionesLeadClienteProm", "GestionesClienteProm")
  specs
}
.resumen_cohorte_cliente <- function(datos, tipo) {
  if (identical(tipo, "contacto_cliente")) {
    base <- datos %>%
      distinct(CodContacto, .keep_all = TRUE) %>%
      filter(!is.na(CohorteContacto))
    resumen <- base %>%
      group_by(Cohorte = CohorteContacto) %>%
      summarise(
        NumContactos = n_distinct(CodContacto),
        NumClientes = n_distinct(CodContacto[EsCliente]),
        TiempoConversionProm = round(mean(TiempoDiasContacto[EsCliente], na.rm = TRUE), 1),
        GestionesPreviasProm = round(mean(GestionesContactoCliente[EsCliente], na.rm = TRUE), 1),
        GestionesClienteProm = round(mean(GestionesCliente[EsCliente], na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(TasaConversion = round(ifelse(NumContactos > 0, NumClientes / NumContactos * 100, 0), 1)) %>%
      arrange(desc(Cohorte))
    total <- tibble::tibble(
      Cohorte = "TOTAL",
      NumContactos = n_distinct(base$CodContacto),
      NumClientes = n_distinct(base$CodContacto[base$EsCliente]),
      TasaConversion = round(
        ifelse(n_distinct(base$CodContacto) > 0,
               n_distinct(base$CodContacto[base$EsCliente]) / n_distinct(base$CodContacto) * 100, 0),
        1
      ),
      TiempoConversionProm = round(mean(base$TiempoDiasContacto[base$EsCliente], na.rm = TRUE), 1),
      GestionesPreviasProm = round(mean(base$GestionesContactoCliente[base$EsCliente], na.rm = TRUE), 1),
      GestionesClienteProm = round(mean(base$GestionesCliente[base$EsCliente], na.rm = TRUE), 1)
    )
  } else {
    base <- datos %>%
      filter(EsLead) %>%
      distinct(CodContacto, .keep_all = TRUE) %>%
      filter(!is.na(CohorteLead))
    resumen <- base %>%
      group_by(Cohorte = CohorteLead) %>%
      summarise(
        NumLeads = n_distinct(CodContacto),
        NumClientes = n_distinct(CodContacto[EsCliente]),
        TiempoConversionProm = round(mean(TiempoDiasLead[EsCliente], na.rm = TRUE), 1),
        GestionesPreviasProm = round(mean(GestionesLeadCliente[EsCliente], na.rm = TRUE), 1),
        GestionesClienteProm = round(mean(GestionesCliente[EsCliente], na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(TasaConversion = round(ifelse(NumLeads > 0, NumClientes / NumLeads * 100, 0), 1)) %>%
      arrange(desc(Cohorte))
    total <- tibble::tibble(
      Cohorte = "TOTAL",
      NumLeads = n_distinct(base$CodContacto),
      NumClientes = n_distinct(base$CodContacto[base$EsCliente]),
      TasaConversion = round(
        ifelse(n_distinct(base$CodContacto) > 0,
               n_distinct(base$CodContacto[base$EsCliente]) / n_distinct(base$CodContacto) * 100, 0),
        1
      ),
      TiempoConversionProm = round(mean(base$TiempoDiasLead[base$EsCliente], na.rm = TRUE), 1),
      GestionesPreviasProm = round(mean(base$GestionesLeadCliente[base$EsCliente], na.rm = TRUE), 1),
      GestionesClienteProm = round(mean(base$GestionesCliente[base$EsCliente], na.rm = TRUE), 1)
    )
  }
  bind_rows(resumen, total)
}
## Embudo Conversion ----
# Sanea NA/NaN/Inf a 0 o
.valor_seguro <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0 || !is.finite(x)) return(0)
  round(x, digits)
}
# Carga una tabla y retorna una estructura vacia si falla
.cargar_embudo_seguro <- function(tabla, estructura) {
  tryCatch(
    CargarDatos(tabla),
    error = function(e) estructura
  )
}
# Extrae un conteo desde una tabla agrupada
.obtener_conteo_embudo <- function(dat, columna, valor) {
  resultado <- dat %>%
    filter(.data[[columna]] == valor) %>%
    pull(n)
  
  if (length(resultado) == 0) 0L else resultado[[1]]
}
# Calcula las metricas generales usando la estructura vigente de CRMNALDESCARTE.
calcular_metricas_embudo <- function() {
  base_contactos <- .crm_marcas_historia(CargarDatos("CRMNALCONTACTO"))
  contactos_actuales <- derivar_etapa_actual(base_contactos)
  contactos <- contactos_actuales
  contactos_conversion <- filtrar_historia_comercial_real(base_contactos)
  
  historial <- .cargar_embudo_seguro(
    "CRMNALHISTORIALETAPA",
    data.frame(
      CodContacto = character(),
      EtapaAnterior = character(),
      EtapaNueva = character(),
      Motivo = character(),
      FechaHora = as.POSIXct(character())
    )
  ) %>%
    mutate(FechaHora = as_datetime(FechaHora))
  
  vinculos <- .cargar_embudo_seguro(
    "CRMNALVINCULONIT",
    data.frame(CodContacto = character())
  )
  
  leads_dat <- .cargar_embudo_seguro(
    "CONTACTOLEAD",
    data.frame(
      CodContacto = character(),
      Asesor = character(),
      FechaConversion = as.POSIXct(character())
    )
  )
  
  clientes_dat <- .cargar_embudo_seguro(
    "CRMNALLEADCLIENTE",
    data.frame(
      CodContacto = character(),
      FechaConversion = as.POSIXct(character())
    )
  )
  
  descartes_dat <- .cargar_embudo_seguro(
    "CRMNALDESCARTE",
    data.frame(
      CodContacto = character(),
      Etapa = character(),
      Razon1 = character(),
      Razon2 = character(),
      Razon3 = character(),
      UsuarioMod = character(),
      FechaHoraModi = as.POSIXct(character())
    )
  ) %>%
    mutate(FechaHoraModi = as_datetime(FechaHoraModi))
  
  # Resume el volumen vigente por etapa.
  volumen_etapa <- contactos %>%
    count(Etapa, name = "n") %>%
    tidyr::complete(
      Etapa = ETAPAS_EMBUDO,
      fill = list(n = 0L)
    )
  
  total_contactos <- .obtener_conteo_embudo(
    volumen_etapa, "Etapa", "CONTACTO"
  )
  total_leads <- .obtener_conteo_embudo(
    volumen_etapa, "Etapa", "LEAD"
  )
  total_prospectos <- .obtener_conteo_embudo(
    volumen_etapa, "Etapa", "PROSPECTO"
  )
  total_clientes <- .obtener_conteo_embudo(
    volumen_etapa, "Etapa", "CLIENTE"
  )
  total_descartados <- .obtener_conteo_embudo(
    volumen_etapa, "Etapa", "DESCARTADO"
  )
  
  # Resume los descartados vigentes por etapa previa.
  descartes_etapa <- contactos %>%
    filter(Etapa == "DESCARTADO") %>%
    count(EtapaPreDescarte, name = "n")
  
  descartados_contacto <- .obtener_conteo_embudo(
    descartes_etapa, "EtapaPreDescarte", "CONTACTO"
  )
  descartados_lead <- .obtener_conteo_embudo(
    descartes_etapa, "EtapaPreDescarte", "LEAD"
  )
  descartados_prospecto <- .obtener_conteo_embudo(
    descartes_etapa, "EtapaPreDescarte", "PROSPECTO"
  )
  
  historial <- filtrar_historia_comercial_real(historial) %>%
    filter(CodContacto %in% contactos_conversion$CodContacto)
  leads_dat <- leads_dat %>% filter(CodContacto %in% contactos_conversion$CodContacto)

  # Identifica las etapas alcanzadas historicamente.
  historico_etapas <- bind_rows(
    historial %>%
      select(CodContacto, EtapaNueva, FechaHora),
    contactos_conversion %>%
      transmute(
        CodContacto,
        EtapaNueva = "CONTACTO",
        FechaHora = as_datetime(FechaHoraCrea)
      )
  ) %>%
    filter(!is.na(CodContacto), !is.na(EtapaNueva))
  
  codigos_contacto <- unique(contactos_conversion$CodContacto)
  codigos_lead <- historico_etapas %>%
    filter(EtapaNueva == "LEAD") %>%
    pull(CodContacto) %>%
    unique()
  codigos_prospecto <- historico_etapas %>%
    filter(EtapaNueva == "PROSPECTO") %>%
    pull(CodContacto) %>%
    unique()
  codigos_cliente <- historico_etapas %>%
    filter(EtapaNueva == "CLIENTE") %>%
    pull(CodContacto) %>%
    unique()
  
  # Calcula las tasas historicas de conversion.
  tasa_contacto_lead <- if (length(codigos_contacto) > 0) {
    length(intersect(codigos_contacto, codigos_lead)) /
      length(codigos_contacto)
  } else 0
  
  tasa_lead_cliente <- if (length(codigos_lead) > 0) {
    length(intersect(codigos_lead, codigos_cliente)) /
      length(codigos_lead)
  } else 0
  
  tasa_global <- if (length(codigos_contacto) > 0) {
    length(intersect(codigos_contacto, codigos_cliente)) /
      length(codigos_contacto)
  } else 0
  
  tasa_prospecto_a_lead <- if (length(codigos_prospecto) > 0) {
    length(intersect(codigos_prospecto, codigos_lead)) /
      length(codigos_prospecto)
  } else 0
  
  # Calcula las tasas de descarte vigentes por etapa previa.
  tasa_descarte_contacto <- if (
    total_contactos + descartados_contacto > 0
  ) {
    descartados_contacto /
      (total_contactos + descartados_contacto)
  } else 0
  
  tasa_descarte_lead <- if (
    total_leads + descartados_lead > 0
  ) {
    descartados_lead /
      (total_leads + descartados_lead)
  } else 0
  
  tasa_descarte_prospecto <- if (
    total_prospectos + descartados_prospecto > 0
  ) {
    descartados_prospecto /
      (total_prospectos + descartados_prospecto)
  } else 0
  
  # Obtiene las primeras fechas historicas por etapa.
  fechas_etapa <- historico_etapas %>%
    filter(
      EtapaNueva %in% c("LEAD", "CLIENTE")
    ) %>%
    group_by(CodContacto, EtapaNueva) %>%
    summarise(
      Fecha = min(FechaHora, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = EtapaNueva,
      values_from = Fecha
    )
  
  for (etapa in c("LEAD", "CLIENTE")) {
    if (!etapa %in% names(fechas_etapa)) fechas_etapa[[etapa]] <- rep(as.POSIXct(NA), nrow(fechas_etapa))
  }
  tiempos <- contactos_conversion %>%
    select(CodContacto, FechaHoraCrea) %>%
    mutate(FechaHoraCrea = as_datetime(FechaHoraCrea)) %>%
    left_join(fechas_etapa, by = "CodContacto")
  
  tiempo_contacto_lead <- tiempos %>%
    filter(!is.na(LEAD)) %>%
    summarise(
      valor = mean(
        as.numeric(difftime(LEAD, FechaHoraCrea, units = "days")),
        na.rm = TRUE
      )
    ) %>%
    pull(valor)
  
  tiempo_lead_cliente <- tiempos %>%
    filter(!is.na(LEAD), !is.na(CLIENTE)) %>%
    summarise(
      valor = mean(
        as.numeric(difftime(CLIENTE, LEAD, units = "days")),
        na.rm = TRUE
      )
    ) %>%
    pull(valor)
  
  tiempo_ciclo_total <- tiempos %>%
    filter(!is.na(CLIENTE)) %>%
    summarise(
      valor = mean(
        as.numeric(
          difftime(CLIENTE, FechaHoraCrea, units = "days")
        ),
        na.rm = TRUE
      )
    ) %>%
    pull(valor)
  
  tiempo_contacto_lead <- ifelse(
    is.finite(tiempo_contacto_lead),
    round(tiempo_contacto_lead, 1),
    0
  )
  tiempo_lead_cliente <- ifelse(
    is.finite(tiempo_lead_cliente),
    round(tiempo_lead_cliente, 1),
    0
  )
  tiempo_ciclo_total <- ifelse(
    is.finite(tiempo_ciclo_total),
    round(tiempo_ciclo_total, 1),
    0
  )
  
  # Calcula la actividad registrada durante los ultimos 30 dias.
  limite_30d <- Sys.time() - lubridate::days(30)
  
  contactos_30d <- contactos_conversion %>%
    mutate(FechaHoraCrea = as_datetime(FechaHoraCrea)) %>%
    filter(FechaHoraCrea >= limite_30d) %>%
    nrow()
  
  leads_30d <- historico_etapas %>%
    filter(
      EtapaNueva == "LEAD",
      FechaHora >= limite_30d
    ) %>%
    nrow()
  
  clientes_30d <- historico_etapas %>%
    filter(
      EtapaNueva == "CLIENTE",
      FechaHora >= limite_30d
    ) %>%
    nrow()
  
  # Calcula la proporcion de leads con NIT vinculado.
  leads_con_vinculo <- leads_dat %>%
    filter(CodContacto %in% vinculos$CodContacto) %>%
    nrow()
  
  pct_leads_vinculo <- if (nrow(leads_dat) > 0) {
    leads_con_vinculo / nrow(leads_dat)
  } else 0
  
  # Resume la conversion historica por origen.
  conversion_por_canal <- contactos_conversion %>%
    mutate(
      EsLead = CodContacto %in% codigos_lead,
      EsCliente = CodContacto %in% codigos_cliente,
      Origen = ifelse(
        is.na(Origen) | Origen == "",
        "SIN DATO",
        Origen
      )
    ) %>%
    group_by(Origen) %>%
    summarise(
      Contactos = n(),
      Leads = sum(EsLead, na.rm = TRUE),
      Clientes = sum(EsCliente, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      TasaConversion = ifelse(
        Contactos > 0,
        Clientes / Contactos,
        0
      )
    ) %>%
    arrange(
      desc(TasaConversion),
      desc(Clientes),
      desc(Contactos)
    )
  
  # Resume las etapas vigentes atribuibles a cada asesor.
  asesor_etapa <- leads_dat %>%
    distinct(CodContacto, Asesor) %>%
    filter(!is.na(Asesor), Asesor != "") %>%
    left_join(
      contactos %>% select(CodContacto, Etapa),
      by = "CodContacto"
    ) %>%
    filter(Etapa %in% c("LEAD", "CLIENTE", "DESCARTADO"))
  
  conversion_por_asesor <- asesor_etapa %>%
    count(Asesor, Etapa, name = "n") %>%
    tidyr::complete(
      Asesor,
      Etapa = c("LEAD", "CLIENTE", "DESCARTADO"),
      fill = list(n = 0L)
    ) %>%
    pivot_wider(
      names_from = Etapa,
      values_from = n,
      values_fill = 0
    ) %>%
    transmute(
      Asesor,
      Leads = LEAD,
      Clientes = CLIENTE,
      Descartados = DESCARTADO,
      Total = Leads + Clientes + Descartados,
      TasaConversion = ifelse(
        Total > 0,
        Clientes / Total,
        0
      ),
      TasaDescarte = ifelse(
        Total > 0,
        Descartados / Total,
        0
      )
    ) %>%
    arrange(desc(TasaConversion), desc(Total))
  
  # Recupera el ultimo descarte vigente de cada contacto.
  ultimo_descarte <- contactos %>%
    filter(Etapa == "DESCARTADO") %>%
    select(CodContacto, EtapaPreDescarte) %>%
    left_join(
      descartes_dat %>%
        arrange(CodContacto, desc(FechaHoraModi)) %>%
        group_by(CodContacto) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        select(
          CodContacto,
          EtapaDescarte = Etapa,
          Razon1,
          Razon2,
          Razon3,
          UsuarioMod,
          FechaHoraModi
        ),
      by = "CodContacto"
    ) %>%
    mutate(
      EtapaPreDescarte = coalesce(
        EtapaDescarte,
        EtapaPreDescarte,
        "CONTACTO"
      ),
      CategoriaMotivo = coalesce(
        Razon1,
        "SIN DATO"
      )
    ) %>%
    select(
      CodContacto,
      EtapaPreDescarte,
      CategoriaMotivo,
      Razon1,
      Razon2,
      Razon3,
      UsuarioMod,
      FechaHoraModi
    )
  
  # Resume Razon1 entre los descartados vigentes.
  motivos_descarte <- ultimo_descarte %>%
    count(CategoriaMotivo, sort = TRUE)
  
  motivos_por_origen <- ultimo_descarte %>%
    count(EtapaPreDescarte, CategoriaMotivo)
  
  list(
    total_contactos = total_contactos,
    total_leads = total_leads,
    total_prospectos = total_prospectos,
    total_clientes = total_clientes,
    total_descartados = total_descartados,
    descartados_contacto = descartados_contacto,
    descartados_lead = descartados_lead,
    descartados_prospecto = descartados_prospecto,
    tasa_contacto_lead = tasa_contacto_lead,
    tasa_lead_cliente = tasa_lead_cliente,
    tasa_global = tasa_global,
    tasa_descarte_contacto = tasa_descarte_contacto,
    tasa_descarte_lead = tasa_descarte_lead,
    tasa_descarte_prospecto = tasa_descarte_prospecto,
    tasa_prospecto_a_lead = tasa_prospecto_a_lead,
    tiempo_contacto_lead = tiempo_contacto_lead,
    tiempo_lead_cliente = tiempo_lead_cliente,
    tiempo_ciclo_total = tiempo_ciclo_total,
    contactos_30d = contactos_30d,
    leads_30d = leads_30d,
    clientes_30d = clientes_30d,
    leads_con_vinculo = leads_con_vinculo,
    pct_leads_vinculo = pct_leads_vinculo,
    conversion_por_canal = conversion_por_canal,
    conversion_por_asesor = conversion_por_asesor,
    motivos_descarte = motivos_descarte,
    motivos_por_origen = motivos_por_origen,
    ultimo_descarte = ultimo_descarte
  )
}
# Trae la ubicación de cada contacto para el resumen geográfico y el mapa
calcular_georeferenciacion <- function() {
  ubicacion <- .cargar_embudo_seguro(
    "CRMNALUBICACION",
    data.frame(
      CodContacto = character(),
      Pais = character(),
      Depto = character(),
      Mpio = character(),
      Localidad = character(),
      Barrio = character(),
      Direccion = character(),
      lat = double(),
      lng = double(),
      UsuarioMod = character(),
      FechaHoraModi = as.POSIXct(character())
    )
  )
  
  contactos <- CargarDatos("CRMNALCONTACTO") %>%
    select(
      CodContacto,
      PerRazSoc,
      PerCod
    )
  
  dat <- ubicacion %>%
    left_join(
      contactos,
      by = "CodContacto"
    )
  
  resumen_depto <- dat %>%
    filter(
      !is.na(Depto),
      Depto != ""
    ) %>%
    mutate(
      Mpio = ifelse(
        is.na(Mpio) | Mpio == "",
        "SIN DATO",
        Mpio
      )
    ) %>%
    count(
      Depto,
      Mpio,
      sort = TRUE,
      name = "Contactos"
    )
  
  puntos_mapa <- dat %>%
    filter(
      !is.na(lat),
      !is.na(lng)
    )
  
  list(
    resumen_depto = resumen_depto,
    puntos_mapa = puntos_mapa,
    total_con_ubicacion = nrow(dat),
    total_con_coordenadas = nrow(puntos_mapa)
  )
}
# Construye el embudo con un hover del mismo color de cada etapa.
grafico_embudo <- function(metricas) {
  etapas <- c("CONTACTO", "LEAD", "CLIENTE")
  valores <- c(metricas$total_contactos, metricas$total_leads, metricas$total_clientes)
  valores[!is.finite(valores)] <- 0
  plotly::plot_ly(type = "funnel", y = etapas, x = valores,
    marker = list(color = unname(COLOR_ETAPA_EMBUDO[etapas])), textinfo = "value",
    hovertemplate = "<b>%{y}</b><br>Entidades: %{x:,.0f}<extra></extra>") %>%
    plotly::layout(height = 450, margin = list(l = 100, r = 40, t = 20, b = 20),
      yaxis = list(categoryorder = "array", categoryarray = etapas, autorange = "reversed"),
      showlegend = FALSE, hoverlabel = list(bgcolor = "#e9ecef", font = list(color = "#343a40"))) %>%
    plotly::config(displayModeBar = FALSE)
}
# Construye el treemap Etapa > Origen > Detalle Origen > Razon1.
grafico_descartados <- function(dat) {
  if (nrow(dat) == 0) {
    return(
      plotly::config(
        plotly::plotly_empty(type = "treemap"),
        displayModeBar = FALSE
      )
    )
  }
  
  .aclarar_color <- function(color, intensidad) {
    rgb <- grDevices::col2rgb(color)
    nuevo <- rgb + (255 - rgb) * intensidad
    
    grDevices::rgb(
      nuevo[1, ], nuevo[2, ], nuevo[3, ],
      maxColorValue = 255
    )
  }
  
  .escalar_intensidad <- function(x, minimo, maximo) {
    x <- ifelse(is.na(x), 0, x)
    
    if (length(unique(x)) <= 1) {
      return(rep((minimo + maximo) / 2, length(x)))
    }
    
    maximo -
      (x - min(x)) / (max(x) - min(x)) *
      (maximo - minimo)
  }
  
  dat <- dat %>%
    mutate(
      EtapaPreDescarte = coalesce(EtapaPreDescarte, "CONTACTO"),
      Origen = coalesce(Origen, "SIN DATO"),
      DetOrigen = coalesce(DetOrigen, "SIN DATO"),
      Razon1 = coalesce(Razon1, "SIN DATO"),
      ColorBase = unname(COLOR_ETAPA_EMBUDO[EtapaPreDescarte])
    )
  
  # Resume el nivel principal del treemap.
  nodos_etapa <- dat %>%
    group_by(EtapaPreDescarte, ColorBase) %>%
    summarise(
      Descartados = sum(Descartados, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      id = paste0("ETAPA|", EtapaPreDescarte),
      parent = "",
      label = EtapaPreDescarte,
      tipo = "Etapa de Descarte",
      valor = Descartados,
      ColorBase = ColorBase,
      intensidad = 0
    )
  
  # Resume el segundo nivel por origen.
  nodos_origen <- dat %>%
    group_by(EtapaPreDescarte, Origen, ColorBase) %>%
    summarise(
      Descartados = sum(Descartados, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(EtapaPreDescarte) %>%
    mutate(
      intensidad = .escalar_intensidad(
        Descartados, 0.15, 0.35
      )
    ) %>%
    ungroup() %>%
    transmute(
      id = paste0("ORIGEN|", EtapaPreDescarte, "|", Origen),
      parent = paste0("ETAPA|", EtapaPreDescarte),
      label = Origen,
      tipo = "Origen",
      valor = Descartados,
      ColorBase = ColorBase,
      intensidad = intensidad
    )
  
  # Resume el tercer nivel por detalle de origen.
  nodos_det_origen <- dat %>%
    group_by(
      EtapaPreDescarte, Origen, DetOrigen, ColorBase
    ) %>%
    summarise(
      Descartados = sum(Descartados, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(EtapaPreDescarte, Origen) %>%
    mutate(
      intensidad = .escalar_intensidad(
        Descartados, 0.35, 0.55
      )
    ) %>%
    ungroup() %>%
    transmute(
      id = paste0(
        "DET|", EtapaPreDescarte, "|",
        Origen, "|", DetOrigen
      ),
      parent = paste0(
        "ORIGEN|", EtapaPreDescarte, "|", Origen
      ),
      label = DetOrigen,
      tipo = "Detalle Origen",
      valor = Descartados,
      ColorBase = ColorBase,
      intensidad = intensidad
    )
  
  # Resume el ultimo nivel por Razon1.
  nodos_razon <- dat %>%
    group_by(
      EtapaPreDescarte, Origen, DetOrigen,
      Razon1, ColorBase
    ) %>%
    summarise(
      Descartados = sum(Descartados, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(
      EtapaPreDescarte, Origen, DetOrigen
    ) %>%
    mutate(
      intensidad = .escalar_intensidad(
        Descartados, 0.55, 0.75
      )
    ) %>%
    ungroup() %>%
    transmute(
      id = paste0(
        "RAZON|", EtapaPreDescarte, "|",
        Origen, "|", DetOrigen, "|", Razon1
      ),
      parent = paste0(
        "DET|", EtapaPreDescarte, "|",
        Origen, "|", DetOrigen
      ),
      label = Razon1,
      tipo = "Razón de Descarte",
      valor = Descartados,
      ColorBase = ColorBase,
      intensidad = intensidad
    )
  
  nodos <- bind_rows(
    nodos_etapa,
    nodos_origen,
    nodos_det_origen,
    nodos_razon
  ) %>%
    mutate(
      color = mapply(
        .aclarar_color,
        ColorBase,
        intensidad,
        USE.NAMES = FALSE
      ),
      texto_hover = paste0(
        "<b>", label, "</b><br>",
        tipo, "<br>",
        "Descartados: ",
        format(valor, big.mark = ",")
      )
    )
  
  plotly::plot_ly(
    data = nodos,
    type = "treemap",
    ids = ~id,
    labels = ~label,
    parents = ~parent,
    values = ~valor,
    branchvalues = "total",
    textinfo = "label+value",
    text = ~texto_hover,
    hoverinfo = "text",
    marker = list(colors = nodos$color),
    tiling = list(
      packing = "squarify"
    )
  ) %>%
    plotly::layout(
      margin = list(l = 5, r = 5, t = 5, b = 5),
      paper_bgcolor = "rgba(0,0,0,0)"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
# Treemap de conversión por canal
treemap_canal <- function(dat) {
  if (nrow(dat) == 0) {
    return(
      plotly::config(
        plotly::plotly_empty(type = "treemap"),
        displayModeBar = FALSE
      )
    )
  }
  
  dat <- dat %>%
    mutate(
      Etiqueta = ifelse(
        is.na(Origen) | Origen == "",
        "SIN DATO",
        Origen
      ),
      PctTexto = paste0(
        round(
          TasaConversion * 100,
          1
        ),
        "%"
      )
    )
  
  p <- plotly::plot_ly(
    type = "treemap",
    labels = ~dat$Etiqueta,
    parents = rep(
      "",
      nrow(dat)
    ),
    values = ~dat$Contactos,
    customdata = ~dat$PctTexto,
    marker = list(
      colors = ~dat$TasaConversion,
      colorscale = list(c(0, "#e9ecef"), c(1, "#DA291C")),
      showscale = TRUE,
      colorbar = list(
        title = "Tasa de\nconversión"
      )
    ),
    text = ~paste0(
      dat$Contactos,
      " contactos | ",
      dat$Leads,
      " leads | ",
      dat$Clientes,
      " clientes"
    ),
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "%{text}<br>",
      "Tasa de conversión a Cliente: %{customdata}",
      "<extra></extra>"
    )
  ) %>%
    plotly::layout(
      margin = list(
        t = 10,
        l = 10,
        r = 10,
        b = 10
      )
    )
  
  plotly::config(
    p,
    displayModeBar = FALSE
  )
}
# Treemap de motivos de descarte
treemap_motivo <- function(dat) {
  if (nrow(dat) == 0) {
    return(
      plotly::config(
        plotly::plotly_empty(type = "treemap"),
        displayModeBar = FALSE
      )
    )
  }
  
  total <- sum(
    dat$n,
    na.rm = TRUE
  )
  
  dat <- dat %>%
    mutate(
      PctTexto = paste0(
        round(
          n / total * 100,
          1
        ),
        "%"
      )
    )
  
  p <- plotly::plot_ly(
    type = "treemap",
    labels = ~dat$CategoriaMotivo,
    parents = rep(
      "",
      nrow(dat)
    ),
    values = ~dat$n,
    customdata = ~dat$PctTexto,
    marker = list(
      colors = ~dat$n,
      colorscale = "Reds",
      showscale = TRUE,
      colorbar = list(
        title = "Casos"
      )
    ),
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "Casos: %{value}<br>",
      "% del total de descartes: %{customdata}",
      "<extra></extra>"
    )
  ) %>%
    plotly::layout(
      margin = list(
        t = 10,
        l = 10,
        r = 10,
        b = 10
      )
    )
  
  plotly::config(
    p,
    displayModeBar = FALSE
  )
}
# Tokeniza los motivos de descarte en texto libre (categoría OTRAS)
tokenizar_motivos_libres <- function(cod_contacto_vec, historial_dat) {
  catalogos <- unique(
    unlist(
      .RAZONES_DESCARTE,
      use.names = FALSE
    )
  )
  
  textos <- historial_dat %>%
    filter(
      CodContacto %in% cod_contacto_vec,
      !is.na(Motivo),
      nzchar(trimws(Motivo)),
      !(Motivo %in% catalogos)
    ) %>%
    pull(Motivo)
  
  if (length(textos) == 0) {
    return(
      data.frame(
        word = character(),
        freq = integer()
      )
    )
  }
  
  palabras <- textos %>%
    str_to_lower() %>%
    str_replace_all(
      "[^a-záéíóúñ ]",
      " "
    ) %>%
    str_split("\\s+") %>%
    unlist() %>%
    trimws()
  
  palabras <- palabras[
    nchar(palabras) > 2 &
      !(palabras %in% .STOPWORDS_ES)
  ]
  
  if (length(palabras) == 0) {
    return(
      data.frame(
        word = character(),
        freq = integer()
      )
    )
  }
  
  tibble::tibble(
    word = palabras
  ) %>%
    count(
      word,
      name = "freq",
      sort = TRUE
    )
}
# Construye el universo historico asegurando todos los indicadores de etapa.
.construir_universo_historico_embudo <- function(base_contactos, historial, leads_dat) {
  base_contactos <- .crm_marcas_historia(base_contactos)
  historial <- filtrar_historia_comercial_real(historial)
  flags_historicos <- historial %>%
    filter(EtapaNueva %in% ETAPAS_EMBUDO) %>%
    distinct(CodContacto, EtapaNueva) %>%
    mutate(Valor = TRUE) %>%
    pivot_wider(
      names_from = EtapaNueva,
      values_from = Valor,
      values_fill = FALSE
    )
  
  # Garantiza que las columnas existan aunque no haya transiciones.
  for (etapa in c("LEAD", "PROSPECTO", "CLIENTE", "DESCARTADO")) {
    if (!etapa %in% names(flags_historicos)) {
      flags_historicos[[etapa]] <- FALSE
    }
  }
  
  atributos_lead <- leads_dat %>%
    mutate(
      FechaConversion = as_datetime(FechaConversion)
    ) %>%
    arrange(CodContacto, FechaConversion) %>%
    group_by(CodContacto) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  # Garantiza los atributos esperados cuando la tabla viene incompleta.
  for (columna in c("LinNegocio", "Segmento", "Asesor")) {
    if (!columna %in% names(atributos_lead)) {
      atributos_lead[[columna]] <- NA_character_
    }
  }
  
  atributos_lead <- atributos_lead %>%
    select(CodContacto, LinNegocio, Segmento, Asesor)
  
  base_contactos %>%
    select(
      CodContacto,
      UsuarioCrea,
      Origen,
      DetOrigen, EsReconstruido, FuenteRegistro, FechaImputada, IncluirConversion
    ) %>%
    left_join(
      flags_historicos,
      by = "CodContacto"
    ) %>%
    left_join(
      atributos_lead,
      by = "CodContacto"
    ) %>%
    mutate(
      ContactoHistorico = IncluirConversion,
      LeadHistorico = IncluirConversion & coalesce(LEAD, FALSE),
      ProspectoHistorico = IncluirConversion & coalesce(PROSPECTO, FALSE),
      ClienteHistorico = IncluirConversion & coalesce(CLIENTE, FALSE),
      DescartadoHistorico = IncluirConversion & coalesce(DESCARTADO, FALSE),
      UsuarioCrea = coalesce(UsuarioCrea, "SIN DATO"),
      Origen = coalesce(Origen, "SIN DATO"),
      DetOrigen = coalesce(DetOrigen, "SIN DATO"),
      LinNegocio = coalesce(LinNegocio, "SIN DATO"),
      Segmento = coalesce(Segmento, "SIN DATO"),
      Asesor = coalesce(Asesor, "SIN DATO")
    ) %>%
    select(
      CodContacto,
      UsuarioCrea,
      Origen,
      DetOrigen,
      LinNegocio,
      Segmento,
      Asesor,
      ContactoHistorico,
      LeadHistorico,
      ProspectoHistorico,
      ClienteHistorico,
      DescartadoHistorico, EsReconstruido, FuenteRegistro, FechaImputada, IncluirConversion
    )
}
.resumen_dimension_embudo <- function(dat, dimension ) {
  dat <- filtrar_historia_comercial_real(dat)
  resumen <- dat %>%
    group_by(
      Dim = .data[[dimension]]
    ) %>%
    summarise(
      Contactos = n_distinct(
        CodContacto
      ),
      Leads = n_distinct(
        CodContacto[
          LeadHistorico
        ]
      ),
      Prospectos = n_distinct(
        CodContacto[
          ProspectoHistorico
        ]
      ),
      Clientes = n_distinct(
        CodContacto[
          ClienteHistorico
        ]
      ),
      Descartados = n_distinct(
        CodContacto[
          DescartadoHistorico
        ]
      ),
      .groups = "drop"
    ) %>%
    mutate(
      TasaContactoCliente = ifelse(
        Contactos > 0,
        Clientes / Contactos,
        0
      ),
      TasaContactoLead = ifelse(
        Contactos > 0,
        Leads / Contactos,
        0
      ),
      TasaLeadCliente = ifelse(
        Leads > 0,
        Clientes / Leads,
        0
      )
    )
  
  total <- dat %>%
    summarise(
      Dim = "TOTAL",
      Contactos = n_distinct(
        CodContacto
      ),
      Leads = n_distinct(
        CodContacto[
          LeadHistorico
        ]
      ),
      Prospectos = n_distinct(
        CodContacto[
          ProspectoHistorico
        ]
      ),
      Clientes = n_distinct(
        CodContacto[
          ClienteHistorico
        ]
      ),
      Descartados = n_distinct(
        CodContacto[
          DescartadoHistorico
        ]
      )
    ) %>%
    mutate(
      TasaContactoCliente = ifelse(
        Contactos > 0,
        Clientes / Contactos,
        0
      ),
      TasaContactoLead = ifelse(
        Contactos > 0,
        Leads / Contactos,
        0
      ),
      TasaLeadCliente = ifelse(
        Leads > 0,
        Clientes / Leads,
        0
      )
    )
  
  bind_rows(
    resumen,
    total
  )
}
.resumen_descartes_embudo <- function(dat) {
  dat %>%
    mutate(
      Origen = coalesce(
        Origen,
        "SIN DATO"
      ),
      DetOrigen = coalesce(
        DetOrigen,
        "SIN DATO"
      ),
      EtapaPreDescarte = coalesce(
        EtapaPreDescarte,
        "CONTACTO"
      ),
      Razon = coalesce(
        Razon,
        "OTRAS"
      )
    ) %>%
    count(
      Origen,
      DetOrigen,
      EtapaPreDescarte,
      Razon,
      name = "Descartados"
    )
}
.resumen_razones_descarte <- function(dat) {
  etapas <- intersect(
    names(
      .RAZONES_DESCARTE
    ),
    c(
      "CONTACTO",
      "LEAD",
      "PROSPECTO",
      "CLIENTE"
    )
  )
  
  purrr::map_dfr(
    etapas,
    function(etapa) {
      catalogo <- .RAZONES_DESCARTE[[etapa]]
      
      conteos <- dat %>%
        filter(
          EtapaPreDescarte == etapa
        ) %>%
        count(
          Razon,
          name = "Descartados"
        )
      
      salida <- tibble::tibble(
        Etapa = etapa,
        Razon = catalogo
      ) %>%
        left_join(
          conteos,
          by = "Razon"
        ) %>%
        mutate(
          Descartados = coalesce(
            Descartados,
            0L
          )
        )
      
      total <- sum(
        salida$Descartados,
        na.rm = TRUE
      )
      
      salida %>%
        mutate(
          Pct = ifelse(
            total > 0,
            round(
              Descartados /
                total *
                100,
              1
            ),
            0
          )
        )
    }
  )
}

# CRMNALSEGR conserva a quienes facturaron alguna vez; la inactividad no borra esa condición ----
.pr_historico_clientes <- function(cargar = .pr_cargar) {
  x <- cargar("CRMNALSEGR")
  if (!all(c("CliNitPpal", "LinNegCod", "FecProceso") %in% names(x))) {
    stop("CRMNALSEGR no contiene el esquema de clientes históricos esperado.")
  }
  x$CliNitPpal <- .crm_nit_historia(x$CliNitPpal)
  x$FecProceso <- as.Date(x$FecProceso)
  x <- x[!is.na(x$CliNitPpal), , drop = FALSE]
  x <- x %>% arrange(desc(FecProceso)) %>% distinct(CliNitPpal, LinNegCod, .keep_all = TRUE)
  # SegmentoRacafe es actividad (CLIENTE / A RECUPERAR), no el segmento comercial de tamaño.
  catalogo <- .com_clientes(cargar("CRMNALCLIENTE"), Sys.Date())
  indice <- match(paste(x$CliNitPpal, x$LinNegCod),
    paste(.crm_nit_historia(catalogo$CliNitPpal), catalogo$LinNegCod))
  x$Asesor <- as.character(catalogo$Asesor[indice])
  x$Segmento <- as.character(catalogo$Segmento[indice])
  x$PerRazSoc <- as.character(catalogo$PerRazSoc[indice])
  x$CLLinNegNo <- ifelse(x$LinNegCod == 10000, "CONVENCIONALES",
    ifelse(x$LinNegCod == 21000, "A LA MEDIDA", as.character(x$LinNegCod)))
  x
}
