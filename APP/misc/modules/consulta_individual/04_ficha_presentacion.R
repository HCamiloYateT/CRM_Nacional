# Presentación de la ficha: sin modificar fuentes, filtros ni cálculos del módulo ----
.IN_COMPLEMENTOS <- c(directorio = "Directorio empresarial", telefonos = "Teléfonos",
  personas = "Personas de contacto", redes = "Redes sociales", ubicaciones = "Ubicaciones", sucursales = "Sucursales")
.in_ficha_valores <- function(x) {
  x <- unlist(strsplit(as.character(x[!is.na(x)]), "|", fixed = TRUE), use.names = FALSE)
  x <- trimws(x)
  unique(x[!toupper(x) %in% c("", ".", "NA", "SIN DATO", "SIN DATOS", "SIN INFORMACIÓN", "SIN INFORMACION",
    "SIN FECHA REGISTRADA")])
}
.in_ficha_texto <- function(x) {
  x <- .in_ficha_valores(x)
  if (!length(x)) return("Sin dato")
  if (length(x) == 1L) x else shiny::tagList(lapply(x, shiny::tags$div))
}
.in_ficha_badge_texto <- function(x) {
  x <- .in_ficha_valores(x)
  if (!length(x)) "Sin dato" else paste(x, collapse = " · ")
}
.in_ficha_fecha <- function(x) {
  if (!length(x) || is.na(x[[1]])) return("Sin dato")
  if (inherits(x, "Date") || grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(x[[1]]))) {
    d <- .in_fecha(x[[1]])
    return(if (is.na(d)) "Sin dato" else format(d, "%d/%m/%Y"))
  }
  d <- .in_instante(x[[1]])
  if (is.na(d)) "Sin dato" else format(d, "%d/%m/%Y %H:%M:%S", tz = "America/Bogota")
}
.in_ficha_numero <- function(x, moneda = FALSE) {
  if (length(x) != 1L || !is.finite(x)) return("Sin dato")
  digitos <- if (x == trunc(x)) 0L else 2L
  valor <- formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
  if (moneda) paste0("$", valor, " COP") else valor
}
.in_ficha_info <- function(campos, ocultar_vacios = FALSE) {
  vacios <- vapply(campos, function(x) !length(.in_ficha_valores(x)), logical(1))
  visibles <- if (ocultar_vacios) names(campos)[!vacios] else names(campos)
  shiny::tagList(do.call(racafeForms::racafeInfoGrid, c(lapply(visibles, function(nombre) {
    racafeForms::racafeInfoItem(nombre, .in_ficha_texto(campos[[nombre]]), empty = "Sin dato")
  }), list(columns = 2L))),
    if (ocultar_vacios && any(vacios)) .crm_nota(paste("Sin dato:", paste(names(campos)[vacios], collapse = ", "))))
}
.in_ficha_crm_registros <- function(contexto, contactos) {
  contactos[.in_id(.gc_columna(contactos, "CodContacto")) %in% .in_codigos(contexto), , drop = FALSE]
}
.in_ficha_estado <- function(entidad, contexto, contactos) {
  propios <- if (entidad$Alcance == "Cuenta principal") contexto else entidad
  x <- .in_ficha_crm_registros(propios, contactos)
  estados <- unique(toupper(.in_ficha_valores(.gc_columna(x, "Estado"))))
  if (!nrow(x)) return(list(texto = "Sin registro CRM propio", variante = "neutral"))
  if (length(estados) == 1L && estados == "ACTIVO") {
    return(list(texto = "Activo en CRM", variante = "success"))
  }
  list(texto = if (!length(estados)) "Estado CRM sin dato" else paste("Estado CRM:", paste(estados, collapse = " / ")),
    variante = if ("DESCARTADO" %in% estados) "warning" else "neutral")
}
.in_ficha_identidad <- function(entidad, contexto, contactos, familia, presupuesto) {
  estado <- .in_ficha_estado(entidad, contexto, contactos)
  asesores <- .in_ficha_valores(entidad$Asesor)
  shiny::tags$header(class = "in-ficha-identidad",
    shiny::h4(entidad$Nombre),
    shiny::tags$div(class = "in-ficha-badges",
      racafeForms::racafeDataBadge(if (entidad$Alcance == "Cuenta principal") "NIT de cuenta principal" else {
        "NIT de facturación"
      }, dplyr::coalesce(entidad$NIT, "Sin dato")),
      lapply(if (length(asesores)) asesores else "Sin dato", function(asesor) {
        racafeForms::racafeDataBadge("Asesor", asesor)
      })),
    shiny::tags$div(class = "in-ficha-badges",
      racafeForms::racafeStatusBadge(estado$texto, variant = estado$variante),
      racafeForms::racafeDataBadge("Tipo", entidad$Tipo),
      racafeForms::racafeDataBadge("Vista", entidad$Alcance),
      racafeForms::racafeDataBadge("Relación", familia$relacion),
      racafeForms::racafeDataBadge("Presupuesto", presupuesto)))
}
.in_ficha_antiguedad_ui <- function(datos) {
  valor <- function(nombre) datos$Informacion[match(nombre, datos$Indicador)]
  racafeForms::racafeMetricGrid(columns = 2L,
    racafeForms::racafeMetric(.in_ficha_texto(valor("Antigüedad del registro de cliente")),
      "Antigüedad del registro de cliente", padding = "compact"),
    racafeForms::racafeMetric(.in_ficha_texto(valor("Antigüedad comercial observada")),
      "Antigüedad comercial observada", padding = "compact")) %>%
    shiny::tagList(.in_ficha_info(list("Fecha de registro del cliente" =
      .in_ficha_fecha(valor("Registrado como cliente desde")),
      "Primera compra observada" = .in_ficha_fecha(valor("Primera compra observada")),
      "Última compra observada" = .in_ficha_fecha(valor("Última compra observada")),
      "Primer contacto vinculado al CRM" = .in_ficha_fecha(valor("Primer contacto comercial vinculado en CRM")))))
}
.in_ficha_presupuesto_ui <- function(datos, estado, tabla_ui) {
  if (!nrow(datos)) return(.crm_nota(estado))
  if (nrow(datos) > 1L) return(shiny::tagList(
    .crm_nota("Metas por registro. Cada fila conserva su línea, cuenta y fecha de actualización."), tabla_ui))
  fila <- datos[1, , drop = FALSE]
  shiny::tagList(racafeForms::racafeMetricGrid(columns = 2L,
    racafeForms::racafeMetric(.in_ficha_numero(fila$MetaMensualSacos),
      "Meta mensual de volumen", detail = "Sacos equivalentes de 70 kg", padding = "compact"),
    racafeForms::racafeMetric(.in_ficha_numero(fila$MetaMensualMargen, moneda = TRUE),
      "Meta mensual de margen", detail = "Margen FCC en COP", padding = "compact")),
    .in_ficha_info(list("Línea de negocio" = .crm_label_comercial(fila$CLLinNegNo), "Segmento" = fila$Segmento,
      "Estado de revisión" = if (is.na(fila$PresupuestoEnRevision)) "Sin dato" else {
        if (fila$PresupuestoEnRevision) "Requiere revisión" else "Sin alertas de revisión"
      }, "Fecha de actualización" = .in_ficha_fecha(fila$FecProceso))))
}
.in_ficha_crm_ui <- function(datos) {
  if (!nrow(datos)) return(.crm_nota("Sin registro CRM asociado para la selección actual."))
  shiny::tagList(lapply(seq_len(nrow(datos)), function(i) {
    fila <- datos[i, , drop = FALSE]
    campo <- function(n) .gc_columna(fila, n)
    shiny::tags$section(class = "in-ficha-registro",
      .in_ficha_info(list("Código de entidad" = .in_id(campo("CodContacto")),
        "Documento registrado en CRM" = .in_id(campo("PerCod")),
        "Fecha de creación en CRM" = .in_ficha_fecha(campo("FechaHoraCrea")),
        "Origen" = campo("Origen"), "Detalle de origen" = campo("DetOrigen")), ocultar_vacios = TRUE))
  }))
}
.in_ficha_contacto_ui <- function(datos) {
  if (!nrow(datos)) return(.crm_nota("Sin datos de contacto y ubicación para la selección actual."))
  shiny::tagList(lapply(seq_len(nrow(datos)), function(i) {
    fila <- datos[i, , drop = FALSE]
    shiny::tags$section(class = "in-ficha-registro",
      if (nrow(datos) > 1L) shiny::tags$div(class = "in-ficha-badges",
        racafeForms::racafeDataBadge("NIT", fila$NIT),
        racafeForms::racafeStatusBadge(.in_ficha_badge_texto(fila$Cuenta), variant = "neutral")),
      .in_ficha_info(list("Contacto" = fila$Contacto, "Teléfonos" = fila$Telefono, "Correos" = fila$Correo,
        "Direcciones" = fila$Direccion, "Ciudad" = fila$Ciudad, "Departamento" = fila$Departamento,
        "Región" = fila$Region, "País" = fila$Pais), ocultar_vacios = TRUE))
  }))
}
.in_ficha_familia_ui <- function(familia) {
  x <- familia$cuentas
  x <- x[order(x$Relacion != "Cuenta padre", seq_len(nrow(x))), , drop = FALSE]
  shiny::tagList(
    if (!nrow(x)) .crm_nota("Sin cuentas relacionadas visibles para los filtros seleccionados."),
    lapply(seq_len(nrow(x)), function(i) racafeForms::racafePanel(title = NULL, padding = "compact",
      racafeForms::racafeInfoGrid(columns = 4L,
        racafeForms::racafeStatusBadge(x$Relacion[[i]], variant = "neutral"),
        racafeForms::racafeInfoItem("NIT", .in_ficha_texto(x$NIT[[i]])),
        racafeForms::racafeInfoItem("Razón social", .in_ficha_texto(x$Cuenta[[i]])),
        racafeForms::racafeInfoItem("Asesor", .in_ficha_texto(x$Asesor[[i]]))))))
}
.in_ficha_complementos <- function(fuentes, contexto) {
  lapply(names(.IN_COMPLEMENTOS), function(clave) {
    tryCatch(list(clave = clave, n = nrow(.in_detalle_crm(fuentes[[clave]], contexto,
      .IN_FUENTES[[clave]]$campos)), error = FALSE), error = function(e) list(clave = clave, n = 0L, error = TRUE))
  })
}
.in_ficha_complementos_ui <- function(estados, ns) {
  total <- sum(vapply(estados, function(x) x$n, integer(1)))
  fallidos <- vapply(estados, function(x) x$error, logical(1))
  .crm_disclosure(shiny::tagList("Registros complementarios ",
    racafeForms::racafeStatusBadge(paste(total, "registros visibles"), variant = "neutral"),
    if (any(fallidos)) racafeForms::racafeStatusBadge("Información parcial", variant = "warning")),
    if (total == 0L && !any(fallidos)) .crm_nota("Sin registros para los filtros seleccionados."),
    lapply(estados, function(x) {
      if (x$error) return(.crm_nota(paste(.IN_COMPLEMENTOS[[x$clave]], "no está disponible. Intente actualizar.")))
      if (x$n > 0L) .in_panel_ui(x$clave, .IN_COMPLEMENTOS[[x$clave]], ns)
    }), open = FALSE)
}
.in_ficha_ui <- function(ns) {
  shiny::tags$div(class = "in-ficha",
    shiny::tags$style(".in-ficha .rf-grid,.in-ficha .rf-panel{min-width:0;}
      .in-ficha .rf-metric-value{font-size:1.3rem;line-height:1.35;}
      .in-ficha .in-ficha-registro+.in-ficha-registro{border-top:1px solid #dee2e6;margin-top:10px;padding-top:10px;}
      .in-ficha-badges{display:flex;flex-wrap:wrap;align-items:center;gap:8px;margin:8px 0;}
      .in-ficha-identidad .rf-status-badge,.in-ficha .rf-status-badge{max-width:100%;white-space:normal;}
      @media(max-width:767px){.in-ficha>.rf-grid{grid-template-columns:minmax(0,1fr);}}"),
    shiny::uiOutput(ns("alcance_crm")),
    racafeForms::racafeMetricGrid(columns = 2L,
      racafeForms::racafePanel(title = "Antigüedad y relación comercial", shiny::uiOutput(ns("antiguedad-tabla"))),
      racafeForms::racafePanel(title = "Presupuesto y metas", shiny::uiOutput(ns("presupuesto_ficha")))),
    racafeForms::racafeInfoGrid(columns = 2L,
      racafeForms::racafePanel(title = "Contacto y ubicación", shiny::uiOutput(ns("contacto_sistema-tabla"))),
      racafeForms::racafePanel(title = "Registro CRM asociado", shiny::uiOutput(ns("ficha-tabla")))),
    .crm_disclosure(shiny::tagList("Cuentas relacionadas ", shiny::uiOutput(ns("familia_contador"), inline = TRUE)),
      shiny::uiOutput(ns("familia-tabla")), open = TRUE),
    shiny::uiOutput(ns("complementarios")))
}
