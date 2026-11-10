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
.in_ficha_identidad <- function(entidad, contexto, contactos, familia, presupuesto, estado_comercial = NULL) {
  estado <- .in_ficha_estado(entidad, contexto, contactos)
  asesores <- .in_ficha_badge_texto(entidad$Asesor)
  es_hija <- !is.na(familia$principal) && !is.na(entidad$NIT) && entidad$NIT != familia$principal
  relacion <- if (es_hija) "Cuenta hija" else if (length(familia$hijos)) "Cuenta padre" else "Cuenta individual"
  revision <- grepl("revisar", presupuesto, ignore.case = TRUE)
  variante <- if (revision) "warning" else if (startsWith(presupuesto, "Con presupuesto")) "info" else "neutral"
  shiny::tags$header(class = "in-ficha-identidad",
    shiny::h4(entidad$Nombre),
    shiny::tags$p(class = "in-ficha-secundario",
      paste(if (entidad$Alcance == "Cuenta principal") "NIT de cuenta principal:" else "NIT de facturación:",
        dplyr::coalesce(entidad$NIT, "Sin dato"), "· Asesor:", asesores)),
    shiny::tags$div(class = "in-ficha-badges",
      racafeForms::racafeStatusBadge(estado$texto, variant = estado$variante),
      racafeForms::racafeDataBadge("Tipo", tools::toTitleCase(tolower(entidad$Tipo)),
        variant = if (entidad$Tipo == "CLIENTE") "info" else "neutral"),
      shiny::tagAppendAttributes(racafeForms::racafeStatusBadge(relacion,
        variant = if (relacion == "Cuenta padre") "danger" else "neutral"), title = familia$relacion),
      racafeForms::racafeStatusBadge(presupuesto, variant = variante), estado_comercial))
}
.in_ficha_valor <- function(id, valor, etiqueta, contexto, icono) {
  caja <- racafeModulos::cajaValor_ui(id, texto = etiqueta, icono = icono, compact = TRUE,
    mostrar_boton = FALSE, mostrar_kpi = FALSE, color_borde = "#dee2e6",
    detalle = shiny::tags$strong(class = "in-ficha-kpi", valor))
  shiny::tags$div(class = "in-ficha-valor", caja, .crm_nota(contexto))
}
.in_ficha_antiguedad_ui <- function(datos, ns = identity) {
  valor <- function(nombre) datos$Informacion[match(nombre, datos$Indicador)]
  racafeForms::racafeMetricGrid(columns = 2L,
    .in_ficha_valor(ns("ficha_edad_registro"), .in_ficha_texto(valor("Antigüedad del registro de cliente")),
      "Antigüedad del registro de cliente", "Tiempo desde el registro del cliente", "calendar-day"),
    .in_ficha_valor(ns("ficha_edad_comercial"), .in_ficha_texto(valor("Antigüedad comercial observada")),
      "Antigüedad comercial observada", "Tiempo desde la primera compra observada", "handshake")) %>%
    shiny::tagList(.in_ficha_info(list("Fecha de registro del cliente" =
      .in_ficha_fecha(valor("Registrado como cliente desde")),
      "Primera compra observada" = .in_ficha_fecha(valor("Primera compra observada")),
      "Última compra observada" = .in_ficha_fecha(valor("Última compra observada")),
      "Primer contacto vinculado al CRM" = .in_ficha_fecha(valor("Primer contacto comercial vinculado en CRM")))))
}
.in_ficha_presupuesto_ui <- function(datos, estado, tabla_ui, ns = identity) {
  if (!nrow(datos)) return(.crm_nota(estado))
  if (nrow(datos) > 1L) return(shiny::tagList(
    .crm_nota("Metas por registro. Cada fila conserva su línea, cuenta y fecha de actualización."), tabla_ui))
  fila <- datos[1, , drop = FALSE]
  revision <- if (is.na(fila$PresupuestoEnRevision)) "Sin dato de revisión" else {
    if (fila$PresupuestoEnRevision) "Requiere revisión" else "No requiere revisión"
  }
  volumen <- .in_ficha_numero(fila$MetaMensualSacos)
  shiny::tagList(racafeForms::racafeMetricGrid(columns = 2L,
    .in_ficha_valor(ns("ficha_meta_volumen"), volumen, "Meta mensual de volumen",
      "Sacos de 70 kg · Cada mes", "weight-hanging"),
    .in_ficha_valor(ns("ficha_meta_margen"), .in_ficha_numero(fila$MetaMensualMargen, moneda = TRUE),
      "Meta mensual de margen", "Margen FCC · Cada mes", "dollar-sign")),
    shiny::tags$div(class = "in-ficha-badges",
      racafeForms::racafeDataBadge("Línea", .in_ficha_badge_texto(.crm_label_comercial(fila$CLLinNegNo))),
      racafeForms::racafeDataBadge("Segmento", .in_ficha_badge_texto(fila$Segmento)),
      racafeForms::racafeStatusBadge(revision,
        variant = if (isTRUE(fila$PresupuestoEnRevision)) "warning" else "neutral")),
    shiny::tags$p(class = "in-ficha-secundario", paste("Actualización:", .in_ficha_fecha(fila$FecProceso))))
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
  asesores <- vapply(x$Asesor, .in_ficha_badge_texto, character(1))
  comunes <- length(unique(asesores)) == 1L
  shiny::tagList(
    if (!nrow(x)) .crm_nota("Sin cuentas relacionadas visibles para los filtros seleccionados."),
    shiny::tags$div(class = "in-ficha-cuentas", lapply(seq_len(nrow(x)), function(i) {
      shiny::tags$div(class = "in-ficha-cuenta",
        shiny::tags$div(class = "in-ficha-cuenta-datos", shiny::tags$strong(.in_ficha_texto(x$Cuenta[[i]])),
          shiny::tags$div(class = "in-ficha-secundario", paste("NIT:", x$NIT[[i]])),
          if (!comunes) shiny::tags$div(class = "in-ficha-secundario", paste("Asesor:", asesores[[i]]))),
        racafeForms::racafeStatusBadge(x$Relacion[[i]],
          variant = if (x$Relacion[[i]] == "Cuenta padre") "danger" else "neutral"))
    })),
    if (comunes) shiny::tags$p(class = "in-ficha-asesor-comun in-ficha-secundario", paste("Asesor:", asesores[[1]])))
}
.in_ficha_complementos <- function(fuentes, contexto) {
  force(contexto)
  lapply(names(.IN_COMPLEMENTOS), function(clave) {
    tryCatch(list(clave = clave, n = nrow(.in_detalle_crm(fuentes[[clave]], contexto,
      .IN_FUENTES[[clave]]$campos)), error = FALSE), error = function(e) list(clave = clave, n = 0L, error = TRUE))
  })
}
.in_ficha_complementos_badge <- function(estados) {
  total <- sum(vapply(estados, function(x) x$n, integer(1)))
  shiny::tagList(racafeForms::racafeStatusBadge(paste(total, "registros"), variant = "neutral"),
    if (any(vapply(estados, function(x) x$error, logical(1)))) {
      racafeForms::racafeStatusBadge("Información parcial", variant = "warning")
    })
}
.in_ficha_complementos_ui <- function(estados, ns) {
  total <- sum(vapply(estados, function(x) x$n, integer(1)))
  fallidos <- vapply(estados, function(x) x$error, logical(1))
  shiny::tagList(
    if (total == 0L && !any(fallidos)) .crm_nota("Sin registros para los filtros seleccionados."),
    lapply(estados, function(x) {
      if (x$error) return(.crm_nota(paste("Información parcial:", .IN_COMPLEMENTOS[[x$clave]],
        "no está disponible. Intente actualizar.")))
      if (x$n > 0L) shiny::tags$section(class = "in-ficha-complemento",
        shiny::tags$div(class = "in-ficha-badges", shiny::tags$strong(.IN_COMPLEMENTOS[[x$clave]]),
          racafeForms::racafeStatusBadge(paste(x$n, "registros"))), .crm_tabla_ui(ns(x$clave)))
    }))
}
# El contador se monta una vez en summary, también con versiones anteriores de racafeShiny.
.in_ficha_caja <- function(ns, clave, titulo, contenido, open = TRUE, badge = NULL) {
  funcion <- racafeShiny::racafeCollapsible
  argumentos <- list(id = ns(paste0("ficha_box_", clave)), title = titulo, open = open,
    contenido, color = "#DA291C", class = "in-ficha-caja")
  nativo <- "header_badge" %in% names(formals(funcion))
  if (nativo) argumentos$header_badge <- badge
  caja <- do.call(funcion, argumentos)
  if (!nativo && !is.null(badge)) caja <- htmltools::tagQuery(caja)$find("summary")$append(
    shiny::tags$span(class = "rsh-collapsible-header-badge", badge))$allTags()
  caja
}
.in_ficha_ui <- function(ns) {
  shiny::tags$div(class = "in-ficha",
    shiny::tags$style(shiny::HTML(".in-ficha-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));
      gap:20px 32px;position:relative;align-items:start;margin-bottom:20px;}
      .in-ficha-grid::before{content:'';position:absolute;width:1px;left:50%;top:0;bottom:0;background:#DA291C;}
      .in-ficha .in-ficha-caja{border:0;border-radius:0;background:transparent;margin:0 0 16px;
        padding:0;min-width:0;box-sizing:border-box;}
      .in-ficha-grid>.in-ficha-caja{margin:0;}.in-ficha-grid>.in-ficha-caja[open]{align-self:stretch;}
      .in-ficha .in-ficha-caja>summary{padding:12px 14px;flex-wrap:wrap;gap:8px;font-size:14px;line-height:1.4;}
      .in-ficha .in-ficha-caja>.rsh-collapsible-content{padding:4px 14px 14px;min-width:0;}
      .in-ficha-identidad{border-bottom:1px solid #DA291C;padding-bottom:10px;margin-bottom:12px;}
      .in-ficha-identidad h4{font-family:inherit;}.in-ficha-secundario{color:#596168;font-size:12px;margin:4px 0;}
      .in-ficha-badges{display:flex;flex-wrap:wrap;align-items:center;gap:6px;margin:8px 0;}
      .in-ficha .rf-status-badge,.in-ficha-identidad .rf-status-badge{width:fit-content;max-width:100%;
        justify-self:start;align-self:center;white-space:normal;flex:0 0 auto;}
      .in-ficha .rf-status-badge--success,.in-ficha-identidad .rf-status-badge--success{background:#e4f3e9;
        color:#21653b;}
      .in-ficha .rf-status-badge--info,.in-ficha-identidad .rf-status-badge--info{background:#e7eff9;color:#31577f;}
      .in-ficha .rf-status-badge--danger,.in-ficha-identidad .rf-status-badge--danger{background:#f9e6e9;
        color:#9b303b;}
      .in-ficha .rf-status-badge--warning,.in-ficha-identidad .rf-status-badge--warning{background:#fff0d2;
        color:#805b12;}
      .in-ficha .rf-status-badge--neutral,.in-ficha-identidad .rf-status-badge--neutral{background:#eef0f2;
        color:#495057;}
      .in-ficha .in-ficha-valor{min-width:0;}
      .in-ficha .in-ficha-kpi{display:block;font-size:1.2rem;line-height:1.35;}
      .in-ficha .in-ficha-valor .rmcv-card{height:auto;}
      .in-ficha .in-ficha-valor>.rf-notice-list{margin-top:6px;}
      .in-ficha .in-ficha-registro .rf-info-grid{grid-template-columns:minmax(0,1fr);gap:2px;margin-bottom:6px;}
      .in-ficha .in-ficha-registro .rf-info-item{display:grid;grid-template-columns:minmax(100px,.4fr) minmax(0,1fr);
        gap:10px;padding:4px 0;min-height:0;}.in-ficha .in-ficha-registro .rf-info-item dt{margin:0;}
      .in-ficha .in-ficha-registro+.in-ficha-registro{border-top:1px solid #dee2e6;margin-top:8px;padding-top:8px;}
      .in-ficha-cuenta{display:flex;justify-content:space-between;gap:12px;align-items:center;padding:9px 0;}
      .in-ficha-cuenta+.in-ficha-cuenta{border-top:1px solid #dee2e6;}
      .in-ficha-cuenta-datos{min-width:0;overflow-wrap:anywhere;}.in-ficha-asesor-comun{margin-top:8px;}
      .in-ficha-complemento+.in-ficha-complemento{border-top:1px solid #dee2e6;margin-top:12px;padding-top:8px;}
      @media(max-width:767px){.in-ficha-grid{grid-template-columns:minmax(0,1fr);gap:14px;}
        .in-ficha-grid::before{display:none;}}")),
    shiny::uiOutput(ns("alcance_crm")),
    shiny::tags$div(class = "in-ficha-grid",
      .in_ficha_caja(ns, "antiguedad", "Antigüedad y relación comercial", shiny::uiOutput(ns("antiguedad-tabla"))),
      .in_ficha_caja(ns, "presupuesto", "Presupuesto y metas", shiny::uiOutput(ns("presupuesto_ficha"))),
      .in_ficha_caja(ns, "contacto", "Contacto y ubicación", shiny::uiOutput(ns("contacto_sistema-tabla"))),
      .in_ficha_caja(ns, "crm", "Registro CRM asociado", shiny::uiOutput(ns("ficha-tabla"))),
    .in_ficha_caja(ns, "familia", "Cuentas relacionadas", shiny::uiOutput(ns("familia-tabla")),
      open = FALSE, badge = shiny::uiOutput(ns("familia_contador"), inline = TRUE)),
    .in_ficha_caja(ns, "complementarios", "Registros complementarios", shiny::uiOutput(ns("complementarios")),
      open = FALSE, badge = shiny::uiOutput(ns("complementarios_contador"), inline = TRUE))))
}
