# Notas compactas fuera de cajas y tablas ----
.crm_nota <- function(texto, title = NULL, swatches = NULL, ...) {
  if (is.null(texto)) return(NULL)
  if (inherits(texto, c("shiny.tag", "shiny.tag.list"))) texto <- list(texto)
  if (is.character(texto)) {
    html <- inherits(texto, "html")
    texto <- unlist(strsplit(texto, "\n", perl = TRUE))
    if (html) texto <- lapply(texto, shiny::HTML)
  }
  if (!is.null(swatches)) texto <- lapply(seq_along(texto), function(i) shiny::tagList(
    shiny::tags$span(class = "crm-nota-swatch", `aria-hidden` = "true",
      style = paste0("background:", swatches[[i]], ";")), texto[[i]]))
  shiny::tagAppendAttributes(racafeForms::racafeNoticeList(texto, title = title, font_size = 11,
    text_color = "#495057", accent_color = "#495057", ...), class = "crm-nota")
}

.crm_titulo_modal <- function(titulo, icono = NULL) shiny::tagList(titulo)

# Calendario del presupuesto: el mes vigente también está pendiente ----
.crm_calendario_presupuesto <- function(cierre, periodo, hoy = Sys.Date()) {
  calendario <- .crm_calendario_cierre(cierre, periodo, hoy)
  anho <- as.integer(format(as.Date(hoy), "%Y"))
  mes <- if (periodo < anho) 13L else if (periodo > anho) 1L else as.integer(format(as.Date(hoy), "%m"))
  calendario$mes_vigente <- mes
  calendario$meses_rest <- max(0L, 13L - mes)
  calendario
}

# Identificadores tributarios sin formato numérico ----
.crm_nits_tabla <- function(datos, etiquetas = NULL) {
  nombres <- names(datos)
  es_identificador <- function(x) grepl("nit", x, ignore.case = TRUE) |
    grepl("^(Pedido|CLPdcCod|PdcCod|NumeroPedido|Número de pedido|Año|Ano|Anio|Anho|Year|Periodo)$",
      x, ignore.case = TRUE)
  son_nit <- es_identificador(nombres)
  if (!is.null(etiquetas)) {
    comunes <- intersect(names(etiquetas), nombres)
    son_nit[match(comunes, nombres)] <- son_nit[match(comunes, nombres)] |
      es_identificador(as.character(etiquetas[comunes]))
  }
  for (nombre in nombres[son_nit]) {
    if (!is.numeric(datos[[nombre]])) next
    valores <- datos[[nombre]]
    texto <- format(valores, scientific = FALSE, trim = TRUE, digits = 22, big.mark = "")
    texto[is.na(valores)] <- NA_character_
    datos[[nombre]] <- texto
  }
  datos
}

# Cierre financiero independiente de los movimientos provisionales ----
.crm_calendario_cierre <- function(cierre, periodo, hoy = Sys.Date()) {
  cierre <- as.Date(cierre)
  if (length(cierre) != 1L || is.na(cierre) || !is.finite(as.numeric(cierre))) {
    return(list(mes_actual = 0L, meses_rest = 12L, fecha_cierre = as.Date(NA)))
  }
  limite <- as.Date(format(as.Date(hoy), "%Y-%m-01")) - 1L
  corte <- min(cierre, limite)
  anho <- as.integer(format(corte, "%Y"))
  mes <- if (anho < periodo) 0L else if (anho > periodo) 12L else as.integer(format(corte, "%m"))
  list(mes_actual = mes, meses_rest = 12L - mes, fecha_cierre = as.Date(format(corte, "%Y-%m-01")))
}

.crm_estado_segmento <- function(pyg, crm) {
  ifelse(is.na(crm), "Sin segmento en CRM", ifelse(is.na(pyg), "Sin segmento en PyG",
    ifelse(pyg == crm, "Coincide", "Segmento diferente")))
}

.crm_descarga_ui <- function(id, ns, label = "Descargar detalle") {
  shiny::tags$div(class = "crm-descargas",
    racafeForms::racafeButtonDownload(id, label = label, icon = "download", ns = ns,
      size = "xs", variant = "secondary"))
}

# Componentes corporativos de presentación ----
.crm_caja_ui <- function(id, texto, icono, footer = NULL, mostrar_boton = FALSE, id_boton = "detalle",
                         tooltip = "Ver detalle", color_fondo = NULL) {
  caja <- racafeModulos::cajaValor_ui(id, texto = texto, icono = icono, footer = NULL, compact = TRUE,
                                     mostrar_boton = mostrar_boton, label_boton = NULL, id_boton = id_boton)
  if (!is.null(color_fondo)) caja <- shiny::tagAppendAttributes(caja, style = paste0("background:", color_fondo, ";"))
  if (mostrar_boton) {
    caja <- htmltools::tagQuery(caja)$find(".rmcv-button")$removeAttrs("title")$removeAttrs("aria-label")$
      addAttrs(title = tooltip, `aria-label` = tooltip,
               style = "width:22px;height:22px;min-width:22px;padding:2px;font-size:10px;line-height:1;")$allTags()
  }
  shiny::tags$div(class = "crm-caja-con-nota", caja, if (!is.null(footer)) .crm_nota(footer))
}

.crm_caja_output <- function(id) shiny::uiOutput(paste0(id, "_tarjeta"))

.crm_tamano_modal <- function(tamano) {
  equivalencias <- c(xl = "ventana", l = "subventana1", m = "subventana3", s = "aviso")
  if (tamano %in% names(equivalencias)) unname(equivalencias[[tamano]]) else tamano
}

.crm_caja <- function(id, valor, formato = "numero", texto = "", icono = "info-circle", colores = NULL,
                      color_fondo_hex = NULL, mostrar_boton = TRUE, contenido_modal = NULL, titulo_modal = "",
                      icono_modal = NULL, tamano_modal = "xl", footer = NULL, footer_class = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  output <- session$output
  .valor <- function(x) if (shiny::is.reactive(x)) x() else x
  accion <- isTRUE(mostrar_boton) && !is.null(contenido_modal)
  output[[paste0(id, "_tarjeta")]] <- .crm_render_ui({
    .crm_caja_ui(session$ns(id), texto = .valor(texto), icono = .valor(icono), footer = .valor(footer),
                  mostrar_boton = accion, id_boton = "btn_abrir", tooltip = .valor(titulo_modal),
                  color_fondo = .valor(color_fondo_hex))
  })
  modulo <- racafeModulos::cajaValor_server(id, valor_r = valor, formato = formato,
                                          empty = "—", id_boton = "btn_abrir")
  if (accion) {
    shiny::observeEvent(modulo$click_detalle(), {
      contenido <- if (is.function(contenido_modal)) contenido_modal() else contenido_modal
      shiny::showModal(racafeForms::racafeModalDetail(
                        title = .crm_titulo_modal(.valor(titulo_modal), icono_modal), contenido,
                        size = .crm_tamano_modal(tamano_modal), easy_close = TRUE,
                        footer = racafeForms::racafeModalFooter()))
    }, ignoreInit = TRUE)
  }
  invisible(modulo)
}

.crm_tabla_vancouver <- function(datos, ...) {
  argumentos <- list(...)
  if (is.null(argumentos$labels)) argumentos$labels <- unlist(.crm_etiquetas(names(datos)), use.names = TRUE)
  datos <- .crm_nits_tabla(datos, argumentos$labels)
  do.call(racafeShiny::racafeTableVancouver,
    c(list(data = datos, header_color = "#495057", header_width = 1.5, decimals = NULL), argumentos))
}

# Etiquetas orientadas al uso comercial, compartidas por tablas y modales ----
.crm_etiquetas <- function(nombres) {
  mapa <- c(Anho = "Año", Anio = "Año", Ano = "Año", Year = "Año", Categoria = "Categoría",
    Sacos70kg = "Sacos facturados (70 kg)", SacosFact = "Sacos facturados (70 kg)",
    MargenFCC = "Margen total FCC (COP)", Margen = "Margen FCC (COP)", FecFact = "Fecha de factura",
    CLCliNit = "NIT de facturación", CliNitPpal = "NIT de la cuenta principal", CLPdcCod = "Pedido",
    CLPdcLin = "Línea del pedido", CLLotCod = "Lote", CLLinNegNo = "Línea de negocio",
    MNFCCPpto = "Margen presupuestado (COP)", SSPpto = "Sacos presupuestados (70 kg)",
    MetaMensualSacos = "Meta mensual de sacos (70 kg)", MetaMensualMargen = "Meta mensual de margen (COP)",
    SacosOP = "Sacos objetivo", MargenOP = "Margen objetivo por kg (COP)", FechaCumpOP = "Fecha objetivo",
    ConsumoEsperado = "Consumo esperado", UnidadConsumo = "Unidad del consumo declarado",
    RecenciaDias = "Días desde la última compra", SegmentoRFM = "Perfil de compra",
    PerRazSoc = "Nombre del cliente", PerCod = "NIT", IdOportunidad = "Oportunidad",
    LinNegCod = "Código de línea de negocio", FecProceso = "Fecha de actualización",
    NombreComercial = "Nombre comercial", EstadoFinanciero = "Estado de la facturación",
    FechaHoraCrea = "Fecha de creación", UsuarioCrea = "Creado por", TipoGestion = "Tipo de gestión",
    CorreoGeneral = "Correo empresarial", PaginaWeb = "Página web", Telefono = "Teléfono",
    TipoTelefono = "Tipo de teléfono", TipoRedSocial = "Red social", UsuarioRed = "Cuenta en la red social",
    NombreSucursal = "Nombre de la sucursal", EsPrincipal = "Sucursal principal", Depto = "Departamento",
    Mpio = "Municipio", Direccion = "Dirección", Descripcion = "Descripción", Observacion = "Observación",
    FrecuenciaCompra = "Frecuencia de compra", CalidadesPreferidas = "Calidades preferidas",
    TipoCompra = "Tipo de compra", EsTostador = "Tiene tostadora", TipoRelacionamiento = "Tipo de contacto",
    FechaRecordatorio = "Fecha del recordatorio", FechaCumplimiento = "Fecha de cumplimiento",
    FechaHora = "Fecha y hora", EtapaAnterior = "Etapa anterior", EtapaNueva = "Etapa alcanzada",
    Antecedente = "Si compra estos productos", Consecuente = "También suele comprar",
    CestasAmbos = "Compras que incluyen ambos", Soporte = "Participación de la combinación",
    Confianza = "Compras de A que también incluyen B", Lift = "Afinidad entre los productos (lift)")
  valores <- unname(mapa[nombres])
  valores[is.na(valores)] <- nombres[is.na(valores)]
  stats::setNames(as.list(valores), nombres)
}

# Tablas Vancouver con búsqueda, ordenamiento y selección estable ----
.crm_tabla_ui <- function(id, titulo = NULL, subtitulo = NULL, footer = NULL, footer_tipo = "info",
                          sortable = TRUE, mostrar_nota = TRUE, estilo = "minimal", searchable = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (!is.null(titulo)) shiny::tags$strong(titulo),
    if (!is.null(subtitulo)) shiny::tags$div(subtitulo),
    if (searchable) shiny::tags$div(style = "max-width:220px;margin-left:auto;",
      racafeForms::racafeInputText("buscar", NULL, placeholder = "Buscar...", ns = ns)),
    shiny::uiOutput(ns("tabla")),
    shiny::uiOutput(ns("paginacion")),
    if (!is.null(footer)) .crm_nota(footer) else shiny::uiOutput(paste0(id, "_nota")))
}

.crm_tabla_datos <- function(datos, columnas = NULL, col_specs = NULL, col_labels = NULL,
                             on_row = NULL, on_sort = NULL, indices = seq_len(nrow(datos))) {
  etiquetas_nit <- col_labels
  for (nombre in names(col_specs)) {
    if (!is.null(col_specs[[nombre]]$name)) etiquetas_nit[[nombre]] <- col_specs[[nombre]]$name
  }
  datos <- .crm_nits_tabla(datos, etiquetas_nit)
  visibles <- if (is.null(columnas)) names(datos) else intersect(columnas, names(datos))
  visibles <- visibles[!vapply(visibles, function(n) identical(col_specs[[n]]$show, FALSE), logical(1))]
  tabla <- .crm_tabla_vancouver(datos[visibles], font_size = 11, row_padding = 2)
  etiquetas <- .crm_etiquetas(visibles)
  for (nombre in visibles) {
    spec <- col_specs[[nombre]]
    if (!is.null(spec$name)) etiquetas[[nombre]] <- spec$name
    if (nombre %in% names(col_labels)) etiquetas[[nombre]] <- col_labels[[nombre]]
    formato <- spec$format
    if (!is.null(formato$cell)) formato <- formato$cell
    if (is.numeric(datos[[nombre]])) {
      digitos <- if (is.null(formato$digits)) 2 else formato$digits
      if (isTRUE(formato$percent)) {
        tabla <- do.call(gt::fmt_percent, list(data = tabla, columns = nombre, decimals = digitos))
      } else {
        tabla <- do.call(gt::fmt_number, list(data = tabla, columns = nombre, decimals = digitos,
          use_seps = is.null(formato$separators) || isTRUE(formato$separators),
          pattern = paste0(if (!is.null(formato$prefix)) formato$prefix, "{x}", formato$suffix)))
      }
    }
    if (is.function(spec$cell) && !grepl("nit", paste(nombre, etiquetas[[nombre]]), ignore.case = TRUE)) {
      valores <- vapply(seq_len(nrow(datos)), function(i) as.character(spec$cell(datos[[nombre]][i])), character(1))
      tabla <- gt::text_transform(tabla, locations = gt::cells_body(columns = tidyselect::all_of(nombre)),
        fn = local({ texto <- valores; function(x) htmltools::htmlEscape(texto) }))
    }
    if (is.function(spec$style)) {
      for (i in seq_len(nrow(datos))) {
        estilo <- spec$style(datos[[nombre]][i])
        if (identical(estilo$color, "inherit")) estilo$color <- NULL
        estilos <- list()
        if (!is.null(estilo$background)) estilos <- c(estilos, list(gt::cell_fill(color = estilo$background)))
        if (!is.null(estilo$color) || !is.null(estilo$fontWeight)) {
          estilos <- c(estilos, list(gt::cell_text(color = estilo$color, weight = estilo$fontWeight)))
        }
        if (length(estilos)) tabla <- gt::tab_style(tabla, style = estilos,
          locations = gt::cells_body(columns = tidyselect::all_of(nombre), rows = i))
      }
    }
  }
  if (!is.null(on_row) && length(visibles)) {
    primera <- visibles[[1]]
    enlaces <- vapply(seq_len(nrow(datos)), function(i) as.character(shiny::tags$button(type = "button",
      class = "btn btn-link crm-tabla-enlace", style = "padding:0;font:inherit;color:inherit;text-align:left;",
      onclick = on_row(indices[[i]]), as.character(datos[[primera]][[i]]))), character(1))
    tabla <- gt::text_transform(tabla, locations = gt::cells_body(columns = tidyselect::all_of(primera)),
      fn = local({ texto <- enlaces; function(x) texto }))
  }
  if (!is.null(on_sort)) etiquetas <- lapply(names(etiquetas), function(nombre) gt::html(as.character(
    shiny::tags$button(type = "button", class = "btn btn-link crm-tabla-enlace",
      style = "padding:0;font:inherit;color:inherit;text-align:inherit;", onclick = on_sort(nombre),
      etiquetas[[nombre]])))) |> stats::setNames(names(etiquetas))
  tabla <- gt::cols_label(tabla, .list = etiquetas)
  gt::sub_missing(tabla, missing_text = "—")
}

.crm_vancouver_server <- function(id, data, columnas = NULL, col_specs = NULL, col_labels = NULL,
                                   id_col = NULL, modo_seleccion = "ninguno", sortable = TRUE,
                                   searchable = TRUE, page_size = 15, titulo = NULL, subtitulo = NULL, ...) {
  shiny::moduleServer(id, function(input, output, session) {
    seleccion <- shiny::reactiveVal(NULL)
    orden <- shiny::reactiveVal(list(col = NULL, desc = FALSE))
    pagina <- shiny::reactiveVal(1L)
    .val <- function(x) if (shiny::is.reactive(x)) x() else x
    .datos <- shiny::reactive({ .val(data) })
    .filas <- shiny::reactive({
      datos <- .datos()
      shiny::req(is.data.frame(datos))
      indices <- seq_len(nrow(datos))
      consulta <- if (searchable) input$buscar else NULL
      if (!is.null(consulta) && nzchar(trimws(consulta))) {
        visibles <- if (is.null(columnas)) names(datos) else intersect(columnas, names(datos))
        coincide <- vapply(indices, function(i) any(vapply(datos[visibles], function(col) {
          grepl(tolower(consulta), tolower(as.character(col[[i]])), fixed = TRUE)
        }, logical(1)), na.rm = TRUE), logical(1))
        indices <- indices[coincide]
      }
      actual <- orden()
      if (!is.null(actual$col) && actual$col %in% names(datos)) {
        indices <- indices[order(datos[[actual$col]][indices], decreasing = actual$desc, na.last = TRUE)]
      }
      if (!is.null(id_col) && id_col %in% names(datos)) {
        total <- !is.na(datos[[id_col]][indices]) & as.character(datos[[id_col]][indices]) == "TOTAL"
        indices <- c(indices[!total], indices[total])
      }
      indices
    })
    shiny::observeEvent(list(.datos(), input$buscar), { pagina(1L); seleccion(NULL) }, ignoreInit = TRUE)
    shiny::observeEvent(input$ordenar, {
      shiny::req(sortable, input$ordenar %in% names(.datos()))
      anterior <- orden()
      orden(list(col = input$ordenar, desc = identical(anterior$col, input$ordenar) && !anterior$desc))
      pagina(1L)
    })
    .paginas <- shiny::reactive(max(1L, ceiling(length(.filas()) / page_size)))
    shiny::observeEvent(input$anterior, pagina(max(1L, pagina() - 1L)))
    shiny::observeEvent(input$siguiente, pagina(min(.paginas(), pagina() + 1L)))
    .evento <- function(id, valor) paste0("Shiny.setInputValue(",
      jsonlite::toJSON(session$ns(id), auto_unbox = TRUE), ",", jsonlite::toJSON(valor, auto_unbox = TRUE),
      ",{priority:'event'});")
    output$tabla <- .crm_render_ui({
      datos <- .datos()
      filas <- .filas()
      desde <- (min(pagina(), .paginas()) - 1L) * page_size
      indices <- head(filas[seq_along(filas) > desde], page_size)
      if (!length(indices)) return(.crm_nota("Sin registros para los filtros seleccionados."))
      shiny::tagList(
        if (!is.null(titulo)) shiny::tags$strong(.val(titulo)),
        if (!is.null(subtitulo)) shiny::tags$div(.val(subtitulo)),
        .crm_tabla_datos(datos[indices, , drop = FALSE], columnas, col_specs, col_labels,
          on_row = if (modo_seleccion == "fila") function(i) .evento("seleccionar", i),
          on_sort = if (sortable) function(col) .evento("ordenar", col), indices = indices))
    })
    output$paginacion <- shiny::renderUI({
      if (.paginas() <= 1L) return(NULL)
      shiny::tags$div(style = "display:flex;gap:8px;align-items:center;justify-content:flex-end;",
        racafeForms::racafeButton("anterior", NULL, icon = "angle-left", variant = "ghost", size = "xs",
          title = "Página anterior", ns = session$ns),
        shiny::tags$small(paste("Página", min(pagina(), .paginas()), "de", .paginas())),
        racafeForms::racafeButton("siguiente", NULL, icon = "angle-right", variant = "ghost", size = "xs",
          title = "Página siguiente", ns = session$ns))
    })
    shiny::observeEvent(input$seleccionar, {
      fila <- suppressWarnings(as.integer(input$seleccionar))
      shiny::req(modo_seleccion == "fila", length(fila) == 1L, fila %in% .filas())
      datos <- .datos()
      seleccion(list(modo = "fila", row = fila, id = if (!is.null(id_col)) datos[[id_col]][fila] else fila,
        data = datos[fila, , drop = FALSE]))
    })
    list(seleccion = shiny::reactive(seleccion()), modo = modo_seleccion, limpiar = function() seleccion(NULL))
  })
}

.crm_tabla_servidor <- function(..., footer = NULL, footer_tipo = "info", modal_titulo_fn = NULL, modal_contenido_fn = NULL,
                                modal_pre_fn = NULL,
                                modal_size = "xl", modal_icon = NULL, modal_footer = NULL,
                                modal_footer_tipo = "info") {
  argumentos <- list(...)
  id <- if (!is.null(argumentos[["id", exact = TRUE]])) argumentos[["id", exact = TRUE]] else argumentos[[1]]
  session <- shiny::getDefaultReactiveDomain()
  output <- session$output
  output[[paste0(id, "_nota")]] <- .crm_render_ui({
    nota <- if (shiny::is.reactive(footer)) footer() else footer
    if (is.null(nota)) return(NULL)
    tipo <- if (footer_tipo == "danger") "error" else if (footer_tipo == "warning") "warning" else "info"
    .crm_nota(nota)
  })
  tabla <- .crm_vancouver_server(...)
  if (!is.null(modal_contenido_fn)) {
    shiny::observeEvent(tabla$seleccion(), {
      seleccion <- tabla$seleccion()
      shiny::req(seleccion)
      if (!is.null(modal_pre_fn)) modal_pre_fn(seleccion)
      titulo <- if (is.function(modal_titulo_fn)) modal_titulo_fn(seleccion) else modal_titulo_fn
      shiny::showModal(racafeForms::racafeModalDetail(
                        title = .crm_titulo_modal(titulo, modal_icon), modal_contenido_fn(seleccion),
                        size = .crm_tamano_modal(modal_size), easy_close = TRUE,
                        footer = racafeForms::racafeModalFooter(
                          left = if (!is.null(modal_footer)) {
                            .crm_nota(modal_footer)
                          }, right = shiny::modalButton("Cerrar"))))
      tabla$limpiar()
    }, ignoreInit = TRUE)
  }
  tabla
}


