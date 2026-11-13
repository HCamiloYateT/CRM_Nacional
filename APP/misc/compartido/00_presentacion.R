# Medición opcional sin registrar valores comerciales ----
.crm_medir <- function(fase, expresion) {
  inicio <- proc.time()[["elapsed"]]
  on.exit({
    receptor <- getOption("crm.perfil")
    if (is.function(receptor)) tryCatch(
      receptor(fase = fase, segundos = proc.time()[["elapsed"]] - inicio), error = function(e) invisible(NULL))
  }, add = TRUE)
  eval.parent(substitute(expresion))
}

# Recurso visual común para la app y las vistas independientes ----
.crm_estilos_ui <- function() {
  if (exists(".estilos_crm", inherits = TRUE)) return(get(".estilos_crm", inherits = TRUE))
  shiny::singleton(shiny::includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
    "3fc6b20f62a47f18a8b35ec37512af56223ffb1e/Styles/crm-nacional.css")))
}

# Notas compactas fuera de cajas y tablas ----
.crm_nota <- function(texto, title = NULL, swatches = NULL, ...) {
  if (is.null(texto)) return(NULL)
  if (inherits(texto, c("shiny.tag", "shiny.tag.list"))) texto <- list(texto)
  if (is.character(texto)) {
    html <- inherits(texto, "html")
    texto <- unlist(strsplit(texto, "\n", perl = TRUE))
    if (html) texto <- lapply(texto, shiny::HTML)
  }
  shiny::tagAppendAttributes(racafeForms::racafeNoticeList(texto, title = title, font_size = 11,
    text_color = "#495057", accent_color = "#495057", variant = "footnote", swatches = swatches, ...), class = "crm-nota")
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
    grepl("(^Codigo|^Código|Cod$|CodPedido$|CodLote$|NumeroFactura$)", x, ignore.case = TRUE) |
    grepl("^(Pedido|CLPdcCod|PdcCod|NumeroPedido|Número de pedido|Lote|CLLotCod|LotCod|CLPdcLin|PdcLin|LineaPedido|LinProCod|LinNegCod|MrcCod|MCCod|Año|Ano|Anio|Anho|Year|Periodo)$",
      x, ignore.case = TRUE) | grepl("^(Código de línea|Línea (del|de) (pedido|producto|negocio))", x, ignore.case = TRUE)
  son_nit <- es_identificador(nombres)
  if (!is.null(etiquetas)) {
    comunes <- intersect(names(etiquetas), nombres)
    son_nit[match(comunes, nombres)] <- son_nit[match(comunes, nombres)] |
      es_identificador(as.character(etiquetas[comunes]))
  }
  for (nombre in nombres[son_nit]) {
    if (!is.numeric(datos[[nombre]])) next
    datos[[nombre]] <- racafeCore::IdentificadorTexto(datos[[nombre]])
  }
  formatos <- list()
  for (nombre in names(datos)) {
    if (!is.numeric(datos[[nombre]]) || inherits(datos[[nombre]], c("Date", "POSIXt"))) next
    conteo <- grepl("^(Numero)?(Clientes|Unidades|Pedidos|Facturas|Lotes|Registros)|^n_|^Dias|^Meses", nombre)
    dinero <- grepl("Margen|MNFCC|Precio|Saldo|Monto|CLV|ValorCOP|Recaudo", nombre, ignore.case = TRUE) &&
      !grepl("cumpl|pct|porcentaje|prob|var|particip|ratio|riesgo", nombre, ignore.case = TRUE)
    if (conteo) formatos[[nombre]] <- list(digits = 0L)
    if (dinero) formatos[[nombre]] <- list(prefix = "$", digits = 0L)
  }
  attr(datos, "formatos_vancouver") <- formatos
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

.crm_descarga_ui <- function(id, ns, label = "Descargar") {
  shiny::tags$div(class = "crm-descargas",
    racafeForms::racafeButtonDownload(id, label = label, icon = "download", ns = ns,
      size = "xs", variant = "secondary"))
}

# Componentes corporativos de presentación ----
.crm_caja_ui <- function(id, texto, icono, footer = NULL, mostrar_boton = FALSE, id_boton = "detalle",
                         tooltip = "Ver detalle", color_fondo = NULL) {
  caja <- racafeModulos::cajaValor_ui(id, texto = texto, icono = icono, footer = footer, compact = TRUE,
    mostrar_boton = mostrar_boton, label_boton = NULL, id_boton = id_boton, tooltip = tooltip,
    button_size = "icon", footer_position = "outside", nota_fn = .crm_nota)
  if (!is.null(color_fondo)) {
    caja <- htmltools::tagQuery(caja)$find(".rmcv-card")$
      addAttrs(style = paste0("background:", color_fondo, ";"))$allTags()
  }
  shiny::tagAppendAttributes(caja, class = "crm-caja-con-nota")
}

.crm_caja_output <- function(id) shiny::uiOutput(paste0(id, "_tarjeta"))

.crm_tamano_modal <- function(tamano) {
  equivalencias <- c(xl = "ventana", l = "subventana1", m = "subventana3", s = "aviso")
  if (tamano %in% names(equivalencias)) unname(equivalencias[[tamano]]) else tamano
}

.crm_caja <- function(id, valor, formato = "numero", texto = "", icono = "info-circle", colores = NULL,
                      color_fondo_hex = NULL, mostrar_boton = TRUE, contenido_modal = NULL, titulo_modal = "",
                      icono_modal = NULL, tamano_modal = "xl", footer = NULL, footer_class = NULL,
                      formatear = NULL) {
  racafeModulos::cajaValorModal(id, valor = valor, formato = formato, texto = texto, icono = icono,
    color_fondo_hex = color_fondo_hex, mostrar_boton = mostrar_boton, contenido_modal = contenido_modal,
    titulo_modal = titulo_modal, tamano_modal = .crm_tamano_modal(tamano_modal), footer = footer,
    ui_fn = .crm_caja_ui, formatear = formatear)
}

.crm_tabla_vancouver <- function(datos, ...) {
  argumentos <- list(...)
  if (is.null(argumentos$labels)) argumentos$labels <- unlist(.crm_etiquetas(names(datos)), use.names = TRUE)
  if (is.null(argumentos$min_width)) argumentos$min_width <- paste0(max(300L, 110L * ncol(datos)), "px")
  datos <- .crm_nits_tabla(datos, argumentos$labels)
  datos[] <- lapply(datos, function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) gsub("convencionales", "Coproductos", x, ignore.case = TRUE) else x
  })
  tabla <- do.call(racafeShiny::racafeTableVancouver,
    c(list(data = datos, header_color = "#495057", header_width = 1.5, decimals = NULL), argumentos))
  formatos <- attr(datos, "formatos_vancouver")
  for (columna in names(formatos)) {
    f <- formatos[[columna]]
    tabla <- gt::fmt_number(tabla, columns = tidyselect::all_of(columna), decimals = f$digits,
      pattern = paste0(f$prefix %||% "", "{x}"))
  }
  fechas <- names(datos)[vapply(datos, inherits, logical(1), "Date")]
  if (length(fechas)) tabla <- gt::fmt(tabla, columns = tidyselect::all_of(fechas), fn = function(x) format(x,
    "%d/%m/%Y"))
  tabla
}

# Etiquetas orientadas al uso comercial, compartidas por tablas y modales ----
.crm_etiquetas <- function(nombres) {
  mapa <- c(PrecioNegociadoOriginal = "Precio negociado · unidad original", UnidadPrecioOriginal = "Unidad original del precio",
    MonedaPrecioOriginal = "Divisa del precio", PrecioNegociadoKg = "Precio negociado · COP/kg",
    ParticipacionSacos = "Participación en sacos", ParticipacionMargen = "Participación en margen",
    PrecioMinimoKg = "Menor precio negociado · COP/kg", PrecioMaximoKg = "Mayor precio negociado · COP/kg",
    PrecioPromedioKg = "Precio negociado promedio · COP/kg", PedidosConPrecio = "Unidades de negociación con precio válido",
    Sacos = "Sacos de 70 kg", Linea = "Línea del pedido", Motivo = "Situación por revisar",
    SacosLoteOrigen = "Cantidad asignada · unidad original", SacosProducidosOrigen = "Producción · unidad original",
    SacosDespachadosOrigen = "Despacho · unidad original", CantidadPedidoOrigen = "Cantidad solicitada · unidad original",
    FechaAnalitica = "Fecha de ejecución o estimación", EstadoEjecucion = "Tipo de evidencia", Anho = "Año", Anio = "Año", Ano = "Año", Year = "Año", Categoria = "Categoría",
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
  racafeModulos::TablaVancouverUI(id, titulo = titulo, subtitulo = subtitulo, footer = footer,
    footer_tipo = footer_tipo, sortable = sortable, mostrar_nota = mostrar_nota, estilo = estilo,
    searchable = searchable, nota_fn = .crm_nota)
}

.crm_tabla_datos <- function(datos, columnas = NULL, col_specs = NULL, col_labels = NULL,
                             on_row = NULL, on_sort = NULL, indices = seq_len(nrow(datos))) {
  racafeModulos::TablaVancouverDatos(datos, columnas, col_specs, col_labels, on_row, on_sort, indices,
    preparar_fn = .crm_nits_tabla, etiquetas_fn = .crm_etiquetas, tabla_fn = .crm_tabla_vancouver,
    texto_fn = .crm_label_comercial)
}

.crm_vancouver_server <- function(id, data, columnas = NULL, col_specs = NULL, col_labels = NULL,
                                   id_col = NULL, modo_seleccion = "ninguno", sortable = TRUE,
                                   searchable = TRUE, page_size = 15, titulo = NULL, subtitulo = NULL, ...) {
  col_specs <- .crm_specs_pedidos(col_specs %||% list(), id)
  racafeModulos::TablaVancouverServer(id, data, columnas, col_specs, col_labels, id_col, modo_seleccion,
    sortable, searchable, page_size, titulo, subtitulo, ..., preparar_fn = .crm_nits_tabla,
    etiquetas_fn = .crm_etiquetas, tabla_fn = .crm_tabla_vancouver, texto_fn = .crm_label_comercial,
    nota_fn = .crm_nota)
}

.crm_tabla_servidor <- function(..., footer = NULL, footer_tipo = "info", modal_titulo_fn = NULL,
                                modal_contenido_fn = NULL, modal_pre_fn = NULL, modal_size = "xl",
                                modal_icon = NULL, modal_footer = NULL, modal_footer_tipo = "info") {
  argumentos <- list(...)
  identificador <- argumentos$id %||% argumentos[[1]]
  argumentos$col_specs <- .crm_specs_pedidos(argumentos$col_specs %||% list(), identificador)
  do.call(racafeModulos::TablaVancouver, c(argumentos, list(footer = footer, footer_tipo = footer_tipo,
    modal_titulo_fn = modal_titulo_fn, modal_contenido_fn = modal_contenido_fn, modal_pre_fn = modal_pre_fn,
    modal_size = .crm_tamano_modal(modal_size), modal_footer = modal_footer,
    modal_footer_tipo = modal_footer_tipo, preparar_fn = .crm_nits_tabla, etiquetas_fn = .crm_etiquetas,
    tabla_fn = .crm_tabla_vancouver, texto_fn = .crm_label_comercial, nota_fn = .crm_nota)))
}



# Ayuda contextual de parámetros ----
.crm_label_ayuda <- function(texto, ayuda) {
  shiny::tagList(texto, shiny::tags$button(type = "button", class = "btn btn-secondary btn-sm ml-1 crm-ayuda",
    title = ayuda, `aria-label` = paste(texto, ayuda), shiny::icon("circle-question")))
}

# Acceso transversal al pedido respetando el alcance del maestro de la sesión ----
.crm_specs_pedidos <- function(specs, id) {
  sesion <- shiny::getDefaultReactiveDomain()
  if (is.null(sesion) || !isTRUE(sesion$userData$trazabilidad_disponible)) return(specs)
  for (campo in c("Pedido", "CLPdcCod", "PdcCod", "NumeroPedido")) local({
    columna <- campo
    if (!is.null(specs[[columna]]$cell)) return(invisible(NULL))
    spec <- specs[[columna]] %||% list()
    spec$html <- TRUE
    spec$cell <- function(v, i) {
      pedido <- .crm_id_texto(v)
      if (is.na(pedido) || pedido == "0") return("—")
      js <- paste0("event.stopPropagation();Shiny.setInputValue('crm_ver_pedido',{pedido:",
        jsonlite::toJSON(pedido, auto_unbox = TRUE), ",nonce:Date.now()},{priority:'event'});")
      shiny::tagAppendAttributes(racafeForms::racafeButton(paste0(id, "_", columna, "_", i), pedido,
        icon = "timeline", variant = "ghost", size = "xxs", ns = sesion$ns, title = "Consultar pedido"), onclick = js)
    }
    specs[[columna]] <<- spec
  })
  specs
}
