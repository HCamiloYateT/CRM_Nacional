# Flujo completo: cada saldo conserva su grano y su unidad ----
.PEN_ETAPAS <- c("Sin asignar", "Por producir", "Por despachar", "Por facturar", "Futuros", "Vencidos",
                 "Sin fecha estimada", "Revisar margen", "Sin pedido", "Sin lote", "Revisar cantidades")
.pen_preparar <- function(maestro, hoy = Sys.Date()) {
  requeridas <- c("CLPdcCod", "CLPdcLin", "CLLotCod", "EstadoFinanciero", "SacosPedido70Linea",
                 "SacosLote70", "SacosProducidos70", "SacosDespachados70", "SacosFact", "Margen", "FecFact",
                 "FacturadosOperativos70")
  if (!all(requeridas %in% names(maestro))) stop("Actualice DataPrep: faltan campos del flujo comercial.")
  x <- maestro
  x$Pedido <- .crm_id_texto(x$CLPdcCod)
  x$Linea <- .crm_id_texto(x$CLPdcLin)
  x$Lote <- .crm_id_texto(x$CLLotCod)
  x$NIT <- .crm_id_texto(x$CliNitPpal)
  x$Cliente <- as.character(x$PerRazSoc)
  x$Fecha <- as.Date(.gc_columna(x, c("PdcFecEst", "FechaEstimadaPedido")))
  x$Creacion <- as.Date(.gc_columna(x, c("FechaCreacionPedido", "PdcFecCre")))
  x$MargenProyectadoLinea <- .gc_num(.gc_columna(x, "MargenProyectadoLinea", NA_real_))
  x$EstadoPedido <- .gc_columna(x, c("EstadoPedido"))
  # Una cantidad operativa por lote; sumar solamente los hechos de ejecución, nunca las proyecciones.
  lotes <- x %>% dplyr::filter(!is.na(CLLotCod), CLLotCod != 0, EstadoFinanciero != "PROYECTADO") %>%
    dplyr::group_by(Lote) %>%
    dplyr::summarise(Pedido = .gc_unico(Pedido), Linea = .gc_unico(Linea), NIT = .gc_unico(NIT),
      Cliente = .gc_unico(Cliente), Fecha = .gc_unico(Fecha),
      Asignados = .gc_unico(SacosLote70), Producidos = .gc_unico(SacosProducidos70),
      Despachados = .gc_unico(SacosDespachados70),
      FacturadosSYSCAFE = .gc_unico(FacturadosOperativos70),
      Facturados = .gc_unico(if ("FacturadosConsolidados70" %in% names(x)) FacturadosConsolidados70 else FacturadosOperativos70),
      FacturadosPeriodo = sum(SacosFact, na.rm = TRUE),
      .groups = "drop")
  asignacion <- lotes %>% dplyr::filter(!is.na(Pedido), !is.na(Linea)) %>%
    dplyr::group_by(Pedido, Linea) %>%
    dplyr::summarise(Asignados = if (anyNA(Asignados)) NA_real_ else sum(Asignados),
      FacturadosVida = if (anyNA(Facturados)) NA_real_ else sum(Facturados), .groups = "drop")
  pedidos <- x %>% dplyr::filter(!is.na(CLPdcCod), CLPdcCod != 0, !is.na(CLPdcLin)) %>%
    dplyr::group_by(Pedido, Linea) %>%
    dplyr::summarise(NIT = .gc_unico(NIT), Cliente = .gc_unico(Cliente), Fecha = .gc_unico(Fecha),
      Creacion = .gc_unico(Creacion), Estado = .gc_unico(EstadoPedido),
      Pedidos = .gc_unico(SacosPedido70Linea),
      Facturados = sum(SacosFact[EstadoFinanciero != "PROYECTADO"], na.rm = TRUE),
      MargenEstimado = .gc_unico(MargenProyectadoLinea),
      .groups = "drop") %>% dplyr::left_join(asignacion, by = c("Pedido", "Linea"))
  # Un pedido sin lote tiene asignación cero; un lote con unidad desconocida conserva NA.
  claves_lote <- paste(asignacion$Pedido, asignacion$Linea)
  pedidos$Asignados[!paste(pedidos$Pedido, pedidos$Linea) %in% claves_lote] <- 0
  sin_lote <- !paste(pedidos$Pedido, pedidos$Linea) %in% claves_lote
  pedidos$FacturadosVida[sin_lote] <- pedidos$Facturados[sin_lote]
  pedidos$Facturados <- pedidos$FacturadosVida
  pedidos$Pendientes <- pmax(pedidos$Pedidos - pedidos$Facturados, 0)
  pedidos$SinAsignar <- pmax(pedidos$Pedidos - pedidos$Asignados, 0)
  pedidos$Activo <- pedidos$Estado %in% c("A", "ACTIVO", "ABIERTO") | is.na(pedidos$Estado)
  inactivos <- pedidos[!pedidos$Activo, c("Pedido", "Linea")]
  lotes$PedidoActivo <- !paste(lotes$Pedido, lotes$Linea) %in% paste(inactivos$Pedido, inactivos$Linea)
  registros <- list()
  agregar <- function(dat, etapa, cantidad, nivel) {
    if (!nrow(dat)) return(invisible(NULL))
    dat$Etapa <- etapa
    dat$Cantidad <- cantidad
    dat$Nivel <- nivel
    registros[[length(registros) + 1L]] <<- dat
  }
  con_lote <- paste(pedidos$Pedido, pedidos$Linea) %in% paste(lotes$Pedido, lotes$Linea)
  cobertura <- if ("FechaInicioTrazabilidad" %in% names(x)) unique(stats::na.omit(x$FechaInicioTrazabilidad)) else as.Date(character())
  fecha_cobertura <- if (length(cobertura)) min(cobertura) else as.Date(NA)
  elegible <- con_lote | (!is.na(fecha_cobertura) & !is.na(pedidos$Creacion) & pedidos$Creacion >= fecha_cobertura)
  sin_trazabilidad <- pedidos[pedidos$Activo & !elegible, , drop = FALSE]
  sin_trazabilidad$Motivo <- "Pedido fuera de cobertura operativa verificable; no se presume pendiente"
  agregar(sin_trazabilidad, "Información incompleta", rep(NA_real_, nrow(sin_trazabilidad)), "Pedido")
  activos <- pedidos[pedidos$Activo & elegible, , drop = FALSE]
  etapas <- list("Sin asignar" = activos$SinAsignar > 0.1,
    "Futuros" = activos$Pendientes > 0.1 & !is.na(activos$Fecha) & activos$Fecha > hoy,
    "Vencidos" = activos$Pendientes > 0.1 & !is.na(activos$Fecha) & activos$Fecha < hoy,
    "Sin fecha estimada" = activos$Pendientes > 0.1 & is.na(activos$Fecha))
  for (etapa in names(etapas)) {
    filas <- which(etapas[[etapa]])
    agregar(activos[filas, ], etapa,
      if (etapa == "Sin asignar") activos$SinAsignar[filas] else activos$Pendientes[filas], "Pedido")
  }
  for (etapa in c("Por producir", "Por despachar", "Por facturar")) {
    saldo <- switch(etapa, "Por producir" = lotes$Asignados - lotes$Producidos,
      "Por despachar" = lotes$Producidos - lotes$Despachados,
      "Por facturar" = lotes$Despachados - lotes$Facturados)
    filas <- which(saldo > 0.1 & lotes$PedidoActivo)
    agregar(lotes[filas, ], etapa, saldo[filas], "Lote")
  }
  filas <- which(activos$Pendientes > 0.1 & !is.finite(activos$MargenEstimado))
  sin_margen <- activos[filas, ]
  sin_margen$Motivo <- "Pedido pendiente sin estimación de margen"
  agregar(sin_margen, "Revisar margen", sin_margen$Pendientes, "Pedido")
  filas <- which(is.na(lotes$Pedido))
  agregar(lotes[filas, ], "Sin pedido", rep(NA_real_, length(filas)), "Lote")
  revisar <- which(is.na(lotes$Asignados) | is.na(lotes$Producidos) | is.na(lotes$Despachados) | is.na(lotes$Facturados) |
    lotes$Producidos > lotes$Asignados + 0.1 | lotes$Despachados > lotes$Producidos + 0.1 |
    lotes$Facturados > lotes$Despachados + 0.1 |
    lotes$FacturadosPeriodo > lotes$FacturadosSYSCAFE + 0.1)
  agregar(lotes[revisar, ], "Revisar cantidades", rep(NA_real_, length(revisar)), "Lote")
  filas <- which(is.na(pedidos$Pedidos) | pedidos$Asignados > pedidos$Pedidos + 0.1 |
    pedidos$Facturados > pedidos$Pedidos + 0.1)
  agregar(pedidos[filas, ], "Revisar cantidades", rep(NA_real_, length(filas)), "Pedido")
  fin <- x %>% dplyr::filter(EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"), !is.na(FecFact)) %>%
    dplyr::mutate(Motivo = dplyr::case_when(
      EstadoFinanciero == "FACTURADO_ABIERTO" ~ "Mes abierto: margen provisional, pendiente de cierre",
      !is.finite(Margen) ~ "Factura cerrada sin margen contable",
      TRUE ~ NA_character_))
  filas <- which(!is.na(fin$Motivo))
  agregar(fin[filas, ], "Revisar margen", fin$SacosFact[filas], "Financiero")
  filas <- which(is.na(fin$CLLotCod) | fin$CLLotCod == 0)
  agregar(fin[filas, ], "Sin lote", fin$SacosFact[filas], "Financiero")
  vacio <- tibble::tibble(Etapa = character(), Nivel = character(), Pedido = character(), Linea = character(),
    Lote = character(), NIT = character(), Cliente = character(), Fecha = as.Date(character()), Cantidad = numeric())
  detalle <- dplyr::bind_rows(vacio, registros)
  detalle$Registro <- seq_len(nrow(detalle))
  list(pedidos = pedidos, lotes = lotes, detalle = detalle)
}

.pen_guardar_oc <- function(lote, orden, usuario, guardar = racafeBD::AgregarDatos) {
  codigo <- suppressWarnings(as.numeric(lote))
  if (length(codigo) != 1L || !is.finite(codigo) || codigo <= 0 || codigo != trunc(codigo)) {
    stop("El lote no es válido.")
  }
  if (length(orden) != 1L || is.na(orden) || !nzchar(trimws(orden))) stop("Escriba la orden de compra.")
  if (length(usuario) != 1L || is.na(usuario) || !nzchar(usuario) || usuario == "SIN IDENTIFICAR") {
    stop("No se identificó el usuario.")
  }
  guardar(data.frame(UsuarioCrea = usuario, FechaHoraCrea = as.character(Sys.time()),
    Lote = codigo, OrdenCompra = trimws(orden), stringsAsFactors = FALSE), "CRMNALORDCMP")
  invisible(TRUE)
}

PendientesGestionUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain", lapply(.PEN_TIPOS, function(tipo) {
    mid <- paste0("tipo_", match(tipo, .PEN_TIPOS))
    .crm_disclosure(tipo, .crm_tabla_ui(ns(paste0(mid, "resumen"))), .crm_plot_output(ns(paste0(mid, "grafico"))),
      .crm_tabla_ui(ns(mid)), open = TRUE)
  }), .crm_disclosure("Gestión documental por lote", .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("detalle"))),
    .crm_nota_metodologica(c("Cada condición conserva su cantidad y su nivel de observación.",
      "Los pedidos pueden aparecer en varias secciones. No se suman etapas solapadas como si fueran un total único.",
      "Los pedidos anteriores a la cobertura operativa verificable se separan como información incompleta.",
      "Cantidades desconocidas no se reemplazan por cero. Seleccione un pedido para consultar su trazabilidad.")))
}

PendientesGestion <- function(id, maestro, usuario = function() "SIN IDENTIFICAR", corte = function() Sys.Date()) {
  shiny::moduleServer(id, function(input, output, session) {
    lote_seleccionado <- shiny::reactiveVal(NULL)
    orden_pendiente <- shiny::reactiveVal(NULL)
    revision_ordenes <- shiny::reactiveVal(0L)
    revision_flujo <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$actualizar, revision_flujo(revision_flujo() + 1L))
    flujo <- shiny::reactive({ revision_flujo(); .pen_preparar(maestro(), corte()) })
    datos <- shiny::reactive(.pen_tipificar(flujo()$detalle, maestro()))
    for (tipo in .PEN_TIPOS) local({
      categoria <- tipo; mid <- paste0("tipo_", match(tipo, .PEN_TIPOS))
      filas <- shiny::reactive({
        x <- datos(); x[x$TipoPendiente == categoria, , drop = FALSE]
      })
      .crm_vancouver_server(paste0(mid, "resumen"), shiny::reactive(.pen_resumen_tipo(filas())))
      sel <- .crm_tabla_servidor(mid, data = filas, id_col = "Registro", modo_seleccion = "fila",
        columnas = c("Cliente", "Pedido", "Linea", "Lote", "Fecha", "Nivel", "Cantidad", "Categoria", "Producto", "CLLinNegNo", "Asesor", "Motivo", "Etapa"),
        modal_titulo_fn = function(sel) "Trazabilidad del pedido",
        modal_contenido_fn = function(sel) TrazabilidadPedidoUI(session$ns(paste0(mid, "traza"))))
      TrazabilidadPedido(paste0(mid, "traza"), maestro,
        shiny::reactive({ x <- sel$seleccion(); if (is.null(x)) NULL else x$fila$Pedido }))
      output[[paste0(mid, "grafico")]] <- .crm_render_plotly({
        x <- filas()
        shiny::validate(shiny::need(nrow(x) > 0L, "No hay registros para esta condición."))
        x$Antiguedad <- as.integer(corte() - as.Date(.gc_columna(x, c("Creacion", "FechaCreacionPedido"))))
        x$Tramo <- dplyr::case_when(is.na(x$Antiguedad) ~ "Sin fecha", x$Antiguedad < 0 ~ "Futuro",
          x$Antiguedad <= 30 ~ "0–30 días", x$Antiguedad <= 60 ~ "31–60 días", TRUE ~ "Más de 60 días")
        x <- x %>% dplyr::group_by(Tramo) %>% dplyr::summarise(Registros = dplyr::n(), .groups = "drop")
        plotly::plot_ly(x, x = ~Tramo, y = ~Registros, type = "bar", marker = list(color = "#7A2028"),
          hovertemplate = "%{x}<br>Registros: %{y:.0f}<extra></extra>")
      })
    })
    .gc_tarjetas_server(c(pedidos = "Pedidos por gestionar", producir = "Lotes por producir",
      despachar = "Lotes por despachar", facturar = "Lotes por facturar"), shiny::reactive({
      x <- datos()
      list(pedidos = dplyr::n_distinct(paste(x$Pedido[!is.na(x$Pedido)], x$Linea[!is.na(x$Pedido)])),
        producir = sum(x$Etapa == "Por producir"), despachar = sum(x$Etapa == "Por despachar"),
        facturar = sum(x$Etapa == "Por facturar"))
    }))
    .crm_tabla_servidor(id = "detalle", data = datos, id_col = "Registro", modo_seleccion = "fila",
      columnas = c("Etapa", "Nivel", "Pedido", "Linea", "Lote", "NIT", "Cliente", "Fecha", "Cantidad"),
      col_labels = c(Fecha = "Fecha estimada", Cantidad = "Sacos 70 kg"),
      modal_titulo_fn = function(fila) "Trazabilidad del pendiente",
      modal_pre_fn = function(fila) {
        x <- datos()[datos()$Registro %in% fila$id, , drop = FALSE]
        lote_seleccionado(if (nrow(x) && !is.na(x$Lote[[1]])) x$Lote[[1]] else NULL)
        orden_pendiente(NULL)
      },
      modal_contenido_fn = function(fila) {
        detalle <- datos()[datos()$Registro %in% fila$id, , drop = FALSE]
        origen <- maestro()
        if (length(detalle$Lote) && !is.na(detalle$Lote[[1]])) {
          origen <- origen[which(as.character(origen$CLLotCod) == detalle$Lote[[1]]), , drop = FALSE]
        } else if (length(detalle$Pedido) && !is.na(detalle$Pedido[[1]])) {
          origen <- origen[which(as.character(origen$CLPdcCod) == detalle$Pedido[[1]] &
            as.character(origen$CLPdcLin) == detalle$Linea[[1]]), , drop = FALSE]
        } else if ("IdRegistroCRM" %in% names(detalle)) {
          origen <- origen[origen$IdRegistroCRM %in% detalle$IdRegistroCRM, , drop = FALSE]
        } else origen <- origen[0, ]
        columnas <- intersect(c("CLPdcCod", "CLPdcLin", "CLLotCod", "OrdenCompra", "FecFact", "SacosFact",
          "FacturadosOperativos70", "Margen", "EstadoFinanciero", "FuenteFinanciera", "PdcFecEst"), names(origen))
        shiny::tagList(.crm_tabla_vancouver(.crm_nits_tabla(detalle), font_size = 11, row_padding = 2),
          shiny::tags$h5("Movimientos y fuentes"),
          .crm_tabla_vancouver(origen[columnas], font_size = 11, row_padding = 2),
          if (!is.null(lote_seleccionado())) racafeForms::racafeForm(variant = "plain",
            racafeForms::racafeInputText("orden_compra", "Añadir orden de compra", ns = session$ns),
            racafeForms::racafeButton("asignar_oc", "Asignar", icon = "floppy-disk", ns = session$ns),
            shiny::tags$h5("Bitácora de órdenes de compra"), shiny::uiOutput(session$ns("bitacora_oc"))))
      })
    bitacora <- shiny::reactive({
      revision_ordenes()
      lote <- lote_seleccionado()
      shiny::req(length(lote) == 1L, lote %in% as.character(maestro()$CLLotCod))
      codigo <- .gc_num(lote)
      shiny::req(is.finite(codigo), codigo > 0)
      tryCatch(list(datos = racafeBD::CargarDatos("CRMNALORDCMP",
        condicion = paste0("Lote = ", format(codigo, scientific = FALSE, trim = TRUE))), error = NULL),
        error = function(e) list(datos = NULL, error = conditionMessage(e)))
    })
    output$bitacora_oc <- .crm_render_ui({
      x <- bitacora()
      if (!is.null(x$error)) return(.crm_nota(paste("No se pudo consultar la bitácora:", x$error)))
      .crm_tabla_vancouver(x$datos, font_size = 11, row_padding = 2)
    })
    shiny::observeEvent(input$asignar_oc, {
      lote <- lote_seleccionado()
      shiny::req(length(lote) == 1L, lote %in% as.character(maestro()$CLLotCod))
      if (is.null(input$orden_compra) || !nzchar(trimws(input$orden_compra))) {
        shiny::showNotification("Escriba la orden de compra.", type = "error")
        return()
      }
      orden_pendiente(list(lote = lote, orden = trimws(input$orden_compra), usuario = usuario()))
      racafeShiny::MostrarModalConfirmacion(ns = session$ns, titulo = "Asignar orden de compra",
        texto = paste("Lote", lote, "— Orden", trimws(input$orden_compra)),
        id_cancelar = "cancelar_oc", id_confirmar = "confirmar_oc", label_confirmar = "Asignar",
        icono_confirmar = "floppy-disk")
    })
    shiny::observeEvent(input$cancelar_oc, { orden_pendiente(NULL); shiny::removeModal() })
    shiny::observeEvent(input$confirmar_oc, {
      pendiente <- orden_pendiente()
      shiny::req(pendiente, pendiente$lote %in% as.character(maestro()$CLLotCod),
        identical(pendiente$usuario, usuario()))
      orden_pendiente(NULL)
      tryCatch({
        .pen_guardar_oc(pendiente$lote, pendiente$orden, pendiente$usuario)
        revision_ordenes(revision_ordenes() + 1L)
        shiny::removeModal()
        shiny::showNotification("Orden de compra asignada.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })
    .gc_descarga(output, datos, "Pendientes_gestion")
    list(total = shiny::reactive(nrow(datos())), datos = datos)
  })
}


# Tipificación reutilizable; las condiciones originales se conservan como motivo operativo ----
.PEN_TIPOS <- c("Sin asignar lote", "Pendiente de producir", "Pendiente de despachar", "Pendiente de facturar",
  "Pendiente de margen", "Inconsistencia de cantidades", "Otros pendientes")
.pen_tipificar <- function(x, maestro = NULL) {
  mapa <- c("Sin asignar" = "Sin asignar lote", "Por producir" = "Pendiente de producir",
    "Por despachar" = "Pendiente de despachar", "Por facturar" = "Pendiente de facturar",
    "Revisar margen" = "Pendiente de margen", "Revisar cantidades" = "Inconsistencia de cantidades",
    "Sin pedido" = "Inconsistencia de cantidades", "Sin lote" = "Inconsistencia de cantidades")
  x$TipoPendiente <- dplyr::coalesce(unname(mapa[x$Etapa]), "Otros pendientes")
  x$TipoPendiente[x$Etapa == "Revisar margen" & x$Nivel == "Pedido"] <- "Otros pendientes"
  dimensiones <- c("Categoria", "Producto", "CLLinNegNo", "Asesor")
  if (is.data.frame(maestro) && nrow(maestro)) {
    m <- maestro
    m$Pedido <- .crm_id_texto(m$CLPdcCod); m$Linea <- .crm_id_texto(m$CLPdcLin)
    m$Lote <- .crm_id_texto(m$CLLotCod)
    for (clave in list(c("Pedido", "Linea", "Lote"), c("Pedido", "Linea"))) {
      disponibles <- intersect(dimensiones, names(m))
      meta <- m %>% dplyr::group_by(dplyr::across(dplyr::all_of(clave))) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(disponibles), .gc_unico), .groups = "drop")
      names(meta)[match(disponibles, names(meta))] <- paste0(disponibles, "Origen")
      x <- dplyr::left_join(x, meta, by = clave, relationship = "many-to-one", na_matches = "never")
      for (d in disponibles) {
        if (!d %in% names(x)) x[[d]] <- NA_character_
        # No repartir una cantidad entre categorías contradictorias de una línea.
        x[[d]] <- dplyr::coalesce(as.character(x[[d]]), as.character(x[[paste0(d, "Origen")]]))
      }
      x <- dplyr::select(x, -dplyr::ends_with("Origen"))
    }
  }
  if (!"Motivo" %in% names(x)) x$Motivo <- x$Etapa
  x$Motivo <- dplyr::coalesce(x$Motivo, x$Etapa)
  dplyr::distinct(x)
}
.pen_resumen_tipo <- function(x) {
  contar <- function(campos) {
    if (!all(campos %in% names(x))) return(NA_integer_)
    z <- x[campos]; buenos <- Reduce(`&`, lapply(z, function(v) !is.na(v) & !as.character(v) %in% c("", "0")))
    if (!any(buenos)) NA_integer_ else nrow(unique(z[buenos, , drop = FALSE]))
  }
  data.frame(Clientes = contar("NIT"), Pedidos = contar("Pedido"), Lineas = contar(c("Pedido", "Linea")), Lotes = contar("Lote"),
    Sacos = if (length(unique(x$Nivel)) > 1L) NA_real_ else .com_suma(x$Cantidad), Registros = nrow(x))
}
