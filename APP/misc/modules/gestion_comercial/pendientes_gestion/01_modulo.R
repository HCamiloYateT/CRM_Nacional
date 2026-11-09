# Flujo completo: cada saldo conserva su grano y su unidad ----
.PEN_ETAPAS <- c("Sin asignar", "Por producir", "Por despachar", "Por facturar", "Futuros", "Vencidos",
                 "Sin fecha estimada", "Revisar margen", "Sin pedido", "Sin lote", "Revisar cantidades")
.pen_preparar <- function(maestro, hoy = Sys.Date()) {
  requeridas <- c("CLPdcCod", "CLPdcLin", "CLLotCod", "EstadoFinanciero", "SacosPedido70Linea",
                 "SacosLote70", "SacosProducidos70", "SacosDespachados70", "SacosFact", "Margen", "FecFact",
                 "FacturadosOperativos70")
  if (!all(requeridas %in% names(maestro))) stop("Actualice DataPrep: faltan campos del flujo comercial.")
  x <- maestro
  x$Pedido <- as.character(x$CLPdcCod)
  x$Linea <- as.character(x$CLPdcLin)
  x$Lote <- as.character(x$CLLotCod)
  x$NIT <- as.character(x$CliNitPpal)
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
      Despachados = .gc_unico(SacosDespachados70), Facturados = .gc_unico(FacturadosOperativos70),
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
  activos <- pedidos[pedidos$Activo, , drop = FALSE]
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
  revisar <- which(is.na(lotes$Asignados) | is.na(lotes$Producidos) | is.na(lotes$Despachados) |
    lotes$Producidos > lotes$Asignados + 0.1 | lotes$Despachados > lotes$Producidos + 0.1 |
    lotes$Facturados > lotes$Despachados + 0.1 |
    lotes$FacturadosPeriodo > lotes$Facturados + 0.1)
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
  racafeForms::racafeForm(variant = "plain",
    .gc_tarjetas_ui(ns, c(pedidos = "Pedidos por gestionar", producir = "Lotes por producir",
      despachar = "Lotes por despachar", facturar = "Lotes por facturar")),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputSelect("etapa", "Pendiente", choices = c("Todos", .PEN_ETAPAS), ns = ns),
      racafeForms::racafeInputSwitch("incluir_futuros", "Incluir pedidos futuros", value = TRUE, ns = ns)),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("detalle")),
    .crm_nota(c("Cantidades en sacos equivalentes de 70 kg; tolerancia: 0,1 sacos.",
      "Los saldos por etapa no se suman: un pedido puede requerir varias gestiones.",
      "Los futuros y vencidos usan la fecha estimada del pedido, sin filtrar por fecha de factura.",
      "Unidades desconocidas se muestran vacías para revisión; el margen abierto no es margen contable.")))
}
PendientesGestion <- function(id, maestro, usuario = function() "SIN IDENTIFICAR") {
  shiny::moduleServer(id, function(input, output, session) {
    lote_seleccionado <- shiny::reactiveVal(NULL)
    orden_pendiente <- shiny::reactiveVal(NULL)
    revision_ordenes <- shiny::reactiveVal(0L)
    flujo <- shiny::reactive(.pen_preparar(maestro()))
    datos <- shiny::reactive({
      x <- flujo()$detalle
      if (!is.null(input$etapa) && input$etapa != "Todos") x <- x[x$Etapa == input$etapa, ]
      if (identical(input$incluir_futuros, FALSE)) {
        claves <- flujo()$pedidos %>% dplyr::filter(!is.na(Fecha), Fecha > Sys.Date())
        x <- dplyr::anti_join(x, claves[c("Pedido", "Linea")], by = c("Pedido", "Linea"))
      }
      x
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
    output$bitacora_oc <- shiny::renderUI({
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
