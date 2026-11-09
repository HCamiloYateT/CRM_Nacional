# MODAL AMPLIO DEL CLIENTE ----
.header_cliente_modal_ui <- function(titulo, fila, identidad, antiguo = FALSE) {
  racafeForms::racafeModalDetail(
    title = .crm_titulo_modal(titulo, "building"), size = "ventana",
    if (antiguo) shiny::tagList(
      shiny::tags$div(style = "display:flex;gap:5px;margin-bottom:10px;flex-wrap:wrap;",
                      .header_badge("NIT:", identidad$nit),
                      .header_badge("Línea:", .header_nombre(
                        fila$CLLinNegNo, as.character(identidad$linneg_cod)))),
      shiny::uiOutput("resumen_cli_ant_modal"),
      .header_formulario_ui("mod_form_cli_ant")
    ) else shiny::tagList(
      shiny::uiOutput("resumen_cli_modal"),
      .header_formulario_ui("mod_form_cli")
    ),
    footer = racafeForms::racafeModalFooter(
      racafeForms::racafeButtonCancel(
        "header_cliente_cerrar", "Cerrar", size = "xs")
    )
  )
}

# DETALLE OPERATIVO DEL CLIENTE ----
.header_cliente_detalle_ui <- function(resumen, key) {
  labels <- c(pedidos = "Pedidos", lotes = "Lotes", produccion = "Producción (sacos)",
    despachos = "Despachos (sacos)", asignados = "Sacos asignados", facturacion = "Facturación (sacos)")
  racafeForms::racafeModalDetail(
    title = .crm_titulo_modal(paste("Detalle:", labels[[key]]), "table-list"), size = "ventana",
    .header_tabla(attr(resumen, "header_detalle"), min_width = "1500px",
                  nota = "Detalle operativo del cliente. Cantidades en sacos equivalentes de 70 kg."),
    footer = racafeForms::racafeModalFooter(
      racafeForms::racafeButton("header_cliente_volver", "Volver", variant = "secondary", size = "xs"),
      racafeForms::racafeButtonCancel("header_cliente_cerrar", "Cerrar", size = "xs")))
}

# Helpers ----
.header_valor <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || all(is.na(x))) return("—")
  format(sum(x, na.rm = TRUE), big.mark = SEP_MILES, decimal.mark = SEP_DECIMAL,
         trim = TRUE, scientific = FALSE)
}

.header_numero <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

.header_fecha <- function(x) {
  x <- suppressWarnings(as.Date(x))
  x <- x[!is.na(x)]
  if (!length(x)) return("—")
  format(max(x), "%d/%m/%Y")
}

.header_nombre <- function(x, fallback = "—") {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x)) & !x %in% c("SIN DATO", "NA")]
  if (!length(x)) return(fallback)
  x[[1]]
}

.header_badge <- function(nombre, valor) {
  racafeForms::racafeDataBadge(sub("[[:space:]:]+$", "", nombre), valor, variant = "neutral", size = "xs")
}

.header_tarjeta <- function(nombre, id, icono, detalle = NULL, mostrar_boton = FALSE) {
  .crm_caja_ui(id, icono = icono, texto = nombre, mostrar_boton = mostrar_boton,
                footer = detalle, tooltip = paste("Ver detalle de", tolower(nombre)))
}

.header_tabla <- function(datos, min_width = NULL, nota = NULL) {
  datos <- .crm_nits_tabla(datos)
  cifras <- grepl("^Sacos|^Lotes$|^Producidos$|^Despachados$|^Facturados$|^Meses para recuperar$", names(datos))
  align <- stats::setNames(ifelse(cifras | vapply(datos, is.numeric, logical(1)), "right", "left"), names(datos))
  if ("Año" %in% names(datos)) align[["Año"]] <- "left"
  shiny::tagList(racafeShiny::racafeTableVancouver(datos, min_width = min_width, align = align, decimals = NULL,
    font_size = 11, row_padding = 2, header_color = "#495057", header_width = 1.5), .crm_nota(nota))
}

# INDICADORES REACTIVOS DEL CLIENTE ----
.header_kpis_server <- function(resumen, on_detail = NULL) {
  keys <- c("pedidos", "lotes", "produccion", "despachos", "asignados", "facturacion")
  stats::setNames(lapply(keys, function(key) {
    modulo <- racafeModulos::cajaValor_server(paste0("header_cli_", key), valor_r = shiny::reactive({
      attr(resumen(), "header_kpis")[[key]]
    }), formato = if (key %in% c("pedidos", "lotes")) "entero" else "numero", empty = "—")
    if (key %in% c("pedidos", "lotes", "facturacion") && is.function(on_detail)) {
      shiny::observeEvent(modulo$click_detalle(), on_detail(key))
    }
    modulo
  }), keys)
}


.header_resumen_cliente <- function(identidad, data_c, crm, antigua = FALSE,
                                    ncliente = NULL, por_clinit = FALSE,
                                    operaciones = NULL) {
  shiny::req(identidad)
  if (is.null(operaciones)) {
    return(shiny::tags$div(
      style = "padding:12px;color:#995b00;",
      shiny::tags$strong("Resumen operativo no disponible"),
      shiny::tags$p("Actualice los datos para consultar pedidos, lotes y facturación.")
    ))
  }
  
  columna_id <- if (por_clinit) "CLCliNit" else "CliNitPpal"
  datos <- data_c %>%
    dplyr::filter(as.character(.data[[columna_id]]) == as.character(identidad$nit),
                  as.character(LinNegCod) == as.character(identidad$linneg_cod))
  
  nombre <- .header_nombre(datos$PerRazSoc, "")
  if (!nzchar(nombre) && !is.null(ncliente)) {
    nombre <- ncliente %>%
      dplyr::filter(as.character(PerCod) == as.character(identidad$nit)) %>%
      dplyr::pull(PerRazSoc) %>%
      .header_nombre("")
  }
  if (!nzchar(nombre)) nombre <- paste("NIT", identidad$nit)
  linea <- .header_nombre(datos$CLLinNegNo,
                          as.character(identidad$linneg_cod))
  
  ids <- if (por_clinit) as.character(identidad$nit) else {
    unique(as.character(datos$CLCliNit))
  }
  ops <- operaciones %>%
    dplyr::filter(as.character(CLCliNit) %in% ids,
                  as.character(LinNegCod) == as.character(identidad$linneg_cod))
  
  if (!nrow(ops)) return(shiny::tags$div(
    shiny::tags$strong(nombre),
    shiny::tags$p(.header_badge("NIT:", identidad$nit),
                  .header_badge("Línea:", linea)),
    shiny::tags$p("No existen pedidos o lotes vinculados en la fuente operativa.")
  ))
  
  ops <- ops %>%
    dplyr::mutate(
      Pedido = as.character(CLPdcCod),
      Linea = as.character(CLPdcLin),
      Lote = as.character(CLLotCod),
      Producto = dplyr::coalesce(ProductoComercial,
                                 paste("Sin nombre comercial (", LinProCod, ")"))
    )
  
  pedidos <- ops %>%
    dplyr::filter(!is.na(CLPdcCod)) %>%
    dplyr::distinct(CLPdcCod, CLPdcLin, .keep_all = TRUE)
  lotes <- ops %>%
    dplyr::filter(!is.na(CLLotCod)) %>%
    dplyr::distinct(CLLotCod, .keep_all = TRUE)
  
  por_producto <- lotes %>%
    dplyr::group_by(Producto) %>%
    dplyr::summarise(
      Lotes = dplyr::n(),
      Sacos = sum(SacLote, na.rm = TRUE),
      Producidos = sum(SacProd, na.rm = TRUE),
      Despachados = sum(SacDesp, na.rm = TRUE),
      Facturados = sum(SacosFact, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(c(Sacos, Producidos, Despachados, Facturados),
                                ~ .header_valor(.x)))
  
  detalle <- ops %>%
    dplyr::arrange(dplyr::desc(PdcFecCre), Pedido, Linea, Lote) %>%
    dplyr::transmute(
      Pedido, Línea = Linea,
      `Fecha pedido` = ifelse(is.na(PdcFecCre), "—", format(PdcFecCre, "%d/%m/%Y")),
      Producto, Lote = dplyr::coalesce(Lote, "—"),
      `Sacos línea` = ifelse(is.na(SacosPedido), "—", format(round(SacosPedido, 2), big.mark = SEP_MILES,
                                                             decimal.mark = SEP_DECIMAL, trim = TRUE)),
      `Sacos lote` = ifelse(is.na(SacLote), "—", format(round(SacLote, 2), big.mark = SEP_MILES,
                                                        decimal.mark = SEP_DECIMAL, trim = TRUE)),
      `Sacos producidos` = ifelse(is.na(SacProd), "—", format(round(SacProd, 2), big.mark = SEP_MILES,
                                                              decimal.mark = SEP_DECIMAL, trim = TRUE)),
      Producción = ifelse(is.na(FecProd), "—", format(FecProd, "%d/%m/%Y")),
      `Sacos despachados` = ifelse(is.na(SacDesp), "—", format(round(SacDesp, 2), big.mark = SEP_MILES,
                                                               decimal.mark = SEP_DECIMAL, trim = TRUE)),
      Despacho = ifelse(is.na(FecDesp), "—", format(FecDesp, "%d/%m/%Y")),
      `Sacos facturados` = ifelse(is.na(SacosFact), "—", format(round(SacosFact, 2), big.mark = SEP_MILES,
                                                                decimal.mark = SEP_DECIMAL, trim = TRUE)),
      Factura = ifelse(is.na(FecFact), "—", format(FecFact, "%d/%m/%Y")),
      Fuente = dplyr::coalesce(FuenteFactura, "SIN FACTURA")
    ) %>%
    utils::head(30)
  
  sin_pedido <- sum(is.na(pedidos$PdcFecCre))
  sin_operacion <- ops %>%
    dplyr::filter(!is.na(FecFact) & (is.na(FecProd) | is.na(FecDesp))) %>%
    nrow()
  
  contenido <- shiny::tags$div(
    style = "padding:0 2px 8px;",
    shiny::tags$div(style = "margin:8px 0;",
                    .header_badge("NIT:", identidad$nit),
                    .header_badge("Línea:", linea)),
    racafeForms::racafeMetricGrid(
      .header_tarjeta("Pedidos", "header_cli_pedidos", "cart-shopping",
        paste("Creación:", .header_fecha(pedidos$PdcFecCre)), mostrar_boton = TRUE),
      .header_tarjeta("Lotes", "header_cli_lotes", "boxes-stacked", "Lotes únicos vinculados al cliente.",
                       mostrar_boton = TRUE),
      .header_tarjeta("Producción (sacos)", "header_cli_produccion", "industry", "Sacos producidos de 70 kg."),
      .header_tarjeta("Despachos (sacos)", "header_cli_despachos", "truck", "Sacos despachados de 70 kg."),
      .header_tarjeta("Sacos asignados", "header_cli_asignados", "weight-hanging", "Sacos asignados de 70 kg."),
      .header_tarjeta("Facturación (sacos)", "header_cli_facturacion", "file-invoice-dollar",
        paste("Última:", .header_fecha(lotes$FecFact)), mostrar_boton = TRUE)
    ),
    .crm_nota("Cantidades expresadas en sacos equivalentes de 70 kg."),
    shiny::tags$strong(style = "font-size:12px;", "Productos y cantidades"),
    .header_tabla(por_producto),
    .crm_nota("Cantidades expresadas en sacos equivalentes de 70 kg."),
    if (sin_pedido > 0 || sin_operacion > 0) .crm_nota(
      paste(sin_pedido, "líneas sin fecha en EXPPEDID;",
            sin_operacion, "registros facturados sin fecha de producción o despacho en EXPCUALO.")
    ),
    racafeShiny::racafeCollapsible(
      id = "header_detalle_pedidos", title = "Detalle de pedidos, lotes y fechas", open = FALSE,
      .header_tabla(detalle, min_width = "1500px"),
      .crm_nota("Cantidades expresadas en sacos equivalentes de 70 kg.")
    )
  )
  attr(contenido, "header_detalle") <- detalle
  attr(contenido, "header_kpis") <- list(pedidos = nrow(pedidos), lotes = nrow(lotes),
    produccion = .header_numero(lotes$SacProd), despachos = .header_numero(lotes$SacDesp),
    asignados = .header_numero(lotes$SacLote), facturacion = .header_numero(lotes$SacosFact))
  contenido
}

# Historial comercial ----
.header_historial_comercial <- function(identidad, clientes_raw, ncliente = NULL) {
  shiny::req(identidad)
  
  registros <- clientes_raw %>%
    dplyr::filter(
      as.character(CLCliNit) == as.character(identidad$nit),
      as.character(LinNegCod) == as.character(identidad$linneg_cod)
    ) %>%
    dplyr::mutate(FecProceso = as.Date(FecProceso)) %>%
    dplyr::filter(!is.na(FecProceso),
                  lubridate::year(FecProceso) < lubridate::year(Sys.Date())) %>%
    dplyr::arrange(dplyr::desc(FecProceso))
  
  if (!nrow(registros)) {
    return(shiny::tags$p(
      style = "font-size:11px;color:#64748b;margin:8px 0;",
      "No se encontraron registros comerciales de años anteriores."
    ))
  }
  
  actual <- registros %>%
    dplyr::group_by(Anio = lubridate::year(FecProceso)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()
  
  campos <- c("FecProceso", "CliNitPpal", "Asesor", "Segmento",
              "Excluir", "NumMesesRecuperar")
  for (campo in setdiff(campos, names(actual))) actual[[campo]] <- NA
  
  nombre_padre <- rep(NA_character_, nrow(actual))
  if (!is.null(ncliente) && all(c("PerCod", "PerRazSoc") %in% names(ncliente))) {
    nombres <- setNames(as.character(ncliente$PerRazSoc),
                        as.character(ncliente$PerCod))
    nombre_padre <- unname(nombres[as.character(actual$CliNitPpal)])
  }
  
  padre <- as.character(actual$CliNitPpal)
  padre[is.na(padre) | !nzchar(padre)] <- "—"
  padre <- ifelse(!is.na(nombre_padre) & nzchar(nombre_padre),
                  paste(padre, nombre_padre, sep = " - "), padre)
  
  mostrar <- function(x) {
    valor <- as.character(x)
    valor[is.na(valor) | !nzchar(trimws(valor)) |
            valor %in% c("NA", "SIN DATO")] <- "Ninguno"
    valor
  }
  
  tabla <- data.frame(
    Año = lubridate::year(actual$FecProceso),
    `Última actualización` = format(actual$FecProceso, "%d/%m/%Y"),
    `Cuenta principal` = padre,
    Asesor = mostrar(actual$Asesor),
    Segmento = mostrar(actual$Segmento),
    Excluir = mostrar(actual$Excluir),
    `Meses para recuperar` = mostrar(actual$NumMesesRecuperar),
    check.names = FALSE
  )
  
  shiny::tags$div(
    style = "padding:0 2px 6px;",
    shiny::tags$div(
      style = "display:flex;justify-content:space-between;align-items:center;",
      shiny::tags$strong(style = "font-size:12px;", "Historial comercial"),
      shiny::tags$span(
        style = "font-size:10px;color:#64748b;",
        paste("Última actualización:",
              format(max(registros$FecProceso), "%d/%m/%Y"))
      )
    ),
    .header_tabla(tabla),
    shiny::tags$p(
      style = "font-size:10px;color:#64748b;margin:5px 0 9px;",
      "Se presenta el registro más reciente de cada año. ",
      "Revise la información anterior y actualice los datos del año vigente."
    )
  )
}

# Formulario del header ----
.header_formulario_ui <- function(id) {
  IndFormularioUI(id)
}


# Listados del header ----
.header_lista_item <- function(titulo, detalle, badges = NULL, id = NULL, value = NULL, metadata = NULL) {
  racafeShiny::racafeListItem(titulo, subtitle = detalle, metadata = metadata, badges = badges, id = id, value = value)
}

.header_lista_badge <- function(texto, variante = "neutral") {
  racafeForms::racafeStatusBadge(texto, variant = variante, size = "xs")
}

.header_items_clientes <- function(datos, tipo, input_id) {
  if (!nrow(datos)) {
    return(racafeShiny::racafeListGroup(empty_message = "No hay clientes por revisar."))
  }
  do.call(racafeShiny::racafeListGroup, lapply(seq_len(nrow(datos)), function(i) {
    fila <- datos[i, , drop = FALSE]
    nit <- as.character(fila$CLCliNit[[1]])
    linea <- .header_nombre(fila$CLLinNegNo, as.character(fila$LinNegCod[[1]]))
    razon <- .header_nombre(fila$PerRazSoc, paste("NIT", nit))
    metadata <- c(Línea = linea)
    if (identical(tipo, "antiguo")) {
      metadata <- c(metadata, Actualización = as.character(fila$UltActualizacion[[1]]))
    }
    .header_lista_item(
      titulo = razon, detalle = paste("NIT:", nit), metadata = metadata,
      id = input_id, value = list(nit = nit, linneg = as.character(fila$LinNegCod[[1]]))
    )
  }))
}

# Indicadores ----
HeaderIndicadoresServer <- function(session) {
  TablaIndicadores("ind_kpis")
  res <- racafeModulos::MenuHeaderServer(
    id = "MenuIndicadores", items_r = TablaIndicadoresUI("ind_kpis"),
    headerText = NULL, href = "#",
    footerText = "Comparación de indicadores"
  )
  shiny::observeEvent(res$footer_click(), {
    bs4Dash::updateTabItems(session, "menu_principal",
                            selected = "GE_Comparacion")
  })
  invisible(res)
}

# Tareas ----
HeaderTareasServer <- function(session, usuario, notas = NULL, actualizaciones = NULL, on_success = NULL,
                               es_admin = function() FALSE) {
  drop <- NotificacionesTareas("HeaderTareas", usuario = usuario, notas = notas,
                               actualizaciones = actualizaciones, on_success = on_success, es_admin = es_admin)
  menu <- racafeModulos::MenuHeaderServer(
    id = "MenuTareas", items_r = drop$items,
    badgeStatus_r = drop$badge_st, badge_n_r = drop$n,
    headerText = NULL, href = "#",
    footerText = "Ver todas las tareas"
  )
  shiny::observeEvent(menu$footer_click(), {
    bs4Dash::updateTabItems(session, "menu_principal", selected = "GC_Tareas")
  })
  menu$tareas <- drop
  invisible(menu)
}

# Clientes ----
HeaderClientesServer <- function(data_c, datos_rv, clientes_raw, usuario,
                                 ncliente, output, es_admin, operaciones = NULL) {
  crm_reciente <- shiny::reactive({
    clientes_raw() %>%
      dplyr::mutate(FecProceso = as.Date(FecProceso)) %>%
      dplyr::group_by(CliNitPpal, LinNegCod) %>%
      dplyr::arrange(dplyr::desc(FecProceso), .by_group = TRUE) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  })
  
  # Clientes sin coincidencia ----
  clientes_operacion <- shiny::reactive({
    nombres <- ncliente() %>%
      dplyr::transmute(
        codigo = as.character(PerCod),
        nombre_ncliente = as.character(PerRazSoc)
      ) %>%
      dplyr::distinct(codigo, .keep_all = TRUE)
    
    datos_rv() %>%
      dplyr::mutate(
        FecFact = as.Date(FecFact),
        CLCliNit = as.character(CLCliNit),
        CliNitPpal = as.character(CliNitPpal),
        LinNegCod = as.character(LinNegCod)
      ) %>%
      dplyr::filter(
        !is.na(FecFact),
        FecFact >= as.Date("2025-01-01"),
        !is.na(CLCliNit),
        !is.na(LinNegCod)
      ) %>%
      dplyr::arrange(dplyr::desc(FecFact)) %>%
      dplyr::distinct(CLCliNit, LinNegCod, .keep_all = TRUE) %>%
      dplyr::left_join(
        nombres %>% dplyr::rename(CLCliNit = codigo, nombre_hija = nombre_ncliente),
        by = "CLCliNit"
      ) %>%
      dplyr::left_join(
        nombres %>% dplyr::rename(CliNitPpal = codigo, nombre_padre = nombre_ncliente),
        by = "CliNitPpal"
      ) %>%
      dplyr::mutate(
        PerRazSoc = dplyr::coalesce(dplyr::na_if(PerRazSoc, "SIN DATO"),
                                    nombre_hija, nombre_padre, paste("NIT", CLCliNit)),
        CLLinNegNo = dplyr::coalesce(CLLinNegNo, LinNegCod),
        label = paste0(CLLinNegNo, " — ", PerRazSoc)
      ) %>%
      dplyr::select(CLCliNit, CliNitPpal, LinNegCod, PerRazSoc, CLLinNegNo, label)
  })
  
  incompletos <- shiny::reactive({
    registrados <- clientes_raw() %>%
      dplyr::transmute(
        CLCliNit = as.character(CLCliNit),
        LinNegCod = as.character(LinNegCod)
      ) %>%
      dplyr::distinct()
    
    clientes_operacion() %>%
      dplyr::anti_join(registrados, by = c("CLCliNit", "LinNegCod")) %>%
      dplyr::arrange(CLLinNegNo, PerRazSoc)
  })
  
  antiguos <- shiny::reactive({
    anho <- lubridate::year(Sys.Date())
    historicos <- clientes_raw() %>%
      dplyr::mutate(
        CLCliNit = as.character(CLCliNit),
        LinNegCod = as.character(LinNegCod),
        FecProceso = as.Date(FecProceso)
      ) %>%
      dplyr::filter(!is.na(CLCliNit), !is.na(LinNegCod),
                    !is.na(FecProceso)) %>%
      dplyr::group_by(CLCliNit, LinNegCod) %>%
      dplyr::summarise(
        UltActualizacion = max(FecProceso),
        RegistroVigente = any(lubridate::year(FecProceso) == anho),
        .groups = "drop"
      ) %>%
      dplyr::filter(lubridate::year(UltActualizacion) < anho,
                    !RegistroVigente)
    
    operaciones_vigentes <- datos_rv() %>%
      dplyr::filter(!is.na(FecFact),
                    lubridate::year(as.Date(FecFact)) == anho) %>%
      dplyr::transmute(CLCliNit = as.character(CLCliNit),
                       LinNegCod = as.character(LinNegCod)) %>%
      dplyr::distinct()
    
    clientes_operacion() %>%
      dplyr::inner_join(operaciones_vigentes,
                        by = c("CLCliNit", "LinNegCod")) %>%
      dplyr::inner_join(historicos, by = c("CLCliNit", "LinNegCod")) %>%
      dplyr::mutate(
        label = paste0(CLLinNegNo, " — ", PerRazSoc),
        UltActualizacion = format(UltActualizacion, "%d/%m/%Y")
      ) %>%
      dplyr::arrange(CLLinNegNo, PerRazSoc) %>%
      dplyr::select(CLCliNit, LinNegCod, PerRazSoc, CLLinNegNo,
                    UltActualizacion, label)
  })
  
  crear_formulario <- function(id, identidad, por_clinit = FALSE) {
    IndFormulario(
      id = id,
      identidad = shiny::reactive(identidad()),
      dat = shiny::reactive({
        sel <- identidad()
        shiny::req(sel)
        
        columna_id <- if (por_clinit) "CLCliNit" else "CliNitPpal"
        base <- data_c() %>%
          dplyr::filter(as.character(.data[[columna_id]]) == as.character(sel$nit),
                        as.character(LinNegCod) == as.character(sel$linneg_cod))
        if (nrow(base)) return(base)
        
        operacion <- datos_rv() %>%
          dplyr::filter(as.character(.data[[columna_id]]) == as.character(sel$nit),
                        as.character(LinNegCod) == as.character(sel$linneg_cod))
        if (nrow(operacion)) return(operacion)
        
        crm <- clientes_raw() %>%
          dplyr::filter(as.character(.data[[columna_id]]) == as.character(sel$nit),
                        as.character(LinNegCod) == as.character(sel$linneg_cod)) %>%
          dplyr::mutate(FecProceso = as.Date(FecProceso)) %>%
          dplyr::group_by(CLCliNit) %>%
          dplyr::arrange(dplyr::desc(FecProceso), .by_group = TRUE) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::ungroup()
        shiny::req(nrow(crm) > 0L)
        
        nombres <- ncliente() %>%
          dplyr::transmute(codigo = as.character(PerCod), nombre = PerRazSoc) %>%
          dplyr::distinct(codigo, .keep_all = TRUE)
        padre <- nombres %>% dplyr::filter(codigo == as.character(sel$nit))
        razon <- if (nrow(padre)) padre$nombre[[1]] else paste("NIT", sel$nit)
        
        crm %>%
          dplyr::mutate(codigo = as.character(CLCliNit)) %>%
          dplyr::left_join(nombres, by = "codigo") %>%
          dplyr::mutate(
            PerRazSoc = razon,
            RazonSocialCliNit = dplyr::coalesce(nombre, paste("NIT", codigo))
          )
      }),
      usr = usuario,
      ncliente = ncliente,
      identificar_por_clinit = por_clinit,
      es_admin = es_admin
    )
  }
  
  identidad_nueva <- shiny::reactiveVal(NULL)
  identidad_antigua <- shiny::reactiveVal(NULL)
  cliente_modal_rv <- shiny::reactiveVal(NULL)
  form_nuevo <- crear_formulario("mod_form_cli", identidad_nueva, por_clinit = TRUE)
  form_antiguo <- crear_formulario("mod_form_cli_ant", identidad_antigua, por_clinit = TRUE)
  
  # Actualización ----
  actualizar_clientes <- function() {
    clientes_raw(racafeBD::CargarDatos("CRMNALCLIENTE"))
  }
  
  shiny::observeEvent(form_nuevo$refresh(), {
    shiny::req(form_nuevo$refresh() > 0L)
    actualizar_clientes()
    shiny::removeModal()
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(form_antiguo$refresh(), {
    shiny::req(form_antiguo$refresh() > 0L)
    actualizar_clientes()
    shiny::removeModal()
  }, ignoreInit = TRUE)
  
  resumen_nuevo <- shiny::reactive({
    .header_resumen_cliente(identidad_nueva(), datos_rv(), clientes_raw(),
                            ncliente = ncliente(), por_clinit = TRUE,
                            operaciones = if (shiny::is.reactive(operaciones)) operaciones() else operaciones)
  })
  
  .header_kpis_server(resumen_nuevo, on_detail = function(key) {
    shiny::showModal(.header_cliente_detalle_ui(resumen_nuevo(), key))
  })
  output$resumen_cli_modal <- shiny::renderUI(resumen_nuevo())

  output$resumen_cli_ant_modal <- shiny::renderUI({
    .header_historial_comercial(identidad_antigua(), clientes_raw(),
                                ncliente = ncliente())
  })
  
  sesion <- shiny::getDefaultReactiveDomain()
  shiny::req(sesion)
  
  items_nuevos <- shiny::reactive({
    .header_items_clientes(incompletos(), "nuevo", "header_cliente_nuevo")
  })
  items_antiguos <- shiny::reactive({
    .header_items_clientes(antiguos(), "antiguo", "header_cliente_antiguo")
  })
  
  mostrar_cliente <- function(fila, antiguo = FALSE) {
    cliente_modal_rv(list(fila = fila, antiguo = antiguo))
    identidad <- list(nit = fila$CLCliNit[[1]],
                      linneg_cod = fila$LinNegCod[[1]])
    if (antiguo) identidad_antigua(identidad) else identidad_nueva(identidad)
    titulo <- .header_nombre(fila$PerRazSoc, paste("NIT", identidad$nit))
    shiny::showModal(.header_cliente_modal_ui(titulo, fila, identidad, antiguo))
  }
  
  shiny::observeEvent(sesion$input$header_cliente_volver, {
    actual <- cliente_modal_rv()
    shiny::req(actual)
    mostrar_cliente(actual$fila, actual$antiguo)
  })

  shiny::observeEvent(sesion$input$header_cliente_cerrar, {
    shiny::removeModal()
  })
  
  seleccionar_cliente <- function(datos, seleccionado, antiguo = FALSE) {
    if (is.null(seleccionado$nit) || is.null(seleccionado$linneg)) return()
    indice <- which(
      as.character(datos$CLCliNit) == as.character(seleccionado$nit) &
        as.character(datos$LinNegCod) == as.character(seleccionado$linneg)
    )
    if (length(indice) != 1L) return()
    mostrar_cliente(datos[indice, , drop = FALSE], antiguo = antiguo)
  }
  
  shiny::observeEvent(sesion$input$header_cliente_nuevo, {
    seleccionar_cliente(incompletos(), sesion$input$header_cliente_nuevo)
  })
  shiny::observeEvent(sesion$input$header_cliente_antiguo, {
    seleccionar_cliente(antiguos(), sesion$input$header_cliente_antiguo, antiguo = TRUE)
  })
  
  menu_nuevo <- racafeModulos::MenuHeaderServer(
    id = "MenuClientes", items_r = items_nuevos,
    badgeStatus_r = "danger", badge_n_r = shiny::reactive(nrow(incompletos())),
    headerText = NULL, href = NULL
  )
  menu_antiguo <- racafeModulos::MenuHeaderServer(
    id = "MenuClientesAnt", items_r = items_antiguos,
    badgeStatus_r = "warning", badge_n_r = shiny::reactive(nrow(antiguos())),
    headerText = NULL, href = NULL
  )
  
  invisible(list(nuevos = menu_nuevo, antiguos = menu_antiguo))
}

# Migración de Cartera ----
HeaderMigrarCarteraServer <- function(usuario, clientes_raw, es_admin) {
  menu <- racafeModulos::MenuHeaderServer(
    id = "MenuMigrarCartera",
    items_r = shiny::reactive(data.frame(
      id = "migrar", label = "Migrar Cartera"
    )),
    key_cols = "id",
    badgeStatus_r = NULL,
    showHeader = FALSE,
    href = NULL,
    modal_icon = NULL,
    modal_size = "subventana2",
    modal_titulo_fn = function(sel) "Migrar cartera entre asesores",
    modal_contenido_fn = function(sel) {
      MigrarCarteraUI(shiny::NS("MenuMigrarCartera"))
    }
  )
  
  MigrarCartera(
    id = "MenuMigrarCartera", usr = usuario,
    es_admin = es_admin, clientes_raw = clientes_raw
  )

  invisible(menu)
}



