# Helpers ----
.header_valor <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || all(is.na(x))) return("—")
  format(sum(x, na.rm = TRUE), big.mark = SEP_MILES, decimal.mark = SEP_DECIMAL,
         trim = TRUE, scientific = FALSE)
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
  racafeForms::racafeStatusBadge(
    paste(nombre, valor), variant = "neutral", size = "xs"
  )
}

.header_tarjeta <- function(nombre, valor, detalle = NULL) {
  racafeForms::racafeMetric(
    value = valor, label = nombre, detail = detalle, padding = "compact"
  )
}

.header_tabla <- function(datos) {
  shiny::tags$div(
    style = "overflow:auto;max-height:190px;",
    shiny::tags$table(
      class = "table table-sm crm-header-table",
      style = "font-size:11px;white-space:nowrap;margin-bottom:0;background:transparent;",
      shiny::tags$thead(shiny::tags$tr(lapply(names(datos), shiny::tags$th))),
      shiny::tags$tbody(lapply(seq_len(nrow(datos)), function(i) {
        shiny::tags$tr(lapply(datos[i, , drop = FALSE], shiny::tags$td))
      }))
    )
  )
}

.header_modal_compacto <- function(altura = "75vh") {
  shinyjs::runjs(paste0(
    "$(document).one('show.bs.modal', '.modal', function() {",
    "$(this).addClass('subventana2');",
    "$(this).find('.modal-dialog').addClass('subventana2');",
    "$(this).find('.modal-body').css({'max-height':'", altura, "','overflow-y':'auto'});",
    "$(this).attr('data-keyboard', 'false');",
    "var modal = $(this).data('bs.modal');",
    "if (modal && modal._config) modal._config.keyboard = false;",
    "$(this).off('keydown.dismiss.bs.modal');",
    "});"
  ))
}

.header_resumen_cliente <- function(identidad, data_c, crm, antigua = FALSE,
                                    ncliente = NULL, por_clinit = FALSE,
                                    operaciones = NULL) {
  shiny::req(identidad)
  if (is.null(operaciones)) {
    return(shiny::tags$div(
      style = "padding:12px;color:#995b00;",
      shiny::tags$strong("Resumen operativo no disponible"),
      shiny::tags$p("El ETL aún no exporta crm_detalle_operaciones. ",
                    "Ejecute DataPrep_CRM_corregido.R y reinicie Shiny.")
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
  
  shiny::tags$div(
    style = "padding:0 2px 8px;",
    shiny::tags$div(style = "margin:8px 0;",
                    .header_badge("NIT:", identidad$nit),
                    .header_badge("Línea:", linea)),
    racafeForms::racafeMetricGrid(
      .header_tarjeta("Pedidos", nrow(pedidos),
                      paste("Creación:", .header_fecha(pedidos$PdcFecCre))),
      .header_tarjeta("Lotes", nrow(lotes)),
      .header_tarjeta("Producción (sacos)", .header_valor(lotes$SacProd)),
      .header_tarjeta("Despachos (sacos)", .header_valor(lotes$SacDesp)),
      .header_tarjeta("Sacos asignados", .header_valor(lotes$SacLote)),
      .header_tarjeta("Facturación (sacos)", .header_valor(lotes$SacosFact),
                      paste("Última:", .header_fecha(lotes$FecFact)))
    ),
    shiny::tags$div(
      style = "font-size:10px;color:#64748b;text-align:right;margin:4px 0 10px;",
      "Cantidades expresadas en sacos equivalentes de 70 kg."
    ),
    shiny::tags$strong(style = "font-size:12px;", "Productos y cantidades"),
    .header_tabla(por_producto),
    shiny::tags$div(
      style = "font-size:10px;color:#64748b;text-align:right;margin:4px 0 8px;",
      "Cantidades expresadas en sacos equivalentes de 70 kg."
    ),
    if (sin_pedido > 0 || sin_operacion > 0) shiny::tags$p(
      style = "font-size:11px;color:#916000;margin:7px 0;",
      paste(sin_pedido, "líneas sin fecha en EXPPEDID;",
            sin_operacion, "registros facturados sin fecha de producción o despacho en EXPCUALO.")
    ),
    racafeShiny::racafeCollapsible(
      id = "header_detalle_pedidos", title = "Detalle de pedidos, lotes y fechas", open = FALSE,
      .header_tabla(detalle),
      shiny::tags$div(
        style = "font-size:10px;color:#64748b;text-align:right;margin:4px 0 8px;",
        "Cantidades expresadas en sacos equivalentes de 70 kg."
      )
    )
  )
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
.header_lista_item <- function(titulo, detalle, badges = NULL, onclick = NULL) {
  shiny::tags$a(
    class = "dropdown-item crm-header-item",
    href = "javascript:void(0);", onclick = onclick,
    shiny::tags$div(class = "crm-header-item-titulo", titulo),
    shiny::tags$div(class = "crm-header-item-detalle", detalle),
    if (length(badges)) shiny::tags$div(
      class = "crm-header-item-badges", do.call(shiny::tagList, badges)
    )
  )
}

.header_lista_badge <- function(texto, variante = "neutral") {
  racafeForms::racafeStatusBadge(texto, variant = variante, size = "xs")
}

.header_items_clientes <- function(datos, tipo, input_id) {
  if (!nrow(datos)) {
    return(shiny::tags$div(class = "dropdown-item crm-header-vacio",
                           "No hay clientes por revisar."))
  }
  shiny::tagList(lapply(seq_len(nrow(datos)), function(i) {
    fila <- datos[i, , drop = FALSE]
    nit <- as.character(fila$CLCliNit[[1]])
    linea <- .header_nombre(fila$CLLinNegNo, as.character(fila$LinNegCod[[1]]))
    razon <- .header_nombre(fila$PerRazSoc, paste("NIT", nit))
    badges <- list(.header_lista_badge(paste("NIT:", nit), "neutral"),
                   .header_lista_badge(linea, "neutral"))
    if (identical(tipo, "antiguo")) {
      badges <- c(badges, list(
        .header_lista_badge(paste("Actualización:", fila$UltActualizacion[[1]]), "warning")
      ))
    }
    .header_lista_item(
      titulo = razon, detalle = paste("NIT:", nit), badges = badges,
      onclick = sprintf(
        "event.stopPropagation();Shiny.setInputValue('%s',%s,{priority:'event'});",
        input_id, jsonlite::toJSON(list(nit = nit, linneg = as.character(fila$LinNegCod[[1]])), auto_unbox = TRUE)
      )
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
HeaderTareasServer <- function(session, usuario, notas = NULL, actualizaciones = NULL) {
  drop <- NotificacionesTareas("HeaderTareas", usuario = usuario, notas = notas,
                               actualizaciones = actualizaciones)
  menu <- racafeModulos::MenuHeaderServer(
    id = "MenuTareas", items_r = drop$items,
    badgeStatus_r = drop$badge_st, badge_n_r = drop$n,
    headerText = NULL, href = "#",
    footerText = "Ver todas las tareas"
  )
  shiny::observeEvent(menu$footer_click(), {
    bs4Dash::updateTabItems(session, "menu_principal", selected = "GC_Tareas")
  })
  invisible(menu)
}

# Clientes ----
HeaderClientesServer <- function(data_c, datos_rv, clientes_raw, usuario,
                                 ncliente, output, es_admin) {
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
  
  output$resumen_cli_modal <- shiny::renderUI({
    .header_resumen_cliente(identidad_nueva(), datos_rv(), clientes_raw(),
                            ncliente = ncliente(), por_clinit = TRUE,
                            operaciones = get0("crm_detalle_operaciones", envir = .GlobalEnv))
  })
  
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
    identidad <- list(nit = fila$CLCliNit[[1]],
                      linneg_cod = fila$LinNegCod[[1]])
    if (antiguo) identidad_antigua(identidad) else identidad_nueva(identidad)
    titulo <- .header_nombre(fila$PerRazSoc, paste("NIT", identidad$nit))
    shiny::showModal(racafeForms::racafeModal(
      title = titulo, size = "subventana2",
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
    ))
  }
  
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
    modal_size = "m",
    modal_pre_fn = function(sel) .header_modal_compacto("75vh"),
    modal_titulo_fn = function(sel) "Migrar cartera entre asesores",
    modal_contenido_fn = function(sel) {
      MigrarCarteraUI(shiny::NS("MenuMigrarCartera"))
    }
  )
  
  MigrarCartera(
    id = "MenuMigrarCartera", usr = usuario,
    es_admin = es_admin, clientes_raw = clientes_raw
  )
  
  shinyjs::runjs("
    if (!window.crmMigracionAccesoDirecto) {
      window.crmMigracionAccesoDirecto = true;
      document.addEventListener('click', function(e) {
        var acceso = e.target.closest('#li_menu_migrar_cartera');
        if (!acceso) return;
        e.preventDefault();
        e.stopPropagation();
        e.stopImmediatePropagation();
        if (window.Shiny) {
          Shiny.setInputValue('crm_abrir_migracion', Date.now(),
                              {priority: 'event'});
        }
      }, true);
    }
  ")
  
  shiny::observeEvent(shiny::getDefaultReactiveDomain()$input$crm_abrir_migracion, {
    modal <- shiny::modalDialog(
      title = "Migrar cartera entre asesores",
      shiny::tags$style(shiny::HTML(
        ".crm-modal-migracion .modal-dialog {width:calc(100% - 24px)!important;
         max-width:720px!important;margin:30px auto!important;}
         .crm-modal-migracion .modal-body {max-height:75vh;overflow-y:auto;}"
      )),
      MigrarCarteraUI(shiny::NS("MenuMigrarCartera")),
      footer = racafeForms::racafeModalFooter(), size = "m", easyClose = FALSE
    )
    modal$attribs$class <- paste(modal$attribs$class, "crm-modal-migracion")
    shiny::showModal(modal)
  }, ignoreInit = TRUE)
  
  invisible(menu)
}
