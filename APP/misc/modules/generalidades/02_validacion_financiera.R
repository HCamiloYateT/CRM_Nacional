# Validación Financiera ----

# Constantes ----
.VF_ANHO <- lubridate::year(Sys.Date())
.VF_TOLERANCIA_SACOS <- 0.1
.VF_CUADRE_MINIMO <- 0.99
.VF_MESES <- c(
  "ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
  "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"
)


# UI ----
ValidacionFinancieraUI <- function(id) {
  ns <- shiny::NS(id)
  
  .kpi_ui <- function(id_kpi, texto, icono) {
    shiny::tags$div(class = "vf-kpi-accion",
      .crm_caja_output(ns(id_kpi)))
  }
  
  .tarjetas <- list(
    c("kpi_cuadre", "Cuadre de sacos", "scale-balanced"),
    c("kpi_lotes", "Lotes con diferencia", "not-equal"),
    c("kpi_fact_sin_pyg", "Facturados sin PyG", "file-circle-xmark"),
    c("kpi_pyg_sin_fact", "PyG sin factura", "receipt"),
    c("kpi_pyg_sin_lote", "PyG sin lote", "box-open"),
    c("kpi_linea", "Línea de negocio", "shuffle"),
    c("kpi_segmentos", "Segmentos diferentes", "users-viewfinder"),
    c("kpi_calidad", "Calidad PyG", "triangle-exclamation")
  )

  ids_carga <- c(
    ns("tabla_extraccion_pyg"),
    vapply(.tarjetas, function(x) ns(paste0(x[[1]], "-valor_display")), "")
  )
  ids_js <- jsonlite::toJSON(unname(ids_carga), auto_unbox = FALSE)
  contenedor_js <- jsonlite::toJSON(ns("vf_contenido"), auto_unbox = TRUE)
  preloader_js <- jsonlite::toJSON(ns("vf_preloader"), auto_unbox = TRUE)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(sprintf("
      #%s { position:relative; min-height:260px; }
      #%s .racafe-caja-valor:not(.rmcv-card) { min-height:102px; padding:10px 12px;
        border:1px solid #E5E7EB; box-shadow:0 1px 3px rgba(0,0,0,.06); }
      #%s .racafe-caja-valor:not(.rmcv-card) .fa, #%s .racafe-caja-valor:not(.rmcv-card) svg { color:#DA291C!important; }
      #%s .racafe-caja-valor:not(.rmcv-card) .shiny-html-output { font-size:1.12rem; }
      #%s .vf-kpi-accion { cursor:default; margin-bottom:8px; }
      #%s .vf-kpi-accion:focus-visible { outline:2px solid #DA291C; outline-offset:2px; }
      #%s { position:absolute; inset:0; z-index:25; display:flex;
        align-items:center; justify-content:center; background:rgba(255,255,255,.92); }
      #%s .vf-spinner { width:35px; height:35px; border:3px solid #E5E7EB;
        border-top-color:#DA291C; border-radius:50%%; animation:vf-spin .8s linear infinite; }
      @keyframes vf-spin { to { transform:rotate(360deg); } }
    ", ns("vf_contenido"), ns("vf_contenido"), ns("vf_contenido"),
                                          ns("vf_contenido"), ns("vf_contenido"), ns("vf_contenido"),
                                          ns("vf_preloader"), ns("vf_preloader"), ns("vf_preloader")))),
    shiny::tags$div(
      id = ns("vf_contenido"),
      shiny::fluidRow(
        shiny::column(
          8,
          racafeShiny::FormatearTexto(
            "Validación financiera - meses cerrados",
            negrita = TRUE, tamano_pct = 1
          ),
          gt::gt_output(ns("tabla_extraccion_pyg")),
          shiny::uiOutput(ns("auditoria_base"))
        ),
        shiny::column(
          4,
          shiny::fluidRow(lapply(.tarjetas, function(item) {
            shiny::column(6, .kpi_ui(item[[1]], item[[2]], item[[3]]))
          }))
        )
      ),
      shiny::tags$div(
        id = ns("vf_preloader"), role = "status", `aria-live` = "polite",
        shiny::tags$div(style = "text-align:center;",
                        shiny::tags$div(class = "vf-spinner", style = "margin:auto;"),
                        shiny::tags$div(style = "margin-top:12px;color:#374151;font-size:12px;",
                                        "Cargando validación financiera..."))
      )
    ),
    shiny::tags$script(shiny::HTML(sprintf("
      (function() {
        var rootId = %s, loaderId = %s, ids = %s;
        function inspect() {
          var root = document.getElementById(rootId);
          if (!root) return false;
          return ids.every(function(id) {
            var el = document.getElementById(id);
            if (!el) return false;
            if (el.classList.contains('shiny-output-error')) return true;
            if (id.indexOf('tabla_extraccion_pyg') >= 0) {
              return !!el.querySelector('.gt_table, table');
            }
            return el.children.length > 0 || el.textContent.trim().length > 0;
          });
        }
        function ready() {
          if (!inspect()) return false;
          var loader = document.getElementById(loaderId);
          if (loader) loader.style.display = 'none';
          return true;
        }
        function init() {
          var root = document.getElementById(rootId);
          if (!root) return;
          if (ready()) return;
          var observer = new MutationObserver(function() {
            if (ready()) observer.disconnect();
          });
          observer.observe(root, {childList:true,subtree:true,characterData:true});
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', init, {once:true});
        } else { init(); }
      })();
    ", contenedor_js, preloader_js, ids_js)))
  )
}

# Server ----
ValidacionFinanciera <- function(id, filtros, dat, fact, margenes_pyg, nit_ppal, clientes_snap, n_cliente) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Funciones ----
    .resolver <- function(x) {
      if (shiny::is.reactive(x)) {
        return(x())
      }
      
      x
    }
    
    .normalizar_segmento <- function(x) {
      x <- stringr::str_to_upper(stringr::str_squish(as.character(x)))
      
      dplyr::case_when(
        is.na(x) | x %in% c("", "SIN DATO", "NA") ~ NA_character_,
        x %in% c("GRANDE", "GRANDES") ~ "GRANDE",
        x %in% c("MEDIANO", "MEDIANOS") ~ "MEDIANO",
        x %in% c("PEQUEÑO", "PEQUEÑOS") ~ "PEQUEÑO",
        TRUE ~ x
      )
    }
    
    .nombre_linea <- function(x) {
      dplyr::case_when(
        x == 10000 ~ "CONVENCIONALES",
        x %in% c(20000, 21000) ~ "A LA MEDIDA",
        TRUE ~ "SIN DATO"
      )
    }
    
    .aplicar_filtros_mapa <- function(dat_mapa, f) {
      if (length(f$asesor) > 0L) {
        dat_mapa <- dat_mapa %>% dplyr::filter(Asesor %in% f$asesor)
      }
      
      if (length(f$segmento) > 0L) {
        dat_mapa <- dat_mapa %>% dplyr::filter(Segmento %in% f$segmento)
      }
      
      if (length(f$linneg) > 0L) {
        dat_mapa <- dat_mapa %>% dplyr::filter(CLLinNegNo %in% f$linneg)
      }
      
      if (length(f$categoria) > 0L) {
        dat_mapa <- dat_mapa %>% dplyr::filter(Categoria %in% f$categoria)
      }
      
      if (length(f$producto) > 0L) {
        dat_mapa <- dat_mapa %>% dplyr::filter(Producto %in% f$producto)
      }
      
      dat_mapa
    }
    
    .gt_base <- function(datos, rowname_col = NULL) {
      .crm_tabla_vancouver(datos, row_padding = 4) %>%
        gt::tab_options(
          table.width = gt::pct(100),
          table_body.hlines.style = "none",
          table_body.hlines.width = gt::px(0),
          table_body.hlines.color = "transparent"
        ) %>%
        gt::sub_missing(missing_text = "")
    }
    
    .gt_escala_valor <- function(tabla, datos, columnas) {
      columnas <- intersect(columnas, names(datos))
      
      for (columna in columnas) {
        filas_neg <- which(!is.na(datos[[columna]]) & datos[[columna]] < 0)
        filas_pos <- which(!is.na(datos[[columna]]) & datos[[columna]] > 0)
        filas_cero <- which(!is.na(datos[[columna]]) & datos[[columna]] == 0)
        
        if (length(filas_neg) > 0L) {
          tabla <- tabla %>%
            gt::tab_style(
              style = gt::cell_text(color = "#C11007", weight = "bold"),
              locations = gt::cells_body(columns = dplyr::all_of(columna), rows = filas_neg)
            )
        }
        
        if (length(filas_pos) > 0L) {
          tabla <- tabla %>%
            gt::tab_style(
              style = gt::cell_text(color = "#3B6D11", weight = "bold"),
              locations = gt::cells_body(columns = dplyr::all_of(columna), rows = filas_pos)
            )
        }
        
        if (length(filas_cero) > 0L) {
          tabla <- tabla %>%
            gt::tab_style(
              style = gt::cell_text(color = "#212529"),
              locations = gt::cells_body(columns = dplyr::all_of(columna), rows = filas_cero)
            )
        }
      }
      
      tabla
    }
    
    .gt_formatear <- function(
    datos,
    rowname_col = NULL,
    titulo = NULL,
    subtitulo = NULL,
    footer = NULL
    ) {
      tabla <- .gt_base(datos, rowname_col = rowname_col)
      cols_sacos <- intersect(
        c(
          "SacosFacturados", "SacosPYG", "DiferenciaSacos", "Diferencia",
          "Sacos70", "Sacos"
        ),
        names(datos)
      )
      cols_margen <- intersect(
        c("MargenPYG", "Margen", "MNFCC", "MargenTotal"),
        names(datos)
      )
      cols_pct <- intersect(
        c("PctCuadre", "PctDiferencia", "Cuadre", "Participacion"),
        names(datos)
      )
      cols_fecha <- names(datos)[vapply(datos, inherits, logical(1), what = "Date")]
      
      if (!is.null(titulo) || !is.null(subtitulo)) {
        tabla <- tabla %>%
          gt::tab_header(
            title = titulo,
            subtitle = subtitulo
          )
      }
      
      if (length(cols_sacos) > 0L) {
        tabla <- tabla %>%
          gt::fmt_number(
            columns = dplyr::all_of(cols_sacos),
            decimals = 2,
            sep_mark = SEP_MILES,
            dec_mark = SEP_DECIMAL
          )
      }
      
      if (length(cols_margen) > 0L) {
        tabla <- tabla %>%
          gt::fmt_number(
            columns = dplyr::all_of(cols_margen),
            decimals = 0,
            sep_mark = SEP_MILES,
            dec_mark = SEP_DECIMAL,
            pattern = "$ {x}"
          )
      }
      
      if (length(cols_pct) > 0L) {
        tabla <- tabla %>%
          gt::fmt_percent(
            columns = dplyr::all_of(cols_pct),
            decimals = 2,
            dec_mark = SEP_DECIMAL
          )
      }
      
      if (length(cols_fecha) > 0L) {
        tabla <- tabla %>%
          gt::fmt_date(
            columns = dplyr::all_of(cols_fecha),
            date_style = "day_month_year"
          )
      }
      
      if (!is.null(footer)) {
        tabla <- tabla %>%
          gt::tab_source_note(source_note = footer)
      }
      
      .gt_escala_valor(
        tabla,
        datos,
        unique(c(cols_margen, "DiferenciaSacos", "Diferencia", "PctDiferencia"))
      )
    }
    
    .resumen_dimension <- function(datos, dimension) {
      datos %>%
        dplyr::mutate(
          .dimension = dplyr::coalesce(as.character(.data[[dimension]]), "SIN DATO")
        ) %>%
        dplyr::group_by(.dimension) %>%
        dplyr::summarise(
          Registros = dplyr::n(),
          SacosFacturados = sum(dplyr::coalesce(SacosFacturados, 0), na.rm = TRUE),
          SacosPYG = sum(dplyr::coalesce(SacosPYG, 0), na.rm = TRUE),
          DiferenciaSacos = SacosPYG - SacosFacturados,
          .groups = "drop"
        ) %>%
        dplyr::rename(!!dimension := .dimension) %>%
        dplyr::arrange(dplyr::desc(abs(DiferenciaSacos)))
    }
    
    .resumen_alertas <- function(datos, alerta_col = "Alerta") {
      datos %>%
        dplyr::mutate(
          Alerta = dplyr::coalesce(as.character(.data[[alerta_col]]), "SIN DATO")
        ) %>%
        dplyr::group_by(Alerta) %>%
        dplyr::summarise(
          Registros = dplyr::n(),
          SacosFacturados = sum(dplyr::coalesce(SacosFacturados, 0), na.rm = TRUE),
          SacosPYG = sum(dplyr::coalesce(SacosPYG, 0), na.rm = TRUE),
          DiferenciaSacos = SacosPYG - SacosFacturados,
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(Registros))
    }
    
    .resumen_general <- function(datos) {
      tibble::tibble(
        Indicador = c("Registros", "Sacos facturados", "Sacos PyG", "Diferencia"),
        Valor = c(
          nrow(datos),
          sum(dplyr::coalesce(datos$SacosFacturados, 0), na.rm = TRUE),
          sum(dplyr::coalesce(datos$SacosPYG, 0), na.rm = TRUE),
          sum(
            dplyr::coalesce(datos$SacosPYG, 0) -
              dplyr::coalesce(datos$SacosFacturados, 0),
            na.rm = TRUE
          )
        )
      )
    }
    
    .modal_tablero_ui <- function(prefijo) {
      shiny::uiOutput(ns(paste0("ui_", prefijo, "_contenido")))
    }
    
    .sin_registros_ui <- function() {
      shiny::div(
        class = "text-center text-muted",
        "No hay registros para los filtros seleccionados."
      )
    }
    
    .resumen_valores <- function(datos) {
      sac_fact <- sum(dplyr::coalesce(datos$SacosFacturados, 0), na.rm = TRUE)
      sac_pyg <- sum(dplyr::coalesce(datos$SacosPYG, 0), na.rm = TRUE)
      
      list(
        registros = nrow(datos),
        sacos_facturados = sac_fact,
        sacos_pyg = sac_pyg,
        diferencia = sac_pyg - sac_fact
      )
    }
    
    .detalle_excel <- function(datos, prefijo) {
      shiny::downloadHandler(
        filename = function() {
          paste0(prefijo, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
        },
        content = function(file) {
          detalle <- datos()
          resumen <- .resumen_general(detalle)
          por_cliente <- .resumen_dimension(detalle, "Cliente")
          por_segmento <- .resumen_dimension(detalle, "Segmento")
          por_asesor <- .resumen_dimension(detalle, "Asesor")
          por_linea <- .resumen_dimension(detalle, "Linea")
          
          openxlsx2::write_xlsx(
            list(
              Resumen = resumen,
              Detalle = detalle,
              RazonSocial = por_cliente,
              Segmento = por_segmento,
              Asesor = por_asesor,
              LineaNegocio = por_linea
            ),
            file = file
          )
        }
      )
    }
    
    .segmento_matriz <- function(datos, fila, columna, metrica) {
      niveles <- c("PEQUEÑO", "MEDIANO", "GRANDE", "SIN SEGMENTO")
      
      base <- datos %>%
        dplyr::mutate(
          Fila = as.character(.data[[fila]]),
          Columna = as.character(.data[[columna]]),
          Fila = dplyr::if_else(
            is.na(Fila) | Fila == "" | !Fila %in% niveles[1:3],
            "SIN SEGMENTO",
            Fila
          ),
          Columna = dplyr::if_else(
            is.na(Columna) | Columna == "" | !Columna %in% niveles[1:3],
            "SIN SEGMENTO",
            Columna
          )
        )
      
      resumen <- switch(
        metrica,
        "UNIDADES COMERCIALES" = base %>%
          dplyr::count(Fila, Columna, name = "Valor"),
        "SACOS 70 KG" = base %>%
          dplyr::group_by(Fila, Columna) %>%
          dplyr::summarise(
            Valor = sum(SacosBase, na.rm = TRUE),
            .groups = "drop"
          ),
        "MARGEN" = base %>%
          dplyr::group_by(Fila, Columna) %>%
          dplyr::summarise(
            Valor = sum(MargenBase, na.rm = TRUE),
            .groups = "drop"
          )
      )
      
      resumen %>%
        tidyr::complete(
          Fila = niveles,
          Columna = niveles,
          fill = list(Valor = 0)
        )
    }
    
    .segmento_formato_valor <- function(valor, metrica) {
      switch(
        metrica,
        "UNIDADES COMERCIALES" = format(
          round(valor, 0),
          big.mark = SEP_MILES,
          decimal.mark = SEP_DECIMAL
        ),
        "SACOS 70 KG" = format(
          round(valor, 0),
          big.mark = SEP_MILES,
          decimal.mark = SEP_DECIMAL
        ),
        "MARGEN" = paste0(
          "$ ",
          format(
            round(valor, 0),
            big.mark = SEP_MILES,
            decimal.mark = SEP_DECIMAL
          )
        )
      )
    }
    
    .segmento_matriz_ui <- function(
    datos,
    fila,
    columna,
    fila_label,
    columna_label,
    titulo,
    prefijo,
    metrica
    ) {
      niveles <- c("PEQUEÑO", "MEDIANO", "GRANDE", "SIN SEGMENTO")
      matriz <- .segmento_matriz(datos, fila, columna, metrica)
      
      # Conservar los IDs de selección de cada celda con el componente de tabla corporativo.
      datos_tabla <- data.frame(Segmento = niveles, check.names = FALSE)
      names(datos_tabla)[[1]] <- fila_label
      for (j in seq_along(niveles)) {
        datos_tabla[[niveles[[j]]]] <- vapply(seq_along(niveles), function(i) {
          celda <- matriz %>% dplyr::filter(Fila == niveles[[i]], Columna == niveles[[j]])
          valor <- celda$Valor[[1L]]
          etiqueta <- .segmento_formato_valor(valor, metrica)
          if (valor == 0) return(as.character(shiny::span(etiqueta)))
          as.character(shiny::actionLink(ns(paste0("seg_", prefijo, "_", i, "_", j)), label = etiqueta))
        }, character(1))
      }
      alineacion <- stats::setNames(c("left", rep("right", length(niveles))), names(datos_tabla))
      tabla <- racafeShiny::racafeCompactTable(datos_tabla, backend = "html", html_columns = niveles,
                                               align = alineacion, font_size = 12, row_padding = 4)
      tabla <- htmltools::tagQuery(tabla)$find("thead th")$
        addAttrs(style = "border-bottom:1.5px solid #495057!important;")$allTags()
      shiny::tagList(
        racafeForms::racafeDetailSection(titulo, tabla),
        racafeForms::racafeFormMessage(paste0("Filas: ", fila_label, " | Columnas: ", columna_label,
                                              " | Métrica: ", metrica), type = "info", size = "compact")
      )
    }
    
    # Fuentes ----
    fecha_cierre_pyg <- max(as.Date(.resolver(margenes_pyg)$FecMes), na.rm = TRUE)
    
    data_raw <- .resolver(dat) %>%
      dplyr::filter(as.Date(FecMes) <= fecha_cierre_pyg)
    
    if (!"Categoria" %in% names(data_raw)) {
      data_raw$Categoria <- NA_character_
    }
    
    if (!"Producto" %in% names(data_raw)) {
      data_raw$Producto <- NA_character_
    }
    
    data_base <- data_raw %>%
      dplyr::transmute(
        FecMes = as.Date(FecMes),
        FecFact = as.Date(FecFact),
        CLLotCod = suppressWarnings(as.numeric(CLLotCod)),
        CLCliNit = suppressWarnings(as.numeric(CLCliNit)),
        CliNitPpal = suppressWarnings(as.numeric(CliNitPpal)),
        LinNegCod = suppressWarnings(as.numeric(LinNegCod)),
        Linea = .nombre_linea(LinNegCod),
        Cliente = as.character(PerRazSoc),
        Asesor = as.character(Asesor),
        SegmentoCRMData = .normalizar_segmento(Segmento),
        SegmentoPYGData = .normalizar_segmento(SegmentacionClientePYG),
        Categoria = as.character(Categoria),
        Producto = as.character(Producto),
        SacLote = suppressWarnings(as.numeric(SacLote)),
        SacosData = suppressWarnings(as.numeric(SacosFact)),
        MargenData = suppressWarnings(as.numeric(Margen))
      )
    
    fact_raw <- .resolver(fact)
    
    if (!"LinNegCod" %in% names(fact_raw)) {
      fact_raw$LinNegCod <- NA_real_
    }
    
    fact_base <- fact_raw %>%
      dplyr::transmute(
        CLLotCod = suppressWarnings(as.numeric(CLLotCod)),
        LinNegCodFactura = suppressWarnings(as.numeric(LinNegCod)),
        FecMesFact = dplyr::coalesce(
          as.Date(FecMesFact),
          lubridate::floor_date(as.Date(FecFact), "month")
        ),
        FecFact = as.Date(FecFact),
        SacosFacturados = suppressWarnings(as.numeric(SacosFact))
      ) %>%
      dplyr::filter(
        !is.na(CLLotCod),
        !is.na(FecFact)
      )
    
    pyg_base <- .resolver(margenes_pyg) %>%
      dplyr::transmute(
        FecMes = as.Date(FecMes),
        ANHO = suppressWarnings(as.integer(ANHO)),
        FecFactPYG = as.Date(FECFACTURA),
        CLLotCod = suppressWarnings(as.numeric(LOTE)),
        CLCliNit = suppressWarnings(as.numeric(CLIENTE)),
        LinNegCod = suppressWarnings(as.numeric(LINNEG)),
        SegmentoPYG = .normalizar_segmento(SEGMENTACION_CLIENTE),
        SacosPYG = suppressWarnings(as.numeric(SACOS70)),
        MargenPYG = suppressWarnings(as.numeric(MARGEN))
      ) %>%
      dplyr::mutate(
        LinNegCod = dplyr::if_else(LinNegCod == 20000, 21000, LinNegCod),
        LineaPYG = .nombre_linea(LinNegCod)
      )
    
    nit_base <- .resolver(nit_ppal) %>%
      dplyr::transmute(
        CLCliNit = suppressWarnings(as.numeric(PerCod)),
        CliNitPpal = suppressWarnings(as.numeric(CliNitPpal))
      ) %>%
      dplyr::distinct(CLCliNit, .keep_all = TRUE)
    
    clientes_snap_base <- .resolver(clientes_snap) %>%
      dplyr::transmute(
        LinNegCod = suppressWarnings(as.numeric(LinNegCod)),
        CliNitPpal = suppressWarnings(as.numeric(CliNitPpal)),
        SegmentoCRM = .normalizar_segmento(Segmento),
        AsesorCRM = as.character(Asesor)
      ) %>%
      dplyr::distinct(LinNegCod, CliNitPpal, .keep_all = TRUE)
    
    n_cliente_base <- .resolver(n_cliente) %>%
      dplyr::transmute(
        CLCliNit = suppressWarnings(as.numeric(PerCod)),
        CliNitPpal = suppressWarnings(as.numeric(CliNitPpal)),
        RazonSocialCreacion = as.character(PerRazSoc),
        SegmentoCreacionCliente = .normalizar_segmento(SegmentoNCLIENTE)
      ) %>%
      dplyr::mutate(
        CliNitPpal = dplyr::coalesce(CliNitPpal, CLCliNit),
        EsPrincipal = CLCliNit == CliNitPpal
      )
    
    n_cliente_principal <- n_cliente_base %>%
      dplyr::arrange(CliNitPpal, dplyr::desc(EsPrincipal)) %>%
      dplyr::group_by(CliNitPpal) %>%
      dplyr::summarise(
        RazonSocialCreacion = dplyr::first(RazonSocialCreacion),
        SegmentoCreacionCliente = dplyr::first(SegmentoCreacionCliente),
        .groups = "drop"
      )
    
    # Extracción PyG financiero ----
    
    extraccion_pyg <- pyg_base %>%
      dplyr::filter(
        ANHO == .VF_ANHO,
        FecMes <= fecha_cierre_pyg,
        LinNegCod %in% c(10000, 21000)
      ) %>%
      dplyr::mutate(
        MesNum = lubridate::month(FecMes),
        LineaClave = dplyr::if_else(
          LinNegCod == 10000,
          "CONV",
          "MEDIDA"
        )
      ) %>%
      dplyr::group_by(MesNum, LineaClave) %>%
      dplyr::summarise(
        Sacos70 = sum(SacosPYG, na.rm = TRUE),
        MNFCC = sum(MargenPYG, na.rm = TRUE),
        .groups = "drop"
      )
    
    meses_cerrados <- 1:12
    
    extraccion_pyg <- tidyr::expand_grid(
      MesNum = meses_cerrados,
      LineaClave = c("CONV", "MEDIDA")
    ) %>%
      dplyr::left_join(
        extraccion_pyg,
        by = c("MesNum", "LineaClave")
      ) %>%
      dplyr::mutate(
        Sacos70 = dplyr::coalesce(Sacos70, 0),
        MNFCC = dplyr::coalesce(MNFCC, 0)
      ) %>%
      tidyr::pivot_wider(
        names_from = LineaClave,
        values_from = c(Sacos70, MNFCC),
        names_glue = "{.value}_{LineaClave}"
      ) %>%
      dplyr::transmute(
        MesNum,
        Mes = .VF_MESES[MesNum],
        SacosConv = Sacos70_CONV,
        MNFCCConv = MNFCC_CONV,
        SacosMedida = Sacos70_MEDIDA,
        MNFCCMedida = MNFCC_MEDIDA,
        SacosTotal = SacosConv + SacosMedida,
        MNFCCTotal = MNFCCConv + MNFCCMedida
      ) %>%
      dplyr::bind_rows(
        dplyr::summarise(
          .,
          MesNum = 13L,
          Mes = "TOTAL",
          SacosConv = sum(SacosConv, na.rm = TRUE),
          MNFCCConv = sum(MNFCCConv, na.rm = TRUE),
          SacosMedida = sum(SacosMedida, na.rm = TRUE),
          MNFCCMedida = sum(MNFCCMedida, na.rm = TRUE),
          SacosTotal = sum(SacosTotal, na.rm = TRUE),
          MNFCCTotal = sum(MNFCCTotal, na.rm = TRUE)
        )
      ) %>%
      dplyr::arrange(MesNum) %>%
      dplyr::select(-MesNum)
    
    auditoria_integridad <- shiny::reactive({
      base_pyg <- pyg_base %>%
        dplyr::filter(
          ANHO == .VF_ANHO,
          FecMes <= fecha_cierre_pyg,
          LinNegCod %in% c(10000, 21000)
        ) %>%
        dplyr::group_by(FecMes, LinNegCod) %>%
        dplyr::summarise(
          RegistrosPYG = dplyr::n(),
          SacosPYG = sum(SacosPYG, na.rm = TRUE),
          MargenPYG = sum(MargenPYG, na.rm = TRUE),
          .groups = "drop"
        )
      
      base_central <- data_base %>%
        dplyr::filter(
          lubridate::year(FecMes) == .VF_ANHO,
          FecMes <= fecha_cierre_pyg,
          LinNegCod %in% c(10000, 21000)
        ) %>%
        dplyr::group_by(FecMes, LinNegCod) %>%
        dplyr::summarise(
          RegistrosData = dplyr::n(),
          SacosData = sum(SacosData, na.rm = TRUE),
          MargenData = sum(MargenData, na.rm = TRUE),
          .groups = "drop"
        )
      
      dplyr::full_join(
        base_pyg,
        base_central,
        by = c("FecMes", "LinNegCod")
      ) %>%
        dplyr::mutate(
          DiferenciaSacos = dplyr::coalesce(SacosData, 0) - dplyr::coalesce(SacosPYG, 0),
          DiferenciaMargen = dplyr::coalesce(MargenData, 0) - dplyr::coalesce(MargenPYG, 0),
          DiferenciaRegistros = dplyr::coalesce(RegistrosData, 0L) -
            dplyr::coalesce(RegistrosPYG, 0L)
        ) %>%
        dplyr::arrange(FecMes, LinNegCod)
    })
    
    output$tabla_extraccion_pyg <- gt::render_gt({
      tabla <- extraccion_pyg %>%
        .crm_tabla_vancouver(row_padding = 4) %>%
        gt::tab_header(
          title = "Extracción PyG financiero",
          subtitle = paste0("Año ", .VF_ANHO)
        ) %>%
        gt::tab_spanner(
          label = "CONVENCIONALES",
          columns = c(SacosConv, MNFCCConv)
        ) %>%
        gt::tab_spanner(
          label = "A LA MEDIDA",
          columns = c(SacosMedida, MNFCCMedida)
        ) %>%
        gt::tab_spanner(
          label = "TOTAL",
          columns = c(SacosTotal, MNFCCTotal)
        ) %>%
        gt::cols_label(
          SacosConv = "Sacos 70 kg",
          MNFCCConv = "$MNFCC",
          SacosMedida = "Sacos 70 kg",
          MNFCCMedida = "$MNFCC",
          SacosTotal = "Sacos 70 kg",
          MNFCCTotal = "$MNFCC"
        ) %>%
        gt::fmt_number(
          columns = c(SacosConv, SacosMedida, SacosTotal),
          decimals = 0,
          sep_mark = SEP_MILES,
          dec_mark = SEP_DECIMAL
        ) %>%
        gt::fmt_number(
          columns = c(MNFCCConv, MNFCCMedida, MNFCCTotal),
          decimals = 0,
          sep_mark = SEP_MILES,
          dec_mark = SEP_DECIMAL,
          pattern = "$ {x}"
        ) %>%
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(rows = Mes == "TOTAL")
        ) %>%
        gt::tab_source_note(
          source_note = paste0(
            "Extracción independiente del PyG; meses sin movimientos se muestran en cero."
          )
        ) %>%
        gt::tab_options(
          table.width = gt::pct(100),
          table_body.hlines.style = "none",
          table_body.hlines.width = gt::px(0),
          table_body.hlines.color = "transparent"
        ) %>%
        gt::sub_missing(missing_text = "")
      
      .gt_escala_valor(
        tabla,
        extraccion_pyg,
        c("MNFCCConv", "MNFCCMedida", "MNFCCTotal")
      )
    })
    
    output$auditoria_base <- shiny::renderUI({
      diferencias <- auditoria_integridad() %>%
        dplyr::filter(
          abs(DiferenciaSacos) > .VF_TOLERANCIA_SACOS |
            abs(DiferenciaMargen) > 1 |
            DiferenciaRegistros != 0L
        )
      
      if (nrow(diferencias) == 0L) {
        return(NULL)
      }
      
      shiny::tagList(
        shiny::tags$hr(),
        shiny::tags$p(
          "Diferencias entre data y PyG: revisar duplicaciones y registros ausentes."
        ),
        gt::gt_output(ns("gt_auditoria_integridad"))
      )
    })
    
    output$gt_auditoria_integridad <- gt::render_gt({
      auditoria_integridad() %>%
        dplyr::filter(
          abs(DiferenciaSacos) > .VF_TOLERANCIA_SACOS |
            abs(DiferenciaMargen) > 1 |
            DiferenciaRegistros != 0L
        ) %>%
        dplyr::select(
          FecMes,
          LinNegCod,
          RegistrosPYG,
          RegistrosData,
          DiferenciaRegistros,
          DiferenciaSacos,
          DiferenciaMargen
        ) %>%
        .gt_formatear(
          titulo = "Auditoría de integridad",
          footer = "Diferencias de data menos PyG, sin compensar ni eliminar registros."
        )
    })
    
    # Reactivos ----
    rango_fechas <- shiny::reactive({
      c(
        as.Date(paste0(.VF_ANHO, "-01-01")),
        min(
          as.Date(paste0(.VF_ANHO, "-12-31")),
          lubridate::ceiling_date(fecha_cierre_pyg, "month") - lubridate::days(1L)
        )
      )
    })
    
    mapa_lotes <- shiny::reactive({
      f <- filtros()
      
      datos <- data_base %>%
        dplyr::filter(!is.na(CLLotCod), FecMes <= fecha_cierre_pyg) %>%
        dplyr::arrange(dplyr::desc(FecMes)) %>%
        dplyr::distinct(CLLotCod, .keep_all = TRUE) %>%
        dplyr::transmute(
          CLLotCod,
          SacLote,
          CliNitPpal,
          CLCliNit,
          PerRazSoc = Cliente,
          Asesor,
          Segmento = SegmentoCRMData,
          CLLinNegNo = Linea,
          Categoria,
          Producto,
          LinNegCod
        )
      
      .aplicar_filtros_mapa(datos, f)
    })
    
    ultimo_mes_cerrado <- shiny::reactive({
      fecha_cierre_pyg
    })
    
    data_periodo <- shiny::reactive({
      fechas <- rango_fechas()
      mes_ini <- lubridate::floor_date(fechas[[1L]], "month")
      mes_fin <- lubridate::floor_date(fechas[[2L]], "month")
      
      data_base %>%
        dplyr::filter(
          FecMes >= mes_ini,
          FecMes <= mes_fin,
          FecMes <= ultimo_mes_cerrado()
        )
    })
    
    pyg_periodo <- shiny::reactive({
      fechas <- rango_fechas()
      
      pyg_base %>%
        dplyr::filter(
          FecMes >= lubridate::floor_date(fechas[[1L]], "month"),
          FecMes <= lubridate::floor_date(fechas[[2L]], "month"),
          FecMes <= ultimo_mes_cerrado()
        )
    })
    
    pyg_filtrado <- shiny::reactive({
      pyg_periodo() %>%
        dplyr::filter(
          !is.na(CLLotCod),
          CLLotCod != 0
        )
    })
    
    fact_filtrado <- shiny::reactive({
      fechas <- rango_fechas()
      fin_cierre <- lubridate::ceiling_date(
        ultimo_mes_cerrado(),
        "month"
      ) - lubridate::days(1L)
      
      lotes_pyg_nacional <- pyg_base %>%
        dplyr::filter(
          ANHO == .VF_ANHO,
          FecMes <= ultimo_mes_cerrado(),
          LinNegCod %in% c(10000, 21000),
          !is.na(CLLotCod)
        ) %>%
        dplyr::pull(CLLotCod) %>%
        unique()
      
      fact_base %>%
        dplyr::filter(
          FecFact >= fechas[[1L]],
          FecFact <= fechas[[2L]],
          FecFact <= fin_cierre,
          LinNegCodFactura %in% c(10000, 21000) |
            CLLotCod %in% lotes_pyg_nacional
        )
    })
    
    lotes_data <- shiny::reactive({
      pyg_periodo() %>%
        dplyr::filter(!is.na(CLLotCod), CLLotCod != 0) %>%
        dplyr::group_by(CLLotCod) %>%
        dplyr::summarise(
          FecMesData = max(FecMes, na.rm = TRUE),
          CLCliNitData = dplyr::first(CLCliNit),
          LineaData = dplyr::first(LineaPYG),
          LinNegCodData = dplyr::first(LinNegCod),
          SegmentoData = dplyr::first(SegmentoPYG),
          SacosData = sum(SacosPYG, na.rm = TRUE),
          MargenData = sum(MargenPYG, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    lotes_fact <- shiny::reactive({
      fact_filtrado() %>%
        dplyr::group_by(CLLotCod) %>%
        dplyr::summarise(
          FecFact = max(FecFact, na.rm = TRUE),
          SacosFacturados = sum(SacosFacturados, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    conciliacion_lotes <- shiny::reactive({
      dplyr::full_join(
        lotes_fact(),
        lotes_data(),
        by = "CLLotCod"
      ) %>%
        dplyr::left_join(
          mapa_lotes() %>%
            dplyr::select(
              CLLotCod,
              CliNitPpal,
              CLCliNit,
              Cliente = PerRazSoc,
              Asesor,
              Segmento,
              LineaSistema = CLLinNegNo,
              LinNegCodSistema = LinNegCod,
              Categoria,
              Producto
            ),
          by = "CLLotCod"
        ) %>%
        dplyr::mutate(
          CLCliNit = dplyr::coalesce(CLCliNit, CLCliNitData),
          Segmento = dplyr::coalesce(Segmento, SegmentoData),
          LineaBase = LineaData,
          LineaPYG = LineaData,
          SacosPYG = SacosData,
          MargenPYG = MargenData,
          DiferenciaSacos = dplyr::coalesce(SacosData, 0) -
            dplyr::coalesce(SacosFacturados, 0),
          PctDiferencia = dplyr::if_else(
            dplyr::coalesce(SacosFacturados, 0) == 0,
            NA_real_,
            DiferenciaSacos / SacosFacturados
          ),
          Alerta = dplyr::case_when(
            is.na(SacosData) & !is.na(SacosFacturados) ~ "FACTURADO SIN BASE",
            !is.na(SacosData) & is.na(SacosFacturados) ~ "BASE SIN FACTURA",
            abs(DiferenciaSacos) > .VF_TOLERANCIA_SACOS ~ "SACOS DIFERENTES",
            TRUE ~ "OK"
          ),
          Mes = dplyr::coalesce(
            lubridate::floor_date(FecFact, "month"),
            FecMesData
          )
        )
    })
    
    # Conciliación por mes ----
    conciliacion_mensual <- shiny::reactive({
      pyg_mes <- pyg_periodo() %>%
        dplyr::filter(!is.na(CLLotCod), CLLotCod != 0) %>%
        dplyr::group_by(FecMes, CLLotCod) %>%
        dplyr::summarise(
          CLCliNitPYG = dplyr::first(CLCliNit),
          LineaPYG = dplyr::first(LineaPYG),
          SacosPYG = sum(SacosPYG, na.rm = TRUE),
          TienePYG = TRUE,
          .groups = "drop"
        )
      
      fact_mes <- fact_filtrado() %>%
        dplyr::group_by(FecMes = FecMesFact, CLLotCod) %>%
        dplyr::summarise(
          SacosFacturados = sum(SacosFacturados, na.rm = TRUE),
          LinNegCodFactura = dplyr::first(LinNegCodFactura),
          TieneFactura = TRUE,
          .groups = "drop"
        )
      
      meses_pyg <- pyg_mes %>%
        dplyr::group_by(CLLotCod) %>%
        dplyr::summarise(
          PeriodosPYG = paste(unique(format(FecMes, "%Y-%m")), collapse = ", "),
          .groups = "drop"
        )
      
      meses_fact <- fact_mes %>%
        dplyr::group_by(CLLotCod) %>%
        dplyr::summarise(
          PeriodosFactura = paste(unique(format(FecMes, "%Y-%m")), collapse = ", "),
          .groups = "drop"
        )
      
      dplyr::full_join(
        fact_mes,
        pyg_mes,
        by = c("FecMes", "CLLotCod")
      ) %>%
        dplyr::left_join(meses_pyg, by = "CLLotCod") %>%
        dplyr::left_join(meses_fact, by = "CLLotCod") %>%
        dplyr::left_join(
          mapa_lotes() %>%
            dplyr::select(CLLotCod, Cliente = PerRazSoc, Segmento, Asesor,
                          LineaSistema = CLLinNegNo),
          by = "CLLotCod"
        ) %>%
        dplyr::mutate(
          Linea = dplyr::coalesce(
            LineaPYG,
            dplyr::na_if(.nombre_linea(LinNegCodFactura), "SIN DATO"),
            LineaSistema
          ),
          TienePYG = dplyr::coalesce(TienePYG, FALSE),
          TieneFactura = dplyr::coalesce(TieneFactura, FALSE),
          SacosFacturados = dplyr::coalesce(SacosFacturados, 0),
          SacosPYG = dplyr::coalesce(SacosPYG, 0),
          DiferenciaSacos = SacosPYG - SacosFacturados,
          PctDiferencia = dplyr::if_else(
            SacosFacturados == 0,
            NA_real_,
            DiferenciaSacos / SacosFacturados
          ),
          Alerta = dplyr::case_when(
            !TienePYG & TieneFactura & !is.na(PeriodosPYG) ~ "DESFASE DE MES",
            !TienePYG & TieneFactura ~ "FACTURADO SIN PYG",
            !TieneFactura & TienePYG & !is.na(PeriodosFactura) ~ "DESFASE DE MES",
            !TieneFactura & TienePYG ~ "PYG SIN FACTURA",
            abs(DiferenciaSacos) > .VF_TOLERANCIA_SACOS ~ "SACOS DIFERENTES",
            TRUE ~ "OK"
          )
        )
    })
    
    diferencias_sacos <- shiny::reactive({
      conciliacion_lotes() %>%
        dplyr::filter(Alerta == "SACOS DIFERENTES")
    })
    
    fact_sin_pyg <- shiny::reactive({
      conciliacion_lotes() %>%
        dplyr::filter(Alerta == "FACTURADO SIN BASE")
    })
    
    pyg_sin_fact <- shiny::reactive({
      conciliacion_lotes() %>%
        dplyr::filter(Alerta == "BASE SIN FACTURA")
    })
    
    pyg_sin_lote <- shiny::reactive({
      pyg_periodo() %>%
        dplyr::filter(
          is.na(CLLotCod) |
            CLLotCod == 0
        ) %>%
        dplyr::left_join(nit_base, by = "CLCliNit") %>%
        dplyr::mutate(
          CliNitPpal = dplyr::coalesce(CliNitPpal, CLCliNit)
        ) %>%
        dplyr::left_join(
          clientes_snap_base,
          by = c("CliNitPpal", "LinNegCod")
        ) %>%
        dplyr::left_join(n_cliente_base, by = "CLCliNit") %>%
        dplyr::transmute(
          FecMes,
          CLLotCod,
          CLCliNit,
          Cliente = RazonSocialCreacion,
          Linea = LineaPYG,
          Segmento = SegmentoCRM,
          Asesor = AsesorCRM,
          SacosFacturados = 0,
          SacosPYG,
          MargenPYG,
          DiferenciaSacos = SacosPYG,
          PctDiferencia = NA_real_,
          Alerta = "PYG SIN LOTE"
        )
    })
    
    lineas_diferentes <- shiny::reactive({
      conciliacion_lotes() %>%
        dplyr::filter(
          !is.na(LineaBase),
          LineaBase != "SIN DATO",
          !is.na(LineaSistema),
          LineaSistema != "SIN DATO",
          LineaBase != LineaSistema
        ) %>%
        dplyr::mutate(
          Linea = paste0(LineaSistema, " -> ", LineaBase),
          Alerta = "LÍNEA DE NEGOCIO DIFERENTE"
        )
    })
    
    cuadro_mensual <- shiny::reactive({
      central <- pyg_periodo() %>%
        dplyr::filter(
          ANHO == .VF_ANHO,
          LinNegCod %in% c(10000, 21000)
        ) %>%
        dplyr::group_by(
          FecMes,
          LinNegCod
        ) %>%
        dplyr::summarise(
          SacosBase = sum(SacosPYG, na.rm = TRUE),
          .groups = "drop"
        )
      
      fact_mes <- fact_filtrado() %>%
        dplyr::left_join(
          dplyr::bind_rows(
            pyg_base %>% dplyr::select(CLLotCod, LinNegCod),
            data_base %>%
              dplyr::filter(FecMes <= fecha_cierre_pyg) %>%
              dplyr::select(CLLotCod, LinNegCod)
          ) %>%
            dplyr::filter(!is.na(CLLotCod), !is.na(LinNegCod)) %>%
            dplyr::group_by(CLLotCod) %>%
            dplyr::summarise(
              LinNegCod = if (dplyr::n_distinct(LinNegCod) == 1L) {
                dplyr::first(LinNegCod)
              } else {
                NA_real_
              },
              .groups = "drop"
            ),
          by = "CLLotCod"
        ) %>%
        dplyr::mutate(
          FecMes = lubridate::floor_date(FecFact, "month"),
          LinNegCod = dplyr::coalesce(LinNegCodFactura, LinNegCod)
        ) %>%
        dplyr::group_by(
          FecMes,
          LinNegCod
        ) %>%
        dplyr::summarise(
          SacosFacturados = sum(SacosFacturados, na.rm = TRUE),
          .groups = "drop"
        )
      
      base <- dplyr::full_join(
        fact_mes,
        central,
        by = c("FecMes", "LinNegCod")
      ) %>%
        dplyr::mutate(
          SacosFacturados = dplyr::coalesce(SacosFacturados, 0),
          SacosBase = dplyr::coalesce(SacosBase, 0),
          DiferenciaSacos = SacosBase - SacosFacturados,
          PctCuadre = dplyr::if_else(
            SacosFacturados == 0,
            NA_real_,
            pmax(
              0,
              1 - abs(DiferenciaSacos) / SacosFacturados
            )
          )
        )
      
      meses <- tibble::tibble(
        FecMes = seq.Date(
          as.Date(paste0(.VF_ANHO, "-01-01")),
          as.Date(paste0(.VF_ANHO, "-12-01")),
          by = "month"
        )
      )
      
      conv <- base %>%
        dplyr::filter(LinNegCod == 10000) %>%
        dplyr::select(
          FecMes,
          FactConv = SacosFacturados,
          PYGConv = SacosBase,
          DifConv = DiferenciaSacos,
          CuadreConv = PctCuadre
        )
      
      medida <- base %>%
        dplyr::filter(LinNegCod == 21000) %>%
        dplyr::select(
          FecMes,
          FactMedida = SacosFacturados,
          PYGMedida = SacosBase,
          DifMedida = DiferenciaSacos,
          CuadreMedida = PctCuadre
        )
      
      meses %>%
        dplyr::left_join(conv, by = "FecMes") %>%
        dplyr::left_join(medida, by = "FecMes") %>%
        dplyr::mutate(
          Mes = .VF_MESES[lubridate::month(FecMes)],
          dplyr::across(
            c(FactConv, PYGConv, DifConv, FactMedida, PYGMedida, DifMedida,
              CuadreConv, CuadreMedida),
            ~ dplyr::coalesce(.x, 0)
          )
        ) %>%
        dplyr::select(
          FecMes,
          Mes,
          FactConv,
          PYGConv,
          DifConv,
          CuadreConv,
          FactMedida,
          PYGMedida,
          DifMedida,
          CuadreMedida
        )
    })
    
    segmentos_validacion <- shiny::reactive({
      f <- filtros()
      
      .primer_segmento <- function(x) {
        x <- x[!is.na(x) & !x %in% c("", "SIN DATO")]
        if (length(x) == 0L) {
          return(NA_character_)
        }
        x[[1L]]
      }
      
      unidades_pyg <- pyg_base %>%
        dplyr::filter(
          ANHO == .VF_ANHO,
          FecMes <= fecha_cierre_pyg,
          !is.na(CLCliNit),
          !is.na(LinNegCod)
        ) %>%
        dplyr::left_join(nit_base, by = "CLCliNit") %>%
        dplyr::mutate(CliNitPpal = dplyr::coalesce(CliNitPpal, CLCliNit)) %>%
        dplyr::arrange(CliNitPpal, LinNegCod, dplyr::desc(FecMes)) %>%
        dplyr::group_by(CliNitPpal, LinNegCod) %>%
        dplyr::summarise(
          SegmentoPYG = .primer_segmento(SegmentoPYG),
          SacosBase = sum(SacosPYG, na.rm = TRUE),
          MargenBase = sum(MargenPYG, na.rm = TRUE),
          .groups = "drop"
        )
      
      atributos <- data_base %>%
        dplyr::filter(
          lubridate::year(FecMes) == .VF_ANHO,
          FecMes <= fecha_cierre_pyg,
          !is.na(CliNitPpal),
          !is.na(LinNegCod)
        ) %>%
        dplyr::group_by(CliNitPpal, LinNegCod) %>%
        dplyr::summarise(
          Cliente = .primer_segmento(Cliente),
          AsesorOperativo = .primer_segmento(Asesor),
          .groups = "drop"
        )
      
      universo <- unidades_pyg %>%
        dplyr::left_join(atributos, by = c("CliNitPpal", "LinNegCod")) %>%
        dplyr::left_join(
          clientes_snap_base,
          by = c("CliNitPpal", "LinNegCod")
        ) %>%
        dplyr::left_join(n_cliente_principal, by = "CliNitPpal") %>%
        dplyr::mutate(
          Cliente = dplyr::coalesce(Cliente, RazonSocialCreacion),
          Asesor = dplyr::coalesce(AsesorCRM, AsesorOperativo),
          Linea = .nombre_linea(LinNegCod),
          Segmento = SegmentoCRM,
          SegPYGComp = dplyr::coalesce(SegmentoPYG, "SIN SEGMENTO"),
          SegCRMComp = dplyr::coalesce(SegmentoCRM, "SIN SEGMENTO"),
          SegCreacionComp = dplyr::coalesce(
            SegmentoCreacionCliente, "SIN SEGMENTO"
          ),
          Alerta = dplyr::if_else(
            SegPYGComp == SegCRMComp & SegPYGComp == SegCreacionComp,
            "OK",
            "DIFERENCIA"
          )
        )
      
      if (length(f$linneg) > 0L) {
        universo <- universo %>% dplyr::filter(Linea %in% f$linneg)
      }
      if (length(f$asesor) > 0L) {
        universo <- universo %>% dplyr::filter(Asesor %in% f$asesor)
      }
      if (length(f$segmento) > 0L) {
        universo <- universo %>% dplyr::filter(SegmentoCRM %in% f$segmento)
      }
      
      universo
    })
    
    segmentos_error <- shiny::reactive({
      segmentos_validacion() %>%
        dplyr::filter(Alerta != "OK")
    })
    
    calidad_pyg <- shiny::reactive({
      margen_cero <- pyg_filtrado() %>%
        dplyr::filter(
          SacosPYG > 0,
          !is.na(MargenPYG),
          MargenPYG == 0
        ) %>%
        dplyr::transmute(
          CLLotCod,
          CLCliNit,
          Linea = LineaPYG,
          SacosPYG,
          MargenPYG,
          Alerta = "SACOS CON MARGEN CERO"
        )
      
      margen_sin_sacos <- pyg_filtrado() %>%
        dplyr::filter(
          !is.na(MargenPYG),
          MargenPYG != 0,
          is.na(SacosPYG) | SacosPYG == 0
        ) %>%
        dplyr::transmute(
          CLLotCod,
          CLCliNit,
          Linea = LineaPYG,
          SacosPYG,
          MargenPYG,
          Alerta = "MARGEN SIN SACOS"
        )
      
      inconsistentes <- pyg_filtrado() %>%
        dplyr::group_by(CLLotCod) %>%
        dplyr::summarise(
          Clientes = dplyr::n_distinct(CLCliNit, na.rm = TRUE),
          Lineas = dplyr::n_distinct(LinNegCod, na.rm = TRUE),
          Segmentos = dplyr::n_distinct(SegmentoPYG, na.rm = TRUE),
          CLCliNit = dplyr::first(CLCliNit),
          Linea = dplyr::first(LineaPYG),
          SacosPYG = sum(SacosPYG, na.rm = TRUE),
          MargenPYG = sum(MargenPYG, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(
          Clientes > 1L |
            Lineas > 1L |
            Segmentos > 1L
        ) %>%
        dplyr::transmute(
          CLLotCod,
          CLCliNit,
          Linea,
          SacosPYG,
          MargenPYG,
          Alerta = "LOTE INCONSISTENTE"
        )
      
      dplyr::bind_rows(
        margen_cero,
        margen_sin_sacos,
        inconsistentes
      ) %>%
        dplyr::left_join(nit_base, by = "CLCliNit") %>%
        dplyr::mutate(
          LinNegCod = dplyr::case_when(
            Linea == "CONVENCIONALES" ~ 10000,
            Linea == "A LA MEDIDA" ~ 21000,
            TRUE ~ NA_real_
          )
        ) %>%
        dplyr::left_join(
          clientes_snap_base,
          by = c("CliNitPpal", "LinNegCod")
        ) %>%
        dplyr::left_join(n_cliente_base, by = "CLCliNit") %>%
        dplyr::transmute(
          CLLotCod,
          Cliente = RazonSocialCreacion,
          Linea,
          Segmento = SegmentoCRM,
          Asesor = AsesorCRM,
          SacosFacturados = 0,
          SacosPYG,
          MargenPYG,
          DiferenciaSacos = SacosPYG,
          PctDiferencia = NA_real_,
          Alerta
        ) %>%
        dplyr::distinct()
    })
    
    cuadre_valor_r <- shiny::reactive({
      datos <- cuadro_mensual()

      fact_total <- sum(
        dplyr::coalesce(datos$FactConv, 0) +
          dplyr::coalesce(datos$FactMedida, 0),
        na.rm = TRUE
      )

      base_total <- sum(
        dplyr::coalesce(datos$PYGConv, 0) +
          dplyr::coalesce(datos$PYGMedida, 0),
        na.rm = TRUE
      )

      if (!is.finite(fact_total) || fact_total <= 0) {
        return(0)
      }

      cuadre <- 1 - abs(base_total - fact_total) / fact_total

      if (!is.finite(cuadre)) {
        return(0)
      }

      pmax(0, pmin(1, cuadre))
    })

    # Cuadre mensual ----
    # Cajas compactas ----
    .crm_caja(
      id = "kpi_cuadre",
      valor = cuadre_valor_r,
      formato = "porcentaje",
      texto = "Cuadre de sacos",
      icono = "scale-balanced",
      contenido_modal = function() shiny::uiOutput(ns("ui_cuadre_modal")),
      titulo_modal = "Cuadre mensual de sacos",
      icono_modal = "circle",
      footer = "Coincidencia entre facturación y la base financiera central en meses cerrados."
    )
    
    cuadre_seleccion_rv <- shiny::reactiveVal(NULL)
    
    output$ui_cuadre_modal <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            6,
            .crm_tabla_ui(
              ns("tabla_cuadre_conv"),
              estilo = "minimal"
            )
          ),
          shiny::column(
            6,
            .crm_tabla_ui(
              ns("tabla_cuadre_medida"),
              estilo = "minimal"
            )
          )
        ),
        shiny::tags$hr(),
        shiny::uiOutput(ns("ui_cuadre_detalle"))
      )
    })
    
    tabla_cuadre_conv <- shiny::reactive({
      cuadro_mensual() %>%
        dplyr::transmute(
          FecMes,
          Mes,
          SacosFacturados = FactConv,
          SacosPYG = PYGConv,
          Diferencia = DifConv,
          PctCuadre = CuadreConv
        )
    })
    
    tabla_cuadre_medida <- shiny::reactive({
      cuadro_mensual() %>%
        dplyr::transmute(
          FecMes,
          Mes,
          SacosFacturados = FactMedida,
          SacosPYG = PYGMedida,
          Diferencia = DifMedida,
          PctCuadre = CuadreMedida
        )
    })
    
    .col_diferencia_cuadre <- function() {
      reactable::colDef(
        format = reactable::colFormat(
          digits = 2,
          separators = TRUE,
          locales = "es-CO"
        ),
        style = function(value) {
          if (!is.numeric(value) || is.na(value)) {
            return(list())
          }
          
          if (value > 0) {
            return(list(color = "#3B6D11", fontWeight = "500"))
          }
          
          if (value < 0) {
            return(list(color = "#A32D2D", fontWeight = "500"))
          }
          
          list(color = "inherit")
        }
      )
    }
    
    .col_pct_cuadre <- function() {
      reactable::colDef(
        format = reactable::colFormat(
          percent = TRUE,
          digits = 2,
          locales = "es-CO"
        ),
        style = function(value) {
          if (!is.numeric(value) || is.na(value)) {
            return(list())
          }
          
          if (value >= .VF_CUADRE_MINIMO) {
            return(list(color = "#3B6D11", fontWeight = "500"))
          }
          
          list(color = "#A32D2D", fontWeight = "500")
        }
      )
    }
    
    .col_sacos_cuadre <- reactable::colDef(
      format = reactable::colFormat(
        digits = 2,
        separators = TRUE,
        locales = "es-CO"
      )
    )
    
    .registrar_tabla_cuadre <- function(id, datos, titulo) {
      .crm_tabla_servidor(
        id = id,
        data = datos,
        titulo = titulo,
        footer = paste0(
          "Seleccione un mes para consultar lotes con diferencias superiores a ",
          .VF_TOLERANCIA_SACOS,
          " sacos."
        ),
        columnas = c(
          "Mes",
          "SacosFacturados",
          "SacosPYG",
          "Diferencia",
          "PctCuadre"
        ),
        col_labels = c(
          Mes = "Mes",
          SacosFacturados = "Sacos facturados",
          SacosPYG = "Sacos PyG financiero",
          Diferencia = "Diferencia sacos",
          PctCuadre = "% cuadre"
        ),
        col_specs = list(
          Mes = reactable::colDef(minWidth = 82),
          SacosFacturados = .col_sacos_cuadre,
          SacosPYG = .col_sacos_cuadre,
          Diferencia = .col_diferencia_cuadre(),
          PctCuadre = .col_pct_cuadre()
        ),
        modo_seleccion = "fila",
        id_col = "FecMes",
        sortable = FALSE,
        searchable = FALSE,
        page_size = 12,
        compact = TRUE,
        mostrar_badge = FALSE,
        mostrar_nota = FALSE
      )
    }
    
    cuadre_tabla_conv <- .registrar_tabla_cuadre(
      id = "tabla_cuadre_conv",
      datos = tabla_cuadre_conv,
      titulo = "Convencionales"
    )
    
    cuadre_tabla_medida <- .registrar_tabla_cuadre(
      id = "tabla_cuadre_medida",
      datos = tabla_cuadre_medida,
      titulo = "A la medida"
    )
    
    .actualizar_mes_cuadre <- function(seleccion, linea) {
      if (is.null(seleccion) || is.null(seleccion$id)) {
        return()
      }
      
      mes <- as.Date(seleccion$id)
      if (!is.na(mes) && mes <= fecha_cierre_pyg) {
        cuadre_seleccion_rv(list(mes = mes, linea = linea))
      } else {
        cuadre_seleccion_rv(NULL)
      }
    }
    
    shiny::observeEvent(cuadre_tabla_conv$seleccion(), {
      .actualizar_mes_cuadre(cuadre_tabla_conv$seleccion(), "CONVENCIONALES")
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(cuadre_tabla_medida$seleccion(), {
      .actualizar_mes_cuadre(cuadre_tabla_medida$seleccion(), "A LA MEDIDA")
    }, ignoreInit = TRUE)
    
    cuadre_detalle <- shiny::reactive({
      seleccion <- cuadre_seleccion_rv()
      
      if (is.null(seleccion)) {
        return(NULL)
      }
      
      conciliacion_mensual() %>%
        dplyr::filter(
          FecMes == seleccion$mes,
          Linea == seleccion$linea,
          abs(DiferenciaSacos) > .VF_TOLERANCIA_SACOS
        ) %>%
        dplyr::select(
          CLLotCod,
          Cliente,
          Segmento,
          Asesor,
          Linea,
          SacosFacturados,
          SacosPYG,
          DiferenciaSacos,
          PctDiferencia,
          PeriodosFactura,
          PeriodosPYG,
          Alerta
        ) %>%
        dplyr::arrange(dplyr::desc(abs(DiferenciaSacos)))
    })
    
    output$ui_cuadre_detalle <- shiny::renderUI({
      mes <- cuadre_seleccion_rv()
      
      if (is.null(mes)) {
        return(
          shiny::div(
            class = "text-center text-muted",
            "Seleccione un mes de la tabla para consultar el detalle por lote."
          )
        )
      }
      
      detalle <- cuadre_detalle()
      
      if (is.null(detalle) || nrow(detalle) == 0L) {
        return(.sin_registros_ui())
      }
      
      gt::gt_output(ns("gt_cuadre_detalle"))
    })
    
    output$gt_cuadre_detalle <- gt::render_gt({
      detalle <- cuadre_detalle()
      shiny::req(!is.null(detalle), nrow(detalle) > 0L)
      
      .gt_formatear(
        detalle,
        titulo = "Detalle por lote",
        subtitulo = paste0(
          .VF_MESES[lubridate::month(cuadre_seleccion_rv()$mes)],
          " - diferencias superiores a 0,1 sacos (incluye desfases)"
        ),
        footer = paste0(
          "Mes de factura frente al mes contable PyG. Los desfases de período ",
          "no equivalen a un lote inexistente."
        )
      )
    })
    
    # Conciliación de lotes ----
    detalle_conciliacion <- shiny::reactive({
      conciliacion_lotes() %>%
        dplyr::filter(Alerta != "OK") %>%
        dplyr::mutate(
          Linea = dplyr::coalesce(LineaSistema, LineaPYG)
        ) %>%
        dplyr::select(
          Mes,
          CLLotCod,
          Cliente,
          Segmento,
          Asesor,
          Linea,
          LineaSistema,
          LineaPYG,
          SacosFacturados,
          SacosPYG,
          DiferenciaSacos,
          PctDiferencia,
          Alerta
        )
    })
    
    .crm_caja(
      id = "kpi_lotes",
      valor = shiny::reactive(nrow(detalle_conciliacion())),
      formato = "entero",
      texto = "Lotes con diferencia",
      icono = "not-equal",
      contenido_modal = function() .modal_tablero_ui("conciliacion"),
      titulo_modal = "Conciliación de lotes",
      icono_modal = "circle",
      footer = paste0(
        "Lotes con diferencia de sacos o presentes únicamente en una de las ",
        "dos fuentes: facturación y PyG financiero."
      )
    )
    
    # Facturados sin PyG ----
    detalle_fact_sin_pyg <- shiny::reactive({
      fact_sin_pyg() %>%
        dplyr::select(
          Mes,
          CLLotCod,
          Cliente,
          Segmento,
          Asesor,
          Linea = LineaSistema,
          LineaSistema,
          SacosFacturados,
          SacosPYG,
          DiferenciaSacos,
          PctDiferencia,
          Alerta
        )
    })
    
    .crm_caja(
      id = "kpi_fact_sin_pyg",
      valor = shiny::reactive(nrow(fact_sin_pyg())),
      texto = "Facturados sin PyG",
      icono = "file-circle-xmark",
      contenido_modal = function() .modal_tablero_ui("fact_sin_pyg"),
      titulo_modal = "Facturados sin PyG financiero",
      icono_modal = "circle",
      footer = "Lotes facturados que no aparecen en la base financiera central de meses cerrados."
    )
    
    # PyG sin factura ----
    detalle_pyg_sin_fact <- shiny::reactive({
      pyg_sin_fact() %>%
        dplyr::mutate(Linea = LineaPYG) %>%
        dplyr::select(
          Mes,
          CLLotCod,
          Cliente,
          Segmento,
          Asesor,
          Linea,
          LineaPYG,
          SacosFacturados,
          SacosPYG,
          DiferenciaSacos,
          PctDiferencia,
          MargenPYG,
          Alerta
        )
    })
    
    .crm_caja(
      id = "kpi_pyg_sin_fact",
      valor = shiny::reactive(nrow(pyg_sin_fact())),
      texto = "PyG sin factura",
      icono = "receipt",
      contenido_modal = function() .modal_tablero_ui("pyg_sin_fact"),
      titulo_modal = "PyG financiero sin factura",
      icono_modal = "circle",
      footer = "Lotes de la base financiera cerrada sin facturación transaccional asociada."
    )
    
    # PyG sin lote ----
    .crm_caja(
      id = "kpi_pyg_sin_lote",
      valor = shiny::reactive(nrow(pyg_sin_lote())),
      texto = "PyG sin lote",
      icono = "box-open",
      contenido_modal = function() .modal_tablero_ui("pyg_sin_lote"),
      titulo_modal = "PyG financiero sin lote válido",
      icono_modal = "circle",
      footer = "Registros del PyG financiero sin código de lote válido."
    )
    
    # Línea de negocio ----
    .crm_caja(
      id = "kpi_linea",
      valor = shiny::reactive(nrow(lineas_diferentes())),
      texto = "Línea de negocio",
      icono = "shuffle",
      contenido_modal = function() .modal_tablero_ui("linea"),
      titulo_modal = "Diferencias de línea de negocio",
      icono_modal = "circle",
      footer = "Lotes cuya línea de negocio en la base central difiere de la clasificación operativa."
    )
    
    # Segmentos ----
    .crm_caja(
      id = "kpi_segmentos",
      valor = shiny::reactive(
        dplyr::n_distinct(segmentos_error()$CliNitPpal, na.rm = TRUE)
      ),
      formato = "entero",
      texto = "Segmentos diferentes",
      icono = "users-viewfinder",
      contenido_modal = function() shiny::uiOutput(ns("ui_segmentos_modal")),
      titulo_modal = "Validación de segmentos",
      icono_modal = "circle",
      footer = "Compara segmentos únicamente para unidades comerciales presentes en meses cerrados."
    )
    
    # Calidad ----
    .crm_caja(
      id = "kpi_calidad",
      valor = shiny::reactive(nrow(calidad_pyg())),
      texto = "Calidad PyG",
      icono = "triangle-exclamation",
      contenido_modal = function() .modal_tablero_ui("calidad"),
      titulo_modal = "Calidad de información PyG financiero",
      icono_modal = "circle",
      footer = "Alertas internas por margen, sacos o inconsistencias dentro del PyG financiero."
    )
    
    .fecha_resumen <- function(datos) {
      if ("Mes" %in% names(datos) && inherits(datos$Mes, "Date")) {
        return(as.Date(datos$Mes))
      }
      
      if ("FecMes" %in% names(datos)) {
        return(as.Date(datos$FecMes))
      }
      
      rep(as.Date(NA), nrow(datos))
    }
    
    .resumen_periodo <- function(datos) {
      fecha <- .fecha_resumen(datos)
      mes_resumen <- as.Date(ultimo_mes_cerrado())
      
      if (is.na(mes_resumen)) {
        mes_resumen <- as.Date(NA)
      }
      
      en_mes <- !is.na(fecha) & !is.na(mes_resumen) &
        lubridate::floor_date(fecha, "month") == lubridate::floor_date(mes_resumen, "month")
      
      sumar <- function(columna, filtro = rep(TRUE, nrow(datos))) {
        if (!columna %in% names(datos)) {
          return(0)
        }
        
        sum(dplyr::coalesce(datos[[columna]][filtro], 0), na.rm = TRUE)
      }
      
      mes_nombre <- if (is.na(mes_resumen)) {
        "SIN MES"
      } else {
        .VF_MESES[lubridate::month(mes_resumen)]
      }
      
      sacos_fact_acum <- sumar("SacosFacturados")
      sacos_fact_mes <- sumar("SacosFacturados", en_mes)
      sacos_pyg_acum <- sumar("SacosPYG")
      sacos_pyg_mes <- sumar("SacosPYG", en_mes)
      
      list(
        mes = mes_resumen,
        mes_nombre = mes_nombre,
        registros = nrow(datos),
        sacos_fact_acum = sacos_fact_acum,
        sacos_fact_mes = sacos_fact_mes,
        sacos_pyg_acum = sacos_pyg_acum,
        sacos_pyg_mes = sacos_pyg_mes,
        diferencia_acum = sacos_pyg_acum - sacos_fact_acum,
        diferencia_mes = sacos_pyg_mes - sacos_fact_mes,
        margen_acum = sumar("MargenPYG"),
        margen_mes = sumar("MargenPYG", en_mes)
      )
    }
    
    # Tableros modales ----
    .registrar_cajas_tablero <- function(prefijo, datos) {
      valores <- shiny::reactive({
        .resumen_periodo(datos())
      })
      
      .footer_mes <- function(variable) {
        shiny::reactive({
          paste0(
            variable,
            " de ",
            valores()$mes_nombre,
            ", último cierre financiero."
          )
        })
      }
      
      .crm_caja(
        id = paste0(prefijo, "_res_registros"),
        valor = shiny::reactive(valores()$registros),
        formato = "entero",
        texto = "Registros",
        icono = "list",
        mostrar_boton = FALSE,
        footer = "Número de registros que cumplen esta condición en el rango filtrado."
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_fact_acum"),
        valor = shiny::reactive(valores()$sacos_fact_acum),
        texto = "Sacos facturados acumulado",
        icono = "file-invoice",
        mostrar_boton = FALSE,
        footer = "Suma de sacos facturados de todos los registros del detalle filtrado."
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_fact_mes"),
        valor = shiny::reactive(valores()$sacos_fact_mes),
        texto = shiny::reactive(paste0("Sacos facturados ", valores()$mes_nombre)),
        icono = "calendar",
        mostrar_boton = FALSE,
        footer = .footer_mes("Suma de sacos facturados")
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_pyg_acum"),
        valor = shiny::reactive(valores()$sacos_pyg_acum),
        texto = "Sacos PyG acumulado",
        icono = "chart-column",
        mostrar_boton = FALSE,
        footer = "Suma de sacos reportados en PyG de todos los registros del detalle filtrado."
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_pyg_mes"),
        valor = shiny::reactive(valores()$sacos_pyg_mes),
        texto = shiny::reactive(paste0("Sacos PyG ", valores()$mes_nombre)),
        icono = "calendar",
        mostrar_boton = FALSE,
        footer = .footer_mes("Suma de sacos reportados en PyG")
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_dif_acum"),
        valor = shiny::reactive(valores()$diferencia_acum),
        texto = "Diferencia acumulada",
        icono = "not-equal",
        mostrar_boton = FALSE,
        footer = "Sacos PyG acumulados menos sacos facturados acumulados."
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_dif_mes"),
        valor = shiny::reactive(valores()$diferencia_mes),
        texto = shiny::reactive(paste0("Diferencia ", valores()$mes_nombre)),
        icono = "not-equal",
        mostrar_boton = FALSE,
        footer = .footer_mes("Sacos PyG menos sacos facturados")
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_margen_acum"),
        valor = shiny::reactive(valores()$margen_acum),
        formato = "dinero",
        texto = "$MNFCC acumulado",
        icono = "dollar-sign",
        mostrar_boton = FALSE,
        footer = "Suma del margen PyG de todos los registros del detalle filtrado."
      )
      
      .crm_caja(
        id = paste0(prefijo, "_res_margen_mes"),
        valor = shiny::reactive(valores()$margen_mes),
        formato = "dinero",
        texto = shiny::reactive(paste0("$MNFCC ", valores()$mes_nombre)),
        icono = "calendar",
        mostrar_boton = FALSE,
        footer = .footer_mes("Suma del margen PyG")
      )
    }
    
    .registrar_tablero <- function(
    prefijo,
    datos,
    mostrar_cajas = TRUE,
    mostrar_tablas = TRUE
    ) {
      .registrar_cajas_tablero(prefijo, datos)
      
      output[[paste0("ui_", prefijo, "_contenido")]] <- shiny::renderUI({
        detalle <- datos()
        
        if (nrow(detalle) == 0L) {
          return(.sin_registros_ui())
        }
        
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::tagAppendAttributes(racafeForms::racafeButtonDownload(id = paste0("dl_", prefijo),
                label = "Descargar detalle",
                                      icon = "download", ns = ns, size = "xs", variant = "success"),
                                        title = "Descargar"),
              racafeCore::Saltos()
            )
          ),
          if (isTRUE(mostrar_cajas)) {
            shiny::tagList(
              shiny::fluidRow(
                shiny::column(
                  12,
                  .crm_caja_output(ns(paste0(prefijo, "_res_registros")))
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  .crm_caja_output(ns(paste0(prefijo, "_res_fact_acum")))
                ),
                shiny::column(
                  6,
                  .crm_caja_output(ns(paste0(prefijo, "_res_fact_mes")))
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  .crm_caja_output(ns(paste0(prefijo, "_res_pyg_acum")))
                ),
                shiny::column(
                  6,
                  .crm_caja_output(ns(paste0(prefijo, "_res_pyg_mes")))
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  .crm_caja_output(ns(paste0(prefijo, "_res_dif_acum")))
                ),
                shiny::column(
                  6,
                  .crm_caja_output(ns(paste0(prefijo, "_res_dif_mes")))
                )
              ),
              if ("MargenPYG" %in% names(detalle)) {
                shiny::fluidRow(
                  shiny::column(
                    6,
                    .crm_caja_output(ns(paste0(prefijo, "_res_margen_acum")))
                  ),
                  shiny::column(
                    6,
                    .crm_caja_output(ns(paste0(prefijo, "_res_margen_mes")))
                  )
                )
              }
            )
          },
          if (isTRUE(mostrar_tablas)) {
            shiny::tagList(
              shiny::tags$hr(),
              shiny::fluidRow(
                shiny::column(3, gt::gt_output(ns(paste0("gt_", prefijo, "_cliente")))),
                shiny::column(3, gt::gt_output(ns(paste0("gt_", prefijo, "_segmento")))),
                shiny::column(3, gt::gt_output(ns(paste0("gt_", prefijo, "_asesor")))),
                shiny::column(3, gt::gt_output(ns(paste0("gt_", prefijo, "_linea"))))
              )
            )
          }
        )
      })
      
      output[[paste0("gt_", prefijo, "_cliente")]] <- gt::render_gt({
        detalle <- datos()
        
        if (nrow(detalle) == 0L) {
          return(NULL)
        }
        
        .resumen_dimension(detalle, "Cliente") %>%
          dplyr::slice_head(n = 12) %>%
          .gt_formatear(
            titulo = "Razón social",
            footer = "Resumen de registros y sacos por razón social."
          )
      })
      
      output[[paste0("gt_", prefijo, "_segmento")]] <- gt::render_gt({
        detalle <- datos()
        
        if (nrow(detalle) == 0L) {
          return(NULL)
        }
        
        .resumen_dimension(detalle, "Segmento") %>%
          .gt_formatear(
            titulo = "Segmento",
            footer = "Resumen de registros y sacos por segmento comercial."
          )
      })
      
      output[[paste0("gt_", prefijo, "_asesor")]] <- gt::render_gt({
        detalle <- datos()
        
        if (nrow(detalle) == 0L) {
          return(NULL)
        }
        
        .resumen_dimension(detalle, "Asesor") %>%
          .gt_formatear(
            titulo = "Asesor",
            footer = "Resumen de registros y sacos por asesor comercial."
          )
      })
      
      output[[paste0("gt_", prefijo, "_linea")]] <- gt::render_gt({
        detalle <- datos()
        
        if (nrow(detalle) == 0L) {
          return(NULL)
        }
        
        .resumen_dimension(detalle, "Linea") %>%
          .gt_formatear(
            titulo = "Línea de negocio",
            footer = "Resumen de registros y sacos por línea de negocio."
          )
      })
    }
    
    .registrar_tablero(
      "conciliacion",
      detalle_conciliacion,
      mostrar_cajas = TRUE,
      mostrar_tablas = FALSE
    )
    .registrar_tablero("fact_sin_pyg", detalle_fact_sin_pyg)
    .registrar_tablero(
      "pyg_sin_fact",
      detalle_pyg_sin_fact,
      mostrar_cajas = TRUE,
      mostrar_tablas = FALSE
    )
    .registrar_tablero(
      "pyg_sin_lote",
      pyg_sin_lote,
      mostrar_cajas = TRUE,
      mostrar_tablas = FALSE
    )
    .registrar_tablero("linea", lineas_diferentes)
    .registrar_tablero("calidad", calidad_pyg)
    
    # Matrices de segmentos ----
    segmento_seleccion_rv <- shiny::reactiveVal(NULL)
    
    segmentos_matriz <- shiny::reactive({
      segmentos_validacion()
    })
    
    shiny::observeEvent(
      input$VF_SegMetrica,
      {
        segmento_seleccion_rv(NULL)
      },
      ignoreInit = TRUE
    )
    
    .registrar_clicks_matriz <- function(prefijo, fila, columna, titulo) {
      niveles <- c("PEQUEÑO", "MEDIANO", "GRANDE", "SIN SEGMENTO")
      
      for (i in seq_along(niveles)) {
        for (j in seq_along(niveles)) {
          local({
            ii <- i
            jj <- j
            origen <- niveles[[ii]]
            destino <- niveles[[jj]]
            input_id <- paste0("seg_", prefijo, "_", ii, "_", jj)
            
            shiny::observeEvent(input[[input_id]], {
              segmento_seleccion_rv(
                list(
                  fila = fila,
                  columna = columna,
                  origen = origen,
                  destino = destino,
                  titulo = titulo
                )
              )
            }, ignoreInit = TRUE)
          })
        }
      }
    }
    
    .registrar_clicks_matriz(
      "pyg_crm",
      "SegmentoPYG",
      "SegmentoCRM",
      "PyG financiero vs CRM comercial"
    )
    .registrar_clicks_matriz(
      "pyg_ncliente",
      "SegmentoPYG",
      "SegmentoCreacionCliente",
      "PyG financiero vs Creación de cliente"
    )
    .registrar_clicks_matriz(
      "crm_ncliente",
      "SegmentoCRM",
      "SegmentoCreacionCliente",
      "CRM comercial vs Creación de cliente"
    )
    
    .crm_caja(
      id = "seg_res_clientes",
      valor = shiny::reactive(
        dplyr::n_distinct(
          segmentos_validacion()$CliNitPpal,
          na.rm = TRUE
        )
      ),
      formato = "entero",
      texto = "Clientes principales",
      icono = "users",
      mostrar_boton = FALSE,
      footer = paste0(
        "Clientes principales presentes exclusivamente en el PyG ",
        "correspondiente a meses cerrados."
      )
    )
    
    .crm_caja(
      id = "seg_res_clientes_dif",
      valor = shiny::reactive(
        segmentos_validacion() %>%
          dplyr::filter(Alerta != "OK") %>%
          dplyr::pull(CliNitPpal) %>%
          dplyr::n_distinct(na.rm = TRUE)
      ),
      formato = "entero",
      texto = "Clientes con diferencias",
      icono = "users-viewfinder",
      mostrar_boton = FALSE,
      footer = paste0(
        "Clientes principales con al menos una unidad comercial donde ",
        "PyG, CRM y Creación de cliente no tienen el mismo segmento."
      )
    )
    
    output$ui_segmentos_modal <- shiny::renderUI({
      detalle <- segmentos_validacion()
      
      if (nrow(detalle) == 0L) {
        return(.sin_registros_ui())
      }
      
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::tagAppendAttributes(racafeForms::racafeButtonDownload(id = "dl_segmentos",
              label = "Descargar detalle", icon = "download",
                                    ns = ns, size = "xs", variant = "success"), title = "Descargar"),
            racafeCore::Saltos()
          )
        ),
        shiny::fluidRow(
          shiny::column(6, .crm_caja_output(ns("seg_res_clientes"))),
          shiny::column(6, .crm_caja_output(ns("seg_res_clientes_dif")))
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            racafeForms::racafeInputSelect(id = "VF_SegMetrica", label = "Métrica de las matrices",
                                    choices = c(
                "UNIDADES COMERCIALES",
                "SACOS 70 KG",
                "MARGEN"
              ),
                                    selected = "UNIDADES COMERCIALES", multiple = FALSE, ns = ns, size = 3,
                                    empty = FALSE)
          )
        ),
        shiny::tags$hr(),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("ui_matriz_pyg_crm"))),
          shiny::column(4, shiny::uiOutput(ns("ui_matriz_pyg_ncliente"))),
          shiny::column(4, shiny::uiOutput(ns("ui_matriz_crm_ncliente")))
        ),
        shiny::tags$hr(),
        shiny::uiOutput(ns("ui_segmento_detalle"))
      )
    })
    
    output$ui_matriz_pyg_crm <- shiny::renderUI({
      metrica <- if (is.null(input$VF_SegMetrica)) {
        "UNIDADES COMERCIALES"
      } else {
        input$VF_SegMetrica
      }
      
      .segmento_matriz_ui(
        segmentos_matriz(),
        "SegmentoPYG",
        "SegmentoCRM",
        "PyG financiero",
        "CRM comercial",
        "PyG financiero vs CRM comercial",
        "pyg_crm",
        metrica
      )
    })
    
    output$ui_matriz_pyg_ncliente <- shiny::renderUI({
      metrica <- if (is.null(input$VF_SegMetrica)) {
        "UNIDADES COMERCIALES"
      } else {
        input$VF_SegMetrica
      }
      
      .segmento_matriz_ui(
        segmentos_matriz(),
        "SegmentoPYG",
        "SegmentoCreacionCliente",
        "PyG financiero",
        "Creación de cliente",
        "PyG financiero vs Creación de cliente",
        "pyg_ncliente",
        metrica
      )
    })
    
    output$ui_matriz_crm_ncliente <- shiny::renderUI({
      metrica <- if (is.null(input$VF_SegMetrica)) {
        "UNIDADES COMERCIALES"
      } else {
        input$VF_SegMetrica
      }
      
      .segmento_matriz_ui(
        segmentos_matriz(),
        "SegmentoCRM",
        "SegmentoCreacionCliente",
        "CRM comercial",
        "Creación de cliente",
        "CRM comercial vs Creación de cliente",
        "crm_ncliente",
        metrica
      )
    })
    
    segmento_detalle <- shiny::reactive({
      sel <- segmento_seleccion_rv()
      
      if (is.null(sel)) {
        return(NULL)
      }
      
      segmentos_matriz() %>%
        dplyr::mutate(
          .seg_fila = as.character(.data[[sel$fila]]),
          .seg_columna = as.character(.data[[sel$columna]]),
          .seg_fila = dplyr::if_else(
            is.na(.seg_fila) |
              .seg_fila == "" |
              !.seg_fila %in% c("PEQUEÑO", "MEDIANO", "GRANDE"),
            "SIN SEGMENTO",
            .seg_fila
          ),
          .seg_columna = dplyr::if_else(
            is.na(.seg_columna) |
              .seg_columna == "" |
              !.seg_columna %in% c("PEQUEÑO", "MEDIANO", "GRANDE"),
            "SIN SEGMENTO",
            .seg_columna
          )
        ) %>%
        dplyr::filter(
          .seg_fila == sel$origen,
          .seg_columna == sel$destino
        ) %>%
        dplyr::select(
          CliNitPpal,
          LinNegCod,
          Cliente,
          Asesor,
          Linea,
          SegmentoPYG,
          SegmentoCRM,
          SegmentoCreacionCliente,
          SacosBase,
          MargenBase,
          Alerta
        ) %>%
        dplyr::arrange(
          Cliente,
          Linea
        )
    })
    
    output$ui_segmento_detalle <- shiny::renderUI({
      sel <- segmento_seleccion_rv()
      
      if (is.null(sel)) {
        return(
          shiny::div(
            "Seleccione una celda de cualquiera de las matrices para consultar los clientes."
          )
        )
      }
      
      detalle <- segmento_detalle()
      
      shiny::tagList(
        shiny::div(
          paste0(
            sel$titulo,
            ": ",
            sel$origen,
            " -> ",
            sel$destino
          )
        ),
        if (is.null(detalle) || nrow(detalle) == 0L) {
          .sin_registros_ui()
        } else {
          gt::gt_output(ns("gt_segmento_detalle"))
        }
      )
    })
    
    output$gt_segmento_detalle <- gt::render_gt({
      detalle <- segmento_detalle()
      shiny::req(!is.null(detalle), nrow(detalle) > 0L)
      
      .gt_formatear(
        detalle,
        titulo = "Clientes principales de la transición seleccionada",
        subtitulo = "Detalle de segmentación",
        footer = paste0(
          "Comparación entre ",
          segmento_seleccion_rv()$titulo,
          "."
        )
      ) %>%
        gt::cols_label(
          CliNitPpal = "NIT cliente principal",
          LinNegCod = "Código línea",
          SegmentoPYG = "PyG financiero",
          SegmentoCRM = "CRM comercial",
          SegmentoCreacionCliente = "Creación de cliente",
          SacosBase = "Sacos 70 kg",
          MargenBase = "$MNFCC"
        )
    })
    
    # Descargas ----
    output$dl_conciliacion <- .detalle_excel(
      detalle_conciliacion,
      "validacion_conciliacion_lotes"
    )
    output$dl_fact_sin_pyg <- .detalle_excel(
      detalle_fact_sin_pyg,
      "validacion_facturados_sin_pyg"
    )
    output$dl_pyg_sin_fact <- .detalle_excel(
      detalle_pyg_sin_fact,
      "validacion_pyg_sin_factura"
    )
    output$dl_pyg_sin_lote <- .detalle_excel(
      pyg_sin_lote,
      "validacion_pyg_sin_lote"
    )
    output$dl_linea <- .detalle_excel(
      lineas_diferentes,
      "validacion_linea_negocio"
    )
    output$dl_segmentos <- .detalle_excel(
      segmentos_validacion,
      "validacion_segmentos"
    )
    output$dl_calidad <- .detalle_excel(
      calidad_pyg,
      "validacion_calidad_pyg"
    )
    
    # Retorno ----
    invisible(
      list(
        extraccion_pyg = extraccion_pyg,
        auditoria_integridad = auditoria_integridad,
        conciliacion_mensual = conciliacion_mensual,
        cuadro_mensual = cuadro_mensual,
        conciliacion_lotes = conciliacion_lotes,
        pyg_sin_lote = pyg_sin_lote,
        lineas_diferentes = lineas_diferentes,
        segmentos = segmentos_validacion,
        calidad_pyg = calidad_pyg
      )
    )
  })
}
