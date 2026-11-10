# RESUMEN CORPORATIVO DE PRODUCTOS ----
.cot_tabla_resumen_ui <- function(productos, total) {
  datos <- data.frame(
    PRODUCTO = vapply(productos, function(x) if (nzchar(x$producto)) x$producto else "PENDIENTE", character(1)),
    `PRESENTACIÓN / EMPAQUE` = vapply(productos, function(x) paste(x$presentacion, x$empaque, sep = " / "), character(1)),
    KG = vapply(productos, function(x) .cot_formatear_numero_tabla(x$cantidad, 1), character(1)),
    `PRECIO/KG` = vapply(productos, function(x) .cot_formatear_pesos(x$precio), character(1)),
    TOTAL = vapply(productos, function(x) .cot_formatear_pesos(x$total), character(1)), check.names = FALSE
  )
  shiny::tags$div(class = "cot-resumen-tabla",
    racafeShiny::racafeCompactTable(datos, backend = "html",
      align = c(KG = "right", `PRECIO/KG` = "right", TOTAL = "right")),
    racafeForms::racafeMoneyTotal("TOTAL", total, currency = "COP", digits = 0)
  )
}

# Constantes ----
.cot_resolver_app_root <- function() {
  candidatos <- c(getwd(), file.path(getwd(), "APP"))
  
  validos <- vapply(candidatos, function(path) {
    dir.exists(file.path(path, "misc", "modules", "controlbar"))
  }, logical(1))
  
  if (!any(validos)) {
    stop(paste0("No fue posible localizar la raíz de APP. getwd(): ", getwd()))
  }
  
  normalizePath(candidatos[which(validos)[[1]]], winslash = "/", mustWork = TRUE)
}

.COT_APP_ROOT <- .cot_resolver_app_root()
.COT_RMD_PATH <- file.path(.COT_APP_ROOT, "misc", "modules", "controlbar", "cotizacion.Rmd")
# El documento PDF necesita un archivo; obtener el logo del mismo repositorio compartido.
.cot_logo_compartido <- function(base_url = .url_compartido) {
  ruta <- tempfile("crm-logo-", fileext = ".png")
  completo <- FALSE
  on.exit(if (!completo) unlink(ruta), add = TRUE)
  estado <- utils::download.file(paste0(base_url, "/img/logo2.png"), ruta, mode = "wb", quiet = TRUE)
  if (estado != 0L || !file.exists(ruta) || file.info(ruta)$size == 0) {
    stop("No fue posible descargar el logo de Compartido para la cotización.", call. = FALSE)
  }
  completo <- TRUE
  normalizePath(ruta, winslash = "/", mustWork = TRUE)
}

VENDEDORES_DATA <- data.frame(
  Asesor = c("CMEDINA", "JGCANON", "LABOYACA", "GACORREDOR"),
  Nombre = c("Carlos Medina", "Jonathan Cañon", "Luis Boyacá", "Gustavo Corredor"),
  Cargo = c("Coordinador Negocios Industria Nacional", "Jefe Negocios Industria Nacional",
            "Asesor Comercial", "Asesor Comercial"),
  Email = c("cmedina@racafe.com", "jgcanon@racafe.com", "laboyaca@racafe.com", "gacorredor@racafe.com"),
  stringsAsFactors = FALSE
)


# Funciones ----
.cot_valor_seguro <- function(x) {
  if (EsVacio(x)) "" else as.character(x[[1]])
}
.cot_numero_seguro <- function(x) {
  if (EsVacio(x)) 0 else suppressWarnings(as.numeric(x[[1]]))
}
.cot_choices <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- x[!x %in% c("", "SIN DATO", "N/A")]
  unique(x)
}
.cot_choices_ui <- function(x) {
  x <- .cot_choices(x)
  stats::setNames(x, stringr::str_to_upper(x))
}
.cot_catalogo_choices <- function(x) {
  valores <- .cot_choices(x)
  stats::setNames(valores, .admon_choices_rotulos(x)[match(valores, as.character(x))])
}

.cot_formatear_numero_tabla <- function(x, dec = 0) {
  x <- suppressWarnings(as.numeric(x))
  
  if (is.na(x)) {
    return("")
  }
  
  format(round(x, dec), big.mark = SEP_MILES, decimal.mark = SEP_DECIMAL, scientific = FALSE, trim = TRUE, nsmall = dec)
}
.cot_formatear_pesos <- function(x) {
  paste0("$ ", .cot_formatear_numero_tabla(x, dec = 0))
}
.cot_normalizar_productos <- function(productos) {
  columnas_requeridas <- c("LinNeg", "Categoria", "Producto")
  
  columnas_faltantes <- setdiff(columnas_requeridas, names(productos))
  
  if (length(columnas_faltantes) > 0L) {
    stop(paste0("CRMNALPRODS no contiene las columnas requeridas: ", paste(columnas_faltantes, collapse = ", ")))
  }
  
  if (!"Excluir" %in% names(productos)) {
    productos$Excluir <- "NO"
  }
  
  productos %>%
    mutate(LinNeg = as.character(LinNeg), Categoria = as.character(Categoria),
           Producto = as.character(Producto), Excluir = coalesce(as.character(Excluir), "NO"),
           Excluir = ifelse(Excluir %in% c("", "N/A", "SIN DATO"), "NO", Excluir))
}


# Modulos Auxiliares -----
## Producto ----
ProductoUI <- function(id, numero, lineas) {
  ns <- NS(id)
  catalogos <- Choices()
  div(id = ns("wrapper"), class = "cotizador-producto-grid",
      racafeForms::racafeFormGroup(title = paste("Producto", numero), variant = "bordered", divider = FALSE,
                                   racafeForms::racafeForm(size = "compact", variant = "plain",
                                                           .crm_select("LinNeg", label = "Línea de Negocio", required = TRUE,
                                                                                          choices = .cot_choices_ui(lineas), selected = "", multiple = FALSE,
                                                                                          ns = ns, size = 8),
                                                           racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                             .crm_select("Categoria", label = "Categoría", required = TRUE,
                                                                                            choices = c("NINGUNO" = ""), selected = "", multiple = FALSE,
                                                                                            ns = ns, size = 8),
                                                             .crm_select("Producto", label = "Producto", required = TRUE,
                                                                                            choices = c("NINGUNO" = ""), selected = "", multiple = FALSE,
                                                                                            ns = ns, size = 8)
                                                           ),
                                                           racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                             .crm_select("Presentacion", label = "Presentación", required = TRUE,
                                                                                            choices = .cot_catalogo_choices(catalogos$cot_presentacion),
                                                                                            selected = "", multiple = FALSE, ns = ns, size = 5),
                                                             .crm_select("Empaque", label = "Empaque", required = TRUE,
                                                                                            choices = .cot_catalogo_choices(catalogos$cot_empaque),
                                                                                            selected = "", multiple = FALSE, ns = ns, size = 4)
                                                           ),
                                                           racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                             racafeForms::racafeInputNumeric("Cantidad", label = "Cantidad (Kilos)", required = TRUE,
                                                                                             value = 0, format = "number", decimals = 1, min = 0, ns = ns),
                                                             racafeForms::racafeInputNumeric("Precio", label = "Precio por Kilo", required = TRUE,
                                                                                             value = 0, format = "cop", decimals = 0, min = 0, ns = ns)
                                                           ),
                                                           shiny::tags$div(class = "cot-product-total",
                                                                           shiny::tags$span("TOTAL DEL PRODUCTO"),
                                                                           uiOutput(ns("Total"), inline = TRUE))
                                   )
      )
  )
}
Producto <- function(id, catalogos_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactivos ----
    ## Datos ----
    observeEvent(catalogos_r(), {
      for (par in list(c("Presentacion", "cot_presentacion"), c("Empaque", "cot_empaque"))) {
        opciones <- .cot_catalogo_choices(catalogos_r()[[par[[2]]]])
        .crm_update_select(session, par[[1]], choices = opciones,
          selected = intersect(shiny::isolate(input[[par[[1]]]]), unname(opciones)))
      }
    }, ignoreInit = TRUE)
    categorias_r <- reactive({
      linea <- input$LinNeg
      req(!is.null(linea), nzchar(linea))
      
      .cot_choices(.admon_choices_valores(catalogos_r(), "categoria", linea))
    })
    productos_choices_r <- reactive({
      linea <- input$LinNeg
      categoria <- input$Categoria
      req(!is.null(linea), nzchar(linea), !is.null(categoria), nzchar(categoria))
      
      .cot_choices(.admon_choices_valores(catalogos_r(), "producto", linea, categoria))
    })
    total_producto_r <- reactive({
      cantidad <- .cot_numero_seguro(input$Cantidad)
      precio <- .cot_numero_seguro(input$Precio)
      
      if (cantidad > 0 && precio > 0) cantidad * precio else 0
    })
    ## Formulario ----
    observeEvent(input$LinNeg, {
      linea <- input$LinNeg
      
      freezeReactiveValue(input, "Categoria")
      freezeReactiveValue(input, "Producto")
      
      .crm_update_select(
        session, "Producto", choices = c("NINGUNO" = ""), selected = ""
      )
      
      opciones <- if (!is.null(linea) && nzchar(linea)) {
        .cot_choices_ui(categorias_r())
      } else {
        character(0)
      }
      
      .crm_update_select(
        session, "Categoria",
        choices = c("NINGUNO" = "", opciones), selected = ""
      )
    }, ignoreNULL = FALSE)
    
    observeEvent(input$Categoria, {
      categoria <- input$Categoria
      
      freezeReactiveValue(input, "Producto")
      
      opciones <- if (!is.null(categoria) && nzchar(categoria) &&
                      !is.null(input$LinNeg) && nzchar(input$LinNeg)) {
        .cot_choices_ui(productos_choices_r())
      } else {
        character(0)
      }
      
      .crm_update_select(
        session, "Producto",
        choices = c("NINGUNO" = "", opciones), selected = ""
      )
    }, ignoreNULL = FALSE)
    
    # Outputs ----
    output$Total <- renderUI({
      shiny::tags$strong(class = "cot-product-total-value",
                         .cot_formatear_pesos(total_producto_r()))
    })
    
    # Retorno ----
    reactive({
      list(LinNeg = input$LinNeg %||% "",
           Categoria = input$Categoria %||% "",
           Producto = input$Producto %||% "",
           Presentacion = input$Presentacion %||% "",
           Empaque = input$Empaque %||% "",
           Cantidad = .cot_numero_seguro(input$Cantidad),
           Precio = .cot_numero_seguro(input$Precio),
           Total = total_producto_r(),
           Valido = all(nzchar(input$LinNeg %||% ""), nzchar(input$Categoria %||% ""), nzchar(input$Producto %||% ""),
                        .cot_valor_seguro(input$Presentacion) %in% catalogos_r()$cot_presentacion,
                        .cot_valor_seguro(input$Empaque) %in% catalogos_r()$cot_empaque,
                        .cot_numero_seguro(input$Cantidad) > 0, .cot_numero_seguro(input$Precio) > 0
           )
      )
    })
  })
}

# Modulos Principal ---- 
CotizacionUI <- function(id) {
  ns <- NS(id)
  
  racafeForms::racafeForm(size = "compact", variant = "plain",
                          racafeForms::racafeFormGroup(title = "Cliente y Vigencia",
                                                       racafeModulos::ListaDesplegableAgregableUI(ns("COT_Cliente")),
                                                       shiny::uiOutput(ns("COT_ErrorCliente")),
                                                       racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                                         racafeForms::racafeInputDateTime("COT_FechaIni", label = "Efectiva Desde",
                                                                                                          value = as.POSIXct(paste(Sys.Date(), "08:30:00"), tz = "America/Bogota"),
                                                                                                          required = TRUE, ns = ns),
                                                                         racafeForms::racafeInputDateTime("COT_FechaFin", label = "Efectiva Hasta",
                                                                                                          value = as.POSIXct(paste(Sys.Date(), "16:00:00"), tz = "America/Bogota"),
                                                                                                          required = TRUE, ns = ns)
                                                                       )
                          ),
                          racafeForms::racafeFormGroup(title = "Condiciones Comerciales",
                                                       racafeForms::racafeInputSwitch("COT_NombrePropio", label = "Cotización genérica",
                                                                                      value = TRUE, variant = "primary", ns = ns),
                                                       shiny::conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("COT_NombrePropio")),
                                                         .crm_select("COT_Responsable", label = "Responsable",
                                                                                        choices = .cot_choices_ui(Choices()$personas), selected = NULL,
                                                                                        multiple = FALSE, ns = ns, size = 10)
                                                       ),
                                                       racafeForms::racafeFormRow(columns = 2, responsive = "fixed",
                                                         .crm_select("COT_Divisa", label = "Divisa", required = TRUE,
                                                                                        choices = .cot_catalogo_choices(Choices()$cot_divisa),
                                                                                        selected = intersect("Peso Colombiano", Choices()$cot_divisa), multiple = FALSE, empty = FALSE,
                                                                                        search = FALSE, ns = ns, size = 2),
                                                         .crm_select("COT_FPago", label = "Forma de Pago", required = TRUE,
                                                                                        choices = .cot_catalogo_choices(Choices()$formapago),
                                                                                        selected = intersect("PAGO ANTICIPADO", Choices()$formapago),
                                                                                        multiple = FALSE, ns = ns, size = 10)
                                                       )
                          ),
                          racafeForms::racafeFormGroup(title = "Productos en la Cotización",
                                                       div(id = ns("productos_container")),
                                                       racafeForms::racafeFormFooter(
                                                         racafeForms::racafeButton("COT_RemoveProducto", label = NULL, icon = "minus",
                                                                                   variant = "secondary", size = "xxs", ns = ns),
                                                         racafeForms::racafeButton("COT_AddProducto", label = NULL, icon = "plus",
                                                                                   variant = "primary", size = "xxs", ns = ns), align = "right"
                                                       )
                          ),
                          racafeForms::racafeFormFooter(
                            racafeForms::racafeButton("COT_SolicitarPDF", label = "Descargar PDF",
                                                      icon = "file-pdf", variant = "primary", size = "xs", ns = ns),
                            align = "right"
                          )
  )
}
Cotizacion <- function(id, usuario, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    catalogos_iniciados_rv <- reactiveVal(FALSE)
    fecha_ini_r <- reactive(input$COT_FechaIni)
    fecha_fin_r <- reactive(input$COT_FechaFin)
    
    # Datos ----
    contactos_rv <- reactiveVal(tryCatch(
      racafeBD::CargarDatos("CRMNALCONTACTO"),
      error = function(e) data.frame(PerCod = numeric(), PerRazSoc = character(), stringsAsFactors = FALSE)
    ))
    
    catalogos_rv <- reactiveVal({
      catalogos <- Choices()
      if (is.null(attr(catalogos, "relaciones"))) {
        .admon_choices_migrar_relaciones()
        catalogos <- Choices()
      }
      catalogos
    })
    
    catalogos_guardados <- ChoicesReactivos()
    observeEvent(catalogos_guardados(), {
      catalogos <- catalogos_guardados()
      catalogos_rv(catalogos)
      iniciales <- c(COT_Divisa = "Peso Colombiano", COT_FPago = "PAGO ANTICIPADO")
      for (par in list(c("COT_Divisa", "cot_divisa"), c("COT_FPago", "formapago"))) {
        opciones <- .cot_catalogo_choices(catalogos[[par[[2]]]])
        actual <- shiny::isolate(input[[par[[1]]]])
        if (!catalogos_iniciados_rv() && is.null(actual)) actual <- iniciales[[par[[1]]]]
        .crm_update_select(session, par[[1]], choices = opciones, selected = intersect(actual, unname(opciones)))
      }
      catalogos_iniciados_rv(TRUE)
    })

    clientes_r <- reactive({
      req(dat())
      
      clientes_data <- dat() %>%
        transmute(CliNitPpal = suppressWarnings(as.numeric(CliNitPpal)), PerRazSoc = as.character(PerRazSoc),
                  CliDir = as.character(CliDir), CliDir1 = as.character(CliDir1),
                  CliTel = as.character(CliTel)) %>%
        filter(!is.na(CliNitPpal), !is.na(PerRazSoc), !PerRazSoc %in% c("", "SIN DATO")) %>%
        distinct(CliNitPpal, .keep_all = TRUE)
      
      clientes_contacto <- contactos_rv() %>%
        transmute(CliNitPpal = suppressWarnings(as.numeric(PerCod)), PerRazSoc = as.character(PerRazSoc),
                  CliDir = NA_character_, CliDir1 = NA_character_, CliTel = NA_character_) %>%
        filter(!is.na(CliNitPpal), !is.na(PerRazSoc), !PerRazSoc %in% c("", "SIN DATO")) %>%
        distinct(CliNitPpal, .keep_all = TRUE)
      
      bind_rows(clientes_data, clientes_contacto) %>%
        arrange(desc(!is.na(CliDir)), desc(!is.na(CliTel)), PerRazSoc) %>%
        distinct(CliNitPpal, .keep_all = TRUE) %>%
        arrange(PerRazSoc)
    })
    
    choices_clientes_r <- reactive({
      clientes_r() %>%
        pull(PerRazSoc) %>%
        .cot_choices()
    })
    
    clientes_iniciales <- isolate({
      req(clientes_r())
      choices_clientes_r()
    })
    
    cliente_lista <- racafeModulos::ListaDesplegableAgregable(
      id = "COT_Cliente",
      label = Obligatorio("Cliente"),
      choices = clientes_iniciales,
      selected = NULL,
      multiple = FALSE
    )
    
    error_cliente_rv <- reactiveVal(NULL)
    output$COT_ErrorCliente <- renderUI({
      error <- error_cliente_rv()
      if (is.null(error)) return(NULL)
      racafeForms::racafeFormMessage(error, type = "error", size = "compact")
    })
    
    cliente_seleccionado_r <- reactive({
      seleccion <- cliente_lista$seleccion()
      
      if (length(seleccion) == 0L) {
        return("")
      }
      
      as.character(seleccion[[1]])
    })
    
    observeEvent(cliente_seleccionado_r(), {
      if (nzchar(cliente_seleccionado_r())) error_cliente_rv(NULL)
    })
    
    cliente_actual_r <- reactive({
      cliente <- cliente_seleccionado_r()
      
      req(nzchar(cliente))
      
      registrado <- clientes_r() %>%
        filter(PerRazSoc == cliente) %>%
        slice(1)
      
      if (nrow(registrado) == 1L) {
        return(registrado)
      }
      
      tibble::tibble(
        CliNitPpal = NA_real_,
        PerRazSoc = cliente,
        CliDir = "",
        CliDir1 = "",
        CliTel = ""
      )
    })
    
    # Forma de Pago ----
    .cache_fpg <- new.env(parent = emptyenv())
    
    .obtener_fpg_cliente <- function(nit) {
      clave <- as.character(nit)
      
      if (exists(clave, envir = .cache_fpg, inherits = FALSE)) {
        return(get(clave, envir = .cache_fpg, inherits = FALSE))
      }
      
      resultado <- tryCatch({
        consulta <- racafeBD::ConsultaSistema("syscafe", sprintf(paste0(
          "SELECT f.ForPagNom ",
          "FROM NCLIENT5 c ",
          "LEFT JOIN NFORPAG f ",
          "ON c.ForPagCod = f.ForPagCod ",
          "WHERE c.CliNit = '%s'"
        ), nit))
        
        forma_pago <- consulta %>% pull(ForPagNom)
        
        forma_pago <- forma_pago[!is.na(forma_pago)]
        
        if (length(forma_pago) == 0L) "CONTADO CONTRA ENTREGA" else as.character(forma_pago[[1]])
      }, error = function(e) "CONTADO CONTRA ENTREGA")
      
      assign(clave, resultado, envir = .cache_fpg)
      
      resultado
    }
    
    observeEvent(cliente_actual_r(), {
      cliente <- cliente_actual_r()
      
      if (nrow(cliente) != 1L || is.na(cliente$CliNitPpal[[1]])) {
        .crm_update_select(session, "COT_FPago", selected = intersect("PAGO ANTICIPADO", catalogos_rv()$formapago))
        
        return(invisible(NULL))
      }

      
      forma_pago <- .obtener_fpg_cliente(cliente$CliNitPpal[[1]])
      
      if (!EsVacio(forma_pago)) {
        .crm_update_select(session, "COT_FPago", selected = forma_pago)
      }
    })
    
    # Vendedor ----
    .obtener_datos_vendedor <- function() {
      if (isTRUE(input$COT_NombrePropio)) {
        return(list(Nombre = "Área Comercial Industria Nacional", Cargo = "Área Comercial Industria Nacional",
                    Email = "comercial@racafe.com"))
      }
      
      asesor <- input$COT_Responsable %||% usuario()
      
      vendedor <- VENDEDORES_DATA[VENDEDORES_DATA$Asesor == asesor, , drop = FALSE]
      
      if (nrow(vendedor) == 0L) {
        return(NULL)
      }
      
      list(Nombre = vendedor$Nombre[[1]], Cargo = vendedor$Cargo[[1]], Email = vendedor$Email[[1]])
    }
    
    # Productos ----
    productos_modulos_rv <- reactiveVal(list())
    
    contador_productos_rv <- reactiveVal(0L)
    
    .crear_nuevo_producto <- function() {
      numero <- isolate(contador_productos_rv()) + 1L
      
      contador_productos_rv(numero)
      
      nuevo_id <- paste0("Producto_", numero)
      
      catalogo <- isolate(catalogos_rv())
      
      lineas <- .cot_choices(catalogo$linneg)
      
      if (length(lineas) == 0L) {
        racafeForms::racafeNotify(type = "error", duration = 6,
                                title = "CRMNALPRODS no contiene líneas de negocio disponibles",
                                session = shiny::getDefaultReactiveDomain())
        
        return(invisible(NULL))
      }
      
      insertUI(selector = paste0("#", ns("productos_container")), where = "beforeEnd",
               ui = ProductoUI(id = ns(nuevo_id), numero = numero, lineas = lineas))
      
      nuevo_modulo <- Producto(nuevo_id, catalogos_rv)
      
      modulos_actuales <- isolate(productos_modulos_rv())
      
      modulos_actuales[[nuevo_id]] <- nuevo_modulo
      
      productos_modulos_rv(modulos_actuales)
      
      invisible(nuevo_id)
    }
    
    observeEvent(TRUE, {
      .crear_nuevo_producto()
    }, once = TRUE)
    
    observeEvent(input$COT_AddProducto, {
      .crear_nuevo_producto()
    })
    
    observeEvent(input$COT_RemoveProducto, {
      modulos_actuales <- productos_modulos_rv()
      
      if (length(modulos_actuales) <= 1L) {
        racafeForms::racafeNotify(session, "Cotización",
                                  "Debe conservar al menos un producto.", type = "warning", duration = 3)
        
        return(invisible(NULL))
      }
      
      ultimo_id <- tail(names(modulos_actuales), 1)
      
      removeUI(selector = paste0("#", ns(ultimo_id), "-wrapper"), immediate = TRUE)
      
      modulos_actuales[[ultimo_id]] <- NULL
      
      productos_modulos_rv(modulos_actuales)
    })
    
    # Total ----
    total_general_r <- reactive({
      modulos <- productos_modulos_rv()
      
      if (length(modulos) == 0L) {
        return(0)
      }
      
      sum(vapply(modulos, function(modulo) .cot_numero_seguro(modulo()$Total), numeric(1)), na.rm = TRUE)
    })
    
    .cot_formulario_completo <- reactive({
      modulos <- productos_modulos_rv()
      if (!length(modulos) || !nzchar(cliente_seleccionado_r())) return(FALSE)
      if (!all(vapply(modulos, function(modulo) isTRUE(modulo()$Valido), logical(1)))) return(FALSE)
      inicio <- fecha_ini_r()
      fin <- fecha_fin_r()
      if (!inherits(inicio, "POSIXct") || !inherits(fin, "POSIXct") ||
          anyNA(c(inicio, fin)) || length(inicio) != 1L || length(fin) != 1L || fin <= inicio) return(FALSE)
      if (!.cot_valor_seguro(input$COT_Divisa) %in% catalogos_rv()$cot_divisa ||
          !.cot_valor_seguro(input$COT_FPago) %in% catalogos_rv()$formapago) return(FALSE)
      if (isFALSE(input$COT_NombrePropio) && !nzchar(input$COT_Responsable %||% "")) return(FALSE)
      TRUE
    })
    
    .cot_resumen_ui <- reactive({
      if (!isTRUE(.cot_formulario_completo())) return(NULL)
      cliente <- cliente_actual_r()
      modulos <- productos_modulos_rv()
      
      if (nrow(cliente) != 1L) {
        return(racafeShiny::FormatearTexto("Seleccione un cliente para visualizar el resumen de la cotización",
                                           tamano_pct = 0.8, color = "#94A3B8"))
      }
      
      productos <- lapply(seq_along(modulos), function(i) {
        producto <- modulos[[i]]()
        
        list(
          numero = i,
          producto = .cot_valor_seguro(producto$Producto),
          presentacion = .cot_valor_seguro(producto$Presentacion),
          empaque = .cot_valor_seguro(producto$Empaque),
          cantidad = .cot_numero_seguro(producto$Cantidad),
          precio = .cot_numero_seguro(producto$Precio),
          total = .cot_numero_seguro(producto$Total)
        )
      })
      
      direccion <- .cot_valor_seguro(cliente$CliDir)
      
      ciudad <- .cot_valor_seguro(cliente$CliDir1)
      
      partes_direccion <- c(direccion, ciudad)
      
      partes_direccion <- partes_direccion[nzchar(partes_direccion)]
      
      direccion_completa <- paste(unique(partes_direccion), collapse = " - ")
      
      if (!nzchar(direccion_completa)) {
        direccion_completa <- "Sin dirección registrada"
      }
      
      telefono <- .cot_valor_seguro(cliente$CliTel)
      
      if (!nzchar(telefono)) {
        telefono <- "Sin teléfono registrado"
      }
      
      fecha_ini <- if (is.null(fecha_ini_r())) "" else format(fecha_ini_r(), "%d/%m/%Y %H:%M")
      
      fecha_fin <- if (is.null(fecha_fin_r())) "" else format(fecha_fin_r(), "%d/%m/%Y %H:%M")
      
      tagList(
        racafeForms::racafeInfoGrid(
          racafeForms::racafeInfoItem("CLIENTE", cliente$PerRazSoc[[1]]),
          racafeForms::racafeInfoItem("TELÉFONO", telefono),
          racafeForms::racafeInfoItem("DIRECCIÓN", direccion_completa),
          racafeForms::racafeInfoItem("VIGENCIA", paste(fecha_ini, "a", fecha_fin)),
          columns = 2
        ),
        racafeForms::racafeInfoGrid(
          racafeForms::racafeDataBadge("PRODUCTOS", length(productos)),
          racafeForms::racafeDataBadge("KG", .cot_formatear_numero_tabla(
            sum(vapply(productos, `[[`, numeric(1), "cantidad")), dec = 1)),
          racafeForms::racafeDataBadge("TOTAL", .cot_formatear_pesos(total_general_r()), variant = "danger"),
          columns = 3
        ),
        if (!length(productos)) {
          racafeForms::racafeFormMessage("No hay productos agregados", type = "muted", size = "compact")
        } else {
          .cot_tabla_resumen_ui(productos, total_general_r())
        }
      )
    })
    
    # Validacion ----
    .cot_esquema_validacion <- function() {
      campos <- list(
        `COT_Cliente-sel` = list(label = "Cliente", required = TRUE, kind = "select"),
        COT_FechaIni = list(label = "Efectiva Desde", required = TRUE, kind = "datetime"),
        COT_FechaFin = list(label = "Efectiva Hasta", required = TRUE, kind = "datetime"),
        COT_Responsable = list(label = "Responsable", required = FALSE, kind = "select"),
        COT_Divisa = list(label = "Divisa", required = TRUE, kind = "select"),
        COT_FPago = list(label = "Forma de Pago", required = TRUE, kind = "select")
      )
      campos_producto <- c(LinNeg = "Línea de Negocio", Categoria = "Categoría",
                           Producto = "Producto", Presentacion = "Presentación", Empaque = "Empaque",
                           Cantidad = "Cantidad (Kilos)", Precio = "Precio por Kilo")
      for (id_producto in names(productos_modulos_rv())) {
        numero <- sub("Producto_", "", id_producto, fixed = TRUE)
        for (campo in names(campos_producto)) {
          clave <- paste0(id_producto, "-", campo)
          campos[[clave]] <- list(label = paste("Producto", numero, campos_producto[[campo]]),
                                  required = TRUE,
                                  kind = if (campo %in% c("Cantidad", "Precio")) "numeric" else "select")
        }
      }
      campos
    }
    
    .cot_reglas_validacion <- function() {
      reglas <- list(COT_FechaFin = function(value, values) {
        if (is.null(values$COT_FechaIni) || value > values$COT_FechaIni) TRUE else
          "La fecha final debe ser posterior a la inicial."
      })
      for (id_producto in names(productos_modulos_rv())) {
        for (campo in c("Cantidad", "Precio")) {
          clave <- paste0(id_producto, "-", campo)
          reglas[[clave]] <- function(value, values) {
            if (length(value) == 1L && is.finite(value) && value > 0) TRUE else
              "Debe ser mayor que cero."
          }
        }
      }
      reglas
    }
    
    .validar_formulario <- function() {
      errores <- character(0)
      if (!nzchar(cliente_seleccionado_r())) errores <- c(errores, "Seleccione un cliente.")
      if (!length(productos_modulos_rv())) errores <- c(errores, "Agregue al menos un producto.")
      errores
    }
    
    # Consecutivo ----
    .cache_max_cot_rv <- reactiveVal(NULL)
    
    .obtener_max_cotizacion <- function() {
      valor_cache <- isolate(.cache_max_cot_rv())
      
      if (!is.null(valor_cache)) {
        return(valor_cache)
      }
      
      cotizaciones <- tryCatch(
        racafeBD::CargarDatos("CRMCOTIZACION"),
        error = function(e) data.frame(ConsCot = integer())
      )
      
      valor <- if (nrow(cotizaciones) == 0L) 0L else suppressWarnings(max(cotizaciones$ConsCot, na.rm = TRUE))
      
      if (!is.finite(valor)) {
        valor <- 0L
      }
      
      valor <- as.integer(valor)
      
      .cache_max_cot_rv(valor)
      
      valor
    }
    
    # PDF ----
    .crear_tabla_encabezado <- function(consecutivo, vendedor, cliente) {
      filas <- list(
        c("Numero Cotización:", sprintf("%05d", consecutivo), "", ""),
        c("Efectiva Desde:", format(fecha_ini_r(), "%d/%m/%Y %H:%M:%S"),
          "Efectiva Hasta:", format(fecha_fin_r(), "%d/%m/%Y %H:%M:%S")),
        c("", "", "", ""),
        c("De:", vendedor$Nombre %||% "", "Email:", vendedor$Email %||% "")
      )
      
      if (!isTRUE(input$COT_NombrePropio)) {
        filas <- c(filas, list(c("Cargo:", vendedor$Cargo %||% "", "", "")))
      }
      
      filas <- c(filas, list(
        c("", "", "", ""),
        c("Razón Social:", cliente$PerRazSoc[[1]] %||% "", "NIT:",
          ifelse(is.na(cliente$CliNitPpal[[1]]), "", as.character(cliente$CliNitPpal[[1]]))),
        c("Dirección:", cliente$CliDir[[1]] %||% "", "Tel:", cliente$CliTel[[1]] %||% ""),
        c("País:", "COLOMBIA", "Ciudad:", cliente$CliDir1[[1]] %||% "")
      ))
      
      do.call(rbind, lapply(filas, function(fila) {
        data.frame(Campo1 = fila[[1]], Valor1 = fila[[2]], Campo2 = fila[[3]], Valor2 = fila[[4]],
                   stringsAsFactors = FALSE)
      }))
    }
    
    .obtener_datos_productos <- function() {
      lapply(isolate(productos_modulos_rv()), function(modulo) modulo())
    }
    
    .crear_tabla_pdf <- function(datos_lista) {
      productos_df <- bind_rows(lapply(datos_lista, function(datos) {
        descripcion <- LimpiarCadena(
          paste(trimws(.cot_valor_seguro(datos$Producto)), "en presentación de",
                trimws(.cot_valor_seguro(datos$Presentacion)), "en empaque",
                trimws(.cot_valor_seguro(datos$Empaque))),
          rem_numeros = FALSE, rem_caresp = FALSE, rem_acentos = FALSE
        )
        
        data.frame(Descripcion = descripcion, Kilos = .cot_numero_seguro(datos$Cantidad),
                   Precio_Kilo = .cot_numero_seguro(datos$Precio), Total = .cot_numero_seguro(datos$Total),
                   stringsAsFactors = FALSE)
      }))
      
      rownames(productos_df) <- NULL
      
      productos_df
    }
    
    .crear_df_bd <- function(datos_lista, consecutivo, cliente) {
      responsable <- if (isTRUE(input$COT_NombrePropio)) {
        "Área Comercial Industria Nacional"
      } else {
        input$COT_Responsable %||% ""
      }
      
      bind_rows(lapply(datos_lista, function(datos) {
        data.frame(
          ConsCot = consecutivo, UsuarioCrea = usuario(), FechaHoraCrea = Sys.time(),
          Cliente = cliente$PerRazSoc[[1]] %||% "", FechaIni = as.POSIXct(fecha_ini_r()),
          FechaFin = as.POSIXct(fecha_fin_r()), LineaNegocio = .cot_valor_seguro(datos$LinNeg),
          Responsable = responsable, Categoria = .cot_valor_seguro(datos$Categoria),
          Producto = .cot_valor_seguro(datos$Producto), Presentacion = .cot_valor_seguro(datos$Presentacion),
          Empaque = .cot_valor_seguro(datos$Empaque), Cantidad = .cot_numero_seguro(datos$Cantidad),
          PrecioKilo = .cot_numero_seguro(datos$Precio), TotalProducto = .cot_numero_seguro(datos$Total),
          stringsAsFactors = FALSE
        )
      }))
    }
    
    # Limpieza ----
    .limpiar_formulario <- function() {
      ids_actuales <- names(isolate(productos_modulos_rv()))
      
      for (id_producto in ids_actuales) {
        removeUI(selector = paste0("#", ns(id_producto), "-wrapper"), immediate = TRUE)
      }
      
      productos_modulos_rv(list())
      
      contador_productos_rv(0L)
      
      .crm_update_select(session, "COT_Cliente-sel", selected = character(0))
      
      .crm_update_select(session, "COT_Divisa", selected = intersect("Peso Colombiano", catalogos_rv()$cot_divisa))
      
      .crm_update_select(session, "COT_FPago", selected = intersect("PAGO ANTICIPADO", catalogos_rv()$formapago))
      
      .crm_update_select(session, "COT_Responsable", selected = character(0))
      
      racafeForms::racafeUpdateDate(session, "COT_FechaIni",
                                    value = as.POSIXct(paste(Sys.Date(), "08:30:00"), tz = "America/Bogota"))
      racafeForms::racafeUpdateDate(session, "COT_FechaFin",
                                    value = as.POSIXct(paste(Sys.Date(), "16:00:00"), tz = "America/Bogota"))
      
      racafeForms::racafeUpdateSwitch(session, "COT_NombrePropio", value = TRUE)
      
      .crear_nuevo_producto()
    }
    
    # Descarga ----
    validacion_intentada_rv <- reactiveVal(FALSE)
    .cot_comprobar_campos <- function() {
      racafeForms::racafeValidateForm(
        input, form = .cot_esquema_validacion(), ns = session$ns,
        required = list(COT_Responsable = function(values) !isTRUE(input$COT_NombrePropio)),
        rules = .cot_reglas_validacion()
      )
    }
    validador_pdf <- list(
      check = function() isolate(.cot_comprobar_campos()),
      validate = function() {
        actual <- isolate(.cot_comprobar_campos())
        validacion_intentada_rv(TRUE)
        racafeForms::racafeShowValidation(session, actual, mode = "inline")
        isTRUE(actual$valid)
      }
    )
    observe({
      if (!isTRUE(validacion_intentada_rv())) return()
      actual <- .cot_comprobar_campos()
      racafeForms::racafeShowValidation(session, actual, mode = "inline", focus = FALSE)
    })
    .cot_limpiar_validacion <- function() {
      validacion_intentada_rv(FALSE)
      actual <- isolate(.cot_comprobar_campos())
      actual$valid <- TRUE
      actual$errors <- character(0)
      actual$pending <- character(0)
      racafeForms::racafeShowValidation(session, actual, mode = "inline", focus = FALSE)
      error_cliente_rv(NULL)
    }
    
    observeEvent(input$COT_SolicitarPDF, {
      if (!nzchar(cliente_seleccionado_r())) {
        error_cliente_rv("Seleccione o ingrese un cliente.")
      }
      if (!isTRUE(validador_pdf$validate())) return(invisible(NULL))
      errores <- .validar_formulario()
      if (length(errores)) {
        racafeForms::racafeNotify(type = "error", duration = 5, title = paste(errores, collapse = " "),
                                session = shiny::getDefaultReactiveDomain())
        return(invisible(NULL))
      }
      if (!isTRUE(.cot_formulario_completo())) {
        racafeForms::racafeNotify(session, "Campos pendientes",
                                  "Complete la cotización antes de generar el PDF.", type = "warning")
        return(invisible(NULL))
      }
      showModal(racafeForms::racafeModalDetail(
        title = "Confirmar cotización", size = "subventana3",
        shiny::tags$p("Revise el resumen antes de generar y registrar el PDF."),
        shiny::tags$div(class = "cot-resumen-modal", .cot_resumen_ui()),
        footer = racafeForms::racafeModalFooter(
          right = shiny::tags$div(class = "cot-modal-actions",
                                  shiny::tagAppendAttributes(shiny::modalButton("Cancelar"),
                                                             class = "rf-btn rf-btn--secondary rf-btn--xs"),
                                  racafeForms::racafeButtonDownload(
                                    "COT_Crear", label = "Confirmar y descargar", icon = "file-pdf",
                                    variant = "primary", size = "xs", ns = ns, validate = TRUE
                                  )
          )
        )
      ))
    })
    
    racafeForms::racafeDownloadHandler(
      id = "COT_Crear", validation = validador_pdf, session = session,
      
      filename = function() {
        cliente <- cliente_actual_r()
        
        cliente_clean <- if (nrow(cliente) == 1L) {
          gsub("[^A-Za-z0-9_]", "_", cliente$PerRazSoc[[1]])
        } else {
          "cliente"
        }
        
        paste0("Cotizacion_", cliente_clean, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      contentType = "application/pdf",
      content = function(file) {
        errores <- .validar_formulario()
        
        if (length(errores) > 0L) {
          stop(paste(errores, collapse = " "), call. = FALSE)
        }


        
        tryCatch({
          consecutivo <- .obtener_max_cotizacion() + 1L
          vendedor <- .obtener_datos_vendedor()
          cliente <- cliente_actual_r()
          
          if (is.null(vendedor) || nrow(cliente) != 1L) {
            stop("Error al obtener los datos del vendedor o cliente.")
          }
          
          datos_productos <- .obtener_datos_productos()
          
          tabla_encabezado <- .crear_tabla_encabezado(consecutivo, vendedor, cliente)
          
          tabla_productos <- .crear_tabla_pdf(datos_productos)
          
          params <- list(
            encabezado = tabla_encabezado, productos = tabla_productos,
            forma_pago = input$COT_FPago %||% "PAGO ANTICIPADO", total_general = total_general_r(),
            divisa = stringr::str_to_upper(input$COT_Divisa %||% "PESO COLOMBIANO"),
            logo = .cot_logo_compartido()
          )
          
          on.exit(unlink(params$logo), add = TRUE)
          if (!file.exists(.COT_RMD_PATH)) {
            stop("Falta la plantilla cotizacion.Rmd para generar el PDF. Contacte al administrador.", call. = FALSE)
          }
          rmarkdown::render(input = .COT_RMD_PATH, output_file = file, params = params,
                            envir = new.env(parent = globalenv()), quiet = FALSE, encoding = "UTF-8")
          
          if (!file.exists(file) || file.info(file)$size <= 0) {
            stop("El PDF no fue generado correctamente.")
          }
          
          df_bd <- .crear_df_bd(datos_productos, consecutivo, cliente)
          
          racafeBD::AgregarDatos(df_bd, "CRMCOTIZACION")
          
          .cache_max_cot_rv(consecutivo)
          
          .cot_limpiar_validacion()
          removeModal()
          .limpiar_formulario()
          racafeForms::racafeNotify(session, "Cotización generada",
                                    "El documento PDF se generó correctamente.",
                                    type = "success", duration = 4)
        }, error = function(error) {
          racafeForms::racafeNotify(type = "error", duration = 10,
                                  title = paste0("Error al generar cotización: ", conditionMessage(error)),
                                  session = shiny::getDefaultReactiveDomain())
          
          stop(conditionMessage(error), call. = FALSE)
        })
      }
    )
  })
}
