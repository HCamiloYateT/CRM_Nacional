# Oportunidades: compatibilidad con registros antiguos y formulario vigente ----
.op_contactos_visibles <- function(contactos, usuario, es_admin) {
  if (isTRUE(es_admin)) return(contactos)
  permitido <- rep(FALSE, nrow(contactos))
  for (col in intersect(c("Asesor", "UsuarioCrea", "Usuario"), names(contactos))) {
    permitido <- permitido | (!is.na(contactos[[col]]) & toupper(trimws(contactos[[col]])) == toupper(usuario))
  }
  contactos[permitido, , drop = FALSE]
}
.op_preparar <- function(oportunidades, contactos, hechos, usuario, es_admin = FALSE, corte = Sys.Date()) {
  x <- oportunidades
  id <- as.character(.gc_columna(x, "IdOportunidad"))
  x$Editable <- !is.na(id) & nzchar(id) & !duplicated(id) & !duplicated(id, fromLast = TRUE)
  x$Oportunidad <- ifelse(x$Editable, id, paste0("HIST-", seq_len(nrow(x))))
  x$Contacto <- as.character(.gc_columna(x, "CodContacto"))
  # Solo homologar razones sociales antiguas cuando determinan un único contacto.
  nombres <- contactos %>% dplyr::group_by(PerRazSoc) %>%
    dplyr::filter(dplyr::n_distinct(CodContacto) == 1L) %>%
    dplyr::summarise(Cod = as.character(dplyr::first(CodContacto)), .groups = "drop")
  antiguo <- as.character(.gc_columna(x, "PerRazSoc"))
  vacios <- is.na(x$Contacto)
  x$Contacto[vacios] <- nombres$Cod[match(antiguo[vacios], nombres$PerRazSoc)]
  x$Tipo <- as.character(.gc_columna(x, c("TipoOportunidad", "Tipo")))
  x$Asesor <- as.character(.gc_columna(x, "Asesor"))
  x$Autor <- as.character(.gc_columna(x, c("UsuarioCrea", "Usuario")))
  x$Linea <- .gc_num(.gc_columna(x, "LinNegCod"))
  codigos <- c(CONVENCIONALES = 10000, `A LA MEDIDA` = 21000, DIFERENCIADOS = 20000)
  sin_linea <- is.na(x$Linea)
  x$Linea[sin_linea] <- unname(codigos[as.character(.gc_columna(x, "LineaNegocio"))[sin_linea]])
  x$Categoria <- as.character(.gc_columna(x, "Categoria"))
  x$Producto <- as.character(.gc_columna(x, "Producto"))
  x$Creacion <- as.Date(.gc_columna(x, "FechaHoraCrea"))
  x$Cumplimiento <- as.Date(.gc_columna(x, "FechaCumpOP"))
  x$SacosObjetivo <- .gc_num(.gc_columna(x, c("Sacos", "SacosOP")))
  x$MargenKg <- .gc_num(.gc_columna(x, c("Margen", "MargenOP")))
  x$Frecuencia <- .gc_num(.gc_columna(x, "FrecuenciaDias"))
  x$Descartada <- toupper(as.character(.gc_columna(x, c("Descartado", "Descartada"), "NO"))) %in% c("SI", "1", "TRUE")
  permitidos <- .op_contactos_visibles(contactos, usuario, es_admin)
  if (!isTRUE(es_admin)) x <- x[x$Contacto %in% as.character(permitidos$CodContacto) |
    (!is.na(x$Autor) & toupper(x$Autor) == toupper(usuario)), , drop = FALSE]
  meta <- contactos %>% dplyr::transmute(Contacto = as.character(CodContacto),
    ClienteContacto = as.character(PerRazSoc), NitContacto = as.character(PerCod)) %>% dplyr::distinct()
  if (anyDuplicated(meta$Contacto)) stop("El catálogo de contactos tiene códigos duplicados.")
  x <- dplyr::left_join(x, meta, by = "Contacto", na_matches = "never")
  x$Cliente <- dplyr::coalesce(x$ClienteContacto, as.character(.gc_columna(x, c("PerRazSoc", "Cliente"))))
  x$NIT <- dplyr::coalesce(x$NitContacto, as.character(.gc_columna(x, c("CliNitPpal", "NIT"))))
  x$PesoSaco <- ifelse(x$Linea == 10000, 62.5, ifelse(x$Linea %in% c(21000, 20000), 70, NA_real_))
  x$Objetivo70 <- x$SacosObjetivo * x$PesoSaco / 70
  x$MargenObjetivo <- x$SacosObjetivo * x$PesoSaco * x$MargenKg
  x$SacosMes <- ifelse(x$Frecuencia > 0, x$Objetivo70 * 30 / x$Frecuencia, NA_real_)
  x$MargenMes <- ifelse(x$Frecuencia > 0, x$MargenObjetivo * 30 / x$Frecuencia, NA_real_)
  x$SacosEjecutados <- x$MargenEjecutado <- rep(NA_real_, nrow(x))
  clave_ejecucion <- function(contacto, linea, categoria, producto) paste(as.character(contacto),
    as.character(linea), categoria, producto, sep = "\r")
  indices_ejecucion <- split(seq_len(nrow(hechos)), clave_ejecucion(hechos$CodContacto,
    hechos$LinNegCod, hechos$Categoria, hechos$Producto))
  for (i in seq_len(nrow(x))) {
    fila <- x[i, ]
    if (is.na(fila$Creacion) || is.na(fila$Cumplimiento) || is.na(fila$Contacto) || is.na(fila$Linea)) next
    clave <- clave_ejecucion(fila$Contacto, fila$Linea, fila$Categoria, fila$Producto)
    fac <- hechos[indices_ejecucion[[clave]], , drop = FALSE] %>%
      dplyr::filter(!is.na(FecFact), FecFact >= fila$Creacion, FecFact <= min(fila$Cumplimiento, corte),
      EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"))
    # No asignar ventas de otros productos a la oportunidad del mismo cliente.
    if (is.na(fila$Producto) || is.na(fila$Categoria)) next
    fac <- fac %>% dplyr::filter(Producto == fila$Producto, Categoria == fila$Categoria)
    x$SacosEjecutados[[i]] <- if (anyNA(fac$SacosFact)) NA_real_ else sum(fac$SacosFact)
    x$MargenEjecutado[[i]] <- if (any(fac$EstadoFinanciero != "CERRADO") || anyNA(fac$Margen)) {
      NA_real_
    } else sum(fac$Margen)
  }
  x$CumplSacos <- .gc_ratio(x$SacosEjecutados, x$Objetivo70)
  x$CumplMargen <- .gc_ratio(x$MargenEjecutado, x$MargenObjetivo)
  x$Estado <- ifelse(x$Descartada, "Descartada", ifelse(!is.na(x$CumplSacos) & x$CumplSacos >= 1 &
    !is.na(x$CumplMargen) & x$CumplMargen >= 1, "Cumplida",
    ifelse(!is.na(x$Cumplimiento) & x$Cumplimiento < corte, "Vencida", "En gestión")))
  x
}

OportunidadesGestionUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("crear", "Nueva oportunidad", icon = "plus", ns = ns)),
    racafeForms::racafeFormRow(columns = 2,
      .crm_select("estado", "Mostrar", choices = c("Activas", "Descartadas"), selected = c("Activas", "Descartadas"),
        multiple = TRUE, ns = ns),
      .crm_select("dimension", "Analizar por", choices = c("Tipo", "Estado", "Categoria", "Producto", "Linea", "Asesor"),
        selected = "Estado", ns = ns)),
    .gc_tarjetas_ui(ns, c(total = "Total de oportunidades", activas = "Activas", cumplidas = "Cumplidas",
      vencidas = "Vencidas", proximas = "Próximas a 30 días", descartadas = "Descartadas",
      sacos = "Sacos potenciales · 70 kg", margen = "Margen potencial · COP")),
    .crm_disclosure("Distribución y potencial comercial", shiny::fluidRow(
      shiny::column(6, .crm_plot_output(ns("distribucion"), height = "320px", compact = TRUE)),
      shiny::column(6, .crm_tabla_ui(ns("resumen")))), open = TRUE),
    .crm_disclosure("Evolución de las oportunidades", shiny::fluidRow(
      shiny::column(6, .crm_plot_output(ns("evolucion"), height = "320px", compact = TRUE)),
      shiny::column(6, .crm_tabla_ui(ns("evolucion_detalle"))))),
    .crm_disclosure("Cumplimiento frente a cada objetivo", .crm_plot_output(ns("cumplimiento")),
      .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("detalle")), open = TRUE),
    .crm_nota_metodologica(c("El potencial comercial no es ejecución financiera ni una venta asegurada.",
      "La misma venta puede respaldar oportunidades solapadas: no sume su ejecución como si fuera exclusiva.",
      "Los objetivos se contrastan por contacto, línea, categoría, producto y período propio de cada oportunidad.",
      "El margen sin cierre no es definitivo. Un objetivo sin meta válida queda sin porcentaje de cumplimiento.")))
}

OportunidadesGestion <- function(id, usuario, es_admin, hechos, cargar = racafeBD::CargarDatos, filtros = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    contacto_seleccionado <- shiny::reactiveVal(NULL)
    oportunidad_seleccionada <- shiny::reactiveVal(NULL)
    fuente <- shiny::reactive({ .pr_fuentes()$revision(); revision(); cargar("CRMNALCLOPT") })
    contactos <- shiny::reactive({ .pr_fuentes()$revision(); revision(); cargar("CRMNALCONTACTO") })
    contacto_permitido <- shiny::reactive({
      cod <- contacto_seleccionado()
      permitidos <- .op_contactos_visibles(contactos(), usuario(), es_admin())
      shiny::req(cod %in% as.character(permitidos$CodContacto))
      cod
    })
    oportunidad_permitida <- shiny::reactive({
      cod <- oportunidad_seleccionada()
      visibles <- .op_preparar(fuente(), contactos(), hechos(), usuario(), es_admin())
      shiny::req(cod %in% visibles$Oportunidad[visibles$Editable])
      cod
    })
    creacion <- CrearOportunidad("crear_op", usuario, contacto_permitido)
    descarte <- DescartarOportunidad("descartar_op", usuario, oportunidad_permitida)
    shiny::observeEvent(list(creacion$creaciones(), descarte$n()), {
      revision(shiny::isolate(revision()) + 1L)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$actualizar, revision(revision() + 1L))
    datos <- shiny::reactive({
      f <- filtros()
      corte <- if (length(f$fecha) == 2L) as.Date(f$fecha[[2]]) else Sys.Date()
      x <- .op_preparar(fuente(), contactos(), if (is.null(f)) hechos() else .gc_filtrar_dimensiones(hechos(), f),
        usuario(), es_admin(), corte)
      if (!is.null(f)) {
        x <- x[.in_linea(x$Linea) %in% f$linneg & x$Categoria %in% f$categoria & x$Producto %in% f$producto, , drop = FALSE]
        x <- x[(is.na(x$Asesor) | x$Asesor %in% f$asesor) & length(f$asesor) > 0L, , drop = FALSE]
      }
      estados <- ifelse(x$Descartada, "Descartadas", "Activas")
      x <- x[estados %in% input$estado, , drop = FALSE]
      x
    })
    .gc_tarjetas_server(c(total = "Total de oportunidades", activas = "Activas", cumplidas = "Cumplidas",
      vencidas = "Vencidas", proximas = "Próximas a 30 días", descartadas = "Descartadas",
      sacos = "Sacos potenciales · 70 kg", margen = "Margen potencial · COP"), shiny::reactive({
        x <- datos(); corte <- if (length(filtros()$fecha) == 2L) as.Date(filtros()$fecha[[2]]) else Sys.Date()
        activas <- !x$Descartada & x$Estado != "Cumplida"
        list(total = nrow(x), activas = sum(activas), cumplidas = sum(x$Estado == "Cumplida"),
          vencidas = sum(x$Estado == "Vencida"), descartadas = sum(x$Descartada),
          proximas = sum(activas & !is.na(x$Cumplimiento) & x$Cumplimiento >= corte & x$Cumplimiento <= corte + 30),
          sacos = .com_suma(x$Objetivo70[activas]), margen = .com_suma(x$MargenObjetivo[activas]))
      }), formatos = list(sacos = "numero", margen = "dinero"))
    resumen <- shiny::reactive({
      dimension <- if (length(input$dimension) == 1L && input$dimension %in% c("Tipo", "Estado", "Categoria", "Producto", "Linea", "Asesor")) {
        input$dimension
      } else "Autor"
      datos() %>% dplyr::group_by(.data[[dimension]]) %>%
        dplyr::summarise(Oportunidades = dplyr::n(), SacosObjetivo70 = .com_suma(Objetivo70),
          MargenObjetivo = .com_suma(MargenObjetivo), .groups = "drop")
    })
    .crm_vancouver_server("resumen", data = resumen,
      col_labels = c(SacosObjetivo70 = "Sacos objetivo (70 kg)", MargenObjetivo = "Margen objetivo (COP)"))
    tabla <- .crm_vancouver_server("detalle", data = datos, id_col = "Oportunidad", modo_seleccion = "fila",
      columnas = c("Oportunidad", "NIT", "Cliente", "Autor", "Categoria", "Producto", "Creacion", "Cumplimiento",
        "Estado", "Objetivo70", "MargenObjetivo", "SacosEjecutados", "MargenEjecutado", "CumplSacos", "CumplMargen"),
      col_labels = c(Estado = "Estado de la oportunidad", MargenObjetivo = "Margen objetivo (COP)",
        Objetivo70 = "Sacos objetivo (70 kg)", CumplSacos = "Cumpl. sacos", CumplMargen = "Cumpl. margen"))
    output$distribucion <- .crm_render_plotly({
      x <- resumen(); dimension <- names(x)[[1]]
      x$Etiqueta <- as.character(x[[dimension]])
      plotly::plot_ly(x, height = 320, x = ~Etiqueta, y = ~SacosObjetivo70,
        type = "bar", marker = list(color = "#7A2028"),
        text = ~paste("Oportunidades:", Oportunidades, "· margen potencial COP:", round(MargenObjetivo)),
        hovertemplate = "%{x}<br>Sacos potenciales: %{y:,.2f}<br>%{text}<extra></extra>") %>%
        plotly::layout(meta = list(crm_altura_compacta = TRUE))
    })
    evolucion <- shiny::reactive(datos() %>% dplyr::filter(!is.na(Creacion)) %>%
      dplyr::mutate(Mes = .gc_mes(Creacion)) %>% dplyr::count(Mes, Estado, name = "Oportunidades"))
    .crm_vancouver_server("evolucion_detalle", evolucion)
    output$evolucion <- .crm_render_plotly(plotly::plot_ly(evolucion(), height = 320, x = ~Mes, y = ~Oportunidades,
      color = ~Estado, type = "bar", colors = c("#7A2028", "#495057", "#C8A77D", "#9B7160"),
      hovertemplate = "%{x|%Y-%m}<br>Oportunidades: %{y:.0f}<extra>%{fullData.name}</extra>") %>%
      plotly::layout(meta = list(crm_altura_compacta = TRUE)))
    output$cumplimiento <- .crm_render_plotly(plotly::plot_ly(datos(), x = ~CumplSacos, y = ~CumplMargen,
      text = ~paste(Cliente, Producto, "· objetivo:", Cumplimiento), type = "scatter", mode = "markers",
      marker = list(color = "#7A2028"),
      hovertemplate = "%{text}<br>Cumplimiento sacos: %{x:.1%}<br>Cumplimiento margen: %{y:.1%}<extra></extra>") %>%
      plotly::layout(xaxis = list(title = "Cumplimiento en sacos", tickformat = ".0%"),
        yaxis = list(title = "Cumplimiento en margen", tickformat = ".0%")))
    pedido_oportunidad <- shiny::reactive({
      fila <- datos()[datos()$Oportunidad %in% oportunidad_seleccionada(), , drop = FALSE]
      if (nrow(fila) != 1L) return(NULL)
      .gc_columna(fila, c("CLPdcCod", "PdcCod", "Pedido"))[[1]]
    })
    TrazabilidadPedido("pedido_asociado", hechos, pedido_oportunidad)
    abrir_creacion <- function(codigo) {
      permitidos <- .op_contactos_visibles(contactos(), usuario(), es_admin())
      if (length(codigo) != 1L || is.na(codigo) || !codigo %in% as.character(permitidos$CodContacto)) {
        shiny::showNotification("Seleccione un contacto habilitado para crear la oportunidad.", type = "warning")
        return(invisible(FALSE))
      }
      contacto_seleccionado(as.character(codigo))
      shiny::showModal(racafeForms::racafeModalDetail(title = "Crear oportunidad", size = "subventana2",
        CrearOportunidadUI(session$ns("crear_op")),
        footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
      invisible(TRUE)
    }
    shiny::observeEvent(input$crear, {
      permitidos <- .op_contactos_visibles(contactos(), usuario(), es_admin())
      if (!nrow(permitidos)) {
        shiny::showNotification("No hay contactos habilitados. Cree o asigne primero un contacto en Prospección.",
          type = "warning", duration = 7)
        return()
      }
      etiquetas <- paste(permitidos$PerCod, permitidos$PerRazSoc, sep = " — ")
      shiny::showModal(racafeForms::racafeModalDetail(title = "Seleccionar entidad", size = "subventana1",
        racafeForms::racafeForm(variant = "plain",
          .crm_select("contacto_creacion", "Entidad de la oportunidad",
            choices = stats::setNames(as.character(permitidos$CodContacto), etiquetas), empty = FALSE,
            ns = session$ns),
          racafeForms::racafeButton("continuar_creacion", "Continuar", icon = "arrow-right", ns = session$ns)),
        footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
    })
    shiny::observeEvent(input$continuar_creacion, abrir_creacion(input$contacto_creacion))
    shiny::observeEvent(tabla$seleccion(), {
      seleccion <- tabla$seleccion()
      fila <- datos()[datos()$Oportunidad == seleccion$id, , drop = FALSE]
      shiny::req(nrow(fila) == 1L)
      oportunidad_seleccionada(fila$Oportunidad)
      shiny::showModal(racafeForms::racafeModalDetail(title = "Detalle de oportunidad", size = "subventana2",
        .crm_tabla_vancouver(.crm_nits_tabla(fila), font_size = 11, row_padding = 2),
        if (length(pedido_oportunidad()) == 1L && !is.na(pedido_oportunidad())) .crm_disclosure("Pedido asociado",
          TrazabilidadPedidoUI(session$ns("pedido_asociado"))),
        if (isTRUE(fila$Editable) && !fila$Descartada) DescartarOportunidadUI(session$ns("descartar_op")),
        if (!isTRUE(fila$Editable)) .crm_nota("Registro histórico de solo lectura: no tiene un ID vigente único."),
        footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
      tabla$limpiar()
    })
    .gc_descarga(output, datos, "Oportunidades")
    list(datos = datos)
  })
}
