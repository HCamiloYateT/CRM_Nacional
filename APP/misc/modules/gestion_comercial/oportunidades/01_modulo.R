# Oportunidades: compatibilidad con registros antiguos y formulario vigente ----
.op_contactos_visibles <- function(contactos, usuario, es_admin) {
  if (isTRUE(es_admin)) return(contactos)
  permitido <- rep(FALSE, nrow(contactos))
  for (col in intersect(c("Asesor", "UsuarioCrea", "Usuario"), names(contactos))) {
    permitido <- permitido | (!is.na(contactos[[col]]) & toupper(trimws(contactos[[col]])) == toupper(usuario))
  }
  contactos[permitido, , drop = FALSE]
}
.op_preparar <- function(oportunidades, contactos, hechos, usuario, es_admin = FALSE) {
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
  for (i in seq_len(nrow(x))) {
    fila <- x[i, ]
    if (is.na(fila$Creacion) || is.na(fila$Cumplimiento) || is.na(fila$Contacto) || is.na(fila$Linea)) next
    fac <- hechos %>% dplyr::filter(as.character(CodContacto) == fila$Contacto, LinNegCod == fila$Linea,
      !is.na(FecFact), FecFact >= fila$Creacion, FecFact <= min(fila$Cumplimiento, Sys.Date()),
      EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"))
    # No asignar ventas de otros productos a la oportunidad del mismo cliente.
    if (is.na(fila$Producto) || is.na(fila$Categoria)) next
    fac <- fac %>% dplyr::filter(Producto == fila$Producto, Categoria == fila$Categoria)
    x$SacosEjecutados[[i]] <- sum(fac$SacosFact, na.rm = TRUE)
    x$MargenEjecutado[[i]] <- if (any(fac$EstadoFinanciero != "CERRADO") || anyNA(fac$Margen)) {
      NA_real_
    } else sum(fac$Margen)
  }
  x$CumplSacos <- .gc_ratio(x$SacosEjecutados, x$Objetivo70)
  x$CumplMargen <- .gc_ratio(x$MargenEjecutado, x$MargenObjetivo)
  x$Estado <- ifelse(x$Descartada, "Descartada", ifelse(!is.na(x$CumplSacos) & x$CumplSacos >= 1 &
    !is.na(x$CumplMargen) & x$CumplMargen >= 1, "Cumplida",
    ifelse(!is.na(x$Cumplimiento) & x$Cumplimiento < Sys.Date(), "Vencida", "En gestión")))
  x
}

OportunidadesGestionUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain",
    racafeForms::racafeActionBar(left = racafeForms::racafeButton("actualizar", "Actualizar", ns = ns,
      variant = "secondary", icon = "arrows-rotate")),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputSelect("contacto", "Contacto para una nueva oportunidad", choices = character(), ns = ns),
      racafeForms::racafeButton("crear", "Nueva oportunidad", icon = "plus", ns = ns)),
    .gc_tarjetas_ui(ns, c(total = "Oportunidades activas", vencidas = "Vencidas",
      sacos = "Objetivo de sacos 70 kg", margen = "Margen objetivo")),
    racafeForms::racafeFormRow(columns = 2,
      racafeForms::racafeInputSelect("estado", "Mostrar", choices = c("Activas", "Todas", "Descartadas"), ns = ns),
      racafeForms::racafeInputSelect("dimension", "Agrupar tablero por", choices = c("Autor", "Categoria", "Producto"),
        ns = ns)),
    .crm_descarga_ui("descargar", ns), .crm_tabla_ui(ns("resumen"), titulo = "Tablero de oportunidades"),
    .crm_tabla_ui(ns("detalle"), titulo = "Oportunidades y cumplimiento"),
    .crm_nota(c("El cumplimiento cruza contacto, línea, categoría y producto entre creación y fecha objetivo.",
      "Los objetivos de oportunidades distintas pueden solaparse: la misma venta no es ejecución exclusiva asignada.",
      "El margen de meses abiertos no se presenta como contable; las descartadas se excluyen de los indicadores.",
      "La frecuencia mensual solo se calcula cuando el registro histórico tiene FrecuenciaDias.")))
}
OportunidadesGestion <- function(id, usuario, es_admin, hechos, cargar = racafeBD::CargarDatos) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    contacto_seleccionado <- shiny::reactiveVal(NULL)
    oportunidad_seleccionada <- shiny::reactiveVal(NULL)
    fuente <- shiny::reactive({ revision(); cargar("CRMNALCLOPT") })
    contactos <- shiny::reactive({ revision(); cargar("CRMNALCONTACTO") })
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
    shiny::observe({
      x <- .op_contactos_visibles(contactos(), usuario(), es_admin())
      etiquetas <- paste(x$PerCod, x$PerRazSoc, sep = " — ")
      racafeForms::racafeUpdateSelect(session, "contacto", choices = stats::setNames(x$CodContacto, etiquetas),
        selected = shiny::isolate(input$contacto))
    })
    datos <- shiny::reactive({
      x <- .op_preparar(fuente(), contactos(), hechos(), usuario(), es_admin())
      if (identical(input$estado, "Descartadas")) x <- x[x$Descartada, ]
      if (is.null(input$estado) || identical(input$estado, "Activas")) x <- x[!x$Descartada, ]
      x
    })
    .gc_tarjetas_server(c(total = "Oportunidades activas", vencidas = "Vencidas",
      sacos = "Objetivo de sacos 70 kg", margen = "Margen objetivo"), shiny::reactive({
      x <- datos(); x <- x[!x$Descartada, ]
      list(total = nrow(x), vencidas = sum(x$Estado == "Vencida"), sacos = .gc_suma(x$Objetivo70),
        margen = .gc_suma(x$MargenObjetivo))
    }), formatos = list(sacos = "numero", margen = "dinero"))
    resumen <- shiny::reactive({
      dimension <- if (length(input$dimension) == 1L && input$dimension %in% c("Autor", "Categoria", "Producto")) {
        input$dimension
      } else "Autor"
      datos() %>% dplyr::group_by(.data[[dimension]]) %>%
        dplyr::summarise(Oportunidades = dplyr::n(), SacosObjetivo70 = .gc_suma(Objetivo70),
          MargenObjetivo = .gc_suma(MargenObjetivo), .groups = "drop")
    })
    .crm_vancouver_server("resumen", data = resumen)
    tabla <- .crm_vancouver_server("detalle", data = datos, id_col = "Oportunidad", modo_seleccion = "fila",
      columnas = c("Oportunidad", "NIT", "Cliente", "Autor", "Categoria", "Producto", "Creacion", "Cumplimiento",
        "Estado", "Objetivo70", "MargenObjetivo", "SacosEjecutados", "MargenEjecutado", "CumplSacos", "CumplMargen"),
      col_labels = c(Objetivo70 = "Objetivo sacos 70 kg", CumplSacos = "Cumpl. sacos", CumplMargen = "Cumpl. margen"))
    abrir_creacion <- function(codigo) {
      permitidos <- .op_contactos_visibles(contactos(), usuario(), es_admin())
      if (length(codigo) != 1L || is.na(codigo) || !codigo %in% as.character(permitidos$CodContacto)) {
        shiny::showNotification("Seleccione un contacto habilitado para crear la oportunidad.", type = "warning")
        return(invisible(FALSE))
      }
      contacto_seleccionado(as.character(codigo))
      racafeForms::racafeUpdateSelect(session, "contacto", selected = as.character(codigo))
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
      codigo <- input$contacto
      if (length(codigo) == 1L && !is.na(codigo) && codigo %in% as.character(permitidos$CodContacto)) {
        abrir_creacion(codigo)
      } else {
        etiquetas <- paste(permitidos$PerCod, permitidos$PerRazSoc, sep = " — ")
        shiny::showModal(racafeForms::racafeModalDetail(title = "Seleccionar contacto", size = "subventana1",
          racafeForms::racafeForm(variant = "plain",
            racafeForms::racafeInputSelect("contacto_creacion", "Contacto de la oportunidad",
              choices = stats::setNames(as.character(permitidos$CodContacto), etiquetas), empty = FALSE,
              ns = session$ns),
            racafeForms::racafeButton("continuar_creacion", "Continuar", icon = "arrow-right", ns = session$ns)),
          footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
      }
    })
    shiny::observeEvent(input$continuar_creacion, abrir_creacion(input$contacto_creacion))
    shiny::observeEvent(tabla$seleccion(), {
      seleccion <- tabla$seleccion()
      fila <- datos()[datos()$Oportunidad == seleccion$id, , drop = FALSE]
      shiny::req(nrow(fila) == 1L)
      oportunidad_seleccionada(fila$Oportunidad)
      shiny::showModal(racafeForms::racafeModalDetail(title = "Detalle de oportunidad", size = "subventana2",
        .crm_tabla_vancouver(.crm_nits_tabla(fila), font_size = 11, row_padding = 2),
        if (isTRUE(fila$Editable) && !fila$Descartada) DescartarOportunidadUI(session$ns("descartar_op")),
        if (!isTRUE(fila$Editable)) .crm_nota("Registro histórico de solo lectura: no tiene un ID vigente único."),
        footer = racafeForms::racafeModalFooter(right = shiny::modalButton("Cerrar"))))
      tabla$limpiar()
    })
    .gc_descarga(output, datos, "Oportunidades")
    list(datos = datos)
  })
}
