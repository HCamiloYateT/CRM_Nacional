# CrearContacto
CrearContactoUI <- function(id) {
  ns <- NS(id)
  .accion_formulario_ui(id,
    useShinyjs(),
    racafeForms::racafePanel(title = NULL, padding = "compact", bordered = FALSE,
                             racafeForms::racafeForm(size = "compact", variant = "plain",
                                                     .crm_select("CampoAutorizaDatos", label = "Autoriza Tratamiento de Datos", required = TRUE,
                                                                                    choices = c("SI", "NO"), selected = NULL, multiple = FALSE,
                                                                                    empty = FALSE, search = FALSE, ns = ns),
                                                     conditionalPanel(
                                                       condition = paste0("input['", ns("CampoAutorizaDatos"), "'] == 'SI'"),
                                                       racafeForms::racafeInputText("CampoNit", label = "NIT",
                                                                                    placeholder = "Sin dígito de verificación", ns = ns),
                                                       uiOutput(ns("ValidacionNit")),
                                                       racafeForms::racafeInputText("CampoRazonSocial", label = "Razón Social", ns = ns),
                                                       uiOutput(ns("SimilitudRazonSocial")),
                                                       .crm_select("CampoOrigen", label = "Origen del Contacto", required = TRUE,
                                                                                      choices = stats::setNames(Choices()$origen, toupper(Choices()$origen)),
                                                                                      selected = NULL, multiple = FALSE, ns = ns),
                                                       shinyjs::hidden(
                                                         div(id = ns("WrapDetalleOrigen"),
                                                             .crm_select("CampoDetalleOrigen", label = "Detalle del Origen", required = TRUE,
                                                                                            choices = character(0), selected = "", multiple = FALSE, ns = ns))
                                                       ),
                                                       racafeForms::racafeFormFooter(align = "right",
                                                                                     racafeForms::racafeButton("BotonGuardar", "Crear Contacto", icon = "save",
                                                                                                               variant = "primary", size = "xs", ns = ns)
                                                       )
                                                     )
                             )
    )
  )
}
CrearContacto <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    contador_creaciones <- reactiveVal(0)
    codigo_creado <- reactiveVal(NULL)
    detalle_es_requerido <- reactiveVal(FALSE)
    
    # Funciones ----
    # Normalizacion de texto.
    .NormalizarRazonSocial <- function(texto) {
      if (EsVacio(texto)) return(NA_character_)
      str_squish(texto) %>% str_to_upper()
    }
    .NormalizarNit <- function(x) {
      x <- trimws(as.character(x))
      x <- x[!is.na(x) & nzchar(x)]
      sub("^0+(?=[0-9])", "", x, perl = TRUE)
    }
    .BuscarNitExistente <- function(nit, vigente = FALSE) {
      nit <- .NormalizarNit(nit)
      if (!length(nit)) return(NULL)
      if (vigente) {
        for (tabla in c("CRMNALCONTACTO", "CRMNALMARLOT")) {
          x <- .accion_por_nit(tabla, nit[[1]])
          if (nrow(x)) return(if (tabla == "CRMNALCONTACTO") "contactos" else "registro comercial")
        }
        x <- .accion_por_nit("CRMNALCLIENTE", nit[[1]])
        if (nrow(x)) return("clientes")
        return(NULL)
      }
      fuentes <- list(
        "clientes" = list(datos = clientes_registrados(), columnas = c("CliNitPpal", "CLCliNit", "PerCod", "CLIENTE")),
        "contactos" = list(datos = todos_contactos(), columnas = c("PerCod")),
        "registro comercial" = list(datos = clientes_marlot(), columnas = c("CLIENTE", "PerCod"))
      )
      for (fuente in names(fuentes)) {
        datos <- fuentes[[fuente]]$datos
        if (!is.data.frame(datos) || !nrow(datos)) next
        columnas <- intersect(fuentes[[fuente]]$columnas, names(datos))
        if (!length(columnas)) next
        existe <- any(vapply(columnas, function(columna) {
          any(.NormalizarNit(datos[[columna]]) %in% nit)
        }, logical(1)))
        if (existe) return(fuente)
      }
      NULL
    }
    .RazonComparable <- function(x) {
      x <- stringr::str_to_upper(stringr::str_squish(as.character(x)))
      x <- iconv(x, to = "ASCII//TRANSLIT")
      x[is.na(x)] <- ""
      x <- gsub("[^A-Z0-9 ]", " ", x)
      trimws(gsub("[[:space:]]+", " ", x))
    }
    .BuscarRazonesSimilares <- function(razon, max_resultados = 5L) {
      if (EsVacio(razon)) return(data.frame())
      razon <- .RazonComparable(razon)
      candidatos <- candidatos_similitud()
      if (!nrow(candidatos)) return(candidatos)
      claves <- candidatos$Clave
      denominador <- pmax(nchar(razon), nchar(claves), 1L)
      similitud <- 1 - as.numeric(utils::adist(razon, claves)[1, ]) / denominador
      candidatos$Similitud <- round(100 * similitud, 1)
      candidatos <- candidatos[similitud >= 0.8 & nchar(claves) >= 4L, , drop = FALSE]
      candidatos <- candidatos[order(-candidatos$Similitud), , drop = FALSE]
      head(unique(candidatos), max_resultados)
    }
    candidatos_similitud <- reactive({
      fuentes <- list(
        "Contacto" = todos_contactos(),
        "Cliente" = clientes_registrados(),
        "Cliente comercial" = clientes_marlot()
      )
      candidatos <- lapply(names(fuentes), function(fuente) {
        datos <- fuentes[[fuente]]
        if (!is.data.frame(datos) || !nrow(datos)) return(NULL)
        columna <- intersect(c("PerRazSoc", "CliRazSoc", "RazonSocial", "RAZON_SOCIAL"),
                             names(datos))
        if (!length(columna)) return(NULL)
        data.frame(Fuente = fuente, RazonSocial = as.character(datos[[columna[[1]]]]))
      })
      candidatos <- dplyr::bind_rows(candidatos)
      if (!nrow(candidatos)) return(candidatos)
      candidatos <- candidatos[!is.na(candidatos$RazonSocial) & nzchar(trimws(candidatos$RazonSocial)), , drop = FALSE]
      candidatos$Clave <- .RazonComparable(candidatos$RazonSocial)
      candidatos
    })
    # Validacion de campos obligatorios y unicidad de NIT antes de guardar
    .ValidarCamposFormulario <- function() {
      racafeForms::racafeValidateForm(
        input,
        form = list(
          CampoAutorizaDatos = list(label = "Autoriza Tratamiento de Datos", required = TRUE, kind = "select"),
          CampoNit = list(label = "NIT", required = FALSE, kind = "text"),
          CampoRazonSocial = list(label = "Razón Social", required = FALSE, kind = "text"),
          CampoOrigen = list(label = "Origen del Contacto", required = TRUE, kind = "select"),
          CampoDetalleOrigen = list(label = "Detalle del Origen", required = FALSE, kind = "select")
        ),
        required = list(
          CampoNit = function(values) EsVacio(values$CampoNit) && EsVacio(values$CampoRazonSocial),
          CampoRazonSocial = function(values) {
            EsVacio(values$CampoNit) && EsVacio(values$CampoRazonSocial)
          },
          CampoDetalleOrigen = function(values) isTRUE(detalle_es_requerido())
        ),
        rules = list(
          CampoAutorizaDatos = function(value, values) if (identical(value, "SI")) TRUE else
            "Se requiere autorización para registrar el contacto.",
          CampoNit = function(value, values) {
            nit <- trimws(as.character(value))
            if (!EsEnteroPositivo(nit)) return("El NIT debe ser un valor numérico válido.")
            fuente <- .BuscarNitExistente(nit)
            if (!is.null(fuente)) {
              return(paste0("El NIT ya existe en ", fuente, ". No se puede crear el contacto."))
            }
            TRUE
          }
        ),
        ns = session$ns
      )
    }
    # Limpiar formulario.
    .LimpiarFormulario <- function() {
      updateTextInput(session = session, inputId = "CampoRazonSocial", value = "")
      updateTextInput(session = session, inputId = "CampoNit", value = "")
      .crm_update_select(session, "CampoAutorizaDatos", selected = character())
      .crm_update_select(session, "CampoOrigen", selected = "")
      .crm_update_select(session, "CampoDetalleOrigen", choices = character(0), selected = "")
    }
    
    # Reactivos ----
    ## Datos ----
    todos_contactos <- reactive({
      contador_creaciones()
      .accion_medir("catalogo_contactos", .pr_cargar("CRMNALCONTACTO"))
    })
    clientes_registrados <- reactive({
      .accion_medir("catalogo_clientes", .pr_cargar("CRMNALCLIENTE"))
    })
    clientes_marlot <- reactive({
      .accion_medir("catalogo_comercial", .pr_cargar("CRMNALMARLOT"))
    })
    origenes <- reactive(.pr_cargar("CRMNALORILEAD"))
    ## Inputs ----
    observeEvent(input$CampoOrigen, {
      if (EsVacio(input$CampoOrigen)) {
        detalle_es_requerido(FALSE)
        .crm_update_select(session, "CampoDetalleOrigen", choices = character(), selected = character())
        shinyjs::hide("WrapDetalleOrigen")
        return(invisible(NULL))
      }
      t1 <- tryCatch(
        origenes() %>% filter(Estado == "A", Origen == input$CampoOrigen, !is.na(Detalle)),
        error = function(e) NULL
      )
      tiene_detalle <- !is.null(t1) && nrow(t1) > 0
      cho <- if (tiene_detalle) Unicos(t1$Detalle) else ""
      
      detalle_es_requerido(tiene_detalle)
      .crm_update_select(session, "CampoDetalleOrigen",
                                      choices = stats::setNames(cho, toupper(cho)), selected = NULL)
      
      if (tiene_detalle) shinyjs::show("WrapDetalleOrigen") else shinyjs::hide("WrapDetalleOrigen")
    })
    # Validaciones ----
    nit_digitado_r <- shiny::debounce(reactive(trimws(input$CampoNit %||% "")), 350)
    razon_digitada_r <- shiny::debounce(reactive(trimws(input$CampoRazonSocial %||% "")), 350)
    
    resultado_nit_r <- reactive({
      nit <- nit_digitado_r()
      if (!nzchar(nit)) return(NULL)
      if (!EsEnteroPositivo(nit)) return("El NIT debe ser un valor numérico válido.")
      fuente <- .BuscarNitExistente(nit)
      if (is.null(fuente)) return(NULL)
      paste0("El NIT ya existe en ", fuente, ". No se puede crear el contacto.")
    })
    
    similares_r <- reactive({
      razon <- razon_digitada_r()
      if (nchar(razon) < 4L) return(data.frame())
      .BuscarRazonesSimilares(razon)
    })
    

    output$SimilitudRazonSocial <- .crm_render_ui({
      coincidencias <- similares_r()
      if (!nrow(coincidencias)) return(NULL)
      shiny::tagList(
        racafeForms::racafeFormMessage(
          "Se encontraron razones sociales similares. Revise antes de crear el contacto.",
          type = "warning", size = "compact"
        ),
        shiny::tags$ul(style = "font-size:11px;margin:4px 0 8px;padding-left:18px;",
                       lapply(seq_len(nrow(coincidencias)), function(i) {
                         shiny::tags$li(sprintf("%s (%s; %.1f%%)",
                                                coincidencias$RazonSocial[[i]],
                                                coincidencias$Fuente[[i]],
                                                coincidencias$Similitud[[i]]))
                       }))
      )
    })
    
    .MostrarValidacion <- function(resultado, focus = FALSE) {
      racafeForms::racafeShowValidation(session, resultado, mode = "inline", focus = focus)
    }
    
    validacion_intentada_rv <- reactiveVal(FALSE)
    confirmacion_rv <- reactiveVal(FALSE)
    solicitud_rv <- reactiveVal(NULL)
    
    observe({
      if (!isTRUE(validacion_intentada_rv())) return()
      actual <- .ValidarCamposFormulario()
      .MostrarValidacion(actual, focus = FALSE)
    })
    
    .LimpiarValidacion <- function() {
      validacion_intentada_rv(FALSE)
      actual <- isolate(.ValidarCamposFormulario())
      actual$errors <- character(0)
      actual$pending <- character(0)
      actual$valid <- TRUE
      .MostrarValidacion(actual, focus = FALSE)
    }
    
    observeEvent(input$FormularioListo, {
      confirmacion_rv(FALSE); solicitud_rv(NULL)
      .LimpiarFormulario(); .LimpiarValidacion()
    })
    # Observadores -----
    observeEvent(input$BotonGuardar, {
      validacion_intentada_rv(TRUE)
      validacion <- .accion_medir("validacion", .ValidarCamposFormulario())
      .MostrarValidacion(validacion, focus = TRUE)
      if (!isTRUE(validacion$valid)) return(invisible(NULL))
      
      razon <- .NormalizarRazonSocial(input$CampoRazonSocial)
      nit <- trimws(input$CampoNit %||% "")
      similares <- .BuscarRazonesSimilares(razon)
      confirmacion_rv(TRUE)
      solicitud_rv(list(usuario = usuario(), valores = lapply(c("CampoNit", "CampoRazonSocial", "CampoOrigen",
        "CampoDetalleOrigen", "CampoAutorizaDatos"), function(id) input[[id]])))
      # La confirmación corporativa conserva el formulario y sus valores.
      racafeForms::racafeConfirm(session, "BotonConfirmarGuardado", "Confirmar creación de contacto",
        paste("¿Desea crear el contacto", razon, "?",
          if (nrow(similares)) "Hay nombres similares; revise las coincidencias antes de confirmar."), ns = ns)
      return(invisible(NULL))

    })
    observeEvent(input$BotonCancelarGuardado, {
      confirmacion_rv(FALSE)
      removeModal()
    })
    observeEvent(input$BotonConfirmarGuardado, {
      if (!isTRUE(confirmacion_rv())) return(invisible(NULL))
      if (!isTRUE(input$BotonConfirmarGuardado)) { confirmacion_rv(FALSE); return(invisible(NULL)) }
      solicitud <- solicitud_rv()
      actuales <- lapply(c("CampoNit", "CampoRazonSocial", "CampoOrigen", "CampoDetalleOrigen", "CampoAutorizaDatos"),
        function(id) input[[id]])
      confirmacion_rv(FALSE)
      if (!identical(solicitud$usuario, usuario()) || !identical(solicitud$valores, actuales)) return(invisible(NULL))
      validacion <- .accion_medir("validacion", .ValidarCamposFormulario())
      if (!isTRUE(validacion$valid)) {
        confirmacion_rv(FALSE)
        removeModal()
        .MostrarValidacion(validacion, focus = TRUE)
        return(invisible(NULL))
      }
      confirmacion_rv(FALSE)
      fuente <- .BuscarNitExistente(input$CampoNit, vigente = TRUE)
      if (!is.null(fuente)) {
        validacion$errors[["CampoNit"]] <- paste("El NIT ya existe en", fuente)
        validacion$pending <- names(validacion$errors)
        .MostrarValidacion(validacion, focus = TRUE)
        return(invisible(NULL))
      }
      codigo <- generar_codigo_contacto()
      
      fila <- data.frame(CodContacto   = codigo,
                         UsuarioCrea   = usuario(), FechaHoraCrea = Sys.time(),
                         UsuarioMod    = usuario(), FechaHoraModi = Sys.time(),
                         AutorizaTD    = input$CampoAutorizaDatos,
                         PerRazSoc     = .NormalizarRazonSocial(input$CampoRazonSocial),
                         PerCod        = input$CampoNit,
                         Origen        = input$CampoOrigen, DetOrigen = input$CampoDetalleOrigen,
                         Estado        = "ACTIVO",
                         stringsAsFactors = FALSE) %>% 
        mutate(across(where(is.character) & !c("PerRazSoc"),
                      ~ str_to_upper(ifelse(. == "", NA_character_, .)))
        )
      
      tryCatch({
        columnas <- .accion_medir("esquema", names(CargarDatos("CRMNALCONTACTO", condicion = "1=0")))
        marcas <- list(EsReconstruido = FALSE, FuenteRegistro = "CRM", FechaImputada = FALSE)
        for (nombre in intersect(names(marcas), columnas)) fila[[nombre]] <- marcas[[nombre]]
        .accion_medir("escritura", AgregarDatos(fila, "CRMNALCONTACTO"))
        .pr_invalidar("CRMNALCONTACTO")
        removeModal()
        .accion_medir("componentes", { .LimpiarValidacion(); .LimpiarFormulario() })
        racafeForms::racafeNotify(session, "Contacto creado", paste("Código:", codigo),
                                  type = "success", duration = 5)
        codigo_creado(codigo)
        contador_creaciones(contador_creaciones() + 1)
      }, error = function(error) {
        removeModal()
        .ManejarErrorAccion(error = error, operacion = "guardar el contacto", usuario = usuario())
      })
    })
    
    # Retorno ----
    list(n = reactive(contador_creaciones()), codigo = reactive(codigo_creado()))
  })
}
