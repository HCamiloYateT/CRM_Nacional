# Dependencias Formulario ----
# Este archivo contiene el formulario original extraído de Individual.R.
# Requiere Choices(), pick_opt(), CargarDatos(),
# confirmSweetAlert(), updateAutonumericInput() y las librerías del CRM.
# Si Individual.R ya se carga íntegramente, NO cargue nuevamente este archivo.

# Selectores compactos ----

# IndFormulario ----
IndFormularioUI <- function(id) {
  ns <- shiny::NS(id)
  catalogos <- Choices()
  catalogos$personas <- as.character(catalogos$personas %||% character())
  catalogos$segmento <- as.character(catalogos$segmento %||% character())

  racafeShiny::racafeCollapsible(
    id = ns("informacion_comercial"), title = "Información comercial (editable)", open = FALSE,
    racafeForms::racafeForm(size = "compact", variant = "plain",
      racafeForms::racafeFormGroup(title = "Jerarquía comercial", variant = "soft",
        shiny::uiOutput(ns("ui_frm_padre_nuevo")),
        racafeForms::racafeInputSwitch("frm_ConvertirPrincipal", "Independizar cuenta", ns = ns),
        racafeForms::racafeFormFooter(
          racafeForms::racafeButtonSave("btn_guardar_hijo", "Guardar jerarquía", ns = ns, size = "xxs")
        ),
        shiny::uiOutput(ns("aviso_independizar"))
      ),
      racafeForms::racafeFormGroup(title = "Clasificación comercial", divider = FALSE,
        racafeForms::racafeFormRow(
          racafeForms::racafeInputSelect("frm_Asesor", "Asesor", choices = catalogos$personas, ns = ns),
          racafeForms::racafeInputSelect("frm_Segmento", "Segmento", choices = catalogos$segmento, ns = ns)
        ),
        racafeForms::racafeFormRow(
          racafeForms::racafeInputSelect("frm_Excluir", "Excluir", choices = c("NO", "SI"),
                                        selected = "NO", empty = FALSE, ns = ns),
          racafeForms::racafeInputNumeric("frm_NumMesesRecuperar", "Meses para recuperar",
                                         value = 3L, format = "integer", min = 1, ns = ns)
        )
      ),
      racafeForms::racafeFormFooter(
        racafeForms::racafeButtonSave("btn_guardar", "Guardar cambios", ns = ns, size = "xxs")
      ),
      shiny::uiOutput(ns("aviso_sin_permiso"))
    )
  )
}

IndFormulario <- function(id, identidad, dat, usr, ncliente,
                          identificar_por_clinit = FALSE, es_admin) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0L)
    
    observe({
      autorizado <- isTRUE(es_admin())
      racafeForms::racafeUpdateEnabled(session, "btn_guardar_hijo", enabled = autorizado)
      racafeForms::racafeUpdateEnabled(session, "btn_guardar", enabled = autorizado)
    })
    
    output$aviso_sin_permiso <- renderUI({
      if (es_admin()) return(NULL)
      racafeForms::racafeFormMessage(
        "Solo puede ser guardado por el administrador.", type = "muted", size = "compact"
      )
    })
    
    identidad_activa <- reactiveVal(NULL)
    observeEvent(identidad(), {
      id_val <- identidad()
      req(!is.null(id_val))
      identidad_activa(list(nit = id_val$nit, linneg_cod = id_val$linneg_cod))
    })
    
    observeEvent(identidad(), {
      pendiente_jerarquia(NULL)
      pendiente_comercial(NULL)
    }, ignoreInit = TRUE)
    
    output$ui_frm_padre_nuevo <- renderUI({
      id_val <- identidad_activa()
      req(!is.null(id_val), nrow(ncliente()) > 0)
      choices_padre <- ncliente() %>%
        dplyr::filter(!is.na(PerCod), !is.na(PerRazSoc),
                      as.character(PerCod) != as.character(id_val$nit)) %>%
        dplyr::arrange(PerRazSoc) %>%
        {setNames(as.character(.$PerCod), paste0(.$PerCod, " - ", .$PerRazSoc))}
      
      racafeForms::racafeInputSelect(
        "frm_CuentaPadreNueva", "Cuenta principal", choices = choices_padre,
        selected = "", ns = ns, disabled = isTRUE(input$frm_ConvertirPrincipal)
      )
    })
    
    observeEvent(input$frm_ConvertirPrincipal, {
      racafeForms::racafeUpdateEnabled(session, "frm_CuentaPadreNueva",
                                       enabled = !isTRUE(input$frm_ConvertirPrincipal))
    })
    
    output$aviso_independizar <- renderUI({
      if (!isTRUE(input$frm_ConvertirPrincipal)) return(NULL)
      racafeForms::racafeFormMessage(
        "Al guardar, esta cuenta pasará a ser su propia cuenta principal.",
        type = "muted", size = "compact"
      )
    })
    
    # Guardado de jerarquía ----
    pendiente_jerarquia <- reactiveVal(NULL)
    pendiente_comercial <- reactiveVal(NULL)
    
    limpiar_seleccion <- function(x) {
      if (is.null(x) || !length(x) || is.na(x[[1]])) return(NA_character_)
      x <- trimws(as.character(x[[1]]))
      if (!nzchar(x) || x %in% c("Ninguno", "Ninguna")) return(NA_character_)
      x
    }
    
    ultimo_registro <- function(datos, nit, linneg) {
      datos %>%
        dplyr::filter(as.character(CLCliNit) == as.character(nit),
                      as.character(LinNegCod) == as.character(linneg)) %>%
        dplyr::arrange(dplyr::desc(FecProceso)) %>%
        dplyr::slice_head(n = 1)
    }
    
    guardar_registro <- function(payload, mensaje) {
      payload$FecProceso <- Sys.time()
      payload$Usr <- as.character(usr())
      racafeBD::AgregarDatos(payload, "CRMNALCLIENTE")
      racafeForms::racafeNotify(type = "success", title = mensaje, session = shiny::getDefaultReactiveDomain())
      refresh(isolate(refresh()) + 1L)
    }
    
    observeEvent(input$btn_guardar_hijo, {
      if (!isTRUE(es_admin())) {
        racafeForms::racafeNotify(type = "warning", title = "Solo puede ser guardado por el administrador.",
                                session = shiny::getDefaultReactiveDomain())
        return()
      }
      req(identidad_activa())
      pendiente_jerarquia(NULL)
      id_val <- identidad_activa()
      cuenta_hija <- as.character(id_val$nit)
      convertir <- isTRUE(input$frm_ConvertirPrincipal)
      nuevo_padre <- if (convertir) cuenta_hija else {
        limpiar_seleccion(input$frm_CuentaPadreNueva)
      }
      if (is.na(nuevo_padre)) {
        racafeForms::racafeNotify(type = "error",
                                title = "Seleccione una cuenta principal o active Independizar cuenta.",
                                session = shiny::getDefaultReactiveDomain())
        return()
      }
      if (!nuevo_padre %in% as.character(ncliente()$PerCod)) {
        racafeForms::racafeNotify(type = "error", title = "La cuenta principal seleccionada no es válida.",
                                session = shiny::getDefaultReactiveDomain())
        return()
      }
      
      if (!convertir && identical(nuevo_padre, cuenta_hija)) {
        racafeForms::racafeNotify(type = "warning", title = "Para independizar esta cuenta, active el interruptor.",
                                session = shiny::getDefaultReactiveDomain())
        return()
      }
      
      snap <- CargarDatos("CRMNALCLIENTE") %>%
        dplyr::mutate(FecProceso = as.POSIXct(FecProceso)) %>%
        dplyr::arrange(dplyr::desc(FecProceso)) %>%
        dplyr::distinct(CLCliNit, LinNegCod, .keep_all = TRUE)
      dependencia <- snap %>%
        dplyr::filter(as.character(CLCliNit) == nuevo_padre,
                      as.character(CliNitPpal) == cuenta_hija)
      propios <- snap %>%
        dplyr::filter(as.character(CliNitPpal) == cuenta_hija,
                      as.character(CLCliNit) != cuenta_hija)
      if (nrow(dependencia) || (!convertir && nrow(propios))) {
        racafeForms::racafeNotify(type = "error", title = "La jerarquía indicada genera una dependencia inválida.",
                                session = shiny::getDefaultReactiveDomain())
        return()
      }
      
      anterior <- ultimo_registro(snap, cuenta_hija, id_val$linneg_cod)
      destino <- snap %>%
        dplyr::filter(as.character(CliNitPpal) == nuevo_padre,
                      as.character(LinNegCod) == as.character(id_val$linneg_cod)) %>%
        dplyr::slice_head(n = 1)
      referencia <- if (nrow(anterior)) anterior else destino
      campo <- function(nombre, defecto) {
        if (!nrow(referencia) || !nombre %in% names(referencia)) return(defecto)
        valor <- referencia[[nombre]][1]
        if (length(valor) == 0L || is.na(valor)) defecto else valor
      }
      payload <- data.frame(
        FecProceso = Sys.time(), Usr = usr(),
        LinNegCod = id_val$linneg_cod,
        CLCliNit = as.numeric(cuenta_hija),
        CliNitPpal = as.numeric(nuevo_padre),
        Segmento = campo("Segmento", NA_character_),
        SSPpto = campo("SSPpto", 0),
        MNFCCPpto = campo("MNFCCPpto", 0),
        Asesor = campo("Asesor", NA_character_),
        NumMesesRecuperar = campo("NumMesesRecuperar", 3L),
        Excluir = campo("Excluir", "NO"),
        stringsAsFactors = FALSE
      )
      pendiente_jerarquia(list(payload = payload,
                               nit = as.character(id_val$nit),
                               linneg = as.character(id_val$linneg_cod)))
      racafeForms::racafeConfirm(
        session = session, id = "confirm_hijo", ns = ns,
        title = "Confirmar cambio de jerarquía",
        text = if (convertir) paste0(
          "La cuenta ", cuenta_hija, " se independizará y dejará de heredar ",
          "la información de su cuenta principal anterior. Este cambio puede ",
          "afectar asesor, segmento, presupuestos, indicadores y reportes. ",
          "Confirme únicamente si ha revisado estas implicaciones."
        ) else paste0(
          "La cuenta ", cuenta_hija, " quedará vinculada a la cuenta ",
          nuevo_padre, " y heredará su información comercial. Esto puede ",
          "modificar asesor, segmento, presupuestos, indicadores y reportes. ",
          "Confirme únicamente si revisó la cuenta principal seleccionada."
        ),
        type = "warning", confirm_label = "Confirmar cambio"
      )
    })
    
    observeEvent(input$confirm_hijo, {
      pendiente <- isolate(pendiente_jerarquia())
      pendiente_jerarquia(NULL)
      if (!isTRUE(input$confirm_hijo) || !isTRUE(es_admin()) ||
          is.null(pendiente)) return()
      actual <- isolate(identidad_activa())
      if (is.null(actual) || as.character(actual$nit) != pendiente$nit ||
          as.character(actual$linneg_cod) != pendiente$linneg) return()
      tryCatch({
        guardar_registro(pendiente$payload, "Jerarquía guardada correctamente.")
      }, error = function(e) {
        racafeForms::racafeNotify(type = "error", duration = NULL,
                                title = paste("Error al guardar jerarquía:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    data_crm <- reactive({
      id_val <- identidad_activa()
      req(!is.null(id_val), length(id_val$nit) == 1L, !is.na(id_val$nit))
      refresh()
      CargarDatos("CRMNALCLIENTE") %>%
        mutate(FecProceso = as_datetime(FecProceso)) %>%
        filter(as.character(if (identificar_por_clinit) CLCliNit else CliNitPpal) ==
                 as.character(id_val$nit),
               LinNegCod == id_val$linneg_cod) %>%
        arrange(desc(FecProceso)) %>%
        slice(1)
    })
    
    observe({
      crm <- data_crm()
      id_val <- identidad_activa()
      req(!is.null(id_val))
      d <- dat()
      asesor_val <- if (nrow(crm) > 0) crm$Asesor[1] %||% "" else d$Asesor[1] %||% ""
      seg_val <- if (nrow(crm) > 0) crm$Segmento[1] %||% "" else d$Segmento[1] %||% ""
      exc_val <- if (nrow(crm) > 0) crm$Excluir[1] %||% "NO" else "NO"
      meses_val <- if (nrow(crm) > 0) crm$NumMesesRecuperar[1] %||% 3L else 3L
      
      racafeForms::racafeUpdateSelect(session, "frm_Asesor", selected = if (is.na(asesor_val) || asesor_val %in% c("Ninguna", "Ninguno")) "" else asesor_val)
      racafeForms::racafeUpdateSelect(session, "frm_Segmento", selected = if (is.na(seg_val) || seg_val %in% c("Ninguna", "Ninguno")) "" else seg_val)
      racafeForms::racafeUpdateSelect(session, "frm_Excluir", selected = exc_val)
      racafeForms::racafeUpdateNumeric(session, "frm_NumMesesRecuperar", value = meses_val)
    })
    
    construir_payload <- function() {
      id_val <- identidad_activa()
      crm <- data_crm()
      datos <- dat()
      
      ssppto <- if (nrow(crm) > 0) as.numeric(crm$SSPpto[1] %||% 0) else 0
      mnfccpto <- if (nrow(crm) > 0) as.numeric(crm$MNFCCPpto[1] %||% 0) else 0
      
      padre_actual <- if (nrow(crm) > 0 && !is.na(crm$CliNitPpal[1])) {
        crm$CliNitPpal[1]
      } else if (nrow(datos) > 0 && !is.na(datos$CliNitPpal[1])) {
        datos$CliNitPpal[1]
      } else {
        id_val$nit
      }
      
      data.frame(
        FecProceso = Sys.time(),
        Usr = usr(),
        LinNegCod = id_val$linneg_cod,
        CLCliNit = id_val$nit,
        CliNitPpal = padre_actual,
        Segmento = input$frm_Segmento %||% NA_character_,
        SSPpto = ssppto,
        MNFCCPpto = mnfccpto,
        Asesor = input$frm_Asesor %||% NA_character_,
        NumMesesRecuperar = input$frm_NumMesesRecuperar %||% 3L,
        Excluir = input$frm_Excluir %||% "NO",
        stringsAsFactors = FALSE
      )
    }
    
    # Guardado comercial ----
    observeEvent(input$btn_guardar, {
      if (!isTRUE(es_admin())) {
        racafeForms::racafeNotify(type = "warning", title = "Solo puede ser guardado por el administrador.",
                                session = shiny::getDefaultReactiveDomain())
        return()
      }
      req(identidad_activa())
      pendiente_comercial(NULL)
      payload <- construir_payload()
      payload$Segmento <- limpiar_seleccion(input$frm_Segmento)
      payload$Asesor <- limpiar_seleccion(input$frm_Asesor)
      payload$Excluir <- input$frm_Excluir %||% "NO"
      meses <- suppressWarnings(as.numeric(input$frm_NumMesesRecuperar))
      if (length(meses) != 1L || is.na(meses) || meses < 1) {
        racafeForms::racafeNotify(type = "error", title = "Meses para recuperar debe ser mayor que cero.",
                                session = shiny::getDefaultReactiveDomain())
        return()
      }
      payload$NumMesesRecuperar <- as.integer(meses)
      id_val <- identidad_activa()
      pendiente_comercial(list(payload = payload,
                               nit = as.character(id_val$nit),
                               linneg = as.character(id_val$linneg_cod)))
      racafeForms::racafeConfirm(
        session = session, id = "confirm_guardar", ns = ns,
        title = "Guardar información comercial",
        text = paste0(
          "Se actualizarán los datos comerciales del cliente: asesor, ",
          "segmento, exclusión y meses para recuperar. ",
          "Estos cambios pueden afectar su seguimiento comercial, ",
          "presupuestos, indicadores y reportes. ¿Desea continuar?"
        ),
        type = "question", confirm_label = "Guardar cambios"
      )
    })
    
    observeEvent(input$confirm_guardar, {
      pendiente <- isolate(pendiente_comercial())
      pendiente_comercial(NULL)
      if (!isTRUE(input$confirm_guardar) || !isTRUE(es_admin()) ||
          is.null(pendiente)) return()
      actual <- isolate(identidad_activa())
      if (is.null(actual) || as.character(actual$nit) != pendiente$nit ||
          as.character(actual$linneg_cod) != pendiente$linneg) return()
      tryCatch({
        guardar_registro(pendiente$payload, "Información comercial guardada correctamente.")
      }, error = function(e) {
        racafeForms::racafeNotify(type = "error", duration = NULL,
                                title = paste("Error al guardar:", conditionMessage(e)),
                                session = shiny::getDefaultReactiveDomain())
      })
    })
    
    return(list(refresh = refresh))
  })
}
