# Migrar Cartera ----
MigrarCarteraUI <- function(ns) {
  personas <- Choices()$personas
  personas <- as.character(personas %||% character())
  personas <- personas[!is.na(personas) & nzchar(personas)]

  racafeShiny::racafeOverlayLoader("migracion_loader", ns = ns, triggers = "confirm_migracion",
    racafeForms::racafeForm(
      size = "compact", variant = "plain",
      racafeForms::racafeFormRow(
        racafeForms::racafeInputSelect(
          "frm_AsesorOrigen", "Asesor origen", choices = personas,
          required = TRUE, ns = ns
        ),
        racafeForms::racafeInputSelect(
          "frm_AsesorDestino", "Asesor destino", choices = personas,
          required = TRUE, ns = ns
        )
      ),
      shiny::uiOutput(ns("resumen_migracion")),
      racafeForms::racafeFormFooter(
        racafeForms::racafeButton(
          "btn_migrar_asesor", "Migrar cartera", icon = "people-arrows",
          variant = "primary", size = "xxs", ns = ns
        ), align = "right"
      ),
      shiny::uiOutput(ns("aviso_migracion"))
    )
  )
}

# Migrar Cartera Server ----
MigrarCartera <- function(id, usr, es_admin, clientes_raw,
                         on_success = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pendiente <- shiny::reactiveVal(NULL)
    guardando <- shiny::reactiveVal(FALSE)

    # Funciones ----
    ultimo_registro <- function(base) {
      base %>%
        dplyr::mutate(FecProceso = as.POSIXct(FecProceso),
                      CLCliNit = as.character(CLCliNit),
                      LinNegCod = as.character(LinNegCod)) %>%
        dplyr::filter(!is.na(CLCliNit), nzchar(CLCliNit),
                      !is.na(LinNegCod), nzchar(LinNegCod)) %>%
        dplyr::group_by(CLCliNit, LinNegCod) %>%
        dplyr::arrange(dplyr::desc(FecProceso), .by_group = TRUE) %>%
        dplyr::slice_head(n = 1L) %>%
        dplyr::ungroup()
    }

    # Reactivos ----
    cartera_vigente <- shiny::reactive({
      ultimo_registro(clientes_raw())
    })

    resumen_migracion_r <- shiny::reactive({
      shiny::req(input$frm_AsesorOrigen, nzchar(input$frm_AsesorOrigen))
      cartera_vigente() %>%
        dplyr::filter(as.character(Asesor) == input$frm_AsesorOrigen)
    })

    # Outputs ----
    output$resumen_migracion <- shiny::renderUI({
      origen <- input$frm_AsesorOrigen
      destino <- input$frm_AsesorDestino
      if (is.null(origen) || !nzchar(origen)) return(NULL)
      n <- nrow(resumen_migracion_r())
      racafeForms::racafeInfoGrid(
        racafeForms::racafeInfoItem("CUENTAS Y LÍNEAS", format(
          n, big.mark = SEP_MILES, decimal.mark = SEP_DECIMAL
        )),
        racafeForms::racafeInfoItem("ASESOR ORIGEN", origen),
        racafeForms::racafeInfoItem("ASESOR DESTINO", destino %||% "Pendiente"),
        columns = 2
      )
    })

    output$aviso_migracion <- shiny::renderUI({
      if (isTRUE(es_admin())) return(NULL)
      racafeForms::racafeFormMessage(
        "Solo puede ser guardado por el administrador.", type = "muted"
      )
    })

    shiny::observe({
      racafeForms::racafeUpdateEnabled(
        session, "btn_migrar_asesor", enabled = isTRUE(es_admin())
      )
    })

    shiny::observeEvent(list(input$frm_AsesorOrigen, input$frm_AsesorDestino), {
      pendiente(NULL)
    }, ignoreInit = TRUE)

    # Migración ----
    shiny::observeEvent(input$btn_migrar_asesor, {
      if (!isTRUE(es_admin())) {
        racafeForms::racafeNotify(session, "Acceso restringido",
          "Solo el administrador puede migrar cartera.", type = "error")
        return(invisible(NULL))
      }
      origen <- input$frm_AsesorOrigen
      destino <- input$frm_AsesorDestino
      if (is.null(origen) || !nzchar(origen) ||
          is.null(destino) || !nzchar(destino)) {
        racafeForms::racafeShowValidation(session, racafeForms::racafeValidateForm(
          input, list(
            frm_AsesorOrigen = list(label = "Asesor origen", required = TRUE, kind = "select"),
            frm_AsesorDestino = list(label = "Asesor destino", required = TRUE, kind = "select")
          ), ns = session$ns
        ), mode = "both")
        return(invisible(NULL))
      }
      if (identical(origen, destino)) {
        racafeForms::racafeNotify(session, "Selección inválida",
          "Origen y destino deben ser diferentes.", type = "warning")
        return(invisible(NULL))
      }
      base <- shiny::isolate(resumen_migracion_r())
      if (!nrow(base)) {
        racafeForms::racafeNotify(session, "Sin cartera",
          "El asesor origen no tiene cuentas vigentes.", type = "warning")
        return(invisible(NULL))
      }
      pendiente(list(origen = origen, destino = destino, base = base))
      racafeForms::racafeConfirm(
        session = session, id = "confirm_migracion", ns = ns,
        title = "Confirmar migración de cartera",
        text = paste0(
          "Se transferirán ", nrow(base), " cuentas y líneas de negocio de ",
          origen, " a ", destino, ". El nuevo asesor asumirá su seguimiento. ",
          "El cambio puede afectar presupuestos, indicadores y reportes ",
          "comerciales. Se conservarán las cuentas principales y los demás datos."
        ),
        type = "warning", confirm_label = "Confirmar migración"
      )
    })

    shiny::observeEvent(input$confirm_migracion, {
      propuesta <- shiny::isolate(pendiente())
      pendiente(NULL)
      if (!isTRUE(input$confirm_migracion) || is.null(propuesta) ||
          isTRUE(shiny::isolate(guardando()))) return(invisible(NULL))
      if (!isTRUE(es_admin())) return(invisible(NULL))

      guardando(TRUE)
      on.exit(guardando(FALSE), add = TRUE)
      racafeShiny::racafeUpdateOverlayLoader(session, "migracion_loader", TRUE)
      on.exit(racafeShiny::racafeUpdateOverlayLoader(session, "migracion_loader", FALSE), add = TRUE)

      tryCatch({
        actual <- ultimo_registro(racafeBD::CargarDatos("CRMNALCLIENTE"))
        base <- actual %>%
          dplyr::filter(as.character(Asesor) == propuesta$origen)
        claves <- c("CLCliNit", "LinNegCod")
        claves_antes <- propuesta$base %>%
          dplyr::select(dplyr::all_of(claves)) %>% dplyr::distinct()
        claves_ahora <- base %>%
          dplyr::select(dplyr::all_of(claves)) %>% dplyr::distinct()
        cambios <- nrow(dplyr::anti_join(claves_antes, claves_ahora,
                                         by = claves)) +
          nrow(dplyr::anti_join(claves_ahora, claves_antes, by = claves))
        if (cambios > 0L) {
          clientes_raw(racafeBD::CargarDatos("CRMNALCLIENTE"))
          racafeForms::racafeNotify(session, "Cartera modificada",
            "La cartera cambió desde la confirmación. Revise y confirme otra vez.",
            type = "warning", duration = 8)
          return(invisible(NULL))
        }

        payload <- base %>%
          dplyr::transmute(
            FecProceso = Sys.time(), Usr = usr(), LinNegCod, CLCliNit,
            CliNitPpal, Segmento, SSPpto, MNFCCPpto,
            Asesor = propuesta$destino, NumMesesRecuperar, Excluir
          )
        if (!nrow(payload)) stop("No hay cuentas disponibles para migrar.")
        racafeBD::AgregarDatos(payload, "CRMNALCLIENTE")
        clientes_raw(racafeBD::CargarDatos("CRMNALCLIENTE"))
        if (is.function(on_success)) on_success()
        shiny::removeModal()
        racafeForms::racafeNotify(session, "Cartera migrada",
          paste(nrow(payload), "cuentas y líneas de negocio migradas."),
          type = "success")
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No se pudo migrar",
          conditionMessage(e), type = "error", duration = 8)
      })
    })
  })
}
