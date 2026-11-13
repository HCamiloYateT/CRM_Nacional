# Escalas del mismo catálogo persistido; las etapas mantienen sus reglas del embudo ----
.esc_dimensiones <- c(etapa = "Etapas del ciclo de vida", estado = "Estados comerciales",
  linneg = "Líneas de negocio", segmento = "Segmentos comerciales", rfm = "Segmentos RFM",
  origen = "Origen de las entidades", categoria = "Categorías de productos", producto = "Productos")
.esc_opciones <- function(catalogos) {
  fijo <- list(etapa = ETAPAS_EMBUDO, estado = c("ACTIVO", "INACTIVO", "NUEVO"),
    rfm = c("LEALES RECIENTES", "LEALES EN RIESGO", "COMPRA ÚNICA OBSERVADA", "INACTIVOS RELATIVOS",
      "EN DESARROLLO"))
  relaciones <- .admon_choices_relaciones(catalogos)
  dplyr::bind_rows(lapply(names(.esc_dimensiones), function(tipo) {
    if (tipo %in% c("categoria", "producto")) {
      filas <- relaciones[relaciones$catalogo == tipo, c("catalogo", "padre", "padre2", "opcion")]
    } else {
      valores <- if (tipo %in% names(fijo)) fijo[[tipo]] else .admon_choices_valores(catalogos, tipo)
      filas <- data.frame(catalogo = rep(tipo, length(valores)), padre = rep("", length(valores)),
        padre2 = rep("", length(valores)), opcion = valores)
    }
    unique(filas)
  }))
}
EscalasComercialesUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(variant = "plain", shiny::uiOutput(ns("escalas")),
    racafeForms::racafeButtonSave("guardar", "Guardar colores", ns = ns))
}
EscalasComerciales <- function(id, es_admin) {
  shiny::moduleServer(id, function(input, output, session) {
    base <- shiny::reactiveVal(NULL)
    borrador <- shiny::reactiveVal(NULL)
    pendiente <- shiny::reactiveVal(NULL)
    shiny::observeEvent(es_admin(), {
      shiny::req(isTRUE(es_admin()))
      x <- .admon_choices_leer(); base(x); borrador(x)
    }, once = TRUE)
    opciones <- shiny::reactive({ shiny::req(isTRUE(es_admin()), base()); .esc_opciones(base()) })
    output$escalas <- .crm_render_ui({
      x <- opciones()
      shiny::tagList(lapply(names(.esc_dimensiones), function(tipo) {
        filas <- which(x$catalogo == tipo)
        .crm_disclosure(.esc_dimensiones[[tipo]],
          if (tipo == "etapa") .crm_nota(c(
            "Código, nombre y orden proceden de las etapas vigentes del embudo.",
            "Su disponibilidad depende de las reglas de transición; aquí se modifica únicamente la presentación.")),
          if (!length(filas)) .crm_nota("No hay opciones disponibles para esta dimensión."),
          lapply(filas, function(i) {
            f <- x[i, ]; color <- unname(.crm_colores(tipo, f$opcion, f$padre, f$padre2))
            configurado <- .admon_choices_colores(shiny::isolate(borrador()))
            llave <- paste(f$catalogo, f$padre, f$padre2, f$opcion, sep = "|")
            j <- match(llave, do.call(paste, c(configurado[1:4], sep = "|")))
            if (!is.na(j)) color <- configurado$color[j]
            shiny::tags$div(style = "display:flex;gap:10px;align-items:center;margin-bottom:8px;",
              shiny::tags$input(type = "color", value = color, `aria-label` = paste("Color de", f$opcion),
                onchange = sprintf("Shiny.setInputValue('%s',{fila:%d,color:this.value,nonce:Date.now()})",
                  session$ns("color"), i)),
              shiny::tags$span(paste(Filter(nzchar, as.list(f[c("padre", "padre2", "opcion")])),
                collapse = " → ")),
              if (tipo == "etapa") shiny::tags$small(paste("CÓDIGO:", f$opcion, "· ORDEN:",
                match(f$opcion, ETAPAS_EMBUDO), "· DISPONIBLE ·", switch(f$opcion,
                  CONTACTO = "Entidad registrada sin compra histórica confirmada.",
                  LEAD = "Entidad calificada con información comercial.",
                  PROSPECTO = "Entidad con relación comercial mediante aliado.",
                  CLIENTE = "Entidad con facturación histórica documentada.",
                  DESCARTADO = "Entidad descartada con motivo y etapa de procedencia."))))
          }))
      }))
    })
    shiny::observeEvent(input$color, {
      shiny::req(isTRUE(es_admin()))
      i <- input$color$fila; color <- input$color$color; x <- opciones()
      shiny::req(length(i) == 1L, is.finite(i), i == trunc(i), i >= 1, i <= nrow(x))
      if (length(color) != 1L || !grepl("^#[[:xdigit:]]{6}$", color)) {
        shiny::showNotification("Color hexadecimal inválido.", type = "error"); return()
      }
      f <- x[i, ]; nuevo <- borrador(); colores <- .admon_choices_colores(nuevo)
      iguales <- colores$catalogo == f$catalogo & colores$padre == f$padre &
        colores$padre2 == f$padre2 & colores$opcion == f$opcion
      attr(nuevo, "colores") <- rbind(colores[!iguales, ], cbind(f, color = toupper(color)))
      borrador(nuevo)
    })
    shiny::observeEvent(input$guardar, {
      shiny::req(isTRUE(es_admin()), !identical(base(), borrador()))
      pendiente(list(base = base(), nuevo = borrador()))
      racafeShiny::MostrarModalConfirmacion(ns = session$ns, titulo = "Confirmar colores comerciales",
        texto = "Los colores se reutilizarán en todos los tableros. Las reglas de transición no cambian.",
        id_cancelar = "cancelar", id_confirmar = "confirmar")
    })
    shiny::observeEvent(input$cancelar, { pendiente(NULL); shiny::removeModal() })
    shiny::observeEvent(input$confirmar, {
      shiny::req(isTRUE(es_admin()), pendiente())
      x <- pendiente(); pendiente(NULL); shiny::removeModal()
      if (!identical(x$nuevo, borrador())) {
        shiny::showNotification("Los colores cambiaron. Revise y confirme de nuevo.", type = "warning"); return()
      }
      tryCatch({
        .admon_choices_guardar_cambios(x$nuevo, x$base)
        base(x$nuevo)
        session$reload()
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })
  })
}
