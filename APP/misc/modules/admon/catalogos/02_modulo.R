# Modulo ----
AdministrarCatalogosUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(
    size = "compact", variant = "plain",
    racafeForms::racafeFormGroup(
      shiny::uiOutput(ns("ADM_CatalogoUI"))
    ),
    shiny::uiOutput(ns("ADM_Tablas")),
    racafeForms::racafeFormFooter(
      racafeForms::racafeButton(
        "ADM_Guardar", "Guardar cambios", icon = "floppy-disk",
        variant = "primary", size = "xs", ns = ns
      ), align = "right"
    ),
    shiny::uiOutput(ns("ADM_Estado")),
    shiny::uiOutput(ns("ADM_Error"))
  )
}

AdministrarCatalogos <- function(id, usuario, grupo, es_admin) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Constantes ----
    etiquetas <- c(
      personas = "Personas", segmento = "Segmentos",
      linneg = "Líneas de negocio → Categorías → Productos",
      origen = "Orígenes → Detalles del origen",
      raz_descarte = "Razones de descarte", formapago = "Formas de pago"
    )
    excluidos <- c("categoria", "producto", "detalle_origen", "paises", "deptos",
                   "direccion_via", "direccion_cardinalidad")
    
    # Reactivos ----
    base_rv <- shiny::reactiveVal(NULL)
    borrador_rv <- shiny::reactiveVal(NULL)
    error_rv <- shiny::reactiveVal(NULL)
    eliminacion_rv <- shiny::reactiveVal(NULL)
    nuevo_nivel_rv <- shiny::reactiveVal(NULL)
    nueva_opcion_rv <- shiny::reactiveVal(NULL)
    seleccionado_1_rv <- shiny::reactiveVal("")
    seleccionado_2_rv <- shiny::reactiveVal("")
    actualizaciones_rv <- shiny::reactiveVal(0L)
    
    catalogos_r <- shiny::reactive({
      shiny::req(isTRUE(es_admin()), borrador_rv())
      borrador_rv()
    })
    
    catalogo_r <- shiny::reactive({
      datos <- catalogos_r()
      disponibles <- setdiff(names(datos), excluidos)
      shiny::req(length(disponibles) > 0L)
      elegido <- input$ADM_Catalogo
      if (length(elegido) == 1L && elegido %in% disponibles) elegido else disponibles[[1]]
    })
    
    jerarquia_r <- shiny::reactive({
      switch(catalogo_r(),
             origen = c("origen", "detalle_origen"),
             linneg = c("linneg", "categoria", "producto"),
             catalogo_r()
      )
    })
    
    valores_1_r <- shiny::reactive({
      .admon_choices_valores(catalogos_r(), jerarquia_r()[[1]])
    })
    
    padre_1_r <- shiny::reactive({
      opciones <- valores_1_r()
      actual <- seleccionado_1_rv()
      if (length(actual) == 1L && actual %in% opciones) actual else ""
    })
    
    valores_2_r <- shiny::reactive({
      if (length(jerarquia_r()) < 2L) return(character(0))
      .admon_choices_valores(catalogos_r(), jerarquia_r()[[2]], padre_1_r())
    })
    
    padre_2_r <- shiny::reactive({
      opciones <- valores_2_r()
      actual <- seleccionado_2_rv()
      if (length(actual) == 1L && actual %in% opciones) actual else ""
    })
    
    valores_3_r <- shiny::reactive({
      if (length(jerarquia_r()) < 3L) return(character(0))
      .admon_choices_valores(catalogos_r(), jerarquia_r()[[3]],
                             padre_1_r(), padre_2_r())
    })
    
    pendientes_r <- shiny::reactive({
      !identical(borrador_rv(), base_rv())
    })
    
    # Funciones ----
    .contexto <- function(nivel) {
      tipos <- shiny::isolate(jerarquia_r())
      if (nivel > length(tipos)) stop("Nivel de catálogo no disponible.", call. = FALSE)
      if (nivel >= 2L && !nzchar(shiny::isolate(padre_1_r()))) {
        stop("Seleccione primero una opción del nivel anterior.", call. = FALSE)
      }
      if (nivel >= 3L && !nzchar(shiny::isolate(padre_2_r()))) {
        stop("Seleccione primero una categoría.", call. = FALSE)
      }
      list(
        catalogo = tipos[[nivel]],
        padre = if (nivel >= 2L) shiny::isolate(padre_1_r()) else "",
        padre2 = if (nivel >= 3L) shiny::isolate(padre_2_r()) else ""
      )
    }
    
    .valores_nivel <- function(nivel) {
      switch(as.character(nivel),
             `1` = shiny::isolate(valores_1_r()),
             `2` = shiny::isolate(valores_2_r()),
             `3` = shiny::isolate(valores_3_r())
      )
    }
    
    .cambiar <- function(funcion) {
      shiny::req(isTRUE(es_admin()), usuario())
      tryCatch({
        borrador_rv(funcion(shiny::isolate(borrador_rv())))
        error_rv(NULL)
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No fue posible modificar",
                                  conditionMessage(e), type = "error")
      })
    }
    
    .recargar <- function() {


      tryCatch({
        .admon_choices_migrar_relaciones()
        datos <- .admon_choices_leer()
        base_rv(datos)
        borrador_rv(datos)
        error_rv(NULL)
      }, error = function(e) error_rv(conditionMessage(e)))
    }
    
    .titulo_nivel <- function(nivel) {
      tipos <- jerarquia_r()
      if (nivel > length(tipos)) return(NULL)
      tipo <- tipos[[nivel]]
      switch(tipo,
             linneg = "Línea de negocio", categoria = "Categoría",
             producto = "Producto", origen = "Origen",
             detalle_origen = "Detalle del origen",
             if (tipo %in% names(etiquetas)) etiquetas[[tipo]] else
               tools::toTitleCase(gsub("_", " ", tipo))
      )
    }
    
    .accion_js <- function(nivel, indice, accion) {
      sprintf(
        paste0("event.stopPropagation();Shiny.setInputValue('%s',",
               "{nivel:%d,indice:%d,accion:'%s',nonce:Date.now()},",
               "{priority:'event'});"),
        ns("ADM_AccionFila"), nivel, indice, accion
      )
    }
    
    .seleccion_js <- function(nivel, valor) {
      sprintf(
        paste0("event.stopPropagation();Shiny.setInputValue('%s',",
               "{nivel:%d,valor:%s,nonce:Date.now()},",
               "{priority:'event'});"),
        ns("ADM_SeleccionFila"), nivel,
        as.character(jsonlite::toJSON(valor, auto_unbox = TRUE))
      )
    }
    
    .boton_fila <- function(nivel, indice, accion, icono, titulo,
                            variante = "secondary") {
      identificador <- paste0("ADM_", nivel, "_", indice, "_", accion)
      shiny::tagAppendAttributes(
        racafeForms::racafeButton(
          identificador, label = NULL, icon = icono,
          variant = variante, size = "xxs", title = titulo, ns = ns
        ), onclick = .accion_js(nivel, indice, accion),
        style = paste0("height:19px!important;min-height:19px!important;",
                       "min-width:19px!important;padding:0 3px!important;",
                       "font-size:10px!important;line-height:17px!important;",
                       "margin:0!important;")
      )
    }
    
    .acciones_fila <- function(nivel, indice, total) {
      shiny::tags$span(
        class = "d-inline-flex align-items-center",
        style = "gap:3px;white-space:nowrap;",
        if (indice > 1L) .boton_fila(nivel, indice, "subir", "arrow-up", "Subir"),
        if (indice < total) .boton_fila(nivel, indice, "bajar", "arrow-down", "Bajar")
      )
    }
    
    .tabla_gt <- function(nivel) {
      valores <- switch(as.character(nivel),
                        `1` = valores_1_r(), `2` = valores_2_r(), `3` = valores_3_r()
      )
      total <- length(valores)
      tabla <- data.frame(
        Numero = seq_len(total), Opcion = valores,
        Orden = rep("", total), Eliminar = rep("", total),
        stringsAsFactors = FALSE
      )
      gt::gt(tabla) |>
        gt::cols_label(Numero = "#", Opcion = "OPCIÓN", Orden = "", Eliminar = "") |>
        gt::cols_width(Numero ~ gt::px(28), Orden ~ gt::px(65), Eliminar ~ gt::px(25)) |>
        gt::cols_align(align = "center", columns = "Numero") |>
        gt::text_transform(
          locations = gt::cells_body(columns = "Opcion"),
          fn = function(x) {
            lapply(seq_along(x), function(i) {
              valor <- valores[[i]]
              seleccionado <- if (nivel == 1L) padre_1_r() else if (nivel == 2L) padre_2_r() else ""
              enlace <- shiny::tags$a(
                href = "javascript:void(0)",
                class = if (identical(valor, seleccionado))
                  "font-weight-bold text-danger" else "text-body",
                onclick = .seleccion_js(nivel, valor),
                valor
              )
              gt::html(as.character(enlace))
            })
          }
        ) |>
        gt::text_transform(
          locations = gt::cells_body(columns = "Orden"),
          fn = function(x) {
            lapply(seq_along(x), function(i) {
              gt::html(as.character(.acciones_fila(nivel, i, total)))
            })
          }
        ) |>
        gt::text_transform(
          locations = gt::cells_body(columns = "Eliminar"),
          fn = function(x) {
            lapply(seq_along(x), function(i) {
              gt::html(as.character(.boton_fila(
                nivel, i, "eliminar", "trash", "Eliminar", "danger"
              )))
            })
          }
        ) |>
        racafeShiny::gt_minimal_style() |>
        gt::tab_options(table.width = gt::pct(100),
                        container.width = gt::pct(100),
                        table.font.size = gt::px(10),
                        data_row.padding = gt::px(1),
                        column_labels.padding = gt::px(2),
                        table.border.top.style = "none",
                        table.border.bottom.style = "none",
                        table_body.hlines.style = "none",
                        table_body.vlines.style = "none",
                        column_labels.border.top.style = "none",
                        column_labels.border.bottom.style = "solid",
                        row.striping.include_table_body = FALSE,
                        table.background.color = "transparent",
                        column_labels.background.color = "transparent")
    }
    
    .footer_nivel <- function(nivel) {
      tipos <- jerarquia_r()
      if (nivel > length(tipos)) return(NULL)
      if (length(tipos) == 1L) return("Catálogo independiente, sin jerarquía")
      if (nivel == 1L) {
        if (!nzchar(padre_1_r())) {
          return(paste("Seleccione", if (tipos[[1]] == "linneg")
            "una línea de negocio" else "un origen",
            "para ver sus dependientes."))
        }
        return(paste("Selección:", padre_1_r()))
      }
      if (nivel == 2L) {
        if (length(tipos) == 3L && !nzchar(padre_2_r())) {
          return("Seleccione una categoría para ver sus productos.")
        }
        return(paste("Depende de:", padre_1_r()))
      }
      if (nivel == 3L) return(paste("Depende de:", padre_1_r(), "/", padre_2_r()))
      NULL
    }
    
    .resumen_cambios <- function(base, nuevo) {
      claves <- unique(c(names(base), names(nuevo)))
      claves <- setdiff(claves, excluidos)
      agregados <- 0L
      eliminados <- 0L
      reorganizados <- 0L
      afectados <- character(0)
      
      comparar <- function(a, b, etiqueta) {
        nuevos <- setdiff(b, a)
        quitados <- setdiff(a, b)
        comunes_a <- a[a %in% b]
        comunes_b <- b[b %in% a]
        cambios_orden <- !identical(comunes_a, comunes_b)
        agregados <<- agregados + length(nuevos)
        eliminados <<- eliminados + length(quitados)
        reorganizados <<- reorganizados + as.integer(cambios_orden)
        if (length(nuevos) || length(quitados) || cambios_orden) {
          afectados <<- c(afectados, etiqueta)
        }
      }
      
      for (clave in claves) {
        if (clave %in% c("categoria", "producto", "detalle_origen")) next
        comparar(base[[clave]] %||% character(), nuevo[[clave]] %||% character(),
                 if (clave %in% names(etiquetas)) etiquetas[[clave]] else clave)
      }
      
      anterior <- .admon_choices_relaciones(base)
      actual <- .admon_choices_relaciones(nuevo)
      grupos <- unique(rbind(anterior[c("catalogo", "padre", "padre2")],
                             actual[c("catalogo", "padre", "padre2")]))
      if (nrow(grupos)) {
        for (i in seq_len(nrow(grupos))) {
          g <- grupos[i, , drop = FALSE]
          filtro <- function(datos) datos$catalogo == g$catalogo &
            datos$padre == g$padre & datos$padre2 == g$padre2
          comparar(anterior$opcion[filtro(anterior)], actual$opcion[filtro(actual)],
                   paste(g$catalogo, g$padre, g$padre2, sep = " / "))
        }
      }
      list(agregados = agregados, eliminados = eliminados,
           reorganizados = reorganizados, afectados = unique(afectados))
    }
    
    # Formulario ----
    shiny::observeEvent(TRUE, {
      shiny::req(isTRUE(es_admin()))
      .recargar()
    }, once = TRUE)
    
    output$ADM_CatalogoUI <- shiny::renderUI({
      shiny::req(isTRUE(es_admin()))
      datos <- base_rv()
      shiny::req(is.list(datos))
      claves <- setdiff(names(datos), excluidos)
      if (!length(claves)) {
        return(racafeForms::racafeFormMessage(
          "El RDS no contiene catálogos disponibles.", type = "warning", size = "compact"
        ))
      }
      rotulos <- vapply(claves, function(clave) {
        if (clave %in% names(etiquetas)) etiquetas[[clave]] else
          tools::toTitleCase(gsub("_", " ", clave))
      }, character(1))
      .crm_select(
        "ADM_Catalogo", "Catálogo", choices = stats::setNames(claves, rotulos),
        selected = claves[[1]], multiple = FALSE, empty = FALSE, ns = ns
      )
    })
    
    shiny::observeEvent(input$ADM_Catalogo, {
      seleccionado_1_rv("")
      seleccionado_2_rv("")
    })
    
    # Tablas ----
    for (nivel in seq_len(3L)) {
      local({
        i <- nivel
        output[[paste0("ADM_Tabla", i)]] <- gt::render_gt({
          shiny::req(isTRUE(es_admin()), i <= length(jerarquia_r()))
          if (i >= 2L) shiny::req(nzchar(padre_1_r()))
          if (i >= 3L) shiny::req(nzchar(padre_2_r()))
          .tabla_gt(i)
        })
      })
    }
    
    shiny::observeEvent(input$ADM_SeleccionFila, {
      shiny::req(isTRUE(es_admin()))
      evento <- input$ADM_SeleccionFila
      nivel <- as.integer(evento$nivel)
      valor <- as.character(evento$valor)
      if (length(valor) != 1L || length(nivel) != 1L || is.na(nivel) ||
          !nivel %in% 1:2 || nivel > length(jerarquia_r())) return()
      if (!valor %in% .valores_nivel(nivel)) return()
      if (nivel == 1L) {
        if (!identical(seleccionado_1_rv(), valor)) {
          seleccionado_1_rv(valor)
          seleccionado_2_rv("")
        }
      } else {
        seleccionado_2_rv(valor)
      }
    })
    
    output$ADM_Tablas <- shiny::renderUI({
      shiny::req(isTRUE(es_admin()), catalogo_r())
      cantidad <- length(jerarquia_r())
      visibles <- 1L
      if (cantidad >= 2L && nzchar(padre_1_r())) visibles <- c(visibles, 2L)
      if (cantidad >= 3L && nzchar(padre_1_r()) && nzchar(padre_2_r())) {
        visibles <- c(visibles, 3L)
      }
      shiny::tags$div(
        class = "row",
        lapply(visibles, function(i) {
          shiny::tags$div(
            class = "col-12 col-md-4 mb-3",
            shiny::tags$div(class = "font-weight-bold mb-1", .titulo_nivel(i)),
            gt::gt_output(ns(paste0("ADM_Tabla", i))),
            shiny::tags$div(
              class = "small text-muted mt-1",
              .footer_nivel(i)
            ),
            racafeForms::racafeFormFooter(
              racafeForms::racafeButton(
                paste0("ADM_Nuevo", i), "Nueva opción", icon = "plus",
                variant = "secondary", size = "xs", ns = ns
              ), align = "right"
            )
          )
        })
      )
    })
    
    output$ADM_Estado <- shiny::renderUI({
      if (isTRUE(pendientes_r())) racafeForms::racafeFormMessage(
        "Hay cambios pendientes de guardar.", type = "warning", size = "compact"
      )
    })
    output$ADM_Error <- shiny::renderUI({
      error <- error_rv()
      if (is.null(error) || !nzchar(error)) return(NULL)
      racafeForms::racafeFormMessage(error, type = "error", size = "compact")
    })
    
    # Edicion ----
    shiny::observeEvent(input$ADM_AccionFila, {
      shiny::req(isTRUE(es_admin()))
      evento <- input$ADM_AccionFila
      nivel <- as.integer(evento$nivel)
      indice <- as.integer(evento$indice)
      if (length(nivel) != 1L || is.na(nivel) || !nivel %in% 1:3) return()
      valores <- .valores_nivel(nivel)
      if (length(indice) != 1L || is.na(indice) || indice < 1L ||
          indice > length(valores)) return()
      contexto <- .contexto(nivel)
      if (evento$accion %in% c("subir", "bajar")) {
        .cambiar(function(datos) .admon_choices_mover_local(
          datos, contexto$catalogo, indice, evento$accion,
          contexto$padre, contexto$padre2
        ))
      } else if (identical(evento$accion, "eliminar")) {
        eliminacion_rv(list(contexto = contexto, valor = valores[[indice]],
                            indice = indice))
        shiny::showModal(racafeForms::racafeModal(
          title = "Confirmar eliminación", size = "subventana5",
          shiny::tags$p("La opción se retirará del catálogo al guardar los cambios."),
          racafeForms::racafeInfoGrid(
            racafeForms::racafeInfoItem("CATÁLOGO", .titulo_nivel(nivel)),
            racafeForms::racafeInfoItem("OPCIÓN", valores[[indice]]),
            if (nzchar(contexto$padre)) racafeForms::racafeInfoItem(
              "DEPENDE DE", paste(c(contexto$padre, contexto$padre2)[
                nzchar(c(contexto$padre, contexto$padre2))], collapse = " / ")
            ), columns = 2
          ),
          footer = racafeForms::racafeModalFooter(
            racafeForms::racafeButtonCancel(
              "ADM_CancelarEliminar", "Cancelar", ns = ns, size = "xs"
            ),
            racafeForms::racafeButtonDelete(
              "ADM_ConfirmarEliminar", "Confirmar eliminación", ns = ns, size = "xs"
            )
          )
        ))
      }
    })
    
    shiny::observeEvent(input$ADM_CancelarEliminar, {
      eliminacion_rv(NULL)
      shiny::removeModal()
    })
    
    shiny::observeEvent(input$ADM_ConfirmarEliminar, {
      pendiente <- shiny::isolate(eliminacion_rv())
      shiny::req(isTRUE(es_admin()), !is.null(pendiente))
      eliminacion_rv(NULL)
      shiny::removeModal()
      .cambiar(function(datos) {
        c <- pendiente$contexto
        valores <- .admon_choices_valores(datos, c$catalogo, c$padre, c$padre2)
        if (length(valores) < pendiente$indice ||
            !identical(valores[[pendiente$indice]], pendiente$valor)) {
          stop("La opción cambió. Repita la selección.", call. = FALSE)
        }
        .admon_choices_eliminar_local(datos, c$catalogo, pendiente$indice,
                                      c$padre, c$padre2)
      })
    })
    
    for (nivel in seq_len(3L)) {
      local({
        i <- nivel
        shiny::observeEvent(input[[paste0("ADM_Nuevo", i)]], {
          shiny::req(isTRUE(es_admin()), i <= length(jerarquia_r()))
          nuevo_nivel_rv(i)
          shiny::showModal(racafeForms::racafeModal(
            title = paste("Nueva opción:", .titulo_nivel(i)), size = "subventana5",
            racafeForms::racafeInputText(
              "ADM_NuevoValor", "Nueva opción", required = TRUE,
              placeholder = "Escriba una opción", ns = ns
            ),
            footer = racafeForms::racafeModalFooter(
              racafeForms::racafeButtonCancel(
                "ADM_CancelarNuevo", "Cancelar", ns = ns, size = "xs"
              ),
              racafeForms::racafeButton(
                "ADM_ConfirmarNuevo", "Continuar", icon = "arrow-right",
                variant = "primary", size = "xs", ns = ns
              )
            )
          ))
        })
      })
    }
    
    shiny::observeEvent(input$ADM_CancelarNuevo, {
      nueva_opcion_rv(NULL)
      nuevo_nivel_rv(NULL)
      shiny::removeModal()
    })
    
    shiny::observeEvent(input$ADM_ConfirmarNuevo, {
      nivel <- shiny::isolate(nuevo_nivel_rv())
      shiny::req(isTRUE(es_admin()), !is.null(nivel))
      contexto <- .contexto(nivel)
      valor <- trimws(as.character(input$ADM_NuevoValor %||% ""))
      tryCatch({
        if (!nzchar(valor)) stop("Ingrese el nombre de la nueva opción.", call. = FALSE)
        .admon_choices_agregar_local(
          shiny::isolate(borrador_rv()), contexto$catalogo,
          valor, contexto$padre, contexto$padre2
        )
        nueva_opcion_rv(list(contexto = contexto, valor = valor, nivel = nivel))
        shiny::showModal(racafeForms::racafeModal(
          title = "Confirmar nueva opción", size = "subventana5",
          shiny::tags$p("Revise la información antes de añadirla al catálogo."),
          racafeForms::racafeInfoGrid(
            racafeForms::racafeInfoItem("CATÁLOGO", .titulo_nivel(nivel)),
            racafeForms::racafeInfoItem("OPCIÓN", valor),
            if (nzchar(contexto$padre)) racafeForms::racafeInfoItem(
              "DEPENDE DE", paste(c(contexto$padre, contexto$padre2)[
                nzchar(c(contexto$padre, contexto$padre2))], collapse = " / ")
            ), columns = 2
          ),
          footer = racafeForms::racafeModalFooter(
            racafeForms::racafeButtonCancel("ADM_CancelarConfirmarNuevo", "Cancelar",
                                            ns = ns, size = "xs"),
            racafeForms::racafeButton("ADM_AceptarNuevo", "Confirmar opción", icon = "plus",
                                      variant = "primary", size = "xs", ns = ns)
          )
        ))
      }, error = function(e) {
        racafeForms::racafeNotify(session, "Opción no añadida",
                                  conditionMessage(e), type = "error")
      })
    })
    
    shiny::observeEvent(input$ADM_CancelarConfirmarNuevo, {
      nueva_opcion_rv(NULL)
      nuevo_nivel_rv(NULL)
      shiny::removeModal()
    })
    
    shiny::observeEvent(input$ADM_AceptarNuevo, {
      pendiente <- shiny::isolate(nueva_opcion_rv())
      shiny::req(isTRUE(es_admin()), !is.null(pendiente))
      nueva_opcion_rv(NULL)
      nuevo_nivel_rv(NULL)
      .cambiar(function(datos) .admon_choices_agregar_local(
        datos, pendiente$contexto$catalogo, pendiente$valor,
        pendiente$contexto$padre, pendiente$contexto$padre2
      ))
      shiny::removeModal()
    })
    
    # Guardar ----
    confirmacion_guardado_rv <- shiny::reactiveVal(FALSE)
    
    shiny::observeEvent(input$ADM_Guardar, {
      shiny::req(isTRUE(es_admin()), usuario(), borrador_rv(), base_rv())
      if (!isTRUE(pendientes_r())) {
        racafeForms::racafeNotify(session, "Sin cambios",
                                  "No hay modificaciones pendientes.", type = "info")
        return(invisible(NULL))
      }
      resumen <- .resumen_cambios(shiny::isolate(base_rv()),
                                  shiny::isolate(borrador_rv()))
      confirmacion_guardado_rv(TRUE)
      shiny::showModal(racafeForms::racafeModal(
        title = "Confirmar actualización de catálogos", size = "subventana3",
        shiny::tags$p("Revise las modificaciones antes de guardarlas."),
        racafeForms::racafeInfoGrid(
          racafeForms::racafeInfoItem("OPCIONES NUEVAS", as.character(resumen$agregados)),
          racafeForms::racafeInfoItem("OPCIONES ELIMINADAS", as.character(resumen$eliminados)),
          racafeForms::racafeInfoItem("LISTAS REORDENADAS", as.character(resumen$reorganizados)),
          columns = 3
        ),
        if (length(resumen$afectados)) shiny::tags$p(
          shiny::tags$strong("Catálogos afectados: "),
          paste(resumen$afectados, collapse = "; ")
        ),
        footer = racafeForms::racafeModalFooter(
          racafeForms::racafeButtonCancel("ADM_CancelarGuardar", "Cancelar",
                                          ns = ns, size = "xs"),
          racafeForms::racafeButtonSave("ADM_ConfirmarGuardar", "Guardar cambios",
                                        ns = ns, size = "xs")
        )
      ))
    })
    
    shiny::observeEvent(input$ADM_CancelarGuardar, {
      confirmacion_guardado_rv(FALSE)
      shiny::removeModal()
    })
    
    shiny::observeEvent(input$ADM_ConfirmarGuardar, {
      shiny::req(isTRUE(es_admin()), isTRUE(confirmacion_guardado_rv()))
      confirmacion_guardado_rv(FALSE)

      shiny::removeModal()

      tryCatch({
        .admon_choices_guardar_cambios(
          shiny::isolate(borrador_rv()), shiny::isolate(base_rv())
        )
        base_rv(shiny::isolate(borrador_rv()))
        actualizaciones_rv(shiny::isolate(actualizaciones_rv()) + 1L)
        racafeForms::racafeNotify(session, "Catálogos actualizados",
                                  "Los cambios se guardaron correctamente.",
                                  type = "success", duration = 4)
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No fue posible guardar",
                                  conditionMessage(e), type = "error", duration = 6)
      })
    })
    
    # Retorno ----
    list(actualizaciones = shiny::reactive(actualizaciones_rv()))
  })
}
