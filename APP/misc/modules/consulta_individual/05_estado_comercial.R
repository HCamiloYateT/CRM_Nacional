# Clasificación del mes: misma unidad comercial y estados que el panel aprobado de Cohortes ----
.in_clasificacion_cliente <- function(panel, hechos, metas, entidad, corte, principal = entidad$Principal) {
  mes <- .gc_mes(corte)
  detalle <- attr(metas, "presupuesto_detalle")
  if (!is.data.frame(detalle)) detalle <- data.frame()
  if (nrow(detalle)) {
    permitidas <- if (entidad$Alcance == "Cuenta principal") {
      .in_id(detalle$CliNitPpal) %in% entidad$Principal
    } else .in_id(detalle$CLCliNit) %in% entidad$NitFacturacion
    detalle <- detalle[permitidas, , drop = FALSE]
  }
  claves <- dplyr::bind_rows(lapply(list(hechos, detalle), function(x) {
    data.frame(CliNitPpal = .in_id(.gc_columna(x, "CliNitPpal")),
      LinNegCod = as.character(.gc_columna(x, "LinNegCod")))
  })) %>% dplyr::filter(!is.na(CliNitPpal), !is.na(LinNegCod)) %>% dplyr::distinct()
  principal <- .in_id(principal)
  if (length(principal) == 1L && !is.na(principal)) {
    claves <- claves[claves$CliNitPpal == principal, , drop = FALSE]
  } else claves <- claves[0, , drop = FALSE]
  if (!nrow(panel) || !nrow(claves)) return(list(datos = data.frame(), mes = mes, error = FALSE))
  if (!all(c("FecProceso", "CliNitPpal", "LinNegCod") %in% names(panel)) ||
      !any(c("EstadoPanel", "Estado") %in% names(panel))) stop("El panel no contiene la clasificación comercial.")
  x <- data.frame(CliNitPpal = .in_id(panel$CliNitPpal), LinNegCod = as.character(panel$LinNegCod),
    FecProceso = .in_fecha(panel$FecProceso), Estado = as.character(.gc_columna(panel, c("EstadoPanel", "Estado"))))
  # No sustituir un mes sin clasificación por estados antiguos ni por la fecha de la primera compra.
  x <- x %>% dplyr::semi_join(claves, by = c("CliNitPpal", "LinNegCod")) %>%
    dplyr::filter(!is.na(FecProceso), .gc_mes(FecProceso) == mes, FecProceso <= as.Date(corte))
  if (nrow(x)) x <- x %>%
    dplyr::group_by(CliNitPpal, LinNegCod) %>% dplyr::filter(FecProceso == max(FecProceso)) %>%
    dplyr::summarise(Estado = {
      valores <- unique(Estado)
      if (length(valores) == 1L) valores[[1L]] else "REVISAR"
    }, .groups = "drop")
  list(datos = dplyr::left_join(claves, x, by = c("CliNitPpal", "LinNegCod")), mes = mes, error = FALSE)
}
.in_clasificacion_badges <- function(resultado, entidad) {
  if (is.null(resultado)) return(NULL)
  mes <- .coha_mes(resultado$mes)
  if (isTRUE(resultado$error)) return(racafeForms::racafeStatusBadge(
    paste("Clasificación comercial no disponible ·", mes), variant = "warning"))
  x <- resultado$datos
  if (!nrow(x)) return(racafeForms::racafeStatusBadge(
    paste("Estado comercial sin dato ·", mes), variant = "neutral"))
  nombres <- c(ACTIVO = "Activo", INACTIVO = "Inactivo", NUEVO = "Cliente nuevo", REVISAR = "Requiere revisión")
  colores <- c(ACTIVO = "success", INACTIVO = "danger", NUEVO = "info", REVISAR = "warning")
  shiny::tagList(lapply(seq_len(nrow(x)), function(i) {
    estado <- nombres[x$Estado[[i]]]; variante <- colores[x$Estado[[i]]]
    if (is.na(estado)) estado <- "Sin clasificación"
    if (is.na(variante)) variante <- "neutral"
    linea <- .crm_label_comercial(.in_linea(x$LinNegCod[[i]]))
    etiqueta <- if (nrow(x) > 1L) paste("Comercial", linea) else "Estado comercial"
    shiny::tagAppendAttributes(racafeForms::racafeDataBadge(etiqueta, paste(estado, "·", mes), variant = variante),
      title = paste("Clasificación de Cohortes ·", linea, "· Cuenta principal", x$CliNitPpal[[i]],
        "· Cliente nuevo corresponde a la clasificación del panel, no a una primera compra inferida."))
  }))
}
