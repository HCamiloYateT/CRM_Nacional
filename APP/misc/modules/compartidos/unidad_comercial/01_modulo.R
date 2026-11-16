# Presentación compartida de la unidad comercial ----
.crm_badge_categoria <- function(valor, tipo) {
  if (length(valor) != 1L || is.na(valor) || !nzchar(valor)) return(NULL)
  color <- unname(.crm_colores(tipo, valor))
  if (is.na(color)) color <- "#495057"
  shiny::tagAppendAttributes(racafeForms::racafeStatusBadge(valor, size = "xs"),
    style = paste0("border:1px solid ", color, ";color:#343A40;background:#fff;"))
}

.crm_unidad_ficha <- function(datos) {
  if (!nrow(datos)) return(.crm_nota("No hay una unidad comercial identificada para esta entidad."))
  campo <- function(x, n) if (n %in% names(x)) x[[n]][1] else NA
  fecha <- function(v) if (!length(v) || is.na(v)) "No disponible" else format(as.Date(v), "%d/%m/%Y")
  racafeForms::racafePanel(variant = "muted", title = paste(unique(stats::na.omit(datos$PerRazSoc)), collapse = " · "),
    lapply(seq_len(nrow(datos)), function(i) {
      x <- datos[i, , drop = FALSE]
      referencia <- campo(x, "MesReferencia")
      mes <- if (is.na(referencia)) "mes de referencia" else paste(
        c("ene.", "feb.", "mar.", "abr.", "may.", "jun.", "jul.", "ago.", "sep.", "oct.", "nov.", "dic.")[
          as.integer(format(as.Date(referencia), "%m"))], format(as.Date(referencia), "%Y"))
      periodo <- paste(fecha(campo(x, "PeriodoDesde")), fecha(campo(x, "PeriodoHasta")), sep = "–")
      asesor <- as.character(campo(x, "Asesor"))
      sin_asesor <- is.na(asesor) || !nzchar(asesor) || asesor %in% c("SIN ASESOR", "SIN DATO")
      contacto <- identical(as.character(campo(x, "EtapaVigente")), "CONTACTO") && sin_asesor
      persona <- if (contacto) campo(x, "UsuarioCrea") else if (sin_asesor) "No disponible" else asesor
      campos <- c(SacosMes = paste("Sacos ·", mes), MargenMes = paste("Margen ·", mes),
        SacosPeriodo = paste("Sacos ·", periodo), MargenPeriodo = paste("Margen ·", periodo),
        PresupuestoSacos = "Presupuesto acumulado · sacos", PresupuestoMargen = "Presupuesto acumulado · margen",
        CumplimientoSacos = "Cumplimiento · sacos", CumplimientoMargen = "Cumplimiento · margen")
      valor <- function(n) {
        v <- campo(x, n)
        if (is.na(v)) return("No disponible")
        if (grepl("Cumplimiento", n)) return(paste0(format(round(v * 100, 1), trim = TRUE), "%"))
        paste0(if (grepl("Margen", n)) "$" else "", format(round(v, 2), big.mark = ",", trim = TRUE))
      }
      shiny::tags$section(class = "crm-unidad-resumen crm-unidad-compacta",
        shiny::tags$div(class = "crm-identidad-badges",
          racafeForms::racafeStatusBadge(paste("NIT", .crm_id_texto(x$CliNitPpal)), size = "xs"),
          .crm_badge_categoria(.crm_label_comercial(campo(x, "CLLinNegNo")), "linneg"),
          .crm_badge_categoria(campo(x, "EtapaVigente"), "etapa"),
          .crm_badge_categoria(campo(x, "EstadoCliente"), "estado"),
          .crm_badge_categoria(campo(x, "Segmento"), "segmento"),
          .crm_badge_categoria(campo(x, "SegmentoRFM"), "rfm"),
          shiny::tags$span(paste0(if (contacto) "Creado por: " else "Asesor: ", persona))),
        shiny::tags$div(class = "crm-unidad-metricas", lapply(names(campos), function(n) {
          shiny::tags$div(shiny::tags$small(campos[[n]]), shiny::tags$strong(valor(n)))
        })),
        shiny::tags$div(class = "crm-unidad-fechas",
          shiny::tags$span(paste("Primera factura:", fecha(campo(x, "PrimeraFacturacion")))),
          shiny::tags$span(paste("Última factura:", fecha(campo(x, "UltimaFacturacion")))),
          shiny::tags$small(paste("Estado oficial:", fecha(campo(x, "FechaEstadoOficial")),
            "· Presupuesto hasta", fecha(campo(x, "CortePresupuesto"))))))
    }))
}

.crm_unidades_sesion <- function() {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session) || is.null(session$userData$unidades_comerciales)) return(data.frame())
  session$userData$unidades_comerciales()
}
UnidadComercialUI <- function(id) shiny::uiOutput(shiny::NS(id)("contenido"))
UnidadComercial <- function(id, datos, seleccion) {
  shiny::moduleServer(id, function(input, output, session) {
    output$contenido <- .crm_render_ui(.crm_unidad_ficha(datos()[datos()$Identificador %in% seleccion(), , drop = FALSE]))
  })
}
