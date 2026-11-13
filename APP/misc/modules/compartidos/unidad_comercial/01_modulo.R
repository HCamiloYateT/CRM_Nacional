# Presentación compartida de la unidad comercial ----
.crm_badge_categoria <- function(valor, tipo) {
  if (length(valor) != 1L || is.na(valor) || !nzchar(valor)) return(NULL)
  color <- unname(.crm_colores(tipo, valor))
  if (is.na(color)) color <- "#495057"
  shiny::tagAppendAttributes(racafeForms::racafeStatusBadge(valor, size = "xs"),
    style = paste0("border:1px solid ", color, ";color:#343A40;background:#fff;"))
}

.crm_unidad_ficha <- function(datos) {
  if (!nrow(datos)) return(.crm_nota("No hay una unidad comercial con línea identificada para esta entidad."))
  titulo <- paste(unique(stats::na.omit(datos$PerRazSoc)), collapse = " · ")
  racafeForms::racafePanel(variant = "muted", title = titulo,
    shiny::tags$div(class = "crm-unidades-compactas", lapply(seq_len(nrow(datos)), function(i) {
      x <- datos[i, , drop = FALSE]
      campos <- c(SacosMes = "Sacos del mes", MargenMes = "Margen del mes · COP", SacosPeriodo = "Sacos del periodo",
        MargenPeriodo = "Margen del periodo · COP", PresupuestoSacos = "Presupuesto de sacos",
        PresupuestoMargen = "Presupuesto de margen · COP", CumplimientoSacos = "Cumplimiento de sacos",
        CumplimientoMargen = "Cumplimiento de margen", PrimeraFacturacion = "Primera factura",
        UltimaFacturacion = "Última factura")
      mostrar <- function(campo) {
        v <- x[[campo]]
        if (!length(v) || is.na(v)) return("No disponible")
        if (inherits(v, "Date")) return(format(v, "%d/%m/%Y"))
        if (grepl("Cumplimiento", campo)) return(paste0(format(round(v * 100, 1), trim = TRUE), "%"))
        paste0(if (grepl("Margen", campo)) "$" else "", format(round(v, 2), big.mark = ",", trim = TRUE))
      }
      shiny::tags$section(class = "crm-unidad-resumen",
        shiny::tags$div(class = "crm-identidad-badges",
          .crm_badge_categoria(.crm_label_comercial(x$CLLinNegNo), "linneg"),
          .crm_badge_categoria(x$EtapaVigente, "etapa"), .crm_badge_categoria(x$EstadoCliente, "estado"),
          .crm_badge_categoria(x$Origen, "origen"), .crm_badge_categoria(x$SegmentoRFM, "rfm")),
        racafeForms::racafeInfoGrid(racafeForms::racafeInfoItem("NIT", x$CliNitPpal),
          racafeForms::racafeInfoItem("Asesor", x$Asesor), racafeForms::racafeInfoItem("Responsable", x$Responsable),
          racafeForms::racafeInfoItem("Segmento", x$Segmento),
          racafeForms::racafeInfoItem("Cierre del estado oficial", if (is.null(x$FechaEstadoOficial) ||
            is.na(x$FechaEstadoOficial)) "No disponible" else format(as.Date(x$FechaEstadoOficial), "%d/%m/%Y"))),
        racafeForms::racafeInfoGrid(lapply(names(campos), function(campo)
          racafeForms::racafeInfoItem(campos[[campo]], mostrar(campo)))),
        if (all(c("PeriodoDesde", "PeriodoHasta", "MesReferencia", "CortePresupuesto") %in% names(x))) {
          .crm_nota(c(paste("Periodo:", format(as.Date(x$PeriodoDesde), "%d/%m/%Y"), "a",
            format(as.Date(x$PeriodoHasta), "%d/%m/%Y")),
            paste("Mes de referencia:", format(as.Date(x$MesReferencia), "%m/%Y"),
              "· Presupuesto hasta", format(as.Date(x$CortePresupuesto), "%d/%m/%Y"))))
        })
    })))

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
