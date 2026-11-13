# Banco de consulta de los componentes reales, restringido a administradores ----
ModulosTransversalesUI <- function(id) {
  ns <- shiny::NS(id)
  bs4Dash::tabsetPanel(id = ns("seccion"),
    tabPanel("Pedidos", racafeForms::racafeForm(variant = "plain",
      racafeForms::racafeFormRow(columns = 4,
        .crm_select("pedido", "Código de pedido", choices = character(), ns = ns)),
      TrazabilidadPedidoUI(ns("pedido_detalle")))),
    tabPanel("Clientes / unidades comerciales", racafeForms::racafeForm(variant = "plain",
      racafeForms::racafeFormRow(columns = 4,
        .crm_select("unidad", "NIT, entidad y línea de negocio", choices = character(), ns = ns)),
      UnidadComercialUI(ns("unidad_detalle")))),
    tabPanel("Acciones", racafeForms::racafeForm(variant = "plain",
      racafeForms::racafeFormRow(columns = 4,
        .crm_select("entidad_accion", "Unidad comercial", choices = character(), ns = ns),
        .crm_select("accion", "Acción", choices = c("Editar", "Relacionamiento", "Promover", "Descartar",
          "CrearOportunidad"), ns = ns)), shiny::uiOutput(ns("accion_contenido")))))
}
ModulosTransversales <- function(id, usuario, es_admin, maestro, unidades) {
  shiny::moduleServer(id, function(input, output, session) {
    fuente <- shiny::reactive({ shiny::req(isTRUE(es_admin())); unidades() })
    shiny::observe({
      x <- fuente()
      etiquetas <- paste(x$PerRazSoc, x$CliNitPpal, .crm_label_comercial(x$CLLinNegNo), sep = " · ")
      opciones <- stats::setNames(x$Identificador, etiquetas)
      for (control in c("unidad", "entidad_accion")) {
        anterior <- shiny::isolate(input[[control]])
        .crm_update_select(session, control, choices = opciones,
          selected = if (length(anterior) && anterior %in% opciones) anterior else head(unname(opciones), 1L))
      }
    })
    shiny::observe({
      shiny::req(isTRUE(es_admin()))
      codigos <- sort(unique(.crm_id_texto(maestro()$CLPdcCod)))
      codigos <- codigos[!is.na(codigos) & codigos != "0"]
      anterior <- shiny::isolate(input$pedido)
      .crm_update_select(session, "pedido", choices = codigos,
        selected = if (length(anterior) && anterior %in% codigos) anterior else head(codigos, 1L))
    })
    TrazabilidadPedido("pedido_detalle", maestro, shiny::reactive({
      shiny::req(isTRUE(es_admin()), identical(input$seccion, "Pedidos")); input$pedido
    }))
    UnidadComercial("unidad_detalle", fuente, shiny::reactive(input$unidad))
    contacto <- function(accion) shiny::reactive({
      shiny::req(isTRUE(es_admin()), identical(input$seccion, "Acciones"), identical(input$accion, accion))
      x <- fuente()[fuente()$Identificador %in% input$entidad_accion, , drop = FALSE]
      shiny::req(nrow(x) == 1L, !is.na(x$CodContacto), nzchar(x$CodContacto))
      x$CodContacto
    })
    constructores <- list(Editar = EditarUI, Relacionamiento = RelacionamientoUI,
      Promover = PromoverUI, Descartar = DescartarUI, CrearOportunidad = CrearOportunidadUI)
    output$accion_contenido <- .crm_render_ui({
      shiny::req(isTRUE(es_admin()), input$seccion == "Acciones")
      x <- fuente()[fuente()$Identificador %in% input$entidad_accion, , drop = FALSE]
      shiny::validate(shiny::need(nrow(x) == 1L && !is.na(x$CodContacto) && nzchar(x$CodContacto),
        "Esta unidad aún no tiene ficha de gestión comercial; no se crea una ficha artificial."))
      shiny::req(input$accion %in% names(constructores))
      constructores[[input$accion]](session$ns(input$accion))
    })
    Editar("Editar", usuario, contacto("Editar"))
    Relacionamiento("Relacionamiento", usuario, contacto("Relacionamiento"))
    Promover("Promover", usuario, contacto("Promover"))
    Descartar("Descartar", usuario, contacto("Descartar"))
    CrearOportunidad("CrearOportunidad", usuario, contacto("CrearOportunidad"))
  })
}
