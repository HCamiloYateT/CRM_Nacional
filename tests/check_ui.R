# REGRESION DE UI Y OPERACIONES PROTEGIDAS ----
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1]] else ".")
library(shiny)
library(bs4Dash)
source(file.path(root, "tests/helpers.R"))
env <- .crm_ui_env(root)
base_ref <- if (length(args) > 1L) args[[2]] else "f71acf7e4f604c94b5a3ac064184c303916a7b8b"
checks <- 0L
.check <- function(condition) {
  stopifnot(condition)
  checks <<- checks + 1L
}
paths <- c(list.files(file.path(root, "APP/misc/modules/controlbar"), "[.]R$", full.names = TRUE),
  list.files(file.path(root, "APP/misc/modules/encabezado"), "[.]R$", full.names = TRUE),
  file.path(root, "APP/misc/modules/generalidades/01_resumen_ejecutivo.R"),
  file.path(root, "APP/misc/modules/generalidades/02_validacion_financiera.R"),
  file.path(root, "APP/misc/ui/header.R"))
.collect <- function(tree) {
  result <- character()
  protected <- c(".tareas_modificar_bd",
    ".tareas_pendientes", ".tareas_actividades", ".tareas_prioridad", "guardar_registro", "construir_payload",
    "ultimo_registro", "crear_formulario", ".registrar_o_comentar", ".obtener_fpg_cliente", ".validar_formulario",
    ".crear_tabla_pdf", ".crear_tabla_encabezado", ".obtener_datos_productos")
  presentation_data <- c("datos", "nombre", "linea", "ids", "ops", "pedidos", "lotes", "por_producto",
    "detalle", "sin_pedido", "sin_operacion", "registros", "actual", "campos", "nombre_padre", "nombres", "padre",
    "mostrar", "tabla")
  collect_math <- function(node) {
    if (is.call(node)) {
      if (identical(node[[1]], as.name("<-")) && is.symbol(node[[2]]) &&
          as.character(node[[2]]) %in% presentation_data) {
        result <<- c(result, paste(deparse(node, width.cutoff = 500L), collapse = "\n"))
      }
      lapply(as.list(node)[-1L], collect_math)
    } else if (is.expression(node) || is.pairlist(node)) lapply(as.list(node), collect_math)
    invisible(NULL)
  }
  visit <- function(node) {
    if (is.character(node) && length(node) == 1L && grepl(
        "^\\s*(SELECT|INSERT|UPDATE|DELETE|MERGE|BEGIN|COMMIT|ROLLBACK|WITH)\\b", node, ignore.case = TRUE)) {
      result <<- c(result, node)
    }
    if (is.call(node)) {
      head <- paste(deparse(node[[1]]), collapse = "")
      if (head == "<-" && length(node) == 3L && is.symbol(node[[2]]) &&
          as.character(node[[2]]) %in% c(".header_resumen_cliente", ".header_historial_comercial")) {
        collect_math(node[[3]])
      }
      if (head == "<-" && length(node) == 3L && is.symbol(node[[2]]) && as.character(node[[2]]) %in% protected) {
        result <<- c(result, paste(deparse(node, width.cutoff = 500L), collapse = "\n"))
      }
      if (grepl("^(DBI::|racafeBD::|dbExecute$|dbGetQuery$|dbWithTransaction$|AgregarDatos$|CargarDatos$)", head)) {
        result <<- c(result, paste(deparse(node, width.cutoff = 500L), collapse = "\n"))
      }
      lapply(as.list(node)[-1L], visit)
    } else if (is.expression(node) || is.pairlist(node)) lapply(as.list(node), visit)
    invisible(NULL)
  }
  visit(tree)
  result
}
for (path in paths) {
  relative <- substring(path, nchar(root) + 2L)
  original <- system2("git", c("-C", shQuote(root), "show", shQuote(paste0(base_ref, ":", relative))), stdout = TRUE)
  before <- parse(text = original, keep.source = TRUE)
  after <- parse(path, keep.source = TRUE)
  .check(identical(.collect(before), .collect(after)))
  .comments <- function(tree) {
    data <- getParseData(tree)
    data$text[data$token == "COMMENT"]
  }
  .check(all(.comments(before) %in% .comments(after)))
}
uis <- list(env$FiltrosUI("filtros"), env$CotizacionUI("cot"), env$CrearNotasUI("nota"),
  env$RegistrarCompetenciaUI("comp"), env$TablaIndicadoresUI("indicadores"),
  env$MigrarCarteraUI(shiny::NS("migracion")))
for (ui in uis) .check(nzchar(htmltools::renderTags(ui)$html))
productos <- list(list(producto = "Café <script>alert(1)</script>", presentacion = "Kg", empaque = "Bolsa",
  cantidad = 12.5, precio = 1234, total = 15425))
html <- htmltools::renderTags(env$.cot_tabla_resumen_ui(productos, 15425))$html
.check(grepl("&lt;script&gt;", html, fixed = TRUE))
.check(grepl("15,425", html, fixed = TRUE))
.check(!grepl("<script>alert", html, fixed = TRUE))
sys.source(file.path(root, "APP/misc/ui/header.R"), env)
html <- htmltools::renderTags(env$header)$html
.check(!grepl("li_menu_", html, fixed = TRUE))
.check(!grepl("crmHeaderRetenerMenu", html, fixed = TRUE))
clientes <- data.frame(CLCliNit = "900123", CLLinNegNo = "A LA MEDIDA", LinNegCod = "A",
  PerRazSoc = "Cliente <script>texto</script>", UltActualizacion = "01/01/2025")
html <- htmltools::renderTags(env$.header_items_clientes(clientes, "antiguo", "cliente"))$html
.check(length(regmatches(html, gregexpr("NIT:", html, fixed = TRUE))[[1]]) == 1L)
.check(grepl("Actualización: 01/01/2025", html, fixed = TRUE))
.check(!grepl("rf-status-badge", html, fixed = TRUE))
.check(grepl("&lt;script&gt;", html, fixed = TRUE))
tarea <- data.frame(cons = 7, titulo = "Revisar Café & <texto>", Tipo = "list", Prioridad = "URGENTE",
  Responsable = "ANA|LUIS", FechaCumplimiento = as.Date("2026-11-06"), Archivado = 0, Cumplido = 0,
  Actividades = "Revisar|Confirmar", Cumplidas = "1|0")
html <- htmltools::renderTags(env$.tareas_lista_item(tarea, "abrir"))$html
.check(grepl("Urgente", html, fixed = TRUE))
.check(grepl("Actividades: 1/2 actividades", html, fixed = TRUE))
.check(grepl("ANA · LUIS", html, fixed = TRUE))
.check(grepl("&lt;texto&gt;", html, fixed = TRUE))
.check(is.na(env$.header_numero(c(NA_real_, NA_real_))))
.check(identical(env$.header_numero(c("0", "2", NA)), 2))
cat(checks, "comprobaciones de SQL, comentarios y UI correctas; no se ejecutaron servidores ni operaciones BD.\n")
