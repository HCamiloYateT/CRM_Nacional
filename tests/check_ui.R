# REGRESION DE UI Y OPERACIONES PROTEGIDAS ----
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1]] else ".")
library(shiny)
library(bs4Dash)
source(file.path(root, "tests/helpers.R"))
env <- .crm_ui_env(root)
base_ref <- if (length(args) > 1L) args[[2]] else "4e338012efb626b0860a81e01d06b6e0b6841d97"
checks <- 0L
.check <- function(condition) {
  stopifnot(condition)
  checks <<- checks + 1L
}
paths <- c(list.files(file.path(root, "APP/misc/modules/controlbar"), "[.]R$", full.names = TRUE),
  list.files(file.path(root, "APP/misc/modules/encabezado"), "[.]R$", full.names = TRUE),
  file.path(root, "APP/misc/modules/generalidades/01_resumen_ejecutivo.R"),
  file.path(root, "APP/misc/ui/header.R"))
.collect <- function(tree) {
  result <- character()
  protected <- c(".header_resumen_cliente", ".header_historial_comercial", ".tareas_modificar_bd",
    ".tareas_pendientes", ".tareas_actividades", ".tareas_prioridad", "guardar_registro", "construir_payload",
    "ultimo_registro", "crear_formulario", ".registrar_o_comentar", ".obtener_fpg_cliente", ".validar_formulario",
    ".crear_tabla_pdf", ".crear_tabla_encabezado", ".obtener_datos_productos")
  visit <- function(node) {
    if (is.character(node) && length(node) == 1L && grepl(
        "^\\s*(SELECT|INSERT|UPDATE|DELETE|MERGE|BEGIN|COMMIT|ROLLBACK|WITH)\\b", node, ignore.case = TRUE)) {
      result <<- c(result, node)
    }
    if (is.call(node)) {
      head <- paste(deparse(node[[1]]), collapse = "")
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
cat(checks, "comprobaciones de SQL, comentarios y UI correctas; no se ejecutaron servidores ni operaciones BD.\n")
