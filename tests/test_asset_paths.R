# Rutas de arranque sin cargar librerías ni ejecutar el servidor ----
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1]] else ".", winslash = "/", mustWork = TRUE)
env <- new.env(parent = baseenv())
nombres <- ".crm_ruta_app"
for (expresion in parse(file.path(root, "APP", "global.R"))) {
  if (is.call(expresion) && identical(expresion[[1]], as.name("<-")) && is.symbol(expresion[[2]]) &&
      as.character(expresion[[2]]) %in% nombres) eval(expresion, envir = env)
}
app <- normalizePath(file.path(root, "APP"), winslash = "/", mustWork = TRUE)
stopifnot(identical(env$.crm_ruta_app(root), app), identical(env$.crm_ruta_app(app), app))
prueba <- function() {
  anterior <- getwd()
  on.exit(setwd(anterior), add = TRUE)
  for (directorio in c(root, app)) {
    setwd(directorio)
    stopifnot(identical(env$.crm_ruta_app(), app))
  }
  ausente <- tempfile("sin-compartido-")
  error <- tryCatch(env$.crm_ruta_app(ausente), error = identity)
  stopifnot(inherits(error, "error"), grepl("shiny::runApp", conditionMessage(error), fixed = TRUE))
}
prueba()
message("OK: rutas desde raíz/APP y diagnóstico de APP ausente, sin recursos vendor locales.")
