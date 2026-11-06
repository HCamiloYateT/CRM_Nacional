# Rutas de arranque sin cargar librerías ni ejecutar el servidor ----
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1]] else ".", winslash = "/", mustWork = TRUE)
env <- new.env(parent = baseenv())
nombres <- c(".crm_ruta_app", ".crm_ruta_compartido")
for (expresion in parse(file.path(root, "APP", "global.R"))) {
  if (is.call(expresion) && identical(expresion[[1]], as.name("<-")) && is.symbol(expresion[[2]]) &&
      as.character(expresion[[2]]) %in% nombres) eval(expresion, envir = env)
}
app <- normalizePath(file.path(root, "APP"), winslash = "/", mustWork = TRUE)
compartido <- normalizePath(file.path(app, "vendor", "Compartido"), winslash = "/", mustWork = TRUE)
stopifnot(identical(env$.crm_ruta_app(root), app), identical(env$.crm_ruta_app(app), app))
stopifnot(identical(env$.crm_ruta_compartido(app, ""), compartido))
stopifnot(identical(env$.crm_ruta_compartido(app, compartido), compartido))
prueba <- function() {
  anterior <- getwd()
  on.exit(setwd(anterior), add = TRUE)
  for (directorio in c(root, app)) {
    setwd(directorio)
    stopifnot(identical(env$.crm_ruta_app(), app))
    stopifnot(identical(env$.crm_ruta_compartido(env$.crm_ruta_app(), ""), compartido))
  }
  opciones <- options(racafeForms.shared_path = compartido)
  on.exit(options(opciones), add = TRUE)
  stopifnot(identical(env$.crm_ruta_compartido(app), compartido))
  options(racafeForms.shared_path = NULL)
  previo <- Sys.getenv("RACAFE_SHARED_PATH", unset = NA_character_)
  on.exit(if (is.na(previo)) Sys.unsetenv("RACAFE_SHARED_PATH") else Sys.setenv(RACAFE_SHARED_PATH = previo),
          add = TRUE)
  Sys.setenv(RACAFE_SHARED_PATH = compartido)
  stopifnot(identical(env$.crm_ruta_compartido(app), compartido))
  ausente <- tempfile("sin-compartido-")
  error <- tryCatch(env$.crm_ruta_compartido(app, ausente), error = identity)
  stopifnot(inherits(error, "error"), grepl("APP/vendor/Compartido completa", conditionMessage(error), fixed = TRUE))
  error <- tryCatch(env$.crm_ruta_app(ausente), error = identity)
  stopifnot(inherits(error, "error"), grepl("shiny::runApp", conditionMessage(error), fixed = TRUE))
}
prueba()
message("OK: rutas desde raíz/APP, configuración externa y diagnóstico de archivos ausentes.")
