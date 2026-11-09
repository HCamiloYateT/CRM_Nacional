# DEFINICIONES UI SIN INICIAR EL CRM ----
.crm_ui_env <- function(root) {
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "APP/misc/compartido/00_presentacion.R"), env)
  sys.source(file.path(root, "APP/misc/compartido/04_interaccion.R"), env)
  env$Choices <- function() list(personas = c("Ana", "Luis"), formapago = "PAGO ANTICIPADO")
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$.url_compartido <- "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main"
  env$SEP_MILES <- ","
  env$SEP_DECIMAL <- "."
  env$.TSK_PRIORIDADES <- c("URGENTE", "IMPORTANTE", "MEDIA", "BAJA")
  paths <- c(list.files(file.path(root, "APP/misc/modules/controlbar"), "[.]R$", full.names = TRUE),
    list.files(file.path(root, "APP/misc/modules/encabezado"), "[.]R$", full.names = TRUE),
    file.path(root, "APP/misc/modules/acciones/00_Funciones.R"))
  for (path in paths) for (expression in parse(path)) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) && length(expression) == 3L &&
        is.call(expression[[3]]) && identical(expression[[3]][[1]], as.name("function"))) eval(expression, env)
  }
  env
}
