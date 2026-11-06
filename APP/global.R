# Configuraciones Iniciales ----
## Variables de control
# Flags de entorno para comportamiento en produccion vs desarrollo
verbose <- FALSE
debug   <- FALSE

## Configuracion regional y del sistema
# Zona horaria Colombia (UTC-5, sin DST): afecta POSIXct, lubridate y logs
Sys.setenv(TZ = "America/Bogota")

# LANG: mensajes de error de librerias C en Linux; ignorado en Windows
Sys.setenv(LANG = "es_CO.UTF-8")
# LANGUAGE: preferencia de idioma para mensajes gettext cuando esta disponible
Sys.setenv(LANGUAGE = "es")

# LC_TIME: nombres de meses/dias en espanol para strftime y lubridate
# LC_MONETARY: simbolo COP disponible en format(style = "currency")
# LC_MESSAGES: warnings del SO en espanol
locales_es <- c(
  "es_CO.UTF-8", "es_CO.utf8", "es_CO",
  "es_ES.UTF-8", "es_ES.utf8", "es_ES",
  "Spanish_Colombia.1252", "Spanish_Spain.1252", "Spanish"
)
.set_spanish_locale <- function(cat) {
  actual <- suppressWarnings(tryCatch(Sys.getlocale(cat), error = function(e) "C"))
  
  if (grepl("^(es|spanish)", actual, ignore.case = TRUE)) {
    return(invisible(actual))
  }
  
  for (loc in locales_es) {
    res <- suppressWarnings(tryCatch(Sys.setlocale(cat, loc), error = function(e) "C"))
    if (!identical(res, "C") && nzchar(res) && grepl("^(es|spanish)", res, ignore.case = TRUE)) {
      return(invisible(res))
    }
  }
  
  invisible(actual)
}
locales_activos <- setNames(
  lapply(c("LC_TIME", "LC_MONETARY", "LC_MESSAGES"), .set_spanish_locale),
  c("LC_TIME", "LC_MONETARY", "LC_MESSAGES")
)

# Fallback independiente del locale del servidor publicado para fechas visibles en UI
meses_es <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)
meses_abrev_es <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
format_fecha_es <- function(fecha = Sys.Date(), abreviado = TRUE) {
  fecha <- as.Date(fecha)
  meses <- if (abreviado) meses_abrev_es else meses_es
  sprintf("%02d %s %d", as.integer(format(fecha, "%d")), meses[as.integer(format(fecha, "%m"))],
          as.integer(format(fecha, "%Y")))
}
locale_time_es_activo <- grepl("^(es|spanish)", Sys.getlocale("LC_TIME"), ignore.case = TRUE)
if (!locale_time_es_activo) {
  warning(
    sprintf(
      paste(
        "No fue posible activar un locale en espanol para LC_TIME; LC_TIME actual: %s.",
        "Instala/genera es_CO.UTF-8 o es_ES.UTF-8 en el servidor de produccion.",
        "Para fechas visibles en la UI usa format_fecha_es(), que no depende del locale del sistema."
      ),
      Sys.getlocale("LC_TIME")
    ),
    call. = FALSE
  )
}
rm(.set_spanish_locale)

# Verificacion de locales activos al arranque
if (verbose) {
  message("[INFO] Estado de locales al arranque:")
  for (.cat in c("LC_TIME", "LC_MONETARY", "LC_NUMERIC", "LC_MESSAGES")) {
    message(sprintf("  %s = %s", .cat, Sys.getlocale(.cat)))
  }
  rm(.cat)
}

## Opciones globales
options(
  # Repositorio estable CRAN
  repos                  = c(CRAN = "https://cloud.r-project.org"),
  # dplyr: silenciar mensajes de summarise y progreso en produccion
  dplyr.summarise.inform = FALSE,
  dplyr.show_progress    = FALSE,
  # Numeros: punto decimal anglosajón — formato visual solo en capa de presentacion
  OutDec                 = ".",
  scipen                 = 999,
  # lubridate: semana ISO lunes=1, sin mensajes
  lubridate.week.start   = 1,
  lubridate.verbose      = FALSE,
  lubridate.quiet        = TRUE,
  # pillar: control de prints de tibbles en consola y logs
  pillar.sigfig          = 4,
  pillar.print_max       = 30,
  pillar.print_min       = 10,
  # Consola
  encoding               = "UTF-8",
  width                  = 120,
  max.print              = 1000,
  # Shiny: full stack trace y error handler segun modo activo
  shiny.autoreload       = FALSE,
  shiny.fullstacktrace   = debug,
  shiny.error            = if (debug) NULL else function() invisible(NULL)
)

# Librerias ----
library("racafeCore")
library("racafeBD")
library("racafeDrive")
library("racafeGraph")
library("racafeShiny")
library("racafeForms")
library("racafeModulos")
racafeCore::Loadpkg(c("shiny", "bs4Dash", "shinyBS", "shinyjs", "shinyWidgets", "tidyverse", "scales", "rlang",
                      "waiter", "glue", "lubridate", "stringr", "purrr", "gt", "plotly"))

# Impresiones ----
tit_app <- "Análisis de Ofertas"
# valores: prototipo, pruebas, staging, produccion, demo, mantenimiento, ninguno
badge_estado <- "staging"

# Datos ----
# Carga datos precargados desde RData
if (file.exists("data/data.RData")) {
  tryCatch(
    {
      load("data/data.RData", envir = globalenv())
      if (verbose) message("[OK] Datos cargados desde data/data.RData")
    },
    error = function(e) {
      # Error real de lectura: siempre visible independiente de verbose
      message(sprintf("[ERROR] Fallo leyendo data/data.RData: %s", e$message))
    }
  )
} else {
  if (verbose) message("[INFO] data/data.RData no encontrado. App inicia sin datos precargados.")
}


# Constantes ----
uid <- Sys.getenv("SYS_UID")
pwd <- Sys.getenv("SYS_PWD")
# Sources ----
load_modules(verbose = verbose)
 