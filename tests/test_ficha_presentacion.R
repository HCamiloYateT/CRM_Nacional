# Verifica presentación sin cambiar los datos ni acceder al CRM ----
library(dplyr)
source("APP/misc/compartido/00_presentacion.R")
source("APP/misc/compartido/05_componentes_forms.R")
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/modules/consulta_individual/00_datos.R")
source("APP/misc/modules/consulta_individual/02_perfil_analitico.R")
source("APP/misc/modules/consulta_individual/03_precios_pedidos.R")
source("APP/misc/modules/consulta_individual/04_ficha_presentacion.R")
html <- function(x) as.character(htmltools::tagList(x))
stopifnot(.in_ficha_numero(0) == "0", .in_ficha_numero(NA_real_) == "Sin dato",
  .in_ficha_numero(235200000, TRUE) == "$235.200.000 COP")
stopifnot(identical(.in_ficha_valores(c("SIN DATO | 12345", "", "12345", NA_character_)), "12345"))
creacion <- as.POSIXct("2018-02-14 10:35:22", tz = "America/Bogota")
stopifnot(.in_ficha_fecha(creacion) == "14/02/2018 10:35:22")
contactos <- data.frame(CodContacto = "CREPES", Estado = "ACTIVO", PerCod = "860076919",
  FechaHoraCrea = creacion, Origen = NA_character_, DetOrigen = NA_character_)
entidad <- data.frame(Codigo = "CREPES", Alcance = "Cuenta de facturación")
estado <- .in_ficha_estado(entidad, entidad, contactos)
stopifnot(estado$texto == "Activo en CRM", estado$variante == "success")
sin <- entidad; sin$Codigo <- NA_character_
stopifnot(.in_ficha_estado(sin, entidad, contactos)$variante == "neutral")
registro <- html(.in_ficha_crm_ui(contactos))
stopifnot(grepl("14/02/2018 10:35:22", registro), grepl("Documento registrado en CRM", registro),
  identical(contactos$FechaHoraCrea, creacion))
metas <- data.frame(MetaMensualSacos = 960, MetaMensualMargen = 235200000, CLLinNegNo = "A LA MEDIDA",
  Segmento = "GRANDE", PresupuestoEnRevision = FALSE, FecProceso = as.Date("2026-01-01"))
una <- html(.in_ficha_presupuesto_ui(metas, "Con presupuesto 2026", "DETALLE POR FILA"))
varias <- html(.in_ficha_presupuesto_ui(rbind(metas, metas), "Con presupuesto 2026", "DETALLE POR FILA"))
stopifnot(grepl("960", una), grepl("235.200.000", una), !grepl("DETALLE POR FILA", una),
  grepl("DETALLE POR FILA", varias), !grepl("rf-metric-value", varias))
vacia <- html(.in_ficha_presupuesto_ui(metas[0, ], "Sin presupuesto registrado", "DETALLE"))
stopifnot(grepl("Sin presupuesto registrado", vacia), !grepl("rf-metric-value", vacia))
# El bloque vacío resume seis secciones sin repetir el mismo aviso.
estados <- lapply(names(.IN_COMPLEMENTOS), function(x) list(clave = x, n = 0L, error = FALSE))
complementos <- html(.in_ficha_complementos_ui(estados, shiny::NS("prueba")))
coincidencias <- regmatches(complementos, gregexpr("Sin registros para los filtros seleccionados", complementos))[[1]]
stopifnot(length(coincidencias) == 1L)
estados[[1]]$error <- TRUE
parcial <- html(.in_ficha_complementos_ui(estados, shiny::NS("prueba")))
stopifnot(grepl("Información parcial", parcial), !grepl("Sin registros para los filtros seleccionados", parcial))
cat("OK: metas de cero/una/varias filas, datos parciales, estados CRM, precisión de fecha y bloque vacío único.\n")

# Los registros múltiples conservan su contacto y su ubicación, sin concatenar sentinelas.
fila_contacto <- data.frame(NIT = c("860076919", "860076920"), Cuenta = c("Principal", NA_character_),
  Contacto = c("SIN DATO | Ana", "Luis"), Telefono = c("123", "456"), Correo = NA_character_,
  Direccion = c("Calle 1", "Calle 2"), Ciudad = "Bogotá", Departamento = NA_character_,
  Region = NA_character_, Pais = NA_character_)
contacto_html <- html(.in_ficha_contacto_ui(fila_contacto))
stopifnot(grepl("Calle 1", contacto_html), grepl("Calle 2", contacto_html), grepl("Luis", contacto_html),
  !grepl("SIN DATO [|]", contacto_html), .in_ficha_badge_texto(NA_character_) == "Sin dato")
# Lista densa: el asesor común no se repite; los asesores distintos se mantienen por fila.
familia_visual <- list(cuentas = data.frame(NIT = c("2", "1", "3"), Cuenta = c("Hija", "Padre", "Otra hija"),
  Relacion = c("Cuenta hija", "Cuenta padre", "Cuenta hija"), Asesor = "Ana"))
lista_html <- html(.in_ficha_familia_ui(familia_visual))
stopifnot(!grepl("rf-panel", lista_html, fixed = TRUE),
  regexpr("Padre", lista_html)[[1]] < regexpr("Hija", lista_html)[[1]],
  length(regmatches(lista_html, gregexpr("Asesor:", lista_html, fixed = TRUE))[[1]]) == 1L)
familia_visual$cuentas$Asesor[[2]] <- "Luis"
lista_html <- html(.in_ficha_familia_ui(familia_visual))
stopifnot(length(regmatches(lista_html, gregexpr("Asesor:", lista_html, fixed = TRUE))[[1]]) == 3L)

# Una selección pendiente no se transforma en seis fallos de fuente ni vuelve a evaluar la promesa.
seleccion_pendiente <- tryCatch(.in_ficha_complementos(list(), stop("Seleccione una entidad")), error = identity)
stopifnot(inherits(seleccion_pendiente, "error"),
  grepl("Seleccione una entidad", conditionMessage(seleccion_pendiente), fixed = TRUE))
