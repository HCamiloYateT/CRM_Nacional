library(dplyr)
source("APP/misc/datos/01_contrato.R")
source("APP/misc/compartido/00_presentacion.R")
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/modules/gestion_comercial/oportunidades/01_modulo.R")
source("APP/misc/modules/prospeccion/kanban/01_modulo.R")
source("APP/misc/modules/productos/sin_clasificar/00_datos.R")
# Los identificadores no se formatean como importes; los importes siguen numéricos.
x <- .crm_nits_tabla(data.frame(Pedido = 1234567, Anho = 2026, NIT = 900123456, Margen = 1234567))
stopifnot(identical(x$Pedido, "1234567"), identical(x$Anho, "2026"), is.numeric(x$Margen))
# El controlbar es el límite del tablero Kanban; no expone asesores ajenos ni amplía vacíos.
pipeline <- data.frame(CodContacto = c("C1", "C2", "C3"), Asesor = c("ANA", "LUIS", "ANA"),
  UsuarioCrea = c("ANA", "LUIS", "ANA"), Segmento = c("MEDIANO", "MEDIANO", "GRANDE"),
  LinNegocio = "A LA MEDIDA")
f <- list(asesor = "ANA", segmento = "MEDIANO", linneg = "A LA MEDIDA")
stopifnot(identical(.kb_alcance(pipeline, f, "ANA", FALSE)$CodContacto, "C1"))
f$asesor <- character()
stopifnot(nrow(.kb_alcance(pipeline, f, "ANA", TRUE)) == 0L)
# Clasificación: se conserva la historia y se cambia solo la referencia confirmada.
fuente <- data.frame(LinNegCod = 21000, LinProCod = c(1, 1, 2, 3), MCCod = 4, MrcCod = 5,
  Categoria = c(NA, NA, "REGIONAL", "ESPECIAL"), Producto = c(NA, NA, "HUILA", "PREMIUM"),
  FecProceso = as.Date(c("2025-01-01", "2026-01-01", "2026-01-01", "2026-01-01")),
  Marca = "RACAFÉ", MCNom = "CAFÉ DE PRUEBA", LinProNom = "SACO", Excluir = "NO")
pendientes <- .clas_pendientes(fuente)
stopifnot(nrow(pendientes) == 1L, pendientes$FecProceso == as.Date("2026-01-01"))
llaves_guardadas <- NULL
cargar <- function(...) fuente
guardar <- function(fila, tabla, llaves, original) {
  stopifnot(tabla == "CRMNALPRODS", nrow(fila) == 1L)
  llaves_guardadas <<- llaves
  i <- .clas_llave(fuente) == .clas_llave(fila) & fuente$FecProceso == fila$FecProceso
  fuente[i, ] <<- fila
}
nuevo <- .clas_guardar(pendientes, "REGIONAL", "HUILA", "ANA", cargar, guardar)
stopifnot(nrow(.clas_pendientes(nuevo)) == 0L, "FecProceso" %in% names(llaves_guardadas),
  is.na(fuente$Categoria[[1]]), fuente$Categoria[[2]] == "REGIONAL", fuente$Producto[[2]] == "HUILA")
# El adaptador de los tableros recibe la clasificación nueva sin duplicar movimientos.
hecho <- data.frame(LinNegCod = 21000, LinProCod = 1, MCCod = 4, MrcCod = 5)
enriquecido <- .crm_enriquecer_productos(hecho, nuevo)
stopifnot(nrow(enriquecido) == 1L, enriquecido$Categoria == "REGIONAL", enriquecido$Producto == "HUILA")
# No sobrescribir cambios de otro usuario; tampoco aceptar combinaciones inexistentes.
stopifnot(inherits(try(.clas_guardar(pendientes, "ESPECIAL", "PREMIUM", "ANA", cargar, guardar),
  silent = TRUE), "try-error"))
fuente$Categoria[[2]] <- fuente$Producto[[2]] <- NA_character_
stopifnot(inherits(try(.clas_guardar(pendientes, "ESPECIAL", "HUILA", "ANA", cargar, guardar),
  silent = TRUE), "try-error"))
cat("OK: formatos de identificadores, alcance Kanban, clasificación, historia y edición concurrente.\n")
