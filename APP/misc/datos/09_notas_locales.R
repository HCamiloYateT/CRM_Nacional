# Persistencia local de notas y tareas ----

.notas_ruta <- function() {
  Sys.getenv("CRM_NACIONAL_NOTAS_RDS", unset = file.path(.ruta_app, "data", "notas_tareas.rds"))
}

.notas_validar <- function(registros) {
  requeridas <- c("cons", "titulo", "Usuario", "Responsable", "FechaHoraCrea", "Tipo", "Descripcion",
                  "FechaCumplimiento", "Actividades", "Cumplidas", "Etiquetas", "Prioridad", "Archivado",
                  "Cumplido", "Respuesta")
  if (!is.data.frame(registros) || !all(requeridas %in% names(registros))) {
    stop("La fuente de notas y tareas tiene un esquema incompleto.", call. = FALSE)
  }
  ids <- suppressWarnings(as.numeric(as.character(registros$cons)))
  if (any(!is.finite(ids) | ids < 1 | ids != trunc(ids)) || anyDuplicated(ids)) {
    stop("Los consecutivos de notas y tareas deben ser positivos y únicos.", call. = FALSE)
  }
  registros$cons <- ids
  texto <- setdiff(requeridas, c("cons", "FechaCumplimiento", "FechaHoraCrea", "Archivado", "Cumplido"))
  registros[texto] <- lapply(registros[texto], as.character)
  registros$FechaCumplimiento <- as.Date(registros$FechaCumplimiento, origin = "1970-01-01")
  registros$FechaHoraCrea <- as.POSIXct(registros$FechaHoraCrea, origin = "1970-01-01", tz = "America/Bogota")
  registros$Archivado <- as.integer(registros$Archivado)
  registros$Cumplido <- as.integer(registros$Cumplido)
  registros
}

.notas_publicar <- function(ruta, estado) {
  temporal <- tempfile(".notas-", tmpdir = dirname(ruta), fileext = ".rds")
  on.exit(unlink(temporal), add = TRUE)
  saveRDS(estado, temporal)
  Sys.chmod(temporal, mode = "0600")
  if (!file.rename(temporal, ruta)) {
    stop("No fue posible publicar el RDS local de notas y tareas.", call. = FALSE)
  }
  invisible(estado)
}

.notas_estado <- function(ruta) {
  estado <- readRDS(ruta)
  if (!is.list(estado) || !identical(estado$version, 1L) || length(estado$ultimo) != 1L ||
      !is.numeric(estado$ultimo) || !is.finite(estado$ultimo) || estado$ultimo < 0 ||
      estado$ultimo != trunc(estado$ultimo)) {
    stop("El RDS local de notas y tareas tiene un formato inválido.", call. = FALSE)
  }
  estado$registros <- .notas_validar(estado$registros)
  if (any(estado$registros$cons > estado$ultimo)) {
    stop("El consecutivo del RDS local de notas y tareas es inválido.", call. = FALSE)
  }
  estado
}

.notas_transaccion <- function(cambio = identity, ruta = .notas_ruta(),
                               importar = function() racafeBD::CargarDatos("CRMNALNOTAS")) {
  if (!nzchar(ruta)) stop("Configure la ruta de persistencia de notas y tareas.", call. = FALSE)
  carpeta <- dirname(ruta)
  if (!dir.exists(carpeta) && !dir.create(carpeta, recursive = TRUE, mode = "0700") && !dir.exists(carpeta)) {
    stop("No se pudo preparar el directorio de notas y tareas.", call. = FALSE)
  }
  bloqueo <- filelock::lock(paste0(ruta, ".lock"), timeout = 10000)
  if (is.null(bloqueo)) stop("El archivo de notas y tareas está ocupado. Intente de nuevo.", call. = FALSE)
  on.exit(filelock::unlock(bloqueo), add = TRUE)
  nuevo <- !file.exists(ruta)
  estado <- if (nuevo) {
    registros <- .notas_validar(importar())
    list(version = 1L, ultimo = max(c(0, registros$cons)), registros = registros)
  } else .notas_estado(ruta)
  actualizado <- cambio(estado)
  if (nuevo || !identical(actualizado, estado)) .notas_publicar(ruta, actualizado)
  actualizado
}

.notas_leer <- function(ruta = .notas_ruta()) {
  if (file.exists(ruta)) return(.notas_estado(ruta)$registros)
  .notas_transaccion(ruta = ruta)$registros
}

.notas_agregar <- function(registro, ruta = .notas_ruta()) {
  estado <- .notas_transaccion(function(actual) {
    registro$cons <- actual$ultimo + 1
    actual$registros <- dplyr::bind_rows(actual$registros, .notas_validar(registro))
    actual$ultimo <- registro$cons
    actual
  }, ruta = ruta)
  estado$registros[estado$registros$cons == estado$ultimo, , drop = FALSE]
}
