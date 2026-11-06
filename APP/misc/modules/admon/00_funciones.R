# Funciones ----
.admon_choices_validar <- function(catalogos) {
  if (!is.list(catalogos) || is.null(names(catalogos)) ||
      anyNA(names(catalogos)) || any(!nzchar(names(catalogos))) ||
      anyDuplicated(names(catalogos)) ||
      !all(vapply(catalogos, is.character, logical(1)))) {
    stop("El archivo de catálogos debe ser una lista nombrada de vectores de texto.", call. = FALSE)
  }
  invisible(TRUE)
}

.admon_choices_normalizar <- function(valor) {
  if (length(valor) != 1L || is.na(valor)) {
    stop("Ingrese una única opción válida.", call. = FALSE)
  }
  valor <- toupper(trimws(gsub("[[:space:]]+", " ", as.character(valor))))
  if (!nzchar(valor) || nchar(valor) > 150L) {
    stop("La opción debe tener entre 1 y 150 caracteres.", call. = FALSE)
  }
  valor
}

.admon_choices_bloqueo <- function(accion, espera = 8) {
  inicio <- Sys.time()
  repeat {
    if (dir.create(.ADMON_CHOICES_LOCK, showWarnings = FALSE)) break
    if (as.numeric(difftime(Sys.time(), inicio, units = "secs")) > espera) {
      stop("Otro administrador está actualizando los catálogos.", call. = FALSE)
    }
    Sys.sleep(0.1)
  }
  on.exit(unlink(.ADMON_CHOICES_LOCK, recursive = TRUE), add = TRUE)
  accion()
}

.admon_choices_guardar <- function(catalogos) {
  .admon_choices_validar(catalogos)
  carpeta <- dirname(.ADMON_CHOICES_RDS)
  archivo <- tempfile("choices_", tmpdir = carpeta, fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  saveRDS(catalogos, archivo, version = 3)
  if (!file.rename(archivo, .ADMON_CHOICES_RDS)) {
    stop("No fue posible guardar el RDS. Verifique permisos.", call. = FALSE)
  }
  invisible(TRUE)
}

.admon_relaciones_vacias <- function() {
  data.frame(catalogo = character(), padre = character(), padre2 = character(),
             opcion = character(), stringsAsFactors = FALSE)
}

.admon_relaciones_fuente <- function() {
  normalizar <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x[is.na(x)] <- ""
    x
  }
  salida <- .admon_relaciones_vacias()
  origen <- tryCatch(racafeBD::CargarDatos("CRMNALORILEAD"), error = function(e) {
    warning(paste("No se pudo obtener la relación origen/detalle:", conditionMessage(e)))
    NULL
  })
  if (is.data.frame(origen) && all(c("Origen", "Detalle") %in% names(origen))) {
    if ("Estado" %in% names(origen)) origen <- origen[origen$Estado == "A", , drop = FALSE]
    salida <- rbind(salida, data.frame(
      catalogo = "detalle_origen", padre = normalizar(origen$Origen), padre2 = "",
      opcion = normalizar(origen$Detalle), stringsAsFactors = FALSE
    ))
  }
  productos <- tryCatch(racafeBD::CargarDatos("CRMNALPRODS"), error = function(e) {
    warning(paste("No se pudo obtener la jerarquía de productos:", conditionMessage(e)))
    NULL
  })
  if (is.data.frame(productos) && all(c("LinNeg", "Categoria", "Producto") %in% names(productos))) {
    lineas <- normalizar(productos$LinNeg)
    categorias <- normalizar(productos$Categoria)
    categorias[categorias == "BLEND"] <- "FUERA DE NORMA"
    valores <- normalizar(productos$Producto)
    salida <- rbind(salida,
                    data.frame(catalogo = "categoria", padre = lineas, padre2 = "", opcion = categorias),
                    data.frame(catalogo = "producto", padre = lineas, padre2 = categorias, opcion = valores)
    )
  }
  salida <- salida[nzchar(salida$opcion) & nzchar(salida$padre), , drop = FALSE]
  unique(salida)
}

.admon_choices_inicializar <- function() {
  if (!file.exists(.ADMON_CHOICES_RDS)) {
    stop(paste0("No existe el archivo de catálogos: ", .ADMON_CHOICES_RDS,
                ". Ejecute la migración inicial antes de iniciar la aplicación."), call. = FALSE)
  }
  invisible(TRUE)
}

.admon_choices_leer <- function() {
  .admon_choices_inicializar()
  catalogos <- readRDS(.ADMON_CHOICES_RDS)
  .admon_choices_validar(catalogos)
  catalogos
}

.admon_choices_relaciones <- function(catalogos) {
  relaciones <- attr(catalogos, "relaciones")
  if (is.null(relaciones)) return(.admon_relaciones_vacias())
  requeridas <- c("catalogo", "padre", "padre2", "opcion")
  if (!is.data.frame(relaciones) || !all(requeridas %in% names(relaciones))) {
    stop("Las relaciones del RDS tienen un formato incompatible.", call. = FALSE)
  }
  relaciones[, requeridas, drop = FALSE]
}

.admon_choices_migrar_relaciones <- function() {
  .admon_choices_inicializar()
  .admon_choices_bloqueo(function() {
    catalogos <- .admon_choices_leer()
    if (!is.null(attr(catalogos, "relaciones"))) return(invisible(FALSE))
    attr(catalogos, "relaciones") <- .admon_relaciones_fuente()
    .admon_choices_guardar(catalogos)
    invisible(TRUE)
  })
}

.admon_choices_valores <- function(catalogos, catalogo, padre = "", padre2 = "") {
  if (identical(catalogo, "detalle_origen") ||
      catalogo %in% c("categoria", "producto")) {
    relaciones <- .admon_choices_relaciones(catalogos)
    if (!nzchar(padre)) return(character())
    filas <- relaciones$catalogo == catalogo & relaciones$padre == padre
    if (identical(catalogo, "producto")) {
      if (!nzchar(padre2)) return(character())
      filas <- filas & relaciones$padre2 == padre2
    }
    return(relaciones$opcion[filas])
  }
  catalogos[[catalogo]]
}

# Edicion diferida ----
.admon_choices_agregar_local <- function(datos, catalogo, valor, padre = "", padre2 = "") {
  valor <- .admon_choices_normalizar(valor)
  if (!catalogo %in% c(names(datos), "detalle_origen")) {
    stop("El catálogo no existe.", call. = FALSE)
  }
  anidado <- catalogo %in% c("detalle_origen", "categoria", "producto")
  if (anidado) {
    padre <- .admon_choices_normalizar(padre)
    if (identical(catalogo, "producto")) padre2 <- .admon_choices_normalizar(padre2)
    origen <- if (identical(catalogo, "detalle_origen")) "origen" else "linneg"
    if (!padre %in% datos[[origen]]) stop("Seleccione un nivel superior válido.", call. = FALSE)
    if (identical(catalogo, "producto") &&
        !padre2 %in% .admon_choices_valores(datos, "categoria", padre)) {
      stop("La categoría no pertenece a esa línea.", call. = FALSE)
    }
  }
  existentes <- .admon_choices_valores(datos, catalogo, padre, padre2)
  if (valor %in% toupper(trimws(existentes))) {
    stop("La opción ya existe en este nivel.", call. = FALSE)
  }
  if (anidado) {
    relaciones <- .admon_choices_relaciones(datos)
    relaciones <- rbind(relaciones, data.frame(catalogo = catalogo,
                                               padre = padre, padre2 = padre2, opcion = valor, stringsAsFactors = FALSE))
    attr(datos, "relaciones") <- relaciones
    if (!identical(catalogo, "detalle_origen") && !valor %in% datos[[catalogo]]) {
      datos[[catalogo]] <- c(datos[[catalogo]], valor)
    }
  } else {
    datos[[catalogo]] <- c(datos[[catalogo]], valor)
  }
  datos
}

.admon_choices_mover_local <- function(datos, catalogo, indice, direccion,
                                       padre = "", padre2 = "") {
  valores <- .admon_choices_valores(datos, catalogo, padre, padre2)
  destino <- indice + if (identical(direccion, "subir")) -1L else 1L
  if (length(indice) != 1L || is.na(indice) || indice < 1L || destino < 1L ||
      destino > length(valores)) return(datos)
  if (catalogo %in% c("detalle_origen", "categoria", "producto")) {
    relaciones <- .admon_choices_relaciones(datos)
    filas <- which(relaciones$catalogo == catalogo & relaciones$padre == padre &
                     (catalogo != "producto" | relaciones$padre2 == padre2))
    relaciones[c(filas[indice], filas[destino]), ] <-
      relaciones[c(filas[destino], filas[indice]), , drop = FALSE]
    attr(datos, "relaciones") <- relaciones
  } else {
    datos[[catalogo]][c(indice, destino)] <- valores[c(destino, indice)]
  }
  datos
}

.admon_choices_eliminar_local <- function(datos, catalogo, indice,
                                          padre = "", padre2 = "") {
  valores <- .admon_choices_valores(datos, catalogo, padre, padre2)
  if (length(indice) != 1L || is.na(indice) || indice < 1L || indice > length(valores)) {
    stop("La opción seleccionada ya no existe.", call. = FALSE)
  }
  valor <- valores[[indice]]
  relaciones <- .admon_choices_relaciones(datos)
  if (identical(catalogo, "origen") &&
      any(relaciones$catalogo == "detalle_origen" & relaciones$padre == valor)) {
    stop("Elimine primero los detalles asociados al origen.", call. = FALSE)
  }
  if (identical(catalogo, "linneg") &&
      any(relaciones$catalogo == "categoria" & relaciones$padre == valor)) {
    stop("Elimine primero las categorías asociadas a la línea.", call. = FALSE)
  }
  if (identical(catalogo, "categoria") &&
      any(relaciones$catalogo == "producto" & relaciones$padre == padre &
          relaciones$padre2 == valor)) {
    stop("Elimine primero los productos asociados a esta categoría.", call. = FALSE)
  }
  if (catalogo %in% c("detalle_origen", "categoria", "producto")) {
    filas <- which(relaciones$catalogo == catalogo & relaciones$padre == padre &
                     (catalogo != "producto" | relaciones$padre2 == padre2))
    relaciones <- relaciones[-filas[indice], , drop = FALSE]
    attr(datos, "relaciones") <- relaciones
    if (catalogo %in% c("categoria", "producto") &&
        !any(relaciones$catalogo == catalogo & relaciones$opcion == valor)) {
      datos[[catalogo]] <- datos[[catalogo]][datos[[catalogo]] != valor]
    }
  } else {
    datos[[catalogo]] <- valores[-indice]
  }
  datos
}

.admon_choices_guardar_cambios <- function(nuevos, base) {
  .admon_choices_validar(nuevos)
  .admon_choices_bloqueo(function() {
    actual <- .admon_choices_leer()
    if (!identical(actual, base)) {
      stop("El RDS cambió en otra sesión. Recargue antes de guardar.", call. = FALSE)
    }
    .admon_choices_guardar(nuevos)
    invisible(TRUE)
  })
}
