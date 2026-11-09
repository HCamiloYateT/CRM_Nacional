# Clasificación de referencias comerciales sin alterar cantidades ni historia ----
.CLAS_CLAVES <- c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
.clas_vacio <- function(x) is.na(x) | toupper(trimws(as.character(x))) %in% c("", "SIN DATO", "SIN CLASIFICAR")
.clas_validar <- function(datos) {
  requeridas <- c(.CLAS_CLAVES, "Categoria", "Producto")
  if (!is.data.frame(datos) || !all(requeridas %in% names(datos))) {
    stop("No fue posible leer el catálogo de productos. Intente actualizar.")
  }
  datos
}
.clas_llave <- function(x) do.call(paste, c(lapply(x[.CLAS_CLAVES], as.character), sep = "|"))
.clas_pendientes <- function(datos) {
  x <- .crm_snapshot_productos(.clas_validar(datos))
  x <- x[.clas_vacio(x$Categoria) | .clas_vacio(x$Producto), , drop = FALSE]
  x$Referencia <- .clas_llave(x)
  x$Marca <- .gc_columna(x, "Marca", "Sin marca registrada")
  x$NombreComercial <- .gc_columna(x, "MCNom", "Sin nombre comercial")
  x$Presentacion <- .gc_columna(x, "LinProNom", "Sin presentación registrada")
  x$Linea <- .gc_columna(x, c("LinNeg", "CLLinNegNo"))
  x
}
.clas_opciones <- function(datos, fila, categoria = NULL) {
  x <- .clas_validar(datos)
  if (nrow(fila) != 1L) return(character())
  x <- x[x$LinNegCod %in% fila$LinNegCod & !.clas_vacio(x$Categoria) & !.clas_vacio(x$Producto), , drop = FALSE]
  opciones <- if (is.null(categoria)) as.character(x$Categoria) else as.character(x$Producto[x$Categoria %in% categoria])
  linea <- as.character(.gc_columna(fila, c("LinNeg", "CLLinNegNo", "Linea")))
  if (is.na(linea) || !nzchar(linea)) {
    linea <- unname(c(`10000` = "CONVENCIONALES", `21000` = "A LA MEDIDA", `20000` = "DIFERENCIADOS")[
      as.character(fila$LinNegCod)])
  }
  if (exists("ChoicesAnidadas", mode = "function") && !is.na(linea)) {
    opciones <- c(opciones, if (is.null(categoria)) ChoicesAnidadas("categoria", linea) else {
      ChoicesAnidadas("producto", linea, categoria)
    })
  }
  sort(unique(opciones[!.clas_vacio(opciones)]))
}
.clas_guardar <- function(fila, categoria, producto, usuario, cargar, guardar) {
  if (nrow(fila) != 1L || anyNA(fila[.CLAS_CLAVES])) stop("Seleccione una referencia con sus códigos completos.")
  if (length(usuario) != 1L || is.na(usuario) || !nzchar(usuario) || usuario == "SIN IDENTIFICAR") {
    stop("No fue posible identificar al usuario.")
  }
  # Releer al confirmar: no sobrescribir una clasificación que otra persona haya completado.
  fuente <- .clas_validar(cargar("CRMNALPRODS"))
  actual <- fuente[.clas_llave(fuente) == .clas_llave(fila), , drop = FALSE]
  if ("FecProceso" %in% names(actual) && nrow(actual)) {
    fechas <- as.POSIXct(actual$FecProceso, tz = "UTC")
    if (any(!is.na(fechas))) actual <- actual[which(fechas == max(fechas, na.rm = TRUE)), , drop = FALSE]
  }
  actual <- dplyr::distinct(actual)
  if (nrow(actual) != 1L) stop("La referencia cambió o tiene registros contradictorios. Actualice antes de guardar.")
  if (!identical(as.character(actual$Categoria), as.character(fila$Categoria)) ||
      !identical(as.character(actual$Producto), as.character(fila$Producto))) {
    stop("Otra persona cambió esta clasificación. Actualice para ver sus cambios.")
  }
  if (length(categoria) != 1L || !categoria %in% .clas_opciones(fuente, actual) ||
      length(producto) != 1L || !producto %in% .clas_opciones(fuente, actual, categoria)) {
    stop("Seleccione una categoría y un producto válidos para esta línea de negocio.")
  }
  actual$Categoria <- categoria
  actual$Producto <- producto
  if ("UsuarioMod" %in% names(actual)) actual$UsuarioMod <- usuario
  if ("FechaHoraModi" %in% names(actual)) actual$FechaHoraModi <- Sys.time()
  claves <- c(.CLAS_CLAVES, if ("FecProceso" %in% names(actual)) "FecProceso")
  guardar(actual, "CRMNALPRODS", llaves = as.list(actual[1, claves, drop = FALSE]),
    original = fila[c("Categoria", "Producto")])
  .crm_snapshot_productos(.clas_validar(cargar("CRMNALPRODS")))
}

# Actualizar solo la clasificación, con bloqueo de la referencia durante la escritura ----
.clas_actualizar_bd <- function(fila, tabla, llaves, original) {
  if (!identical(tabla, "CRMNALPRODS") || !all(.CLAS_CLAVES %in% names(llaves)) ||
      !all(names(llaves) %in% c(.CLAS_CLAVES, "FecProceso")) || nrow(fila) != 1L) {
    stop("La referencia de producto no es válida.")
  }
  con <- racafeBD::ConectarBD()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  columna <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  literal <- function(x) as.character(DBI::dbQuoteLiteral(con, x))
  condiciones <- vapply(names(llaves), function(n) {
    if (is.na(llaves[[n]])) paste(columna(n), "IS NULL") else paste(columna(n), "=", literal(llaves[[n]]))
  }, character(1))
  donde <- paste(condiciones, collapse = " AND ")
  DBI::dbWithTransaction(con, {
    actual <- DBI::dbGetQuery(con, paste0("SELECT Categoria, Producto FROM CRMNALPRODS WHERE ", donde,
      " FOR UPDATE"))
    if (nrow(actual) != 1L || !identical(as.character(actual$Categoria), as.character(original$Categoria)) ||
        !identical(as.character(actual$Producto), as.character(original$Producto))) {
      stop("Otra persona cambió esta clasificación. Actualice antes de guardar.")
    }
    campos <- intersect(c("Categoria", "Producto", "UsuarioMod", "FechaHoraModi"), names(fila))
    cambios <- vapply(campos, function(n) paste(columna(n), "=", literal(fila[[n]][[1]])), character(1))
    DBI::dbExecute(con, paste0("UPDATE CRMNALPRODS SET ", paste(cambios, collapse = ", "), " WHERE ", donde))
  })
  invisible(TRUE)
}
