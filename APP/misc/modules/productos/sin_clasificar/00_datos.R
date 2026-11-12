# Clasificación de referencias comerciales sin alterar cantidades ni historia ----
.CLAS_CLAVES <- c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
.clas_vacio <- function(x) {
  texto <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(trimws(as.character(x))))
  is.na(x) | texto %in% c("", "SIN DATO", "SIN DATOS", "SIN CLASIFICAR", "SIN CATEGORIA",
    "SIN PRODUCTO", "SIN INFORMACION", "N/A", "NA", "NULL", "S/D")
}
.clas_validar <- function(datos) {
  requeridas <- c(.CLAS_CLAVES, "Categoria", "Producto")
  if (!is.data.frame(datos) || !all(requeridas %in% names(datos))) {
    stop("No fue posible leer el catálogo de productos. Intente actualizar.")
  }
  datos
}
.clas_llave <- function(x) do.call(paste, c(lapply(x[.CLAS_CLAVES], as.character), sep = "|"))
.clas_pendientes <- function(datos, operaciones = list()) {
  catalogo <- .crm_snapshot_productos(.clas_validar(datos))
  if (is.data.frame(operaciones)) operaciones <- list(operaciones)
  normalizar <- function(x) {
    if (!is.data.frame(x) || !nrow(x)) return(NULL)
    referencias <- data.frame(LinNegCod = .gc_columna(x, c("LinNegCodPedido", "LinNegCod")),
      LinProCod = .gc_columna(x, c("LinProCodPedido", "LinProCod")),
      MCCod = .gc_columna(x, c("NombreComercialPedidoCod", "MCCod", "CodigoNombreComercialLote", "CLMCCod")),
      MrcCod = .gc_columna(x, c("MarcaPedidoCod", "MrcCod", "CodigoMarcaLote", "CLMrcCod")),
      Marca = .gc_columna(x, c("MarcaPedido", "Marca", "CLMrcNom")),
      MCNom = .gc_columna(x, c("NombreComercialPedido", "MCNom", "CLMCNom", "NombreComercial")),
      LinProNom = .gc_columna(x, c("LinProNom", "CLLinProNo")),
      LinNeg = .gc_columna(x, c("LinNeg", "CLLinNegNo")),
      Lote = as.character(.gc_columna(x, "CLLotCod")), stringsAsFactors = FALSE)
    referencias$Referencia <- .clas_llave(referencias)
    referencias
  }
  observadas <- dplyr::bind_rows(lapply(operaciones, normalizar))
  catalogo$Referencia <- .clas_llave(catalogo)
  catalogo$EnCatalogo <- TRUE
  if (nrow(observadas)) {
    nuevas <- observadas[!observadas$Referencia %in% catalogo$Referencia, , drop = FALSE]
    nuevas <- nuevas[!duplicated(nuevas$Referencia), , drop = FALSE]
    if (nrow(nuevas)) {
      filas <- catalogo[rep(NA_integer_, nrow(nuevas)), , drop = FALSE]
      for (nombre in intersect(names(nuevas), names(filas))) {
        if (is.numeric(catalogo[[nombre]])) filas[[nombre]] <- suppressWarnings(as.numeric(nuevas[[nombre]])) else {
          filas[[nombre]] <- as.character(nuevas[[nombre]])
        }
      }
      filas$Categoria <- NA_character_
      filas$Producto <- NA_character_
      filas$EnCatalogo <- FALSE
      catalogo <- dplyr::bind_rows(catalogo, filas)
    }
  }
  if (nrow(observadas)) {
    indice <- match(catalogo$Referencia, observadas$Referencia)
    for (nombre in intersect(c("Marca", "MCNom", "LinProNom", "LinNeg"), names(catalogo))) {
      faltantes <- .clas_vacio(catalogo[[nombre]]) & !is.na(indice)
      catalogo[[nombre]][faltantes] <- observadas[[nombre]][indice[faltantes]]
    }
  }
  x <- catalogo[.clas_vacio(catalogo$Categoria) | .clas_vacio(catalogo$Producto), , drop = FALSE]
  x$NumeroLotes <- 0L
  if (nrow(observadas)) {
    lotes <- unique(observadas[!.clas_vacio(observadas$Lote), c("Referencia", "Lote"), drop = FALSE])
    conteo <- table(lotes$Referencia)
    x$NumeroLotes <- as.integer(conteo[match(x$Referencia, names(conteo))])
    x$NumeroLotes[is.na(x$NumeroLotes)] <- 0L
  }
  x$Marca <- .gc_columna(x, "Marca", "Sin marca registrada")
  x$NombreComercial <- .gc_columna(x, "MCNom", "Sin nombre comercial")
  x$Presentacion <- .gc_columna(x, "LinProNom", "Sin presentación registrada")
  x$Linea <- .gc_columna(x, c("LinNeg", "CLLinNegNo"))
  x[order(-x$NumeroLotes, x$Marca, x$NombreComercial), , drop = FALSE]
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
  nueva <- !nrow(actual) && identical(fila$EnCatalogo, FALSE)
  if (nueva) {
    actual <- fuente[NA_integer_, , drop = FALSE]
    for (nombre in intersect(names(fuente), names(fila))) actual[[nombre]] <- fila[[nombre]]
    if ("FecProceso" %in% names(actual)) {
      actual$FecProceso <- if (inherits(fuente$FecProceso, "Date")) Sys.Date() else {
        if (is.character(fuente$FecProceso)) format(Sys.time(), "%Y-%m-%d %H:%M:%S") else Sys.time()
      }
    }
  }
  if (nrow(actual) != 1L) stop("La referencia cambió o tiene registros contradictorios. Actualice antes de guardar.")
  if (!nueva && (!identical(as.character(actual$Categoria), as.character(fila$Categoria)) ||
      !identical(as.character(actual$Producto), as.character(fila$Producto)))) {
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
    original = if (nueva) NULL else fila[c("Categoria", "Producto")])
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
    if (is.null(original)) {
      condiciones_nueva <- condiciones[names(llaves) %in% .CLAS_CLAVES]
      existe <- DBI::dbGetQuery(con, paste0("SELECT Categoria FROM CRMNALPRODS WHERE ",
        paste(condiciones_nueva, collapse = " AND "), " FOR UPDATE"))
      if (nrow(existe)) stop("Otra persona registró esta referencia. Actualice antes de guardar.")
      campos <- intersect(names(fila), DBI::dbListFields(con, tabla))
      DBI::dbAppendTable(con, tabla, fila[campos])
    } else {
      actual <- DBI::dbGetQuery(con, paste0("SELECT Categoria, Producto FROM CRMNALPRODS WHERE ", donde,
        " FOR UPDATE"))
      if (nrow(actual) != 1L || !identical(as.character(actual$Categoria), as.character(original$Categoria)) ||
          !identical(as.character(actual$Producto), as.character(original$Producto))) {
        stop("Otra persona cambió esta clasificación. Actualice antes de guardar.")
      }
      campos <- intersect(c("Categoria", "Producto", "UsuarioMod", "FechaHoraModi"), names(fila))
      cambios <- vapply(campos, function(n) paste(columna(n), "=", literal(fila[[n]][[1]])), character(1))
      DBI::dbExecute(con, paste0("UPDATE CRMNALPRODS SET ", paste(cambios, collapse = ", "), " WHERE ", donde))
    }
  })
  invisible(TRUE)
}
