# Identidad comercial: códigos y NIT, nunca coincidencias por nombre ----
.in_id <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE, digits = 22, big.mark = "")
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("", "NA", "0")] <- NA_character_
  x
}
.in_fuente <- function(cargar, tabla) {
  tryCatch({
    datos <- cargar(tabla)
    if (!is.data.frame(datos)) stop("La fuente no entregó una tabla.")
    list(datos = datos, error = NULL)
  }, error = function(e) list(datos = data.frame(), error = paste(tabla, conditionMessage(e), sep = ": ")))
}
.in_catalogo <- function(contactos, clientes, historial, vinculos, usuario, admin, filtros, hechos = NULL) {
  c <- .op_contactos_visibles(contactos, usuario, admin)
  asesor <- .in_id(.gc_columna(c, "Asesor"))
  # Una entidad aún sin asesor conserva su ficha; una selección global vacía no incluye ninguna.
  permitidas <- length(filtros$asesor) > 0L & (asesor %in% filtros$asesor | is.na(asesor))
  c <- c[permitidas, , drop = FALSE]
  base <- data.frame(Clave = if (nrow(c)) paste0("contacto:", .in_id(.gc_columna(c, "CodContacto"))) else character(),
    Codigo = .in_id(.gc_columna(c, "CodContacto")), Nombre = as.character(.gc_columna(c, "PerRazSoc")),
    NIT = .in_id(.gc_columna(c, "PerCod")), Principal = rep(NA_character_, nrow(c)),
    Tipo = as.character(.gc_columna(c, "Etapa", "CONTACTO")),
    Asesor = as.character(.gc_columna(c, "Asesor")), Alcance = rep("Entidad", nrow(c)), stringsAsFactors = FALSE)
  base <- base[!is.na(base$Codigo), , drop = FALSE]
  if (anyDuplicated(base$Clave)) stop("Hay códigos de contacto duplicados. Revise el catálogo.")
  if (nrow(historial) && all(c("CodContacto", "EtapaNueva", "FechaHora") %in% names(historial))) {
    h <- historial[!is.na(historial$FechaHora), , drop = FALSE]
    h <- h[order(h$FechaHora, decreasing = TRUE), , drop = FALSE]
    etapa <- as.character(h$EtapaNueva[match(base$Codigo, .in_id(h$CodContacto))])
    base$Tipo <- dplyr::coalesce(etapa, base$Tipo)
  }
  if ("Estado" %in% names(c)) {
    base$Tipo[base$Codigo %in% .in_id(c$CodContacto[c$Estado == "DESCARTADO"])] <- "DESCARTADO"
  }
  base$NitFacturacion <- base$NIT
  base$VinculoEnRevision <- rep(FALSE, nrow(base))
  if (nrow(vinculos) && all(c("CodContacto", "NitVinculado") %in% names(vinculos))) {
    for (i in seq_len(nrow(base))) {
      ids <- unique(stats::na.omit(.in_id(vinculos$NitVinculado[
        .in_id(vinculos$CodContacto) %in% base$Codigo[[i]]])))
      if (length(ids) == 1L) base$NitFacturacion[[i]] <- ids
      if (length(ids) > 1L) {
        base$NitFacturacion[[i]] <- NA_character_
        base$VinculoEnRevision[[i]] <- TRUE
      }
    }
  }
  # La cuenta principal es otra vista: agrupa sucursales autorizadas, sin duplicar sus movimientos.
  principales <- clientes %>% dplyr::group_by(CliNitPpal) %>%
    dplyr::summarise(Nombre = dplyr::first(PerRazSoc), Asesor = paste(sort(unique(Asesor)), collapse = " | "),
      .groups = "drop")
  if (is.data.frame(hechos) && nrow(hechos)) {
    cuentas <- data.frame(CliNitPpal = .in_id(hechos$CliNitPpal),
      PerRazSoc = as.character(.gc_columna(hechos, "PerRazSoc")),
      Asesor = as.character(.gc_columna(hechos, "Asesor")))
    cuentas <- cuentas[!is.na(cuentas$CliNitPpal), , drop = FALSE] %>%
      dplyr::group_by(CliNitPpal) %>% dplyr::summarise(Nombre = dplyr::first(PerRazSoc),
        Asesor = paste(sort(unique(Asesor)), collapse = " | "), .groups = "drop")
    principales$CliNitPpal <- .in_id(principales$CliNitPpal)
    principales$Nombre <- dplyr::coalesce(principales$Nombre,
      cuentas$Nombre[match(principales$CliNitPpal, cuentas$CliNitPpal)])
    principales <- dplyr::bind_rows(principales,
      cuentas[!cuentas$CliNitPpal %in% principales$CliNitPpal, , drop = FALSE])
  }
  p <- data.frame(Clave = if (nrow(principales)) paste0("principal:", .in_id(principales$CliNitPpal)) else character(),
    Codigo = rep(NA_character_, nrow(principales)),
    Nombre = principales$Nombre, NIT = .in_id(principales$CliNitPpal),
    Principal = .in_id(principales$CliNitPpal), Tipo = rep("CLIENTE", nrow(principales)), Asesor = principales$Asesor,
    Alcance = rep("Cuenta principal", nrow(principales)), NitFacturacion = .in_id(principales$CliNitPpal),
    VinculoEnRevision = rep(FALSE, nrow(principales)))
  base <- dplyr::bind_rows(base, p)
  base$Nombre[is.na(base$Nombre) | !nzchar(base$Nombre)] <- "Sin nombre registrado"
  base$Etiqueta <- paste(base$Nombre, dplyr::coalesce(base$NIT, "Sin NIT"), base$Tipo, base$Alcance, sep = " · ")
  base[order(base$Nombre, base$Alcance), , drop = FALSE]
}
.in_elegida <- function(catalogo, clave) {
  if (length(clave) != 1L || is.na(clave)) return(catalogo[0, , drop = FALSE])
  catalogo[catalogo$Clave == clave, , drop = FALSE]
}
.in_hechos <- function(hechos, entidad) {
  if (nrow(entidad) != 1L) return(hechos[0, , drop = FALSE])
  if (entidad$Alcance == "Cuenta principal") {
    filas <- .in_id(.gc_columna(hechos, "CliNitPpal")) %in% entidad$Principal
  } else {
    filas <- .in_id(.gc_columna(hechos, "CodContacto")) %in% entidad$Codigo
    nit <- stats::na.omit(entidad$NitFacturacion)
    filas <- filas | .in_id(.gc_columna(hechos, "CLCliNit")) %in% nit
  }
  hechos[filas, , drop = FALSE]
}
.in_periodo <- function(hechos, fecha) {
  if (length(fecha) != 2L || anyNA(fecha) || fecha[[1]] > fecha[[2]]) stop("Revise el periodo del controlbar.")
  hechos[!is.na(hechos$FecFact) & hechos$FecFact >= fecha[[1]] & hechos$FecFact <= fecha[[2]] &
    hechos$EstadoFinanciero %in% c("CERRADO", "FACTURADO_ABIERTO"), , drop = FALSE]
}
.in_resumen <- function(hechos) {
  list(sacos = if (nrow(hechos)) .com_suma(hechos$SacosFact) else 0,
    margen = if (nrow(hechos)) .com_suma(hechos$Margen) else 0,
    pedidos = length(unique(stats::na.omit(.in_id(.gc_columna(hechos, "CLPdcCod"))))),
    productos = length(unique(stats::na.omit(.in_id(.gc_columna(hechos, "Producto"))))))
}
# Solo campos útiles para consulta; no exponer columnas internas de las tablas CRM ----
.IN_FUENTES <- list(
  directorio = list(tabla = "CRMNALDIRECTORIOEMPRESARIAL", campos = c("CorreoGeneral", "PaginaWeb")),
  telefonos = list(tabla = "CRMNALDIRECTORIOTELEFONO", campos = c("Telefono", "Numero", "TipoTelefono")),
  redes = list(tabla = "CRMNALDIRECTORIORED", campos = c("TipoRedSocial", "UsuarioRed")),
  sucursales = list(tabla = "CRMNALSUCURSALES", campos = c("NombreSucursal", "Nombre", "Direccion", "Municipio",
    "Departamento", "Depto", "Mpio", "Localidad", "Barrio", "Tipo", "EsPrincipal", "Telefono", "Correo", "Contacto")),
  personas = list(tabla = "CRMNALDIRECTORIOPERSONA", campos = c("Nombre", "Cargo", "Correo", "Telefono")),
  ubicaciones = list(tabla = "CRMNALUBICACION", campos = c("Departamento", "Municipio", "Depto", "Mpio", "Direccion")),
  potencial = list(tabla = "CRMNALPOTENCIAL", campos = c("ConsumoEsperado", "UnidadConsumo", "FrecuenciaCompra",
    "TipoCompra", "CalidadesPreferidas", "EsTostador", "Maquila", "Observaciones")),
  oportunidades = list(tabla = "CRMNALCLOPT", campos = c("IdOportunidad", "Categoria", "Producto", "LineaNegocio",
    "SacosOP", "Sacos", "MargenOP", "Margen", "FechaCumpOP", "Descartado", "UsuarioCrea")),
  gestiones = list(tabla = "CRMNALRELACIONAMIENTO", campos = c("FechaHoraCrea", "TipoGestion", "TipoRelacionamiento",
    "Descripcion", "Observacion", "Comentario", "UsuarioCrea")),
  recordatorios = list(tabla = "CRMNALRECORDATORIO", campos = c("FechaRecordatorio", "FechaCumplimiento",
    "Descripcion", "Cumplido", "Canal", "Mensaje", "Enviado", "UsuarioCrea")),
  etapas = list(tabla = "CRMNALHISTORIALETAPA", campos = c("FechaHora", "EtapaAnterior", "EtapaNueva", "Usuario")))
.in_detalle_crm <- function(fuente, entidad, campos) {
  if (!is.null(fuente$error)) {
    message("[Consulta individual] ", fuente$error)
    shiny::validate(shiny::need(FALSE, "Esta información no está disponible. Intente actualizar."))
  }
  x <- fuente$datos
  if (nrow(entidad) != 1L || is.na(entidad$Codigo)) return(data.frame())
  if (!nrow(x)) return(x)
  if (!"CodContacto" %in% names(x)) stop("La fuente no identifica la entidad comercial.")
  x <- x[.in_id(x$CodContacto) %in% entidad$Codigo, , drop = FALSE]
  columnas <- intersect(campos, names(x))
  if (nrow(x) && !length(columnas)) stop("La fuente no contiene los campos de consulta esperados.")
  .crm_nits_tabla(x[columnas])
}
.in_presupuesto <- function(clientes, entidad) {
  detalle <- attr(clientes, "presupuesto_detalle")
  if (is.null(detalle) || nrow(entidad) != 1L) return(data.frame())
  filas <- if (entidad$Alcance == "Cuenta principal") {
    .in_id(detalle$CliNitPpal) %in% entidad$Principal
  } else .in_id(detalle$CLCliNit) %in% stats::na.omit(entidad$NitFacturacion)
  x <- detalle[filas, , drop = FALSE]
  names(x)[names(x) == "SSPpto"] <- "MetaMensualSacos"
  names(x)[names(x) == "MNFCCPpto"] <- "MetaMensualMargen"
  .crm_nits_tabla(x[intersect(c("CLCliNit", "CLLinNegNo", "Asesor", "Segmento", "FecProceso",
    "MetaMensualSacos", "MetaMensualMargen", "PresupuestoEnRevision"), names(x))])
}

.in_tiene_detalle <- function(fuente, codigo) {
  x <- fuente$datos
  !is.na(codigo) && is.data.frame(x) && "CodContacto" %in% names(x) &&
    any(.in_id(x$CodContacto) %in% codigo)
}
