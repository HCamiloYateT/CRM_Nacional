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
.in_catalogo <- function(contactos, clientes, historial, vinculos, usuario, admin, filtros, hechos = NULL,
                         directorio = NULL, asignaciones = clientes) {
  # Asignación comercial del principal; ninguna hija conserva una asignación independiente.
  mapa <- if (is.data.frame(directorio) && all(c("PerCod", "CliNitPpal") %in% names(directorio))) {
    stats::setNames(.in_id(directorio$CliNitPpal), .in_id(directorio$PerCod))
  } else character()
  asignaciones <- asignaciones %>% dplyr::group_by(CliNitPpal) %>%
    dplyr::summarise(Asesor = paste(sort(unique(stats::na.omit(Asesor))), collapse = " | "), .groups = "drop")
  principal_contacto <- unname(mapa[.in_id(.gc_columna(contactos, "PerCod"))])
  principal_contacto <- dplyr::coalesce(principal_contacto, .in_id(.gc_columna(contactos, "PerCod")))
  asignado <- asignaciones$Asesor[match(principal_contacto, .in_id(asignaciones$CliNitPpal))]
  tiene_padre <- principal_contacto %in% .in_id(asignaciones$CliNitPpal)
  contactos$Asesor <- as.character(.gc_columna(contactos, "Asesor"))
  contactos$Asesor[tiene_padre] <- asignado[tiene_padre]
  visibles <- .op_contactos_visibles(contactos, usuario, admin)
  if (!isTRUE(admin)) {
    asignado_al_usuario <- vapply(strsplit(toupper(dplyr::coalesce(contactos$Asesor, "")), " | ", fixed = TRUE),
      function(x) toupper(usuario) %in% x, logical(1))
    codigos_visibles <- union(as.character(visibles$CodContacto), as.character(contactos$CodContacto[asignado_al_usuario]))
    visibles <- contactos[as.character(contactos$CodContacto) %in% codigos_visibles, , drop = FALSE]
  }
  c <- .crm_marcas_historia(visibles)
  asesor <- .in_id(.gc_columna(c, "Asesor"))
  # Una entidad aún sin asesor conserva su ficha; una selección global vacía no incluye ninguna.
  permitidas <- length(filtros$asesor) > 0L & (is.na(asesor) |
    vapply(strsplit(dplyr::coalesce(asesor, ""), " | ", fixed = TRUE),
      function(x) any(x %in% filtros$asesor), logical(1)))
  c <- c[permitidas, , drop = FALSE]
  base <- data.frame(Clave = if (nrow(c)) paste0("contacto:", .in_id(.gc_columna(c, "CodContacto"))) else character(),
    Codigo = .in_id(.gc_columna(c, "CodContacto")), Nombre = as.character(.gc_columna(c, "PerRazSoc")),
    NIT = .in_id(.gc_columna(c, "PerCod")), Principal = rep(NA_character_, nrow(c)),
    Tipo = as.character(ifelse(c$EsReconstruido, "CLIENTE", as.character(.gc_columna(c, "Etapa", "CONTACTO")))),
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
  # Una entrada por NIT de facturación: el contacto complementa la cuenta, no la duplica.
  detalle <- attr(clientes, "presupuesto_detalle")
  cuentas <- data.frame(NIT = character(), Principal = character(), Nombre = character(), Asesor = character())
  agregar <- function(x, nit) {
    if (!is.data.frame(x) || !nrow(x)) return(invisible(NULL))
    cuentas <<- dplyr::bind_rows(cuentas, data.frame(NIT = .in_id(.gc_columna(x, nit)),
      Principal = .in_id(.gc_columna(x, "CliNitPpal")),
      Nombre = as.character(.gc_columna(x, c("RazonSocialFacturacion", "PerRazSoc"))),
      Asesor = as.character(.gc_columna(x, "Asesor"))))
  }
  agregar(detalle, "CLCliNit")
  agregar(hechos, "CLCliNit")
  cuentas <- cuentas[!is.na(cuentas$NIT), , drop = FALSE] %>%
    dplyr::group_by(NIT) %>% dplyr::summarise(Principal = .gc_unico(Principal),
      Nombre = dplyr::first(Nombre), Asesor = paste(sort(unique(stats::na.omit(Asesor))), collapse = " | "),
      .groups = "drop")
  nombres_directorio <- character()
  if (is.data.frame(directorio) && all(c("PerCod", "PerRazSoc") %in% names(directorio))) {
    nombres <- directorio %>% dplyr::transmute(NIT = .in_id(PerCod), Nombre = as.character(PerRazSoc)) %>%
      dplyr::group_by(NIT) %>% dplyr::summarise(Nombre = .gc_unico(Nombre), .groups = "drop")
    nombres_directorio <- nombres$NIT[!is.na(nombres$Nombre)]
    cuentas$Nombre <- dplyr::coalesce(nombres$Nombre[match(cuentas$NIT, nombres$NIT)], cuentas$Nombre)
  }
  cuentas$Clave <- if (nrow(cuentas)) paste0("nit:", cuentas$NIT) else character()
  cuentas$Codigo <- rep(NA_character_, nrow(cuentas))
  cuentas$Tipo <- rep("CLIENTE", nrow(cuentas))
  cuentas$Alcance <- rep("Cuenta de facturación", nrow(cuentas))
  cuentas$NitFacturacion <- cuentas$NIT
  cuentas$VinculoEnRevision <- rep(FALSE, nrow(cuentas))
  if (nrow(base)) {
    base$Clave <- ifelse(!is.na(base$NitFacturacion), paste0("nit:", base$NitFacturacion), base$Clave)
  }
  cuentas$Asesor <- asignaciones$Asesor[match(cuentas$Principal, .in_id(asignaciones$CliNitPpal))]
  base$Principal <- dplyr::coalesce(unname(mapa[base$NitFacturacion]), base$Principal)
  for (i in seq_len(nrow(cuentas))) {
    relacionados <- base[base$Clave == cuentas$Clave[[i]], , drop = FALSE]
    if (!nrow(relacionados)) next
    cuentas$Codigo[[i]] <- paste(unique(relacionados$Codigo), collapse = "|")
    nit_con_ventas <- is.data.frame(hechos) && any(.in_id(.gc_columna(hechos, "CLCliNit")) %in% cuentas$NIT[[i]])
    if (any(relacionados$Tipo == "DESCARTADO")) cuentas$Tipo[[i]] <- "DESCARTADO" else
      if (!nit_con_ventas) cuentas$Tipo[[i]] <- relacionados$Tipo[[1]]
    if (!cuentas$NIT[[i]] %in% nombres_directorio) cuentas$Nombre[[i]] <- relacionados$Nombre[[1]]
    cuentas$VinculoEnRevision[[i]] <- any(relacionados$VinculoEnRevision)
  }
  base <- base[!base$Clave %in% cuentas$Clave, , drop = FALSE]
  # Varias fichas con el mismo documento también se consultan juntas, sin perder sus directorios.
  if (nrow(base)) {
    base <- dplyr::bind_rows(lapply(split(base, base$Clave), function(x) {
      fila <- x[1, , drop = FALSE]
      fila$Codigo <- paste(unique(x$Codigo), collapse = "|")
      fila$Asesor <- paste(unique(stats::na.omit(x$Asesor)), collapse = " | ")
      fila$VinculoEnRevision <- any(x$VinculoEnRevision)
      fila
    }))
  }
  # Consolidar es una vista adicional solo cuando hay varios NIT hijos en el alcance actual.
  padres <- cuentas %>% dplyr::filter(!is.na(Principal)) %>% dplyr::group_by(Principal) %>%
    dplyr::filter(dplyr::n_distinct(NIT) > 1L) %>% dplyr::slice_head(n = 1L) %>% dplyr::ungroup()
  if (nrow(padres)) {
    padres$NIT <- padres$Principal
    padres$NitFacturacion <- padres$Principal
    padres$Clave <- paste0("principal:", padres$Principal)
    padres$Codigo <- NA_character_
    padres$Alcance <- "Cuenta principal"
    padres$Nombre <- dplyr::coalesce(clientes$PerRazSoc[match(padres$Principal, .in_id(clientes$CliNitPpal))],
      padres$Nombre)
  }
  base <- dplyr::bind_rows(cuentas, base, padres)
  base$EsPrincipal <- base$Alcance == "Cuenta principal" |
    (!is.na(base$Principal) & base$NIT == base$Principal & !base$Principal %in% padres$Principal)
  base$Nombre[is.na(base$Nombre) | !nzchar(base$Nombre)] <- "Sin nombre registrado"
  base$Etiqueta <- paste(base$Nombre, dplyr::coalesce(base$NIT, "Sin documento"),
    ifelse(base$Alcance == "Cuenta principal", "TODAS LAS CUENTAS HIJAS", base$Tipo), sep = " · ")
  base[order(base$Nombre, base$Alcance), , drop = FALSE]
}
.in_codigos <- function(entidad) {
  if (nrow(entidad) != 1L || is.na(entidad$Codigo)) return(character())
  strsplit(entidad$Codigo, "|", fixed = TRUE)[[1]]
}

.in_elegida <- function(catalogo, clave) {
  if (length(clave) != 1L || is.na(clave)) return(catalogo[0, , drop = FALSE])
  catalogo[catalogo$Clave == clave, , drop = FALSE]
}
.in_hechos <- function(hechos, entidad) {
  if (nrow(entidad) != 1L) return(hechos[0, , drop = FALSE])
  if (entidad$Alcance == "Cuenta principal") {
    filas <- .in_id(.gc_columna(hechos, "CliNitPpal")) %in% entidad$Principal
  } else if (entidad$Alcance == "Cuenta de facturación") {
    filas <- .in_id(.gc_columna(hechos, "CLCliNit")) %in% entidad$NitFacturacion
  } else {
    filas <- .in_id(.gc_columna(hechos, "CodContacto")) %in% .in_codigos(entidad)
    nit <- stats::na.omit(entidad$NitFacturacion)
    filas <- filas | .in_id(.gc_columna(hechos, "CLCliNit")) %in% nit
  }
  hechos[filas, , drop = FALSE]
}
.in_periodo <- function(hechos, fecha) {
  if (length(fecha) != 2L || anyNA(fecha) || fecha[[1]] > fecha[[2]]) stop("Revise el Período de análisis.")
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
    "TipoCompra", "CalidadesPreferidas", "EsTostador", "Maquila", "Observaciones", "FechaHoraModi", "UsuarioMod")),
  oportunidades = list(tabla = "CRMNALCLOPT", campos = c("IdOportunidad", "Categoria", "Producto", "LineaNegocio",
    "SacosOP", "Sacos", "MargenOP", "Margen", "FechaCumpOP", "Descartado", "UsuarioCrea", "LinNegCod",
    "FechaHoraCrea", "FechaHoraModi", "FrecuenciaDias", "Descripcion")),
  gestiones = list(tabla = "CRMNALRELACIONAMIENTO", campos = c("FechaHoraCrea", "TipoGestion", "TipoRelacionamiento",
    "Descripcion", "Observacion", "Comentario", "UsuarioCrea")),
  recordatorios = list(tabla = "CRMNALRECORDATORIO", campos = c("FechaRecordatorio", "FechaCumplimiento",
    "Descripcion", "Cumplido", "Canal", "Mensaje", "Enviado", "UsuarioCrea")),
  etapas = list(tabla = "CRMNALHISTORIALETAPA", campos = c("FechaHora", "EtapaAnterior", "EtapaNueva", "Usuario")),
  conversion_lead = list(tabla = "CONTACTOLEAD", campos = c("FechaConversion", "Segmento", "LinNegocio", "Asesor")),
  conversion_cliente = list(tabla = "CRMNALLEADCLIENTE", campos = c("FechaConversion", "NitFacturacion")),
  alianzas = list(tabla = "CRMNALPROSPECTOALIANZA", campos = c("FechaHoraCrea", "CodClienteAliado", "Observacion")),
  descartes = list(tabla = "CRMNALDESCARTE", campos = c("FechaHoraModi", "Etapa", "Razon1", "Razon2", "Razon3")))
.in_detalle_crm <- function(fuente, entidad, campos) {
  if (!is.null(fuente$error)) {
    message("[Consulta individual] ", fuente$error)
    shiny::validate(shiny::need(FALSE, "Esta información no está disponible. Intente actualizar."))
  }
  x <- fuente$datos
  if (nrow(entidad) != 1L || is.na(entidad$Codigo)) return(data.frame())
  if (!nrow(x)) return(x)
  if (!"CodContacto" %in% names(x)) stop("La fuente no identifica la entidad comercial.")
  x <- x[.in_id(x$CodContacto) %in% .in_codigos(entidad), , drop = FALSE]
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
    any(.in_id(x$CodContacto) %in% strsplit(codigo, "|", fixed = TRUE)[[1]])
}
