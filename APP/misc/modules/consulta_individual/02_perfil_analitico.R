# Datos de relación, presupuesto y antigüedad de la entidad seleccionada ----
.in_fecha <- function(x) {
  if (inherits(x, "Date")) y <- x else if (inherits(x, "POSIXt")) {
    y <- as.Date(x, tz = "America/Bogota")
  } else {
    y <- as.Date(rep(NA_character_, length(x)))
    if (is.character(x) || is.factor(x)) for (formato in c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y")) {
      pendientes <- is.na(y)
      y[pendientes] <- suppressWarnings(as.Date(as.character(x[pendientes]), format = formato))
    }
  }
  y[!is.na(y) & y <= as.Date("1900-01-01")] <- as.Date(NA)
  y
}
.in_facturacion_historica <- function(hechos, corte) {
  .com_hechos(hechos, as.Date(corte), cerrados = FALSE)
}
.in_edad <- function(fecha, corte) {
  fecha <- .in_fecha(fecha)
  fecha <- fecha[!is.na(fecha) & fecha <= corte]
  if (!length(fecha)) return("Sin fecha registrada")
  meses <- lubridate::interval(min(fecha), corte) %/% lubridate::period(1, units = "month")
  paste(meses %/% 12, "años y", meses %% 12, "meses")
}
.in_estado_presupuesto <- function(metas, entidad, corte) {
  x <- .in_presupuesto(metas, entidad)
  anho <- format(as.Date(corte), "%Y")
  if (!nrow(x)) return(paste("Sin presupuesto registrado para", anho))
  if (any(x$PresupuestoEnRevision %in% TRUE) || anyNA(x$MetaMensualSacos) || anyNA(x$MetaMensualMargen)) {
    return(paste("Presupuesto", anho, "por revisar"))
  }
  if (any(x$MetaMensualSacos > 0 | x$MetaMensualMargen > 0)) return(paste("Con presupuesto", anho))
  paste("Sin meta asignada en", anho)
}
.in_familia <- function(entidad, catalogo, mapa, directorio) {
  p <- .in_id(entidad$Principal)
  if (is.na(p)) {
    candidatos <- unique(stats::na.omit(.in_id(mapa$CliNitPpal[.in_id(mapa$PerCod) %in% entidad$NitFacturacion])))
    if (length(candidatos) == 1L) p <- candidatos
  }
  cuentas <- catalogo[catalogo$Alcance == "Cuenta de facturación" & !is.na(catalogo$NIT), , drop = FALSE]
  miembros <- if (is.na(p)) cuentas[0, , drop = FALSE] else {
    cuentas[!is.na(cuentas$Principal) & cuentas$Principal == p, , drop = FALSE]
  }
  hijos <- unique(miembros$NIT[miembros$NIT != p])
  nombre <- if (!is.na(p) && all(c("PerCod", "PerRazSoc") %in% names(directorio))) {
    .gc_unico(as.character(directorio$PerRazSoc[.in_id(directorio$PerCod) %in% p]))
  } else NA_character_
  if (is.na(nombre) && !is.na(p)) nombre <- .gc_unico(catalogo$Nombre[catalogo$NIT %in% p])
  es_hijo <- !is.na(p) && !is.na(entidad$NIT) && entidad$NIT != p
  relacion <- if (is.na(p)) "Sin cuenta padre registrada" else if (es_hijo) {
    paste("Cuenta hija de", dplyr::coalesce(nombre, p), "· NIT", p)
  } else if (!length(hijos)) "Cuenta sin hijas visibles" else {
    paste("Cuenta padre ·", length(hijos), if (length(hijos) == 1L) "cuenta hija visible" else "cuentas hijas visibles")
  }
  tabla <- data.frame(NIT = miembros$NIT, Cuenta = miembros$Nombre,
    Relacion = ifelse(miembros$NIT == p, "Cuenta padre", "Cuenta hija"), Asesor = miembros$Asesor)
  list(principal = p, nombre = nombre, hijos = hijos, relacion = relacion, cuentas = tabla)
}
.in_contexto_crm <- function(entidad, catalogo) {
  entidad$OrigenCRM <- "Propio"
  if (entidad$Alcance != "Cuenta principal" && !is.na(entidad$Codigo)) return(entidad)
  p <- entidad$Principal
  if (is.na(p)) return(entidad)
  if (entidad$Alcance == "Cuenta principal") {
    filas <- catalogo[catalogo$Alcance != "Cuenta principal" &
      ((!is.na(catalogo$Principal) & catalogo$Principal == p) | catalogo$NIT %in% p), , drop = FALSE]
    entidad$OrigenCRM <- "Consolidado"
  } else {
    filas <- catalogo[catalogo$Alcance != "Cuenta principal" & catalogo$NIT %in% p, , drop = FALSE]
    entidad$OrigenCRM <- "Cuenta padre"
  }
  codigos <- unique(unlist(lapply(seq_len(nrow(filas)), function(i) .in_codigos(filas[i, , drop = FALSE]))))
  entidad$Codigo <- if (length(codigos)) paste(codigos, collapse = "|") else NA_character_
  entidad
}
# La información de NCLIENTE está identificada por el NIT real, sin heredar teléfonos del padre al hijo.
.in_datos_cliente <- function(entidad, familia, directorio, hechos) {
  nits <- if (entidad$Alcance == "Cuenta principal") c(familia$cuentas$NIT, entidad$NIT) else entidad$NitFacturacion
  nits <- unique(stats::na.omit(nits))
  vacio <- data.frame(NIT = character(), Cuenta = character(), Contacto = character(), Telefono = character(),
    Correo = character(), Direccion = character(), Ciudad = character(), Departamento = character(),
    Region = character(), Pais = character(), Registro = as.Date(character()))
  if (!length(nits)) return(vacio)
  filas <- lapply(nits, function(nit) {
    d <- if ("PerCod" %in% names(directorio)) {
      directorio[.in_id(directorio$PerCod) %in% nit, , drop = FALSE]
    } else data.frame()
    # El ETL antiguo guarda el contacto del principal en las filas de sus hijos: no usarlo como contacto del hijo.
    h <- hechos[.in_id(hechos$CLCliNit) %in% nit & .in_id(hechos$CliNitPpal) %in% nit, , drop = FALSE]
    texto <- function(columnas) {
      valores <- c(as.character(.gc_columna(d, columnas)), as.character(.gc_columna(h, columnas)))
      valores <- unique(trimws(valores[!is.na(valores) & nzchar(trimws(valores)) & valores != "."]))
      if (length(valores)) paste(valores, collapse = " | ") else NA_character_
    }
    data.frame(NIT = nit, Cuenta = dplyr::coalesce(texto(c("PerRazSoc", "RazonSocialCliNit")), entidad$Nombre),
      Contacto = texto(c("CliConCom", "CliCont", "Contacto")),
      Telefono = texto(c("CliTelCom", "CliTel", "Telefono")),
      Correo = texto(c("CliEmlCom", "Correo", "CorreoGeneral")),
      Direccion = texto(c("CliDir", "CliDir1", "Direccion")),
      Ciudad = texto(c("CiuExtNom", "Mpio", "Municipio")), Departamento = texto(c("Depto", "Departamento")),
      Region = texto(c("Region", "Región")), Pais = texto(c("Pais", "País")),
      Registro = .gc_fecha_min(.in_fecha(.gc_columna(d, c("FecCreacion", "PerFecCre")))))
  })
  dplyr::bind_rows(vacio, filas)
}
.in_antiguedad <- function(entidad, datos_cliente, contactos, hechos, corte) {
  c <- contactos[.in_id(.gc_columna(contactos, "CodContacto")) %in% .in_codigos(entidad), , drop = FALSE]
  fechas_registro <- datos_cliente$Registro[datos_cliente$NIT %in% entidad$NIT]
  registro <- .gc_fecha_min(fechas_registro[!is.na(fechas_registro) & fechas_registro <= corte])
  alta_crm <- .gc_fecha_min(.in_fecha(.gc_columna(c, "FechaHoraCrea")))
  positivo <- hechos[is.finite(hechos$SacosFact) & hechos$SacosFact > 0, , drop = FALSE]
  primera <- .gc_fecha_min(positivo$FecFact)
  ultima <- .gc_fecha_max(positivo$FecFact)
  data.frame(Indicador = c("Registrado como cliente desde", "Antigüedad del registro de cliente",
    "Primer contacto comercial vinculado en CRM", "Primera compra observada", "Antigüedad comercial observada",
    "Última compra observada"),
    Informacion = c(as.character(registro), .in_edad(registro, corte), as.character(alta_crm),
      as.character(primera), .in_edad(primera, corte), as.character(ultima)))
}
.in_historico_anual <- function(hechos) {
  hechos %>% dplyr::mutate(Anho = as.integer(format(FecFact, "%Y"))) %>% dplyr::group_by(Anho) %>%
    dplyr::summarise(Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen),
      Pedidos = dplyr::n_distinct(CLPdcCod, na.rm = TRUE),
      DiasConCompra = dplyr::n_distinct(FecFact[SacosFact > 0], na.rm = TRUE),
      .groups = "drop")
}
.in_preferencias <- function(hechos) {
  x <- hechos
  x$Marca <- as.character(.gc_columna(x, "Marca"))
  x$NombreComercial <- as.character(.gc_columna(x, "MCNom"))
  x %>% dplyr::group_by(CLLinNegNo, Categoria, Producto, Marca, NombreComercial) %>%
    dplyr::summarise(Sacos = .com_suma(SacosFact), Margen = .com_suma(Margen),
      DiasConCompra = dplyr::n_distinct(FecFact[SacosFact > 0], na.rm = TRUE),
      PrimeraCompra = .gc_fecha_min(FecFact), UltimaCompra = .gc_fecha_max(FecFact),
      .groups = "drop") %>% dplyr::arrange(dplyr::desc(Sacos))
}

.in_instante <- function(x) {
  if (inherits(x, "POSIXt")) y <- as.POSIXct(x) else if (inherits(x, "Date")) {
    y <- as.POSIXct(as.character(x), tz = "America/Bogota")
  } else {
    y <- as.POSIXct(rep(NA_character_, length(x)), tz = "America/Bogota")
    formatos <- c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y")
    if (is.character(x) || is.factor(x)) for (formato in formatos) {
      pendientes <- is.na(y)
      y[pendientes] <- suppressWarnings(as.POSIXct(as.character(x[pendientes]), format = formato,
        tz = "America/Bogota"))
    }
  }
  y[!is.na(y) & as.Date(y, tz = "America/Bogota") <= as.Date("1900-01-01")] <- as.POSIXct(NA)
  y
}

.in_oportunidades <- function(fuente, entidad) {
  x <- .in_detalle_crm(fuente, entidad, .IN_FUENTES$oportunidades$campos)
  if (!nrow(x)) return(x)
  estado <- toupper(as.character(.gc_columna(x, c("Descartado", "Descartada"))))
  data.frame(Oportunidad = as.character(.gc_columna(x, "IdOportunidad")),
    Creacion = .in_fecha(.gc_columna(x, "FechaHoraCrea")),
    FechaObjetivo = .in_fecha(.gc_columna(x, "FechaCumpOP")),
    Linea = .in_linea(.gc_columna(x, c("LineaNegocio", "LinNegCod"))),
    Categoria = as.character(.gc_columna(x, "Categoria")), Producto = as.character(.gc_columna(x, "Producto")),
    SacosObjetivo = .gc_num(.gc_columna(x, c("SacosOP", "Sacos"))),
    MargenObjetivoKg = .gc_num(.gc_columna(x, c("MargenOP", "Margen"))),
    EstadoActual = ifelse(estado %in% c("SI", "TRUE", "1"), "Descartada",
      ifelse(estado %in% c("NO", "FALSE", "0"), "Sin descartar", "Sin estado registrado")))
}

# Una cronología conserva las transiciones explícitas, conversiones y gestiones disponibles ----
.in_cronologia <- function(entidad, contactos, fuentes, corte) {
  codigos <- .in_codigos(entidad)
  salida <- data.frame(Fecha = as.POSIXct(character(), tz = "America/Bogota"),
    EntidadCRM = character(), Evento = character(),
    Detalle = character(), Responsable = character(), Origen = character(),
    Registro = character(), Momento = character())
  agregar <- function(x, fecha, evento, detalle, usuario, origen) {
    if (!nrow(x) || !"CodContacto" %in% names(x)) return(invisible(NULL))
    x <- x[.in_id(x$CodContacto) %in% codigos, , drop = FALSE]
    if (!nrow(x)) return(invisible(NULL))
    original <- .gc_columna(x, fecha)
    fechas <- .in_instante(original)
    tiene_hora <- inherits(original, "POSIXt") | grepl("[0-9]{2}:[0-9]{2}", as.character(original))
    momento <- ifelse(is.na(fechas), NA_character_, ifelse(tiene_hora,
      format(fechas, "%Y-%m-%d %H:%M:%S", tz = "America/Bogota"),
      format(fechas, "%Y-%m-%d", tz = "America/Bogota")))
    filas <- is.na(fechas) | as.Date(fechas, tz = "America/Bogota") <= corte
    nuevo <- data.frame(Fecha = fechas, EntidadCRM = as.character(x$CodContacto), Evento = evento,
      Detalle = as.character(.gc_columna(x, detalle)),
      Responsable = as.character(.gc_columna(x, usuario)), Origen = origen,
      Registro = as.character(.gc_columna(x, c("IdHistorial", "IdHistoriaEtapa", "IdRelacionamiento",
        "IdOportunidad", "IdAlianza", "IdRecordatorio"))), Momento = momento)
    salida <<- dplyr::bind_rows(salida, nuevo[filas, , drop = FALSE])
  }
  agregar(contactos, "FechaHoraCrea", "Creación de contacto", c("Origen", "DetOrigen"),
    "UsuarioCrea", "Registro CRM")
  for (nombre in names(fuentes)) {
    fuente <- fuentes[[nombre]]
    if (!is.null(fuente$error) || !is.data.frame(fuente$datos)) next
    x <- fuente$datos
    if (nombre == "etapas" && nrow(x)) {
      x$Cambio <- paste(.gc_columna(x, "EtapaAnterior", "Inicio"), "→", .gc_columna(x, "EtapaNueva"))
      agregar(x, "FechaHora", "Cambio de etapa", "Cambio", c("Usuario", "UsuarioCrea"), "Historial de etapas")
    }
    if (nombre == "conversion_lead") agregar(x, "FechaConversion", "Conversión a lead", "Segmento",
      c("Asesor", "UsuarioCrea"), "Conversión registrada")
    if (nombre == "conversion_cliente") agregar(x, "FechaConversion", "Conversión a cliente", "NitFacturacion",
      c("UsuarioCrea", "Usuario"), "Conversión registrada")
    if (nombre == "gestiones") agregar(x, "FechaHoraCrea", "Gestión comercial", c("Comentario", "Descripcion"),
      "UsuarioCrea", "Gestiones")
    if (nombre == "oportunidades") agregar(x, "FechaHoraCrea", "Creación de oportunidad", c("Producto", "Descripcion"),
      "UsuarioCrea", "Oportunidades")
    if (nombre == "potencial") agregar(x, c("FechaHoraModi", "FechaHoraCrea"),
      "Preferencias declaradas (último registro)", c("CalidadesPreferidas", "TipoCompra"),
      c("UsuarioMod", "UsuarioCrea"), "Potencial declarado")
    if (nombre == "alianzas") agregar(x, "FechaHoraCrea", "Alianza comercial", "Observacion",
      "UsuarioCrea", "Alianzas")
    if (nombre == "descartes") agregar(x, c("FechaHoraModi", "FechaHoraCrea", "FechaHora"), "Descarte registrado",
      c("Razon1", "Comentario"), c("UsuarioMod", "UsuarioCrea", "Usuario"), "Descartes")
  }
  salida <- dplyr::distinct(salida)
  # No duplicar la conversión histórica cuando ya existe su transición explícita el mismo día.
  transiciones <- salida[salida$Evento == "Cambio de etapa", ]
  duplicado <- vapply(seq_len(nrow(salida)), function(i) {
    etapa <- if (salida$Evento[i] == "Conversión a lead") "LEAD" else if (
      salida$Evento[i] == "Conversión a cliente") "CLIENTE" else NA_character_
    if (is.na(etapa) || is.na(salida$Fecha[i])) return(FALSE)
    any(transiciones$EntidadCRM == salida$EntidadCRM[i] &
      as.Date(transiciones$Fecha, tz = "America/Bogota") == as.Date(salida$Fecha[i], tz = "America/Bogota") &
      grepl(paste0("→ ", etapa, "$"), transiciones$Detalle), na.rm = TRUE)
  }, logical(1))
  salida <- salida[!duplicado, , drop = FALSE]
  salida[order(salida$Fecha, salida$EntidadCRM, na.last = TRUE), , drop = FALSE]
}

# RFM y valor se ajustan en la unidad consultada, no copiando al hijo el perfil del padre ----
.in_poblacion_modelo <- function(hechos, nivel = "nit") {
  x <- hechos
  if (nivel == "nit") x$CliNitPpal <- .in_id(x$CLCliNit)
  x
}
.in_id_modelo <- function(entidad) {
  if (entidad$Alcance == "Cuenta principal") entidad$Principal else entidad$NitFacturacion
}
.in_venta_base <- function(hechos, filtros, corte, nivel = "nit") {
  f <- filtros
  f$categoria <- unique(hechos$Categoria)
  f$producto <- unique(hechos$Producto)
  fuente <- .gc_filtrar_dimensiones(hechos, f)
  fechas <- fuente$FecFact[!is.na(fuente$FecFact) & fuente$FecFact <= corte]
  desde <- if (length(fechas)) min(fechas) else corte
  x <- .prod_preparar(fuente, as.Date(c(desde, corte)))
  if (nivel == "nit") x$NIT <- x$NITFactura
  cestas <- .mba_cestas(x, "cliente", "producto", filtros)
  modelo <- .mba_itemsets(cestas$membresia, cestas$cestas$Cesta, soporte = 0, minimo = 2L, longitud = 2L)
  reglas <- .mba_reglas(modelo, confianza = 0.1, lift = 1.01, estadistica = FALSE)
  list(reglas = reglas, base = cestas)
}
.in_propuestas <- function(base, entidad) {
  movimientos <- base$base$movimientos
  movimientos <- movimientos[movimientos$NIT %in% .in_id_modelo(entidad), , drop = FALSE]
  .mba_recomendaciones(base$reglas, movimientos, base$base$mapa, maximo = 30L)
}

.in_cruce_oportunidades <- function(propuestas, base, fuente, entidad_crm) {
  if (!nrow(propuestas)) return(character())
  if (!is.null(fuente$error)) return(rep("No fue posible consultar oportunidades", nrow(propuestas)))
  if (!length(.in_codigos(entidad_crm))) return(rep("Sin vínculo CRM para comprobar", nrow(propuestas)))
  oportunidades <- tryCatch(.in_oportunidades(fuente, entidad_crm), error = function(e) NULL)
  if (is.null(oportunidades)) return(rep("No fue posible consultar el seguimiento", nrow(propuestas)))
  if (!nrow(oportunidades)) return(rep("Sin oportunidad registrada", nrow(propuestas)))
  codigos <- c(`10000` = "CONVENCIONALES", `21000` = "A LA MEDIDA", `20000` = "DIFERENCIADOS")
  linea <- unname(codigos[oportunidades$Linea])
  oportunidades$Linea <- dplyr::coalesce(linea, oportunidades$Linea)
  vapply(propuestas$Falta, function(falta) {
    mapa <- base$base$movimientos %>% dplyr::distinct(Etiqueta, Linea, Categoria, Producto)
    item <- mapa[mapa$Etiqueta %in% falta, , drop = FALSE]
    if (nrow(item) != 1L) return("Verificar coincidencia con las oportunidades")
    filas <- oportunidades$Linea %in% item$Linea & oportunidades$Categoria %in% item$Categoria &
      oportunidades$Producto %in% item$Producto
    x <- oportunidades[filas, , drop = FALSE]
    if (!nrow(x)) return("No se identificó una oportunidad coincidente")
    if (any(x$EstadoActual == "Sin descartar")) return("Ya registrada: consultar seguimiento")
    if (all(x$EstadoActual == "Descartada")) return("Hay una oportunidad descartada")
    "Hay registros: verificar su estado"
  }, character(1))
}

.in_linea <- function(x) {
  nombres <- unname(c(`10000` = "CONVENCIONALES", `21000` = "A LA MEDIDA", `20000` = "DIFERENCIADOS")[as.character(x)])
  dplyr::coalesce(nombres, as.character(x))
}
