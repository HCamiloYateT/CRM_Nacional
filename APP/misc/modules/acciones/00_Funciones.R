# Funciones Acciones
## Generales ----
# Arma "NIT - Razon Social" para titulos de formularios
.titulo_identificacion <- function(nit, razsoc) {
  paste0(nit %||% "SIN NIT", " - ", razsoc %||% "SIN RAZÓN SOCIAL")
}
# Escapa comillas simples para condiciones SQL
.rc_escapar_sql <- function(x) gsub("'", "''", x)
# Card generico de resumen (borde de color, header y cuerpo)
.rc_card <- function(borde_color, header, cuerpo, extra_header = NULL, bg = "#FFFFFF") {
  tags$div(
    style = paste0(
      "flex:1; min-width:220px; background:", bg, "; border-left:4px solid ", borde_color, ";",
      "border-radius:6px; padding:14px; box-shadow:0 1px 4px rgba(0,0,0,.08); margin-bottom:10px;"
    ),
    tags$div(
      style = "display:flex; justify-content:space-between; align-items:flex-start; gap:8px; margin-bottom:6px;",
      tags$div(style = "font-weight:700; font-size:13px; color:#5a6370;", header),
      extra_header
    ),
    tags$p(style = "font-size:12px; margin:0; color:#555;", cuerpo)
  )
}
# Badge con icono y color por etapa del embudo
.badge_etapa <- function(etapa) {
  color <- .COLORES_ETAPA[[etapa]] %||% "#6c757d"
  tags$span(
    style = paste0("display:inline-block; padding:2px 10px; border-radius:10px; ",
                   "background:", color, "; color:#fff; font-size:12px; font-weight:600;"),
    etapa
  )
}
# Badge con icono y color por canal de recordatorio (sin uso hoy, se deja para reuso futuro)
.badge_canal <- function(canal) {
  est <- .CANAL_ESTILO[[canal]] %||% list(icono = "circle", color = "#6c757d")
  tags$span(class = "badge", style = paste0("background-color:", est$color, ";color:#fff;"),
            icon(est$icono, class = if (est$icono == "whatsapp") "fab" else NULL), " ", canal)
}
# Etapa vigente de UN contacto, reutilizando derivar_etapa_actual() de embudo/00_Funciones.R
obtener_etapa_contacto <- function(cod_contacto) {
  cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto))
  fila <- tryCatch(CargarDatos("CRMNALCONTACTO", condicion = cond), error = function(e) data.frame())
  if (nrow(fila) == 0) return(NA_character_)
  derivar_etapa_actual(contactos = fila)$Etapa[[1]]
}
# Determina codigo y nombre de linea de negocio a partir del segmento principal
determinar_linea_negocio <- function(segmento_principal) {
  if (is.null(segmento_principal) || is.na(segmento_principal)) {
    return(list(cod = NA_character_, nombre = NA_character_))
  }
  if (segmento_principal == "A LA MEDIDA") {
    return(list(cod = "21000", nombre = "A LA MEDIDA"))
  } else if (segmento_principal == "CONVENCIONALES") {
    return(list(cod = "10000", nombre = "CONVENCIONALES"))
  } else if (segmento_principal == "DIFERENCIADOS") {
    return(list(cod = "20000", nombre = "DIFERENCIADOS"))
  } else {
    return(list(cod = NA_character_, nombre = NA_character_))
  }
}
# Registra una transicion de etapa en el historial — fuente de verdad unica
registrar_transicion_etapa <- function(cod_contacto, etapa_anterior, etapa_nueva, usr,
                                       motivo = NA_character_) {
  registro_historial <- data.frame(
    CodContacto = cod_contacto, EtapaAnterior = etapa_anterior, EtapaNueva = etapa_nueva,
    Usuario = usr, FechaHora = Sys.time(), Motivo = motivo, stringsAsFactors = FALSE
  )
  AgregarDatos(registro_historial, "CRMNALHISTORIALETAPA")
}
# Muestra notificacion de error al usuario
NotificarError <- function(error, operacion, codigo_contacto = NULL) {
  mensaje_contacto <- if (!is.null(codigo_contacto)) {
    paste0(" (contacto ", codigo_contacto, ")")
  } else {
    ""
  }
  
  racafeForms::racafeNotify(duration = 6, type = "error",
                          title = paste0("Error al ", operacion, mensaje_contacto, ": ", conditionMessage(error)),
                          session = shiny::getDefaultReactiveDomain())
}
# Registra el error en log de consola/servidor
RegistrarError <- function(error, operacion, usuario = NULL, codigo_contacto = NULL) {
  mensaje_log <- paste0(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    "Operacion: ", operacion,
    " | Usuario: ", usuario %||% "DESCONOCIDO",
    " | Contacto: ", codigo_contacto %||% "N/A",
    " | Error: ", conditionMessage(error)
  )
  message(mensaje_log)
  invisible(mensaje_log)
}
# Combina registro en log + notificacion al usuario ante error de una accion
.ManejarErrorAccion <- function(error, operacion, usuario = NULL, codigo_contacto = NULL) {
  RegistrarError(error = error, operacion = operacion, usuario = usuario,
                 codigo_contacto = codigo_contacto)
  NotificarError(
    error = error,
    operacion = operacion,
    codigo_contacto = codigo_contacto
  )
}

## Editar ----
# Columna de icono de accion para tablas reactable
.coldef_accion <- function(etiqueta, icono, color) {
  reactable::colDef(
    name = "", minWidth = 46, html = TRUE, sortable = FALSE,
    cell = function(value) {
      as.character(tags$span(
        title = etiqueta,
        style = paste0("display:inline-flex; align-items:center; justify-content:center;",
                       "width:28px; height:28px; border-radius:6px; cursor:pointer;",
                       "background:", color, "; color:white; font-size:12px;"),
        icon(icono)
      ))
    }
  )
}
# Valida NIT: numerico, sin duplicar contacto activo ni CRMNALMARLOT
.ValidarNit <- function(nit_ingresado, codigo_contacto_actual = NULL) {
  if (is.null(nit_ingresado) || nit_ingresado == "" || is.na(nit_ingresado)) return(NULL)
  nit_ingresado <- trimws(nit_ingresado)
  
  if (!EsEnteroPositivo(nit_ingresado)) {
    return(FormatearTexto("* El NIT debe ser un valor numerico valido",
                          negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.75))
  }
  
  contactos <- CargarDatos("CRMNALCONTACTO")
  if (!is.null(codigo_contacto_actual)) {
    contactos <- contactos %>% filter(CodContacto != codigo_contacto_actual)
  }
  
  if (nit_ingresado %in% contactos$PerCod) {
    return(FormatearTexto("* El NIT ya existe como contacto activo",
                          negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.75))
  }
  
  # Columna correcta es PerCod, no CLIENTE (verificado contra FormularioLeads.R)
  if (nit_ingresado %in% CargarDatos("CRMNALMARLOT")$PerCod) {
    return(FormatearTexto("* El NIT ya existe en CRMNALMARLOT",
                          negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.75))
  }
  
  NULL
}
# Valida Razon Social: coincidencia exacta y nombres similares en NCLIENTE/FACT
.ValidarRazonSocial <- function(razon_social_ingresada, codigo_contacto_actual = NULL) {
  if (is.null(razon_social_ingresada) || nchar(trimws(razon_social_ingresada)) < 1) return(NULL)
  
  razon_social_mayuscula <- str_to_upper(razon_social_ingresada)
  coincidencia_exacta <- .BuscarContactoPorRazonSocial(
    razon_social_ingresada = razon_social_ingresada,
    codigo_contacto_actual = codigo_contacto_actual
  )
  
  nombres_similares <- NULL
  if (nchar(razon_social_mayuscula) >= 3) {
    similares_ncliente <- NCLIENTE %>%
      filter(grepl(razon_social_mayuscula, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
      pull(PerRazSoc) %>% str_to_upper() %>% unique()
    
    similares_facturacion <- FACT %>%
      filter(grepl(razon_social_mayuscula, str_to_upper(PerRazSoc), fixed = TRUE)) %>%
      pull(PerRazSoc) %>% str_to_upper() %>% unique()
    
    nombres_similares <- unique(c(similares_ncliente, similares_facturacion))
    if (length(nombres_similares) > 5) nombres_similares <- nombres_similares[1:5]
  }
  
  if (!is.null(coincidencia_exacta)) {
    return(FormatearTexto(
      paste0("* Ya existe un contacto con esta razon social (NIT: ",
             coincidencia_exacta$nit %||% "sin NIT", ")"),
      negrita = TRUE, color = "orange", tamano_pct = 0.75
    ))
  }
  
  if (!is.null(nombres_similares) && length(nombres_similares) > 0) {
    return(tagList(
      FormatearTexto("Nombres similares encontrados:", negrita = TRUE, color = "orange", tamano_pct = 0.75),
      tags$ul(lapply(nombres_similares, function(nombre) tags$li(nombre)))
    ))
  }
  
  NULL
}
# Busca un contacto activo con la misma Razon Social exacta
.BuscarContactoPorRazonSocial <- function(razon_social_ingresada, codigo_contacto_actual = NULL) {
  if (is.null(razon_social_ingresada) || nchar(trimws(razon_social_ingresada)) < 1) return(NULL)
  
  razon_social_mayuscula <- str_to_upper(trimws(razon_social_ingresada))
  contactos <- CargarDatos("CRMNALCONTACTO")
  if (!is.null(codigo_contacto_actual)) {
    contactos <- contactos %>% filter(CodContacto != codigo_contacto_actual)
  }
  
  coincidencia <- contactos %>%
    filter(str_to_upper(PerRazSoc) == razon_social_mayuscula) %>%
    slice(1)
  
  if (nrow(coincidencia) == 0) return(NULL)
  
  list(codigo_contacto = coincidencia$CodContacto, nit = coincidencia$PerCod)
}
# Genera IdPersona con timestamp
.generar_id_persona   <- function() paste0("PC-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
# Genera IdRed con timestamp
.generar_id_red       <- function() paste0("RD-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
# Genera IdTelefono con timestamp
.generar_id_telefono  <- function() paste0("TL-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
# Carga el correo empresarial del directorio de un contacto
cargar_info_empresarial_directorio <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALDIRECTORIOEMPRESARIAL") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame())
}
# Guarda (upsert) el correo empresarial del directorio de un contacto
guardar_info_empresarial_directorio <- function(cod_contacto, correo_empresarial, usr) {
  fila <- data.frame(CodContacto = cod_contacto, CorreoGeneral = correo_empresarial,
                     UsuarioMod = usr, FechaHoraModi = Sys.time(), stringsAsFactors = FALSE)
  ReemplazarDatos(fila, "CRMNALDIRECTORIOEMPRESARIAL", llaves = list(CodContacto = cod_contacto))
}
# Lista los telefonos del directorio de un contacto
listar_telefonos_directorio <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALDIRECTORIOTELEFONO") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(IdTelefono = character(), Telefono = character()))
}
# Registra un telefono en el directorio de un contacto
registrar_telefono_directorio <- function(cod_contacto, telefono, usr) {
  fila <- data.frame(IdTelefono = .generar_id_telefono(), CodContacto = cod_contacto,
                     Telefono = telefono, UsuarioCrea = usr, FechaHoraCrea = Sys.time(),
                     stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALDIRECTORIOTELEFONO")
}
# Elimina un telefono del directorio
eliminar_telefono_directorio <- function(id_telefono) {
  vacio <- CargarDatos("CRMNALDIRECTORIOTELEFONO") %>% filter(FALSE)
  ReemplazarDatos(vacio, "CRMNALDIRECTORIOTELEFONO", llaves = list(IdTelefono = id_telefono))
}
# Lista las redes sociales del directorio de un contacto
listar_redes_directorio <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALDIRECTORIORED") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(IdRed = character(), TipoRedSocial = character(),
                                    UsuarioRed = character()))
}
# Registra una red social en el directorio de un contacto
registrar_red_social_directorio <- function(cod_contacto, tipo_red, usuario_red) {
  fila <- data.frame(IdRed = .generar_id_red(), CodContacto = cod_contacto,
                     TipoRedSocial = tipo_red, UsuarioRed = usuario_red, stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALDIRECTORIORED")
}
# Elimina una red social del directorio
eliminar_red_social_directorio <- function(id_red) {
  vacio <- CargarDatos("CRMNALDIRECTORIORED") %>% filter(FALSE)
  ReemplazarDatos(vacio, "CRMNALDIRECTORIORED", llaves = list(IdRed = id_red))
}
# Lista las personas de contacto del directorio, con sus redes en HTML
listar_personas_directorio <- function(cod_contacto) {
  personas <- tryCatch({
    CargarDatos("CRMNALDIRECTORIOPERSONA") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(IdPersona = character(), Nombre = character(),
                                    Cargo = character(), Telefono = character(), Correo = character()))
  if (nrow(personas) == 0) return(personas %>% mutate(RedesHTML = character()))
  
  # Reutiliza .linea_red_social (mismo helper que Empresarial) en vez de un
  # formateador propio: evita duplicacion de logica de formato y garantiza
  # que el texto del link sea siempre el usuario, nunca el nombre de la red.
  redes <- tryCatch({
    CargarDatos("CRMNALDIRECTORIOPERSONARED") %>%
      filter(IdPersona %in% personas$IdPersona) %>%
      mutate(RedHTML = mapply(.linea_red_social, TipoRedSocial, UsuarioRed)) %>%
      group_by(IdPersona) %>%
      summarise(RedesHTML = paste(RedHTML, collapse = ""), .groups = "drop")
  }, error = function(e) data.frame(IdPersona = character(), RedesHTML = character()))
  
  personas %>%
    left_join(redes, by = "IdPersona") %>%
    mutate(RedesHTML = ifelse(is.na(RedesHTML), "", RedesHTML))
}
# Registra una persona de contacto en el directorio, retorna su IdPersona
registrar_persona_directorio <- function(cod_contacto, nombre, cargo, telefono, correo, usr) {
  id_persona <- .generar_id_persona()
  fila <- data.frame(
    IdPersona = id_persona, CodContacto = cod_contacto, Nombre = nombre, Cargo = cargo,
    Telefono = telefono, Correo = correo, UsuarioCrea = usr, FechaHoraCrea = Sys.time(),
    stringsAsFactors = FALSE
  )
  AgregarDatos(fila, "CRMNALDIRECTORIOPERSONA")
  id_persona
}
# Elimina una persona de contacto y sus redes sociales asociadas
eliminar_persona_directorio <- function(id_persona) {
  vacio_persona <- CargarDatos("CRMNALDIRECTORIOPERSONA") %>% filter(FALSE)
  ReemplazarDatos(vacio_persona, "CRMNALDIRECTORIOPERSONA", llaves = list(IdPersona = id_persona))
  
  redes <- tryCatch(CargarDatos("CRMNALDIRECTORIOPERSONARED") %>% filter(IdPersona == id_persona),
                    error = function(e) data.frame(IdRed = character()))
  vacio_red <- redes %>% filter(FALSE)
  for (id_red in redes$IdRed) {
    ReemplazarDatos(vacio_red, "CRMNALDIRECTORIOPERSONARED", llaves = list(IdRed = id_red))
  }
}
# Registra una red social asociada a una persona de contacto
registrar_red_social_persona_directorio <- function(id_persona, tipo_red, usuario_red) {
  fila <- data.frame(IdRed = .generar_id_red(), IdPersona = id_persona,
                     TipoRedSocial = tipo_red, UsuarioRed = usuario_red, stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALDIRECTORIOPERSONARED")
}
# Arma el link HTML (icono + href) de una red social
.linea_red_social <- function(tipo, usuario) {
  info <- .ICONOS_RED_SOCIAL[[tipo]] %||% list(clase = "fas fa-link", color = "#6c757d")
  as.character(tags$div(
    style = "margin-bottom:3px; white-space:nowrap;",
    tags$i(class = info$clase, style = paste0("color:", info$color, "; width:16px;")),
    tags$a(href = .href_red_social(tipo, usuario), target = "_blank", rel = "noopener",
           style = "margin-left:5px; font-size:12px;", usuario)
  ))
}
# Construye el href correspondiente segun tipo de red social
.href_red_social <- function(tipo, usuario) {
  usuario <- trimws(usuario)
  if (grepl("^https?://", usuario, ignore.case = TRUE)) return(usuario)
  if (identical(tipo, "WhatsApp")) return(paste0("https://wa.me/", gsub("[^0-9]", "", usuario)))
  if (identical(tipo, "Telegram")) return(paste0("https://t.me/", gsub("^@", "", usuario)))
  paste0("https://", gsub("^@", "", usuario))
}
# Genera IdSucursal con timestamp
.generar_id_sucursal  <- function() paste0("SU-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
# Guarda (upsert) la ubicacion principal de un contacto
guardar_ubicacion_contacto <- function(cod_contacto, pais, depto, mpio, direccion, lat, lng, usr,
                                       localidad = NA_character_, barrio = NA_character_) {
  fila <- data.frame(CodContacto = cod_contacto, Pais = pais, Depto = depto, Mpio = mpio,
                     Localidad = localidad, Barrio = barrio, Direccion = direccion,
                     lat = lat, lng = lng, UsuarioMod = usr, FechaHoraModi = Sys.time(),
                     stringsAsFactors = FALSE)
  ReemplazarDatos(fila, "CRMNALUBICACION", llaves = list(CodContacto = cod_contacto))
}
# Carga la ubicacion principal de un contacto
cargar_ubicacion_contacto <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALUBICACION") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame())
}
# Lista las sucursales de un contacto
listar_sucursales_contacto <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALSUCURSALES") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(IdSucursal = character(), Nombre = character(), Tipo = character(),
                                    Depto = character(), Mpio = character(), Localidad = character(),
                                    Barrio = character(), Direccion = character(), lat = double(),
                                    lng = double(), EsPrincipal = numeric()))
}
# Registra una sucursal; si es principal, desmarca las demas del contacto
registrar_sucursal_contacto <- function(cod_contacto, nombre, tipo, depto, mpio, direccion, lat, lng, es_principal, usr,
                                        localidad = NA_character_, barrio = NA_character_) {
  if (isTRUE(es_principal)) {
    sucursales <- CargarDatos("CRMNALSUCURSALES") %>%
      filter(CodContacto == cod_contacto, EsPrincipal == 1)
    if (nrow(sucursales) > 0) {
      for (id in sucursales$IdSucursal) {
        fila_desmarcada <- sucursales %>% filter(IdSucursal == id) %>% mutate(EsPrincipal = 0)
        ReemplazarDatos(fila_desmarcada, "CRMNALSUCURSALES", llaves = list(IdSucursal = id))
      }
    }
  }
  fila <- data.frame(IdSucursal = .generar_id_sucursal(), CodContacto = cod_contacto, Nombre = nombre,
                     Tipo = tipo, Depto = depto, Mpio = mpio, Localidad = localidad, Barrio = barrio,
                     Direccion = direccion, lat = lat, lng = lng, EsPrincipal = as.numeric(es_principal),
                     UsuarioCrea = usr, FechaHoraCrea = Sys.time(), stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALSUCURSALES")
}
# Elimina una sucursal
eliminar_sucursal_contacto <- function(id_sucursal) {
  vacio <- CargarDatos("CRMNALSUCURSALES") %>% filter(FALSE)
  ReemplazarDatos(vacio, "CRMNALSUCURSALES", llaves = list(IdSucursal = id_sucursal))
}
# Autocompleta direccion contra Places API (Google)
google_autocompletar_direccion <- function(texto, pais_iso2 = "CO") {
  if (is.null(texto) || length(texto) != 1 || is.na(texto) ||
      !nzchar(trimws(texto)) || nchar(texto) < 4) {
    return(list(status = "TEXTO_CORTO", sugerencias = NULL))
  }
  
  respuesta <- tryCatch({
    request("https://places.googleapis.com/v1/places:autocomplete") %>%
      req_headers(
        `Content-Type` = "application/json",
        `X-Goog-Api-Key` = google_key
      ) %>%
      req_body_json(list(
        input = texto,
        includedRegionCodes = list(pais_iso2),
        languageCode = "es"
      )) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  }, error = function(e) NULL)
  
  if (is.null(respuesta)) {
    return(list(status = "ERROR_CONEXION", sugerencias = NULL))
  }
  if (resp_status(respuesta) != 200) {
    cuerpo_error <- tryCatch(resp_body_json(respuesta), error = function(e) NULL)
    mensaje <- cuerpo_error$error$status %||% paste("HTTP", resp_status(respuesta))
    return(list(status = mensaje, sugerencias = NULL))
  }
  
  datos <- resp_body_json(respuesta, simplifyVector = FALSE)
  if (length(datos$suggestions) == 0) {
    return(list(status = "ZERO_RESULTS", sugerencias = NULL))
  }
  
  sugerencias <- do.call(rbind, lapply(datos$suggestions, function(s) {
    data.frame(
      texto = s$placePrediction$text$text,
      place_id = s$placePrediction$placeId,
      stringsAsFactors = FALSE
    )
  }))
  list(status = "OK", sugerencias = sugerencias)
}
# Busca dentro de addressComponents de la respuesta de Google
.extraer_componente <- function(componentes, tipo) {
  if (length(componentes) == 0) {
    return(NA_character_)
  }
  for (comp in componentes) {
    if (tipo %in% unlist(comp$types)) {
      return(comp$longText)
    }
  }
  NA_character_
}
# Detalle de un lugar ya seleccionado (Places API)
google_obtener_detalle_lugar <- function(place_id) {
  if (is.null(place_id) || !nzchar(place_id)) {
    return(list(status = "SIN_PLACE_ID"))
  }
  
  respuesta <- tryCatch({
    request(paste0("https://places.googleapis.com/v1/places/", place_id)) %>%
      req_headers(
        `X-Goog-Api-Key` = google_key,
        `X-Goog-FieldMask` = "id,formattedAddress,location,addressComponents"
      ) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  }, error = function(e) NULL)
  
  if (is.null(respuesta)) {
    return(list(status = "ERROR_CONEXION"))
  }
  if (resp_status(respuesta) != 200) {
    cuerpo_error <- tryCatch(resp_body_json(respuesta), error = function(e) NULL)
    mensaje <- cuerpo_error$error$status %||% paste("HTTP", resp_status(respuesta))
    return(list(status = mensaje))
  }
  
  datos <- resp_body_json(respuesta, simplifyVector = FALSE)
  componentes <- datos$addressComponents
  
  list(
    status = "OK",
    direccion_formateada = datos$formattedAddress,
    pais = .extraer_componente(componentes, "country"),
    depto = .extraer_componente(componentes, "administrative_area_level_1"),
    mpio = .extraer_componente(componentes, "locality") %||%
      .extraer_componente(componentes, "administrative_area_level_2"),
    sublocalidad = .extraer_componente(componentes, "sublocality_level_1") %||%
      .extraer_componente(componentes, "sublocality"),
    barrio = .extraer_componente(componentes, "neighborhood"),
    lat = datos$location$latitude,
    lng = datos$location$longitude
  )
}
# Carga la informacion de potencial de un contacto
cargar_potencial_contacto <- function(cod_contacto) {  # sin cambio de nombre; tabla renombrada
  tryCatch({
    CargarDatos("CRMNALPOTENCIAL") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame())
}
# Guarda (upsert) la informacion de potencial de un contacto
guardar_potencial_contacto <- function(cod_contacto, es_tostador, alianza, detalle_alianza,
                                       maquila, detalle_maquila, anios_operacion, tipo_compra,
                                       calidades_preferidas, consumo_esperado, unidad_consumo,
                                       frecuencia_compra, certificaciones_interes, observaciones, usr) {
  fila <- data.frame(
    CodContacto = cod_contacto, EsTostador = es_tostador, AlianzaTostadora = alianza,
    DetalleAlianza = detalle_alianza, Maquila = maquila, DetalleMaquila = detalle_maquila,
    AniosOperacion = anios_operacion, TipoCompra = tipo_compra,
    CalidadesPreferidas = paste(calidades_preferidas, collapse = "|"),
    ConsumoEsperado = consumo_esperado, UnidadConsumo = unidad_consumo,
    FrecuenciaCompra = frecuencia_compra,
    CertificacionesInteres = paste(certificaciones_interes, collapse = "|"),
    Observaciones = observaciones, UsuarioMod = usr, FechaHoraModi = Sys.time(),
    stringsAsFactors = FALSE
  )
  ReemplazarDatos(fila, "CRMNALPOTENCIAL", llaves = list(CodContacto = cod_contacto))
}

## Relacionamiento ----
# Badge con icono y color por tipo de gestion
.badge_tipo_gestion <- function(tipo) {
  est <- .GESTION_ESTILO[[tipo]] %||% list(icono = "circle", color = "#212f3d")
  tags$span(class = "badge", style = paste0("background-color:", est$color, ";color:#fff;"),
            icon(est$icono), " ", tipo)
}
# Limpia observers dinamicos de recordatorios/relacionamientos que ya no estan vigentes
.rc_limpiar_observers <- function(rv_list, ids_vigentes) {
  for (btn_id in names(rv_list)) {
    id_rec <- sub("^REC_[A-Za-z]+_", "", btn_id)
    if (!(id_rec %in% ids_vigentes)) {
      rv_list[[btn_id]]$destroy()
      rv_list[[btn_id]] <- NULL
    }
  }
  rv_list
}
# Genera IdRelacionamiento con timestamp
.generar_id_relacionamiento <- function() {
  paste0("RL-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
}
# Genera IdRecordatorio con timestamp
.generar_id_recordatorio <- function() {
  paste0("RC-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
}
# Registra un relacionamiento (gestion) de un contacto
registrar_relacionamiento <- function(cod_contacto, tipo, comentario, usr) {
  fila <- data.frame(IdRelacionamiento = .generar_id_relacionamiento(), CodContacto = cod_contacto,
                     TipoGestion = tipo, Comentario = comentario, UsuarioCrea = usr,
                     FechaHoraCrea = Sys.time(), stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALRELACIONAMIENTO")
}
# Lista el historial de relacionamiento de un contacto
listar_relacionamiento <- function(cod_contacto) {
  tryCatch({
    cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto))
    CargarDatos("CRMNALRELACIONAMIENTO", condicion = cond) %>%
      mutate(FechaHoraCrea = as_datetime(FechaHoraCrea)) %>%
      arrange(desc(FechaHoraCrea))
  }, error = function(e) {
    data.frame(IdRelacionamiento = character(), TipoGestion = character(), Comentario = character(),
               UsuarioCrea = character(), FechaHoraCrea = as.POSIXct(character()))
  })
}
# Registra un recordatorio de seguimiento para un contacto
registrar_recordatorio <- function(cod_contacto, asesor, fecha, canal, mensaje, usr) {
  fila <- data.frame(IdRecordatorio = .generar_id_recordatorio(), CodContacto = cod_contacto, Asesor = asesor,
                     FechaRecordatorio = fecha, Canal = canal, Mensaje = mensaje, Enviado = 0,
                     FechaHoraEnvio = as.POSIXct(NA), UsuarioCrea = usr, FechaHoraCrea = Sys.time(),
                     stringsAsFactors = FALSE)
  AgregarDatos(fila, "CRMNALRECORDATORIO")
}
# Lista los recordatorios de un contacto (pendientes primero)
listar_recordatorio <- function(cod_contacto) {
  tryCatch({
    cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto))
    CargarDatos("CRMNALRECORDATORIO", condicion = cond) %>%
      mutate(FechaRecordatorio = as_datetime(FechaRecordatorio),
             FechaHoraEnvio    = as_datetime(FechaHoraEnvio)) %>%
      arrange(Enviado, FechaRecordatorio)
  }, error = function(e) {
    data.frame(IdRecordatorio = character(), CodContacto = character(), Asesor = character(),
               FechaRecordatorio = as.POSIXct(character()), Canal = character(), Mensaje = character(),
               Enviado = numeric(), FechaHoraEnvio = as.POSIXct(character()),
               UsuarioCrea = character(), FechaHoraCrea = as.POSIXct(character()),
               stringsAsFactors = FALSE)
  })
}

## Promover ----
# Choices "NIT - Razon Social" del cliente principal, para vincular alianzas
.choices_cliente_ppal <- function() {
  clientes_actual %>%
    distinct(CliNitPpal) %>%
    left_join(n_cliente %>% select(CliNitPpal = PerCod, PerRazSoc), by = "CliNitPpal") %>%
    distinct(CliNitPpal, PerRazSoc) %>%
    filter(!is.na(CliNitPpal), !is.na(PerRazSoc)) %>%
    arrange(PerRazSoc) %>%
    {setNames(as.character(.$CliNitPpal), paste0(.$CliNitPpal, " - ", .$PerRazSoc))}
}
# Mensaje contextual mostrado al usuario segun a donde promueve la etapa actual (sin uso detectado; revisar)
.mensaje_promocion_destino <- function(etapa, destino_contacto = NULL) {
  switch(etapa,
         "CONTACTO"  = if (identical(destino_contacto, "PROSPECTO")) {
           "La promoción llevará este contacto al estado PROSPECTO."
         } else {
           "La promoción llevará este contacto al estado LEAD."
         },
         "PROSPECTO" = "La promoción llevará este prospecto al estado LEAD.",
         "LEAD"      = "Al vincular el NIT de facturación, este lead pasará al estado CLIENTE.",
         NULL
  )}
# Genera IdAlianza con timestamp
.generar_id_alianza <- function() paste0("AL-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
# Registra una alianza (Prospecto vinculado a un Cliente existente)
registrar_alianza_prospecto <- function(cod_contacto, cod_cliente_aliado, usr, observacion = NA_character_) {
  fila <- data.frame(
    IdAlianza = .generar_id_alianza(), CodContacto = cod_contacto, CodClienteAliado = cod_cliente_aliado,
    UsuarioCrea = usr, FechaHoraCrea = Sys.time(), Observacion = observacion, stringsAsFactors = FALSE
  )
  AgregarDatos(fila, "CRMNALPROSPECTOALIANZA")
  invisible(fila)
}
# Promueve un Contacto a Prospecto, registrando sus alianzas
convertir_contacto_a_prospecto <- function(cod_contacto, cods_cliente_aliado, usr, observacion = NA_character_) {
  if (length(cods_cliente_aliado) == 0) stop("Debe vincular al menos una alianza")
  for (cod_cliente in cods_cliente_aliado) registrar_alianza_prospecto(cod_contacto, cod_cliente, usr, observacion)
  registrar_transicion_etapa(cod_contacto, "CONTACTO", "PROSPECTO", usr)
  invisible(NULL)
}
# Inserta la conversion a Lead en CONTACTOLEAD y registra el historial —
# compartida por convertir_contacto_a_lead() y convertir_prospecto_a_lead()
.insertar_conversion_lead <- function(cod_contacto, asesor, segmento, linea_negocio_input,
                                      etapa_anterior, usr) {
  linea <- determinar_linea_negocio(linea_negocio_input)
  fila_lead <- data.frame(
    CodContacto = cod_contacto, FechaConversion = Sys.time(), CodLinNegocio = linea$cod,
    LinNegocio = linea$nombre, Asesor = asesor, Segmento = segmento, stringsAsFactors = FALSE
  )
  AgregarDatos(fila_lead, "CONTACTOLEAD")
  registrar_transicion_etapa(cod_contacto, etapa_anterior, "LEAD", usr)
  invisible(fila_lead)
}
# Convierte un Contacto en Lead
convertir_contacto_a_lead <- function(cod_contacto, asesor, segmento, linea_negocio_input, usr) {
  .insertar_conversion_lead(cod_contacto, asesor, segmento, linea_negocio_input,
                            etapa_anterior = "CONTACTO", usr = usr)
}
# Reclasifica un Prospecto a Lead — las alianzas existentes NO se eliminan,
# quedan como historial de que ese Lead alguna vez fue Prospecto
convertir_prospecto_a_lead <- function(cod_contacto, asesor, segmento, linea_negocio_input, usr) {
  .insertar_conversion_lead(cod_contacto, asesor, segmento, linea_negocio_input,
                            etapa_anterior = "PROSPECTO", usr = usr)
}
# Registra (upsert) el vinculo manual de NIT de un lead con el NIT que factura
registrar_vinculo_nit <- function(cod_contacto, nit_vinculado, usr, observacion = NA_character_) {
  fila <- data.frame(CodContacto = cod_contacto, NitVinculado = nit_vinculado, UsuarioVinculo = usr,
                     FechaHoraVinculo = Sys.time(), Observacion = observacion, stringsAsFactors = FALSE)
  ReemplazarDatos(fila, "CRMNALVINCULONIT", llaves = list(CodContacto = cod_contacto))
}
# Carga el vinculo de NIT de un contacto, si existe
cargar_vinculo_nit <- function(cod_contacto) {
  tryCatch({
    CargarDatos("CRMNALVINCULONIT") %>% filter(CodContacto == cod_contacto)
  }, error = function(e) data.frame(NitVinculado = character()))
}

## Descartar ----
# Registra (upsert) la razon/motivo de descarte de un contacto
## Descartar ----
registrar_descarte <- function(cod_contacto, etapa, razon1, razon2, razon3, usr) {
  fila <- data.frame(
    CodContacto = cod_contacto, Etapa = etapa, Razon1 = razon1, Razon2 = razon2, Razon3 = razon3,
    UsuarioMod = usr, FechaHoraModi = Sys.time(), stringsAsFactors = FALSE
  )
  ReemplazarDatos(fila, "CRMNALDESCARTE", llaves = list(CodContacto = cod_contacto))
  invisible(fila)
}
descartar_generico <- function(cod_contacto, etapa_origen, motivo, usr, razon1 = NA_character_,
                               razon2 = NA_character_, razon3 = NA_character_) {
  fila_actual <- CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto)
  if (nrow(fila_actual) == 0) stop("Contacto no encontrado: ", cod_contacto)
  
  fila_actual$Estado           <- "DESCARTADO"
  fila_actual$EtapaPreDescarte <- etapa_origen
  fila_actual$UsuarioMod       <- usr
  fila_actual$FechaHoraModi    <- Sys.time()
  
  ReemplazarDatos(fila_actual, "CRMNALCONTACTO", llaves = list(CodContacto = cod_contacto))
  registrar_transicion_etapa(cod_contacto, etapa_origen, "DESCARTADO", usr, motivo = motivo)
  
  # Razon1 separada del motivo completo — si el caller no la provee
  # explicitamente (ej. codigo legacy que aun llama a esta funcion con la
  # firma de 4 argumentos), se extrae la primera razon del catalogo de esa
  # etapa que aparezca como prefijo del texto de motivo; si no matchea
  # ninguna, se guarda "OTRAS" en vez de dejar Razon1 vacio
  if (is.na(razon1)) {
    catalogo <- .RAZONES_DESCARTE[[etapa_origen]] %||% character(0)
    encontrada <- catalogo[vapply(catalogo, function(r) startsWith(motivo %||% "", r), logical(1))]
    razon1 <- if (length(encontrada) > 0) encontrada[[1]] else "OTRAS"
  }
  
  tryCatch({
    registrar_descarte(cod_contacto, etapa_origen, razon1, razon2, razon3, usr)
  }, error = function(e) {
    warning("No se pudo registrar el descarte en CRMNALDESCARTE para ", cod_contacto,
            ": ", conditionMessage(e))
  })
  
  invisible(fila_actual)
}
## Reactivar ----
# Obtiene la etapa previa al descarte de un contacto (fallback CONTACTO)
.obtener_etapa_pre_descarte <- function(cod_contacto) {
  cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto))
  fila <- tryCatch(CargarDatos("CRMNALCONTACTO", condicion = cond), error = function(e) data.frame())
  if (nrow(fila) == 0 || !"EtapaPreDescarte" %in% names(fila)) return("CONTACTO")
  fila$EtapaPreDescarte[[1]] %||% "CONTACTO"
}
# Reactiva un contacto descartado, devolviendolo a su etapa previa
reactivar_contacto <- function(cod_contacto, usr) {
  fila_actual <- CargarDatos("CRMNALCONTACTO") %>% filter(CodContacto == cod_contacto)
  if (nrow(fila_actual) == 0) stop("Contacto no encontrado: ", cod_contacto)
  
  etapa_origen <- if ("EtapaPreDescarte" %in% names(fila_actual)) {
    fila_actual$EtapaPreDescarte[[1]] %||% "CONTACTO"
  } else "CONTACTO"
  
  fila_actual$Estado        <- "ACTIVO"
  fila_actual$UsuarioMod    <- usr
  fila_actual$FechaHoraModi <- Sys.time()
  
  ReemplazarDatos(fila_actual, "CRMNALCONTACTO", llaves = list(CodContacto = cod_contacto))
  registrar_transicion_etapa(cod_contacto, "DESCARTADO", etapa_origen, usr, motivo = "Reactivación")
  invisible(fila_actual)
}

## CrearOportunidad ----
# Genera IdOportunidad con timestamp
.generar_id_oportunidad <- function() {
  paste0("OP-", format(Sys.time(), "%Y%m%d%H%M%OS3") %>% gsub("\\.", "", .))
}
# Peso del saco (kg) segun linea de negocio, para calculo de toneladas
peso_saco_linneg <- function(linneg) {
  dplyr::case_when(
    linneg == "CONVENCIONALES" ~ 62.5,
    linneg == "A LA MEDIDA"    ~ 70,
    linneg == "DIFERENCIADOS"  ~ 70,
    TRUE                       ~ NA_real_
  )
}
# Registra una nueva oportunidad comercial para un contacto
registrar_oportunidad <- function(cod_contacto, linnegcod, categoria, producto,
                                  fecha_cumplimiento, sacos, margen, comentarios = NA_character_,
                                  etapa, usr, frecuencia = NULL) {
  fila <- data.frame(
    IdOportunidad = .generar_id_oportunidad(),
    CodContacto   = cod_contacto,
    LinNegCod     = linnegcod,
    Categoria     = categoria,
    Producto      = producto,
    FechaCumpOP   = fecha_cumplimiento,
    Sacos         = as.numeric(sacos),
    Margen        = as.numeric(margen),
    Comentarios   = comentarios,
    Etapa         = etapa,
    Descartado    = "NO",
    Razon         = NA_character_,
    UsuarioCrea   = usr,
    FechaHoraCrea = Sys.time(),
    stringsAsFactors = FALSE
  )
  if (!is.null(frecuencia)) {
    if (length(frecuencia) != 1L || !is.finite(frecuencia) || frecuencia <= 0) {
      stop("La frecuencia debe ser un número positivo de días.")
    }
    esquema <- racafeBD::CargarDatos("CRMNALCLOPT", condicion = "1=0")
    if (!"FrecuenciaDias" %in% names(esquema)) {
      stop("Falta FrecuenciaDias en oportunidades. Aplique la migración documentada antes de crear.")
    }
    fila$FrecuenciaDias <- as.numeric(frecuencia)
  }
  racafeBD::AgregarDatos(fila, "CRMNALCLOPT")
  invisible(fila)
}
# Lista las oportunidades de un contacto (sin uso detectado hoy; revisar)
listar_oportunidades <- function(cod_contacto, incluir_descartadas = FALSE) {
  dat <- tryCatch({
    cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto))
    CargarDatos("CRMNALCLOPT", condicion = cond) %>%
      mutate(FechaHoraCrea = as_datetime(FechaHoraCrea))
  }, error = function(e) {
    data.frame(IdOportunidad = character(), CodContacto = character(), LinNegCod = numeric(),
               Categoria = character(), Producto = character(), FechaCumpOP = as.Date(character()),
               Sacos = numeric(), Margen = numeric(), Comentarios = character(), Etapa = character(),
               Descartado = character(), Razon = character(), UsuarioCrea = character(),
               FechaHoraCrea = as.POSIXct(character()))
  })
  if (!incluir_descartadas) dat <- dat %>% filter(Descartado != "SI")
  dat %>% arrange(desc(FechaHoraCrea))
}

## DescartarOportunidad ----
# Marca una oportunidad como descartada
descartar_oportunidad <- function(id_oportunidad, razon, usr) {
  fila_actual <- CargarDatos("CRMNALCLOPT",
                             condicion = sprintf("IdOportunidad = '%s'", .rc_escapar_sql(id_oportunidad)))
  if (nrow(fila_actual) == 0) stop("La oportunidad no existe o ya fue eliminada")
  if (identical(fila_actual$Descartado[[1]], "SI")) {
    stop("Esta oportunidad ya se encuentra descartada")
  }
  
  fila_actual$Descartado <- "SI"
  fila_actual$Razon <- razon
  ReemplazarDatos(fila_actual, "CRMNALCLOPT", llaves = list(IdOportunidad = id_oportunidad))
  invisible(fila_actual)
}
## Crear Tarea ----
.nota_obtener_etiquetas <- function() {
  notas <- tryCatch(racafeBD::CargarDatos("CRMNALNOTAS"), error = function(e) data.frame())
  
  if (nrow(notas) == 0L || !"Etiquetas" %in% names(notas)) {
    return(character(0))
  }
  
  etiquetas <- notas$Etiquetas
  etiquetas <- etiquetas[!is.na(etiquetas)]
  etiquetas <- etiquetas[nzchar(etiquetas)]
  
  if (length(etiquetas) == 0L) {
    return(character(0))
  }
  
  etiquetas <- unlist(strsplit(etiquetas, "\\|"))
  etiquetas <- trimws(etiquetas)
  etiquetas <- etiquetas[nzchar(etiquetas)]
  
  etiquetas %>%
    stringr::str_to_sentence() %>%
    unique() %>%
    sort()
}
.nota_obtener_consecutivo <- function() {
  notas <- tryCatch(racafeBD::CargarDatos("CRMNALNOTAS"), error = function(e) data.frame())
  
  if (nrow(notas) == 0L || !"cons" %in% names(notas)) {
    return(1L)
  }
  
  consecutivo <- suppressWarnings(max(notas$cons, na.rm = TRUE))
  
  if (!is.finite(consecutivo)) {
    return(1L)
  }
  
  as.integer(consecutivo) + 1L
}
.nota_construir_registro <- function(titulo, tipo, descripcion, fecha_cumplimiento, prioridad, responsable,
                                     etiquetas, actividades, cumplidas, usuario) {
  data.frame(
    cons = .nota_obtener_consecutivo(), titulo = trimws(titulo), Usuario = usuario, Responsable = responsable,
    FechaHoraCrea = Sys.time(), Tipo = tipo, Descripcion = trimws(descripcion %||% ""),
    FechaCumplimiento = fecha_cumplimiento, Actividades = paste(actividades, collapse = "|"),
    Cumplidas = paste(cumplidas, collapse = "|"), Etiquetas = paste(etiquetas, collapse = "|"),
    Prioridad = prioridad, Archivado = 0L, Cumplido = 0L, Respuesta = "", stringsAsFactors = FALSE
  )
}
.nota_actividad_ui <- function(ns, numero) {
  tags$div(id = ns(paste0("NOT_Actividad_", numero)), class = "nota-actividad-row",
           tags$div(class = "nota-actividad-texto",
                    racafeForms::racafeInputText(paste0("NOT_Texto_", numero), label = NULL, value = "",
                                                     placeholder = paste("Actividad", numero), ns = ns)
           ),
           tags$div(class = "nota-actividad-accion",
                    uiOutput(ns(paste0("NOT_BotonCumplir_", numero)))
           ),
           tags$div(class = "nota-actividad-accion",
                    racafeForms::racafeButton(paste0("NOT_Eliminar_", numero), label = NULL, icon = "trash",
                                               variant = "danger", size = "xxs", ns = ns,
                                               title = "Eliminar actividad")
           )
  )
}
## Registrar Competencia----
.registrar_competencia_datos_vacios <- function() {
  data.frame(
    UsuarioCrea = character(),
    FechaHoraCrea = as.POSIXct(character()),
    Competencia = character(),
    PrecioUGQ = numeric(),
    Observaciones = character(),
    stringsAsFactors = FALSE
  )
}
.registrar_competencia_cargar_datos <- function() {
  tryCatch({
    datos <- CargarDatos("CRMNALCOMPETENCIA")
    
    if (is.null(datos)) {
      return(.registrar_competencia_datos_vacios())
    }
    
    datos
  }, error = function(e) {
    .registrar_competencia_datos_vacios()
  })
}
.registrar_competencia_choices <- function(datos) {
  if (is.null(datos) || nrow(datos) == 0L || !"Competencia" %in% names(datos)) {
    return(character(0))
  }
  
  competencias <- as.character(datos$Competencia)
  competencias <- competencias[!is.na(competencias)]
  competencias <- trimws(competencias)
  competencias <- competencias[nzchar(competencias)]
  
  sort(unique(competencias))
}
.registrar_competencia_ugq_actual <- function() {
  indicadores <- .ObtenerIndicadoresActuales(
    uid = uid,
    pwd = pwd
  )
  
  if (is.null(indicadores) || nrow(indicadores) == 0L) {
    return(NA_real_)
  }
  
  valor <- indicadores %>%
    dplyr::filter(as.character(Item) == "Costo UGQ Racafé") %>%
    dplyr::pull(Valor)
  
  if (length(valor) == 0L) {
    return(NA_real_)
  }
  
  suppressWarnings(
    as.numeric(valor[[1]])
  )
}
