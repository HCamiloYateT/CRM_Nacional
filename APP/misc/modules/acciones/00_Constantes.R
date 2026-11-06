# Constantes Acciones ----

## Generales ----
# Color estandar para mensajes de error en formularios
COLOR_ERROR = "#C11007"
# Paleta icono/color por etapa del embudo, usada por .badge_etapa()
.COLORES_ETAPA <- c(
  CONTACTO   = "#5D6D7E", PROSPECTO = "#C8862A", LEAD = "#1C398E",
  CLIENTE    = "#198754", DESCARTADO = COLOR_ERROR
)
# Paleta icono/color por canal de recordatorio, usada por .badge_canal() (sin uso hoy, se deja para reuso futuro)
.CANAL_ESTILO <- list(
  Correo    = list(icono = "envelope",          color = "#1C398E"),
  Llamada   = list(icono = "phone",             color = "#0E7C61"),
  WhatsApp  = list(icono = "whatsapp",          color = "#25D366"),
  "Reunión" = list(icono = "users",             color = "#7D3C98"),
  Visita    = list(icono = "map-location-dot",  color = "#B7950B")
)
# Choices comunes del formulario de Potencial, reusados tambien en catalogo de detalle de descarte
.choices_potencial <- list(
  si_no              = c("SI", "NO"),
  tipo_compra        = c("VERDE", "TOSTADO", "AMBOS"),
  frecuencia_compra  = c("SEMANAL", "QUINCENAL", "MENSUAL", "TRIMESTRAL", "ESPORADICA"),
  unidad_consumo     = c("SACOS/MES", "KG/MES", "TON/MES"),
  certificaciones    = c("ORGANICO", "RAINFOREST ALLIANCE", "FAIR TRADE", "UTZ", "NINGUNA")
)

## Editar ----
# API key de Google Maps/Places para geocodificacion
google_key <- Sys.getenv("GOOGLE_MAPS_KEY")
# Iconos y colores por tipo de red social del directorio
.ICONOS_RED_SOCIAL <- list(
  "WhatsApp"  = list(clase = "fab fa-whatsapp",  color = "#25D366"),
  "LinkedIn"  = list(clase = "fab fa-linkedin",  color = "#0A66C2"),
  "Instagram" = list(clase = "fab fa-instagram", color = "#E1306C"),
  "Facebook"  = list(clase = "fab fa-facebook",  color = "#1877F2"),
  "X"         = list(clase = "fab fa-x-twitter", color = "#000000"),
  "Threads"   = list(clase = "fab fa-threads",   color = "#000000"),
  "Telegram"  = list(clase = "fab fa-telegram",  color = "#26A5E4"),
  "Sitio Web" = list(clase = "fas fa-globe",     color = "#6c757d")
)
# Tipos de red social disponibles (nombres de .ICONOS_RED_SOCIAL)
.TIPOS_RED_SOCIAL <- names(.ICONOS_RED_SOCIAL)
# Limite maximo de personas de contacto por directorio
.MAX_PERSONAS_CONTACTO <- 10
# Limite maximo de telefonos generales por directorio
.MAX_TELEFONOS_GENERAL <- 5
# Color de resaltado de la sede marcada como principal
.COLOR_SEDE_PRINCIPAL <- "#C11007"
# Colores por tipo de sede/sucursal
.COLORES_TIPO_SEDE <- c(
  "Punto de Venta" = "#C8862A",
  "Bodega" = "#6B4226",
  "Oficina" = "#2E86C1",
  "Sucursal" = "#8E44AD",
  "Planta" = "#117A65",
  "Otro" = "#7F8C8D"
)
# Tipos de sede disponibles (nombres de .COLORES_TIPO_SEDE)
.TIPOS_SEDE <- names(.COLORES_TIPO_SEDE)
# Catalogo de paises para el buscador de ubicacion (iso2 -> nombre)
.paises_anpaises <- CargarDatos("ANPAISES")
.PAISES_BUSCADOR <- setNames(.paises_anpaises$iso2, .paises_anpaises$nombre)
.PAISES_BUSCADOR <- .PAISES_BUSCADOR[order(names(.PAISES_BUSCADOR))]

## Relacionamiento ----
# Tipos de gestion disponibles para registrar un relacionamiento
.TIPOS_RELACIONAMIENTO <- c("Comentario", "Llamada", "Visita", "Reunión", "Correo")
# Canales disponibles para programar un recordatorio
.CANALES_RECORDATORIO <- c("Correo", "Llamada", "WhatsApp", "Reunión", "Visita")
# Paleta icono/color por tipo de gestion, usada por .badge_tipo_gestion()
.GESTION_ESTILO <- list(
  Comentario = list(icono = "comment",          color = "#5D6D7E"),
  Llamada    = list(icono = "phone",            color = "#0E7C61"),
  Visita     = list(icono = "map-location-dot", color = "#B7950B"),
  "Reunión"  = list(icono = "users",            color = "#7D3C98"),
  Correo     = list(icono = "envelope",         color = "#1C398E")
)

## Descartar ----
# Catalogo de razones de descarte, por etapa
.RAZONES_DESCARTE <- list(
  CONTACTO = c(
    "NO AUTORIZA TRATAMIENTO DE DATOS", "SIN INTERÉS COMERCIAL",
    "DATOS INCOMPLETOS O ERRÓNEOS", "DUPLICADO", "NO FUE POSIBLE CONTACTAR",
    "PRECIO INCOMPATIBLE", "CALIDAD INCOMPATIBLE", "CONDICIONES COMERCIALES NO ACEPTADAS",
    "VOLUMEN INSUFICIENTE", "PRODUCTO NO DISPONIBLE", "COBERTURA GEOGRÁFICA NO DISPONIBLE",
    "PLAZOS DE PAGO NO ACEPTADOS", "REQUERIMIENTOS LOGÍSTICOS NO VIABLES",
    "REQUERIMIENTOS TÉCNICOS NO VIABLES", "CLIENTE SIN CAPACIDAD DE COMPRA",
    "CONTACTO SIN PODER DE DECISIÓN", "NEGOCIO FUERA DEL MERCADO OBJETIVO", "OTRAS"
  ),
  PROSPECTO = c(
    "SIN CLIENTE ALIADO DISPONIBLE EN LA ZONA", "VOLUMEN INSUFICIENTE INCLUSO PARA ALIANZA",
    "CLIENTE ALIADO NO ACEPTÓ LA RELACIÓN", "SIN INTERÉS EN EL MODELO DE ALIANZA",
    "PRECIO INCOMPATIBLE", "CALIDAD INCOMPATIBLE", "CONDICIONES COMERCIALES NO ACEPTADAS",
    "PLAZOS DE PAGO NO ACEPTADOS", "REQUERIMIENTOS LOGÍSTICOS NO VIABLES",
    "REQUERIMIENTOS TÉCNICOS NO VIABLES", "PRODUCTO NO DISPONIBLE",
    "COBERTURA GEOGRÁFICA NO DISPONIBLE", "VOLUMEN POTENCIAL INSUFICIENTE",
    "FRECUENCIA DE COMPRA INSUFICIENTE", "SIN CAPACIDAD DE COMPRA",
    "NO CUMPLE PERFIL COMERCIAL", "NO CUMPLE REQUISITOS PARA PROMOCIÓN A LEAD", "OTRAS"
  ),
  LEAD = c(
    "NO AUTORIZA TRATAMIENTO DE DATOS", "SIN INTERÉS COMERCIAL",
    "DATOS INCOMPLETOS O ERRÓNEOS", "DUPLICADO", "NO FUE POSIBLE CONTACTAR",
    "PRECIO INCOMPATIBLE", "CALIDAD INCOMPATIBLE", "CONDICIONES COMERCIALES NO ACEPTADAS",
    "VOLUMEN INSUFICIENTE", "PRODUCTO NO DISPONIBLE", "PLAZOS DE PAGO NO ACEPTADOS",
    "REQUERIMIENTOS LOGÍSTICOS NO VIABLES", "REQUERIMIENTOS TÉCNICOS NO VIABLES",
    "COBERTURA GEOGRÁFICA NO DISPONIBLE", "COMPETENCIA CON MEJOR OFERTA",
    "SIN CAPACIDAD DE COMPRA", "NEGOCIACIÓN SIN ACUERDO", "OPORTUNIDAD PERDIDA",
    "PROYECTO O NECESIDAD CANCELADA", "DECISIÓN DE COMPRA APLAZADA INDEFINIDAMENTE",
    "NO CUMPLE PERFIL COMERCIAL", "OTRAS"
  ),
  CLIENTE = c(
    "SIN COMPRAS EN LOS ÚLTIMOS 12 MESES", "CAMBIO A COMPETENCIA",
    "CIERRE O LIQUIDACIÓN DEL NEGOCIO", "CONDICIONES COMERCIALES NO ACEPTADAS",
    "PRECIO INCOMPATIBLE", "CALIDAD INCOMPATIBLE", "PLAZOS DE PAGO NO ACEPTADOS",
    "REQUERIMIENTOS LOGÍSTICOS NO VIABLES", "PRODUCTO NO DISPONIBLE",
    "DISMINUCIÓN SOSTENIDA DEL VOLUMEN DE COMPRA", "INACTIVIDAD COMERCIAL",
    "INCUMPLIMIENTO DE PAGOS", "RIESGO DE CARTERA", "CAMBIO DE PROVEEDOR",
    "CAMBIO EN LAS NECESIDADES DEL CLIENTE", "PÉRDIDA DE INTERÉS COMERCIAL",
    "REESTRUCTURACIÓN O CAMBIO DE ACTIVIDAD DEL NEGOCIO", "RELACIÓN COMERCIAL FINALIZADA",
    "OTRAS"
  )
)
# Atajos por etapa de .RAZONES_DESCARTE (sin uso directo detectado; se dejan por si algun modulo las necesita sueltas)
.RAZONES_DESCARTE_CONTACTO  <- .RAZONES_DESCARTE$CONTACTO
.RAZONES_DESCARTE_PROSPECTO <- .RAZONES_DESCARTE$PROSPECTO
.RAZONES_DESCARTE_LEAD      <- .RAZONES_DESCARTE$LEAD
.RAZONES_DESCARTE_CLIENTE   <- .RAZONES_DESCARTE$CLIENTE
# Etiqueta legible por etapa, para textos del formulario de descarte
.ETIQUETA_DESCARTE_ETAPA <- c(
  CONTACTO = "Contacto", PROSPECTO = "Prospecto", LEAD = "Lead", CLIENTE = "Cliente"
)
# Contactos activos para el detalle de "DUPLICADO" — mismo patron que
# .choices_cliente_ppal() (Generales), pero sobre todos los contactos
# activos, no solo clientes principales
.choices_contacto_duplicado <- function() {
  CargarDatos("CRMNALCONTACTO") %>%
    filter(Estado == "ACTIVO") %>%
    distinct(CodContacto, PerRazSoc) %>%
    filter(!is.na(PerRazSoc)) %>%
    arrange(PerRazSoc) %>%
    { setNames(as.character(.$CodContacto), paste0(.$CodContacto, " - ", .$PerRazSoc)) }
}
# Catalogo de detalle requerido por razon de descarte
.DETALLE_RAZON_DESCARTE <- list(
  "PRECIO INCOMPATIBLE" = list(
    tipo = "numero", label = "¿A qué precio está comprando actualmente?"
  ),
  "CALIDAD INCOMPATIBLE" = list(              # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "¿Qué calidad(es) está comprando?",
    choices = c("Supremo", "Extra", "Excelso", "Usual Good Quality (UGQ)",
                "Pasilla", "Especial / Micro-lote", "Otra calidad")
  ),
  "VOLUMEN INSUFICIENTE" = list(
    tipo = "numero", label = "Volumen esperado por el cliente (sacos/mes)"
  ),
  "VOLUMEN INSUFICIENTE INCLUSO PARA ALIANZA" = list(
    tipo = "numero", label = "Volumen esperado, aun bajo modelo de alianza (sacos/mes)"
  ),
  "VOLUMEN POTENCIAL INSUFICIENTE" = list(
    tipo = "numero", label = "Volumen potencial estimado (sacos/mes)"
  ),
  "DISMINUCIÓN SOSTENIDA DEL VOLUMEN DE COMPRA" = list(
    tipo = "numero", label = "Volumen actual de compra (sacos/mes)"
  ),
  "FRECUENCIA DE COMPRA INSUFICIENTE" = list(  # REUSO .choices_potencial
    tipo = "lista", multiple = FALSE, label = "Frecuencia de compra actual o esperada",
    choices = .choices_potencial$frecuencia_compra
  ),
  "CONDICIONES COMERCIALES NO ACEPTADAS" = list(  # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "¿Qué condición comercial no fue aceptada?",
    choices = c("Descuento por volumen", "Garantías", "Exclusividad", "Rebates",
                "Plazo de pago", "Condiciones logísticas", "Otra condición")
  ),
  "PLAZOS DE PAGO NO ACEPTADOS" = list(         # REUSO Choices()$formapago
    tipo = "lista", multiple = FALSE, label = "Forma/plazo de pago solicitado por el cliente",
    choices = function() Choices()$formapago
  ),
  "PRODUCTO NO DISPONIBLE" = list(              # REUSO Choices()$producto
    tipo = "lista", multiple = TRUE, label = "¿Qué producto requiere el cliente?",
    choices = function() Choices()$producto
  ),
  "COBERTURA GEOGRÁFICA NO DISPONIBLE" = list(  # REUSO Choices()$deptos
    tipo = "lista", multiple = FALSE, label = "Departamento requerido por el cliente",
    choices = function() Choices()$deptos
  ),
  "REQUERIMIENTOS LOGÍSTICOS NO VIABLES" = list(  # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Requerimiento logístico no viable",
    choices = c("Entrega en 24h", "Transporte propio del cliente", "Empaque especial",
                "Frecuencia de despacho", "Volumen mínimo por despacho", "Otro requerimiento")
  ),
  "REQUERIMIENTOS TÉCNICOS NO VIABLES" = list(  # NUEVO, parcialmente reusa .choices_potencial$certificaciones
    tipo = "lista", multiple = TRUE, label = "Requerimiento técnico no viable",
    choices = c(.choices_potencial$certificaciones, "Ficha técnica específica",
                "Análisis de laboratorio", "Otro requerimiento")
  ),
  "COMPETENCIA CON MEJOR OFERTA" = list(        # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "¿En qué la competencia ofrece mejor condición?",
    choices = c("Mejor precio", "Mejor plazo de pago", "Mejor calidad",
                "Mejor servicio/logística", "Relación comercial previa", "Otro factor")
  ),
  "CAMBIO A COMPETENCIA" = list(                # NUEVO, mismo catalogo que arriba
    tipo = "lista", multiple = TRUE, label = "¿Por qué factor cambió a la competencia?",
    choices = c("Mejor precio", "Mejor plazo de pago", "Mejor calidad",
                "Mejor servicio/logística", "Relación comercial previa", "Otro factor")
  ),
  "CAMBIO DE PROVEEDOR" = list(                 # NUEVO, mismo catalogo
    tipo = "lista", multiple = TRUE, label = "¿Por qué factor cambió de proveedor?",
    choices = c("Mejor precio", "Mejor plazo de pago", "Mejor calidad",
                "Mejor servicio/logística", "Relación comercial previa", "Otro factor")
  ),
  "NO FUE POSIBLE CONTACTAR" = list(            # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Medios de contacto intentados sin respuesta",
    choices = c("Llamada sin respuesta", "Correo sin respuesta", "WhatsApp sin respuesta",
                "Visita sin resultado", "Referido sin respuesta", "Número/correo inválido")
  ),
  "DUPLICADO" = list(                           # NUEVO, reusa patron .choices_cliente_ppal
    tipo = "lista", multiple = FALSE, label = "Contacto/cliente original con el que duplica",
    choices = function() .choices_contacto_duplicado()
  ),
  "SIN CLIENTE ALIADO DISPONIBLE EN LA ZONA" = list(  # REUSO Choices()$deptos
    tipo = "lista", multiple = FALSE, label = "Zona evaluada para la alianza",
    choices = function() Choices()$deptos
  ),
  "CLIENTE ALIADO NO ACEPTÓ LA RELACIÓN" = list(  # REUSO Choices()$aliado
    tipo = "lista", multiple = TRUE, label = "Cliente(s) aliado(s) consultados",
    choices = function() Choices()$aliado
  ),
  "SIN COMPRAS EN LOS ÚLTIMOS 12 MESES" = list(  # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Tiempo sin compras",
    choices = c("1-3 meses sin comprar", "3-6 meses sin comprar",
                "6-12 meses sin comprar", "Más de 12 meses sin comprar")
  ),
  "CIERRE O LIQUIDACIÓN DEL NEGOCIO" = list(     # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Fuente de la información",
    choices = c("Prensa/medios", "Información de un tercero", "Visita en sitio",
                "Información del propio cliente", "Cámara de comercio", "Otra fuente")
  ),
  "INCUMPLIMIENTO DE PAGOS" = list(              # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Antigüedad de la mora",
    choices = c("1-30 días de mora", "31-60 días", "61-90 días", "Más de 90 días")
  ),
  "RIESGO DE CARTERA" = list(                    # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Tipo de riesgo identificado",
    choices = c("Mora reiterada", "Cheques devueltos", "Concordato/Reestructuración",
                "Reporte negativo en centrales de riesgo", "Garantías insuficientes", "Otro riesgo")
  ),
  "CAMBIO EN LAS NECESIDADES DEL CLIENTE" = list(  # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "¿Qué necesidad cambió?",
    choices = c("Cambio de producto requerido", "Cambio de volumen requerido",
                "Cambio de presentación/empaque", "Cambio de condiciones comerciales", "Otro cambio")
  ),
  "PÉRDIDA DE INTERÉS COMERCIAL" = list(         # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Contexto de la pérdida de interés",
    choices = c("Cambio de contacto/decisor", "Cambio de estrategia del cliente",
                "Sin respuesta a seguimiento comercial", "Otro motivo")
  ),
  "REESTRUCTURACIÓN O CAMBIO DE ACTIVIDAD DEL NEGOCIO" = list(  # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Tipo de reestructuración",
    choices = c("Cambio de razón social", "Fusión o adquisición",
                "Cambio de actividad económica", "Reducción de operación", "Otro")
  ),
  "RELACIÓN COMERCIAL FINALIZADA" = list(        # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Origen de la finalización",
    choices = c("Decisión del cliente", "Decisión de Racafe",
                "Vencimiento de contrato/acuerdo", "Otro motivo")
  ),
  "INACTIVIDAD COMERCIAL" = list(                # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Tiempo sin gestión comercial",
    choices = c("1-3 meses sin gestión", "3-6 meses sin gestión", "Más de 6 meses sin gestión")
  ),
  "CLIENTE SIN CAPACIDAD DE COMPRA" = list(      # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Tipo de limitación de capacidad",
    choices = c("Capacidad financiera insuficiente", "Capacidad de almacenamiento insuficiente",
                "Capacidad de consumo/producción insuficiente", "Otro")
  ),
  "SIN CAPACIDAD DE COMPRA" = list(              # NUEVO, mismo catalogo que arriba
    tipo = "lista", multiple = FALSE, label = "Tipo de limitación de capacidad",
    choices = c("Capacidad financiera insuficiente", "Capacidad de almacenamiento insuficiente",
                "Capacidad de consumo/producción insuficiente", "Otro")
  ),
  "CONTACTO SIN PODER DE DECISIÓN" = list(       # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Situación del decisor",
    choices = c("Se identificó al decisor pero no se logró contacto",
                "No se identificó al decisor",
                "Decisión centralizada fuera del país/región", "Otro")
  ),
  "NEGOCIO FUERA DEL MERCADO OBJETIVO" = list(   # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Criterio fuera del mercado objetivo",
    choices = c("Sector económico no objetivo", "Tamaño de negocio no objetivo",
                "Ubicación fuera de cobertura", "Otro criterio")
  ),
  "SIN INTERÉS EN EL MODELO DE ALIANZA" = list(  # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Motivo del rechazo al modelo de alianza",
    choices = c("Prefiere venta directa", "No conoce el modelo de alianza",
                "Desconfianza en el intermediario", "Otro motivo")
  ),
  "NO CUMPLE PERFIL COMERCIAL" = list(           # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Criterio del perfil comercial no cumplido",
    choices = c("Volumen", "Segmento", "Ubicación", "Capacidad de pago", "Otro criterio")
  ),
  "NO CUMPLE REQUISITOS PARA PROMOCIÓN A LEAD" = list(  # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Requisito no cumplido",
    choices = c("Volumen insuficiente", "Sin decisor identificado",
                "Datos incompletos", "Sin interés confirmado", "Otro requisito")
  ),
  "NEGOCIACIÓN SIN ACUERDO" = list(              # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Punto sin acuerdo en la negociación",
    choices = c("Precio", "Plazo de pago", "Volumen", "Condiciones logísticas",
                "Condiciones comerciales generales", "Otro punto")
  ),
  "OPORTUNIDAD PERDIDA" = list(                  # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Motivo de la oportunidad perdida",
    choices = c("Perdida ante la competencia", "Cliente pospuso la decisión",
                "Cliente canceló el proyecto", "Otro motivo")
  ),
  "PROYECTO O NECESIDAD CANCELADA" = list(       # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Motivo de la cancelación",
    choices = c("Cancelación interna del cliente", "Cambio de prioridades",
                "Falta de presupuesto", "Otro motivo")
  ),
  "DECISIÓN DE COMPRA APLAZADA INDEFINIDAMENTE" = list(  # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Motivo del aplazamiento",
    choices = c("Sin fecha definida de retoma", "Pendiente de aprobación interna del cliente",
                "Pendiente de presupuesto", "Otro motivo")
  ),
  "NO AUTORIZA TRATAMIENTO DE DATOS" = list(     # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Canal donde se registró la negativa",
    choices = c("Verbal", "Correo electrónico", "Formulario web", "Documento físico")
  ),
  "SIN INTERÉS COMERCIAL" = list(                # NUEVO propuesto
    tipo = "lista", multiple = FALSE, label = "Contexto de la falta de interés",
    choices = c("No respondió a la gestión comercial", "Manifestó explícitamente no tener interés",
                "Prefiere otro proveedor", "Otro motivo")
  ),
  "DATOS INCOMPLETOS O ERRÓNEOS" = list(         # NUEVO propuesto
    tipo = "lista", multiple = TRUE, label = "Dato(s) incompleto(s) o erróneo(s)",
    choices = c("NIT/Razón social", "Dirección", "Teléfono", "Correo",
                "Persona de contacto", "Otro dato")
  )
)
# Detalle a usar para una razon: config explicita o fallback generico —
# garantiza que TODA razon (excepto OTRAS) siempre tenga un campo de
# detalle estructurado (lista) que renderizar y validar. Fallback deja
# constancia explicita de que falta curar esa razon puntual.
.detalle_config_razon <- function(razon) {
  .DETALLE_RAZON_DESCARTE[[razon]] %||%
    list(tipo = "lista", multiple = FALSE, label = "Detalle adicional",
         choices = c("Sin detalle adicional disponible para esta razón — contactar a Sistemas"))
}

## DescartarOportunidad ----
# Catalogo de razones de descarte de oportunidades comerciales
.RAZONES_DESCARTE_OPORTUNIDAD <- c(
  "Cliente canceló la solicitud",
  "Precio no competitivo",
  "Perdido con la competencia",
  "Presupuesto insuficiente del cliente",
  "Cliente pospuso la decisión",
  "Cambio en las condiciones comerciales",
  "Producto/calidad no disponible en el momento requerido",
  "Volumen no viable operativamente",
  "Cliente cambió de proveedor habitual",
  "Falta de respuesta del cliente",
  "Error en el registro de la oportunidad",
  "OTRAS"
)
## Crear Tarea -----
.TSK_PRIORIDADES <- c("URGENTE", "IMPORTANTE", "MEDIA", "BAJA")