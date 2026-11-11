# Constantes ----
.ADMON_CHOICES_RDS <- Sys.getenv(
  "CRM_CHOICES_RDS",
  "/home/compartido/data/CRM_Nacional/choices.rds"
)
.ADMON_CHOICES_LOCK <- paste0(.ADMON_CHOICES_RDS, ".lock")

# Opciones iniciales para RDS anteriores; nunca reemplazan un catálogo existente o vacío.
.ADMON_CHOICES_INICIALES <- list(
  cot_presentacion = c("SACOS DE 70 KGS" = "Sacos de 70kgs", "SACOS DE 62.5 KGS" = "Sacos de 62.5Kgs",
    "SACOS DE 35 KGS" = "Sacos de 35Kgs", "GRAINPRO 70 KGS" = "Grainpro 70kgs"),
  cot_empaque = c("Blanco #6", "Premarcado #7", "Premarcado #7 Arte del cliente"),
  cot_divisa = c("Peso Colombiano", "Dólares"),
  formapago = c("PAGO ANTICIPADO", "CONTADO CONTRA ENTREGA"),
  pot_tipo_compra = c("VERDE", "TOSTADO", "AMBOS"),
  pot_frecuencia_compra = c("SEMANAL", "QUINCENAL", "MENSUAL", "TRIMESTRAL", "ESPORADICA"),
  pot_unidad_consumo = c("SACOS/MES", "KG/MES", "TON/MES"),
  pot_certificaciones = c("ORGANICO", "RAINFOREST ALLIANCE", "FAIR TRADE", "UTZ", "NINGUNA"),
  directorio_red_social = c("WHATSAPP", "LINKEDIN", "INSTAGRAM", "FACEBOOK", "X", "THREADS", "TELEGRAM", "SITIO WEB"),
  tipo_nota = c("NOTA DE REUNIÓN" = "text", "TAREA" = "list")
)
.admon_choices_actualizacion <- shiny::reactiveVal(0L)
