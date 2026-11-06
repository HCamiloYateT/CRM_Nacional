# Constantes ----
.ADMON_CHOICES_RDS <- Sys.getenv(
  "CRM_CHOICES_RDS",
  "/home/compartido/data/CRM_Nacional/choices.rds"
)
.ADMON_CHOICES_LOCK <- paste0(.ADMON_CHOICES_RDS, ".lock")
