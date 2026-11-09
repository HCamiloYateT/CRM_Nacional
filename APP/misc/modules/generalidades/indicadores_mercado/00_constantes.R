# Generales -----
# Indicadores ----
.IND_CLAVES_CANON <- c(
  "TRM", "PrecioNY", "PrecioCarga", "Diferencial", "UGCRacafe",
  "PrecioBolsa", "PrecioFNC", "UGCFNC",
  "CALConsumo", "COMConsumo", "CALPasilla", "COMMolidos",
  "COMSoluble", "CALRipio", "COMRipio", "COMRobusta"
)

.IND_NOMBRES_CORTOS <- c(
  "TRM" = "TRM (Hoja de trabajo)",
  "PrecioNY" = "Precio NYC (HT)",
  "PrecioCarga" = "Precio Carga (Promedio de últimas entradas del día)",
  "Diferencial" = "Diferencial de Compra (HT)",
  "UGCRacafe" = "Costo UGQ Racafé",
  "PrecioBolsa" = "Precio Bolsa (FNC)",
  "PrecioFNC" = "Precio Carga (FNC)",
  "UGCFNC" = "Costo UGQ (FNC)",
  "CALConsumo" = "Precio Consumo (Calculadora)",
  "COMConsumo" = "Precio Consumo (Compras)",
  "CALPasilla" = "Precio Pasilla (Calculadora)",
  "COMMolidos" = "Precio Molidos (Compras)",
  "COMSoluble" = "Precio Soluble (Compras)",
  "CALRipio" = "Precio Ripio (Calculadora)",
  "COMRipio" = "Precio Ripio (Compras)",
  "COMRobusta" = "Precio Robusta (Compras)"
)[.IND_CLAVES_CANON]

.IND_NOMBRES_COMPARACION <- c(
  "TRM" = "TRM ($COP/USD)",
  "PrecioNY" = "Precio NYC (HT) (¢USD/lb)",
  "PrecioCarga" = "Precio Carga ($COP/carga)",
  "Diferencial" = "Diferencial de Compra (HT) ($USD/lb)",
  "UGCRacafe" = "UGC Racafé ($COP/kg)",
  "PrecioBolsa" = "Precio Bolsa FNC (¢USD/lb)",
  "PrecioFNC" = "Precio FNC ($COP/carga)",
  "UGCFNC" = "UGC FNC ($COP/kg)",
  "CALConsumo" = "CAL Consumo ($COP/kg)",
  "COMConsumo" = "COM Consumo ($COP/kg)",
  "CALPasilla" = "CAL Pasilla ($COP/kg)",
  "COMMolidos" = "COM Molidos ($COP/kg)",
  "COMSoluble" = "COM Soluble ($COP/kg)",
  "CALRipio" = "CAL Ripio ($COP/kg)",
  "COMRipio" = "COM Ripio ($COP/kg)",
  "COMRobusta" = "COM Robusta ($COP/kg)"
)[.IND_CLAVES_CANON]

.IND_NOMBRES_INV <- stats::setNames(
  names(.IND_NOMBRES_CORTOS),
  unname(.IND_NOMBRES_CORTOS)
)

.IND_COLORES_TENDENCIA <- c(
  sube = "#1F7A55",
  baja = "#9F0712",
  igual = "#374151"
)

.IND_FLECHAS_TENDENCIA <- c(
  sube = "▲",
  baja = "▼",
  igual = "▶"
)

.IND_RUTA_DISPONIBLE <- file.path(
  "/home/htamara/6_IndustriaNacional",
  "CRM Cliente Nacional/CRMNacional/data/cafexasignar.rds"
)