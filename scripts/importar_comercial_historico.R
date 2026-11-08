# Importa exclusivamente datos: nunca sourcea scripts del ZIP.
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Uso: Rscript scripts/importar_comercial_historico.R ruta.zip [salida.rds]")
source("APP/misc/02_fuentes_comerciales.R")
salida <- if (length(args) > 1L) args[[2]] else "APP/data/comercial_historico.rds"
dir.create(dirname(salida), recursive = TRUE, showWarnings = FALSE)
datos <- .com_leer_historico(args[[1]])
saveRDS(datos, salida, compress = "xz")
message("Importado: ", salida, ". Reinicie la app. ",
        "DataPrep también lo incorporará a homologaciones en su próxima ejecución.")
