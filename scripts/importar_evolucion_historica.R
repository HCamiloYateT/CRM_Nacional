# Preparar una sola vez; DataPrep incorpora el resultado en homologaciones ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Uso: Rscript scripts/importar_evolucion_historica.R libro2016_2022 libro2023 destino.rds")
library(dplyr)
source("APP/misc/datos/03_evolucion_historica.R")
x <- .ev_importar_excel(args[1:2])
dir.create(dirname(args[[3]]), recursive = TRUE, showWarnings = FALSE)
saveRDS(x, args[[3]], compress = "xz")
cat("Histórico mensual preparado:", nrow(x), "filas;", format(min(x$FecMes)), "a", format(max(x$FecMes)), "\n")
