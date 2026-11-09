# Comparar clasificaciones del último archivo disponible, sin recuperar segmentos antiguos ----
.vf_segmentos_recientes <- function(x) {
  if (!"FechaCorteArchivo" %in% names(x)) x$FechaCorteArchivo <- as.Date(NA)
  x <- x %>% dplyr::group_by(LinNegCod) %>%
    dplyr::filter(if (any(!is.na(FechaCorteArchivo))) {
      !is.na(FechaCorteArchivo) & FechaCorteArchivo == max(FechaCorteArchivo, na.rm = TRUE)
    } else ANHO == max(ANHO, na.rm = TRUE)) %>% dplyr::ungroup()
  # En exportaciones antiguas sin corte de archivo, la última observación del cliente es la referencia.
  x %>% dplyr::group_by(CliNitPpal, LinNegCod) %>%
    dplyr::filter(FecFactPYG == max(FecFactPYG, na.rm = TRUE)) %>%
    dplyr::summarise(FechaSegmentoPYG = max(dplyr::coalesce(FechaCorteArchivo, FecFactPYG), na.rm = TRUE),
      SegmentoPYG = {
        valores <- unique(SegmentoPYG)
        if (length(valores) == 1L) valores[[1L]] else NA_character_
      }, .groups = "drop")
}
