# Histórico financiero local: solo datos, sin ejecutar código adjunto ----
.ev_validar_historico <- function(x) {
  requeridas <- c("CLCliNit", "LinNegCod", "FecMes", "SacosFact", "Margen", "Fuente")
  if (!is.data.frame(x) || !all(requeridas %in% names(x))) stop("Histórico de evolución incompatible.")
  x$FecMes <- as.Date(x$FecMes)
  x$CLCliNit <- as.character(x$CLCliNit)
  if (anyNA(x$FecMes) || anyNA(x$CLCliNit) || any(!nzchar(x$CLCliNit))) stop("Histórico sin fecha o cliente.")
  if (anyDuplicated(x[c("CLCliNit", "LinNegCod", "FecMes")])) stop("Histórico con claves mensuales duplicadas.")
  if (!is.numeric(x$SacosFact) || !is.numeric(x$Margen)) stop("El histórico debe conservar magnitudes numéricas.")
  x
}
.ev_leer_historico <- function(ruta) .ev_validar_historico(readRDS(ruta))

.ev_importar_excel <- function(archivos) {
  if (length(archivos) != 2L || any(!file.exists(archivos))) stop("Indique los dos Excel históricos existentes.")
  if (anyDuplicated(normalizePath(archivos))) stop("No repita el mismo libro.")
  tablas <- lapply(seq_along(archivos), function(i) {
    hojas <- readxl::excel_sheets(archivos[[i]])
    hoja <- intersect(c("BASE PYG", "BASE"), hojas)
    if (length(hoja) != 1L) stop("El Excel debe contener una hoja BASE PYG o BASE única.")
    necesarias <- c("FEC FACTURA", "CLIENTE", "LIN.NEG", "SS 70 KLS", "MNFCC$", "TIP")
    cabecera <- names(readxl::read_excel(archivos[[i]], sheet = hoja, n_max = 0, .name_repair = "unique_quiet"))
    if (!all(necesarias %in% cabecera)) stop("El Excel no contiene las columnas financieras esperadas.")
    tipos <- rep("skip", length(cabecera))
    tipos[match(necesarias, cabecera)] <- c("date", "text", "numeric", "numeric", "numeric", "text")
    x <- as.data.frame(readxl::read_excel(archivos[[i]], sheet = hoja, col_types = tipos,
      .name_repair = "unique_quiet"))
    fecha <- if (inherits(x[["FEC FACTURA"]], c("POSIXt", "Date"))) as.Date(x[["FEC FACTURA"]]) else {
      as.Date(suppressWarnings(as.numeric(x[["FEC FACTURA"]])), origin = "1899-12-30")
    }
    tipo <- toupper(trimws(as.character(x$TIP)))
    valido <- !is.na(fecha) & fecha >= as.Date("2016-01-01") & fecha < as.Date("2024-01-01") &
      !is.na(x$CLIENTE) & tipo %in% c("NAL", "ARENALES", "TRILLADORAS") &
      x[["LIN.NEG"]] %in% c(10000, 21000, 20000)
    fuente <- basename(archivos[[i]])
    # El archivo dedicado a 2023 prevalece sobre los registros 2023 del libro antiguo.
    data.frame(CLCliNit = as.character(x$CLIENTE[valido]),
      LinNegCod = ifelse(x[["LIN.NEG"]][valido] == 10000, 10000, 21000),
      FecMes = as.Date(format(fecha[valido], "%Y-%m-01")),
      SacosFact = suppressWarnings(as.numeric(x[["SS 70 KLS"]][valido])),
      Margen = suppressWarnings(as.numeric(x[["MNFCC$"]][valido])), Fuente = fuente,
      Prioridad = if (hoja == "BASE") 2L else 1L, stringsAsFactors = FALSE)
  })
  prioridades <- vapply(tablas, function(t) {
    if (!nrow(t)) stop("Un libro no contiene movimientos históricos válidos.")
    unique(t$Prioridad)
  }, integer(1))
  if (!setequal(prioridades, c(1L, 2L))) stop("Use un libro BASE PYG y un libro BASE, sin repetir versiones.")
  x <- dplyr::bind_rows(tablas)
  # Elegir un libro por mes/línea completo; no sumar dos versiones del mismo periodo.
  x <- x %>% dplyr::group_by(FecMes, LinNegCod) %>%
    dplyr::filter(Prioridad == max(Prioridad)) %>% dplyr::ungroup()
  x <- x %>% dplyr::group_by(CLCliNit, LinNegCod, FecMes) %>%
    dplyr::summarise(SacosFact = if (anyNA(SacosFact)) NA_real_ else sum(SacosFact),
      Margen = if (anyNA(Margen)) NA_real_ else sum(Margen),
      Fuente = paste(sort(unique(Fuente)), collapse = " | "), .groups = "drop")
  .ev_validar_historico(as.data.frame(x))
}
