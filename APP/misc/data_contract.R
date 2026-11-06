# Contrato de exportación ----
.crm_columnas_data <- c(
  "FecMes", "ANHO", "MES", "CLASE", "CLSucCod", "Sucursal", "CLPdcCod", "CLPdcLin", "PdcRefCli", "PdcFecCre",
  "PdcPrecioKilo", "Usuario", "CLLotCod", "FechaEmbarque", "CLCliNit", "CodContacto", "TieneContacto", "EtapaContacto",
  "EstadoContacto", "EtapaPreDescarte", "CliNitPpal", "RazonSocialCliNit", "PerRazSoc", "Asesor", "Segmento",
  "SegmentacionClientePYG", "SSPpto", "MNFCCPpto", "NumMesesRecuperar", "Excluir", "CliCont", "CliDir", "CliDir1",
  "CliTel", "CliConCom", "CliTelCom", "CliEmlCom", "CiuExtNom", "LinNegCod", "CLLinNegNo", "LinProCod", "MCCod",
  "MrcCod", "CLPdcFctAD", "UMeFac", "CLPdcCntCl", "SacLote", "FecAsignLote", "CodOrdTril", "FecOrdTril", "SacProd",
  "FecProd", "CodDesp", "FecDesp", "SacDesp", "SacFact", "CLPdcCanFa", "CLLotSacXP", "CLLotPenXD", "CLLotDesXF",
  "FecFact", "SacosFact", "Margen", "MarSaco", "PendProducir", "PendDespachar", "PendFacturar"
)

.crm_contrato_exportacion <- list(
  data = .crm_columnas_data,
  n_cliente = c("PerCod", "PerRazSoc", "CliNitPpal", "SegmentoNCLIENTE"),
  fact_cliente = c("FctNit", "MinFecFact", "UltFecFact", "SacosFact"),
  clientes_actual = c("CliNitPpal", "LinNegCod", "Segmento"),
  clientes_snap = c("CliNitPpal", "LinNegCod", "Segmento", "Asesor"),
  fact = c("CLLotCod", "LinNegCod", "FecMesFact", "FecPrimerFact", "FecFact", "SacosFact"),
  nit_ppal = c("PerCod", "CliNitPpal"),
  margenes_pyg = c("FecMes", "ANHO", "FECFACTURA", "LOTE", "CLIENTE", "LINNEG", "SACOS70", "MARGEN",
                  "SEGMENTACION_CLIENTE"),
  productos_snap = c("LinNegCod", "LinProCod", "MCCod", "MrcCod", "LinProNom", "MCNom", "Marca", "Categoria",
                     "Producto", "Excluir"),
  segmentaciones_actual = c("CliNitPpal", "LinNegCod", "SegmentoRacafe"),
  ord_cmp = c("CLLotCod", "OrdenCompra"),
  crm_detalle_operaciones = c("CLCliNit", "LinNegCod", "CLPdcCod", "CLPdcLin", "CLLotCod", "PdcFecCre",
                             "LinProCod", "ProductoComercial", "SacosPedido", "SacLote", "SacProd", "FecProd",
                             "SacDesp", "FecDesp", "SacosFact", "FecFact", "FuenteFactura")
)

.crm_validar_clave <- function(dat, claves, nombre) {
  if (anyDuplicated(dat[claves])) {
    stop(sprintf("%s contiene claves duplicadas (%s). Ejecute DataPrep.R.", nombre, paste(claves, collapse = ", ")),
         call. = FALSE)
  }
  invisible(dat)
}

.crm_validar_exportacion <- function(datos) {
  requeridos <- c(names(.crm_contrato_exportacion), "ultimo_periodo_pyg")
  faltantes <- setdiff(requeridos, ls(datos, all.names = TRUE))
  if (length(faltantes)) {
    stop(paste("La exportación no contiene:", paste(faltantes, collapse = ", "), "Ejecute DataPrep.R."), call. = FALSE)
  }
  cierre <- datos$ultimo_periodo_pyg
  if (!inherits(cierre, "Date") || length(cierre) != 1L || is.na(cierre) || !is.finite(as.numeric(cierre))) {
    stop("ultimo_periodo_pyg debe contener una fecha de cierre válida. Ejecute DataPrep.R.", call. = FALSE)
  }
  for (nombre in names(.crm_contrato_exportacion)) {
    dat <- get(nombre, envir = datos, inherits = FALSE)
    columnas <- setdiff(.crm_contrato_exportacion[[nombre]], names(dat))
    if (!is.data.frame(dat) || length(columnas)) {
      stop(sprintf("Exportación incompatible: %s. Columnas faltantes: %s. Ejecute DataPrep.R.",
                   nombre, paste(columnas, collapse = ", ")), call. = FALSE)
    }
  }
  for (columna in c("FecMes", "FecFact")) {
    if (!inherits(datos$data[[columna]], "Date")) {
      stop(paste("data$", columna, " debe ser Date. Ejecute DataPrep.R.", sep = ""), call. = FALSE)
    }
  }
  .crm_validar_clave(datos$productos_snap, c("LinNegCod", "LinProCod", "MCCod", "MrcCod"), "productos_snap")
  .crm_validar_clave(datos$segmentaciones_actual, c("CliNitPpal", "LinNegCod"), "segmentaciones_actual")
  .crm_validar_clave(datos$ord_cmp, "CLLotCod", "ord_cmp")
  invisible(TRUE)
}

.crm_cargar_exportacion <- function(ruta, destino = globalenv()) {
  if (!file.exists(ruta)) stop("No existe data/data.RData. Ejecute DataPrep.R antes de iniciar el CRM.", call. = FALSE)
  datos <- new.env(parent = emptyenv())
  load(ruta, envir = datos)
  .crm_validar_exportacion(datos)
  list2env(as.list(datos, all.names = TRUE), envir = destino)
  invisible(TRUE)
}

# Enriquecimiento sin multiplicar hechos financieros ----
.crm_preparar_datos <- function(dat, productos, segmentaciones, ordenes) {
  claves_producto <- c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
  .crm_validar_clave(productos, claves_producto, "productos_snap")
  .crm_validar_clave(segmentaciones, c("CliNitPpal", "LinNegCod"), "segmentaciones_actual")
  .crm_validar_clave(ordenes, "CLLotCod", "ord_cmp")
  segmentaciones <- dplyr::select(segmentaciones, -dplyr::any_of("FecMes"))
  resultado <- dat %>%
    .crm_enriquecer_productos(productos) %>%
    dplyr::left_join(segmentaciones, by = c("CliNitPpal", "LinNegCod"), na_matches = "never") %>%
    dplyr::left_join(ordenes, by = "CLLotCod", na_matches = "never") %>%
    dplyr::mutate(
      SegmentoAsignadoSistema = Segmento %in% c(NA_character_, "", "SIN DATO"),
      Excluir = ifelse(Excluir %in% c(NA_character_, "", "SIN DATO"), "NO", Excluir),
      ProdExcluir = ifelse(ProdExcluir %in% c(NA_character_, "", "SIN DATO"), "NO", ProdExcluir),
      Segmento = dplyr::case_when(
        !SegmentoAsignadoSistema ~ Segmento,
        LinNegCod == 10000 & SacLote <= 240 ~ "DETAL",
        LinNegCod == 10000 & SacLote > 240 ~ "MEDIANO",
        LinNegCod == 21000 & SacLote < 50 ~ "DETAL",
        LinNegCod == 21000 & SacLote >= 50 ~ "MEDIANO",
        TRUE ~ "SIN DATO"
      ),
      SegmentoRacafe = ifelse(SegmentoRacafe %in% c(NA_character_, "", "SIN DATO") & !is.na(FecFact),
                              "CLIENTE", SegmentoRacafe),
      SegmentoRacafe = ifelse(SegmentoRacafe %in% c(NA_character_, ""), "SIN DATO", SegmentoRacafe),
      dplyr::across(c(Asesor, CLLinNegNo, Categoria, Producto),
                    ~ ifelse(.x %in% c(NA_character_, ""), "SIN DATO", .x))
    )
  if (nrow(resultado) != nrow(dat)) stop("El enriquecimiento multiplicó registros de data.", call. = FALSE)
  resultado
}

# Detalle operativo independiente de las filas mensuales del PyG ----
.crm_preparar_operaciones <- function(operaciones, pedidos, facturacion, productos) {
  .crm_validar_clave(pedidos, c("PdcCod", "PdcLin"), "ped")
  .crm_validar_clave(facturacion, "CLLotCod", "fact_lote")
  claves_producto <- c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
  .crm_validar_clave(productos, claves_producto, "productos_snap")
  cantidades <- pedidos %>%
    dplyr::transmute(CLPdcCod = PdcCod, CLPdcLin = PdcLin,
                     SacosPedido = as.numeric(PdcCan) * ifelse(UMeFac == 62.5, 62.5 / 70, 1))
  operaciones %>%
    dplyr::left_join(cantidades, by = c("CLPdcCod", "CLPdcLin")) %>%
    .crm_enriquecer_productos(productos) %>%
    dplyr::left_join(dplyr::select(facturacion, CLLotCod, SacosFact, FecFact), by = "CLLotCod") %>%
    dplyr::mutate(
      dplyr::across(c(SacLote, SacProd, SacDesp), ~ as.numeric(.x) * ifelse(UMeFac == 62.5, 62.5 / 70, 1)),
      dplyr::across(c(PdcFecCre, FecProd, FecDesp, FecFact),
                    ~ dplyr::na_if(as.Date(.x), as.Date("1753-01-01"))),
      ProductoComercial = dplyr::coalesce(Producto, LinProNom, as.character(CLLinProNo)),
      FuenteFactura = ifelse(!is.na(FecFact), "FACTURACIÓN", NA_character_)
    ) %>%
    dplyr::select(CLCliNit, LinNegCod, CLPdcCod, CLPdcLin, CLLotCod, PdcFecCre, LinProCod, ProductoComercial,
                  SacosPedido, SacLote, SacProd, FecProd, SacDesp, FecDesp, SacosFact, FecFact, FuenteFactura)
}

# Completar dimensiones solo cuando el código de producto determina un valor único ----
.crm_enriquecer_productos <- function(dat, productos) {
  claves <- c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
  .crm_validar_clave(productos, claves, "productos_snap")
  productos <- dplyr::select(productos, dplyr::all_of(claves), LinProNom, MCNom, Marca, Categoria, Producto,
                             ProdExcluir = Excluir)
  .valor_unico <- function(x) {
    valores <- unique(as.character(x[!is.na(x) & nzchar(trimws(as.character(x)))]))
    if (length(valores) == 1L) valores[[1]] else NA_character_
  }
  # PyG conserva LinProCod; marca y empaque pueden faltar sin perder el hecho financiero.
  generales <- productos %>%
    dplyr::group_by(LinNegCod, LinProCod) %>%
    dplyr::summarise(dplyr::across(-dplyr::all_of(c("MCCod", "MrcCod")), .valor_unico), .groups = "drop") %>%
    dplyr::rename_with(~ paste0(.x, "Fallback"), -dplyr::all_of(c("LinNegCod", "LinProCod")))
  resultado <- dat %>%
    dplyr::left_join(productos, by = claves, na_matches = "never") %>%
    dplyr::left_join(generales, by = c("LinNegCod", "LinProCod"), na_matches = "never")
  for (columna in c("LinProNom", "MCNom", "Marca", "Categoria", "Producto", "ProdExcluir")) {
    resultado[[columna]] <- dplyr::coalesce(resultado[[columna]], resultado[[paste0(columna, "Fallback")]])
  }
  dplyr::select(resultado, -dplyr::ends_with("Fallback"))
}
