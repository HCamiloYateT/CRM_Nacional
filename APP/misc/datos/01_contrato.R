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
  if (any(c("crm_maestro", "homologaciones") %in% ls(datos))) {
    .crm_validar_maestro(datos$crm_maestro, datos$homologaciones)
    datos <- .crm_adaptar_maestro(datos$crm_maestro, datos$homologaciones)
  }
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
      Segmento = ifelse(SegmentoAsignadoSistema, "SIN DATO", Segmento),
      SegmentoRacafe = ifelse(SegmentoRacafe %in% c(NA_character_, "", "SIN DATO") & !is.na(FecFact),
                              "CLIENTE", SegmentoRacafe),
      SegmentoRacafe = ifelse(SegmentoRacafe %in% c(NA_character_, ""), "SIN DATO", SegmentoRacafe),
      dplyr::across(c(Asesor, CLLinNegNo, Categoria, Producto),
                    ~ ifelse(.x %in% c(NA_character_, ""), "SIN DATO", .x))
    )
  if ("SegmentoVigente" %in% names(dat)) {
    # No sustituir el segmento asignado en CRM por el segmento financiero que se contrasta con él.
    resultado$SegmentacionClientePYGHistorica <- resultado$SegmentacionClientePYG
    resultado$SegmentacionClientePYG <- resultado$SegmentoVigente
  }
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
  bloqueos <- if ("HomologacionAmbigua" %in% names(productos)) productos %>%
    dplyr::filter(HomologacionAmbigua) %>% dplyr::distinct(LinNegCod, LinProCod) else
      productos[0, c("LinNegCod", "LinProCod")]
  # Clasificar por los códigos originales del pedido; los códigos del lote se conservan para auditoría.
  llaves <- paste0(claves, "Clasificacion")
  origen <- c("LinNegCodPedido", "LinProCodPedido", "NombreComercialPedidoCod", "MarcaPedidoCod")
  for (i in seq_along(claves)) dat[[llaves[[i]]]] <- if (origen[[i]] %in% names(dat))
    dplyr::coalesce(dat[[origen[[i]]]], dat[[claves[[i]]]]) else dat[[claves[[i]]]]
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
  exactos <- productos %>% dplyr::rename_with(~ paste0(.x, "Clasificacion"), dplyr::all_of(claves))
  generales <- generales %>% dplyr::rename(LinNegCodClasificacion = LinNegCod, LinProCodClasificacion = LinProCod)
  resultado <- dat %>%
    dplyr::left_join(exactos, by = llaves, na_matches = "never", relationship = "many-to-one") %>%
    dplyr::left_join(generales, by = c("LinNegCodClasificacion", "LinProCodClasificacion"),
      na_matches = "never", relationship = "many-to-one")
  for (columna in c("LinProNom", "Categoria", "Producto", "ProdExcluir")) {
    respaldo <- resultado[[paste0(columna, "Fallback")]]
    if (columna %in% c("Categoria", "Producto")) respaldo[paste(resultado$LinNegCodClasificacion,
      resultado$LinProCodClasificacion) %in% paste(bloqueos$LinNegCod, bloqueos$LinProCod)] <- NA_character_
    resultado[[columna]] <- dplyr::coalesce(resultado[[columna]], respaldo)
  }
  dplyr::select(resultado, -dplyr::ends_with("Fallback"), -dplyr::all_of(llaves))
}

# Exportación maestra de main ----
.crm_columnas_maestro <- c(.crm_columnas_data, "IdRegistroCRM", "EstadoFinanciero", "FechaCortePyG",
                           "SacosLote70", "SacosProducidos70", "SacosDespachados70", "SacosPedido70Linea",
                           "FechaCreacionPedido", "FechaProduccion", "FechaDespachoReal",
                           "CodigoMarcaLote", "CodigoNombreComercialLote", "FacturadosOperativos70")

.crm_validar_tabla <- function(dat, columnas, nombre) {
  if (!is.data.frame(dat) || !all(columnas %in% names(dat))) {
    stop(sprintf("Exportación incompatible: %s. Ejecute DataPrep.R actualizado.", nombre), call. = FALSE)
  }
  invisible(TRUE)
}

.crm_validar_maestro <- function(maestro, homologaciones) {
  .crm_validar_tabla(maestro, .crm_columnas_maestro, "crm_maestro")
  requeridas <- c("clientes", "clientes_historico", "nit_principal", "segmentos", "productos", "ordenes_compra",
                  "clientes_sistema", "facturacion", "margenes_pyg", "lotes")
  if (!is.list(homologaciones) || !all(requeridas %in% names(homologaciones))) {
    stop("homologaciones no contiene las referencias necesarias. Ejecute DataPrep.R actualizado.", call. = FALSE)
  }
  .crm_validar_clave(maestro, "IdRegistroCRM", "crm_maestro")
  for (columna in c("FecMes", "FecFact", "FechaCortePyG")) {
    if (!inherits(maestro[[columna]], "Date")) stop(paste("crm_maestro:", columna, "debe ser Date."), call. = FALSE)
  }
  cierres <- unique(maestro$FechaCortePyG)
  if (length(cierres) != 1L || is.na(cierres) || !is.finite(as.numeric(cierres))) {
    stop("crm_maestro debe declarar una única fecha de cierre válida.", call. = FALSE)
  }
  estados <- c("CERRADO", "FACTURADO_ABIERTO", "PENDIENTE", "PROYECTADO")
  if (anyNA(maestro$EstadoFinanciero) || any(!maestro$EstadoFinanciero %in% estados)) {
    stop("crm_maestro contiene estados financieros desconocidos.", call. = FALSE)
  }
  proyectados <- maestro$EstadoFinanciero == "PROYECTADO"
  if (any(!is.na(maestro$SacosFact[proyectados])) || any(!is.na(maestro$Margen[proyectados]))) {
    stop("Las proyecciones no pueden contener facturación o margen ejecutado.", call. = FALSE)
  }
  for (nombre in c("clientes", "clientes_historico")) {
    .crm_validar_tabla(homologaciones[[nombre]], c("CliNitPpal", "LinNegCod", "Segmento", "Asesor"), nombre)
  }
  .crm_validar_tabla(homologaciones$clientes_sistema, .crm_contrato_exportacion$n_cliente, "clientes_sistema")
  .crm_validar_tabla(homologaciones$facturacion, .crm_contrato_exportacion$fact, "facturacion")
  .crm_validar_tabla(homologaciones$margenes_pyg, .crm_contrato_exportacion$margenes_pyg, "margenes_pyg")
  .crm_validar_tabla(homologaciones$productos, .crm_contrato_exportacion$productos_snap, "productos")
  .crm_validar_tabla(homologaciones$segmentos, .crm_contrato_exportacion$segmentaciones_actual, "segmentos")
  .crm_validar_tabla(homologaciones$nit_principal, c("PerCod", "CliNitPpal"), "nit_principal")
  .crm_validar_tabla(homologaciones$ordenes_compra, c("Lote", "OrdenCompra"), "ordenes_compra")
  .crm_validar_tabla(homologaciones$lotes, c("CLLotCod", "CLCliNit"), "lotes")
  .crm_validar_clave(homologaciones$lotes, "CLLotCod", "lotes")
  .crm_validar_clave(homologaciones$facturacion, c("CLLotCod", "FecMesFact"), "facturacion")
  invisible(TRUE)
}

.crm_snapshot_productos <- function(productos) {
  claves <- c("LinNegCod", "LinProCod", "MCCod", "MrcCod")
  productos <- productos %>% dplyr::group_by(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::group_modify(function(d, llave) {
      if ("FecProceso" %in% names(d) && any(!is.na(d$FecProceso))) {
        d <- d[d$FecProceso %in% max(d$FecProceso, na.rm = TRUE), , drop = FALSE]
      }
      # Empates contradictorios quedan sin clasificación; nunca escoger una categoría arbitraria.
      campos <- intersect(c("Categoria", "Producto"), names(d))
      ambiguo <- nrow(unique(d[campos])) > 1L
      fila <- d[1L, , drop = FALSE]
      if (ambiguo) fila[campos] <- NA_character_
      fila$HomologacionAmbigua <- ambiguo
      fila
    }) %>% dplyr::ungroup()
  .crm_validar_clave(productos, claves, "productos")
  productos
}

.crm_operaciones_maestro <- function(maestro, productos, facturacion) {
  acumulada <- facturacion %>%
    dplyr::group_by(CLLotCod) %>%
    dplyr::summarise(SacosFact = sum(SacosFact, na.rm = TRUE), FecFact = max(FecFact), .groups = "drop")
  # Las cantidades ya están convertidas a 70 kg en el maestro; no volver a aplicar UMeFac.
  operaciones <- maestro %>%
    dplyr::filter(!is.na(CLPdcCod), !is.na(CLPdcLin)) %>%
    dplyr::arrange(EstadoFinanciero == "PROYECTADO", dplyr::desc(FecFact)) %>%
    dplyr::distinct(CLPdcCod, CLPdcLin, CLLotCod, .keep_all = TRUE) %>%
    dplyr::mutate(MCCod = dplyr::coalesce(MCCod, CodigoNombreComercialLote),
                  MrcCod = dplyr::coalesce(MrcCod, CodigoMarcaLote)) %>%
    dplyr::select(-dplyr::any_of(c("SacosFact", "FecFact"))) %>%
    .crm_enriquecer_productos(productos) %>%
    dplyr::left_join(acumulada, by = "CLLotCod", na_matches = "never") %>%
    dplyr::transmute(CLCliNit, LinNegCod, CLPdcCod, CLPdcLin, CLLotCod, LinProCod,
                     ProductoComercial = dplyr::coalesce(Producto, LinProNom),
                     PdcFecCre = FechaCreacionPedido, SacosPedido = SacosPedido70Linea,
                     SacLote = SacosLote70, SacProd = SacosProducidos70, SacDesp = SacosDespachados70,
                     FecProd = FechaProduccion, FecDesp = FechaDespachoReal, SacosFact, FecFact,
                     FuenteFactura = ifelse(!is.na(FecFact), "FACTURACIÓN", NA_character_))
  operaciones
}

.crm_adaptar_maestro <- function(maestro, homologaciones) {
  .crm_validar_maestro(maestro, homologaciones)
  h <- homologaciones
  datos <- new.env(parent = emptyenv())
  datos$crm_maestro <- maestro
  datos$homologaciones <- h
  # Las proyecciones se conservan en crm_maestro y se excluyen de la base de ejecución de los módulos existentes.
  datos$data <- maestro %>%
    dplyr::filter(EstadoFinanciero != "PROYECTADO") %>% dplyr::select(dplyr::all_of(.crm_columnas_data), dplyr::any_of(c("SegmentoVigente", "FechaSegmento", "SegmentoAmbiguo",
      "LinNegCodPedido", "LinProCodPedido", "MarcaPedidoCod", "NombreComercialPedidoCod", "MarcaPedido", "NombreComercialPedido",
      "PrecioNegociadoOriginal", "MonedaPrecioOriginal", "UnidadPrecioOriginal", "PrecioNegociadoKg", "EstadoFinanciero")))
  datos$n_cliente <- h$clientes_sistema
  datos$clientes_actual <- h$clientes
  datos$clientes_hist <- h$clientes_historico
  datos$clientes_raw <- h$clientes_historico
  datos$clientes_snap <- h$clientes
  datos$nit_ppal <- h$nit_principal
  datos$fact <- h$facturacion
  datos$margenes_pyg <- h$margenes_pyg
  datos$segmentaciones_actual <- h$segmentos
  datos$productos_snap <- .crm_snapshot_productos(h$productos)
  datos$ultimo_periodo_pyg <- unique(maestro$FechaCortePyG)[[1]]
  datos$ord_cmp <- h$ordenes_compra %>%
    dplyr::transmute(CLLotCod = as.numeric(Lote), OrdenCompra = trimws(as.character(OrdenCompra))) %>%
    dplyr::filter(!is.na(CLLotCod), CLLotCod != 0) %>%
    dplyr::group_by(CLLotCod) %>%
    dplyr::summarise(OrdenCompra = paste(sort(unique(OrdenCompra[!is.na(OrdenCompra) & nzchar(OrdenCompra)])),
                                        collapse = ", "), .groups = "drop")
  datos$fact_cliente <- h$facturacion %>%
    dplyr::left_join(dplyr::select(h$lotes, CLLotCod, CLCliNit), by = "CLLotCod", na_matches = "never") %>%
    dplyr::filter(!is.na(CLCliNit), !is.na(FecPrimerFact), !is.na(FecFact)) %>%
    dplyr::group_by(FctNit = CLCliNit) %>%
    dplyr::summarise(MinFecFact = min(FecPrimerFact), UltFecFact = max(FecFact),
                     SacosFact = sum(SacosFact, na.rm = TRUE), .groups = "drop")
  datos$crm_detalle_operaciones <- .crm_operaciones_maestro(maestro, datos$productos_snap, h$facturacion)
  .crm_validar_exportacion(datos)
  datos
}
