# Generales -----
# Funciones Indicadores ----
.GetFncData <- function() {
  tryCatch({
    contenido <- rvest::read_html("https://federaciondecafeteros.org/wp/")
    
    items <- contenido %>%
      rvest::html_elements("ul.e-n-menu-heading li.e-n-menu-item span.e-n-menu-title-text") %>%
      rvest::html_text(trim = TRUE)
    
    precio_raw <- items[[1]]
    bolsa_raw <- items[[2]]
    
    precio <- as.numeric(gsub("\\D", "", precio_raw))
    bolsa <- bolsa_raw %>%
      gsub(".*:\\s*\\$?", "", .) %>%
      gsub(",", ".", .) %>%
      as.numeric()
    
    list(
      precio = precio,
      bolsa = bolsa
    )
  }, error = function(e) {
    warning("Error obteniendo datos de FNC: ", e$message)
    
    list(
      precio = NA_real_,
      bolsa = NA_real_
    )
  })
}
.GetSystemData <- function(uid, pwd) {
  tryCatch({
    cons_main <- ConsultaSistema(
      "syscafe",
      paste0(
        "SELECT (SELECT MedTRM FROM EXPMECAF ",
        "WHERE MedFec = (SELECT MAX(MedFec) FROM EXPMECAF)) AS TRM"
      )
    )
    
    ny <- ConsultaSistema(
      "syscafe",
      paste0(
        "WITH Futuros AS (",
        "SELECT InFuFch AS Fecha, ",
        "DATEFROMPARTS(CAST(AnoBol AS INT), CAST(MesBolCod AS INT), 1) AS Date, ",
        "AVG(InFuCieVr) AS InFuCieVr, ",
        "ROW_NUMBER() OVER (PARTITION BY InFuFch ",
        "ORDER BY DATEFROMPARTS(CAST(AnoBol AS INT), CAST(MesBolCod AS INT), 1)) AS Posicion ",
        "FROM INFFUT1 ",
        "WHERE CiaCod = 10 AND TipCFCod = 'A' ",
        "AND InFuFch >= (SELECT MAX(InFuFch) FROM INFFUT1 ",
        "WHERE CiaCod = 10 AND TipCFCod = 'A') ",
        "AND InFuCieVr > 0 ",
        "GROUP BY InFuFch, AnoBol, MesBolCod",
        ") ",
        "SELECT Fecha, InFuCieVr AS NY ",
        "FROM Futuros ",
        "WHERE Posicion = 2"
      )
    )
    
    precio_carga_raw <- ConsultaSistema(
      "syscafe",
      paste0(
        "SELECT h2.HTFec, ",
        "SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) AS KilTo, ",
        "SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE ",
        "((h2.HTPreCom * (f.FactRen / 125.0) - ",
        "(f.FactRen * ((h.HTPreCon * f.PorCon / 100.0) + ",
        "(h.HTPrePas * f.PorPas / 100.0) + ",
        "(h.HTPreRip * f.PorRip / 100.0))) + ",
        "(f.FactBas * ((h.HTPreCon * f.PorCon / 100.0) + ",
        "(h.HTPrePas * f.PorPas / 100.0) + ",
        "(h.HTPreRip * f.PorRip / 100.0)))) * ",
        "(125.0 / f.FactBas) * h2.HTKilCom) END) AS PreTo ",
        "FROM EXPHOTR2 h2 ",
        "LEFT JOIN EXPFACON f ON h2.FactSec = f.FactSec ",
        "LEFT JOIN EXPHOTRA h ON h2.HTFec = h.HTFec ",
        "WHERE h2.HTFec = (SELECT MAX(HTFec) FROM EXPHOTR2) ",
        "GROUP BY h2.HTFec ",
        "HAVING SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) > 0"
      )
    )
    
    precio_carga <- precio_carga_raw %>%
      dplyr::group_by(Fecha = as.Date(HTFec)) %>%
      dplyr::summarise(PrecioCarga = mean(PreTo / KilTo, na.rm = TRUE), .groups = "drop")
    
    precios_adicionales <- ConsultaSistema(
      "syscafe",
      paste0(
        "SELECT HTPreRip, HTPrePas, HTPreCon, ",
        "CASE WHEN HTSigno = '+' THEN HTPtos ELSE -HTPtos END AS Diferencial ",
        "FROM EXPHOTRA ",
        "WHERE HTFec = (SELECT MAX(HTFec) FROM EXPHOTR2);"
      )
    )
    
    compras_arenales <- ConsultaSistema(
      "cafesys",
      paste0(
        "SELECT r.ResFch, r.TipCaf, r.CalCod, r.TotKls, r.ResVlrNeg, ",
        "c.CalNom AS Producto ",
        "FROM RESDIA r ",
        "LEFT JOIN CALTRN c ON r.TipCaf = c.TipCaf AND r.CalCod = c.CalCod ",
        "WHERE r.CiaCod = 10 AND r.SucCod = 32 ",
        "AND r.ResFch >= DATEADD(month, -1, GETDATE()) ",
        "AND r.TMoDes = 'COMPRAS        '"
      )
    ) %>%
      dplyr::mutate(VlrKilo = ResVlrNeg / TotKls) %>%
      dplyr::group_by(Producto) %>%
      dplyr::summarise(VlrKilo = stats::weighted.mean(VlrKilo, TotKls, na.rm = TRUE),
                       .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Producto, values_from = VlrKilo)
    
    list(
      trm = cons_main$TRM,
      ny = ny$NY,
      precio_carga = precio_carga$PrecioCarga,
      precios_adicionales = precios_adicionales,
      precios_compras = compras_arenales
    )
  }, error = function(e) {
    warning("Error obteniendo datos del sistema: ", e$message)
    
    list(
      trm = NA_real_,
      ny = NA_real_,
      precio_carga = NA_real_,
      precios_adicionales = data.frame(
        HTPreRip = NA_real_,
        HTPrePas = NA_real_,
        HTPreCon = NA_real_,
        Diferencial = NA_real_
      ),
      precios_compras = data.frame(
        CONSUMO = NA_real_,
        MOLIDOS = NA_real_,
        SOLUBLE = NA_real_,
        RIPIO = NA_real_,
        ROBUSTA = NA_real_
      )
    )
  })
}
.ValorColumnaIndicador <- function(datos, columna) {
  if (is.null(datos) || nrow(datos) == 0L || !columna %in% names(datos)) return(NA_real_)
  
  valor <- suppressWarnings(as.numeric(datos[[columna]][[1]]))
  
  if (length(valor) == 0L) return(NA_real_)
  
  valor
}
.ObtenerIndicadoresActuales <- function(uid, pwd) {
  fnc_data <- .GetFncData()
  system_data <- .GetSystemData(uid = uid, pwd = pwd)
  
  precio_fnc <- fnc_data$precio
  precio_carga <- system_data$precio_carga
  
  datos <- data.frame(
    PrecioBolsa = fnc_data$bolsa,
    TRM = system_data$trm,
    PrecioFNC = precio_fnc,
    UGCFNC = (precio_fnc / 125) / (70 / 96.89),
    PrecioNY = system_data$ny,
    PrecioCarga = precio_carga,
    Diferencial = .ValorColumnaIndicador(system_data$precios_adicionales, "Diferencial"),
    UGCRacafe = (precio_carga / 125) / (70 / 96.89),
    CALConsumo = .ValorColumnaIndicador(system_data$precios_adicionales, "HTPreCon"),
    COMConsumo = .ValorColumnaIndicador(system_data$precios_compras, "CONSUMO"),
    CALPasilla = .ValorColumnaIndicador(system_data$precios_adicionales, "HTPrePas"),
    COMMolidos = .ValorColumnaIndicador(system_data$precios_compras, "MOLIDOS"),
    COMSoluble = .ValorColumnaIndicador(system_data$precios_compras, "SOLUBLE"),
    CALRipio = .ValorColumnaIndicador(system_data$precios_adicionales, "HTPreRip"),
    COMRipio = .ValorColumnaIndicador(system_data$precios_compras, "RIPIO"),
    COMRobusta = .ValorColumnaIndicador(system_data$precios_compras, "ROBUSTA")
  )
  
  datos %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Item", values_to = "Valor") %>%
    dplyr::mutate(
      Item = dplyr::recode(Item, !!!.IND_NOMBRES_CORTOS),
      Item = factor(Item, levels = unname(.IND_NOMBRES_CORTOS))
    ) %>%
    dplyr::arrange(Item)
}
.CargarHistoricoIndicadores <- function(fecha) {
  tryCatch(
    CargarDatos("CRMINDICADORES") %>%
      dplyr::mutate(FechaActualizacion = as.Date(FechaActualizacion)) %>%
      dplyr::filter(FechaActualizacion == fecha) %>%
      dplyr::arrange(dplyr::desc(FechaActualizacion)) %>%
      dplyr::slice(1),
    error = function(e) data.frame()
  )
}
.CargarAnteriorIndicadores <- function(fecha = Sys.Date() - 1) {
  datos <- .CargarHistoricoIndicadores(fecha)
  
  if (nrow(datos) == 0L) {
    return(tibble::tibble(Item = character(), anterior = numeric()))
  }
  
  columnas_disponibles <- intersect(.IND_CLAVES_CANON, names(datos))
  
  if (length(columnas_disponibles) == 0L) {
    return(tibble::tibble(Item = character(), anterior = numeric()))
  }
  
  datos %>%
    dplyr::select(dplyr::all_of(columnas_disponibles)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Item", values_to = "anterior") %>%
    dplyr::mutate(Item = dplyr::recode(Item, !!!.IND_NOMBRES_CORTOS))
}
.CargarDisponibleIndicadores <- function() {
  tryCatch(
    readRDS(.IND_RUTA_DISPONIBLE),
    error = function(e) NULL
  )
}
.PrepararDisponibleIndicadores <- function(datos) {
  datos %>%
    dplyr::filter(Tipo_Negocio == "Disponible") %>%
    dplyr::select(Total_Sacos, PrecioxCarga) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Concepto", values_to = "Valor") %>%
    dplyr::mutate(
      Concepto = dplyr::recode(
        Concepto,
        "Total_Sacos" = "Sacos",
        "PrecioxCarga" = "Precio / Carga"
      )
    )
}
.FechaDisponibleIndicadores <- function(datos) {
  tryCatch(
    format(max(as.Date(datos$Fecha), na.rm = TRUE), "%d %b %Y"),
    error = function(e) "—"
  )
}
.PrepararTendenciaIndicadores <- function(datos_actuales, datos_anteriores, formato) {
  orden <- unname(.IND_NOMBRES_CORTOS)
  
  datos_actuales %>%
    dplyr::mutate(Item = as.character(Item)) %>%
    dplyr::left_join(datos_anteriores, by = "Item") %>%
    dplyr::mutate(
      tendencia = dplyr::case_when(
        is.na(anterior) ~ "igual",
        Valor > anterior ~ "sube",
        Valor < anterior ~ "baja",
        TRUE ~ "igual"
      ),
      color = unname(.IND_COLORES_TENDENCIA[tendencia]),
      flecha = unname(.IND_FLECHAS_TENDENCIA[tendencia]),
      Valor = paste0(
        "<span style='color:", color, ";font-weight:600;font-size:11px;'>",
        flecha, " ", formato(Valor), "</span>"
      ),
      Item = factor(Item, levels = orden)
    ) %>%
    dplyr::arrange(Item) %>%
    dplyr::select(Item, Valor)
}
.NormalizarIndicadoresActuales <- function(datos) {
  if (is.null(datos) || nrow(datos) == 0L) return(tibble::tibble())
  
  datos %>%
    dplyr::mutate(Item = as.character(Item)) %>%
    dplyr::select(Item, Valor) %>%
    dplyr::mutate(Item = dplyr::recode(Item, !!!.IND_NOMBRES_INV)) %>%
    tidyr::pivot_wider(names_from = Item, values_from = Valor)
}
.ExtraerValorIndicador <- function(datos, columna) {
  if (!columna %in% names(datos)) return(NA_real_)
  
  valor <- suppressWarnings(as.numeric(datos[[columna]]))
  
  if (length(valor) == 0L || is.na(valor[[1]])) return(NA_real_)
  
  valor[[1]]
}
.ConstruirComparacionIndicadores <- function(datos_anteriores, datos_actuales) {
  columnas_disponibles <- .IND_CLAVES_CANON[
    .IND_CLAVES_CANON %in% names(datos_anteriores) &
      .IND_CLAVES_CANON %in% names(datos_actuales)
  ]
  
  tibble::tibble(Indicador = columnas_disponibles) %>%
    dplyr::mutate(
      Nombre_Indicador = unname(.IND_NOMBRES_COMPARACION[Indicador]),
      Valor_Anterior = purrr::map_dbl(
        Indicador,
        ~ .ExtraerValorIndicador(datos_anteriores, .x)
      ),
      Valor_Actual = purrr::map_dbl(
        Indicador,
        ~ .ExtraerValorIndicador(datos_actuales, .x)
      ),
      Diferencia_Absoluta = Valor_Actual - Valor_Anterior,
      Variacion_Porcentual = Variacion(ini = Valor_Anterior, fin = Valor_Actual)
    ) %>%
    dplyr::select(
      Nombre_Indicador, Valor_Anterior, Valor_Actual,
      Diferencia_Absoluta, Variacion_Porcentual
    )
}
.TablaComparacionIndicadoresVacia <- function(mensaje) {
  data.frame(Mensaje = mensaje) %>%
    gt::gt() %>%
    gt::cols_label(Mensaje = "") %>%
    gt::tab_options(table.width = gt::pct(100), column_labels.hidden = TRUE) %>%
    gt::tab_style(
      style = gt::cell_text(align = "center", style = "italic", color = "#64748B"),
      locations = gt::cells_body()
    )
}


