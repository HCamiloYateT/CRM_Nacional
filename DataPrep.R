tictoc::tic("CRM NACIONAL")
print(paste0("*********** ", Sys.time(), " ***********"))
setwd("/home/compartido/APP/8_IndustriaNacional/CRM_Nacional/")
Sys.setenv(LANG = "es_CO.UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")
options(dplyr.summarise.inform = FALSE,
        OutDec = ".",
        scipen = 999,
        lubridate.week.start = 1,
        repos = c(CRAN = "https://cloud.r-project.org"))

# Librerias ----
REQUIRED_PACKAGES <- c("racafeCore", "racafeBD", "racafeDrive", "tidyverse",
                       "lubridate", "openxlsx2", "readxl", "rfm", "CLVTools",
                       "future", "furrr", "gdata", "janitor", "tictoc")
racafeCore::Loadpkg(REQUIRED_PACKAGES)

# Funciones ----

## Lotes ----
.lotes_full_sql <- function() {
  racafeBD::ConsultaSistema("syscafe",
                            query = "SELECT  CLSucCod, CLLotCod, CLPdcCod, CLPdcLin,
                   CLPdcAnoEm*100 + CLPdcMesEm AS Periodo,
                   CLPdcCntCl, CLLotCan AS SacLote, CLLotFec AS FecAsignLote,
                   CLotrCod AS CodOrdTril, CLOTrFec AS FecOrdTril,
                   CLLotSacPr AS SacProd, CLLotFecPr AS FecProd,
                   CLIDeCod AS CodDesp, CLIDeFec AS FecDesp,
                   CLLotFchDe AS FecDespachoReal, CLLotSacDe AS SacDesp,
                   CLPdcFctAD, CLLotSacFa AS SacFact, CLPdcCanFa, CLLinNegCo,
                   CLLotSacXP, CLLotPenXD, CLLotDesXF, CLCliNit, CLCliRazSo,
                   CLLinNegCo AS LinNegCod, CLLinNegNo,
                   CLLinProCo AS LinProCod, CLLinProNo,
                   CLMCCod AS MCCod, CLMCNom, CLMrcCod AS MrcCod, CLMrcNom
             FROM  EXPCUALO
             WHERE CiaCod = 10
               AND (CLLinNegCo = 21000 OR
                        (CLLinNegCo = 10000
                           AND CLLinProCo IN (90030, 90040, 40050, 40100, 90025, 40150, 40200, 40010, 90035, 40250)
                        )
                    )
               AND  CLCliNit <> 32
               AND  CLLinNegCo IN (10000, 21000)
               AND  CLLotCan > 0")
}
.lotes_activos_sql <- function(anho_corte) {
  racafeBD::ConsultaSistema(
    "syscafe",
    query = sprintf(
      "SELECT CLSucCod, CLLotCod, CLPdcCod, CLPdcLin,
              CLPdcAnoEm*100 + CLPdcMesEm AS Periodo,
              CLPdcCntCl, CLLotCan AS SacLote, CLLotFec AS FecAsignLote,
              CLotrCod AS CodOrdTril, CLOTrFec AS FecOrdTril,
              CLLotSacPr AS SacProd, CLLotFecPr AS FecProd,
              CLIDeCod AS CodDesp, CLIDeFec AS FecDesp,
                   CLLotFchDe AS FecDespachoReal, CLLotSacDe AS SacDesp,
              CLPdcFctAD, CLLotSacFa AS SacFact, CLPdcCanFa, CLLinNegCo,
              CLLotSacXP, CLLotPenXD, CLLotDesXF, CLCliNit, CLCliRazSo,
              CLLinNegCo AS LinNegCod, CLLinNegNo,
              CLLinProCo AS LinProCod, CLLinProNo,
              CLMCCod AS MCCod, CLMCNom, CLMrcCod AS MrcCod, CLMrcNom
       FROM   EXPCUALO
       WHERE  CiaCod = 10
         AND  (CLLinNegCo = 21000 OR
                        (CLLinNegCo = 10000
                           AND CLLinProCo IN (90030, 90040, 40050, 40100, 90025, 40150, 40200, 40010, 90035, 40250)
                        )
              )
         AND  CLCliNit <> 32
         AND  CLLinNegCo IN (10000, 21000)
         AND  CLLotCan > 0
         AND  YEAR(CONVERT(date, CLLotFec, 23)) >= %d",
      anho_corte
    )
  )
}
.cargar_lotes_incremental <- function(datos_previos = NULL) {
  if (is.null(datos_previos) || nrow(datos_previos) == 0L) {
    message("Sin cache de lotes: ejecutando carga completa...")
    return(.lotes_full_sql())
  }
  anho_corte    <- lubridate::year(Sys.Date()) - 1L
  lotes_activos <- .lotes_activos_sql(anho_corte)
  
  lotes_congelados <- datos_previos %>%
    anti_join(lotes_activos, by = "CLLotCod")
  
  bind_rows(lotes_congelados, lotes_activos)
}

## Facturación ----
.cargar_fact <- function() {
  fact_sys_nal <- racafeBD::ConsultaSistema(
    "syscafe",
    query = "SELECT F1.FcnLot AS CLLotCod,
                    F2.FcnFec AS FecFact,
                    SUM(
                      CASE
                        WHEN UM.UMeFac = 62.5 THEN F1.FcnSacLot * 62.5 / 70.0
                        ELSE F1.FcnSacLot
                      END
                    ) AS SacosFact
             FROM FCTFACN1 F1
             LEFT JOIN FCTFACNA F2
               ON F1.FcnNum = F2.FcnNum
              AND F1.CiaCod = F2.CiaCod
             LEFT JOIN EXPPEDID PD
               ON F1.PdcCod = PD.PdcCod
              AND F1.CiaCod = PD.CiaCod
             LEFT JOIN NUNIMEDI UM
               ON PD.UMeCod = UM.UMeCod
             WHERE F1.CiaCod = 10
               AND F2.CiaCod = 10
               AND F2.FcnEtd = 'C'
             GROUP BY F1.FcnLot, F2.FcnFec"
  )
  
  fact_sys_ext <- racafeBD::ConsultaSistema(
    "syscafe",
    query = "SELECT F1.FctLot AS CLLotCod,
                    F2.FctFec AS FecFact,
                    SUM(
                      CASE
                        WHEN UM.UMeFac = 62.5 THEN F1.FctSacEmb * 62.5 / 70.0
                        ELSE F1.FctSacEmb
                      END
                    ) AS SacosFact
             FROM FCTFACC2 F1
             LEFT JOIN FCTFACCA F2
               ON F1.FctNum = F2.FctNum
              AND F1.CiaCod = F2.CiaCod
             LEFT JOIN EXPPEDID PD
               ON F2.PdcCod = PD.PdcCod
              AND F2.CiaCod = PD.CiaCod
             LEFT JOIN NUNIMEDI UM
               ON PD.UMeCod = UM.UMeCod
             WHERE F1.CiaCod = 10
               AND F2.CiaCod = 10
               AND F1.FctLotAnu <> 'S'
             GROUP BY F1.FctLot, F2.FctFec"
  )
  
  dplyr::bind_rows(fact_sys_nal, fact_sys_ext) %>%
    dplyr::mutate(CLLotCod = suppressWarnings(as.numeric(CLLotCod)),
                  FecFact = as.Date(FecFact),
                  FecMesFact = lubridate::floor_date(FecFact, "month"),
                  SacosFact = suppressWarnings(as.numeric(SacosFact))) %>%
    dplyr::filter(!is.na(CLLotCod), !is.na(FecFact), FecFact >= fecha_inicio_analisis) %>%
    dplyr::group_by(CLLotCod, FecMesFact) %>%
    dplyr::summarise(FecPrimerFact = min(FecFact, na.rm = TRUE),
                     FecFact = max(FecFact, na.rm = TRUE),
                     SacosFact = sum(SacosFact, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(Facturado = TRUE)
}

## Márgenes ----
.seleccionar_hoja_base <- function(hojas) {
  bases <- hojas[grepl("^BASE( \\d{4})?$", hojas, ignore.case = TRUE)]
  if (length(bases) == 0L) {
    return(NA_character_)
  }
  
  if (any(grepl("\\d{4}", bases))) {
    anhos <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", bases, perl = TRUE))
    anhos[is.na(anhos)] <- 0L
    return(bases[which.max(anhos)])
  }
  
  bases[[1L]]
}
.limpiar_nombres_marlot <- function(df) {
  df <- df %>%
    dplyr::select(!dplyr::matches("^\\.\\.\\.[0-9]+$"))
  
  names(df) <- janitor::make_clean_names(
    names(df),
    case = "snake"
  ) %>%
    toupper()
  
  df
}
.convertir_fecha_margen <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  
  x <- trimws(as.character(x))
  
  resultado <- rep(as.Date(NA), length(x))
  
  es_serial <- grepl("^\\d+(\\.\\d+)?$", x)
  
  if (any(es_serial, na.rm = TRUE)) {
    resultado[es_serial] <- as.Date(suppressWarnings(as.numeric(x[es_serial])), origin = "1899-12-30")
  }
  
  es_fecha <- !es_serial &
    !is.na(x) &
    x != ""
  
  if (any(es_fecha)) {
    resultado[es_fecha] <- suppressWarnings(
      lubridate::parse_date_time(
        x[es_fecha],
        orders = c("Ymd", "Y-m-d", "dmy", "d/m/Y", "dmY", "m/d/Y", "mdY", "Ymd HMS", "Y-m-d HMS"),
        quiet = TRUE
      ) %>%
        as.Date()
    )
  }
  
  resultado
}
.normalizar_base_margen <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }
  
  df <- .limpiar_nombres_marlot(df)
  
  equivalencias <- c("LIN_NEG" = "LINNEG",
                     "FEC_FACTURA" = "FECFACTURA",
                     "PRODUCTO_SEGUN_MARCA" = "PRODUCTO",
                     "SS_70_KLS" = "SACOS70",
                     "MNFCC" = "MARGEN")
  
  for (actual in names(equivalencias)) {
    nuevo <- unname(equivalencias[[actual]])
    
    if (actual %in% names(df) && !nuevo %in% names(df)) {
      names(df)[names(df) == actual] <- nuevo
    }
  }
  
  if ("FECFACTURA" %in% names(df)) {
    df$FECFACTURA <- .convertir_fecha_margen(df$FECFACTURA)
  }
  
  if (!"ANHO" %in% names(df) && "FECFACTURA" %in% names(df)) {
    df <- df %>%
      mutate(ANHO = year(FECFACTURA))
  }
  
  if ("ANHO" %in% names(df)) {
    df <- df %>%
      mutate(ANHO = as.integer(ANHO))
  }
  
  if ("CLASE" %in% names(df)) {
    df <- df %>%
      mutate(CLASE = as.character(CLASE))
  }
  
  df
}
.homologar_tipos_margenes <- function(datos) {
  datos <- purrr::compact(datos)
  
  if (length(datos) == 0L) {
    return(datos)
  }
  
  columnas <- unique(unlist(purrr::map(datos, names), use.names = FALSE))
  
  for (columna in columnas) {
    clases <- purrr::map_chr(
      datos,
      function(df) {
        if (!columna %in% names(df)) {
          return("ausente")
        }
        clase <- class(df[[columna]])[[1L]]
        
        if (is.null(clase) || length(clase) == 0L) {
          return("unknown")
        }
        clase
      }
    )
    
    clases <- setdiff(unique(clases), "ausente")
    
    if (length(clases) <= 1L) {
      next
    }
    
    if (columna == "FECFACTURA") {
      datos <- purrr::map(
        datos,
        function(df) {
          if (columna %in% names(df)) {
            df[[columna]] <- .convertir_fecha_margen(df[[columna]])
          }
          
          df
        }
      )
      
      next
    }
    
    if (any(clases %in% c("character", "factor", "Date", "POSIXct", "POSIXlt"))) {
      datos <- purrr::map(
        datos,
        function(df) {
          if (columna %in% names(df)) {
            df[[columna]] <- as.character(df[[columna]])
          }
          
          df
        }
      )
      
      next
    }
    
    if (all(clases %in% c("logical", "integer", "numeric"))) {
      datos <- purrr::map(
        datos,
        function(df) {
          if (columna %in% names(df)) {
            df[[columna]] <- suppressWarnings(as.numeric(df[[columna]]))
          }
          
          df
        }
      )
    }
  }
  
  datos
}
.cargar_margenes_onedrive <- function(usuario, id_carpeta_raiz) {
  anho <- racafeDrive::ListarContenidoCarpetaId(usuario, id_carpeta_raiz) %>%
    dplyr::filter(stringr::str_detect(nombre, "^AÑO\\s\\d{4}$")) %>%
    dplyr::mutate(anho_num = as.integer(gsub("^AÑO (\\d{4})$", "\\1", nombre))) %>%
    dplyr::slice_max(anho_num, n = 1L, with_ties = FALSE) %>%
    dplyr::select(nombre, id, anho_num)
  
  if (nrow(anho) == 0L) {
    message("No se encontró carpeta de año")
    return(NULL)
  }
  
  meses <- racafeDrive::ListarContenidoCarpetaId(usuario, anho$id[[1L]]) %>%
    dplyr::filter(grepl("^\\d{2}\\.", nombre)) %>%
    dplyr::mutate(mes_num = as.integer(sub("^(\\d{2})\\. .*$", "\\1", nombre))) %>%
    dplyr::arrange(dplyr::desc(mes_num)) %>%
    dplyr::select(nombre, id, mes_num)
  
  if (nrow(meses) == 0L) {
    message("No se encontró carpeta de mes")
    return(NULL)
  }
  
  mes <- NULL
  archivos_pyg <- NULL
  
  for (i in seq_len(nrow(meses))) {
    archivos_mes <- racafeDrive::ListarContenidoCarpetaId(usuario, meses$id[[i]]) %>%
      dplyr::filter(
        grepl(
          "^\\d{2}\\. PYG POR LOTE( AÑO)? \\d{4}.*(COPRODUCTOS|A LA MEDIDA).*\\.xlsx$",
          nombre,
          ignore.case = TRUE
        )
      ) %>%
      dplyr::mutate(
        clase = dplyr::case_when(
          stringr::str_detect(stringr::str_to_upper(nombre), "COPRODUCTOS") ~ "Coproductos",
          stringr::str_detect(stringr::str_to_upper(nombre), "A LA MEDIDA") ~ "A la Medida",
          TRUE ~ "Sin clasificar"
        )
      ) %>%
      dplyr::select(nombre, id, clase)
    
    tiene_coproductos <- any(archivos_mes$clase == "Coproductos")
    tiene_a_la_medida <- any(archivos_mes$clase == "A la Medida")
    
    if (tiene_coproductos && tiene_a_la_medida) {
      mes <- meses[i, ]
      
      archivos_pyg <- archivos_mes %>%
        dplyr::filter(clase %in% c("Coproductos", "A la Medida")) %>%
        dplyr::group_by(clase) %>%
        dplyr::slice(1L) %>%
        dplyr::ungroup()
      
      break
    }
    
    message(
      "Mes sin archivos PYG completos: ",
      meses$nombre[[i]],
      " | Coproductos: ",
      ifelse(tiene_coproductos, "SI", "NO"),
      " | A la Medida: ",
      ifelse(tiene_a_la_medida, "SI", "NO")
    )
  }
  
  if (is.null(mes)) {
    message("No se encontraron meses con archivos PYG de Coproductos y A la Medida en: ", anho$nombre[[1L]])
    return(NULL)
  }
  
  message("PYG seleccionado: ", anho$nombre[[1L]], " / ", mes$nombre[[1L]])
  
  resultados <- purrr::map(
    seq_len(nrow(archivos_pyg)),
    function(i) {
      archivo <- archivos_pyg[i, ]
      
      ruta <- tryCatch(
        racafeDrive::DescargarArchivoId(archivo_id = archivo$id[[1L]], usuario = usuario),
        error = function(e) {
          message("Error descarga ", archivo$nombre[[1L]], ": ", conditionMessage(e))
          NULL
        }
      )
      
      if (is.null(ruta)) {
        return(NULL)
      }
      
      on.exit(
        if (file.exists(ruta)) {
          file.remove(ruta)
        },
        add = TRUE
      )
      
      hojas <- tryCatch(
        readxl::excel_sheets(ruta),
        error = function(e) {
          message("Error hojas ", archivo$nombre[[1L]], ": ", conditionMessage(e))
          character(0L)
        }
      )
      
      hoja_sel <- .seleccionar_hoja_base(hojas)
      
      if (is.na(hoja_sel)) {
        message("Sin hoja BASE: ", archivo$nombre[[1L]])
        return(NULL)
      }
      
      # Normalización de datos ----
      tryCatch(
        {
          datos_raw <- readxl::read_excel(ruta, sheet = hoja_sel, col_names = FALSE, .name_repair = "minimal")
          
          nombres <- datos_raw[1L, ] %>%
            unlist(use.names = FALSE) %>%
            as.character()
          
          nombres_vacios <- is.na(nombres) | trimws(nombres) == ""
          
          nombres[nombres_vacios] <- paste0("COLUMNA_", which(nombres_vacios))
          
          names(datos_raw) <- janitor::make_clean_names(nombres, case = "snake") %>%
            toupper()
          
          datos_raw <- datos_raw[-1L, , drop = FALSE] %>%
            dplyr::select(!dplyr::matches("^COLUMNA_[0-9]+$")) %>%
            .normalizar_base_margen() %>%
            dplyr::mutate(ANHO = as.integer(anho$anho_num[[1L]]), CLASE = as.character(archivo$clase[[1L]]))
          
          datos_raw
        },
        error = function(e) {
          message("Error datos ", archivo$nombre[[1L]], " / ", hoja_sel, ": ", e$message)
          NULL
        }
      )
    }
  )
  
  resultados <- .homologar_tipos_margenes(resultados)
  
  if (length(resultados) == 0L) {
    return(NULL)
  }
  
  dplyr::bind_rows(resultados)
}

## Imputación ----
.imputar_cascada <- function(data_in, valor_col, niveles) {
  for (grupo_vars in niveles) {
    
    if (length(grupo_vars) == 0L) {
      media_global <- mean(data_in[[valor_col]], na.rm = TRUE)
      data_in[[valor_col]] <- ifelse(is.na(data_in[[valor_col]]), media_global, data_in[[valor_col]])
      next
    }
    
    medias_nivel <- data_in %>%
      filter(!is.na(.data[[valor_col]])) %>%
      group_by(across(all_of(grupo_vars))) %>%
      summarise(.media_tmp = mean(.data[[valor_col]], na.rm = TRUE), .groups = "drop")
    
    data_in <- data_in %>%
      left_join(medias_nivel, by = grupo_vars) %>%
      mutate(across(all_of(valor_col), ~ ifelse(is.na(.), .media_tmp, .))) %>%
      select(-.media_tmp)
  }
  data_in
}

# Cascada de imputacion de margen por saco (MarSaco)
NIVELES_MARGEN <- list(c("CLCliNit", "CLLinNegNo", "LinProCod", "MCCod", "MrcCod"),
                       c("CLLinNegNo", "LinProCod", "MCCod", "MrcCod"),
                       c("LinProCod", "MCCod", "MrcCod"),
                       c("CLLinNegNo", "LinProCod"),
                       c("LinProCod"),
                       c("CLLinNegNo"),
                       character(0))

## CLV ----
.preparar_base_clv <- function(data_fuente = data) {
  data_fuente %>%
    dplyr::filter(!is.na(FecFact), lubridate::year(FecFact) >= 2020L,
                  !is.na(CliNitPpal), !is.na(LinNegCod)) %>%
    dplyr::mutate(Id = paste(CliNitPpal, LinNegCod, sep = "_"), Date = as.Date(FecFact)) %>%
    dplyr::group_by(Id, CliNitPpal, LinNegCod, Date) %>%
    dplyr::summarise(Price = sum(SacosFact, na.rm = TRUE), .groups = "drop")
}
.procesar_clv <- function(fecha_corte, base_clv, unidad = "week") {
  base_trans <- base_clv %>%
    dplyr::filter(Date < fecha_corte) %>%
    dplyr::mutate(Date = racafeCore::PrimerDia(Date, uni = unidad)) %>%
    dplyr::group_by(Id, CliNitPpal, LinNegCod, Date) %>%
    dplyr::summarise(Price = sum(Price, na.rm = TRUE), .groups = "drop")
  
  if (nrow(base_trans) == 0L) {
    return(data.frame())
  }
  
  ids_map <- base_trans %>%
    dplyr::select(Id, CliNitPpal, LinNegCod) %>%
    dplyr::distinct()
  
  apparel_trans <- base_trans %>% dplyr::select(Id, Date, Price)
  fec_split <- racafeCore::PrimerDia(max(apparel_trans$Date)) - lubridate::months(1) - lubridate::days(1)
  
  apparel_trans_filtered <- apparel_trans %>%
    dplyr::group_by(Id) %>%
    dplyr::mutate(first_transaction = min(Date)) %>%
    dplyr::filter(first_transaction <= fec_split) %>%
    dplyr::ungroup() %>%
    dplyr::select(-first_transaction)
  
  if (nrow(apparel_trans_filtered) == 0L) {
    return(data.frame())
  }
  
  resultado <- tryCatch(
    {
      clv_data <- CLVTools::clvdata(apparel_trans_filtered, date.format = "ymd", time.unit = unidad,
                                    name.id = "Id", name.date = "Date", name.price = "Price",
                                    estimation.split = fec_split)
      est_pnbd <- CLVTools::pnbd(clv.data = clv_data)
      pred <- CLVTools::predict(est_pnbd,
                                prediction.end = racafeCore::PrimerDia(fecha_corte) +
                                  lubridate::months(1) - lubridate::days(1))
      
      col_spending <- if ("predicted.period.spending" %in% names(pred)) {
        pred$predicted.period.spending
      } else {
        pred$CET * pred$predicted.mean.spending
      }
      
      pred %>%
        dplyr::mutate(SacosPred = pmax(0, round(col_spending)), FecProceso = fecha_corte + lubridate::days(1),
                      Churn = 1 - PAlive) %>%
        dplyr::left_join(ids_map, by = "Id") %>%
        dplyr::select(FecProceso, CliNitPpal, LinNegCod, Churn, SacosPred) %>%
        dplyr::group_by(FecProceso, CliNitPpal, LinNegCod) %>%
        dplyr::summarise(Churn = mean(Churn, na.rm = TRUE), SacosPred = sum(SacosPred, na.rm = TRUE),
                         .groups = "drop")
    },
    error = function(e) {
      message("Error CLV fecha ", fecha_corte, ": ", e$message)
      data.frame()
    }
  )
  
  resultado
}
.cargar_historico_segmentacion <- function(tabla) {
  tryCatch(
    racafeBD::CargarDatos(tabla) %>% dplyr::mutate(FecProceso = as.Date(FecProceso)),
    error = function(e) data.frame()
  )
}
.fechas_pendientes <- function(fechas, historico, fecha_proceso = identity) {
  if (length(fechas) == 0L || nrow(historico) == 0L || !"FecProceso" %in% names(historico)) {
    return(fechas)
  }
  
  ultima_fecha <- suppressWarnings(max(as.Date(historico$FecProceso), na.rm = TRUE))
  
  if (!is.finite(as.numeric(ultima_fecha))) {
    return(fechas)
  }
  
  fechas[as.Date(fecha_proceso(fechas)) > ultima_fecha]
}

## Contactos ----
.generar_codigo_contacto <- function(contactos) {
  hoy <- format(Sys.Date(), "%Y%m%d")
  prefijo <- paste0("CT-", hoy, "-")
  
  existentes <- contactos %>%
    filter(stringr::str_starts(CodContacto, prefijo)) %>%
    pull(CodContacto)
  
  consecutivo <- if (length(existentes) == 0L) {
    1L
  } else {
    max(as.integer(stringr::str_remove(existentes, prefijo)), na.rm = TRUE) + 1L
  }
  
  paste0(prefijo, formatC(consecutivo, width = 3L, flag = "0"))
}
.derivar_etapa_actual <- function(contactos = NULL) {
  if (is.null(contactos)) {
    contactos <- racafeBD::CargarDatos("CRMNALCONTACTO")
  }
  
  if (!"EtapaPreDescarte" %in% names(contactos)) {
    contactos$EtapaPreDescarte <- NA_character_
  }
  
  historial <- tryCatch(
    racafeBD::CargarDatos("CRMNALHISTORIALETAPA") %>%
      mutate(FechaHora = lubridate::as_datetime(FechaHora)),
    error = function(e) {
      data.frame(
        CodContacto = character(), EtapaNueva = character(), FechaHora = as.POSIXct(character()),
        stringsAsFactors = FALSE)
    })
  
  ultima_transicion <- historial %>%
    group_by(CodContacto) %>%
    filter(FechaHora == max(FechaHora)) %>%
    slice(1L) %>%
    ungroup() %>%
    select(CodContacto, EtapaUltimaTransicion = EtapaNueva)
  
  contactos %>%
    left_join(ultima_transicion, by = "CodContacto") %>%
    mutate(
      EtapaPreDescarte = ifelse(is.na(EtapaPreDescarte), "CONTACTO", EtapaPreDescarte),
      Etapa = case_when(
        Estado == "DESCARTADO" ~ "DESCARTADO",
        is.na(EtapaUltimaTransicion) ~ "CONTACTO",
        TRUE ~ EtapaUltimaTransicion)
    ) %>%
    select(-EtapaUltimaTransicion)
}

## Contactos clientes ----
.asegurar_contactos_clientes <- function(clientes, usuario = "SISTEMA") {
  contactos <- tryCatch(
    racafeBD::CargarDatos("CRMNALCONTACTO"),
    error = function(e) {
      data.frame(
        CodContacto = character(), PerCod = numeric(), PerRazSoc = character(), Estado = character(),
        EtapaPreDescarte = character(), FechaHoraCrea = as.POSIXct(character()),
        FechaHoraModi = as.POSIXct(character()), stringsAsFactors = FALSE)
    })
  
  contactos <- contactos %>%
    mutate(
      PerCod = suppressWarnings(as.numeric(PerCod)), CodContacto = as.character(CodContacto),
      FechaHoraCrea = suppressWarnings(lubridate::as_datetime(FechaHoraCrea)),
      FechaHoraModi = suppressWarnings(lubridate::as_datetime(FechaHoraModi)))
  
  clientes <- clientes %>%
    transmute(CliNitPpal = suppressWarnings(as.numeric(CliNitPpal)), PerRazSoc = as.character(PerRazSoc)) %>%
    filter(!is.na(CliNitPpal)) %>%
    distinct(CliNitPpal, .keep_all = TRUE)
  
  contactos_existentes <- contactos %>%
    filter(!is.na(PerCod)) %>%
    distinct(PerCod, .keep_all = TRUE) %>%
    transmute(CliNitPpal = PerCod, CodContacto, TieneContacto = "SI")
  
  clientes_mapa <- clientes %>%
    left_join(contactos_existentes, by = "CliNitPpal")
  
  faltantes <- clientes_mapa %>%
    filter(is.na(CodContacto))
  
  if (nrow(faltantes) > 0L) {
    for (i in seq_len(nrow(faltantes))) {
      codigo <- .generar_codigo_contacto(contactos)
      
      nuevo <- data.frame(
        CodContacto = codigo, PerCod = faltantes$CliNitPpal[[i]], PerRazSoc = faltantes$PerRazSoc[[i]],
        Estado = "ACTIVO", EtapaPreDescarte = "CONTACTO", UsuarioCrea = usuario, FechaHoraCrea = Sys.time(),
        UsuarioMod = usuario, FechaHoraModi = Sys.time(), stringsAsFactors = FALSE)
      
      tryCatch(
        racafeBD::AgregarDatos(nuevo, "CRMNALCONTACTO"),
        error = function(e) {
          stop(
            "No fue posible crear el contacto para CliNitPpal ", faltantes$CliNitPpal[[i]], ": ",
            conditionMessage(e))
        })
      
      contactos <- bind_rows(contactos, nuevo)
      
      clientes_mapa$CodContacto[
        clientes_mapa$CliNitPpal == faltantes$CliNitPpal[[i]]
      ] <- codigo
      
      clientes_mapa$TieneContacto[
        clientes_mapa$CliNitPpal == faltantes$CliNitPpal[[i]]
      ] <- "NO"
    }
  }
  
  contactos_etapa <- .derivar_etapa_actual(contactos) %>%
    mutate(PerCod = suppressWarnings(as.numeric(PerCod))) %>%
    distinct(PerCod, .keep_all = TRUE) %>%
    transmute(
      CliNitPpal = PerCod, CodContactoEtapa = as.character(CodContacto), EtapaContacto = as.character(Etapa),
      EstadoContacto = as.character(Estado), EtapaPreDescarte = as.character(EtapaPreDescarte))
  
  clientes_mapa %>%
    mutate(TieneContacto = coalesce(TieneContacto, "NO")) %>%
    left_join(contactos_etapa, by = "CliNitPpal") %>%
    mutate(CodContacto = coalesce(CodContacto, CodContactoEtapa)) %>%
    select(CliNitPpal, CodContacto, TieneContacto, EtapaContacto, EstadoContacto, EtapaPreDescarte)
}

## Segmentación ----
.identificar_clientes_faltantes <- function(df_referencia, df_comparacion) {
  df_referencia %>%
    group_by(CliNitPpal, LinNegCod, Usuario) %>%
    summarise(FecFact = max(FecFact), .groups = "drop") %>%
    distinct() %>%
    anti_join(
      df_comparacion %>% select(CliNitPpal, LinNegCod) %>% distinct(),
      by = join_by(CliNitPpal, LinNegCod)
    )
}
.generar_fechas_analisis <- function(fecha_inicio, fecha_fin = Sys.Date(), por = "month", ajuste = 0L) {
  fechas <- seq.Date(as.Date(fecha_inicio), racafeCore::PrimerDia(fecha_fin), by = por)
  if (ajuste != 0L) {
    fechas <- fechas + lubridate::days(ajuste)
  }
  
  fechas
}
.fecha_mes_estado <- function(x) {
  x <- as.Date(x)
  es_fin_mes <- !is.na(x) & lubridate::day(x) == lubridate::days_in_month(x)
  x[es_fin_mes] <- x[es_fin_mes] + lubridate::days(1L)
  lubridate::floor_date(x, unit = "month")
}
.asegurar_columnas <- function(df, columnas) {
  faltantes <- setdiff(columnas, names(df))
  
  for (columna in faltantes) {
    df[[columna]] <- NA
  }
  
  df
}
.convertir_numero_margen <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  x <- trimws(as.character(x))
  resultado <- suppressWarnings(as.numeric(x))
  faltantes <- is.na(resultado) & !is.na(x) & x != ""
  
  if (any(faltantes)) {
    resultado[faltantes] <- suppressWarnings(
      readr::parse_number(x[faltantes], locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
    )
  }
  
  resultado
}

# Extracción de datos ----

## Márgenes ----
print("Cargando datos de margenes...")
USUARIO_MARGENES <- "wmunozs"
ID_CARPETA_INFORMES <- "014N3L2U2EH7DJE5B6V5AK6USIEW5IJH2N"

# Datos historicos cacheados (anos anteriores al corriente)
margenes_hist <- tryCatch(
  {
    racafeBD::CargarDatos("CRMNALMARLOT") %>%
      .normalizar_base_margen() %>%
      filter(
        !is.na(FECFACTURA),
        year(FECFACTURA) < year(Sys.Date())
      ) %>%
      mutate(ANHO = as.integer(ANHO), CLASE = as.character(CLASE))
  },
  error = function(e) {
    message("Error margenes historicos: ", e$message)
    data.frame()
  }
)

# Datos del año corriente desde OneDrive
margenes_nuevos <- tryCatch(
  {
    .cargar_margenes_onedrive(USUARIO_MARGENES, ID_CARPETA_INFORMES)
  },
  error = function(e) {
    message("Error margenes OneDrive: ", e$message)
    NULL
  }
)

# Combinar y persistir si hay datos nuevos
if (!is.null(margenes_nuevos) && nrow(margenes_nuevos) > 0L) {
  tryCatch(
    {
      margenes_lista <- .homologar_tipos_margenes(list(margenes_hist, margenes_nuevos))
      
      margenes <- bind_rows(margenes_lista) %>%
        mutate(ANHO = as.integer(ANHO), CLASE = as.character(CLASE))
      
      if (!all(c("ANHO", "CLASE") %in% names(margenes))) {
        stop("CRMNALMARLOT debe contener las variables ANHO y CLASE")
      }
      
      racafeBD::EscribirDatos(margenes, "CRMNALMARLOT")
      
      message(
        "Margenes guardados OK: ",
        format(nrow(margenes), big.mark = ".", decimal.mark = ","),
        " registros y ",
        ncol(margenes),
        " variables"
      )
    },
    error = function(e) {
      message("Error al guardar margenes: ", e$message)
    }
  )
} else {
  message("Sin datos nuevos de margenes; se conserva lo existente")
}

## Datos principales ----
print("Cargando datos principales...")

margenes_pyg <- racafeBD::CargarDatos("CRMNALMARLOT") %>%
  .normalizar_base_margen() %>%
  mutate(
    FECFACTURA = as.Date(FECFACTURA),
    FecMes = floor_date(FECFACTURA, unit = "month")
  ) %>%
  filter(
    !is.na(FECFACTURA),
    !is.na(FecMes)
  ) %>%
  .asegurar_columnas(
    c(
      "SUCURSAL", "LOTE", "CLIENTE", "LINNEG", "FECFACTURA",
      "KILOS", "SACOS", "SACOS70", "MARGEN", "PED_SISTEMA", "ANHO", "MES", "CLASE", "SEGMENTACION_CLIENTE",
      "ANO_EMBARQ", "MES_EMBARQ", "TIP_PRD"
    )
  )

if (nrow(margenes_pyg) == 0L) {
  stop("CRMNALMARLOT no contiene registros con FECFACTURA valida")
}

# PERIODO HISTORICO ----
fecha_inicio_analisis <- max(
  as.Date("2020-01-01"),
  lubridate::floor_date(min(margenes_pyg$FecMes, na.rm = TRUE), "year")
)

margenes_pyg <- margenes_pyg %>%
  dplyr::filter(FecMes >= fecha_inicio_analisis)

if (nrow(margenes_pyg) == 0L) {
  stop("No hay margenes disponibles desde el inicio del periodo de analisis")
}

message("Inicio del universo comercial: ", format(fecha_inicio_analisis, "%Y-%m-%d"))

ultimo_periodo_pyg <- max(margenes_pyg$FecMes, na.rm = TRUE)

fecha_inicio_operativo <- ultimo_periodo_pyg %m+% months(1L)
mes_actual <- floor_date(Sys.Date(), unit = "month")

message("Ultimo periodo cerrado PYG: ", format(ultimo_periodo_pyg, "%Y-%m"))

message("Reconstruccion operativa desde: ", format(fecha_inicio_operativo, "%Y-%m-%d"))

# Tabla maestra de sucursales — con tildes para que LimpiarNombres produzca
sucs <- data.frame(
  SucCod = c(12, 15, 20, 26, 30, 32, 35, 50, 55),
  Sucursal = c(
    "Trilladora 12", "Bachué", "Medellín", "Popayán", "Armenia",
    "Arenales", "Pereira", "Bucaramanga", "Huila"
  )
) %>%
  mutate(across(where(is.character), racafeCore::LimpiarNombres))

# Snapshot completo de CRMNALCLIENTE
personas <- racafeBD::CargarDatos("CRMNALCLIENTE") %>%
  mutate(
    FecProceso = as.Date(FecProceso),
    FecMes = .fecha_mes_estado(FecProceso),
    LinNegCod = as.numeric(LinNegCod),
    CLCliNit = as.numeric(CLCliNit),
    CliNitPpal = as.numeric(CliNitPpal)
  )

local_hist <- racafeBD::CargarDatos("CRMNALLOCAL") %>%
  mutate(
    FecProceso = as.Date(FecProceso),
    FecMes = .fecha_mes_estado(FecProceso),
    CliNitPpal = as.numeric(CliNitPpal)
  ) %>%
  arrange(FecProceso) %>%
  group_by(FecMes, Usr, CliNitPpal) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  select(-FecProceso)

clientes_hist <- personas %>%
  left_join(
    local_hist,
    by = c("FecMes", "Usr", "CliNitPpal")
  ) %>%
  arrange(FecMes, LinNegCod, CLCliNit)

clientes_actual <- clientes_hist %>%
  group_by(
    LinNegCod,
    CliNitPpal
  ) %>%
  filter(
    FecMes == max(FecMes, na.rm = TRUE)
  ) %>%
  slice_tail(n = 1L) %>%
  ungroup()

# NIT principal por cliente
nit_ppal <- clientes_hist %>%
  arrange(FecMes) %>%
  group_by(CLCliNit) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  select(
    PerCod = CLCliNit,
    CliNitPpal
  ) %>%
  distinct()

cliente_actual_map <- clientes_hist %>%
  arrange(FecProceso) %>%
  group_by(CliNitPpal) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  select(CliNitPpal, Asesor, Segmento, SSPpto, MNFCCPpto, NumMesesRecuperar, Excluir)

# Datos maestros de clientes desde sistema transaccional
creacion <- racafeBD::ConsultaSistema("syscafe", "SELECT DISTINCT PerCod, PerFecCre FROM NPERSONA")

n_cliente <- racafeBD::ConsultaSistema(
  "syscafe",
  query = "SELECT c.CliNit AS PerCod, c.CliCont, c.CliDir,
                  c.CliDir1, c.CliTel, c.CliConCom, c.CliTelCom,
                  c.CliEmlCom, c.CiuExtCod, c.CliFPagDbl, c.CliSegCod,
                  CASE c.CliSegCod
                    WHEN 1 THEN 'GRANDE'
                    WHEN 2 THEN 'MEDIANO'
                    ELSE 'PEQUEÑO'
                  END AS SegmentoNCLIENTE,
                  ce.CiuExtNom, p.PerRazSoc,
                  p.PerFecCre AS FecCreacion
           FROM   NCLIENTE c
           LEFT JOIN NCIUEXT ce ON c.CiuExtCod = ce.CiuExtCod
           LEFT JOIN NPERSONA p ON c.CliNit = p.PerCod"
) %>%
  left_join(
    nit_ppal,
    by = "PerCod"
  ) %>%
  mutate(
    CliNitPpal = ifelse(is.na(CliNitPpal), PerCod, CliNitPpal),
    across(where(is.character), \(x) ifelse(x %in% c("", "."), NA, x))
  )

## Contactos ----
contactos_clientes <- .asegurar_contactos_clientes(
  cliente_actual_map %>%
    left_join(n_cliente %>% transmute(CliNitPpal = suppressWarnings(as.numeric(PerCod)), PerRazSoc),
              by = "CliNitPpal"))

contactos_raw <- contactos_clientes

# Pedidos activos de venta nacional
# PEDIDOS ----
ped <- racafeBD::ConsultaSistema(
  "syscafe",
  query = "SELECT p1.PdcCod,
                  pd.PdcCntCli AS PdcRefCli,
                  pd.PdcFecCre,
                  p1.PdcLin,
                  p1.PdcFecCie,
                  p1.PdcFecEst,
                  p1.PdcCan,
                  pd.PdcUsu AS Usuario,
                  um.UMeFac,
                  pd.PdcTipCaf AS TipCaf,
                  pd.PdcPrePes AS PdcPrecioKilo,
                  pd.CliNit AS CLCliNit,
                  pd.PdcEst,
                  p1.LinNegCod,
                  p1.LinProCod
           FROM   EXPPEDID pd
           INNER JOIN EXPPEDI1 p1
             ON p1.PdcCod = pd.PdcCod
            AND p1.CiaCod = pd.CiaCod
           LEFT JOIN NUNIMEDI um ON pd.UMeCod = um.UMeCod
           WHERE pd.CiaCod = 10
             AND pd.CliNit <> 32
             AND pd.PdcVtaNal = 1"
) %>%
  mutate(
    PdcFecCre = as.Date(PdcFecCre),
    PdcFecCie = as.Date(PdcFecCie),
    PdcFecEst = as.Date(PdcFecEst),
    PdcFecCie = na_if(PdcFecCie, as.Date("1753-01-01")),
    PdcFecEst = na_if(PdcFecEst, as.Date("1753-01-01")),
    PdcCod = as.numeric(PdcCod),
    PdcLin = as.numeric(PdcLin),
    CLCliNit = as.numeric(CLCliNit),
    LinNegCod = as.numeric(LinNegCod),
    LinProCod = as.numeric(LinProCod),
    PdcCan = as.numeric(PdcCan),
    UMeFac = as.numeric(UMeFac),
    PdcEst = trimws(as.character(PdcEst)),
    SacosPedido = if_else(is.finite(PdcCan) & is.finite(UMeFac) & UMeFac > 0, PdcCan * UMeFac / 70, NA_real_)
  )

# Cuadro de lotes con carga incremental (activos + congelados del cache)
ruta_lotes_cache <- "APP/data/lotes_raw.rds"

datos_previos <- NULL

if (file.exists(ruta_lotes_cache)) {
  datos_previos <- tryCatch(
    {
      prev <- readRDS(ruta_lotes_cache)
      
      if (is.data.frame(prev) && nrow(prev) > 0L) {
        prev
      } else {
        NULL
      }
    },
    error = function(e) {
      message("Error cargando cache de lotes: ", e$message)
      NULL
    }
  )
} else {
  message("Cache de lotes no existe; se realizara carga completa")
}

lotes_raw <- .cargar_lotes_incremental(
  datos_previos
) %>%
  mutate(
    CLLotCod = as.numeric(CLLotCod),
    CLPdcCod = as.numeric(CLPdcCod),
    CLPdcLin = as.numeric(CLPdcLin),
    CLCliNit = as.numeric(CLCliNit),
    LinNegCod = as.numeric(LinNegCod),
    LinProCod = as.numeric(LinProCod)
  )

# Facturacion: query completa garantiza un registro por lote (no particionar)
fact <- .cargar_fact() %>%
  dplyr::mutate(
    CLLotCod = as.numeric(CLLotCod),
    FecPrimerFact = as.Date(FecPrimerFact),
    FecFact = as.Date(FecFact)
  ) %>%
  dplyr::left_join(
    lotes_raw %>%
      dplyr::select(CLLotCod, LinNegCod) %>%
      dplyr::distinct(),
    by = "CLLotCod"
  )

# UNIVERSO HISTORICO DE PEDIDOS ----
pedidos_con_facturacion_historica <- lotes_raw %>%
  dplyr::select(CLPdcCod, CLPdcLin, CLLotCod) %>%
  dplyr::semi_join(
    dplyr::bind_rows(
      fact %>%
        dplyr::filter(FecFact >= fecha_inicio_analisis) %>%
        dplyr::select(CLLotCod),
      margenes_pyg %>%
        dplyr::transmute(CLLotCod = .convertir_numero_margen(LOTE))
    ) %>%
      dplyr::filter(!is.na(CLLotCod)) %>%
      dplyr::distinct(),
    by = "CLLotCod"
  ) %>%
  dplyr::distinct(CLPdcCod, CLPdcLin)

pedidos_excluidos_historicos <- ped %>%
  dplyr::filter(
    !dplyr::coalesce(PdcFecCre >= fecha_inicio_analisis, FALSE) &
      !dplyr::coalesce(PdcFecEst >= fecha_inicio_analisis, FALSE)
  ) %>%
  dplyr::anti_join(pedidos_con_facturacion_historica,
                   by = c("PdcCod" = "CLPdcCod", "PdcLin" = "CLPdcLin"))

ped <- ped %>%
  dplyr::filter(
    dplyr::coalesce(PdcFecCre >= fecha_inicio_analisis, FALSE) |
      dplyr::coalesce(PdcFecEst >= fecha_inicio_analisis, FALSE)
  ) %>%
  dplyr::bind_rows(
    ped %>%
      dplyr::semi_join(pedidos_con_facturacion_historica,
                       by = c("PdcCod" = "CLPdcCod", "PdcLin" = "CLPdcLin"))
  ) %>%
  dplyr::distinct(PdcCod, PdcLin, .keep_all = TRUE)

message("Lineas historicas excluidas sin actividad financiera desde ",
        format(fecha_inicio_analisis, "%Y-%m-%d"), ": ",
        nrow(pedidos_excluidos_historicos))

# Construccion del cuadro de datos principal

# Integridad de fuentes ----
.verificar_clave_unica <- function(datos, claves, nombre) {
  duplicados <- datos %>%
    dplyr::count(dplyr::across(dplyr::all_of(claves))) %>%
    dplyr::filter(n > 1L)
  
  if (nrow(duplicados) > 0L) {
    stop("La fuente ", nombre, " tiene ", nrow(duplicados), " claves duplicadas: ", paste(claves, collapse = ", "))
  }
  
  invisible(TRUE)
}

data_pyg <- margenes_pyg %>%
  transmute(
    FecMes,
    ANHO = suppressWarnings(as.integer(ANHO)),
    MES = as.character(MES),
    CLASE = as.character(CLASE),
    SegmentacionClientePYG = as.character(SEGMENTACION_CLIENTE),
    CLSucCod = .convertir_numero_margen(SUCURSAL),
    CLLotCod = .convertir_numero_margen(LOTE),
    FechaEmbarque = as.Date(
      ifelse(
        !is.na(.convertir_numero_margen(ANO_EMBARQ)) &
          !is.na(.convertir_numero_margen(MES_EMBARQ)) &
          .convertir_numero_margen(MES_EMBARQ) %in% 1:12,
        sprintf(
          "%04d-%02d-01",
          as.integer(.convertir_numero_margen(ANO_EMBARQ)),
          as.integer(.convertir_numero_margen(MES_EMBARQ))
        ),
        NA_character_
      )
    ),
    CLPdcCod = .convertir_numero_margen(PED_SISTEMA),
    PdcPrecioKilo = NA_real_,
    PdcRefCli = NA_character_,
    PdcFecCre = as.Date(NA),
    CLPdcLin = NA_real_,
    CLPdcCntCl = NA_character_,
    Usuario = NA_character_,
    SacLote = NA_real_,
    FecAsignLote = as.Date(NA),
    CodOrdTril = NA_real_,
    FecOrdTril = as.Date(NA),
    SacProd = NA_real_,
    FecProd = as.Date(NA),
    CodDesp = NA_real_,
    FecDesp = as.Date(NA),
    SacDesp = NA_real_,
    FecFact = as.Date(FECFACTURA),
    SacosFact = .convertir_numero_margen(SACOS70),
    Margen = .convertir_numero_margen(MARGEN),
    UMeFac = NA_real_,
    LinNegCod = .convertir_numero_margen(LINNEG),
    CLLinNegNo = dplyr::case_when(
      .convertir_numero_margen(LINNEG) == 10000 ~ "CONVENCIONALES",
      .convertir_numero_margen(LINNEG) %in% c(20000, 21000) ~ "A LA MEDIDA",
      TRUE ~ NA_character_
    ),
    LinProCod = .convertir_numero_margen(TIP_PRD),
    MCCod = NA_real_,
    MrcCod = NA_real_,
    CLPdcFctAD = NA_character_,
    SacFact = .convertir_numero_margen(SACOS70),
    CLPdcCanFa = .convertir_numero_margen(SACOS70),
    CLCliNit = .convertir_numero_margen(CLIENTE),
    CLLotSacXP = 0,
    CLLotPenXD = 0,
    CLLotDesXF = 0,
    PendProducir = 0,
    PendDespachar = 0,
    PendFacturar = 0
  ) %>%
  mutate(
    CLLotCod = ifelse(is.na(CLLotCod) | CLLotCod == 0, NA_real_, CLLotCod),
    LinNegCod = ifelse(LinNegCod == 20000L, 21000L, LinNegCod),
    FechaEmbarque = coalesce(FechaEmbarque, FecMes)
  )

# Validación de pedidos ----
.verificar_clave_unica(ped, c("PdcCod", "PdcLin"), "EXPPEDI1/EXPPEDID")

if (any(is.na(ped$PdcCod) | is.na(ped$PdcLin))) {
  stop("Hay pedidos sin identificador o sin linea de embarque")
}

ped_sin_conversion <- ped %>%
  filter(PdcEst == "A", !is.finite(SacosPedido) | SacosPedido < 0)

if (nrow(ped_sin_conversion) > 0L) {
  warning("Pedidos activos sin conversion valida PdcCan x UMeFac / 70: ",
          nrow(ped_sin_conversion), " lineas. Quedan sin proyeccion numerica")
}

ped_sin_fecha <- ped %>%
  filter(PdcEst == "A", is.na(PdcFecEst))

if (nrow(ped_sin_fecha) > 0L) {
  message("Pedidos activos sin fecha estimada (PdcFecEst vacia): ",
          nrow(ped_sin_fecha), " lineas. Se excluyen de la proyeccion")
}

pedidos_fechas_distintas <- ped %>%
  group_by(PdcCod) %>%
  filter(n_distinct(PdcFecEst, na.rm = TRUE) > 1L) %>%
  ungroup() %>%
  arrange(PdcCod, PdcFecEst, PdcLin)

pedido_mapa <- ped %>%
  transmute(
    CLPdcCod = PdcCod,
    CLPdcLin = PdcLin,
    PdcPrecioKilo,
    PdcRefCli,
    PdcFecCre,
    PdcFecCie,
    PdcFecEst,
    Usuario,
    UMeFac,
    CLCliNitPedido = CLCliNit,
    LinNegCodPedido = LinNegCod,
    LinProCodPedido = LinProCod,
    PdcEst
  )

.verificar_clave_unica(pedido_mapa, c("CLPdcCod", "CLPdcLin"), "ped")
.verificar_clave_unica(lotes_raw, "CLLotCod", "lotes_raw")
.verificar_clave_unica(fact, c("CLLotCod", "FecMesFact"), "fact")

# INTEGRIDAD PEDIDO LOTE ----
lotes_sin_pedido <- lotes_raw %>%
  anti_join(pedido_mapa, by = c("CLPdcCod", "CLPdcLin"))

if (nrow(lotes_sin_pedido) > 0L) {
  warning("Lotes sin linea de pedido nacional: ", nrow(lotes_sin_pedido),
          ". Permanecen en lotes_raw y se excluyen de la base operativa")
}

pedido_sin_lote <- pedido_mapa %>%
  anti_join(lotes_raw, by = c("CLPdcCod", "CLPdcLin"))

base_operativa <- pedido_mapa %>%
  left_join(lotes_raw, by = c("CLPdcCod", "CLPdcLin"),
            relationship = "one-to-many") %>%
  mutate(
    CLCliNit = coalesce(CLCliNitPedido, CLCliNit),
    LinNegCod = coalesce(LinNegCodPedido, LinNegCod),
    LinProCod = coalesce(LinProCodPedido, LinProCod),
    PdcRefCli = coalesce(PdcRefCli, CLPdcCntCl),
    CLLinNegNo = case_when(
      CLLinNegNo == "DIFERENCIADOS" ~ "A LA MEDIDA",
      !is.na(CLLinNegNo) ~ CLLinNegNo,
      LinNegCod == 10000 ~ "CONVENCIONALES",
      LinNegCod %in% c(20000, 21000) ~ "A LA MEDIDA",
      TRUE ~ NA_character_
    ),
    LinNegCod = ifelse(LinNegCod == 20000L, 21000L, LinNegCod),
    FechaEmbarque = coalesce(
      suppressWarnings(as.Date(paste0(Periodo, "01"), "%Y%m%d")),
      PdcFecEst
    )
  )

if (n_distinct(base_operativa$CLPdcCod, base_operativa$CLPdcLin) != nrow(pedido_mapa)) {
  stop("La base operativa no conserva todas las lineas de pedido")
}

.verificar_clave_unica(
  base_operativa %>% filter(!is.na(CLLotCod)),
  "CLLotCod",
  "base_operativa"
)

fact_abierto <- fact %>%
  filter(FecMesFact >= fecha_inicio_operativo)

fact_sin_lote_base <- fact_abierto %>%
  anti_join(
    base_operativa %>% filter(!is.na(CLLotCod)) %>% select(CLLotCod),
    by = "CLLotCod"
  )

if (nrow(fact_sin_lote_base) > 0L) {
  warning(
    "Facturas abiertas sin lote en la población nacional: ",
    nrow(fact_sin_lote_base), ". Revisar estos lotes en fact y lotes_raw: ",
    paste(head(unique(fact_sin_lote_base$CLLotCod), 15L), collapse = ", ")
  )
}

fact_lote_acum <- fact %>%
  group_by(CLLotCod) %>%
  summarise(SacosFactAcum = sum(SacosFact, na.rm = TRUE), FecUltimaFact = max(FecFact, na.rm = TRUE), .groups = "drop")

data_fact_abierta <- base_operativa %>%
  inner_join(
    fact_abierto %>%
      select(-LinNegCod),
    by = "CLLotCod"
  ) %>%
  mutate(
    FecMes = as.Date(FecMesFact),
    ANHO = lubridate::year(FecMes),
    MES = toupper(format(FecMes, "%B")),
    CLASE = NA_character_,
    SegmentacionClientePYG = NA_character_,
    Margen = NA_real_,
    PendProducir = NA_real_,
    PendDespachar = NA_real_,
    PendFacturar = NA_real_
  ) %>%
  select(
    FecMes,
    ANHO,
    MES,
    CLASE,
    SegmentacionClientePYG,
    CLSucCod,
    CLLotCod,
    FechaEmbarque,
    CLPdcCod,
    PdcPrecioKilo,
    PdcRefCli,
    PdcFecCre,
    CLPdcLin,
    CLPdcCntCl,
    Usuario,
    SacLote,
    FecAsignLote,
    CodOrdTril,
    FecOrdTril,
    SacProd,
    FecProd,
    CodDesp,
    FecDesp,
    SacDesp,
    FecFact,
    SacosFact,
    Margen,
    UMeFac,
    LinNegCod,
    CLLinNegNo,
    LinProCod,
    MCCod,
    MrcCod,
    CLPdcFctAD,
    SacFact,
    CLPdcCanFa,
    CLCliNit,
    CLLotSacXP,
    CLLotPenXD,
    CLLotDesXF,
    PendProducir,
    PendDespachar,
    PendFacturar
  )

# Facturación cerrada conciliada por lote ----
pyg_fact_lote <- data_pyg %>%
  filter(!is.na(CLLotCod)) %>%
  group_by(CLLotCod) %>%
  summarise(SacosCerrados = sum(SacosFact, na.rm = TRUE), .groups = "drop")

# Facturación abierta por lote ----
fact_abierta_lote <- fact %>%
  filter(FecMesFact > ultimo_periodo_pyg) %>%
  group_by(CLLotCod) %>%
  summarise(SacosAbiertos = sum(SacosFact, na.rm = TRUE), .groups = "drop")

# AUDITORIA CIERRE OPERATIVO ----
# Facturación operacional cerrada de respaldo para lotes sin PYG identificable ----
fact_cerrada_lote <- fact %>%
  filter(FecMesFact <= ultimo_periodo_pyg) %>%
  group_by(CLLotCod) %>%
  summarise(SacosOperativosCerrados = sum(SacosFact, na.rm = TRUE),
            .groups = "drop")

auditoria_cierre_operativo <- full_join(
  pyg_fact_lote,
  fact_cerrada_lote,
  by = "CLLotCod"
) %>%
  mutate(
    SacosCerradosPYG = coalesce(SacosCerrados, 0),
    SacosCerradosSYSCAFE = coalesce(SacosOperativosCerrados, 0),
    DiferenciaSacos = SacosCerradosSYSCAFE - SacosCerradosPYG,
    EstadoConciliacion = case_when(
      is.na(SacosCerrados) ~ "SIN_PYG",
      is.na(SacosOperativosCerrados) ~ "SIN_SYSCAFE",
      abs(DiferenciaSacos) > 0.01 ~ "DIFERENCIA",
      TRUE ~ "CONCILIADO"
    )
  )

if (any(auditoria_cierre_operativo$EstadoConciliacion != "CONCILIADO")) {
  warning("Existen diferencias entre SYSCAFE y CRMNALMARLOT en meses cerrados; ",
          "se conserva exclusivamente el volumen financiero de CRMNALMARLOT")
}

# Relación única entre lote y línea ----
relacion_lote_linea <- lotes_raw %>%
  transmute(
    CLLotCod,
    PdcCod = CLPdcCod,
    PdcLin = CLPdcLin,
    SacLote = as.numeric(SacLote),
    SacProd = as.numeric(SacProd),
    SacDesp = as.numeric(SacDesp)
  ) %>%
  filter(!is.na(CLLotCod), !is.na(PdcCod), !is.na(PdcLin))

.verificar_clave_unica(relacion_lote_linea, "CLLotCod", "relacion_lote_linea")

# Un lote puede contener movimientos en varios meses; se cuenta una sola vez ----
resumen_lotes_linea <- relacion_lote_linea %>%
  left_join(pyg_fact_lote, by = "CLLotCod") %>%
  left_join(fact_abierta_lote, by = "CLLotCod") %>%
  mutate(
    SacosCerrados = coalesce(SacosCerrados, 0),
    SacosAbiertos = coalesce(SacosAbiertos, 0),
    SacosFacturados = SacosCerrados + SacosAbiertos
  ) %>%
  group_by(PdcCod, PdcLin) %>%
  summarise(
    NumLotes = n(),
    CantidadAsignadaOrigen = sum(SacLote, na.rm = TRUE),
    CantidadProducidaOrigen = sum(SacProd, na.rm = TRUE),
    CantidadDespachadaOrigen = sum(SacDesp, na.rm = TRUE),
    SacosFacturados = sum(SacosFacturados, na.rm = TRUE),
    .groups = "drop"
  )

.verificar_clave_unica(resumen_lotes_linea, c("PdcCod", "PdcLin"),
                       "resumen_lotes_linea")

# AUDITORIA UNIDADES LOTES ----
resumen_lotes_linea <- resumen_lotes_linea %>%
  mutate(
    UnidadLoteValidada = FALSE,
    SacosAsignados = NA_real_,
    SacosProducidos = NA_real_,
    SacosDespachados = NA_real_
  )

lineas_pendientes <- ped %>%
  left_join(resumen_lotes_linea, by = c("PdcCod", "PdcLin")) %>%
  transmute(
    CLPdcCod = PdcCod,
    CLPdcLin = PdcLin,
    SacosPedido,
    SacosFacturadosLinea = coalesce(SacosFacturados, 0),
    SacosPendientesLinea = if_else(
      is.finite(SacosPedido),
      pmax(SacosPedido - SacosFacturadosLinea, 0),
      NA_real_
    )
  )

.verificar_clave_unica(lineas_pendientes, c("CLPdcCod", "CLPdcLin"),
                       "lineas_pendientes")


data_pendiente <- base_operativa %>%
  left_join(lineas_pendientes %>%
              select(CLPdcCod, CLPdcLin, SacosPendientesLinea),
            by = c("CLPdcCod", "CLPdcLin"), relationship = "many-to-one") %>%
  group_by(CLPdcCod, CLPdcLin) %>%
  mutate(
    FilaSaldo = row_number(),
    PendProducir = NA_real_,
    PendDespachar = NA_real_,
    PendFacturar = if_else(FilaSaldo == 1L, SacosPendientesLinea, 0)
  ) %>%
  ungroup() %>%
  filter(PdcEst == "A", coalesce(SacosPendientesLinea > 0, FALSE)) %>%
# MES OPERATIVO ----
mutate(
  FecMes = pmax(coalesce(lubridate::floor_date(PdcFecEst, "month"),
                         mes_actual), fecha_inicio_operativo),
  ANHO = lubridate::year(FecMes),
  MES = toupper(format(FecMes, "%B")),
  CLASE = NA_character_,
  SegmentacionClientePYG = NA_character_,
  FecFact = as.Date(NA),
  SacosFact = NA_real_,
  Margen = NA_real_
) %>%
  select(
    FecMes,
    ANHO,
    MES,
    CLASE,
    SegmentacionClientePYG,
    CLSucCod,
    CLLotCod,
    FechaEmbarque,
    CLPdcCod,
    PdcPrecioKilo,
    PdcRefCli,
    PdcFecCre,
    CLPdcLin,
    CLPdcCntCl,
    Usuario,
    SacLote,
    FecAsignLote,
    CodOrdTril,
    FecOrdTril,
    SacProd,
    FecProd,
    CodDesp,
    FecDesp,
    SacDesp,
    FecFact,
    SacosFact,
    Margen,
    UMeFac,
    LinNegCod,
    CLLinNegNo,
    LinProCod,
    MCCod,
    MrcCod,
    CLPdcFctAD,
    SacFact,
    CLPdcCanFa,
    CLCliNit,
    CLLotSacXP,
    CLLotPenXD,
    CLLotDesXF,
    PendProducir,
    PendDespachar,
    PendFacturar
  )

data_operativo <- bind_rows(data_fact_abierta, data_pendiente)

if (any(data_operativo$FecMes <= ultimo_periodo_pyg, na.rm = TRUE)) {
  stop("Los datos operativos contienen registros en meses cerrados")
}

fact_conciliada <- data_fact_abierta %>%
  group_by(FecMes) %>%
  summarise(SacosData = sum(SacosFact, na.rm = TRUE), .groups = "drop")

fact_referencia <- fact_abierto %>%
  semi_join(base_operativa, by = "CLLotCod") %>%
  group_by(FecMes = FecMesFact) %>%
  summarise(SacosFactura = sum(SacosFact, na.rm = TRUE), .groups = "drop")

revision_fact_abierta <- full_join(
  fact_referencia,
  fact_conciliada,
  by = "FecMes"
) %>%
  mutate(Diferencia = coalesce(SacosData, 0) - coalesce(SacosFactura, 0))

if (any(abs(revision_fact_abierta$Diferencia) > 0.01)) {
  print(revision_fact_abierta)
  stop("El volumen operativo de data no coincide con las facturas abiertas asociadas a lotes")
}

# Integridad de dimensiones ----
.verificar_clave_unica(nit_ppal, "PerCod", "nit_ppal")
.verificar_clave_unica(cliente_actual_map, "CliNitPpal", "cliente_actual_map")
.verificar_clave_unica(sucs, "SucCod", "sucs")
.verificar_clave_unica(n_cliente, "PerCod", "n_cliente")
.verificar_clave_unica(contactos_raw, "CliNitPpal", "contactos_raw")

filas_financieras <- nrow(data_pyg)
filas_operativas <- nrow(data_operativo)

data <- bind_rows(
  data_pyg,
  data_operativo
) %>%
  mutate(
    FecMes = as.Date(FecMes),
    CLCliNit = as.numeric(CLCliNit),
    LinNegCod = as.numeric(LinNegCod)
  ) %>%
  left_join(
    nit_ppal %>%
      rename(CLCliNit = PerCod, CliNitPpalActual = CliNitPpal),
    by = "CLCliNit"
  ) %>%
  mutate(
    CliNitPpal = coalesce(CliNitPpalActual, CLCliNit)
  ) %>%
  select(
    -CliNitPpalActual
  ) %>%
  left_join(
    cliente_actual_map,
    by = "CliNitPpal"
  ) %>%
  left_join(
    sucs,
    by = c("CLSucCod" = "SucCod")
  ) %>%
  left_join(
    n_cliente %>%
      select(-CliNitPpal),
    by = c("CliNitPpal" = "PerCod")
  ) %>%
  left_join(
    n_cliente %>%
      select(PerCod, RazonSocialCliNit = PerRazSoc),
    by = c("CLCliNit" = "PerCod")
  ) %>%
  left_join(
    contactos_raw,
    by = "CliNitPpal"
  ) %>%
  mutate(
    MarSaco = ifelse(
      FecMes <= ultimo_periodo_pyg &
        !is.na(SacosFact) &
        SacosFact != 0 &
        !is.na(Margen),
      Margen / SacosFact,
      NA_real_
    )
  ) %>%
  .imputar_cascada(
    valor_col = "MarSaco",
    niveles = NIVELES_MARGEN
  ) %>%
  mutate(
    MarSaco = if_else(
      FecMes <= ultimo_periodo_pyg &
        !(is.finite(SacosFact) & SacosFact != 0 & is.finite(Margen)),
      NA_real_, MarSaco
    ),
    Margen = ifelse(FecMes > ultimo_periodo_pyg & !is.na(SacosFact) & !is.na(MarSaco), SacosFact * MarSaco, Margen),
    across(contains("Fec"), ~ if_else(as.Date(.) == as.Date("1753-01-01"), as.Date(NA), as.Date(.))),
    CliNitPpal = ifelse(is.na(CliNitPpal), CLCliNit, CliNitPpal),
    CLLinNegNo = ifelse(CLLinNegNo == "DIFERENCIADOS", "A LA MEDIDA", CLLinNegNo),
    LinNegCod = ifelse(LinNegCod == 20000L, 21000L, LinNegCod)
  ) %>%
  select(
    FecMes,
    ANHO,
    MES,
    CLASE,
    CLSucCod,
    Sucursal,
    CLPdcCod,
    CLPdcLin,
    PdcRefCli,
    PdcFecCre,
    PdcPrecioKilo,
    Usuario,
    CLLotCod,
    FechaEmbarque,
    CLCliNit,
    CodContacto,
    TieneContacto,
    EtapaContacto,
    EstadoContacto,
    EtapaPreDescarte,
    CliNitPpal,
    RazonSocialCliNit,
    PerRazSoc,
    Asesor,
    Segmento,
    SegmentacionClientePYG,
    SSPpto,
    MNFCCPpto,
    NumMesesRecuperar,
    Excluir,
    CliCont,
    CliDir,
    CliDir1,
    CliTel,
    CliConCom,
    CliTelCom,
    CliEmlCom,
    CiuExtNom,
    LinNegCod,
    CLLinNegNo,
    LinProCod,
    MCCod,
    MrcCod,
    CLPdcFctAD,
    UMeFac,
    CLPdcCntCl,
    SacLote,
    FecAsignLote,
    CodOrdTril,
    FecOrdTril,
    SacProd,
    FecProd,
    CodDesp,
    FecDesp,
    SacDesp,
    SacFact,
    CLPdcCanFa,
    CLLotSacXP,
    CLLotPenXD,
    CLLotDesXF,
    FecFact,
    SacosFact,
    Margen,
    MarSaco,
    PendProducir,
    PendDespachar,
    PendFacturar
  ) %>%
  mutate(across(where(is.character), ~ replace_na(., "SIN DATO")))

# Validación de integridad financiera ----
if (nrow(data) != filas_financieras + filas_operativas) {
  stop("Los cruces dimensionales multiplicaron registros de data")
}

fuente_cerrada <- margenes_pyg %>%
  transmute(
    FecMes,
    LinNegCod = ifelse(.convertir_numero_margen(LINNEG) == 20000, 21000, .convertir_numero_margen(LINNEG)),
    SacosFact = .convertir_numero_margen(SACOS70),
    Margen = .convertir_numero_margen(MARGEN)
  ) %>%
  group_by(FecMes, LinNegCod) %>%
  summarise(
    RegistrosPYG = n(),
    SacosPYG = sum(SacosFact, na.rm = TRUE),
    MargenPYG = sum(Margen, na.rm = TRUE),
    .groups = "drop"
  )

resumen_cerrado <- data %>%
  filter(FecMes <= ultimo_periodo_pyg) %>%
  group_by(FecMes, LinNegCod) %>%
  summarise(
    RegistrosData = n(),
    SacosData = sum(SacosFact, na.rm = TRUE),
    MargenData = sum(Margen, na.rm = TRUE),
    .groups = "drop"
  )

comparacion_cierre <- full_join(
  fuente_cerrada,
  resumen_cerrado,
  by = c("FecMes", "LinNegCod")
) %>%
  mutate(
    DiferenciaRegistros = coalesce(RegistrosData, 0L) - coalesce(RegistrosPYG, 0L),
    DiferenciaSacos = coalesce(SacosData, 0) - coalesce(SacosPYG, 0),
    DiferenciaMargen = coalesce(MargenData, 0) - coalesce(MargenPYG, 0)
  )

if (any(
  is.na(comparacion_cierre$DiferenciaSacos) |
  abs(comparacion_cierre$DiferenciaSacos) > 0.01 |
  abs(comparacion_cierre$DiferenciaMargen) > 1 |
  comparacion_cierre$DiferenciaRegistros != 0L
)) {
  print(comparacion_cierre %>%
          filter(abs(DiferenciaSacos) > 0.01 |
                   abs(DiferenciaMargen) > 1 |
                   DiferenciaRegistros != 0L))
  stop("La base data no coincide con margenes_pyg en los meses cerrados")
}

if (any(!is.na(data$FecFact) & data$FecMes > ultimo_periodo_pyg &
        !is.finite(data$SacosFact))) {
  stop("Hay facturas de meses abiertos sin sacos validos")
}

if (any(!is.na(data$FecFact) & data$FecMes > ultimo_periodo_pyg &
        !is.finite(data$Margen))) {
  stop("Existen facturas abiertas sin margen imputado valido")
}

COLUMNAS_DATA <- c(
  "FecMes", "ANHO", "MES", "CLASE", "CLSucCod", "Sucursal", "CLPdcCod", "CLPdcLin", "PdcRefCli", "PdcFecCre",
  "PdcPrecioKilo", "Usuario", "CLLotCod", "FechaEmbarque", "CLCliNit", "CodContacto", "TieneContacto", "EtapaContacto",
  "EstadoContacto", "EtapaPreDescarte", "CliNitPpal", "RazonSocialCliNit", "PerRazSoc", "Asesor", "Segmento",
  "SegmentacionClientePYG", "SSPpto", "MNFCCPpto", "NumMesesRecuperar", "Excluir", "CliCont", "CliDir", "CliDir1",
  "CliTel", "CliConCom", "CliTelCom", "CliEmlCom", "CiuExtNom", "LinNegCod", "CLLinNegNo", "LinProCod", "MCCod",
  "MrcCod", "CLPdcFctAD", "UMeFac", "CLPdcCntCl", "SacLote", "FecAsignLote", "CodOrdTril", "FecOrdTril", "SacProd",
  "FecProd", "CodDesp", "FecDesp", "SacDesp", "SacFact", "CLPdcCanFa", "CLLotSacXP", "CLLotPenXD", "CLLotDesXF",
  "FecFact", "SacosFact", "Margen", "MarSaco", "PendProducir", "PendDespachar", "PendFacturar"
)

if (!identical(names(data), COLUMNAS_DATA)) {
  stop("La estructura final de data cambio respecto al contrato de 67 variables")
}

## Segmentaciones ----
es_primer_dia <- lubridate::day(Sys.Date()) == 1L

if (es_primer_dia) {
  # Históricos de segmentación ----
  seg_hist_bd <- .cargar_historico_segmentacion("CRMNALSEGR")
  rfm_s_hist_bd <- .cargar_historico_segmentacion("CRMNALRFM")
  rfm_m_hist_bd <- .cargar_historico_segmentacion("CRMNALRFMM")
  clv_hist_bd <- .cargar_historico_segmentacion("CRMNALCLV")
  
  # Segmento Racafé ----
  print("Calculando segmento Racafé...")
  ana_dates <- .generar_fechas_analisis("2020-02-01")
  ana_dates <- .fechas_pendientes(ana_dates, seg_hist_bd)
  
  personas_segmento <- clientes_hist %>%
    dplyr::select(FecMes, LinNegCod, CliNitPpal, NumMesesRecuperar) %>%
    dplyr::distinct(FecMes, LinNegCod, CliNitPpal, .keep_all = TRUE)
  
  seg_nuevo <- purrr::map_dfr(
    ana_dates,
    function(x) {
      parametros_mes <- personas_segmento %>%
        dplyr::filter(FecMes == x) %>%
        dplyr::select(LinNegCod, CliNitPpal, NumMesesRecuperar)
      
      data %>%
        dplyr::filter(FecFact < x, !is.na(FecFact)) %>%
        dplyr::group_by(LinNegCod, CliNitPpal) %>%
        dplyr::summarise(FecUltDesp = max(FecFact, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(parametros_mes, by = c("LinNegCod", "CliNitPpal")) %>%
        dplyr::mutate(Meses = ifelse(is.na(NumMesesRecuperar), 4L, NumMesesRecuperar),
                      SegmentoRacafe = ifelse(FecUltDesp >= x - lubridate::months(Meses), "CLIENTE",
                                              "CLIENTE A RECUPERAR"),
                      FecProceso = x) %>%
        dplyr::select(-FecUltDesp, -NumMesesRecuperar)
    }
  )
  
  seg <- dplyr::bind_rows(seg_hist_bd, seg_nuevo) %>%
    dplyr::distinct(FecProceso, CliNitPpal, LinNegCod, .keep_all = TRUE)
  
  if (nrow(seg_nuevo) > 0L) {
    .identificar_clientes_faltantes(data, seg_nuevo)
    racafeBD::EscribirDatos(seg, "CRMNALSEGR")
  } else {
    message("Segmento Racafé actualizado; no existen períodos pendientes")
  }
  
  # RFM ----
  personas_rfm <- clientes_hist %>%
    dplyr::select(FecMes, LinNegCod, CliNitPpal, Segmento) %>%
    dplyr::distinct(FecMes, LinNegCod, CliNitPpal, .keep_all = TRUE)
  
  segmentos_racafe <- seg %>%
    dplyr::mutate(FecProceso = as.Date(FecProceso)) %>%
    dplyr::select(LinNegCod, CliNitPpal, FecProceso, SegmentoRacafe)
  
  SEGMENTOS_RFM <- c("DETAL", "MEDIANO", "GRANDES")
  SEGMENTOS_RACAFE_RFM <- c("CLIENTE A RECUPERAR", "CLIENTE")
  LINEAS_NEGOCIO_RFM <- c(10000, 21000)
  
  SEGMENT_NAMES <- c(
    "CAMPEONES", "CLIENTES LEALES", "POTENCIALES LEALES", "NUEVOS CLIENTES", "PROMETEDORES",
    "NECESITAN ATENCIÓN", "A PUNTO DE DORMIR", "EN RIESGO", "NO PODEMOS PERDERLOS", "HIBERNANDO", "PERDIDOS"
  )
  
  RECENCY_LOWER <- c(4, 2, 3, 4, 3, 3, 2, 1, 1, 2, 1)
  RECENCY_UPPER <- c(5, 4, 5, 5, 4, 4, 3, 2, 1, 3, 1)
  FREQUENCY_LOWER <- c(4, 3, 1, 1, 1, 3, 1, 2, 4, 2, 1)
  FREQUENCY_UPPER <- c(5, 4, 3, 1, 1, 4, 2, 5, 5, 3, 1)
  MONETARY_LOWER <- c(4, 4, 1, 1, 1, 3, 1, 2, 4, 2, 1)
  MONETARY_UPPER <- c(5, 5, 3, 1, 1, 4, 2, 5, 5, 3, 1)
  
  .procesar_rfm <- function(data_source, variable_revenue, historico, tabla_destino) {
    print(paste("RFM", variable_revenue))
    
    data_rfm <- data_source %>%
      dplyr::filter(!is.na(FecFact), FecFact < racafeCore::PrimerDia(Sys.Date()),
                    !is.na(.data[[variable_revenue]])) %>%
      dplyr::mutate(FecMes = lubridate::floor_date(FecFact, unit = "month")) %>%
      dplyr::select(-Segmento) %>%
      dplyr::left_join(personas_rfm, by = c("FecMes", "CliNitPpal", "LinNegCod")) %>%
      dplyr::transmute(customer_id = CliNitPpal, Segmento, LinNegCod, order_date = FecFact,
                       revenue = .data[[variable_revenue]]) %>%
      dplyr::filter(!is.na(revenue))
    
    ana_dates_rfm <- .generar_fechas_analisis("2020-02-01", ajuste = -1L)
    ana_dates_rfm <- .fechas_pendientes(ana_dates_rfm, historico, function(x) x + lubridate::days(1L))
    
    rfm_nuevo <- purrr::map_dfr(
      ana_dates_rfm,
      function(x) {
        aux0 <- data_rfm %>%
          dplyr::mutate(FecProceso = x + lubridate::days(1L)) %>%
          dplyr::left_join(segmentos_racafe %>% dplyr::filter(FecProceso == x + lubridate::days(1L)),
                           by = c("LinNegCod", "customer_id" = "CliNitPpal", "FecProceso")) %>%
          dplyr::filter(order_date <= x, order_date > x - lubridate::years(1), Segmento %in% SEGMENTOS_RFM,
                        SegmentoRacafe %in% SEGMENTOS_RACAFE_RFM, LinNegCod %in% LINEAS_NEGOCIO_RFM)
        
        if (nrow(aux0) == 0L) {
          return(data.frame())
        }
        
        aux0 %>%
          dplyr::group_by(Segmento, SegmentoRacafe, LinNegCod) %>%
          dplyr::group_modify(
            ~ {
              aux1 <- rfm::rfm_table_order(.x, customer_id, order_date, revenue, x)
              rfm::rfm_segment(aux1, SEGMENT_NAMES, RECENCY_LOWER, RECENCY_UPPER, FREQUENCY_LOWER,
                               FREQUENCY_UPPER, MONETARY_LOWER, MONETARY_UPPER) %>%
                dplyr::mutate(FecProceso = x + lubridate::days(1L),
                              segment = ifelse(segment == "Others", "OTROS", segment)) %>%
                dplyr::rename(SegmentoAnalitica = segment, CliNitPpal = customer_id)
            }
          ) %>%
          dplyr::ungroup()
      }
    )
    
    resultado <- dplyr::bind_rows(historico, rfm_nuevo) %>%
      dplyr::distinct(FecProceso, CliNitPpal, LinNegCod, .keep_all = TRUE)
    
    if (nrow(rfm_nuevo) > 0L) {
      .identificar_clientes_faltantes(data, rfm_nuevo)
      racafeBD::EscribirDatos(resultado, tabla_destino)
    } else {
      message(tabla_destino, " actualizado; no existen períodos pendientes")
    }
    
    resultado
  }
  
  rfm_s <- .procesar_rfm(data, variable_revenue = "SacosFact", historico = rfm_s_hist_bd,
                         tabla_destino = "CRMNALRFM")
  rfm_m <- .procesar_rfm(data, variable_revenue = "Margen", historico = rfm_m_hist_bd,
                         tabla_destino = "CRMNALRFMM")
  
  # CLV ----
  base_clv <- .preparar_base_clv(data)
  ana_dates_clv <- .generar_fechas_analisis("2020-01-01", ajuste = -1L)
  ana_dates_clv <- .fechas_pendientes(ana_dates_clv, clv_hist_bd, function(x) x + lubridate::days(1L))
  
  if (length(ana_dates_clv) > 12L) {
    cores <- parallel::detectCores(logical = FALSE)
    cores <- ifelse(is.na(cores), 1L, cores)
    workers <- max(1L, min(length(ana_dates_clv), cores - 1L))
    future::plan(future::multisession, workers = workers)
    clv_nuevo <- furrr::future_map_dfr(ana_dates_clv, .procesar_clv, base_clv = base_clv, .progress = TRUE)
    future::plan(future::sequential)
  } else {
    clv_nuevo <- purrr::map_dfr(ana_dates_clv, .procesar_clv, base_clv = base_clv)
  }
  
  clv_results <- dplyr::bind_rows(clv_hist_bd, clv_nuevo) %>%
    dplyr::distinct(FecProceso, CliNitPpal, LinNegCod, .keep_all = TRUE)
  
  if (nrow(clv_nuevo) > 0L) {
    racafeBD::EscribirDatos(clv_results, "CRMNALCLV")
  } else {
    message("CLV actualizado; no existen períodos pendientes")
  }
  
  message("Segmentaciones completadas: ", Sys.time())
} else {
  message("Segmentaciones omitidas (no es primer día del mes): ", Sys.Date())
}

## Segmentaciones históricas ----
segmentos_raw <- racafeBD::CargarDatos("CRMNALSEGR") %>%
  mutate(
    FecProceso = as.Date(FecProceso),
    FecMes = .fecha_mes_estado(FecProceso),
    CliNitPpal = as.numeric(CliNitPpal),
    LinNegCod = as.numeric(LinNegCod)
  )

segmentos_hist_mes <- segmentos_raw %>%
  arrange(FecProceso) %>%
  group_by(
    FecMes,
    CliNitPpal,
    LinNegCod
  ) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  select(FecMes, CliNitPpal, LinNegCod, SegmentoRacafe)

rfm_hist_s <- racafeBD::CargarDatos("CRMNALRFM") %>%
  mutate(
    FecProceso = as.Date(FecProceso),
    FecMes = .fecha_mes_estado(FecProceso),
    CliNitPpal = as.numeric(CliNitPpal),
    LinNegCod = as.numeric(LinNegCod)
  ) %>%
  arrange(FecProceso) %>%
  group_by(
    FecMes,
    CliNitPpal,
    LinNegCod
  ) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  select(
    FecMes,
    CliNitPpal,
    LinNegCod,
    SegmentoAnaliticaS = SegmentoAnalitica,
    rfm_scoreS = rfm_score,
    transaction_countS = transaction_count,
    recency_daysS = recency_days,
    amountS = amount,
    recency_scoreS = recency_score,
    frequency_scoreS = frequency_score,
    monetary_scoreS = monetary_score
  )

rfm_hist_m <- racafeBD::CargarDatos("CRMNALRFMM") %>%
  mutate(
    FecProceso = as.Date(FecProceso),
    FecMes = .fecha_mes_estado(FecProceso),
    CliNitPpal = as.numeric(CliNitPpal),
    LinNegCod = as.numeric(LinNegCod)
  ) %>%
  arrange(FecProceso) %>%
  group_by(
    FecMes,
    CliNitPpal,
    LinNegCod
  ) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  select(
    FecMes,
    CliNitPpal,
    LinNegCod,
    SegmentoAnaliticaM = SegmentoAnalitica,
    rfm_scoreM = rfm_score,
    transaction_countM = transaction_count,
    recency_daysM = recency_days,
    amountM = amount,
    recency_scoreM = recency_score,
    frequency_scoreM = frequency_score,
    monetary_scoreM = monetary_score
  )

clv_hist <- racafeBD::CargarDatos("CRMNALCLV") %>%
  mutate(
    FecProceso = as.Date(FecProceso),
    FecMes = .fecha_mes_estado(FecProceso),
    CliNitPpal = as.numeric(CliNitPpal),
    LinNegCod = as.numeric(LinNegCod)
  ) %>%
  arrange(FecProceso) %>%
  group_by(
    FecMes,
    CliNitPpal,
    LinNegCod
  ) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  select(FecMes, CliNitPpal, LinNegCod, Churn, SacosPred)

segmentaciones_hist <- segmentos_hist_mes %>%
  full_join(
    rfm_hist_s,
    by = c("FecMes", "CliNitPpal", "LinNegCod")
  ) %>%
  full_join(
    rfm_hist_m,
    by = c("FecMes", "CliNitPpal", "LinNegCod")
  ) %>%
  full_join(
    clv_hist,
    by = c("FecMes", "CliNitPpal", "LinNegCod")
  ) %>%
  arrange(
    CliNitPpal,
    LinNegCod,
    FecMes
  ) %>%
  group_by(
    CliNitPpal,
    LinNegCod
  ) %>%
  tidyr::fill(
    -FecMes,
    .direction = "down"
  ) %>%
  ungroup()

segmentaciones_actual <- segmentaciones_hist %>%
  group_by(
    CliNitPpal,
    LinNegCod
  ) %>%
  filter(
    FecMes == max(FecMes, na.rm = TRUE)
  ) %>%
  slice_tail(n = 1L) %>%
  ungroup()

# Proyección comercial por línea de embarque ----

fecha_actualizacion_operativa <- Sys.time()
fecha_actualizacion_pyg <- Sys.time()

# Imputación de margen con historia financiera exclusivamente cerrada ----
margenes_entrenamiento <- data %>%
  filter(
    FecMes <= ultimo_periodo_pyg,
    is.finite(SacosFact), SacosFact > 0,
    is.finite(Margen)
  ) %>%
  group_by(CLCliNit, LinNegCod, LinProCod) %>%
  summarise(
    SacosEntrenamiento = sum(SacosFact),
    MargenEntrenamiento = sum(Margen),
    NumOperaciones = n(),
    .groups = "drop"
  ) %>%
  mutate(MarSacoCliente = MargenEntrenamiento / SacosEntrenamiento)

margenes_producto <- data %>%
  filter(
    FecMes <= ultimo_periodo_pyg,
    is.finite(SacosFact), SacosFact > 0,
    is.finite(Margen)
  ) %>%
  group_by(LinNegCod, LinProCod) %>%
  summarise(MarSacoProducto = sum(Margen) / sum(SacosFact), .groups = "drop")

margenes_linea <- data %>%
  filter(
    FecMes <= ultimo_periodo_pyg,
    is.finite(SacosFact), SacosFact > 0,
    is.finite(Margen)
  ) %>%
  group_by(LinNegCod) %>%
  summarise(MarSacoLinea = sum(Margen) / sum(SacosFact), .groups = "drop")

margen_global <- data %>%
  filter(
    FecMes <= ultimo_periodo_pyg,
    is.finite(SacosFact), SacosFact > 0,
    is.finite(Margen)
  ) %>%
  summarise(MarSacoGlobal = sum(Margen) / sum(SacosFact)) %>%
  pull(MarSacoGlobal)

if (length(margen_global) != 1L || !is.finite(margen_global)) {
  margen_global <- NA_real_
}

# Proyección activa e histórico de pedidos ----
pedidos_integrados <- ped %>%
  left_join(resumen_lotes_linea, by = c("PdcCod", "PdcLin")) %>%
  mutate(
    NumLotes = coalesce(NumLotes, 0L),
    SacosAsignados = coalesce(SacosAsignados, 0),
    SacosProducidos = coalesce(SacosProducidos, 0),
    SacosDespachados = coalesce(SacosDespachados, 0),
    SacosFacturados = coalesce(SacosFacturados, 0),
    CantidadAsignadaOrigen = coalesce(CantidadAsignadaOrigen, 0),
    CantidadProducidaOrigen = coalesce(CantidadProducidaOrigen, 0),
    CantidadDespachadaOrigen = coalesce(CantidadDespachadaOrigen, 0),
    UnidadLoteValidada = coalesce(UnidadLoteValidada, FALSE),
    SacosPendientes = if_else(is.finite(SacosPedido), pmax(SacosPedido - SacosFacturados, 0), NA_real_),
    FechaFactProbable = PdcFecEst,
    FecMesProbable = floor_date(PdcFecEst, "month"),
    FechaEstimadaVencida = !is.na(PdcFecEst) & PdcFecEst < Sys.Date(),
    FechaVencida = FechaEstimadaVencida &
      coalesce(SacosPendientes > 0, FALSE),
    Proyectar = PdcEst == "A" & !is.na(PdcFecEst) &
      coalesce(SacosPendientes > 0, FALSE)
  ) %>%
  left_join(margenes_entrenamiento,
            by = c("CLCliNit", "LinNegCod", "LinProCod")) %>%
  left_join(margenes_producto, by = c("LinNegCod", "LinProCod")) %>%
  left_join(margenes_linea, by = "LinNegCod") %>%
  mutate(
    MarSacoEstimado = coalesce(
      if_else(coalesce(NumOperaciones, 0L) >= 3L,
              MarSacoCliente, NA_real_),
      MarSacoProducto,
      MarSacoLinea,
      margen_global
    ),
    MetodoMargen = case_when(
      coalesce(NumOperaciones, 0L) >= 3L &
        is.finite(MarSacoCliente) ~ "CLIENTE_PRODUCTO",
      is.finite(MarSacoProducto) ~ "PRODUCTO",
      is.finite(MarSacoLinea) ~ "LINEA_NEGOCIO",
      is.finite(margen_global) ~ "GLOBAL",
      TRUE ~ "SIN_ESTIMACION"
    ),
    MargenEstimado = if_else(Proyectar & is.finite(MarSacoEstimado), SacosPendientes * MarSacoEstimado, NA_real_),
    IngresoEstimado = if_else(
      Proyectar & is.finite(PdcPrecioKilo) & is.finite(SacosPendientes),
      SacosPendientes * 70 * PdcPrecioKilo,
      NA_real_
    ),
    FechaCorteOperativo = as.Date(fecha_actualizacion_operativa)
  )

.verificar_clave_unica(pedidos_integrados, c("PdcCod", "PdcLin"),
                       "pedidos_integrados")

# Alertas de conciliación ----
alertas_proyeccion <- pedidos_integrados %>%
  mutate(
    Alerta = case_when(
      is.na(SacosPedido) ~ "UNIDAD_SIN_CONVERSION",
      PdcEst == "A" & is.na(PdcFecEst) ~ "SIN_FECHA_ESTIMADA",
      SacosFacturados > SacosPedido + 0.01 ~ "FACTURACION_SUPERA_PEDIDO",
      UnidadLoteValidada & SacosAsignados > SacosPedido + 0.01 ~ "LOTES_SUPERAN_PEDIDO",
      FechaVencida ~ "FECHA_VENCIDA",
      Proyectar & !is.finite(MarSacoEstimado) ~ "SIN_MARGEN",
      !UnidadLoteValidada & NumLotes > 0 ~ "UNIDAD_LOTE_SIN_VALIDAR",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Alerta)) %>%
  select(PdcCod, PdcLin, PdcEst, Alerta, PdcFecCie, PdcFecEst,
         SacosPedido, SacosAsignados, SacosFacturados, SacosPendientes)

# La fecha probable no se fuerza al mes abierto aunque ya haya vencido ----
proyecciones_comerciales <- pedidos_integrados %>%
  filter(Proyectar) %>%
  transmute(
    CLPdcCod = PdcCod,
    CLPdcLin = PdcLin,
    CLCliNit,
    LinNegCod,
    LinProCod,
    PdcEst,
    PdcFecCre,
    PdcFecCie,
    PdcFecEst,
    FechaFactProbable,
    FecMesProbable,
    FechaVencida,
    NumLotes,
    SacosPedido,
    SacosAsignados,
    SacosFacturados,
    SacosProyectados = SacosPendientes,
    PdcPrecioKilo,
    IngresoEstimado,
    MarSacoEstimado,
    MargenEstimado,
    MetodoMargen,
    OrigenDato = "PEDIDO",
    EstadoFinanciero = "PROYECTADO",
    FechaCorteOperativo
  )

if (anyDuplicated(proyecciones_comerciales[c("CLPdcCod", "CLPdcLin")])) {
  stop("La proyeccion multiplica lineas de pedido")
}

if (any(!is.finite(proyecciones_comerciales$SacosProyectados) |
        proyecciones_comerciales$SacosProyectados <= 0)) {
  stop("La proyeccion contiene volumen no positivo o no finito")
}

# Fuente analítica extendida: no modifica los 67 campos originales de data ----
# ESTADOS FINANCIEROS ----
fechas_pedido_linea <- ped %>%
  transmute(CLPdcCod = PdcCod, CLPdcLin = PdcLin,
            PdcFecEst, FecMesEstimado = floor_date(PdcFecEst, "month"))

.verificar_clave_unica(fechas_pedido_linea, c("CLPdcCod", "CLPdcLin"),
                       "fechas_pedido_linea")

data_crm <- bind_rows(
  data %>%
    left_join(fechas_pedido_linea, by = c("CLPdcCod", "CLPdcLin"),
              relationship = "many-to-one") %>%
    mutate(
      TipoRegistro = if_else(!is.na(FecFact), "FACTURADO", "PENDIENTE"),
      OrigenDato = case_when(
        !is.na(FecFact) & FecMes <= ultimo_periodo_pyg ~ "PYG",
        !is.na(FecFact) ~ "SYSCAFE",
        TRUE ~ "PEDIDO"
      ),
      EstadoFinanciero = case_when(
        !is.na(FecFact) & FecMes <= ultimo_periodo_pyg ~ "CERRADO",
        !is.na(FecFact) ~ "FACTURADO_ABIERTO",
        TRUE ~ "PENDIENTE"
      ),
      FechaFactProbable = if_else(is.na(FecFact), PdcFecEst, as.Date(NA)),
      FecMesProbable = if_else(is.na(FecFact), FecMesEstimado, as.Date(NA)),
      FechaVencida = !is.na(PdcFecEst) & PdcFecEst < Sys.Date() & is.na(FecFact),
      FecMesAnalisis = if_else(!is.na(FecFact), FecMes, FecMesEstimado),
      SacosProyectados = NA_real_,
      MargenEstimado = NA_real_,
      IngresoEstimado = NA_real_,
      MargenContable = if_else(EstadoFinanciero == "CERRADO", Margen, NA_real_),
      MargenAbiertoEstimado = if_else(
        EstadoFinanciero == "FACTURADO_ABIERTO", Margen, NA_real_
      ),
      MargenProyectado = NA_real_,
      FuenteFinanciera = case_when(
        EstadoFinanciero == "CERRADO" ~ "CRMNALMARLOT",
        EstadoFinanciero == "FACTURADO_ABIERTO" ~ "SYSCAFE_ESTIMADO",
        TRUE ~ "SIN_EJECUCION"
      )
    ),
  proyecciones_comerciales %>%
    mutate(
      TipoRegistro = "PROYECTADO",
      FecMes = FecMesProbable,
      FecMesEstimado = FecMesProbable,
      FecMesAnalisis = FecMesProbable,
      FecFact = as.Date(NA),
      SacosFact = NA_real_,
      Margen = NA_real_,
      MargenContable = NA_real_,
      MargenAbiertoEstimado = NA_real_,
      MargenProyectado = MargenEstimado,
      FuenteFinanciera = "PROYECCION_PEDIDO"
    )
) %>%
  mutate(
    MargenKg = if_else(is.finite(SacosFact) & SacosFact > 0 &
                         is.finite(Margen), Margen / (SacosFact * 70), NA_real_),
    MargenKgProyectado = if_else(is.finite(SacosProyectados) &
                                   SacosProyectados > 0 &
                                   is.finite(MargenProyectado),
                                 MargenProyectado / (SacosProyectados * 70),
                                 NA_real_)
  )

if (nrow(data_crm) != nrow(data) + nrow(proyecciones_comerciales)) {
  stop("La base data_crm tiene un numero inesperado de registros")
}

# INDICADORES DE PEDIDOS ----
indicadores_pedidos <- pedidos_integrados %>%
  transmute(
    PdcCod, PdcLin, CLCliNit, LinNegCod, LinProCod, PdcEst,
    PdcFecCre, PdcFecEst, FecMesEstimado = FecMesProbable,
    FechaVencida, NumLotes, UnidadLoteValidada,
    SacosPedido, SacosFacturados, SacosPendientes,
    CantidadAsignadaOrigen, CantidadProducidaOrigen,
    CantidadDespachadaOrigen,
    MargenEstimado, MetodoMargen,
    PedidoSinLote = NumLotes == 0L,
    PedidoConFechaEstimada = !is.na(PdcFecEst)
  )

.verificar_clave_unica(indicadores_pedidos, c("PdcCod", "PdcLin"),
                       "indicadores_pedidos")

indicadores_pedidos_mes <- indicadores_pedidos %>%
  filter(PdcEst == "A", !is.na(FecMesEstimado)) %>%
  group_by(FecMesEstimado, LinNegCod) %>%
  summarise(
    LineasPedido = n(),
    LineasSinLote = sum(PedidoSinLote),
    LineasVencidas = sum(FechaVencida),
    SacosComprometidos = sum(SacosPedido, na.rm = TRUE),
    SacosPendientes = sum(SacosPendientes, na.rm = TRUE),
    .groups = "drop"
  )

# CONTROL DE FUENTES FINANCIERAS ----
if (any(data_crm$EstadoFinanciero == "PROYECTADO" &
        !is.na(data_crm$SacosFact), na.rm = TRUE)) {
  stop("Los registros proyectados no pueden tener sacos facturados")
}

if (any(data_crm$EstadoFinanciero == "PENDIENTE" &
        !is.na(data_crm$Margen), na.rm = TRUE)) {
  stop("Los pedidos pendientes no pueden tener margen ejecutado")
}

# AUDITORIA FINANCIERA MENSUAL ----
control_pyg_mensual <- margenes_pyg %>%
  transmute(
    CLLotCod = .convertir_numero_margen(LOTE),
    FecMes = as.Date(FecMes),
    SacosPYG = .convertir_numero_margen(SACOS70),
    MargenPYG = .convertir_numero_margen(MARGEN)
  ) %>%
  filter(!is.na(CLLotCod), CLLotCod != 0) %>%
  group_by(CLLotCod, FecMes) %>%
  summarise(
    SacosPYG = sum(SacosPYG, na.rm = TRUE),
    MargenPYG = sum(MargenPYG, na.rm = TRUE),
    .groups = "drop"
  )

control_syscafe_mensual <- fact %>%
  filter(FecMesFact <= ultimo_periodo_pyg) %>%
  group_by(CLLotCod, FecMes = FecMesFact) %>%
  summarise(SacosSYSCAFE = sum(SacosFact, na.rm = TRUE), .groups = "drop")

auditoria_financiera_mensual <- full_join(
  control_pyg_mensual,
  control_syscafe_mensual,
  by = c("CLLotCod", "FecMes")
) %>%
  mutate(
    DiferenciaSacos = coalesce(SacosSYSCAFE, 0) - coalesce(SacosPYG, 0),
    EstadoConciliacion = case_when(
      is.na(SacosPYG) ~ "SIN_PYG",
      is.na(SacosSYSCAFE) ~ "SIN_SYSCAFE",
      abs(DiferenciaSacos) > 0.01 ~ "DIFERENCIA",
      TRUE ~ "CONCILIADO"
    ),
    FuenteVerdad = "CRMNALMARLOT"
  )

# Indicadores de calidad ----
validaciones_etl <- tibble(
  FechaProceso = Sys.time(),
  UltimoPeriodoPYG = ultimo_periodo_pyg,
  FechaInicioAnalisis = fecha_inicio_analisis,
  PedidosHistoricosExcluidos = nrow(pedidos_excluidos_historicos),
  PedidosLineas = nrow(ped),
  PedidosSinLote = nrow(pedido_sin_lote),
  LotesSinPedido = nrow(lotes_sin_pedido),
  PedidosConFechasDistintas = n_distinct(pedidos_fechas_distintas$PdcCod),
  PedidosActivosSinFecha = nrow(ped_sin_fecha),
  PedidosActivosSinConversion = nrow(ped_sin_conversion),
  LineasProyectadas = nrow(proyecciones_comerciales),
  LineasVencidas = sum(indicadores_pedidos$FechaVencida),
  LineasSinLoteActivas = sum(indicadores_pedidos$PdcEst == "A" &
                               indicadores_pedidos$PedidoSinLote),
  LineasConLoteSinUnidadValidada = sum(indicadores_pedidos$NumLotes > 0L &
                                         !indicadores_pedidos$UnidadLoteValidada),
  SacosPendientesSinLote = sum(indicadores_pedidos$SacosPendientes[
    indicadores_pedidos$PdcEst == "A" & indicadores_pedidos$PedidoSinLote
  ], na.rm = TRUE),
  LineasConAlertas = nrow(alertas_proyeccion),
  RegistrosPYG = nrow(data_pyg),
  RegistrosFacturaAbierta = nrow(data_fact_abierta),
  LotesConDiferenciaCierre = sum(auditoria_cierre_operativo$EstadoConciliacion != "CONCILIADO"),
  LoteMesConDiferencia = sum(auditoria_financiera_mensual$EstadoConciliacion != "CONCILIADO")
)

print(validaciones_etl)

# Persistencia ----
ruta_lotes_cache <- "APP/data/lotes_raw.rds"

if (!dir.exists(dirname(ruta_lotes_cache))) {
  dir.create(dirname(ruta_lotes_cache), recursive = TRUE)
}

saveRDS(lotes_raw, ruta_lotes_cache)

# EXPORTACION MAESTRA ----

.normalizar_fecha_crm <- function(x) {
  x <- as.Date(x)
  x[!is.na(x) & x <= as.Date("1753-01-01")] <- as.Date(NA)
  x
}

# HOMOLOGACIONES ----
lin_neg <- racafeBD::ConsultaSistema(
  "syscafe", "SELECT LinNegCod, LinNegNom AS LinNeg FROM NLINEANE WHERE CiaCod = 10"
) %>% distinct(LinNegCod, .keep_all = TRUE)

lin_pro <- racafeBD::ConsultaSistema(
  "syscafe", "SELECT LinProCod, LinProNom FROM NTIPPROD WHERE CiaCod = 10"
) %>% distinct(LinProCod, .keep_all = TRUE)

n_marcom <- racafeBD::ConsultaSistema(
  "syscafe", "SELECT MCCod, MCNom FROM NMARCOM"
) %>% distinct(MCCod, .keep_all = TRUE)

n_marcas <- racafeBD::ConsultaSistema(
  "syscafe", "SELECT MrcCod, MrcNom AS Marca FROM NMARCAS"
) %>% distinct(MrcCod, .keep_all = TRUE)

productos_hom <- racafeBD::CargarDatos("CRMNALPRODS")
ordenes_hom <- racafeBD::CargarDatos("CRMNALORDCMP")

homologaciones <- list(
  clientes = clientes_actual,
  clientes_historico = clientes_hist,
  nit_principal = nit_ppal,
  contactos = contactos_clientes,
  segmentos = segmentaciones_actual,
  segmentos_historico = segmentaciones_hist,
  lineas_negocio = lin_neg,
  tipos_producto = lin_pro,
  nombres_comerciales = n_marcom,
  marcas = n_marcas,
  productos = productos_hom,
  ordenes_compra = ordenes_hom,
  sucursales = sucs,
  clientes_sistema = n_cliente,
  facturacion = fact,
  margenes_pyg = margenes_pyg,
  lotes = lotes_raw
)

# TRAZABILIDAD FACTURAS ----
.facturas_referencia <- function() {
  sql_nal <- paste(
    "SELECT F1.FcnLot AS CLLotCod, F2.FcnFec AS FecFact,",
    "F1.FcnNum AS NumeroFactura, 'NACIONAL' AS TipoFactura",
    "FROM FCTFACN1 F1 INNER JOIN FCTFACNA F2",
    "ON F1.CiaCod = F2.CiaCod AND F1.FcnNum = F2.FcnNum",
    "WHERE F1.CiaCod = 10 AND F2.FcnEtd = 'C'",
    sprintf("AND F2.FcnFec >= '%s'",
            format(fecha_inicio_analisis, "%Y-%m-%d"))
  )
  sql_ext <- paste(
    "SELECT F1.FctLot AS CLLotCod, F2.FctFec AS FecFact,",
    "F1.FctNum AS NumeroFactura, 'EXTERIOR' AS TipoFactura",
    "FROM FCTFACC2 F1 INNER JOIN FCTFACCA F2",
    "ON F1.CiaCod = F2.CiaCod AND F1.FctNum = F2.FctNum",
    "WHERE F1.CiaCod = 10 AND F1.FctLotAnu <> 'S'",
    sprintf("AND F2.FctFec >= '%s'",
            format(fecha_inicio_analisis, "%Y-%m-%d"))
  )
  bind_rows(
    racafeBD::ConsultaSistema("syscafe", sql_nal),
    racafeBD::ConsultaSistema("syscafe", sql_ext)
  ) %>%
    mutate(
      CLLotCod = as.numeric(CLLotCod),
      FecMes = floor_date(as.Date(FecFact), "month"),
      ReferenciaFactura = paste0(TipoFactura, ":", NumeroFactura)
    ) %>%
    filter(!is.na(CLLotCod), !is.na(FecMes)) %>%
    group_by(CLLotCod, FecMes) %>%
    summarise(
      FacturasSYSCAFE = paste(sort(unique(ReferenciaFactura)), collapse = "; "),
      NumFacturasSYSCAFE = n_distinct(ReferenciaFactura),
      .groups = "drop"
    )
}

referencias_factura <- .facturas_referencia()
.verificar_clave_unica(referencias_factura, c("CLLotCod", "FecMes"),
                       "referencias_factura")

# DATOS MAESTROS ----
if (!"FecDespachoReal" %in% names(lotes_raw)) {
  lotes_raw$FecDespachoReal <- as.Date(NA)
}

mapa_lotes <- lotes_raw %>%
  transmute(
    CLLotCod = as.numeric(CLLotCod),
    PedidoLote = as.numeric(CLPdcCod),
    LineaLote = as.numeric(CLPdcLin),
    SucursalLote = as.numeric(CLSucCod),
    SacosLoteOrigen = as.numeric(SacLote),
    SacosProducidosOrigen = as.numeric(SacProd),
    SacosDespachadosOrigen = as.numeric(SacDesp),
    FechaAsignacionLote = .normalizar_fecha_crm(FecAsignLote),
    FechaOrdenTrilla = .normalizar_fecha_crm(FecOrdTril),
    FechaProduccion = .normalizar_fecha_crm(FecProd),
    FechaInstruccionDespacho = .normalizar_fecha_crm(FecDesp),
    FechaDespachoReal = .normalizar_fecha_crm(FecDespachoReal),
    CodigoOrdenTrilla = as.numeric(CodOrdTril),
    CodigoDespacho = as.numeric(CodDesp),
    CodigoMarcaLote = as.numeric(MrcCod),
    CodigoNombreComercialLote = as.numeric(MCCod)
  ) %>%
  distinct(CLLotCod, .keep_all = TRUE)

.verificar_clave_unica(mapa_lotes, "CLLotCod", "mapa_lotes")

mapa_pedidos <- ped %>%
  transmute(
    CLPdcCod = as.numeric(PdcCod),
    CLPdcLin = as.numeric(PdcLin),
    EstadoPedido = PdcEst,
    FechaCreacionPedido = .normalizar_fecha_crm(PdcFecCre),
    FechaCierrePedido = .normalizar_fecha_crm(PdcFecCie),
    FechaEstimadaPedido = .normalizar_fecha_crm(PdcFecEst),
    CantidadPedidoOrigen = as.numeric(PdcCan),
    UnidadKgPedido = as.numeric(UMeFac),
    SacosPedido70Linea = as.numeric(SacosPedido),
    PrecioKiloPedido = as.numeric(PdcPrecioKilo)
  )

.verificar_clave_unica(mapa_pedidos, c("CLPdcCod", "CLPdcLin"),
                       "mapa_pedidos")

saldos_lineas <- pedidos_integrados %>%
  transmute(
    CLPdcCod = as.numeric(PdcCod),
    CLPdcLin = as.numeric(PdcLin),
    NumLotesLinea = as.integer(NumLotes),
    SacosCerradosYAbiertos70Linea = as.numeric(SacosFacturados),
    SacosPendientes70Linea = as.numeric(SacosPendientes),
    MargenProyectadoLinea = as.numeric(MargenEstimado),
    MetodoMargenLinea = as.character(MetodoMargen),
    FechaVencidaLinea = as.logical(FechaVencida)
  )

.verificar_clave_unica(saldos_lineas, c("CLPdcCod", "CLPdcLin"),
                       "saldos_lineas")

crm_maestro <- data_crm %>%
  mutate(
    CLPdcCod = as.numeric(CLPdcCod),
    CLPdcLin = as.numeric(CLPdcLin),
    CLLotCod = as.numeric(CLLotCod)
  ) %>%
  left_join(mapa_lotes, by = "CLLotCod", relationship = "many-to-one") %>%
  mutate(
    CLPdcCod = coalesce(CLPdcCod, PedidoLote),
    CLPdcLin = coalesce(CLPdcLin, LineaLote)
  ) %>%
  left_join(mapa_pedidos, by = c("CLPdcCod", "CLPdcLin"),
            relationship = "many-to-one") %>%
  left_join(saldos_lineas, by = c("CLPdcCod", "CLPdcLin"),
            relationship = "many-to-one") %>%
  left_join(referencias_factura, by = c("CLLotCod", "FecMes"),
            relationship = "many-to-one") %>%
  mutate(
    PdcFecEst = coalesce(FechaEstimadaPedido, PdcFecEst),
    PdcFecEst = .normalizar_fecha_crm(PdcFecEst),
    FechaVencida = !is.na(PdcFecEst) & PdcFecEst < Sys.Date() &
      coalesce(SacosPendientes70Linea > 0, FALSE),
    FactorSacos70 = if_else(is.finite(UnidadKgPedido) &
                              UnidadKgPedido > 0, UnidadKgPedido / 70, NA_real_),
    SacosLote70 = SacosLoteOrigen * FactorSacos70,
    SacosProducidos70 = SacosProducidosOrigen * FactorSacos70,
    SacosDespachados70 = SacosDespachadosOrigen * FactorSacos70,
    FuentePedidos = if_else(!is.na(FechaCreacionPedido), "EXPPEDID/EXPPEDI1", NA_character_),
    FuenteLote = if_else(!is.na(PedidoLote), "EXPCUALO", NA_character_),
    FuenteFacturas = if_else(!is.na(FacturasSYSCAFE),
                             "FCTFACN1/FCTFACNA/FCTFACC2/FCTFACCA", NA_character_),
    MetricaSacosCerrados70 = if_else(EstadoFinanciero == "CERRADO",
                                     coalesce(SacosFact, 0), 0),
    MetricaMargenContable = if_else(EstadoFinanciero == "CERRADO",
                                    coalesce(MargenContable, 0), 0),
    MetricaSacosAbiertos70 = if_else(EstadoFinanciero == "FACTURADO_ABIERTO",
                                     coalesce(SacosFact, 0), 0),
    MetricaMargenAbiertoEstimado = if_else(
      EstadoFinanciero == "FACTURADO_ABIERTO",
      coalesce(MargenAbiertoEstimado, 0), 0
    ),
    MetricaSacosProyectados70 = if_else(EstadoFinanciero == "PROYECTADO",
                                        coalesce(SacosProyectados, 0), 0),
    MetricaMargenProyectado = if_else(EstadoFinanciero == "PROYECTADO",
                                      coalesce(MargenProyectado, 0), 0),
    MetricaSacosFacturados70 = MetricaSacosCerrados70 + MetricaSacosAbiertos70,
    MetricaMargenEscenario = MetricaMargenContable +
      MetricaMargenAbiertoEstimado + MetricaMargenProyectado
  ) %>%
  group_by(CLPdcCod, CLPdcLin) %>%
  mutate(
    FilaIndicadorPedido = row_number() == which.max(
      EstadoFinanciero == "PROYECTADO"
    ) & !is.na(CLPdcLin),
    MetricaSacosPedido70 = if_else(FilaIndicadorPedido,
                                   coalesce(SacosPedido70Linea, 0), 0),
    MetricaSacosPendientes70 = if_else(FilaIndicadorPedido,
                                       coalesce(SacosPendientes70Linea, 0), 0)
  ) %>%
  ungroup() %>%
  mutate(
    IdRegistroCRM = row_number(),
    FechaCortePyG = ultimo_periodo_pyg,
    FechaInicioHistorico = fecha_inicio_analisis,
    FechaActualizacion = Sys.time()
  )

primer_lote <- !is.na(crm_maestro$CLLotCod) &
  !duplicated(ifelse(is.na(crm_maestro$CLLotCod),
                     paste0("SIN_", seq_len(nrow(crm_maestro))),
                     crm_maestro$CLLotCod))

crm_maestro <- crm_maestro %>%
  mutate(
    MetricaSacosLote70 = if_else(primer_lote, coalesce(SacosLote70, 0), 0),
    MetricaSacosProducidos70 = if_else(primer_lote,
                                       coalesce(SacosProducidos70, 0), 0),
    MetricaSacosDespachados70 = if_else(primer_lote,
                                        coalesce(SacosDespachados70, 0), 0)
  )

# VALIDACIONES FINALES ----
if (nrow(crm_maestro) != nrow(data_crm)) {
  stop("crm_maestro: los cruces de homologacion multiplicaron registros")
}

if (any(crm_maestro$EstadoFinanciero == "CERRADO" &
        crm_maestro$FuenteFinanciera != "CRMNALMARLOT", na.rm = TRUE)) {
  stop("crm_maestro: existen registros cerrados sin fuente CRMNALMARLOT")
}

control_maestro <- crm_maestro %>%
  filter(EstadoFinanciero == "CERRADO") %>%
  group_by(FecMes, LinNegCod) %>%
  summarise(Sacos = sum(MetricaSacosCerrados70, na.rm = TRUE),
            Margen = sum(MetricaMargenContable, na.rm = TRUE),
            .groups = "drop")

control_pyg <- margenes_pyg %>%
  group_by(FecMes, LinNegCod = if_else(
    .convertir_numero_margen(LINNEG) == 20000, 21000,
    .convertir_numero_margen(LINNEG)
  )) %>%
  summarise(Sacos = sum(.convertir_numero_margen(SACOS70), na.rm = TRUE),
            Margen = sum(.convertir_numero_margen(MARGEN), na.rm = TRUE),
            .groups = "drop")

diferencias <- full_join(control_pyg, control_maestro,
                         by = c("FecMes", "LinNegCod"),
                         suffix = c("PYG", "Maestro")) %>%
  mutate(
    DiferenciaSacos = coalesce(SacosMaestro, 0) - coalesce(SacosPYG, 0),
    DiferenciaMargen = coalesce(MargenMaestro, 0) - coalesce(MargenPYG, 0)
  )

if (any(abs(diferencias$DiferenciaSacos) > 0.01 |
        abs(diferencias$DiferenciaMargen) > 1, na.rm = TRUE)) {
  stop("crm_maestro: la ejecucion cerrada no concilia con CRMNALMARLOT")
}

cat("\nCRM MAESTRO | Registros:", nrow(crm_maestro),
    "| Sacos PyG:", sum(crm_maestro$MetricaSacosCerrados70),
    "| Sacos abiertos:", sum(crm_maestro$MetricaSacosAbiertos70),
    "| Sacos proyectados:", sum(crm_maestro$MetricaSacosProyectados70),
    "| Ultimo cierre:", as.character(ultimo_periodo_pyg), "\n")

# PERSISTENCIA ----
if (!dir.exists("APP/data")) dir.create("APP/data", recursive = TRUE)

# Validar el contrato compartido sin cambiar los dos objetos publicados.
sys.source("APP/misc/data_contract.R", envir = environment())
.crm_validar_maestro(crm_maestro, homologaciones)

save(crm_maestro, homologaciones, file = "APP/data/data.RData",
     compress = "xz")

tictoc::toc()
