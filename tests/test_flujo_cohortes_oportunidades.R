# Ejecutar desde la raíz del repo; sin conexiones ni escrituras de negocio.
library(dplyr)
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/modules/gestion_comercial/pendientes_gestion/01_modulo.R")
source("APP/misc/modules/cohortes_clientes/compartido/00_cohortes.R")
source("APP/misc/modules/gestion_comercial/oportunidades/01_modulo.R")

# Dos facturas del mismo lote no multiplican asignación, producción o despacho.
maestro <- tibble::tibble(CLPdcCod = c(1, 1, 1, 1, 2, 3), CLPdcLin = 1,
  CLLotCod = c(10, 10, 11, NA, NA, 30),
  EstadoFinanciero = c("CERRADO", "CERRADO", "CERRADO", "PROYECTADO", "PENDIENTE", "PENDIENTE"),
  SacosPedido70Linea = c(100, 100, 100, 100, 60, 20), SacosLote70 = c(40, 40, 30, NA, NA, NA),
  FacturadosOperativos70 = c(5, 5, 5, 0, 0, 0),
  SacosProducidos70 = c(20, 20, 30, NA, NA, NA), SacosDespachados70 = c(10, 10, 20, NA, NA, NA),
  SacosFact = c(2, 3, 5, NA, NA, NA), Margen = c(20, 30, 50, NA, NA, NA),
  FecFact = as.Date(c("2026-01-02", "2026-01-03", "2026-01-04", NA, NA, NA)),
  CliNitPpal = c(100, 100, 100, 100, 200, 300), PerRazSoc = "Cliente",
  PdcFecEst = as.Date(c(rep("2026-01-10", 4), "2027-01-01", NA)),
  FechaCreacionPedido = as.Date("2026-01-01"), EstadoPedido = "A")
flujo <- .pen_preparar(maestro, hoy = as.Date("2026-11-08"))
stopifnot(nrow(flujo$pedidos) == 3L, nrow(flujo$lotes) == 3L,
  flujo$pedidos$Asignados[flujo$pedidos$Pedido == "1"] == 70,
  flujo$pedidos$SinAsignar[flujo$pedidos$Pedido == "1"] == 30,
  flujo$pedidos$Pendientes[flujo$pedidos$Pedido == "1"] == 90,
  is.na(flujo$pedidos$Asignados[flujo$pedidos$Pedido == "3"]),
  sum(flujo$detalle$Cantidad[flujo$detalle$Etapa == "Por producir"]) == 20,
  any(flujo$detalle$Etapa == "Futuros" & flujo$detalle$Pedido == "2"))
# Unidades contradictorias/ausentes son revisables; nunca se convierten en saldo cero.
stopifnot(any(flujo$detalle$Etapa == "Revisar cantidades" & flujo$detalle$Lote == "30", na.rm = TRUE))
stopifnot(nrow(.pen_preparar(maestro[0, ])$detalle) == 0L)

# Un cliente no comprador permanece en el denominador y las cohortes jóvenes no reciben ceros futuros.
hechos <- tibble::tibble(CliNitPpal = c(1, 1, 2, 3), LinNegCod = 21000,
  CodContacto = c("A", "A", "B", "C"), FecFact = as.Date(c("2026-01-05", "2026-03-05", "2026-02-05", "2026-03-06")),
  EstadoFinanciero = c("CERRADO", "FACTURADO_ABIERTO", "CERRADO", "FACTURADO_ABIERTO"),
  SacosFact = c(10, 2, 5, 3), Margen = c(100, 20, 50, 30), Categoria = "Café", Producto = "Molido",
  FechaCortePyG = as.Date("2026-02-01"))
panel <- .coh_panel(hechos, as.Date("2026-01-01"), as.Date("2026-03-01"))
r <- .coh_resumen(panel, "retencion")
enero <- r[r$Cohorte == as.Date("2026-01-01"), ]
stopifnot(nrow(enero) == 3L, identical(enero$Activos, c(1L, 0L, 1L)), all(enero$Base == 1),
  panel$Margen[panel$CliNitPpal == 1 & panel$Mes == as.Date("2026-02-01")] == 0,
  is.na(panel$Margen[panel$CliNitPpal == 1 & panel$Mes == as.Date("2026-03-01")]))
matriz <- .coh_matriz(r)
stopifnot(is.na(matriz$M1[matriz$Cohorte == "2026-03-01"]))
recorte <- .coh_panel(hechos, as.Date("2026-02-01"), as.Date("2026-03-01"))
stopifnot(!1 %in% recorte$CliNitPpal, nrow(.coh_panel(hechos[0, ], Sys.Date(), Sys.Date())) == 0L)

# El tablero usa el esquema de creación vigente, conserva permisos y excluye ventas ajenas al producto.
ops <- tibble::tibble(IdOportunidad = c("OP1", "OP2"), CodContacto = c("A", "B"), UsuarioCrea = c("ANA", "LUIS"),
  LinNegCod = 21000, Categoria = "Café", Producto = "Molido", FechaHoraCrea = as.POSIXct("2026-01-01"),
  FechaCumpOP = as.Date("2026-02-01"), Sacos = 10, Margen = 2, FrecuenciaDias = 30, Descartado = "NO")
contactos <- data.frame(CodContacto = c("A", "B"), PerRazSoc = c("Uno", "Dos"), PerCod = c(1, 2),
  UsuarioCrea = c("ANA", "LUIS"))
op <- .op_preparar(ops, contactos, hechos, "ANA")
stopifnot(nrow(op) == 1L, op$Objetivo70 == 10, op$MargenObjetivo == 1400,
  op$SacosEjecutados == 10, op$MargenEjecutado == 100, op$SacosMes == 10)
stopifnot(nrow(.op_preparar(ops, contactos, hechos, "ADMIN", TRUE)) == 2L,
  nrow(.op_preparar(ops[0, ], contactos, hechos, "ANA")) == 0L)
cat("OK: granos de pedidos/lotes, proyecciones excluidas, unidades desconocidas, cohortes censuradas y permisos.\n")
# Una fila histórica sin ID sigue siendo visible, pero nunca habilita descarte por un ID inventado.
antiguas <- tibble::tibble(Usuario = "ANA", PerRazSoc = "Uno", LineaNegocio = "A LA MEDIDA",
  FechaHoraCrea = as.POSIXct("2026-01-01"), FechaCumpOP = as.Date("2026-02-01"),
  SacosOP = 10, MargenOP = 2, FrecuenciaDias = 30, Descartada = FALSE, Categoria = "Café", Producto = "Molido")
antigua <- .op_preparar(antiguas, contactos, hechos, "ANA")
stopifnot(nrow(antigua) == 1L, !antigua$Editable, antigua$Cliente == "Uno", antigua$Objetivo70 == 10)
# Un pedido inactivo no genera trabajo productivo ni pendiente de asignación.
inactivo <- maestro
inactivo$EstadoPedido <- "C"
f_inactivo <- .pen_preparar(inactivo)
stopifnot(!any(f_inactivo$detalle$Etapa %in% c("Por producir", "Por despachar", "Por facturar", "Sin asignar")))
# La transición reactivada se identifica tras un mes sin compra.
stopifnot(panel$EstadoMes[panel$CliNitPpal == 1 & panel$Mes == as.Date("2026-03-01")] == "Compra reactivada")

# Persistencia aislada: conserva la frecuencia y falla antes de escribir si falta el campo o es inválida.
local({
  entorno <- new.env(parent = globalenv())
  escrituras <- list()
  tiene_frecuencia <- TRUE
  entorno$.generar_id_oportunidad <- function() "PRUEBA"
  entorno$`::` <- function(pkg, nombre) {
    p <- as.character(substitute(pkg)); n <- as.character(substitute(nombre))
    if (p == "racafeBD" && n == "CargarDatos") return(function(...) {
      if (tiene_frecuencia) data.frame(FrecuenciaDias = numeric()) else data.frame()
    })
    if (p == "racafeBD" && n == "AgregarDatos") return(function(fila, tabla) {
      escrituras[[length(escrituras) + 1L]] <<- fila
    })
    getExportedValue(p, n)
  }
  codigo <- parse("APP/misc/modules/acciones/00_Funciones.R")
  asignacion <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name("registrar_oportunidad")), as.list(codigo))[[1]]
  eval(asignacion, entorno)
  crear <- function(frecuencia) entorno$registrar_oportunidad("A", 21000, "Café", "Molido",
    as.Date("2027-01-01"), 10, 2, etapa = "CLIENTE", usr = "ANA", frecuencia = frecuencia)
  crear(30)
  stopifnot(length(escrituras) == 1L, escrituras[[1]]$FrecuenciaDias == 30)
  stopifnot(inherits(try(crear(0), silent = TRUE), "try-error"), length(escrituras) == 1L)
  tiene_frecuencia <- FALSE
  stopifnot(inherits(try(crear(30), silent = TRUE), "try-error"), length(escrituras) == 1L)
})
cat("OK: frecuencia persistida y validada sin escrituras de negocio reales.\n")

# Una factura antigua, fuera del PyG visible, sí reduce el saldo operativo.
con_historia <- maestro
con_historia$FacturadosOperativos70[con_historia$CLLotCod %in% 10] <- 10
historia <- .pen_preparar(con_historia)
stopifnot(historia$pedidos$Pendientes[historia$pedidos$Pedido == "1"] == 85,
  sum(historia$detalle$Cantidad[historia$detalle$Etapa == "Por facturar"]) == 15)

# La consulta operativa conserva facturas antiguas y el periodo financiero continúa recortado.
local({
  entorno <- new.env(parent = globalenv())
  entorno$fecha_inicio_analisis <- as.Date("2026-01-01")
  entorno$`::` <- function(pkg, nombre) {
    p <- as.character(substitute(pkg)); n <- as.character(substitute(nombre))
    if (p == "racafeBD" && n == "ConsultaSistema") return(function(sistema, query) {
      if (grepl("FCTFACN1", query, fixed = TRUE)) {
        data.frame(CLLotCod = 10, FecFact = as.Date(c("2020-01-01", "2026-01-01")), SacosFact = c(5, 10))
      } else data.frame(CLLotCod = numeric(), FecFact = as.Date(character()), SacosFact = numeric())
    })
    getExportedValue(p, n)
  }
  asignacion <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name(".cargar_fact")), as.list(parse("DataPrep.R")))[[1]]
  eval(asignacion, entorno)
  financiero <- entorno$.cargar_fact()
  operativo <- entorno$.cargar_fact(as.Date("1900-01-01"))
  stopifnot(nrow(financiero) == 1L, sum(financiero$SacosFact) == 10,
    nrow(operativo) == 2L, sum(operativo$SacosFact) == 15)
})
cat("OK: saldos operativos históricos separados del periodo financiero.\n")
frecuencia <- .coh_resumen(panel, "frecuencia")
f_enero <- frecuencia[frecuencia$Cohorte == as.Date("2026-01-01"), ]
stopifnot(isTRUE(all.equal(f_enero$Valor, c(1, 1 / 2, 2 / 3))))
# Una devolución o ajuste en un mes sin compra positiva conserva su margen negativo.
ajuste <- hechos[1, ]
ajuste$FecFact <- as.Date("2026-02-06")
ajuste$SacosFact <- -1
ajuste$Margen <- -10
con_ajuste <- .coh_panel(bind_rows(hechos, ajuste), as.Date("2026-01-01"), as.Date("2026-03-01"))
stopifnot(con_ajuste$Margen[con_ajuste$CliNitPpal == 1 & con_ajuste$Mes == as.Date("2026-02-01")] == -10)
# La orden usa el usuario de sesión y no escribe con lote o texto inválidos.
local({
  guardados <- list()
  guardar <- function(fila, tabla) guardados[[length(guardados) + 1L]] <<- fila
  .pen_guardar_oc("10", " OC-1 ", "ANA", guardar)
  stopifnot(length(guardados) == 1L, guardados[[1]]$UsuarioCrea == "ANA", guardados[[1]]$OrdenCompra == "OC-1")
  stopifnot(inherits(try(.pen_guardar_oc("10 OR 1=1", "OC", "ANA", guardar), silent = TRUE), "try-error"),
    inherits(try(.pen_guardar_oc("10", " ", "ANA", guardar), silent = TRUE), "try-error"), length(guardados) == 1L)
})
