library(dplyr)
source("APP/misc/01_analitica_comercial.R")
source("APP/misc/modules/productos/00_funciones.R")
source("APP/misc/modules/productos/02_market_basket.R")

# Conjuntos conocidos: el denominador incluye las cestas de un solo ítem.
m <- data.frame(Cesta = c(1,1,2,2,3,4,4,5), Item = c(1,2,1,2,1,2,3,3))
modelo <- .mba_itemsets(m, 1:5, soporte = 0.2, minimo = 1, longitud = 3)
r <- .mba_reglas(modelo, confianza = 0, lift = 0)
ab <- r[r$Antecedente == "1" & r$Consecuente == "2", ]
stopifnot(nrow(ab) == 1L, ab$CestasAmbos == 2L, ab$Soporte == 0.4,
  isTRUE(all.equal(ab$Confianza, 2 / 3)), isTRUE(all.equal(ab$Lift, 10 / 9)),
  isTRUE(all.equal(ab$Leverage, 0.04)), isTRUE(all.equal(ab$Conviccion, 1.2)), ab$Jaccard == 0.5)
stopifnot(all(r$Qvalor[!is.na(r$Qvalor)] >= r$Pvalor[!is.na(r$Pvalor)]),
  !"1,2,3" %in% names(modelo$conjuntos))
# Repetir líneas de un mismo producto no crea compras ni cestas adicionales.
repetido <- .mba_itemsets(rbind(m,m[1,]),1:5,0.2,1,3)
stopifnot(repetido$conjuntos[["1,2"]]$cuenta == 2L)
# Convicción: infinito e indeterminado se mantienen distintos.
stopifnot(is.infinite(.mba_metricas(5,2,3,2)[["Conviccion"]]),
  is.na(.mba_metricas(5,5,5,5)[["Conviccion"]]))
# Apriori completo contra enumeración exhaustiva, hasta longitud cuatro.
set.seed(42)
for (iteracion in 1:12) {
  cestas <- lapply(1:8,function(i) sample(1:5,sample(1:5,1)))
  membresia <- bind_rows(lapply(seq_along(cestas),function(i)data.frame(Cesta=i,Item=cestas[[i]])))
  ajustado <- .mba_itemsets(membresia,1:8,0.25,2,4)
  esperados <- list()
  for (k in 1:4) for (items in utils::combn(1:5,k,simplify=FALSE)) {
    cuenta <- sum(vapply(cestas,function(cesta)all(items %in% cesta),logical(1)))
    if(cuenta>=2)esperados[[.mba_clave(items)]] <- cuenta
  }
  stopifnot(setequal(names(ajustado$conjuntos),names(esperados)))
  for(clave in names(esperados))stopifnot(ajustado$conjuntos[[clave]]$cuenta==esperados[[clave]])
}
# Los límites producen error, nunca un resultado parcial silencioso.
stopifnot(inherits(try(.mba_itemsets(m,1:5,0.1,1,3,limite=1),silent=TRUE),"try-error"))
validacion <- .mba_evaluar(ab,m,4:5)
stopifnot(validacion$SoporteValidacion == 0, validacion$ConfianzaValidacion %in% c(0,NA_real_))

# Hechos netos: cantidades de lote/proyecciones no intervienen en ejecución.
hechos <- data.frame(EstadoFinanciero=c("CERRADO","CERRADO","FACTURADO_ABIERTO","PROYECTADO"),
  FecFact=as.Date(c("2026-01-01","2026-01-02","2026-02-01",NA)),
  SacosFact=c(1,9,-1,NA),Margen=c(70,315,-20,NA),Categoria="Café",Producto=c("A","B","A","C"),
  CliNitPpal=123,CLCliNit=123,CLPdcCod=c(1,1,2,3),CLLotCod=c(10,11,12,13),
  CLLinNegNo="A LA MEDIDA",PerRazSoc="Cliente de prueba",SacLote=999)
x <- .prod_preparar(hechos,as.Date(c("2026-01-01","2026-02-28")))
indicadores <- .prod_indicadores(x)
stopifnot(nrow(x)==3L,indicadores$sacos==9,indicadores$margen==365,indicadores$cerrado==385,
  indicadores$abierto== -20,nrow(.prod_preparar(hechos,as.Date(c("2026-01-01","2026-02-28")),TRUE))==2L)
resumen <- .prod_resumen(x)
stopifnot(sum(resumen$Sacos)==9, sum(resumen$Margen)==365)
base <- .mba_cestas(x,"pedido")
stopifnot(nrow(base$cestas)==1L,base$cestas$Items==2L)
# El foco selecciona cestas completas: B permanece como acompañante de A.
foco <- .mba_cestas(x,"pedido",filtros=list(categoria="Café",producto="A"))
stopifnot(nrow(foco$membresia)==2L)
# Una devolución completa anula ese ítem en la cesta, sin borrar el producto acompañante.
ajuste <- hechos[1,];ajuste$SacosFact <- -1;ajuste$Margen <- -70
netos <- .mba_cestas(.prod_preparar(bind_rows(hechos,ajuste),as.Date(c("2026-01-01","2026-02-28"))),"pedido")
stopifnot(nrow(netos$membresia)==1L,netos$mapa$Etiqueta[[1]]=="A LA MEDIDA › Café › B")
# Vacíos y pedidos desconocidos no se combinan en una cesta artificial.
sinpedido <- hechos[1:2,];sinpedido$CLPdcCod <- NA_real_
stopifnot(nrow(.mba_cestas(.prod_preparar(sinpedido,as.Date(c("2026-01-01","2026-02-28"))),"pedido")$cestas)==0L)
cat("OK: Apriori exacto, métricas, límites, denominadores, foco, devoluciones y ejecución neta.\n")
# Separación temporal por fechas completas, no por filas de una misma cesta.
base_t <- list(cestas = data.frame(Cesta = 1:5, Fecha = as.Date("2026-01-01") + 0:4, NIT = 1:5,
  Items = c(2,2,1,2,1)), membresia = m, mapa = data.frame(Item = 1:3, Etiqueta = c("A","B","C")),
  movimientos = data.frame(NIT = as.character(c(1,1,2,2,3,4,4,5)), Cliente = "Prueba", Item = m$Item),
  cobertura = data.frame(Cestas = 5L))
temporal <- .mba_analizar(base_t, 0.1, 1, 3, 0, 0, validacion = TRUE)
stopifnot(temporal$diagnostico$CestasModelo == 3L, temporal$diagnostico$CestasValidacion == 2L,
  temporal$diagnostico$CorteModelo == as.Date("2026-01-03"), "ConfianzaValidacion" %in% names(temporal$reglas))
# Una hipótesis no propone productos ya observados en el cliente ni se presenta como venta segura.
recomendaciones <- .mba_recomendaciones(r,base_t$movimientos,base_t$mapa)
if(nrow(recomendaciones))for(i in seq_len(nrow(recomendaciones))) {
  comprados <- base_t$mapa$Etiqueta[base_t$mapa$Item %in% base_t$movimientos$Item[
    base_t$movimientos$NIT == recomendaciones$NIT[[i]]]]
  stopifnot(!recomendaciones$Falta[[i]] %in% comprados)
}
stopifnot(identical(.prod_identificador(c(1000000000, NA_real_)), c("1000000000", NA_character_)))
