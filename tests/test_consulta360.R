library(dplyr)
root <- normalizePath(".")
source("APP/misc/compartido/00_presentacion.R")
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/datos/02_fuentes_comerciales.R")
source("APP/misc/modules/gestion_comercial/oportunidades/01_modulo.R")
source("APP/misc/modules/gestion_comercial/fidelidad_riesgo/00_modelos.R")
source("APP/misc/modules/productos/compartido/00_funciones.R")
source("APP/misc/modules/consulta_individual/00_datos.R")
source("APP/misc/modules/consulta_individual/02_perfil_analitico.R")
source("tests/fixtures/consulta_individual.R")
source("tests/fixtures/consulta360.R")
corte <- as.Date("2026-11-09")
clientes <- .com_clientes(clientes_individual, corte, filtros_individual)
metas <- .com_clientes(clientes_individual, corte, filtros_individual, anual = TRUE)
hechos <- .gc_filtrar_dimensiones(hechos360, filtros_individual)
catalogo <- .in_catalogo(contactos_individual, clientes, etapas_individual, vinculos_individual,
  "ANA", TRUE, filtros_individual, hechos, directorio360)
hija <- .in_elegida(catalogo, "nit:102")
padre <- .in_elegida(catalogo, "principal:101")
mapa <- data.frame(PerCod = c(101, 102), CliNitPpal = 101)
familia <- .in_familia(hija, catalogo, mapa, directorio360)
stopifnot(familia$principal == "101", identical(familia$hijos, "102"), grepl("Cuenta hija", familia$relacion))
stopifnot(grepl("1 cuenta hija", .in_familia(padre, catalogo, mapa, directorio360)$relacion))
stopifnot(grepl("Con presupuesto 2026", .in_estado_presupuesto(metas, hija, corte)))
anterior <- clientes_individual; anterior$FecProceso <- as.Date("2025-01-01")
sin_vigencia <- .com_clientes(anterior, corte, filtros_individual, anual = TRUE)
stopifnot(grepl("Sin presupuesto registrado", .in_estado_presupuesto(sin_vigencia, hija, corte)))
contradictorio <- clientes_individual
contradictorio <- rbind(contradictorio, transform(contradictorio[2, ], SSPpto = 999))
revision <- .com_clientes(contradictorio, corte, filtros_individual, anual = TRUE)
stopifnot(grepl("por revisar", .in_estado_presupuesto(revision, hija, corte)))
historia <- .in_facturacion_historica(.in_hechos(hechos, hija), corte)
stopifnot(min(historia$FecFact) < as.Date("2026-01-01"), !any(historia$FecFact > corte))
stopifnot(nrow(.in_historico_anual(historia)) == 4L)
datos <- .in_datos_cliente(hija, familia, directorio360, hechos)
stopifnot(datos$Telefono == "222222", datos$Ciudad == "Medellín", datos$Registro == as.Date("2015-06-01"))
# Una cuenta hija sin directorio propio no hereda el teléfono del principal que lleva el ETL.
hechos$CliTel <- "TELEFONO DEL PADRE"
stopifnot(is.na(.in_datos_cliente(hija, familia, directorio360[1, ], hechos)$Telefono))
perfil <- .in_antiguedad(hija, datos, contactos_individual, historia, corte)
stopifnot("2015-06-01" %in% perfil$Informacion, grepl("11 años", .in_edad(datos$Registro, corte)))
# No copiar el RFM del grupo al NIT hijo.
poblacion <- .in_poblacion_modelo(hechos, "nit")
rfm <- .fid_rfm(poblacion, corte)
propio <- rfm[rfm$CliNitPpal == "102", ]
stopifnot(nrow(propio) == 1L, propio$Sacos == sum(historia$SacosFact), propio$Compras == 4L)
modelo <- .fid_analizar_modelos(poblacion, corte)
stopifnot(is.finite(modelo$clientes$CLVMargen[modelo$clientes$CliNitPpal == "102"]))
# Las propuestas pertenecen al NIT consultado, no a otros hijos ni a sus comparables.
base <- .in_venta_base(hechos360, filtros_individual, corte, "nit")
propuestas <- .in_propuestas(base, hija)
stopifnot(nrow(propuestas) > 0, all(propuestas$NIT == "102"), all(propuestas$Lift > 1),
  all(propuestas$CestasEvidencia >= 2), any(grepl("C$", propuestas$Falta)), !any(grepl("B$", propuestas$Falta)))
# La cronología agrega conversiones antiguas y evita duplicarlas si ya tienen transición el mismo día.
fuentes <- list(etapas = list(datos = data.frame(CodContacto = "C2", FechaHora = as.Date("2024-01-01"),
  EtapaAnterior = "CONTACTO", EtapaNueva = "LEAD", Usuario = "ANA")),
  conversion_lead = list(datos = data.frame(CodContacto = "C2", FechaConversion = as.Date("2024-01-01"))),
  conversion_cliente = list(datos = data.frame(CodContacto = "C2", FechaConversion = as.Date("2025-01-01"))))
cronologia <- .in_cronologia(hija, contactos_individual, fuentes, corte)
stopifnot(sum(as.Date(cronologia$Fecha, tz = "America/Bogota") == as.Date("2024-01-01")) == 1L,
  any(cronologia$Evento == "Conversión a cliente"), all(cronologia$EntidadCRM == "C2"))
# Un seguimiento del padre solo se vincula a través del catálogo autorizado y queda identificado.
cat_padre <- catalogo; cat_padre$Codigo[cat_padre$Clave == "nit:101"] <- "PADRE"
sin_ficha <- hija; sin_ficha$Codigo <- NA_character_
ctx <- .in_contexto_crm(sin_ficha, cat_padre)
stopifnot(ctx$Codigo == "PADRE", ctx$OrigenCRM == "Cuenta padre")
ctx_vacio <- .in_contexto_crm(sin_ficha, cat_padre[cat_padre$Clave != "nit:101", ])
stopifnot(is.na(ctx_vacio$Codigo))
cat("OK: presupuesto, familia, contacto/geografía, antigüedad, historia, RFM/CLV, conversión y venta cruzada.\n")
# Las propuestas avisan cuando ya existe una oportunidad para el mismo producto/categoría/línea.
opor <- list(datos = data.frame(CodContacto = "C2", IdOportunidad = "OP-1", Categoria = "Café", Producto = "C",
  LinNegCod = 21000, Descartado = "NO"))
seguimiento <- .in_cruce_oportunidades(propuestas, base, opor, hija)
stopifnot(any(grepl("Ya registrada", seguimiento)))
# Dos transiciones reales del mismo día conservan su hora y no se colapsan.
horas <- fuentes
horas$etapas$datos <- data.frame(CodContacto = "C2", EtapaAnterior = "CONTACTO", EtapaNueva = "LEAD", Usuario = "ANA",
  FechaHora = as.POSIXct(c("2024-01-01 08:00:00", "2024-01-01 15:00:00"), tz = "America/Bogota"))
detalle <- .in_cronologia(hija, contactos_individual, horas, corte)
stopifnot(sum(detalle$Evento == "Cambio de etapa") == 2L)
# Fechas desconocidas o inválidas no se inventan ni impiden consultar los otros eventos.
stopifnot(is.na(.in_instante(NA)), is.na(.in_fecha("sin fecha")),
  !is.na(.in_instante(c("sin fecha", "2024-01-01 10:00:00"))[[2]]))
