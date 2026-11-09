library(dplyr)
source("APP/misc/compartido/00_presentacion.R")
source("APP/misc/compartido/01_analitica_comercial.R")
source("APP/misc/datos/02_fuentes_comerciales.R")
source("APP/misc/modules/gestion_comercial/oportunidades/01_modulo.R")
source("APP/misc/modules/consulta_individual/00_datos.R")
source("tests/fixtures/consulta_individual.R")
clientes <- .com_clientes(clientes_individual, as.Date("2026-11-09"), filtros_individual)
catalogo <- .in_catalogo(contactos_individual, clientes, etapas_individual, vinculos_individual,
  "ANA", FALSE, filtros_individual)
stopifnot(all(c("nit:900123456", "nit:102", "principal:101") %in% catalogo$Clave),
  !"nit:303" %in% catalogo$Clave, !"principal:303" %in% catalogo$Clave,
  catalogo$Tipo[catalogo$Clave == "nit:102"] == "CLIENTE",
  catalogo$NIT[catalogo$Clave == "nit:900123456"] == "900123456")
# Un contacto propio aún sin asesor no desaparece por no tener clasificación comercial.
sin_asesor <- contactos_individual[1, ]; sin_asesor$Asesor <- NA_character_
no_asignado <- .in_catalogo(sin_asesor, clientes, etapas_individual, vinculos_individual,
  "ANA", FALSE, filtros_individual)
stopifnot("nit:900123456" %in% no_asignado$Clave)
# Una entidad sin ventas sigue consultable; los nombres iguales no unen clientes.
contacto <- .in_elegida(catalogo, "nit:900123456")
stopifnot(nrow(.in_hechos(hechos_individual, contacto)) == 0L)
lead <- .in_elegida(catalogo, "nit:102")
stopifnot(nrow(.in_hechos(hechos_individual, lead)) == 1L)
principal <- .in_elegida(catalogo, "principal:101")
x <- .in_hechos(hechos_individual, principal)
stopifnot(nrow(x) == 3L, sum(x$SacosFact) == 9, !any(x$CLCliNit == 303))
# La entidad exacta usa el NIT vinculado; la cuenta consolida cada fila una sola vez.
periodo <- .in_periodo(x, as.Date(c("2026-10-01", "2026-11-09")))
stopifnot(nrow(periodo) == 2L, .in_resumen(periodo)$sacos == 5,
  .in_resumen(periodo)$pedidos == 2L, .in_resumen(periodo)$productos == 2L)
x$Margen[[1]] <- NA_real_
stopifnot(is.na(.in_resumen(x)$margen))
# No reutilizar la selección anterior tras cambiar permisos o vaciar el controlbar.
f <- filtros_individual; f$asesor <- character()
vacio <- .in_catalogo(contactos_individual, .com_clientes(clientes_individual, as.Date("2026-11-09"), f),
  etapas_individual, vinculos_individual, "ANA", FALSE, f)
stopifnot(nrow(vacio) == 0L, nrow(.in_elegida(vacio, "principal:101")) == 0L,
  nrow(.in_hechos(hechos_individual, .in_elegida(vacio, "principal:101"))) == 0L)
# El rol administrador amplía contactos, pero siempre respeta la selección de asesores.
f$asesor <- c("ANA", "LUIS")
admin <- .in_catalogo(contactos_individual, .com_clientes(clientes_individual, as.Date("2026-11-09"), f),
  etapas_individual, vinculos_individual, "ANA", TRUE, f)
stopifnot("nit:303" %in% admin$Clave, "nit:303" %in% admin$Clave)
# Vínculo ambiguo: no atribuir por NIT ni por coincidencia de nombre.
v <- rbind(vinculos_individual, transform(vinculos_individual, NitVinculado = 303))
a <- .in_catalogo(contactos_individual, clientes, etapas_individual, v, "ANA", FALSE, filtros_individual)
stopifnot(a$VinculoEnRevision[a$Codigo %in% "C2"],
  is.na(a$NitFacturacion[a$Codigo %in% "C2"]))
# Fallo de una fuente opcional no equivale a ausencia de información ni afecta las otras.
fallo <- .in_fuente(function(...) stop("fuera de servicio"), "CRMNALPOTENCIAL")
stopifnot(grepl("fuera de servicio", fallo$error),
  inherits(try(.in_detalle_crm(fallo, lead, "ConsumoEsperado"), silent = TRUE), "try-error"))
detalle <- .in_detalle_crm(list(datos = data.frame(CodContacto = c("C1", "C3"),
  Telefono = c("123", "456"), CampoPrivado = "no mostrar")), contacto, "Telefono")
stopifnot(identical(detalle$Telefono, "123"), identical(names(detalle), "Telefono"))
presupuesto <- .in_presupuesto(clientes, principal)
stopifnot(nrow(presupuesto) == 2L, sum(presupuesto$MetaMensualSacos) == 30)
stopifnot(.in_id(900123456) == "900123456")
source("APP/misc/modules/gestion_comercial/fidelidad_riesgo/00_modelos.R")
stopifnot(nrow(.fid_rfm(hechos_individual[0, ], as.Date("2026-11-09"))) == 0L)
negativos <- hechos_individual; negativos$SacosFact <- -1
stopifnot(nrow(.fid_rfm(negativos, as.Date("2026-11-09"))) == 0L)
# Las cuentas con ejecución y sin presupuesto también están en el catálogo.
cuentas <- .in_catalogo(contactos_individual[0, ], clientes[0, ], etapas_individual,
  vinculos_individual, "ANA", FALSE, filtros_individual,
  .gc_filtrar_dimensiones(hechos_individual, filtros_individual))
stopifnot("principal:101" %in% cuentas$Clave, !"principal:303" %in% cuentas$Clave)
cat("OK: identidad, alcance, permisos, NIT, sucursales, fechas, vacíos y fuentes opcionales.\n")

# El código CRM puede estar compartido: una cuenta hija siempre filtra por su propio NIT.
compartidos <- hechos_individual; compartidos$CodContacto <- "C2"
hija <- .in_elegida(catalogo, "nit:102")
stopifnot(nrow(.in_hechos(compartidos, hija)) == 1L,
  all(.in_hechos(compartidos, hija)$CLCliNit == 102),
  sum(catalogo$NIT == "102") == 1L)
# Una cuenta sin hijas no se repite como una segunda vista consolidada.
f$asesor <- "LUIS"
una <- .in_catalogo(contactos_individual, .com_clientes(clientes_individual, as.Date("2026-11-09"), f),
  etapas_individual, vinculos_individual, "LUIS", TRUE, f)
stopifnot(sum(una$NIT == "303") == 1L, !"principal:303" %in% una$Clave)
# Los nombres de las cuentas hijas se toman de su propio NIT, no del nombre del principal.
directorio <- data.frame(PerCod = c(101, 102), PerRazSoc = c("CASA MATRIZ", "SEDE NORTE"))
con_nombres <- .in_catalogo(contactos_individual, clientes, etapas_individual, vinculos_individual,
  "ANA", FALSE, filtros_individual, hechos_individual, directorio)
stopifnot(con_nombres$Nombre[con_nombres$Clave == "nit:102"] == "SEDE NORTE")
