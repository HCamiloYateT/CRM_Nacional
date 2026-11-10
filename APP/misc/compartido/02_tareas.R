# Reglas compartidas entre tablero, ficha y dropdown de tareas ----
.tareas_visibles_usuario <- function(datos, usuario = NULL, es_admin = FALSE) {
  if (isTRUE(es_admin) || is.null(usuario)) return(datos)
  if (length(usuario) != 1L || is.na(usuario) || !nzchar(trimws(usuario))) return(datos[0, , drop = FALSE])
  responsable <- vapply(as.character(datos$Responsable), function(x) {
    if (is.na(x)) return(FALSE)
    usuario %in% trimws(strsplit(x, "|", fixed = TRUE)[[1]])
  }, logical(1))
  datos[(!is.na(datos$Usuario) & as.character(datos$Usuario) == usuario) | responsable, , drop = FALSE]
}

.tareas_partir <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) {
    return(character(0))
  }
  trimws(strsplit(as.character(x[[1]]), "|", fixed = TRUE)[[1]])
}

.tareas_actividades <- function(registro) {
  textos <- .tareas_partir(registro$Actividades)
  estados <- .tareas_partir(registro$Cumplidas)
  if (length(estados) < length(textos)) {
    estados <- c(estados, rep("0", length(textos) - length(estados)))
  }
  if (length(estados) > length(textos)) estados <- estados[seq_along(textos)]
  list(textos = textos, cumplidas = estados == "1")
}

.tareas_prioridad <- function(x) {
  x <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    x == "URGENTE" ~ "danger",
    x == "IMPORTANTE" ~ "warning",
    TRUE ~ "neutral"
  )
}

