# NULL omite cambios de opciones; un vector vacío conserva la intención de vaciar la lista ----
source("APP/misc/compartido/05_componentes_forms.R")
stopifnot(is.null(.crm_choices(NULL)), identical(.crm_choices(character()), character()),
  identical(.crm_choices(integer()), integer()), identical(.crm_choices(list()), list()))
stopifnot(!length(.crm_choices(c("NINGUNO", "NINGUNA", "", NA_character_))))
opciones <- .crm_choices(c("CONVENCIONALES", "A LA MEDIDA"))
stopifnot(identical(unname(opciones), c("CONVENCIONALES", "A LA MEDIDA")),
  names(opciones)[[1]] == "Coproductos")
grupos <- .crm_choices(list(Lineas = c(Coproductos = "CONVENCIONALES"), Pendientes = NULL))
stopifnot(grupos$Lineas[[1]] == "CONVENCIONALES", is.null(grupos$Pendientes))
shiny::testServer(function(input, output, session) {}, {
  enviados <- list()
  session$sendInputMessage <- function(inputId, message) {
    enviados[[length(enviados) + 1L]] <<- list(id = inputId, mensaje = message)
  }
  # Reproduce la llamada del observador de usuario sin reenviar el catálogo.
  .crm_update_select(session, "asesor", selected = "ANA")
  stopifnot(length(enviados) == 1L, enviados[[1]]$id == "asesor")
  .crm_update_select(session, "asesor", choices = character(), selected = character())
  stopifnot(length(enviados) == 2L)
})
cat("OK: NULL, listas vacías, grupos y actualización real de selector Shiny sin excepción.\n")
