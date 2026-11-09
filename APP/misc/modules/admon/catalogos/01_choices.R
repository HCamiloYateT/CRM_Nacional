# Choices Listas Desplegables ----
Choices <- function() {
  .admon_choices_leer()
}

# Choices anidadas ----
ChoicesAnidadas <- function(catalogo, padre, padre2 = "") {
  .admon_choices_valores(.admon_choices_leer(), catalogo, padre, padre2)
}
