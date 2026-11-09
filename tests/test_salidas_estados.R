# Las salidas de cálculo no deben quedar vacías al validar o fallar ----
library(shiny)
library(dplyr)
source("APP/misc/compartido/04_interaccion.R")
error_texto <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
testServer(function(input, output, session) {
  output$valor <- .crm_render_ui({
    if (identical(input$caso, "vacio")) req(FALSE)
    if (identical(input$caso, "error")) stop("Causa sintética identificable")
    tags$span("Resultado válido")
  })
}, {
  session$setInputs(caso = "valido")
  stopifnot(grepl("Resultado válido", output$valor$html))
  session$setInputs(caso = "vacio")
  stopifnot(grepl("No hay datos", error_texto(output$valor)))
  session$setInputs(caso = "error")
  stopifnot(grepl("No se pudo completar el cálculo", error_texto(output$valor)))
  session$setInputs(caso = "valido")
  stopifnot(grepl("Resultado válido", output$valor$html))
})
# La tipografía conserva títulos, unidades, rangos y tamaños grandes deliberados.
p <- plotly::plotly_build(plotly::plot_ly(x = 1:3, y = c(10, 20, 30), type = "bar") %>%
  plotly::layout(title = "Margen", xaxis = list(title = "Mes", range = c(0, 4)),
    yaxis = list(title = "COP"), annotations = list(list(text = "Nota", x = 1, y = 10, font = list(size = 8)))))
p <- .crm_tipografia_plotly(p)
stopifnot(p$x$layout$title$text == "Margen", p$x$layout$xaxis$title$text == "Mes",
  p$x$layout$yaxis$title$text == "COP", identical(p$x$layout$xaxis$range, c(0, 4)),
  p$x$layout$font$size == 12, p$x$layout$xaxis$tickfont$size == 11,
  p$x$layout$annotations[[1]]$font$size == 11)
cat("OK: salida válida, vacío explícito, fallo identificado, recuperación y tipografía sin cambiar datos.\n")
