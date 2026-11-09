# Ventas: entrada propia y motor longitudinal compartido ----
CohorteVentasUI <- function(id) CohorteClientesUI(id, "ventas")
CohorteVentas <- function(id, datos) CohorteClientes(id, datos, "ventas")
