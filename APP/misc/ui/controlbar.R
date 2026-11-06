# Controlbar ----
controlbar <- bs4DashControlbar(id = "controlbar", skin = "light", pinned = NULL, overlay = FALSE, width = "450px",
                                controlbarMenu(id = "controlbarMenu", type = "tabs",
                                               controlbarItem("Filtros", FiltrosUI("Filtros")),
                                               controlbarItem("Cotizador", CotizacionUI("Cotizador")),
                                               controlbarItem("Crear Tareas/Notas", CrearNotasUI("CrearNota")),
                                               controlbarItem("Crear Contacto", CrearContactoUI("AccionCrear")),
                                               controlbarItem("Competencia", RegistrarCompetenciaUI("RegistrarCompetencia"))))