# Constantes Embudo ----
## Generales ----
# Icono, color, etiqueta y modal por accion
.CONFIG_ACCIONES_EMBUDO <- list(Editar = list(etiqueta = "Editar", icono = "pen", color = "#0d6efd", modal = "ventana"),
                                Relacionamiento = list(etiqueta = "Gestión Comercial", icono = "comments", color = "#6f42c1", modal = "ventana"),
                                Promover = list(etiqueta = "Promover Etapa", icono = "arrow-up", color = "#198754", modal = "subventana4"),
                                Descartar = list(etiqueta = "Descartar", icono = "ban", color = "#C11007", modal = "subventana4"),
                                CrearOportunidad = list(etiqueta = "Nueva Oportunidad", icono = "handshake", color = "#fd7e14", modal = "subventana2"),
                                Reactivar = list(etiqueta = "Reactivar", icono = "rotate-left", color = "#6c757d", modal = NULL)
                                )
## Panel ----
# Columnas base (comunes a todas las etapas) de la tabla del panel
.COLUMNAS_BASE_PANEL <- c("CodContacto", "PerCod", "PerRazSoc", "Origen")
# Listado de etapas del embudo, en orden
ETAPAS_EMBUDO <- c("CONTACTO", "LEAD", "PROSPECTO", "CLIENTE", "DESCARTADO")
## Kanban ----
# Color por etapa para las columnas/tarjetas del kanban
COLOR_ETAPA_EMBUDO <- c("CONTACTO" = "#c8c8c8",
                        "LEAD" = "#0073A8",
                        "PROSPECTO" = "#C9A66B",
                        "CLIENTE" = "#b3001b",
                        "DESCARTADO" = "#1a1a1a"
                        )
# Delega el click de un boton de tarjeta del kanban al input Shiny AccionSeleccionada del namespace
JS_KANBAN_EMBUDO <- "
$(document).on('click', '[data-kanban-action]', function(e) {
  e.stopPropagation();
  var $btn = $(this);
  var $board = $btn.closest('[data-kanban-ns]');
  if ($board.length === 0) return;
  var ns_prefix = $board.data('kanban-ns');
  Shiny.setInputValue(
    ns_prefix + 'AccionSeleccionada',
    { codigo: $btn.data('cod-contacto'), accion: $btn.data('kanban-action'), ts: Date.now() },
    { priority: 'event' }
  );
});
"
## Acciones por Etapa ----
.ACCIONES_ETAPA_EMBUDO <- list(
  CONTACTO = c("Editar", "Relacionamiento", "Promover", "Descartar", "CrearOportunidad"),
  LEAD = c("Editar", "Relacionamiento", "Promover", "Descartar", "CrearOportunidad"),
  PROSPECTO = c("Editar", "Relacionamiento", "Promover", "Descartar", "CrearOportunidad"),
  CLIENTE = c("Editar", "Relacionamiento", "CrearOportunidad", "Descartar"),
  DESCARTADO = c("Editar", "Relacionamiento", "Reactivar")
)
## Descartados ----
.TOP_N_RAZONES_DESCARTE <- 10
## Embudo ----
.STOPWORDS_ES <- c(
  "el", "la", "los", "las", "un", "una", "unos", "unas",
  "de", "del", "al", "a", "en", "y", "o", "que", "no",
  "se", "por", "con", "para", "es", "su", "sus", "lo",
  "le", "les", "mas", "más", "muy", "sin", "sobre",
  "entre", "este", "esta", "esto", "estos", "estas", "ya",
  "fue", "ha", "han", "como", "porque", "cuando", "donde",
  "pero", "si", "ni", "nos", "va", "hay", "era", "eran",
  "son", "ser", "estan", "están", "asi", "así"
)
