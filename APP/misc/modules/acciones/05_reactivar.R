# Reactivar.
# Modulo Principal ----
ReactivarUI <- function(id) {
  ns <- NS(id)
  tagList()
}
Reactivar <- function(id, usuario, codigo_contacto, disparador = codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    .confirmacion <- .accion_confirmacion(input, session, codigo_contacto, usuario)
    ns <- session$ns
    ret <- reactiveVal(0)
    ultimo_disparado <- reactiveVal(NULL)
    
    # Abrir aviso automatico ----
    observeEvent(disparador(), {
      req(codigo_contacto())
      
      clave_disparo <- paste(codigo_contacto(), disparador(), sep = "|")
      if (identical(ultimo_disparado(), clave_disparo)) return(invisible(NULL))
      
      ultimo_disparado(clave_disparo)
      
      destino <- .obtener_etapa_pre_descarte(codigo_contacto())
      
      .confirmacion$abrir(
        ns = ns,
        titulo = "Confirmar reactivación",
        texto = paste0("Este registro volverá a la etapa: ", destino, ". ¿Desea continuar?"),
        id_cancelar = "REA_Cancelar",
        id_confirmar = "REA_Confirmar",
        label_confirmar = "Reactivar",
        icono_confirmar = "rotate-left"
      )
    })
    
    # Cancelar ----
    observeEvent(input$REA_Cancelar, removeModal())
    
    # Confirmar ----
    observeEvent(input$REA_Confirmar, {
      if (!.confirmacion$aceptar("REA_Confirmar")) return(invisible(NULL))
      tryCatch({
        reactivar_contacto(codigo_contacto(), usuario())
        removeModal()
        racafeForms::racafeNotify(duration = 4, type = "success", title = "Registro reactivado exitosamente",
                                session = shiny::getDefaultReactiveDomain())
        ret(ret() + 1)
      }, error = function(error) {
        removeModal()
        .ManejarErrorAccion(error = error, operacion = "reactivar el contacto", usuario = usuario())
      })
    })
    
    # Retorno ----
    list(n = reactive(ret()))
  })
}
