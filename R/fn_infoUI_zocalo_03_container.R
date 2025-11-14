#' Función auxiliar para generar el contenedor UI del zócalo de metadatos.
#'
#' @param dt_output_id El ID del objeto DTOutput (ej. "settings_table_display")
#'                   que se incrustará dentro del zócalo.
#' @return Un objeto de tagList (código HTML/Shiny UI).
fn_infoUI_zocalo_03_container <- function(dt_output_id) {

  div(
    class = "p-3 rounded shadow-sm",
    style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",

    # Título principal
    # h4(
    #   class = "mb-3 pb-2",
    #   style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
    #   icon("info-circle"),
    #   "Data Selection Metadata" # Título genérico
    # ),

    div(
      class = "mb-3 p-2 rounded",
      style = "background-color: rgba(13, 110, 253, 0.05); border-left: 4px solid #0d6efd;",

      h5(class = "text-primary",
         icon("sliders",
              style = "padding-left: 10px;",
              class = "me-2"), "Special Settings"),

      # Contenedor para la tabla DT que recibe el ID dinámicamente
      div(class = "d-flex flex-wrap",
          DT::DTOutput(dt_output_id) # Usa el ID pasado como argumento
      )
    )
  )
}
