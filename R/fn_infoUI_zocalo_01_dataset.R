#' @export
fn_infoUI_zocalo_01_dataset <- function(data_obj) {
  req(data_obj)

  list_safe <- tryCatch(data_obj, error = function(e) NULL)
  if (is.null(list_safe)) return("Sin datos.")

  the_source <- list_safe$"source"
  the_file <- list_safe$"file"
  str_shape <- list_safe$"str_shape"


  div(
    class = "p-3 rounded shadow-sm",
    style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",

    # Título principal
    # h4(
    #   class = "mb-3 pb-2",
    #   style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
    #   icon("info-circle"),
    #   "Data Selection"
    # ),

    div(
      class = "mb-3 p-2 rounded",
      style = "background-color: rgba(13, 110, 253, 0.05); border-left: 4px solid #0d6efd;",

      h5(class = "text-primary",
         icon("database",
              style = "padding-left: 10px;",
              class = "me-2"), "User file - dataset"),

      div(class = "d-flex flex-wrap",
          div(class = "me-4 mb-2",
              tags$b(style = "padding-left: 10px;", "Source: "),
              span(the_source, style = "font-family: monospace;")),

          div(class = "me-4 mb-2",
              tags$b(style = "padding-left: 10px;", "File: "),
              span(the_file, style = "font-family: monospace;")),

          div(class = "me-4 mb-2",
              tags$b(style = "padding-left: 10px;", "Shape: "),
              span(str_shape, style = "font-family: monospace;"))

      )
    )

  )
}
