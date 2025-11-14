#' @export
fn_infoUI_zocalo_02_VarSelection <- function(data_obj) {
  req(data_obj)

  list_safe <- tryCatch(data_obj, error = function(e) NULL)
  if (is.null(list_safe)) return("Sin datos.")

  var_name_factor <- list_safe$"var_name_factor"
  var_name_rv <- list_safe$"var_name_rv"
  str_shape <- list_safe$"str_shape"
  alpha_value <- list_safe$"alpha_value"

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
         icon("filter",
              style = "padding-left: 10px;",
              class = "me-2"), "Variable Selection - minidataset"),

      div(class = "d-flex flex-wrap",
          div(class = "me-4 mb-2",
              tags$b(style = "padding-left: 10px;", "Factor: "),
              span(var_name_factor, style = "font-family: monospace;")),

          div(class = "me-4 mb-2",
              tags$b(style = "padding-left: 10px;", "Response Variable: "),
              span(var_name_rv, style = "font-family: monospace;")),

          div(class = "me-4 mb-2",
              tags$b(style = "padding-left: 10px;", "Shape: "),
              span(str_shape,
                   style = "font-family: monospace;")),
          div(class = "me-4 mb-2",
              tags$b(style = "padding-left: 10px;", "Alpha value: "),
              span(alpha_value,
                   style = "font-family: monospace;"))
      )
    )

  )
}
