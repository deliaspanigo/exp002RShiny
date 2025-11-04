# app.R
library(shiny)
library(htmltools) # Necesario para tagList

# Definición de los botones (Usamos una lista para generar 20 botones fácilmente)
# Incluimos iconos de datos, estilo y acción.
icon_list <- list(
  # Iconos para Selección de Variables / Datos
  list(id = "btn_data_1", icon = "table-list", label = "Variables (Lista)"),
  list(id = "btn_data_2", icon = "filter", label = "Filtros"),
  list(id = "btn_data_3", icon = "sliders", label = "Ajustes Var."),
  list(id = "btn_data_4", icon = "database", label = "Importar Datos"),
  list(id = "btn_data_5", icon = "chart-simple", label = "Resumen Estad."),

  # Iconos para Colores / Estilo
  list(id = "btn_style_6", icon = "palette", label = "Paleta Colores"),
  list(id = "btn_style_7", icon = "eye-dropper", label = "Selector Color"),
  list(id = "btn_style_8", icon = "wand-magic-sparkles", label = "Estilizar"),
  list(id = "btn_style_9", icon = "paint-roller", label = "Pincel"),
  list(id = "btn_style_10", icon = "text-height", label = "Fuentes"),

  # Iconos de Acción / Control
  list(id = "btn_action_11", icon = "sync", label = "Refrescar"),
  list(id = "btn_action_12", icon = "play", label = "Ejecutar"),
  list(id = "btn_action_13", icon = "stop", label = "Detener"),
  list(id = "btn_action_14", icon = "download", label = "Descargar"),
  list(id = "btn_action_15", icon = "upload", label = "Cargar Archivo"),

  # Iconos Misceláneos / Comunes
  list(id = "btn_misc_16", icon = "gear", label = "Configuración"),
  list(id = "btn_misc_17", icon = "bell", label = "Notificaciones"),
  list(id = "btn_misc_18", icon = "circle-info", label = "Ayuda"),
  list(id = "btn_misc_19", icon = "lock", label = "Bloquear"),
  list(id = "btn_misc_20", icon = "users", label = "Usuarios")
)

# Estilo base del botón para replicar tu formato
button_style <- "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;"
icon_style <- "font-size: 40px; display: block; margin-bottom: 8px;" # Reduje un poco el tamaño del icono para que quepa bien

# Función para generar un actionButton con tu estilo
create_icon_button <- function(button_data) {
  actionButton(
    inputId = button_data$id,
    label = tagList(
      icon(button_data$icon, style = icon_style),
      span(button_data$label, style = "white-space: normal; text-align: center;")
    ),
    class = "btn-default", # Puedes cambiar a btn-primary, btn-success, etc.
    style = button_style
  )
}

ui <- fluidPage(
  # --- Carga Local de Font Awesome ---
  # Esto asegura que los iconos se vean sin conexión a Internet
  tags$head(
    tags$link(rel = "stylesheet", href = "shared/font-awesome/css/all.min.css"),
    tags$style(HTML("
            .col-sm-2 {
                padding-left: 5px !important;
                padding-right: 5px !important;
            }
            /* Estilo para que el botón ocupe todo el ancho de la columna */
            .action-button {
                width: 100%;
            }
        "))
  ),

  # Título
  h2("🚀 Matriz de 20 Botones de Icono Personalizados"),
  hr(),

  # Contenedor de la cuadrícula de botones
  div(
    style = "margin-left: 50px; margin-right: 50px;",

    # Generar las 4 filas de 5 botones
    lapply(0:3, function(row_index) {
      start_index <- row_index * 5 + 1
      end_index <- (row_index + 1) * 5

      # Crear una fila de Shiny
      fluidRow(
        # Itera sobre 5 botones para esta fila
        lapply(start_index:end_index, function(button_index) {
          column(2, # Usa columnas de ancho 2 (5 * 2 = 10, dejando 2 de margen)
                 create_icon_button(icon_list[[button_index]])
          )
        })
      )
    })
  ),

  hr(),
  h3("Valores de los Clicks:"),
  # Output para mostrar cuántas veces se ha hecho clic en los botones
  verbatimTextOutput("click_counts")
)

server <- function(input, output, session) {

  # Lógica simple para rastrear los clicks
  output$click_counts <- renderPrint({
    counts <- sapply(icon_list, function(btn) {
      input[[btn$id]] %||% 0
    })
    names(counts) <- sapply(icon_list, function(btn) btn$id)

    # Filtra para mostrar solo los botones que han sido clickeados al menos una vez
    clicked_counts <- counts[counts > 0]

    if (length(clicked_counts) == 0) {
      cat("Click en cualquier botón para ver el contador.")
    } else {
      print(clicked_counts)
    }
  })
}

shinyApp(ui = ui, server = server)
