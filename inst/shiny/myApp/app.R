
library("bslib")
library("digest")
library("dplyr")
library("fs")
library("ggplot2")
library("miniUI")
library("openxlsx")
library("palmerpenguins")
library("quarto")
library("rmarkdown")
library('rstudioapi')
library("shiny")
library("shinyjs")
library("usethis")
library("EnvStats")
library("agricolae")
library("plotly")
library("reticulate")
library("webshot2")
library("readxl")
library("DT")
library("writexl")

MY_PACKAGE_NAME <- "exp002RShiny"
my_color_blue   <- "#0d6efd"  # Blue - Bootstrap primary
my_color_green  <- "#198754"  # Green - Bootstrap success
my_color_orange <- "#fd7e14"  # Orange - Bootstrap warning
my_color_red    <- "#dc3545"  # Red - Bootstrap danger

# 1. Define la ruta de la carpeta que contiene los archivos de funciones
#    Asegúrate de cambiar "ruta/a/tu/carpeta" por la ruta real.
ruta_carpeta <- "../../../R"

# 2. Obtiene la lista de todos los archivos .R dentro de esa carpeta
#    `full.names = TRUE` asegura que obtenemos la ruta completa del archivo,
#    lo cual es necesario para `source()`.
archivos_r <- list.files(
  path = ruta_carpeta,
  pattern = "\\.R$", # Usa una expresión regular para buscar archivos que terminen en .R
  full.names = TRUE
)

# 3. Utiliza un bucle para cargar (source) cada archivo
#    Esto ejecutará el código en cada archivo, cargando tus funciones en el entorno global.
if (length(archivos_r) > 0) {
  for (archivo in archivos_r) {
    source(archivo, encoding = "UTF-8") # Se recomienda especificar la codificación
    message(paste("Cargado:", archivo)) # Opcional: muestra un mensaje para saber qué se cargó
  }
  message("\n¡Todas las funciones han sido cargadas exitosamente!")
} else {
  message("No se encontraron archivos .R en la carpeta especificada.")
}
print(getwd())

##############################
mod_download_ui <- function(id, title) {
  # Create a namespace using the 'id' to ensure element IDs are unique
  ns <- NS(id)

  fluidRow(
    # Column for the title/description
    column(4, strong(title)),

    # Column for the action buttons
    column(8,

      div(uiOutput(ns("set_btn")))
    )
  )
}

mod_download_server <- function(id, r_file_path) {
  # Note: The 'shinyjs' and 'digest' packages are required for this module.

  moduleServer(id, function(input, output, session) {

    # Get the namespace function for use inside the server
    ns <- session$ns

    super_btn_download <- reactiveValues()
    super_btn_download$"class" <- "btn-warning btn-sm"

    super_btn_open <- reactiveValues()
    super_btn_open$"class" <- "btn-warning btn-sm"

    output$"set_btn" <- renderUI({
      # Download Button
      div(
        downloadButton(
          outputId = ns("btn_download"),
          label = NULL,
          icon = icon("download", class = "fa-2x"),
          class = super_btn_download$"class"
        ),

        # Open Button (Binoculars)
        actionButton(
          inputId = ns("btn_open"),
          label = NULL,
          icon = icon("binoculars", class = "fa-2x"),
          class = super_btn_open$"class"
        )
      )
    })
    observeEvent(r_file_path(), {

      print("DENTRO")
      print(r_file_path())
      print(is.null(r_file_path()))


      # print(file.exists(r_file_path()))

      if(is.null(r_file_path())){
        print("paso 1")
        print(r_file_path())

        super_btn_download$"class" <- "btn-danger btn-sm"
        super_btn_open$"class"     <- "btn-danger btn-sm"

        # shinyjs::runjs(paste0("
        #     $('#", ns("btn_download"), "').removeClass('disabled');
        #     $('#", ns("btn_download"), "').removeClass('btn-warning');
        #     $('#", ns("btn_download"), "').addClass('btn-danger');
        #   "))

        # shinyjs::removeClass("btn_open", "btn-warning")
        # shinyjs::addClass("btn_open", "btn-primary")
        print(" ")
        print(" ")
        print(" ")
      } else
        if(!file.exists(r_file_path())) {

          super_btn_download$"class" <- "btn-danger btn-sm"
          super_btn_open$"class"     <- "btn-danger btn-sm"

      } else
      if(!is.null(r_file_path()) & file.exists(r_file_path())) {
        # shinyjs::removeClass("btn_open", "btn-primary")
        # shinyjs::removeClass("btn_open", "btn-warning")
        super_btn_download$"class" <- "btn-warning btn-sm"
        super_btn_open$"class"     <- "btn-warning btn-sm"
      }
    },  ignoreNULL = FALSE,
    ignoreInit = FALSE)
    # ----------------------------------------
    # Logic for the Open Button (btn_open)
    # ----------------------------------------
    observeEvent(input$btn_open, {

      message(crayon::green("Open button clicked!"))

      # 1. CHANGE BUTTON COLOR B1: Orange -> Green (Persistent)
      shinyjs::removeClass("btn_open", "btn-warning")
      shinyjs::addClass("btn_open", "btn-success")

      # 2. Get the file path
      html_path <- r_file_path()

      # *** CRITICAL CHECK: Ensure the file exists ***
      if (is.null(html_path) || !file.exists(html_path)) {
        showNotification("Error: The file has not been generated or cannot be found.", type = "error")
        shinyjs::removeClass("btn_open", "btn-success")
        shinyjs::addClass("btn_open", "btn-warning")
        return(NULL)
      }

      html_dir <- dirname(html_path)
      html_filename <- basename(html_path)

      # 3. Obtener la extensión del archivo REAL (no la URL)
      library("tools")
      the_file_ext <- tools::file_ext(html_path)  # ← USAR html_path, NO html_url

      # 4. Manejar diferentes tipos de archivo
      if(the_file_ext == "html" | the_file_ext == "pdf") {  # ← Corregí "hmtl" a "html"

        # Para HTML/PDF: usar URL temporal
        resource_id <- digest::digest(html_dir, algo = "md5")
        shiny::addResourcePath(resource_id, html_dir)
        html_url <- file.path(resource_id, html_filename)
        shinyjs::runjs(paste0("window.open('", html_url, "', '_blank');"))

      } else if(the_file_ext == "docx" | the_file_ext == "xlsx") {

        # Para Word/Excel: usar ruta local del sistema
        browseURL(html_path)  # ← USAR html_path, NO html_url

      } else {

        showNotification(paste("Tipo de archivo no soportado:", the_file_ext), type = "warning")
        # Revertir color del botón
        shinyjs::removeClass("btn_open", "btn-success")
        shinyjs::addClass("btn_open", "btn-warning")

      }

    })

    # ----------------------------------------
    # Logic for the Download Button (btn_download)
    # ----------------------------------------
    output$btn_download <- downloadHandler(

      # 1. Define the filename when downloading
      filename = function() {
        full_path <- r_file_path()
        if (!is.null(full_path) && file.exists(full_path)) {
          return(basename(full_path))
        } else {
          return("empty_file.html")
        }
      },

      # 2. Logic to copy the content to the download file
      content = function(file) {
        file_to_download <- r_file_path()

        if (!is.null(file_to_download) && file.exists(file_to_download)) {

          # Change download button color to green
          # Note: We use session$ns() here because downloadHandler is not automatically namespaced
          # like other reactive outputs, so we need the full jQuery selector.
          shinyjs::runjs(paste0("
            $('#", ns("btn_download"), "').removeClass('disabled');
            $('#", ns("btn_download"), "').removeClass('btn-warning');
            $('#", ns("btn_download"), "').addClass('btn-success');
          "))

          # Copy the generated file to the temporary 'file' managed by Shiny
          file.copy(file_to_download, file)

        } else {
          warning("Temporary file for download not found.")
          writeLines("Error: File not generated.", file)
        }
      }
    )
  })
}
##############################

ui <- bslib::page_sidebar(
  padding = c(15, 15, 15, 15), # top, right, bottom, left
  shinyjs::useShinyjs(),
  # Header
  tags$head(
    # Font Awesome from our local shiny package
    tags$link(rel = "stylesheet", href = "shared/font-awesome/css/all.min.min.css")
  ),
  # Header - CSS
  tags$head(
    tags$style(HTML("
     :root {
            --bs-border-thickness: 3px;
            --bs-border-color-dark: #4d2600;
            --bs-primary-color: #ff8c00;
            --bs-text-color: #a0522d;
            --bs-bg-hover: #ffe4c4;
            --bs-border-radius: 0.5rem;
     }


         .btn-primary {
            /* Fondo: Tu naranja principal */
            # background-color: #007bc2 !important; /* #ff8c00 */
            /* Texto: Blanco */
            color: white !important;
            /* Borde: Negro forzado */
            border-color: var(--rs-btn-primary-border) !important; /* #000000 */
            /* Sombra ligera para elevación */
            box-shadow: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.075);
            /* Radio y transición */
            border-radius: var(--rs-border-radius);
            transition: all var(--rs-transition-speed);
        }
        .btn-primary:hover {
          color: black !important;
          background-color: #0069a5 !important;
          border-color: var(--rs-btn-primary-border) !important; /* Mantiene el borde negro */
          box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);
        }



             /* Estilo NARANJA (btn-warning) FORZADO */
             .btn-warning {
                 background-color: #ff8c00 !important;
                 color: white !important;
                 border-color: #000000 !important;
             }
              .btn-warning:hover {
          background-color: #ff8c00 !important;
          color: black !important;
          border-color: #000000 !important;
            }

           /* Estilo VERDE (btn-success) FORZADO */
           .btn-success {
               background-color: #4CAF50 !important;
               color: white !important;
               border-color: #000000 !important;
           }
           .btn-success:hover {
          background-color: #4CAF50 !important;
          color: black !important;
          border-color: #000000 !important;
           }

             /* Asegura que los estados focus/active/hover usen nuestros colores */
             .btn-success:active, .btn-success:focus, .btn-success:hover {
                 background-color: #4CAF50 !important;
                 border-color: #000000 !important;
             }
             .btn-warning:active, .btn-warning:focus, .btn-warning:hover {
                 background-color: #ff8c00 !important;
                 border-color: #000000 !important;
             }

             /* Selecciona elementos cuya CLASE EXACTA sea 'sidebar' */
           .sidebar {
            background-color: #ffe4c4 !important; /* Ejemplo: Gris muy claro */
            border-right: 3px solid #FFFFFF !important;
            border-radius: 0.5rem !important;
            padding: 15px !important;
           }

            .main {
              /* Fondo del Panel Principal (Ejemplo: Blanco puro) */
              background-color: #FFFFFF !important;

              /* Bordes redondeados */
              border-radius: 0.5rem !important;

            }

            /* Borde negro de 3px para todas las tarjetas (navset_card_tab o card) */
            .card {
                border: 3px solid #000000 !important;
            }

            /* 🎯 Selector para la barra de título/pestañas */
            .card-header.card-navs {
                /* Aquí va tu estilo de altura (min-height: 100px !important;) */

                /* 🌟 LÍNEA NEGRA DE 3PX EN LA PARTE INFERIOR 🌟 */
                border-bottom: 3px solid #000000 !important;
            }

            /* Si tienes un header adicional debajo de las pestañas (card_header),
               y quieres una línea allí también, usa este selector: */
            .card-header:not(.card-navs) {
                border-bottom: 3px solid #000000 !important;
            }



/* ----------------------------------------------------------- */
/* 1. PESTAÑA NO ACTIVA (FONDO NARANJA, BORDE INFERIOR OSCURO) */
/* ----------------------------------------------------------- */
.card-header .nav-link {
    /* 🛑 CLAVE 1: FONDO NARANJA */
    background-color: var(--bs-primary-color) !important;
    color: var(--bs-color-text-active) !important; /* Texto claro sobre naranja */

    /* Borde completo negro/oscuro */
    border: var(--bs-border-thickness) solid var(--bs-border-color-dark) !important;

    /* 🛑 CLAVE 2: MANTENER BORDE INFERIOR OSCURO 🛑 */
    /* Esto hace que se vea la línea divisoria entre la pestaña y la barra de encabezado */
    border-bottom: var(--bs-border-thickness) solid var(--bs-border-color-dark) !important;

    /* Asegurar que las esquinas inferiores no estén redondeadas si el borde es visible */
    border-radius: var(--bs-border-radius) var(--bs-border-radius) 0 0 !important;
}

/* ----------------------------------------------------------- */
/* 2. PESTAÑA ACTIVA (FONDO VERDE, BORDE INFERIOR BLANCO) */
/* ----------------------------------------------------------- */
.card-header .nav-link.active {
    /* 🛑 CLAVE 1: FONDO VERDE */
    background-color: #4CAF50 !important; /* Usamos #FFFFFF o el color de fondo de la tarjeta */
    color: var(--bs-border-color-dark) !important; /* Texto oscuro sobre blanco */

    /* Borde completo negro/oscuro */
    border: var(--bs-border-thickness) solid var(--bs-border-color-dark) !important;

    /* 🛑 CLAVE 2: BORDE INFERIOR VERDE 🛑 */
    border-bottom-color: #4CAF50 !important;

    /* Si quieres que sea de 6px (como pediste antes), agrega esto: */
    /* border-bottom-width: 6px !important; */

    box-shadow: none;
}
/* ----------------------------------------------------------- */
/* 🛑 BORDE NEGRO DE 3PX PARA TODOS LOS BOTONES 🛑 */
/* ----------------------------------------------------------- */
.btn {
    border: 3px solid #000000 !important;
    border-radius: 0.5rem !important;
}

             /* Los estilos de layout (body, .main, .bslib-page-sidebar) han sido eliminados */
        "))
  ),


  sidebar = bslib::sidebar(
    padding = c(0, 15, 0, 15), # top, right, bottom, left
    div(
      style = "text-align: left;",
      tags$img(src = "Rscience_logo_01.png", width = "40%", style = "padding-bottom: 10px;"),
      tags$b("v1.0.24"),
      br(),

      # SideBar Panel--------------------------------------------------------
      uiOutput("the_super_side")
      #-------------------------------------------------------------------------
    )
  ),
  # Main Panel -----------------------------------------------------------------
  # 1. Contenedor Principal (Define la Altura Total)
  div(
    # Estilos Flexbox: Vertical, altura fija para que los % funcionen
    # style = "display: flex; flex-direction: column; height: calc(100vh - 60px); width: 100%; border: 2px solid #ccc; box-sizing: border-box;",
    style = "display: flex; flex-direction: column; height: calc(100vh - 40px); width: 100%;",

    # 2. Primer Objeto: 90% de Altura
    div(
      # flex: 0 0 90% -> No crece, NO SE ENCOGE, 90% de altura base
      # style = "flex: 0 0 90%; width: 100%; border: 1px dashed blue; background-color: #e6f0ff; box-sizing: border-box; padding: 10px; overflow-y: auto;",
      style = "flex: 0 0 95%; width: 100%; overflow-y: auto;",
      uiOutput("the_super_main")#,
      # tags$p("Contenedor Superior (90% reservado)", style = "color: blue; margin-top: 10px;")
    ),

    # # 3. Segundo Objeto: 10% de Altura
    div(
      # flex: 0 0 10% -> No crece, NO SE ENCOGE, 10% de altura base
      # style = "flex: 0 0 5%;
      #      width: 100%;
      #      border: 1px dashed red;
      #      background-color: #ffe6e6;
      #      box-sizing: border-box;
      #      padding: 10px;
      #      overflow: hidden;", # <-- ¡Esta es la adición CRUCIAL!
      style = "flex: 0 0 5%;
           width: 100%;
           overflow: hidden;",
      uiOutput("final_info")#,
      #tags$p("Contenedor Inferior (10% reservado)", style = "color: red; margin-top: 5px;")
    )
  )

  #-----------------------------------------------------------------------------
)

server <- function(input, output, session) {

  set_reactive_values_from_list <- function(rv, data_list) {

    # 1. Verificar que ambos son objetos válidos
    if (!is.list(data_list)) {
      stop("El argumento 'data_list' debe ser una lista R estándar.")
    }

    # 2. Iterar sobre los nombres de la lista de datos
    for (name in names(data_list)) {
      # 3. Asignar el valor de la lista al elemento correspondiente en reactiveValues
      #    Esto automáticamente crea/actualiza el elemento en rv
      rv[[name]] <- data_list[[name]]
    }

    # Nota: Aunque rv se modifica por referencia, devolverlo es una buena práctica.
    invisible(rv)
  }

  ### Toogle 01 - ClassRoom ----------------------------------------------------
  output$the_toggle_01_classroom <- renderUI({

    div(
      tags$head(
        tags$style(HTML("
      /* Toggle style */
      .form-check-input {
        background-color: #4c78dd !important; /* Blue color for default */
        border-color: #4c78dd !important;
        width: 3.5em !important; /* Increase toggle width */
        height: 1.8em !important; /* Increase height proportionally */
      }

      /* Style when activated (TRUE) */
      .form-check-input:checked {
        background-color: #4CAF50 !important; /* Green color for true */
        border-color: #4CAF50 !important;
      }

      /* Ensure smooth transition */
      .form-check-input {
        transition: background-color 0.3s, border-color 0.3s;
      }

      /* Adjust the indicator circle inside the toggle */
      .form-switch .form-check-input:after {
        height: calc(1.8em - 4px) !important;
        width: calc(1.8em - 4px) !important;
      }

      /* Adjust container spacing */
      .form-switch {
        padding-left: 0 !important;
      }
    "))
      ),
      div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-3",
        span("   ", class = "fw-bold"),
        tags$div(
          class = "form-check form-switch",
          tags$input(
            id = "toggle01_classroom",
            type = "checkbox",
            class = "form-check-input",
            role = "switch",
            checked = NA
          )
        ),
        uiOutput("toggle01_classroom_state", inline = TRUE)
      )
    )
  })


  output$toggle01_classroom_state <- renderUI({
    # 1.Text to show
    the_selection <- ifelse(
      test = input$toggle01_classroom,
       yes = "Data Analysis",  # Active  - Green
        no = "ClassRoom"        # Deafult - Blue
    )

    # 2. Span with style
    span(
      the_selection,
      class = "fw-bold",
      style = paste(
        "display: inline-block;",
        "min-width: 140px;",
        "text-align: left;",
        "font-size: 20px;"
      )
    )
  })


  ### Toogle 02 - Input --------------------------------------------------------

  output$the_toggle_02_input <- renderUI({

    div(
      tags$head(
        tags$style(HTML("
      /* Toggle style */
      .form-check-input {
        background-color: #4c78dd !important; /* Blue color for default (false) */
        border-color: #4c78dd !important;
        width: 3.5em !important; /* Increase toggle width */
        height: 1.8em !important; /* Increase height proportionally */
      }

      /* Style when activated (true) */
      .form-check-input:checked {
        background-color: #4CAF50 !important; /* Green color for true */
        border-color: #4CAF50 !important;
      }

      /* Ensure smooth transition */
      .form-check-input {
        transition: background-color 0.3s, border-color 0.3s;
      }

      /* Adjust the indicator circle inside the toggle */
      .form-switch .form-check-input:after {
        height: calc(1.8em - 4px) !important;
        width: calc(1.8em - 4px) !important;
      }

      /* Adjust container spacing */
      .form-switch {
        padding-left: 0 !important;
      }
    "))
      ),
      div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-3",
        span("   ", class = "fw-bold"),
        tags$div(
          class = "form-check form-switch",
          tags$input(
            id = "toggle02_input",
            type = "checkbox",
            class = "form-check-input",
            role = "switch"
          )
        ),
        # span("Python", class = "fw-bold"),
        uiOutput("toggle02_input_state", inline = TRUE)
      )
    )
  })

  output$toggle02_input_state <- renderUI({
    # 1. Text to show
    the_selection <- ifelse(
      test = input$toggle02_input,
       yes = "Output",
        no = "Input")

    # 2. Span with style
    span(
      the_selection,
      class = "fw-bold",
      style = paste(
        "display: inline-block;",
        "min-width: 140px;",
        "text-align: left;",
        "font-size: 20px;"  # 👈 Añade esta línea para definir el tamaño de la letra
      )
    )
  })


  ### Toogle 03 - ShowRoom------------------------------------------------------

  output$the_toggle_03_showroom <- renderUI({
    div(
      tags$head(
        tags$style(HTML("
      /* Toggle style */
      .form-check-input {
        background-color: #4c78dd !important; /* Blue color for default (false) */
        border-color: #4c78dd !important;
        width: 3.5em !important; /* Increase toggle width */
        height: 1.8em !important; /* Increase height proportionally */
      }

      /* Style when activated (true) */
      .form-check-input:checked {
        background-color: #4CAF50 !important; /* Green color for true */
        border-color: #4CAF50 !important;
      }

      /* Ensure smooth transition */
      .form-check-input {
        transition: background-color 0.3s, border-color 0.3s;
      }

      /* Adjust the indicator circle inside the toggle */
      .form-switch .form-check-input:after {
        height: calc(1.8em - 4px) !important;
        width: calc(1.8em - 4px) !important;
      }

      /* Adjust container spacing */
      .form-switch {
        padding-left: 0 !important;
      }
    "))
      ),
      div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-3",
        span("   ", class = "fw-bold"),
        tags$div(
          class = "form-check form-switch",
          tags$input(
            id = "toggle03_showroom",
            type = "checkbox",
            class = "form-check-input",
            role = "switch"
          )
        ),
        # span("Python", class = "fw-bold"),
        uiOutput("toggle03_showroom_state", inline = TRUE)
      )
    )
  })

  output$toggle03_showroom_state <- renderUI({
    # 1. Text to show
    the_selection <- ifelse(
      test = input$toggle03_showroom,
       yes = "Download",
        no = "ShowRoom"
    )

    # 2. Span with style
    span(
      the_selection,
      class = "fw-bold",
      style = paste(
        "display: inline-block;",
        "min-width: 140px;",
        "text-align: left;",
        "font-size: 20px;"
      )
    )
  })


  ### Super SideBar ------------------------------------------------------------
  output$"the_super_side" <- renderUI({
    div(
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",
      uiOutput("the_toggle_01_classroom"),
      conditionalPanel(
        condition = "input.toggle01_classroom == true",
        uiOutput("the_toggle_02_input"),
        conditionalPanel(
          condition = "input.toggle02_input == false",
          uiOutput("menu_input")
        ),
        conditionalPanel(
          condition = "input.toggle02_input == true",
          uiOutput("the_toggle_03_showroom")
        )


      )
    )
  })


  ### Menu Input ---------------------------------------------------------------
  output$"menu_input" <- renderUI({

    # Button style
    str_style_btn <- "width: 90px; height: 90px; display: flex; align-items: center; justify-content: center; margin-bottom: 8px;"

    # Icon Style
    str_style_icon <- "font-size: 50px; display: block; margin: 0 auto;"

    div(
      actionButton(
        inputId = "btn_dataset",
        label = tagList(icon("database", style = str_style_icon)),
        class = "btn-primary",
        style = str_style_btn,
        title = "Dataset"
      ),

      actionButton(
        inputId = "btn_var_selector",
        label = tagList(icon("filter", style = str_style_icon)),
        class = "btn-primary",
        style = str_style_btn,
        title = "Variable Selector"
      ),

      actionButton(
        inputId = "btn_settings",
        label = tagList(icon("sliders", style = str_style_icon)),
        class = "btn-primary",
        style = str_style_btn,
        title = "Settings"
      ),

      actionButton(
        inputId = "btn_play_front",
        label = tagList(icon("play", style = str_style_icon)),
        class = "btn-primary",
        style = str_style_btn,
        title = "Play!"
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      actionButton(
        inputId = "btn_refresh",
        label = tagList(icon("arrows-rotate", style = str_style_icon)),
        class = "btn-primary",
        style = str_style_btn,
        title = "Refresh"
      )
    )
  })


    # Standard module for dataset loading (MASTER_module_import - SERVER) ------
    the_list01_Dataset_internal <- MASTER_module_import_server(id = "MASTER_import", show_dev = FALSE)

    # Standard module for dataset loading (MASTER_module_import - UI) ----------
    output$"super_dataset_selection" <- renderUI({

      # Standard module for dataset loading (MASTER_module_import - UI)
      MASTER_module_import_ui(id = "MASTER_import")

    })

    # Stone 01 - Dataset - Default Values --------------------------------------
    the_list01_Dataset_R <- list("source" = NA,
                                 "file" = NA,
                                 "str_shape"= NA,
                                 "my_dataset" = NA,
                                 "info_status" = "waiting",
                                 "info_check_go_forward" = FALSE,
                                 "info_color" = my_color_blue)

    the_list01_Dataset_stone <- reactiveValues()
    set_reactive_values_from_list(rv = the_list01_Dataset_stone,
                                  data_list = the_list01_Dataset_R)


    # Stone 02 - Var Selection - Default Values --------------------------------
    the_list02_VarSelection_R <- list("var_name_factor" = NA,
                                      "var_name_rv" = NA,
                                      "alpha_value" = NA,
                                      "vector_var_names" = NA,
                                      "minidataset" = NA,
                                      "ncol" = NA,
                                      "nrow" = NA,
                                      "str_shape" = NA,
                                      "info_status" = "waiting",
                                      "info_color" = my_color_blue)

    the_list02_VarSelection_stone <- reactiveValues()
    set_reactive_values_from_list(rv = the_list02_VarSelection_stone,
                                  data_list = the_list02_VarSelection_R)

    # Stone 03 - SpecialSettings - Default Values ------------------------------
    the_list03_SpecialSettigns_R <- list("df_order" = NA,
                                         "vector_ordered_levels" = NA,
                                         "vector_ordered_colors" = NA,
                                         "minidataset" = NA,
                                         "nrow" = NA,
                                         "ncol" = NA,
                                         "info_status" = "waiting",
                                         "info_color" = my_color_blue,
                                         "shiny_obj_name" = NA)

    the_list03_SpecialSettigns_stone <- reactiveValues()
    set_reactive_values_from_list(rv = the_list03_SpecialSettigns_stone,
                                  data_list = the_list03_SpecialSettigns_R)


  observeEvent(input$btn_dataset, {


    showModal(
      modalDialog(
        size = "xl",
        easyClose = FALSE,

        # Aplicamos estilos personalizados para hacer el modal más grande y posicionarlo más arriba
        tags$div(
          tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }

        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }

        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }

        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }

        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
        ),

        # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
        div(
          style = "height: 100%; overflow-y: auto; padding: 15px;",
          uiOutput("super_dataset_selection")
          # Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
        ),

        footer = tags$div(
          style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
          # Botón Cancelar de ancho completo
          tags$button(
            id = "btn_cancel01",
            type = "button",
            class = "btn btn-default",
            style = "width: 50%; height: 45px;", # Aumentado la altura
            "data-bs-dismiss" = "modal",
            "CANCEL"
          ),
          actionButton(inputId = "confirm_action01", label = "ADD",
                       class = "btn-primary", style = "width: 50%; height: 45px;") # Aumentado la altura

        )

      )
    )



  })
  observeEvent(input$confirm_action01, {


    if (is.null(the_list01_Dataset_internal()$"my_dataset")) {
      showNotification(
        "Please, select a dataset.",
        type = "warning"
      )
      return()
    }


    # All Ok...
    # 1) Show notification
    fn_show_notification_ok(the_message = "Dataset imported successfully.")


    # 2) Change color on botton
    shinyjs::removeClass(id = "btn_dataset", class = "btn-primary")
    shinyjs::addClass(id = "btn_dataset",  class = "btn-success")

    # 3) Basics
    the_nrow <- nrow(the_list01_Dataset_internal()$"my_dataset")
    the_ncol <- ncol(the_list01_Dataset_internal()$"my_dataset")
    the_str_shape <- paste0(the_nrow, " Rows", " x ", the_ncol, " Cols")

    # 3) Put on stone
    the_list01_Dataset_stone$"source" <- the_list01_Dataset_internal()[["data_source"]]
    the_list01_Dataset_stone$"file"   <- the_list01_Dataset_internal()[["original_file_name"]]
    the_list01_Dataset_stone$"str_shape"  <- the_str_shape
    the_list01_Dataset_stone$"my_dataset" <- the_list01_Dataset_internal()$"my_dataset"
    the_list01_Dataset_stone$"info_status" <- "done"
    the_list01_Dataset_stone$"info_check_go_forward" <- TRUE
    the_list01_Dataset_stone$"info_color" <- my_color_green

    # 4) Remove Modal
    removeModal()
  })
  ###---------------------------------------------------------------------------

  output$"var_selection" <- renderUI({
    req(the_list01_Dataset_internal())

    amount_cols <- ncol(the_list01_Dataset_internal()$"my_dataset")
    amount_digits <- nchar(as.character(amount_cols))
    if(amount_digits == 1) amount_digits <- amount_digits + 1
    str_new <- paste0("%0", amount_digits, "d")
    vector_orden <- sprintf(str_new, 1:amount_cols)

    vector_colnames <- colnames(the_list01_Dataset_internal()$"my_dataset")
    #vector_colnames <- paste0(vector_orden, " - ", vector_colnames, " - ", openxlsx::int2col(1:length(vector_colnames)))
    vector_output_names <- paste0("Var ", vector_orden, " - Column ", openxlsx::int2col(1:length(vector_colnames)), " - ", vector_colnames)
    names(vector_colnames) <- vector_output_names
    vector_colnames <- c("Select a variable..." = "", vector_colnames)

    vector_alpha <- c("0.10 (10%)" = "0.10",
                      "0.05 (5%)" = "0.05",
                      "0.01 (1%)" = "0.01")
    div(
      selectInput(inputId = "var_name_rv", label = "Response Variable (RV)", choices = vector_colnames),
      selectInput(inputId = "var_name_factor", label = "Factor", choices = vector_colnames),
      selectInput(inputId = "alpha_value", label = "Alpha value", choices = vector_alpha, selected = vector_alpha[2])

    )
  })


  observeEvent(input$"btn_var_selector", {



    # 2. Mostramos el modal con el contenido del módulo ya inicializado
    # Usando tamaño "xl" (extra large)
    showModal(
      modalDialog(
        # title = "Seleccionar Base de Datos",
        size = "xl", # Mantenemos "xl" como base
        easyClose = TRUE,

        # Aplicamos estilos personalizados para hacer el modal más grande y posicionarlo más arriba
        tags$div(
          tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }

        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }

        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }

        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }

        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
        ),

        # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
        div(
          style = "height: 100%; overflow-y: auto; padding: 15px;",
          uiOutput("var_selection")
          # Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
        ),

        footer = tags$div(
          style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
          # Botón Cancelar de ancho completo
          tags$button(
            id = "btn_cancel02",
            type = "button",
            class = "btn btn-default",
            style = "width: 50%; height: 45px;", # Aumentado la altura
            "data-bs-dismiss" = "modal",
            "CANCEL"
          ),
          actionButton(inputId = "confirm_action02", label = "ADD",
                       class = "btn-primary", style = "width: 50%; height: 45px;") # Aumentado la altura

        )

      )
    )



  })
  observeEvent(input$confirm_action02, {

    # # # Hace falta modificar la funcion de importacion
    # para que tenga un objeto como "check_output" con T o F, y que ese
    # valor se resetee cada vez que hay un cambio de selecion de datos.
    # Creo que debo crear como sif uera un "internal_DATA".

    # req(the_list01_Dataset_internal())
    # 1) Hacer validaciones sobre la importacion realizada.
    #    Si todo esta bien...
    # 2) Asignar nuevos valores a "valores_internos".
    # 3) Cerrar el modal
    # Verificar que se haya seleccionado un dataset primero
    # print(the_list01_Dataset_internal())
    if (is.null(the_list01_Dataset_internal()$"my_dataset")) {
      # print(the_list01_Dataset_internal())
      showNotification(
        "Please, select a dataset.",
        type = "warning"
      )

      return()
    }




    # 1) Show notification
    fn_show_notification_ok(the_message = "Variable selection selected successfully.")

    # 2) Change color on botton
    shinyjs::removeClass(id = "btn_var_selector", class = "btn-primary")
    shinyjs::addClass(id = "btn_var_selector",  class = "btn-success")

    # 3) Put on stone
    vector_var_names <- c(input$"var_name_rv", input$"var_name_factor")
    minidataset <- the_list01_Dataset_internal()$"my_dataset"[vector_var_names]
    minidataset[,input$"var_name_factor"] <- as.factor(as.character(minidataset[,input$"var_name_factor"]))

    the_list02_VarSelection_stone$"var_name_factor" <- input$"var_name_factor"
    the_list02_VarSelection_stone$"var_name_rv" <- input$"var_name_rv"
    the_list02_VarSelection_stone$"alpha_value" <- input$"alpha_value"
    the_list02_VarSelection_stone$"vector_var_names" <- vector_var_names
    the_list02_VarSelection_stone$"minidataset" <- minidataset
    the_list02_VarSelection_stone$"ncol" <- ncol(minidataset)
    the_list02_VarSelection_stone$"nrow" <- nrow(minidataset)
    the_list02_VarSelection_stone$"str_shape" <- paste0(nrow(minidataset), " Rows x ", ncol(minidataset), " Cols")
    the_list02_VarSelection_stone$"info_status" <- "done"
    the_list02_VarSelection_stone$"info_check_go_forward" <- TRUE
    the_list02_VarSelection_stone$"info_color" <- my_color_green
    # 4) Remove Modal
    removeModal()

  })
  ###---------------------------------------------------------------------------
  output$settings_selection <- renderUI({
    req(the_list02_VarSelection_stone$"minidataset")

    minidataset <- the_list02_VarSelection_stone$"minidataset"
    var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"

    # 1. Obtener los niveles del factor
    vector_levels <- levels(minidataset[, var_name_factor])
    num_levels <- length(vector_levels)

    if (num_levels == 0) {
      return(p("No se encontraron niveles en la variable factor seleccionada."))
    }

    # 2. Definir una paleta de colores por defecto (hasta 8 colores distintos)
    # Si hay más de 8 niveles, puedes usar una paleta más grande o 'viridis'/'rainbow'
    # default_colors <- setNames(
    #   RColorBrewer::brewer.pal(min(num_levels, 8), "Dark2"),
    #   vector_levels[1:min(num_levels, 8)]
    # )
    default_colors <- setNames(
      rainbow(num_levels),
      vector_levels[1:num_levels]
    )
    # 3. Preparar las opciones de orden (del 1 al N)
    order_choices <- 1:num_levels

    # 4. Generar la lista de inputs dinámicos usando lapply
    # Cada elemento de la lista será un div conteniendo el selector de orden y el selector de color.
    level_inputs <- lapply(seq_along(vector_levels), function(i) {
      level <- vector_levels[i]
      default_color <- default_colors[i] #if (i <= 8) default_colors[i] else "#CCCCCC"

      # Usamos fluidRow para que los inputs se muestren uno al lado del otro
      fluidRow(
        id = paste0("config_row_", level),

        # Selector de Orden (el usuario asigna la posición deseada)
        column(4,
               selectInput(
                 inputId = paste0("order_", level),
                 label = paste("Level:", level),
                 choices = order_choices,
                 selected = i # Orden inicial por defecto es la posición actual
               )
        ),

        # Selector de Color
        column(4,
               colourpicker::colourInput(
                 inputId = paste0("color_", level),
                 label = paste("Color:", level),
                 value = default_color,
                 showColour = "background"
               )
        )
      )
    })

    # 5. Devolver todos los elementos generados
    tagList(
      # h3(icon("sliders-h"), "Configuración de Niveles"),
      # p("Defina el orden de visualización y el color para cada categoría:"),
      level_inputs
    )
  })


  the_list03_SpecialSettigns_internal <- reactive({
    # req(the_list01_Dataset_stone)
    # req(the_list02_VarSelection_stone)
    req(the_list02_VarSelection_stone$"minidataset")
    req(the_list02_VarSelection_stone$"var_name_factor")
    vector_levels <- levels(the_list02_VarSelection_stone$"minidataset"[, the_list02_VarSelection_stone$"var_name_factor"])
    req(vector_levels)
    # 1. Crear un data.frame para almacenar las configuraciones
    settings_df <- data.frame(
      level = vector_levels,
      order = rep(NA, length(vector_levels)), #NA_integer_,
      color = rep(NA, length(vector_levels)), #NA_character_,
      stringsAsFactors = FALSE
    )

    # 2. Iterar y capturar los valores de input
    for (level in vector_levels) {
      # Captura el valor del input de orden
      order_val <- input[[paste0("order_", level)]]

      # Captura el valor del input de color
      color_val <- input[[paste0("color_", level)]]

      # Asigna los valores al data.frame
      idx <- which(settings_df$level == level)
      if (!is.null(order_val)) {
        settings_df$order[idx] <- as.integer(order_val)
      }
      if (!is.null(color_val)) {
        settings_df$color[idx] <- color_val
      }
    }

    # 3. Ordenar el data.frame según la elección del usuario y devolverlo
    # Esto te dará el orden final de los niveles.
    df_order <- settings_df[order(settings_df$order), ]
    vector_ordered_levels <- df_order$level
    vector_ordered_colors <- df_order$color

    output_list <- list()
    output_list$"df_order" <- df_order
    output_list$"vector_ordered_levels" <- vector_ordered_levels
    output_list$"vector_ordered_colors" <- vector_ordered_colors

    output_list

  })

  output$settings_table_display <- DT::renderDT({
    # Requiere la función reactiva que has definido
    req(the_list03_SpecialSettigns_internal())

    settings_df <- the_list03_SpecialSettigns_internal()$"df_order"

    # 2. Renombrar columnas para la visualización
    settings_df <- settings_df %>%
      dplyr::select(
        Level = level,
        Order = order,
        ColorCode = color
      )

    # 3. Crear una columna HTML para mostrar el color
    # Esta columna contendrá un pequeño div con el color de fondo.
    settings_df$ColorSwatch <- paste0(
      '<div style="width: 100%; height: 20px; background-color:',
      settings_df$ColorCode,
      '; border: 1px solid #000; border-radius: 3px;"></div>'
    )

    # 4. Seleccionar y ordenar las columnas para el display final
    final_display_df <- settings_df %>%
      dplyr::select(
        "Level" = Level,
        "Order" = Order,
        "Color" = ColorSwatch,
        "Hex Code" = ColorCode
      )

    # 5. Renderizar la tabla con DT, indicando que la columna 'Color' es HTML
    DT::datatable(
      final_display_df,
      escape = c("Level", "Order", "Hex Code"), # Solo escapa (trata como texto) estas columnas
      options = list(
        dom = 't', # Muestra solo la tabla (t) sin búsqueda, info, etc.
        paging = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE # Oculta los números de fila
    )
  })
  output$settings_table_display02 <- DT::renderDT({
    # Requiere la función reactiva que has definido
    req(the_list03_SpecialSettigns_stone$"df_order")

    settings_df <- the_list03_SpecialSettigns_stone$"df_order"

    # 2. Renombrar columnas para la visualización
    settings_df <- settings_df %>%
      dplyr::select(
        Level = level,
        Order = order,
        ColorCode = color
      )

    # 3. Crear una columna HTML para mostrar el color
    # Esta columna contendrá un pequeño div con el color de fondo.
    settings_df$ColorSwatch <- paste0(
      '<div style="width: 100%; height: 20px; background-color:',
      settings_df$ColorCode,
      '; border: 1px solid #000; border-radius: 3px;"></div>'
    )

    # 4. Seleccionar y ordenar las columnas para el display final
    final_display_df <- settings_df %>%
      dplyr::select(
        "Nivel" = Level,
        "Orden" = Order,
        "Color" = ColorSwatch,
        "Hex Cod" = ColorCode
      )

    # 5. Renderizar la tabla con DT, indicando que la columna 'Color' es HTML
    DT::datatable(
      final_display_df,
      escape = c("Nivel", "Orden", "Hex Cod"), # Solo escapa (trata como texto) estas columnas
      options = list(
        dom = 't', # Muestra solo la tabla (t) sin búsqueda, info, etc.
        paging = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE # Oculta los números de fila
    )
  })
  observeEvent(input$"btn_settings", {



    # 2. Mostramos el modal con el contenido del módulo ya inicializado
    # Usando tamaño "xl" (extra large)
    showModal(
      modalDialog(
        # title = "Seleccionar Base de Datos",
        size = "xl", # Mantenemos "xl" como base
        easyClose = TRUE,

        # Aplicamos estilos personalizados para hacer el modal más grande y posicionarlo más arriba
        tags$div(
          tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }

        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }

        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }

        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }

        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
        ),

        # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
        div(
          style = "height: 100%; overflow-y: auto; padding: 15px;",
          tagList(
            h3(icon("sliders-h"), "Configuración de Niveles"),
            p("Defina el orden de visualización y el color para cada categoría:"),
            # level_inputs
          ),
          fluidRow(
            column(6, uiOutput("settings_selection")),
            column(6, DT::DTOutput("settings_table_display"))
          )
          # Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
        ),

        footer = tags$div(
          style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
          # Botón Cancelar de ancho completo
          tags$button(
            id = "btn_cancel03",
            type = "button",
            class = "btn btn-default",
            style = "width: 50%; height: 45px;", # Aumentado la altura
            "data-bs-dismiss" = "modal",
            "CANCEL"
          ),
          actionButton(inputId = "confirm_action03", label = "ADD",
                       class = "btn-primary", style = "width: 50%; height: 45px;") # Aumentado la altura

        )

      )
    )



  })
  observeEvent(input$confirm_action03, {

    # # # Hace falta modificar la funcion de importacion
    # para que tenga un objeto como "check_output" con T o F, y que ese
    # valor se resetee cada vez que hay un cambio de selecion de datos.
    # Creo que debo crear como sif uera un "internal_DATA".

    # req(the_list01_Dataset_internal())
    # 1) Hacer validaciones sobre la importacion realizada.
    #    Si todo esta bien...
    # 2) Asignar nuevos valores a "valores_internos".
    # 3) Cerrar el modal
    # Verificar que se haya seleccionado un dataset primero
    # print(the_list01_Dataset_internal())
    if (is.null(the_list01_Dataset_internal()$"my_dataset")) {
      # print(the_list01_Dataset_internal())
      showNotification(
        "Please, select a dataset.",
        type = "warning"
      )

      return()
    }




    # 1) Show notification
    fn_show_notification_ok(the_message = "Variable selection selected successfully.")

    # 2) Change color on botton
    shinyjs::removeClass(id = "btn_settings", class = "btn-primary")
    shinyjs::addClass(id = "btn_settings",  class = "btn-success")

    # 3) Put on stone
    vector_ordered_levels <- the_list03_SpecialSettigns_internal()$"vector_ordered_levels"
    vector_ordered_colors <- the_list03_SpecialSettigns_internal()$"vector_ordered_colors"
    minidaset_without_change <- the_list02_VarSelection_stone$"minidataset"
    var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"

    minidaset_with_change <- minidaset_without_change
    minidaset_with_change[,var_name_factor] <- factor(
      x = minidaset_without_change[,var_name_factor],       # La variable original de factor
      levels = vector_ordered_levels  # El orden de los niveles que calculamos en el Paso 2
    )

    the_list03_SpecialSettigns_stone$"df_order" <-  the_list03_SpecialSettigns_internal()$"df_order"
    the_list03_SpecialSettigns_stone$"vector_ordered_levels" <- the_list03_SpecialSettigns_internal()$"vector_ordered_levels"
    the_list03_SpecialSettigns_stone$"vector_ordered_colors" <- the_list03_SpecialSettigns_internal()$"vector_ordered_colors"
    the_list03_SpecialSettigns_stone$"minidataset" <- minidaset_with_change
    the_list03_SpecialSettigns_stone$"nrow" <- nrow(minidaset_with_change)
    the_list03_SpecialSettigns_stone$"ncol" <- ncol(minidaset_with_change)
    the_list03_SpecialSettigns_stone$"info_status" <- "done"
    the_list03_SpecialSettigns_stone$"info_check_go_forward" <- TRUE
    the_list03_SpecialSettigns_stone$"info_color" <- my_color_green
    the_list03_SpecialSettigns_stone$"shiny_obj_name" <- "control03_plotly"#"settings_table_display02"

    # 4) Remove Modal
    removeModal()

  })

  ###---------------------------------------------------------------------------


  output$"df_control01" <- DT::renderDataTable({
    # El código de configuración y cálculo
    req(the_list03_SpecialSettigns_stone$"minidataset")
    nrow_dataset <- nrow(the_list01_Dataset_internal()$"my_dataset")
    ncol_dataset <- ncol(the_list01_Dataset_internal()$"my_dataset")
    nrow_minidataset <- the_list03_SpecialSettigns_stone$"nrow"
    ncol_minidataset <- the_list03_SpecialSettigns_stone$"ncol"

    df_output <- data.frame(
      "source" = c("dataset", "minidataset"),
      "ncol" = c(ncol_dataset, ncol_minidataset),
      "nrow" = c(nrow_dataset, nrow_minidataset)
    )

    # 4. Seleccionar y ordenar las columnas para el display final
    final_display_df <- df_output %>%
      dplyr::select(
        "Source" = source,
        "Number of cols" = ncol,
        "Number of rows" = nrow
      )

    # 5. Renderizar la tabla con DT
    DT::datatable(
      final_display_df,
      # Usamos autoWidth para que ocupe el espacio mínimo
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        autoWidth = TRUE, # Ayuda a que la tabla no ocupe todo el ancho

        # ********* CLAVE DEL CENTRADO *********
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
        # **************************************
      ),
      rownames = FALSE
    )
  })

  output$"df_control02" <- DT::renderDataTable({
    # El código de configuración y cálculo
    req(the_list03_SpecialSettigns_stone$"minidataset")
    # ... (Cálculos y preparación de final_display_df) ...

    minidataset <- the_list03_SpecialSettigns_stone$"minidataset"
    var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"
    var_name_rv <- the_list02_VarSelection_stone$"var_name_rv"

    # 2. Conversión y resumen
    minidataset[,var_name_factor] <- as.factor(minidataset[,var_name_factor])

    tabla_resumen <- minidataset %>%
      group_by(across(all_of(var_name_factor))) %>%
      summarise(
        n = n(),
        min = min(across(all_of(var_name_rv))),
        max = max(across(all_of(var_name_rv)))
      )

    settings_df <- the_list03_SpecialSettigns_stone$"df_order"

    # 2. Renombrar columnas para la visualización
    settings_df <- settings_df %>%
      dplyr::select(
        Level = level,
        Order = order,
        ColorCode = color
      )

    # 3. Crear una columna HTML para mostrar el color
    settings_df$ColorSwatch <- paste0(
      '<div style="width: 100%; height: 20px; background-color:',
      settings_df$ColorCode,
      '; border: 1px solid #000; border-radius: 3px;"></div>'
    )

    settings_df <- cbind.data.frame(settings_df, tabla_resumen)

    # 4. Seleccionar y ordenar las columnas para el display final
    final_display_df <- settings_df %>%
      dplyr::select(
        "Order" = Order,
        "Level" = Level,
        "n",
        "Min" = min,
        "Max" = max,
        "Color" = ColorSwatch,
        "Hex Cod" = ColorCode
      )

    # 5. Renderizar la tabla con DT
    DT::datatable(
      final_display_df,
      # ¡Asegúrate de marcar la columna 'Color' con I() o usa escape = FALSE si la tabla es simple!
      # El escape es correcto aquí: las columnas listadas se escapan (texto), Color no se escapa (HTML).
      escape = c("Order", "Level", "n", "Min", "Max", "Hex Cod"),
      options = list(
        # CLAVE: Indica a DataTables que intente ajustar el ancho de las columnas
        autoWidth = TRUE,
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        # AÑADIR/MANTENER ESTO PARA CENTRAR TODAS LAS COLUMNAS
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      rownames = FALSE
    )

  }) # <-- ELIMINAMOS LA SECCIÓN DE OPCIONES EXTERNA

  output$"control03_plotly" <- plotly::renderPlotly({

    # Asegurarse de que los datos requeridos existen
    req(the_list03_SpecialSettigns_stone$"minidataset")
    minidataset <- the_list03_SpecialSettigns_stone$"minidataset"
    var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"
    var_name_rv <- the_list02_VarSelection_stone$"var_name_rv"
    settings_df <- the_list03_SpecialSettigns_stone$"df_order"
    vector_ordered_levels <- the_list03_SpecialSettigns_stone$"vector_ordered_levels"
    vector_ordered_colors <- the_list03_SpecialSettigns_stone$"vector_ordered_colors"

    #################################
    df_rv_position_levels <- data.frame(
      "order_level"  = 1:nlevels(minidataset[,var_name_factor]),
      "level" = levels(minidataset[,var_name_factor]),
      "n"            = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], length),
      "variable"     = rep(var_name_rv, nlevels(minidataset[,var_name_factor])),
      "min"          = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], min),
      "mean"         = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], mean),
      "Q1"           = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], quantile, 0.25),
      "median"       = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], median),
      "Q3"           = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], quantile, 0.75),
      "max"          = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], max),
      "color" = vector_ordered_colors,
      stringsAsFactors = FALSE
    )
    df_rv_position_levels[,"level"] <- factor(
      x = df_rv_position_levels[,"level"],       # La variable original de factor
      levels = df_rv_position_levels[,"level"]  # El orden de los niveles que calculamos en el Paso 2
    )
    rownames(df_rv_position_levels) <- NULL

    df_table_factor_plot004 <- df_rv_position_levels
    ########################################################
    # --- CÓDIGO CLAVE DE LA SOLUCIÓN ---
    # 1. Crear el vector de números de fila secuenciales (1, 2, 3, ...)
    row_sequence <- 1:nrow(minidataset)

    # 2. Formatear el texto para el cursor (Ej: "Row: 1", "Row: 2", ...)
    hover_text <- paste0("Row: ", row_sequence)
    # ------------------------------------

    # # # New plotly...
    plot004_factor <- plotly::plot_ly()

    # # # Boxplot and info...
    plot004_factor <- plotly::add_trace(p = plot004_factor,
                                        type = "box",
                                        x = df_table_factor_plot004$level ,
                                        color = df_table_factor_plot004$level,
                                        colors = df_table_factor_plot004$color,
                                        lowerfence = df_table_factor_plot004$min,
                                        q1 = df_table_factor_plot004$Q1,
                                        median = df_table_factor_plot004$median,
                                        q3 = df_table_factor_plot004$Q3,
                                        upperfence = df_table_factor_plot004$max,
                                        boxmean = TRUE,
                                        boxpoints = FALSE,
                                        line = list(color = "black", width = 3)
    )

    # # # Title and settings...
    # plot004_factor <- plotly::layout(p = plot004_factor,
    #                                  title = "Plot 004 - Boxplot and means",
    #                                  font = list(size = 20),
    #                                  margin = list(t = 100))


    # # # Without zerolines...
    plot004_factor <- plotly::layout(p = plot004_factor,
                                    xaxis = list(zeroline = FALSE,
                                                 title = var_name_factor),
                                    yaxis = list(zeroline = FALSE,
                                                 title = var_name_rv),
                                     font = list(size = 20))

    # # # Output plot004_anova...
    plot004_factor

    # # Crear un nuevo plot
    # plot001_factor <- plotly::plot_ly()
    #
    # # Scatter plot
    # plot001_factor <- plotly::add_trace(p = plot001_factor,
    #                                     type = "scatter",
    #                                     mode = "markers",
    #                                     x = minidataset[,var_name_factor],
    #                                     y = minidataset[,var_name_rv],
    #                                     color = minidataset[,var_name_factor],
    #                                     colors = settings_df$color,
    #
    #                                     # *********************************
    #                                     # 1. ASIGNAR EL TEXTO DEL CURSOR:
    #                                     # Usamos el nuevo vector secuencial y formateado
    #                                     text = hover_text,
    #
    #                                     # 2. CONFIGURAR EL CONTENIDO DEL CURSOR:
    #                                     # Mantenemos 'text+x+y+name' para incluir el nuevo texto
    #                                     hoverinfo = 'text+x+y+name',
    #                                     # *********************************
    #
    #                                     marker = list(size = 15, opacity = 0.7))
    #
    # # Título y settings
    # # plot001_factor <- plotly::layout(p = plot001_factor,
    # #                                  title = "Scatterplot",
    # #                                  font = list(size = 20),
    # #                                  margin = list(t = 100))
    #
    # # Sin zerolines
    # plot001_factor <- plotly::layout(p = plot001_factor,
    #                                  xaxis = list(zeroline = FALSE, title = var_name_factor),
    #                                  yaxis = list(zeroline = FALSE, title = var_name_rv),
    #                                  font = list(size = 20))
    #
    # # El bloque renderPlotly debe devolver el objeto Plotly al final
    # plot001_factor
  })
  ###---------------------------------------------------------------------------
  output$"output_side_panel" <- renderUI({

    str_style_btn <- "font-size: 65px; display: block; margin-bottom: 8px;"

    div(
      # style = "overflow-y: hidden; flex: 1; display: flex; flex-direction: column; min-height: 100%;",
      #
      # class = "d-flex flex-column align-items-center",
      card(
        # style = "height: 100%;",  # Altura de la card (100% del contenedor padre)
        style = "height: 72vh; min-height: 72vh;",  # Altura de la card (100% del contenedor padre)

        actionButton(
          inputId = "btn_classroom",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            #icon("database", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            icon("chalkboard-user", style = str_style_btn),
            #span("Dataset")
          ),
          class = "btn-success", #"btn-warning", #"btn-primary",
          #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
          title = ""
        ),
        # uiOutput("botonera_html"),
        br(),
        actionButton(
          inputId = "btn_general_download",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            icon("download", style = str_style_btn),
            #span("Dataset")
          ),
          class = "btn-warning", #"btn-primary",
          #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
          title = ""
        )
      )
    )
  })
  ###---------------------------------------------------------------------------

  output$"the_super_main" <- renderUI({
       div(
         style = "width: 100%; height: 100%;",
      # style = "height: 90vh; width: 100%; overflow: hidden; display: flex; flex-direction: column;",
      conditionalPanel(
        condition = "input.toggle01_classroom == false", # ClassRoom
        uiOutput("main_classroom_general")
      ),
      conditionalPanel(
        condition = "input.toggle01_classroom == true", # Data Analysis...
          conditionalPanel(
            condition = "input.toggle02_input == false", # Input
            #ns = ns,
            uiOutput("main_input_general")
          ),
          conditionalPanel(
            condition = "input.toggle02_input == true", # Output...
              conditionalPanel(
                condition = "input.toggle03_showroom == false", # Show
                #ns = ns,
                # uiOutput("main_output_general")
                uiOutput("main_output_01_html_report")
              ),
              conditionalPanel(
                condition = "input.toggle03_showroom == true", # Download
                #ns = ns,
                uiOutput("main_output_02_html_report")

              )

          )
        )
      )

  })

  output$"final_info" <- renderUI({
    tags$p(
      "Rscience 1.0.19 - General Linear Model - Fixed Effects - Balanced tratments - Anova - Anova 1 Way - Script 01",
      style = paste(
        "color: #1E88E5;",                  # 🟦 Color de la letra (azul vibrante)
        "font-family: 'Arial Black', sans-serif;",  # ✒️ Tipo de letra
        "font-size: 18px;",                 # 📏 Tamaño de la letra
        "font-weight: bold;",               #  মোটা En negrita (alternativa a 'class = "fw-bold"')
        "background-color: #FFFDE7;",       # 💡 Color de resaltado/fondo (amarillo pálido)
        "padding: 5px;"                     # Espacio alrededor del texto dentro del fondo
      )
    )
  })

  output$"df_my_dataset" <- renderTable({
    the_list01_Dataset_stone$"my_dataset"
    # the_list01_Dataset_internal()$"my_dataset"
  })

  output$"df_my_minidataset" <- renderTable({
    the_list02_VarSelection_stone$"minidataset"
  })


  output$"main_input_general" <- renderUI({

    div(
      style = "height: 100%; width: 100%;",

      tags$div(
        class = "fill",
        style = "height: 100%; width: 100%;",

        bslib::navset_card_tab(
          title = h4("Inputs"),
          height = "100%",  # ← Esto es clave para bslib
          # 1. user_selection
          bslib::nav_panel(
            title = "user_selection",
            style = "height: 100%; width: 100%;",
            bslib::card_body(
              fillable = TRUE,
              style = "height: 100%; width: 100%; padding: 0;",
              tags$div(
                style = "display: flex; flex-direction: column; height: 100%; width: 100%; gap: 15px; padding: 15px;",  # ← Padding general y gap

                # 20% - Primera sección con padding
                div(
                  style = "flex: 0 0 20%; min-height: 0; display: flex; flex-direction: column; padding: 10px;",  # ← Padding interno
                  div(
                    style = "flex: 1; min-height: 0; overflow-y: auto; overflow-x: hidden;",
                    fn_infoUI_zocalo_01_dataset(data_obj = reactiveValuesToList(the_list01_Dataset_stone))
                  )
                ),

                # 20% - Segunda sección con padding
                div(
                  style = "flex: 0 0 20%; min-height: 0; display: flex; flex-direction: column; padding: 10px;",  # ← Padding interno
                  div(
                    style = "flex: 1; min-height: 0; overflow-y: auto; overflow-x: hidden;",
                    fn_infoUI_zocalo_02_VarSelection(data_obj = reactiveValuesToList(the_list02_VarSelection_stone))
                  )
                ),

                # 60% - Tercera sección con padding
                div(
                  style = "flex: 0 0 50%; min-height: 0; display: flex; flex-direction: column; padding: 10px;",  # ← Padding interno
                  fn_infoUI_zocalo_03_container(
                    data_obj = reactiveValuesToList(the_list03_SpecialSettigns_stone),
                    width = "100%",
                    height = "100%"
                  )
                )
              )
            )
          ),

          # 2. dataset
          bslib::nav_panel(
            title = "dataset",
            style = "height: 100%; width: 100%;",
            bslib::card_body(
              fillable = TRUE,
              style = "height: 100%; width: 100%;",
              h4("Dataset"),
              tableOutput("df_my_dataset")
            )
          ),

          # 3. minidataset
          bslib::nav_panel(
            title = "minidataset",
            # style = "height: 100%; width: 100%;",
            bslib::card_body(
              fillable = TRUE,
              # style = "height: 100%; width: 100%;",
              h4("minidataset"),
              tableOutput("df_my_minidataset")
            )
          ),

          # 4. control
          bslib::nav_panel(
            title = "control",
            style = "height: 100%; width: 100%;",
            bslib::card_body(
              fillable = TRUE,
              style = "height: 100%; width: 100%;",
              h4("Control"),
              tags$div(
                # style = "flex-grow: 1; overflow-y: auto;",
                # style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente

                "- Original vs. Filtered Row Count.", br(),
                "- Rows Removed Due to Missing Data (NA) in selected columns.", br(),
                "- Min/Max by Factor Level for the Response Variable (RV)", br(),

                tags$hr(style = "border-top: 3px solid #000000;"),
                tags$div(
                  # Aplicamos Flexbox para control vertical
                  # style = "display: flex; flex-direction: column; height: 60vh; overflow-y: auto; padding: 10px;",

                  # Elementos que deben fluir
                  DT::DTOutput("df_control01"),

                  tags$hr(style = "border-top: 3px solid #000000;"),

                  DT::DTOutput("df_control02"),

                  tags$hr(style = "border-top: 3px solid #000000;")#,

                  # Aseguramos que el Plotly tenga un alto que respete el contenedor
                  # plotlyOutput por defecto puede ser muy alto o tener un alto fijo.
                  # plotly::plotlyOutput("control03_plotly", height = "600px") # Dale un alto inicial manejable
                )
              )
            )
          )
        )
      )
    )
  })

  output$"main_output_general" <- renderUI({
    # Contenido de la primera opción, inicialmente oculto
    # El tabsetPanel actúa como contenedor principal para las diferentes vistas.
    tabsetPanel(
      id = "panel_principal", # ID que usaremos para controlar qué pestaña está activa
      type = "hidden",        # ¡CLAVE! Oculta las pestañas/navegación de Shiny

      # Tab 1: Contenido de ClassRoom
      tabPanel(
        value = "tab_classroom", # El valor que usaremos en el servidor para activar esta pestaña
        title = "ClassRoom",
        uiOutput("main_output_01_html_report")
      ),

      # Tab 2: Contenido de Descarga
      tabPanel(
        value = "tab_descarga", # El valor que usaremos en el servidor para activar esta pestaña
        title = "Descarga",
        uiOutput("main_output_02_html_report")
      ),

      # Tab 3: Mensaje Inicial/Default
      tabPanel(
        value = "tab_inicial", # El valor por defecto al inicio
        title = "Inicial",
        p("Seleccione una opción.")
      ),

      # Puedes añadir otras pestañas aquí
      # tabPanel(value = "otra_tab", title = "Otra", ...)
    )
  })

  output$"main_classroom_general" <- renderUI({
    # titlePanel("Gestor de Archivos con Estado Persistente (INPUT)"),


    # str_style_NAV_PANEL <- "flex-grow: 1; overflow-y: auto; height: 74vh; width: 100%;"
    str_style_NAV_PANEL <- "flex-grow: 1; overflow-y: auto; height: 72vh; width: 100%; overflow: hidden;"

    bslib::navset_card_tab(
      # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
      title = tags$div(
        style = "
        min-height: 10px;
        padding-top: 0px;      /* ↑ Arriba */
        padding-right: 0px;    /* → Derecha */
        padding-bottom: 0px;   /* ↓ Abajo */
        padding-left: 0px;     /* ← Izquierda */
      ",
        tags$h4("ClassRoom"),
      ),
      # title =
      # div(
      # style = "height: 90vh; width: 100%; overflow: hidden;", # Asegurar que el contenedor tenga altura suficiente

      bslib::nav_panel(
        title = "theory",
        fluidRow(
          column(2, h4("Theory")),
          column(9),
          column(1,
                 actionButton(inputId = "open01",
                              label = NULL,
                              icon = icon("binoculars", class = "fa-2x"),
                              class = "btn-warning btn-sm"))
        ),
        tags$div(
          # style = "flex-grow: 1; overflow-y: auto;",
          style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente
          # p("Mostramos la selección... (Este texto es mínimo, pero el contenedor ocupa el 90vh completo.)"),
          # fn_infoUI_zocalo_dataset(data_obj = the_list01_Dataset_internal()),
          # fn_infoUI_zocalo_01_dataset(data_obj = the_list01_Dataset_show()),
          htmlOutput("html_01_anova_intro")
        )


      ),
      bslib::nav_panel(
        title = "Tukey",
        h4("Tukey"),
        tags$div(
          # style = "flex-grow: 1; overflow-y: auto;",
          style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente
          htmlOutput("html_02_tukey")
        )
      ),
      bslib::nav_panel(
        title = "Decision Making",
        h4("Decision Making"),
        tags$div(
          # style = "flex-grow: 1; overflow-y: auto;",
          style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente
          htmlOutput("html_03_decision_making")
        )
      ),
      bslib::nav_panel(
        title = "ASA",
        h4("ASA"),
        tags$div(
          # style = "flex-grow: 1; overflow-y: auto;",
          style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente
          htmlOutput("html_04_ASA")
        )
      ),

    )

    # )


    # [CAMBIO APLICADO] Utilizamos tags$div para envolver y aplicar el estilo de altura y ancho.
    # tags$div(
    #   style = "height: 90vh; width: 100%; overflow: hidden; display: flex; flex-direction: column;",
    #   bslib::navset_card_tab(
    #     # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
    #
    #     title = 'Input',
    #
    #     bslib::nav_panel(
    #       title = "user_selection",
    #       # El CSS ahora fuerza a este contenedor (tab-pane.active) a llenar el 100%
    #       # del espacio disponible (90vh - encabezado de la tarjeta).
    #       p("Mostramos la selección... (Este texto es mínimo, pero el contenedor ocupa el 90vh completo.)")
    #     ),
    #     bslib::nav_panel(
    #       title = "dataset",
    #       "Mostramos el dataset..."
    #     )
    #   )
    # )
  })
  ###---------------------------------------------------------------------------
  # Lo inicializamos en NULL o con el ID del botón que quieres activo por defecto.
  # Usaremos "btn_classroom" como valor inicial.
  # last_btn_clicked <- reactiveVal("btn_classroom")

  # last_btn_clicked <- reactiveVal(NULL)
  # last_btn_clicked mantendrá un registro del botón activo
  last_btn_clicked <- reactiveVal(NULL)

  # ------------------------------------------------------------------
  # 1. Inicialización (Disparar la configuración visual/tab UNA VEZ)
  # ------------------------------------------------------------------
  observeEvent(session, {
    # Establece el valor inicial, disparando el observador principal.
    last_btn_clicked("btn_classroom")
    message("✅ Inicialización forzada completada: btn_classroom establecido.")

  }, once = TRUE)
  # observeEvent(session, {
  #   # Cambia el valor de NULL a "btn_classroom".
  #   # Esto forzará el disparo de cualquier otro observador
  #   # (como el de visibilidad) que dependa de last_btn_clicked().
  #   last_btn_clicked("btn_classroom")
  #   message("Valor inicial de last_btn_clicked establecido.")
  # }, once = TRUE)
  # ------------------------------------------------------------------
  # 2. Observar los Clicks de los Botones
  # ------------------------------------------------------------------

  # ------------------------------------------------------------------
  # 2. Observar los Clicks de los Botones (Actualiza el reactiveVal)
  # ------------------------------------------------------------------

  # Observar el botón ClassRoom
  observeEvent(input$btn_classroom, {
    if (isolate(last_btn_clicked()) != "btn_classroom") {
      last_btn_clicked("btn_classroom")
      message("Botón activo: btn_classroom")
    }
  })

  # Observar el botón de Descarga
  observeEvent(input$btn_general_download, {
    if (isolate(last_btn_clicked()) != "btn_general_download") {
      last_btn_clicked("btn_general_download")
      message("Botón activo: btn_general_download")
    }
  })

  # En server.R
  # ------------------------------------------------------------------
  # 3. Lógica de Cambio de Vista y Estilo (Reacciona al reactiveVal)
  # ------------------------------------------------------------------
  observeEvent(last_btn_clicked(), {
    print(last_btn_clicked())
    active_btn <- last_btn_clicked()
    req(active_btn) # Asegura que active_btn no sea NULL

    target_tab <- NULL

    # Mapear el valor del botón al valor de la pestaña (value) y actualizar estilos
    if (active_btn == "btn_classroom") {
      target_tab <- "tab_classroom"

      # Actualizar estilos de los botones (success=activo, warning=inactivo)
      # shinyjs::removeClass(id = "btn_general_download", class = "btn-success")
      # shinyjs::addClass(id = "btn_general_download", class = "btn-warning")
      #
      # shinyjs::removeClass(id = "btn_classroom", class = "btn-warning")
      # shinyjs::addClass(id = "btn_classroom", class = "btn-success")

    } else if (active_btn == "btn_general_download") {
      target_tab <- "tab_descarga"

      # Actualizar estilos de los botones
      # shinyjs::removeClass(id = "btn_classroom", class = "btn-success")
      # shinyjs::addClass(id = "btn_classroom", class = "btn-warning")
      #
      # shinyjs::removeClass(id = "btn_general_download", class = "btn-warning")
      # shinyjs::addClass(id = "btn_general_download", class = "btn-success")

    } else {
      # Valor por defecto (si es desconocido)
      target_tab <- "tab_inicial"
    }

    # 🌟 Actualizar el tabsetPanel (Cambia la vista sin perder el estado)
    updateTabsetPanel(session,
                      inputId = "panel_principal",
                      selected = target_tab)

  }, ignoreNULL = FALSE) # Ejecuta en el cambio inicial de NULL

  observeEvent(input$"panel_principal", {
    # print(last_btn_clicked())
    # active_btn <- last_btn_clicked()
    # req(active_btn) # Asegura que active_btn no sea NULL

    target_tab <- input$"panel_principal"

    # Mapear el valor del botón al valor de la pestaña (value) y actualizar estilos
    if (target_tab == "tab_classroom") {

      # Actualizar estilos de los botones (success=activo, warning=inactivo)
      shinyjs::removeClass(id = "btn_general_download", class = "btn-success")
      shinyjs::addClass(id = "btn_general_download", class = "btn-warning")

      shinyjs::removeClass(id = "btn_classroom", class = "btn-warning")
      shinyjs::addClass(id = "btn_classroom", class = "btn-success")

    } else if (target_tab == "tab_descarga") {


      # Actualizar estilos de los botones
      shinyjs::removeClass(id = "btn_classroom", class = "btn-success")
      shinyjs::addClass(id = "btn_classroom", class = "btn-warning")

      shinyjs::removeClass(id = "btn_general_download", class = "btn-warning")
      shinyjs::addClass(id = "btn_general_download", class = "btn-success")

    } else {
      # Valor por defecto (si es desconocido)
      target_tab <- "tab_inicial"
    }

    # 🌟 Actualizar el tabsetPanel (Cambia la vista sin perder el estado)
    # updateTabsetPanel(session,
    #                   inputId = "panel_principal",
    #                   selected = target_tab)

  }, ignoreNULL = TRUE) # Ejecuta en el cambio inicial de NULL
  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------

  output$"botonera_html" <- renderUI({
    # div(
    #   class = "d-flex flex-column align-items-center",


    fluidRow(
      # Usar una columna para contener todos los botones
      # 'width = 12' ocupa todo el ancho de la fila
      column(width = 12,
             # Los botones ahora se alinearán horizontalmente por defecto,
             # especialmente si separamos las llamadas a 'br()'
             actionButton(inputId = "btn_play_html",
                          label = NULL,
                          icon = icon("play", class = "fa-2x"),
                          class = "btn-warning btn-sm"),

             downloadButton(outputId = "btn_download_html",
                            label = NULL,
                            icon = icon("download", class = "fa-2x"),
                            class = "btn-warning btn-sm"),

             actionButton(inputId = "btn_open_html",
                          label = NULL,
                          icon = icon("binoculars", class = "fa-2x"),
                          class = "btn-warning btn-sm")
             # **Importante:** Quitamos todos los 'br()' que causaban los saltos de línea.
      )
    )
    # )
  })

  output$"main_output_01_html_report" <- renderUI({
    bslib::card(
      id = "output-main-card",

      # [CAMBIO] Usamos bslib::card_header() para forzar el título.
      bslib::card_header(
        style = "height: 60px; overflow: hidden;",
        fluidRow(
          column(3, tags$h4("Output - ShowRoom")),
          column(7),
          column(2, uiOutput("botonera_html"))
        )
      ),

      card_body(
        class = "p-0",
        tags$div(
          # style = "flex-grow: 1; overflow-y: auto;",
          style = "flex-grow: 1; overflow-y: auto; height: 82vh; width: 100%; overflow: hidden;", # Asegurar que el contenedor tenga altura suficiente

          # Contenido que deseas mostrar dentro de la tarjeta
          htmlOutput("html_viewer")
        )
      )
    )





  })

  output$"main_output_02_html_report" <- renderUI({
    div(
      # shiny::titlePanel("Gestor de Archivos con Estado Persistente (OUTPUT)"),
      bslib::navset_card_tab(
        # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
        title = tags$h4("Output - Download"),

        bslib::nav_panel(
          title = "nueva_descarga",
          # uiOutput("special01"),
          mod_download_ui("report_html", "File 01 - html full report"),
          mod_download_ui("report_pdf",  "File 02 - PDF medium report"),
          mod_download_ui("report_docx", "File 03 - Word medium report"),
          mod_download_ui("report_xlsx", "File 04 - Excel medium report"),
          mod_download_ui("report_zip_png", "File 05 - PNG files on zip folder"),
          mod_download_ui("reveal_html", "File 06 - Reveal HTML Presentation")


        ),
        bslib::nav_panel(
          title = "folder_files",
          "Despues aca el path y los files."
        ),

        bslib::nav_panel(
          title = "PDF",

          # Usamos layout_columns para dividir el espacio
          layout_columns(
            col_widths = c(4, 4, 4), # Columna Izquierda (4 unidades), Columna Derecha (8 unidades)

            # === Columna Izquierda: Botones (4/12 del ancho) ===
            div(
              # 1. Botón Generar (Inicio: Naranja)
              actionButton("generar", "1. Generar Carpeta y Archivo Temporal", class = "btn-warning"),
              br(), br(),
              # 2. Botón Descargar (Inicio: Naranja)
              downloadButton("descargar", "2. Descargar Archivo PDF", class = "btn-warning")
              # Los br() ya no son necesarios dentro de una columna separada
            ),

            # === Columna Derecha: Output y Lista (8/12 del ancho) ===
            div(
              h2("Output folder path:"),
              uiOutput("text_output_folder_path01"),
              h2("List Files:"), # Tienes este h2 repetido, asegúrate de que sea intencional
              verbatimTextOutput("text_list_files01"),
              br()
            ),
            div(uiOutput("pdf_viewer"))
          )
        )
      )
    )
  })






  ##############################################
  output$my_action_button <- renderUI({

    # selected_opt <- switch(button_state,
    #                        "initial"   = "btn-primary",    # Azul inicial
    #                        "confirmed" = "btn-success",    # Verde después de confirmar
    #                        "error"     = "btn-danger")
    btn_class <-  "btn-primary"#fn_R_switch_class_from_button_state(button_state = button_state())


  })


  ##############################################################################

  # 01 - PDF
  output$text_output_folder_path01 <- renderText({
    req(str_output_folder01())
    str_output_folder01()
  })
  output$text_list_files01 <- renderText({
    req(str_output_folder01())

    # Obtiene el vector de nombres de archivos
    files_list <- list.files(path = str_output_folder01(), recursive = TRUE)

    # Concatena los nombres de los archivos separados por un salto de línea (\n)
    # y devuelve una única cadena de texto
    paste(files_list, collapse = "\n")
  })

  #############################################################33

  str_file_name_input_qmd <- reactive({"report_template_pdf.qmd"})
  str_file_path_input_qmd <- reactive({


    str_path_qmd <- file.path(str_input_folder_quarto(), str_file_name_input_qmd())
    str_path_qmd
  })

  ##################################################################
  str_output_folder01 <- reactiveVal(NULL)
  str_output_file_name_pdf    <- reactiveVal(NULL)
  str_output_file_path_pdf    <- reactiveVal(NULL)
  the_time_here_format        <- reactiveVal(NULL)
  # --- Lógica del Botón "Generar" (Naranja -> Verde) ---
  # Asegúrate de que shinyjs::useShinyjs() esté en tu UI
  # Asegúrate de que shinyjs::useShinyjs() esté en tu UI
  observeEvent(input$generar, {

    # 1. INICIALIZACIÓN: Crear el objeto de progreso y bloquear la pantalla

    progress <- Progress$new(session, min = 0, max = 1)

    # Modal inicial con la barra de progreso integrada
    showModal(modalDialog(
      id = "processing_modal",
      title = tags$div(
        tags$i(class = "fa fa-cog fa-spin fa-1x"), # Spinner en el título
        " Rscience Proccesing Data..."
      ),
      tagList(
        tags$div(id = "modal_content",

                 # Inicialmente un spinner grande.

                 tags$p(tags$b("Proccesing state:"), tags$span(id = "ID_progress_message", "Initializing...")),
                 tags$p(tags$i(tags$span(id = "ID_progress_detail", ""))),

                 # Barra de progreso: style="height: 30px;" para hacerla más gruesa
                 tags$div(class = "progress", style = "height: 30px;",
                          tags$div(id = "ID_progress_bar",
                                   class = "progress-bar progress-bar-striped active",
                                   role = "progressbar",
                                   style = "width: 0%;")),
                 br(),
                 # Contenedor del check/spinner que vamos a manipular
                 tags$div(id = "ID_my_check",
                          style = "text-align: center; height: 200px;",
                          tags$i(class = "fa fa-spinner fa-spin fa-6x")) # Spinner inicial
        )
      ),
      easyClose = FALSE,
      footer = NULL
    ))

    # 2. FUNCIÓN DE ACTUALIZACIÓN PERSONALIZADA (JS + R)
    FN_update_modal_progress <- function(value, message, detail = "") {
      progress$set(value = value, message = message, detail = detail)

      # Lógica JavaScript para actualizar la UI del modal
      percentage <- round(value * 100)

      shinyjs::runjs(
        paste0(
          'document.getElementById("ID_progress_message").innerHTML = "<b>', message, '</b>";',
          'document.getElementById("ID_progress_detail").innerHTML = "', detail, '";',
          'document.getElementById("ID_progress_bar").style.width = "', percentage, '%";'
        )
      )
    }

    # Definición de la función de creación de carpeta (se mantiene)
    create_new_temporal_output_folder_path <- function(){
      my_temp_folder <- tempdir()
      the_sys_time <- Sys.time()
      timestamp_format <- format(the_sys_time, "%Y%m%d_%H%M%S")
      the_time_here_format(timestamp_format)

      new_sub_folder <- paste0("temp_", timestamp_format)
      nueva_carpeta <- file.path(my_temp_folder, new_sub_folder)
      return(nueva_carpeta)
    }

    # 3. MANEJO DEL FLUJO CON tryCatch (Avanzando paso a paso)
    tryCatch({

      # === PASOS INTERMEDIOS (Se mantienen iguales) ===
      FN_update_modal_progress(value = 0.05, message= "Inicializando", detail = "Preparando variables y entorno...")

      # 1. Crear carpeta temporal (10%)
      FN_update_modal_progress(0.10, "Preparación de archivos", detail = "Creando carpeta temporal de trabajo...")
      my_output_folder01 <- create_new_temporal_output_folder_path()
      str_output_folder01(my_output_folder01)
      dir.create(my_output_folder01, recursive = TRUE)

      # 2. Copiar archivos (25%)
      FN_update_modal_progress(0.25, "Preparación de archivos", detail = "Copiando plantillas y dependencias...")
      fs::dir_copy(
        path = str_input_folder_quarto(),
        new_path = str_output_folder01(),
        overwrite = T
      )



      # 3. Definir rutas (40%)
      FN_update_modal_progress(0.40, "Preparación de archivos", detail = "Calculando rutas y nombres de archivo...")
      file_name_no_ext <- tools::file_path_sans_ext(str_file_name_input_qmd())
      str_pdf_file_name <- paste0(file_name_no_ext,"_", the_time_here_format(), ".pdf")
      str_output_file_name_pdf(str_pdf_file_name)
      my_str_pdf <- file.path(str_output_folder01(), str_output_file_name_pdf())
      str_output_file_path_pdf(my_str_pdf)

      # 4. Configurar entorno de renderizado (50%)
      FN_update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")
      dir_original <- getwd()
      my_temporal_folder <- str_output_folder01()
      setwd(my_temporal_folder)

      # 5. Llamada BLOQUEANTE (50% -> 90%)
      FN_update_modal_progress(0.55, "Renderizando Quarto", detail = "Ejecutando el renderizado (puede tardar)...")

      quarto::quarto_render(input = str_file_name_input_qmd(),
                            output_format = "typst",
                            output_file = str_output_file_name_pdf(),
                            execute_params = list(activate_params: "TRUE",
                                                  file_name = "mtcars",
                                                  file_source = "r_source",
                                                  var_name_rv = "mpg",
                                                  var_name_factor = "cyl",
                                                  alpha_value = the_list02_VarSelection_internal()$"alpha_value", #0.05",
                                                  vector_ordered_levels = c("6", "4", "8"),
                                                  vector_ordered_colors = c("#000000", "#00FF00", "#0000FF"),
                                                  current_time = "R_outside",
                                                  script_used = "R_outside",
                                                  the_package = "R_outside",
                                                  tool_used = "R_outside"),
                            #execute_params = my_bag,
                            quiet = FALSE)

      setwd(dir_original)

      # 6. Progreso tras el bloqueo (90%)
      FN_update_modal_progress(0.90, "Renderizando Quarto", detail = "Renderizado completado. Finalizando...")


      # === PASO C: Finalización Exitosa (90% - 100%) ===

      # C1. Terminar barra de progreso al 100%
      FN_update_modal_progress(1.0, "¡Proceso Completado!", detail = "Éxito al generar el reporte.")

      # C2. Actualizar estado y color del botón
      removeClass("generar", "btn-warning")
      addClass("generar", "btn-success")

      output$mensaje_estado <- renderText({
        "¡Carpeta y archivo creados exitosamente! El Botón 1 está en verde. Listo para la descarga."
      })
      message(crayon::green("Process completed!"))

      # ----------------------------------------------------
      # 🟢 C3. CAMBIAR EL MODAL A CHECK DE ÉXITO (CORRECCIÓN FINAL) 🟢
      # ----------------------------------------------------

      shinyjs::runjs(
        'document.getElementById("ID_my_check").innerHTML =
      "<i class=\\"fa fa-check-circle fa-6x\\" style=\\"color: #4CAF50;\\"></i>";

   document.getElementById("ID_progress_message").innerHTML =
      "<b>Reporte Generado Exitosamente</b>";
   document.getElementById("ID_progress_detail").innerHTML =
      "Cerrando la ventana en 3 segundos...";

   document.getElementById("ID_progress_bar").classList.remove("active");

   document.getElementById("ID_progress_bar").style.width = "100%";
  '
      )

      # C4. Esperar 3 segundos para confirmación visual
      Sys.sleep(3)

    }, error = function(e) {
      # 🛑 MANEJO DE ERRORES:
      warning("Error al renderizar Quarto: ", e$message)

      # Cierra el modal de proceso
      removeModal()

      # Muestra un modal de error
      showModal(modalDialog(
        title = "⚠️ Error de Renderizado",
        paste("Ha ocurrido un error. Consulte la consola de R para más detalles. Mensaje:", e$message),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))

      return(NULL)
    }, finally = {
      # 4. LIMPIEZA: Cerrar el objeto de progreso de R siempre
      progress$close()
    })

    # 5. 🟢 DESBLOQUEAR LA PANTALLA
    removeModal()
    message("")
  })


  output$btn_export_pdf <- downloadHandler(
    filename = str_output_file_name_pdf(),
    content = function(file) {
      # quarto::quarto_render(
      #   input = str_file_path_input_qmd(),
      #   execute_params = list(species = input$in_species)
      # )
      fs::file_copy(
        str_output_file_path_pdf(),
        file
      )
    }
  )






  # --- Lógica del Botón "Descargar" ---
  output$descargar <- downloadHandler(

    filename = function() {
      la_ruta <- str_output_file_name_pdf()

      if (!is.null(la_ruta)) {
        basename(la_ruta)
      } else {
        "archivo_vacio.pdf"
      }
    },


    content = function(file) {
      archivo_a_descargar <- str_output_file_path_pdf()

      if (!is.null(archivo_a_descargar) && file.exists(archivo_a_descargar)) {

        # 1. CAMBIO DE COLOR B2: Naranja -> Verde (Persistente)
        runjs("
                    // Quitamos la clase temporal 'disabled' si la puso el navegador
                    $('#descargar').removeClass('disabled');
                    $('#descargar').removeClass('btn-warning');
                    $('#descargar').addClass('btn-success');

                    // IMPORTANTE: NO SE RESTABLECE EL BOTÓN 1 A NARANJA AQUÍ.
                    // AMBOS BOTONES PERMANECERÁN VERDES.
                ")

        fs::file_copy(archivo_a_descargar, file)

      } else {
        warning("No se encontró el archivo temporal para descargar.")
        writeLines("Error: Archivo no generado.", file)
      }
    }
  )

  # Lado del Servidor

  output$pdf_viewer <- renderUI({
    # 1. Asegúrate de que el path exista (o espera a que el PDF se genere)
    req(str_output_file_path_pdf())

    pdf_path <- str_output_file_path_pdf()

    # *** VERIFICACIÓN CRUCIAL: Asegúrate de que el archivo exista ***
    if (!file.exists(pdf_path)) {
      return(p("Error: El archivo PDF aún no se ha generado o no se encuentra."))
    }

    pdf_dir <- dirname(pdf_path)
    pdf_filename <- basename(pdf_path)

    # 2. DEFINIR UN NOMBRE ÚNICO PARA EL RECURSO TEMPORAL
    resource_id <- digest::digest(pdf_dir, algo = "md5")

    # 3. REGISTRAR EL RECURSO
    shiny::addResourcePath(resource_id, pdf_dir)

    # 4. Construir la URL con el ID único del recurso
    pdf_url <- file.path(resource_id, pdf_filename)

    # 5. Crear el iframe con dimensiones más pequeñas
    tags$iframe(
      # CAMBIOS AQUÍ: Reducción de height y width
      style = 'height: 400px; width: 100%; border: none;',
      src = pdf_url,
      type = "application/pdf"
    )
  })

  #####################################################



  ANCESTRAL_PLAY <- reactiveVal(FALSE)
  observeEvent(input$"btn_play_html", {

    ANCESTRAL_PLAY(TRUE)
  })
  observeEvent(input$"btn_play_front", {

    ANCESTRAL_PLAY(TRUE)
  })


  #################################################################################

  #################################################################################






  ####################################################
  TOTEM_special_paths <- reactiveValues()
  TOTEM_special_paths$"STR_REACTIVE_folder_path_package" <- NULL
  TOTEM_special_paths$"STR_REACTIVE_folder_path_quarto"  <- NULL
  TOTEM_special_paths$"getwd"  <- getwd()
  TOTEM_special_paths$"check"  <- FALSE

  STR_REACTIVE_folder_path_package <- reactive({

    find_my_folder_path_package <- function(){

      selected_package_path <- tryCatch(
        # Intenta ejecutar este código
        expr = {
          find.package(MY_PACKAGE_NAME)
        },
        # Si ocurre un error, ejecuta este código y devuelve su resultado
        error = function(e) {
          # El error de 'find.package' se dispara cuando no encuentra el paquete.
          # En ese caso, devolvemos getwd(), que es el path del archivo app.R
          # y lo recortamos para quedarnos en la subcarpeta del package.
          the_local_path <- strsplit(getwd(), MY_PACKAGE_NAME)
          the_local_path <-file.path(the_local_path[[1]][1], MY_PACKAGE_NAME, "inst")
          return(the_local_path)
        }
      )




      vector_folder_paths <- list.dirs(path = selected_package_path, recursive = T)
      dt_selected_quarto_folder <- grepl("quarto$", vector_folder_paths, ignore.case = TRUE)
      selected_quarto_folder_path <- vector_folder_paths[dt_selected_quarto_folder]

      #print(selected_quarto_folder_path)

      return(selected_quarto_folder_path)
    }
    find_my_folder_path_package()

  })
  STR_REACTIVE_folder_path_quarto <- reactive({

    selected_package_path <- STR_REACTIVE_folder_path_package()

    vector_folder_paths <- list.dirs(path = selected_package_path, recursive = T)
    dt_selected_quarto_folder <- grepl("quarto$", vector_folder_paths, ignore.case = TRUE)
    selected_quarto_folder_path <- vector_folder_paths[dt_selected_quarto_folder]

    return(selected_quarto_folder_path)



  })

  observeEvent(STR_REACTIVE_folder_path_package(), {
    req(STR_REACTIVE_folder_path_package())
    TOTEM_special_paths$"STR_REACTIVE_folder_path_package" <- STR_REACTIVE_folder_path_package()
  })

  observeEvent(STR_REACTIVE_folder_path_quarto(), {
    req(STR_REACTIVE_folder_path_quarto())
    TOTEM_special_paths$"STR_REACTIVE_folder_path_quarto" <- STR_REACTIVE_folder_path_quarto()
  })

  observeEvent(TOTEM_special_paths, {
    req(STR_REACTIVE_folder_path_quarto())
    check_01 <- !is.null(TOTEM_special_paths$"STR_REACTIVE_folder_path_package")
    check_02 <- !is.null(TOTEM_special_paths$"STR_REACTIVE_folder_path_quarto")
    check_03 <- !is.null(TOTEM_special_paths$"getwd")

    vector_check <- c(check_01, check_02, check_03)
    check_general <- sum(vector_check) == length(vector_check)
    if(check_general)   TOTEM_data_analysis$"check"   <- TRUE
  })

  #################################################################################
  TOTEM_input_folder_path <- reactiveValues()
  TOTEM_input_folder_path$"folder_path" <- NULL
  TOTEM_input_folder_path$"check"       <- NULL

  observe({
    req(!is.null(TOTEM_special_paths$"STR_REACTIVE_folder_path_quarto"))
    str_folder_path   <- TOTEM_special_paths$"STR_REACTIVE_folder_path_quarto"
    check_folder_path <- dir.exists(str_folder_path)

    TOTEM_input_folder_path$"folder_path" <- str_folder_path
    TOTEM_input_folder_path$"check"       <- check_folder_path

  })
  #################################################################################

  # Filename
  list_R_file_name_input_template <- list()
  list_R_file_name_output_report  <- list()

  list_R_file_name_input_template$"html" <- "report_template_01_html.qmd"
  list_R_file_name_output_report$"html"  <- "report_template_01_html.html"

  list_R_file_name_input_template$"pdf"  <- "report_template_02_pdf.qmd"
  list_R_file_name_output_report$"pdf"   <- "report_02_anova_1_way.pdf"

  list_R_file_name_input_template$"docx" <- "report_template_03_docx.qmd"
  list_R_file_name_output_report$"docx"  <- "report_03_anova_1_way.docx"

  list_R_file_name_input_template$"xlsx" <-  "report_template_04_excel.qmd"
  list_R_file_name_output_report$"xlsx"  <-  "report_04_anova_1_way.xlsx"

  list_R_file_name_input_template$"zip_png" <-  ""
  list_R_file_name_output_report$"zip_png"  <-  "png_plotly.zip"

  list_R_file_name_input_template$"reveal_html" <-  "report_template_05_reveal.qmd"
  list_R_file_name_output_report$"reveal_html"  <-  "report_05_anova_1_way.html"
  #################################################################################
  TOTEM_file_path_input_template <- reactiveValues()
  TOTEM_file_path_input_template$"html"  <- list("file_path" = "", "check" = "")
  TOTEM_file_path_input_template$"pdf"   <- list("file_path" = "", "check" = "")
  TOTEM_file_path_input_template$"docx"  <- list("file_path" = "", "check" = "")
  TOTEM_file_path_input_template$"xlsx"  <- list("file_path" = "", "check" = "")
  TOTEM_file_path_input_template$"reveal_html"  <- list("file_path" = "", "check" = "")

  TOTEM_file_path_input_template$"check_general" <- FALSE

  observe({
    req(TOTEM_input_folder_path$"check")
    input_folder_path <- TOTEM_input_folder_path$"folder_path"

    str_file_path_input_html <- file.path(input_folder_path, list_R_file_name_input_template$"html")
    TOTEM_file_path_input_template$"html"$"file_path"  <-  str_file_path_input_html
    TOTEM_file_path_input_template$"html"$"check"     <-  file.exists(str_file_path_input_html)

    str_file_path_input_pdf <- file.path(input_folder_path, list_R_file_name_input_template$"pdf")
    TOTEM_file_path_input_template$"pdf"$"file_path"  <-  str_file_path_input_pdf
    TOTEM_file_path_input_template$"pdf"$"check"     <-  file.exists(str_file_path_input_pdf)

    str_file_path_input_docx <- file.path(input_folder_path, list_R_file_name_input_template$"docx")
    TOTEM_file_path_input_template$"docx"$"file_path"  <-  str_file_path_input_docx
    TOTEM_file_path_input_template$"docx"$"check"     <-  file.exists(str_file_path_input_docx)

    str_file_path_input_xlsx <- file.path(input_folder_path, list_R_file_name_input_template$"xlsx")
    TOTEM_file_path_input_template$"xlsx"$"file_path"  <-  str_file_path_input_xlsx
    TOTEM_file_path_input_template$"xlsx"$"check"     <-  file.exists(str_file_path_input_xlsx)

    str_file_path_input_reveal_html <- file.path(input_folder_path, list_R_file_name_input_template$"reveal_html")
    TOTEM_file_path_input_template$"reveal_html"$"file_path"  <-  str_file_path_input_reveal_html
    TOTEM_file_path_input_template$"reveal_html"$"check"      <-  file.exists(str_file_path_input_reveal_html)

    vector_check <- c(TOTEM_file_path_input_template$"html"$"check",
                       TOTEM_file_path_input_template$"pdf"$"check",
                       TOTEM_file_path_input_template$"docx"$"check",
                       TOTEM_file_path_input_template$"xlsx"$"check",
                       TOTEM_file_path_input_template$"reveal_html"$"check")

    TOTEM_file_path_input_template$"check_general" <- sum(vector_check) == length(vector_check)

    if(TOTEM_file_path_input_template$"check_general") {
      print("TODO OK")
    }
  })
  # - Control Pre Play:
  # Debe tener todos los botones compeltos anteriores, y debe haber encontrado la carpeta
  # del package.
  # ----------------------------------------------------------------------------
# - Da clic en play
  #-----------------------------------------------------------------------------
  # - Abre el modal...
  # ----------------------------------------------------------------------------
  # - Toma la hora del sistema
  # - Crea la carpeta temporal nueva
  # ----------------------------------------------------------------------------
  # - Copia los archivos locales a la carpeta temporal
  # - Suplantar los elementos en el codigo del archvio .qmd
  # - Ejecutar y obtener HTML master
  # - Ejecutar y obtener pDF
  # - Ejecutar y obtener Word
  # - Ejecutar y obtener Excel
  # - Ejecutar y obtejer presentacion HTML
  # - Cierra el model...

  # -

  TOTEM_data_analysis <- reactiveValues()
  TOTEM_data_analysis$"step01" <- list("step_number" = 1, "str_summary" = "Play button pressed."           , "check" = FALSE, "status_info" = "Waiting...")
  TOTEM_data_analysis$"step02" <- list("step_number" = 2, "str_summary" = "Upgrade for modal."             , "check" = FALSE, "status_info" = "Waiting...")
  TOTEM_data_analysis$"step03" <- list("step_number" = 3, "str_summary" = "Open Modal."                    , "check" = FALSE, "status_info" = "Waiting...")
  TOTEM_data_analysis$"step04" <- list("step_number" = 4, "str_summary" = "Time and new temporal folder."  , "check" = FALSE, "status_info" = "Waiting...")
  TOTEM_data_analysis$"step05" <- list("step_number" = 5, "str_summary" = "Coping files."                  , "check" = FALSE, "status_info" = "Waiting...")

  # Step 01 - Clic on play -----------------------------------------------------
  step01 <- eventReactive(ANCESTRAL_PLAY(), {
    req(ANCESTRAL_PLAY())

    isolate({
      TOTEM_data_analysis$"step01"$"check"  <- TRUE
      TOTEM_data_analysis$"step01"$"status_info" <- "Done!"
    })

    js_code <- "
    // Función para cambiar los estilos de los botones
    function updateButtons() {
      // 1. Botón 'btn_play_html'
      var btn_html = document.getElementById('btn_play_html');
      if (btn_html) {
        btn_html.classList.remove('btn-warning');
        btn_html.classList.add('btn-success');
      }

      // 2. Botón 'btn_play_front'
      var btn_front = document.getElementById('btn_play_front');
      if (btn_front) {
        btn_front.classList.remove('btn-primary');
        btn_front.classList.add('btn-success');
      }
    }

    // Ejecutar inmediatamente por si ya existen
    updateButtons();

    // Observar cambios en el DOM por si se añaden después
    var observer = new MutationObserver(function(mutations) {
      updateButtons();
    });

    // Configurar y iniciar el observador
    observer.observe(document.body, {
      childList: true,
      subtree: true
    });

    // Opcional: dejar de observar después de 10 segundos
    setTimeout(function() {
      observer.disconnect();
    }, 10000);
  "

    shinyjs::runjs(js_code)

    print("step01 ---")
    return(list("check" = TOTEM_data_analysis$"step01"$"check"))
  })


  # Step 02 - Upgrade for modal ------------------------------------------------
  RVs_progress <- reactiveValues()
  RVs_progress$"progress_bar" <- NULL
  RVs_progress$"my_show_modal" <- NULL
  RVs_progress$"FN_update_modal_progress" <- NULL
  step02 <- eventReactive(step01(), {
    req(step01()$"check")

    # 1. INICIALIZACIÓN: Crear el objeto de progreso y bloquear la pantalla
    progress_bar <- shiny::Progress$new(session, min = 0, max = 1)

    my_show_modal <- showModal(modalDialog(
      id = "processing_modal",
      title = tags$div(
        tags$i(class = "fa fa-cog fa-spin fa-1x"),
        " Rscience Processing Data..."
      ),
      tagList(
        tags$div(id = "modal_content",
                 tags$p(tags$b("Processing state:"), tags$span(id = "ID_progress_message", "Initializing...")),
                 tags$p(tags$i(tags$span(id = "ID_progress_detail", ""))),
                 tags$div(class = "progress", style = "height: 30px;",
                          tags$div(id = "ID_progress_bar",
                                   class = "progress-bar progress-bar-striped active",
                                   role = "progressbar",
                                   style = "width: 0%;")),
                 br(),
                 tags$div(id = "ID_my_check",
                          style = "text-align: center; height: 200px;",
                          tags$i(class = "fa fa-spinner fa-spin fa-6x"))
        )
      ),
      easyClose = FALSE,
      footer = NULL
    ))


    FN_update_modal_progress <- function(value, message, detail = "") {
      progress_bar$set(value = value, message = message, detail = detail)

      # Lógica JavaScript para actualizar la UI del modal
      percentage <- round(value * 100)

      shinyjs::runjs(
        paste0(
          'document.getElementById("ID_progress_message").innerHTML = "<b>', message, '</b>";',
          'document.getElementById("ID_progress_detail").innerHTML = "', detail, '";',
          'document.getElementById("ID_progress_bar").style.width = "', percentage, '%";'
        )
      )
    }

    isolate({
        RVs_progress$"progress_bar"  <- progress_bar
        RVs_progress$"my_show_modal" <- my_show_modal
        RVs_progress$"FN_update_modal_progress" = FN_update_modal_progress

        TOTEM_data_analysis$"step02"$"check"  <- TRUE
        TOTEM_data_analysis$"step02"$"status_info" <- "Done!"
    })

    print("step02 ---")

    return(list("check" = TOTEM_data_analysis$"step02"$"check"))
  })


  # Step 03 - Open Modal -------------------------------------------------------
  step03 <- eventReactive(step02(), {
    req(step02()$"check")

    # Upload modal
    my_show_modal <- RVs_progress$"my_show_modal"

    # Modal activation!!!
    my_show_modal

    isolate({
      TOTEM_data_analysis$"step03"$"check"  <- TRUE
      TOTEM_data_analysis$"step03"$"status_info" <- "Done!"
    })

    print("step03 ---")

    return(list("check" = TOTEM_data_analysis$"step03"$"check"))
  })

  # Step 04 - Time and new temporal folder -------------------------------------
  STR_INTERNAL_temp_folder_path <- reactiveValues()
  STR_INTERNAL_temp_folder_path$"folder_path" <- NULL
  STR_INTERNAL_temp_folder_path$"check" <- FALSE
  step04 <- eventReactive(step03(), {
    req(step03()$"check")


    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"

    # Definición de la función de creación de carpeta (se mantiene)
    create_new_temporal_output_folder_path <- function(){

      # Lógica de Creación del Archivo Temporal (Mantenida)
      my_temp_folder <- tempdir()
      the_sys_time <- Sys.time()
      timestamp_format <- format(the_sys_time, "%Y%m%d_%H%M%S")
      the_time_here_format(timestamp_format)

      timestamp_content <- format(the_sys_time, "%H:%M:%S del %d-%m-%Y")
      new_sub_folder <- paste0("temp_", timestamp_format)
      nueva_carpeta <- file.path(my_temp_folder, new_sub_folder)
      return(nueva_carpeta)
    }

    # === PASOS INTERMEDIOS (Se mantienen iguales) ===
    FN_update_modal_progress(value = 0.05, message = "Inicializando", detail = "Preparando variables y entorno...")

    # 1. Crear carpeta temporal (10%)
    FN_update_modal_progress(value = 0.10, message = "Preparación de archivos", detail = "Creando carpeta temporal de trabajo...")
    str_new_temp_folder_path <- create_new_temporal_output_folder_path() #file.path(my_output_folder02, str_subfolder_output)

    dir.create(str_new_temp_folder_path, recursive = TRUE)

    check_new_temp_folder_path <- dir.exists(str_new_temp_folder_path)


    STR_INTERNAL_temp_folder_path$"folder_path" <- str_new_temp_folder_path
    STR_INTERNAL_temp_folder_path$"check" <- check_new_temp_folder_path


    isolate({
      TOTEM_data_analysis$"step04"$"check"  <- check_new_temp_folder_path
      if(check_new_temp_folder_path){
        TOTEM_data_analysis$"step04"$"status_info" <- "Done!"
      } else TOTEM_data_analysis$"step04"$"status_info" <- "Problem!"

    })

    print("step04 ---")

    return(list("check" = TOTEM_data_analysis$"step04"$"check"))
  })

  # Step 05 - Coping files -----------------------------------------------------
  ## Input Folder path
  STR_INTERNAL_input_folder_path <- reactiveValues()
  STR_INTERNAL_input_folder_path$"folder_path" <- NULL
  STR_INTERNAL_input_folder_path$"check" <- FALSE

  ## Seting input folder path
  observeEvent(STR_REACTIVE_folder_path_quarto(), {
    # Solo actualiza si el valor no es NULL o la ruta es válida
    if (!is.null(STR_REACTIVE_folder_path_quarto())) {
      # Usamos () para leer el valor, y () para ESCRIBIR en el reactiveVal
      str_folder_path <- STR_REACTIVE_folder_path_quarto()
      check_folder_path <- dir.exists(str_folder_path)
      STR_INTERNAL_input_folder_path$"folder_path" <- str_folder_path
      STR_INTERNAL_input_folder_path$"check" <- check_folder_path
    }
  })

  ## Coping files from input folder to temporal folder...
  step05 <- eventReactive(step04(), {
    req(step04()$"check")

    print("step05 --- End")

    # my_show_modal <- RVs_progress$"my_show_modal"
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"


    FN_update_modal_progress(0.25, "Preparación de archivos", detail = "Copiando plantillas y dependencias...")

    fs::dir_copy(
      path = STR_INTERNAL_input_folder_path$"folder_path",
      new_path = STR_INTERNAL_temp_folder_path$"folder_path",
      overwrite = T
    )

    ###########################################################################
    # Buscar y eliminar archivos .RData
    archivos_rdata <- list.files(STR_INTERNAL_temp_folder_path$"folder_path",
                                 pattern = "\\.RData$",
                                 full.names = TRUE,
                                 ignore.case = TRUE)

    if (length(archivos_rdata) > 0) {
      unlink(archivos_rdata)
      message("✓ Archivos .RData eliminados: ", length(archivos_rdata))
    }
    ###########################################################################

    isolate({
      TOTEM_data_analysis$"step05"$"check"  <- TRUE
      TOTEM_data_analysis$"step05"$"status_info" <- "Done!"
    })

    print("step05 --- End")

    return(list("check" = TOTEM_data_analysis$"step05"$"check"))
  })


  # Step 06 - Create output folder
  STR_INTERNAL_output_folder_path <- reactiveValues()
  STR_INTERNAL_output_folder_path$"folder_path" <- NULL
  STR_INTERNAL_output_folder_path$"check" <- FALSE
  subfolder_output <- "output_folder"

  step06 <- eventReactive(step05(), {
    req(step05()$"check")

    print("step06 --- Init")

    # my_show_modal <- RVs_progress$"my_show_modal"
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"


    FN_update_modal_progress(0.25, "Preparación de archivos", detail = "Copiando plantillas y dependencias...")

    str_temp_folder_path <- STR_INTERNAL_temp_folder_path$"folder_path"
    str_output_folder_path <- file.path(str_temp_folder_path, subfolder_output)

    # Create output folder
    dir.create(str_output_folder_path, recursive = TRUE, showWarnings = FALSE)

    # Cleaning output_folder
    unlink(list.files(str_output_folder_path, full.names = TRUE), recursive = TRUE)

    # Check if exists
    check_output_folder_path <- dir.exists(str_output_folder_path)

    STR_INTERNAL_output_folder_path$"folder_path" <- str_output_folder_path
    STR_INTERNAL_output_folder_path$"check" <- check_output_folder_path

    isolate({
      TOTEM_data_analysis$"step06"$"check"  <- TRUE
      TOTEM_data_analysis$"step06"$"status_info" <- "Done!"
    })

    print("step06 --- End")

    return(list("check" = TRUE))
  })

  # Step 07 - Create output file paths
  TOTEM_file_path_output_report <- reactiveValues()
  TOTEM_file_path_output_report$"html"  <- list("file_path" = "", "check" = "")
  TOTEM_file_path_output_report$"pdf"   <- list("file_path" = "", "check" = "")
  TOTEM_file_path_output_report$"docx"  <- list("file_path" = "", "check" = "")
  TOTEM_file_path_output_report$"xlsx"  <- list("file_path" = "", "check" = "")
  TOTEM_file_path_output_report$"reveal_html"  <- list("file_path" = "", "check" = "")

  TOTEM_file_path_output_report$"check_general" <- FALSE

  step07 <- eventReactive(step06(), {

    print("step07 --- Init")

    # req(TOTEM_input_folder_path$"check")
    str_output_folder_path <- STR_INTERNAL_output_folder_path$"folder_path"

    str_file_path_input_html <- file.path(str_output_folder_path, list_R_file_name_output_report$"html")
    TOTEM_file_path_output_report$"html"$"file_path" <- str_file_path_input_html


    str_file_path_input_pdf <- file.path(str_output_folder_path, list_R_file_name_output_report$"pdf")
    TOTEM_file_path_output_report$"pdf"$"file_path" <- str_file_path_input_pdf

    str_file_path_input_docx <- file.path(str_output_folder_path, list_R_file_name_output_report$"docx")
    TOTEM_file_path_output_report$"docx"$"file_path" <- str_file_path_input_docx

    str_file_path_input_xlsx <- file.path(str_output_folder_path, list_R_file_name_output_report$"xlsx")
    TOTEM_file_path_output_report$"xlsx"$"file_path" <- str_file_path_input_xlsx

    str_file_path_input_zip_png <- file.path(str_output_folder_path, list_R_file_name_output_report$"zip_png")
    TOTEM_file_path_output_report$"zip_png"$"file_path" <- str_file_path_input_zip_png

    str_file_path_input_reveal_html <- file.path(str_output_folder_path, list_R_file_name_output_report$"reveal_html")
    TOTEM_file_path_output_report$"reveal_html"$"file_path" <- str_file_path_input_reveal_html





    isolate({
      TOTEM_data_analysis$"step07"$"check"  <- TRUE
      TOTEM_data_analysis$"step07"$"status_info" <- "Done!"
    })

    print("step07 --- End")

    return(list("check" = TRUE))
  })


  step08 <- eventReactive(step07(), {

    # 1. Print -----------------------------------------------------------------
    print("step08 --- Init")

    # 2. Modal -----------------------------------------------------------------
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"
    FN_update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")

    # 3. Basics ----------------------------------------------------------------
    str_work_dir_original <- TOTEM_special_paths$"getwd"
    str_work_dir_new <- STR_INTERNAL_temp_folder_path$"folder_path"


    # 4. Changing work directory -----------------------------------------------
    setwd(str_work_dir_new)

    # 5. New content -----------------------------------------------------------
    list_for_replace <- list()
    list_for_replace[["AAA_import_dataset_internal_AAA"]] <-  the_list01_Dataset_internal()$"str_import_internal"
    list_for_replace[["AAA_import_dataset_external_AAA"]] <-  the_list01_Dataset_internal()$"str_import_external"
    list_for_replace["BBB_var_name_rv_BBB"] <- the_list02_VarSelection_stone$"var_name_rv"
    list_for_replace["BBB_var_name_factor_BBB"] <- the_list02_VarSelection_stone$"var_name_factor"
    list_for_replace["BBB_alpha_value_BBB"] <- the_list02_VarSelection_stone$"alpha_value"
    list_for_replace["CCC_vector_ordered_levels_CCC"] <- paste0("c(", paste(shQuote(the_list03_SpecialSettigns_stone$"vector_ordered_levels", type = "sh"), collapse = ", "), ")")
    list_for_replace["CCC_vector_ordered_colors_CCC"] <- paste0("c(", paste(shQuote(the_list03_SpecialSettigns_stone$"vector_ordered_colors", type = "sh"), collapse = ", "), ")")

    ## 5.1 Selected file for changes
    str_file_name <- "file00_01_RQuarto.qmd"

    ## 5.2 Basics
    vector_for_replace <- unlist(list_for_replace)
    vector_file_content <- readLines(str_file_name, warn = FALSE)
    names(vector_for_replace) <- names(list_for_replace)

    ## 5.3 Replacement...
    vector_new_content <- stringr::str_replace_all(string = vector_file_content,
                                                      vector_for_replace)


    vector_new_content <- stringr::str_replace_all(string = vector_new_content,
                                                      pattern = "\\#\\+\\+\\+---",
                                                      replacement = "")


    ## 5.4 Saving new file
    writeLines(vector_new_content, str_file_name)

    # 6. Rendering quarto ------------------------------------------------------
    my_input_file_name  <- list_R_file_name_input_template$"html"
    my_output_file_name <- basename(TOTEM_file_path_output_report$"html"$"file_path")

    quarto::quarto_render(input = my_input_file_name,
                          output_format = "html",
                          output_file = my_output_file_name,
                          execute = TRUE,
                          execute_params = list(    activate_params= "FALSE",
                                                    file_source= "from_params",
                                                    file_name= "from_params",
                                                    the_package= "from_params",
                                                    tool_used= "from_params",
                                                    script_used= "from_params",
                                                    current_time= "from_params"),
                          #execute_params = my_bag,
                          quiet = FALSE)

    # # 7. Moving file to output folder ------------------------------------------

    file_path_01 <- file.path(str_work_dir_new, my_output_file_name)
    file_path_02 <- file.path(str_work_dir_new, subfolder_output, my_output_file_name)
    print(file_path_01)
    print(file_path_02)
    file.rename(from = file_path_01,
                to = file_path_02)


    # 8. Checking output file exists -------------------------------------------
    check_file_exists <- file.exists(TOTEM_file_path_output_report$"html"$"file_path")
    TOTEM_file_path_output_report$"html"$"check" <- check_file_exists


    # 9. Return to original work directory -------------------------------------
    setwd(str_work_dir_original)

    # 10. Final print ----------------------------------------------------------
    print("step08 --- End")

    # 11. Return ---------------------------------------------------------------
    return(list("check" = TRUE))
  })

  step09 <- eventReactive(step08(), {

    # 1. Print -----------------------------------------------------------------
    print("step09 --- Init")

    # 2. Modal -----------------------------------------------------------------
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"
    FN_update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")

    # 3. Basics ----------------------------------------------------------------
    str_work_dir_original <- TOTEM_special_paths$"getwd"
    str_work_dir_new <- STR_INTERNAL_temp_folder_path$"folder_path"


    # 4. Changing work directory -----------------------------------------------
    setwd(str_work_dir_new)

    # 6. Rendering quarto ------------------------------------------------------
    my_input_file_name  <- list_R_file_name_input_template$"pdf"
    my_output_file_name <- basename(TOTEM_file_path_output_report$"pdf"$"file_path")

    quarto::quarto_render(input = my_input_file_name,
                          output_format = "pdf",
                          output_file = my_output_file_name,
                          quiet = FALSE)


    # 7. Moving file to output folder ------------------------------------------
    file.rename(from = my_output_file_name,
                to = file.path(subfolder_output, my_output_file_name))


    # 8. Checking output file exists -------------------------------------------
    check_file_exists <- file.exists(TOTEM_file_path_output_report$"pdf"$"file_path")
    TOTEM_file_path_output_report$"pdf"$"check" <- check_file_exists


    # 9. Return to original work directory -------------------------------------
    setwd(str_work_dir_original)

    # 10. Final print ----------------------------------------------------------
    print("step09 --- End")

    # 11. Return ---------------------------------------------------------------
    return(list("check" = TRUE))
  })

  step10 <- eventReactive(step09(), {

    # 1. Print -----------------------------------------------------------------
    print("step10 --- Init")

    # 2. Modal -----------------------------------------------------------------
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"
    FN_update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")

    # 3. Basics ----------------------------------------------------------------
    str_work_dir_original <- TOTEM_special_paths$"getwd"
    str_work_dir_new <- STR_INTERNAL_temp_folder_path$"folder_path"


    # 4. Changing work directory -----------------------------------------------
    setwd(str_work_dir_new)

    # 6. Rendering quarto ------------------------------------------------------
    my_input_file_name  <- list_R_file_name_input_template$"docx"
    my_output_file_name <- basename(TOTEM_file_path_output_report$"docx"$"file_path")

    quarto::quarto_render(input = my_input_file_name,
                          output_format = "docx",
                          output_file = my_output_file_name,
                          quiet = FALSE)


    # 7. Moving file to output folder ------------------------------------------
    file.rename(from = my_output_file_name,
                to = file.path(subfolder_output, my_output_file_name))


    # 8. Checking output file exists -------------------------------------------
    check_file_exists <- file.exists(TOTEM_file_path_output_report$"docx"$"file_path")
    TOTEM_file_path_output_report$"docx"$"check" <- check_file_exists


    # 9. Return to original work directory -------------------------------------
    setwd(str_work_dir_original)

    # 10. Final print ----------------------------------------------------------
    print("step10 --- End")

    # 11. Return ---------------------------------------------------------------
    return(list("check" = TRUE))
  })

  step11 <- eventReactive(step10(), {

    # 1. Print -----------------------------------------------------------------
    print("step11 --- Init")

    # 2. Modal -----------------------------------------------------------------
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"
    FN_update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")

    # 3. Basics ----------------------------------------------------------------
    str_work_dir_original <- TOTEM_special_paths$"getwd"
    str_work_dir_new <- STR_INTERNAL_temp_folder_path$"folder_path"


    # 4. Changing work directory -----------------------------------------------
    setwd(str_work_dir_new)

    # 6. Rendering quarto ------------------------------------------------------
    my_input_file_name  <- list_R_file_name_input_template$"xlsx"
    my_output_file_name <- basename(TOTEM_file_path_output_report$"xlsx"$"file_path")

    png_zip_file_name <- list_R_file_name_output_report$"zip_png"

    png_folder_name <- tools::file_path_sans_ext(png_zip_file_name)

    str_html_file_name_from_xlsx <- sub(pattern = "[.]qmd", replacement = ".html", x = my_input_file_name)


    # Prevention protocol
    if (dir.exists(png_folder_name)) unlink(png_folder_name, recursive = TRUE)
    if (file.exists(str_html_file_name_from_xlsx)) file.remove(str_html_file_name_from_xlsx)
    if (file.exists(my_output_file_name)) file.remove(my_output_file_name)
    if (file.exists(png_zip_file_name)) file.remove(png_zip_file_name)


    quarto::quarto_render(input = my_input_file_name,
                          output_format = "html",
                          output_file = str_html_file_name_from_xlsx, #output_file = my_output_file_name,
                          quiet = FALSE,
                          execute_params = list(activate_params = "TRUE",
                                                load_from_file = "R_obj_env.RData",
                                                file_name_xlsx = my_output_file_name,
                                                png_folder_name = png_folder_name)
                          )


    # 7. Moving file to output folder ------------------------------------------
    # 7.1 Deleting html file from xlsx.
    # Rendering was only for enviroment and objects.
    if(file.exists(str_html_file_name_from_xlsx)) file.remove(str_html_file_name_from_xlsx)

    # 7.2 xlsx file to output folder
    file.rename(from = my_output_file_name,
                to = file.path(subfolder_output, my_output_file_name))

    # 7.3 png file on zip file to output folder
    file.rename(from = png_zip_file_name,
                to = file.path(subfolder_output, png_zip_file_name))


    # 8. Checking output file exists -------------------------------------------
    check_file_xlsx_exists <- file.exists(TOTEM_file_path_output_report$"xlsx"$"file_path")
    TOTEM_file_path_output_report$"xlsx"$"check" <- check_file_xlsx_exists

    check_file_zip_png_exists <- file.exists(TOTEM_file_path_output_report$"zip_png"$"file_path")
    TOTEM_file_path_output_report$"zip_png"$"check" <- check_file_zip_png_exists

    # 9. Return to original work directory -------------------------------------
    setwd(str_work_dir_original)

    # 10. Final print ----------------------------------------------------------
    print("step11 --- End")

    # 11. Return ---------------------------------------------------------------
    return(list("check" = TRUE))
  })

  step12 <- eventReactive(step11(), {

    # 1. Print -----------------------------------------------------------------
    print("step12 --- Init")

    # 2. Modal -----------------------------------------------------------------
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"
    FN_update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")

    # 3. Basics ----------------------------------------------------------------
    str_work_dir_original <- TOTEM_special_paths$"getwd"
    str_work_dir_new <- STR_INTERNAL_temp_folder_path$"folder_path"


    # 4. Changing work directory -----------------------------------------------
    setwd(str_work_dir_new)

    # 6. Rendering quarto ------------------------------------------------------
    my_input_file_name  <- list_R_file_name_input_template$"reveal_html"
    my_output_file_name <- basename(TOTEM_file_path_output_report$"reveal_html"$"file_path")

    quarto::quarto_render(input = my_input_file_name,
                          output_format = "revealjs",
                          output_file = my_output_file_name,
                          quiet = FALSE,
                          execute_params = list(activate_params = "TRUE",
                                                load_from_file = "R_obj_env.RData")
                          )


    # 7. Moving file to output folder ------------------------------------------
    file.rename(from = my_output_file_name,
                to = file.path(subfolder_output, my_output_file_name))


    # 8. Checking output file exists -------------------------------------------
    check_file_exists <- file.exists(TOTEM_file_path_output_report$"reveal_html"$"file_path")
    TOTEM_file_path_output_report$"reveal_html"$"check" <- check_file_exists


    # 9. Return to original work directory -------------------------------------
    setwd(str_work_dir_original)

    # 10. Final print ----------------------------------------------------------
    print("step12 --- End")

    # 11. Return ---------------------------------------------------------------
    return(list("check" = TRUE))
  })

  observeEvent(step12(), {
    # req(FALSE)
    req(step12()$"check")
    # 1. INICIALIZACIÓN: Crear el objeto de progreso y bloquear la pantalla

    print("step13 --- Init")

    my_show_modal <- RVs_progress$"my_show_modal"
    progress_bar <-  RVs_progress$"progress_bar"
    FN_update_modal_progress <- RVs_progress$"FN_update_modal_progress"


    # 3. MANEJO DEL FLUJO CON tryCatch (Avanzando paso a paso)
    tryCatch({



      str_work_dir_original <- TOTEM_special_paths$"getwd"
      str_work_dir_new <- STR_INTERNAL_temp_folder_path$"folder_path"

      setwd(str_work_dir_new)


      # #########################################################################################


      # #########################################################################################



      print(list.files())
      print(list.files("output_folder"))
      setwd(str_work_dir_original)

      # 6. Progreso tras el bloqueo (90%)
      FN_update_modal_progress(0.90, "Renderizando Quarto", detail = "Renderizado completado. Finalizando...")


      # === PASO C: Finalización Exitosa (90% - 100%) ===

      # C1. Terminar barra de progreso al 100%
      FN_update_modal_progress(1.0, "¡Proceso Completado!", detail = "Éxito al generar el reporte.")

      # C2. Actualizar estado y color del botón
      message(crayon::green("Process completed!"))
      message("")

      ###############################################

      # Define el nuevo estado (TRUE para checked, FALSE para unchecked)
      nuevo_estado <- TRUE # O FALSE

      # Construir el código JavaScript
      js_code <- paste0(
        # 1. Cambia visualmente el estado del checkbox
        "var checkbox = document.getElementById('toggle02_input');",
        "checkbox.checked = ", tolower(nuevo_estado), ";",

        # 2. ¡CLAVE! Notifica a Shiny (R) del nuevo valor
        "Shiny.setInputValue('toggle02_input', checkbox.checked, {priority: 'event'});"
      )

      # 3. Ejecutar el código JavaScript
      shinyjs::runjs(js_code)
      ###############################


      output$mensaje_estado02 <- renderText({
        "¡Carpeta y archivo creados exitosamente! El Botón 1 está en verde. Listo para la descarga."
      })


      # ----------------------------------------------------
      # 🟢 C3. CAMBIAR EL MODAL A CHECK DE ÉXITO (CORRECCIÓN FINAL) 🟢
      # ----------------------------------------------------

      shinyjs::runjs(
        'document.getElementById("ID_my_check").innerHTML =
      "<i class=\\"fa fa-check-circle fa-6x\\" style=\\"color: #4CAF50;\\"></i>";

   document.getElementById("ID_progress_message").innerHTML =
      "<b>Reporte Generado Exitosamente</b>";
   document.getElementById("ID_progress_detail").innerHTML =
      "Cerrando la ventana en 3 segundos...";

   document.getElementById("ID_progress_bar").classList.remove("active");

   document.getElementById("ID_progress_bar").style.width = "100%";
  '
      )

      # C4. Esperar 3 segundos para confirmación visual
      Sys.sleep(3)

    }, error = function(e) {
      # 🛑 MANEJO DE ERRORES:
      warning("Error al renderizar Quarto: ", e$message)

      # Cierra el modal de proceso
      removeModal()

      # Muestra un modal de error
      showModal(modalDialog(
        title = "⚠️ Error de Renderizado",
        paste("Ha ocurrido un error. Consulte la consola de R para más detalles. Mensaje:", e$message),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))

      return(NULL)
    }, finally = {
      # 4. LIMPIEZA: Cerrar el objeto de progreso de R siempre
      progress_bar$close()
    })

    # 5. 🟢 DESBLOQUEAR LA PANTALLA
    removeModal()
    message("")


    print("step13 --- END")

  })




  observeEvent(input$btn_open_html, {
    # C2. Actualizar estado y color del botón
    message(crayon::green("OPen completed!"))
    message("")

    # 1. CAMBIO DE COLOR B1: Naranja -> Verde (Persistente)
    removeClass("btn_open_html", "btn-warning")
    addClass("btn_open_html", "btn-success")

    # --- CAMBIO CLAVE AQUÍ ---
    # 2. Obtener la URL del archivo
    # Usamos isolate() para asegurarnos de que el observeEvent solo reaccione a input$open02
    # y no a cambios en str_output_file_path_html (si es un reactive)

    html_path <- isolate(TOTEM_file_path_output_report$"html"$"file_path")

    # *** VERIFICACIÓN CRUCIAL: Asegúrate de que el archivo exista ***
    if (!file.exists(html_path)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(html_path)
    html_filename <- basename(html_path)

    # 2. DEFINIR UN NOMBRE ÚNICO PARA EL RECURSO TEMPORAL
    resource_id <- digest::digest(html_dir, algo = "md5")

    # 3. REGISTRAR EL RECURSO
    shiny::addResourcePath(resource_id, html_dir)

    # 4. Construir la URL con el ID único del recurso
    html_url <- file.path(resource_id, html_filename)

    # 3. Ejecutar JavaScript para abrir la URL en una nueva pestaña
    # window.open(URL, '_blank') es el comando estándar de JavaScript para esto.
    shinyjs::runjs(paste0("window.open('",  html_url, "', '_blank');"))
    # --------------------------
  })

  observeEvent(input$open01, {
    # C2. Actualizar estado y color del botón
    message(crayon::green("OPen completed!"))
    message("")

    # 1. CAMBIO DE COLOR B1: Naranja -> Verde (Persistente)
    removeClass("open01", "btn-warning")
    addClass("open01", "btn-success")

    path_folder_inst <- str_input_folder_package()
    sub_folder <- "classroom"
    selected_file <- "classroom_01_anova_intro.html"

    full_path_file <- file.path(path_folder_inst, sub_folder, selected_file)

    html_path <- isolate(full_path_file)

    # *** VERIFICACIÓN CRUCIAL: Asegúrate de que el archivo exista ***
    if (!file.exists(html_path)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(html_path)
    html_filename <- basename(html_path)

    # 2. DEFINIR UN NOMBRE ÚNICO PARA EL RECURSO TEMPORAL
    resource_id <- digest::digest(html_dir, algo = "md5")

    # 3. REGISTRAR EL RECURSO
    shiny::addResourcePath(resource_id, html_dir)

    # 4. Construir la URL con el ID único del recurso
    html_url <- file.path(resource_id, html_filename)

    # 3. Ejecutar JavaScript para abrir la URL en una nueva pestaña
    # window.open(URL, '_blank') es el comando estándar de JavaScript para esto.
    shinyjs::runjs(paste0("window.open('",  html_url, "', '_blank');"))
    # --------------------------
  })


  # 01 - PDF
  output$text_output_folder_path02 <- renderText({
    req(str_output_folder02())
    str_output_folder02()
  })
  output$text_list_files02 <- renderText({
    req(str_output_folder02())

    # Obtiene el vector de nombres de archivos
    files_list <- list.files(path = str_output_folder02(), recursive = TRUE)

    # Concatena los nombres de los archivos separados por un salto de línea (\n)
    # y devuelve una única cadena de texto
    paste(files_list, collapse = "\n")
  })


  # --- Lógica del Botón "Descargar" ---
  output$btn_download_html<- downloadHandler(

    filename = function() {
      la_ruta <- str_output_file_name_html()

      if (!is.null(la_ruta)) {
        basename(la_ruta)
      } else {
        "archivo_vacio.html"
      }
    },


    content = function(file) {
      archivo_a_descargar <- TOTEM_file_path_output_report$"html"$"file_path"
      print(archivo_a_descargar)
      if (!is.null(archivo_a_descargar) && file.exists(archivo_a_descargar)) {

        # 1. CAMBIO DE COLOR B2: Naranja -> Verde (Persistente)
        runjs("
                    // Quitamos la clase temporal 'disabled' si la puso el navegador
                    $('#btn_download_html').removeClass('disabled');
                    $('#btn_download_html').removeClass('btn-warning');
                    $('#btn_download_html').addClass('btn-success');

                    // IMPORTANTE: NO SE RESTABLECE EL BOTÓN 1 A NARANJA AQUÍ.
                    // AMBOS BOTONES PERMANECERÁN VERDES.
                ")

        fs::file_copy(archivo_a_descargar, file)

      } else {
        warning("No se encontró el archivo temporal para descargar.")
        writeLines("Error: Archivo no generado.", file)
      }
    }
  )


  output$html_viewer <- renderText({
    # 1. Asegúrate de que el path exista
    req(TOTEM_file_path_output_report$"html"$"file_path")

    html_path <- TOTEM_file_path_output_report$"html"$"file_path"

    if (!file.exists(html_path)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(html_path)
    html_filename <- basename(html_path)

    # 2. Definir y Registrar Recurso
    resource_id <- digest::digest(html_dir, algo = "md5")
    shiny::addResourcePath(resource_id, html_dir)

    # 3. Construir la URL con el ID único del recurso
    html_url <- paste0("/", file.path(resource_id, html_filename))

    # 4. Crear el iframe con ID, scrolling="no", y altura mínima inicial (10px)
    armado_v <- paste('<div style="height: 100%; width: 100%; "><iframe style="height: 100%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")

    # tags$iframe(
    #   id = "my_report_iframe", # <-- ID para que JS pueda encontrarlo
    #   style = 'height: 10px; width: 100%; border: none; overflow: hidden;', # <-- Altura inicial
    #   src = html_url,
    #   type = "text/html",
    #   scrolling = "no" # <-- CLAVE: Deshabilita el scroll interno
    # )
  })

  output$html_viewer2 <- renderText({

    # 1. Asegúrate de que el path del archivo original exista
    req(TOTEM_file_path_output_report$"html"$"file_path")

    html_path_original <- TOTEM_file_path_output_report$"html"$"file_path"

    if (!file.exists(html_path_original)) {
      return(p("Error: El archivo HTML original aún no se ha generado o no se encuentra."))
    }

    # --- PARTE 1: EXTRACCIÓN Y RE-ENSAMBLAJE (Lógica de Head Inyectado) ---

    # 1. Leer y parsear el archivo HTML
    doc <- xml2::read_html(html_path_original)

    # A. Extraer el HEAD Completo (Estilos y JS)
    head_content <- doc %>% rvest::html_node("head") %>% as.character()
    if (is.null(head_content)) {
      return(p("Error: No se pudo extraer la sección <head> del HTML."))
    }

    # B. Extraer y Limpiar la sección "#zocalo"
    selector_zocalo <- "#zocalo"
    zocalo_node <- doc %>% rvest::html_node(selector_zocalo)

    if (is.null(zocalo_node) || inherits(zocalo_node, "xml_missing")) {
      # Es común que estos elementos sean opcionales, si falla, retornamos vacío
      zocalo_html_clean <- paste0("")
    } else {
      full_zocalo_html <- zocalo_node %>% as.character()
      # Limpiar etiquetas div padre (CRÍTICO: Regex probada)
      zocalo_html_clean <- sub('^<div[^>]*>([\\s\\S]*)</div>$', '\\1', full_zocalo_html)
    }

    # C. Extraer y Limpiar la sección "#tab-classroom"
    selector_classroom <- "#tab-classroom"
    classroom_node <- doc %>% rvest::html_node(selector_classroom)

    if (is.null(classroom_node) || inherits(classroom_node, "xml_missing")) {
      return(p(paste("❗ Error: Selector", selector_classroom, "no encontrado.")))
    }

    full_classroom_html <- classroom_node %>% as.character()
    # Limpiar etiquetas div padre (CRÍTICO: Regex probada)
    classroom_html_clean <- sub('^<div[^>]*>([\\s\\S]*)</div>$', '\\1', full_classroom_html)


    # 2. Ensamblar el nuevo documento HTML con el HEAD y los fragmentos en ORDEN
    html_output <- paste0(
      '<!DOCTYPE html>
      ', head_content, '
      <body>
        <h1>Fragmento Combinado: Zócalo + ClassRoom</h1>
        <hr>

        <div id="zocalo-fragment">', zocalo_html_clean, '</div>

        <h2>ClassRoom (ANOVA)</h2>
        <hr>

        <div id="classroom-fragment">', classroom_html_clean, '</div>

      </body>
      </html>'
    )

    # --- PARTE 2: GUARDADO Y GESTIÓN DE RECURSOS ---

    # Definimos la carpeta de destino y generamos un nombre de archivo único
    output_dir <- dirname(html_path_original)
    temp_filename <- paste0("fragment_zocalo_classroom_", digest::digest(html_output, algo="md5"), ".html")
    temp_filepath <- file.path(output_dir, temp_filename)

    # 3. Guardar el fragmento estilizado en la carpeta de destino
    writeLines(html_output, temp_filepath)

    # 4. Definir y Registrar Recurso
    resource_id <- digest::digest(output_dir, algo = "md5")
    shiny::addResourcePath(resource_id, output_dir)

    # 5. Construir la URL y el IFRAME
    html_url <- paste0("/", file.path(resource_id, temp_filename))

    armado_v <- paste('<div style="height: 85%; width: 100%; "><iframe style="height: 85%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")

    # Devolvemos el iframe como texto (renderText)
    return(armado_v)
  })


  ####################################
  # General reset
  observeEvent(input$"btn_refresh", {

    set_reactive_values_from_list(rv = the_list01_Dataset_stone, data_list = the_list01_Dataset_R)
    set_reactive_values_from_list(rv = the_list02_VarSelection_stone, data_list = the_list02_VarSelection_R)
    set_reactive_values_from_list(rv = the_list03_SpecialSettigns_stone, data_list = the_list03_SpecialSettigns_R)

    shinyjs::removeClass(id = "btn_dataset", class = "btn-success")
    shinyjs::addClass(id = "btn_dataset",  class = "btn-primary")

    shinyjs::removeClass(id = "btn_var_selector", class = "btn-success")
    shinyjs::addClass(id = "btn_var_selector",  class = "btn-primary")

    shinyjs::removeClass(id = "btn_settings", class = "btn-success")
    shinyjs::addClass(id = "btn_settings",  class = "btn-primary")

    shinyjs::removeClass(id = "btn_play_front", class = "btn-success")
    shinyjs::addClass(id = "btn_play_front",  class = "btn-primary")

    ANCESTRAL_PLAY(FALSE)
    shinyjs::removeClass(id = "btn_play_html", class = "btn-success")
    shinyjs::addClass(id = "btn_play_html",  class = "btn-warning")

    shinyjs::removeClass(id = "btn_download_html", class = "btn-success")
    shinyjs::addClass(id = "btn_download_html",  class = "btn-warning")

    shinyjs::removeClass(id = "btn_open_html", class = "btn-success")
    shinyjs::addClass(id = "btn_open_html",  class = "btn-warning")


  })



  output$html_01_anova_intro <- renderText({
    # 1. Asegúrate de que el path exista
    path_folder_inst <- str_input_folder_package()
    sub_folder <- "classroom"
    selected_file <- "classroom_01_anova_intro.html"

    full_path_file <- file.path(path_folder_inst, sub_folder, selected_file)

    # req(str_output_file_path_html())

    # html_path <- str_output_file_path_html()

    if (!file.exists(full_path_file)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(full_path_file)
    html_filename <- basename(full_path_file)

    # 2. Definir y Registrar Recurso
    resource_id <- digest::digest(html_dir, algo = "md5")
    shiny::addResourcePath(resource_id, html_dir)

    # 3. Construir la URL con el ID único del recurso
    html_url <- paste0("/", file.path(resource_id, html_filename))

    # 4. Crear el iframe con ID, scrolling="no", y altura mínima inicial (10px)
    armado_v <- paste('<div style="height: 100%; width: 100%; "><iframe style="height: 100%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")

    # tags$iframe(
    #   id = "my_report_iframe", # <-- ID para que JS pueda encontrarlo
    #   style = 'height: 10px; width: 100%; border: none; overflow: hidden;', # <-- Altura inicial
    #   src = html_url,
    #   type = "text/html",
    #   scrolling = "no" # <-- CLAVE: Deshabilita el scroll interno
    # )
  })


  output$html_02_tukey <- renderText({
    # 1. Asegúrate de que el path exista
    path_folder_inst <- str_input_folder_package()
    sub_folder <- "classroom"
    selected_file <- "classroom_02_tukey.html"

    full_path_file <- file.path(path_folder_inst, sub_folder, selected_file)

    # req(str_output_file_path_html())

    # html_path <- str_output_file_path_html()

    if (!file.exists(full_path_file)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(full_path_file)
    html_filename <- basename(full_path_file)

    # 2. Definir y Registrar Recurso
    resource_id <- digest::digest(html_dir, algo = "md5")
    shiny::addResourcePath(resource_id, html_dir)

    # 3. Construir la URL con el ID único del recurso
    html_url <- paste0("/", file.path(resource_id, html_filename))

    # 4. Crear el iframe con ID, scrolling="no", y altura mínima inicial (10px)
    armado_v <- paste('<div style="height: 100%; width: 100%; "><iframe style="height: 100%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")

    # tags$iframe(
    #   id = "my_report_iframe", # <-- ID para que JS pueda encontrarlo
    #   style = 'height: 10px; width: 100%; border: none; overflow: hidden;', # <-- Altura inicial
    #   src = html_url,
    #   type = "text/html",
    #   scrolling = "no" # <-- CLAVE: Deshabilita el scroll interno
    # )
  })

  output$html_03_decision_making <- renderText({
    # 1. Asegúrate de que el path exista
    path_folder_inst <- str_input_folder_package()
    sub_folder <- "classroom"
    selected_file <- "classroom_02_tukey.html"

    full_path_file <- file.path(path_folder_inst, sub_folder, selected_file)

    # req(str_output_file_path_html())

    # html_path <- str_output_file_path_html()

    if (!file.exists(full_path_file)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(full_path_file)
    html_filename <- basename(full_path_file)

    # 2. Definir y Registrar Recurso
    resource_id <- digest::digest(html_dir, algo = "md5")
    shiny::addResourcePath(resource_id, html_dir)

    # 3. Construir la URL con el ID único del recurso
    html_url <- paste0("/", file.path(resource_id, html_filename))

    # 4. Crear el iframe con ID, scrolling="no", y altura mínima inicial (10px)
    armado_v <- paste('<div style="height: 100%; width: 100%; "><iframe style="height: 100%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")

    # tags$iframe(
    #   id = "my_report_iframe", # <-- ID para que JS pueda encontrarlo
    #   style = 'height: 10px; width: 100%; border: none; overflow: hidden;', # <-- Altura inicial
    #   src = html_url,
    #   type = "text/html",
    #   scrolling = "no" # <-- CLAVE: Deshabilita el scroll interno
    # )
  })

  output$html_04_ASA <- renderText({
    # 1. Asegúrate de que el path exista
    path_folder_inst <- str_input_folder_package()
    sub_folder <- "classroom"
    selected_file <- "classroom_04_ASA.html"

    full_path_file <- file.path(path_folder_inst, sub_folder, selected_file)

    # req(str_output_file_path_html())

    # html_path <- str_output_file_path_html()

    if (!file.exists(full_path_file)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(full_path_file)
    html_filename <- basename(full_path_file)

    # 2. Definir y Registrar Recurso
    resource_id <- digest::digest(html_dir, algo = "md5")
    shiny::addResourcePath(resource_id, html_dir)

    # 3. Construir la URL con el ID único del recurso
    html_url <- paste0("/", file.path(resource_id, html_filename))

    # 4. Crear el iframe con ID, scrolling="no", y altura mínima inicial (10px)
    armado_v <- paste('<div style="height: 100%; width: 100%; "><iframe style="height: 100%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")

    # tags$iframe(
    #   id = "my_report_iframe", # <-- ID para que JS pueda encontrarlo
    #   style = 'height: 10px; width: 100%; border: none; overflow: hidden;', # <-- Altura inicial
    #   src = html_url,
    #   type = "text/html",
    #   scrolling = "no" # <-- CLAVE: Deshabilita el scroll interno
    # )
  })

  ##############################################################################

  # File 01 - html full
  output$special01 <- renderUI({
    fluidRow(
      column(4, "HTML FULL"),
      column(2, downloadButton(outputId = "btn_download_01_html_full",
                   label = NULL,
                   icon = icon("download", class = "fa-2x"),
                   class = "btn-warning btn-sm"),
    actionButton(inputId = "btn_open_01_html_full",
                 label = NULL,
                 icon = icon("binoculars", class = "fa-2x"),
                 class = "btn-warning btn-sm")

    )
    )
  })

  observeEvent(input$btn_open_01_html_full, {
    # C2. Actualizar estado y color del botón
    message(crayon::green("OPen completed!"))
    message("")

    # 1. CAMBIO DE COLOR B1: Naranja -> Verde (Persistente)
    removeClass("btn_open_01_html_full", "btn-warning")
    addClass("btn_open_01_html_full", "btn-success")

    # --- CAMBIO CLAVE AQUÍ ---
    # 2. Obtener la URL del archivo
    # Usamos isolate() para asegurarnos de que el observeEvent solo reaccione a input$open02
    # y no a cambios en str_output_file_path_html (si es un reactive)

    html_path <- isolate(TOTEM_file_path_output_report$"html"$"file_path")

    # *** VERIFICACIÓN CRUCIAL: Asegúrate de que el archivo exista ***
    if (!file.exists(html_path)) {
      return(p("Error: El archivo HTML aún no se ha generado o no se encuentra."))
    }

    html_dir <- dirname(html_path)
    html_filename <- basename(html_path)

    # 2. DEFINIR UN NOMBRE ÚNICO PARA EL RECURSO TEMPORAL
    resource_id <- digest::digest(html_dir, algo = "md5")

    # 3. REGISTRAR EL RECURSO
    shiny::addResourcePath(resource_id, html_dir)

    # 4. Construir la URL con el ID único del recurso
    html_url <- file.path(resource_id, html_filename)

    # 3. Ejecutar JavaScript para abrir la URL en una nueva pestaña
    # window.open(URL, '_blank') es el comando estándar de JavaScript para esto.
    shinyjs::runjs(paste0("window.open('",  html_url, "', '_blank');"))
    # --------------------------
  })

  output$btn_download_01_html_full <- downloadHandler(

    filename = function() {
      la_ruta <- str_output_file_name_html()

      if (!is.null(la_ruta)) {
        basename(la_ruta)
      } else {
        "archivo_vacio.html"
      }
    },


    content = function(file) {
      archivo_a_descargar <- TOTEM_file_path_output_report$"html"$"file_path"
      print(archivo_a_descargar)
      if (!is.null(archivo_a_descargar) && file.exists(archivo_a_descargar)) {

        # 1. CAMBIO DE COLOR B2: Naranja -> Verde (Persistente)
        runjs("
                    // Quitamos la clase temporal 'disabled' si la puso el navegador
                    $('#btn_download_01_html_full').removeClass('disabled');
                    $('#btn_download_01_html_full').removeClass('btn-warning');
                    $('#btn_download_01_html_full').addClass('btn-success');

                    // IMPORTANTE: NO SE RESTABLECE EL BOTÓN 1 A NARANJA AQUÍ.
                    // AMBOS BOTONES PERMANECERÁN VERDES.
                ")

        fs::file_copy(archivo_a_descargar, file)

      } else {
        warning("No se encontró el archivo temporal para descargar.")
        writeLines("Error: Archivo no generado.", file)
      }
    }
  )

  mod_download_server("report_html", reactive(TOTEM_file_path_output_report$"html"$"file_path"))
  mod_download_server("report_pdf" , reactive(TOTEM_file_path_output_report$"pdf"$"file_path"))
  mod_download_server("report_docx", reactive(TOTEM_file_path_output_report$"docx"$"file_path"))
  mod_download_server("report_xlsx", reactive(TOTEM_file_path_output_report$"xlsx"$"file_path"))
  mod_download_server("report_zip_png", reactive(TOTEM_file_path_output_report$"zip_png"$"file_path"))
  mod_download_server("reveal_html", reactive(TOTEM_file_path_output_report$"reveal_html"$"file_path"))


  # observe({
  #   req(str_output_file_path_xlsx())
  #   print("El folder...")
  #   print(str_output_folder02())
  #   print(dir.exists(str_output_folder02()))
  #   print("\n")
  #   print("El folder del excel")
  #   print(dirname(str_output_file_path_xlsx()))
  #   print(dir.exists(dirname(str_output_file_path_xlsx())))
  #   print("\n")
  #   print("El file del excel")
  #   print(str_output_file_path_xlsx())
  #   print(file.exists(str_output_file_path_xlsx()))
  # })

##################################################################

}

shinyApp(ui, server)
