
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
MY_PACKAGE_NAME <- "exp002RShiny"
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
ui <- bslib::page_sidebar(
  padding = c(15, 15, 15, 15), # top, right, bottom, left = 0, 0, 10, 10
  shinyjs::useShinyjs(),
  # Carga los recursos CSS y JS de Font Awesome de la librería local de Shiny
  tags$head(
    # Esto carga la versión de Font Awesome incluida en el paquete shiny
    tags$link(rel = "stylesheet", href = "shared/font-awesome/css/all.min.min.css")
  ),
  # CSS con !important para forzar los colores y aplicar estilos al Main Panel
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
}

             /* Los estilos de layout (body, .main, .bslib-page-sidebar) han sido eliminados */
        "))
  ),


  sidebar = bslib::sidebar(
    padding = c(0, 15, 0, 15), # top, right, bottom, left = 0, 0, 10, 10
    # Argumento 'style' eliminado. El sidebar vuelve a su comportamiento predeterminado.
    div(
      style = "text-align: center;", # <--- ESTO CENTRA TODO EL CONTENIDO
      tags$img(src = "Rscience_logo_01.png", width = "40%", style = "padding-bottom: 10px;"),
      tags$b("v1.0.14"),
      br(),

      uiOutput("the_toggle"),
      uiOutput("the_super_side")

    )
  ),
uiOutput("the_super_main")
)

server <- function(input, output, session) {

  ###---------------------------------------------------------------------------
  output$the_toggle <- renderUI({
    # Toggle estilo R/Python
    # Agregar CSS personalizado para los colores del toggle
    div(
      tags$head(
        tags$style(HTML("
      /* Estilo para el toggle */
      .form-check-input {
        background-color: #4c78dd !important; /* Color azul para R (por defecto) */
        border-color: #4c78dd !important;
        width: 3.5em !important; /* Aumentar el ancho del toggle */
        height: 1.8em !important; /* Aumentar la altura proporcionalmente */
      }

      /* Estilo cuando está activado (Python) */
      .form-check-input:checked {
        background-color: #4CAF50 !important; /* Color verde para Python */
        border-color: #4CAF50 !important;
      }

      /* Asegurar que la transición sea suave */
      .form-check-input {
        transition: background-color 0.3s, border-color 0.3s;
      }

      /* Ajustar el círculo indicador dentro del toggle */
      .form-switch .form-check-input:after {
        height: calc(1.8em - 4px) !important;
        width: calc(1.8em - 4px) !important;
      }

      /* Ajustar el espacio del contenedor */
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
            id = "toggle",
            type = "checkbox",
            class = "form-check-input",
            role = "switch"
          )
        ),
        # span("Python", class = "fw-bold"),
        uiOutput("toggle_state", inline = TRUE)
      )
    )
  })

  # Muestra "input" o "output" según el estado del toggle
  output$toggle_state <- renderUI({
    the_selection <- ifelse(test = input$toggle, yes = "output", no = "input")
    span(the_selection, class = "fw-bold")
  })

  output$"the_super_side" <- renderUI({
    div(
      conditionalPanel(
        condition = "input.toggle == false",
        #ns = ns,
        uiOutput("input_side_panel")
      ),
      conditionalPanel(
        condition = "input.toggle == true",
        #ns = ns,
        uiOutput("output_side_panel")
      )
    )
  })

  output$"input_side_panel" <- renderUI({

    div(
      # class = "d-flex flex-column align-items-center",
      card(
        style = "height: 77vh; min-height: 77vh;",  # Altura de la card (100% del contenedor padre)

        actionButton(
          inputId = "btn_dataset",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            #icon("database", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            icon("database", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            #span("Dataset")
          ),
          class = "btn-primary",
          #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
          title = ""
        ),
        actionButton(
          inputId = "btn_var_selector",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            icon("filter", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            #span("Dataset")
          ),
          class = "btn-primary",
          #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
          title = ""
        ),
        actionButton(
          inputId = "btn_config",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            icon("sliders", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            #span("Dataset")
          ),
          class = "btn-primary",
          #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
          title = ""
        ),
        actionButton(
          inputId = "btn_play_front",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            icon("play", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            #span("Dataset")
          ),
          class = "btn-primary",
          #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
          title = ""
        ),
        actionButton(
          inputId = "btn_refresh",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            icon("arrows-rotate", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            #span("Dataset")
          ),
          class = "btn-primary",
          #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
          title = ""
        )
      )
    )
  })

  # step01_Dataset         <- reactiveVal("open")
  # step02_VarSelection    <- reactiveVal("waiting")
  # step03_SpecialSettings <- reactiveVal("waiting")
  # step04_Play            <- reactiveVal("waiting")

  the_list01_Dataset_internal <- MASTER_module_import_server(id = "MASTER_import", show_dev = FALSE)


  the_list01_Dataset_stone <- reactiveValues("Source: " = NA, "File: " = NA, "Shape: "= NA)

  output$"super_dataset_selection" <- renderUI({
    MASTER_module_import_ui(id = "MASTER_import")
    # card(
    #   style = "height: 70vh; min-height: 70vh;",
    #   full_screen = TRUE,
    #   card_header(
    #     # class = "d-flex align-items-center",
    #     style = "background-color: #ff9a3c; color: white; border-bottom: 1px solid #e67e22; padding-left: 10px;",
    #     tags$i(class = "fa fa-database me-2"),
    #     tags$b("Data Import")
    #   ),
    #   card_body(
    #     style = "padding: 15px; background-color: #fff3e6;",
    #     uiOutput("box01_data_source")
    #   )
    # )
  })

  observeEvent(input$btn_dataset, {



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

    # req(the_list01_Dataset_internal())
    if (is.null(the_list01_Dataset_internal()$"my_dataset")) {
      # print(the_list01_Dataset_internal())
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

    ########
    the_nrow <- nrow(the_list01_Dataset_internal()$"my_dataset")
    the_ncol <- ncol(the_list01_Dataset_internal()$"my_dataset")

    # 3) Put on stone
    the_list01_Dataset_stone$"Source: " <- the_list01_Dataset_internal()[["data_source"]]
    the_list01_Dataset_stone$"File: "   <- the_list01_Dataset_internal()[["original_file_name"]]
    the_list01_Dataset_stone$"Shape: "  <- paste0(the_nrow, " Rows", " x ", the_ncol, " Cols")

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

  the_list02_VarSelection_stone <- reactiveValues("var_name_factor" = NA,
                                                  "var_name_rv" = NA,
                                                  "alpha_value" = NA,
                                                  "vector_var_names" = NA,
                                                  "minidataset" = NA,
                                                  "ncol" = NA,
                                                  "nrow" = NA,
                                                  "str_shape" = NA)




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

  the_list03_SpecialSettigns_stone <- reactiveValues("df_order" = NA,
                                                  "vector_ordered_levels" = NA,
                                                  "vector_ordered_colors" = NA,
                                                  "minidataset" = NA)

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
  observeEvent(input$"btn_config", {



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
    shinyjs::removeClass(id = "btn_config", class = "btn-primary")
    shinyjs::addClass(id = "btn_config",  class = "btn-success")

    # 3) Put on stone
    vector_ordered_levels <- the_list03_SpecialSettigns_internal()$"vector_ordered_levels"
    vector_ordered_colors <- the_list03_SpecialSettigns_internal()$"vector_ordered_colors"
    minidaset_without_change <- the_list02_VarSelection_stone$"minidataset"
    var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"
    minidaset_with_change <- factor(
      x = minidaset_without_change[,var_name_factor],       # La variable original de factor
      levels = vector_ordered_levels  # El orden de los niveles que calculamos en el Paso 2
    )

    the_list03_SpecialSettigns_stone$"df_order" <-  the_list03_SpecialSettigns_internal()$"df_order"
    the_list03_SpecialSettigns_stone$"vector_ordered_levels" <- the_list03_SpecialSettigns_internal()$"vector_ordered_levels"
    the_list03_SpecialSettigns_stone$"vector_ordered_colors" <- the_list03_SpecialSettigns_internal()$"vector_ordered_colors"
    the_list03_SpecialSettigns_stone$"minidataset" <- minidaset_with_change

    # 4) Remove Modal
    removeModal()

  })
  ###---------------------------------------------------------------------------
  output$"output_side_panel" <- renderUI({

    div(
      # style = "overflow-y: hidden; flex: 1; display: flex; flex-direction: column; min-height: 100%;",
      #
      # class = "d-flex flex-column align-items-center",
      card(
        # style = "height: 100%;",  # Altura de la card (100% del contenedor padre)
        style = "height: 77vh; min-height: 77vh;",  # Altura de la card (100% del contenedor padre)

        actionButton(
          inputId = "btn_classroom",
          label = tagList(
            # Ahora este icono se renderiza usando los archivos CSS locales
            #icon("database", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
            icon("chalkboard-user", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
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
            icon("download", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
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
      # style = "height: 90vh; width: 100%; overflow: hidden; display: flex; flex-direction: column;",
      conditionalPanel(
        condition = "input.toggle == false",
        #ns = ns,
        uiOutput("main_input_general")
      ),
      conditionalPanel(
        condition = "input.toggle == true",
        #ns = ns,
        uiOutput("main_output_general")
      ),
      "Rscience 1.0.11 - General Linear Model - Fixed Effects - Balanced tratments - Anova - Anova 1 Way - Script 01"

    )
  })

  output$"df_my_dataset" <- renderTable({
    the_list01_Dataset_internal()$"my_dataset"
  })

  output$"df_my_minidataset" <- renderTable({
    the_list02_VarSelection_stone$"minidataset"
  })

  output$"main_input_general" <- renderUI({
    # titlePanel("Gestor de Archivos con Estado Persistente (INPUT)"),


      # str_style_NAV_PANEL <- "flex-grow: 1; overflow-y: auto; height: 74vh; width: 100%;"
      str_style_NAV_PANEL <- "flex-grow: 1; overflow-y: auto; height: 74vh; width: 100%; overflow: hidden;"

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
          tags$h4("Input"),
        ),
        # title =
        # div(
          # style = "height: 90vh; width: 100%; overflow: hidden;", # Asegurar que el contenedor tenga altura suficiente

          bslib::nav_panel(
                  title = "user_selection",
                  h4("User Selection"),
                  tags$div(
                    # style = "flex-grow: 1; overflow-y: auto;",
                    style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente
                    p("Mostramos la selección... (Este texto es mínimo, pero el contenedor ocupa el 90vh completo.)"),
                    # fn_infoUI_zocalo_dataset(data_obj = the_list01_Dataset_internal()),
                    # fn_infoUI_zocalo_01_dataset(data_obj = the_list01_Dataset_show()),
                    fn_infoUI_zocalo_01_dataset(data_obj = reactiveValuesToList(the_list01_Dataset_stone)),


                    fn_infoUI_zocalo_02_VarSelection(data_obj = reactiveValuesToList(the_list02_VarSelection_stone)),

                    # reactiveValuesToList(la_lista01)
                    # DT::DTOutput("settings_table_display02")
                    tags$div(
                      # style = "flex-grow: 1; overflow-y: auto;",
                      style = "flex-grow: 1; overflow-y: auto; height: 74vh; width: 100%; overflow: auto;",

                    fn_infoUI_zocalo_03_container(dt_output_id = "settings_table_display02")
                    )
                  )


                ),
                bslib::nav_panel(
                  title = "dataset",
                  h4("Dataset"),
                  tags$div(
                    # style = "flex-grow: 1; overflow-y: auto;",
                    style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente
                    tags$div(
                      # style = "flex-grow: 1; overflow-y: auto;",
                      style = "flex-grow: 1; overflow-y: auto; height: 74vh; width: 100%; overflow: auto;",
                      tableOutput("df_my_dataset")
                    )
                  )
                ),
                bslib::nav_panel(
                  title = "minidataset",
                  h4("minidataset"),
                  tags$div(
                    # style = "flex-grow: 1; overflow-y: auto;",
                    style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente
                    tags$div(
                      # style = "flex-grow: 1; overflow-y: auto;",
                      style = "flex-grow: 1; overflow-y: auto; height: 74vh; width: 100%; overflow: auto;",
                      tableOutput("df_my_minidataset")
                    )
                  )
                ),
              bslib::nav_panel(
                title = "control",
                h4("Control"),
                tags$div(
                  # style = "flex-grow: 1; overflow-y: auto;",
                  style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente

                  "Mostramos el dataset..."
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

             downloadButton(outputId = "descargar02",
                            label = NULL,
                            icon = icon("download", class = "fa-2x"),
                            class = "btn-warning btn-sm"),

             actionButton(inputId = "open02",
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
        column(2, tags$h4("Output")),
        column(8),
        column(2, uiOutput("botonera_html"))
        )
        ),

      card_body(
      class = "p-0",
        tags$div(
            # style = "flex-grow: 1; overflow-y: auto;",
            style = "flex-grow: 1; overflow-y: auto; height: 84vh; width: 100%; overflow: hidden;", # Asegurar que el contenedor tenga altura suficiente

            # Contenido que deseas mostrar dentro de la tarjeta
            htmlOutput("html_viewer")
          )
        )
    )





  })

  output$"main_output_02_html_report" <- renderUI({
    div(
      shiny::titlePanel("Gestor de Archivos con Estado Persistente (OUTPUT)"),
      bslib::navset_card_tab(
        # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
        title = 'Look at them penguins!',

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

  str_input_folder_package <- reactive({

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

  str_input_folder_quarto <- reactive({

    selected_package_path <- str_input_folder_package()

    vector_folder_paths <- list.dirs(path = selected_package_path, recursive = T)
    dt_selected_quarto_folder <- grepl("quarto$", vector_folder_paths, ignore.case = TRUE)
    selected_quarto_folder_path <- vector_folder_paths[dt_selected_quarto_folder]

    return(selected_quarto_folder_path)



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
    update_modal_progress <- function(value, message, detail = "") {
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
      update_modal_progress(value = 0.05, message= "Inicializando", detail = "Preparando variables y entorno...")

      # 1. Crear carpeta temporal (10%)
      update_modal_progress(0.10, "Preparación de archivos", detail = "Creando carpeta temporal de trabajo...")
      my_output_folder01 <- create_new_temporal_output_folder_path()
      str_output_folder01(my_output_folder01)
      dir.create(my_output_folder01, recursive = TRUE)

      # 2. Copiar archivos (25%)
      update_modal_progress(0.25, "Preparación de archivos", detail = "Copiando plantillas y dependencias...")
      fs::dir_copy(
        path = str_input_folder_quarto(),
        new_path = str_output_folder01(),
        overwrite = T
      )

      # 3. Definir rutas (40%)
      update_modal_progress(0.40, "Preparación de archivos", detail = "Calculando rutas y nombres de archivo...")
      file_name_no_ext <- tools::file_path_sans_ext(str_file_name_input_qmd())
      str_pdf_file_name <- paste0(file_name_no_ext,"_", the_time_here_format(), ".pdf")
      str_output_file_name_pdf(str_pdf_file_name)
      my_str_pdf <- file.path(str_output_folder01(), str_output_file_name_pdf())
      str_output_file_path_pdf(my_str_pdf)

      # 4. Configurar entorno de renderizado (50%)
      update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")
      dir_original <- getwd()
      my_temporal_folder <- str_output_folder01()
      setwd(my_temporal_folder)

      # 5. Llamada BLOQUEANTE (50% -> 90%)
      update_modal_progress(0.55, "Renderizando Quarto", detail = "Ejecutando el renderizado (puede tardar)...")

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
      update_modal_progress(0.90, "Renderizando Quarto", detail = "Renderizado completado. Finalizando...")


      # === PASO C: Finalización Exitosa (90% - 100%) ===

      # C1. Terminar barra de progreso al 100%
      update_modal_progress(1.0, "¡Proceso Completado!", detail = "Éxito al generar el reporte.")

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
  str_file_name_input_qmd02 <- reactive({"report_template_html.qmd"})
  str_file_path_input_qmd02 <- reactive({


    str_path_qmd <- file.path(str_input_folder_quarto(), str_file_name_input_qmd02())
    str_path_qmd
  })

  str_output_folder02 <- reactiveVal(NULL)
  str_output_file_name_html    <- reactiveVal(NULL)
  str_output_file_path_html    <- reactiveVal(NULL)
  the_time_here_format02        <- reactiveVal(NULL)
  # --- Lógica del Botón "Generar" (Naranja -> Verde) ---


  ANCESTRAL_PLAY <- reactiveVal(FALSE)
  observeEvent(input$"btn_play_html", {

    ANCESTRAL_PLAY(TRUE)
  })
  observeEvent(input$"btn_play_front", {

    ANCESTRAL_PLAY(TRUE)
    })

  observeEvent(ANCESTRAL_PLAY(), {
    req(ANCESTRAL_PLAY())
    # 1. INICIALIZACIÓN: Crear el objeto de progreso y bloquear la pantalla

    ##############

    # Define el nuevo estado (TRUE para checked, FALSE para unchecked)
    nuevo_estado <- TRUE # O FALSE

    # Construir el código JavaScript
    js_code <- paste0(
      # 1. Cambia visualmente el estado del checkbox
      "var checkbox = document.getElementById('toggle');",
      "checkbox.checked = ", tolower(nuevo_estado), ";",

      # 2. ¡CLAVE! Notifica a Shiny (R) del nuevo valor
      "Shiny.setInputValue('toggle', checkbox.checked, {priority: 'event'});"
    )

    # 3. Ejecutar el código JavaScript
    shinyjs::runjs(js_code)
    ###############################

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
    update_modal_progress <- function(value, message, detail = "") {
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

    # 3. MANEJO DEL FLUJO CON tryCatch (Avanzando paso a paso)
    tryCatch({

      # === PASOS INTERMEDIOS (Se mantienen iguales) ===
      update_modal_progress(value = 0.05, message= "Inicializando", detail = "Preparando variables y entorno...")

      # 1. Crear carpeta temporal (10%)
      update_modal_progress(0.10, "Preparación de archivos", detail = "Creando carpeta temporal de trabajo...")
      my_output_folder02 <- create_new_temporal_output_folder_path()
      str_output_folder02(my_output_folder02)
      dir.create(my_output_folder02, recursive = TRUE)

      # 2. Copiar archivos (25%)
      update_modal_progress(0.25, "Preparación de archivos", detail = "Copiando plantillas y dependencias...")
      fs::dir_copy(
        path = str_input_folder_quarto(),
        new_path = str_output_folder02(),
        overwrite = T
      )

      # 3. Definir rutas (40%)
      update_modal_progress(0.40, "Preparación de archivos", detail = "Calculando rutas y nombres de archivo...")
      file_name_no_ext <- tools::file_path_sans_ext(str_file_name_input_qmd02())
      str_html_file_name <- paste0(file_name_no_ext,"_", the_time_here_format(), ".html")
      str_output_file_name_html(str_html_file_name)
      my_str_html <- file.path(str_output_folder02(), str_output_file_name_html())
      str_output_file_path_html(my_str_html)

      # 4. Configurar entorno de renderizado (50%)
      update_modal_progress(0.50, "Renderizando Quarto", detail = "Cargando contexto de ejecución...")
      dir_original <- getwd()
      my_temporal_folder <- str_output_folder02()
      setwd(my_temporal_folder)

      # 5. Llamada BLOQUEANTE (50% -> 90%)
      update_modal_progress(0.55, "Renderizando Quarto", detail = "Ejecutando el renderizado (puede tardar)...")

      # print()
      list_for_replace <- list()
      list_for_replace[["AAA_import_dataset_internal_AAA"]] <-  the_list01_Dataset_internal()$"str_import_internal"
      list_for_replace[["AAA_import_dataset_external_AAA"]] <-  the_list01_Dataset_internal()$"str_import_external"
      list_for_replace["BBB_var_name_rv_BBB"] <- the_list02_VarSelection_stone$"var_name_rv"
      list_for_replace["BBB_var_name_factor_BBB"] <- the_list02_VarSelection_stone$"var_name_factor"
      list_for_replace["BBB_alpha_value_BBB"] <- the_list02_VarSelection_stone$"alpha_value"
      list_for_replace["CCC_vector_ordered_levels_CCC"] <- paste0("c(", paste(shQuote(the_list03_SpecialSettigns_stone$"vector_ordered_levels", type = "sh"), collapse = ", "), ")")
      list_for_replace["CCC_vector_ordered_colors_CCC"] <- paste0("c(", paste(shQuote(the_list03_SpecialSettigns_stone$"vector_ordered_colors", type = "sh"), collapse = ", "), ")")
      #
      # print(list.files())
      the_file <- "file00_01_RQuarto.qmd"
      # the_file <- "inst/quarto/file00_01_RQuarto.qmd"
      contenido_archivo <- readLines(the_file, warn = FALSE)
      # # --- PASO 2: Realizar el reemplazo masivo ---
      # # str_replace_all(string_a_modificar, lista_patrones_y_reemplazos)
      # # 1. Convertir la lista en un vector con nombre
      vector_for_replace <- unlist(list_for_replace)
      names(vector_for_replace) <- names(list_for_replace)
      #
      # # 2. Realizar el reemplazo masivo con el vector
      contenido_reemplazado <- stringr::str_replace_all(string = contenido_archivo,
                                                        vector_for_replace)

      # contenido_reemplazado <- stringr::str_replace_all(string = contenido_archivo,
      #                                                   pattern = names(vector_for_replace),
      #                                                   replacement = vector_for_replace)

      contenido_reemplazado <- stringr::str_replace_all(string = contenido_reemplazado,
                                                        pattern = "\\#\\+\\+\\+---",
                                                        replacement = "")


      # --- PASO 3: Guardar el archivo modificado ---
      writeLines(contenido_reemplazado, the_file)


      quarto::quarto_render(input = basename(str_file_name_input_qmd02()),
                            output_format = "html",
                            output_file = basename(str_output_file_name_html()),
                            execute_params = list(    activate_params= "FALSE",
                                                      file_source= "from_params",
                                                      file_name= "from_params",
                                                      the_package= "from_params",
                                                      tool_used= "from_params",
                                                      script_used= "from_params",
                                                      current_time= "from_params"),
                            #execute_params = my_bag,
                            quiet = FALSE)

      setwd(dir_original)

      # 6. Progreso tras el bloqueo (90%)
      update_modal_progress(0.90, "Renderizando Quarto", detail = "Renderizado completado. Finalizando...")


      # === PASO C: Finalización Exitosa (90% - 100%) ===

      # C1. Terminar barra de progreso al 100%
      update_modal_progress(1.0, "¡Proceso Completado!", detail = "Éxito al generar el reporte.")

      # C2. Actualizar estado y color del botón
      message(crayon::green("Process completed!"))
      message("")

      # 1. CAMBIO DE COLOR B1: Naranja -> Verde (Persistente)
      # Usa 'later' para ejecutar el código JavaScript después de 300 milisegundos.
      # Esto le da tiempo al navegador para renderizar todos los elementos pendientes.
      # later::later(function() {



      # }, delay = 0.3) # 0.3 segundos es un buen valor inicial

      # session$onFlush(once = TRUE, function() {
      #
      #   # Estos comandos se ejecutarán solo después de que Shiny
      #   # haya enviado la señal para renderizar todos los elementos.
      #
      #   shinyjs::removeClass(id = "btn_play_html", class = "btn-warning")
      #   shinyjs::addClass(id = "btn_play_html", class = "btn-success")
      #
      #   removeClass("btn_play_front", "btn-primary")
      #   addClass("btn_play_front", "btn-success")
      #
      # })


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
      progress$close()
    })

    # 5. 🟢 DESBLOQUEAR LA PANTALLA
    removeModal()
    message("")



  })


  # En tu server
  observeEvent(ANCESTRAL_PLAY(), {
    req(ANCESTRAL_PLAY())

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
  })

  observeEvent(input$open02, {
    # C2. Actualizar estado y color del botón
    message(crayon::green("OPen completed!"))
    message("")

    # 1. CAMBIO DE COLOR B1: Naranja -> Verde (Persistente)
    removeClass("open02", "btn-warning")
    addClass("open02", "btn-success")

    # --- CAMBIO CLAVE AQUÍ ---
    # 2. Obtener la URL del archivo
    # Usamos isolate() para asegurarnos de que el observeEvent solo reaccione a input$open02
    # y no a cambios en str_output_file_path_html (si es un reactive)

    html_path <- isolate(str_output_file_path_html())

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
  output$descargar02 <- downloadHandler(

    filename = function() {
      la_ruta <- str_output_file_name_html()

      if (!is.null(la_ruta)) {
        basename(la_ruta)
      } else {
        "archivo_vacio.html"
      }
    },


    content = function(file) {
      archivo_a_descargar <- str_output_file_path_html()

      if (!is.null(archivo_a_descargar) && file.exists(archivo_a_descargar)) {

        # 1. CAMBIO DE COLOR B2: Naranja -> Verde (Persistente)
        runjs("
                    // Quitamos la clase temporal 'disabled' si la puso el navegador
                    $('#descargar02').removeClass('disabled');
                    $('#descargar02').removeClass('btn-warning');
                    $('#descargar02').addClass('btn-success');

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
    req(str_output_file_path_html())

    html_path <- str_output_file_path_html()

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
    req(str_output_file_path_html())

    html_path_original <- str_output_file_path_html()

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
}

shinyApp(ui, server)
