
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
MY_PACKAGE_NAME <- "exp002RShiny"

ui <- page_sidebar(
  shinyjs::useShinyjs(),
  # Carga los recursos CSS y JS de Font Awesome de la librería local de Shiny
  tags$head(
    # Esto carga la versión de Font Awesome incluida en el paquete shiny
    tags$link(rel = "stylesheet", href = "shared/font-awesome/css/all.min.css")
  ),
  # CSS con !important para forzar los colores (Mismo CSS robusto anterior)
  tags$head(
    tags$style(HTML("
            /* Estilo NARANJA (btn-warning) FORZADO */
            .btn-warning {
                background-color: #ff8c00 !important;
                color: white !important;
                border-color: #cc7000 !important;
            }
            /* Estilo VERDE (btn-success) FORZADO */
            .btn-success {
                background-color: #4CAF50 !important;
                color: white !important;
                border-color: #388E3C !important;
            }
            /* Asegura que los estados focus/active/hover usen nuestros colores */
            .btn-success:active, .btn-success:focus, .btn-success:hover {
                background-color: #4CAF50 !important;
                border-color: #388E3C !important;
            }
            .btn-warning:active, .btn-warning:focus, .btn-warning:hover {
                background-color: #ff8c00 !important;
                border-color: #cc7000 !important;
            }
        "))
  ),

  sidebar = sidebar(
    "v1.0.11",
    "Version con pestañas.",
    uiOutput("the_toggle"),
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


  ),
  conditionalPanel(
    condition = "input.toggle == false",
    #ns = ns,
    uiOutput("main_input_general")
  ),
  conditionalPanel(
    condition = "input.toggle == true",
    #ns = ns,
    uiOutput("main_output_general")
  )
)

server <- function(input, output, session) {

  output$"main_input_general" <- renderUI({

    div(
      titlePanel("Gestor de Archivos con Estado Persistente (INPUT)"),
      navset_card_tab(
        # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
        title = 'Look at them penguins!',


        nav_panel(
          title = "user_selection",

          # Usamos layout_columns para dividir el espacio
          "Mostramos la seleccion..."
        ),
        nav_panel(
          title = "dataset",

          "Mostramos el dataset..."
          # div(uiOutput("html_viewer"))

        ),

        # Define las pestañas con nav_panel()
        nav_panel(
          title = "Gráfico Principal",
          h1('Penguins are cool!'),
          value_box(
            'Number of penguins',
            value = textOutput('out_n_penguins'),
            showcase = shiny::icon('hashtag'),
            min_height = 100,
            max_height = 150
          ),
          textOutput("mensaje_estado"),
          plotOutput('out_plt_penguins')
        )
      )
    )
  })

  # output$"main_output_general" <- renderUI({
  #   div(uiOutput("main_output_01_html_report"),
  #       uiOutput("main_output_02_html_report"))
  #
  # })

  output$"main_output_01_html_report" <- renderUI({
    div(
      titlePanel("Gestor de Archivos con Estado Persistente (OUTPUT)"),
      fluidRow(
        # Usar una columna para contener todos los botones
        # 'width = 12' ocupa todo el ancho de la fila
        column(width = 12,
               # Los botones ahora se alinearán horizontalmente por defecto,
               # especialmente si separamos las llamadas a 'br()'
               actionButton(inputId = "generar02",
                            label = NULL,
                            icon = icon("play", class = "fa-2x"),
                            class = "btn-warning"),

               downloadButton(outputId = "descargar02",
                              label = NULL,
                              icon = icon("download", class = "fa-2x"),
                              class = "btn-warning"),

               actionButton(inputId = "open02",
                            label = NULL,
                            icon = icon("binoculars", class = "fa-2x"),
                            class = "btn-warning")
               # **Importante:** Quitamos todos los 'br()' que causaban los saltos de línea.
        )
      ),
      div(
        style = "height: 100vh; width: 100%; overflow: hidden;", # Asegurar que el contenedor tenga altura suficiente

        # htmlOutput("html_viewer2"),
        htmlOutput("html_viewer")
      )


            )


  })

  output$"main_output_02_html_report" <- renderUI({
    div(
      titlePanel("Gestor de Archivos con Estado Persistente (OUTPUT)"),
      navset_card_tab(
        # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
        title = 'Look at them penguins!',

        nav_panel(
          title = "folder_files",
          "Despues aca el path y los files."
        ),
        nav_panel(
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
        ),


        # Define las pestañas con nav_panel()
        nav_panel(
          title = "Gráfico Principal",
          h1('Penguins are cool!'),
          value_box(
            'Number of penguins',
            value = textOutput('out_n_penguins'),
            showcase = shiny::icon('hashtag'),
            min_height = 100,
            max_height = 150
          ),
          textOutput("mensaje_estado"),
          plotOutput('out_plt_penguins')
        )
      )
    )
  })

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

  #############################################
  output$"input_side_panel" <- renderUI({

  div(
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
      inputId = "btn_play",
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
    })
  ##############################################
  output$"output_side_panel" <- renderUI({

    div(
      actionButton(
        inputId = "btn_ClassRoom",
        label = tagList(
          # Ahora este icono se renderiza usando los archivos CSS locales
          #icon("database", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
          icon("chalkboard-user", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
          #span("Dataset")
        ),
        class = "btn-primary",
        #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
        title = ""
      ),
      actionButton(
        inputId = "btn_general_download",
        label = tagList(
          # Ahora este icono se renderiza usando los archivos CSS locales
          icon("download", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
          #span("Dataset")
        ),
        class = "btn-primary",
        #style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
        title = ""
      )
    )
  })

  # Lo inicializamos en NULL o con el ID del botón que quieres activo por defecto.
  # Usaremos "btn_ClassRoom" como valor inicial.
  last_btn_clicked <- reactiveVal("btn_ClassRoom")

  # ------------------------------------------------------------------
  # 2. Observar los Clicks de los Botones
  # ------------------------------------------------------------------

  # Observar el botón ClassRoom
  observeEvent(input$btn_ClassRoom, {
    # req(input$btn_ClassRoom) no es estrictamente necesario, pero es bueno
    # Usamos isolate() para acceder al valor sin crear una dependencia
    # Solo actualiza si el valor es diferente (opcional, pero eficiente)
    if (isolate(last_btn_clicked()) != "btn_ClassRoom") {
      last_btn_clicked("btn_ClassRoom")
      message("Botón activo: btn_ClassRoom")
    }
  })

  # Observar el botón de Descarga
  observeEvent(input$btn_general_download, {
    # req(input$btn_general_download)
    if (isolate(last_btn_clicked()) != "btn_general_download") {
      last_btn_clicked("btn_general_download")
      message("Botón activo: btn_general_download")
    }
  })

  output$"main_output_general" <- renderUI({
    active_btn <- last_btn_clicked()

    if (active_btn == "btn_ClassRoom") {
      uiOutput("main_output_01_html_report")
      # return(h3("Mostrando contenido de ClassRoom (Análisis)"))
      # Aquí es donde llamarías a tu output$html_viewer2, por ejemplo:
      # return(uiOutput("html_viewer2"))

    } else if (active_btn == "btn_general_download") {
      uiOutput("main_output_02_html_report")
      # return(h3("Mostrando controles de Descarga General"))
      # Aquí mostrarías los controles de descarga

    } else {
      return(p("Seleccione una opción."))
    }
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
                            execute_params = list(file_name = "mtcars",
                                                  file_source = "r_source",
                                                  var_name_rv = "mpg",
                                                  var_name_factor = "cyl",
                                                  alpha_value = "0.05",
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



  output$out_plt_penguins <- renderPlot({
    palmerpenguins::penguins |>
      dplyr::filter(!is.na(sex)) |>
      ggplot(
        aes(
          x = body_mass_g,
          y = flipper_length_mm,
          fill = (species == input$in_species)
        )
      ) +
      geom_point(
        size = 4,
        shape = 21,
        col = 'white',
        show.legend = FALSE
      ) +
      geom_point(
        data = r_df_penguins(),
        size = 4,
        shape = 21,
        col = 'white',
        show.legend = FALSE
      ) +
      scale_fill_manual(
        values = c("TRUE" = 'dodgerblue4', "FALSE" = 'grey80')
      ) +
      theme_minimal(base_size = 12, base_family = 'Source Sans Pro') +
      labs(x = 'Weight (g)', y = 'Flipper length (mm)')
  })

  ####################################################





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


  observeEvent(input$generar02, {

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

      quarto::quarto_render(input = basename(str_file_name_input_qmd02()),
                            output_format = "html",
                            output_file = basename(str_output_file_name_html()),
                            execute_params = list(file_name = "mtcars",
                                                  file_source = "r_source",
                                                  var_name_rv = "mpg",
                                                  var_name_factor = "cyl",
                                                  alpha_value = "0.05",
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
      message(crayon::green("Process completed!"))
      message("")

      # 1. CAMBIO DE COLOR B1: Naranja -> Verde (Persistente)
      removeClass("generar02", "btn-warning")
      addClass("generar02", "btn-success")

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
    armado_v <- paste('<div style="height: 85%; width: 100%; "><iframe style="height: 85%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")

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
