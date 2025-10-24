
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
  useShinyjs(),
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
    "v1.0.7",
    "Sin saltos, a lo largo.",
    selectInput(
      'in_species',
      'Penguin species',
      choices = palmerpenguins::penguins$species |> unique()
    ),
    # downloadButton(
    #   'btn_export_pdf',
    #   'Export Report',
    #   icon = shiny::icon('file-pdf')
    # ),
    downloadButton(
      'btn_export_excel',
      'Export Report',
      icon = shiny::icon('file-excel')
    )
  ),
  titlePanel("Gestor de Archivos con Estado Persistente (Ambos Verdes)"),


  navset_card_tab(
    # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
    title = 'Look at them penguins!',


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
    nav_panel(
      title = "HTML",

      # Usamos layout_columns para dividir el espacio
      layout_columns(
        col_widths = c(4, 4, 4), # Columna Izquierda (4 unidades), Columna Derecha (8 unidades)

        # === Columna Izquierda: Botones (4/12 del ancho) ===
        div(
          # 1. Botón Generar (Inicio: Naranja)
          actionButton("generar02", "1. Generar Carpeta y Archivo Temporal", class = "btn-warning"),
          br(), br(),
          # 2. Botón Descargar (Inicio: Naranja)
          downloadButton("descargar02", "2. Descargar Archivo HTML", class = "btn-warning")
          # Los br() ya no son necesarios dentro de una columna separada
        ),

        # === Columna Derecha: Output y Lista (8/12 del ancho) ===
        div(
          h2("Output folder path:"),
          uiOutput("text_output_folder_path02"),
          h2("List Files:"), # Tienes este h2 repetido, asegúrate de que sea intencional
          verbatimTextOutput("text_list_files02"),
          br()
        ),
        div(uiOutput("html_viewer"))
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

server <- function(input, output, session) {

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

  r_df_penguins <- reactive({
    palmerpenguins::penguins |>
      dplyr::filter(species == input$in_species)
  })

  output$out_n_penguins <- renderText({
    r_df_penguins() |> nrow()
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
  str_output_file_name_pdf   <- reactiveVal(NULL)
  str_output_file_path_pdf   <- reactiveVal(NULL)
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
                            execute_params = list(species = input$in_species),
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

output$btn_export_excel <- downloadHandler(
  filename = function() {
    glue::glue("{input$in_species}_raw.xlsx")
  },
  content = function(file) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")

    header_style <- openxlsx::createStyle(textDecoration = "bold")
    openxlsx::writeData(
      wb,
      "Data",
      r_df_penguins(),
      headerStyle = header_style
    )
    openxlsx::setColWidths(
      wb,
      "Data",
      cols = 1:ncol(r_df_penguins()),
      widths = c(rep(20, 6), 10, 5)
    )

    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
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

    # 01 - Str new folder output
    my_output_folder02 <- create_new_temporal_output_folder_path()
    str_output_folder02(my_output_folder02)

    # 02 - Create new folder output
    dir.create(my_output_folder02, recursive = TRUE)

    # 03 - Copy file from input folder to output folder
    print("La 1:")
    print(str_input_folder_quarto())
    print("La 2:")
    print(str_output_folder02())

    fs::dir_copy(
      path = str_input_folder_quarto(),
      new_path = str_output_folder02(),
      overwrite = T
    )

    # 03 - PDF - File name
    file_name_no_ext <- tools::file_path_sans_ext(str_file_name_input_qmd02())
    str_html_file_name <- paste0(file_name_no_ext,"_", the_time_here_format(), ".html")
    str_output_file_name_html(str_html_file_name)

    # 04 - PDF file path
    my_str_html <- file.path(str_output_folder02(), str_output_file_name_html())
    str_output_file_path_html(my_str_html)

    # Run quarto
    dir_original <- getwd()
    my_temporal_folder <- str_output_folder02()
    setwd(my_temporal_folder)

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

    message(crayon::green("Process completed!"))
    message("")

    # 1. CAMBIO DE COLOR B1: Naranja -> Verde (Persistente)
    removeClass("generar02", "btn-warning")
    addClass("generar02", "btn-success")



    output$mensaje_estado02 <- renderText({
      "¡Carpeta y archivo creados exitosamente! El Botón 1 está en verde. Listo para la descarga."
    })
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


  output$html_viewer <- renderUI({
    # 1. Asegúrate de que el path exista (o espera a que el PDF se genere)
    req(str_output_file_path_html())

    html_path <- str_output_file_path_html()

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

    # 5. Crear el iframe con dimensiones más pequeñas
    tags$iframe(
      # CAMBIOS AQUÍ: Reducción de height y width
      style = 'height: 400px; width: 100%; border: none;',
      src = html_url,
      type = "text/html"
    )
  })


}

shinyApp(ui, server)
