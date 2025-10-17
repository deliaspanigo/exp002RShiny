library(shiny)
library(bslib)
library(ggplot2)
library("miniUI")
library('rstudioapi')

ui <- page_sidebar(
  sidebar = sidebar(
    "v1.0.3",
    selectInput(
      'in_species',
      'Penguin species',
      choices = palmerpenguins::penguins$species |> unique()
    ),
    downloadButton(
      'btn_export_pdf',
      'Export Report',
      icon = shiny::icon('file-pdf')
    ),
    downloadButton(
      'btn_export_excel',
      'Export Report',
      icon = shiny::icon('file-excel')
    )
  ),

  h1('Penguins are cool!'),
  value_box(
    'Number of penguins',
    value = textOutput('out_n_penguins'),
    showcase = shiny::icon('hashtag'),
    min_height = 100,
    max_height = 150
  ),

  card(
    card_header('Look at them penguins!'),
    card_body(plotOutput('out_plt_penguins'))
  )
)

server <- function(input, output, session) {
  r_df_penguins <- reactive({
    palmerpenguins::penguins |>
      dplyr::filter(species == input$in_species)
  })

  output$out_n_penguins <- renderText({
    r_df_penguins() |> nrow()
  })


  my_folder_package <- reactive({

    the_package_path <- find.package("exp002RShiny")
    vector_folder_paths <- list.dirs(path = the_package_path, recursive = T)
    dt_selected_quarto_folder <- grepl("quarto$", vector_folder_paths, ignore.case = TRUE)
    selected_quarto_folder_path <- vector_folder_paths[dt_selected_quarto_folder]

    print(selected_quarto_folder_path)
    selected_quarto_folder_path
  })

  str_file_path_input_qmd <- reactive({

    folder_local <- 'inst/quarto/'
    folder_package <- my_folder_package()

    str_file <- "report_template.qmd"

    str_path_local   <- file.path(folder_local, str_file)
    str_path_package <- file.path(folder_package, str_file)

    str_selected <- ifelse(file.exists(str_path_local), str_path_local, str_path_package)
    str_selected
  })

  str_file_path_output_pdf <- reactive({
    folder_local <- 'inst/quarto/'
    folder_package <- my_folder_package()

    str_file <- "report_template.pdf"

    str_path_local   <- file.path(folder_local, str_file)
    str_path_package <- file.path(folder_package, str_file)

    str_selected <- ifelse(file.exists(str_path_local), str_path_local, str_path_package)
    str_selected
  })

output$btn_export_pdf <- downloadHandler(
  filename = function() {
    glue::glue("{input$in_species}_report.pdf")
  },
  content = function(file) {
    quarto::quarto_render(
      input = str_file_path_input_qmd(),
      execute_params = list(species = input$in_species)
    )
    fs::file_copy(
      str_file_path_output_pdf(),
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
}

shinyApp(ui, server) |> print()
