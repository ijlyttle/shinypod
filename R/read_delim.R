read_delim_ui <- function(id){

  ns <- shiny::NS(id)

  ui <- shiny::tagList()

  # specify file
  ui$file <-
    shiny::fileInput(
      inputId = ns("file"),
      label = "File",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  # specify delim
  ui$delim <-
    shiny::selectizeInput(
      inputId = ns("delim"),
      label = "Delimiter",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t")
    )

  # specify decimal
  ui$decimal_mark <-
    shiny::selectizeInput(
      inputId = ns("decimal_mark"),
      label = "Decimal mark",
      choices = c(Point = ".", Comma = ",")
    )

  # specify timezones
  tz_choice <- c("UTC", lubridate::olson_time_zones())

  # timezone to parse
  ui$tz_parse <-
    shiny::selectizeInput(
      inputId = ns("tz_parse"),
      label = shiny::tags$span(
        shiny::tags$a(
          id = ns("tz_parse_link"),
          "Timezone to parse",
          shiny::icon("info-circle")
        )
      ),
      choices = tz_choice
    )

  ui$tz_parse_modal <-
    shinyBS::bsModal(
      id = ns("tz_parse_modal"),
      title = "Timezones",
      trigger = ns("tz_parse_link"),
      size = "large",
      shiny::HTML(
        readr::read_lines(
          system.file("help", "read_delim", "tz.html", package = "shinypod")
        )
      )
    )

  # timezone to display
  ui$tz_display <-
    shiny::selectizeInput(
      inputId = ns("tz_display"),
      label = shiny::tags$span(
        shiny::tags$a(
          id = ns("tz_display_link"),
          "Timezone to display",
          shiny::icon("info-circle")
        )
      ),
      choices = tz_choice
    )

  ui$tz_display_modal <-
    shinyBS::bsModal(
      id = ns("tz_display_modal"),
      title = "Timezones",
      trigger = ns("tz_display_link"),
      size = "large",
      shiny::HTML(
        readr::read_lines(
          system.file("help", "read_delim", "tz.html", package = "shinypod")
        )
      )
    )

  # text output
  ui$text <-
    shiny::htmlOutput(
      outputId = ns("text"),
      container = pre_scroll
    )

  # data-frame output
  ui$data <-
    shiny::htmlOutput(
      outputId = ns("data"),
      container = pre_scroll
    )

  ui
}

read_delim_server <- function(
  input, output, session,
  delim = ",",
  decimal_mark = "."
){

  ns <- session$ns

  ## input updates ##
  ###################

  shiny::updateSelectizeInput(
    session,
    inputId = "delim",
    selected = delim
  )

  shiny::updateSelectizeInput(
    session,
    inputId = "decimal_mark",
    selected = decimal_mark
  )

  ## reactives ##
  ###############

  # reactive to read in the raw text from the file-specification input
  rct_txt <- reactive({

    shiny::validate(
      shiny::need(input$file, "File not selected")
    )

    infile <- input$file$datapath

    readr::read_file(infile)
  })

  rct_data <- reactive({

    df <-
      readr::read_delim(
        file = rct_txt(),
        delim = input$delim,
        locale = readr::locale(
          decimal_mark = input$decimal_mark,
          tz = input$tz_parse
        )
      )

    df <- df_with_tz(df, tz = input$tz_display)

    shiny::validate(
      shiny::need(is.data.frame(df), "No data")
    )

    df
  })

  ## outputs ##
  #############

  # sets the output for the raw text
  output$text <-
    renderUI({

      shinyjs::hide(ns("delim"))
      shinyjs::hide(ns("decimal_mark"))
      shinyjs::hide(ns("tz_parse"))
      shinyjs::hide(ns("tz_display"))

      shiny::validate(
        shiny::need(rct_txt(), "File did not load properly")
      )

      shinyjs::show(ns("delim"))
      shinyjs::show(ns("decimal_mark"))
      shinyjs::show(ns("tz_parse"))
      shinyjs::show(ns("tz_display"))

      shinyjs::toggle(
        id = ns("decimal_mark"),
        condition = df_has_numeric(rct_data())
      )

      shinyjs::toggle(
        id = ns("tz_parse"),
        condition = df_has_time_non_8601(
          rct_data(), rct_txt(), delim = input$delim
        )
      )

      shinyjs::toggle(
        id = ns("tz_display"),
        condition = df_has_time(rct_data())
      )

      h <- rct_txt()
      h <- readr::read_lines(h, n_max = 7)
      h <- paste(h, collapse = "<br/>")
      h <- shiny::HTML(h)

      h
    })

  # sets the output for the parsed dataframe
  output$data <-
    renderUI({

      h <-
        devtools::with_options(
          list(width = 10000, dpylr.width = Inf, dplyr.print_min = 6),
          capture.output(print(rct_data()))
        )
      h <- paste(h, collapse = "<br/>")
      h <- htmltools::HTML(h)

      h
    })


  # returns a dataframe
  rct_data
}
