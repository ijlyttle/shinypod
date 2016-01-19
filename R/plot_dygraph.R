# Design Questions:
# 1. Should the different functions be separate or packaged together?
# 2. The server function: should it be a proper function -- should it have a proper return value and not just modify the environment?
# 3. Server function: should each reactive data source be it's own function input?  Or should it be packaged in a reactive list?

dygraph_input <- function(id) {
  ns <- NS(id)

  tagList(
    # time
    shiny::uiOutput(ns("controller_time_out")),

    # Y1 axes
    shiny::uiOutput(ns("controller_y1_out")),

    # Y2 axes
    shiny::uiOutput(ns("controller_y2_out"))
  )
}

dygraph_output <- function(id) {
  ns <- NS(id)

  tagList(
    tags$pre(
      "Zoom: Click-drag\tPan: Shift-Click-Drag\tReset: Double-Click"
    ),
    dygraphs::dygraphOutput(ns("view_dygraph"))
  )
}

dygraph_server <- function(
  input, output, session, plot_df) {

  # reactives
  rct_data <- reactive({

    shiny::validate(
      shiny::need(plot_df, "No data")
    )

    plot_df
  })

  rct_var_time <- reactive({
    df_names_inherits(rct_data(), c("POSIXct"))
  })

  rct_var_num <- reactive({
    var_num <- df_names_inherits(rct_data(), c("numeric", "integer"))

    shiny::validate(
      shiny::need(var_num, "Cannot display graph: dataset has no numeric variables")
    )

    var_num
  })

  selection <- reactiveValues(
    time = NULL,
    Y1 = NULL,
    Y2 = NULL
  )

  # observers
  shiny::observe({
    selection$time <- input[["controller_time"]]
    selection$Y1 <- input[["controller_y1"]]
    selection$Y2 <- input[["controller_y2"]]
  })

  # when the y-variables change,
  #   if there are y-varaiables available to select,
  #     and there are no y-variables selected:
  #     then - put the first y-variable on the Y1 axis
  shiny::observeEvent(
    eventExpr = rct_var_num(),
    handlerExpr = {
      if (is.null(selection$Y1) &&
          is.null(selection$Y2)   ){
        selection$Y1 <- rct_var_num()[[1]]
      }
    }
  )

  # outputs

  # select time variable
  output[["controller_time_out"]] <-
    renderUI({
      ns <- session$ns
      selectizeInput(
        inputId = ns("controller_time"),
        label = "Time",
        choices = rct_var_time(),
        selected = selection$time
      )
    })

  # select Y1 variable
  output[["controller_y1_out"]] <-
    renderUI({
      ns <- session$ns
      selectizeInput(
        inputId = ns("controller_y1"),
        label = "Y1 axis",
        choices = setdiff(rct_var_num(), input[["controller_y2"]]),
        multiple = TRUE,
        selected = selection$Y1
      )
    })

  # select Y2 variable
  output[["controller_y2_out"]] <-
    renderUI({
      ns <- session$ns
      selectizeInput(
        inputId = ns("controller_y2"),
        label = "Y2 axis",
        choices = setdiff(rct_var_num(), input[["controller_y1"]]),
        multiple = TRUE,
        selected = selection$Y2
      )
    })

  # dygraph
  output[["view_dygraph"]] <- dygraphs::renderDygraph({
  #return(reactive({
    var_time <- selection$time
    var_y1 <- selection$Y1
    var_y2 <- selection$Y2

    shiny::validate(
      shiny::need(var_time, "Graph cannot display without a time-variable"),
      shiny::need(c(var_y1, var_y2), "Graph cannot display without any y-variables")
    )

    # create the mts object
    vec_time <- rct_data()[[var_time]]
    df_num <- rct_data()[c(var_y1, var_y2)]

    dy_xts <- xts::xts(df_num, order.by = vec_time, lubridate::tz(vec_time))

    dyg <- dygraphs::dygraph(dy_xts)
    #dyg <- do.call(dygraphs::dyOptions, c(list(dyg), rctval_dyopt[[item_dyopt]]))
    dyg <- dygraphs::dyAxis(dyg, "x", label = var_time)
    dyg <- dygraphs::dyAxis(dyg, "y", label = paste(var_y1, collapse = ", "))
    dyg <- dygraphs::dyAxis(dyg, "y2", label = paste(var_y2, collapse = ", "))

    # put stuff on y2 axis
    for(v in var_y2) {
      dyg <- dygraphs::dySeries(dyg, v, axis = "y2")
    }

    dyg
  #}))

  })
}