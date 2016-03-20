library("shiny")
library("shinypod")

shinyServer(function(input, output, session) {
  rct_data = reactive({
    letters
  })

  rct_choice_1 = reactive({
    choice_1 = setdiff(rct_data(), input[["second"]])

    choice_1
  })

  rct_choice_2 = reactive({
    choice_2 = setdiff(rct_data(), input[["first"]])

    choice_2
  })


  rct_choice_3 = reactive({
    choice_3 = setdiff(rct_data(), input[["fourth"]])

    choice_3
  })


  rct_choice_4 = reactive({
    choice_4 = setdiff(rct_data(), input[["third"]])

    choice_4
  })

  #choice 1
  shiny::observeEvent(
    eventExpr = rct_choice_1(),
    handlerExpr = {
      print("choice1::")
      print(rct_choice_1())
      updateSelectInput(
        session,
        inputId = "first",
        choices = rct_choice_1(),
        selected = update_selected(input[["first"]], rct_choice_1(), index = 1)
      )
    }
  )

  #choice 2
  shiny::observeEvent(
    eventExpr = rct_choice_2(),
    handlerExpr = {
      print("choice2::")
      print(rct_choice_2())
      updateSelectInput(
        session,
        inputId = "second",
        choices = rct_choice_2(),
        selected = update_selected(input[["second"]], rct_choice_2(), index = NULL)
      )
    }
  )

  #choice 3
  shiny::observeEvent(
    eventExpr = rct_choice_3(),
    handlerExpr = {
      updateSelectInput(
        session,
        inputId = "third",
        choices = rct_choice_3(),
        #selected = update_selected(input[["third"]], rct_choice_3(), index = 1)
        selected = rct_choice_3()[1]
      )
    }
  )

  #choice 4
  shiny::observeEvent(
    eventExpr = rct_choice_4(),
    handlerExpr = {
      updateSelectInput(
        session,
        inputId = "fourth",
        choices = rct_choice_4(),
        #selected = update_selected(input[["fourth"]], rct_choice_4(), index = NULL)
        selected = rct_choice_4()[2]
      )
    }
  )

  output$txt <- renderPrint({
    input[["first"]]
  })
})