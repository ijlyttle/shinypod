# shinypod 0.0.99

This is a repository for reusable shiny modules.

We have reusable modules for:

* uploading and parsing a CSV file into a data-frame (including handling time-zones)
* (in progress) configuring a two-y-axes dygraph using a data-frame

## Installation

This package is not on CRAN; it is based on the new CRAN (0.13.0) version of shiny. To install:

```R
devtools::install_github("ijlyttle/shinypod")
```

## Philosophy

For each module foo, we have `foo_ui_input()`, `foo_ui_output()`, and `foo_server()`.

For each module foo, we also have a couple of functions that return ui arrangements for a sidebar layout: `foo_ui_sidebar_side()` and `foo_ui_sidebar_main()`.

The thought is for the fundamental functions, `foo_ui_input()` and `foo_ui_output()`, each to return a simple `shiny::tagList`, and for `foo_ui_sidebar_side()` and `foo_ui_sidebar_main()` to return more-customized html, having called the fundamental functions.

## Example

Let's say you wanted to be able to upload and parse a csv file, and have the dataframe be returned by a reactive function.

We can write this app using the "all-in-one" approach:

```R
library("shiny")
library("shinyjs")
library("shinyBS")
library("shinypod")

app <- shinyApp(
  ui = {
    shinyUI(
      fluidPage(
        useShinyjs(),
        sidebarLayout(
          sidebarPanel(read_delim_sidebar_side("read_csv")),
          mainPanel(read_delim_sidebar_main("read_csv"))
        )
      )
    )  
  },
  server = {
    shinyServer(function(input, output, session) {
    
      rct_data <- callModule(read_delim_server, id = "read_csv")
    
      observe(print(rct_data()))
    })  
  }
)

runApp(app)
```


