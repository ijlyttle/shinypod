# shinypod 0.0.99

This is a repository for reusable shiny modules.

We have reusable modules for:

* uploading and parsing a CSV file into a data-frame (including handling time-zones)
* (in progress) configuring a two-y-axes dygraph using a data-frame

## Installation

This is based on the dev version of shiny. To install:

```R
devtools::install_github("rstudio/shiny")
devtools::install_github("ijlyttle/shinypod")
```

## Philosophy

For each module foo, we have `foo_ui()` and `foo_server()`.

For each module foo, we also have a couple of functions that return ui arrangements for a sidebar layout: `foo_ui_sidebar_side()` and `foo_ui_sidebar_main()`, each of which calls `foo_ui()`.

The thought is for `foo_ui()` return a simple `shiny::tagList`, and for `foo_ui_sidebar_side()` and `foo_ui_sidebar_main()` to (possibly) return more-customized html.

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


