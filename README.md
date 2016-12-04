# shinypod 0.0.99.9100

## Warning

*The API is not yet stable - but your feedback is welcome.*

As you write more-and-more shiny apps and they become more-and-more complex, you may notice a couple of things:

1. Among different apps, you may be doing the same things over and over again, like uploading and parsing csv files.
2. Within a given app, your ui and server functions may become difficult to manage as inputs, outputs, and reactives pile up.

Adressing these problems is the motivation for shiny to [introduce modules](http://shiny.rstudio.com/articles/modules.html).

The goals of this package are to propose a design framework for shiny modules, and provide some implementations.

So far, we have reusable modules for:

* uploading and parsing a CSV file into a data-frame (including handling time-zones)
* configuring a two-y-axes dygraph using a data-frame

## Installation

This package is not on CRAN; however, it is based on the new CRAN (0.13.0) version of shiny. To install:

```R
devtools::install_github("ijlyttle/shinypod")
```

## Philosophy

For a shiny module, we propose two layers of functions: (1) a fundamental layer that concerns itself with the inputs, outputs, and the return values from the server module, (2) a presentation layer that arranges the inputs and outputs according to some framework. For this package, for each "pod" there is a set of presentation functions based on the sidebar layout.

For each module `foo`, we have fundamental functions: `foo_ui()` and `foo_server()`. In the fundamental layer, we are concerned with returning a result, and with 

For each module `foo`, we also have a display functions, based on a sidebar layout: `foo_sb_side()`, `foo_sb_main()`, and `foo_sb_server()`. These functions rely on the fundamental functions. 

As a developer, if the sidebar functions work for you, you would have no reason to call the fundamental functions directly.

Each of the ui functions has an associated argument `id`, which is used to keep the shiny namespace orderly.

## Examples

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
          sidebarPanel(read_delim_sb_side("read_csv")),
          mainPanel(read_delim_sb_main("read_csv"))
        )
      )
    )  
  },
  server = {
    shinyServer(function(input, output, session) {
    
      rct_data <- callModule(read_delim_sb_server, id = "read_csv")
    
      observe(print(rct_data()))
    })  
  }
)

runApp(app)
```

## Deployed examples

Some simple shinypod apps deployed at shinyapps:

- [Parse CSV file](https://ijlyttle.shinyapps.io/read_delim/)
- [Parse CSV file and dygraph](https://ijlyttle.shinyapps.io/read_delim_dygraph/)
