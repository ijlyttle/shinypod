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

For each module foo, we also have a couple of functions that return ui arrangements for a sidebar layout: `foo_ui_side()` and `foo_ui_main()`, each of which calls `foo_ui()`.

The thought is for `foo_ui()` return a simple `shiny::tagList`, and for `foo_ui_side()` and `foo_ui_main()` to return more-customized html.


