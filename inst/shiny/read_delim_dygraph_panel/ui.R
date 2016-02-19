library("shiny")
library("shinyjs")
library("shinypod")
library("shinythemes")
library("shinypod")
library("bsplus")
library("dygraphs")

tagList(
  includeCSS(system.file(file.path("css", "omnibus.css"), package = "bsplus")),
  useShinyjs(),
  navbarPage(
    title = "Parse CSV & Dygraph",
    windowTitle = "Parse CSV & Dygraph",
    tabPanel(
      title = "Main",
      collapse_sidebar_set(
        id_set = "set",
        bstype_open = "primary",
        bstype_closed = "info",
        collapse_sidebar_layout(
          id_layout = "layout_csv",
          collapse_sidebar_panel =
            collapse_sidebar_panel(
              title = "Parse CSV",
              read_delim_sidebar_side("csv")
            ),
          collapse_main_panel =
            collapse_main_panel(
              bs_panel_open(
                read_delim_sidebar_main("csv")
              )
            )
        ),
        collapse_sidebar_layout(
          id_layout = "layout_dygraph",
          collapse_sidebar_panel =
            collapse_sidebar_panel(
              title = "Dygraph",
              dygraph_sidebar_side("dyg")
            ),
          collapse_main_panel =
            collapse_main_panel(
              bs_panel_open(
                dygraphOutput("csv_dyg")
              )
            )
        )
      )

    )
  ),
  includeScript(system.file(file.path("js", "navbar_mod_shiny.js"), package = "bsplus")),
  includeScript(system.file(file.path("js", "collapse_panel_set.js"), package = "bsplus")),
  includeScript(system.file(file.path("js", "strong_color.js"), package = "bsplus"))
)

