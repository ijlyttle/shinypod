library("shiny")
library("shinyjs")
library("shinypod")

# selection of files to provide
choice_file <-
  system.file("extdata", package = "shinypod") %>%
  list.files(pattern = "\\.csv", full.names = TRUE)

names(choice_file) <- basename(choice_file)

shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("read_delim"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          inputId = "sample_text",
          label = "Sample text",
          choices = choice_file
        ),
        read_delim_sb_side("data")
      ),
      mainPanel(
        htmlOutput(
          outputId = "text_preview",
          container = shinypod::pre_scroll
        ),
        read_delim_sb_main("data")
      )
    )
  )
)
