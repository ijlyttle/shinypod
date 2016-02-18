This app uses [shiny modules](http://shiny.rstudio.com/articles/modules.html) to:

- upload and parse a delimited file into a dataframe, using [readr](https://github.com/hadley/readr).
- if that dataframe has time-indexed data, plots it using [dygraphs](https://rstudio.github.io/dygraphs/).

Here is a sample [data file](https://raw.githubusercontent.com/ijlyttle/shinypod/master/inst/extdata/wx_ames.csv), describing weather observations in Ames, Iowa (USA) for a month.

These modules are provided in a package, [shinypod](https://github.com:ijlyttle/shinypod), hosted at GitHub.
