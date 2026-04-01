pacman::p_load(
  shiny, tidyverse, plotly, timetk, modeltime, tidymodels, lubridate,
  DT, htmltools, bslib, reactable, reactablefmtr, bsicons,
  shinycssloaders, shinyWidgets, readr
)

source("modules/mod_eda.R")
source("modules/mod_forecast.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)