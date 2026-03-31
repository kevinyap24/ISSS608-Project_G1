pacman::p_load(shiny, tidyverse, plotly, timetk, modeltime, tidymodels, lubridate)

source("modules/mod_forecast.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)