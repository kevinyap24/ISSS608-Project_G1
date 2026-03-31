source("modules/mod_forecast.R")

ui <- navbarPage(
  title = "SG CPI Dashboard",
  # YQ and JH to add their tabs here later:
  # tabPanel("EDA", eda_ui("eda")),
  # tabPanel("Decomposition", decomp_ui("decomp")),
  tabPanel("Forecasting", forecast_ui("forecast"))
)