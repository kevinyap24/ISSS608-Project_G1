source("modules/mod_eda.R")
source("modules/mod_forecast.R")

ui <- navbarPage(
  title = "SG CPI Dashboard",
  theme = bslib::bs_theme(
    base_font = bslib::font_google("IBM Plex Sans"),
    code_font = bslib::font_google("IBM Plex Mono"),
    bg         = "#f5f0e8",
    fg         = "#1a1612",
    primary    = "#c0392b",
    secondary  = "#3d3530"
  ),
  tabPanel("EDA",eda_ui("eda")),
  tabPanel("Forecasting", forecast_ui("forecast"))
  # YQ and JH to add their tabs here later:
  # tabPanel("Decomposition", decomp_ui("decomp"))
)