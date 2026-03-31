server <- function(input, output, session) {
  cpi_data             <- read_rds("data/cpi_h.rds")
  parent_lookup        <- read_rds("data/parent_lookup.rds")
  reconciled_forecasts <- read_rds("data/reconciled_forecasts.rds")
  fc_ets               <- read_rds("data/fc_ets.rds")
  fc_arima             <- read_rds("data/fc_arima.rds")
  fc_boost             <- read_rds("data/fc_boost.rds")
  fc_prophet           <- read_rds("data/fc_prophet.rds")
  
  forecast_server("forecast", cpi_data, parent_lookup, reconciled_forecasts, fc_ets, fc_arima, fc_boost, fc_prophet)
  
  # JH and YQ add their module server calls here:
  # eda_server("eda", cpi_data)
  # decomp_server("decomp", decomp_data)
}