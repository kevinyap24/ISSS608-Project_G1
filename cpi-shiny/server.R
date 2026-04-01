server <- function(input, output, session) {
# Shared data
cpi_data             <- read_rds("data/cpi_h.rds")
cpi_dashboard        <- read_rds("data/aa_cpi_dashboard.rds")
dashboard_table      <- read_rds("data/aa_dashboard_table.rds")
latest_lvl1          <- read_rds("data/aa_latest_lvl1.rds")
parent_lookup        <- read_rds("data/parent_lookup.rds")
reconciled_forecasts <- read_rds("data/reconciled_forecasts.rds")
fc_ets               <- read_rds("data/fc_ets.rds")
fc_arima             <- read_rds("data/fc_arima.rds")
fc_boost             <- read_rds("data/fc_boost.rds")
fc_prophet           <- read_rds("data/fc_prophet.rds")
  
# EDA module
eda_server("eda", cpi_data, cpi_dashboard, dashboard_table, latest_lvl1)
  
# Forecasting module
forecast_server("forecast", cpi_data, parent_lookup,
                  reconciled_forecasts, fc_ets, fc_arima, fc_boost, fc_prophet)
  
# JH add decomposition module here:
# decomp_server("decomp", cpi_data)
}
