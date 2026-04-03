pacman::p_load(readr, dplyr, purrr, modeltime, parsnip, timetk)

setwd("C:/Users/KEVIN/Documents/kevinyap24/ISSS608-Project_G1/cpi-shiny")

cc_cpi_data      <- read_rds("data/cpi_h.rds")
cc_parent_lookup <- read_rds("data/parent_lookup.rds")

cc_holdout_start <- as.Date("2025-01-01")
cc_holdout_end   <- as.Date("2025-12-01")
cc_all_series    <- unique(cc_parent_lookup$series)

fit_and_predict_holdout <- function(series_name) {
  message("Processing: ", series_name)
  
  cc_train <- cc_cpi_data %>%
    filter(series == series_name, date < cc_holdout_start) %>%
    select(date, cpi) %>% arrange(date)
  
  cc_holdout <- cc_cpi_data %>%
    filter(series == series_name, date >= cc_holdout_start, date <= cc_holdout_end) %>%
    select(date, cpi) %>% arrange(date)
  
  if (nrow(cc_train) < 24 || nrow(cc_holdout) == 0) return(NULL)
  
  models <- list(
    ets     = exp_smoothing()       %>% set_engine("ets"),
    arima   = arima_reg()           %>% set_engine("auto_arima"),
    boost   = arima_boost(trees=100, learn_rate=0.1) %>% set_engine("auto_arima_xgboost"),
    prophet = prophet_reg()         %>% set_engine("prophet")
  )
  
  results <- purrr::imap_dfr(models, function(spec, model_name) {
    tryCatch({
      fitted   <- parsnip::fit(spec, cpi ~ date, data = cc_train)
      tbl      <- modeltime_table(fitted)
      calib    <- modeltime_calibrate(tbl, new_data = cc_holdout)
      fc       <- modeltime_forecast(calib, new_data = cc_holdout, actual_data = cc_train)
      preds    <- fc %>% filter(.key == "prediction") %>% select(date = .index, predicted = .value)
      joined   <- inner_join(cc_holdout %>% rename(actual = cpi), preds, by = "date")
      rmse     <- sqrt(mean((joined$actual - joined$predicted)^2, na.rm = TRUE))
      
      tibble(series = series_name, model = model_name, rmse = rmse,
             predictions = list(preds %>% mutate(series = series_name)))
    }, error = function(e) {
      message("  Failed: ", model_name, " — ", e$message)
      NULL
    })
  })
  
  results
}

# Run for all series — this will take several minutes
all_results <- purrr::map_dfr(cc_all_series, fit_and_predict_holdout)

# Pick best model per series
best_per_series <- all_results %>%
  group_by(series) %>%
  slice_min(rmse, n = 1, with_ties = FALSE) %>%
  ungroup()

# Extract holdout predictions from best model
cc_auto_holdout_predictions <- purrr::map_dfr(
  best_per_series$predictions,
  function(pred_tbl) pred_tbl
)

# Save model performance summary too
cc_model_performance <- best_per_series %>%
  select(series, best_model = model, rmse)

write_rds(cc_auto_holdout_predictions, "data/cc_auto_holdout_predictions.rds")
write_rds(cc_model_performance,        "data/cc_model_performance.rds")

message("Done! Files saved to data/")