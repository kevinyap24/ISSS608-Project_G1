pacman::p_load(shiny,shinythemes,dplyr,lubridate,plotly,timetk,ggplot2,shinyWidgets,readr,
  tidyr,DT,htmltools,bslib,reactable,reactablefmtr,bsicons,shinycssloaders,
  dataui
)


# =========================================================
# LOAD DATA
# =========================================================
# joan
aa_cpi_data <- read_rds("data/aa_cpi_data.rds")
aa_cpi_h_display <- read_rds("data/aa_cpi_h_display.rds")

aa_cpi_dashboard <- read_rds("data/aa_cpi_dashboard.rds")
aa_dashboard_table_rds <- read_rds("data/aa_dashboard_table.rds")
aa_latest_lvl1 <- read_rds("data/aa_latest_lvl1.rds")

# kelvin
cc_cpi_data <- read_rds("data/cpi_h.rds")
cc_parent_lookup <- read_rds("data/parent_lookup.rds")
cc_reconciled_forecasts <- read_rds("data/reconciled_forecasts.rds")
cc_ets <- read_rds("data/fc_ets.rds")
cc_arima <- read_rds("data/fc_arima.rds")
cc_boost <- read_rds("data/fc_boost.rds")
cc_prophet<- read_rds("data/fc_prophet.rds")

# =========================================================
# DT STYLES
# =========================================================

trend_js <- JS(
  "function(data, type, row, meta) {",
  "  if(type === 'display') {",
  "    var color = data === '▲' ? 'green' : (data === '▼' ? 'red' : 'gray');",
  "    return '<span style=\"color:' + color + '; font-weight:bold; font-size:18px;\">' + data + '</span>';",
  "  }",
  "  return data;",
  "}"
)



# =========================================================
# TREND FUNCTION
# =========================================================

plot_cpi_time_series <- function(data,
                                 plot_level,
                                 category = NULL,
                                 group_name = NULL,
                                 series_select = NULL,
                                 use_year_colour = FALSE,
                                 facet_by = TRUE,
                                 facet_ncol = 1,
                                 facet_nrow = 1,
                                 facet_scales = "fixed",
                                 smooth = FALSE,
                                 smooth_period = "auto",
                                 smooth_span = NULL,
                                 smooth_degree = 2,
                                 plotly_slider = FALSE,
                                 interactive = TRUE,
                                 add_caption = TRUE,
                                 caption_text = "Source: CEIC Database | Index, 2024 = 100",
                                 title = NULL,
                                 x_lab = "",
                                 y_lab = "CPI Index",
                                 color_lab = "Year",
                                 line_color = "#2c3e50",
                                 return_plot = TRUE) {
  
  data <- data %>%
    filter(!is.na(series)) %>%
    mutate(year = factor(lubridate::year(date)))
  
  if (plot_level == 0) {
    plot_data <- data %>% filter(level == 0)
    default_title <- "CPI Top Level Time Series"
    facet_col <- "series"
    
  } else if (plot_level == 1) {
    plot_data <- data %>% filter(level == 1)
    default_title <- "CPI Major Groups Time Series"
    facet_col <- "division"
    
  } else if (plot_level == 2) {
    plot_data <- data %>% filter(level == 2)
    
    if (!is.null(category)) {
      plot_data <- plot_data %>% filter(division == category)
      default_title <- paste(category, "CPI Subcategories Time Series")
    } else {
      default_title <- "CPI Subcategories Time Series"
    }
    
    facet_col <- "series"
    
  } else if (plot_level == 3) {
    plot_data <- data %>% filter(level == 3)
    
    if (!is.null(category)) {
      plot_data <- plot_data %>% filter(division == category)
    }
    
    if (!is.null(group_name)) {
      plot_data <- plot_data %>% filter(group == group_name)
      default_title <- paste(group_name, "Detailed CPI Time Series")
    } else {
      default_title <- "Detailed CPI Time Series"
    }
    
    facet_col <- "series"
    
  } else {
    stop("plot_level must be 0, 1, 2, or 3")
  }
  
  if (!is.null(series_select) && length(series_select) > 0) {
    available_series <- unique(plot_data$series)
    missing_series <- setdiff(series_select, available_series)
    
    if (length(missing_series) > 0) {
      stop(
        paste(
          "These series were not found:",
          paste(missing_series, collapse = ", ")
        )
      )
    }
    
    plot_data <- plot_data %>% filter(series %in% series_select)
  }
  
  if (nrow(plot_data) == 0) {
    stop("No data found for the specified criteria")
  }
  
  if (is.null(title)) {
    title <- " "
  }
  if (smooth && !use_year_colour) {
    line_color <- "rgba(120,120,120,0.8)"
  } else {
    line_color <- "#2c3e50"
  }
  
  if (use_year_colour && facet_by) {
    p <- timetk::plot_time_series(
      .data = plot_data,
      .date_var = date,
      .value = cpi,
      .color_var = year,
      .facet_vars = tidyselect::all_of(facet_col),
      .facet_ncol = facet_ncol,
      .facet_nrow = facet_nrow,
      .facet_scales = facet_scales,
      .smooth = smooth,
      .smooth_period = smooth_period,
      .smooth_span = smooth_span,
      .smooth_degree = smooth_degree,
      .legend_show = TRUE,
      .title = title,
      .x_lab = x_lab,
      .y_lab = y_lab,
      .color_lab = color_lab,
      .interactive = interactive,
      .plotly_slider = plotly_slider
    )
    
  } else if (use_year_colour && !facet_by) {
    p <- timetk::plot_time_series(
      .data = plot_data,
      .date_var = date,
      .value = cpi,
      .color_var = year,
      .facet_ncol = facet_ncol,
      .facet_nrow = facet_nrow,
      .facet_scales = facet_scales,
      .smooth = smooth,
      .smooth_period = smooth_period,
      .smooth_span = smooth_span,
      .smooth_degree = smooth_degree,
      .legend_show = TRUE,
      .title = title,
      .x_lab = x_lab,
      .y_lab = y_lab,
      .color_lab = color_lab,
      .interactive = interactive,
      .plotly_slider = plotly_slider
    )
    
  } else if (!use_year_colour && facet_by) {
    p <- timetk::plot_time_series(
      .data = plot_data,
      .date_var = date,
      .value = cpi,
      .facet_vars = tidyselect::all_of(facet_col),
      .facet_ncol = facet_ncol,
      .facet_nrow = facet_nrow,
      .facet_scales = facet_scales,
      .line_color = line_color,
      .smooth = smooth,
      .smooth_period = smooth_period,
      .smooth_span = smooth_span,
      .smooth_degree = smooth_degree,
      .legend_show = FALSE,
      .title = title,
      .x_lab = x_lab,
      .y_lab = y_lab,
      .interactive = interactive,
      .plotly_slider = plotly_slider
    )
    
  } else {
    p <- timetk::plot_time_series(
      .data = plot_data,
      .date_var = date,
      .value = cpi,
      .facet_ncol = facet_ncol,
      .facet_nrow = facet_nrow,
      .facet_scales = facet_scales,
      .line_color = line_color,
      .smooth = smooth,
      .smooth_period = smooth_period,
      .smooth_span = smooth_span,
      .smooth_degree = smooth_degree,
      .legend_show = FALSE,
      .title = title,
      .x_lab = x_lab,
      .y_lab = y_lab,
      .interactive = interactive,
      .plotly_slider = plotly_slider
    )
  }
  
  if (add_caption) {
    if (interactive) {
      p <- p %>%
        plotly::layout(
          annotations = list(
            list(
              text = caption_text,
              x = 1,
              y = -0.16,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "right",
              font = list(size = 10, color = "gray50")
            )
          ),
          margin = list(b = 90)
        )
    } else {
      p <- p +
        labs(caption = caption_text) +
        theme(
          plot.caption = element_text(size = 9, colour = "grey40", hjust = 1)
        )
    }
  }
 
  if (return_plot) return(p) else print(p)
}

# =========================================================
# SEASONALITY FUNCTION
# =========================================================

plot_cpi_seasonal <- function(data,
                              plot_level,
                              category = NULL,
                              group_name = NULL,
                              series_select = NULL,
                              interactive = TRUE,
                              geom = "boxplot",
                              geom_color = "#2c3e50",
                              feature_set = c("month.lbl"),
                              return_plots = FALSE,
                              add_caption = TRUE,
                              caption_text = "Source: CEIC Database | Index: 2024 = 100") {
  
  data <- data %>% filter(!is.na(series))
  
  if (length(feature_set) == 0) {
    stop("Please select at least one seasonality feature.")
  }
  
  if (plot_level == 0) {
    filtered <- data %>% filter(level == 0)
    
  } else if (plot_level == 1) {
    filtered <- data %>% filter(level == 1)
    
  } else if (plot_level == 2) {
    filtered <- data %>% filter(level == 2)
    if (!is.null(category)) {
      filtered <- filtered %>% filter(division == category)
    }
    
  } else if (plot_level == 3) {
    filtered <- data %>% filter(level == 3)
    
    if (!is.null(category)) {
      filtered <- filtered %>% filter(division == category)
    }
    
    if (!is.null(group_name)) {
      filtered <- filtered %>% filter(group == group_name)
    }
    
  } else {
    stop("plot_level must be 0, 1, 2, or 3")
  }
  
  available_series <- filtered %>%
    distinct(series) %>%
    pull(series)
  
  if (!is.null(series_select) && length(series_select) > 0) {
    missing_series <- setdiff(series_select, available_series)
    
    if (length(missing_series) > 0) {
      stop(
        paste(
          "These series were not found at the specified level:",
          paste(missing_series, collapse = ", ")
        )
      )
    }
    series_list <- series_select
  } else {
    series_list <- available_series
  }
  
  if (length(series_list) == 0) {
    stop("No series found for the specified criteria")
  }
  
  plots <- list()
  
  for (s in series_list) {
    plot_data <- filtered %>% filter(series == s)
    
    p <- timetk::plot_seasonal_diagnostics(
      .data = plot_data,
      .date_var = date,
      .value = cpi,
      .feature_set = feature_set,
      .geom = geom,
      .geom_color = geom_color,
      .title = "",
      .x_lab = "Date",
      .y_lab = "CPI Index",
      .interactive = interactive
    )
    
    if (add_caption) {
      if (interactive) {
        p <- p %>%
          plotly::layout(
            xaxis = list(
              tickmode = "array",
              tickvals = 1:12,
              ticktext = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
              title = list(
                text = " ",
                font = list(size = 10)
              ),
              tickfont = list(size = 9)
            ),
            yaxis = list(
              title = list(
                text = " ",
                font = list(size = 10)
              ),
              tickfont = list(size = 9)
            ),
            annotations = list(
              list(
                text = caption_text,
                x = 1,
                y = -0.15,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                xanchor = "right",
                font = list(size = 10, color = "gray50")
              )
            ),
            margin = list(b = 80)
          )
      } else {
        p <- p + labs(caption = caption_text)
      }
    }
    
    plots[[s]] <- p
  }
  
  if (return_plots) return(plots)
  print(plots[[1]])
}

# =========================================================
# ACF FUNCTION
# =========================================================

plot_cpi_acf <- function(data,
                         plot_level,
                         category = NULL,
                         group_name = NULL,
                         series_select = NULL,
                         lags = 24,
                         interactive = TRUE,
                         line_color = "#2c3e50",
                         line_size = 0.5,
                         line_alpha = 1,
                         point_color = "#2c3e50",
                         point_size = 1,
                         point_alpha = 1,
                         x_intercept = NULL,
                         x_intercept_color = "#E31A1C",
                         hline_color = "#2c3e50",
                         white_noise_line_type = 2,
                         white_noise_line_color = "#A6CEE3",
                         show_ccf_vars_only = FALSE,
                         show_white_noise_bars = TRUE,
                         plotly_slider = FALSE,
                         ccf_target = NULL,
                         ccf_predictors = NULL,
                         add_caption = TRUE,
                         caption_text = "Source: CEIC Database | Index: 2024 = 100") {
  
  data <- data %>% filter(!is.na(series))
  
  if (plot_level == 0) {
    plot_data <- data %>% filter(level == 0)
    plot_title <- paste("CPI Autocorrelation Diagnostics (Lags =", lags, ")")
    
  } else if (plot_level == 1) {
    plot_data <- data %>% filter(level == 1)
    plot_title <- paste("CPI Major Groups Autocorrelation Diagnostics (Lags =", lags, ")")
    
  } else if (plot_level == 2) {
    plot_data <- data %>% filter(level == 2)
    if (!is.null(category)) {
      plot_data <- plot_data %>% filter(division == category)
      plot_title <- paste(category, "Autocorrelation Diagnostics (Lags =", lags, ")")
    } else {
      plot_title <- paste("CPI Subcategory Autocorrelation Diagnostics (Lags =", lags, ")")
    }
    
  } else if (plot_level == 3) {
    plot_data <- data %>% filter(level == 3)
    
    if (!is.null(category)) {
      plot_data <- plot_data %>% filter(division == category)
    }
    
    if (!is.null(group_name)) {
      plot_data <- plot_data %>% filter(group == group_name)
      plot_title <- paste(group_name, "Autocorrelation Diagnostics (Lags =", lags, ")")
    } else {
      plot_title <- paste("CPI Detailed Class Autocorrelation Diagnostics (Lags =", lags, ")")
    }
    
  } else {
    stop("plot_level must be 0, 1, 2, or 3")
  }
  
  available_series <- unique(plot_data$series)
  
  if (nrow(plot_data) == 0) {
    stop("No data found for the specified criteria")
  }
  
  if (!is.null(series_select) && length(series_select) > 0) {
    missing_series <- setdiff(series_select, available_series)
    if (length(missing_series) > 0) {
      stop(
        paste(
          "These series were not found at the specified level:",
          paste(missing_series, collapse = ", ")
        )
      )
    }
    plot_data <- plot_data %>% filter(series %in% series_select)
  }
  
  use_ccf <- !is.null(ccf_target) && !is.null(ccf_predictors)
  
  if (!use_ccf) {
    acf_data <- plot_data %>% group_by(series)
    
    p <- timetk::plot_acf_diagnostics(
      .data = acf_data,
      .date_var = date,
      .value = cpi,
      .lags = lags,
      .show_white_noise_bars = show_white_noise_bars,
      .line_color = line_color,
      .line_size = line_size,
      .line_alpha = line_alpha,
      .point_color = point_color,
      .point_size = point_size,
      .point_alpha = point_alpha,
      .x_intercept = x_intercept,
      .x_intercept_color = x_intercept_color,
      .hline_color = hline_color,
      .white_noise_line_type = white_noise_line_type,
      .white_noise_line_color = white_noise_line_color,
      .title = "",
      .x_lab = paste0("Lag (selected = ", lags, ")"),
      .y_lab = "Correlation",
      .interactive = interactive,
      .plotly_slider = plotly_slider
    )
    
  } else {
    series_needed <- c(ccf_target, ccf_predictors)
    
    missing_series <- setdiff(series_needed, unique(plot_data$series))
    if (length(missing_series) > 0) {
      stop(
        paste(
          "These series were not found in the filtered data:",
          paste(missing_series, collapse = ", ")
        )
      )
    }
    
    wide_data <- plot_data %>%
      filter(series %in% series_needed) %>%
      select(date, series, cpi) %>%
      distinct() %>%
      pivot_wider(names_from = series, values_from = cpi) %>%
      arrange(date) %>%
      drop_na(all_of(series_needed))
    
    if (nrow(wide_data) == 0) {
      stop("No complete cases available after reshaping to wide format.")
    }
    
    target_sym <- rlang::sym(ccf_target)
    
    p <- timetk::plot_acf_diagnostics(
      .data = wide_data,
      .date_var = date,
      .value = !!target_sym,
      .ccf_vars = all_of(ccf_predictors),
      .lags = lags,
      .show_ccf_vars_only = show_ccf_vars_only,
      .show_white_noise_bars = show_white_noise_bars,
      .line_color = line_color,
      .line_size = line_size,
      .line_alpha = line_alpha,
      .point_color = point_color,
      .point_size = point_size,
      .point_alpha = point_alpha,
      .x_intercept = x_intercept,
      .x_intercept_color = x_intercept_color,
      .hline_color = hline_color,
      .white_noise_line_type = white_noise_line_type,
      .white_noise_line_color = white_noise_line_color,
      .title = paste0(
        plot_title,
        " | Target: ", ccf_target,
        " | Predictors: ", paste(ccf_predictors, collapse = ", ")
      ),
      .x_lab = "Lag",
      .y_lab = "Autocorrelation",
      .interactive = interactive,
      .plotly_slider = plotly_slider
    )
  }
  
  if (add_caption) {
    if (interactive) {
      p <- p %>%
        plotly::layout(
          annotations = list(
            list(
              text = caption_text,
              x = 1,
              y = -0.15,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "right",
              font = list(size = 10, color = "gray50")
            )
          ),
          margin = list(b = 80)
        )
    } else {
      p <- p + labs(caption = caption_text)
    }
  }
  
  p
}

# =========================================================
# UI
# =========================================================
aa_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#d4af37",
  bg = "#f7f5f0",
  fg = "#1f2937",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)
aa_nav_css <- "
.navbar {
  background-color: #0f172a !important;
  min-height: 64px;
}

.navbar-brand {
  color: #ffffff !important;
  font-weight: 600;
  letter-spacing: 0.3px;
}

.navbar-nav .nav-link {
  color: #e5e7eb !important;
  font-weight: 500;
  padding-top: 1rem;
  padding-bottom: 1rem;
}

.navbar-nav .nav-link:hover {
  color: #d4af37 !important;
}

.navbar-nav .nav-link.active {
  color: #d4af37 !important;
  border-bottom: 2px solid #d4af37;
}

/* inner tabs: Trend / Seasonality / Autocorrelation */
.nav-tabs .nav-link {
  color: #1f2937 !important;
}

.nav-tabs .nav-link:hover {
  color: #d4af37 !important;
}

.nav-tabs .nav-link.active {
  color: #d4af37 !important;
  background-color: transparent !important;
  border-bottom: 3px solid #d4af37 !important;
  border-top: none !important;
  border-left: none !important;
  border-right: none !important;
}

/* selected range */
.irs-bar {
  background: #0f172a !important;
  border-top: 1px solid #0f172a !important;
  border-bottom: 1px solid #0f172a !important;
}

/* handle */
.irs-handle {
  background: #0f172a !important;
  border: 2px solid #ffffff !important;
}

/* full track */
.irs-line {
  background: #e5e7eb !important;
}

/* labels */
.irs-from, .irs-to, .irs-single {
  background: #0f172a !important;
  color: #ffffff !important;
}
"


ui <- navbarPage(
  title = "SG CPI",
  theme = aa_theme,
  
  header = tags$head(
    tags$style(HTML(aa_nav_css))
  ),

  tabPanel(
    "Dashboard",
    fluidPage(
      fluid = TRUE,
            tags$head(
              tags$style(HTML("
              .aa-fadein {
              animation: aaFadeIn 0.5s ease-in-out;
            }
          
            @keyframes aaFadeIn {
              from {
                opacity: 0;
                transform: translateY(6px);
              }
              to {
                opacity: 1;
                transform: translateY(0);
              }
            }
          
            .shiny-spinner-output-container {
              width: 100%;
            }
            
    
    .aa-hero-wrap {
      margin-bottom: 0;
      height: 100%;
    }
    
    .aa-hero-wrap .bslib-value-box,
    .aa-hero-wrap .value-box {
      min-height: 100px !important;
      height: 100% !important;
      border-radius: 14px !important;
      overflow: hidden !important;
    }
    
          .aa-hero-wrap .value-box-grid {
        display: grid !important;
        grid-template-columns: 1fr 200px;
        column-gap: 16px !important;
        align-items: center !important;
        padding: 14px 18px !important;
      }
    
    .aa-hero-wrap .value-box-showcase {
      min-width: 150px !important;
      min-height: 130px !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      overflow: hidden !important;
    }
    
    .aa-hero-wrap .value-box-body {
      min-width: 0 !important;
      display: flex !important;
      flex-direction: column !important;
      justify-content: center !important;
    }
    
    .aa-hero-wrap .value-box-title {
      font-size: 1.05rem !important;
      font-weight: 600 !important;
      line-height: 1.2 !important;
      margin-bottom: 8px !important;
    }
    
    .aa-hero-main {
      font-size: 2.8rem;
      font-weight: 700;
      line-height: 1;
      margin-bottom: 12px;
    }
    
    .aa-hero-line {
      font-size: 0.95rem;
      line-height: 1.3;
      margin-bottom: 4px;
      opacity: 0.98;
    }
    
    .aa-hero-spark {
      width: 185px;
      height: 120px;
    }
    
    .aa-right-container {
      height: 100%;
      display: flex;
      flex-direction: column;
    }
    
    .aa-kpi-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      grid-template-rows: 1fr 1fr;
      gap: 4px;
      height: 100%;
      flex: 1;
    }
    
    .aa-kpi-wrap {
      min-width: 0;
      min-height: 82px;
      height: 100%;
    }
    
    .aa-kpi-wrap .bslib-value-box,
    .aa-kpi-wrap .value-box {
      min-height: 82px !important;
      height: 100% !important;
      border-radius: 10px !important;
      margin-bottom: 0 !important;
      overflow: hidden !important;
    }
    
    .aa-kpi-wrap .value-box-grid {
      height: 100% !important;
      display: grid !important;
      grid-template-columns: 42px minmax(0, 1fr) !important;
      column-gap: 8px !important;
      align-items: center !important;
      padding: 8px 10px !important;
    }
    
    .aa-kpi-wrap .value-box-showcase {
      width: 42px !important;
      height: 30px !important;
      min-width: 42px !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      overflow: hidden !important;
    }
    
    .aa-kpi-wrap .value-box-body {
      min-width: 0 !important;
      display: flex !important;
      flex-direction: column !important;
      justify-content: center !important;
      gap: 2px !important;
    }
    
    .aa-kpi-wrap .value-box-title {
      margin: 0 !important;
      font-size: 0.8rem !important;
      line-height: 1.08 !important;
      font-weight: 600 !important;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    
    .aa-kpi-wrap .value-box-value {
      margin: 0 !important;
      line-height: 1.02 !important;
      font-size: 1.02rem !important;
      font-weight: 700 !important;
    }
    
    .aa-kpi-wrap .value-box-value > div,
    .aa-kpi-wrap .value-box-value > span,
    .aa-kpi-wrap .value-box-value p {
      margin: 0 !important;
      font-size: inherit !important;
      line-height: inherit !important;
      font-weight: inherit !important;
    }
    
    .aa-kpi-subtitle {
      margin: 1px 0 0 0 !important;
      font-size: 0.75rem !important;
      line-height: 1.08 !important;
      font-weight: 500 !important;
      opacity: 0.95;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    
    .aa-kpi-text {
      display: inline-block;
      max-width: 100%;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      vertical-align: bottom;
    }
    
    .aa-box {
      width: 100%;
      overflow-x: hidden;
      padding: 0;
      margin-bottom: 0;
    }
    
    .bslib-layout-columns {
      align-items: stretch !important;
    }
    
    .bslib-layout-columns > div:first-child,
    .bslib-layout-columns > div:last-child {
      display: flex;
      flex-direction: column;
    }
    
    .bslib-layout-columns > div:first-child .aa-hero-wrap,
    .bslib-layout-columns > div:last-child .aa-right-container {
      flex: 1;
      display: flex;
      flex-direction: column;
    }
    
    .bslib-layout-columns > div:first-child .aa-hero-wrap .bslib-value-box,
    .bslib-layout-columns > div:first-child .aa-hero-wrap .value-box {
      flex: 1;
    }
      "))
      ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              
              div(
                class = "aa-hero-wrap aa-fadein",
                withSpinner(
                  uiOutput("aa_hero_cpi"),
                  type = 4,
                  color = "#1a1612"
                )
              ),
              
              div(
                class = "aa-right-container",
                div(
                  class = "aa-kpi-grid",
                  div(class = "aa-kpi-wrap", uiOutput("aa_kpi_mom")),
                  div(class = "aa-kpi-wrap", uiOutput("aa_kpi_yoy")),
                  div(class = "aa-kpi-wrap", uiOutput("aa_kpi_top")),
                  div(class = "aa-kpi-wrap", uiOutput("aa_kpi_drag"))
                )
     

          )
        ),
      
      div(
        class = "aa-box",
        
        div(
          style = "display:flex; align-items:flex-end; justify-content:space-between; margin-bottom:8px;",
          
          div(
            style = "display:flex; flex-direction:column;",
            
            # title + tooltip inline
            div(
              style = "display:flex; align-items:center; gap:6px; font-size:18px; font-weight:700; color:#201a15;",
              
              "Major Category Contribution Dashboard",
              
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Contribution (pp) shows how much each category adds to or drags down overall CPI inflation, measured in percentage points."
              )
            ),
            
            # subtitle
          
            div(
              "Click a division to view subcategories.",
              style = "font-size:12px; color#faf7f2;"
            )
          )
        ),
        
        tagList(
          reactable::reactableOutput("aa_dashboard_table"),
          
          div(
            style = "
              font-size:12px;
              color#faf7f2;
              margin-top:6px;
              text-align:right;
            ",
            "Source: Singapore Department of Statistics | CEIC Database| Base Year: 2024"
          )
        )
      )
    )
  ),
  
  
  tabPanel(
    "EDA",
    fluidPage(
      h4("CPI EDA", style = "margin-top:10px;"),
      tags$head(
        tags$style(HTML("
    .container-fluid {
      padding-top: 0px !important;
    }
    .row {
      margin-top: 2px !important;
      margin-bottom: 2px !important;
    }
  "))
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          tags$details(
            open = "open",
            tags$summary(
              style = "
        font-size: 18px;
        font-weight: 580;
        cursor: pointer;
        margin-bottom: 5px;
      ",
              "Control Panel"
            ),
            
            tags$div(
              style = "
        background:#faf7f2;
        padding:2px;
        border-radius:2px;
        margin-top:5px;
      ",
              
              radioButtons(
                "aa_plot_level",
                "Hierarchy Level",
                choices = c(
                  "All Items" = 0,
                  "Major Category" = 1,
                  "Group" = 2,
                  "Class" = 3
                ),
                selected = 1
              ),
              
              uiOutput("aa_division_ui"),
              uiOutput("aa_group_ui"),
              uiOutput("aa_series_select_ui"),
              
              sliderTextInput(
                "aa_date_range",
                "Date Range",
                choices = sort(unique(cc_cpi_data$date)),
                selected = range(cc_cpi_data$date),
                grid = FALSE,
                dragRange = TRUE
              ),
              
              hr(),
              
              conditionalPanel(
                condition = "input.aa_eda_tabs == 'Trend'",
                checkboxInput("aa_smooth", "Smoothed Trend", TRUE),
                selectInput(
                  "aa_colour_by_year",
                  "Colour by Year",
                  choices = c("None" = "none", "Year" = "year"),
                  selected = "none"
                ),
                selectInput(
                  "aa_facet_cols",
                  "Facet Columns",
                  choices = c("1" = 1, "2" = 2, "3" = 3),
                  selected = 2
                )
              ),
              
              conditionalPanel(
                condition = "input.aa_eda_tabs == 'Seasonality'",
                selectInput(
                  "aa_season_geom",
                  "Seasonality Plot",
                  choices = c("Boxplot" = "boxplot", "Violin" = "violin"),
                  selected = "boxplot"
                ),
                checkboxGroupInput(
                  "aa_season_feature",
                  "Seasonality Features",
                  choices = c(
                    "Month" = "month.lbl",
                    "Quarter" = "quarter",
                    "Year" = "year"
                  ),
                  selected = c("month.lbl", "quarter")
                )
              ),
              
              conditionalPanel(
                condition = "input.aa_eda_tabs == 'Autocorrelation'",
                checkboxInput("aa_acf_white_noise", "Show White Noise Bars", TRUE),
                numericInput("aa_lags", "Lags", value = 24, min = 1, max = 60)
              ),
              
              actionButton("aa_reset", "Reset", class = "btn-primary")
            )
          )
        ),
        
        mainPanel(
          width = 9,
          
          tabsetPanel(
            id = "aa_eda_tabs",
            
            tabPanel(
              "Trend",
              div(
                style = "font-size:13px; color#faf7f2; margin-top: 20px;",
                "Visualising CPI trends over time to identify long term inflation patterns."
              ),
              br(),
              plotlyOutput("aa_trend_plot", height = "550px")
            ),
            
            tabPanel(
              "Seasonality",
              div(
                style = "font-size:13px; color#faf7f2; margin-top: 20px;",
                "Identifying recurring seasonal patterns in CPI across months, quarters and years."
              ),
              br(),
              uiOutput("aa_seasonality_cards_ui")
            ),
            
            tabPanel(
              "Autocorrelation",
              div(
                style = "font-size:13px; color#faf7f2; margin-top: 20px;",
                "Showing how CPI values are correlated with past periods to identify persistence and lag effects."
              ),
              br(),
              plotlyOutput("aa_acf_plot", height = "550px")
            )
          )
      )))
    ),
  
  
  tabPanel(
    "Forecast",
    fluidPage(
      
      h4("Forecasting CPI", style = "margin-top:10px;"),
      
      p(
        "Hierarchical CPI forecasts using bottom-up aggregation (training: 2020–2024, validation: 2025, forecast: 2026).",
        style = "font-size:13px; color#faf7f2; margin-bottom:2px;"
      ),
      
      p(
        "The chart shows actual CPI in black, projected values in purple, and confidence intervals as shaded areas.",
        style = "font-size:13px; color#faf7f2; margin-bottom:20px;"
      ),
      tags$head(
        tags$style(HTML("
        .cc-forecast-note {
          font-size: 13px;
          color: #6b7280;
          margin-top: 20px;
          margin-bottom: 12px;
        }
      "))
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          tags$details(
            open = "open",
            tags$summary(
              style = "
              font-size: 18px;
              font-weight: 580;
              cursor: pointer;
              margin-bottom: 12px;
            ",
              "Control Panel"
            ),
            
            tags$div(
              style = "
              background:#faf7f2;
              padding:15px;
              border-radius:6px;
              margin-top:10px;
            ",
              
              selectInput(
                "cc_forecast_mode",
                "Forecast Mode",
                choices = c(
                  "Auto (Best Model by RMSE)" = "auto",
                  "Manual (Select Model)" = "manual"
                ),
                selected = "auto"
              ),
              
              conditionalPanel(
                condition = "input.cc_forecast_mode == 'manual'",
                selectInput(
                  "cc_model_select",
                  "Model",
                  choices = c(
                    "ETS" = "ets",
                    "Auto-ARIMA" = "arima",
                    "ARIMA + XGBoost" = "boost",
                    "Prophet" = "prophet"
                  ),
                  selected = "ets"
                )
              ),
              
              selectInput(
                "cc_level_select",
                "Hierarchy Level",
                choices = NULL
              ),
              
              selectInput(
                "cc_category_select",
                "Category",
                choices = NULL
              ),
              radioButtons(
                "cc_date_window",
                "Date Window",
                choices = c(
                  "6 Months" = "6",
                  "12 Months" = "12",
                  "24 Months" = "24",
                  "All" = "all"
                ),
                selected = "24",
                inline = FALSE
              )
            )
          )
        ),
        
        mainPanel(
          width = 8,
          
          wellPanel(
            uiOutput("cc_chart_header"),
            plotlyOutput("cc_cpi_plot", height = "500px")
        )
      )
  ))
  ),
  tabPanel(
    "Decomposition"),  ## add decomposition here
  
  tabPanel(
    "Data Explorer",
    fluidPage(
      tags$style(HTML("
  
  .dataTables_wrapper .dataTables_filter input,
  .dataTables_wrapper select {
    height: 26px !important;
    font-size: 12px !important;
  }

  table.dataTable thead input {
    height: 24px !important;
    font-size: 12px !important;
    padding: 2px 4px !important;
  }
  
  table.dataTable {
    border-spacing: 0px !important;
  }

  table.dataTable td,
  table.dataTable th {
    white-space: nowrap;
  }


")),
      
      div(
        style = "
        width: 100%;
        max-width: 1200px;
        margin: 10px auto;
        overflow-x: hidden;
        font-family: 'Segoe UI', Arial, sans-serif;
      ",
        
        div(
          style = "text-align: center; margin-bottom: 8px;",
          h4(
            "CPI Data Explorer",
            style = "color: #2c3e50; margin-bottom: 5px;"
          )
          
        ),
        
        div(
          style = "
              display:flex;
              justify-content:space-between;
              align-items:center;
              margin-bottom:6px;
              padding:6px 10px;
              background-color:#f8f9fa;
              border-radius:5px;
            ",
          div(
            style = "text-align:center; flex:1; line-height:1.1;",
            div(style = "font-size:11px; color:#7f8c8d; margin-bottom:2px;", "Total Rows"),
            div(style = "font-size:14px; font-weight:600;", textOutput("aa_total_rows", inline = TRUE))
          ),
          div(
            style = "text-align: center; flex: 1; border-left: 1px solid #dee2e6; border-right: 1px solid #dee2e6;",
            div(style = "font-size: 12px; color: #7f8c8d;", "Divisions"),
            textOutput("aa_total_divisions", inline = TRUE)
          ),
          div(
            style = "text-align: center; flex: 1;",
            div(style = "font-size: 12px; color: #7f8c8d;", "Last Updated"),
            textOutput("aa_last_updated", inline = TRUE)
          )
        ),
        
        DTOutput("aa_cpi_table"),
        
        div(
          style = "margin-top: 15px; font-size: 12px; color: #7f8c8d; text-align: center;",
          "▲ = Increase from previous month | ▼ = Decrease from previous month | ◆ = No change",
          br(),
          "Base Year: 2024 | Source: CEIC Database"
        )
      )
    )
  ))
  

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  
  # =========================
  # DASHBOARD
  # =========================

  # =========================
  # REACTABLE DATA PREP
  # =========================
  
  aa_dashboard_table_data <- reactive({
    aa_dashboard_table_rds
  })
  
  aa_latest_top_cpi <- reactive({
    aa_cpi_data %>%
      mutate(date = as.Date(date)) %>%
      filter(series == "Consumer Price Index (CPI)") %>%
      arrange(date)
  })
  
  aa_latest_lvl1_dash <- reactive({
    aa_latest_lvl1
  })
  
  # reusable KPI card
    
  aa_short_series <- function(x) {
    dplyr::case_when(
      x == "Information & Communication (InfoComm)" ~ "InfoComm",
      x == "Clothing and Footwear (C&F)" ~ "C&F",
      x == "Housing & Utilities" ~ "Housing",
      x == "Household Durables & Services (HDS)" ~ "HDS",
      x == "Recreation, Sport & Culture (RSC)" ~ "RSC",
      x == "Miscellaneous Goods & Services (MG&S)" ~ "MG&S",
      x == "Food" ~ "Food",
      x == "Health" ~ "Health",
      x == "Transport" ~ "Transport",
      x == "Education" ~ "Education",
      TRUE ~ x
    )
  }
  
  aa_series_icon <- function(x, direction = c("up", "down")) {
    direction <- match.arg(direction)
  
    if (x == "Food") {
      return(bsicons::bs_icon("basket", size = "22px"))
    }
  
    if (x == "Clothing and Footwear (C&F)") {
      return(bsicons::bs_icon("handbag", size = "22px"))
    }
  
    if (x %in% c("Housing & Utilities", "Housing")) {
      return(bsicons::bs_icon("house-door", size = "22px"))
    }
  
    if (x %in% c("Household Durables & Services (HDS)", "HDS")) {
      return(bsicons::bs_icon("lamp", size = "22px"))
    }
  
    if (x == "Health") {
      return(bsicons::bs_icon("heart-pulse", size = "22px"))
    }
  
    if (x == "Transport") {
      return(bsicons::bs_icon("car-front-fill", size = "22px"))
    }
  
    if (x %in% c("Information & Communication (InfoComm)", "InfoComm")) {
      return(bsicons::bs_icon("wifi", size = "22px"))
    }
  
    if (x %in% c("Recreation, Sport & Culture (RSC)", "RSC")) {
      return(bsicons::bs_icon("controller", size = "22px"))
    }
  
    if (x == "Education") {
      return(bsicons::bs_icon("book", size = "22px"))
    }
  
    if (x %in% c("Miscellaneous Goods & Services (MG&S)", "MG&S")) {
      return(bsicons::bs_icon("grid-3x3-gap", size = "22px"))
    }
  
    if (direction == "up") {
      bsicons::bs_icon("arrow-up-circle", size = "22px")
    } else {
      bsicons::bs_icon("arrow-down-circle", size = "22px")
    }
  }
  
  aa_kpi_value_box <- function(title, value_ui, subtitle = NULL, showcase = NULL, fg = "#1f2937") {
    value_box(
      title = title,
      value = value_ui,
      showcase = showcase,
      theme = bslib::value_box_theme(
        fg = fg
      ),
      if (!is.null(subtitle)) {
        div(class = "aa-kpi-subtitle", subtitle)
      },
      full_screen = FALSE
    )
  }
  
  aa_hero_sparkline <- function(data) {
    aa_df <- data %>%
      dplyr::filter(is.finite(cpi)) %>%
      dplyr::arrange(date)
    
    if (nrow(aa_df) < 2) return(NULL)
    
    plotly::plot_ly(aa_df) %>%
      plotly::add_lines(
        x = ~date,
        y = ~cpi,
        color = I("#f5f0e8"),
        fill = "tozeroy",
        alpha = 0.2,
        hovertemplate = "CPI: %{y:.2f}<extra></extra>"
        ) %>%
      plotly::layout(
        xaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
        yaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
        hovermode = "x unified",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "#f5f0e8"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      plotly::config(displayModeBar = FALSE) %>%
      htmlwidgets::onRender(
        "function(el) {
        el.closest('.bslib-value-box')
          .addEventListener('bslib.card', function(ev) {

            Plotly.relayout(el, {
              'xaxis.visible': ev.detail.fullScreen,
              'yaxis.visible': ev.detail.fullScreen
            });

            setTimeout(function() {
              Plotly.Plots.resize(el);
            }, 100);

          });
      }"
      )
  }
  
  aa_hero_narrative <- function(data) {
    aa_df <- data %>%
      dplyr::filter(is.finite(cpi)) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        cpi_prev_12m = dplyr::lag(cpi, 12),
        yoy_pct = (cpi / cpi_prev_12m - 1) * 100
      )
    
    aa_latest_row <- aa_df %>% dplyr::slice_tail(n = 1)
    aa_first_row  <- aa_df %>% dplyr::slice_head(n = 1)
    aa_peak_row   <- aa_df %>% dplyr::slice_max(cpi, n = 1, with_ties = FALSE)
    
    aa_latest_val <- aa_latest_row$cpi[1]
    aa_latest_yoy <- aa_latest_row$yoy_pct[1]
    aa_start_val  <- aa_first_row$cpi[1]
    aa_peak_val   <- aa_peak_row$cpi[1]
    aa_peak_date  <- format(aa_peak_row$date[1], "%b %Y")
    
    aa_change_5y <- aa_latest_val - aa_start_val
    aa_direction <- dplyr::case_when(
      aa_change_5y > 0 ~ "Upward trend over the past 5 years",
      aa_change_5y < 0 ~ "Downward trend over the past 5 years",
      TRUE ~ "Broadly stable over the past 5 years"
    )
    
    aa_direction_detail <- dplyr::case_when(
      aa_change_5y > 0 ~ sprintf("Up %.1f points since %s", aa_change_5y, format(aa_first_row$date[1], "%b %Y")),
      aa_change_5y < 0 ~ sprintf("Down %.1f points since %s", abs(aa_change_5y), format(aa_first_row$date[1], "%b %Y")),
      TRUE ~ sprintf("Little net change since %s", format(aa_first_row$date[1], "%b %Y"))
    )
    
    tagList(
      div(class = "aa-hero-main", sprintf("%.1f", aa_latest_val)),
      div(class = "aa-hero-line", aa_direction),
      div(
        class = "aa-hero-line",
        sprintf(
          "Earliest: %s",
          ifelse(is.na(aa_start_val), "NA", sprintf("%.1f", aa_start_val))
        )
      ),
      div(class = "aa-hero-line", sprintf("5Y peak: %.1f in %s", aa_peak_val, aa_peak_date)),
      div(class = "aa-hero-line", aa_direction_detail)
    )
  }
  
  
  
  # =========================
  # KPI OUTPUTS
  # =========================
  output$aa_hero_cpi <- renderUI({
    aa_df <- aa_latest_top_cpi()
    validate(need(nrow(aa_df) > 13, "Not enough CPI data available"))
    
    div(
      class = "aa-hero-wrap",
      
      bslib::value_box(
        title = "Headline CPI Index",
        
        value = aa_hero_narrative(aa_df),
        
        showcase = aa_hero_sparkline(aa_df),
        
        full_screen = TRUE,
        
        theme = bslib::value_box_theme(
          bg =  "#201a15",
          fg = "#f5f0e8"
        )
      )
    )
  })
  
  output$aa_kpi_mom <- renderUI({
    aa_df <- aa_latest_top_cpi()
    validate(need(nrow(aa_df) >= 2, "Not enough history for MoM"))
    
    aa_latest_row <- aa_df %>% dplyr::slice_tail(n = 1)
    aa_latest_date <- aa_latest_row$date[1]
    aa_prev_date <- aa_latest_date %m-% months(1)
    
    aa_prev_row <- aa_df %>% dplyr::filter(date == aa_prev_date)
    validate(need(nrow(aa_prev_row) == 1, "Matching prior-month not found"))
    
    aa_mom <- (aa_latest_row$cpi[1] / aa_prev_row$cpi[1] - 1) * 100
    
    aa_kpi_value_box(
      title = "CPI MoM",
      value_ui = div(
        style = "font-size:28px; font-weight:700; line-height:1.1;",
        sprintf("%+.2f%%", aa_mom)
      ),
      subtitle = "vs Nov 2025",
      showcase = bsicons::bs_icon("arrow-left-right", size = "24px"),
      fg = if (aa_mom >= 0) "#c62828" else "#1565c0"
    )
  })
  
  output$aa_kpi_yoy <- renderUI({
    aa_df <- aa_latest_top_cpi()
    validate(need(nrow(aa_df) >= 13, "Not enough history for YoY"))
    
    aa_latest_row <- aa_df %>% dplyr::slice_tail(n = 1)
    aa_latest_date <- aa_latest_row$date[1]
    aa_prev_date <- aa_latest_date %m-% months(12)
    
    aa_prev_row <- aa_df %>% dplyr::filter(date == aa_prev_date)
    validate(need(nrow(aa_prev_row) == 1, "Matching prior-year month not found"))
    
    aa_yoy <- (aa_latest_row$cpi[1] / aa_prev_row$cpi[1] - 1) * 100
    
    aa_kpi_value_box(
      title = "CPI YoY",
      value_ui = div(
        style = "font-size:28px; font-weight:700; line-height:1.1;",
        sprintf("%+.2f%%", aa_yoy)
      ),
      subtitle = "vs Dec 2024",
      showcase = bsicons::bs_icon("activity", size = "24px"),
      fg = if (aa_yoy >= 0) "#c62828" else "#1565c0"
    )
  })
  
  output$aa_kpi_top <- renderUI({
    aa_df <- aa_latest_lvl1_dash()
    validate(need(nrow(aa_df) > 0, "No data"))
    
    aa_top_row <- aa_df %>% slice(1)
    aa_name <- aa_short_series(aa_top_row$series)
    
    aa_kpi_value_box(
      title = "Top Inflation Driver",
      
      # MAIN VALUE category name
      value_ui = div(
        style = "font-size:20px; font-weight:700; line-height:1.2;",
        title = aa_top_row$series,
        aa_name
      ),
      
      # SUBTITLE  contribution
      subtitle = sprintf("%+.2f pp", aa_top_row$contribution_pp),
      
      showcase = aa_series_icon(aa_top_row$series, "up"),
      fg = "#c62828"
    )
  })
  
  output$aa_kpi_drag <- renderUI({
    aa_df <- aa_latest_lvl1_dash()
    validate(need(nrow(aa_df) > 0, "No data"))
    
    aa_drag_row <- aa_df %>%
      arrange(contribution_pp) %>%
      slice(1)
    
    aa_name <- aa_short_series(aa_drag_row$series)
    
    aa_kpi_value_box(
      title = "Top Deflation Driver",
      
      # MAIN VALUE → category
      value_ui = div(
        style = "font-size:20px; font-weight:700; line-height:1.2;",
        title = aa_drag_row$series,
        aa_name
      ),
      
      # SUBTITLE → contribution
      subtitle = sprintf("%+.2f pp", aa_drag_row$contribution_pp),
      
      showcase = aa_series_icon(aa_drag_row$series, "down"),
      fg = "#1e5eff"
    )
  })

  
  # =========================
  # REACTABLE OUTPUT
  # =========================
  
  
  output$aa_dashboard_table <- renderReactable({
    aa_latest <- aa_dashboard_table_data()
    req(nrow(aa_latest) > 0)
    
     
      reactable(
        aa_latest,
        defaultPageSize = 10,
        defaultSorted = "contribution_pp",
        defaultSortOrder = "desc",
        compact = TRUE,
        bordered = FALSE,
        striped = FALSE,
        highlight = TRUE,
        pagination = FALSE,
        searchable = FALSE,
        width = "100%",
        resizable = TRUE,
        fullWidth = TRUE,
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(fontWeight = "600"),
          style = list(fontSize = "14px")
        ),
        columns = list(
          series = colDef(
            name = "Category",
            align = "left",
            width =280,
            cell = function(value, index) {
              htmltools::tags$a(
                href = "#",
                onclick = sprintf(
                  "Shiny.setInputValue('aa_clicked_division', '%s', {priority: 'event'}); return false;",
                  value
                ),
                style = "color:#1f3b5b; font-weight:600; text-decoration:none;",
                value
              )
            }
          ),
          cpi = colDef(
            name = "Latest CPI",
            format = colFormat(digits = 2),
            minWidth = 70
          ),
          trend_12m = colDef(
            name = "12M Trend", 
            minWidth = 150, 
            sortable = FALSE, 
            cell = react_sparkline(
              aa_latest, height = 30, 
              show_line = TRUE, 
              line_color = "black", 
              bandline = "innerquartiles", 
              bandline_color = "grey", 
              decimals = 2, statline = "mean", 
              statline_color = "grey", 
              statline_label_size = "0.65em", 
              highlight_points = 
                highlight_points(
                  min = "black", max = "black"
                  ))
            ),
          yoy_pct = colDef(
            name = "YoY %",
            minWidth = 65,
            cell = function(value) {
              aa_color <- if (is.na(value)) {
                "#666"
              } else if (value > 0) {
                "#b22222"
              } else if (value < 0) {
                "#1f77b4"
              } else {
                "#444"
              }
              
              div(
                style = paste0("color:", aa_color, "; font-weight: 600;"),
                ifelse(is.na(value), "NA", sprintf("%.2f", value))
              )
            }
          ),
          weight_pct = colDef(
            name = "Weight (%)",
            format = colFormat(digits = 2),
            minWidth = 70
          ),
          contribution_pp = colDef(
            name = "Contribution (pp)",
            minWidth = 120,
            align = "right",
            cell = function(value) {
              aa_bar_width <- if (is.na(value)) 0 else min(abs(value) * 120, 120)
              
              aa_bar_color <- if (is.na(value)) {
                "#bdbdbd"
              } else if (value > 0) {
                "#d73027"
              } else if (value < 0) {
                "#4575b4"
              } else {
                "#bdbdbd"
              }
              
              div(
                style = "display:flex; align-items:center; justify-content:flex-end; gap:8px;",
                div(
                  style = paste0(
                    "height:10px; width:", aa_bar_width, "px; ",
                    "background:", aa_bar_color, "; ",
                    "border-radius:6px; opacity:0.85;"
                  )
                ),
                div(
                  style = "min-width:48px; text-align:right;",
                  ifelse(is.na(value), "NA", sprintf("%.2f", value))
                )
              )
            }
          )
        ),
        theme = reactableTheme(
          backgroundColor = "#f7f3ef",   # light beige table
          borderColor = "#e6ddd5",
          stripedColor = "#f2ece6",
          highlightColor = "#efe7df",
          cellPadding = "8px 10px",
          
          style = list(
            color = "#2c2c2c",
            backgroundColor = "#f7f3ef"
          ),
          
          headerStyle = list(
            backgroundColor = "#1e1813",  # dark header only
            color = "#f3eee8",            # light text
            borderColor = "#1e1813",
            fontWeight = "580",
            fontSize = "13px"
          )
        ))
  })
  
  observeEvent(input$aa_clicked_division, {
    req(input$aa_clicked_division)
    
    aa_division <- input$aa_clicked_division
    
    aa_subcat_df <- aa_cpi_dashboard %>%
      dplyr::filter(
        division == aa_division,
        if (aa_division == "Food") level %in% c(2, 3) else level == 2
      ) %>%
      dplyr::arrange(level, series, date)
    
    if (nrow(aa_subcat_df) == 0) {
      showModal(
        modalDialog(
          title = div(
            style = "font-size:14px; font-weight:700;",
            paste("Sub-categories under", aa_division)
          ),
          easyClose = TRUE,
          footer = modalButton("Close"),
          "No sub-category data found for this division."
        )
      )
      return()
    }
    
    aa_latest_date <- max(aa_subcat_df$date, na.rm = TRUE)
    
    showModal(
      modalDialog(
        title = if (aa_division == "Food") {
          div(style = "font-size:16px; font-weight:700;",
          "Food sub-categories and detailed items")
        } else {
          div(style = "font-size:16px; font-weight:700;",
          paste("Sub-categories under", aa_division))
        },
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        reactable::reactableOutput("aa_subcategory_table")
      )
    )
    
    output$aa_subcategory_table <- reactable::renderReactable({
      
      aa_yoy_cell <- function(value) {
        aa_color <- if (is.na(value)) {
          "#666"
        } else if (value > 0) {
          "#c62828"
        } else if (value < 0) {
          "#1565c0"
        } else {
          "#444"
        }
        
        htmltools::div(
          style = paste0(
            "color:", aa_color,
            "; font-weight:600; font-size:14px;"
          ),
          ifelse(is.na(value), "NA", sprintf("%.2f", value))
        )
      }
      
      aa_contrib_cell <- function(value) {
        aa_color <- if (is.na(value)) {
          "#666"
        } else if (value > 0) {
          "#c62828"
        } else if (value < 0) {
          "#1565c0"
        } else {
          "#444"
        }
        
        htmltools::div(
          style = paste0(
            "color:", aa_color,
            "; font-weight:600; font-size:14px;"
          ),
          ifelse(is.na(value), "NA", sprintf("%.2f", value))
        )
      }
      
      aa_share_cell <- function(value) {
        aa_value <- ifelse(is.na(value), 0, value)
        aa_bar_width <- aa_value / 100 * 120
        
        htmltools::div(
          style = "display:flex; align-items:center; gap:10px; justify-content:space-between; font-size:14px;",
          htmltools::div(
            style = paste0(
              "height:10px; width:", aa_bar_width, "px; ",
              "background:#6baed6; border-radius:5px;"
            )
          ),
          ifelse(is.na(value), "NA", sprintf("%.2f", value))
        )
      }
      
      
      aa_theme <- reactable::reactableTheme(
        backgroundColor = "#f7f3ef",
        borderColor = "#e6ddd5",
        stripedColor = "#f2ece6",
        highlightColor = "#efe7df",
        cellPadding = "8px 10px",
        style = list(
          color = "#2c2c2c",
          backgroundColor = "#f7f3ef",
          fontSize = "14px"
        ),
        headerStyle = list(
          backgroundColor = "#1e1813",
          color = "#f3eee8",
          borderColor = "#1e1813",
          fontWeight = "600",
          fontSize = "13px"
        )
      )
      
      if (aa_division == "Food") {
        
        aa_lvl2_df <- aa_subcat_df %>%
          dplyr::filter(date == aa_latest_date, level == 2) %>%
          dplyr::transmute(
            Subcategory = series,
            CPI = round(cpi, 2),
            `YoY %` = round(yoy_pct, 2),
            `Division share (%)` = round(division_share_pct, 2),
            `Contribution (pp)` = round(contribution_pp, 2)
          ) %>%
          dplyr::arrange(dplyr::desc(`Contribution (pp)`))
        
        aa_lvl3_df <- aa_subcat_df %>%
          dplyr::filter(date == aa_latest_date, level == 3) %>%
          dplyr::transmute(
            Parent = parent_series,
            Detail = series,
            CPI = round(cpi, 2),
            `YoY %` = round(yoy_pct, 2),
            `Subgroup share (%)` = round(subgroup_share_pct, 2),
            `Contribution (pp)` = round(contribution_pp, 2)
          )
        
        reactable::reactable(
          aa_lvl2_df,
          compact = TRUE,
          bordered = FALSE,
          striped = TRUE,
          highlight = TRUE,
          defaultPageSize = 10,
          theme = aa_theme,
          defaultColDef = reactable::colDef(
            align = "center",
            style = list(fontSize = "14px", background = "#f7f3ef")
          ),
          details = function(index) {
            aa_parent <- aa_lvl2_df$Subcategory[index]
            
            aa_child_df <- aa_lvl3_df %>%
              dplyr::filter(Parent == aa_parent) %>%
              dplyr::select(
                Detail,
                CPI,
                `YoY %`,
                `Subgroup share (%)`,
                `Contribution (pp)`
              ) %>%
              dplyr::arrange(dplyr::desc(`Contribution (pp)`))
            
            reactable::reactable(
              aa_child_df,
              compact = TRUE,
              bordered = FALSE,
              striped = FALSE,
              highlight = TRUE,
              pagination = FALSE,
              theme = aa_theme,
              defaultColDef = reactable::colDef(
                align = "center",
                style = list(fontSize = "14px", background = "#f7f3ef")
              ),
              columns = list(
                Detail = reactable::colDef(minWidth = 200,align = "left"),
                CPI = reactable::colDef(format = reactable::colFormat(digits = 2)),
                `YoY %` = reactable::colDef(cell = aa_yoy_cell),
                `Subgroup share (%)` = reactable::colDef(minWidth = 150,cell = aa_share_cell),
                `Contribution (pp)` = reactable::colDef(minWidth = 150,cell = aa_contrib_cell)
              )
            )
          },
          columns = list(
            Subcategory = reactable::colDef(minWidth = 200, align = "left"),
            CPI = reactable::colDef(format = reactable::colFormat(digits = 2)),
            `YoY %` = reactable::colDef(cell = aa_yoy_cell),
            `Division share (%)` = reactable::colDef(minWidth = 150,cell = aa_share_cell),
            `Contribution (pp)` = reactable::colDef(minWidth = 150,cell = aa_contrib_cell)
          )
        )
        
      } else {
        
        aa_modal_df <- aa_subcat_df %>%
          dplyr::filter(date == aa_latest_date, level == 2) %>%
          dplyr::transmute(
            Subcategory = series,
            CPI = round(cpi, 2),
            `YoY %` = round(yoy_pct, 2),
            `Division share (%)` = round(division_share_pct, 2),
            `Contribution (pp)` = round(contribution_pp, 2)
          ) %>%
          dplyr::arrange(dplyr::desc(`Contribution (pp)`))
        
        reactable::reactable(
          aa_modal_df,
          compact = TRUE,
          bordered = FALSE,
          striped = TRUE,
          highlight = TRUE,
          defaultPageSize = 10,
          theme = aa_theme,
          defaultColDef = reactable::colDef(
            align = "center",
            style = list(fontSize = "14px", background = "#f7f3ef")
          ),
          columns = list(
            Subcategory = reactable::colDef(minWidth = 200, align = "left"),
            CPI = reactable::colDef(format = reactable::colFormat(digits = 2)),
            `YoY %` = reactable::colDef(cell = aa_yoy_cell),
            `Division share (%)` = reactable::colDef(minWidth = 150,cell = aa_share_cell),
            `Contribution (pp)` = reactable::colDef(minWidth = 150,cell = aa_contrib_cell)
          )
        )
      }
    })
  })
  
  # =========================
  # DATA EXPLORER (CPI TABLE)
  # =========================
  
  output$aa_cpi_table_subtitle <- renderUI({
    h6(
      paste("Latest Data:", format(max(aa_cpi_h_display$As_of, na.rm = TRUE), "%B %Y")),
      style = "color: #7f8c8d; font-weight: normal; margin-top: 0;"
    )
  })
  
  output$aa_total_rows <- renderText({
    as.character(nrow(aa_cpi_h_display))
  })
  
  output$aa_total_divisions <- renderText({
    as.character(
      aa_cpi_h_display %>%
        filter(level_label == "Major Category") %>%
        summarise(n = n_distinct(division, na.rm = TRUE)) %>%
        pull(n)
    )
  })
  
  output$aa_last_updated <- renderText({
    format(max(aa_cpi_h_display$As_of, na.rm = TRUE), "%b %Y")
  })
  
  output$aa_cpi_table <- renderDT({
    dt_data <- aa_cpi_h_display %>%
      select(
        Level = level_label,
        Division = major_group,
        Group = group,
        Class = class_name,
        Series = display_name,
        `CPI Value` = cpi_rounded,
        `As of label` = As_of_lab,
        Trend,
        Trend_export
      )
    
    datatable(
      dt_data,
      class = "cell-border stripe hover",
      filter = "top",
      extensions = c("Buttons", "ColReorder", "Responsive"),
      width = "100%",
      options = list(
        pageLength = 10,
        lengthMenu = list(
          c(10, 25, 50, 100, -1),
          c("10", "25", "50", "100", "All")
        ),
        dom = "B<'toolbar'>lfrtip",
        buttons = list(
          list(
            extend = "collection",
            text = "Export",
            buttons = list(
              list(
                extend = "copy",
                text = "Copy",
                exportOptions = list(
                  columns = c(0:6, 8),
                  modifier = list(page = "current")
                )
              ),
              list(
                extend = "csv",
                text = "CSV",
                exportOptions = list(
                  columns = c(0:6, 8),
                  modifier = list(page = "current")
                )
              ),
              list(
                extend = "excel",
                text = "Excel",
                exportOptions = list(
                  columns = c(0:6, 8),
                  modifier = list(page = "current")
                )
              ),
              list(
                extend = "pdf",
                text = "PDF",
                exportOptions = list(
                  columns = c(0:6, 8),
                  modifier = list(page = "current")
                )
              ),
              list(
                extend = "print",
                text = "Print",
                exportOptions = list(
                  columns = c(0:6, 8),
                  modifier = list(page = "current")
                )
              )
            )
          ),
          list(extend = "colvis", text = "Columns")
        ),
        scrollX = FALSE,
        autoWidth = TRUE,
        responsive = TRUE,
        columnDefs = list(
          list(className = "all", targets = c(0, 1, 4, 5, 6, 7)),
          list(className = "min-tablet", targets = c(2, 3)),
          list(className = "dt-center", targets = "_all"),
          list(
            targets = 7,
            render = trend_js,
            orderable = FALSE,
            searchable = FALSE
          ),
          list(
            targets = 8,
            visible = FALSE
          )
        )
      ),
      selection = "single",
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # =========================
  # EDA SECTION
  # =========================
  
  safe_division <- reactive({
    if (is.null(input$aa_division) || identical(input$aa_division, "")) NULL else input$aa_division
  })
  
  safe_group <- reactive({
    if (is.null(input$aa_group_name) || identical(input$aa_group_name, "")) NULL else input$aa_group_name
  })
  
  filtered_data <- reactive({
    req(input$aa_date_range)
    
    aa_cpi_data %>%
      filter(
        date >= min(as.Date(input$aa_date_range)),
        date <= max(as.Date(input$aa_date_range))
      )
  })
  
  aa_max_select <- reactive({
    req(input$aa_eda_tabs)
    
    switch(input$aa_eda_tabs,
           "Trend" = 6,
           "Seasonality" = 2,
           "Autocorrelation" = 3,
           6
    )
  })
  
  output$aa_division_ui <- renderUI({
    req(input$aa_plot_level)
    
    lvl <- as.numeric(input$aa_plot_level)
    
    if (lvl == 2) {
      choices <- filtered_data() %>%
        filter(level == 2, !is.na(division)) %>%
        distinct(division) %>%
        arrange(division) %>%
        pull(division)
      
      if (length(choices) == 0) return(NULL)
      
      selectInput(
        "aa_division",
        "Division",
        choices = choices,
        selected = choices[1]
      )
      
    } else if (lvl == 3) {
      choices <- filtered_data() %>%
        filter(level == 3, !is.na(division)) %>%
        distinct(division) %>%
        arrange(division) %>%
        pull(division)
      
      if (length(choices) == 0) return(NULL)
      
      selectInput(
        "aa_division",
        "Division",
        choices = choices,
        selected = choices[1]
      )
    }
  })
  
  output$aa_group_ui <- renderUI({
    req(input$aa_plot_level)
    
    lvl <- as.numeric(input$aa_plot_level)
    
    if (lvl == 3) {
      req(input$aa_division)
      
      groups <- filtered_data() %>%
        filter(
          level == 3,
          !is.na(group),
          division == input$aa_division
        ) %>%
        distinct(group) %>%
        arrange(group) %>%
        pull(group)
      
      if (length(groups) == 0) return(NULL)
      
      selectInput(
        "aa_group_name",
        "Group",
        choices = groups,
        selected = groups[1]
      )
    }
  })
  
  output$aa_series_select_ui <- renderUI({
    req(input$aa_plot_level)
    
    lvl <- as.numeric(input$aa_plot_level)
    df <- filtered_data()
    
    if (lvl == 0) {
      df <- df %>% filter(level == 0)
    } else if (lvl == 1) {
      df <- df %>% filter(level == 1)
    } else if (lvl == 2) {
      req(input$aa_division)
      df <- df %>%
        filter(
          level == 2,
          division == input$aa_division
        )
    } else if (lvl == 3) {
      req(input$aa_division)
      req(input$aa_group_name)
      df <- df %>%
        filter(
          level == 3,
          division == input$aa_division,
          group == input$aa_group_name
        )
    }
    
    choices <- df %>%
      filter(!is.na(series)) %>%
      distinct(series) %>%
      arrange(series) %>%
      pull(series)
    
    max_select <- aa_max_select()
    
    selectizeInput(
      "aa_series_select",
      label = paste0("Series (max ", max_select, ")"),
      choices = choices,
      selected = head(choices, min(2, length(choices))),
      multiple = TRUE,
      options = list(
        maxItems = max_select,
        plugins = list("remove_button"),
        placeholder = paste0("Select up to ", max_select, " series")
      )
    )
  })
  
  output$aa_trend_plot <- renderPlotly({
    req(input$aa_facet_cols, input$aa_colour_by_year)
    
    validate(
      need(!is.null(input$aa_series_select) && length(input$aa_series_select) > 0,
           "Please select at least one series.")
    )
    
    plot_cpi_time_series(
      data = filtered_data(),
      plot_level = as.numeric(input$aa_plot_level),
      category = safe_division(),
      group_name = safe_group(),
      series_select = input$aa_series_select,
      use_year_colour = isTRUE(input$aa_colour_by_year == "year"),
      facet_ncol = as.numeric(input$aa_facet_cols),
      smooth = isTRUE(input$aa_smooth),
      interactive = TRUE
    )
  })
  
  output$aa_seasonality_cards_ui <- renderUI({
    selected_series <- input$aa_series_select
    n <- length(selected_series)
    
    validate(
      need(!is.null(selected_series) && length(selected_series) > 0,
           "Please select at least one series.")
    )
    
    col_width <- if (n == 1) 12 else 6
    
    plot_output_list <- lapply(seq_along(selected_series), function(i) {
      column(
        width = col_width,
        div(
          style = "
          background: white;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 12px;
          margin-bottom: 20px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.08);
        ",
          div(
            selected_series[i],
            style = "margin-top:0; margin-bottom:8px; font-size:16px; font-weight:600;"
          ),
          plotlyOutput(outputId = paste0("aa_season_plot_", i), height = "550px")
        )
      )
    })
    
    do.call(fluidRow, plot_output_list)
  })
  
  output$aa_acf_plot <- renderPlotly({
    req(input$aa_lags)
    
    validate(
      need(!is.null(input$aa_series_select) && length(input$aa_series_select) > 0,
           "Please select at least one series.")
    )
    
    plot_cpi_acf(
      data = filtered_data(),
      plot_level = as.numeric(input$aa_plot_level),
      category = safe_division(),
      group_name = safe_group(),
      series_select = input$aa_series_select,
      lags = input$aa_lags,
      interactive = TRUE,
      show_white_noise_bars = isTRUE(input$aa_acf_white_noise)
    )
  })
  
  observeEvent(input$aa_reset, {
    updateRadioButtons(session, "aa_plot_level", selected = 1)
    updateSelectInput(session, "aa_colour_by_year", selected = "none")
    updateSelectInput(session, "aa_facet_cols", selected = "1")
    updateSelectInput(session, "aa_season_geom", selected = "boxplot")
    updateCheckboxGroupInput(session, "aa_season_feature", selected = "month.lbl")
    updateCheckboxInput(session, "aa_smooth", value = TRUE)
    updateCheckboxInput(session, "aa_acf_white_noise", value = TRUE)
    updateNumericInput(session, "aa_lags", value = 24)
    updateSliderTextInput(session, "aa_date_range", selected = range(aa_cpi_data$date))
  })
  
  observeEvent(input$aa_series_select, {
    req(input$aa_eda_tabs)
    
    max_select <- aa_max_select()
    
    if (!is.null(input$aa_series_select) && length(input$aa_series_select) > max_select) {
      updateSelectizeInput(
        session,
        "aa_series_select",
        selected = input$aa_series_select[1:max_select]
      )
      
      showNotification(
        paste("Please select up to", max_select, "series only."),
        type = "warning"
      )
    }
  }, ignoreInit = TRUE)
  
  observe({
    req(input$aa_series_select)
    req(input$aa_season_geom)
    req(input$aa_season_feature)
    
    selected_series <- input$aa_series_select
    
    for (i in seq_along(selected_series)) {
      local({
        idx <- i
        series_name <- selected_series[idx]
        
        output[[paste0("aa_season_plot_", idx)]] <- renderPlotly({
          plots <- plot_cpi_seasonal(
            data = filtered_data(),
            plot_level = as.numeric(input$aa_plot_level),
            category = safe_division(),
            group_name = safe_group(),
            series_select = series_name,
            geom = input$aa_season_geom,
            feature_set = input$aa_season_feature,
            interactive = TRUE,
            return_plots = TRUE
          )
          
          plots[[1]]
        })
      })
    }
  })
  
  # =========================
  # FORECAST
  # =========================
  
  cc_all_series <- dplyr::pull(cc_parent_lookup, series) %>% unique()
  cc_all_levels <- dplyr::pull(cc_parent_lookup, Level) %>% unique() %>% sort()
  
  cc_actuals_all <- cc_cpi_data %>%
    dplyr::filter(series %in% cc_all_series) %>%
    dplyr::select(series, date, cpi) %>%
    dplyr::mutate(.key = "actual")
  
  cc_selected_forecast <- reactive({
    if (input$cc_forecast_mode == "auto") {
      cc_reconciled_forecasts
    } else {
      switch(
        input$cc_model_select,
        "ets" = cc_ets,
        "arima" = cc_arima,
        "boost" = cc_boost,
        "prophet" = cc_prophet
      )
    }
  })
  
  cc_plot_data <- reactive({
    cc_forecast_all <- cc_selected_forecast() %>%
      dplyr::filter(series %in% cc_all_series) %>%
      dplyr::select(
        series, date, cpi = .value,
        lo90, hi90, lo95, hi95, lo99, hi99
      ) %>%
      dplyr::mutate(.key = "prediction")
    
    dplyr::bind_rows(cc_actuals_all, cc_forecast_all)
  })
  
  observe({
    updateSelectInput(
      session,
      "cc_level_select",
      choices = setNames(cc_all_levels, paste("Level", cc_all_levels)),
      selected = 0
    )
  })
  
  observeEvent(input$cc_level_select, {
    req(input$cc_level_select)
    
    cc_lvl <- as.integer(input$cc_level_select)
    
    cc_cats <- cc_parent_lookup %>%
      dplyr::filter(Level == cc_lvl) %>%
      dplyr::pull(series) %>%
      unique() %>%
      sort()
    
    cc_cats <- cc_cats[!cc_cats %in% c(
      "All Items Less Accommodation",
      "All Items Less Imputed Rentals For Housing"
    )]
    
    default_cat <- if ("Consumer Price Index (CPI)" %in% cc_cats) {
      "Consumer Price Index (CPI)"
    } else {
      cc_cats[1]
    }
    
    updateSelectInput(
      session,
      "cc_category_select",
      choices = cc_cats,
      selected = default_cat
    )
  })
  
  output$cc_chart_header <- renderUI({
    req(input$cc_category_select, input$cc_level_select)
    
    cc_model_label <- if (input$cc_forecast_mode == "auto") {
      "Auto — Best Model by RMSE"
    } else {
      switch(
        input$cc_model_select,
        "ets" = "ETS",
        "arima" = "Auto-ARIMA",
        "boost" = "ARIMA + XGBoost",
        "prophet" = "Prophet"
      )
    }
    
    tagList(
      h5(input$cc_category_select,style = "font-weight:600;"
      ),
      tags$p(
        paste0(
          "Level ", input$cc_level_select,
          " | CPI Index (Base: 2024 = 100)",
          " | ", cc_model_label
        ),
        style = "color:#666; margin-bottom:15px;"
      )
    )
  })
  
  output$cc_cpi_plot <- plotly::renderPlotly({
    req(input$cc_category_select)
    
    cc_s <- input$cc_category_select
    
    cc_actual_data <- cc_plot_data() %>%
      dplyr::filter(series == cc_s, .key == "actual") %>%
      dplyr::arrange(date)
    
    cc_fc_data <- cc_plot_data() %>%
      dplyr::filter(series == cc_s, .key == "prediction") %>%
      dplyr::arrange(date)
    
    req(nrow(cc_actual_data) > 0, nrow(cc_fc_data) > 0)
    
    cc_fc_start <- min(cc_fc_data$date, na.rm = TRUE)
    cc_fc_end   <- max(cc_fc_data$date, na.rm = TRUE)
    
    cc_x_range <- NULL
    
    if (!is.null(input$cc_date_window)) {
      if (input$cc_date_window == "6") {
        cc_x_range <- c(cc_fc_start, cc_fc_start %m+% months(5))
      } else if (input$cc_date_window == "12") {
        cc_x_range <- c(cc_fc_start, cc_fc_end)
      } else if (input$cc_date_window == "24") {
        cc_x_range <- c(cc_fc_start %m-% months(12), cc_fc_end)
      } else if (input$cc_date_window == "all") {
        cc_x_range <- NULL
      }
    }
    
    if (!is.null(cc_x_range)) {
      cc_actual_visible <- cc_actual_data %>%
        dplyr::filter(date >= cc_x_range[1], date <= cc_x_range[2])
      
      cc_fc_visible <- cc_fc_data %>%
        dplyr::filter(date >= cc_x_range[1], date <= cc_x_range[2])
    } else {
      cc_actual_visible <- cc_actual_data
      cc_fc_visible <- cc_fc_data
    }
    
    cc_y_values <- c(
      cc_actual_visible$cpi,
      cc_fc_visible$cpi,
      cc_fc_visible$lo90, cc_fc_visible$hi90,
      cc_fc_visible$lo95, cc_fc_visible$hi95,
      cc_fc_visible$lo99, cc_fc_visible$hi99
    )
    cc_y_values <- cc_y_values[is.finite(cc_y_values)]
    
    cc_y_range <- NULL
    if (length(cc_y_values) > 0) {
      cc_y_min <- min(cc_y_values)
      cc_y_max <- max(cc_y_values)
      cc_y_pad <- (cc_y_max - cc_y_min) * 0.08
      
      if (cc_y_pad == 0) {
        cc_y_pad <- max(abs(cc_y_min) * 0.05, 0.5)
      }
      
      cc_y_range <- c(cc_y_min - cc_y_pad, cc_y_max + cc_y_pad)
    }
    
    cc_last_actual <- cc_actual_data %>%
      dplyr::slice_tail(n = 1)
    
    cc_first_fc <- cc_fc_data %>%
      dplyr::slice_head(n = 1)
    
    p <- plotly::plot_ly()
    
    p <- p %>%
      plotly::add_ribbons(
        x = cc_fc_data$date,
        ymin = cc_fc_data$lo99,
        ymax = cc_fc_data$hi99,
        name = "99% CI",
        showlegend = TRUE,
        line = list(color = "transparent"),
        fillcolor = "rgba(100, 116, 139, 0.10)",
        hoverinfo = "skip"
      ) %>%
      plotly::add_ribbons(
        x = cc_fc_data$date,
        ymin = cc_fc_data$lo95,
        ymax = cc_fc_data$hi95,
        name = "95% CI",
        showlegend = TRUE,
        line = list(color = "transparent"),
        fillcolor = "rgba(100, 116, 139, 0.18)",
        hoverinfo = "skip"
      ) %>%
      plotly::add_ribbons(
        x = cc_fc_data$date,
        ymin = cc_fc_data$lo90,
        ymax = cc_fc_data$hi90,
        name = "90% CI",
        showlegend = TRUE,
        line = list(color = "transparent"),
        fillcolor = "rgba(100, 116, 139, 0.28)",
        hoverinfo = "skip"
      ) %>%
      plotly::add_trace(
        x = cc_actual_data$date,
        y = cc_actual_data$cpi,
        type = "scatter",
        mode = "lines",
        name = "Actual",
        showlegend = TRUE,
        line = list(color = "#2c3e50", width = 2),
        hovertemplate = "<b>%{x|%b %Y}</b><br>CPI: %{y:.2f}<extra>Actual</extra>"
      ) %>%
      plotly::add_trace(
        x = c(cc_last_actual$date, cc_first_fc$date),
        y = c(cc_last_actual$cpi, cc_first_fc$cpi),
        type = "scatter",
        mode = "lines",
        name = "Transition",
        line = list(color = "#7c3aed", width = 2, dash = "dot"),
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      plotly::add_trace(
        x = cc_fc_data$date,
        y = cc_fc_data$cpi,
        type = "scatter",
        mode = "lines",
        name = "Forecast",
        showlegend = TRUE,
        line = list(color = "#7c3aed" , width = 2, dash = "dot"),
        hovertemplate = "<b>%{x|%b %Y}</b><br>CPI: %{y:.2f}<extra>Forecast</extra>"
      ) %>%
      plotly::layout(
        xaxis = list(
          title = "",
          rangeslider = list(visible = TRUE),
          range = cc_x_range
        ),
        yaxis = list(
          title = "CPI Value",
          range = cc_y_range
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.05,
          xanchor = "left",
          yanchor = "bottom",
          traceorder = "normal",
          font = list(size = 12)
        ),
        margin = list(t = 10, r = 20, b = 50, l = 60)
      )
    
    p
  })

}




shinyApp(ui, server)
