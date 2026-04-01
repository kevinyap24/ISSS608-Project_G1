pacman::p_load(
  shiny, dplyr, lubridate, plotly, timetk,
  ggplot2, shinyWidgets, readr, tidyr, DT,
  htmltools, bslib, reactable,
  bsicons, shinycssloaders, sparkline
)

filter    <- dplyr::filter
select    <- dplyr::select
mutate    <- dplyr::mutate
pull      <- dplyr::pull
bind_rows <- dplyr::bind_rows
observe      <- shiny::observe
observeEvent <- shiny::observeEvent
reactive     <- shiny::reactive
renderUI     <- shiny::renderUI

plot_cpi_time_series <- function(data, plot_level, category = NULL,
                                 group_name = NULL, series_select = NULL,
                                 use_year_colour = FALSE, facet_by = TRUE,
                                 facet_ncol = 1, facet_nrow = 1,
                                 facet_scales = "fixed", smooth = FALSE,
                                 smooth_period = "auto", smooth_span = NULL,
                                 smooth_degree = 2, plotly_slider = FALSE,
                                 interactive = TRUE, add_caption = TRUE,
                                 caption_text = "Source: CEIC Database | Index, 2024 = 100",
                                 title = NULL, x_lab = "", y_lab = "CPI Index",
                                 color_lab = "Year", line_color = "#c0392b",
                                 return_plot = TRUE) {
  
  data <- data %>%
    dplyr::filter(!is.na(series)) %>%
    dplyr::mutate(year = factor(lubridate::year(date)))
  
  if (plot_level == 0) {
    plot_data <- data %>% dplyr::filter(level == 0)
    facet_col <- "series"
  } else if (plot_level == 1) {
    plot_data <- data %>% dplyr::filter(level == 1)
    facet_col <- "division"
  } else if (plot_level == 2) {
    plot_data <- data %>% dplyr::filter(level == 2)
    if (!is.null(category)) plot_data <- plot_data %>% dplyr::filter(division == category)
    facet_col <- "series"
  } else if (plot_level == 3) {
    plot_data <- data %>% dplyr::filter(level == 3)
    if (!is.null(category))   plot_data <- plot_data %>% dplyr::filter(division == category)
    if (!is.null(group_name)) plot_data <- plot_data %>% dplyr::filter(group == group_name)
    facet_col <- "series"
  } else {
    stop("plot_level must be 0, 1, 2, or 3")
  }
  
  if (!is.null(series_select) && length(series_select) > 0) {
    available_series <- unique(plot_data$series)
    missing_series   <- setdiff(series_select, available_series)
    if (length(missing_series) > 0)
      stop(paste("These series were not found:", paste(missing_series, collapse = ", ")))
    plot_data <- plot_data %>% dplyr::filter(series %in% series_select)
  }
  
  if (nrow(plot_data) == 0) stop("No data found for the specified criteria")
  if (is.null(title)) title <- " "
  
  if (use_year_colour && facet_by) {
    p <- timetk::plot_time_series(
      .data = plot_data, .date_var = date, .value = cpi, .color_var = year,
      .facet_vars = tidyselect::all_of(facet_col),
      .facet_ncol = facet_ncol, .facet_nrow = facet_nrow,
      .facet_scales = facet_scales, .smooth = smooth,
      .smooth_period = smooth_period, .smooth_span = smooth_span,
      .smooth_degree = smooth_degree, .legend_show = TRUE,
      .title = title, .x_lab = x_lab, .y_lab = y_lab,
      .color_lab = color_lab, .interactive = interactive,
      .plotly_slider = plotly_slider
    )
  } else if (use_year_colour && !facet_by) {
    p <- timetk::plot_time_series(
      .data = plot_data, .date_var = date, .value = cpi, .color_var = year,
      .facet_ncol = facet_ncol, .facet_nrow = facet_nrow,
      .facet_scales = facet_scales, .smooth = smooth,
      .smooth_period = smooth_period, .smooth_span = smooth_span,
      .smooth_degree = smooth_degree, .legend_show = TRUE,
      .title = title, .x_lab = x_lab, .y_lab = y_lab,
      .color_lab = color_lab, .interactive = interactive,
      .plotly_slider = plotly_slider
    )
  } else if (!use_year_colour && facet_by) {
    p <- timetk::plot_time_series(
      .data = plot_data, .date_var = date, .value = cpi,
      .facet_vars = tidyselect::all_of(facet_col),
      .facet_ncol = facet_ncol, .facet_nrow = facet_nrow,
      .facet_scales = facet_scales, .line_color = line_color,
      .smooth = smooth, .smooth_period = smooth_period,
      .smooth_span = smooth_span, .smooth_degree = smooth_degree,
      .legend_show = FALSE, .title = title, .x_lab = x_lab,
      .y_lab = y_lab, .interactive = interactive,
      .plotly_slider = plotly_slider
    )
  } else {
    p <- timetk::plot_time_series(
      .data = plot_data, .date_var = date, .value = cpi,
      .facet_ncol = facet_ncol, .facet_nrow = facet_nrow,
      .facet_scales = facet_scales, .line_color = line_color,
      .smooth = smooth, .smooth_period = smooth_period,
      .smooth_span = smooth_span, .smooth_degree = smooth_degree,
      .legend_show = FALSE, .title = title, .x_lab = x_lab,
      .y_lab = y_lab, .interactive = interactive,
      .plotly_slider = plotly_slider
    )
  }
  
  if (add_caption && interactive) {
    p <- p %>% plotly::layout(
      annotations = list(list(
        text = caption_text, x = 1, y = -0.16,
        xref = "paper", yref = "paper", showarrow = FALSE,
        xanchor = "right", font = list(size = 10, color = "#7a6f68")
      )),
      margin = list(b = 90)
    )
  }
  
  if (return_plot) return(p) else print(p)
}

plot_cpi_seasonal <- function(data, plot_level, category = NULL,
                              group_name = NULL, series_select = NULL,
                              interactive = TRUE, geom = "boxplot",
                              geom_color = "#c0392b",
                              feature_set = c("month.lbl"),
                              return_plots = FALSE, add_caption = TRUE,
                              caption_text = "Source: CEIC Database | Index: 2024 = 100") {
  
  data <- data %>% dplyr::filter(!is.na(series))
  if (length(feature_set) == 0) stop("Please select at least one seasonality feature.")
  
  if (plot_level == 0) {
    filtered <- data %>% dplyr::filter(level == 0)
  } else if (plot_level == 1) {
    filtered <- data %>% dplyr::filter(level == 1)
  } else if (plot_level == 2) {
    filtered <- data %>% dplyr::filter(level == 2)
    if (!is.null(category)) filtered <- filtered %>% dplyr::filter(division == category)
  } else if (plot_level == 3) {
    filtered <- data %>% dplyr::filter(level == 3)
    if (!is.null(category))   filtered <- filtered %>% dplyr::filter(division == category)
    if (!is.null(group_name)) filtered <- filtered %>% dplyr::filter(group == group_name)
  } else {
    stop("plot_level must be 0, 1, 2, or 3")
  }
  
  available_series <- filtered %>% dplyr::distinct(series) %>% dplyr::pull(series)
  
  if (!is.null(series_select) && length(series_select) > 0) {
    missing_series <- setdiff(series_select, available_series)
    if (length(missing_series) > 0)
      stop(paste("These series were not found:", paste(missing_series, collapse = ", ")))
    series_list <- series_select
  } else {
    series_list <- available_series
  }
  
  if (length(series_list) == 0) stop("No series found for the specified criteria")
  
  plots <- list()
  for (s in series_list) {
    plot_data <- filtered %>% dplyr::filter(series == s)
    p <- timetk::plot_seasonal_diagnostics(
      .data = plot_data, .date_var = date, .value = cpi,
      .feature_set = feature_set, .geom = geom, .geom_color = geom_color,
      .title = "", .x_lab = "Date", .y_lab = "CPI Index",
      .interactive = interactive
    )
    if (add_caption && interactive) {
      p <- p %>% plotly::layout(
        annotations = list(list(
          text = caption_text, x = 1, y = -0.15,
          xref = "paper", yref = "paper", showarrow = FALSE,
          xanchor = "right", font = list(size = 10, color = "#7a6f68")
        )),
        margin = list(b = 80)
      )
    }
    plots[[s]] <- p
  }
  
  if (return_plots) return(plots)
  print(plots[[1]])
}

plot_cpi_acf <- function(data, plot_level, category = NULL, group_name = NULL,
                         series_select = NULL, lags = 24, interactive = TRUE,
                         line_color = "#c0392b", line_size = 0.5, line_alpha = 1,
                         point_color = "#c0392b", point_size = 1, point_alpha = 1,
                         x_intercept = NULL, x_intercept_color = "#e74c3c",
                         hline_color = "#1a1612", white_noise_line_type = 2,
                         white_noise_line_color = "#d4a853",
                         show_ccf_vars_only = FALSE, show_white_noise_bars = TRUE,
                         plotly_slider = FALSE, ccf_target = NULL,
                         ccf_predictors = NULL, add_caption = TRUE,
                         caption_text = "Source: CEIC Database | Index: 2024 = 100") {
  
  data <- data %>% dplyr::filter(!is.na(series))
  
  if (plot_level == 0) {
    plot_data <- data %>% dplyr::filter(level == 0)
  } else if (plot_level == 1) {
    plot_data <- data %>% dplyr::filter(level == 1)
  } else if (plot_level == 2) {
    plot_data <- data %>% dplyr::filter(level == 2)
    if (!is.null(category)) plot_data <- plot_data %>% dplyr::filter(division == category)
  } else if (plot_level == 3) {
    plot_data <- data %>% dplyr::filter(level == 3)
    if (!is.null(category))   plot_data <- plot_data %>% dplyr::filter(division == category)
    if (!is.null(group_name)) plot_data <- plot_data %>% dplyr::filter(group == group_name)
  } else {
    stop("plot_level must be 0, 1, 2, or 3")
  }
  
  available_series <- unique(plot_data$series)
  if (nrow(plot_data) == 0) stop("No data found for the specified criteria")
  
  if (!is.null(series_select) && length(series_select) > 0) {
    missing_series <- setdiff(series_select, available_series)
    if (length(missing_series) > 0)
      stop(paste("These series were not found:", paste(missing_series, collapse = ", ")))
    plot_data <- plot_data %>% dplyr::filter(series %in% series_select)
  }
  
  use_ccf <- !is.null(ccf_target) && !is.null(ccf_predictors)
  
  if (!use_ccf) {
    acf_data <- plot_data %>% dplyr::group_by(series)
    p <- timetk::plot_acf_diagnostics(
      .data = acf_data, .date_var = date, .value = cpi, .lags = lags,
      .show_white_noise_bars = show_white_noise_bars,
      .line_color = line_color, .line_size = line_size, .line_alpha = line_alpha,
      .point_color = point_color, .point_size = point_size, .point_alpha = point_alpha,
      .x_intercept = x_intercept, .x_intercept_color = x_intercept_color,
      .hline_color = hline_color, .white_noise_line_type = white_noise_line_type,
      .white_noise_line_color = white_noise_line_color,
      .title = "", .x_lab = paste0("Lag (selected = ", lags, ")"),
      .y_lab = "Correlation", .interactive = interactive,
      .plotly_slider = plotly_slider
    )
  } else {
    series_needed  <- c(ccf_target, ccf_predictors)
    missing_series <- setdiff(series_needed, unique(plot_data$series))
    if (length(missing_series) > 0)
      stop(paste("These series were not found:", paste(missing_series, collapse = ", ")))
    
    wide_data <- plot_data %>%
      dplyr::filter(series %in% series_needed) %>%
      dplyr::select(date, series, cpi) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = series, values_from = cpi) %>%
      dplyr::arrange(date) %>%
      tidyr::drop_na(dplyr::all_of(series_needed))
    
    if (nrow(wide_data) == 0) stop("No complete cases available after reshaping.")
    
    target_sym <- rlang::sym(ccf_target)
    p <- timetk::plot_acf_diagnostics(
      .data = wide_data, .date_var = date, .value = !!target_sym,
      .ccf_vars = dplyr::all_of(ccf_predictors), .lags = lags,
      .show_ccf_vars_only = show_ccf_vars_only,
      .show_white_noise_bars = show_white_noise_bars,
      .line_color = line_color, .line_size = line_size, .line_alpha = line_alpha,
      .point_color = point_color, .point_size = point_size, .point_alpha = point_alpha,
      .x_intercept = x_intercept, .x_intercept_color = x_intercept_color,
      .hline_color = hline_color, .white_noise_line_type = white_noise_line_type,
      .white_noise_line_color = white_noise_line_color,
      .title = "", .x_lab = "Lag", .y_lab = "Correlation",
      .interactive = interactive, .plotly_slider = plotly_slider
    )
  }
  
  if (add_caption && interactive) {
    p <- p %>% plotly::layout(
      annotations = list(list(
        text = caption_text, x = 1, y = -0.15,
        xref = "paper", yref = "paper", showarrow = FALSE,
        xanchor = "right", font = list(size = 10, color = "#7a6f68")
      )),
      margin = list(b = 80)
    )
  }
  
  p
}

# ==============================================================================
# UI
# ==============================================================================
eda_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:ital,wght@0,300;0,500;0,700;1,300&display=swap');

      .eda-wrapper {
        font-family: 'IBM Plex Sans', sans-serif;
        background: #f5f0e8;
        min-height: 100vh;
        padding: 24px;
      }
      .eda-wrapper .well {
        background: #ede8df !important;
        border: 1px solid #d0c8be !important;
        border-left: 4px solid #c0392b !important;
        border-radius: 0 !important;
        box-shadow: none !important;
      }
      .eda-wrapper label {
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 10px !important;
        font-weight: 600 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.1em !important;
        color: #7a6f68 !important;
      }
      .eda-wrapper select,
      .eda-wrapper input[type='number'] {
        font-family: 'IBM Plex Sans', sans-serif !important;
        font-size: 13px !important;
        font-weight: 300 !important;
        background: #f5f0e8 !important;
        border: 1px solid #d0c8be !important;
        border-radius: 0 !important;
        color: #1a1612 !important;
      }
      .eda-wrapper select:focus,
      .eda-wrapper input:focus {
        border-color: #c0392b !important;
        outline: none !important;
        box-shadow: none !important;
      }
      .eda-wrapper .nav-tabs {
        border-bottom: 2px solid #c0392b !important;
        font-family: 'IBM Plex Mono', monospace !important;
      }
      .eda-wrapper .nav-tabs > li > a {
        font-size: 11px !important;
        letter-spacing: 0.1em !important;
        text-transform: uppercase !important;
        color: #7a6f68 !important;
        border-radius: 0 !important;
        border: 1px solid transparent !important;
      }
      .eda-wrapper .nav-tabs > li.active > a,
      .eda-wrapper .nav-tabs > li.active > a:hover {
        color: #1a1612 !important;
        background: #ede8df !important;
        border-color: #d0c8be #d0c8be #ede8df !important;
        border-bottom: 2px solid #c0392b !important;
      }
      .eda-wrapper .nav-tabs > li > a:hover {
        color: #c0392b !important;
        background: transparent !important;
      }
      .eda-wrapper .tab-content {
        background: #ede8df !important;
        border: 1px solid #d0c8be !important;
        border-top: none !important;
        padding: 20px !important;
      }
      .eda-subtitle {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 11px;
        color: #7a6f68;
        letter-spacing: 0.04em;
        margin-bottom: 16px;
      }
      .eda-wrapper .btn-primary {
        background: #c0392b !important;
        border-color: #c0392b !important;
        border-radius: 0 !important;
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 11px !important;
        letter-spacing: 0.08em !important;
        text-transform: uppercase !important;
      }
      .eda-wrapper .btn-primary:hover {
        background: transparent !important;
        color: #c0392b !important;
      }
      .eda-season-card {
        background: #f5f0e8;
        border: 1px solid #d0c8be;
        border-left: 4px solid #c0392b;
        padding: 16px;
        margin-bottom: 20px;
      }
      .eda-season-card-title {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        color: #c0392b;
        margin-bottom: 12px;
      }
      .eda-wrapper .dataTables_wrapper {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 13px;
        font-weight: 300;
      }
      .eda-wrapper table.dataTable thead th {
        background: #1a1612 !important;
        color: #f5f0e8 !important;
        font-family: 'IBM Plex Mono', monospace !important;
        font-size: 10px !important;
        letter-spacing: 0.1em !important;
        text-transform: uppercase !important;
        border: none !important;
      }
      .eda-wrapper table.dataTable tbody tr:hover {
        background: #ede8df !important;
      }
      .eda-wrapper .bslib-value-box {
        border-radius: 0 !important;
        border-left: 4px solid #c0392b !important;
      }
      .eda-wrapper hr { border-color: #d0c8be; }
      .eda-summary-bar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        background: #ede8df;
        border: 1px solid #d0c8be;
        border-left: 4px solid #c0392b;
        padding: 10px 16px;
        margin-bottom: 12px;
      }
      .eda-summary-item { text-align: center; flex: 1; }
      .eda-summary-label {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        color: #7a6f68;
        margin-bottom: 2px;
      }
      .eda-summary-value {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 14px;
        font-weight: 600;
        color: #1a1612;
      }
      
      /* ── Hero box full height ── */
      .eda-wrapper .bslib-layout-columns > div:first-child {
        display: flex !important;
        flex-direction: column !important;
      }
      .eda-wrapper .bslib-layout-columns > div:first-child .shiny-spinner-output-container,
      .eda-wrapper .bslib-layout-columns > div:first-child .bslib-value-box {
        flex: 1 !important;
        height: 100% !important;
      }
    "))),
    
    div(class = "eda-wrapper",
        tabsetPanel(
          id = ns("eda_main_tabs"),
          
          tabPanel("Dashboard",
                   br(),
                   bslib::layout_columns(
                     col_widths = c(6, 6),
                     div(shinycssloaders::withSpinner(
                       shiny::uiOutput(ns("aa_hero_cpi")), type = 4, color = "#c0392b"
                     )),
                     div(
                       div(
                         style = "display:grid; grid-template-columns:1fr 1fr; gap:8px;",
                         shiny::uiOutput(ns("aa_kpi_mom")),
                         shiny::uiOutput(ns("aa_kpi_yoy")),
                         shiny::uiOutput(ns("aa_kpi_top")),
                         shiny::uiOutput(ns("aa_kpi_drag"))
                       )
                     )
                   ),
                   br(),
                   div(
                     div(
                       style = "display:flex; align-items:center; gap:6px; font-family:'IBM Plex Sans',sans-serif; font-size:15px; font-weight:700; color:#1a1612; margin-bottom:4px;",
                       "Major Category Contribution Dashboard",
                       bslib::tooltip(bsicons::bs_icon("info-circle"),
                                      "Contribution (pp) shows how much each category adds to or drags down overall CPI inflation.")
                     ),
                     div(class = "eda-subtitle",
                         "Latest CPI, 12-month trend, YoY%, weight, and contribution by major category"),
                     reactable::reactableOutput(ns("aa_dashboard_table")),
                     div(style = "font-size:11px; color:#7a6f68; margin-top:6px; text-align:right; font-family:'IBM Plex Mono',monospace;",
                         "Source: Singapore Department of Statistics | CEIC Database | Base Year: 2024")
                   )
          ),
          
          tabPanel("Data Explorer",
                   br(),
                   div(class = "eda-summary-bar",
                       div(class = "eda-summary-item",
                           div(class = "eda-summary-label", "Total Rows"),
                           div(class = "eda-summary-value", shiny::uiOutput(ns("aa_total_rows"), inline = TRUE))
                       ),
                       div(class = "eda-summary-item",
                           div(class = "eda-summary-label", "Divisions"),
                           div(class = "eda-summary-value", shiny::uiOutput(ns("aa_total_divisions"), inline = TRUE))
                       ),
                       div(class = "eda-summary-item",
                           div(class = "eda-summary-label", "Last Updated"),
                           div(class = "eda-summary-value", shiny::uiOutput(ns("aa_last_updated"), inline = TRUE))
                       )
                   ),
                   DT::DTOutput(ns("aa_cpi_table")),
                   div(style = "margin-top:12px; font-family:'IBM Plex Mono',monospace; font-size:11px; color:#7a6f68; text-align:center;",
                       "▲ = Increase | ▼ = Decrease | ◆ = No change | Base Year: 2024 | Source: CEIC Database")
          ),
          
          tabPanel("EDA",
                   br(),
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       tags$details(
                         open = "open",
                         tags$summary(
                           style = "font-family:'IBM Plex Mono',monospace; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:0.1em; cursor:pointer; margin-bottom:12px; color:#1a1612;",
                           "Control Panel"
                         ),
                         radioButtons(ns("aa_plot_level"), "Hierarchy Level",
                                      choices = c("All Items" = 0, "Major Category" = 1, "Group" = 2, "Class" = 3),
                                      selected = 1
                         ),
                         shiny::uiOutput(ns("aa_division_ui")),
                         shiny::uiOutput(ns("aa_group_ui")),
                         shiny::uiOutput(ns("aa_series_select_ui")),
                         shinyWidgets::sliderTextInput(ns("aa_date_range"), "Date Range",
                                                       choices  = as.character(seq(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "month")),
                                                       selected = c("2020-01-01", "2025-12-01"),
                                                       grid = FALSE, dragRange = TRUE
                         ),
                         hr(),
                         shiny::conditionalPanel(
                           condition = paste0("input['", ns("aa_eda_tabs"), "'] == 'Trend'"),
                           checkboxInput(ns("aa_smooth"), "Smooth", FALSE),
                           selectInput(ns("aa_colour_by_year"), "Colour by Year",
                                       choices = c("None" = "none", "Year" = "year"), selected = "year"),
                           selectInput(ns("aa_facet_cols"), "Facet Columns",
                                       choices = c("1" = 1, "2" = 2, "3" = 3), selected = 2)
                         ),
                         shiny::conditionalPanel(
                           condition = paste0("input['", ns("aa_eda_tabs"), "'] == 'Seasonality'"),
                           selectInput(ns("aa_season_geom"), "Seasonality Plot",
                                       choices = c("Boxplot" = "boxplot", "Violin" = "violin"), selected = "boxplot"),
                           checkboxGroupInput(ns("aa_season_feature"), "Seasonality Features",
                                              choices = c("Month" = "month.lbl", "Quarter" = "quarter", "Year" = "year"),
                                              selected = c("month.lbl", "quarter"))
                         ),
                         shiny::conditionalPanel(
                           condition = paste0("input['", ns("aa_eda_tabs"), "'] == 'Autocorrelation'"),
                           checkboxInput(ns("aa_acf_white_noise"), "Show White Noise Bars", TRUE),
                           numericInput(ns("aa_lags"), "Lags", value = 24, min = 1, max = 60)
                         ),
                         actionButton(ns("aa_reset"), "Reset", class = "btn-primary")
                       )
                     ),
                     mainPanel(
                       width = 9,
                       tabsetPanel(
                         id = ns("aa_eda_tabs"),
                         tabPanel("Trend",
                                  div(class = "eda-subtitle", style = "margin-top:15px;",
                                      "Visualises CPI trends over time to identify long-term inflation patterns."),
                                  plotly::plotlyOutput(ns("aa_trend_plot"), height = "550px")
                         ),
                         tabPanel("Seasonality",
                                  div(class = "eda-subtitle", style = "margin-top:15px;",
                                      "Identify recurring seasonal patterns in CPI across months, quarters and years."),
                                  shiny::uiOutput(ns("aa_seasonality_cards_ui"))
                         ),
                         tabPanel("Autocorrelation",
                                  div(class = "eda-subtitle", style = "margin-top:15px;",
                                      "Shows how CPI values correlate with past periods to identify persistence and lag effects."),
                                  plotly::plotlyOutput(ns("aa_acf_plot"), height = "550px")
                         )
                       )
                     )
                   )
          )
        )
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================
eda_server <- function(id, cpi_data, cpi_dashboard, dashboard_table, latest_lvl1) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    trend_js <- DT::JS(
      "function(data, type, row, meta) {",
      "  if(type === 'display') {",
      "    var color = data === '▲' ? '#c0392b' : (data === '▼' ? '#1565c0' : '#7a6f68');",
      "    return '<span style=\"color:' + color + '; font-weight:bold; font-size:18px;\">' + data + '</span>';",
      "  }",
      "  return data;",
      "}"
    )
    
    cpi_h_display <- shiny::reactive({
      data <- cpi_data
      
      if (!"class_name" %in% names(data)) {
        data <- data %>% dplyr::mutate(class_name = NA_character_)
      }
      
      data %>%
        dplyr::arrange(series, date) %>%
        dplyr::group_by(series) %>%
        dplyr::mutate(
          As_of      = date,
          As_of_lab  = format(date, "%b %Y"),
          cpi_change = cpi - dplyr::lag(cpi),
          Trend = dplyr::case_when(
            is.na(cpi_change) ~ "◆",
            cpi_change > 0    ~ "▲",
            cpi_change < 0    ~ "▼",
            TRUE              ~ "◆"
          ),
          Trend_export = dplyr::case_when(
            is.na(cpi_change) ~ "No Change",
            cpi_change > 0    ~ "Up",
            cpi_change < 0    ~ "Down",
            TRUE              ~ "No Change"
          ),
          level_label = dplyr::case_when(
            level == 0 ~ "Top Level",
            level == 1 ~ "Major Category",
            level == 2 ~ "Sub-Category",
            level == 3 ~ "Detailed",
            TRUE ~ paste("Level", level)
          ),
          major_group  = dplyr::if_else(level == 0, NA_character_, division),
          display_name = dplyr::case_when(
            level == 0 ~ series,
            level == 1 ~ division,
            level == 2 ~ group,
            level == 3 ~ dplyr::coalesce(class_name, series),
            TRUE ~ series
          ),
          cpi_rounded = round(cpi, 2)
        ) %>%
        dplyr::ungroup()
    })
    
    aa_short_series <- function(x) {
      dplyr::case_when(
        x == "Information & Communication (InfoComm)" ~ "InfoComm",
        x == "Clothing and Footwear (C&F)"            ~ "C&F",
        x == "Housing & Utilities"                    ~ "Housing",
        x == "Household Durables & Services (HDS)"    ~ "HDS",
        x == "Recreation, Sport & Culture (RSC)"      ~ "RSC",
        x == "Miscellaneous Goods & Services (MG&S)"  ~ "MG&S",
        TRUE ~ x
      )
    }
    
    aa_series_icon <- function(x) {
      icon_name <- dplyr::case_when(
        x == "Food"                                                        ~ "basket",
        x %in% c("Clothing and Footwear (C&F)", "C&F")                    ~ "handbag",
        x %in% c("Housing & Utilities", "Housing")                         ~ "house-door",
        x %in% c("Household Durables & Services (HDS)", "HDS")             ~ "lamp",
        x == "Health"                                                      ~ "heart-pulse",
        x == "Transport"                                                   ~ "car-front-fill",
        x %in% c("Information & Communication (InfoComm)", "InfoComm")     ~ "wifi",
        x %in% c("Recreation, Sport & Culture (RSC)", "RSC")               ~ "controller",
        x == "Education"                                                   ~ "book",
        x %in% c("Miscellaneous Goods & Services (MG&S)", "MG&S")          ~ "grid-3x3-gap",
        TRUE ~ "arrow-up-circle"
      )
      bsicons::bs_icon(icon_name, size = "22px")
    }
    
    aa_kpi_box <- function(title, value_ui, subtitle = NULL, showcase = NULL,
                           bg = "#ede8df", fg = "#1a1612") {
      bslib::value_box(
        title = title, value = value_ui, showcase = showcase,
        theme = bslib::value_box_theme(bg = bg, fg = fg),
        if (!is.null(subtitle)) div(style = "font-family:'IBM Plex Mono',monospace; font-size:11px;", subtitle),
        full_screen = FALSE
      )
    }
    
    aa_hero_sparkline <- function(data) {
      aa_df <- data %>% dplyr::filter(is.finite(cpi)) %>% dplyr::arrange(date)
      if (nrow(aa_df) < 2) return(NULL)
      plotly::plot_ly(aa_df) %>%
        plotly::add_lines(x = ~date, y = ~cpi, color = I("#f5f0e8"),
                          fill = "tozeroy", alpha = 0.2,
                          hovertemplate = "CPI: %{y:.2f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
          yaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
          hovermode = "x unified",
          margin = list(t = 0, r = 0, l = 0, b = 0),
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          hoverlabel = list(
            bgcolor     = "#f5f0e8",
            bordercolor = "#1a1612",
            font        = list(
              color  = "#1a1612",
              size   = 12,
              family = "IBM Plex Mono"
            )
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    }
    
    aa_hero_narrative <- function(data) {
      aa_df <- data %>%
        dplyr::filter(is.finite(cpi)) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
          cpi_prev_12m = dplyr::lag(cpi, 12),
          yoy_pct = (cpi / cpi_prev_12m - 1) * 100
        )
      aa_latest <- aa_df %>% dplyr::slice_tail(n = 1)
      aa_first  <- aa_df %>% dplyr::slice_head(n = 1)
      aa_peak   <- aa_df %>% dplyr::slice_max(cpi, n = 1, with_ties = FALSE)
      aa_change <- aa_latest$cpi[1] - aa_first$cpi[1]
      aa_dir <- dplyr::case_when(
        aa_change > 0 ~ "Upward trend over the past 5 years",
        aa_change < 0 ~ "Downward trend over the past 5 years",
        TRUE ~ "Broadly stable over the past 5 years"
      )
      tagList(
        div(style = "font-size:2.8rem; font-weight:700; line-height:1;",
            sprintf("%.1f", aa_latest$cpi[1])),
        div(style = "font-size:0.95rem; line-height:1.3; margin-top:8px;", aa_dir),
        div(style = "font-size:0.95rem;", sprintf("Earliest: %.1f", aa_first$cpi[1])),
        div(style = "font-size:0.95rem;",
            sprintf("5Y peak: %.1f in %s", aa_peak$cpi[1], format(aa_peak$date[1], "%b %Y"))),
        div(style = "font-size:0.95rem;",
            sprintf("%+.1f points since %s", aa_change, format(aa_first$date[1], "%b %Y")))
      )
    }
    
    aa_latest_top_cpi <- shiny::reactive({
      cpi_data %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::filter(series == "Consumer Price Index (CPI)") %>%
        dplyr::arrange(date)
    })
    
    output$aa_hero_cpi <- shiny::renderUI({
      aa_df <- aa_latest_top_cpi()
      shiny::validate(shiny::need(nrow(aa_df) > 13, "Not enough CPI data"))
      bslib::value_box(
        title = "Headline CPI Index",
        value = aa_hero_narrative(aa_df),
        showcase = aa_hero_sparkline(aa_df),
        full_screen = TRUE,
        height = "100%",
        theme = bslib::value_box_theme(bg = "#1a1612", fg = "#f5f0e8")
      )
    })
    
    output$aa_kpi_mom <- shiny::renderUI({
      aa_df <- aa_latest_top_cpi()
      shiny::validate(shiny::need(nrow(aa_df) >= 2, "Not enough history"))
      aa_latest <- aa_df %>% dplyr::slice_tail(n = 1)
      aa_prev   <- aa_df %>% dplyr::filter(date == aa_latest$date[1] %m-% months(1))
      shiny::validate(shiny::need(nrow(aa_prev) == 1, "Prior month not found"))
      aa_mom <- (aa_latest$cpi[1] / aa_prev$cpi[1] - 1) * 100
      aa_kpi_box(
        title = "CPI MoM",
        value_ui = div(style = "font-size:26px; font-weight:700;", sprintf("%+.2f%%", aa_mom)),
        subtitle = paste("vs", format(aa_prev$date[1], "%b %Y")),
        showcase = bsicons::bs_icon("arrow-left-right", size = "22px"),
        bg = if (aa_mom >= 0) "#fdf0f0" else "#f0f4ff",
        fg = if (aa_mom >= 0) "#c0392b" else "#1565c0"
      )
    })
    
    output$aa_kpi_yoy <- shiny::renderUI({
      aa_df <- aa_latest_top_cpi()
      shiny::validate(shiny::need(nrow(aa_df) >= 13, "Not enough history"))
      aa_latest <- aa_df %>% dplyr::slice_tail(n = 1)
      aa_prev   <- aa_df %>% dplyr::filter(date == aa_latest$date[1] %m-% months(12))
      shiny::validate(shiny::need(nrow(aa_prev) == 1, "Prior year not found"))
      aa_yoy <- (aa_latest$cpi[1] / aa_prev$cpi[1] - 1) * 100
      aa_kpi_box(
        title = "CPI YoY",
        value_ui = div(style = "font-size:26px; font-weight:700;", sprintf("%+.2f%%", aa_yoy)),
        subtitle = paste("vs", format(aa_prev$date[1], "%b %Y")),
        showcase = bsicons::bs_icon("activity", size = "22px"),
        bg = if (aa_yoy >= 0) "#fdf0f0" else "#f0f4ff",
        fg = if (aa_yoy >= 0) "#c0392b" else "#1565c0"
      )
    })
    
    output$aa_kpi_top <- shiny::renderUI({
      aa_df <- latest_lvl1
      shiny::validate(shiny::need(nrow(aa_df) > 0, "No data"))
      aa_top <- aa_df %>% dplyr::slice(1)
      aa_kpi_box(
        title = "Top Inflation Driver",
        value_ui = div(style = "font-size:18px; font-weight:700;",
                       aa_short_series(aa_top$series)),
        subtitle = sprintf("%+.2f pp", aa_top$contribution_pp),
        showcase = aa_series_icon(aa_top$series),
        bg = "#fdf0f0", fg = "#c0392b"
      )
    })
    
    output$aa_kpi_drag <- shiny::renderUI({
      aa_df <- latest_lvl1
      shiny::validate(shiny::need(nrow(aa_df) > 0, "No data"))
      aa_drag <- aa_df %>% dplyr::arrange(contribution_pp) %>% dplyr::slice(1)
      aa_kpi_box(
        title = "Top Deflation Driver",
        value_ui = div(style = "font-size:18px; font-weight:700;",
                       aa_short_series(aa_drag$series)),
        subtitle = sprintf("%+.2f pp", aa_drag$contribution_pp),
        showcase = aa_series_icon(aa_drag$series),
        bg = "#f0f4ff", fg = "#1565c0"
      )
    })
    
    output$aa_dashboard_table <- reactable::renderReactable({
      req(nrow(dashboard_table) > 0)
      
      tbl <- as.data.frame(dashboard_table)
      n   <- nrow(tbl)
      
      html_vals <- character(n)
      for (i in seq_len(n)) {
        vals <- tryCatch(as.numeric(unlist(tbl$trend_12m[[i]])), error = function(e) NULL)
        if (is.null(vals) || length(vals) < 2) {
          html_vals[i] <- "—"
        } else {
          vals <- vals[is.finite(vals)]
          if (length(vals) < 2) {
            html_vals[i] <- "—"
          } else {
            mean_val <- mean(vals, na.rm = TRUE)
            val_min  <- min(vals, na.rm = TRUE)
            val_max  <- max(vals, na.rm = TRUE)
            mean_pct <- if (val_max == val_min) 50 else
              round((1 - (mean_val - val_min) / (val_max - val_min)) * 30)
            
            spk <- sparkline::spk_chr(
              vals,
              type             = "line",
              lineColor        = "#1a1612",
              fillColor        = "transparent",
              lineWidth        = 1.5,
              spotColor        = "#c0392b",
              minSpotColor     = "#c0392b",
              maxSpotColor     = "#c0392b",
              normalRangeMin   = quantile(vals, 0.25, na.rm = TRUE),
              normalRangeMax   = quantile(vals, 0.75, na.rm = TRUE),
              normalRangeColor = "rgba(192,57,43,0.15)",
              drawNormalOnTop  = FALSE,
              width            = 500, 
              height           = 30
            )
            
            html_vals[i] <- paste0(
              '<div style="position:relative; display:inline-flex; align-items:center; gap:4px;">',
              '<div style="position:relative; display:inline-block;">',
              spk,
              '<div style="position:absolute; top:', mean_pct,
              'px; left:0; right:0; border-top:1.5px dotted #c0392b; pointer-events:none;"></div>',
              '</div>',
              '<span style="font-family:\'IBM Plex Mono\',monospace; font-size:10px; color:#7a6f68; white-space:nowrap;">',
              sprintf("%.2f", mean_val),
              '</span>',
              '</div>'
            )
          }
        }
      }
      
      tbl$trend_12m_html <- html_vals
      tbl$trend_12m      <- NULL
      
      tbl <- tbl[, c("series", "cpi", "trend_12m_html", "yoy_pct", "weight_pct", "contribution_pp")]
      
      reactable::reactable(
        tbl,
        defaultPageSize = 10, pagination = FALSE,
        compact = TRUE, highlight = TRUE, striped = FALSE,
        defaultSorted = "contribution_pp", defaultSortOrder = "desc",
        defaultColDef = reactable::colDef(
          align = "center",
          headerStyle = list(
            fontFamily = "'IBM Plex Mono', monospace",
            fontSize = "10px", textTransform = "uppercase",
            letterSpacing = "0.08em", backgroundColor = "#1a1612",
            color = "#f5f0e8"
          )
        ),
        columns = list(
          series = reactable::colDef(
            name = "Division", align = "left", width = 280,
            cell = function(value) {
              htmltools::tags$span(style = "font-weight:600; color:#1a1612;", value)
            }
          ),
          cpi = reactable::colDef(
            name = "Latest CPI",
            format = reactable::colFormat(digits = 2), minWidth = 70
          ),
          trend_12m_html = reactable::colDef(
            name = "12M Trend", minWidth = 180, sortable = FALSE,
            html = TRUE,
            cell = function(value) value
          ),
          yoy_pct = reactable::colDef(
            name = "YoY %", minWidth = 65,
            cell = function(value) {
              aa_color <- if (is.na(value)) "#7a6f68"
              else if (value > 0) "#c0392b"
              else if (value < 0) "#1565c0"
              else "#3d3530"
              div(style = paste0("color:", aa_color, "; font-weight:600;"),
                  ifelse(is.na(value), "NA", sprintf("%.2f", value)))
            }
          ),
          weight_pct = reactable::colDef(
            name = "Weight (%)",
            format = reactable::colFormat(digits = 2), minWidth = 70
          ),
          contribution_pp = reactable::colDef(
            name = "Contribution (pp)", minWidth = 120, align = "right",
            cell = function(value) {
              aa_bar_w <- if (is.na(value)) 0 else min(abs(value) * 120, 120)
              aa_color <- if (is.na(value)) "#d0c8be"
              else if (value > 0) "#c0392b"
              else if (value < 0) "#1565c0"
              else "#d0c8be"
              div(style = "display:flex; align-items:center; justify-content:flex-end; gap:8px;",
                  div(style = paste0("height:10px; width:", aa_bar_w,
                                     "px; background:", aa_color, "; border-radius:0; opacity:0.85;")),
                  div(style = "min-width:48px; text-align:right; font-family:'IBM Plex Mono',monospace;",
                      ifelse(is.na(value), "NA", sprintf("%.2f", value))))
            }
          )
        ),
        theme = reactable::reactableTheme(
          borderColor = "#d0c8be", highlightColor = "#ede8df",
          cellPadding = "8px 10px",
          headerStyle = list(backgroundColor = "#1a1612", color = "#f5f0e8")
        )
      )
    })
    
    output$aa_total_rows <- shiny::renderUI({
      shiny::req(cpi_h_display())
      as.character(nrow(cpi_h_display()))
    })
    
    output$aa_total_divisions <- shiny::renderUI({
      shiny::req(cpi_h_display())
      as.character(
        cpi_h_display() %>%
          dplyr::filter(level_label == "Major Category") %>%
          dplyr::summarise(n = dplyr::n_distinct(division, na.rm = TRUE)) %>%
          dplyr::pull(n)
      )
    })
    
    output$aa_last_updated <- shiny::renderUI({
      shiny::req(cpi_h_display())
      as.character(format(max(cpi_h_display()$As_of, na.rm = TRUE), "%b %Y"))
    })
    
    output$aa_cpi_table <- DT::renderDT({
      dt_data <- cpi_h_display() %>%
        dplyr::select(
          Level = level_label, Division = major_group, Group = group,
          Class = class_name, Series = display_name,
          `CPI Value` = cpi_rounded, `As of` = As_of_lab,
          Trend, Trend_export
        )
      DT::datatable(
        dt_data, class = "cell-border stripe hover",
        filter = "top", extensions = c("Buttons", "ColReorder", "Responsive"),
        width = "100%",
        options = list(
          pageLength = 10,
          dom = "Blfrtip",
          buttons = list(
            list(extend = "collection", text = "Export",
                 buttons = list(
                   list(extend = "copy",  exportOptions = list(columns = c(0:7))),
                   list(extend = "csv",   exportOptions = list(columns = c(0:7))),
                   list(extend = "excel", exportOptions = list(columns = c(0:7)))
                 )
            ),
            list(extend = "colvis", text = "Columns")
          ),
          scrollX = FALSE, autoWidth = TRUE, responsive = TRUE,
          columnDefs = list(
            list(targets = 7, render = trend_js, orderable = FALSE, searchable = FALSE),
            list(targets = 8, visible = FALSE)
          )
        ),
        selection = "single", rownames = FALSE, escape = FALSE
      )
    })
    
    shiny::observe({
      date_choices <- sort(unique(cpi_data$date))
      shinyWidgets::updateSliderTextInput(session, "aa_date_range",
                                          choices = date_choices, selected = range(date_choices))
    })
    
    filtered_data <- shiny::reactive({
      req(input$aa_date_range)
      cpi_data %>%
        dplyr::filter(
          date >= min(as.Date(input$aa_date_range)),
          date <= max(as.Date(input$aa_date_range))
        )
    })
    
    safe_division <- shiny::reactive({
      if (is.null(input$aa_division) || identical(input$aa_division, "")) NULL
      else input$aa_division
    })
    
    safe_group <- shiny::reactive({
      if (is.null(input$aa_group_name) || identical(input$aa_group_name, "")) NULL
      else input$aa_group_name
    })
    
    aa_max_select <- shiny::reactive({
      req(input$aa_eda_tabs)
      switch(input$aa_eda_tabs, "Trend" = 6, "Seasonality" = 2, "Autocorrelation" = 3, 6)
    })
    
    output$aa_division_ui <- shiny::renderUI({
      req(input$aa_plot_level)
      lvl <- as.numeric(input$aa_plot_level)
      if (!lvl %in% c(2, 3)) return(NULL)
      choices <- filtered_data() %>%
        dplyr::filter(level == lvl, !is.na(division)) %>%
        dplyr::distinct(division) %>% dplyr::arrange(division) %>% dplyr::pull(division)
      if (length(choices) == 0) return(NULL)
      selectInput(ns("aa_division"), "Division", choices = choices, selected = choices[1])
    })
    
    output$aa_group_ui <- shiny::renderUI({
      req(input$aa_plot_level)
      if (as.numeric(input$aa_plot_level) != 3) return(NULL)
      req(input$aa_division)
      groups <- filtered_data() %>%
        dplyr::filter(level == 3, !is.na(group), division == input$aa_division) %>%
        dplyr::distinct(group) %>% dplyr::arrange(group) %>% dplyr::pull(group)
      if (length(groups) == 0) return(NULL)
      selectInput(ns("aa_group_name"), "Group", choices = groups, selected = groups[1])
    })
    
    output$aa_series_select_ui <- shiny::renderUI({
      req(input$aa_plot_level)
      lvl <- as.numeric(input$aa_plot_level)
      df  <- filtered_data()
      if (lvl == 0) {
        df <- df %>% dplyr::filter(level == 0)
      } else if (lvl == 1) {
        df <- df %>% dplyr::filter(level == 1)
      } else if (lvl == 2) {
        req(input$aa_division)
        df <- df %>% dplyr::filter(level == 2, division == input$aa_division)
      } else if (lvl == 3) {
        req(input$aa_division, input$aa_group_name)
        df <- df %>% dplyr::filter(level == 3, division == input$aa_division,
                                   group == input$aa_group_name)
      }
      choices <- df %>%
        dplyr::filter(!is.na(series)) %>%
        dplyr::distinct(series) %>% dplyr::arrange(series) %>% dplyr::pull(series)
      max_s <- aa_max_select()
      selectizeInput(ns("aa_series_select"),
                     label = paste0("Series (max ", max_s, ")"),
                     choices = choices,
                     selected = head(choices, min(2, length(choices))),
                     multiple = TRUE,
                     options = list(
                       maxItems = max_s,
                       plugins = list("remove_button"),
                       placeholder = paste0("Select up to ", max_s, " series")
                     )
      )
    })
    
    output$aa_trend_plot <- plotly::renderPlotly({
      req(input$aa_facet_cols, input$aa_colour_by_year)
      shiny::validate(shiny::need(
        !is.null(input$aa_series_select) && length(input$aa_series_select) > 0,
        "Please select at least one series."))
      plot_cpi_time_series(
        data = filtered_data(), plot_level = as.numeric(input$aa_plot_level),
        category = safe_division(), group_name = safe_group(),
        series_select = input$aa_series_select,
        use_year_colour = isTRUE(input$aa_colour_by_year == "year"),
        facet_ncol = as.numeric(input$aa_facet_cols),
        smooth = isTRUE(input$aa_smooth), interactive = TRUE
      )
    })
    
    output$aa_seasonality_cards_ui <- shiny::renderUI({
      selected_series <- input$aa_series_select
      shiny::validate(shiny::need(
        !is.null(selected_series) && length(selected_series) > 0,
        "Please select at least one series."))
      n <- length(selected_series)
      col_width <- if (n == 1) 12 else 6
      plot_output_list <- lapply(seq_along(selected_series), function(i) {
        column(width = col_width,
               div(class = "eda-season-card",
                   div(class = "eda-season-card-title", selected_series[i]),
                   plotly::plotlyOutput(
                     outputId = ns(paste0("aa_season_plot_", i)),
                     height = "550px"
                   )
               )
        )
      })
      do.call(fluidRow, plot_output_list)
    })
    
    output$aa_acf_plot <- plotly::renderPlotly({
      req(input$aa_lags)
      shiny::validate(shiny::need(
        !is.null(input$aa_series_select) && length(input$aa_series_select) > 0,
        "Please select at least one series."))
      plot_cpi_acf(
        data = filtered_data(), plot_level = as.numeric(input$aa_plot_level),
        category = safe_division(), group_name = safe_group(),
        series_select = input$aa_series_select, lags = input$aa_lags,
        interactive = TRUE,
        show_white_noise_bars = isTRUE(input$aa_acf_white_noise)
      )
    })
    
    shiny::observe({
      req(input$aa_series_select, input$aa_season_geom, input$aa_season_feature)
      selected_series <- input$aa_series_select
      for (i in seq_along(selected_series)) {
        local({
          idx         <- i
          series_name <- selected_series[idx]
          output[[paste0("aa_season_plot_", idx)]] <- plotly::renderPlotly({
            plots <- plot_cpi_seasonal(
              data = filtered_data(), plot_level = as.numeric(input$aa_plot_level),
              category = safe_division(), group_name = safe_group(),
              series_select = series_name, geom = input$aa_season_geom,
              feature_set = input$aa_season_feature,
              interactive = TRUE, return_plots = TRUE
            )
            plots[[1]]
          })
        })
      }
    })
    
    shiny::observeEvent(input$aa_reset, {
      updateRadioButtons(session, "aa_plot_level", selected = 1)
      updateSelectInput(session, "aa_colour_by_year", selected = "none")
      updateSelectInput(session, "aa_facet_cols", selected = "1")
      updateSelectInput(session, "aa_season_geom", selected = "boxplot")
      updateCheckboxGroupInput(session, "aa_season_feature", selected = "month.lbl")
      updateCheckboxInput(session, "aa_smooth", value = FALSE)
      updateCheckboxInput(session, "aa_acf_white_noise", value = TRUE)
      updateNumericInput(session, "aa_lags", value = 24)
      date_choices <- sort(unique(cpi_data$date))
      shinyWidgets::updateSliderTextInput(session, "aa_date_range",
                                          selected = range(date_choices))
    })
    
    shiny::observeEvent(input$aa_series_select, {
      req(input$aa_eda_tabs)
      max_s <- aa_max_select()
      if (!is.null(input$aa_series_select) && length(input$aa_series_select) > max_s) {
        updateSelectizeInput(session, "aa_series_select",
                             selected = input$aa_series_select[1:max_s])
        shiny::showNotification(
          paste("Please select up to", max_s, "series only."), type = "warning")
      }
    }, ignoreInit = TRUE)
    
  })
}