pacman::p_load(plotly, tidyverse, shiny)

filter    <- dplyr::filter
select    <- dplyr::select
mutate    <- dplyr::mutate
pull      <- dplyr::pull
bind_rows <- dplyr::bind_rows

observe      <- shiny::observe
observeEvent <- shiny::observeEvent
reactive     <- shiny::reactive
renderUI     <- shiny::renderUI

# ==============================================================================
# UI
# ==============================================================================
forecast_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:ital,wght@0,300;0,500;0,700;1,300&display=swap');

      * { box-sizing: border-box; }

      body {
        font-family: 'IBM Plex Sans', sans-serif;
        background: #f5f0e8;
        color: #1a1612;
        margin: 0; padding: 0;
      }

      .dashboard-wrapper {
        max-width: 1100px;
        margin: 0 auto;
        padding: 32px 24px;
      }

      .dashboard-header { margin-bottom: 24px; }

      .dashboard-header h1 {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 22px;
        font-weight: 700;
        color: #1a1612;
        margin: 0 0 4px 0;
        letter-spacing: -0.3px;
      }

      .dashboard-header p {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 12px;
        color: #7a6f68;
        margin: 0;
        letter-spacing: 0.04em;
      }

      .controls-row {
        display: flex;
        align-items: center;
        gap: 12px;
        margin-bottom: 16px;
        flex-wrap: wrap;
      }

      .control-group { display: flex; flex-direction: column; gap: 4px; }

      .control-label {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.12em;
        color: #7a6f68;
      }

      .control-group select {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 13px;
        font-weight: 300;
        color: #1a1612;
        background: #ede8df;
        border: 1px solid #d0c8be;
        border-radius: 0;
        padding: 8px 32px 8px 12px;
        appearance: none;
        -webkit-appearance: none;
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 24 24' fill='none' stroke='%237a6f68' stroke-width='2'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E\");
        background-repeat: no-repeat;
        background-position: right 10px center;
        cursor: pointer;
        min-width: 160px;
        transition: border-color 0.15s;
      }

      .control-group select:focus {
        outline: none;
        border-color: #c0392b;
      }

      .chart-card {
        background: #ede8df;
        border: 1px solid #d0c8be;
        border-left: 4px solid #c0392b;
        border-radius: 0;
        padding: 20px 20px 8px 20px;
      }

      .chart-title {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 14px;
        font-weight: 700;
        color: #1a1612;
        margin: 0 0 2px 0;
      }

      .chart-subtitle {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 11px;
        color: #7a6f68;
        margin: 0 0 16px 0;
        letter-spacing: 0.04em;
      }

      .plotly .modebar { top: 8px !important; right: 8px !important; }
    "))),
    
    div(class = "dashboard-wrapper",
        div(class = "dashboard-header",
            tags$h1("Singapore CPI Forecast Dashboard"),
            tags$p("Hierarchical forecasts aggregated bottom-up · Jan 2020 – Dec 2026")
        ),
        div(class = "controls-row",
            
            # -- Forecast Mode --
            div(class = "control-group",
                div(class = "control-label", "Forecast Mode"),
                selectInput(ns("forecast_mode"), label = NULL,
                            choices = c("Auto (Best Model by RMSE)" = "auto",
                                        "Manual (Select Model)"     = "manual"),
                            width = "220px")
            ),
            
            # -- Model selector: only shown in manual mode --
            conditionalPanel(
              condition = paste0("input['", ns("forecast_mode"), "'] == 'manual'"),
              div(class = "control-group",
                  div(class = "control-label", "Model"),
                  selectInput(ns("model_select"), label = NULL,
                              choices = c("ETS"             = "ets",
                                          "Auto-ARIMA"      = "arima",
                                          "ARIMA + XGBoost" = "boost",
                                          "Prophet"         = "prophet"),
                              width = "200px")
              )
            ),
            
            # -- Hierarchy Level --
            div(class = "control-group",
                div(class = "control-label", "Hierarchy Level"),
                selectInput(ns("level_select"), label = NULL,
                            choices = NULL, width = "160px")
            ),
            
            # -- Category --
            div(class = "control-group",
                div(class = "control-label", "Category"),
                selectInput(ns("category_select"), label = NULL,
                            choices = NULL, width = "380px")
            )
        ),
        
        div(class = "chart-card",
            uiOutput(ns("chart_header")),
            plotlyOutput(ns("cpi_plot"), height = "480px")
        )
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================
forecast_server <- function(id, cpi_data, parent_lookup,
                            reconciled_forecasts,
                            fc_ets, fc_arima, fc_boost, fc_prophet) {
  moduleServer(id, function(input, output, session) {
    
    all_series <- dplyr::pull(parent_lookup, series) %>% unique()
    all_levels <- dplyr::pull(parent_lookup, Level)  %>% unique() %>% sort()
    
    # Actuals
    actuals_all <- cpi_data %>%
      dplyr::filter(series %in% all_series) %>%
      dplyr::select(series, date, cpi) %>%
      dplyr::mutate(.key = "actual")
    
    # Reactive: select forecast table based on mode and model
    selected_forecast <- shiny::reactive({
      if (input$forecast_mode == "auto") {
        reconciled_forecasts
      } else {
        switch(input$model_select,
               "ets"     = fc_ets,
               "arima"   = fc_arima,
               "boost"   = fc_boost,
               "prophet" = fc_prophet)
      }
    })
    
    # Reactive: build combined plot data
    plot_data <- shiny::reactive({
      forecast_all <- selected_forecast() %>%
        dplyr::filter(series %in% all_series) %>%
        dplyr::select(series, date, cpi = .value,
                      lo90, hi90, lo95, hi95, lo99, hi99) %>%
        dplyr::mutate(.key = "prediction")
      dplyr::bind_rows(actuals_all, forecast_all)
    })
    
    # Populate level dropdown on load
    shiny::observe({
      updateSelectInput(session, "level_select",
                        choices = setNames(all_levels, paste("Level", all_levels)))
    })
    
    # Update category dropdown when level changes
    shiny::observeEvent(input$level_select, {
      lvl  <- as.integer(input$level_select)
      cats <- parent_lookup %>%
        dplyr::filter(Level == lvl) %>%
        dplyr::pull(series) %>% unique() %>% sort()
      updateSelectInput(session, "category_select", choices = cats)
    })
    
    # Chart header — shows selected model in subtitle
    output$chart_header <- shiny::renderUI({
      req(input$category_select)
      model_label <- if (input$forecast_mode == "auto") {
        "Auto — Best Model by RMSE"
      } else {
        switch(input$model_select,
               "ets"     = "ETS",
               "arima"   = "Auto-ARIMA",
               "boost"   = "ARIMA + XGBoost",
               "prophet" = "Prophet")
      }
      tagList(
        div(class = "chart-title", input$category_select),
        div(class = "chart-subtitle",
            paste0("Level ", input$level_select,
                   "  ·  CPI Index (Base: 2024 = 100)",
                   "  ·  ", model_label))
      )
    })
    
    # Plot
    output$cpi_plot <- plotly::renderPlotly({
      req(input$category_select)
      s           <- input$category_select
      actual_data <- plot_data() %>% dplyr::filter(series == s, .key == "actual")
      fc_data     <- plot_data() %>% dplyr::filter(series == s, .key == "prediction")
      
      p <- plot_ly(height = 480)
      
      if (nrow(fc_data) > 0) {
        p <- p %>%
          # CI ribbons — red-toned to match website palette
          add_ribbons(
            x         = fc_data$date,
            ymin      = fc_data$lo99,
            ymax      = fc_data$hi99,
            name      = "99% CI",
            line      = list(color = "transparent"),
            fillcolor = "rgba(192, 57, 43, 0.08)",
            hoverinfo = "skip"
          ) %>%
          add_ribbons(
            x         = fc_data$date,
            ymin      = fc_data$lo95,
            ymax      = fc_data$hi95,
            name      = "95% CI",
            line      = list(color = "transparent"),
            fillcolor = "rgba(192, 57, 43, 0.13)",
            hoverinfo = "skip"
          ) %>%
          add_ribbons(
            x         = fc_data$date,
            ymin      = fc_data$lo90,
            ymax      = fc_data$hi90,
            name      = "90% CI",
            line      = list(color = "transparent"),
            fillcolor = "rgba(192, 57, 43, 0.20)",
            hoverinfo = "skip"
          ) %>%
          # Forecast line — red (#c0392b)
          add_trace(
            x             = fc_data$date,
            y             = fc_data$cpi,
            type          = "scatter", mode = "lines",
            name          = "Forecast",
            line          = list(color = "#c0392b", width = 2.5, dash = "dot"),
            hovertemplate = "<b>%{x|%b %Y}</b><br>CPI: %{y:.2f}<extra>Forecast</extra>"
          )
      }
      
      p %>%
        # Actual line — dark (#1a1612)
        add_trace(
          x             = actual_data$date,
          y             = actual_data$cpi,
          type          = "scatter", mode = "lines",
          name          = "Actual",
          line          = list(color = "#1a1612", width = 2.5),
          hovertemplate = "<b>%{x|%b %Y}</b><br>CPI: %{y:.2f}<extra>Actual</extra>"
        ) %>%
        layout(
          xaxis = list(
            title       = "",
            showgrid    = TRUE, gridcolor = "#d8d2c8", zeroline = FALSE,
            tickfont    = list(size = 11, color = "#7a6f68", family = "IBM Plex Mono"),
            rangeslider = list(visible = TRUE, thickness = 0.07,
                               bgcolor = "#ede8df", bordercolor = "#d0c8be", borderwidth = 1),
            rangeselector = list(
              buttons = list(
                list(count = 6,  label = "6M",  step = "month", stepmode = "backward"),
                list(count = 12, label = "1Y",  step = "month", stepmode = "backward"),
                list(count = 24, label = "2Y",  step = "month", stepmode = "backward"),
                list(step = "all", label = "All")
              ),
              bgcolor = "#3d3530", 
              activecolor = "#c0392b",
              bordercolor = "#d0c8be", 
              font = list(size = 11, family = "IBM Plex Mono", color = "#f5f0e8")
            )
          ),
          yaxis = list(
            title    = list(text = "CPI Value",
                            font = list(size = 11, color = "#7a6f68", family = "IBM Plex Mono")),
            showgrid = TRUE, gridcolor = "#d8d2c8", zeroline = FALSE,
            tickfont = list(size = 11, color = "#7a6f68", family = "IBM Plex Mono")
          ),
          legend = list(
            orientation = "h", x = 0, xanchor = "left",
            y = -0.32, yanchor = "top",
            font    = list(size = 12, family = "IBM Plex Sans"),
            bgcolor = "rgba(0,0,0,0)"
          ),
          plot_bgcolor  = "#ede8df", paper_bgcolor = "#ede8df",
          margin        = list(t = 10, r = 10, b = 60, l = 55),
          hoverlabel    = list(bgcolor = "#1a1612",
                               font = list(size = 12, color = "#f5f0e8",
                                           family = "IBM Plex Sans"),
                               bordercolor = "#1a1612")
        ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = list("lasso2d", "select2d", "autoScale2d"))
    })
  })
}

# ==============================================================================
# TEMPORARY TEST — delete everything below before final deployment
# ==============================================================================
# library(shiny)
# library(tidyverse)
# library(plotly)
# 
# cpi_data             <- read_rds("../../data/cpi_h.rds")
# parent_lookup        <- read_rds("../../data/parent_lookup.rds")
# reconciled_forecasts <- read_rds("../../data/reconciled_forecasts.rds")
# fc_ets               <- read_rds("../../data/fc_ets.rds")
# fc_arima             <- read_rds("../../data/fc_arima.rds")
# fc_boost             <- read_rds("../../data/fc_boost.rds")
# fc_prophet           <- read_rds("../../data/fc_prophet.rds")
# 
# ui <- fluidPage(forecast_ui("forecast"))
# server <- function(input, output, session) {
#   forecast_server("forecast", cpi_data, parent_lookup,
#                   reconciled_forecasts, fc_ets, fc_arima, fc_boost, fc_prophet)
# }
# 
# shinyApp(ui, server)