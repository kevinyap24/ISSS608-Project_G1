pacman::p_load(plotly, tidyverse)

forecast_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=DM+Mono:wght@400;500&display=swap');
      * { box-sizing: border-box; }
      body { font-family: 'DM Sans', sans-serif; background: #f8fafc; color: #1e293b; margin: 0; padding: 0; }
      .dashboard-wrapper { max-width: 1100px; margin: 0 auto; padding: 32px 24px; }
      .dashboard-header { margin-bottom: 24px; }
      .dashboard-header h1 { font-size: 22px; font-weight: 600; color: #0f172a; margin: 0 0 4px 0; letter-spacing: -0.3px; }
      .dashboard-header p { font-size: 13px; color: #94a3b8; margin: 0; }
      .controls-row { display: flex; align-items: center; gap: 12px; margin-bottom: 16px; }
      .control-group { display: flex; flex-direction: column; gap: 4px; }
      .control-label { font-size: 11px; font-weight: 500; text-transform: uppercase; letter-spacing: 0.6px; color: #94a3b8; }
      .control-group select { font-family: 'DM Sans', sans-serif; font-size: 13px; color: #1e293b; background: #ffffff;
        border: 1px solid #e2e8f0; border-radius: 8px; padding: 8px 32px 8px 12px; appearance: none; -webkit-appearance: none;
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 24 24' fill='none' stroke='%2394a3b8' stroke-width='2'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E\");
        background-repeat: no-repeat; background-position: right 10px center; cursor: pointer; min-width: 160px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04); transition: border-color 0.15s, box-shadow 0.15s; }
      .control-group select:focus { outline: none; border-color: #3B82F6; box-shadow: 0 0 0 3px rgba(59,130,246,0.12); }
      #category_select { min-width: 320px; }
      .chart-card { background: #ffffff; border-radius: 12px; border: 1px solid #e2e8f0; padding: 20px 20px 8px 20px; box-shadow: 0 1px 4px rgba(0,0,0,0.05); }
      .chart-title { font-size: 14px; font-weight: 600; color: #0f172a; margin: 0 0 2px 0; }
      .chart-subtitle { font-size: 12px; color: #94a3b8; margin: 0 0 16px 0; font-family: 'DM Mono', monospace; }
      .plotly .modebar { top: 8px !important; right: 8px !important; }
    "))),
    
    div(class = "dashboard-wrapper",
        div(class = "dashboard-header",
            tags$h1("Singapore CPI Forecast Dashboard"),
            tags$p("Hierarchical forecasts aggregated bottom-up · Jan 2020 – Dec 2026")
        ),
        div(class = "controls-row",
            div(class = "control-group",
                div(class = "control-label", "Hierarchy Level"),
                selectInput(ns("level_select"), label = NULL,
                            choices = NULL, width = "160px")
            ),
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

forecast_server <- function(id, cpi_data, parent_lookup, reconciled_forecasts) {
  moduleServer(id, function(input, output, session) {
    
    # Prepare plot data
    all_series <- parent_lookup %>% pull(series) %>% unique()
    
    actuals_all <- cpi_data %>%
      filter(series %in% all_series) %>%
      select(series, date, cpi) %>%
      mutate(.key = "actual")
    
    forecast_all <- reconciled_forecasts %>%
      filter(series %in% all_series) %>%
      select(series, date, cpi = .value,
             lo90, hi90, lo95, hi95, lo99, hi99) %>%
      mutate(.key = "prediction")
    
    plot_data  <- bind_rows(actuals_all, forecast_all)
    all_levels <- parent_lookup %>% pull(Level) %>% unique() %>% sort()
    
    # Populate level dropdown on load
    observe({
      updateSelectInput(session, "level_select",
                        choices = setNames(all_levels, paste("Level", all_levels)))
    })
    
    # Update category dropdown when level changes
    observeEvent(input$level_select, {
      lvl  <- as.integer(input$level_select)
      cats <- parent_lookup %>%
        filter(Level == lvl) %>%
        pull(series) %>% unique() %>% sort()
      updateSelectInput(session, "category_select", choices = cats)
    })
    
    # Chart header
    output$chart_header <- renderUI({
      req(input$category_select)
      tagList(
        div(class = "chart-title", input$category_select),
        div(class = "chart-subtitle",
            paste0("Level ", input$level_select, "  ·  CPI Index (Base: 2024 = 100)"))
      )
    })
    
    # Plot
    output$cpi_plot <- renderPlotly({
      req(input$category_select)
      s           <- input$category_select
      actual_data <- plot_data %>% filter(series == s, .key == "actual")
      fc_data     <- plot_data %>% filter(series == s, .key == "prediction")
      
      p <- plot_ly(height = 480)
      
      if (nrow(fc_data) > 0) {
        p <- p %>%
          add_ribbons(
            x         = fc_data$date,
            ymin      = fc_data$lo99,
            ymax      = fc_data$hi99,
            name      = "99% CI",
            line      = list(color = "transparent"),
            fillcolor = "rgba(249, 115, 22, 0.10)",
            hoverinfo = "skip"
          ) %>%
          add_ribbons(
            x         = fc_data$date,
            ymin      = fc_data$lo95,
            ymax      = fc_data$hi95,
            name      = "95% CI",
            line      = list(color = "transparent"),
            fillcolor = "rgba(249, 115, 22, 0.15)",
            hoverinfo = "skip"
          ) %>%
          add_ribbons(
            x         = fc_data$date,
            ymin      = fc_data$lo90,
            ymax      = fc_data$hi90,
            name      = "90% CI",
            line      = list(color = "transparent"),
            fillcolor = "rgba(249, 115, 22, 0.22)",
            hoverinfo = "skip"
          ) %>%
          add_trace(
            x             = fc_data$date,
            y             = fc_data$cpi,
            type          = "scatter", mode = "lines",
            name          = "Forecast",
            line          = list(color = "#F97316", width = 2.5, dash = "dot"),
            hovertemplate = "<b>%{x|%b %Y}</b><br>CPI: %{y:.2f}<extra>Forecast</extra>"
          )
      }
      
      p %>%
        add_trace(
          x             = actual_data$date,
          y             = actual_data$cpi,
          type          = "scatter", mode = "lines",
          name          = "Actual",
          line          = list(color = "#3B82F6", width = 2.5),
          hovertemplate = "<b>%{x|%b %Y}</b><br>CPI: %{y:.2f}<extra>Actual</extra>"
        ) %>%
        layout(
          xaxis = list(
            title = "", showgrid = TRUE, gridcolor = "#f1f5f9", zeroline = FALSE,
            tickfont = list(size = 11, color = "#64748b", family = "DM Sans"),
            rangeslider = list(visible = TRUE, thickness = 0.07,
                               bgcolor = "#f8fafc", bordercolor = "#e2e8f0", borderwidth = 1),
            rangeselector = list(
              buttons = list(
                list(count = 6,  label = "6M",  step = "month", stepmode = "backward"),
                list(count = 12, label = "1Y",  step = "month", stepmode = "backward"),
                list(count = 24, label = "2Y",  step = "month", stepmode = "backward"),
                list(step = "all", label = "All")
              ),
              bgcolor = "#f8fafc", activecolor = "#3B82F6",
              bordercolor = "#e2e8f0", font = list(size = 11, family = "DM Sans")
            )
          ),
          yaxis = list(
            title    = list(text = "CPI Value",
                            font = list(size = 11, color = "#94a3b8", family = "DM Sans")),
            showgrid = TRUE, gridcolor = "#f1f5f9", zeroline = FALSE,
            tickfont = list(size = 11, color = "#64748b", family = "DM Sans")
          ),
          legend = list(orientation = "h", x = 0, xanchor = "left",
                        y = -0.32, yanchor = "top",
                        font = list(size = 12, family = "DM Sans"),
                        bgcolor = "rgba(0,0,0,0)"),
          plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
          margin = list(t = 10, r = 10, b = 60, l = 55),
          hoverlabel = list(bgcolor = "#1e293b",
                            font = list(size = 12, color = "#ffffff", family = "DM Sans"),
                            bordercolor = "#1e293b")
        ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = list("lasso2d", "select2d", "autoScale2d"))
    })
  })
}

# ---- paste your forecast_ui() and forecast_server() code above ----

# TEMPORARY TEST - delete this before final deployment
library(shiny)
library(tidyverse)
library(plotly)

cpi_data             <- read_rds("../../data/cpi_h.rds")
parent_lookup        <- read_rds("../../data/parent_lookup.rds")
reconciled_forecasts <- read_rds("../../data/reconciled_forecasts.rds")

ui     <- fluidPage(forecast_ui("forecast"))
server <- function(input, output, session) {
  forecast_server("forecast", cpi_data, parent_lookup, reconciled_forecasts)
}

shinyApp(ui, server)
