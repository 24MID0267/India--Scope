# R/mod_charts.R
# Interactive Visualizations and Charts Module

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

mod_charts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "charts-container bg-white p-3 rounded shadow-sm border mb-3",
      tabsetPanel(
        id = ns("chart_tabs"),
        type = "pills",
        tabPanel(
          title = "Regional Comparison",
          div(class = "mt-3", plotlyOutput(ns("bar_comparison_chart"), height = "360px"))
        ),
        tabPanel(
          title = "Survey Comparison (NFHS-4 vs 5)",
          div(class = "mt-3", plotlyOutput(ns("survey_trend_chart"), height = "360px"))
        ),
        tabPanel(
          title = "Demographic Breakdown",
          div(class = "mt-3", plotlyOutput(ns("demographic_breakdown_chart"), height = "360px"))
        )
      )
    )
  )
}

mod_charts_server <- function(id, selected_geo, current_category, current_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Regional Comparison Bar Chart
    output$bar_comparison_chart <- renderPlotly({
      level <- selected_geo$level
      st <- selected_geo$state
      dt <- selected_geo$district
      ind_id <- current_indicator()
      ind_meta <- get_indicator_meta(ind_id)
      ind_name <- if (!is.null(ind_meta)) ind_meta$indicator_name else ind_id
      ind_unit <- if (!is.null(ind_meta)) ind_meta$unit else ""
      
      chart_data <- NULL
      x_var <- ""
      highlight_target <- ""
      chart_title <- ""
      
      if (level == "India") {
        # Compare all States
        states_df <- get_states_data() %>% filter(state_name != "India")
        if (ind_id %in% names(states_df)) {
          chart_data <- states_df %>%
            select(name = state_name, val = !!sym(ind_id)) %>%
            filter(!is.na(val)) %>%
            arrange(desc(val)) %>%
            slice(1:20) # Top 20 for readability
          x_var <- "name"
          chart_title <- paste("Top States/UTs by", ind_name)
        }
      } else {
        # State or District level: compare districts in selected state
        req(st)
        dist_df <- get_districts_data(st)
        if (ind_id %in% names(dist_df)) {
          chart_data <- dist_df %>%
            select(name = district_name, val = !!sym(ind_id)) %>%
            filter(!is.na(val)) %>%
            arrange(desc(val))
          x_var <- "name"
          highlight_target <- if (level == "District" && !is.null(dt)) dt else ""
          chart_title <- paste(st, "Districts -", ind_name)
        }
      }
      
      if (is.null(chart_data) || nrow(chart_data) == 0) {
        # Fallback empty chart with honest explanation
        return(
          plot_ly() %>%
            layout(
              title = list(text = paste("District-level data for", ind_name, "is unavailable (State level metric only)"), font = list(size = 14, color = "#6c757d")),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }
      
      # Set colors: highlight selected region in amber/orange, others in teal/blue
      chart_data$bar_color <- ifelse(
        tolower(chart_data$name) == tolower(highlight_target),
        "#d84315", # Vibrant contrast color for selected item
        "#00838f"  # Premium dark cyan/teal
      )
      
      # Order factors for descending bar chart
      chart_data$name <- factor(chart_data$name, levels = rev(chart_data$name))
      
      plot_ly(
        data = chart_data,
        x = ~val,
        y = ~name,
        type = "bar",
        orientation = "h",
        marker = list(color = ~bar_color),
        hoverinfo = "text",
        text = ~paste0("<b>", name, "</b><br/>", ind_name, ": ", format_indicator_value(val, ind_unit))
      ) %>%
        layout(
          title = list(text = chart_title, font = list(size = 14, color = "#263238")),
          xaxis = list(title = paste0(ind_name, " (", ind_unit, ")")),
          yaxis = list(title = ""),
          margin = list(l = 140, r = 20, t = 40, b = 40),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 2. Survey Trend Chart (NFHS-4 vs NFHS-5)
    output$survey_trend_chart <- renderPlotly({
      ind_id <- current_indicator()
      level <- selected_geo$level
      st <- selected_geo$state
      dt <- selected_geo$district
      
      # Check if this indicator has an NFHS-4 counterpart
      has_trend <- FALSE
      val_4 <- NA_real_
      val_5 <- NA_real_
      entity_name <- "India"
      
      if (str_ends(ind_id, "_nfhs5")) {
        base_name <- sub("_nfhs5$", "", ind_id)
        ind_4_id <- paste0(base_name, "_nfhs4")
        
        if (level == "India") {
          row <- get_national_data()
          val_5 <- row[[ind_id]]
          val_4 <- if (ind_4_id %in% names(row)) row[[ind_4_id]] else NA_real_
          entity_name <- "All India"
          has_trend <- !is.na(val_5) && !is.na(val_4)
        } else if (level == "State") {
          row <- get_state_data(st)
          val_5 <- row[[ind_id]]
          val_4 <- if (ind_4_id %in% names(row)) row[[ind_4_id]] else NA_real_
          entity_name <- st
          has_trend <- !is.na(val_5) && !is.na(val_4)
        } else if (level == "District") {
          row <- get_district_data(st, dt)
          if (!is.null(row)) {
            val_5 <- row[[ind_id]]
            val_4 <- if (ind_4_id %in% names(row)) row[[ind_4_id]] else NA_real_
            entity_name <- dt
            has_trend <- !is.na(val_5) && !is.na(val_4)
          }
        }
      }
      
      if (!has_trend) {
        return(
          plot_ly() %>%
            layout(
              title = list(
                text = "Two-period survey comparison is available for NFHS healthcare indicators.",
                font = list(size = 13, color = "#6c757d")
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }
      
      trend_df <- data.frame(
        Period = c("NFHS-4 (2015-16)", "NFHS-5 (2019-21)"),
        Value = c(val_4, val_5)
      )
      
      plot_ly(
        data = trend_df,
        x = ~Period,
        y = ~Value,
        type = "scatter",
        mode = "lines+markers+text",
        text = ~paste0(formatC(Value, format = "f", digits = 1), "%"),
        textposition = "top center",
        line = list(color = "#00897b", width = 3),
        marker = list(size = 10, color = "#004d40")
      ) %>%
        layout(
          title = list(
            text = paste(entity_name, ": Change from NFHS-4 to NFHS-5"),
            font = list(size = 14, color = "#263238")
          ),
          xaxis = list(title = ""),
          yaxis = list(title = "Percentage (%)", range = c(0, max(val_4, val_5) * 1.25)),
          margin = list(l = 50, r = 30, t = 40, b = 40),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 3. Demographic Breakdown Chart
    output$demographic_breakdown_chart <- renderPlotly({
      level <- selected_geo$level
      st <- selected_geo$state
      dt <- selected_geo$district
      
      row <- if (level == "India") {
        get_national_data()
      } else if (level == "State") {
        get_state_data(st)
      } else {
        get_district_data(st, dt)
      }
      
      entity_name <- if (level == "India") "India" else if (level == "State") st else dt
      
      if (is.null(row) || !"male_literacy_rate" %in% names(row)) {
        return(
          plot_ly() %>%
            layout(
              title = list(text = "Demographic breakdown data unavailable", font = list(size = 13, color = "#6c757d")),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }
      
      # Literacy gender comparison
      df <- data.frame(
        Category = c("Male Literacy", "Female Literacy", "Total Literacy"),
        Rate = c(row$male_literacy_rate, row$female_literacy_rate, row$effective_literacy_rate)
      )
      
      plot_ly(
        data = df,
        x = ~Category,
        y = ~Rate,
        type = "bar",
        marker = list(color = c("#1565c0", "#ad1457", "#2e7d32")),
        text = ~paste0(formatC(Rate, format = "f", digits = 1), "%"),
        textposition = "outside"
      ) %>%
        layout(
          title = list(
            text = paste(entity_name, ": Male vs Female Literacy (Census 2011)"),
            font = list(size = 14, color = "#263238")
          ),
          xaxis = list(title = ""),
          yaxis = list(title = "Literacy Rate (%)", range = c(0, 110)),
          margin = list(l = 50, r = 20, t = 40, b = 40),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
