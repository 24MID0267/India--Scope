# R/mod_filters.R
# Search, Category, and Indicator Selection Module

library(shiny)
library(dplyr)

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "filter-panel p-3 bg-white rounded shadow-sm border mb-3",
      # Search bar for States & Districts
      div(
        class = "mb-3",
        tags$label(class = "form-label fw-bold small text-secondary", icon("search"), " Quick Search (State / District)"),
        selectizeInput(
          ns("search_location"),
          label = NULL,
          choices = NULL,
          options = list(
            placeholder = "Type state or district name...",
            maxOptions = 15,
            closeAfterSelect = TRUE
          )
        )
      ),
      # Category Selector
      div(
        class = "mb-3",
        tags$label(class = "form-label fw-bold small text-secondary", icon("layer-group"), " Data Category"),
        radioButtons(
          ns("category_select"),
          label = NULL,
          choices = c(
            "Population & Literacy",
            "Healthcare",
            "Employment & Economy",
            "Education"
          ),
          selected = "Population & Literacy"
        )
      ),
      # Dynamic Indicator Selector
      div(
        class = "mb-2",
        tags$label(class = "form-label fw-bold small text-secondary", icon("chart-line"), " Primary Indicator"),
        selectInput(
          ns("indicator_select"),
          label = NULL,
          choices = NULL
        )
      ),
      # Geographic Level Badge
      div(
        class = "geo-level-badge mt-2 pt-2 border-top d-flex justify-content-between align-items-center",
        tags$span(class = "small text-muted", "Current Level:"),
        uiOutput(ns("current_level_pill"))
      )
    )
  )
}

mod_filters_server <- function(id, selected_geo, current_category, current_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize Search Locations (States & Districts)
    observe({
      states_df <- get_states_data() %>% filter(state_name != "India")
      districts_df <- get_districts_data()
      
      state_choices <- setNames(
        paste0("state:", states_df$state_name),
        paste0(states_df$state_name, " (State)")
      )
      
      district_choices <- setNames(
        paste0("district:", districts_df$state_name, "::", districts_df$district_name),
        paste0(districts_df$district_name, " (", districts_df$state_name, ")")
      )
      
      all_choices <- list(
        "States & UTs" = as.list(state_choices),
        "Districts" = as.list(district_choices)
      )
      
      updateSelectizeInput(
        session,
        "search_location",
        choices = all_choices,
        server = TRUE
      )
    })
    
    # Handle Search Selection
    observeEvent(input$search_location, {
      req(input$search_location)
      val <- input$search_location
      
      if (startsWith(val, "state:")) {
        st <- sub("^state:", "", val)
        selected_geo$level <- "State"
        selected_geo$state <- st
        selected_geo$district <- NULL
      } else if (startsWith(val, "district:")) {
        parts <- strsplit(sub("^district:", "", val), "::")[[1]]
        if (length(parts) == 2) {
          selected_geo$level <- "District"
          selected_geo$state <- parts[1]
          selected_geo$district <- parts[2]
        }
      }
      # Reset input selection
      updateSelectizeInput(session, "search_location", selected = character(0))
    })
    
    # Update Indicator choices when Category changes
    observeEvent(input$category_select, {
      cat_val <- input$category_select
      current_category(cat_val)
      
      meta_sub <- get_indicators_by_category(cat_val)
      if (nrow(meta_sub) > 0) {
        choices_vec <- setNames(meta_sub$indicator_id, meta_sub$indicator_name)
        updateSelectInput(
          session,
          "indicator_select",
          choices = choices_vec,
          selected = choices_vec[1]
        )
        current_indicator(choices_vec[1])
      }
    })
    
    # Update current_indicator reactive when user picks an indicator
    observeEvent(input$indicator_select, {
      req(input$indicator_select)
      current_indicator(input$indicator_select)
    })
    
    # Current level badge UI
    output$current_level_pill <- renderUI({
      level <- selected_geo$level
      badge_class <- switch(
        level,
        "India" = "bg-primary text-white",
        "State" = "bg-info text-dark",
        "District" = "bg-success text-white",
        "bg-secondary text-white"
      )
      tags$span(class = paste("badge rounded-pill", badge_class), level)
    })
  })
}
