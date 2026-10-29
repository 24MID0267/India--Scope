# R/mod_rankings.R
# Regional Rankings and Comparison Leaderboard Module

library(shiny)
library(dplyr)

mod_rankings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "rankings-container bg-white p-3 rounded shadow-sm border mb-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$h6(class = "fw-bold text-secondary m-0", icon("trophy"), " Regional Leaderboard"),
        uiOutput(ns("rankings_badge_ui"))
      ),
      div(
        class = "rankings-table-wrapper table-responsive",
        style = "max-height: 400px; overflow-y: auto;",
        uiOutput(ns("rankings_list_ui"))
      )
    )
  )
}

mod_rankings_server <- function(id, selected_geo, current_category, current_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$rankings_badge_ui <- renderUI({
      level <- selected_geo$level
      st <- selected_geo$state
      label <- if (level == "India") "All 35 States & UTs" else paste(st, "Districts")
      tags$span(class = "badge bg-light text-dark border", label)
    })
    
    output$rankings_list_ui <- renderUI({
      level <- selected_geo$level
      st <- selected_geo$state
      dt <- selected_geo$district
      ind_id <- current_indicator()
      ind_meta <- get_indicator_meta(ind_id)
      ind_unit <- if (!is.null(ind_meta)) ind_meta$unit else ""
      ind_dir  <- if (!is.null(ind_meta)) ind_meta$direction else "higher_is_better"
      
      df <- NULL
      if (level == "India") {
        states_df <- get_states_data() %>% filter(state_name != "India")
        if (ind_id %in% names(states_df)) {
          df <- states_df %>%
            select(name = state_name, val = !!sym(ind_id)) %>%
            filter(!is.na(val))
        }
      } else {
        req(st)
        dist_df <- get_districts_data(st)
        if (ind_id %in% names(dist_df)) {
          df <- dist_df %>%
            select(name = district_name, val = !!sym(ind_id)) %>%
            filter(!is.na(val))
        }
      }
      
      if (is.null(df) || nrow(df) == 0) {
        return(
          div(
            class = "text-center text-muted p-4",
            tags$i(class = "fa fa-info-circle fa-2x mb-2 text-secondary"),
            tags$p("District rankings unavailable for this indicator (published at state level only).")
          )
        )
      }
      
      # Sort according to indicator direction
      df <- if (ind_dir == "lower_is_better") {
        df %>% arrange(val)
      } else {
        df %>% arrange(desc(val))
      }
      
      df$rank <- seq_len(nrow(df))
      max_val <- max(df$val, na.rm = TRUE)
      
      table_rows <- lapply(seq_len(nrow(df)), function(i) {
        item <- df[i, ]
        is_selected <- if (level == "India") {
          !is.null(st) && (tolower(item$name) == tolower(st))
        } else {
          !is.null(dt) && (tolower(item$name) == tolower(dt))
        }
        
        pct_width <- if (max_val > 0) min(100, round((item$val / max_val) * 100)) else 0
        
        tags$tr(
          class = if (is_selected) "table-warning fw-bold" else "",
          style = "cursor: pointer;",
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("ranking_click"),
            item$name
          ),
          tags$td(
            class = "text-center text-muted small",
            style = "width: 40px;",
            if (item$rank == 1) tags$span(class = "badge bg-warning text-dark", "1")
            else if (item$rank == 2) tags$span(class = "badge bg-secondary text-white", "2")
            else if (item$rank == 3) tags$span(class = "badge bg-danger text-white", "3")
            else item$rank
          ),
          tags$td(
            class = "text-truncate",
            style = "max-width: 150px;",
            title = item$name,
            item$name
          ),
          tags$td(
            class = "text-end fw-bold",
            format_indicator_value(item$val, ind_unit)
          ),
          tags$td(
            style = "width: 70px; vertical-align: middle;",
            div(
              class = "progress",
              style = "height: 6px;",
              div(
                class = "progress-bar bg-info",
                role = "progressbar",
                style = paste0("width: ", pct_width, "%;")
              )
            )
          )
        )
      })
      
      tags$table(
        class = "table table-hover table-sm align-middle mb-0",
        tags$thead(
          class = "table-light small text-secondary",
          tags$tr(
            tags$th(class = "text-center", "#"),
            tags$th("Region"),
            tags$th(class = "text-end", "Value"),
            tags$th("Share")
          )
        ),
        tags$tbody(table_rows)
      )
    })
    
    # Handle click on ranking row to navigate
    observeEvent(input$ranking_click, {
      req(input$ranking_click)
      clicked_name <- input$ranking_click
      if (selected_geo$level == "India") {
        selected_geo$level <- "State"
        selected_geo$state <- clicked_name
        selected_geo$district <- NULL
      } else {
        selected_geo$level <- "District"
        selected_geo$district <- clicked_name
      }
    })
  })
}
