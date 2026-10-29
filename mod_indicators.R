# R/mod_indicators.R
# KPI Metric Cards Module

library(shiny)
library(dplyr)

mod_indicators_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "kpi-container mb-3",
      uiOutput(ns("kpi_cards_grid"))
    )
  )
}

mod_indicators_server <- function(id, selected_geo, current_category, current_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$kpi_cards_grid <- renderUI({
      level <- selected_geo$level
      st <- selected_geo$state
      dt <- selected_geo$district
      cat_name <- current_category()
      active_ind <- current_indicator()
      
      # Retrieve national, state, and district rows
      national_row <- get_national_data()
      state_row <- if (!is.null(st)) get_state_data(st) else NULL
      district_row <- if (!is.null(st) && !is.null(dt)) get_district_data(st, dt) else NULL
      
      # Select indicators for the category
      cat_inds <- get_indicators_by_category(cat_name)
      if (nrow(cat_inds) == 0) return(NULL)
      
      # Ensure active indicator is displayed first
      active_row <- cat_inds %>% filter(indicator_id == active_ind)
      other_rows <- cat_inds %>% filter(indicator_id != active_ind)
      ordered_inds <- bind_rows(active_row, other_rows) %>% slice(1:min(4, n()))
      
      cards <- lapply(seq_len(nrow(ordered_inds)), function(i) {
        ind <- ordered_inds[i, ]
        ind_id <- ind$indicator_id
        ind_name <- ind$indicator_name
        ind_unit <- ind$unit
        ind_year <- ind$ref_period
        ind_src  <- ind$source_name
        is_primary <- (ind_id == active_ind)
        
        # Determine value and status based on geographic level
        val <- NA_real_
        parent_val <- NA_real_
        status_label <- "Verified Observation"
        status_class <- "badge-verified"
        geo_note <- ""
        
        if (level == "India") {
          val <- if (ind_id %in% names(national_row)) national_row[[ind_id]] else NA_real_
          geo_note <- "National Total / Average"
        } else if (level == "State") {
          val <- if (!is.null(state_row) && ind_id %in% names(state_row)) state_row[[ind_id]] else NA_real_
          parent_val <- if (ind_id %in% names(national_row)) national_row[[ind_id]] else NA_real_
          geo_note <- paste("State:", st)
        } else if (level == "District") {
          # Check if indicator exists at district level
          if (!is.null(district_row) && ind_id %in% names(district_row) && !is.na(district_row[[ind_id]])) {
            val <- district_row[[ind_id]]
            parent_val <- if (!is.null(state_row) && ind_id %in% names(state_row)) state_row[[ind_id]] else NA_real_
            geo_note <- paste("District:", dt)
          } else {
            # Indicator unavailable at district level - show state figure with explicit label
            val <- if (!is.null(state_row) && ind_id %in% names(state_row)) state_row[[ind_id]] else NA_real_
            parent_val <- if (ind_id %in% names(national_row)) national_row[[ind_id]] else NA_real_
            status_label <- "State-Level Reference"
            status_class <- "badge-reference"
            geo_note <- paste("Parent State (", st, ") - District data unavailable")
          }
        }
        
        # Comparison delta calculation
        comparison_ui <- NULL
        if (!is.na(val) && !is.na(parent_val) && is.numeric(val) && is.numeric(parent_val) && parent_val != 0) {
          delta <- val - parent_val
          bench_label <- if (level == "State") "vs India avg" else if (level == "District" && status_label == "Verified Observation") "vs State avg" else "vs India avg"
          delta_text <- if (ind_unit == "%") {
            paste0(ifelse(delta >= 0, "+", ""), formatC(delta, format = "f", digits = 1), " pp")
          } else {
            paste0(ifelse(delta >= 0, "+", ""), format(round(delta), big.mark = ","))
          }
          delta_class <- if (delta >= 0) "text-success" else "text-danger"
          comparison_ui <- tags$div(
            class = "kpi-delta small mt-1",
            tags$span(class = paste("fw-bold", delta_class), delta_text),
            tags$span(class = "text-muted ms-1", bench_label)
          )
        }
        
        div(
          class = "col-md-3 col-sm-6 mb-2",
          div(
            class = paste("kpi-card p-3 rounded shadow-sm border bg-white h-100", if (is_primary) "kpi-card-active" else ""),
            div(
              class = "d-flex justify-content-between align-items-start mb-1",
              tags$span(class = "kpi-title text-secondary fw-bold small text-truncate", title = ind_name, ind_name),
              tags$span(class = paste("badge kpi-badge", status_class), status_label)
            ),
            div(
              class = "kpi-value my-1",
              if (is.na(val)) {
                tags$span(class = "text-muted fst-italic", "Not Available")
              } else {
                tags$span(class = "fs-3 fw-bold text-dark", format_indicator_value(val, ind_unit))
              }
            ),
            comparison_ui,
            div(
              class = "kpi-footer d-flex justify-content-between align-items-center mt-2 pt-2 border-top small text-muted",
              tags$span(class = "text-truncate", title = geo_note, geo_note),
              tags$span(class = "badge bg-light text-secondary border", ind_year)
            )
          )
        )
      })
      
      div(class = "row g-2", cards)
    })
  })
}
