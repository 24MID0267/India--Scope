# R/mod_data_table.R
# Interactive Data Table and CSV Export Module

library(shiny)
library(DT)
library(dplyr)

mod_data_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "data-table-container bg-white p-3 rounded shadow-sm border mb-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2",
        div(
          tags$h6(class = "fw-bold text-secondary m-0", icon("table"), " Data Explorer Table"),
          tags$span(class = "small text-muted", "Sorted and filterable dataset for selected geography")
        ),
        div(
          class = "d-flex gap-2",
          downloadButton(
            ns("btn_download_csv"),
            label = "Export CSV",
            class = "btn-sm btn-outline-success"
          )
        )
      ),
      DTOutput(ns("indicators_dt"))
    )
  )
}

mod_data_table_server <- function(id, selected_geo, current_category, current_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive table data computation
    table_data_reactive <- reactive({
      level <- selected_geo$level
      st <- selected_geo$state
      cat_name <- current_category()
      
      cat_inds <- get_indicators_by_category(cat_name)
      if (nrow(cat_inds) == 0) return(data.frame())
      
      ind_cols <- cat_inds$indicator_id
      col_labels <- setNames(cat_inds$indicator_name, cat_inds$indicator_id)
      
      if (level == "India") {
        # State-level table
        df <- get_states_data() %>% filter(state_name != "India")
        avail_cols <- intersect(ind_cols, names(df))
        
        display_df <- df %>%
          select(State = state_name, all_of(avail_cols))
        
        # Rename columns to human-readable names
        for (col in avail_cols) {
          names(display_df)[names(display_df) == col] <- col_labels[col]
        }
        display_df
      } else {
        # District-level table
        req(st)
        df <- get_districts_data(st)
        avail_cols <- intersect(ind_cols, names(df))
        
        if (length(avail_cols) == 0) {
          return(data.frame(Message = "District-level indicators are unavailable for this category (State-level survey only)."))
        }
        
        display_df <- df %>%
          select(District = district_name, all_of(avail_cols))
        
        for (col in avail_cols) {
          names(display_df)[names(display_df) == col] <- col_labels[col]
        }
        display_df
      }
    })
    
    # Render DT
    output$indicators_dt <- renderDT({
      df <- table_data_reactive()
      req(nrow(df) > 0)
      
      datatable(
        df,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50),
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = "ftip"
        ),
        rownames = FALSE,
        class = "stripe hover compact table-bordered"
      )
    })
    
    # Download handler for CSV Export
    output$btn_download_csv <- downloadHandler(
      filename = function() {
        geo_name <- if (selected_geo$level == "India") "All_India" else selected_geo$state
        cat_slug <- gsub("[^A-Za-z0-9]", "_", tolower(current_category()))
        paste0("IndiaScope_", geo_name, "_", cat_slug, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- table_data_reactive()
        write.csv(df, file, row.names = FALSE, na = "NA")
      }
    )
  })
}
