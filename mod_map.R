# R/mod_map.R
# Interactive Map Module for IndiaScope

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "map-container-wrapper position-relative",
      # Map controls bar overlay
      div(
        class = "map-overlay-controls d-flex align-items-center justify-content-between p-2 mb-2 bg-white rounded shadow-sm border",
        div(
          class = "breadcrumb-nav d-flex align-items-center gap-2",
          uiOutput(ns("breadcrumb_ui"))
        ),
        div(
          class = "map-actions d-flex gap-2",
          actionButton(
            ns("btn_reset_india"),
            label = "Reset to India",
            icon = icon("globe-asia"),
            class = "btn-sm btn-outline-primary"
          ),
          uiOutput(ns("back_to_state_ui"))
        )
      ),
      # The main Leaflet map
      leafletOutput(ns("main_map"), height = "580px"),
      # Map legend & status badge overlay
      div(
        class = "map-legend-overlay p-2 bg-white rounded shadow-sm border mt-2 small",
        uiOutput(ns("map_status_info"))
      )
    )
  )
}

mod_map_server <- function(id, selected_geo, current_indicator, current_category) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render Breadcrumbs
    output$breadcrumb_ui <- renderUI({
      level <- selected_geo$level
      st <- selected_geo$state
      dt <- selected_geo$district
      
      tags$nav(
        `aria-label` = "breadcrumb",
        tags$ol(
          class = "breadcrumb m-0 p-0 bg-transparent",
          tags$li(
            class = paste("breadcrumb-item", if (level == "India") "active fw-bold text-primary" else ""),
            actionLink(ns("nav_india"), label = "India", class = "text-decoration-none")
          ),
          if (!is.null(st) && (level %in% c("State", "District"))) {
            tags$li(
              class = paste("breadcrumb-item", if (level == "State") "active fw-bold text-primary" else ""),
              actionLink(ns("nav_state"), label = st, class = "text-decoration-none")
            )
          },
          if (!is.null(dt) && level == "District") {
            tags$li(
              class = "breadcrumb-item active fw-bold text-success",
              tags$span(dt)
            )
          }
        )
      )
    })
    
    # Back to State button (visible only in District view)
    output$back_to_state_ui <- renderUI({
      if (selected_geo$level == "District") {
        actionButton(
          ns("btn_back_state"),
          label = paste("Back to", selected_geo$state),
          icon = icon("arrow-left"),
          class = "btn-sm btn-outline-secondary"
        )
      }
    })
    
    # Map status info
    output$map_status_info <- renderUI({
      ind_meta <- get_indicator_meta(current_indicator())
      ind_name <- if (!is.null(ind_meta)) ind_meta$indicator_name else current_indicator()
      ind_unit <- if (!is.null(ind_meta)) ind_meta$unit else ""
      ind_year <- if (!is.null(ind_meta)) ind_meta$ref_period else ""
      ind_src  <- if (!is.null(ind_meta)) ind_meta$source_name else ""
      
      tagList(
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          tags$span(
            tags$strong("Active Metric: "),
            ind_name,
            if (nzchar(ind_unit)) tags$span(class = "text-muted", paste0(" (", ind_unit, ")")),
            tags$span(class = "badge bg-info text-dark ms-2", paste("Period:", ind_year))
          ),
          tags$span(
            class = "text-muted",
            tags$i(class = "fa fa-info-circle me-1"),
            "Source: ", ind_src
          )
        )
      )
    })
    
    # Base Map Initialization
    output$main_map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 3, maxZoom = 12)) %>%
        addProviderTiles(
          providers$CartoDB.Positron,
          options = providerTileOptions(noWrap = TRUE)
        ) %>%
        setView(lng = 78.9629, lat = 22.5937, zoom = 4)
    })
    
    # Reactive observer for Map Layer rendering based on selected_geo and current_indicator
    observe({
      level <- selected_geo$level
      st_name <- selected_geo$state
      dt_name <- selected_geo$district
      ind_id <- current_indicator()
      ind_meta <- get_indicator_meta(ind_id)
      ind_unit <- if (!is.null(ind_meta)) ind_meta$unit else ""
      ind_dir  <- if (!is.null(ind_meta)) ind_meta$direction else "higher_is_better"
      
      proxy <- leafletProxy("main_map")
      proxy %>% clearShapes() %>% clearControls()
      
      if (level == "India") {
        # Level 1: India Overview
        states_sf <- get_states_sf()
        states_data <- get_states_data() %>% filter(state_name != "India")
        
        # Merge data onto sf
        plot_sf <- states_sf %>%
          left_join(states_data, by = "state_name")
        
        vals <- if (ind_id %in% names(plot_sf)) plot_sf[[ind_id]] else rep(NA_real_, nrow(plot_sf))
        has_vals <- !all(is.na(vals))
        
        pal <- if (has_vals) get_choropleth_palette(vals, ind_dir) else NULL
        
        # Labels
        labels <- sprintf(
          "<strong>%s</strong><br/>%s: <strong>%s</strong>",
          plot_sf$state_name,
          if (!is.null(ind_meta)) ind_meta$indicator_name else ind_id,
          sapply(vals, format_indicator_value, unit = ind_unit)
        ) %>% lapply(htmltools::HTML)
        
        fill_colors <- if (has_vals) pal(vals) else rep("#e0e0e0", nrow(plot_sf))
        
        proxy %>%
          addPolygons(
            data = plot_sf,
            layerId = ~state_name,
            fillColor = fill_colors,
            fillOpacity = 0.75,
            weight = 1.2,
            color = "#495057",
            opacity = 1,
            dashArray = "1",
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#0d6efd",
              dashArray = "",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "4px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          setView(lng = 78.9629, lat = 22.5937, zoom = 4)
        
        if (has_vals) {
          clean_vals <- vals[!is.na(vals)]
          if (length(clean_vals) > 0) {
            proxy %>% addLegend(
              pal = pal,
              values = clean_vals,
              title = if (!is.null(ind_meta)) paste0(ind_meta$indicator_name, " (", ind_unit, ")") else ind_id,
              position = "bottomright",
              layerId = "map_legend"
            )
          }
        }
        
      } else if (level %in% c("State", "District")) {
        # Level 2 & 3: Selected State Districts View
        req(st_name)
        dist_sf <- get_districts_sf(st_name)
        dist_data <- get_districts_data(st_name)
        
        if (nrow(dist_sf) > 0) {
          # Match district names
          dist_sf$matched_name <- sapply(dist_sf$district_name, function(d) {
            match_district(d, dist_data$district_name)
          })
          
          plot_sf <- dist_sf %>%
            left_join(dist_data, by = c("matched_name" = "district_name"))
          
          # Check if indicator exists at district level
          vals <- if (ind_id %in% names(plot_sf)) plot_sf[[ind_id]] else rep(NA_real_, nrow(plot_sf))
          has_vals <- !all(is.na(vals))
          
          pal <- if (has_vals) get_choropleth_palette(vals, ind_dir) else NULL
          fill_colors <- if (has_vals) pal(vals) else rep("#cfd8dc", nrow(plot_sf))
          
          labels <- sprintf(
            "<strong>%s District</strong><br/>State: %s<br/>%s: <strong>%s</strong>",
            plot_sf$district_name.x,
            st_name,
            if (!is.null(ind_meta)) ind_meta$indicator_name else ind_id,
            if (has_vals) {
              sapply(vals, format_indicator_value, unit = ind_unit)
            } else {
              "State-level figure only"
            }
          ) %>% lapply(htmltools::HTML)
          
          # Render District polygons
          proxy %>%
            addPolygons(
              data = plot_sf,
              layerId = ~district_name.x,
              fillColor = fill_colors,
              fillOpacity = 0.75,
              weight = 1.2,
              color = "#37474f",
              opacity = 1,
              dashArray = "1",
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#ff6f00",
                dashArray = "",
                fillOpacity = 0.9,
                bringToFront = TRUE
              ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "4px 8px"),
                textsize = "13px",
                direction = "auto"
              )
            )
          
          # If a specific district is selected (Level 3), highlight it
          if (level == "District" && !is.null(dt_name)) {
            selected_dist_poly <- dist_sf %>%
              filter(tolower(district_name) == tolower(dt_name) | matched_name == dt_name)
            
            if (nrow(selected_dist_poly) > 0) {
              proxy %>%
                addPolygons(
                  data = selected_dist_poly,
                  layerId = "selected_district_highlight",
                  fillColor = "#ffd54f",
                  fillOpacity = 0.6,
                  weight = 4,
                  color = "#d84315",
                  opacity = 1
                )
            }
          }
          
          # Zoom into state bounding box
          bbox <- st_bbox(dist_sf)
          proxy %>% fitBounds(
            lng1 = as.numeric(bbox["xmin"]),
            lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]),
            lat2 = as.numeric(bbox["ymax"])
          )
          
          if (has_vals) {
            clean_vals <- vals[!is.na(vals)]
            if (length(clean_vals) > 0) {
              proxy %>% addLegend(
                pal = pal,
                values = clean_vals,
                title = if (!is.null(ind_meta)) paste0(ind_meta$indicator_name, " (", ind_unit, ")") else ind_id,
                position = "bottomright",
                layerId = "map_legend"
              )
            }
          }
        }
      }
    })
    
    # Handle Polygon Click Events
    observeEvent(input$main_map_shape_click, {
      click <- input$main_map_shape_click
      clicked_id <- click$id
      req(clicked_id)
      
      # Ignore clicks on highlight layer
      if (clicked_id == "selected_district_highlight") return()
      
      if (selected_geo$level == "India") {
        # Clicked on a state: drill down to State view
        selected_geo$level <- "State"
        selected_geo$state <- clicked_id
        selected_geo$district <- NULL
      } else if (selected_geo$level == "State") {
        # Clicked on a district: drill down to District view
        selected_geo$level <- "District"
        selected_geo$district <- clicked_id
      } else if (selected_geo$level == "District") {
        # Clicked on another district in the same state: switch selected district
        selected_geo$district <- clicked_id
      }
    })
    
    # Breadcrumb and Action Button Events
    observeEvent(input$nav_india, {
      selected_geo$level <- "India"
      selected_geo$state <- NULL
      selected_geo$district <- NULL
    })
    
    observeEvent(input$btn_reset_india, {
      selected_geo$level <- "India"
      selected_geo$state <- NULL
      selected_geo$district <- NULL
    })
    
    observeEvent(input$nav_state, {
      selected_geo$level <- "State"
      selected_geo$district <- NULL
    })
    
    observeEvent(input$btn_back_state, {
      selected_geo$level <- "State"
      selected_geo$district <- NULL
    })
  })
}
