# app.R
# IndiaScope — India Data Intelligence Explorer
# Main Shiny Application Entry Point

library(shiny)
library(bslib)
library(leaflet)
library(DT)

# Source global dependencies and data
source("global.R")

ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center gap-2",
    tags$i(class = "fa fa-compass text-primary fs-4"),
    tags$span(class = "fw-bold", "IndiaScope"),
    tags$span(class = "text-muted fs-6 fw-normal d-none d-md-inline", "| India Data Intelligence Explorer")
  ),
  theme = app_theme,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "indiascope.js")
  ),
  
  # Tab 1: Interactive Explorer
  nav_panel(
    title = "Interactive Explorer",
    icon = icon("map-marked-alt"),
    div(
      class = "container-fluid px-3 py-2",
      # Top bar with KPI cards
      div(
        class = "row mb-2",
        div(
          class = "col-12",
          mod_indicators_ui("indicators_ui")
        )
      ),
      # Main layout: Left filter & ranking / Right map & charts
      div(
        class = "row g-3",
        # Left Column (Controls & Rankings)
        div(
          class = "col-lg-3 col-md-4",
          mod_filters_ui("filters_ui"),
          mod_rankings_ui("rankings_ui"),
          div(
            class = "d-grid gap-2 mb-3",
            mod_source_details_ui("source_ui")
          )
        ),
        # Right Column (Interactive Map & Analytics Panels)
        div(
          class = "col-lg-9 col-md-8",
          mod_map_ui("map_ui"),
          div(class = "mt-3", mod_charts_ui("charts_ui")),
          div(class = "mt-3", mod_data_table_ui("table_ui"))
        )
      )
    )
  ),
  
  # Tab 2: Sources & Methodology Register
  nav_panel(
    title = "Data Sources & Register",
    icon = icon("database"),
    div(
      class = "container py-4",
      div(
        class = "card p-4 shadow-sm mb-4 bg-white",
        tags$h4(class = "fw-bold text-primary", icon("book-open"), " IndiaScope Mandatory Source Register"),
        tags$p(
          class = "text-muted",
          "Every statistic in IndiaScope is verified and traceable to published official government statistical returns and surveys. ",
          "No values are fabricated. Unsupported geographic levels are explicitly flagged."
        ),
        hr(),
        DTOutput("sources_meta_dt")
      ),
      div(
        class = "card p-4 shadow-sm bg-white",
        tags$h5(class = "fw-bold", "Survey Coverage & Granularity Guide"),
        tags$ul(
          class = "list-group list-group-flush mt-2",
          tags$li(
            class = "list-group-item d-flex justify-content-between align-items-start",
            div(
              class = "ms-2 me-auto",
              div(class = "fw-bold", "1. Census of India (ORGI, 2011)"),
              "Primary Census Abstract (PCA) covering total, male, female population, literacy rates, child population (0-6), SC/ST distributions, and work participation across all 640 districts and 35 states/UTs."
            ),
            tags$span(class = "badge bg-success rounded-pill", "National, State, District")
          ),
          tags$li(
            class = "list-group-item d-flex justify-content-between align-items-start",
            div(
              class = "ms-2 me-auto",
              div(class = "fw-bold", "2. National Family Health Survey (NFHS-5: 2019-21, NFHS-4: 2015-16)"),
              "Comprehensive health, nutrition, and infrastructure metrics covering institutional births, full immunization, antenatal care, child stunting, and sanitation facilities."
            ),
            tags$span(class = "badge bg-success rounded-pill", "National, State, District")
          ),
          tags$li(
            class = "list-group-item d-flex justify-content-between align-items-start",
            div(
              class = "ms-2 me-auto",
              div(class = "fw-bold", "3. Periodic Labour Force Survey (MoSPI PLFS 2023-24)"),
              "Official annual employment estimates for LFPR, WPR, and Unemployment Rate (15+ years, usual status ps+ss). Sampled and representative at State and National levels."
            ),
            tags$span(class = "badge bg-primary rounded-pill", "National, State")
          ),
          tags$li(
            class = "list-group-item d-flex justify-content-between align-items-start",
            div(
              class = "ms-2 me-auto",
              div(class = "fw-bold", "4. Unified District Information System for Education Plus (UDISE+ 2021-22)"),
              "Official flash statistics on school infrastructure (electricity, drinking water, computers, internet) and student-teacher ratios from the Ministry of Education."
            ),
            tags$span(class = "badge bg-primary rounded-pill", "National, State")
          )
        )
      )
    )
  ),
  
  # Tab 3: System Health & Validation
  nav_panel(
    title = "System Diagnostics",
    icon = icon("check-circle"),
    div(
      class = "container py-4",
      div(
        class = "card p-4 shadow-sm bg-white",
        tags$h4(class = "fw-bold text-success", icon("shield-alt"), " Data Integrity & System Health"),
        tags$p(class = "text-muted", "Automated validation status of spatial boundary files, data joins, CRS projections, and indicator valid ranges."),
        hr(),
        actionButton("btn_run_diagnostics", "Re-run Validation Suite", icon = icon("sync"), class = "btn-outline-primary mb-3"),
        verbatimTextOutput("diagnostics_output")
      )
    )
  )
)

server <- function(input, output, session) {
  # Core reactive state for geography navigation
  # level: "India", "State", or "District"
  selected_geo <- reactiveValues(
    level = "India",
    state = NULL,
    district = NULL
  )
  
  # Active indicator and category reactives
  current_category <- reactiveVal("Population & Literacy")
  current_indicator <- reactiveVal("effective_literacy_rate")
  
  # Initialize Modules
  mod_filters_server("filters_ui", selected_geo, current_category, current_indicator)
  mod_indicators_server("indicators_ui", selected_geo, current_category, current_indicator)
  mod_map_server("map_ui", selected_geo, current_indicator, current_category)
  mod_charts_server("charts_ui", selected_geo, current_category, current_indicator)
  mod_rankings_server("rankings_ui", selected_geo, current_category, current_indicator)
  mod_data_table_server("table_ui", selected_geo, current_category, current_indicator)
  mod_source_details_server("source_ui", current_indicator)
  
  # Source Register DataTable (Tab 2)
  output$sources_meta_dt <- renderDT({
    meta <- get_indicators_meta()
    display_meta <- meta %>%
      select(
        Category = category,
        Indicator = indicator_name,
        Unit = unit,
        `Reference Period` = ref_period,
        Granularity = geo_granularity,
        Source = source_name,
        `Dataset ID` = source_id
      )
    
    datatable(
      display_meta,
      options = list(
        pageLength = 15,
        dom = "ftip",
        autoWidth = TRUE
      ),
      rownames = FALSE,
      class = "stripe hover compact table-bordered"
    )
  })
  
  # System Diagnostics Runner (Tab 3)
  output$diagnostics_output <- renderPrint({
    input$btn_run_diagnostics # Dependency
    cat("Executing IndiaScope Data Validation Suite...\n")
    report <- run_data_validation("data/processed")
    cat("Status: ", if (report$overall_passed) "ALL CHECKS PASSED [OK]" else "WARNINGS FOUND", "\n")
    cat("Timestamp: ", format(report$timestamp), "\n\n")
    for (chk in report$checks) {
      cat(sprintf("[%s] %s\n", if (chk$passed) "PASS" else "FAIL", chk$name))
    }
  })
}

shinyApp(ui = ui, server = server)
