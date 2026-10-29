# R/mod_source_details.R
# Source Register and Methodology Metadata Module

library(shiny)
library(dplyr)

mod_source_details_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("btn_view_sources"),
      label = "Source & Methodology Details",
      icon = icon("book-open"),
      class = "btn-sm btn-outline-info"
    )
  )
}

mod_source_details_server <- function(id, current_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$btn_view_sources, {
      ind_id <- current_indicator()
      ind_meta <- get_indicator_meta(ind_id)
      
      showModal(modalDialog(
        title = tags$div(
          class = "d-flex align-items-center gap-2",
          tags$i(class = "fa fa-database text-primary"),
          tags$span("Metadata & Traceability: ", if (!is.null(ind_meta)) ind_meta$indicator_name else ind_id)
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        if (is.null(ind_meta)) {
          tags$p("Metadata not found.")
        } else {
          tagList(
            tags$table(
              class = "table table-bordered table-sm",
              tags$tbody(
                tags$tr(
                  tags$th(class = "table-light", style = "width: 30%;", "Indicator Name"),
                  tags$td(ind_meta$indicator_name)
                ),
                tags$tr(
                  tags$th(class = "table-light", "Category"),
                  tags$td(ind_meta$category)
                ),
                tags$tr(
                  tags$th(class = "table-light", "Dataset ID"),
                  tags$td(tags$code(ind_meta$source_id))
                ),
                tags$tr(
                  tags$th(class = "table-light", "Publishing Organization"),
                  tags$td(ind_meta$source_name)
                ),
                tags$tr(
                  tags$th(class = "table-light", "Reporting Reference Period"),
                  tags$td(tags$span(class = "badge bg-primary", ind_meta$ref_period))
                ),
                tags$tr(
                  tags$th(class = "table-light", "Measurement Unit"),
                  tags$td(ind_meta$unit)
                ),
                tags$tr(
                  tags$th(class = "table-light", "Geographic Granularity"),
                  tags$td(ind_meta$geo_granularity)
                ),
                tags$tr(
                  tags$th(class = "table-light", "Indicator Definition"),
                  tags$td(ind_meta$description)
                ),
                tags$tr(
                  tags$th(class = "table-light", "Official Source Documentation"),
                  tags$td(
                    tags$a(
                      href = switch(
                        ind_meta$source_id,
                        "CENSUS_2011_PCA" = "https://censusindia.gov.in/census.website/en/data/population-finder",
                        "NFHS_5" = "http://rchiips.org/nfhs/factsheet_NFHS-5.shtml",
                        "MOSPI_PLFS_2023_24" = "https://mospi.gov.in/",
                        "MOE_UDISE_2021_22" = "https://dashboard.udiseplus.gov.in/",
                        "https://data.gov.in/"
                      ),
                      target = "_blank",
                      icon("external-link-alt"),
                      " View Official Publication Portal"
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "alert alert-secondary small mt-3",
              tags$strong("Methodology Note: "),
              switch(
                ind_meta$source_id,
                "CENSUS_2011_PCA" = "Data compiled from the official Primary Census Abstract (PCA) tables published by the Office of the Registrar General & Census Commissioner, India. Effective literacy excludes children aged 0-6.",
                "NFHS_5" = "Data compiled from the National Family Health Survey-5 (2019-21) district and state factsheets conducted under the stewardship of the Ministry of Health and Family Welfare (MoHFW) by the International Institute for Population Sciences (IIPS).",
                "MOSPI_PLFS_2023_24" = "Estimates are derived from the official Annual Report of the Periodic Labour Force Survey (PLFS) 2023-24 (Usual Status ps+ss, Age 15+). Note: PLFS is officially sampled for state and national representation; district figures are not officially produced.",
                "MOE_UDISE_2021_22" = "Extracted from the Unified District Information System for Education Plus (UDISE+) official flash statistics and dashboard published by the Department of School Education & Literacy, Ministry of Education.",
                "Official administrative and statistical returns."
              )
            )
          )
        }
      ))
    })
  })
}
