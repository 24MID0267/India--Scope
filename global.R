# global.R
# IndiaScope Global Configuration and Dependencies

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(sf)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(readr)
  library(stringr)
  library(scales)
})

# Source all R modules
r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
for (f in r_files) {
  source(f, local = FALSE)
}

# Pre-load and cache processed datasets for instantaneous startup
tryCatch({
  load_all_data("data/processed")
}, error = function(e) {
  warning("Initial data load encountered a notice: ", e$message)
})

# Application Theme using bslib
app_theme <- bs_theme(
  version = 5,
  bootswatch = "litera",
  primary = "#0f4c81",
  secondary = "#64748b",
  success = "#2e7d32",
  info = "#0288d1",
  warning = "#f57c00",
  danger = "#d32f2f",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)
