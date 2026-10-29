# R/data_loader.R
# Manages reading, caching, and serving pre-processed spatial and indicator datasets

suppressPackageStartupMessages(library(dplyr))

# Module-level environment cache for sub-second retrieval
.DATA_CACHE <- new.env(parent = emptyenv())

#' Load all spatial and indicator datasets into memory
#' @param data_dir Path to data directory
load_all_data <- function(data_dir = "data/processed") {
  if (exists("initialized", envir = .DATA_CACHE) && .DATA_CACHE$initialized) {
    return(invisible(TRUE))
  }
  
  cat("IndiaScope: Initializing data cache from", data_dir, "...\n")
  
  states_sf_file <- file.path(data_dir, "india_states_sf.rds")
  districts_sf_file <- file.path(data_dir, "india_districts_sf.rds")
  states_master_file <- file.path(data_dir, "states_master.rds")
  districts_master_file <- file.path(data_dir, "districts_master.rds")
  meta_file <- file.path(data_dir, "indicators_meta.rds")
  
  # Ensure all processed files exist
  req_files <- c(states_sf_file, districts_sf_file, states_master_file, districts_master_file, meta_file)
  missing_files <- req_files[!file.exists(req_files)]
  if (length(missing_files) > 0) {
    stop("Missing required processed data files: ", paste(missing_files, collapse = ", "))
  }
  
  .DATA_CACHE$states_sf <- readRDS(states_sf_file)
  .DATA_CACHE$districts_sf <- readRDS(districts_sf_file)
  .DATA_CACHE$states_master <- readRDS(states_master_file)
  .DATA_CACHE$districts_master <- readRDS(districts_master_file)
  .DATA_CACHE$indicators_meta <- readRDS(meta_file)
  .DATA_CACHE$initialized <- TRUE
  
  cat("IndiaScope: Data cache initialized successfully!\n")
  invisible(TRUE)
}

#' Get state boundary sf object
get_states_sf <- function() {
  load_all_data()
  .DATA_CACHE$states_sf
}

#' Get district boundary sf object, optionally filtered by state
#' @param state_name Optional state name to filter
get_districts_sf <- function(state_name = NULL) {
  load_all_data()
  sf_dist <- .DATA_CACHE$districts_sf
  if (!is.null(state_name) && nzchar(state_name)) {
    sf_dist <- sf_dist %>% filter(state_name == !!state_name)
  }
  sf_dist
}

#' Get master states dataset (including India national row)
get_states_data <- function() {
  load_all_data()
  .DATA_CACHE$states_master
}

#' Get national (India) summary row
get_national_data <- function() {
  load_all_data()
  .DATA_CACHE$states_master %>% filter(state_name == "India")
}

#' Get state data row
#' @param state_name Character name of state
get_state_data <- function(state_name) {
  load_all_data()
  if (is.null(state_name) || !nzchar(state_name)) return(get_national_data())
  res <- .DATA_CACHE$states_master %>% filter(state_name == !!state_name)
  if (nrow(res) == 0) return(get_national_data())
  res
}

#' Get districts dataset, optionally filtered by state
#' @param state_name Optional state name to filter
get_districts_data <- function(state_name = NULL) {
  load_all_data()
  df <- .DATA_CACHE$districts_master
  if (!is.null(state_name) && nzchar(state_name)) {
    df <- df %>% filter(state_name == !!state_name)
  }
  df
}

#' Get specific district data row
#' @param state_name Character state name
#' @param district_name Character district name
get_district_data <- function(state_name, district_name) {
  load_all_data()
  state_df <- get_districts_data(state_name)
  if (nrow(state_df) == 0) return(NULL)
  
  # Try exact match first
  row <- state_df %>% filter(tolower(district_name) == tolower(!!district_name))
  if (nrow(row) > 0) return(row[1, ])
  
  # Try matched district name using alias/fuzzy helper
  matched_name <- match_district(district_name, state_df$district_name)
  if (!is.na(matched_name)) {
    return(state_df %>% filter(district_name == matched_name) %>% slice(1))
  }
  NULL
}

#' Get indicator metadata table
get_indicators_meta <- function() {
  load_all_data()
  .DATA_CACHE$indicators_meta
}

#' Get metadata for a specific indicator ID
#' @param ind_id Character indicator ID
get_indicator_meta <- function(ind_id) {
  load_all_data()
  meta <- .DATA_CACHE$indicators_meta %>% filter(indicator_id == !!ind_id)
  if (nrow(meta) == 0) return(NULL)
  as.list(meta[1, ])
}

#' Get list of indicators for a specific category
#' @param cat_name Character category name
get_indicators_by_category <- function(cat_name) {
  load_all_data()
  .DATA_CACHE$indicators_meta %>% filter(category == !!cat_name)
}
