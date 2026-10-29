# R/data_validation.R
# System and data integrity validation routines for IndiaScope

library(sf)
library(dplyr)

#' Run comprehensive system and data validation suite
#' @return A list containing validation status, summary, and detailed check results
run_data_validation <- function(data_dir = "data/processed") {
  results <- list()
  all_passed <- TRUE
  
  # 1. Check required processed files
  req_files <- c(
    "india_states_sf.rds",
    "india_districts_sf.rds",
    "states_master.rds",
    "districts_master.rds",
    "indicators_meta.rds"
  )
  
  files_exist <- sapply(file.path(data_dir, req_files), file.exists)
  results$files_check <- list(
    name = "Required Processed Files Existence",
    passed = all(files_exist),
    details = files_exist
  )
  if (!all(files_exist)) all_passed <- FALSE
  
  # 2. Check geometries
  states_sf <- readRDS(file.path(data_dir, "india_states_sf.rds"))
  districts_sf <- readRDS(file.path(data_dir, "india_districts_sf.rds"))
  
  states_crs <- st_crs(states_sf)$epsg
  valid_states <- all(st_is_valid(states_sf))
  valid_districts <- all(st_is_valid(districts_sf))
  
  results$geometry_check <- list(
    name = "Boundary Geometry & CRS Validity",
    passed = valid_states && valid_districts && (states_crs == 4326 || is.na(states_crs)),
    details = c(
      valid_states_geom = valid_states,
      valid_districts_geom = valid_districts,
      states_count = nrow(states_sf),
      districts_count = nrow(districts_sf)
    )
  )
  if (!valid_states || !valid_districts) all_passed <- FALSE
  
  # 3. Check Data Quality & Value Ranges
  states_master <- readRDS(file.path(data_dir, "states_master.rds"))
  districts_master <- readRDS(file.path(data_dir, "districts_master.rds"))
  meta <- readRDS(file.path(data_dir, "indicators_meta.rds"))
  
  # Total population must be > 1 billion at national level
  india_pop <- states_master %>% filter(state_name == "India") %>% pull(pop_total)
  pop_valid <- length(india_pop) > 0 && india_pop > 1000000000
  
  # Literacy rate must be between 40% and 100%
  lit_valid <- all(states_master$effective_literacy_rate >= 40 & states_master$effective_literacy_rate <= 100, na.rm = TRUE)
  
  # Sex ratio must be between 600 and 1200 (Census 2011 Daman & Diu was 618 due to industrial male migration)
  sex_ratio_valid <- all(states_master$sex_ratio >= 600 & states_master$sex_ratio <= 1200, na.rm = TRUE)
  
  results$data_quality_check <- list(
    name = "Indicator Value Ranges & National Totals",
    passed = pop_valid && lit_valid && sex_ratio_valid,
    details = c(
      india_pop = india_pop,
      pop_valid = pop_valid,
      lit_valid = lit_valid,
      sex_ratio_valid = sex_ratio_valid
    )
  )
  if (!pop_valid || !lit_valid || !sex_ratio_valid) all_passed <- FALSE
  
  # 4. Check State Boundary vs Data Matching
  sf_states <- unique(states_sf$state_name)
  data_states <- setdiff(unique(states_master$state_name), "India")
  unmatched_states <- setdiff(sf_states, data_states)
  
  results$state_matching_check <- list(
    name = "State Geographic Alignment",
    passed = length(unmatched_states) == 0,
    details = list(
      matched_count = length(intersect(sf_states, data_states)),
      unmatched = unmatched_states
    )
  )
  if (length(unmatched_states) > 0) all_passed <- FALSE
  
  # 5. Check Indicators Metadata Completeness
  req_meta_cols <- c("indicator_id", "category", "indicator_name", "unit", "ref_period", "source_id")
  meta_complete <- all(req_meta_cols %in% names(meta)) && all(!is.na(meta$indicator_id))
  
  results$metadata_check <- list(
    name = "Indicators Source Register Completeness",
    passed = meta_complete,
    details = list(
      indicators_count = nrow(meta),
      categories = unique(meta$category)
    )
  )
  if (!meta_complete) all_passed <- FALSE
  
  list(
    overall_passed = all_passed,
    timestamp = Sys.time(),
    checks = results
  )
}
