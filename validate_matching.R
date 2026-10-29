# validate_matching.R
# Check geographic key matching between boundaries and master datasets

library(sf)
library(dplyr)

states_sf <- readRDS("data/processed/india_states_sf.rds")
districts_sf <- readRDS("data/processed/india_districts_sf.rds")
states_master <- readRDS("data/processed/states_master.rds")
districts_master <- readRDS("data/processed/districts_master.rds")

cat("=== STATE BOUNDARY VS DATA MATCHING ===\n")
sf_state_names <- unique(states_sf$state_name)
master_state_names <- unique(states_master$state_name[states_master$state_name != "India"])

matched_states <- intersect(sf_state_names, master_state_names)
cat("SF states count:", length(sf_state_names), "\n")
cat("Master states count:", length(master_state_names), "\n")
cat("Direct matched states count:", length(matched_states), "\n")

unmatched_sf_states <- setdiff(sf_state_names, master_state_names)
if (length(unmatched_sf_states) > 0) {
  cat("Unmatched in SF:", paste(unmatched_sf_states, collapse = ", "), "\n")
} else {
  cat("All SF states match 100% perfectly!\n")
}

cat("\n=== DISTRICT BOUNDARY VS DATA MATCHING ===\n")
cat("SF districts count:", nrow(districts_sf), "\n")
cat("Master districts count:", nrow(districts_master), "\n")

# Check sample state e.g. Tamil Nadu
tn_sf_dist <- districts_sf %>% filter(state_name == "Tamil Nadu") %>% pull(district_name)
tn_master_dist <- districts_master %>% filter(state_name == "Tamil Nadu") %>% pull(district_name)

cat("Tamil Nadu SF districts (", length(tn_sf_dist), "):\n")
print(sort(tn_sf_dist))
cat("Tamil Nadu Master districts (", length(tn_master_dist), "):\n")
print(sort(tn_master_dist))
matched_tn <- intersect(tolower(tn_sf_dist), tolower(tn_master_dist))
cat("Matched TN districts:", length(matched_tn), "out of", length(tn_sf_dist), "\n")
