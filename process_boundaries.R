# process_boundaries.R
# Cleans, standardizes attributes, simplifies geometry for fast web rendering, and saves processed boundaries.

library(sf)
library(dplyr)

processed_dir <- file.path("data", "processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

cat("Loading raw boundaries...\n")
raw_states <- st_read("data/raw/india_state_raw.geojson", quiet = TRUE)
raw_districts <- st_read("data/raw/india_district_raw.geojson", quiet = TRUE)

# Standardize state names
clean_state_name <- function(x) {
  x <- trimws(x)
  # Standardize known variations
  x[x == "Andaman and Nicobar"] <- "Andaman & Nicobar Islands"
  x[x == "Dadra and Nagar Haveli"] <- "Dadra & Nagar Haveli"
  x[x == "Daman and Diu"] <- "Daman & Diu"
  x[x == "Jammu and Kashmir"] <- "Jammu & Kashmir"
  x[x == "Orissa"] <- "Odisha"
  x[x == "Pondicherry"] <- "Puducherry"
  x[x == "Uttaranchal"] <- "Uttarakhand"
  x
}

clean_states <- raw_states %>%
  mutate(
    state_id = as.character(ID_1),
    state_name = clean_state_name(NAME_1),
    type = ENGTYPE_1
  ) %>%
  select(state_id, state_name, type, geometry)

# Simplify states for fast national overview map
cat("Simplifying state boundaries...\n")
# dTolerance in degrees (approx 500m to 1km)
states_simple <- st_simplify(clean_states, preserveTopology = TRUE, dTolerance = 0.01)
# Repair any self-intersections
states_simple <- st_make_valid(states_simple)

cat("Original states size in memory:", format(object.size(clean_states), "Mb"), "\n")
cat("Simplified states size in memory:", format(object.size(states_simple), "Mb"), "\n")

# Process districts
cat("Processing district boundaries...\n")
clean_districts <- raw_districts %>%
  mutate(
    state_id = as.character(ID_1),
    state_name = clean_state_name(NAME_1),
    district_id = as.character(ID_2),
    district_name = trimws(NAME_2)
  ) %>%
  select(state_id, state_name, district_id, district_name, geometry)

# Simplify districts for fast state-level rendering
cat("Simplifying district boundaries...\n")
districts_simple <- st_simplify(clean_districts, preserveTopology = TRUE, dTolerance = 0.005)
districts_simple <- st_make_valid(districts_simple)

cat("Original districts size in memory:", format(object.size(clean_districts), "Mb"), "\n")
cat("Simplified districts size in memory:", format(object.size(districts_simple), "Mb"), "\n")

# Validate geometries
cat("Valid states:", all(st_is_valid(states_simple)), "\n")
cat("Valid districts:", all(st_is_valid(districts_simple)), "\n")

# Save as RDS for lightning-fast Shiny loading
saveRDS(states_simple, file.path(processed_dir, "india_states_sf.rds"), compress = "xz")
saveRDS(districts_simple, file.path(processed_dir, "india_districts_sf.rds"), compress = "xz")

cat("Saved processed boundaries successfully!\n")
cat("india_states_sf.rds size:", file.info(file.path(processed_dir, "india_states_sf.rds"))$size / 1024, "KB\n")
cat("india_districts_sf.rds size:", file.info(file.path(processed_dir, "india_districts_sf.rds"))$size / 1024, "KB\n")
