# inspect_boundaries.R
library(sf)

cat("=== INSPECTING STATES ===\n")
states <- st_read("data/raw/india_state_raw.geojson", quiet = TRUE)
cat("CRS:", st_crs(states)$input, "\n")
cat("Number of states/features:", nrow(states), "\n")
cat("Columns:", paste(names(states), collapse = ", "), "\n")
print(head(as.data.frame(states)[, names(states) != "geometry"], 10))

cat("\n=== INSPECTING DISTRICTS ===\n")
districts <- st_read("data/raw/india_district_raw.geojson", quiet = TRUE)
cat("CRS:", st_crs(districts)$input, "\n")
cat("Number of districts/features:", nrow(districts), "\n")
cat("Columns:", paste(names(districts), collapse = ", "), "\n")
print(head(as.data.frame(districts)[, names(districts) != "geometry"], 10))
