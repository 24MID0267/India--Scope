# download_boundaries.R
# Script to download and inspect state and district boundary GeoJSON files

raw_dir <- file.path("data", "raw")
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

state_url <- "https://raw.githubusercontent.com/geohacker/india/master/state/india_state.geojson"
dist_url <- "https://raw.githubusercontent.com/geohacker/india/master/district/india_district.geojson"

state_file <- file.path(raw_dir, "india_state_raw.geojson")
dist_file <- file.path(raw_dir, "india_district_raw.geojson")

cat("Downloading state boundaries...\n")
if (!file.exists(state_file) || file.info(state_file)$size == 0) {
  download.file(state_url, state_file, mode = "wb", quiet = FALSE)
}
cat("State file size:", file.info(state_file)$size, "bytes\n")

cat("Downloading district boundaries...\n")
if (!file.exists(dist_file) || file.info(dist_file)$size == 0) {
  download.file(dist_url, dist_file, mode = "wb", quiet = FALSE)
}
cat("District file size:", file.info(dist_file)$size, "bytes\n")

library(sf)
cat("Reading state boundaries...\n")
states_sf <- st_read(state_file, quiet = TRUE)
cat("State geometries loaded:", nrow(states_sf), "\n")
print(names(states_sf))
print(head(states_sf[, 1:min(3, ncol(states_sf))]))

cat("Reading district boundaries...\n")
dist_sf <- st_read(dist_file, quiet = TRUE)
cat("District geometries loaded:", nrow(dist_sf), "\n")
print(names(dist_sf))
print(head(dist_sf[, 1:min(3, ncol(dist_sf))]))
