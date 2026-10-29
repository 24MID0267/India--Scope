# check_name_alignment.R
library(sf)
library(dplyr)
library(readr)

states_sf <- readRDS("data/processed/india_states_sf.rds")
sf_states <- sort(unique(states_sf$state_name))
cat("States in GeoJSON (count:", length(sf_states), "):\n")
print(sf_states)

# Read NFHS-5 states
nfhs_states <- read_csv("data/raw/nfhs5_states.csv", col_types = cols())
nfhs_names <- sort(unique(nfhs_states$state))
cat("\nStates in NFHS-5 (count:", length(nfhs_names), "):\n")
print(nfhs_names)

# Inspect NFHS-5 indicators
cat("\nSample NFHS-5 indicators:\n")
print(unique(nfhs_states$indicator)[1:20])
