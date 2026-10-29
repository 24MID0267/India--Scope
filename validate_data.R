# data/scripts/validate_data.R
# Standalone runnable validation script for IndiaScope

cat("=========================================================\n")
cat("      INDIASCOPE DATA & SYSTEM VALIDATION SUITE          \n")
cat("=========================================================\n\n")

source("R/data_loader.R")
source("R/data_validation.R")
source("R/helpers.R")

test_results <- list()

# 1. Run core validation
cat("1. Running core data & geometry validation...\n")
core_val <- run_data_validation("data/processed")
test_results$core <- core_val$overall_passed

for (chk_name in names(core_val$checks)) {
  chk <- core_val$checks[[chk_name]]
  cat(sprintf("   [%s] %s\n", if (chk$passed) "OK" else "FAIL", chk$name))
}

# 2. Check duplicate keys in States and Districts
cat("\n2. Checking duplicate observation keys...\n")
states_df <- get_states_data()
districts_df <- get_districts_data()

state_dup <- any(duplicated(states_df$state_name))
cat("   - State keys unique:", !state_dup, "\n")

# Check district keys (state_name + district_name)
dist_keys <- paste(districts_df$state_name, districts_df$district_name, sep = "::")
dist_dup <- any(duplicated(dist_keys))
cat("   - District composite keys unique:", !dist_dup, "\n")
test_results$duplicates <- (!state_dup && !dist_dup)

# 3. Check for invalid or impossible values
cat("\n3. Validating indicator ranges and types...\n")
meta <- get_indicators_meta()
cat("   - Total registered indicators:", nrow(meta), "\n")
cat("   - Indicator categories:", paste(unique(meta$category), collapse = ", "), "\n")

# Sex ratio must be positive
valid_sex_ratio <- all(states_df$sex_ratio > 0, na.rm = TRUE)
# Population must be positive
valid_pop <- all(states_df$pop_total > 0, na.rm = TRUE)
# Rates must not exceed 100%
valid_rates <- all(states_df$effective_literacy_rate <= 100, na.rm = TRUE)

cat("   - Sex ratio > 0:", valid_sex_ratio, "\n")
cat("   - Population > 0:", valid_pop, "\n")
cat("   - Rates <= 100%:", valid_rates, "\n")
test_results$ranges <- (valid_sex_ratio && valid_pop && valid_rates)

# 4. Check Boundary Coverage
cat("\n4. Checking Boundary Geometries...\n")
states_sf <- get_states_sf()
districts_sf <- get_districts_sf()

cat("   - Total state polygons:", nrow(states_sf), "\n")
cat("   - Total district polygons:", nrow(districts_sf), "\n")
cat("   - All state geometries valid:", all(st_is_valid(states_sf)), "\n")
cat("   - All district geometries valid:", all(st_is_valid(districts_sf)), "\n")
test_results$geometries <- (all(st_is_valid(states_sf)) && all(st_is_valid(districts_sf)))

# Summary
cat("\n=========================================================\n")
all_good <- all(unlist(test_results))
if (all_good) {
  cat("  RESULT: ALL VALIDATION SUITES PASSED SUCCESSFULLY [OK] \n")
} else {
  cat("  RESULT: SOME CHECKS FAILED OR REPORTED WARNINGS        \n")
}
cat("=========================================================\n")

if (!all_good) {
  quit(status = 1)
}
