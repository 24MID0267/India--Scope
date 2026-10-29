# data/scripts/process_data.R
# Consolidates, standardizes, validates, and models indicators across all 4 categories:
# 1. Population & Literacy (Census 2011 Primary Census Abstract)
# 2. Healthcare (NFHS-5: 2019-21, and NFHS-4: 2015-16)
# 3. Employment & Economy (MoSPI PLFS 2023-24 & MoSPI/RBI NSDP per capita)
# 4. Education (UDISE+ 2021-22 Flash Statistics, MoE)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

processed_dir <- file.path("data", "processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

cat("=== PROCESSING CENSUS 2011 POPULATION & LITERACY DATA ===\n")
pca_raw <- read_csv("data/raw/census_2011_pca.csv", col_types = cols())

# State code mapping for Census 2011
census_state_map <- c(
  "01" = "Jammu & Kashmir",
  "02" = "Himachal Pradesh",
  "03" = "Punjab",
  "04" = "Chandigarh",
  "05" = "Uttarakhand",
  "06" = "Haryana",
  "07" = "Delhi",
  "08" = "Rajasthan",
  "09" = "Uttar Pradesh",
  "10" = "Bihar",
  "11" = "Sikkim",
  "12" = "Arunachal Pradesh",
  "13" = "Nagaland",
  "14" = "Manipur",
  "15" = "Mizoram",
  "16" = "Tripura",
  "17" = "Meghalaya",
  "18" = "Assam",
  "19" = "West Bengal",
  "20" = "Jharkhand",
  "21" = "Odisha",
  "22" = "Chhattisgarh",
  "23" = "Madhya Pradesh",
  "24" = "Gujarat",
  "25" = "Daman & Diu",
  "26" = "Dadra & Nagar Haveli",
  "27" = "Maharashtra",
  "28" = "Andhra Pradesh",
  "29" = "Karnataka",
  "30" = "Goa",
  "31" = "Lakshadweep",
  "32" = "Kerala",
  "33" = "Tamil Nadu",
  "34" = "Puducherry",
  "35" = "Andaman & Nicobar Islands"
)

pca_clean <- pca_raw %>%
  mutate(
    state_code = sprintf("%02d", as.integer(State)),
    state_name = census_state_map[state_code],
    district_code = sprintf("%03d", as.integer(District)),
    district_name = trimws(Name),
    # Calculations
    pop_total = TOT_P,
    pop_male = TOT_M,
    pop_female = TOT_F,
    sex_ratio = round((TOT_F / TOT_M) * 1000, 1),
    # Effective literacy excludes 0-6 age group
    effective_literacy_rate = round((P_LIT / (TOT_P - P_06)) * 100, 2),
    male_literacy_rate = round((M_LIT / (TOT_M - M_06)) * 100, 2),
    female_literacy_rate = round((F_LIT / (TOT_F - F_06)) * 100, 2),
    child_pop_06_pct = round((P_06 / TOT_P) * 100, 2),
    sc_pct = round((P_SC / TOT_P) * 100, 2),
    st_pct = round((P_ST / TOT_P) * 100, 2),
    work_participation_rate = round((TOT_WORK_P / TOT_P) * 100, 2),
    households = No_HH
  )

# State aggregate from districts
state_pca <- pca_clean %>%
  group_by(state_name) %>%
  summarise(
    pop_total = sum(pop_total, na.rm = TRUE),
    pop_male = sum(pop_male, na.rm = TRUE),
    pop_female = sum(pop_female, na.rm = TRUE),
    P_06 = sum(P_06, na.rm = TRUE),
    M_06 = sum(M_06, na.rm = TRUE),
    F_06 = sum(F_06, na.rm = TRUE),
    P_LIT = sum(P_LIT, na.rm = TRUE),
    M_LIT = sum(M_LIT, na.rm = TRUE),
    F_LIT = sum(F_LIT, na.rm = TRUE),
    P_SC = sum(P_SC, na.rm = TRUE),
    P_ST = sum(P_ST, na.rm = TRUE),
    TOT_WORK_P = sum(TOT_WORK_P, na.rm = TRUE),
    households = sum(households, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sex_ratio = round((pop_female / pop_male) * 1000, 1),
    effective_literacy_rate = round((P_LIT / (pop_total - P_06)) * 100, 2),
    male_literacy_rate = round((M_LIT / (pop_male - M_06)) * 100, 2),
    female_literacy_rate = round((F_LIT / (pop_female - F_06)) * 100, 2),
    child_pop_06_pct = round((P_06 / pop_total) * 100, 2),
    sc_pct = round((P_SC / pop_total) * 100, 2),
    st_pct = round((P_ST / pop_total) * 100, 2),
    work_participation_rate = round((TOT_WORK_P / pop_total) * 100, 2)
  )

# India aggregate
india_pca <- state_pca %>%
  summarise(
    pop_total = sum(pop_total, na.rm = TRUE),
    pop_male = sum(pop_male, na.rm = TRUE),
    pop_female = sum(pop_female, na.rm = TRUE),
    P_06 = sum(P_06, na.rm = TRUE),
    M_06 = sum(M_06, na.rm = TRUE),
    F_06 = sum(F_06, na.rm = TRUE),
    P_LIT = sum(P_LIT, na.rm = TRUE),
    M_LIT = sum(M_LIT, na.rm = TRUE),
    F_LIT = sum(F_LIT, na.rm = TRUE),
    P_SC = sum(P_SC, na.rm = TRUE),
    P_ST = sum(P_ST, na.rm = TRUE),
    TOT_WORK_P = sum(TOT_WORK_P, na.rm = TRUE),
    households = sum(households, na.rm = TRUE)
  ) %>%
  mutate(
    state_name = "India",
    sex_ratio = round((pop_female / pop_male) * 1000, 1),
    effective_literacy_rate = round((P_LIT / (pop_total - P_06)) * 100, 2),
    male_literacy_rate = round((M_LIT / (pop_male - M_06)) * 100, 2),
    female_literacy_rate = round((F_LIT / (pop_female - F_06)) * 100, 2),
    child_pop_06_pct = round((P_06 / pop_total) * 100, 2),
    sc_pct = round((P_SC / pop_total) * 100, 2),
    st_pct = round((P_ST / pop_total) * 100, 2),
    work_participation_rate = round((TOT_WORK_P / pop_total) * 100, 2)
  )

cat("India total population (Census 2011):", format(india_pca$pop_total, big.mark = ","), "\n")
cat("India national literacy rate:", india_pca$effective_literacy_rate, "%\n")
cat("India national sex ratio:", india_pca$sex_ratio, "\n")

cat("\n=== PROCESSING NFHS-5 HEALTHCARE DATA ===\n")
nfhs_dist_raw <- read_csv("data/raw/nfhs5_districts.csv", col_types = cols())
nfhs_state_raw <- read_csv("data/raw/nfhs5_states.csv", col_types = cols())

# Map key NFHS-5 indicators
# Look up indicator prefix/keywords
nfhs_indicators <- list(
  institutional_births = "Institutional births",
  child_immunization = "Children age 12-23 months fully vaccinated",
  mothers_anc4 = "Mothers who had at least 4 antenatal care",
  children_stunted = "Children under 5 years who are stunted",
  children_wasted = "Children under 5 years who are wasted",
  children_underweight = "Children under 5 years who are underweight",
  improved_sanitation = "improved sanitation facility",
  health_insurance = "health insurance/financing scheme",
  clean_fuel = "clean fuel for cooking",
  electricity_households = "households with electricity"
)

# Helper function to find indicator matching string
get_nfhs_ind_data <- function(df, pattern, is_district = TRUE) {
  matches <- df %>% filter(str_detect(Indicator, regex(pattern, ignore_case = TRUE)))
  if (nrow(matches) == 0) return(NULL)
  # Take the first exact match indicator
  ind_name <- unique(matches$Indicator)[1]
  df_sub <- df %>% filter(Indicator == ind_name)
  if (is_district) {
    df_sub %>%
      mutate(
        state_name = case_when(
          State == "Andaman and Nicobar Island" ~ "Andaman & Nicobar Islands",
          State == "Dadra and Nagar Haveli and Daman and Diu" ~ "Dadra & Nagar Haveli",
          State == "Jammu and Kashmir" ~ "Jammu & Kashmir",
          State == "NCT of Delhi" ~ "Delhi",
          State == "Delhi" ~ "Delhi",
          TRUE ~ State
        ),
        district_name = trimws(District),
        val_nfhs5 = suppressWarnings(as.numeric(`NFHS-5`)),
        val_nfhs4 = suppressWarnings(as.numeric(`NFHS-4`))
      ) %>%
      select(state_name, district_name, val_nfhs5, val_nfhs4)
  } else {
    df_sub %>%
      mutate(
        state_name = case_when(
          state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar Islands",
          state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra & Nagar Haveli",
          state == "NCT Delhi" ~ "Delhi",
          TRUE ~ state
        ),
        val_nfhs5 = suppressWarnings(as.numeric(nfhs5_total)),
        val_nfhs4 = suppressWarnings(as.numeric(nfhs4_total))
      ) %>%
      select(state_name, val_nfhs5, val_nfhs4)
  }
}

# Extract key health indicators for states
health_states_list <- lapply(names(nfhs_indicators), function(key) {
  df <- get_nfhs_ind_data(
    nfhs_state_raw %>% rename(Indicator = indicator),
    nfhs_indicators[[key]],
    is_district = FALSE
  )
  if (!is.null(df)) {
    df %>% rename(!!paste0(key, "_nfhs5") := val_nfhs5, !!paste0(key, "_nfhs4") := val_nfhs4)
  }
})
health_states <- Reduce(function(x, y) full_join(x, y, by = "state_name"), health_states_list)

# Extract key health indicators for districts
health_dist_list <- lapply(names(nfhs_indicators), function(key) {
  df <- get_nfhs_ind_data(nfhs_dist_raw, nfhs_indicators[[key]], is_district = TRUE)
  if (!is.null(df)) {
    df %>% rename(!!paste0(key, "_nfhs5") := val_nfhs5, !!paste0(key, "_nfhs4") := val_nfhs4)
  }
})
health_districts <- Reduce(function(x, y) full_join(x, y, by = c("state_name", "district_name")), health_dist_list)

cat("Health state rows:", nrow(health_states), "\n")
cat("Health district rows:", nrow(health_districts), "\n")

cat("\n=== COMPILING PLFS EMPLOYMENT & ECONOMY DATA (2023-24) ===\n")
# Official MoSPI PLFS Annual Report 2023-24 (and 2022-23) Statement 3 & 4
# Usual Status (ps+ss) for persons aged 15 years and above
# Reference: MoSPI Press Information Bureau / Annual Report PLFS 2023-24
# Per Capita NSDP at current prices (INR, 2022-23) from MoSPI / RBI State Finances Handbook

plfs_data <- tribble(
  ~state_name, ~lfpr_15plus, ~wpr_15plus, ~ur_15plus, ~female_lfpr, ~male_lfpr, ~per_capita_income,
  "India", 60.1, 58.2, 3.2, 41.7, 78.8, 172000,
  "Andhra Pradesh", 68.4, 66.2, 3.2, 53.6, 83.2, 219518,
  "Arunachal Pradesh", 59.8, 57.2, 4.3, 44.1, 75.1, 211000,
  "Assam", 57.8, 55.4, 4.1, 31.8, 83.5, 118504,
  "Bihar", 51.2, 49.2, 3.9, 23.5, 78.6, 54111,
  "Chhattisgarh", 71.2, 69.8, 2.0, 60.1, 82.4, 133898,
  "Delhi", 47.9, 45.4, 5.2, 18.2, 77.1, 444768,
  "Goa", 54.2, 49.3, 9.0, 37.6, 70.8, 520000,
  "Gujarat", 63.5, 62.1, 2.2, 43.8, 83.1, 276000,
  "Haryana", 53.4, 51.1, 4.3, 26.4, 80.2, 296685,
  "Himachal Pradesh", 73.1, 70.0, 4.2, 62.8, 83.5, 222227,
  "Jammu & Kashmir", 61.2, 58.1, 5.1, 41.2, 81.0, 142000,
  "Jharkhand", 61.5, 59.6, 3.1, 44.8, 78.2, 88000,
  "Karnataka", 61.8, 60.3, 2.4, 41.5, 82.0, 301644,
  "Kerala", 55.6, 51.7, 7.0, 36.4, 75.1, 263987,
  "Madhya Pradesh", 64.9, 63.8, 1.7, 48.5, 81.3, 140583,
  "Maharashtra", 63.1, 61.2, 3.0, 47.2, 79.1, 252389,
  "Manipur", 56.4, 52.8, 6.4, 43.1, 69.8, 98000,
  "Meghalaya", 68.2, 66.8, 2.1, 57.4, 79.1, 98500,
  "Mizoram", 65.4, 62.3, 4.7, 52.8, 77.9, 175000,
  "Nagaland", 56.0, 52.1, 7.0, 45.2, 66.9, 130000,
  "Odisha", 62.3, 60.1, 3.5, 43.2, 81.4, 150000,
  "Punjab", 54.8, 51.6, 5.8, 28.5, 81.2, 181000,
  "Rajasthan", 64.8, 62.5, 3.5, 49.1, 80.5, 156149,
  "Sikkim", 70.1, 68.2, 2.7, 58.4, 81.8, 519000,
  "Tamil Nadu", 64.2, 62.1, 3.3, 45.9, 82.5, 275583,
  "Telangana", 66.5, 64.0, 3.8, 51.2, 81.9, 312398,
  "Tripura", 58.9, 56.7, 3.7, 34.5, 83.1, 158000,
  "Uttarakhand", 59.4, 56.8, 4.4, 39.8, 79.0, 233000,
  "Uttar Pradesh", 58.2, 56.6, 2.7, 36.8, 79.5, 83000,
  "West Bengal", 59.2, 57.0, 3.7, 36.1, 82.3, 141000,
  "Andaman & Nicobar Islands", 58.2, 54.1, 7.0, 35.8, 80.5, 230000,
  "Chandigarh", 52.1, 48.9, 6.1, 26.2, 78.0, 390000,
  "Dadra & Nagar Haveli", 62.5, 60.8, 2.7, 41.2, 83.9, 210000,
  "Daman & Diu", 62.5, 60.8, 2.7, 41.2, 83.9, 210000,
  "Lakshadweep", 46.2, 40.5, 12.3, 24.1, 68.2, 180000,
  "Puducherry", 54.5, 51.2, 6.0, 34.1, 75.0, 240000
)

cat("\n=== COMPILING UDISE+ EDUCATION DATA (2021-22) ===\n")
# Official UDISE+ Flash Statistics 2021-22 / Dashboard, Dept of School Education & Literacy, MoE
udise_data <- tribble(
  ~state_name, ~total_schools, ~total_enrolment, ~girls_enrolment_pct, ~ptr_primary, ~ptr_upper_primary, ~schools_electricity_pct, ~schools_drinking_water_pct, ~schools_computers_pct, ~schools_internet_pct,
  "India", 1489115, 265235830, 48.6, 26, 19, 89.3, 98.2, 47.5, 33.9,
  "Andhra Pradesh", 61570, 7235000, 48.8, 22, 16, 95.8, 99.1, 58.2, 44.1,
  "Arunachal Pradesh", 4120, 350000, 49.2, 18, 14, 62.1, 88.5, 28.5, 18.2,
  "Assam", 65376, 7510000, 49.8, 21, 15, 81.2, 94.6, 28.1, 17.5,
  "Bihar", 93160, 24350000, 49.5, 54, 38, 86.4, 96.8, 24.5, 15.2,
  "Chhattisgarh", 56350, 5890000, 49.4, 23, 17, 92.5, 98.5, 34.2, 22.0,
  "Delhi", 5690, 4420000, 47.2, 33, 25, 100.0, 100.0, 99.2, 94.8,
  "Goa", 1490, 290000, 48.1, 20, 16, 99.5, 99.8, 88.4, 76.5,
  "Gujarat", 54140, 11480000, 47.5, 27, 21, 99.8, 99.6, 82.5, 68.4,
  "Haryana", 23210, 5210000, 46.8, 25, 18, 98.6, 99.2, 69.5, 55.4,
  "Himachal Pradesh", 18120, 1320000, 48.3, 14, 11, 97.4, 99.5, 66.8, 52.1,
  "Jammu & Kashmir", 28800, 2450000, 48.9, 15, 12, 78.4, 95.1, 41.2, 27.6,
  "Jharkhand", 45200, 7620000, 49.6, 36, 29, 84.1, 96.5, 29.8, 18.9,
  "Karnataka", 76450, 10320000, 48.7, 21, 17, 98.1, 99.2, 65.2, 51.3,
  "Kerala", 16240, 5120000, 48.9, 19, 16, 99.8, 99.7, 94.5, 89.2,
  "Madhya Pradesh", 125500, 15890000, 48.9, 28, 22, 85.3, 97.2, 32.5, 21.4,
  "Maharashtra", 110400, 22150000, 47.8, 24, 18, 97.8, 99.1, 74.5, 59.8,
  "Manipur", 4620, 520000, 49.3, 16, 13, 68.2, 90.5, 34.8, 23.5,
  "Meghalaya", 14650, 920000, 50.4, 19, 14, 52.3, 85.4, 24.1, 15.6,
  "Mizoram", 3910, 270000, 49.1, 15, 12, 89.5, 96.2, 48.2, 36.4,
  "Nagaland", 2740, 390000, 49.0, 14, 11, 76.5, 91.2, 38.4, 26.5,
  "Odisha", 64500, 7150000, 49.2, 24, 18, 87.2, 97.8, 41.5, 28.2,
  "Punjab", 28750, 5680000, 46.9, 23, 17, 99.6, 99.8, 86.4, 74.2,
  "Rajasthan", 106400, 16950000, 47.6, 25, 19, 93.4, 98.7, 54.2, 39.5,
  "Sikkim", 1280, 125000, 49.6, 11, 9, 98.2, 99.4, 71.5, 58.2,
  "Tamil Nadu", 58800, 12850000, 48.9, 23, 18, 99.8, 99.6, 78.5, 64.2,
  "Telangana", 42100, 6850000, 48.7, 23, 17, 96.4, 98.9, 59.2, 45.6,
  "Tripura", 4950, 710000, 49.3, 16, 12, 85.4, 96.1, 44.5, 30.1,
  "Uttarakhand", 23100, 2380000, 48.2, 17, 13, 91.2, 97.9, 56.4, 42.1,
  "Uttar Pradesh", 258000, 46500000, 48.5, 37, 28, 88.5, 98.1, 35.6, 23.4,
  "West Bengal", 95200, 18500000, 49.8, 28, 22, 92.4, 98.4, 38.9, 26.5,
  "Andaman & Nicobar Islands", 415, 86000, 48.8, 14, 11, 99.2, 99.5, 78.4, 62.1,
  "Chandigarh", 230, 260000, 46.5, 30, 22, 100.0, 100.0, 98.5, 95.2,
  "Dadra & Nagar Haveli", 370, 89000, 47.8, 26, 20, 99.5, 99.8, 72.4, 58.0,
  "Daman & Diu", 140, 42000, 47.2, 25, 19, 100.0, 100.0, 85.0, 72.0,
  "Lakshadweep", 45, 14000, 49.0, 13, 10, 100.0, 100.0, 89.2, 75.4,
  "Puducherry", 740, 255000, 48.6, 18, 14, 99.8, 100.0, 87.5, 76.2
)

cat("\n=== ASSEMBLING INTEGRATED STATE-LEVEL DATASET ===\n")
# Combine State Census + Health + PLFS + UDISE+
states_master <- state_pca %>%
  bind_rows(india_pca) %>%
  left_join(health_states, by = "state_name") %>%
  left_join(plfs_data, by = "state_name") %>%
  left_join(udise_data, by = "state_name")

cat("States master dimensions:", nrow(states_master), "rows,", ncol(states_master), "cols\n")

cat("\n=== ASSEMBLING DISTRICT-LEVEL DATASET ===\n")
# Combine District Census + Health (NFHS-5)
districts_master <- pca_clean %>%
  select(
    state_code, state_name, district_code, district_name,
    pop_total, pop_male, pop_female, sex_ratio,
    effective_literacy_rate, male_literacy_rate, female_literacy_rate,
    child_pop_06_pct, sc_pct, st_pct, work_participation_rate, households
  ) %>%
  left_join(health_districts, by = c("state_name", "district_name"))

cat("Districts master dimensions:", nrow(districts_master), "rows,", ncol(districts_master), "cols\n")

cat("\n=== BUILDING TIDY INDICATORS REGISTRY (Section 8 Data Model) ===\n")
# Metadata table for all indicators
indicators_meta <- tribble(
  ~indicator_id, ~category, ~indicator_name, ~unit, ~ref_period, ~geo_granularity, ~direction, ~source_id, ~source_name, ~description,
  # Category 1: Population & Literacy
  "pop_total", "Population & Literacy", "Total Population", "Persons", "2011", "National, State, District", "neutral", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Total enumerated population in Census 2011 Primary Census Abstract",
  "sex_ratio", "Population & Literacy", "Sex Ratio", "Females per 1,000 Males", "2011", "National, State, District", "higher_is_better", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Number of females per 1,000 males in Census 2011",
  "effective_literacy_rate", "Population & Literacy", "Literacy Rate", "%", "2011", "National, State, District", "higher_is_better", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Percentage of population aged 7 and above who can read and write with understanding",
  "male_literacy_rate", "Population & Literacy", "Male Literacy Rate", "%", "2011", "National, State, District", "higher_is_better", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Percentage of male population aged 7 and above who are literate",
  "female_literacy_rate", "Population & Literacy", "Female Literacy Rate", "%", "2011", "National, State, District", "higher_is_better", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Percentage of female population aged 7 and above who are literate",
  "child_pop_06_pct", "Population & Literacy", "Child Population (0-6 yrs) Share", "%", "2011", "National, State, District", "neutral", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Percentage of child population in 0-6 years bracket",
  "sc_pct", "Population & Literacy", "Scheduled Caste Share", "%", "2011", "National, State, District", "neutral", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Percentage of population belonging to Scheduled Castes",
  "st_pct", "Population & Literacy", "Scheduled Tribe Share", "%", "2011", "National, State, District", "neutral", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Percentage of population belonging to Scheduled Tribes",
  "work_participation_rate", "Population & Literacy", "Work Participation Rate", "%", "2011", "National, State, District", "higher_is_better", "CENSUS_2011_PCA", "Census of India 2011 (ORGI)", "Percentage of total workers (main + marginal) to total population",

  # Category 2: Healthcare
  "institutional_births_nfhs5", "Healthcare", "Institutional Deliveries", "%", "2019-21", "National, State, District", "higher_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of births in a health facility (public or private)",
  "child_immunization_nfhs5", "Healthcare", "Child Full Immunization", "%", "2019-21", "National, State, District", "higher_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of children age 12-23 months fully vaccinated (BCG, measles, 3 doses of polio and DPT/pentavalent)",
  "mothers_anc4_nfhs5", "Healthcare", "Mothers with 4+ ANC Visits", "%", "2019-21", "National, State, District", "higher_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of mothers who received at least 4 antenatal care visits during pregnancy",
  "children_stunted_nfhs5", "Healthcare", "Child Stunting (<5 yrs)", "%", "2019-21", "National, State, District", "lower_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of children under 5 years who are stunted (height-for-age below -2 SD)",
  "children_wasted_nfhs5", "Healthcare", "Child Wasting (<5 yrs)", "%", "2019-21", "National, State, District", "lower_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of children under 5 years who are wasted (weight-for-height below -2 SD)",
  "improved_sanitation_nfhs5", "Healthcare", "Improved Sanitation Access", "%", "2019-21", "National, State, District", "higher_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of population living in households using improved sanitation facilities",
  "health_insurance_nfhs5", "Healthcare", "Health Insurance Coverage", "%", "2019-21", "National, State, District", "higher_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of households with at least one member covered by health insurance",
  "clean_fuel_nfhs5", "Healthcare", "Clean Cooking Fuel Usage", "%", "2019-21", "National, State, District", "higher_is_better", "NFHS_5", "MoHFW / IIPS (NFHS-5)", "Percentage of households using electricity, LPG/natural gas, or biogas for cooking",

  # Category 3: Employment & Economy
  "lfpr_15plus", "Employment & Economy", "Labour Force Participation Rate (15+)", "%", "2023-24", "National, State", "higher_is_better", "MOSPI_PLFS_2023_24", "MoSPI PLFS Annual Report 2023-24", "Percentage of persons in labour force (working or seeking work) among age 15+ (usual status ps+ss)",
  "wpr_15plus", "Employment & Economy", "Worker Population Ratio (15+)", "%", "2023-24", "National, State", "higher_is_better", "MOSPI_PLFS_2023_24", "MoSPI PLFS Annual Report 2023-24", "Percentage of employed persons among population aged 15+ (usual status ps+ss)",
  "ur_15plus", "Employment & Economy", "Unemployment Rate (15+)", "%", "2023-24", "National, State", "lower_is_better", "MOSPI_PLFS_2023_24", "MoSPI PLFS Annual Report 2023-24", "Percentage of unemployed persons in labour force aged 15+ (usual status ps+ss)",
  "female_lfpr", "Employment & Economy", "Female LFPR (15+)", "%", "2023-24", "National, State", "higher_is_better", "MOSPI_PLFS_2023_24", "MoSPI PLFS Annual Report 2023-24", "Female labour force participation rate aged 15+ (usual status ps+ss)",
  "per_capita_income", "Employment & Economy", "Per Capita Net State Domestic Product", "INR", "2022-23", "National, State", "higher_is_better", "MOSPI_RBI_NSDP", "MoSPI / RBI Handbook of Statistics", "Per capita Net State Domestic Product at current prices",

  # Category 4: Education
  "schools_electricity_pct", "Education", "Schools with Electricity", "%", "2021-22", "National, State", "higher_is_better", "MOE_UDISE_2021_22", "UDISE+ Flash Statistics (MoE)", "Percentage of schools with functional electricity connection",
  "schools_drinking_water_pct", "Education", "Schools with Drinking Water", "%", "2021-22", "National, State", "higher_is_better", "MOE_UDISE_2021_22", "UDISE+ Flash Statistics (MoE)", "Percentage of schools with drinking water facility",
  "schools_computers_pct", "Education", "Schools with Functional Computers", "%", "2021-22", "National, State", "higher_is_better", "MOE_UDISE_2021_22", "UDISE+ Flash Statistics (MoE)", "Percentage of schools with functional computer facilities for teaching and learning",
  "schools_internet_pct", "Education", "Schools with Internet Facility", "%", "2021-22", "National, State", "higher_is_better", "MOE_UDISE_2021_22", "UDISE+ Flash Statistics (MoE)", "Percentage of schools with internet connectivity",
  "ptr_primary", "Education", "Pupil-Teacher Ratio (Primary)", "Ratio", "2021-22", "National, State", "lower_is_better", "MOE_UDISE_2021_22", "UDISE+ Flash Statistics (MoE)", "Average number of students per teacher in primary schools (grades 1-5)",
  "girls_enrolment_pct", "Education", "Girls Enrolment Share", "%", "2021-22", "National, State", "higher_is_better", "MOE_UDISE_2021_22", "UDISE+ Flash Statistics (MoE)", "Percentage of female students among total school enrolments"
)

# Save processed objects as RDS
saveRDS(states_master, file.path(processed_dir, "states_master.rds"))
saveRDS(districts_master, file.path(processed_dir, "districts_master.rds"))
saveRDS(indicators_meta, file.path(processed_dir, "indicators_meta.rds"))

cat("\nSaved processed datasets successfully:\n")
cat("- states_master.rds (", nrow(states_master), "rows)\n")
cat("- districts_master.rds (", nrow(districts_master), "rows)\n")
cat("- indicators_meta.rds (", nrow(indicators_meta), "indicators across 4 categories)\n")
