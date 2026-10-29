# R/helpers.R
# Utility and helper functions for IndiaScope

library(stringr)
library(scales)

# Common district name variations between official census/survey tables and GIS boundaries
DISTRICT_ALIASES <- list(
  # Tamil Nadu
  "thoothukudi" = "thoothukkudi",
  "nilgiris" = "the nilgiris",
  "tiruchchirappalli" = "tiruchirappalli",
  "tirunelveli kattabo" = "tirunelveli",
  "villupuram" = "viluppuram",
  "kanyakumari" = "kanniyakumari",
  "kanchipuram" = "kancheepuram",
  # Karnataka
  "bangalore" = "bangalore urban",
  "bangalore rural" = "bengaluru rural",
  "belgaum" = "belagavi",
  "bellary" = "ballari",
  "bijapur" = "vijayapura",
  "chikmagalur" = "chikkamagaluru",
  "gulbarga" = "kalaburagi",
  "mysore" = "mysuru",
  "shimoga" = "shivamogga",
  "tumkur" = "tumakuru",
  # Kerala
  "kasaragod" = "kasargod",
  # Maharashtra
  "ahmadnagar" = "ahmednagar",
  "bid" = "beed",
  "buldana" = "buldhana",
  "gondiya" = "gondia",
  "mumbai suburban" = "mumbai suburban",
  "raigarh" = "raigad",
  # Odisha
  "baleshwar" = "balasore",
  "baudh" = "boudh",
  "debagarh" = "deogarh",
  "jajapur" = "jajpur",
  # West Bengal
  "barddhaman" = "bardhaman",
  "hugli" = "hooghly",
  "haora" = "howrah",
  "koch bihar" = "cooch behar",
  "maldah" = "malda",
  "north twenty four parganas" = "north 24 parganas",
  "south twenty four parganas" = "south 24 parganas",
  "purba medinipur" = "east midnapore",
  "paschim medinipur" = "west midnapore",
  # Uttar Pradesh
  "bara banki" = "barabanki",
  "kheri" = "lakhimpur kheri",
  "sant ravidas nagar" = "bhadohi",
  "jyotiba phule nagar" = "amroha",
  "kanshiram nagar" = "kasganj",
  "mahamaya nagar" = "hathras",
  # Uttarakhand
  "garhwal" = "pauri garhwal",
  "hardwar" = "haridwar",
  # Gujarat
  "dohad" = "dahod",
  "panch mahals" = "panchmahal",
  "sabar kantha" = "sabarkantha",
  "banas kantha" = "banaskantha",
  "the dangs" = "dang",
  # Andhra Pradesh / Telangana
  "cuddapah" = "ysr",
  "visakhapatnam" = "visakhapatnam",
  "rangareddi" = "ranga reddy",
  # J&K / Ladakh
  "leh ladakh" = "leh",
  "badgam" = "budgam",
  "shupiyan" = "shopian",
  "punch" = "poonch"
)

#' Normalize district name for robust matching across data sources and map layers
#' @param name Character string of district name
#' @return Normalized lowercase clean string
normalize_district_name <- function(name) {
  if (is.null(name) || is.na(name) || length(name) == 0) return("")
  # Convert to lowercase, remove punctuation and extra spaces
  clean <- tolower(trimws(name))
  clean <- str_replace_all(clean, "[[:punct:]]", " ")
  clean <- str_squish(clean)
  
  # Check alias dictionary
  if (clean %in% names(DISTRICT_ALIASES)) {
    return(DISTRICT_ALIASES[[clean]])
  }
  clean
}

#' Match a query district against a list of target districts using exact and fuzzy matching
#' @param query Character query name
#' @param targets Vector of candidate district names
#' @return The matched element from targets or NA_character_
match_district <- function(query, targets) {
  if (is.null(query) || is.na(query) || length(query) == 0) return(NA_character_)
  if (length(targets) == 0) return(NA_character_)
  
  # 1. Exact case-insensitive match
  exact_idx <- which(tolower(targets) == tolower(query))
  if (length(exact_idx) > 0) return(targets[exact_idx[1]])
  
  # 2. Normalized match
  norm_query <- normalize_district_name(query)
  norm_targets <- sapply(targets, normalize_district_name, USE.NAMES = FALSE)
  norm_idx <- which(norm_targets == norm_query)
  if (length(norm_idx) > 0) return(targets[norm_idx[1]])
  
  # 3. Substring match
  sub_idx <- which(str_detect(norm_targets, fixed(norm_query)) | str_detect(norm_query, fixed(norm_targets)))
  if (length(sub_idx) > 0) return(targets[sub_idx[1]])
  
  # Return original if no match
  NA_character_
}

#' Format indicator value based on indicator type and unit
#' @param val Numeric value
#' @param unit Character unit ("%", "Persons", "INR", "Ratio", etc.)
#' @return Formatted character string
format_indicator_value <- function(val, unit = "") {
  if (is.null(val) || is.na(val) || length(val) == 0) {
    return("Data Unavailable")
  }
  
  if (unit == "%") {
    return(paste0(formatC(val, format = "f", digits = 1), "%"))
  } else if (unit == "INR") {
    if (val >= 100000) {
      return(paste0("₹", formatC(val / 100000, format = "f", digits = 2), " Lakh"))
    } else {
      return(paste0("₹", format(round(val), big.mark = ",")))
    }
  } else if (unit %in% c("Persons", "Count", "")) {
    if (val >= 10000000) {
      return(paste0(formatC(val / 10000000, format = "f", digits = 2), " Cr (", format(val, big.mark = ","), ")"))
    } else if (val >= 100000) {
      return(paste0(formatC(val / 100000, format = "f", digits = 2), " Lakh (", format(val, big.mark = ","), ")"))
    } else {
      return(format(val, big.mark = ","))
    }
  } else if (unit == "Ratio" || str_detect(unit, "per 1,000")) {
    return(formatC(val, format = "f", digits = 1))
  } else {
    return(paste(format(val, big.mark = ","), unit))
  }
}

#' Generate color palette function for choropleth mapping
#' @param values Numeric vector
#' @param direction "higher_is_better", "lower_is_better", "neutral"
#' @return Leaflet colorNumeric function
get_choropleth_palette <- function(values, direction = "higher_is_better") {
  vals_clean <- values[!is.na(values)]
  if (length(vals_clean) == 0) {
    vals_clean <- c(0, 100)
  }
  
  # Palette choices based on indicator direction
  palette_colors <- if (direction == "higher_is_better") {
    c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c") # Teal / Emerald
  } else if (direction == "lower_is_better") {
    c("#006d2c", "#2ca25f", "#fe9929", "#d95f0e", "#990000") # Green to Red
  } else {
    c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c") # Classic Deep Blue
  }
  
  leaflet::colorNumeric(
    palette = palette_colors,
    domain = c(min(vals_clean), max(vals_clean)),
    na.color = "#d9d9d9"
  )
}
