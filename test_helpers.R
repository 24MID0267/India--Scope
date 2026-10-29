# tests/testthat/test_helpers.R
# Automated unit tests for IndiaScope helper functions and data loaders

library(testthat)
suppressPackageStartupMessages(library(dplyr))
source("../../R/helpers.R")
source("../../R/data_loader.R")

test_that("normalize_district_name cleans strings and applies aliases", {
  expect_equal(normalize_district_name("Thoothukudi"), "thoothukkudi")
  expect_equal(normalize_district_name("Nilgiris"), "the nilgiris")
  expect_equal(normalize_district_name("  BANGALORE  "), "bangalore urban")
  expect_equal(normalize_district_name("Leh(Ladakh)"), "leh")
  expect_equal(normalize_district_name("South 24 Parganas"), "south 24 parganas")
  expect_equal(normalize_district_name(""), "")
  expect_equal(normalize_district_name(NULL), "")
})

test_that("match_district matches exact, normalized, and alias names", {
  targets <- c("Chennai", "Coimbatore", "Thoothukkudi", "The Nilgiris", "Madurai")
  
  # Exact match
  expect_equal(match_district("Chennai", targets), "Chennai")
  expect_equal(match_district("chennai", targets), "Chennai")
  
  # Alias / normalized match
  expect_equal(match_district("Thoothukudi", targets), "Thoothukkudi")
  expect_equal(match_district("Nilgiris", targets), "The Nilgiris")
  
  # Non-existent district returns NA
  expect_true(is.na(match_district("NonExistentDistrict123", targets)))
})

test_that("format_indicator_value formats numbers and units properly", {
  # Percentages
  expect_equal(format_indicator_value(72.984, "%"), "73.0%")
  expect_equal(format_indicator_value(100, "%"), "100.0%")
  
  # Currency
  expect_equal(format_indicator_value(275000, "INR"), "₹2.75 Lakh")
  expect_equal(format_indicator_value(50000, "INR"), "₹50,000")
  
  # Counts
  expect_true(grepl("Cr", format_indicator_value(1210854977, "Persons")))
  expect_true(grepl("Lakh", format_indicator_value(500000, "Persons")))
  
  # Missing / NA
  expect_equal(format_indicator_value(NA, "%"), "Data Unavailable")
  expect_equal(format_indicator_value(NULL, "INR"), "Data Unavailable")
})

test_that("Data loader retrieves verified datasets and national totals", {
  load_all_data("../../data/processed")
  
  # National data
  nat <- get_national_data()
  expect_equal(nrow(nat), 1)
  expect_equal(unname(as.character(nat$state_name)), "India")
  expect_true(nat$pop_total > 1000000000)
  expect_true(nat$effective_literacy_rate > 70)
  
  # State data
  states <- get_states_data()
  expect_true(nrow(states) >= 35)
  expect_true("Tamil Nadu" %in% states$state_name)
  expect_true("Maharashtra" %in% states$state_name)
  
  # District data
  tn_dist <- get_districts_data("Tamil Nadu")
  expect_true(nrow(tn_dist) >= 30)
  expect_true("Chennai" %in% tn_dist$district_name)
  
  # Indicator metadata
  meta <- get_indicators_meta()
  expect_true(nrow(meta) >= 20)
  expect_true("effective_literacy_rate" %in% meta$indicator_id)
})
