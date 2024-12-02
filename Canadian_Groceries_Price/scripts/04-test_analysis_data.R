#### Preamble ####
# Purpose: Tests the cleaned and aggregated grocery pricing analysis data for consistency, accuracy, and validity.
# Author: Amy Jin
# Date: 2024.12.1
# Contact: amyzh.jin@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `testthat` package must be installed. Ensure the cleaned data is saved as `cleaned_bundle_data.csv`.
# Any other information needed? Adjust the file path to match your local environment if necessary.



#### Workspace setup ####
library(tidyverse)
library(testthat)

data <- read_csv("data/02-analysis_data/analysis_data.csv")


#### Test data ####
test_that("Data has correct columns", {
  expected_columns <- c("nowtime", "vendor", "product_category", "avg_price", "min_price", "max_price")
  expect_true(all(expected_columns %in% colnames(analysis_data)))
})

test_that("No missing values in key columns", {
  key_columns <- c("nowtime", "vendor", "product_category", "avg_price")
  for (col in key_columns) {
    expect_false(any(is.na(analysis_data[[col]])), info = paste("Column", col, "contains NA values"))
  }
})

test_that("Dates are in correct format", {
  expect_true(all(!is.na(as.Date(analysis_data$nowtime))), info = "Invalid date format in 'nowtime'")
})

test_that("Product categories are limited to Eggs, Milk, Bread", {
  valid_categories <- c("Eggs", "Milk", "Bread")
  expect_true(all(analysis_data$product_category %in% valid_categories), info = "Unexpected product categories found")
})

test_that("Average prices are within a reasonable range", {
  expect_true(all(analysis_data$avg_price >= 0 & analysis_data$avg_price <= 100), info = "Unrealistic average prices detected")
})

test_that("Minimum prices are less than or equal to maximum prices", {
  expect_true(all(analysis_data$min_price <= analysis_data$max_price), info = "Minimum price exceeds maximum price")
})

test_that("Vendors are consistent with expected list", {
  valid_vendors <- c("Voila", "T&T", "Loblaws", "No Frills", "Metro", "Galleria", "Walmart Canada", "Save-On-Foods")
  expect_true(all(analysis_data$vendor %in% valid_vendors), info = "Unexpected vendors found")
})

# Run the tests
test_results <- test_dir(tempdir(), reporter = "summary")
print(test_results)
