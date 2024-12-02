#### Preamble ####
# Purpose: Tests the structure and validity of the simulated grocery dataset.
# Author: Amy Jin
# Date: 2024.12.1
# Contact: amyzh.jin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - The grocery data simulation script must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj

#### Workspace setup ####
library(tidyverse)

grocery_data <- read_csv("data/00-simulated_data/simulated_grocery_data.csv")

# Test if the data was successfully loaded
# Load necessary libraries
library(testthat)

# Parameters to test against
expected_means <- mean_prices
expected_sd <- price_sd
expected_correlation_matrix <- correlation_matrix

# Perform Tests
test_that("Simulated data has correct dimensions", {
  expect_equal(nrow(price_data), n_days)  # Check number of rows
  expect_equal(ncol(price_data), 4)       # Check number of columns (Eggs, Milk, Bread, Day)
})

test_that("Means of simulated data are close to expected values", {
  calculated_means <- colMeans(price_data[, -4])  # Exclude "Day" column
  for (i in 1:3) {
    expect_equal(calculated_means[i], expected_means[i], tolerance = 0.1)  # Allow slight deviations
  }
})

test_that("Standard deviations of simulated data are close to expected values", {
  calculated_sd <- apply(price_data[, -4], 2, sd)
  for (i in 1:3) {
    expect_equal(calculated_sd[i], expected_sd[i], tolerance = 0.1)
  }
})

test_that("Correlation matrix matches expected correlations", {
  calculated_correlation <- cor(price_data[, -4])  # Exclude "Day" column
  for (i in 1:3) {
    for (j in 1:3) {
      expect_equal(calculated_correlation[i, j], expected_correlation_matrix[i, j], tolerance = 0.1)
    }
  }
})

test_that("Prices remain within realistic ranges", {
  for (col in c("Eggs", "Milk", "Bread")) {
    expect_true(all(price_data[[col]] > 0))  # Prices should be positive
  }
})

# Run the tests
test_results <- test_dir(tempdir(), reporter = "summary")
print(test_results)
