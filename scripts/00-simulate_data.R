#### Preamble ####
# Purpose: Simulates a dataset of grocery product information, including vendors, brands, and pricing.
# Author: Amy Jin
# Date: 2024.12.1
# Contact: amyzh.jin@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `starter_folder` rproj

#### Workspace setup ####
library(tidyverse)
library(MASS)
set.seed(56)

#### Simulate data ####
# Define parameters
n_days <- 100  # Number of days for simulation
mean_prices <- c(4.5, 3.0, 2.0)  # Mean prices for eggs, milk, bread
price_sd <- c(0.5, 0.3, 0.2)  # Standard deviations for each product
correlation_matrix <- matrix(c(
  1, 0.8, 0.7,
  0.8, 1, 0.6,
  0.7, 0.6, 1
), ncol = 3)  # Correlation structure between products

# Generate correlated price data
price_data <- mvrnorm(
  n = n_days, 
  mu = mean_prices, 
  Sigma = diag(price_sd) %*% correlation_matrix %*% diag(price_sd)
)

# Convert to data frame
price_data <- as.data.frame(price_data)
colnames(price_data) <- c("Eggs", "Milk", "Bread")
price_data$Day <- 1:n_days


#### Save data ####
write_csv(price_data, "data/00-simulated_data/simulated_grocery_data.csv")
