#### Preamble ####
# Purpose: Analyze whether prices on bundled products (Eggs, Milk, Bread) move in tandem at a single grocer.
# Author: Amy Jin
# Date: 2024.12.1
# Contact: amyzh.jin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The `tidyverse`, `rstanarm`, and `vars` packages must be installed.
#   - Cleaned and aggregated grocery data must be saved as "cleaned_bundle_data.csv".
#   - Ensure RStan is properly configured for Bayesian modeling.
# Any other information needed? 
#   - Adjust file paths as necessary to match your local project directory structure.

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vars)

#### Read data ####
# Load the cleaned and aggregated dataset
data <- read.csv("data/02-analysis_data/analysis_data.csv")  # Replace with your file path

# Filter and prepare data for a single vendor (e.g., Loblaws)
vendor_data <- data %>%
  filter(vendor == "Loblaws") %>%  # Replace with desired vendor
  select(nowtime, product_category, avg_price) %>%
  pivot_wider(names_from = product_category, values_from = avg_price) %>%
  arrange(as.Date(nowtime))

# Check for missing values and handle them
vendor_data <- vendor_data %>%
  mutate(across(c(Eggs, Milk, Bread), ~ if_else(is.na(.), mean(., na.rm = TRUE), .)))  # Fill NAs with column means

#### Correlation Analysis ####
# Compute correlations between products
cor_matrix <- cor(vendor_data %>% select(Eggs, Milk, Bread), use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)

#### Time Series Modeling: Vector Autoregression (VAR) ####
# Prepare data for VAR model
time_series_data <- vendor_data %>%
  select(Eggs, Milk, Bread) %>%
  as.matrix()

# Fit VAR model
var_model <- VAR(time_series_data, p = 1, type = "const")  # p is the lag order
summary(var_model)

# Impulse Response Analysis: Examine interactions between prices
irf <- irf(var_model, impulse = "Eggs", response = c("Milk", "Bread"), n.ahead = 10)
plot(irf)

#### Bayesian Modeling ####
# Convert long format data for Bayesian regression
long_vendor_data <- vendor_data %>%
  pivot_longer(cols = c(Eggs, Milk, Bread), names_to = "product", values_to = "price") %>%
  mutate(product = factor(product))

# Fit Bayesian model to examine price dependencies
bayesian_model <- stan_glm(
  formula = price ~ product + as.Date(nowtime),
  data = long_vendor_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 1113
)

#### Save model ####
saveRDS(
  bayesian_model,
  file = "models/bayesian_bundle_model.rds"
)

#### Model Summary ####
print(summary(bayesian_model))

#### Visualization ####
# Plot normalized prices over time
vendor_data_long <- vendor_data %>%
  pivot_longer(cols = c(Eggs, Milk, Bread), names_to = "Product", values_to = "Price")

ggplot(vendor_data_long, aes(x = as.Date(nowtime), y = Price, color = Product)) +
  geom_line(size = 1) +
  labs(
    title = "Price Trends for Eggs, Milk, and Bread at Loblaws",
    x = "Date",
    y = "Price ($)",
    color = "Product"
  ) +
  theme_minimal()
