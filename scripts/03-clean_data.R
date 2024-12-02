#### Preamble ####
# Purpose: Simulates a dataset of grocery product information, including vendors, brands, and pricing.
# Author: Amy Jin
# Date: 2024.12.1
# Contact: amyzh.jin@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed.
# Any other information needed? Make sure you are in the `starter_folder` R project.


#### Workspace setup ####
library(tidyverse)

#### Clean data ####
# Load required libraries
library(dplyr)
library(readr)
library(stringr)

# Load the data
product_data <- read_csv("data/01-raw_data/hammer-4-product.csv")  # Metadata and product details
raw_data <- read_csv("data/01-raw_data/hammer-4-raw.csv")          # Time-series price data

# Join the product and raw data on `id` and `product_id`
merged_data <- raw_data %>%
  inner_join(product_data, by = c("product_id" = "id"))

merged_data <- merged_data %>%
  mutate(
    current_price = gsub("[^0-9\\.]", "", current_price),  # Remove non-numeric characters
    current_price = as.numeric(current_price)  # Convert to numeric
  )

# Check for conversion issues
if (any(is.na(merged_data$current_price))) {
  warning("Some `current_price` values could not be converted to numeric. Check the data for inconsistencies.")
}
# Filter data for the specific products: Eggs, Milk, Bread
bundle_keywords <- c("egg", "milk", "bread")

# Inspect column names in merged_data
print(colnames(merged_data))

cleaned_data <- merged_data %>%
  filter(
    str_detect(tolower(product_name), paste(bundle_keywords, collapse = "|"))
  ) %>%
  dplyr::select(
    nowtime, vendor, product_name, brand, current_price, units
  ) %>%
  mutate(
    nowtime = as.Date(nowtime),  # Convert timestamp to date
    product_category = case_when(
      str_detect(tolower(product_name), "egg") ~ "Eggs",
      str_detect(tolower(product_name), "milk") ~ "Milk",
      str_detect(tolower(product_name), "bread") ~ "Bread",
      TRUE ~ "Other"
    )
  )



# Check for missing or inconsistent data
cleaned_data <- cleaned_data %>%
  filter(!is.na(current_price))  # Remove rows with missing prices


# Aggregate data by date, vendor, and product category
aggregated_data <- cleaned_data %>%
  group_by(nowtime, vendor, product_category) %>%
  summarize(
    avg_price = mean(current_price, na.rm = TRUE),
    min_price = min(current_price, na.rm = TRUE),
    max_price = max(current_price, na.rm = TRUE),
    .groups = "drop"
  )


#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/analysis_data.csv")
