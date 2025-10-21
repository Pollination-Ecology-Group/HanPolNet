## code to prepare `pollen_network_data` dataset
library(dplyr)
library(tidyr)
library(stringr)

# Read the raw data
pollen_raw <- read.csv("data-raw/Total_deposition_22_24.csv", check.names = FALSE)

# Clean and pivot to long format
pollen_network_data <- pollen_raw %>%
  # Rename the first column to be consistent
  rename(stigma_id = ID) %>%
  # Pivot from wide (one column per plant) to long format
  pivot_longer(
    cols = -stigma_id,
    names_to = "plant_code",
    values_to = "pollen_count"
  ) %>%
  # Remove rows where no pollen was found
  filter(pollen_count > 0 & !is.na(pollen_count)) %>%
  # Extract year and inflorescence ID from the stigma ID
  mutate(
    year = as.integer(str_extract(stigma_id, "^\\d{2}")),
    inflorescence_id = str_extract(stigma_id, "Suc_B\\d+")
  ) %>%
  select(stigma_id, year, inflorescence_id, plant_code, pollen_count)

# Save the final .rda object to the data/ folder
usethis::use_data(pollen_network_data, overwrite = TRUE)

message("✅ `pollen_network_data.rda` has been created and saved.")
