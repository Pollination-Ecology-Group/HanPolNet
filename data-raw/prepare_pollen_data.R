## code to prepare `pollen_deposition` dataset
library(dplyr)
library(stringr)

# Read the raw data
pollen_raw <- read.csv("data-raw/pollen_deposition_suc_pra.csv")

# Clean and structure the data
pollen_deposition <- pollen_raw %>%
  mutate(
    # Extract the inflorescence ID (e.g., "B1", "B2") from the main id
    inflorescence_id = str_extract(id, "Suc_B\\d+") %>% str_remove("Suc_"),
    # Ensure year and day are numeric/integer
    year = as.integer(year),
    day = as.integer(day)
  ) %>%
  # Select and reorder columns
  select(
    id,
    inflorescence_id,
    plant_species,
    conspecific_pollen,
    heterospecific_pollen,
    pollen_total,
    year,
    day
  )

# Save the final .rda object to the data/ folder
usethis::use_data(pollen_deposition, overwrite = TRUE)

message("✅ `pollen_deposition.rda` has been created and saved to the data/ folder.")
