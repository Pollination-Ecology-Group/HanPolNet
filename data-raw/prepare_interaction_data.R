## code to prepare `interaction_data` dataset

# --- 1. Load Libraries and Existing Data ---
library(dplyr)

load("data/plant_metadata.rda")

# --- 2. Read and Select Raw Interaction Data ---
raw_interactions <- read.csv("data-raw/jine_ctverce_spojene_11_24_vz_20250515 - jine_ctverce_spojene_11_24_vz_20250409.csv")

# Select and rename the columns we need for the final dataset
interaction_data <- raw_interactions %>%
  select(
    sampling_id = id_i, # <-- CORRECTED this line
    year = rok,
    month = mesic,
    day = den,
    hour = hod,
    minute = min,
    plot_id = osnova,
    plant_name = druhK_opraveno,
    pollinator_id_final = druh_final,
    pollinator_id_corrected = druhO_opraveno,
    interaction_count = pocet,
    experiment_run = beh,
    sex = pohlavi,
    shade = stin
  )

# --- 3. Clean the Data ---
interaction_data <- interaction_data %>%
  mutate(
    pollinator_id = coalesce(pollinator_id_final, pollinator_id_corrected)
  )

# --- 4. Join with Plant Metadata ---
interaction_data <- interaction_data %>%
  left_join(plant_metadata, by = c("plant_name" = "plant_name"))

# --- 5. Finalize and Check ---
interaction_data <- interaction_data %>%
  select(
    sampling_id,
    year,
    month,
    day,
    hour,
    minute,
    plot_id,
    plant_code,
    plant_name,
    pollinator_id,
    interaction_count,
    experiment_run,
    sex,
    shade
  )

unmatched_plants <- interaction_data %>%
  filter(is.na(plant_code)) %>%
  distinct(plant_name)

if (nrow(unmatched_plants) > 0) {
  warning("The following plant names in the interaction data did not have a match in plant_metadata and have NA for plant_code:")
  print(unmatched_plants)
}

# --- 6. Save to Package ---
usethis::use_data(interaction_data, overwrite = TRUE)

message("✅ `interaction_data.rda` has been created and saved to the data/ folder.")
