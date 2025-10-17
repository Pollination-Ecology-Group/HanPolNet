## code to prepare `interaction_data` dataset

library(dplyr)
library(stringr)

load("data/plant_metadata.rda")

raw_interactions <- read.csv("data-raw/jine_ctverce_spojene_11_24_vz_20250515 - jine_ctverce_spojene_11_24_vz_20250409.csv")

# --- 1. Select and Rename Columns ---
interaction_data <- raw_interactions %>%
  select(
    sampling_id = id_i, year = rok, month = mesic, day = den, hour = hod,
    minute = min, plot_id = osnova, plant_name = druhK_opraveno,
    pollinator_id_final = druh_final, pollinator_id_corrected = druhO_opraveno,
    interaction_count = pocet, experiment_run = beh, sex = pohlavi, shade = stin
  )

# --- 2. Clean and Standardize All Columns ---
interaction_data <- interaction_data %>%
  mutate(
    # Ensure plot_id is numeric
    plot_id = as.numeric(as.character(plot_id)),

    # Clean plant names: underscores, whitespace, and specific typos
    plant_name = gsub("_", " ", plant_name),
    plant_name = str_trim(plant_name),
    plant_name = case_when(
      plant_name == "Cirisum arvense" ~ "Cirsium arvense",
      plant_name == "Epilobium angustifolia" ~ "Epilobium angustifolium",
      plant_name == "Kanutia arvensis" ~ "Knautia arvensis",
      plant_name == "Pimpinella saxifraga" ~ "Pimpinella saxifraga s.str.",
      plant_name == "Galium album" ~ "Galium album s.lat.",
      plant_name == "Leucanthemum vulgare" ~ "Leucanthemum vulgare agg.",
      plant_name == "Myosotis palustris" ~ "Myosotis palustris agg.",
      plant_name == "Lychnis floscuculi" ~ "Lychnis flos-cuculi",
      TRUE ~ plant_name
    ),

    # Convert "nothing" records and junk to NA for plants
    plant_name = if_else(plant_name %in% c("nic", "indet", "UNDETERMINED trifolium", "Pol sp", "Per pin", "Ing. Petr Krása"), NA_character_, plant_name),

    # Handle pollinator IDs
    pollinator_id_final = if_else(pollinator_id_final == "", NA_character_, as.character(pollinator_id_final)),
    pollinator_id = coalesce(pollinator_id_final, as.character(pollinator_id_corrected)),

    # Convert "nothing" records to NA for pollinators
    pollinator_id = if_else(pollinator_id %in% c("nic", "indet"), NA_character_, pollinator_id),

    # Ensure interaction_count is numeric
    interaction_count = as.numeric(interaction_count)
  )

# --- 3. Join with Plant Metadata ---
interaction_data <- interaction_data %>%
  left_join(plant_metadata, by = "plant_name")

# --- 4. Finalize Column Order and Check for Errors ---
interaction_data <- interaction_data %>%
  select(
    sampling_id, year, month, day, hour, minute, plot_id,
    plant_code, plant_name, pollinator_id, interaction_count,
    experiment_run, sex, shade
  )

unmatched_plants <- interaction_data %>%
  filter(is.na(plant_code) & !is.na(plant_name)) %>% # Only warn if plant_name was not NA
  distinct(plant_name)

if (nrow(unmatched_plants) > 0) {
  unmatched_names_str <- paste(unmatched_plants$plant_name, collapse = ", ")
  warning(paste0("The following plant names still did not have a match: ", unmatched_names_str))
}

# --- 5. Save to Package ---
usethis::use_data(interaction_data, overwrite = TRUE)

message("✅ `interaction_data.rda` has been created and saved (with zero-interaction rows included as NAs).")
