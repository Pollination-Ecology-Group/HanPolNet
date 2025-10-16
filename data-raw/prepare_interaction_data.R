## code to prepare `interaction_data` dataset

library(dplyr)
library(stringr) # For trimming whitespace

load("data/plant_metadata.rda")

raw_interactions <- read.csv("data-raw/jine_ctverce_spojene_11_24_vz_20250515 - jine_ctverce_spojene_11_24_vz_20250409.csv")

# Select and Rename Columns
interaction_data <- raw_interactions %>%
  select(
    sampling_id = id_i, year = rok, month = mesic, day = den, hour = hod,
    minute = min, plot_id = osnova, plant_name = druhK_opraveno,
    pollinator_id_final = druh_final, pollinator_id_corrected = druhO_opraveno,
    interaction_count = pocet, experiment_run = beh, sex = pohlavi, shade = stin
  )

interaction_data <- interaction_data %>%
  mutate(
    # Clean plant names before the join
    plant_name = gsub("_", " ", plant_name),
    plant_name = str_trim(plant_name),

    # --- ADD MORE SPECIFIC FIXES HERE ---
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

    # Handle pollinator IDs and convert interaction_count
    pollinator_id_final = if_else(pollinator_id_final == "", NA_character_, as.character(pollinator_id_final)),
    pollinator_id = coalesce(pollinator_id_final, as.character(pollinator_id_corrected)),
    interaction_count = as.numeric(interaction_count)
  ) %>%
  # Filter out junk entries that are not plant names
  filter(!plant_name %in% c( "indet", "UNDETERMINED trifolium", "Pol sp", "Per pin", "Ing. Petr Krása"))
# ... (the rest of the script remains the same) ...

# Join with Plant Metadata
interaction_data <- interaction_data %>%
  left_join(plant_metadata, by = "plant_name")

# Finalize and Check
interaction_data <- interaction_data %>%
  select(
    sampling_id, year, month, day, hour, minute, plot_id,
    plant_code, plant_name, pollinator_id, interaction_count,
    experiment_run, sex, shade
  )

unmatched_plants <- interaction_data %>%
  filter(is.na(plant_code)) %>%
  distinct(plant_name)

unmatched_plants_filtered <- unmatched_plants %>%
  filter(plant_name != "" & !is.na(plant_name))

if (nrow(unmatched_plants_filtered) > 0) {
  unmatched_names_str <- paste(unmatched_plants_filtered$plant_name, collapse = ", ")
  warning(paste0("The following plant names still did not have a match: ", unmatched_names_str))
}

usethis::use_data(interaction_data, overwrite = TRUE)

message("✅ `interaction_data.rda` has been created and saved to the data/ folder.")
