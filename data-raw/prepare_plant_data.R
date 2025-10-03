## code to prepare `plant_abundance` and `plant_metadata` datasets

# --- 1. Load the raw data ---
# We read the CSV files from their location in data-raw.
# The check.names = FALSE is often useful for column names that might be
# non-standard R names (though yours look fine).
plant_abundance_raw <- read.csv("data-raw/osnovy11_25.csv", row.names = 1, check.names = FALSE)
plant_metadata <- read.csv("data-raw/plant_code_table.csv", row.names = 1)


# --- 2. Clean and process the abundance data ---
# The row names 'yy-pp' are useful, but it's much better practice
# to have 'year' and 'plot_id' as separate columns for easier filtering.
# Let's create them!

# Keep the original row names for a moment
plot_year_id <- rownames(plant_abundance_raw)

# Separate the string into two parts at the hyphen
plot_year_split <- strsplit(plot_year_id, "-")

# Create the new columns
year <- as.integer(sapply(plot_year_split, `[`, 1))
plot_id <- as.integer(sapply(plot_year_split, `[`, 2))

# Combine into a new, clean data frame. Let's call it plant_abundance.
# We add the year and plot_id as the first two columns.
plant_abundance <- cbind(year = year, plot_id = plot_id, plant_abundance_raw)

# Good practice: remove the row names now that we have them as columns
rownames(plant_abundance) <- NULL

# --- 3. Save the clean data to the `data/` directory ---
# This is the magic step! `usethis::use_data()` will save the R objects
# you name (plant_abundance, plant_metadata) as compressed .rda files in the data/ folder.
# The overwrite = TRUE is there in case you need to re-run this script later.
usethis::use_data(plant_abundance, plant_metadata, overwrite = TRUE)
