#' Get and filter plant abundance data
#'
#' This function retrieves the plant abundance data, allowing for filtering and
#' standardization of abundance values to make different survey methods comparable.
#'
#' @details
#' The `output = "standardized"` option is designed to place all abundance
#' values onto a common 0-1 scale, which is useful for calculating indices or
#' comparing across species surveyed with different methods. The standardization
#' is performed as follows:
#'
#' \itemize{
#'   \item **Sublot Counts:** For species measured on the 0-64 presence/absence
#'     scale (`stalk_counted == 0`), values are converted to a proportion by
#'     dividing by 64. A value of 64 becomes 1.0 (maximum frequency).
#'   \item **Stalk Counts:** For species measured by direct stalk counts
#'     (`stalk_counted == 1`), values are scaled relative to the maximum abundance
#'     observed for that specific species across the entire dataset. Each value is
#'     divided by that species' global maximum, resulting in a scale where the
#'     highest observed count for that species is 1.0.
#' }
#'
#' @param years A numeric vector of years to include. If NULL, all years are returned.
#' @param species A character vector of species codes to include. If NULL, all species are returned.
#' @param counted_stalks A logical value. If TRUE, returns only species where stalks were counted.
#' If FALSE, returns only species where stalks were not counted (presence/absence).
#' If NULL (the default), returns all species regardless of counting method.
#' @param output A character string specifying the output format. Can be one of
#'   `"raw"` (the default) or `"standardized"`. See Details section.
#'
#' @return A data.frame containing the filtered plant abundance data. If `output = "standardized"`,
#'   all abundance values are scaled to a 0-1 range.
#'
#' @export
#'
#' @examples
#' # Get raw data for the year 2011
#' d11_raw <- get_plant_data(years = 11, output = "raw")
#'
#' # Get standardized data for all years and view summary
#' d_std <- get_plant_data(output = "standardized")
#' summary(d_std$Ach_mil) # Values will be between 0 and 1
#'
get_plant_data <- function(years = NULL, species = NULL, counted_stalks = NULL, output = "raw") {

  # --- Initial Filtering Logic (same as before) ---
  filtered_abundance <- plant_abundance
  target_species <- plant_metadata

  if (!is.null(counted_stalks)) {
    count_value <- as.integer(counted_stalks)
    target_species <- target_species[target_species$stalk_counted == count_value, ]
  }

  if (!is.null(species)) {
    target_species <- target_species[target_species$plant_code %in% species, ]
  }

  species_to_keep <- target_species$plant_code

  # Ensure we only try to select columns that actually exist
  species_in_data <- intersect(species_to_keep, colnames(filtered_abundance))

  if (length(species_in_data) > 0) {
    filtered_abundance <- filtered_abundance[, c("year", "plot_id", species_in_data)]
  }

  if (!is.null(years)) {
    filtered_abundance <- filtered_abundance[filtered_abundance$year %in% years, ]
  }

  # --- NEW: Standardization Logic ---
  # Check if the user requested standardized output
  if (output == "standardized") {

    # 1. Identify which columns in our current data frame need which treatment
    all_species_cols <- colnames(filtered_abundance)[-(1:2)] # Get all species columns

    sublot_species <- plant_metadata$plant_code[plant_metadata$stalk_counted == 0]
    stalk_species <- plant_metadata$plant_code[plant_metadata$stalk_counted == 1]

    cols_to_scale_sublot <- intersect(all_species_cols, sublot_species)
    cols_to_scale_stalk <- intersect(all_species_cols, stalk_species)

    # 2. Scale the sublot columns (divide by 64)
    if (length(cols_to_scale_sublot) > 0) {
      filtered_abundance[, cols_to_scale_sublot] <- filtered_abundance[, cols_to_scale_sublot] / 64
    }

    # 3. Scale the stalk count columns (divide by global max for each species)
    if (length(cols_to_scale_stalk) > 0) {
      for (col in cols_to_scale_stalk) {
        # IMPORTANT: We calculate max from the *original*, full dataset
        # to ensure the scaling is consistent regardless of filtering.
        max_val <- max(plant_abundance[[col]], na.rm = TRUE)

        # Avoid dividing by zero if a species was never observed
        if (max_val > 0) {
          filtered_abundance[[col]] <- filtered_abundance[[col]] / max_val
        }
      }
    }
  } else if (output != "raw") {
    # A friendly warning if the user enters an invalid option
    warning("Invalid `output` argument. Returning 'raw' data.")
  }

  return(filtered_abundance)
}
