#' Get and filter plant abundance data
#'
#' This function retrieves the plant abundance data, allowing for filtering by
#' year, species, and whether the plant stalks were counted.
#'
#' The function uses two datasets:
#' - `plant_abundance`: A data.frame containing plant counts with columns for year, plot_id, and various plant species.
#' - `plant_metadata`: A data.frame containing metadata about each plant species, including whether stalks were counted (1) or not (0).
#' The function filters the `plant_abundance` data based on the criteria provided by the user.
#' If no filters are provided, the function returns the entire dataset.
#' The filtering is done in two main steps:
#' 1. **Species Filtering**: The function first filters the `plant_metadata` to identify which species to include based on the `species` and `counted_stalks` parameters. It then selects only those species from the `plant_abundance` dataset.
#' 2. **Year Filtering**: The function then filters the `plant_abundance` dataset by the specified years.
#' The final result is a data.frame containing only the rows and columns that match the user's criteria.
#' This function is useful for researchers who want to analyze specific subsets of the plant abundance data without having to manually filter the datasets themselves.
#' It is important to note that the function assumes the presence of the `plant_abundance` and `plant_metadata` datasets in the environment where it is called.
#' Make sure to load the package or source the data before using this function.
#' @param years A numeric vector of years to include. If NULL, all years are returned.
#' @param species A character vector of species codes to include. If NULL, all species are returned.
#' @param counted_stalks A logical value. If TRUE, returns only species where stalks were counted.
#' If FALSE, returns only species where stalks were not counted (presence/absence).
#' If NULL (the default), returns all species regardless of counting method.
#'
#' @return A data.frame containing the filtered plant abundance data.
#' @export
#'
#' @examples
#' # Get all data for the year 2011
#' d11 <- get_plant_data(years = 11)
#'
#' # Get data for Achillea millefolium where stalks were counted
#' d_ach <- get_plant_data(species = "Ach_mil", counted_stalks = TRUE)
get_plant_data <- function(years = NULL, species = NULL, counted_stalks = NULL) {

  # Start with the full dataset
  filtered_abundance <- plant_abundance

  # --- Filter species based on metadata ---
  target_species <- plant_metadata

  # Filter by counted_stalks if the argument is not NULL
  if (!is.null(counted_stalks)) {
    # We need to match the logical input (TRUE/FALSE) to the 1/0 in your data
    count_value <- as.integer(counted_stalks)
    target_species <- target_species[target_species$stalk_counted == count_value, ]
  }

  # Further filter by the species list if provided
  if (!is.null(species)) {
    target_species <- target_species[target_species$plant_code %in% species, ]
  }

  # Get the final list of species codes (column names) to keep
  species_to_keep <- target_species$plant_code

  # Select only the desired species columns, plus the year and plot_id
  # We check if species_to_keep is not empty
  if (length(species_to_keep) > 0) {
    # The `all_of()` is important to handle the character vector correctly
    filtered_abundance <- filtered_abundance[, c("year", "plot_id", species_to_keep)]
  }


  # --- Filter by years if provided ---
  if (!is.null(years)) {
    filtered_abundance <- filtered_abundance[filtered_abundance$year %in% years, ]
  }

  # Return the final, filtered data frame
  return(filtered_abundance)
}
