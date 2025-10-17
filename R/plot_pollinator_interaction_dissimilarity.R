#' Plot Pollinator Interaction Dissimilarity Against a Null Model
#'
#' Calculates and plots the average Morisita-Horn dissimilarity of a pollinator's
#' interaction partners (plants) across different plots within a year, comparing
#' observed values to a null model expectation.
#'
#' @param years A numeric vector of years to analyze.
#' @param top_n_pollinators An integer specifying how many of the most abundant
#'   pollinators to include in the plot. If NULL, includes all pollinators found
#'   in enough plots.
#' @param min_plots An integer. Pollinators observed in fewer than this many plots
#'   within a year will be excluded, as dissimilarity cannot be calculated. Defaults to 3.
# @param n_null_models The number of null model iterations to run (e.g., 999).
# @param null_model_type The null model algorithm to use (e.g., "shuffle").
#' @param ... Additional filtering arguments passed to `get_interaction_data()`
#'   (e.g., `is_pollinator = TRUE`).
#'
#' @return A data frame containing the calculated dissimilarities (for now). Plotting will be added later.
#' @export
#' @import ggplot2
#' @importFrom dplyr %>% filter group_by summarise n_distinct arrange slice pull left_join
#' @importFrom vegan vegdist
#' @importFrom tidyr pivot_wider complete
#' @importFrom tibble column_to_rownames rownames_to_column
#'
plot_pollinator_interaction_dissimilarity <- function(years,
                                                      top_n_pollinators = 20,
                                                      min_plots = 3,
                                                      # n_null_models = 99,
                                                      # null_model_type = "shuffle",
                                                      ...) {

  # --- 1. Get Standardized Interaction Data ---
  message("Fetching and preparing interaction data...")
  interaction_data_std <- get_interaction_data(
    years = years,
    standardize = TRUE, # Use standardized rate
    remove_zeros = TRUE,
    ...
  )

  if (nrow(interaction_data_std) == 0) {
    warning("No interaction data found for the specified filters.")
    return(NULL)
  }

  # --- 2. Data Preparation ---
  # Summarize total interaction rate per pollinator, per plant, per plot, per year
  interaction_summary <- interaction_data_std %>%
    dplyr::group_by(year, plot_id, pollinator_id, plant_code) %>%
    dplyr::summarise(total_rate = sum(rate, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::filter(total_rate > 0) # Only keep actual interactions

  # Identify pollinators present in enough plots for each year
  pollinator_plot_counts <- interaction_summary %>%
    dplyr::group_by(year, pollinator_id) %>%
    dplyr::summarise(n_plots = dplyr::n_distinct(plot_id), .groups = 'drop') %>%
    dplyr::filter(n_plots >= min_plots)

  # Optionally filter for top N pollinators based on overall abundance (total rate)
  if (!is.null(top_n_pollinators)) {
    top_pollinators <- interaction_summary %>%
      dplyr::group_by(pollinator_id) %>%
      dplyr::summarise(overall_rate = sum(total_rate)) %>%
      dplyr::arrange(dplyr::desc(overall_rate)) %>%
      dplyr::slice(1:top_n_pollinators) %>%
      dplyr::pull(pollinator_id)

    pollinator_plot_counts <- pollinator_plot_counts %>%
      dplyr::filter(pollinator_id %in% top_pollinators)
  }

  # Filter the main summary to only include these pollinators
  analysis_data <- interaction_summary %>%
    dplyr::semi_join(pollinator_plot_counts, by = c("year", "pollinator_id"))

  # --- 3. Calculate Observed Dissimilarity ---
  message("Calculating observed dissimilarities...")
  results_list <- list()
  unique_year_pollinators <- unique(analysis_data[, c("year", "pollinator_id")])

  for (i in 1:nrow(unique_year_pollinators)) {
    current_year <- unique_year_pollinators$year[i]
    current_pollinator <- unique_year_pollinators$pollinator_id[i]

    # Subset data for this specific pollinator and year
    subset_data <- analysis_data %>%
      dplyr::filter(year == current_year, pollinator_id == current_pollinator)

    # Create the community matrix: plots as rows, plants as columns
    community_matrix <- subset_data %>%
      tidyr::pivot_wider(
        id_cols = plot_id,
        names_from = plant_code,
        values_from = total_rate,
        values_fill = 0
      ) %>%
      tibble::column_to_rownames(var = "plot_id")

    # Calculate pairwise Morisita-Horn dissimilarities between plots
    # vegdist calculates distance (0=identical), we want dissimilarity (0=identical)
    if (nrow(community_matrix) >= 2) {
      dissimilarity_values <- vegan::vegdist(community_matrix, method = "horn")

      # Store the mean dissimilarity
      results_list[[length(results_list) + 1]] <- data.frame(
        year = current_year,
        pollinator_id = current_pollinator,
        observed_dissimilarity = mean(dissimilarity_values, na.rm = TRUE)
        # We will add null model results here later
      )
    }
  }

  observed_results <- dplyr::bind_rows(results_list)

  message("Observed calculations complete.")

  # --- Placeholder for Null Models & Plotting ---
  # For now, just return the observed results so we can test this part

  if (nrow(observed_results) > 0) {
    return(observed_results)
  } else {
    warning("No pollinators met the criteria for dissimilarity calculation.")
    return(NULL)
  }
}
