#' Get, filter, and standardize plant-pollinator interaction data
#'
#' This function retrieves the interaction data, allowing for flexible filtering
#' and an option to standardize interaction counts by sampling effort.
#'
#' @details
#' Standardization is performed by calculating the total number of sampling visits
#' (`n_visits`) for each unique combination of `plot_id` and `experiment_run`
#' across the **entire dataset**. This effort value is then joined back to the
#' filtered data. A new column, `rate`, is calculated as `interaction_count / n_visits`,
#' representing the number of observed interactions for that record per sampling visit.
#'
#' @param standardize A logical value. If `FALSE` (the default), the raw `interaction_count`
#'   is returned. If `TRUE`, the `n_visits` and the standardized `rate` columns are added.
#' @param remove_zeros A logical value. If `TRUE` (the default), it removes rows
#'   where no interaction was recorded (i.e., where `plant_code` is `NA`).
#' @param ... (All other filtering arguments remain the same).
#'
#' @return A data frame containing the interaction data. If `standardize = TRUE`,
#'   it includes `n_visits` and `rate` columns.
#' @export
#' @importFrom dplyr %>%
#'
get_interaction_data <- function(years = NULL, plot_id = NULL, plant_species = NULL,
                                 pollinator_species = NULL, verified_taxa = NULL,
                                 is_pollinator = NULL, start_time = NULL, end_time = NULL,
                                 experiment_run = NULL, remove_zeros = TRUE,
                                 standardize = FALSE) {

  # --- 1. Join Interaction Data with Pollinator Metadata ---
  # Use a left_join here to make sure we don't lose interactions if metadata is missing
  # but filter them out later if needed.
  full_data <- dplyr::left_join(
    interaction_data,
    pollinator_metadata,
    by = "pollinator_id"
  )

  # --- 2. Handle Standardization (if requested) ---
  if (standardize) {
    # First, calculate sampling effort from the COMPLETE, unfiltered interaction data
    # to get the correct denominator.
    effort_data <- interaction_data %>%
      dplyr::group_by(.data$experiment_run, .data$plot_id) %>%
      dplyr::summarize(n_visits = dplyr::n_distinct(.data$sampling_id), .groups = 'drop')

    # Join this effort data onto our main data frame
    full_data <- dplyr::left_join(full_data, effort_data, by = c("experiment_run", "plot_id"))
  }

  # --- 3. Apply Filters ---
  # Start with the full data and progressively filter it down.
  filtered_data <- full_data

  if (remove_zeros) {
    filtered_data <- filtered_data %>% dplyr::filter(!is.na(.data$plant_code))
  }
  if (!is.null(years)) {
    filtered_data <- filtered_data[filtered_data$year %in% years, ]
  }
  if (!is.null(plot_id)) {
    filtered_data <- filtered_data[filtered_data$plot_id %in% plot_id, ]
  }
  if (!is.null(plant_species)) {
    filtered_data <- filtered_data[filtered_data$plant_code %in% plant_species, ]
  }
  if (!is.null(pollinator_species)) {
    filtered_data <- filtered_data[filtered_data$pollinator_id %in% pollinator_species, ]
  }
  if (!is.null(verified_taxa)) {
    filtered_data <- filtered_data[filtered_data$verified == verified_taxa, ]
  }
  if (!is.null(is_pollinator)) {
    filtered_data <- filtered_data[filtered_data$is_pollinator == is_pollinator, ]
  }
  if (!is.null(experiment_run)) {
    filtered_data <- filtered_data[filtered_data$experiment_run %in% experiment_run, ]
  }

  # Time-of-Day Filtering
  if (!is.null(start_time) || !is.null(end_time)) {
    filtered_data$time_decimal <- filtered_data$hour + (filtered_data$minute / 60)
    if (!is.null(start_time)) {
      start_decimal <- as.numeric(substr(start_time, 1, 2)) + (as.numeric(substr(start_time, 4, 5)) / 60)
      filtered_data <- filtered_data[filtered_data$time_decimal >= start_decimal, ]
    }
    if (!is.null(end_time)) {
      end_decimal <- as.numeric(substr(end_time, 1, 2)) + (as.numeric(substr(end_time, 4, 5)) / 60)
      filtered_data <- filtered_data[filtered_data$time_decimal <= end_decimal, ]
    }
    filtered_data$time_decimal <- NULL
  }

  # --- 4. Finalize Output ---
  if (standardize) {
    # Calculate the rate for the filtered data
    filtered_data <- filtered_data %>%
      dplyr::mutate(rate = .data$interaction_count / .data$n_visits)
  }

  return(filtered_data)
}
