#' Summarize and Standardize Interaction Rates for a Focal Plant
#'
#' This function filters for a focal plant species, aggregates interaction counts,
#' and standardizes them by sampling effort to calculate an interaction rate.
#'
#' @param data A data frame of interaction data, typically from `get_interaction_data()`.
#'   It's recommended to pre-filter this data for your desired subset
#'   (e.g., only true pollinators).
#' @param focal_plant A character string specifying the plant code of the
#'   focal species (e.g., "Suc_pra").
#' @param group_by A character vector of columns to group by before summarizing.
#'   Common choices are `c("year")` for a locality-level summary, or
#'   `c("year", "plot_id")` for a plot-level summary.
#'
#' @return A data frame containing the summarized interaction rates. It includes the
#'   grouping columns, `total_interactions`, `n_visits`, and the standardized `rate`
#'   (interactions per visit).
#' @export
#' @importFrom dplyr %>%
#'
summarize_interaction_rate <- function(data, focal_plant, group_by) {

  # --- 1. Filter for the focal plant ---
  focal_data <- data %>%
    dplyr::filter(.data$plant_code == focal_plant)

  if (nrow(focal_data) == 0) {
    warning("No interaction data found for the specified focal_plant.")
    return(NULL)
  }

  # --- 2. Calculate Sampling Effort ---
  # Effort is calculated from the *input* data's context.
  effort_data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
    dplyr::summarize(n_visits = dplyr::n_distinct(.data$sampling_id), .groups = 'drop')

  # --- 3. Summarize Interaction Counts ---
  # Group by the user-specified variables.
  summary_data <- focal_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
    dplyr::summarize(total_interactions = sum(.data$interaction_count, na.rm = TRUE), .groups = 'drop')

  # --- 4. Join and Calculate Rate ---
  final_summary <- dplyr::left_join(summary_data, effort_data, by = group_by) %>%
    dplyr::mutate(rate = .data$total_interactions / .data$n_visits)

  return(final_summary)
}
