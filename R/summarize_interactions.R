#' Summarize and Standardize Interaction Counts
#'
#' This function takes a filtered set of interaction data, aggregates the interaction
#' counts by specified grouping variables, and standardizes them by sampling effort.
#'
#' @param data A data frame of interaction data, typically filtered by `get_interaction_data()`.
#' @param effort_data A data frame of sampling effort, created by `calculate_sampling_effort()`.
#' @param ... The columns to group by before summarizing (e.g., `plot_id`,
#'   `experiment_run`, `plant_code`). These are passed directly to `dplyr::group_by()`.
#'
#' @return A data frame with the summarized and standardized interaction rates.
#' @export
#' @importFrom dplyr %>%
#'
summarize_interactions <- function(data, effort_data, ...) {

  summary_data <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(total_interactions = sum(.data$interaction_count, na.rm = TRUE)) %>%
    dplyr::ungroup()

  join_cols <- intersect(names(summary_data), c("plot_id", "experiment_run"))

  if (!"plot_id" %in% join_cols || !"experiment_run" %in% join_cols) {
    stop("Grouping variables (...) must include 'plot_id' and 'experiment_run' to link to effort data.")
  }

  summary_with_effort <- dplyr::left_join(summary_data, effort_data, by = join_cols)

  final_summary <- summary_with_effort %>%
    dplyr::mutate(rate = .data$total_interactions / .data$n_visits)

  return(final_summary)
}
