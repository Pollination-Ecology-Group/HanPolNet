#' Calculate Sampling Effort
#'
#' This function calculates the total sampling effort, defined as the number of
#' unique sampling events (visits), for each plot within each experimental run.
#'
#' @param data A data frame of interaction data, typically the raw `interaction_data` object.
#'
#' @return A data frame with `experiment_run`, `plot_id`, and `n_visits` (the total
#'   number of unique `sampling_id`s for that combination).
#' @export
#' @importFrom dplyr %>%
#'
calculate_sampling_effort <- function(data) {
  effort_data <- data %>%
    dplyr::group_by(.data$experiment_run, .data$plot_id) %>%
    dplyr::summarize(n_visits = dplyr::n_distinct(.data$sampling_id)) %>%
    dplyr::ungroup()

  return(effort_data)
}
