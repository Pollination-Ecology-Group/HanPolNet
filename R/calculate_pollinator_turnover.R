#' Calculate Pollinator Community Turnover for a Focal Plant
#'
#' Calculates the year-to-year similarity (Ruzicka index) of the pollinator
#' community visiting a specific focal plant species.
#'
#' @details
#' This function takes summarized interaction rates (interactions per visit) and
#' reshapes the data into a community matrix (years or plot-years as rows,
#' pollinator species as columns). It then uses `vegan::vegdist` with
#' `method = "jaccard"` (which calculates the Ruzicka distance for quantitative
#' data) to find the dissimilarity between consecutive time points. The final
#' output is the similarity, calculated as `1 - distance`.
#'
#' @param rate_data A data frame containing standardized interaction rates,
#'   typically the output of `summarize_interaction_rate()`. It must contain
#'   columns for `year`, `plot_id` (if calculating at plot level),
#'   `pollinator_id`, and `rate`.
#' @param focal_plant A character string specifying the plant code of the
#'   focal species for which the rates were calculated (used for labeling).
#' @param level A character string specifying the level of analysis:
#'   `"locality"` (aggregates across plots within years) or `"plot"` (calculates
#'   turnover for each plot individually).
#'
#' @return A data frame with the year-to-year similarity values.
#'   If `level = "locality"`, columns are `year1`, `year2`, `year_pair`, `similarity`.
#'   If `level = "plot"`, columns are `plot_id`, `year1`, `year2`, `year_pair`, `similarity`.
#' @export
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom vegan vegdist
#'
calculate_pollinator_turnover <- function(rate_data, focal_plant, level = "locality") {

  # --- Input Validation ---
  required_cols <- c("year", "pollinator_id", "rate")
  if (level == "plot") required_cols <- c(required_cols, "plot_id")
  if (!all(required_cols %in% names(rate_data))) {
    stop("Input 'rate_data' is missing required columns based on the specified 'level'.")
  }
  if (!level %in% c("locality", "plot")) {
    stop("`level` must be either 'locality' or 'plot'.")
  }

  # --- Prepare Community Matrix ---
  if (level == "locality") {
    # Aggregate rates across plots for each pollinator within each year
    yearly_data <- rate_data %>%
      dplyr::group_by(.data$year, .data$pollinator_id) %>%
      dplyr::summarise(total_rate = sum(.data$rate, na.rm = TRUE), .groups = 'drop')

    # Reshape to wide format: years as rows, pollinators as columns
    community_matrix <- yearly_data %>%
      tidyr::pivot_wider(names_from = .data$pollinator_id,
                         values_from = .data$total_rate,
                         values_fill = 0) %>%
      dplyr::arrange(.data$year) %>%
      tibble::column_to_rownames(var = "year")

  } else { # level == "plot"
    # Create a unique identifier for each plot-year
    rate_data <- rate_data %>%
      dplyr::mutate(plot_year = paste(.data$year, .data$plot_id, sep = "_")) %>%
      dplyr::select(.data$plot_year, .data$pollinator_id, .data$rate)

    # Reshape to wide format: plot-years as rows, pollinators as columns
    community_matrix <- rate_data %>%
      tidyr::pivot_wider(names_from = .data$pollinator_id,
                         values_from = .data$rate,
                         values_fill = 0) %>%
      tibble::column_to_rownames(var = "plot_year")
  }

  # --- Calculate Year-to-Year Dissimilarity ---
  # Remove species that are absent in all samples
  community_matrix <- community_matrix[, colSums(community_matrix, na.rm = TRUE) > 0]

  # Ensure there are at least 2 time points/samples
  if(nrow(community_matrix) < 2) {
    stop("Need at least two years (or plot-years) to calculate turnover.")
  }

  # Calculate pairwise distances (Ruzicka)
  dist_matrix <- as.matrix(vegan::vegdist(community_matrix, method = "jaccard"))

  # --- Extract Consecutive Year Similarities ---
  if (level == "locality") {
    years <- as.numeric(rownames(dist_matrix))
    turnover_data <- data.frame(
      year1 = years[-length(years)],
      year2 = years[-1],
      similarity = 1 - diag(dist_matrix[-1, -ncol(dist_matrix)])
    )
    turnover_data$year_pair <- paste(turnover_data$year1, turnover_data$year2, sep = "-")

  } else { # level == "plot"
    plot_years <- rownames(dist_matrix)
    plot_ids <- as.numeric(sapply(strsplit(plot_years, "_"), `[`, 2))
    years <- as.numeric(sapply(strsplit(plot_years, "_"), `[`, 1))

    unique_plots <- unique(plot_ids)
    results_list <- list()

    for (p in unique_plots) {
      plot_indices <- which(plot_ids == p)
      if (length(plot_indices) < 2) next # Skip plots with only one year

      plot_matrix <- dist_matrix[plot_indices, plot_indices]
      plot_years_subset <- years[plot_indices]
      # Order the matrix rows/cols by year
      order_idx <- order(plot_years_subset)
      plot_matrix_ordered <- plot_matrix[order_idx, order_idx]
      years_ordered <- plot_years_subset[order_idx]

      # Extract consecutive year similarities for this plot
      plot_turnover <- data.frame(
        plot_id = p,
        year1 = years_ordered[-length(years_ordered)],
        year2 = years_ordered[-1],
        similarity = 1 - diag(plot_matrix_ordered[-1, -ncol(plot_matrix_ordered)])
      )
      results_list[[length(results_list) + 1]] <- plot_turnover
    }
    turnover_data <- dplyr::bind_rows(results_list)
    turnover_data$year_pair <- paste(turnover_data$year1, turnover_data$year2, sep = "-")
  }

  return(turnover_data)
}
