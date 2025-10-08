#' Summarize Pairwise Similarity Between Consecutive Years
#'
#' For each pair of consecutive years in a dataset, this function calculates all
#' pairwise Ruzicka similarities between the plots of those two years and
#' returns the mean and standard deviation.
#'
#' @param data A data frame containing standardized abundance data, typically from
#'   `get_plant_data(output = "standardized")`. Must contain 'year' and 'plot_id'
#'   columns.
#'
#' @return A data frame with the following columns: `year1`, `year2`, `year_pair`,
#'   `mean_similarity`, and `sd_similarity`.
#' @export
#'
summarize_yearly_similarity <- function(data) {

  all_years <- sort(unique(data$year))
  year_pairs <- data.frame(year1 = all_years[-length(all_years)],
                           year2 = all_years[-1])

  results_list <- list()

  for (i in 1:nrow(year_pairs)) {
    yr1 <- year_pairs$year1[i]
    yr2 <- year_pairs$year2[i]

    data_y1 <- data[data$year == yr1, !names(data) %in% c("year", "plot_id")]
    data_y2 <- data[data$year == yr2, !names(data) %in% c("year", "plot_id")]

    keep_cols_y1 <- colSums(data_y1, na.rm = TRUE) > 0
    keep_cols_y2 <- colSums(data_y2, na.rm = TRUE) > 0
    all_keepers <- names(which(keep_cols_y1 | keep_cols_y2))

    # --- FIX: Add `drop = FALSE` to prevent simplification to a vector ---
    # This ensures we always pass a data frame to vegan, even with one column.
    dist_obj <- vegan::vegdist(
      data_y1[, all_keepers, drop = FALSE],
      data_y2[, all_keepers, drop = FALSE],
      method = "jaccard"
    )
    sim_values <- 1 - as.vector(dist_obj)

    results_list[[i]] <- data.frame(
      year1 = yr1,
      year2 = yr2,
      mean_similarity = mean(sim_values, na.rm = TRUE),
      sd_similarity = sd(sim_values, na.rm = TRUE)
    )
  }

  summary_df <- do.call(rbind, results_list)
  summary_df$year_pair <- paste(summary_df$year1, summary_df$year2, sep = "-")

  return(summary_df)
}
