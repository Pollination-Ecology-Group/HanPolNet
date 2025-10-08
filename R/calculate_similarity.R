#' Calculate Pairwise Community Similarity
#'
#' Computes a similarity matrix for ecological communities using the Ruzicka
#' index (the quantitative version of the Jaccard index).
#'
#' @details
#' This function serves as a wrapper around `vegan::vegdist(method = "jaccard")`.
#' The Ruzicka similarity is calculated from the resulting distance object as
#' `1 - distance`.
#'
#' The function can operate in two modes depending on the `group_by` argument:
#' \itemize{
#'   \item `group_by = c("year", "plot_id")`: This treats each unique plot-year
#'     combination as a distinct sample. The function returns a matrix comparing
#'     every sample to every other sample.
#'   \item `group_by = "year"`: This aggregates the data for each year by taking
#'     the mean abundance of each species across all plots. It then returns a
#'     matrix comparing each year's average community composition to every other year.
#' }
#'
#' @param data A data frame containing standardized abundance data, typically from
#'   `get_plant_data(output = "standardized")`.
#' @param group_by A character vector specifying how to group samples for comparison.
#'   Must be either `c("year", "plot_id")` (the default) or `"year"`.
#'
#' @return A list containing three elements:
#'   \item{global_similarity}{The overall mean similarity across all pairs.}
#'   \item{variation}{The standard deviation of similarity values across all pairs.}
#'   \item{similarity_matrix}{The full pairwise similarity matrix.}
#' @export
#' @importFrom stats aggregate sd
#'
#' @examples
#' # --- Example 1: Comparing aggregated years ---
#' std_data <- get_plant_data(output = "standardized")
#' year_sim <- calculate_similarity(std_data, group_by = "year")
#' print(year_sim$global_similarity)
#' print(year_sim$similarity_matrix)
#'
#' # --- Example 2: Comparing individual plot-years for a subset ---
#' std_subset <- get_plant_data(years = 21:22, output = "standardized")
#' plot_sim <- calculate_similarity(std_subset, group_by = c("year", "plot_id"))
#'
calculate_similarity <- function(data, group_by = c("year", "plot_id")) {

  # --- 1. Input Validation ---
  if (!all(c("year", "plot_id") %in% colnames(data))) {
    stop("Input 'data' must contain 'year' and 'plot_id' columns.")
  }

  # --- 2. Data Preparation based on group_by ---
  # FIX: Use identical() to ensure the check is on the entire object
  if (identical(group_by, "year")) {
    # Aggregate by year: calculate mean abundance for each species
    # The dot formula `.` means "all other columns"
    agg_formula <- as.formula(paste(". ~ year"))
    community_data <- aggregate(agg_formula, data = data[, -which(names(data) == "plot_id")], FUN = mean)

    rownames(community_data) <- community_data$year
    community_data <- community_data[, -1] # Remove the year column

  } else {
    # Default behavior: treat each plot-year as a sample
    community_data <- data

    # Create unique row names for the output matrix
    rownames(community_data) <- paste(community_data$year, community_data$plot_id, sep = "_")
    community_data <- community_data[, -c(1:2)] # Remove year and plot_id columns
  }

  # --- 3. Core Calculation ---
  # Remove species that have zero abundance across all samples
  community_data <- community_data[, colSums(community_data, na.rm = TRUE) > 0]

  # vegan::vegdist with method="jaccard" on quantitative data calculates Ruzicka distance
  dist_obj <- vegan::vegdist(community_data, method = "jaccard", binary = FALSE)

  # --- 4. Format Output ---
  sim_matrix <- 1 - as.matrix(dist_obj)

  # For global stats, use the dist object to avoid diagonal and duplicates
  all_sim_values <- 1 - as.vector(dist_obj)

  results <- list(
    global_similarity = mean(all_sim_values, na.rm = TRUE),
    variation = sd(all_sim_values, na.rm = TRUE),
    similarity_matrix = sim_matrix
  )

  return(results)
}
