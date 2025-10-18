#' Calculate Pollinator Sharing Among Plants
#'
#' This function quantifies the degree of pollinator sharing (niche overlap)
#' among plant species in a network using the Morisita-Horn similarity index.
#'
#' @details
#' The function first constructs a quantitative plant-by-pollinator interaction
#' matrix. It then calculates all pairwise Morisita-Horn dissimilarities between
#' plant species using `vegan::vegdist(method = "horn")` and converts the result
#' to similarity (`1 - dissimilarity`). The final output depends on the `level` argument.
#'
#' @param data A data frame of interaction data, typically filtered by
#'   `get_interaction_data()`. Must contain `plant_code`, `pollinator_id`, and
#'   `interaction_count` or `rate`.
#' @param level A character string specifying the level of analysis:
#'   `"community"` (the default) or `"species"`.
#' @param use_rate A logical value. If `TRUE` (the default), the function will
#'   use the standardized `rate` column. If `FALSE`, it will use
#'   the raw `interaction_count`.
#'
#' @return If `level = "community"`, a single numeric value. If `level = "species"`,
#'   a data frame with `plant_code` and `mean_sharing_score`.
#' @export
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom vegan vegdist
#'
calculate_pollinator_sharing <- function(data, level = "community", use_rate = TRUE) {

  # --- Input Validation ---
  if (!level %in% c("community", "species")) {
    stop("`level` must be either 'community' or 'species'.")
  }
  value_col <- if (use_rate) "rate" else "interaction_count"
  if (!value_col %in% names(data)) {
    stop("Data is missing the required abundance column: '", value_col, "'")
  }

  # --- Create the Plant x Pollinator Matrix ---
  # Summarize interactions to get total abundance per plant/pollinator pair
  interaction_summary <- data %>%
    # --- FIX: Explicitly remove rows with no plant code ---
    dplyr::filter(!is.na(.data$plant_code)) %>%
    dplyr::group_by(.data$plant_code, .data$pollinator_id) %>%
    dplyr::summarise(abundance = sum(.data[[value_col]], na.rm = TRUE), .groups = 'drop')

  # Reshape to a wide matrix
  community_matrix <- interaction_summary %>%
    tidyr::pivot_wider(
      names_from = .data$pollinator_id,
      values_from = .data$abundance,
      values_fill = 0
    ) %>%
    tibble::column_to_rownames(var = "plant_code")

  # Remove any rows/columns that are all zeros
  community_matrix <- community_matrix[rowSums(community_matrix) > 0, colSums(community_matrix) > 0]

  if (nrow(community_matrix) < 2) {
    warning("Cannot calculate sharing with fewer than two plant species in the dataset.")
    return(NA)
  }

  # --- Calculate Similarity ---
  dist_matrix <- vegan::vegdist(community_matrix, method = "horn")
  sim_matrix <- 1 - as.matrix(dist_matrix)
  diag(sim_matrix) <- NA

  # --- Return Result based on Level ---
  if (level == "community") {
    return(mean(sim_matrix, na.rm = TRUE))
  } else { # level == "species"
    species_scores <- data.frame(
      plant_code = rownames(sim_matrix),
      mean_sharing_score = rowMeans(sim_matrix, na.rm = TRUE)
    )
    return(species_scores)
  }
}
