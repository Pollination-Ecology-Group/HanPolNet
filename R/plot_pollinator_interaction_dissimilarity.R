#' Plot Pollinator Interaction Dissimilarity Against a Null Model
#'
#' Calculates and plots the average Morisita-Horn dissimilarity of a pollinator's
#' interaction partners (plants) across different plots, comparing observed
#' values to null model expectations. Allows for different null model types.
#'
#' @param years A numeric vector of years to analyze.
#' @param plant_abundance_data A data frame containing standardized plant abundance data,
#'   typically from `get_plant_data(output = "standardized")`. Required if
#'   `null_model_type = "abundance_weighted"`.
#' @param null_model_type A character string specifying the null model algorithm:
#'   `"shuffle"` (default): Randomly shuffles interactions while approximately
#'   preserving row/column frequencies (vegan's "r00").
#'   `"abundance_weighted"`: Shuffles interactions within each plot based on the
#'   relative abundance of available plants in that plot.
#' @param summarize_years A logical value. If `FALSE` (default), results are faceted by year.
#'   If `TRUE`, results are averaged across years.
#' @param top_n_pollinators An integer specifying how many pollinators to include.
#' @param min_plots An integer. Pollinators in fewer plots are excluded. Defaults to 3.
#' @param n_null_models The number of null model iterations.
#' @param ... Additional filtering arguments passed to `get_interaction_data()`.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom dplyr %>% filter group_by summarise n_distinct arrange slice pull left_join bind_rows desc n semi_join across everything select rename mutate case_when
#' @importFrom vegan vegdist nullmodel decostand
#' @importFrom stats simulate quantile rmultinom
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tibble column_to_rownames
#'
plot_pollinator_interaction_dissimilarity <- function(years,
                                                      plant_abundance_data = NULL,
                                                      null_model_type = "shuffle",
                                                      summarize_years = FALSE,
                                                      top_n_pollinators = 20,
                                                      min_plots = 3,
                                                      n_null_models = 99,
                                                      ...) {

  # --- Input Validation ---
  if (!null_model_type %in% c("shuffle", "abundance_weighted")) {
    stop("`null_model_type` must be 'shuffle' or 'abundance_weighted'")
  }
  if (null_model_type == "abundance_weighted" && is.null(plant_abundance_data)) {
    stop("`plant_abundance_data` must be provided when `null_model_type = 'abundance_weighted'`")
  }
  required_abund_cols <- c("year", "plot_id")
  if (!is.null(plant_abundance_data) && !all(required_abund_cols %in% names(plant_abundance_data))) {
    stop("`plant_abundance_data` is missing required columns: 'year', 'plot_id'.")
  }


  # --- 1. Get Interaction Data ---
  message("Fetching and preparing interaction data...")
  interaction_data_std <- get_interaction_data( years = years, standardize = TRUE, remove_zeros = TRUE, ...)
  if (nrow(interaction_data_std) == 0) {
    warning("No interaction data found for the specified filters.")
    return(NULL)
  }
  interaction_summary <- interaction_data_std %>%
    dplyr::group_by(year, plot_id, pollinator_id, plant_code) %>%
    dplyr::summarise(total_rate = sum(rate, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::filter(total_rate > 0)

  # --- 1b. Prepare Plant Abundance Data (if needed) ---
  if (null_model_type == "abundance_weighted") {
    message("Preparing plant abundance data for null model...")
    # Ensure abundance data covers the relevant years and plots
    plant_abund_filtered <- plant_abundance_data %>%
      dplyr::filter(year %in% years) %>%
      # Convert to long format for easier lookup
      tidyr::pivot_longer(
        cols = -c(year, plot_id),
        names_to = "plant_code",
        values_to = "abundance"
      ) %>%
      dplyr::filter(abundance > 0) # Only consider present plants
  }


  # --- 2. Filter Pollinators ---
  pollinator_plot_counts <- interaction_summary %>%
    dplyr::group_by(year, pollinator_id) %>%
    dplyr::summarise(n_plots = dplyr::n_distinct(plot_id), .groups = 'drop') %>%
    dplyr::filter(n_plots >= min_plots)
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
  analysis_data <- interaction_summary %>%
    dplyr::semi_join(pollinator_plot_counts, by = c("year", "pollinator_id"))

  # --- 3. Calculate Observed and Null Dissimilarities ---
  message("Calculating observed and null model dissimilarities...")
  results_list <- list()
  unique_year_pollinators <- unique(analysis_data[, c("year", "pollinator_id")])

  for (i in 1:nrow(unique_year_pollinators)) {
    current_year <- unique_year_pollinators$year[i]
    current_pollinator <- unique_year_pollinators$pollinator_id[i]

    subset_data <- analysis_data %>%
      dplyr::filter(year == current_year, pollinator_id == current_pollinator)

    # Create the observed community matrix: plots as rows, plants as columns
    community_matrix_obs <- subset_data %>%
      tidyr::pivot_wider(id_cols = plot_id, names_from = plant_code,
                         values_from = total_rate, values_fill = 0) %>%
      tibble::column_to_rownames(var = "plot_id")

    if (nrow(community_matrix_obs) >= 2) {
      # Observed Dissimilarity
      obs_dist <- vegan::vegdist(community_matrix_obs, method = "horn")
      obs_mean_dissim <- mean(obs_dist, na.rm = TRUE)

      # Null Model Generation
      null_mean_dissims <- numeric(n_null_models)

      if (null_model_type == "shuffle") {
        nm <- vegan::nullmodel(community_matrix_obs, method = "r00")
        null_sims <- simulate(nm, nsim = n_null_models)
        for(j in 1:n_null_models) {
          null_dist <- vegan::vegdist(null_sims[, , j], method = "horn")
          null_mean_dissims[j] <- mean(null_dist, na.rm = TRUE)
        }
      } else { # abundance_weighted
        # Get abundance data for the current year
        abund_year <- plant_abund_filtered %>% dplyr::filter(year == current_year)

        # Iterate through null models
        for (j in 1:n_null_models) {
          # Create a null matrix structure same as observed
          null_matrix_j <- community_matrix_obs * 0

          # For each plot (row in observed matrix)
          for (plot_r in rownames(community_matrix_obs)) {
            plot_r_id <- as.numeric(plot_r)
            observed_interactions_in_plot <- community_matrix_obs[plot_r, ]
            total_interactions_this_plot <- sum(observed_interactions_in_plot)

            # If no interactions in this plot, skip
            if(total_interactions_this_plot <= 0) next

            # Get available plants and their abundances in this plot for this year
            available_plants <- abund_year %>% dplyr::filter(plot_id == plot_r_id)

            # If no abundance data for this plot, skip (or handle differently?)
            if(nrow(available_plants) == 0) next

            # Perform multinomial sampling: redistribute interactions based on abundance
            # Ensure probabilities sum to 1
            abund_probs <- available_plants$abundance / sum(available_plants$abundance)

            # Use rmultinom to shuffle interactions according to abundance probs
            # Note: This uses interaction counts, assuming rates roughly scale
            shuffled_counts <- stats::rmultinom(1, size = round(total_interactions_this_plot), prob = abund_probs)

            # Place shuffled counts back into the null matrix
            rownames(shuffled_counts) <- available_plants$plant_code
            common_plants <- intersect(colnames(null_matrix_j), rownames(shuffled_counts))
            null_matrix_j[plot_r, common_plants] <- shuffled_counts[common_plants, 1]
          }
          # Calculate dissimilarity for this null matrix
          # Use decostand to ensure relative abundances if needed by Horn index? Horn uses raw counts/abund.
          if (nrow(null_matrix_j) >= 2 && sum(null_matrix_j) > 0) {
            null_dist <- vegan::vegdist(null_matrix_j, method = "horn")
            null_mean_dissims[j] <- mean(null_dist, na.rm = TRUE)
          } else {
            null_mean_dissims[j] <- NA # Assign NA if matrix is invalid
          }
        }
      } # end abundance_weighted else

      # Get 95% quantiles from the valid null distribution
      valid_null_dissims <- null_mean_dissims[!is.na(null_mean_dissims)]
      if (length(valid_null_dissims) > 1) { # Need at least 2 values for quantile
        null_quantiles <- stats::quantile(valid_null_dissims, probs = c(0.025, 0.975), na.rm = TRUE)
      } else {
        null_quantiles <- c(NA, NA) # Assign NA if quantiles can't be calculated
      }

      results_list[[length(results_list) + 1]] <- data.frame(
        year = current_year, pollinator_id = current_pollinator,
        observed_dissimilarity = obs_mean_dissim,
        null_lower_95 = null_quantiles[1], null_upper_95 = null_quantiles[2]
      )
    } # end if nrow >= 2
  } # end loop through pollinators/years

  all_results_yearly <- dplyr::bind_rows(results_list)
  # ... (Summarizing and Plotting code remains the same) ...
  if (nrow(all_results_yearly) == 0) {
    warning("No pollinators met the criteria for dissimilarity calculation.")
    return(NULL)
  }

  # --- 4. Conditionally Summarize Across Years ---
  if (summarize_years) {
    message("Summarizing results across years...")
    plot_data <- all_results_yearly %>%
      dplyr::group_by(pollinator_id) %>%
      dplyr::summarise(
        dplyr::across(c(observed_dissimilarity, null_lower_95, null_upper_95), mean, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::arrange(observed_dissimilarity) %>%
      dplyr::mutate(pollinator_id = factor(pollinator_id, levels = unique(pollinator_id)))

    plot_subtitle <- paste("Observed (red) vs. 95% Null Model Range (grey) - Averaged across years:",
                           paste(range(years), collapse="-"))
  } else {
    plot_data <- all_results_yearly %>%
      dplyr::group_by(year) %>%
      dplyr::arrange(observed_dissimilarity, .by_group = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(observed_dissimilarity) %>%
      dplyr::mutate(pollinator_id = factor(pollinator_id, levels = unique(.$pollinator_id)))

    plot_subtitle <- "Observed (red) vs. 95% Null Model Range (grey)"
  }

  message("Calculations complete. Generating plot...")

  # --- 5. Create the Plot ---
  p <- ggplot(plot_data, aes(x = pollinator_id)) +
    geom_linerange(aes(ymin = null_lower_95, ymax = null_upper_95), color = "grey", linewidth = 2) +
    geom_point(aes(y = observed_dissimilarity), color = "red", size = 3) +
    coord_flip() +
    labs(
      title = "Pollinator Interaction Dissimilarity (Morisita-Horn)",
      subtitle = plot_subtitle,
      x = "Pollinator Species",
      y = "Mean Morisita-Horn Dissimilarity Across Plots"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

  if (!summarize_years) {
    p <- p + facet_wrap(~ year, ncol = 1, scales = "free_y")
  }

  return(p)
}
