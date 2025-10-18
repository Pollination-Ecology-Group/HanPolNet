#' Plot and Compare Conspecific Abundance vs. HCA
#'
#' This function creates side-by-side violin plots to compare year-to-year changes
#' in the abundance of a focal plant ("Conspecific") versus the abundance of its
#' neighbors ("Heterospecific" - HCA).
#'
#' @details
#' If `add_stats = TRUE`, the function performs an unpaired Wilcoxon rank-sum test
#' to compare the distribution of one year to the next. This test is performed
#' independently for the "Conspecific" and "Heterospecific (HCA)" groups, and
#' the results are displayed as separate brackets correctly positioned over the violins.
#'
#' @param hca_data A data frame produced by `calculate_hca()`.
#' @param abundance_data A standardized data frame from `get_plant_data(output = "standardized")`.
#' @param focal_species A character string specifying the plant code of the focal species.
#' @param years A numeric vector of years to include in the analysis.
#' @param add_stats A logical value. If `TRUE` (the default), statistical comparisons are displayed.
#' @param colors A named character vector of two colors for the plot.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom ggpubr stat_pvalue_manual
#'
plot_hca_neighborhood <- function(hca_data, abundance_data, focal_species, years, add_stats = TRUE,
                                  colors = c("Conspecific" = "#0072B2", "Heterospecific (HCA)" = "#D55E00")) {

  # --- 1. Prepare Data for Plotting ---
  if (!focal_species %in% names(abundance_data)) {
    stop("focal_species '", focal_species, "' not found in abundance_data.")
  }

  # Filter for the relevant years
  hca_data <- hca_data %>% dplyr::filter(.data$year %in% years)
  abundance_data <- abundance_data %>% dplyr::filter(.data$year %in% years)

  # Get conspecific abundance for the focal species
  conspecific_data <- abundance_data[, c("year", "plot_id", focal_species)]
  names(conspecific_data)[3] <- "value"
  conspecific_data$type <- "Conspecific"

  # Get heterospecific abundance (HCA)
  heterospecific_data <- hca_data[, c("year", "plot_id", "hca")]
  names(heterospecific_data)[3] <- "value"
  heterospecific_data$type <- "Heterospecific (HCA)"

  # Combine into one long data frame
  plot_data <- dplyr::bind_rows(conspecific_data, heterospecific_data) %>%
    dplyr::mutate(type = factor(.data$type, levels = c("Conspecific", "Heterospecific (HCA)")))

  # --- 2. Create the Base Plot ---
  dodge_width <- 0.8
  p <- ggplot(plot_data, aes(x = as.factor(.data$year), y = .data$value, fill = .data$type)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = dodge_width),
                alpha = 0.2, size = 1.5) +
    geom_violin(position = position_dodge(width = dodge_width), trim = TRUE, alpha = 0.7) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste("Floral Neighborhood of", focal_species),
      subtitle = "Year-to-year changes in conspecific vs. heterospecific (HCA) abundance",
      x = "Year",
      y = "Standardized Abundance",
      fill = "Abundance Type"
    ) +
    coord_cartesian(ylim = c(0, NA)) + # Ensure y-axis starts at 0
    theme_minimal() +
    theme(legend.position = "bottom")

  # --- 3. Conditionally Add Stats ---
  if (add_stats) {
    years_present <- as.character(sort(unique(plot_data$year)))
    comparison_list <- lapply(1:(length(years_present) - 1), function(i) {
      c(years_present[i], years_present[i+1])
    })

    # Use rstatix to perform tests and get bracket positions
    stat_test <- plot_data %>%
      dplyr::group_by(.data$type) %>%
      rstatix::wilcox_test(value ~ year, comparisons = comparison_list) %>%
      rstatix::add_significance("p") %>%
      rstatix::add_xy_position(x = "year", dodge = dodge_width)

    p <- p + ggpubr::stat_pvalue_manual(
      stat_test,
      label = "p.signif",
      tip.length = 0.01
    )
  }

  return(p)
}
