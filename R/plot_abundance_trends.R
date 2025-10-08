#' Plot Yearly Trends in Abundance
#'
#' Creates faceted boxplots to visualize year-to-year changes in conspecific
#' and heterospecific (HCA) abundance, with an option for statistical comparisons.
#'
#' @details
#' This function visualizes the distribution of abundance values for each year.
#' If `add_stats = TRUE`, it uses `ggpubr::stat_compare_means` to perform a
#' Wilcoxon rank-sum test (Mann-Whitney U test) to check for significant
#' differences between consecutive years.
#'
#' @param hca_data A data frame produced by `calculate_hca()`.
#' @param abundance_data A standardized data frame from `get_plant_data(output = "standardized")`.
#' @param focal_species A character string specifying the plant code of the focal species.
#' @param add_stats A logical value. If `TRUE` (the default), statistical comparisons
#'   are performed and p-values are added to the plot.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom ggpubr stat_compare_means
#' @importFrom utils combn
#'
plot_abundance_trends <- function(hca_data, abundance_data, focal_species, add_stats = TRUE) {

  # --- 1. Data Preparation ---
  if (!focal_species %in% names(abundance_data)) {
    stop("focal_species '", focal_species, "' not found in abundance_data.")
  }
  conspecific_data <- abundance_data[, c("year", "plot_id", focal_species)]
  names(conspecific_data)[3] <- "conspecific"
  plot_data <- merge(hca_data, conspecific_data, by = c("year", "plot_id"))
  names(plot_data)[3] <- "heterospecific"

  plot_data_long <- reshape(
    plot_data,
    varying = c("heterospecific", "conspecific"),
    v.names = "abundance",
    timevar = "type",
    times = c("Heterospecific (HCA)", "Conspecific"),
    direction = "long",
    idvar = c("year", "plot_id")
  )
  plot_data_long$type <- factor(plot_data_long$type, levels = c("Conspecific", "Heterospecific (HCA)"))

  # --- 2. Create the Base Plot ---
  p <- ggplot(plot_data_long, aes(x = as.factor(.data$year), y = .data$abundance)) +
    geom_boxplot(aes(fill = as.factor(.data$year)), show.legend = FALSE) +
    facet_wrap(~ type, scales = "free_y") +
    labs(
      title = "Yearly Trends in Floral Abundance",
      x = "Year",
      y = "Standardized Abundance"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # --- 3. Conditionally Add Statistical Layer ---
  if (add_stats) {
    # Create list of comparisons for the stat test
    years_present <- as.character(sort(unique(plot_data_long$year)))
    comparison_list <- lapply(1:(length(years_present)-1), function(i) {
      c(years_present[i], years_present[i+1])
    })

    p <- p + ggpubr::stat_compare_means(
      method = "wilcox.test",
      paired = FALSE,
      comparisons = comparison_list
    ) +
      labs(subtitle = "P-values from Wilcoxon rank-sum test between consecutive years.")
  }

  return(p)
}
