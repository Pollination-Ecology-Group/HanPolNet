#' Plot the Floral Neighborhood (Conspecific vs. Heterospecific Abundance)
#'
#' This function creates visualizations of the Heterospecific Co-flowering
#' Abundance (HCA) index alongside the abundance of a focal species.
#'
#' @details
#' This function takes the output from `calculate_hca()` and the original
#' standardized abundance data to create one of three plot types:
#' \itemize{
#'   \item `"by_year"`: Faceted violin plots comparing the distributions of
#'     conspecific and heterospecific abundance for each year, with p-values
#'     from a Wilcoxon signed-rank test.
#'   \item `"by_plot"`: A timeline for a single, specified plot, showing how its
#'     floral neighborhood has changed over the years.
#'   \item `"grid"`: A comprehensive view showing a bar for every plot, faceted by year.
#' }
#'
#' @param hca_data A data frame produced by `calculate_hca()`.
#' @param abundance_data A standardized data frame from `get_plant_data(output = "standardized")`,
#'   used to get the focal species' abundance.
#' @param focal_species A character string specifying the plant code of the
#'   focal species. This must match the one used in `calculate_hca()`.
#' @param plot_type A character string specifying the plot to generate. Must be one
#'   of `"by_year"`, `"by_plot"`, or `"grid"`.
#' @param plot_id A numeric value specifying the plot to display. Only required
#'   when `plot_type = "by_plot"`.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom ggpubr stat_compare_means
#' @importFrom stats aggregate sd
#'
plot_hca_neighborhood <- function(hca_data, abundance_data, focal_species,
                                  plot_type = "by_year", plot_id = NULL) {

  # --- Input Validation and Data Prep (same as before) ---
  valid_types <- c("by_year", "by_plot", "grid")
  if (!plot_type %in% valid_types) {
    stop("`plot_type` must be one of: ", paste(valid_types, collapse = ", "))
  }
  if (!focal_species %in% names(abundance_data)) {
    stop("focal_species '", focal_species, "' not found in abundance_data.")
  }
  if (plot_type == "by_plot" && is.null(plot_id)) {
    stop("You must provide a `plot_id` when using `plot_type = 'by_plot'`.")
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

  # --- Generate Plot based on plot_type ---

  p <- switch(
    plot_type,

    "by_year" = {
      # --- Plot 1: Yearly Average with Stat Test (UPDATED) ---
      # Create a list of pairs to compare
      comparison_list <- list(c("Conspecific", "Heterospecific (HCA)"))

      ggplot(plot_data_long, aes(x = .data$type, y = .data$abundance)) +
        geom_violin(trim = FALSE, aes(fill = .data$type)) +
        geom_jitter(height = 0, width = 0.1, alpha = 0.2) +
        # Add the statistical comparison using a Wilcoxon test
        ggpubr::stat_compare_means(
          method = "wilcox.test",
          paired = TRUE,
          comparisons = comparison_list
        ) +
        facet_wrap(~ year) +
        labs(
          title = "Conspecific vs. Heterospecific Abundance by Year",
          subtitle = "Points are individual plots. P-values from paired Wilcoxon signed-rank test.",
          x = "Abundance Type",
          y = "Standardized Abundance"
        ) +
        theme(legend.position = "none") # Hide legend as fills are self-explanatory
    },

    "by_plot" = {
      # Plot 2: Single Plot Timeline (Unchanged)
      single_plot_data <- plot_data_long[plot_data_long$plot_id == plot_id, ]
      if(nrow(single_plot_data) == 0) {
        stop("plot_id ", plot_id, " not found in the dataset.")
      }

      ggplot(single_plot_data, aes(x = as.factor(.data$year), y = .data$abundance, fill = .data$type)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(
          title = paste("Floral Neighborhood for Plot:", plot_id),
          x = "Year", y = "Standardized Abundance", fill = "Abundance Type"
        )
    },

    "grid" = {
      # Plot 3: Grid View (Unchanged)
      ggplot(plot_data_long, aes(x = as.factor(.data$plot_id), y = .data$abundance, fill = .data$type)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~ year) +
        labs(
          title = "Floral Neighborhood by Plot and Year",
          x = "Plot ID", y = "Standardized Abundance", fill = "Abundance Type"
        ) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))
    }
  )

  return(p + theme_minimal() + scale_fill_brewer(palette = "Set2"))
}
