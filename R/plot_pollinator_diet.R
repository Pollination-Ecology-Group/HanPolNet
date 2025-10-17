#' Plot and Compare Conspecific vs. Heterospecific Interaction Rates
#'
#' @param focal_plant A character string specifying the plant code of the focal species.
#' @param years A numeric vector of years to include in the analysis.
#' @param summary_level A character string: `"plot"` (default) or `"interaction"`.
#' @param add_stats A logical value. If `TRUE` (the default), statistical comparisons are displayed.
#' @param log_scale A logical value. If `FALSE` (the default), a linear y-axis is used.
#' @param colors A named character vector of two colors for the plot.
#' @param ylim An optional numeric vector of length 2 to set the y-axis limits (e.g., c(0, 10)).
#' @param ... Additional filtering arguments to pass to `get_interaction_data()`.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom ggpubr stat_pvalue_manual
#'
plot_pollinator_diet <- function(focal_plant, years, summary_level = "plot", add_stats = TRUE, log_scale = FALSE,
                                 colors = c("Conspecific" = "lightseagreen", "Heterospecific" = "lightcoral"),
                                 ylim = NULL, ...) {

  # --- Data Preparation ---
  # (This section is unchanged)
  interaction_subset <- get_interaction_data(years = years, standardize = TRUE, remove_zeros = TRUE, ...)
  if (nrow(interaction_subset) == 0) {
    warning("No interaction data found for the specified filters.")
    return(NULL)
  }
  focal_pollinators <- interaction_subset %>%
    dplyr::filter(.data$plant_code == focal_plant) %>%
    dplyr::distinct(.data$pollinator_id) %>%
    dplyr::pull(.data$pollinator_id)
  if (length(focal_pollinators) == 0) {
    warning("No pollinators were observed visiting the focal plant in the filtered data.")
    return(NULL)
  }
  diet_data <- interaction_subset %>%
    dplyr::filter(.data$pollinator_id %in% focal_pollinators) %>%
    dplyr::mutate(type = factor(dplyr::if_else(.data$plant_code == focal_plant, "Conspecific", "Heterospecific"),
                                levels = c("Conspecific", "Heterospecific")))

  if (summary_level == "plot") {
    plot_data <- diet_data %>%
      dplyr::group_by(.data$year, .data$plot_id, .data$type) %>%
      dplyr::summarize(y_value = sum(.data$rate, na.rm = TRUE), .groups = 'drop')
    y_label <- "Total Rate per Plot"
    subtitle_text <- "Each point represents the total standardized rate for a single plot"
  } else { # "interaction"
    plot_data <- diet_data %>% dplyr::rename(y_value = .data$rate)
    y_label <- "Standardized Interaction Rate (per visit)"
    subtitle_text <- "Each point represents a single interaction event"
  }

  # --- Create the Base Plot ---
  dodge_width <- 0.8
  plot_data_final <- plot_data
  if (log_scale) {
    min_nonzero <- min(plot_data_final$y_value[plot_data_final$y_value > 0], na.rm = TRUE)
    plot_data_final$plot_y <- plot_data_final$y_value + min_nonzero / 10
  } else {
    plot_data_final$plot_y <- plot_data_final$y_value
  }

  p <- ggplot(plot_data_final, aes(x = as.factor(.data$year), y = .data$plot_y, fill = .data$type)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = dodge_width),
                alpha = 0.2, size = 1.5) +
    # --- FIX: Change trim to TRUE to prevent drawing below zero ---
    geom_violin(position = position_dodge(width = dodge_width), trim = TRUE, alpha = 0.7) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste("Pollinator Diet for Visitors of", focal_plant),
      subtitle = subtitle_text, x = "Year",
      y = ifelse(log_scale, paste(y_label, "(log10 scale)"), y_label),
      fill = "Interaction Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # --- Add Stats ---
  if (add_stats) {
    # ... (stat logic is unchanged) ...
    years_present <- as.character(sort(unique(plot_data$year)))
    comparison_list <- lapply(1:(length(years_present) - 1), function(i) c(years_present[i], years_present[i+1]))
    stat_test_con <- plot_data %>% dplyr::filter(type == "Conspecific") %>% rstatix::wilcox_test(y_value ~ year, comparisons = comparison_list) %>% rstatix::add_significance("p") %>% rstatix::add_xy_position(x = "year")
    stat_test_het <- plot_data %>% dplyr::filter(type == "Heterospecific") %>% rstatix::wilcox_test(y_value ~ year, comparisons = comparison_list) %>% rstatix::add_significance("p") %>% rstatix::add_xy_position(x = "year")
    stat_test_con <- stat_test_con %>% dplyr::mutate(xmin = xmin - dodge_width / 4, xmax = xmax - dodge_width / 4)
    stat_test_het <- stat_test_het %>% dplyr::mutate(xmin = xmin + dodge_width / 4, xmax = xmax + dodge_width / 4)
    p <- p +
      ggpubr::stat_pvalue_manual(stat_test_con, label = "p.signif", tip.length = 0.01) +
      ggpubr::stat_pvalue_manual(stat_test_het, label = "p.signif", tip.length = 0.01)
  }

  # --- Apply Scale ---
  if (log_scale) {
    p <- p + scale_y_log10()
  }

  # --- NEW: Conditionally set y-axis limits ---
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }

  return(p)
}
