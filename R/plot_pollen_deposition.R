#' Plot Pollen Deposition
#'
#' Creates side-by-side violin plots to compare conspecific vs. heterospecific
#' pollen deposition, grouped by a specified variable (e.g., year or day).
#'
#' @param group_by A character string specifying the column to use for the
#'   x-axis grouping (e.g., `"year"` or `"day"`). Defaults to `"year"`.
#' @param add_stats A logical value. If `TRUE` (the default), a paired Wilcoxon
#'   test is performed within each group to compare conspecific and
#'   heterospecific pollen counts.
#' @param colors A named character vector of two colors for the plot.
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom ggpubr stat_compare_means
#' @export
plot_pollen_deposition <- function(group_by = "year", add_stats = TRUE,
                                   colors = c("Conspecific" = "#0072B2", "Heterospecific" = "#D55E00")) {

  # --- 1. Prepare Data for Plotting ---
  plot_data <- pollen_deposition %>%
    tidyr::pivot_longer(
      cols = c(conspecific_pollen, heterospecific_pollen),
      names_to = "type",
      values_to = "pollen_count"
    ) %>%
    dplyr::mutate(
      type = factor(stringr::str_to_title(gsub("_pollen", "", type)),
                    levels = c("Conspecific", "Heterospecific"))
    )

  # --- 2. Create the Base Plot ---
  p <- ggplot(plot_data, aes(x = as.factor(.data[[group_by]]), y = .data$pollen_count, fill = .data$type)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
                alpha = 0.3, size = 1.5) +
    geom_violin(position = position_dodge(width = 0.8), trim = TRUE, alpha = 0.7) +
    scale_fill_manual(values = colors) +
    labs(
      title = "Pollen Deposition on Succisa pratensis Stigmas",
      subtitle = paste("Grouped by", group_by),
      x = stringr::str_to_title(group_by),
      y = "Number of Pollen Grains",
      fill = "Pollen Type"
    ) +
    coord_cartesian(ylim = c(0, NA)) + # Ensure y-axis starts at 0
    theme_minimal() +
    theme(legend.position = "bottom")

  # --- 3. Conditionally Add Stats ---
  if (add_stats) {
    p <- p + ggpubr::stat_compare_means(
      aes(group = .data$type),
      method = "wilcox.test",
      paired = TRUE,
      label = "p.signif"
    )
  }

  return(p)
}
