#' Visualize Community Similarity Changes
#'
#' Creates one of three plot types to visualize community similarity dynamics
#' over time, based on the output of `calculate_similarity()`.
#'
#' @param similarity_results The list object returned by `calculate_similarity()`.
#'   For `plot_type = "yearly_turnover"`, the input should be from `group_by = "year"`.
#'   For the other two plot types, the input should be from the more detailed
#'   `group_by = c("year", "plot_id")`.
#' @param plot_type A character string specifying the plot to generate. Must be one of:
#'   \itemize{
#'     \item `"yearly_turnover"`: A line plot showing the similarity of each year's
#'       average community to the previous year.
#'     \item `"within_year_heterogeneity"`: Violin plots showing the distribution of
#'       pairwise similarities between all plots within each year.
#'     \item `"plot_turnover"`: A line plot tracking the similarity of each
#'       individual plot to itself in the previous year.
#'   }
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#'
plot_similarity_change <- function(similarity_results, plot_type) {

  # --- Input Validation ---
  valid_types <- c("yearly_turnover", "within_year_heterogeneity", "plot_turnover")
  if (!plot_type %in% valid_types) {
    stop("`plot_type` must be one of: ", paste(valid_types, collapse = ", "))
  }

  sim_matrix <- similarity_results$similarity_matrix

  # --- Plot Type 1: Yearly Turnover ---
  if (plot_type == "yearly_turnover") {
    years <- as.numeric(rownames(sim_matrix))
    turnover_data <- data.frame(
      year_pair = paste(years[-length(years)], years[-1], sep = "-"),
      similarity_to_previous = diag(sim_matrix[-1, -ncol(sim_matrix)])
    )

    p <- ggplot(turnover_data, aes(x = .data$year_pair, y = .data$similarity_to_previous, group = 1)) +
      geom_line() +
      geom_point(size = 3) +
      labs(
        title = "Yearly Community Turnover",
        subtitle = "Similarity of each year to the previous year",
        x = "Year-to-Year Comparison",
        y = "Ruzicka Similarity"
      ) +
      ylim(0, 1) +
      theme_minimal()
  }

  # --- Plot Type 2: Within-Year Heterogeneity (UPDATED TO VIOLIN PLOT) ---
  if (plot_type == "within_year_heterogeneity") {
    sim_matrix[lower.tri(sim_matrix, diag = TRUE)] <- NA
    sim_df <- as.data.frame(as.table(sim_matrix), stringsAsFactors = FALSE)
    names(sim_df) <- c("sample1", "sample2", "similarity")
    sim_df <- na.omit(sim_df)

    sim_df$year1 <- as.numeric(sapply(strsplit(sim_df$sample1, "_"), `[`, 1))
    sim_df$year2 <- as.numeric(sapply(strsplit(sim_df$sample2, "_"), `[`, 1))

    within_year_df <- sim_df[sim_df$year1 == sim_df$year2, ]

    p <- ggplot(within_year_df, aes(x = as.factor(.data$year1), y = .data$similarity)) +
      # Add a violin layer to show the density distribution
      geom_violin(trim = FALSE) +
      # Add the jittered points on top with transparency
      geom_jitter(height = 0, width = 0.1, alpha = 0.3) +
      labs(
        title = "Within-Year Community Heterogeneity",
        subtitle = "Distribution of pairwise similarities between plots in each year",
        x = "Year",
        y = "Ruzicka Similarity"
      ) +
      ylim(0, 1) +
      theme_minimal()
  }

  # --- Plot Type 3: Individual Plot Turnover ---
  if (plot_type == "plot_turnover") {
    sim_df <- as.data.frame(as.table(sim_matrix), stringsAsFactors = FALSE)
    names(sim_df) <- c("sample1", "sample2", "similarity")

    sample1_split <- strsplit(as.character(sim_df$sample1), "_")
    sample2_split <- strsplit(as.character(sim_df$sample2), "_")

    sim_df$year1 <- as.numeric(sapply(sample1_split, `[`, 1))
    sim_df$plot1 <- as.numeric(sapply(sample1_split, `[`, 2))
    sim_df$year2 <- as.numeric(sapply(sample2_split, `[`, 1))
    sim_df$plot2 <- as.numeric(sapply(sample2_split, `[`, 2))

    all_years <- sort(unique(c(sim_df$year1, sim_df$year2)))
    year_pairs <- data.frame(year1 = all_years[-length(all_years)], year2 = all_years[-1])

    plot_turnover_df <- sim_df[which(sim_df$plot1 == sim_df$plot2), ]
    plot_turnover_df <- merge(plot_turnover_df, year_pairs, by = c("year1", "year2"))

    plot_turnover_df$year_pair <- paste(plot_turnover_df$year1, plot_turnover_df$year2, sep = "-")

    p <- ggplot(plot_turnover_df, aes(x = .data$year_pair, y = .data$similarity, group = .data$plot1, color = as.factor(.data$plot1))) +
      geom_line() +
      geom_point() +
      labs(
        title = "Individual Plot Turnover",
        subtitle = "Similarity of each plot to itself in the previous year",
        x = "Year-to-Year Comparison",
        y = "Ruzicka Similarity",
        color = "Plot ID"
      ) +
      ylim(0, 1) +
      theme_minimal()

    if(length(unique(plot_turnover_df$plot1)) > 15) {
      p <- p + theme(legend.position = "none")
    }
  }

  return(p)
}
