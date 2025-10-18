#' Analyze and Plot Pollinator Sharing Turnover
#'
#' This function calculates and visualizes the change in pollinator sharing
#' (Morisita-Horn similarity) over a specified time variable (e.g., year).
#'
#' @details
#' The function works by splitting the input data by the `compare_by` variable
#' (e.g., for each year). For each data subset, it calculates the pollinator
#' sharing metric using `calculate_pollinator_sharing()`. It then combines the
#' results and generates a line plot showing the trend over time.
#'
#' @param data A data frame of interaction data, typically filtered by
#'   `get_interaction_data()`. It must be standardized (contain a `rate` column)
#'   if `use_rate = TRUE`.
#' @param compare_by A character string specifying the column name to compare
#'   across (e.g., `"year"`). This column must be numeric.
#' @param level A character string specifying the level of analysis, passed to
#'   `calculate_pollinator_sharing()`: `"community"` (the default) or `"species"`.
#' @param use_rate A logical value. If `TRUE` (the default), the function will
#'   use the standardized `rate` column. If `FALSE`, it will use raw
#'   `interaction_count`.
#'
#' @return A ggplot object showing the trend of pollinator sharing over time.
#' @export
#' @import ggplot2
#' @importFrom dplyr %>% bind_rows
#'
pollinator_sharing_turnover <- function(data, compare_by = "year", level = "community", use_rate = TRUE) {

  # --- 1. Input Validation ---
  if (!compare_by %in% names(data)) {
    stop("The `compare_by` column '", compare_by, "' was not found in the data.")
  }

  # --- 2. Calculate Sharing Scores for each Time Point ---
  time_points <- sort(unique(data[[compare_by]]))

  turnover_results <- lapply(time_points, function(tp) {
    # Subset data for the current time point
    data_subset <- data[data[[compare_by]] == tp, ]

    if (nrow(data_subset) > 0) {
      sharing_score <- calculate_pollinator_sharing(
        data = data_subset,
        level = level,
        use_rate = use_rate
      )

      # Add the time point to the results
      if (is.data.frame(sharing_score)) {
        sharing_score[[compare_by]] <- tp
      } else { # It's a single value for community level
        sharing_score <- data.frame(
          score = sharing_score,
          placeholder = "Community"
        )
        names(sharing_score)[1] <- "score" # rename to avoid conflict
        sharing_score[[compare_by]] <- tp
      }
      return(sharing_score)
    } else {
      return(NULL)
    }
  })

  # Combine the list of results into a single data frame
  turnover_df <- dplyr::bind_rows(turnover_results)

  if (nrow(turnover_df) == 0) {
    warning("No data was generated. Check if input data is empty.")
    return(NULL)
  }

  # --- 3. Create the Plot ---
  if (level == "community") {
    p <- ggplot(turnover_df, aes(x = .data[[compare_by]], y = .data$score)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 4) +
      labs(
        title = "Overall Pollinator Sharing Turnover",
        x = compare_by,
        y = "Mean Community-Wide Pollinator Sharing"
      )
  } else { # level == "species"
    p <- ggplot(turnover_df, aes(x = .data[[compare_by]], y = .data$mean_sharing_score,
                                 group = .data$plant_code, color = .data$plant_code)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.5) +
      labs(
        title = "Pollinator Sharing Turnover for Individual Plant Species",
        x = compare_by,
        y = "Mean Pollinator Sharing Score"
      ) +
      theme(legend.position = "none") # Hide legend for clarity
  }

  return(p + scale_x_continuous(breaks = time_points) + ylim(0, 1) + theme_minimal())
}
