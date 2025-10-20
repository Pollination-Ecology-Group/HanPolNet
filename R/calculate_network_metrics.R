#' Calculate Network- and Species-Level Metrics
#'
#' This function calculates a suite of common metrics for bipartite
#' plant-pollinator networks.
#'
#' @param data A data frame of interaction data, typically filtered by
#'   `get_interaction_data()`.
#' @param level A character string: `"network"` (the default) calculates metrics
#'   for the entire network. `"species"` calculates metrics for each individual species.
#'
#' @return If `level = "network"`, a data frame with one row containing network-level
#'   metrics (Connectance, NODF, Modularity). If `level = "species"`, a data frame
#'   with metrics for each species (Normalized Degree, d' Specialization,
#'   Betweenness Centrality).
#' @importFrom bipartite networklevel specieslevel
#' @importFrom igraph graph_from_incidence_matrix betweenness
#' @export
#'
calculate_network_metrics <- function(data, level = "network") {

  # --- 1. Input Validation and Matrix Creation ---
  if (!level %in% c("network", "species")) {
    stop("`level` must be either 'network' or 'species'.")
  }

  # Use raw counts for network structure metrics
  network_matrix <- data %>%
    dplyr::filter(!is.na(.data$plant_code) & !is.na(.data$pollinator_id)) %>%
    dplyr::group_by(.data$plant_code, .data$pollinator_id) %>%
    dplyr::summarise(n = sum(.data$interaction_count, na.rm = TRUE), .groups = 'drop') %>%
    tidyr::pivot_wider(
      names_from = .data$pollinator_id,
      values_from = .data$n,
      values_fill = 0
    ) %>%
    tibble::column_to_rownames(var = "plant_code")

  if (nrow(network_matrix) < 2 || ncol(network_matrix) < 2) {
    warning("Network matrix is too small to calculate metrics.")
    return(NULL)
  }

  # --- 2. Calculate Metrics based on Level ---
  if (level == "network") {
    # The `bipartite::networklevel` function calculates many metrics at once
    network_metrics <- bipartite::networklevel(
      network_matrix,
      index = c("connectance", "nestedness") # nestedness calculates NODF
    )

    # Modularity is calculated separately
    # Use suppressWarnings as it can be verbose
    modules <- suppressWarnings(bipartite::computeModules(network_matrix))
    modularity_score <- modules@likelihood

    results <- data.frame(
      connectance = network_metrics["connectance"],
      nodf = network_metrics["nestedness"],
      modularity = modularity_score
    )
    rownames(results) <- NULL

  } else { # level == "species"
    # The `bipartite::specieslevel` function calculates degree and d'
    sl_metrics <- bipartite::specieslevel(
      network_matrix,
      index = c("normalised degree", "d")
    )

    # Betweenness centrality is calculated using igraph
    graph <- igraph::graph_from_incidence_matrix(network_matrix, weighted = TRUE)
    betweenness_scores <- igraph::betweenness(graph, directed = FALSE, normalized = TRUE)

    # Combine the results
    plants <- sl_metrics$`higher level` %>%
      tibble::rownames_to_column("species_code") %>%
      dplyr::mutate(level = "plant")

    pollinators <- sl_metrics$`lower level` %>%
      tibble::rownames_to_column("species_code") %>%
      dplyr::mutate(level = "pollinator")

    results <- dplyr::bind_rows(plants, pollinators) %>%
      dplyr::left_join(
        data.frame(
          species_code = names(betweenness_scores),
          betweenness_centrality = betweenness_scores
        ),
        by = "species_code"
      )
  }

  return(results)
}
