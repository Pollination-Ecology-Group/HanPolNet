#' Calculate the Heterospecific Co-flowering Abundance (HCA) Index
#'
#' This function calculates the abundance of co-flowering heterospecifics for
#' each plot and year from a standardized abundance dataset.
#'
#' @details
#' Calculation of HCA
#'
#' The HCA index is calculated for each row (a unique plot-year combination) using
#' the formula:
#'
#' $$HCA_{y,p} = \sum_{i=1}^{N_{co}} w_i \times \text{Abundance}_i$$
#'
#' Where:
#' \itemize{
#'   \item `HCA_{y,p}` is the Heterospecific Co-flowering Abundance for a given year `y` and plot `p`.
#'   \item `N_{co}` is the number of co-flowering heterospecific species in the dataset.
#'   \item `w_i` is the weight for species `i`. If not provided, it defaults to 1 for all species.
#'   \item `Abundance_i` is the standardized abundance of species `i` (ranging from 0 to 1).
#' }
#'
#' ## Interpreting the HCA Value
#' The HCA is an **unbounded cumulative score**, not a proportion.
#' \itemize{
#'   \item A value of **0** means there is no co-flowering heterospecific abundance in that plot (i.e., no other species were present).
#'   \item A **higher value** indicates a greater cumulative abundance of surrounding heterospecific plants. For example, with default weights of 1, an HCA of 2.5 could mean that five species were present, each at 50% of its maximum observed abundance (5 * 0.5), or that one species was at its maximum (1.0) and three others were at 50% abundance (1.0 + 3 * 0.5).
#' }
#'
#' @param data A data frame containing standardized abundance data, typically the
#'   output of `get_plant_data(output = "standardized")`. Must contain 'year'
#'   and 'plot_id' columns.
#' @param focal_species A single character string specifying the plant code of the
#'   focal species. This species will be excluded from the HCA calculation. If
#'   `NULL`, the sum is across all species present.
#' @param weights An optional named numeric vector where names are species codes
#'   and values are the weights (`wi`). Species in the data that are not in this
#'   vector will be assigned a weight of 0. If `NULL`, all species get a weight of 1.
#'
#' @return A data frame with three columns: `year`, `plot_id`, and `hca`.
#' @export
#'
#' @examples
#' # First, get standardized data for a few years
#' std_data <- get_plant_data(years = c(21, 22), output = "standardized")
#'
#' # Calculate unweighted HCA, excluding 'Suc_pra' as the focal species
#' hca_values <- calculate_hca(std_data, focal_species = "Suc_pra")
#'
#' head(hca_values)
#'

calculate_hca <- function(data, focal_species = NULL, weights = NULL) {

  # --- 1. Input Validation ---
  if (!all(c("year", "plot_id") %in% colnames(data))) {
    stop("Input 'data' must contain 'year' and 'plot_id' columns.")
  }

  # --- 2. Identify Species Columns ---
  # Assume all columns other than year and plot_id are species abundances
  species_cols <- setdiff(colnames(data), c("year", "plot_id"))

  # --- 3. Exclude the Focal Species ---
  if (!is.null(focal_species)) {
    if (!focal_species %in% species_cols) {
      warning("focal_species '", focal_species, "' not found in data columns.")
    }
    species_cols <- setdiff(species_cols, focal_species)
  }

  # Subset the data to only the species we want to sum
  abundance_data <- data[, species_cols, drop = FALSE]

  # --- 4. Prepare Weights ---
  if (is.null(weights)) {
    # If no weights are provided, create a named vector of 1s
    w <- rep(1, length(species_cols))
    names(w) <- species_cols
  } else {
    # If weights are provided, use them. For any species not in the weights vector,
    # assign a weight of 0 so they don't contribute to the sum.
    w <- setNames(rep(0, length(species_cols)), species_cols)
    # Find the species that are in both the data and the provided weights
    common_species <- intersect(names(w), names(weights))
    w[common_species] <- weights[common_species]
  }

  # --- 5. Calculate HCA ---
  # This is a fast way to multiply each column by its corresponding weight
  # and then sum across the rows.

  # Ensure the weights are in the same order as the data columns before multiplying
  w_ordered <- w[colnames(abundance_data)]

  # The sweep() function is perfect for column-wise operations like this
  weighted_abundances <- sweep(abundance_data, 2, w_ordered, FUN = "*")
  hca_values <- rowSums(weighted_abundances, na.rm = TRUE)

  # --- 6. Format and Return Output ---
  result <- data.frame(
    year = data$year,
    plot_id = data$plot_id,
    hca = hca_values
  )

  return(result)
}
