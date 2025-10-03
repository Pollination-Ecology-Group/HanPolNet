#' Plant Abundance Data
#'
#' A dataset containing plant abundance counts or scores from research plots
#' collected between 2011 and 2025.
#'
#' @format A data frame with rows for each plot-year observation and columns for each species.
#' \describe{
#'   \item{year}{The year of the observation (e.g., 11, 12, ... 25).}
#'   \item{plot_id}{The numeric identifier for the research plot.}
#'   \item{...}{Subsequent columns are species codes (e.g., "Ach_mil"), with
#'   numeric values representing abundance.}
#' }
#' @source Original field data collected from <Your Field Site Name>.
"plant_abundance"

#' Plant Species Metadata
#'
#' A support table providing details for each plant species code used in the
#' `plant_abundance` dataset.
#'
#' @format A data frame with one row for each species.
#' \describe{
#'   \item{plant_code}{A short, unique code for the species (e.g., "Ach_mil").}
#'   \item{plant_name}{The full scientific name of the plant species.}
#'   \item{stalk_counted}{An indicator of the survey method: `1` if individual
#'   stalks were counted, `0` if only presence was noted.}
#' }
#' @source Compiled from field notes and botanical references.
"plant_metadata"
