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

#' Plant-Pollinator Interaction Data
#'
#' A dataset containing observations of pollinator visits to plants,
#' recorded between 2011 and 2024.
#'
#' @format A data frame with one row per interaction event.
#' \describe{
#'   \item{sampling_id}{A unique identifier for each sampling event.}
#'   \item{year, month, day, hour, minute}{Date and time of the observation.}
#'   \item{plot_id}{The numeric identifier for the research plot, linking to `plant_abundance`.}
#'   \item{plant_code}{The short code for the plant species, linking to `plant_abundance`.}
#'   \item{plant_name}{The full scientific name of the plant species.}
#'   \item{pollinator_id}{The identifier for the pollinator species.}
#'   \item{interaction_count}{The number of individuals observed in the interaction.}
#'   \item{experiment_run}{An identifier for the experimental run or locality.}
#'   \item{sex}{The sex of the pollinator, if recorded.}
#'   \item{shade}{A flag indicating if the observation was in the shade.}
#' }
"interaction_data"
