# Description: This script tests the functions in the HanPolNet package.
# It checks data loading, filtering, standardization, HCA calculation,
# and similarity calculations.


library("HanPolNet")
library("ggplot2")
?HanPolNet

# Check if the plant_abundance data is available and looks right
head(plant_abundance)
?plant_abundance

# Check the plant metadata
head(plant_metadata)
?plant_metadata


# Now, test your new function! Let's get all data from 2022.
data_2022 <- get_plant_data(years = 22)
?get_plant_data

head(data_2022)

# How about just the data for counted Trifolium species in 2021?
# Note: I'm using a placeholder `species` here, adjust if needed.
trifolium_counted_21 <- get_plant_data(years = 21,
                                       species = NULL,
                                       counted_stalks = NULL)
head(trifolium_counted_21)




os234 <- get_plant_data(years = c(22,23,24),
                        species = NULL,
                        counted_stalks = NULL)

table(os234$year, os234$plot_id)



## Testing the standardization feature ####
# Get raw data (should look the same as before)
raw_data <- get_plant_data(years = 22)
summary(raw_data$Ach_pta) # Should see values > 1

# Get standardized data
std_data <- ?get_plant_data(years = 22, output = "standardized")
summary(std_data$Ach_pta) # All values should now be between 0 and 1


## Testing the HCA calculation function ####
# First, let's get some standardized data to work with
test_data <- get_plant_data(years = c(22,23,24), output = "standardized")

test_data <- test_data[test_data$Suc_pra > 0,]

# Now, calculate the HCA for that data, using 'Suc_pra' as our focal species
hca_results <- calculate_hca(data = test_data, focal_species = "Suc_pra")

# Let's see what it looks like!
print("Calculated HCA values for 2022:")
head(hca_results)



## Testing the similarity calculation function ####
# --- Test 1: Compare aggregated years ---
all_std_data <- get_plant_data(output = "standardized")
year_similarity <- calculate_similarity(all_std_data, group_by = "year")

# Check the results
cat("Global similarity between years:\n")
cat("Mean:", year_similarity$global_similarity, " | SD:", year_similarity$variation, "\n\n")

cat("Pairwise similarity matrix between years (rounded):\n")
print(round(year_similarity$similarity_matrix, 2))


# --- Test 2: Compare a small subset of plot-years ---
subset_std_data <- get_plant_data(years = "21", plot_id = c(1:5), output = "standardized")
plot_similarity <- calculate_similarity(subset_std_data) # Uses default group_by

cat("\nGlobal similarity between plot-years in the subset:\n")
cat("Mean:", plot_similarity$global_similarity, " | SD:", plot_similarity$variation, "\n\n")

cat("Pairwise similarity matrix for the subset (rounded):\n")
print(round(plot_similarity$similarity_matrix, 2))


## Testing the similarity change plotting function ####
# --- Prepare the necessary data ---
# For year-level plots

all_std_data <- get_plant_data(output = "standardized")
year_similarity <- calculate_similarity(all_std_data, group_by = "year")

# For plot-level plots
plot_level_std_data <- get_plant_data(years = 22:25, output = "standardized")
plot_similarity <- calculate_similarity(plot_level_std_data)


# --- Generate and view each plot ---

# Plot 1: Yearly Turnover
p1 <- plot_similarity_change(year_similarity, plot_type = "yearly_turnover")
print(p1)

# Plot 2: Within-Year Heterogeneity
p2 <- plot_similarity_change(plot_similarity, plot_type = "within_year_heterogeneity")
print(p2)

# Plot 3: Individual Plot Turnover
p3 <- plot_similarity_change(plot_similarity, plot_type = "plot_turnover")
print(p3)

## Testing the HCA neighborhood plotting function ####
# 1. Prepare the data
std_data <- get_plant_data(output = "standardized", years = 22:24)
std_data <- std_data[std_data$Suc_pra > 0,]

hca_results <- calculate_hca(std_data, focal_species = "Suc_pra")

# 2. Generate and view each plot

# Plot 1: Yearly Average
p_year <- plot_hca_neighborhood(hca_results, std_data,
                                focal_species = "Suc_pra",
                                plot_type = "by_year")
print(p_year)

# Plot 2: Single Plot Timeline (e.g., for plot 10)
p_plot10 <- plot_hca_neighborhood(hca_results, std_data,
                                  focal_species = "Suc_pra",
                                  plot_type = "by_plot", plot_id = 10)
print(p_plot10)

# Plot 3: Grid View (let's use a subset to keep it readable)
std_subset <- get_plant_data(years = 21:23, plot_id = unique(std_data$plot_id), output = "standardized")
hca_subset <- calculate_hca(std_subset, focal_species = "Suc_pra")

p_grid <- plot_hca_neighborhood(hca_subset, std_subset,
                                focal_species = "Suc_pra",
                                plot_type = "grid")
print(p_grid)

## Testing the new trends plotting function ####
# Prepare the data
std_data <- get_plant_data(output = "standardized", years = 22:24)
hca_results <- calculate_hca(std_data, focal_species = "Suc_pra")

# Generate the new trends plot
trends_plot <- plot_abundance_trends(hca_results, std_data, focal_species = "Suc_pra", add_stats = FALSE)
plot_abundance_trends
print(trends_plot)





# --- Verification Script ---
# This script verifies that the pollinator_id in the clean dataset
# 1. Load both the clean and raw datasets
load("data/interaction_data.rda")
raw_interactions <- read.csv("data-raw/jine_ctverce_spojene_11_24_vz_20250515 - jine_ctverce_spojene_11_24_vz_20250409.csv")

# 2. Find a suitable row for testing
# We're looking for the first row where druh_final is empty but druhO_opraveno is not.
test_index <- which(
  (is.na(raw_interactions$druh_final) | raw_interactions$druh_final == "") &
    (raw_interactions$druhO_opraveno != "" & !is.na(raw_interactions$druhO_opraveno))
)[1]


# 3. Run the check and print a report
if (!is.na(test_index)) {
  # Get the key identifiers from that row in the RAW data
  raw_sampling_id <- raw_interactions$id_i[test_index]
  expected_id <- as.character(raw_interactions$druhO_opraveno[test_index])

  # Find the matching row in the CLEAN data using the unique sampling_id
  clean_row <- interaction_data[interaction_data$sampling_id == raw_sampling_id, ]
  actual_id <- as.character(clean_row$pollinator_id)

  # Print a summary
  cat("--- Testing Pollinator ID Fallback ---\n")
  cat("Found a test case in raw data row:", test_index, "\n")
  cat("Sampling ID:", raw_sampling_id, "\n")
  cat("  - Raw 'druh_final' was empty.\n")
  cat("  - Expected ID from 'druhO_opraveno':", expected_id, "\n")
  cat("  - Actual ID in final 'pollinator_id':", actual_id, "\n\n")

  # The final check
  if (identical(actual_id, expected_id)) {
    cat("✅ TEST PASSED: The pollinator ID was correctly filled in.\n")
  } else {
    cat("❌ TEST FAILED: The IDs do not match.\n")
  }

} else {
  cat("Could not find a suitable test case in the raw data.\n")
}



## Testing the interaction data retrieval function ####
# Example usage of get_interaction_data()
# Example 1: Get all interactions with Apis mellifera on Succisa pratensis in 2022
pollinators_on_succisa <- get_interaction_data(
is_pollinator = TRUE
)
head(pollinators_on_succisa)

sort(table(pollinators_on_succisa$pollinator_id))


# Example 2: Get interactions from true pollinators, but only those that are NOT verified
unverified_pollinators <- get_interaction_data(
  is_pollinator = TRUE,
  verified_taxa = FALSE
)
head(unverified_pollinators)

# Example 3: Get all interactions that occurred in the morning (e.g., before 11:00 AM)
morning_interactions <- get_interaction_data(end_time = "11:00")
summary(morning_interactions$hour) # Should show hours are all less than 11

# Example 4: Get all interactions involving a specific pollinator species (e.g., "Aglais_io")
get_interaction_data(
  is_pollinator = TRUE, pollinator_species  = "Apis_mellifera"
)
# Example 5: Get all interactions involving a specific plant species (e.g., "Ach_mil")
get_interaction_data(
  is_pollinator = TRUE, plant_species  = "Suc_pra", start_time = "08:00", end_time = "18:00"
)



## Testing the sampling effort calculation function ####
# --- Full Workflow Example ---

# 1. Calculate sampling effort ONCE from the full dataset
effort <- calculate_sampling_effort(interaction_data)

# 2. Filter for a specific subset of interactions you care about
# (e.g., true pollinators in 2022)
pollinator_visits_22 <- get_interaction_data(
  years = 22,
  is_pollinator = TRUE
)

# 3. Summarize and standardize this subset
# Let's find the interaction rate per plant species in each plot
interaction_rates <- summarize_interactions(
  data = pollinator_visits_22,
  effort_data = effort,
  plot_id, experiment_run, plant_code # These are the grouping variables
)

# View the results!
head(interaction_rates)

#
# Calculate effort using the full, unfiltered interaction_data
effort <- calculate_sampling_effort(interaction_data)

# Let's inspect the result
cat("Calculated sampling effort (first 6 rows):\n")
print(head(effort))


# Filter for true pollinators on Suc_pra in year 22
succisa_interactions_22 <- get_interaction_data(
  years = 21,
  plant_species = "Suc_pra",
  is_pollinator = TRUE,
  experiment_run = c( "Handrkov13_VIII",
                      "Handrkov12_VIII",
                      "Handrkov14_VIII",
                      "Handrkov15_VIII",
                      "Handrkov16_VIII",
                      "Handrkov21_VIII",
                      "Handrkov11_VIII",
                      "Handrkov24_VIII",
                      "Handrkov23_VIII",
                      "Handrkov17_VIII",
                      "Handrkov19_VIII",
                      "Handrkov22_VIII",
                      "Handrkov20_VIII",
                      "Handrkov18_VIII")
)

cat("\nFiltered data contains", nrow(succisa_interactions_22), "interaction records.\n")

# Calculate the standardized rate for each plot
succisa_rates_per_plot <- summarize_interactions(
  data = succisa_interactions_22,
  effort_data = effort,
  plot_id, experiment_run # Group by plot
)

cat("\nStandardized interaction rates for Suc_pra in 2022 (first 6 rows):\n")
print(head(succisa_rates_per_plot))



# --- MANUAL VERIFICATION ---
# Let's check the result for the very first plot in our summary table
# --- Full Workflow & Verification ---

# 1. First, ensure your package is up-to-date with all changes
devtools::load_all()

# 2. Calculate the sampling effort from the full, clean interaction_data
# This now correctly includes the "zero-interaction" visits
effort <- calculate_sampling_effort(interaction_data)

# 3. Filter for our specific research question
# (True pollinators on Succisa pratensis in 2022)
succisa_interactions_22 <- get_interaction_data(
  years = 22,
  plant_species = "Suc_pra",
  is_pollinator = TRUE
)

# 4. Summarize and standardize this filtered subset
# We'll group by plot_id and the experiment_run
succisa_rates_per_plot <- summarize_interactions(
  data = succisa_interactions_22,
  effort_data = effort,
  plot_id, experiment_run
)

cat("Standardized interaction rates for Suc_pra in 2022 (first 6 rows):\n")
print(head(succisa_rates_per_plot))


# --- MANUAL VERIFICATION ---
# Let's check the result for the very first plot in our summary table
if (nrow(succisa_rates_per_plot) > 0) {

  first_plot_id <- succisa_rates_per_plot$plot_id[1]
  first_run <- succisa_rates_per_plot$experiment_run[1]
  cat("\n--- Manually verifying Plot", first_plot_id, "in Run", as.character(first_run), "---\n")

  # 1. Manually count total interactions for this plot in our filtered data
  manual_total_interactions <- sum(
    succisa_interactions_22$interaction_count[
      succisa_interactions_22$plot_id == first_plot_id &
        succisa_interactions_22$experiment_run == first_run
    ],
    na.rm = TRUE
  )
  cat("Manual sum of interactions:", manual_total_interactions, "\n")

  # 2. Get the sampling effort for this plot from our effort table
  manual_n_visits <- effort$n_visits[
    effort$plot_id == first_plot_id &
      effort$experiment_run == first_run
  ]
  cat("Sampling effort (n_visits):", manual_n_visits, "\n")

  # 3. Manually calculate the rate
  manual_rate <- manual_total_interactions / manual_n_visits
  cat("Manual rate calculation:", manual_rate, "\n")

  # 4. Get the rate calculated by the function
  function_rate <- succisa_rates_per_plot$rate[1]
  cat("Function-calculated rate:", function_rate, "\n\n")

  # 5. Compare the results
  if (isTRUE(all.equal(manual_rate, function_rate))) {
    cat("✅ TEST PASSED: The manual calculation matches the function output.\n")
  } else {
    cat("❌ TEST FAILED: The calculations do not match.\n")
  }
}



## Testing the interaction rate summarization function ####
# --- Full Workflow Example ---

# 1. Get the data you want to analyze (e.g., true pollinators only)
pollinator_interactions <- get_interaction_data(is_pollinator = TRUE)

# 2. Get the locality-level summary (average rate per year)
yearly_rates <- summarize_interaction_rate(
  data = pollinator_interactions,
  focal_plant = "Suc_pra",
  group_by = c("year")
)
cat("--- Yearly Interaction Rates for Succisa pratensis ---\n")
print(yearly_rates)

# 3. Get the plot-level summary (rate for each plot in each year)
plot_rates <- summarize_interaction_rate(
  data = pollinator_interactions,
  focal_plant = "Suc_pra",
  group_by = c("year", "plot_id")
)
cat("\n--- Plot-Level Interaction Rates (first 6 rows) ---\n")
print(head(plot_rates))

# 4. Now you can easily plot these results!
# For example, a simple line plot of the yearly trend:
ggplot(yearly_rates, aes(x = year, y = rate)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Interaction Rate on Succisa pratensis (Locality Average)")



### Testing the pollinator turnover calculation function ####
# --- Corrected Workflow Example ---

# 1. Get standardized interaction data (e.g., true pollinators)
# We need standardize = TRUE to get the rate column later
pollinator_interactions_std <- get_interaction_data(is_pollinator = TRUE, standardize = TRUE)

# 2. Calculate TOTAL interaction rate PER PLOT/YEAR for the focal plant
# This uses summarize_interaction_rate as intended
focal_plant_rates <- summarize_interaction_rate(
  data = pollinator_interactions_std,
  focal_plant = "Suc_pra",
  group_by = c("year", "plot_id")
)
cat("--- Plot-Level TOTAL Interaction Rates for Succisa pratensis ---\n")
print(head(focal_plant_rates))

# 3. Prepare data for TURNOVER analysis
# We need the interaction rate PER POLLINATOR visiting the focal plant
# Use the general summarize_interactions function for this.
# First, filter the standardized data for the focal plant
focal_plant_interactions <- pollinator_interactions_std %>%
  filter(plant_code == "Suc_pra")

# Load the full effort data (calculated from raw data)
effort <- calculate_sampling_effort(interaction_data)

# Now summarize rate per pollinator visiting the focal plant
turnover_input_data <- summarize_interactions(
  data = focal_plant_interactions,
  effort_data = effort,
  experiment_run, year, plot_id, pollinator_id # Group by year, plot, AND pollinator
)
cat("\n--- Data Ready for Turnover Calculation (first 6 rows) ---\n")
print(head(turnover_input_data))


# 4. Calculate Plot-Level Pollinator Turnover using the correctly prepared data
plot_turnover <- calculate_pollinator_turnover(
  rate_data = turnover_input_data,
  focal_plant = "Suc_pra", # Label for context, not used in calculation
  level = "plot"
)

cat("\n--- Plot-Level Pollinator Turnover (first 6 rows) ---\n")
print(head(plot_turnover))

# 5. Plot the plot-level turnover
ggplot(plot_turnover, aes(x = year_pair, y = similarity, group = plot_id, color = as.factor(plot_id))) +
  geom_line() +
  labs(title = "Pollinator Community Turnover on Succisa pratensis (Plot Level)",
       x = "Year-to-Year Comparison", y = "Ruzicka Similarity", color = "Plot ID") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")











