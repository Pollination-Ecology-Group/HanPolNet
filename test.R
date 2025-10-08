# Description: This script tests the functions in the HanPolNet package.
# It checks data loading, filtering, standardization, HCA calculation,
# and similarity calculations.


library("HanPolNet")
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

