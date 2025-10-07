

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
test_data <- get_plant_data(years = 22, output = "standardized")

# Now, calculate the HCA for that data, using 'Suc_pra' as our focal species
hca_results <- ?calculate_hca(data = test_data, focal_species = "Suc_pra")

# Let's see what it looks like!
print("Calculated HCA values for 2022:")
head(hca_results)
