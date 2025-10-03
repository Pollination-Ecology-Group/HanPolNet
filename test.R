

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



