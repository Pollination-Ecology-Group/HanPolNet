# READ ME

# HanPolNet

The goal of HanPolNet is to provide the data and functions necessary to analyze the interannual changes in plant-pollinator interaction networks.

## Installation

You can install the development version of HanPolNet from [GitHub](https://github.com) with:

```{r}
install.packages("devtools")
devtools::install_github("Pollination-Ecology-Group/HanPolNet") 
```

## Citation

If you use this package or its data in your research, please cite it as:

```{r}
citation("HanPolNet")
```

## Authors

This package was developed by Jakub Štenc with guidance and contributions from Larry (AI Assistant), a persona of Gemini, a large language model from Google.

------------------------------------------------------------------------

## Purpose

This package serves to combine data collected during decade and half long lasting project called "Opylovači" now operated by Pollination Ecology Group and Flower Ecology Lab based at the Departments of Zoology and Botany, Faculty of Science, Charles University (Praha, CZE).

------------------------------------------------------------------------

## How does this package works

#### A Typical Workflow

A common analysis with this package involves a clear sequence of steps: from getting clean data to calculating indices and finally visualizing the results.

Example:

```{r}
library(HanPolNet)

# 1. GET DATA: Retrieve standardized plant abundance and interaction data.
plant_data_std <- get_plant_data(output = "standardized")
interaction_data_std <- get_interaction_data(is_pollinator = TRUE, standardize = TRUE)

# 2. CALCULATE INDICES: Compute metrics like HCA or community similarity.
hca_results <- calculate_hca(plant_data_std, focal_species = "Suc_pra")
similarity_results <- calculate_similarity(plant_data_std, group_by = "year")

# 3. VISUALIZE: Create plots to explore trends and patterns.
plot_abundance_trends(hca_results, plant_data_std, focal_species = "Suc_pra")

plot_pollinator_diet(focal_plant = "Suc_pra", years = 22:24)
```

### How Data is Organized in This Package

To ensure transparency and reproducibility, the data in this package follows a standard R package workflow, separating the raw source data from the clean, ready-to-use data. Think of it like a kitchen and a pantry.

#### `data/` (The Pantry)

This directory contains the final, clean datasets saved in R's optimized `.rda` format. When you load the package, this data is immediately available for your analysis.

-   `plant_abundance`: A tidy data frame of plant counts, with year and plot ID as separate columns.

-   `plant_metadata`: A support table with information about each plant species, including their full name and counting method.

-   `interaction_data`: A data frame of individual plant-pollinator interaction observations.

Think of the `data/` folder as a well-organized pantry, stocked with ingredients ready to be used in your analytical recipes.

------------------------------------------------------------------------

### `data-raw/` (The Kitchen)

This directory is the "kitchen" where the data was prepared. It contains the original raw data files (e.g., `.csv` files) and the R scripts used to process them into the final versions found in the `data/` folder.

This directory is part of the development source code but is **not included** in the final installed package. Its purpose is to provide a complete, reproducible trail from the original source files to the analysis-ready data. If you ever need to see exactly how the data was cleaned, the "recipe" is in here.

------------------------------------------------------------------------

### Included Datasets

This package contains three core data frames that provide information on plant abundance, species metadata, and plant-pollinator interactions.

------------------------------------------------------------------------

#### `plant_abundance`

A data frame of plant abundance, with each row representing an observation for a specific plot in a specific year.

-   `year`: The year of the observation (numeric, e.g., 21 for 2021).

-   `plot_id`: The unique numeric identifier for the research plot.

-   **[Species Columns]**: All subsequent columns are named with plant species codes (e.g., `Ach_mil`). The values are the abundance scores, which are either direct stalk counts or a 0-64 sublot presence score.

------------------------------------------------------------------------

#### `plant_metadata`

A support table that provides descriptive information for each plant species found in the `plant_abundance` dataset.

-   `plant_code`: A short, unique code for the species (e.g., `Ach_mil`), used as the column name in `plant_abundance`.

-   `plant_name`: The full scientific name of the plant species (e.g., `Achillea millefolium`).

-   `stalk_counted`: An indicator of the survey method. A value of `1` means individual stalks were counted; a value of `0` means a 0-64 presence/absence score in subplots was used.

------------------------------------------------------------------------

#### `interaction_data`

A data frame containing individual observations of pollinator visits to plants. Each row represents a single recorded interaction event.

-   `sampling_id`: A unique identifier for the specific sampling event, combining date, time, and plot information.

-   `year`, `month`, `day`, `hour`, `minute`: The date and time of the observation.

-   `plot_id`: The numeric identifier for the research plot, which links to the `plant_abundance` data.

-   `plant_code`: The short code for the plant species that was visited, linking to `plant_metadata`.

-   `plant_name`: The full scientific name of the plant species.

-   `pollinator_id`: The identifier for the observed pollinator species.

-   `interaction_count`: The number of pollinator individuals observed during the event (`pocet`).

-   `experiment_run`: An identifier for the experimental run or locality (`beh`).

-   `sex`: The sex of the pollinator (`pohlavi`), if recorded.

-   `shade`: A flag (`stin`) indicating if the observation was made in the shade.

------------------------------------------------------------------------

#### `pollinator_metadata`

A support table that provides taxonomic classification and functional trait information for each unique pollinator taxon found in the `interaction_data` dataset.

-   `pollinator_id`: The unique identifier for the pollinator taxon, used to link with the `interaction_data` table.

-   `verified`: A logical flag (`TRUE`/`FALSE`) indicating if the taxonomic identification of the pollinator has been verified at least once in the raw data.

-   `is_pollinator`: A logical flag (`TRUE`/`FALSE`) classifying whether the taxon is considered an effective pollinator. This is based on general ecological knowledge (e.g., bees are `TRUE`, ants are `FALSE`). Larval stages are automatically classified as `FALSE`.

-   `order`: The taxonomic order of the pollinator (e.g., "Hymenoptera", "Diptera").

-   `family`: A placeholder column for the taxonomic family, intended for future manual curation.

-   `genus`: A placeholder column for the taxonomic genus, intended for future manual curation.

-   `notes`: A placeholder column for any additional notes.

------------------------------------------------------------------------

## Core Functions

### Plant Data Access and Standardization: `get_plant_data()`

The primary function for accessing data in this package is `get_plant_data()`. It allows you to retrieve the plant abundance dataset with powerful options for filtering and preprocessing.

#### Filtering

You can filter the dataset by any combination of the following parameters: \* `years` \* `plot_id` \* `species` \* `counted_stalks` (to select for a specific survey method)

#### Standardization

A key feature of this function is its ability to standardize the abundance data via the `output = "standardized"` argument. This is crucial for analysis because the raw data was collected using two different methods:

1.  **Sublot Counts:** For some species, abundance was recorded as a score from 0 to 64, representing the number of subplots where the species was present.
2.  **Stalk Counts:** For other species, the absolute number of individual stalks was counted.

To make these two scales comparable for analysis, the standardization process converts both to a **common 0-to-1 scale**:

-   **Sublot Counts** are converted to a proportion by **dividing by 64**. A score of 64 becomes 1.0 (maximum frequency).
-   **Stalk Counts** are scaled relative to the maximum abundance observed for that specific species across the entire dataset. Each value is **divided by that species' global maximum**, resulting in a scale where the highest observed count for that species is 1.0.

By default (`output = "raw"`), the function returns the original, unstandardized values.

------------------------------------------------------------------------

### Interaction Data: `get_interaction_data()`

This is the main function for accessing and preparing the plant-pollinator interaction data for analysis. It allows for detailed filtering based on a variety of parameters and can automatically standardize interaction counts by sampling effort.

#### Filtering

The function can filter the interaction dataset by any combination of the following arguments:

-   `years`, `plot_id`, `plant_species`, `pollinator_species`

-   `experiment_run` (to isolate specific experimental setups)

-   `start_time` and `end_time` (to select interactions within a specific time window)

-   `verified_taxa` (to select only taxonomically verified pollinators)

-   `is_pollinator` (to select only taxa classified as true pollinators)

-   `remove_zeros` (to exclude records where no interaction was observed, i.e., "nothing" was recorded)

#### Standardization

A key feature is the `standardize = TRUE` argument. When used, the function calculates and adds two new columns to the dataset:

1.  **`n_visits`**: This is the **sampling effort**. It represents the total number of unique sampling visits made to a specific plot within a specific `experiment_run`. This value is calculated from the *entire, unfiltered* dataset to ensure it is a true measure of effort.

2.  **`rate`**: This is the **standardized interaction rate**, calculated for each row as `interaction_count / n_visits`. This value represents the number of observed interactions per sampling visit, making it comparable across plots and years with different sampling intensities.

If `standardize = FALSE` (the default), the function returns only the raw `interaction_count`.

------------------------------------------------------------------------

### Analytical Functions

This package includes two primary analytical functions: `calculate_hca()` for assessing co-flowering abundance and `calculate_similarity()` for comparing community composition.

### Heterospecific Co-flowering Abundance: `calculate_hca()`

#### Calculating the Heterospecific Co-flowering Abundance (HCA) Index

The primary analytical function in this package is `calculate_hca()`. It is designed to compute a community-level index of heterospecific plant abundance for each unique plot and year.

##### The Formula

The function implements the following formula:

$$HCA_{y,p} = \sum_{i=1}^{N_{co}} w_i \times \text{Abundance}_i$$

Where: \* $HCA_{y,p}$ is the Heterospecific Co-flowering Abundance for a given year `y` and plot `p`. \* $N_{co}$ is the number of co-flowering heterospecific species being considered (i.e., all species except the `focal_species`). \* $w_i$ is a weighting factor for each species `i`. By default, this is set to 1 for all species. \* $\text{Abundance}_i$ is the standardized abundance of species `i`. This requires that the input data be on a common scale. The `get_plant_data(output = "standardized")` function is designed specifically to prepare data for this calculation.

##### Interpreting the HCA Value

The HCA is an **unbounded cumulative score**, not a proportion. It represents the total standardized abundance of neighboring plants.

-   A value of **0** indicates that no co-flowering heterospecifics were present in the plot.
-   A **higher value** indicates a greater cumulative abundance of co-flowering partners. For example, with default weights of 1, an HCA of 2.5 could mean that five different species were present, each at 50% of its maximum observed abundance (5 \* 0.5), or a different combination of fewer, more abundant species.

### Visualizing Yearly Trends: `plot_abundance_trends()`

To assess whether year-to-year changes in abundance are statistically significant, the `plot_abundance_trends()` function provides a powerful visualization.

#### The Visualization

The function produces faceted boxplots, creating two separate panels: 1. **Conspecific:** Shows the distribution of the focal species' abundance across all plots for each year. 2. **Heterospecific (HCA):** Shows the distribution of the HCA index across all plots for each year.

This allows you to see the trend and variation for both metrics side-by-side.

#### The Statistical Test

This plot automatically performs a **Wilcoxon rank-sum test** (also known as a Mann-Whitney U test) to compare the distributions between **consecutive years**. The data is treated as unpaired because the set of observations from one year is considered an independent group from the next. The resulting p-values are displayed on the plot with brackets, allowing you to quickly identify significant changes in abundance over time.

#### Interpreting the P-Values

The p-value is a statistical measure that helps you determine if the observed difference between two groups (in this case, two years) is meaningful. The test is based on a **null hypothesis**, which states that there is no real difference in the abundance distributions between the two years.

-   A **small p-value** (conventionally, **p \< 0.05**) provides evidence against the null hypothesis. It suggests that the difference you see between the two years is unlikely to be due to random chance. You can conclude there is a **statistically significant change** in abundance. On the plot, these are often marked with asterisks (e.g., `*` for p \< 0.05, `***` for p \< 0.001).

-   A **large p-value** (e.g., **p ≥ 0.05**) means you don't have enough evidence to reject the null hypothesis. The observed difference could plausibly be due to random variation. In this case, you would conclude there is **no statistically significant change** between the two years. This is often marked as "ns" (not significant) on the plot.

### Community Similarity: `calculate_similarity()`

The `calculate_similarity()` function is used to compare the species composition and abundance between different samples. It calculates a pairwise similarity matrix, which is essential for analyses like ordination or clustering.

#### The Ruzicka Index

The function calculates the **Ruzicka similarity index**, which is a quantitative version of the more common Jaccard index. It is well-suited for abundance data (not just presence/absence) and is calculated with the following formula:

$$S_{\text{Ruzicka}} = \frac{\sum_{i=1}^{N} \min(A_i, B_i)}{\sum_{i=1}^{N} \max(A_i, B_i)}$$

Where: \* $S$ is the similarity between community A and community B. \* $A_i$ and $B_i$ are the standardized abundances of species `i` in communities A and B, respectively. \* $N$ is the total number of species shared between the two communities.

#### Interpreting the Similarity Score

The Ruzicka similarity score ranges from **0 to 1**: \* A value of **1** indicates that the two communities are identical, both in species composition and in their proportional abundances. \* A value of **0** indicates that the two communities share no species. \* Values in between represent a gradient of similarity.

#### Comparison Modes

The function can operate in two modes, controlled by the `group_by` argument:

1.  **`group_by = c("year", "plot_id")` (Default):** This mode provides the most detailed comparison. It treats each unique plot-year observation as a distinct sample and returns a matrix comparing every sample to every other sample. This is useful for fine-grained analysis, like Multidimensional Scaling (MDS) of individual plots.

2.  **`group_by = "year"`:** This mode is for high-level, year-to-year comparisons. It first calculates the *average* species composition for each year (by averaging abundances across all plots). It then returns a smaller matrix comparing each year's aggregate community profile to every other year.

#### `plot_abundance_trends()`

Creates faceted boxplots to visualize and test year-to-year changes in conspecific abundance and the HCA index. It uses a Wilcoxon rank-sum test to check for significant differences between consecutive years.

#### `plot_similarity_change()`

Visualizes the results of `calculate_similarity()` in three different ways (`plot_type`):

1.  **`"yearly_turnover"`**: Shows how the average plant community changes from one year to the next.

2.  **`"within_year_heterogeneity"`**: Shows how different the plots were from each other within each year.

3.  **`"plot_turnover"`**: Tracks how much each individual plot changed from one year to the next.

## Pollinator Diet Visualization: `plot_pollinator_diet()`

The `plot_pollinator_diet()` function is designed to visualize and statistically compare the interaction rates of pollinators that visit a specific focal plant. It helps answer the question: "Do the pollinators visiting my focal plant interact differently with it compared to other plants, and how does this change over time?"

### Conspecific vs. Heterospecific Interaction Rate

In the context of this function:

-   **Conspecific Interaction Rate**: Refers to the standardized rate (interactions per visit) at which a pollinator (known to visit the `focal_plant`) interacts *with the `focal_plant` itself*.

-   **Heterospecific Interaction Rate**: Refers to the standardized rate at which the *same group* of pollinators interacts *with any other plant species*.

The function calculates these rates based on data filtered by `get_interaction_data()` using the `standardize = TRUE` argument.

### The Visualization

The function creates side-by-side violin plots for each year specified. Within each year, one violin shows the distribution of **Conspecific** rates, and the other shows the distribution of **Heterospecific** rates. Jittered points can be displayed to show individual data points (either single interaction events or total rates per plot).

### Statistical Test

If `add_stats = TRUE`, the function performs an **unpaired Wilcoxon rank-sum test** (Mann-Whitney U test) to compare the distributions between **consecutive years**. This test is performed **independently** for the "Conspecific" group and the "Heterospecific" group.

-   **What it tests**: Whether there is a significant change in the interaction rates from one year to the next *within* each category.

-   **Interpretation**: Brackets with significance stars (`*`, `**`, `***`) or "ns" (not significant) are drawn above the violins, connecting consecutive years for each type separately. A significant result (e.g., `*`) suggests a meaningful change in interaction rates for that group between those two years.

### Key Parameters

-   `focal_plant`: The plant species code (e.g., `"Suc_pra"`) to focus the analysis on.

-   `years`: A numeric vector specifying which years to include.

-   `summary_level`: Controls what each data point represents.

    -   `"plot"` (default): Each point is the **total standardized rate for a single plot**.

    -   `"interaction"`: Each point is a **single interaction event**.

-   `add_stats`: `TRUE` (default) to display year-to-year statistical comparisons, `FALSE` to hide them.

-   `log_scale`: `FALSE` (default) for a linear y-axis. Set to `TRUE` to use a log10 scale, recommended for skewed rate data.

-   `colors`: Allows customization of the violin plot colors.

-   `ylim`: Allows manual setting of the y-axis limits.

-   `...`: Allows passing additional filters (like `is_pollinator = TRUE`) directly to `get_interaction_data()`.

------------------------------------------------------------------------

## Pollinator Interaction Dissimilarity: `plot_pollinator_interaction_dissimilarity()`

This function performs a sophisticated analysis to understand the consistency or variability of pollinator foraging patterns across different contexts (plots), comparing observed patterns to null model expectations. It helps answer questions about pollinator specialization and niche partitioning.

### The Analysis Concept

For a given pollinator species and year, the function assesses how similar or different its interaction patterns are across the various plots it was observed in.

-   **"Diet"**: The set of plant species visited by the pollinator in a specific plot, weighted by the standardized interaction rate on each plant.

-   **Comparison**: The function compares the "diet" of the pollinator between all pairs of plots where it was found within a given year.

-   **Dissimilarity Metric**: The **Morisita-Horn index** is used to quantify the dissimilarity between these plot-level diets.

### Morisita-Horn Index

This index (MH) is well-suited for abundance data. It ranges from 0 (identical communities) to 1 (completely different communities, sharing no species). It considers both the presence/absence of species and their relative abundances.

$$MH=1−(da​+db​)×Na​×Nb​2∑i=1S​(ai​×bi​)​$$

*(Note: This is one formulation; `vegan::vegdist(method="horn")` calculates a related distance, which we convert to dissimilarity).*

Where:

-   ai​,bi​: Abundance of plant species i in the pollinator's diet in plot A and plot B.

-   Na​,Nb​: Total abundance (sum of rates) in plot A and plot B.

-   da​,db​: Simpson's index components related to species proportions.

The function calculates all pairwise MH values between plots for a given pollinator and year, and then computes the **average** MH dissimilarity.

### The Null Model Comparison

To determine if the observed average dissimilarity is statistically meaningful, it's compared against a null model.

-   **Observed Value (Red Dot)**: The average MH dissimilarity calculated from your actual interaction data.

-   **Null Model (Grey Bar)**: Represents the range of average MH values expected purely by chance. This is generated by:

    1.  **Shuffling**: Randomly rearranging the observed interactions. The function offers two methods via `null_model_type`:

        -   `"shuffle"` (default): Uses `vegan`'s "r00" algorithm, which shuffles interactions while approximately preserving interaction totals per plant and per plot.

        -   `"abundance_weighted"`: Shuffles interactions *within each plot* based on the relative abundance of available plants in that plot (requires providing `plant_abundance_data`). This tests if pollinators visit plants simply in proportion to their availability.

    2.  **Iteration**: Repeating the shuffling process many times (controlled by `n_null_models`).

    3.  **Calculation**: Calculating the average MH dissimilarity for each randomized dataset.

    4.  **Range**: The grey bar shows the 95% confidence interval (middle 95% of values) from this distribution of null dissimilarities.

### Interpreting the Plot

The plot displays pollinators along the y-axis (often ordered by observed dissimilarity) and the mean MH dissimilarity on the x-axis.

-   **Red Dot vs. Grey Bar**: The key comparison.

    -   If the **red dot is outside the grey bar**: The pollinator's diet consistency across plots is significantly different from random expectation.

        -   **Above the bar**: The pollinator is more variable/specialized across plots than expected by chance.

        -   **Below the bar**: The pollinator is more consistent/generalized across plots than expected by chance.

    -   If the **red dot is inside the grey bar**: The observed dissimilarity is consistent with the null model (random expectation).

### Key Parameters

-   `years`: Selects the year(s) for analysis.

-   `summarize_years`: If `TRUE`, averages results across all specified years into a single plot panel. If `FALSE` (default), creates a separate panel for each year.

-   `null_model_type`: Choose between `"shuffle"` (interaction structure) or `"abundance_weighted"` (plant availability).

-   `plant_abundance_data`: Required only if using `null_model_type = "abundance_weighted"`.

-   `top_n_pollinators`, `min_plots`: Filter which pollinators are included based on abundance and number of plots observed in.

-   `n_null_models`: Number of randomizations (use a high number like 999 for final analysis).

-   `...`: Pass filters to `get_interaction_data()` (e.g., `is_pollinator = TRUE`).

    ------------------------------------------------------------------------

## Pollinator Sharing Analysis

This package includes two functions to analyze how plant species share pollinators within the network.

### 1. Calculating the Metric: `calculate_pollinator_sharing()`

This function quantifies the degree of pollinator sharing (niche overlap) for a specific network of interactions.

#### The Morisita-Horn Index

The function uses the **Morisita-Horn similarity index** to compare the pollinator communities visiting each plant. This index is well-suited for abundance data, as it considers both the presence and the relative abundance of shared pollinators. The score ranges from **0** (no shared pollinators) to **1** (identical pollinator communities).

#### Levels of Analysis

The function's `level` argument allows you to calculate this metric in two ways:

-   `level = "community"` (default): Returns a **single number** representing the average pollinator sharing across the entire plant community. This gives a "big picture" measure of how generalized the network is.

-   `level = "species"`: Returns a **data frame** with the average sharing score for each individual plant species. This helps identify which plants are more specialized (low score) or generalized (high score) in their pollinator use.

### 2. Visualizing the Dynamics: `pollinator_sharing_turnover()`

This function automates the process of calculating and visualizing how pollinator sharing changes over time. It repeatedly calls `calculate_pollinator_sharing()` on different subsets of your data and plots the results.

#### How it Works

You provide the function with the interaction data and a time variable to compare across (e.g., `"year"`). The function then:

1.  Splits the data for each time point (e.g., for each year).

2.  Calculates the sharing score for each of these network "snapshots."

3.  Generates a line plot showing the trend of the sharing score over time.

This can be done for the entire community (`level = "community"`) or for individual plant species (`level = "species"`), creating either a single trend line or a "spaghetti plot" with a line for each species.
