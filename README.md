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

## Core Functions

### Data Access and Standardization: `get_plant_data()`

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
