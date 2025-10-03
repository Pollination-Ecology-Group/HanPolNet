---
title: "README"
format: html
editor: visual
---

# READ ME

# HanPolNet 

The goal of HanPolNet is to provide the data and functions necessary to analyze the interannual changes in plant-pollinator interaction networks, as detailed in <Your Manuscript Title>.

## Installation

You can install the development version of HanPolNet from [GitHub](https://github.com) with:

```{r}
#| echo: true
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

## Purpose

This package serves to combine data collected during decade and half long lasting project called "Opylovači" now operated by Pollination Ecology Group and Flower Ecology Lab based at the Departments of Zoology and Botany, Faculty of Science, Charles University (Praha, CZE).

## How does this package works

### How Data is Organized in This Package

To ensure transparency and reproducibility, the data in this package follows a standard R package workflow, separating the raw source data from the clean, ready-to-use data. Think of it like a kitchen and a pantry.

#### `data/` (The Pantry)

This directory contains the final, clean datasets, saved in R's optimized `.rda` format. When you load the package, this data is immediately available for you to use in your analysis.

-   `plant_abundance`: A tidy data frame of plant counts, with year and plot ID as separate columns.

-   `plant_metadata`: A support table with information about each plant species, including their full name and counting method.

Think of the `data/` folder as a well-organized pantry, stocked with ingredients that are ready to be used in your analytical recipes.

#### `data-raw/` (The Kitchen)

This directory is the "kitchen" where the data was prepared. It contains:

1.  The original, raw data files (e.g., `.csv` files).

2.  The R scripts that were used to read, clean, and process the raw data into the final versions found in the `data/` folder.

This directory is part of the development source code but is **not included** in the final installed package. Its purpose is to provide a complete, reproducible trail from the original source files to the analysis-ready data. If you ever need to see exactly how the data was cleaned or want to start from the very beginning, the "recipe" is in here.

### What data are included in this package

-   `plant_abundance`: A tidy data frame of plant counts, with year and plot ID as separate columns.

-   `plant_metadata`: A support table with information about each plant species, including their full name and counting method.

### What functions are included in this package

before merging tables from different years, first check for lower and upper case sensitvyte
