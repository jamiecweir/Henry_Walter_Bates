<img width="408" height="424" alt="image" src="https://github.com/user-attachments/assets/e36d95d1-48c9-4973-81b2-99836ed2b099" />


Henry Walter Bates (1825-1892)  

---

# Resource abundance can buffer trophic mismatch in a caterpillar-passerine food-chain
This repository contains the data and analytical pipeline for Weir et al. (2026). Resource abundance can buffer trophic mismatch in a caterpillar-passerine food-chain. EcoEvoRxiv. https://doi.org/10.32942/X2KD4J. 

The paper explores the role of spring caterpillar abundance in buffering the negative fitness consequences of phenological mismatch with the timing of peak abundance for three species of passerine birds. In the workflow, we analyse caterpillar abundance across spring and estimate the timing and magnitude of peak abundance over 14 sites and 15 years. We estimate the breeding performance of three woodland passerine species, and relate this to different aspects of the spring caterpillar peak, principally timing and height. We also consider the relationship between phenological mismatch and temperature in this caterpillar-bird system.

## Data
Data consist of caterpillar abundance data (`frass_data_FINAL.csv`) and bird breeding performance data (`Nest_XXX.csv`) from 14 sites across England, collected over the 15-year period from 2008-2023. Processed data used in subsequent analyses are also provided (`complete_data.csv`). 
MetOffice HadUK data are required for site temperature analyses, but the base data are not provided directly here. Files covering the complete period April-June for all site by year combinations are required (see `2_temperature_data_extraction.R`), and can be obtained directly from the MetOffice. However, relevant processed temperature data used for subsequent analyses are provided here (`mean_monthly temp.csv`, `mean_temp_data_XXX.csv`, `complete_data_with_temp.csv`).
Geolocations of study sites are also included (`site_locations.csv`).

## Workflow pipeline
This pipeline contains the analytical milestones needed to reproduce the results of this study. It does not contain code used for data visualisation (e.g. predictions and plotting), preferences for which will vary.

- **Step 1** `1_frass_modelling.R` : Estimating the caterpillar peak date and height (i.e. peak caterpillar biomass) in each site by year combination across available data. 
- **Step 2** `2_temperature_data_extraction.R` : Extract interpolated MetOffice temperature data for spring in each site by year combination relevant to our dataset.
- **Step 3** `3_temperature_variation_modelling.R` : Model variation in spring temperature across all sites over time.
- **Step 4** `4_bird_data_processing_and_mismatch_estimation.R` : Collate bird breeding performance data across sites and years, pairing data with relevant caterpillar abundance data and estimating trophic mismatch at the nest-level (i.e. difference in timing between peak chick demand within a nest and date of peak caterpillar abundance).
- **Step 5** `5_temperature_effects_on_mismatch.R` : Model the relationship between temperature and trophic mismatch in three target bird species.
- **Step 6** `6_bird_breeding_performance_modelling.R` : Model the effects of mismatch and caterpillar abundance on breeding performance across three target bird species.
- **Step 7** `7_variance_component_estimation.R` : Estimate variance components for all relevant models, (i) temperature variation, (ii) temperature-mismatch relationship, (iii) effects of mismatch and resource abundance on bird breeding performance.
- **Step 8** `8_forecast_mismatch_productivity_under_climate_change.R` : Rough forecast of mismatch under climate change based on modelled relationship, for Table 2. 

## Rights
The authors reserve all rights to the data, which cannot be utilised in whole or in part without their prior consent.

## Contact
JamieCWeir@outlook.com
