<img width="408" height="424" alt="image" src="https://github.com/user-attachments/assets/9e67a1e5-8daf-4b2a-a1ca-125b91bf4ba0" />


Henry Walter Bates (1825-1892)  

# Resource abundance can buffer trophic mismatch in a caterpillar-passerine food-chain
This repository contains the data and analytical pipeline for Weir et al. (2026). Resource abundance can buffer trophic mismatch in a caterpillar-passerine food-chain. PREPRINT CITATION. 

The paper explores the role of spring caterpillar abundance in buffering the negative fitness consequences of phenological mismatch with the timing of peak abundance for three species of passerine bird. In the workflow, we analyse caterpillar abundance across spring and estimate the timing and magnitude of peak abundance over 14 sites and 15 years. We estimate the breeding performance of three woodland passerine species, and relate this to different aspects of the spring caterpillar peak, principally timing and height. We also consider the relationship between phenological mismatch and temperature in this caterpillar-bird system.

## Data
Data consist of caterpillar abundance data (XXX) and bird breeding performance data (XXX) from 14 sites across England, collected over the 15-year period from 2008-2023. MetOffice HadUK data are also required for site temperature analyses, but base data are not provided directly here. Files covering the complete period April-June for all site by year combinations are required (see XXX.temperature_extraction.R), and can be obtained directly from the MetOffice. Relevant temperature data required for subsequent analyses are provided under 'data' (XXX).

## Workflow pipeline
This pipeline contains the analytical milestones needed to recreate the results of this study. It does not contain code used for data visualisation (e.g. predictions and plotting), preferences for which will vary.
