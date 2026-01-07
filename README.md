# Attractiveness Cue Affects Moral Judgments

## Project Overview
An experimental data analysis project examining how attractiveness cues influence moral judgments, using trial-level behavioral data and validation-focused data cleaning in R.

**Type:** Experimental, quantitative  
**Tools:** R, dplyr, tidyr, lmerTest, ggplot2, R Markdown  
**Author:** Da Eun Han

## Methods and Data
Participants classified words as moral, immoral, or attractive using keyboard responses, with additional distractor words classified via the space bar. Distractors were either vanity-related or color-related (control), depending on condition. The responses and latency were logged per trial. Across two test blocks, attractiveness was paired with either morality or immorality on the same response key. Discrepancies in response latency between the two test blocks indicate the strength of association between morality and attractiveness. Associative strength was then compared between the vanity-prime and color-prime conditions.

## Key Analyses
- Data cleaning, feature construction, Data Joining
- Sanity check
- Data transformation for latency variable
- Mixed-effects linear models


## Project Structure
- `raw_data/` – Original csv files each for participant level demographic and trial-level IAT data
- `processed_data/` – Cleaned and feature-engineered CSV dataset with trial-level behavioral data merged with demographic survey data
  
- `preparation/` – R script for cleaning, feature engineering, and joining dataset
- `analysis/` – R Markdown document containing data manipulation, analysis, visualizations, and interpretations.

## Reference
This repository contains a self-contained reanalysis and visualization of key findings from the published study, adapted for portfolio and methodological demonstration purposes. It does not include the full set of studies or analyses reported in the original article

Primary publication:

Han, D., & Laurent, S. M. (2023). Beautiful seems good, but perhaps not in every way: Linking attractiveness to moral evaluation through perceived vanity. Journal of Personality and Social Psychology, 124(2), 264–280.

